#Requires -Version 5.1
<#
.SYNOPSIS
  Wrap a .duckdb fixture as an Emscripten WORKERFS filesystem image.

.DESCRIPTION
  The PowerShell twin of package_fixture.sh, for Windows where bash is not
  available. Same inputs, same outputs: <stem>.data + <stem>.js.metadata, which
  is what webr::mount(type = "WORKERFS") consumes.

  WORKERFS is the point of the whole spike: it is supposed to avoid copying file
  contents into wasm linear memory until they are actually read. If that holds
  for DuckDB's random-access pattern, a 400 MB snapshot is viable in a browser
  tab. If it does not, the snapshot budget is roughly "whatever fits in 4 GB
  alongside R".

  Requires emsdk: https://emscripten.org/docs/getting_started/downloads.html
    git clone https://github.com/emscripten-core/emsdk
    cd emsdk
    .\emsdk.bat install latest
    .\emsdk.bat activate latest
    .\emsdk_env.ps1

  .\emsdk_env.ps1 sets $env:EMSDK for the CURRENT terminal only. Open a new one
  and you must run it again, or this script will not find file_packager.py.

.EXAMPLE
  .\package_fixture.ps1 fixtures\bold_spike_01.duckdb

.NOTES
  If PowerShell refuses to run this ("running scripts is disabled on this
  system"), unblock it for this terminal only -- no admin rights needed:
    Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
#>
[CmdletBinding()]
param(
  [Parameter(Mandatory = $true, Position = 0)]
  [string]$Fixture
)

$ErrorActionPreference = 'Stop'

# Print failures as plain readable text. PowerShell's default formatter renders an
# uncaught throw as a red stack trace and collapses a multi-line message onto one
# line -- which is exactly the wrong thing for messages whose job is to tell you
# what to install. Cleanup in `finally` blocks still runs before this fires.
trap {
  Write-Host ""
  Write-Host $_.Exception.Message -ForegroundColor Red
  Write-Host ""
  exit 1
}

# --- locate the fixture -------------------------------------------------------
if (-not (Test-Path -LiteralPath $Fixture -PathType Leaf)) {
  throw "No such file: $Fixture"
}
$db   = Get-Item -LiteralPath $Fixture
$dir  = $db.Directory.FullName
$base = $db.Name
$stem = [System.IO.Path]::GetFileNameWithoutExtension($base)

# A .wal beside the database makes it impossible to open read-only -- DuckDB
# cannot replay a WAL without write access, so the browser would refuse it.
if (Test-Path -LiteralPath (Join-Path $dir "$base.wal")) {
  throw "$base.wal exists beside the fixture; rebuild it (the browser cannot open a database with a WAL read-only)"
}

# --- locate file_packager.py --------------------------------------------------
$packager = $null
if ($env:EMSDK) {
  $candidate = [System.IO.Path]::Combine($env:EMSDK, 'upstream', 'emscripten', 'tools', 'file_packager.py')
  if (Test-Path -LiteralPath $candidate -PathType Leaf) {
    $packager = (Get-Item -LiteralPath $candidate).FullName
  }
}
if (-not $packager) {
  $onPath = Get-Command 'file_packager.py' -ErrorAction SilentlyContinue
  if ($onPath) { $packager = $onPath.Source }
}
if (-not $packager) {
  throw @"
file_packager.py not found.

Install emsdk, then activate it in THIS terminal:
  cd <wherever you cloned emsdk>
  .\emsdk_env.ps1

That sets EMSDK, which is how this script finds the packager. It does not
persist between terminals.
"@
}

# --- locate a working Python --------------------------------------------------
# emsdk ships its own Python on Windows. Prefer it: a bare `python` on PATH may be
# the Microsoft Store alias stub, which exits without running anything, so each
# candidate is tested by actually invoking it.
function Test-PythonCandidate {
  param([string]$Exe)
  if (-not $Exe) { return $false }
  try {
    $out = & $Exe --version 2>&1
  } catch {
    return $false
  }
  return ($LASTEXITCODE -eq 0 -and "$out" -match 'Python 3')
}

$pythonCandidates = New-Object System.Collections.Generic.List[string]
if ($env:EMSDK_PYTHON) { $pythonCandidates.Add($env:EMSDK_PYTHON) }
if ($env:EMSDK) {
  $bundled = Get-ChildItem -Path (Join-Path $env:EMSDK 'python') -Filter 'python.exe' `
                           -Recurse -Depth 2 -File -ErrorAction SilentlyContinue
  foreach ($p in $bundled) { $pythonCandidates.Add($p.FullName) }
}
foreach ($name in @('python3', 'python')) {
  $cmd = Get-Command $name -ErrorAction SilentlyContinue
  if ($cmd -and $cmd.Source) { $pythonCandidates.Add($cmd.Source) }
}

$python = $null
foreach ($candidate in $pythonCandidates) {
  if (Test-PythonCandidate -Exe $candidate) { $python = $candidate; break }
}
if (-not $python) {
  throw @"
No working Python 3 found.

emsdk normally provides one -- run .\emsdk_env.ps1 in this terminal first.
Otherwise install Python from https://www.python.org/downloads/ (tick "Add
python.exe to PATH" in the installer).

Note: if `python` opens the Microsoft Store, that is a placeholder, not Python.
Turn it off under Settings > Apps > Advanced app settings > App execution aliases.
"@
}

# --- package ------------------------------------------------------------------
# Everything below runs from the fixture's own directory with RELATIVE paths.
# Emscripten's argument parsing and PowerShell's argument quoting both get
# fragile with spaces in paths, and "C:\Users\Firstname Lastname\..." is normal
# on Windows. Relative names sidestep it entirely.
$stage = Join-Path $dir '.pkgstage'

Push-Location -LiteralPath $dir
try {
  if (Test-Path -LiteralPath $stage) { Remove-Item -LiteralPath $stage -Recurse -Force }
  New-Item -ItemType Directory -Path $stage | Out-Null
  Copy-Item -LiteralPath $db.FullName -Destination (Join-Path $stage $base)

  Write-Host "Packaging $base ($([math]::Round($db.Length / 1MB, 1)) MB)..."

  # --separate-metadata gives <stem>.data + <stem>.js.metadata, the pair webR wants.
  # --preload <dir>@/ places the file at the mount root, so it appears at
  # /bold/<basename> once mounted at /bold.
  & $python $packager "$stem.data" `
      --preload ".pkgstage@/" `
      --separate-metadata `
      "--js-output=$stem.js"

  if ($LASTEXITCODE -ne 0) {
    throw "file_packager.py exited with code $LASTEXITCODE"
  }
} finally {
  Pop-Location
  if (Test-Path -LiteralPath $stage) {
    Remove-Item -LiteralPath $stage -Recurse -Force -ErrorAction SilentlyContinue
  }
}

$dataFile = Join-Path $dir "$stem.data"
$metaFile = Join-Path $dir "$stem.js.metadata"
foreach ($f in @($dataFile, $metaFile)) {
  if (-not (Test-Path -LiteralPath $f)) {
    throw "Expected output missing: $f -- file_packager reported success but wrote nothing"
  }
}

Write-Host ""
Write-Host "Wrote:"
foreach ($f in @($dataFile, $metaFile)) {
  $written = Get-Item -LiteralPath $f
  Write-Host ("  {0,9:N1} MB  {1}" -f ($written.Length / 1MB), $written.FullName)
}

# Paths are quoted in the hint because a Windows home directory routinely contains
# a space, and an unquoted path is the copy-paste that fails.
Write-Host ""
Write-Host "Now point app\app.R at it -- do NOT copy it into app\, which shinylive would"
Write-Host "bundle into the app payload. export.R copies it into site\ for you."
Write-Host ""
Write-Host "  FIXTURE_IMAGE <- Sys.getenv(`"SPIKE_FIXTURE_IMAGE`", `"fixtures/$stem.data`")"
Write-Host "  FIXTURE_DB    <- Sys.getenv(`"SPIKE_FIXTURE_DB`",    `"/bold/$base`")"
Write-Host ""
Write-Host "Edit those DEFAULTS -- do not rely on the environment variables. app.R runs"
Write-Host "inside webR in the browser, where your shell environment does not exist, so"
Write-Host "only the values baked into the exported file take effect. Then: Rscript export.R"
