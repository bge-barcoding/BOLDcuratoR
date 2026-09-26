# BOLDcurator installer for Windows -- the pip/uv route.
#
#   powershell -ExecutionPolicy ByPass -c "irm https://bge-barcoding.github.io/BOLDcuratoR/install.ps1 | iex"
#
# What it does, in order (each step is also a command you can run yourself;
# see the website's "Prefer to run the steps yourself?" panel):
#
#   1. installs uv (https://docs.astral.sh/uv/) with Astral's own installer,
#      only if it isn't already on this machine;
#   2. uv tool install --python 3.11 "boldcurator[desktop]"   -- uv fetches
#      Python 3.11 itself if needed and keeps BOLDcurator in its own
#      environment, away from any other Python on the machine;
#   3. boldcurator install-shortcut   -- "BOLDcurator (Python)" in the
#      Start menu and on the Desktop (named so it never replaces the
#      setup.exe installer's own "BOLDcurator" shortcut);
#   4. uv tool update-shell   -- so `boldcurator` works in new terminals.
#
# Running it again upgrades an existing install and refreshes the shortcut.
# Nothing here needs admin rights; everything lands in your user profile.
#
# Overrides (for testing -- CI points BOLDCURATOR_SPEC at a freshly built
# wheel):  BOLDCURATOR_SPEC  what to install (default "boldcurator[desktop]")
#          BOLDCURATOR_PYTHON  Python version (default 3.11, what CI tests)
#
# The whole script is one script block so that, run through `irm | iex` in
# someone's own PowerShell window, it leaves no variables behind and a
# failure stops the script without closing their window.

& {
    $ErrorActionPreference = 'Stop'

    $spec = if ($env:BOLDCURATOR_SPEC) { $env:BOLDCURATOR_SPEC } else { 'boldcurator[desktop]' }
    $pythonVersion = if ($env:BOLDCURATOR_PYTHON) { $env:BOLDCURATOR_PYTHON } else { '3.11' }

    # uv's installer puts uv in ~\.local\bin (or $env:XDG_BIN_HOME), which a
    # fresh install hasn't put on this window's PATH yet -- so look there too.
    function Find-Uv {
        $cmd = Get-Command uv -ErrorAction SilentlyContinue
        if ($cmd) { return $cmd.Source }
        foreach ($dir in @($env:XDG_BIN_HOME, "$HOME\.local\bin", "$HOME\.cargo\bin")) {
            if ($dir -and (Test-Path (Join-Path $dir 'uv.exe'))) { return (Join-Path $dir 'uv.exe') }
        }
        return $null
    }

    $uv = Find-Uv
    if (-not $uv) {
        Write-Host '==> Installing uv, the tool that installs and updates BOLDcurator'
        Invoke-RestMethod https://astral.sh/uv/install.ps1 | Invoke-Expression
        $uv = Find-Uv
        if (-not $uv) { throw 'uv was installed but could not be found; open a new PowerShell window and run this again' }
    }

    Write-Host "==> Installing BOLDcurator ($spec, Python $pythonVersion)"
    & $uv tool install --python $pythonVersion --upgrade $spec
    if ($LASTEXITCODE -ne 0) { throw 'uv tool install did not succeed (see the messages above)' }

    $binDir = (& $uv tool dir --bin | Out-String).Trim()
    Write-Host '==> Adding the BOLDcurator (Python) shortcut'
    & (Join-Path $binDir 'boldcurator.exe') install-shortcut
    if ($LASTEXITCODE -ne 0) { throw 'BOLDcurator is installed, but the shortcut could not be created' }

    # Best effort: the app and its shortcut already work without it. The
    # try matters on Windows PowerShell 5.1, where redirecting a native
    # command's stderr under ErrorActionPreference=Stop turns any line of
    # it into a terminating error.
    try { & $uv tool update-shell *> $null } catch { }

    Write-Host ''
    Write-Host 'BOLDcurator is installed.'
    Write-Host 'Open "BOLDcurator (Python)" from the Start menu or the Desktop.'
    Write-Host 'Update later with:   uv tool upgrade boldcurator'
    Write-Host 'Uninstall with:      boldcurator remove-shortcut; uv tool uninstall boldcurator'
}
