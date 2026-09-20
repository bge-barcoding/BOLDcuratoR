; Inno Setup script (plan 4.3): wraps the PyInstaller --onedir output
; (../dist/boldcurator/) into a real setup.exe -- Start Menu entry, an
; optional desktop shortcut, an uninstaller, an "Add/Remove Programs"
; entry. Compiled by .github/workflows/python-release.yml on Windows via
; `ISCC.exe windows-installer.iss`; see python/packaging/README.md for the
; full account, including what has and hasn't been verified.
;
; This is a second, independent way to get the app onto a machine
; alongside the plain zip -- it does not change what's inside dist\, only
; how it's delivered. Which --window mode the app itself uses (plan
; 4.1a/desktop.py) is unrelated to this file.
;
; Local build: pass /DMyAppVersion=x.y.z to ISCC, or accept the dev
; default below. CI passes the release tag.

#define MyAppName "BOLDcurator"
#ifndef MyAppVersion
  #define MyAppVersion "0.0.0-dev"
#endif
#define MyAppPublisher "BGE Barcoding"
#define MyAppExeName "boldcurator.exe"
#define MyAppSourceDir "..\dist\boldcurator"

[Setup]
; Fixed once, for this project -- Inno Setup uses this (not the version)
; to recognise "this is the same app" across upgrades. Never change it.
AppId={{FEF3C88D-F1B0-4476-915B-2DEC1239A1B4}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
OutputDir=Output
; Fixed, not version-suffixed: the project website (website/index.html)
; links straight to
; https://github.com/<org>/<repo>/releases/latest/download/BOLDcuratorSetup-x64.exe,
; which only stays correct release after release if this name never
; changes. The version itself is still recorded in AppVersion above (shown
; in the wizard and Add/Remove Programs) -- only the filename drops it.
OutputBaseFilename=BOLDcuratorSetup-x64
Compression=lzma
SolidCompression=yes
WizardStyle=modern
SetupIconFile=icon.ico
UninstallDisplayIcon={app}\{#MyAppExeName}
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
; Unsigned for now -- plan 4.4 is a deliberate, budget-driven no; this
; triggers a SmartScreen warning with a manual "Run anyway", not a block.

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; \
    GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "{#MyAppSourceDir}\*"; DestDir: "{app}"; \
    Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\Uninstall {#MyAppName}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; \
    Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; \
    Description: "{cm:LaunchProgram,{#MyAppName}}"; \
    Flags: nowait postinstall skipifsilent
