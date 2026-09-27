# macOS signing and notarisation runbook (plan 4.4)

How BOLDcurator's macOS download goes from "Apple could not verify
“BOLDcurator” is free of malware" to opening normally: register for an Apple
Developer ID, prove signing works on a real Mac, hand the credentials to
GitHub, then have the release workflow sign and notarise every macOS build.

This file is written to be followed across several sessions, by people and
by Claude Code. **Keep the status table up to date** -- tick a step in the
same commit that finishes it (or in a small commit of its own), so whoever
picks this up next knows where things stand.

## Status

| Step | What | Who | Status | Notes |
|---|---|---|---|---|
| A1 | Choose the Apple Developer account | Project owner | ☐ | |
| A2 | Enrolled; Team ID known | Account Holder | ☐ | Team ID: `__________` (not secret) |
| A3 | Developer ID Application certificate on the Mac | Colleague / Account Holder | ☐ | |
| A4 | Notarisation API key + `boldcurator-notary` profile | Colleague | ☐ | |
| A5 | Local sign + notarise of a real build passes | Colleague (+ Claude on the Mac) | ☐ | |
| B1 | Certificate exported as `.p12` | Colleague | ☐ | |
| B2 | Five repository secrets added | Repo admin | ☐ | |
| B3 | Secrets checked, local copies cleaned up | Repo admin | ☐ | |
| C1 | `packaging/entitlements.plist` added | Claude | ☐ | |
| C2 | `packaging/macos_sign.sh` added | Claude | ☐ | |
| C3 | Signing steps in `python-release.yml` | Claude | ☐ | |
| C4 | Docs + website FAQ updated | Claude | ☐ | |
| C5 | Branch build signed; artifact passes Gatekeeper on a Mac | Claude + colleague | ☐ | |
| C6 | Live: a new release ships signed zips | Project owner + Claude | ☐ | |
| D  | Upkeep reminders recorded (renewal dates) | Project owner | ☐ | Membership renews: `______` Cert expires: `______` |

## For Claude: how to use this file

- Read the status table first; carry on from the first unticked step.
- **Parts A and B are done by people on a Mac and in GitHub's settings.**
  Guide them one step at a time, ask them to paste the ✅ checkpoint output,
  and check it against what's written here. If this session is running in
  the cloud rather than on their Mac, you can't run the `security`,
  `codesign` or `xcrun` commands yourself -- they run them and paste back.
- **Never ask for, accept, print or commit a secret value**: the `.p12`
  file or its password, the `.p8` key, or their base64 text. Key ID, Issuer
  ID and Team ID are not secret, but still go into GitHub as secrets, not
  into this file (apart from the Team ID in the table). If someone pastes a
  secret into the chat anyway, tell them to treat it as leaked (see D3).
- **Part C is code, done by Claude**, and only once B is ticked. Before
  changing anything, re-read `.github/workflows/python-release.yml`: the
  step names and ordering quoted in C3 are from when this was written
  (V3.4, September 2026) and may have moved.
- Don't create a pull request unless asked. Tick the table as you go.

## Why this is needed

The macOS builds are signed **ad hoc** (PyInstaller's default): a signature
that proves the files haven't changed since the build, but says nothing
about who built them. macOS 15 (Sequoia) blocks any quarantined app --
anything downloaded with a browser -- that isn't signed with an Apple
**Developer ID** and **notarised**, with "Apple could not verify ... is free
of malware" and only *Move to Trash* / *Done*. The way past it (Done, then
System Settings → Privacy & Security → Open Anyway, plus a password) is
exactly what curators on Apple Silicon have been stuck on. The v3.4 build
itself is fine: CI verifies its signature, its macOS 11 minimum and every
smoke test, and it runs once the quarantine flag is cleared. Intel Macs
tend to get through only because they're more often on macOS 14 or older,
where right-click → Open still bypasses the check.

**Notarisation** means uploading the signed app to Apple, whose service
scans it and records a "ticket". **Stapling** attaches that ticket to the
`.app`, so Gatekeeper can accept it even offline. With both, a curator gets
the ordinary "downloaded from the Internet -- Open?" prompt and nothing
else.

This is macOS only. Windows SmartScreen needs a separate code-signing
certificate (Microsoft's Azure-based Trusted Signing is the low-cost route,
about US$10/month); not covered here.

---

## Part A -- Register and prove it works (people, on a Mac)

### A0. Before starting

- A Mac on macOS 14 or newer, ideally Apple Silicon (it can test the
  arm64 build, which is the one that's been failing).
- Command Line Tools: `xcode-select --install` (full Xcode not needed).
  ✅ `xcrun --find notarytool` prints a path.
- This repository cloned and this branch checked out:
  ```sh
  git clone https://github.com/bge-barcoding/BOLDcuratoR.git
  cd BOLDcuratoR
  git checkout claude/jolly-ramanujan-94h6ye   # or main, once merged
  ```
- An Apple Account with two-factor authentication turned on.
- Optional but recommended: run `claude` in the repository folder and say
  *"walk me through python/packaging/MACOS_SIGNING.md, Part A"*.

### A1. Choose the account

A Developer ID certificate belongs to an Apple Developer Program team. The
name on the team is what Gatekeeper shows as the developer.

| Option | Name curators see | Cost | Time | Notes |
|---|---|---|---|---|
| **1. NHM's existing organisation account** (ask NHM digital/IT whether one exists) | The Natural History Museum | Nothing extra | Days (internal) | Best, if it exists. Only the **Account Holder** can create a Developer ID certificate, or an Admin they've given that permission to. They can make it and send you the `.p12` (A3), or add you to the team. |
| **2. New NHM organisation account** | The Natural History Museum | US$99/year (about £79), or free with a **fee waiver** | Days to 2 weeks | Needs: NHM's **D-U-N-S number** (free; check at developer.apple.com/enroll/duns-lookup -- NHM very likely has one); someone with **legal authority** to agree to Apple's terms for NHM; a work email on NHM's domain; NHM's public website. Apple verifies by phone. |
| **3. Individual account** | Your personal name | US$99/year | Hours to 2 days | Quickest. But the account belongs to one person, and moving to an organisation later means a new certificate (fine -- just repeat A3 onwards). |

**Fee waiver (option 2):** Apple waives the membership fee for non-profit
organisations, accredited educational institutions and government entities
in eligible countries. NHM is a registered charity and a public body, so it
may qualify. The waiver is requested for an organisation enrolment, before
paying -- follow the current steps on Apple's "Membership fee waiver" page
(developer.apple.com/support/membership-fee-waiver).

✅ Tick A1 with the choice in Notes.

### A2. Enrol

Go to developer.apple.com/programs/enroll and choose *Individual / Sole
Proprietor* or *Organization* to match A1 (the Apple Developer app on an
iPhone or Mac is the quickest route for an individual). For an
organisation, the legal entity name must match the D-U-N-S record exactly.

✅ developer.apple.com/account → Membership details shows **Apple Developer
Program** as active, and a 10-character **Team ID**. Put the Team ID in the
status table (it isn't secret).

### A3. Create the Developer ID Application certificate

Do this **on the Mac that will do A5 and B1**: the certificate's private key
is created on that Mac and never leaves it except inside the `.p12` export.
A team can hold only a few Developer ID certificates, so make one and reuse
it; don't create extras.

1. Open **Keychain Access** (Spotlight: "Keychain Access"; on macOS 15 it's
   in `/System/Library/CoreServices/Applications/`).
2. Menu **Keychain Access → Certificate Assistant → Request a Certificate
   From a Certificate Authority…**. Enter your email and a common name
   (e.g. "BOLDcurator signing"), leave CA Email empty, choose **Saved to
   disk**, save the `.certSigningRequest` file.
3. developer.apple.com/account → **Certificates, IDs & Profiles** →
   Certificates → **+** → under Software choose **Developer ID
   Application** (not *Developer ID Installer*, not *Apple Distribution*) →
   Continue → Profile Type **G2 Sub-CA** → upload the request file →
   Download the `.cer`.
4. Double-click the downloaded `.cer` to add it to the **login** keychain.

(With full Xcode installed, Xcode → Settings → Accounts → your team →
Manage Certificates → **+** → Developer ID Application does steps 1-4 in
one go.)

**If only the Account Holder can do this** (option 1 or 2): they do steps
1-4 on their own Mac, then B1 (export a `.p12`), and give the colleague the
`.p12` and its password through a password manager, never email or chat.
The colleague double-clicks the `.p12` to import it and carries on.

✅ `security find-identity -v -p codesigning` lists a line like
`1) 3F2A…E9 "Developer ID Application: The Natural History Museum (AB12CD34EF)"`.
If there's more than one Developer ID Application line, note the full name
in quotes -- A5 needs it.

### A4. Create the notarisation API key

1. appstoreconnect.apple.com → **Users and Access** → **Integrations** tab
   → **App Store Connect API** → **Team Keys**. (The first time, the
   Account Holder has to click *Request Access* once.)
2. **+** / *Generate API Key*: name "BOLDcurator notarisation", access
   **Developer** → Generate.
3. **Download** the `AuthKey_<KEYID>.p8` file. Apple lets you download it
   **only once** -- keep it until B2 is done.
4. Note the **Key ID** (in the key's row) and the **Issuer ID** (shown above
   the list).
5. Save them as a notarytool profile in the login keychain:
   ```sh
   xcrun notarytool store-credentials boldcurator-notary \
     --key ~/Downloads/AuthKey_XXXXXXXXXX.p8 \
     --key-id XXXXXXXXXX \
     --issuer xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
   ```
   It checks them with Apple before saving.

✅ `xcrun notarytool history --keychain-profile boldcurator-notary` runs
without an error (an empty history is fine).

(Fallback if API keys aren't available: an app-specific password from
account.apple.com, with `store-credentials boldcurator-notary --apple-id
<email> --team-id <TEAMID> --password <app-specific password>`. CI would
then need those three as secrets instead of the key; prefer the API key.)

### A5. Sign and notarise a real build locally

This proves the certificate, the key, the entitlements and the signing
order all work -- **before** anything touches CI -- on exactly what curators
download.

1. Put C1's `entitlements.plist` and C2's `macos_sign.sh` in a work folder.
   If Part C is already merged they're in `python/packaging/`; otherwise
   copy the two code blocks from C1 and C2 below (Claude can write them
   out for you). Then:
   ```sh
   mkdir -p ~/boldcurator-signing && cd ~/boldcurator-signing
   # (entitlements.plist and macos_sign.sh are here now)
   chmod +x macos_sign.sh
   ```
2. Download the current Apple Silicon build and unzip it (use
   `boldcurator-macos-x86_64.zip` on an Intel Mac). `curl` doesn't mark
   files as downloaded from the internet, so nothing is quarantined yet:
   ```sh
   curl -fL -o boldcurator-macos-arm64.zip \
     https://github.com/bge-barcoding/BOLDcuratoR/releases/latest/download/boldcurator-macos-arm64.zip
   rm -rf unzipped && ditto -x -k boldcurator-macos-arm64.zip unzipped
   ```
3. Sign, notarise and staple (a few minutes; most of it is Apple's queue).
   Leave out `--identity` if A3's check showed only one Developer ID
   Application line; otherwise pass the full quoted name:
   ```sh
   ./macos_sign.sh --notary-profile boldcurator-notary \
     --entitlements entitlements.plist \
     unzipped/BOLDcurator.app
   ```
   ✅ It ends with `accepted`, `source=Notarized Developer ID` and
   `Done: unzipped/BOLDcurator.app is signed, notarised and stapled.`
   If notarisation is rejected, the script prints Apple's log -- paste it
   to Claude (and see Troubleshooting).
4. The signed app still works under the hardened runtime:
   ```sh
   unzipped/BOLDcurator.app/Contents/MacOS/BOLDcurator selftest --network
   ```
   ✅ Same passing output as before signing. A crash or a missing-library
   error here means an entitlement is needed (Troubleshooting).
5. The real test -- what a curator's browser download looks like:
   ```sh
   rm -rf gatekeeper-test && ditto unzipped gatekeeper-test
   xattr -r -w com.apple.quarantine "0081;$(printf %x "$(date +%s)");Safari;" gatekeeper-test/BOLDcurator.app
   open gatekeeper-test/BOLDcurator.app
   ```
   ✅ macOS asks *"BOLDcurator" is an app downloaded from the Internet. Are
   you sure you want to open it?* with an **Open** button -- **not** "Apple
   could not verify…". Click Open; the app starts.

Tick A3-A5. Nothing in A5 is committed; the work folder can be deleted.

---

## Part B -- Hand the credentials to GitHub (a repo admin)

### B1. Export the certificate as `.p12`

Keychain Access → **login** keychain → **My Certificates** → right-click
"Developer ID Application: …" (it has a disclosure triangle with the
private key under it -- that's the one) → **Export…** → format **Personal
Information Exchange (.p12)** → save as `boldcurator-developer-id.p12` →
set a **strong password** (a password manager's generator).

✅ The file exists, and `openssl pkcs12 -in boldcurator-developer-id.p12
-nokeys -legacy | grep subject` (enter the password) shows the Developer ID
name. (Drop `-legacy` if your openssl complains about it.)

### B2. Add the five repository secrets

GitHub → the repository → **Settings → Secrets and variables → Actions →
New repository secret**, once for each. Names must be exactly these:

| Secret | Value | How to get it (copies to the clipboard) |
|---|---|---|
| `MACOS_CERT_P12_BASE64` | the `.p12`, base64 | `base64 -i boldcurator-developer-id.p12 \| pbcopy` |
| `MACOS_CERT_PASSWORD` | the `.p12`'s password | from the password manager |
| `APPLE_API_KEY_P8_BASE64` | the `.p8`, base64 | `base64 -i ~/Downloads/AuthKey_XXXXXXXXXX.p8 \| pbcopy` |
| `APPLE_API_KEY_ID` | Key ID (A4) | App Store Connect |
| `APPLE_API_ISSUER_ID` | Issuer ID (A4) | App Store Connect |

Rules:
- These never go into the repository, an issue, a PR, or a chat with
  Claude. The repo's `.gitignore` ignores `*.p12`, `*.p8`, `*.cer` and
  `*.certSigningRequest` as a backstop.
- Repository secrets are readable by anyone who can push a workflow change
  to this repository -- keep write access to people you trust with the
  certificate.

### B3. Check and clean up

✅ The Actions secrets page lists all five names.

Then store the `.p12` (with its password) and the `.p8` in the team's
password manager and delete the local copies (`rm` them, empty the Trash).
The certificate itself stays in the login keychain, so A5 still works on
this Mac. Tick B1-B3.

---

## Part C -- Sign the release builds in CI (Claude, in a later session)

Start only once B is ticked. Work on a branch; don't publish a release
from it. After C1-C4, run through C5 before merging.

### C1. `python/packaging/entitlements.plist`

The hardened runtime (required for notarisation) blocks a few things
Python may do. Start with just this one; add more only when a smoke test
proves one is needed (Troubleshooting).

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <!-- ctypes (truststore, pywebview/pyobjc) may need writable+executable
         memory for callbacks; the hardened runtime forbids it otherwise. -->
    <key>com.apple.security.cs.allow-unsigned-executable-memory</key>
    <true/>
</dict>
</plist>
```

### C2. `python/packaging/macos_sign.sh`

Re-signs the `.app` that PyInstaller already built (ad hoc), inside out:
every Mach-O file first, deepest first, then each nested bundle
(`Python.framework`), then the app itself with the entitlements. The
PyInstaller step in the workflow stays exactly as it is. (If this ever
proves fragile, PyInstaller's own `--codesign-identity` and
`--osx-entitlements-file` flags are the alternative -- but then signing
needs a full rebuild, and A5 can't test a downloaded build.)

Mark it executable (`git update-index --chmod=+x`), and run `shellcheck`
and `bash -n` on it. When this was written it had only been passed by
`shellcheck` and run on Linux with stand-in `codesign`/`xcrun`/`spctl`,
which checked the signing order (nested first, symlinks skipped, main
executable last via the bundle) and both notarisation outcomes. It has not
yet run against Apple's real tools -- A5 is that first run.

```bash
#!/usr/bin/env bash
# Sign BOLDcurator.app with a Developer ID, notarise it, staple the ticket.
# Used by .github/workflows/python-release.yml and for a local check on a
# Mac -- see python/packaging/MACOS_SIGNING.md.
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: macos_sign.sh [options] BOLDcurator.app

  --identity ID         codesign identity (default: "Developer ID Application",
                        which matches when the keychain holds exactly one).
                        "-" signs ad hoc: no hardened runtime, no notarisation.
  --entitlements FILE   entitlements for the app (default: entitlements.plist
                        next to this script)
  --keychain FILE       keychain holding the identity (CI's temporary keychain)
  --notary-profile NAME notarytool keychain profile (a local Mac); otherwise
                        APPLE_API_KEY_PATH, APPLE_API_KEY_ID and
                        APPLE_API_ISSUER_ID are used (CI)
  --no-notarize         sign only
EOF
}

die() { echo "macos_sign.sh: $*" >&2; exit 1; }

identity="Developer ID Application"
entitlements="$(cd "$(dirname "$0")" && pwd)/entitlements.plist"
keychain=""
notary_profile=""
notarize=1

while [ $# -gt 0 ]; do
  case "$1" in
    --identity)       identity="${2:?}"; shift 2 ;;
    --entitlements)   entitlements="${2:?}"; shift 2 ;;
    --keychain)       keychain="${2:?}"; shift 2 ;;
    --notary-profile) notary_profile="${2:?}"; shift 2 ;;
    --no-notarize)    notarize=0; shift ;;
    -h|--help)        usage; exit 0 ;;
    -*)               usage >&2; die "unknown option: $1" ;;
    *)                break ;;
  esac
done
[ $# -eq 1 ] || { usage >&2; exit 2; }
app="${1%/}"
[ -f "$app/Contents/Info.plist" ] || die "$app is not an .app bundle"
[ -f "$entitlements" ] || die "no entitlements file at $entitlements"

sign=(codesign --force --sign "$identity")
if [ "$identity" = "-" ]; then
  notarize=0
  sign+=(--timestamp=none)
else
  sign+=(--timestamp --options runtime)
fi
if [ -n "$keychain" ]; then sign+=(--keychain "$keychain"); fi

auth=()
if [ "$notarize" = 1 ]; then
  if [ -n "$notary_profile" ]; then
    auth=(--keychain-profile "$notary_profile")
  elif [ -n "${APPLE_API_KEY_PATH:-}" ] && [ -n "${APPLE_API_KEY_ID:-}" ] && [ -n "${APPLE_API_ISSUER_ID:-}" ]; then
    auth=(--key "$APPLE_API_KEY_PATH" --key-id "$APPLE_API_KEY_ID" --issuer "$APPLE_API_ISSUER_ID")
  else
    die "no notarisation credentials: pass --notary-profile, set APPLE_API_KEY_PATH/APPLE_API_KEY_ID/APPLE_API_ISSUER_ID, or use --no-notarize"
  fi
fi

# Deepest paths first: anything nested must be signed before what contains it.
by_depth() { awk -F/ '{ print NF "\t" $0 }' | sort -rn | cut -f2-; }

main_exe="$app/Contents/MacOS/$(/usr/libexec/PlistBuddy -c 'Print :CFBundleExecutable' "$app/Contents/Info.plist")"

echo "==> Signing every Mach-O file in $app"
count=0
while IFS= read -r f; do
  [ "$f" = "$main_exe" ] && continue      # signed with the bundle, below
  if file -b "$f" | grep -q 'Mach-O'; then
    "${sign[@]}" "$f"
    count=$((count + 1))
  fi
done < <(find "$app/Contents" -type f | by_depth)
echo "    $count files"

echo "==> Signing nested bundles"
while IFS= read -r b; do
  echo "    $b"
  "${sign[@]}" "$b"
done < <(find "$app/Contents" -type d \( -name '*.framework' -o -name '*.app' -o -name '*.xpc' \) | by_depth)

echo "==> Signing the app"
"${sign[@]}" --entitlements "$entitlements" "$app"
codesign --verify --deep --strict --verbose=2 "$app"
codesign --display --verbose=2 "$app" 2>&1 | grep -E '^(Authority|TeamIdentifier|Timestamp|flags)=?' || true

if [ "$notarize" = 0 ]; then
  echo "Done: $app is signed (not notarised)."
  exit 0
fi

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

echo "==> Notarising (usually a few minutes)"
ditto -c -k --keepParent "$app" "$tmp/notarize.zip"
result="$(xcrun notarytool submit "$tmp/notarize.zip" "${auth[@]}" \
            --wait --timeout 45m --output-format json)" || true
echo "$result"
field() { python3 -c 'import json,sys; print(json.loads(sys.argv[2] or "{}").get(sys.argv[1], ""))' "$1" "$result" 2>/dev/null || true; }
submission_id="$(field id)"
status="$(field status)"
if [ "$status" != "Accepted" ]; then
  if [ -n "$submission_id" ]; then
    xcrun notarytool log "$submission_id" "${auth[@]}" || true
  fi
  die "notarisation status: ${status:-no result}"
fi

echo "==> Stapling"
xcrun stapler staple "$app"
xcrun stapler validate "$app"

echo "==> Gatekeeper assessment"
spctl --assess --type execute -vvv "$app" 2>&1 | tee "$tmp/spctl.txt"
grep -q 'source=Notarized Developer ID' "$tmp/spctl.txt" \
  || die "Gatekeeper does not accept $app as notarised"

echo "Done: $app is signed, notarised and stapled."
```

### C3. `.github/workflows/python-release.yml`

Only the two macOS matrix entries change behaviour, and only when the
secrets exist: every new step is gated on `HAVE_MACOS_SIGNING`, so forks,
and this repo before B2, build exactly what they build today.

1. **Job env** -- add to the `build` job's existing `env:` (next to
   `MACOSX_DEPLOYMENT_TARGET`):
   ```yaml
      # 'true' once the Developer ID secrets exist (MACOS_SIGNING.md, Part B).
      HAVE_MACOS_SIGNING: ${{ secrets.MACOS_CERT_P12_BASE64 != '' }}
   ```
2. **`timeout-minutes: 30` → `45`** on the `build` job (notarisation queue).
3. **After the step `macOS -- every binary runs on the target macOS, and
   the bundle's signature is intact`, before `Smoke test -- selftest`**,
   insert these three steps. Their position matters: every smoke test after
   them then runs on the signed, hardened-runtime build, which is where a
   missing entitlement would show up.
   ```yaml
      - name: macOS -- signing is required but the secrets are missing
        # Set the repository variable MACOS_SIGNING_REQUIRED=true once
        # signing is live (C6), so a lost secret fails the build instead of
        # quietly shipping an unsigned app again.
        if: runner.os == 'macOS' && vars.MACOS_SIGNING_REQUIRED == 'true' && env.HAVE_MACOS_SIGNING != 'true'
        run: |
          echo "::error::MACOS_SIGNING_REQUIRED is set but the Developer ID secrets are missing (python/packaging/MACOS_SIGNING.md, Part B)"
          exit 1

      - name: macOS -- load the Developer ID certificate into a temporary keychain
        if: runner.os == 'macOS' && env.HAVE_MACOS_SIGNING == 'true'
        env:
          MACOS_CERT_P12_BASE64: ${{ secrets.MACOS_CERT_P12_BASE64 }}
          MACOS_CERT_PASSWORD: ${{ secrets.MACOS_CERT_PASSWORD }}
          APPLE_API_KEY_P8_BASE64: ${{ secrets.APPLE_API_KEY_P8_BASE64 }}
        run: |
          set -euo pipefail
          keychain="$RUNNER_TEMP/signing.keychain-db"
          keychain_password="$(uuidgen)"
          security create-keychain -p "$keychain_password" "$keychain"
          security set-keychain-settings -lut 21600 "$keychain"
          security unlock-keychain -p "$keychain_password" "$keychain"

          printf '%s' "$MACOS_CERT_P12_BASE64" | base64 --decode > "$RUNNER_TEMP/cert.p12"
          security import "$RUNNER_TEMP/cert.p12" -k "$keychain" -P "$MACOS_CERT_PASSWORD" -T /usr/bin/codesign
          rm -f "$RUNNER_TEMP/cert.p12"
          # The intermediate that issues Developer ID certificates, so
          # codesign can build the chain on a clean runner.
          curl -fsSL -o "$RUNNER_TEMP/DeveloperIDG2CA.cer" https://www.apple.com/certificateauthority/DeveloperIDG2CA.cer
          security import "$RUNNER_TEMP/DeveloperIDG2CA.cer" -k "$keychain"
          # Lets codesign use the key without a GUI "allow" prompt.
          security set-key-partition-list -S apple-tool:,apple:,codesign: -s -k "$keychain_password" "$keychain" >/dev/null
          security list-keychains -d user -s "$keychain" login.keychain-db
          security find-identity -v -p codesigning "$keychain"

          printf '%s' "$APPLE_API_KEY_P8_BASE64" | base64 --decode > "$RUNNER_TEMP/AuthKey.p8"
          echo "SIGNING_KEYCHAIN=$keychain" >> "$GITHUB_ENV"
          echo "APPLE_API_KEY_PATH=$RUNNER_TEMP/AuthKey.p8" >> "$GITHUB_ENV"

      - name: macOS -- sign with the Developer ID, notarise, staple
        if: runner.os == 'macOS' && env.HAVE_MACOS_SIGNING == 'true'
        env:
          APPLE_API_KEY_ID: ${{ secrets.APPLE_API_KEY_ID }}
          APPLE_API_ISSUER_ID: ${{ secrets.APPLE_API_ISSUER_ID }}
        run: |
          packaging/macos_sign.sh --keychain "$SIGNING_KEYCHAIN" dist/BOLDcurator.app
          echo "**${{ matrix.name }}**: signed with Developer ID, notarised, stapled." >> "$GITHUB_STEP_SUMMARY"

      - name: macOS -- no signing secrets, shipping ad hoc
        if: runner.os == 'macOS' && env.HAVE_MACOS_SIGNING != 'true'
        run: |
          echo "**${{ matrix.name }}**: ad-hoc signed only (no Developer ID secrets) -- curators will see Gatekeeper's warning." >> "$GITHUB_STEP_SUMMARY"
   ```
4. **In the step `macOS -- zip the .app on the Mac, then check the zip
   itself`**, append to its `run:` (after the existing `selftest` line), so
   the zip curators get is checked, not just the folder it came from:
   ```bash
          if [ "$HAVE_MACOS_SIGNING" = true ]; then
            xcrun stapler validate ziptest/BOLDcurator.app
            spctl --assess --type execute -vvv ziptest/BOLDcurator.app 2>&1 | tee spctl.txt
            grep -q 'source=Notarized Developer ID' spctl.txt
          fi
   ```
5. **Last step of the `build` job** (after the Windows installer upload):
   ```yaml
      - name: macOS -- remove the signing keychain and API key
        if: always() && runner.os == 'macOS' && env.HAVE_MACOS_SIGNING == 'true'
        run: |
          if [ -n "${SIGNING_KEYCHAIN:-}" ]; then security delete-keychain "$SIGNING_KEYCHAIN" || true; fi
          rm -f "${APPLE_API_KEY_PATH:-}"
   ```
6. Update the comment block at the top of the workflow and the `Build`
   step's comment to mention that macOS builds are Developer ID signed when
   the secrets exist.

Check the result parses (`python3 -c "import yaml,sys; yaml.safe_load(open(sys.argv[1]))" .github/workflows/python-release.yml`)
and re-read the diff for step ordering.

### C4. Docs

- `python/packaging/README.md`: the "Code signing" bullet under "Not
  verified anywhere yet" becomes a short "macOS code signing" section
  (what's signed, where the secrets live, a link here); the "macOS: why a
  `.app` bundle" section no longer needs its "one remaining Open Anyway"
  caveat.
- `python/PROGRESS.md`: tick 4.4 for macOS (Windows still unsigned).
- `website/index.html`: the macOS half of the "unidentified developer" FAQ
  answer becomes "current downloads open normally -- macOS asks once
  whether to open an app downloaded from the internet; click Open". Keep a
  sentence for older downloads (Done, then Privacy & Security → Open
  Anyway, or `xattr -dr com.apple.quarantine /Applications/BOLDcurator.app`).
  The step "Download and open the app" in Getting started says the same.

### C5. Test on the branch

1. Actions → **Build desktop executables** → **Run workflow** → pick the
   branch, leave *tag* empty (build only, publishes nothing).
2. ✅ The run summary has "signed with Developer ID, notarised, stapled"
   for both `macos-arm64` and `macos-x86_64`, and every smoke test passed.
3. Download the `boldcurator-macos-arm64` artifact from the run page on a
   Mac, **in a browser** (so it's quarantined as a curator's would be).
   Unzip the artifact zip, then the `boldcurator-macos-arm64.zip` inside it
   by double-clicking, and open `BOLDcurator.app`.
   ✅ Only the "downloaded from the Internet -- Open?" prompt; the app
   starts; Download from Zenodo works.
   (On an Intel Mac, do the same with `boldcurator-macos-x86_64`.)

If a smoke test failed only after signing, it's almost certainly an
entitlement (Troubleshooting); fix on the branch and re-run.

### C6. Go live

The release workflow builds **the code at the release's tag**. Tags from
before Part C was merged (V3.4 and older) don't contain `macos_sign.sh`, so
re-running the workflow on one of them can't sign anything. Go live with a
**new release** instead:

1. Merge Part C to `main`.
2. Publish a new GitHub release from `main` (e.g. `v3.5`). The `plan` job
   sees `python/` and the workflow changed and runs a full build.
3. ✅ On a Mac, download `boldcurator-macos-arm64.zip` from the website's
   button **in a browser**, unzip, open: only the ordinary Open prompt.
   Also in Terminal:
   `spctl --assess --type execute -vvv /Applications/BOLDcurator.app` →
   `source=Notarized Developer ID`.
4. Add the repository **variable** (not secret) `MACOS_SIGNING_REQUIRED` =
   `true` (Settings → Secrets and variables → Actions → Variables), so a
   future lost secret fails the build instead of shipping unsigned.
5. Tick C6 and fill in the dates in row D.

A later release where nothing under `python/` changed reuses the previous
release's zips (see `packaging/README.md`, "Releases") -- those are already
signed, so that's fine.

---

## Part D -- Upkeep

- **D1. Membership renews yearly.** If it lapses, apps already downloaded
  keep working (their notarisation tickets stay valid), but new builds
  can't be notarised -- the sign step fails. Put the renewal date in the
  status table.
- **D2. The Developer ID certificate expires after 5 years.** Apps signed
  before then keep working (the signature is timestamped). To renew: repeat
  A3 and B1, replace `MACOS_CERT_P12_BASE64` and `MACOS_CERT_PASSWORD`,
  and run C5's build-only check. The API key doesn't expire.
- **D3. If a secret leaks** (committed, pasted in a chat or issue, a
  laptop lost): revoke the certificate (developer.apple.com → Certificates)
  and the API key (App Store Connect → Integrations), then repeat A3, A4
  and B with new ones. Revoking the certificate can make builds it already
  signed stop opening -- rebuild and re-release straight away.

## Troubleshooting

| Symptom | Likely cause and fix |
|---|---|
| `find-identity` shows **0 valid identities** after A3 | The `.cer` was imported without its private key (the request was made on a different Mac), or into the wrong keychain. Import the `.p12` from whoever made the request, or redo A3 on this Mac. |
| codesign: **ambiguous** identity | More than one Developer ID Application certificate in the keychain: pass `--identity "Developer ID Application: Name (TEAMID)"` or the SHA-1 from `find-identity`. |
| codesign: **unable to build chain to self-signed root** | The Developer ID G2 intermediate is missing: download `DeveloperIDG2CA.cer` from apple.com/certificateauthority and double-click it (C3 already imports it in CI). |
| codesign hangs, or **errSecInternalComponent** in CI | The keychain is locked or the key's partition list wasn't set: check C3's `unlock-keychain` and `set-key-partition-list` lines. |
| Notary log: **The binary is not signed with a valid Developer ID certificate** | A file was missed or re-signed ad hoc afterwards; the log names it. Check it's a Mach-O file `find` reaches (not behind a symlink), and that nothing modifies the app after `macos_sign.sh`. |
| Notary log: **The signature does not include a secure timestamp** | A `codesign` call lacked `--timestamp` (or Apple's timestamp server was unreachable -- re-run). |
| Notary log: **The executable does not have the hardened runtime enabled** | A `codesign` call lacked `--options runtime`. |
| Notary log complains about a **`.a` static library** or other build-only file | Not needed at runtime (numpy ships a few): delete them from the bundle before signing -- e.g. `find "$app/Contents" -name '*.a' -delete` at the top of `macos_sign.sh` -- and note why in `packaging/README.md`. Re-run C5 to prove nothing needed it. |
| Notary log: **built with an SDK older than 10.9** | A very old wheel. `packaging/macos_deployment_target.py` repins wheels; pin a newer version of the package the log names. |
| Signed app **quits straight away**, or `selftest` fails only after signing | Hardened runtime. Look in `~/.boldcurator/boldcurator.log` (Finder launch) or the Terminal output. "code signature … not valid for use in process" / "library load disallowed by system policy" → add `com.apple.security.cs.disable-library-validation`. A crash in `ctypes`/libffi → the C1 entitlement is missing. Add entitlements one at a time and re-test. |
| Still "Apple could not verify…" after C6 | The zip wasn't the signed one (check the run summary says signed), or it wasn't stapled (`xcrun stapler validate`). A copy approved earlier via Open Anyway can also cache the old result: delete it and re-download. |
| Build summary says **ad-hoc** | Secrets missing, misspelt, or the workflow ran from a fork. Check the five names in B2. |
