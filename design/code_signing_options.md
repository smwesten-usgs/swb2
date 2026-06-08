# Code Signing Options for SWB2

**Date:** June 2026

---

## Option 1: Signed Git Tags (Recommended — Do Now)

Cryptographic proof that release tags were created by you and are untampered.

**Setup:**
```bash
# Generate GPG key (if you don't have one)
gpg --full-generate-key

# Tell git to use it
git config --global user.signingkey <KEY_ID>

# Sign a tag
git tag -s v2.0.0 -m "Release 2.0.0"

# Optionally sign all commits going forward
git config --global commit.gpgsign true
```

**External steps:**
- Upload public GPG key to GitHub (Settings → SSH and GPG keys)
- Upload to code.usgs.gov (User Settings → GPG Keys)

**Effort:** ~30 minutes. No ongoing cost.

**Why:** Important for USGS immutable release tags. Proves provenance.

---

## Option 2: SHA-256 Checksums (Do at Each Release)

Publish hashes so users can verify downloaded binaries are intact.

**Process:**
```bash
sha256sum swb2.exe swbstats2.exe > SHA256SUMS
```

Include `SHA256SUMS` file with each release. Document verification in README.

**Effort:** Trivial. No external dependencies.

---

## Option 3: Signed Windows Binaries (Future — Requires Certificate)

Applies a code signing certificate to `swb2.exe` so Windows doesn't show "unknown publisher" / SmartScreen warnings.

**Repo changes:**
- Post-build script calling `signtool.exe`
- Certificate thumbprint stored in build config (never the private key)

**Signing command:**
```
signtool sign /sha1 <thumbprint> /t http://timestamp.digicert.com /fd sha256 swb2.exe
```

**External process (the bottleneck):**
- Obtain a code signing certificate:
  - Option A: USGS/DOI enterprise PKI through OACIO
  - Option B: Commercial CA (DigiCert, Sectigo) — ~$200-500/year, organizational validation
  - Option C: Azure Trusted Signing (if DOI has an account)
- Timestamp signatures so they remain valid after cert expiration

**Effort:** Certificate procurement takes days to weeks. Signing itself is one build step.

**No Fortran code changes required** — only build system/release process changes.

---

## Priority

| Level | When | Why |
|-------|------|-----|
| Signed git tags | Now | Free, proves provenance for immutable release tags |
| SHA-256 checksums | Each release | Zero cost, users can verify downloads |
| Signed Windows binaries | When CI is set up | Eliminates SmartScreen; start certificate conversation with USGS IT early |
