# SWB2 Release Process Guide

**Last updated:** 2026-08-18  
**Applies to:** Releases on the three-repo platform (code.usgs.gov GitLab, github.com/smwesten-usgs, github.com/DOI-USGS)

---

## Overview

Releasing a new version of SWB2 requires coordinating:
1. A correct `code.json` (with dual-object array structure)
2. An approved DISCLAIMER in the tagged commit
3. A git tag with **no `v` prefix** (e.g., `2.4.1` not `v2.4.1`)
4. A GitLab Release with binaries attached
5. A DOI pointing to the release

The USGS code.usgs.gov validator checks that:
- A git ref (tag) exists matching the `"version"` string in `code.json` exactly
- The URLs in `code.json` for the release object point to the tag name (not `main`)
- `LICENSE.md` and `DISCLAIMER.md` are accessible at those URLs

---

## Prerequisites

Before starting, ensure:
- [ ] Code is ready (all tests pass, changelog updated)
- [ ] `meson.build` has the correct version number (e.g., `version: '2.5.0'`)
- [ ] You have your GitLab personal access token
- [ ] You know the GitLab project ID: **17912**

---

## Step-by-Step Procedure

### Step 1: Prepare `code.json` on `main`

The `code.json` must be an **array with two objects**:

```json
[
  {
    "version": "main",
    "status": "Development",
    "...URLs point to main..."
  },
  {
    "version": "X.Y.Z",
    "status": "Production",
    "...URLs point to X.Y.Z..."
  }
]
```

**Critical URL patterns for the release object:**
```
permissions.licenses[0].URL  → https://code.usgs.gov/water/soil-water-balance/swb2/-/raw/X.Y.Z/LICENSE.md
downloadURL                  → https://code.usgs.gov/water/soil-water-balance/swb2/-/archive/X.Y.Z/swb2-X.Y.Z.zip
disclaimerURL                → https://code.usgs.gov/water/soil-water-balance/swb2/-/raw/X.Y.Z/DISCLAIMER.md
```

⚠️ **GOTCHA:** Do NOT use `main` in the release object URLs. Do NOT use a `v` prefix in the version string.

Update `metadataLastUpdated` to today's date (YYYY-MM-DD format, two-digit month/day).

### Step 2: Update disclaimer files to APPROVED language

Three files need the approved disclaimer:
- `DISCLAIMER.md`
- `README.md` (the disclaimer section near the bottom)
- `src/disclaimers.F90` (both subroutines)

**Approved text:**
> This software has been approved for release by the U.S. Geological Survey (USGS). Although the software has been subjected to rigorous review, the USGS reserves the right to update the software as needed pursuant to further analysis and review. No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. Furthermore, the software is released on condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from its authorized or unauthorized use.

### Step 3: Commit the release changes

```bash
git add code.json DISCLAIMER.md README.md src/disclaimers.F90 meson.build
git commit -m "release: prepare X.Y.Z with approved disclaimer and updated code.json"
```

⚠️ **GOTCHA:** Get ALL release files into ONE commit. This is the commit you'll tag. If you forget a file, you'll have to delete the tag and redo it.

### Step 4: Tag it (NO `v` prefix)

```bash
git tag X.Y.Z
```

⚠️ **GOTCHA:** The tag name must be `X.Y.Z`, NOT `vX.Y.Z`. The validator looks for a git ref matching the version string in `code.json` exactly.

### Step 5: Revert disclaimer to provisional on `main`

```bash
git checkout HEAD~1 -- DISCLAIMER.md README.md src/disclaimers.F90
git commit -m "revert disclaimer to provisional on main for ongoing development"
```

This keeps `main` marked as preliminary while the tag preserves the approved state.

### Step 6: Push to GitLab

```bash
git push gitlab main
git push gitlab X.Y.Z
```

### Step 7: Push tag to GitHub

```bash
git push origin X.Y.Z
git push upstream X.Y.Z
```

(`origin` = smwesten-usgs/swb2, `upstream` = DOI-USGS/swb2)

### Step 8: Create GitLab Release

Go to: https://code.usgs.gov/water/soil-water-balance/swb2/-/releases/new

- Select tag: `X.Y.Z`
- Add title and release notes

### Step 9: Upload binaries

**Zip the binaries:**
```powershell
Compress-Archive -Path E:\projects\swb_development\git\swb2\bin\* -DestinationPath E:\projects\swb_development\git\swb2\swb2-X.Y.Z-win64.zip
```

**Upload to package registry:**
```bash
curl --header "PRIVATE-TOKEN: YOUR_TOKEN" ^
  --upload-file swb2-X.Y.Z-win64.zip ^
  "https://code.usgs.gov/api/v4/projects/17912/packages/generic/swb2/X.Y.Z/swb2-X.Y.Z-win64.zip"
```

**Link to the release:**
```bash
curl --header "PRIVATE-TOKEN: YOUR_TOKEN" ^
  --header "Content-Type: application/json" ^
  --data "{\"name\": \"swb2-X.Y.Z-win64.zip\", \"url\": \"https://code.usgs.gov/api/v4/projects/17912/packages/generic/swb2/X.Y.Z/swb2-X.Y.Z-win64.zip\", \"link_type\": \"package\"}" ^
  --request POST ^
  "https://code.usgs.gov/api/v4/projects/17912/releases/X.Y.Z/assets/links"
```

### Step 10: Update the DOI

Go to: https://www1.usgs.gov/csas/doi/

Update the target URL to:
`https://code.usgs.gov/water/soil-water-balance/swb2/-/releases/X.Y.Z`

---

## Recovery: If You Mess Up

### Forgot a file in the tagged commit

```bash
# Delete remote tag (must delete GitLab Release first via UI)
git push gitlab :refs/tags/X.Y.Z

# Delete local tag
git tag -d X.Y.Z

# Fix the file(s), amend the commit
git add <forgotten-file>
git commit --amend --no-edit

# Re-tag and push
git tag X.Y.Z
git push gitlab main --force-with-lease
git push gitlab X.Y.Z
```

### Tag has wrong name (e.g., `v2.4.1` instead of `2.4.1`)

```bash
git push gitlab :refs/tags/v2.4.1    # delete wrong remote tag
git tag -d v2.4.1                     # delete wrong local tag
git tag 2.4.1                         # create correct tag
git push gitlab 2.4.1                 # push correct tag
```

### URLs in code.json point to `main` instead of version

This requires a new commit (you can't fix committed content in place). Delete the tag, fix `code.json`, recommit, re-tag.

---

## Future Improvement: Build-Time Disclaimer Selection

Currently, `src/disclaimers.F90` is manually edited to swap between provisional and approved text for each release. This is error-prone and accounts for most of the release pain.

### Proposed Solution: Meson build option

Add a `release_type` option to `meson_options.txt`:

```meson
option('release_type',
  type: 'combo',
  choices: ['provisional', 'approved'],
  value: 'provisional',
  description: 'Controls which USGS disclaimer is compiled into the binary'
)
```

Then in `meson.build`, pass it as a preprocessor define:

```meson
release_type = get_option('release_type')
if release_type == 'approved'
  add_global_arguments('-DAPPROVED_RELEASE', language: 'fortran')
endif
```

And rewrite `src/disclaimers.F90` to use preprocessor conditionals:

```fortran
module disclaimers

  use constants_and_conversions, only  : TRUE
  use logfiles, only                   : LOGS, LOG_ALL
  implicit none

contains

  subroutine write_disclaimer()

    write(*,"(/,a)") ' Disclaimer'
    write(*,"(a,/)") '============'

#ifdef APPROVED_RELEASE
    write(*,"(a)") 'This software has been approved for release by the U.S. Geological Survey (USGS). Although'
    write(*,"(a)") 'the software has been subjected to rigorous review, the USGS reserves the right to update'
    write(*,"(a)") 'the software as needed pursuant to further analysis and review. No warranty, expressed'
    write(*,"(a)") 'or implied, is made by the USGS or the U.S. Government as to the functionality of the'
    write(*,"(a)") 'software and related material nor shall the fact of release constitute any such warranty.'
    write(*,"(a)") 'Furthermore, the software is released on condition that neither the USGS nor the'
    write(*,"(a)") 'U.S. Government shall be held liable for any damages resulting from its authorized'
    write(*,"(a,/)") 'or unauthorized use.'
#else
    write(*,"(a)") 'This software is preliminary or provisional and is subject to revision. It is'
    write(*,"(a)") 'being provided to meet the need for timely best science. The software has not'
    write(*,"(a)") 'received final approval by the U.S. Geological Survey (USGS). No warranty,'
    write(*,"(a)") 'expressed or implied, is made by the USGS or the U.S. Government as to the'
    write(*,"(a)") 'functionality of the software and related material nor shall the fact of release'
    write(*,"(a)") 'constitute any such warranty. The software is provided on the condition that'
    write(*,"(a)") 'neither the USGS nor the U.S. Government shall be held liable for any damages'
    write(*,"(a,/)") 'resulting from the authorized or unauthorized use of the software.'
#endif

  end subroutine write_disclaimer

!--------------------------------------------------------------------------------------------------

  subroutine log_disclaimer()

    call LOGS%write( sMessage=' Disclaimer', lEcho=TRUE)
    call LOGS%write( sMessage='============', lEcho=TRUE, iLinesAfter=1)

#ifdef APPROVED_RELEASE
    call LOGS%write( sMessage='This software has been approved for release by the U.S. Geological Survey (USGS). Although', lEcho=TRUE)
    call LOGS%write( sMessage='the software has been subjected to rigorous review, the USGS reserves the right to update', lEcho=TRUE)
    call LOGS%write( sMessage='the software as needed pursuant to further analysis and review. No warranty, expressed', lEcho=TRUE)
    call LOGS%write( sMessage='or implied, is made by the USGS or the U.S. Government as to the functionality of the', lEcho=TRUE)
    call LOGS%write( sMessage='software and related material nor shall the fact of release constitute any such warranty.', lEcho=TRUE)
    call LOGS%write( sMessage='Furthermore, the software is released on condition that neither the USGS nor the', lEcho=TRUE)
    call LOGS%write( sMessage='U.S. Government shall be held liable for any damages resulting from its authorized', lEcho=TRUE)
    call LOGS%write( sMessage='or unauthorized use.', lEcho=TRUE, iLinesAfter=1)
#else
    call LOGS%write( sMessage='This software is preliminary or provisional and is subject to revision. It is', lEcho=TRUE)
    call LOGS%write( sMessage='being provided to meet the need for timely best science. The software has not', lEcho=TRUE)
    call LOGS%write( sMessage='received final approval by the U.S. Geological Survey (USGS). No warranty,', lEcho=TRUE)
    call LOGS%write( sMessage='expressed or implied, is made by the USGS or the U.S. Government as to the', lEcho=TRUE)
    call LOGS%write( sMessage='functionality of the software and related material nor shall the fact of release', lEcho=TRUE)
    call LOGS%write( sMessage='constitute any such warranty. The software is provided on the condition that', lEcho=TRUE)
    call LOGS%write( sMessage='neither the USGS nor the U.S. Government shall be held liable for any damages', lEcho=TRUE)
    call LOGS%write( sMessage='resulting from the authorized or unauthorized use of the software.', lEcho=TRUE, iLinesAfter=1)
#endif

  end subroutine log_disclaimer

end module disclaimers
```

**Usage:**
- Normal development builds: `meson setup builddir` (defaults to provisional)
- Official release builds: `meson setup builddir -Drelease_type=approved`

**Benefits:**
- `src/disclaimers.F90` never needs manual editing
- The source file on `main` always contains BOTH disclaimers — no commenting/uncommenting
- Release builds automatically get the approved text
- Eliminates the most common source of release errors

**Note:** The file would need to be renamed to `disclaimers.F90` → `disclaimers.F90` (keeping `.F90` uppercase extension) to ensure the Fortran preprocessor runs. Most compilers treat uppercase `.F90` as "preprocess first" — verify this works with your compiler setup.

### Remaining manual steps even with this improvement

You still need to manually update `DISCLAIMER.md` and `README.md` for the tagged commit. These are plain text files that the validator reads via URL — they can't be preprocessor-driven. However, you could add a small script (e.g., `scripts/prepare_release.py`) that:
1. Swaps DISCLAIMER.md to approved
2. Swaps the disclaimer section in README.md to approved
3. Updates `code.json` version URLs
4. Stages everything

Then your release workflow becomes:
```bash
python scripts/prepare_release.py 2.5.0
git commit -m "release: prepare 2.5.0"
git tag 2.5.0
python scripts/revert_to_provisional.py
git commit -m "revert to provisional on main"
```

---

## Remotes Reference

| Remote | URL | Purpose |
|--------|-----|---------|
| `gitlab` | https://code.usgs.gov/water/soil-water-balance/swb2.git | Official USGS release (code.json validated here) |
| `origin` | git@github.com:smwesten-usgs/swb2.git | Personal GitHub fork |
| `upstream` | git@github.com:DOI-USGS/swb2.git | DOI-USGS organization mirror |

## Key Identifiers

| Item | Value |
|------|-------|
| GitLab project ID | 17912 |
| Group hierarchy | water/soil-water-balance/swb2 |
| DOI management | https://www1.usgs.gov/csas/doi/ |
| Release page pattern | https://code.usgs.gov/water/soil-water-balance/swb2/-/releases/X.Y.Z |
