# Official Release v2.4.0 — Post-Action Summary

**Date:** August 7, 2026  
**Version released:** v2.4.0  
**IPDS:** IP-190898  
**DOI:** (activate via USGS Asset Identifier Service → points to GitLab release URL)  
**GitLab Release URL:** https://code.usgs.gov/water/soil-water-balance/swb2/-/releases/v2.4.0

---

## Timeline

| Date | Milestone |
|------|-----------|
| June 2, 2026 | Provisional release prepared (code.json updated, repo public on code.usgs.gov) |
| June 2026 | Provisional release issue submitted on CHS GitLab |
| June–August 2026 | Reviews completed (admin security, code, domain), bureau approval obtained |
| August 7, 2026 | Official release tag `v2.4.0` created and pushed to code.usgs.gov |
| August 7, 2026 | GitLab Release created from tag |
| August 7, 2026 | DOI published to DataCite |
| August 7, 2026 | Official Release issue submitted on CHS GitLab |
| TBD | IPDS dissemination step completed |

---

## What We Did (The Git Commands)

Starting point: commit `b128d6cc` was already tagged as `v2.4.0-rc0` and pushed to `gitlab/main`.

```bash
# 1. Create a release branch from the frozen RC commit
git checkout -b release/v2.4.0 v2.4.0-rc0

# 2. Edit DISCLAIMER.md (change to approved wording) and code.json (status, version, date)

# 3. Stage and commit — DO NOT FORGET git add or -a flag!
git add DISCLAIMER.md code.json
git commit -m "release: update disclaimer to approved and modify code.json for v2.4.0"

# 4. Tag the new commit (one commit ahead of v2.4.0-rc0)
git tag -a v2.4.0 -m "Release v2.4.0: Bureau-approved official release"

# 5. Verify the tag points to a different commit than rc0
git log --oneline v2.4.0-rc0..v2.4.0
# Should show one commit

# 6. Push the tag to code.usgs.gov (carries the new commit with it)
git push gitlab v2.4.0

# 7. Create GitLab Release via web UI (Code → Tags → v2.4.0 → Create release)

# 8. Clean up local branch
git checkout main
git branch -d release/v2.4.0
```

---

## Gotchas and Lessons Learned

### 1. `git commit -m "..."` does NOT stage tracked modified files

This is the single biggest gotcha. If you edit DISCLAIMER.md and code.json but only run `git commit -m "message"`, nothing gets committed. You need either:
- `git add DISCLAIMER.md code.json` then `git commit -m "..."`
- `git commit -am "..."` (the `-a` flag stages all tracked modifications)

Without this, the tag ends up pointing to the same content as the RC tag. The fix requires deleting the tag locally and on the remote, then redoing:

```bash
git tag -d v2.4.0
git push gitlab --delete v2.4.0
# ... fix, recommit, retag, repush
```

### 2. Pushing a tag pushes its commit (if the remote doesn't have it)

`git push gitlab v2.4.0` pushes the tag AND the commit it points to (plus any ancestors the remote lacks). You do NOT need to push the branch. The release branch is just local scaffolding.

### 3. The release branch is never merged back to main

`main` keeps the **provisional** disclaimer permanently. Only the tagged commit has the approved disclaimer. This is intentional USGS policy — ongoing development is always "provisional."

### 4. The DOI points to the GitLab Release (not the tag, not the branch)

URL format: `https://code.usgs.gov/water/soil-water-balance/swb2/-/releases/v2.4.0`

The Release is preferred because its notes are editable (you can add errata, point to newer versions, etc.) while the tag itself is immutable.

### 5. DOI metadata is editable after publication

"Recommended" fields (dates, contributors, format type, related identifiers) can all be added later. Don't let optional metadata block publication.

### 6. The CHS admin process is mostly automated validation

After submitting the Official Release issue, an admin runs a validator that checks:
- code.json URLs resolve
- Required files exist at repo root
- Tag exists
- DISCLAIMER.md on the tag has approved wording

Most rejections are code.json URL issues.

---

## Files Changed for the Release (on the tag only)

### DISCLAIMER.md — changed from provisional to approved

**Provisional (stays on `main`):**
> This software is preliminary or provisional and is subject to revision...

**Approved (on tag `v2.4.0` only):**
> This software has been approved for release by the U.S. Geological Survey (USGS). Although the software has been subjected to rigorous review, the USGS reserves the right to update the software as needed pursuant to further analysis and review. No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. Furthermore, the software is released on condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from its authorized or unauthorized use.

### code.json — key changes

| Field | Before | After |
|-------|--------|-------|
| `status` | `"Development"` | `"Production"` |
| `version` | `"2.4.1"` (or whatever was there) | `"2.4.0"` |
| `date.metadataLastUpdated` | `"2026-06-02"` | `"2026-08-07"` |

---

## Checklist for Next Official Release

Use this as a quick-reference for the next time (v2.5.0, v3.0.0, etc.):

- [ ] Code is stable; all intended changes merged to `main`
- [ ] Tag an RC: `git tag -a vX.Y.Z-rc0 -m "Release candidate" && git push gitlab vX.Y.Z-rc0`
- [ ] Complete reviews (code, domain, security) and document in IPDS
- [ ] Obtain bureau approval (Center Director sign-off in IPDS)
- [ ] Reserve DOI via USGS Asset Identifier Service
- [ ] Create release branch: `git checkout -b release/vX.Y.Z vX.Y.Z-rc0`
- [ ] Update DISCLAIMER.md to approved wording
- [ ] Update code.json: version, status → "Production", metadataLastUpdated
- [ ] **`git add` the files** then commit
- [ ] Tag: `git tag -a vX.Y.Z -m "Release vX.Y.Z: description"`
- [ ] Verify: `git log --oneline vX.Y.Z-rc0..vX.Y.Z` (should show 1 commit)
- [ ] Push tag: `git push gitlab vX.Y.Z`
- [ ] Create GitLab Release from tag (via web UI)
- [ ] Activate DOI pointing to release URL
- [ ] Submit Official Release issue on code.chs.usgs.gov/software/software-management
- [ ] Sync to GitHub: `git push origin vX.Y.Z && git push upstream vX.Y.Z`
- [ ] Complete IPDS dissemination step
- [ ] Delete local release branch: `git branch -d release/vX.Y.Z`
- [ ] On `main`: revert code.json status back to "Development", bump version to next dev version

---

## Policy Confusion / Institutional Notes

The USGS has multiple overlapping guidance documents that use inconsistent terminology:

- **SM 502.11 (May 2026)** — The authoritative policy. Defines "Software Project" (not citable) vs "Scientific Software Information Product" (citable, requires DOI).
- **CHS GitLab templates** — Use older "provisional" / "official" terminology. Operational gates for making repos public.
- **UMID SOP v1.1** — Center-level interpretation. Requires three reviews (code, domain, security) for all software. May exceed bureau policy.
- **USGS Git course (cdi.code-pages.usgs.gov)** — Pedagogical, demonstrates a more complex workflow than needed for simple releases.

See `usgs_requirements/policy_analysis__SM502.11_vs_operational_guidance.md` for the full reconciliation.

---

## Reference Links

- CHS GitLab Software Management (submit issues): https://code.chs.usgs.gov/software/software-management/-/issues
- USGS Asset Identifier Service (DOIs): https://www1.usgs.gov/csas/doi/
- IPDS: https://ipds.usgs.gov
- USGS Git and Software Release Course: https://cdi.code-pages.usgs.gov/usgs-git-and-software-release-course/publication.html
- SM 502.11: https://www.usgs.gov/survey-manual/sm-50211-fundamental-science-practices-review-approval-and-release-scientific
- FSP Disclaimer Guidance: https://www.usgs.gov/about/organization/science-support/office-science-quality-and-integrity/fundamental-science-5
