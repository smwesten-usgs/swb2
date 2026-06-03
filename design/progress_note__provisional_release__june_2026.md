# Progress Note: Provisional Release — June 2, 2026

## Summary

SWB2 is ready for provisional release. All substantive requirements have been met. Only two mechanical steps remain.

## Completed Steps

- **Center director approval** — obtained
- **Administrative security review** — completed
- **Repository hosting** — code.usgs.gov/water/soil-water-balance/swb2 (authoritative), DOI-USGS/swb2 on GitHub (mirror)
- **Required files present:**
  - `README.md` — project overview, purpose, links to TM6-A59 docs
  - `DISCLAIMER.md` — uses provisional wording
  - `LICENSE.md` — CC0/public domain
  - `code.json` — updated 2026-06-02

## code.json Updates (2026-06-02)

Key changes from the 2018 version:

| Field | Old | New | Rationale |
|-------|-----|-----|-----------|
| `name` | "Soil-Water-Balance version 2.0" | "swb2" | Matches repo name |
| `status` | "APPROVED" | "Development" | Correct code.gov schema value for provisional |
| URLs | `blob/swb2/...` (old branch pattern) | `/-/raw/main/...` | Repo was split; old paths are dead |
| `repositoryURL` | `.../tree/swb2` | `.../swb2.git` | Should be the clone URL per schema |
| `downloadURL` | old archive path | `/-/archive/main/swb2-main.zip` | Correct GitLab archive URL |
| `metadataLastUpdated` | 2018-09-06 | 2026-06-02 | Today |
| `tags` | 4 tags | 6 (added hydrology, water resources) | Discoverability |

The repo uses a single `code.json` pointing to code.usgs.gov (the authoritative archive). The same file is pushed to both remotes. If DOI-USGS GitHub ever requires its own variant, the only differences would be URL patterns (`/blob/main/` instead of `/-/raw/main/`, etc.).

## Remaining Steps

### 1. Commit and push the updated code.json

```bash
git add code.json
git commit -m "Update code.json for provisional release"
git push gitlab main
git push upstream main
```

### 2. Submit Provisional Release issue

- Go to the CHS GitLab software management site (https://code.chs.usgs.gov/software/software-management)
- Open a new issue using the **Provisional Release** template
- Reference repository: `https://code.usgs.gov/water/soil-water-balance/swb2`
- Note: center director approval obtained, admin security review completed

## Reference

- Software Release Checklist (PDF): https://code.chs.usgs.gov/software/software-management/-/raw/main/software-release-checklist.pdf
- USGS Software Management: https://www.usgs.gov/products/software/software-management
- SM 502.11 (FSP): https://www.usgs.gov/office-of-science-quality-and-integrity/fundamental-science-practices-fsp-procedures-review-and
