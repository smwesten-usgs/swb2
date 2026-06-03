# SWB2 Git Workflow

## Remote Layout

```
origin    → smwesten-usgs/swb2 (GitHub)    Active development + CI
upstream  → DOI-USGS/swb2 (GitHub)         Official org presence, synced from origin
gitlab    → code.usgs.gov                  Authoritative USGS archive, synced at release
```

```bash
git remote -v
# Expected:
origin    git@github.com:smwesten-usgs/swb2.git (fetch/push)
upstream  git@github.com:DOI-USGS/swb2.git (fetch/push)
gitlab    https://code.usgs.gov/water/soil-water-balance/swb2.git (fetch/push)
```

Note: code.usgs.gov requires HTTPS + Personal Access Token (SSH access blocked as of June 2026).

## Rationale

- **smwesten-usgs** stays primary because it has unrestricted GitHub Actions for CI (Fortran toolchains, netCDF/HDF5 deps, platform matrix). DOI-USGS org repos have more restrictive CI policies.
- **DOI-USGS** is the recognized USGS org on GitHub; kept in sync so the code is discoverable in the official catalog.
- **code.usgs.gov** is the authoritative archive per USGS policy; pushed to at release milestones.

Long-term, if DOI-USGS CI restrictions loosen, the primary can flip to DOI-USGS with minimal disruption since both repos mirror the same content.

## Branching Strategy

- **`main`** is the stable branch. All changes reach `main` via pull request.
- Use feature branches for all work, even small changes. This keeps the `main` history clear about when and why changes landed.

Branch naming:
```
feature/<short-description>
bugfix/<short-description>
hotfix/<short-description>
release/<version>
```

## Daily Development Workflow

### Start a feature

```bash
git fetch origin
git checkout -b feature/my-change origin/main
```

### Work and commit

```bash
git add <files>
git commit -m "Descriptive message"
git push origin feature/my-change
```

### Merge via pull request

Open a PR on smwesten-usgs/swb2: `feature/my-change` → `main`. Review the diff (even as the sole reviewer — it forces a moment of reflection), then merge. Delete the feature branch after merge.

Via CLI:
```bash
gh pr create --base main --title "Short description" --body "Details"
# After review:
gh pr merge --squash --delete-branch
```

### Update local main

```bash
git checkout main
git pull origin main
```

## Syncing to DOI-USGS and code.usgs.gov

### After merging to main (or periodically)

```bash
git push upstream main --tags
```

### At release milestones

```bash
git push upstream main --tags
git push gitlab main --tags
```

Or as a single alias:
```bash
git config alias.release-sync "!git push upstream main --tags && git push gitlab main --tags"
# Then:
git release-sync
```

### Automated sync (optional future CI)

A GitHub Actions workflow on smwesten-usgs can auto-push to DOI-USGS on successful main builds:

```yaml
# .github/workflows/sync-doi-usgs.yml
on:
  push:
    branches: [main]
    tags: ['v*']
jobs:
  sync:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      - run: |
          git remote add doi https://x-access-token:${{ secrets.DOI_USGS_TOKEN }}@github.com/DOI-USGS/swb2.git
          git push doi main --tags
```

## Tagging Releases

```bash
git tag -a v2.1.0 -m "Release 2.1.0: brief description"
git push origin --tags
git release-sync
```

## Summary

| Action | Where |
|--------|-------|
| Daily development | smwesten-usgs/swb2 (feature branches → PR → main) |
| CI/testing | smwesten-usgs/swb2 (GitHub Actions) |
| Issue tracking | smwesten-usgs/swb2 |
| Org visibility / code.gov | DOI-USGS/swb2 (synced) |
| Official USGS archive | code.usgs.gov (synced at release) |
| Provisional/official releases | Submitted against code.usgs.gov repo |
