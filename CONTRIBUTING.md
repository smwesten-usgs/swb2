# Contributing to SWB2

## Getting Started

Active development happens at [smwesten-usgs/swb2](https://github.com/smwesten-usgs/swb2). To contribute:

1. Fork the repository on GitHub
2. Clone your fork locally
3. Create a feature branch from `main`
4. Make your changes, commit, push to your fork
5. Open a pull request against `main` on smwesten-usgs/swb2

```bash
git clone git@github.com:YOUR-USERNAME/swb2.git
cd swb2
git remote add upstream git@github.com:smwesten-usgs/swb2.git
git fetch upstream
git checkout -b feature/my-change upstream/main
```

## Branching

All changes reach `main` via pull request. Branch names should be prefixed:

- `feature/<description>` — new functionality
- `bugfix/<description>` — bug fixes
- `hotfix/<description>` — urgent fixes to released code
- `release/<version>` — release preparation

## Building and Testing

> **Note:** The build system is under active cleanup. Meson is the supported
> build system going forward. If you encounter issues building from source,
> please open an issue describing your platform and the error output.

```bash
# Configure
meson setup builddir

# Compile
meson compile -C builddir

# Run tests
meson test -C builddir
```

Requires: gfortran ≥10 (or Intel ifx), Meson ≥1.6.0, Ninja, and NetCDF-C (with HDF5 and zlib). See `design/developer_quickstart.md` for platform-specific setup details.

Override NetCDF location: `meson setup builddir -Dnetcdf_root=/path/to/netcdf`

## Commit Messages

We follow [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/):

| Prefix | Use |
|--------|-----|
| `fix:` | Bug fix (PATCH version bump) |
| `feat:` | New feature (MINOR version bump) |
| `refactor:` | Code restructuring, no behavior change |
| `perf:` | Performance improvement |
| `test:` | Adding or modifying tests |
| `docs:` | Documentation changes |
| `build:` | Build system or dependency changes |
| `ci:` | CI configuration changes |
| `style:` | Formatting, indentation, comments |
| `chore:` | Miscellaneous (gitignore, line endings, etc.) |

For breaking changes, include `BREAKING CHANGE:` in the commit body. A breaking change can accompany any prefix type.

## Version Numbering

We follow [Semantic Versioning](https://semver.org/): `MAJOR.MINOR.PATCH`

- **MAJOR** — breaking changes (incompatible control file syntax, removed features)
- **MINOR** — new functionality, backwards compatible
- **PATCH** — backwards compatible bug fixes

## Code Conventions

- All types use `iso_c_binding` kinds: `integer(c_int)`, `real(c_float)`, `real(c_double)`, `logical(c_bool)`
- Boolean constants: `TRUE` and `FALSE` from `constants_and_conversions`
- Error handling: `call die(message, __FILE__, __LINE__)`
- Logging: `LOGS%write(message)`
- String operations: use `fstring` module operators (`.contains.`, `.strequal.`)

## Questions or Issues

File issues at https://github.com/smwesten-usgs/swb2/issues.
