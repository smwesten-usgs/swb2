# Best practices for SWB2 development

## Commit messages

From release version 2.0.1, we will adopt commit messages based on the "Conventional Commits" specification (https://www.conventionalcommits.org/en/v1.0.0/). Commit messages will be prefixed with one of the following:

| prefix  | example of change associated with commit                                          | 
|---------|-----------------------------------------------------------------------------------|
| fix     | patch a bug in your codebase (this correlates with PATCH in semantic versioning). |
| feat    | introduces a new feature to the codebase (this correlates with MINOR in semantic versioning). |
| chore   | miscellaneous: fix git mistake, alter line endings, etc. | 
| docs    | update online documentation files or README.md files |
| style   | minor changes involving indentation, comment placement, etc. |
| refactor| code changes designed to enhance reliability or understandabilty |
| perf    | code changes associated with performance testing or enhancements |
| test    | changes to unit, integration, or other tests or testing frameworks |
| ci      | modifications to continuous integration config files |
| build   | changes to CMake files, build configuration scripts, or other build-related mods |

In addition, a change to the code that signifies a major change to code will be identified with a `BREAKING CHANGE` label in the commit message:

*BREAKING CHANGE*: a commit that has the text `BREAKING CHANGE`: at the beginning of its optional body or footer section introduces a breaking API change (correlating with MAJOR in semantic versioning). A breaking change can be part of commits of any type. e.g., a fix:, feat: & chore: types would all be valid, in addition to any other type.

## Version numbering

We will be following semantic version numbering (https://semver.org/). This amounts to a three-component version number with parts identified as follows:

Given a version number MAJOR.MINOR.PATCH, increment the:

MAJOR version when breaking changes are made,
MINOR version when functionality is added in a backwards compatible manner, and
PATCH version when backwards compatible bug fixes are implemented.
