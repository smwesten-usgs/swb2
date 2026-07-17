# Feature Consideration: Self-Documenting Control File Directives

**Date:** 2026-07-17  
**Status:** Proposed  
**Priority:** Low-Medium — quality-of-life improvement for developers and users  

---

## Motivation

SWB2 supports dozens of control file directives with various valid options. Even the primary developer occasionally forgets the full set. There's no built-in way to ask the code "what do you understand?" — users must consult external documentation (which may be stale) or read the source.

A `--list-directives` (or `--help-directives`) CLI flag that prints all valid directives, their options, and a brief description would:

- Serve as always-accurate documentation (lives in the code, can't go stale)
- Help users write control files without external references
- Help developers verify that new directives are properly registered
- Reduce support burden

---

## Option 1: Self-Documenting String Table (Recommended)

### Concept

A single small module (`directives_registry.F90`) containing a flat array of derived types, each describing one directive. A `--list-directives` CLI flag triggers a subroutine that iterates the array and prints a formatted table.

### Sketch

```fortran
module directives_registry

  use iso_c_binding, only : c_int
  implicit none
  private

  public :: print_directives_help

  type :: DIRECTIVE_ENTRY_T
    character(len=40) :: name            ! e.g. "CROP_COEFFICIENT_METHOD"
    logical           :: required        ! must appear in control file?
    character(len=80) :: valid_options   ! e.g. "FAO-56 | NONE"
    character(len=120) :: description    ! one-line explanation
  end type

  integer(c_int), parameter :: NUM_DIRECTIVES = 35  ! update as needed

  type(DIRECTIVE_ENTRY_T), parameter :: DIRECTIVES(NUM_DIRECTIVES) = [ &
    DIRECTIVE_ENTRY_T("INTERCEPTION_METHOD",         .true.,  &
      "BUCKET | GASH | NONE",                                 &
      "Method for computing canopy interception"),            &
    DIRECTIVE_ENTRY_T("EVAPOTRANSPIRATION_METHOD",   .true.,  &
      "HARGREAVES | JENSEN_HAISE | GRIDDED | NONE",           &
      "Reference ET calculation method"),                     &
    DIRECTIVE_ENTRY_T("CROP_COEFFICIENT_METHOD",     .true.,  &
      "FAO-56 | NONE",                                        &
      "Basal crop coefficient (Kcb) approach"),               &
    DIRECTIVE_ENTRY_T("GROWING_DEGREE_DAY_METHOD",   .true.,  &
      "SIMPLE | BASKERVILLE_EMIN | NONE",                     &
      "GDD accumulation algorithm"),                          &
    ! ... remaining directives ...
    DIRECTIVE_ENTRY_T("LOOKUP_TABLE_TEMPERATURE_UNITS", .false., &
      "FAHRENHEIT | CELSIUS",                                    &
      "Temperature units used in lookup table GDD columns")     &
  ]

contains

  subroutine print_directives_help()
    integer :: i
    write(*,'(A)') "SWB2 Control File Directives"
    write(*,'(A)') "============================"
    write(*,'(A)') ""
    write(*,'(A,T42,A,T48,A,T70,A)') "Directive", "Req?", "Valid Options", "Description"
    write(*,'(A)') repeat("-", 120)
    do i = 1, NUM_DIRECTIVES
      write(*,'(A,T42,L1,T48,A,T130,A)') &
        trim(DIRECTIVES(i)%name),         &
        DIRECTIVES(i)%required,           &
        trim(DIRECTIVES(i)%valid_options), &
        trim(DIRECTIVES(i)%description)
    end do
  end subroutine

end module directives_registry
```

### CLI Integration

In the main program argument parsing (before any model initialization):

```fortran
if (any(command_line_args == "--list-directives")) then
  call print_directives_help()
  stop
end if
```

### Pros

- **Always accurate** — if a developer adds a directive, they add it here too (it's the natural place)
- **Zero runtime cost** — only invoked on explicit CLI request
- **Simple** — one module, one array, one print subroutine
- **Could also validate control files** — future enhancement: check that all required directives are present, warn on unrecognized directives
- **Serves as internal documentation** — reading the array is faster than grepping through `model_domain.F90`

### Cons

- **Manual maintenance** — adding a directive requires updating this table (could forget)
- **String length limits** — long option lists or descriptions may need truncation
- **Duplicates information** — the parsing logic in `model_domain.F90` also has the valid option strings

### Effort

- Initial implementation (module + populate for all ~35 directives): **4-6 hours**
- CLI flag integration: **30 minutes**
- Ongoing: **2 minutes per new directive** (add one line to the array)

---

## Option 2: Extract from Source Code (Zero New Data)

### Concept

The parsing code in `model_domain.F90` already contains all valid directive names and method options as string literals inside `containssimilar` and `strapprox` checks. A Python/shell script parses the source and generates the reference list automatically.

### Sketch

```python
"""Extract valid SWB2 directives from model_domain.F90 source code."""
import re
from pathlib import Path

source = Path("src/model_domain.F90").read_text()

# Pattern: elseif ( sCmdText .containssimilar. "DIRECTIVE_NAME" ) then
directive_pattern = re.compile(
    r'containssimilar\.\s*"([^"]+)"', re.IGNORECASE
)

# Pattern: Method_Name .strapprox. "OPTION_NAME"
option_pattern = re.compile(
    r'strapprox\.\s*"([^"]+)"', re.IGNORECASE
)

# Group options by the directive they appear under...
```

### Pros

- **No new Fortran code** — purely an external tool
- **Always in sync** — extracts from the actual parsing logic (can't diverge)
- **Could generate documentation** (Markdown, man page, etc.) from the same extraction

### Cons

- **Fragile** — depends on the source code formatting staying consistent
- **Not built into the executable** — users can't just run `swb2 --help-directives`
- **Incomplete** — can extract names and options, but not descriptions (those aren't in the source)
- **Requires Python** — adds a build/documentation dependency

### Effort

- Initial script: **2-3 hours**
- Ongoing: **zero** (auto-extracts from source)
- But: no built-in CLI help, no descriptions

---

## Option 3: Authoritative Data File + Generated Include

### Concept

A single authoritative data file (TSV, TOML, or YAML) defines all directives. A pre-build script generates:
1. A Fortran `include` file (the string table for `--list-directives`)
2. Optionally, validation logic for the parser
3. Optionally, a Markdown/HTML reference page

### Sketch

**`directives.tsv`** (the single source of truth):
```
name	required	valid_options	description
INTERCEPTION_METHOD	true	BUCKET|GASH|NONE	Method for computing canopy interception
EVAPOTRANSPIRATION_METHOD	true	HARGREAVES|JENSEN_HAISE|GRIDDED|NONE	Reference ET calculation method
CROP_COEFFICIENT_METHOD	true	FAO-56|NONE	Basal crop coefficient (Kcb) approach
...
```

**`generate_directives_module.py`** (build step):
```python
"""Generate directives_registry.F90 from directives.tsv."""
# Read TSV, emit Fortran module with the DIRECTIVE_ENTRY_T array
```

**Meson integration:**
```meson
# In src/meson.build:
directives_src = custom_target('directives_registry',
  input: 'directives.tsv',
  output: 'directives_registry.F90',
  command: [python, 'generate_directives_module.py', '@INPUT@', '@OUTPUT@']
)
```

### Pros

- **Single source of truth** — one file drives code, docs, and validation
- **Can generate multiple outputs** — Fortran module, Markdown docs, man page, shell completions
- **Clean separation** — the data isn't buried in Fortran code
- **Enables future control file validation** — parser can check directives against the registry at runtime

### Cons

- **Build complexity** — adds a code generation step to the Meson build
- **Requires Python at build time** (already a pixi dependency, so not a new requirement)
- **Over-engineered for ~35 directives** — the benefit of auto-generation is marginal when the table is small and changes rarely
- **Harder to read casually** — developer must look at the TSV, not the Fortran source

### Effort

- TSV file + generator script: **3-4 hours**
- Meson integration: **1-2 hours**
- Ongoing: **edit the TSV** (similar effort to Option 1, but in a different file)

---

## Option 4: Hybrid — Option 1 with a Validation Hook

### Concept

Use Option 1 (simple Fortran string table) but add one enhancement: after parsing the control file, validate that every directive encountered is in the registry and every required directive was found. This catches typos and missing directives at startup rather than as cryptic errors later.

### Sketch

```fortran
subroutine validate_control_file(found_directives)
  type(FSTRING_LIST_T), intent(in) :: found_directives
  integer :: i
  logical :: found

  ! Check all required directives were specified
  do i = 1, NUM_DIRECTIVES
    if (DIRECTIVES(i)%required) then
      found = found_directives%contains(DIRECTIVES(i)%name)
      if (.not. found) then
        call warn("Required directive missing: "//trim(DIRECTIVES(i)%name) &
          //" (valid options: "//trim(DIRECTIVES(i)%valid_options)//")", &
          lFatal=.true.)
      end if
    end if
  end do

  ! Warn on unrecognized directives (possible typos)
  do i = 1, found_directives%count
    if (.not. directive_is_known(found_directives%get(i))) then
      call warn("Unrecognized directive: "//trim(found_directives%get(i)) &
        //" — check spelling?", lFatal=.false.)
    end if
  end do
end subroutine
```

### Pros

- Everything from Option 1, plus:
- **Catches typos** — `EVAPOTRANSPRATION_METHOD` (missing 'I') gets flagged immediately
- **Catches missing directives** — "you forgot to specify SOIL_MOISTURE_METHOD"
- **Friendlier error messages** — includes valid options in the error text

### Cons

- Slightly more implementation effort than bare Option 1
- Need to track which directives were encountered during parsing (minor bookkeeping)

### Effort

- Same as Option 1, plus **2-3 hours** for the validation subroutine and hooking it into the parser

---

## Comparison Summary

| Aspect | Option 1 (Table) | Option 2 (Extract) | Option 3 (Generated) | Option 4 (Table+Validate) |
|--------|:-:|:-:|:-:|:-:|
| Built into executable | ✅ | ❌ | ✅ | ✅ |
| Always in sync with parser | Manual | ✅ Auto | Manual (but single source) | Manual |
| Descriptions included | ✅ | ❌ | ✅ | ✅ |
| Catches typos | ❌ | ❌ | Possible | ✅ |
| Build complexity | None | None | Adds codegen step | None |
| Runtime cost | Zero | N/A | Zero | Trivial (startup only) |
| Effort | 4-6 hours | 2-3 hours | 5-7 hours | 6-9 hours |
| Ongoing maintenance | ~2 min/directive | Zero | ~2 min/directive | ~2 min/directive |

---

## Recommendation

**Option 4 (Table + Validation)** provides the best long-term value:

1. `swb2 --list-directives` gives users a complete reference
2. Control file typos are caught immediately with helpful messages
3. Missing required directives produce clear errors listing valid options
4. Zero build complexity (pure Fortran, no code generation)
5. Serves as internal developer documentation

However, if the priority is minimal effort for immediate value, **Option 1** (just the table and print) is perfectly adequate and can be extended to Option 4 later.

The parsing validation (Option 4's addition) would have prevented several debugging sessions where a misspelled directive silently fell through to the `else` branch and produced confusing downstream errors.

---

## CLI Interface (Any Option)

```
swb2 --list-directives          # Print all directives, options, descriptions
swb2 --list-directives METHOD   # Filter to directives containing "METHOD"
swb2 --validate myfile.ctl      # Parse control file and report issues without running
```

The `--validate` flag (checking a control file without running the model) would be particularly useful for the PEST++ workflow where hundreds of control files are generated from templates.
