project(
  'swb2',
  ['fortran', 'c', 'cpp'],
  version: 'v2.4.1',
  meson_version: '>= 1.6.0',
  default_options: ['optimization=2', 'debug=true', 'fortran_std=f2018']
)

version = meson.project_version()
system = build_machine.system()

# Git metadata
git_branch = run_command(['git', 'rev-parse', '--abbrev-ref', 'HEAD'], capture: true, check: true)
git_branch_str = '"' + git_branch.stdout().strip() + '"'

git_hash = run_command(['git', 'log', '-1', '--format=%h'], capture: true, check: true)
git_hash_str = '"' + git_hash.stdout().strip() + '"'

build_number = run_command(['git', 'rev-list', version + '..', '--count'], capture: true, check: false)
if build_number.returncode() == 0
  build_number_str = '"' + build_number.stdout().strip() + '"'
else
  # Tag doesn't exist yet (e.g., new version number before first commit with that tag).
  # Default to "0" — the tag will be created at release time.
  build_number_str = '"0"'
endif

message('GIT BRANCH IS: ' + git_branch_str)
message('GIT HASH IS: ' + git_hash_str)
message('VERSION IS: ' + version)

# Version parsing
version_core = version.substring(1)
version_parts = version_core.split('.')
swb_major_version = version_parts[0]
swb_minor_version = version_parts[1]
swb_patch_and_suffix = version_parts[2].split('-')
swb_patch_version = swb_patch_and_suffix[0]
swb_suffix = swb_patch_and_suffix.length() > 1 ? swb_patch_and_suffix[1] : ''

# Configuration data for version_control.F90 template
conf_data = configuration_data()
conf_data.set('version', version)
conf_data.set('GIT_BRANCH_STRING', git_branch_str)
conf_data.set('GIT_HASH_STRING', git_hash_str)
conf_data.set('BUILD_NUMBER_STRING', build_number_str)
conf_data.set('SWB_MAJOR_VERSION_STRING', '"' + swb_major_version + '"')
conf_data.set('SWB_MINOR_VERSION_STRING', '"' + swb_minor_version + '"')
conf_data.set('SWB_PATCH_VERSION_STRING', '"' + swb_patch_version + '"')
conf_data.set('SWB_SUFFIX_STRING', '"' + swb_suffix + '"')
conf_data.set('VERSION_STRING', '"' + version + '"')

# Platform configuration
if system == 'linux'
  conf_data.set('PLATFORM_NAME', '"Linux"')
  conf_data.set('WINDOWS_SYSTEM', 'False')
elif system == 'darwin'
  conf_data.set('PLATFORM_NAME', '"MacOS"')
  conf_data.set('WINDOWS_SYSTEM', 'False')
elif system == 'windows'
  conf_data.set('PLATFORM_NAME', '"Windows"')
  conf_data.set('WINDOWS_SYSTEM', 'True')
endif

# Build profile
profile = get_option('profile')

add_global_arguments('-DDEBUG_PRINT', language: 'fortran')

# Compilers
fc = meson.get_compiler('fortran')
fc_id = fc.get_id()
compile_args = []
c_compile_args = []
link_args = []

cc = meson.get_compiler('c')
cc_id = cc.get_id()

message('Profile: ' + profile)
message('Fortran compiler: ' + fc_id)
message('C compiler: ' + cc_id)
message('Build machine: ' + system)

# ===========================================================================
# gfortran flags
# ===========================================================================
if fc_id == 'gcc'

  # --- Flags common to all profiles ---
  compile_args += [
    '-ffree-line-length-none',
    '-Wcharacter-truncation',
    '-Wno-intrinsic-shadow',
  ]

  link_args += ['-static-libgfortran', '-static-libquadmath', '-static-libgcc']

  # --- Profile-specific flags ---
  if profile == 'release'
    compile_args += [
      '-ffpe-trap=overflow,zero',
      '-fbacktrace',
    ]

  elif profile == 'develop'
    compile_args += [
      '-fcheck=all',
      '-finit-real=nan',
      '-ffpe-trap=overflow,zero',
      '-fbacktrace',
    ]

  elif profile == 'static_analysis'
    # Goal: emit comprehensive warnings about everything non-conforming.
    compile_args += [
      '-Wall',
      '-Wextra',
      '-Wimplicit-interface',
      '-Wno-unused-dummy-argument',
      '-pedantic',
    ]
  endif

  # OS preprocessor defines (mirrors Intel predefined macros)
  if system == 'linux'
    compile_args += '-D__linux__'
  elif system == 'darwin'
    compile_args += '-D__APPLE__'
  elif system == 'windows'
    compile_args += '-D_WIN32'
  endif
endif

# ===========================================================================
# Intel ifx flags (Windows - intel-llvm-cl)
# ===========================================================================
if (fc_id == 'intel-llvm-cl') and (system == 'windows')

  c_compile_args += [
    '/Qdiag-disable:10430',
    '/Wno-unused-command-line-argument',
    '/Wno-unknown-argument',
    '/Wmost',
    '/Wno-tautological-compare',
    '/Wno-missing-braces',
    '/Wno-infinite-recursion',
    '/fp-speculation=safe',
    '/fp-model=precise',
    '/no-fma',
  ]

  if profile == 'release' or profile == 'develop'
    compile_args += [
      '/heap-arrays:0',
      '/traceback',
      '/W0',
      '/fp:precise',
      '/fpp',
      '/Qdiag-disable:7416',
      '/Qdiag-disable:7025',
      '/Qdiag-disable:6048',
      '/Qdiag-disable:5268',
      '/Qdiag-error-limit:1',
    ]

  elif profile == 'static_analysis'
    # Emit all warnings, flag non-standard code, do not suppress diagnostics
    compile_args += [
      '/heap-arrays:0',
      '/traceback',
      '/fp:precise',
      '/fpp',
      '/warn:all',
      '/stand:f18',
      '/Qdiag-error-limit:0',
    ]
  endif

# ===========================================================================
# Intel ifx flags (Linux/macOS - intel-llvm)
# ===========================================================================
elif fc_id == 'intel-llvm'

  if profile == 'release'
    compile_args += [
      '-fpe0',
      '-fpp',
      '-fp-model=precise',
      '-no-heap-arrays',
      '-traceback',
      '-diag-disable:7416',
      '-diag-disable:7025',
      '-diag-disable:5268',
    ]

  elif profile == 'develop'
    compile_args += [
      '-fpe-all=0',
      '-fpp',
      '-fp-model=strict',
      '-check=all',
      '-no-heap-arrays',
      '-traceback',
      '-diag-disable:7416',
      '-diag-disable:7025',
      '-diag-disable:5268',
    ]

  elif profile == 'static_analysis'
    # Emit all warnings, flag non-standard code, no error limit
    compile_args += [
      '-fpp',
      '-fp-model=precise',
      '-no-heap-arrays',
      '-traceback',
      '-warn', 'all',
      '-stand', 'f18',
      '-diag-error-limit=0',
    ]
  endif

  link_args += '-static-intel'

endif

add_project_arguments(fc.get_supported_arguments(compile_args), language: 'fortran')
add_project_arguments(cc.get_supported_arguments(c_compile_args), language: 'c')
add_project_link_arguments(fc.get_supported_arguments(link_args), language: 'fortran')

subdir('src')
subdir('test/unit_tests')
