# change from previous build files: swb version is only defined in the meson
# 'project' call... the value should propagate through the build using
# meson variables
project(
  'swb2',
  ['fortran', 'c', 'cpp'],
  version: 'v2.3.4-rc0',
  meson_version: '>= 1.6.0',
  default_options : ['optimization=2',     #meson understands: {plain, 0, g, 1, 2, 3, s}
                     'debug=true']         # whether or not to compile with debug symbols enabled
)

version = meson.project_version()

# determine what kind of system we're building on
# expected value is one of 'windows', 'darwin' (MacOS), or 'linux'
system = build_machine.system()

git_branch = run_command(['git', 'rev-parse', '--abbrev-ref', 'HEAD'], capture : true, check : true)
git_branch_str = '"' + git_branch.stdout().strip() + '"'

git_hash = run_command(['git', 'log', '-1', '--format=%h'], capture : true, check : true)
git_hash_str = '"' + git_hash.stdout().strip() + '"'

build_number = run_command(['git', 'rev-list', version + '..', '--count'], capture : true, check : true)
build_number_str = '"' + build_number.stdout().strip() + '"'

message('GIT BRANCH IS: ' + git_branch_str)
message('GIT HASH IS: ' + git_hash_str)
message('VERSION IS: ' + version)

conf_data = configuration_data()
conf_data.set('version', version)
conf_data.set('GIT_BRANCH_STRING', git_branch_str)
conf_data.set('GIT_HASH_STRING', git_hash_str)
conf_data.set('BUILD_NUMBER_STRING', build_number_str)

# Parse version components
version_core = version.substring(1)  # remove leading 'v'
version_parts = version_core.split('.')  # ['2', '3', '4-rc0']

swb_major_version = version_parts[0]
swb_minor_version = version_parts[1]
swb_patch_and_suffix = version_parts[2].split('-')  # ['4', 'rc0']
swb_patch_version = swb_patch_and_suffix[0]
swb_suffix = swb_patch_and_suffix.length() > 1 ? swb_patch_and_suffix[1] : ''

# Use in configuration
conf_data.set('SWB_MAJOR_VERSION_STRING', '"' + swb_major_version + '"')
conf_data.set('SWB_MAJOR_VERSION_STRING', '"' + swb_major_version + '"')
conf_data.set('SWB_MINOR_VERSION_STRING', '"' + swb_minor_version + '"')
conf_data.set('SWB_PATCH_VERSION_STRING', '"' + swb_patch_version + '"')
conf_data.set('SWB_SUFFIX_STRING', '"' + swb_suffix + '"')
conf_data.set('VERSION_STRING', '"' + version + '"')

# Attempt to gracefully handle netcdf dependency
netcdf_root = get_option('netcdf_root')

# Platform-specific defaults
if netcdf_root == ''
  if system == 'windows'
    netcdf_root = 'C:/Program Files/netCDF 4.9.3'
  elif system == 'darwin'
    netcdf_root = '/opt/homebrew/opt/netcdf'
  elif system == 'linux'
    netcdf_root = '/usr/local'
  else
    error('Unsupported platform: ' + system)
  endif
endif

add_global_arguments('-DDEBUG_PRINT', language : 'fortran')

if get_option('optimization') == '2'
  profile = 'release'
else
  profile = 'develop'
endif

#inc_dirs = include_directories(netcdf_root / 'include')

# get compiler information
fc = meson.get_compiler('fortran')
fc_id = fc.get_id()
compile_args = []
c_compile_args = []
link_args = []

cc = meson.get_compiler('c')
cc_id = cc.get_id()

if system == 'linux'
  conf_data.set('PLATFORM_NAME','"Linux"')
  conf_data.set('WINDOWS_SYSTEM', 'False')
  message('setting Meson conf_data for Linux')
elif system == 'darwin'
  conf_data.set('PLATFORM_NAME','"MacOS"')
  conf_data.set('WINDOWS_SYSTEM', 'False')
  message('setting Meson conf_data for MacOS')
elif system == 'windows'
  conf_data.set('PLATFORM_NAME','"Windows"')
  conf_data.set('WINDOWS_SYSTEM', 'True')
  message('setting Meson conf_data for Windows')
endif

message('The compilation profile is:', profile)
message('Using netcdf_root: ' + netcdf_root)
message('Meson sees the compiler name as:' + fc_id)
message('Meson sees the C compiler name as: ' + cc_id)
message('Meson sees the build machine as: ' + system)

# Command line options for gfortran
if fc_id == 'gcc'
  # General options
  compile_args += [
                   '-fall-intrinsics',
                   '-pedantic',
                   '-std=gnu',
                   '-fallow-argument-mismatch',
                   '-Wcharacter-truncation',
                   '-ffree-line-length-none',
                   '-Wno-error=line-truncation',
                   '-Wno-unused-dummy-argument', # This makes problems with OOP
                   '-Wno-intrinsic-shadow',      # We shadow intrinsics with methods, which should be fine
                   '-Wno-maybe-uninitialized',   # "Uninitialized" flags produce false positives with allocatables
                   '-Wno-uninitialized',
                   '-Wno-conversion',
                   ]

  # Options specific to profile
  if profile == 'release'
    compile_args += ['-ffpe-summary=overflow', '-ffpe-trap=overflow,zero,invalid']
  elif profile == 'develop'
    #compile_args += ['-fcheck=all', '-finit-real=nan', '-ffpe-trap=overflow,zero,invalid', '-g', '-fbacktrace' ]
    #compile_args += ['-fcheck=all', '-finit-real=nan', '-g', '-fbacktrace' ]
    compile_args += ['-fcheck=all', '-g', '-fbacktrace' ]
  endif

  # Define OS with gfortran for OS specific code
  # These are identical to pre-defined macros available with ifort
  system = build_machine.system()
  if system == 'linux'
    compile_args += '-D__linux__'
  elif system == 'darwin'
    compile_args += '-D__APPLE__'
  elif system == 'windows'
    compile_args += '-D_WIN32'
  endif
endif

# Command line options for ifort
if (fc_id == 'intel-llvm-cl') and (system == 'windows')
  # windows

### Meson apparently throws in a /utf-8 compiler argument, which clang complains about. The kludge below is an attempt
### to silence this
  c_compile_args += ['/Qdiag-disable:10430',
                     '/Wno-unused-command-line-argument',
                     '/Wno-unknown-argument',
                     '/Wmost',
                     '/Wno-tautological-compare',
                     '/Wno-missing-braces',
                     '/Wno-infinite-recursion',
                     '/fp-speculation=safe',
                     '/fp-model=precise',
                     '/no-fma',
                     '/MT'
                    ]

  compile_args += [#'/fpe:0',              # Activate all floating point exceptions
                   '/heap-arrays:0',
                   '/traceback',
                   '/W0',
                   '/MT',
                   '/fp:precise',
                   '/fpp',                # Activate preprocessing
                   '/Qdiag-disable:7416', # f2008 warning
                   '/Qdiag-disable:7025', # f2008 warning
                   '/Qdiag-disable:6048', # f2008: default-logical-variable must be of type default logical
                   '/Qdiag-disable:5268', # Line too long
                   '/Qdiag-error-limit:1',
                  ]


elif fc_id == 'intel-llvm'
  # linux and macOS

  if profile == 'release'
  compile_args += ['-fpe0',               # Activate subset of floating point exceptions
                   '-fpp',
                   '-fp-model=precise',
                   '-no-heap-arrays',
                   '-traceback',
                   '-diag-disable:7416',  # f2008 warning
                   '-diag-disable:7025',  # f2008 warning
                   '-diag-disable:5268',  # Line too long
                  ]

  elif profile == 'develop'
  compile_args += ['-fpe-all=0',              # Activate all floating point exceptions
                   '-fpp',
                   '-fp-model=strict',
                   '-check=all',
                   '-no-heap-arrays',
                   '-traceback',
                   '-diag-disable:7416',  # f2008 warning
                   '-diag-disable:7025',  # f2008 warning
                   '-diag-disable:5268',  # Line too long
                  ]
  endif

link_args += '-static-intel'
  
endif

add_project_arguments(fc.get_supported_arguments(compile_args), language: 'fortran')
add_project_arguments(cc.get_supported_arguments(c_compile_args), language: 'c')
add_project_link_arguments(fc.get_supported_arguments(link_args), language: 'fortran')

subdir('src')
subdir('test/unit_tests')
