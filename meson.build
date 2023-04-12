project(
  'swb2',
  'fortran','c','cpp',
  meson_version: '>= 0.59.0',
  default_options : [
    'optimization=2',
    'warning_level=0',    # nice in theory, but Meson doesn't appear to control warning levels on Windows with LLVM-Intel compiler?
    'fortran_std=f2008'
])

version = get_option('swb_version')

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

swb_major_version = get_option('swb_major_version')
swb_minor_version = get_option('swb_minor_version')
swb_patch_version = get_option('swb_patch_version')

conf_data.set('SWB_MAJOR_VERSION_STRING', swb_major_version)
conf_data.set('SWB_MINOR_VERSION_STRING', swb_minor_version)
conf_data.set('SWB_PATCH_VERSION_STRING', swb_patch_version)

system = build_machine.system()
if system == 'linux'
  conf_data.set('PLATFORM_NAME','"Linux"')
  conf_data.set('WINDOWS_SYSTEM', 'False')
elif system == 'darwin'
  conf_data.set('PLATFORM_NAME','"MacOS"')
  conf_data.set('WINDOWS_SYSTEM', 'False')
elif system == 'windows'
  conf_data.set('PLATFORM_NAME','"Windows"')
  conf_data.set('WINDOWS_SYSTEM', 'True')
endif

if get_option('optimization_level') == 3
  error('Only optimization levels <= 2 are supported')
endif

if get_option('optimization_level') == 2
  profile = 'release'
else
  profile = 'develop'
endif

message('The compilation profile is:', profile)

fc = meson.get_compiler('fortran')
fc_id = fc.get_id()
compile_args = []
c_compile_args = []
link_args = []

cc = meson.get_compiler('c')
cc_id = cc.get_id()

# Command line options for gfortran
if fc_id == 'gcc'
  # General options
  compile_args += [
                   '-fall-intrinsics',
                   '-pedantic',
                   '-Wcharacter-truncation',
                   '-Wno-unused-dummy-argument', # This makes problems with OOP
                   '-Wno-intrinsic-shadow',      # We shadow intrinsics with methods, which should be fine
                   '-Wno-maybe-uninitialized',   # "Uninitialized" flags produce false positives with allocatables
                   '-Wno-uninitialized',
                   ]

  # Options specific to profile
  if profile == 'release'
    compile_args += ['-ffpe-summary=overflow', '-ffpe-trap=overflow,zero,invalid']
  elif profile == 'develop'
    compile_args += ['-fcheck=all', '-ffpe-trap=overflow,zero,invalid,denormal']
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
if fc_id == 'intel-llvm-cl'
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
                   '/fpp',                # Activate preprocessing
                   '/Qdiag-disable:7416', # f2008 warning
                   '/Qdiag-disable:7025', # f2008 warning
                   '/Qdiag-disable:6048', # f2008: default-logical-variable must be of type default logical
                   '/Qdiag-disable:5268', # Line too long
                   '/Qdiag-error-limit:1',
                  ]
  link_args += ['/Qdiag-disable:10441',
               ]

elif fc_id == 'intel'
  # linux and macOS
  compile_args += ['-fpe0',              # Activate all floating point exceptions
                   '-fpp',
                   '-no-heap-arrays',
                   '-traceback',
                   '-diag-disable:7416',  # f2008 warning
                   '-diag-disable:7025',  # f2008 warning
                   '-diag-disable:5268',  # Line too long
                  ]
  link_args += '-static-intel'
endif

add_project_arguments(fc.get_supported_arguments(compile_args), language: 'fortran')
add_project_arguments(cc.get_supported_arguments(c_compile_args), language: 'c')
add_project_link_arguments(fc.get_supported_arguments(link_args), language: 'fortran')

include_directories('C:/Program Files/netCDF 4.9.0/include')

subdir('src')
subdir('test/unit_tests')
