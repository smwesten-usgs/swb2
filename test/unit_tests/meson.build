# Unit tests using the test-drive framework
# test-drive source (testdrive.F90) is vendored directly — single file, no DLL issues.

test_srcs = files(
  'testdrive.F90',
  'test_fixtures.F90',
  'test_timer.F90',
  'test_allocatable_string.F90',
  'test_exceptions.F90',
  'test_gash.F90',
  'test_datetime.F90',
  'test_fao56.F90',
  'test_constants_and_conversions.F90',
  'test_fstring_list.F90',
  'tester.F90',
)

swbtest = executable('swbtest',
  sources: test_srcs,
  link_with: [swb_library],
  link_language: 'fortran',
  dependencies: swb_deps,
  install: true,
)

# Register all tests as a single run
test('All unit tests', swbtest, workdir: meson.current_source_dir())

# Register individual suites for selective execution
foreach suite : ['timer', 'allocatable_string', 'exceptions', 'gash', 'datetime', 'fao56', 'constants', 'fstring_list']
  test(suite, swbtest, args: suite, workdir: meson.current_source_dir())
endforeach
