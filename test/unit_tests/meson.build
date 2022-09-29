swbtest_src = files(
  'fruit.F90',
  'fruit_util.F90',
  'test_allocatable_string.F90',
  'test_datetime.F90',
  'test_exceptions__index_values_valid.F90',
  'test_FAO56_functions.F90',
  'test_gash.F90',
  'test_timer.F90',
  'fruit_driver.F90'
)

executable('swbtest', 
            swbtest_src,
            link_with: [swb_external, swb_library], 
            dependencies : dependencies,
            install: true)
