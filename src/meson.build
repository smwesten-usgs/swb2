configure_file(
  input: 'version_control.tpl',
  output: 'version_control.F90',
  configuration: {
    'BUILD_NUMBER_STRING': conf_data.get('BUILD_NUMBER_STRING'),
    'GIT_BRANCH_STRING': conf_data.get('GIT_BRANCH_STRING'),
    'GIT_HASH_STRING': conf_data.get('GIT_HASH_STRING'),
    'PLATFORM_NAME': conf_data.get('PLATFORM_NAME'),
    'SWB_MAJOR_VERSION_STRING': conf_data.get('SWB_MAJOR_VERSION_STRING'),
    'SWB_MINOR_VERSION_STRING': conf_data.get('SWB_MINOR_VERSION_STRING'),
    'SWB_PATCH_VERSION_STRING': conf_data.get('SWB_PATCH_VERSION_STRING'),
  },
  install: true,
  install_dir: 'src/generated'
)

# Common library sources (everything except main programs)
swb_common_src = files(
    'actual_et__gridded_values.F90',
    'actual_et__fao56__two_stage.F90',
    'actual_et__fao56.F90',
    'actual_et__thornthwaite_mather.F90',
    'awc__gridded_values.F90',
    'awc__depth_integrated.F90',
    'constants_and_conversions.F90',
    'continuous_frozen_ground_index.F90',
    'crop_coefficients__fao56.F90',
    'daily_calculation.F90',
    'data_catalog.F90',
    'data_catalog_entry.F90',
    'datetime.F90',
    'dictionary.F90',
    'direct_net_infiltration__gridded_data.F90',
    'direct_soil_moisture__gridded_data.F90',
    'disclaimers.F90',
    'et__gridded_values.F90',
    'et__hargreaves_samani.F90',
    'et__jensen_haise.F90',
    'et__zone_values.F90',
    'exceptions.F90',
    'fog__monthly_grid.F90',
    'file_operations.F90',
    'fstring.F90',
    'fstring_list.F90',
    'grid.F90',
    'growing_degree_day.F90',
    'growing_degree_day_baskerville_emin.F90',
    'growing_season.F90',
    'interception__bucket.F90',
    'interception__gash.F90',
    'irrigation.F90',
    'kiss_random_number_generator.F90',
    'logfiles.F90',
    'mass_balance__impervious_surface.F90',
    'mass_balance__interception.F90',
    'mass_balance__snow.F90',
    'mass_balance__soil.F90',
    'maximum_net_infiltration.F90',
    'meteorological_calculations.F90',
    'model_domain.F90',
    'model_initialize.F90',
    'model_iterate.F90',
    'model_iterate_multiple_simulations.F90',
    'netcdf4_support.F90',
    'netcdf_c_api_interfaces.F90',
    'output.F90',
    'parameters.F90',
    'precipitation__method_of_fragments.F90',
    'proj4_support.F90',
    'rooting_depth__FAO56.F90',
    'routing__D8.F90',
    'running_grid_stats.F90',
    'runoff__curve_number.F90',
    'runoff__gridded_values.F90',
    'simulation_datetime.F90',
    'snowfall__original.F90',
    'snowmelt__original.F90',
    'solar_calculations.F90',
    'storm_drain_capture.F90',
    'timer.F90',
    'weather_data_tabular.F90',
    meson.current_build_dir() + '/version_control.F90',
)

subdir('proj4')

# Static library from common sources — linked by all executables
swb_library = static_library('swb_library',
    swb_common_src,
    link_with: [proj4],
)

# NetCDF and related dependencies
# Prefer pkg-config (works inside pixi, works on Linux with system packages)
netcdf_dep = dependency('netcdf', required: false)
hdf5_dep = dependency('hdf5', required: false)

if netcdf_dep.found() and hdf5_dep.found()
  swb_deps = [netcdf_dep, hdf5_dep]
  message('Dependencies found via pkg-config')
else
  # Fallback: manual search using netcdf_root
  message('pkg-config not available; using netcdf_root: ' + netcdf_root)
  zlib = cc.find_library('zlib', dirs: [netcdf_root / 'lib'], required: false)
  hdf5 = cc.find_library('hdf5', dirs: [netcdf_root / 'lib'], required: true)
  hdf5_hl = cc.find_library('hdf5_hl', dirs: [netcdf_root / 'lib'], required: true)
  netcdf = cc.find_library('netcdf', dirs: [netcdf_root / 'lib'], required: true)
  swb_deps = [zlib, hdf5, hdf5_hl, netcdf]
endif

executable('swb2',
    files('main.F90'),
    link_with: [swb_library],
    link_language: 'fortran',
    dependencies: swb_deps,
    install: true,
)

executable('swbstats2',
    files('swbstats2_support.F90', 'swbstats2.F90'),
    link_with: [swb_library],
    link_language: 'fortran',
    dependencies: swb_deps,
    install: true,
)
