file(GLOB fortran_files
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     *.F90 )

 # the following snippet assigns the local filename to __SRCNAME__
 # for each of the files contained in the 'SRCS' list
 #
 # i.e. rather than using __FILE__, which might expand to
 # d:/Some_dir/some_subdir/filename.F90, we can use
 # __SRCNAME__, which expands to 'filename.F90'

 foreach(f IN LISTS fortran_files)
   get_filename_component(b ${f} NAME)
   set_source_files_properties(${f} PROPERTIES
     COMPILE_DEFINITIONS "__SRCNAME__=\"${b}\"")
 endforeach()


set(SRC_PROJ4
      proj4/aasincos.c
      proj4/adjlon.c
      proj4/bch2bps.c
      proj4/bchgen.c
      proj4/biveval.c
      proj4/custom_swb_functions.c
      proj4/dmstor.c
      proj4/emess.c
      proj4/emess.h
      proj4/gen_cheb.c
      proj4/geocent.c
      proj4/geocent.h
      proj4/geod_for.c
      proj4/geod_inv.c
      proj4/geod_set.c
      proj4/geodesic.h
      proj4/mk_cheby.c
      proj4/nad_cvt.c
      proj4/nad_init.c
      proj4/nad_intr.c
      proj4/org_proj4_Projections.h
      proj4/p_series.c
      proj4/PJ_aea.c
      proj4/PJ_aeqd.c
      proj4/PJ_airy.c
      proj4/PJ_aitoff.c
      proj4/pj_apply_gridshift.c
      proj4/pj_apply_vgridshift.c
      proj4/PJ_august.c
      proj4/pj_auth.c
      proj4/PJ_bacon.c
      proj4/PJ_bipc.c
      proj4/PJ_boggs.c
      proj4/PJ_bonne.c
      proj4/PJ_cass.c
      proj4/PJ_cc.c
      proj4/PJ_cea.c
      proj4/PJ_chamb.c
      proj4/PJ_collg.c
      proj4/PJ_crast.c
      proj4/pj_ctx.c
      proj4/pj_datum_set.c
      proj4/pj_datums.c
      proj4/PJ_denoy.c
      proj4/pj_deriv.c
      proj4/PJ_eck1.c
      proj4/PJ_eck2.c
      proj4/PJ_eck3.c
      proj4/PJ_eck4.c
      proj4/PJ_eck5.c
      proj4/pj_ell_set.c
      proj4/pj_ellps.c
      proj4/PJ_eqc.c
      proj4/PJ_eqdc.c
      proj4/pj_errno.c
      proj4/pj_factors.c
      proj4/PJ_fahey.c
      proj4/PJ_fouc_s.c
      proj4/pj_fwd.c
      proj4/PJ_gall.c
      proj4/pj_gauss.c
      proj4/pj_geocent.c
      proj4/PJ_geos.c
      proj4/PJ_gins8.c
      proj4/PJ_gn_sinu.c
      proj4/PJ_gnom.c
      proj4/PJ_goode.c
      proj4/pj_gridinfo.c
      proj4/pj_gridlist.c
      proj4/PJ_gstmerc.c
      proj4/PJ_hammer.c
      proj4/PJ_hatano.c
      proj4/PJ_healpix.c
      proj4/PJ_igh.c
      proj4/PJ_imw_p.c
      proj4/pj_init.c
      proj4/pj_initcache.c
      proj4/pj_inv.c
      proj4/PJ_isea.c
      proj4/PJ_krovak.c
      proj4/PJ_labrd.c
      proj4/PJ_laea.c
      proj4/PJ_lagrng.c
      proj4/PJ_larr.c
      proj4/PJ_lask.c
      proj4/pj_latlong.c
      proj4/PJ_lcc.c
      proj4/PJ_lcca.c
      proj4/pj_list.c
      proj4/pj_list.h
      proj4/pj_log.c
      proj4/PJ_loxim.c
      proj4/PJ_lsat.c
      proj4/pj_malloc.c
      proj4/PJ_mbt_fps.c
      proj4/PJ_mbtfpp.c
      proj4/PJ_mbtfpq.c
      proj4/PJ_merc.c
      proj4/PJ_mill.c
      proj4/pj_mlfn.c
      proj4/PJ_mod_ster.c
      proj4/PJ_moll.c
      proj4/pj_msfn.c
      proj4/pj_mutex.c
      proj4/PJ_natearth.c
      proj4/PJ_nell.c
      proj4/PJ_nell_h.c
      proj4/PJ_nocol.c
      proj4/PJ_nsper.c
      proj4/PJ_nzmg.c
      proj4/PJ_ob_tran.c
      proj4/PJ_ocea.c
      proj4/PJ_oea.c
      proj4/PJ_omerc.c
      proj4/pj_open_lib.c
      proj4/PJ_ortho.c
      proj4/pj_param.c
      proj4/pj_phi2.c
      proj4/PJ_poly.c
      proj4/pj_pr_list.c
      proj4/PJ_putp2.c
      proj4/PJ_putp3.c
      proj4/PJ_putp4p.c
      proj4/PJ_putp5.c
      proj4/PJ_putp6.c
      proj4/pj_qsfn.c
      proj4/pj_release.c
      proj4/PJ_robin.c
      proj4/PJ_rpoly.c
      proj4/PJ_sconics.c
      proj4/PJ_somerc.c
      proj4/PJ_stere.c
      proj4/PJ_sterea.c
      proj4/pj_strerrno.c
      proj4/PJ_sts.c
      proj4/PJ_tcc.c
      proj4/PJ_tcea.c
      proj4/PJ_tmerc.c
      proj4/PJ_tpeqd.c
      proj4/pj_transform.c
      proj4/pj_tsfn.c
      proj4/pj_units.c
      proj4/PJ_urm5.c
      proj4/PJ_urmfps.c
      proj4/pj_utils.c
      proj4/PJ_vandg.c
      proj4/PJ_vandg2.c
      proj4/PJ_vandg4.c
      proj4/PJ_wag2.c
      proj4/PJ_wag3.c
      proj4/PJ_wag7.c
      proj4/PJ_wink1.c
      proj4/PJ_wink2.c
      proj4/pj_zpoly1.c
      proj4/proj.def
      proj4/proj_api.h
      proj4/proj_etmerc.c
      proj4/proj_mdist.c
      proj4/proj_rouss.c
      proj4/projects.h
      proj4/rtodms.c
      proj4/vector1.c

      )

add_library( proj4_lib ${SRC_PROJ4} )

set(SRCS
      actual_et__gridded_values.F90
      actual_et__fao56__two_stage.F90
      actual_et__fao56.F90
      actual_et__thornthwaite_mather.F90
      actual_et__thornthwaite_mather_eqns.F90
      awc__gridded_values.F90
      awc__depth_integrated.F90
      constants_and_conversions.F90
      continuous_frozen_ground_index.F90
      crop_coefficients__fao56.F90
      daily_calculation.F90
      data_catalog.F90
      data_catalog_entry.F90
      datetime.F90
      dictionary.F90
      direct_net_infiltration__gridded_data.F90
      direct_soil_moisture__gridded_data.F90
      disclaimers.F90
      et__gridded_values.F90
      et__hargreaves_samani.F90
      et__jensen_haise.F90
      et__zone_values.F90
      exceptions.F90
      fog__monthly_grid.F90
      file_operations.F90
      fstring.F90
      fstring_list.F90
      grid.F90
      growing_degree_day.F90
      growing_degree_day_baskerville_emin.F90
      growing_season.F90
      interception__bucket.F90
      interception__gash.F90
      irrigation.F90
      kiss_random_number_generator.F90
      logfiles.F90
      mass_balance__impervious_surface.F90
      mass_balance__interception.F90
      mass_balance__snow.F90
      mass_balance__soil.F90
      maximum_net_infiltration.F90
      meteorological_calculations.F90
      model_domain.F90
      model_initialize.F90
      model_iterate.F90
      model_iterate_multiple_simulations.F90
      netcdf4_support.F90
      netcdf_c_api_interfaces.F90
      output.F90
      parameters.F90
      precipitation__method_of_fragments.F90
      proj4_support.F90
      rooting_depth__FAO56.F90
      routing__D8.F90
      running_grid_stats.F90
      runoff__curve_number.F90
      runoff__gridded_values.F90
      simulation_datetime.F90
      snowfall__original.F90
      snowmelt__original.F90
      solar_calculations.F90
      storm_drain_capture.F90
      timer.F90

      generated/version_control.F90

      )

#https://stackoverflow.com/questions/14199708/cmake-include-library-dependencies-in-a-static-library

#add_library(combinedLib INTERFACE)
#target_link_libraries(combinedLib INTERFACE ${ZLIB_LIBRARY} ${HDF5_LIBRARY} ${HDF5_HL_LIBRARY} ${NETCDF_LIBRARY})

add_library(swb_lib ${SRCS})
target_link_libraries( swb_lib proj4_lib ${EXTERNAL_LIBRARIES} )

add_executable(swb2 main.F90 )
target_link_libraries( swb2 proj4_lib swb_lib ${EXTERNAL_LIBRARIES} )

add_executable(swbstats2 swbstats2.F90 swbstats2_support.F90)
target_link_libraries( swbstats2 proj4_lib swb_lib ${EXTERNAL_LIBRARIES} )
