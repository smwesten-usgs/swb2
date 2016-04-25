module proj4_support

  use strings, only      : right, left
  use string_list, only  : STRING_LIST_T, create_list
  implicit none

  private

contains

  subroutine create_attributes_from_proj4_string( proj4_string, attribute_name_list,   &
                                                  attribute_value_list )

    character (len=*), intent(in)         :: proj4_string
    type ( STRING_LIST_T, intent(out)     :: attribute_name_list
    type (STRING_LIST_T), intent(out)     :: attribute_value_list


    ! [ LOCALS ]
    type (STRING_LIST_T)           :: proj4_list
    integer (kind=c_int)           :: indx
    character (len=:), allocatable :: temp_string
    character (len=:), allocatable :: valuestring
    character (len=:), allocatable :: namestring

    proj4_list = create_list( proj4_string )

    do indx=1, proj4_list%count
 
      temp_string = proj4_list%get( indx )
      namestring = left( temp_string, "=" )
      valuestring = right( temp_string, "=" )

      select case ( namestring )
        case ( "+proj" )

          call attribute_name_list%append("grid_mapping_name")
          select case ( valuestring )
            case ( "aea" )
              call attribute_value_list%append("albers_conical_equal_area")
            case ( "aeqd" ) 
              call attribute_value_list%append("azimuthal_equidistant")
            case ( "tmerc" )
              call attribute_value_list%append("transverse_mercator")
            case ( "merc" )
              call attribute_value_list%append("mercator")
            case ( "cea" )
              call attribute_value_list%append("lambert_cylindrical_equal_area")
            case ( "lcc" )
              call attribute_value_list%append("lambert_conformal_conic")  
            case default
              call attribute_value_list%append("unknown")
          end select

        case ( "+datum" )

          call attribute_name_list%append("datum")
          select case ( valuestring )
            case ( "aea" )
              call attribute_value_list%append("albers_conical_equal_area")
            case ( "aeqd" ) 
              call attribute_value_list%append("azimuthal_equidistant")
            case ( "tmerc" )
              call attribute_value_list%append("transverse_mercator")
            case ( "merc" )
              call attribute_value_list%append("mercator")
            case ( "cea" )
              call attribute_value_list%append("lambert_cylindrical_equal_area")
            case ( "lcc" )
              call attribute_value_list%append("lambert_conformal_conic")  
            case default
              call attribute_value_list%append("unknown")
          end select


        case ( "+lon_0" )

          call attribute_name_list%append("longitude_of_central_meridian")
          call attribute_value_list%append( valuestring )

        case ( "+lat_0" )

          call attribute_name_list%append("latitude_of_projection_origin")
          call attribute_value_list%append( valuestring )

        case ( "+x_0" )

          call attribute_name_list%append("false_easting")
          call attribute_value_list%append( valuestring )

        case ( "+y_0" )

          call attribute_name_list%append("false_northing")
          call attribute_value_list%append( valuestring )

        case ( "+lat_1" )

          call attribute_name_list%append("latitude_of_first_standard_parallel")
          call attribute_value_list%append( valuestring )

        case ( "+lat_2" )

          call attribute_name_list%append("latitude_of_second_standard_parallel")
          call attribute_value_list%append( valuestring )

        case ( "+a" )
          
          call attribute_name_list%append("semi_major_axis")
          call attribute_value_list%append( valuestring )

        case ( "+b" ) 
        
          call attribute_name_list%append("semi_minor_axis")
          call attribute_value_list%append( valuestring )

        case ( "+k", "+k_0" )

          call attribute_name_list%append("scale_factor_at_central_meridian")
          call attribute_value_list%append( valuestring )

        case ( "+units" )   

          call attribute_name_list%append("units")
          call attribute_value_list%append( valuestring )

        case ( "+zone" )

          call attribute_name_list%append("UTM_zone")
          call attribute_value_list%append( valuestring )

        case default

      end select  

    enddo


  end subroutine create_attributes_from_proj4_string


end module proj4_support