program test_proj4_support

  use iso_c_binding, only            : c_int
  use constants_and_conversions
  use string_list, only              : STRING_LIST_T
  use proj4_support
  implicit none

  ! [ LOCALS ]
  character (len=:), allocatable :: PROJ4_string
  type (STRING_LIST_T)           :: attribute_name_list
  type (STRING_LIST_T)           :: attribute_value_list
  integer (kind=c_int)           :: indx

  PROJ4_string = "+proj=utm +zone=4 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

  write(*, "(/,'Input PROJ4 string: ',a)") PROJ4_string 
  write(*,"(a)") repeat("-",80)

  call create_attributes_from_proj4_string( PROJ4_string, attribute_name_list,   &
                                                  attribute_value_list )

  do indx=1, attribute_name_list%count

    write(*, fmt="(a,': ',t40,a)") attribute_name_list%get( indx ), attribute_value_list%get( indx )

  enddo 



  PROJ4_string = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

  write(*, "(/,'Input PROJ4 string: ',a)") PROJ4_string 
  write(*,"(a)") repeat("-",80)

  call create_attributes_from_proj4_string( PROJ4_string, attribute_name_list,   &
                                                  attribute_value_list )

  do indx=1, attribute_name_list%count

    write(*, fmt="(a,': ',t40,a)") attribute_name_list%get( indx ), attribute_value_list%get( indx )

  enddo 


  PROJ4_string = "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "

  write(*, "(/,'Input PROJ4 string: ',a)") PROJ4_string 
  write(*,"(a)") repeat("-",80)

  call create_attributes_from_proj4_string( PROJ4_string, attribute_name_list,   &
                                                  attribute_value_list )

  do indx=1, attribute_name_list%count

    write(*, fmt="(a,': ',t40,a)") attribute_name_list%get( indx ), attribute_value_list%get( indx )

  enddo 



end program test_proj4_support