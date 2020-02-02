
set( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules )

set( srcs "main.F90" "kinds.F90" "constants.F90" "times.F90" "time_date.F90" "geometry.F90" )

add_executable( main ${srcs} )

find_package( HDF5 REQUIRED COMPONENTS Fortran )
target_link_libraries( main ${HDF5_LIBRARIES} )
target_include_directories( main PRIVATE ${HDF5_INCLUDE_DIRS}/shared )


if( OpenMP )
  target_link_libraries( main OpenMP::OpenMP_Fortran )
endif()
