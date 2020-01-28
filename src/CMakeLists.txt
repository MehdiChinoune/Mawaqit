
set( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules )

set( srcs "main.F90" "kinds.F90" "constants.F90" "times.F90" "time_date.F90" "geometry.F90" )

add_executable( main ${srcs} )
