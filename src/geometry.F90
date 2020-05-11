module geometry
  use kinds, only : wp
  implicit none
  real(wp), parameter :: d_max = 120000._wp
  !
contains
  !
  subroutine surrounding( longitude, latitude, h )
    use iso_fortran_env, only : int16
    use constants, only : pi, deg, earth_radius_equator
    implicit none
    real(wp), intent(in) :: longitude, latitude
    real(wp), intent(out) :: h(:)
    ! Length of latitudinal degree
    real(wp), parameter :: d_lat = earth_radius_equator *deg
    integer, parameter :: nx1 = 3600, ny1 = 3600
    !
    integer(int16), allocatable :: heights(:,:)
    integer :: lon_min, lon_max, lat_min, lat_max, nx, ny
    !
    integer :: i0, j0, i, j, i1, j1, k, imax, jmax, kmax
    real(wp) :: d_lon, theta, d_lat1, d_lon1, d, lat_deg, long_deg
    real(wp) :: hmax, phi, h0, h1, h2, r, arc
    ! longitude and latitude in degrees
    long_deg = longitude/deg
    lat_deg = latitude/deg
    ! Length of one longitudinal degree
    d_lon = earth_radius_equator *cos( latitude ) *deg
    !
    d_lon1 = d_lon/nx1
    lon_min = int( long_deg - d_max/d_lon )
    lon_max = int( long_deg + d_max/d_lon )
    d_lat1 = d_lat/ny1
    lat_min = int( lat_deg - d_max/d_lat )
    lat_max = int( lat_deg + d_max/d_lat )
    !
    allocate( heights(0:(lon_max-lon_min+1)*nx1, 0:(lat_max-lat_min+1)*ny1 ) )
    heights = 0_int16
    nx = size( heights, 1 ) - 1
    ny = size( heights, 2 ) - 1
    do i = lon_min, lon_max
      do j = lat_max, lat_min, -1
        call get_data( i, j, heights( (i-lon_min)*nx1:(i-lon_min+1)*nx1, &
          (j-lat_min)*ny1:(j-lat_min+1)*ny1 ) )
      end do
    end do
    !
    i0 = int( modulo( long_deg, 1._wp ) * nx1 ) + (int(long_deg)-lon_min)*nx1
    j0 = int( modulo( lat_deg, 1._wp ) * ny1 ) + (int(lat_deg)-lat_min)*ny1
    h0 = heights(i0,j0) + 2
    !
    imax = int( d_max/d_lon1 )
    jmax = int( d_max/d_lat1 )
    !
    h = 0._wp
    !
    !$omp parallel do private(theta, hmax, kmax, d, r, k, i1, j1, arc, phi, h1, h2)
    do i = 1, size(h)
      theta = -i * 2._wp*pi/size(h)
      hmax = 0._wp
      kmax = hypot( jmax*cos(theta), imax*sin(theta) )
      d = d_max/kmax
      r = radius( longitude, latitude, theta )
      !
      do k = 1, kmax
        i1 = i0 - nint( k*d*sin(theta)/d_lon1 )
        if( i1>=nx ) i1 = nx
        if( i1<=0 ) i1 = 0
        !
        j1 = j0 + nint( k*d*cos(theta)/d_lat1 )
        if( j1>=ny ) j1 = ny
        if( j1<=0 ) j1 = 0
        !
        arc = hypot( (i1-i0)*d_lon1, (j1-j0)*d_lat1 )
        phi = arc/r
        h1 = (r+h0)* ( 1._wp/cos(phi) - 1._wp )
        h2 = heights(i1,j1) - ( h0 + h1 )
        !
        h(i) = max( h(i), atan2( h2, ( r+heights(i1,j1) )* tan(phi) ) )
      end do
      !
    end do
    !
  end subroutine surrounding

  ! Get Data
  subroutine get_data( i, j, heights )
    use iso_fortran_env, only : int16
    use hdf5, only : h5open_f, h5close_f, h5fopen_f, h5fclose_f, h5dopen_f, &
      h5dclose_f, h5dread_f, h5f_acc_rdonly_f, h5t_std_i16le, hid_t, hsize_t
    integer, intent(in) :: i, j
    integer(int16), intent(out) :: heights(:,:)
    !
    character(*), parameter :: dset_name = "SRTMGL1_DEM"
    !
    character :: dir(2) ! direction( north or south, east or west )
    character(21) :: filename
    logical :: file_found
    integer :: down_stat
    !
    integer(hid_t) :: file_id, dset_id
    integer :: error, ny
    !
    if( j==37 .and. i>=0 .and. i<=5 ) return
    ! North or South
    if( j>=0 ) then
      dir(1) = "N"
    else
      dir(1) = "S"
    end if
    ! East or West
    if( i>=0 ) then
      dir(2) = "E"
    else
      dir(2) = "W"
    end if
    !
    write( filename, '(A1,i2.2,A1,i3.3,".SRTMGL1_NC.nc")' ) dir(1), abs(j), &
      dir(2), abs(i)
    inquire( file = "data/"//filename, exist = file_found )
    if( .not. file_found ) then
      call execute_command_line( &
        "wget https://e4ftl01.cr.usgs.gov/MEASURES/SRTMGL1_NC.003/2000.02.11/"&
        &//filename//" -P ./data -nv", cmdstat=down_stat )
      if( down_stat==0 ) then
        print*, "Failed to download "//filename
        return
      end if
    end if
    !
    ! Initialize Fortran Interface
    call h5open_f(error)
    ! Open File
    call h5fopen_f("data/"//filename, h5f_acc_rdonly_f, file_id, error)
    ! Open Dataset
    call h5dopen_f(file_id, dset_name, dset_id, error )
    !
    ! Read Data
    ny = size( heights, 2 )
    call h5dread_f( dset_id, h5t_std_i16le, heights, shape(heights,hsize_t) , error )
    heights( :, 1:ny ) = heights( :, ny:1:-1 )
    !
    ! Close Dataset
    call h5dclose_f(dset_id, error)
    ! Close File
    call h5fclose_f(file_id, error)
    ! Close Fortran Interface
    call h5close_f(error)
    !
  end subroutine get_data

  ! Calculating the new radius of a given plan
  pure real(wp) function radius( longitude, latitude, theta)
    use constants, only : pi, circumference => earth_circumference_equator, &
      earth_radius_equator
    real(wp), intent(in) :: longitude, latitude, theta
    real(wp) :: a(3), b(3), c(3), o(3)
    real(wp) :: ac(3), bc(3), normal(3)
    real(wp) :: tmp1, tmp2, sigma
    !
    ! Points of intersection A nand B of latitude circle with crossing plan.
    a = earth_radius_equator * [ cos(longitude)*cos(latitude), &
      sin(longitude)*cos(latitude), sin(latitude) ]
    b = [ -a(1), -a(2), a(3) ]
    ! Third point C which belongs to crossing plan
    tmp1 = d_max*cos(theta)/circumference *2._wp*pi
    tmp2 = d_max*sin(theta)/circumference *2._wp*pi
    c = earth_radius_equator * [ cos(longitude+tmp2)*cos(latitude+tmp1), &
      sin(longitude+tmp2)*cos(latitude+tmp1), sin(latitude+tmp1) ]
    ! \vec{AC}, \vec{BC}, their normal \vec{N}
    ac = c-a
    bc = c-b
    normal = cross_product( ac, bc )
    sigma = -dot_product( normal, a )
    ! The center of crossing circle
    o = - normal*sigma/norm2(normal)**2
    ! Radius
    radius = norm2(c-o)
    !
  end function radius

  pure function cross_product( u, v )
    real(wp) :: cross_product(3)
    real(wp), intent(in) :: u(3), v(3)
    !
    cross_product = [ u(2)*v(3)-u(3)*v(2), u(3)*v(1)-u(1)*v(3), u(1)*v(2)-u(2)*v(1) ]
    !
  end function cross_product
  !
end module geometry
