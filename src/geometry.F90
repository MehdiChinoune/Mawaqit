module geometry
  use kinds, only : wp
  implicit none
  !
contains
  !
  subroutine surrounding( longitude, latitude, h )
    use constants, only : pi, deg, earth_radius_equator, &
      circ => earth_circumference_equator
    implicit none
    real(wp), intent(in) :: longitude, latitude
    real(wp), intent(out) :: h(:)
    ! Length of latitudinal degree
    real(wp), parameter :: d_lat = earth_radius_equator *deg
    real(wp), parameter :: d_max = 80000._wp
    integer, parameter :: nx1 = 3600, ny1 = 3600
    !
    integer(2), allocatable :: heights(:,:)
    integer :: iu, lon_min, lon_max, lat_min, lat_max, nx, ny
    character :: dir(2) ! direction( north or south, east or west )
    character(16) :: filename
    !
    integer :: i0, j0, i, j, i1, j1, k, imax, jmax, kmax
    real(wp) :: d_lon, theta, d_lat1, d_lon1, d, lat_deg, long_deg
    real(wp) :: hmax, phi, h1, radius, arc
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
    nx = size( heights, 1 ) - 1
    ny = size( heights, 2 ) - 1
    do i = lon_min, lon_max
      do j = lat_max, lat_min, -1
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
        write( filename, '("data/",A1,i2.2,A1,i3.3,".hgt")' ) dir(1), j, dir(2), i
        open( newunit = iu, file = filename, action = "read", status = "old", &
          form = "unformatted", access = "stream" )
        read( iu ) heights( (i-lon_min)*nx1:(i-lon_min+1)*nx1, &
          (lat_max-j)*ny1:(lat_max-j+1)*ny1 )
        close( iu )
      end do
    end do
    !
    i0 = int( modulo( long_deg, 1._wp ) * nx1 ) + (int(long_deg)-lon_min)*nx1
    j0 = int( (1._wp - modulo( lat_deg, 1._wp ) ) * ny1 ) + (lat_max-int(lat_deg))*ny1
    !
    imax = int( d_max/d_lon1 )
    jmax = int( d_max/d_lat1 )
    !
    h = 0._wp
    !
    !$omp parallel do private(theta, hmax, kmax, d, radius, k, i1, j1, arc, phi, h1)
    do i = 1, size(h)
      theta = i * 2._wp*pi/size(h)
      hmax = 0._wp
      kmax = hypot( jmax*cos(theta), imax*sin(theta) )
      d = d_max/kmax
      ! Calculating the new radius
      block
        real(wp) :: a(3), b(3), c(3), o(3)
        real(wp) :: ac(3), bc(3), normal(3)
        real(wp) :: tmp1, tmp2, sigma
        !
        a = earth_radius_equator * [ cos(longitude)*cos(latitude), &
          sin(longitude)*cos(latitude), sin(latitude) ]
        b = [ -a(1), -a(2), a(3) ]
        !
        tmp1 = d_max*cos(theta)/circ *2._wp*pi
        tmp2 = d_max*sin(theta)/circ *2._wp*pi
        c = earth_radius_equator * [ cos(longitude+tmp2)*cos(latitude+tmp1), &
          sin(longitude+tmp2)*cos(latitude+tmp1), sin(latitude+tmp1) ]
        !
        ac = c-a
        bc = c-b
        normal = cross_product( ac, bc )
        sigma = -dot_product( normal, a )
        !
        o = - normal*sigma/norm2(normal)**2
        radius = norm2(c-o)
      end block
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
        phi = arc/radius
        h1 = radius* ( 1._wp - 1._wp/cos(phi) )
        !
        h(i) = max( h(i), atan2( heights(i1, j1)-heights(i0,j0)-2-h1, arc ) )
      end do
    end do
    !
  end subroutine surrounding

  ! Cross product of two vectors \vec{u} and \vec{v}
  pure function cross_product( u, v )
    real(wp) :: cross_product(3)
    real(wp), intent(in) :: u(3), v(3)
    !
    cross_product = [ u(2)*v(3)-u(3)*v(2), u(3)*v(1)-u(1)*v(3), u(1)*v(2)-u(2)*v(1) ]
    !
  end function cross_product
  !
end module geometry
