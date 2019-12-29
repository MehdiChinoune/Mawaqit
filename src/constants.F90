module constants
  use kinds, only : wp
  implicit none
  ! \pi
  real(wp), parameter :: pi = 3.141592653589793238_wp
  ! one degree in radian
  real(wp), parameter :: deg = pi/180
  ! Earth's circumference at the equator in meter (m)
  real(wp), parameter :: earth_circumference_equator = 40075017._wp
  ! Earth's radius at the equator in meter (m)
  real(wp), parameter :: earth_radius_equator = earth_circumference_equator / (2._wp*pi)
  ! Earth's circumference at the pole in meter (m)
  real(wp), parameter :: earth_circumference_pole = 40007863._wp
  ! Earth's radius at the pole in meter (m)
  real(wp), parameter :: earth_radius_pole = earth_circumference_pole / (2._wp*pi)
  ! Number of days in each month
  integer, parameter :: month_days(12) = [ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
  !
end module constants
