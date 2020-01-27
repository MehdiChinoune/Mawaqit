module times
  use kinds, only : wp
  use constants, only : pi, deg
  implicit none
  !
  real(wp), parameter :: fajr_angle = 18._wp
  real(wp), parameter :: ishaa_angle = 17.1_wp
  real(wp), parameter :: sunrise_angle = 17.1_wp
  !
  real(wp), protected :: dec      ! Sun's Declination
  integer, protected :: mid_day   ! Midday time
  integer, protected :: eq_time   ! Equation of time
  logical, protected :: initiated = .false.
  ! Prayer times
  type prayers
    integer :: fajr, dhuhur, asr, maghrib, ishaa, sunrise
  end type prayers

contains

  subroutine prayer_times(latitude,times)
    real(wp), intent(in) :: latitude
    type(prayers), intent(out) :: times
    !
    real(wp) :: theta
    !
    ! Check if the common variables was calculated
    if( .not. initiated ) error stop "You should call init(...) before"
    ! Dhuhur
    times%dhuhur = mid_day + 30
    ! Asr
    theta = atan( 1._wp/( 1._wp + tan(latitude-dec) ) )
    times%asr = mid_day + time_angle(latitude,-theta)
    ! Maghrib (Sunset)
    times%maghrib = mid_day + time_angle(latitude,1.465*deg)
    ! Sunrise
    times%sunrise = mid_day - time_angle(latitude,0.77*deg)
    ! Fajr
    times%fajr = mid_day - time_angle(latitude,18.*deg)
    ! Ishaa
    times%ishaa = mid_day + time_angle(latitude,17.1*deg)
    !
  end subroutine prayer_times

  ! Calculate azimuth angle for relative sun's angle (to the horizon)
  real(wp) function azimuth(latitude,angle)
    real(wp), intent(in) :: latitude, angle
    real(wp) :: alpha
    !
    alpha = asin( sin(dec)*sin(latitude)+cos(dec)*cos(latitude)*cos(angle) )
    azimuth = acos( (sin(dec)*cos(latitude)-cos(dec)*sin(latitude)*cos(angle)) &
      /cos(alpha) ) - dec
    !
  end function azimuth

  ! Calculate the time (relative to Midday) corresponding to sun's angle
  ! (relative to the horizon)
  integer function time_angle(latitude,theta)
    real(wp), intent(in) :: latitude, theta
    time_angle = acos( ( -sin(theta)-sin(latitude)*sin(dec) ) &
      /( cos(latitude)*cos(dec) ) )/deg*240
  end function time_angle

  ! Calculate the common variables : Equation of time, Declination, Midday
  subroutine init(time_zone,longitude,day)
    use time_date, only : dat, julian_day
    integer, intent(in) :: time_zone
    real(wp), intent(in) :: longitude
    type(dat), intent(in) :: day
    !
    integer :: n ! Number of days since 01/01/2000
    real(wp) :: l, g, lambda, eps, alpha
    !
    n = julian_day(day) - 2451545
    l = mod( 280.459_wp+0.98564736_wp*n, 360._wp )*deg
    g = mod( 357.529_wp+0.98560028_wp*n, 360._wp )*deg
    lambda =  l + ( 1.915_wp*sin(g)+0.02_wp*sin(2*g) )*deg
    eps = ( 23.439_wp+3.6e-7_wp*n )*deg
    alpha = atan2( cos(eps)*sin(lambda), cos(lambda) )
    !
    dec = asin( sin(eps)*sin(lambda) )
    eq_time = ( (l-alpha)/deg - 180*nint((l-alpha)/deg/180) )*240
    mid_day = 12*3600 + ( time_zone*15 - longitude )*240 - eq_time
    initiated = .true.
    !
  end subroutine init

end module times
