module time_date
  use kinds, only : wp
  implicit none
  ! date type
  type dat
    integer :: years, months, days
  end type dat
  ! time type
  type time
    integer :: hours, minutes, seconds
  end type time

contains

  ! Formula to claculate julian day
  integer function julian_day(day)
    type(dat), intent(in) :: day
    integer :: a, b, y, m, d
    !
    d = day%days
    m = day%months
    y = day%years
    !
    a = int(y/100)
    b = 2 - a + int(a/4)
    !
    if(m<3) then
      y = y-1
      m = m+12
    end if
    julian_day = int(365.25_wp*(y+4716)) + int(30.6001_wp*(m+1)) + d + b - 1523.5_wp
    !
  end function julian_day

  ! Convert from seconds to hh:mm:ss
  type(time) function to_hms(seconds)
    integer, intent(in) :: seconds
    !
    to_hms%hours = seconds/3600
    to_hms%minutes = seconds/60 - to_hms%hours*60
    to_hms%seconds = seconds - 60*(to_hms%hours*60+to_hms%minutes)
    !
  end function to_hms

end module time_date
