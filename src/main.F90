program main
  use kinds, only : wp
  use constants, only : deg, month_days
  use times, only : prayer_times, init, prayers
  use time_date, only : to_hms, dat
  use geometry, only : surrounding
  implicit none
  !
  integer :: in_unit, ou_unit
  integer :: time_zone
  real(wp) :: longitude, latitude
  type(dat) :: day_start, day_stop
  !
  integer :: i, i_f, month, year
  real(wp), allocatable :: h(:)
  type(dat) :: day
  type(prayers) :: waqt
  !
  open( newunit = in_unit, file = "data.dat", status = "old", action = "read" )
  !
  read( in_unit, * ) time_zone
  read( in_unit, * ) longitude
  read( in_unit, * ) latitude
  read( in_unit, * ) day_start
  read( in_unit, * ) day_stop
  longitude = longitude*deg
  latitude = latitude*deg
  !
  close( in_unit )
  !
  open( newunit = ou_unit, file = "output.dat", status = "replace", action = "write" )
  print*, "day       : fajr     sunrise  dhuhur   asr      maghrib  ishaa"
  write( ou_unit, '(A61)' ) "Day        Fajr     Sunrise  Dhuhur   Asr      Maghrib  Ishaa"
  !
  allocate( h(3600) )
  call surrounding( longitude, latitude, h )
  !
  i = day_start%days
  month = day_start%months
  year = day_start%years
  i_f = month_days(month)
  do while(i<=i_f)
    if( month==2 .and. mod(year,4)/=0 .and. i==29 ) cycle
    day = dat(i,month,year)
    call init(time_zone, longitude, day)
    call prayer_times(latitude,waqt,h,.true.)
    print'(2(i2.2,"/"),i4," : ",6(2(i2.2,":"),i2.2,1x))', day%days, day%months, day%years, &
      to_hms(waqt%fajr), to_hms(waqt%sunrise), to_hms(waqt%dhuhur), &
      to_hms(waqt%asr), to_hms(waqt%maghrib), to_hms(waqt%ishaa)
    call prayer_times(latitude,waqt,h)
    print'(2(i2.2,"/"),i4," : ",6(2(i2.2,":"),i2.2,1x))', day%days, day%months, day%years, &
      to_hms(waqt%fajr), to_hms(waqt%sunrise), to_hms(waqt%dhuhur), &
      to_hms(waqt%asr), to_hms(waqt%maghrib), to_hms(waqt%ishaa)
    write( ou_unit, '( 2(i2.2,"/"), i4, 1x, 6(2(i2.2,":"),i2.2,1x) )') &
      day%days, day%months, day%years, &
      to_hms(waqt%fajr), to_hms(waqt%sunrise), to_hms(waqt%dhuhur), &
      to_hms(waqt%asr), to_hms(waqt%maghrib), to_hms(waqt%ishaa)
    !
    i = i + 1
    if( i>i_f .and. month/=day_stop%months ) then
      i = 1
      month = month + 1
      if( month==13 .and. year/=day_stop%years ) then
        year = year + 1
        month = 1
      end if
      if( month<day_stop%months ) then
        i_f = month_days(month)
      else
        i_f = day_stop%days
      end if
    end if
  end do
  !
  close( ou_unit )
  !
end program main
