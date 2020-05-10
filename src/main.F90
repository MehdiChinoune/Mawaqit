program main
  use kinds, only : wp
  use constants, only : deg, month_days
  use times, only : init, prayers, prayer_times, extra_times, other_times
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
  type(extra_times) :: okhra
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
  open( newunit = ou_unit, file = "output.csv", status = "replace", action = "write" )
  print*, "Day       : Fajr     Sunrise  Dhuhur   Asr      Maghrib  Ishaa"
  write( ou_unit, '(A67)' ) "Day        ,Fajr     ,Sunrise  ,Dhuhur   ,Asr      ,Maghrib  ,Ishaa"
  !
  allocate( h(3600) )
  call surrounding( longitude, latitude, h )
  !
  i = day_start%days
  month = day_start%months
  year = day_start%years
  if( month==day_stop%months ) then
    i_f = day_stop%days
  else
    i_f = month_days(month)
  end if
  !
  do while(i<=i_f)
    if( month==2 .and. mod(year,4)/=0 .and. i==29 ) cycle
    day = dat(i,month,year)
    call init(time_zone, longitude, day)
    !
    call prayer_times(latitude,waqt)
    print'(2(i2.2,"/"),i4," : ",6(2(i2.2,":"),i2.2,1x))', day%days, day%months, day%years, &
      to_hms(waqt%Fajr), to_hms(waqt%Sunrise), to_hms(waqt%Dhuhur), to_hms(waqt%Asr), &
      to_hms(waqt%Maghrib), to_hms(waqt%Ishaa)
    write( ou_unit, '( 2(i2.2,"/"), i4, 6(1x,",",2(i2.2,":"),i2.2) )') &
      day%days, day%months, day%years, &
      to_hms(waqt%Fajr), to_hms(waqt%Sunrise), to_hms(waqt%Dhuhur), to_hms(waqt%Asr), &
      to_hms(waqt%Maghrib), to_hms(waqt%Ishaa)
    !
    call other_times(latitude,okhra,h)
    print'(2(i2.2,"/"),i4," : ",2(2(i2.2,":"),i2.2,1x),18x,2(2(i2.2,":"),i2.2,1x))', &
      day%days, day%months, day%years, &
      to_hms(okhra%fajr_correct), to_hms(okhra%sunrise), to_hms(okhra%sunset), &
      to_hms(okhra%ishaa_correct)
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
    !
  end do
  !
  close( ou_unit )
  !
end program main
