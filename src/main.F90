program main
  use kinds, only : wp
  use constants, only : deg, month_days
  use times, only : prayer_times, init, prayers, azimuth
  use time_date, only : to_hms, dat
  implicit none
  !
  integer :: in_unit, ou_unit
  integer :: time_zone, month, year
  real(wp) :: longitude, latitude
  integer :: i
  type(dat) :: day
  type(prayers) :: waqt
  !
  open( newunit = in_unit, file = "data.dat", status = "old", action = "read" )
  !
  read( in_unit, * ) time_zone
  read( in_unit, * ) longitude
  read( in_unit, * ) latitude
  read( in_unit, * ) month, year
  latitude = latitude*deg
  !
  close( in_unit )
  !
  open( newunit = ou_unit, file = "output.dat", status = "replace", action = "write" )
  !
  do i = 1, month_days(month)
    if( month==2 .and. mod(year,4)/=0 .and. i==29 ) cycle
    day = dat(year,month,i)
    call init(time_zone, longitude, day)
    call prayer_times(latitude,waqt)
    print*, day
    print'("fajr   :",2(i2,":"),i2,1x)',to_hms(waqt%fajr)
    print'("sunrise:",2(i2,":"),i2,1x)',to_hms(waqt%sunrise)
    print'("dhuhur :",2(i2,":"),i2,1x)',to_hms(waqt%dhuhur)
    print'("asr    :",2(i2,":"),i2,1x)',to_hms(waqt%asr)
    print'("maghrib:",2(i2,":"),i2,1x)',to_hms(waqt%maghrib)
    print'("ishaa  :",2(i2,":"),i2,1x)',to_hms(waqt%ishaa)
    write( ou_unit, *) day
    write( ou_unit, '("fajr   :",2(i2,":"),i2,1x)' ) to_hms(waqt%fajr)
    write( ou_unit, '("sunrise:",2(i2,":"),i2,1x)' ) to_hms(waqt%sunrise)
    write( ou_unit, '("dhuhur :",2(i2,":"),i2,1x)' ) to_hms(waqt%dhuhur)
    write( ou_unit, '("asr    :",2(i2,":"),i2,1x)' ) to_hms(waqt%asr)
    write( ou_unit, '("maghrib:",2(i2,":"),i2,1x)' ) to_hms(waqt%maghrib)
    write( ou_unit, '("ishaa  :",2(i2,":"),i2,1x)' ) to_hms(waqt%ishaa)
  end do
  !
  close( ou_unit )
  !
end program main
