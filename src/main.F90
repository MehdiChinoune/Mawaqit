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
  print*, "day       : fajr     sunrise  dhuhur   asr      maghrib  ishaa"
  write( ou_unit, '(A61)' ) "Day        Fajr     Sunrise  Dhuhur   Asr      Maghrib  Ishaa"
  !
  do i = 1, month_days(month)
    if( month==2 .and. mod(year,4)/=0 .and. i==29 ) cycle
    day = dat(year,month,i)
    call init(time_zone, longitude, day)
    call prayer_times(latitude,waqt)
    print'(2(i2.2,"/"),i4," : ",6(2(i2.2,":"),i2.2,1x))', day%days, day%months, day%years, &
      to_hms(waqt%fajr), to_hms(waqt%sunrise), to_hms(waqt%dhuhur), &
      to_hms(waqt%asr), to_hms(waqt%maghrib), to_hms(waqt%ishaa)
    write( ou_unit, '( 2(i2.2,"/"), i4, 1x, 6(2(i2.2,":"),i2.2,1x) )') &
      day%days, day%months, day%years, &
      to_hms(waqt%fajr), to_hms(waqt%sunrise), to_hms(waqt%dhuhur), &
      to_hms(waqt%asr), to_hms(waqt%maghrib), to_hms(waqt%ishaa)
  end do
  !
  close( ou_unit )
  !
end program main
