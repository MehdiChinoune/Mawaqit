module kinds
  implicit none
  integer, parameter :: sp = selected_real_kind(6,37)    ! single precision
  integer, parameter :: dp = selected_real_kind(15,307)  ! double precison
  integer, parameter :: xp = selected_real_kind(18,4931) ! extended-double precision
  integer, parameter :: qp = selected_real_kind(33,4931) ! quadruple precision
  integer, parameter :: wp = sp ! working precision
end module kinds
