! Create file for RTD
! FY @ MTU 2017
subroutine detector_inirtd()
use vars
use params
implicit none
integer :: i
integer :: rki
character(len=5) :: filenm
if (nrestart.eq.0) then
 do i=1,ndet_rtd
  rki = plocation_rtd(i,2)
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/RTD'//trim(filenm)//'.txt',status='new')
   close(69)
  end if
 end do
end if
end subroutine
