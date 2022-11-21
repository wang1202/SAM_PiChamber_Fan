! Create file for Thermistors
! FY @ MTU 2017
subroutine detector_inithermistors()
use vars
use params
implicit none
integer :: i
integer :: rki
character(len=5) :: filenm
if (nrestart.eq.0) then
 do i=1,ndet_thermistors
  rki = plocation_thermistors(i,2)
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Thermistors'//trim(filenm)//'.txt',status='new')
   close(69)
  end if
 end do
end if
end subroutine
