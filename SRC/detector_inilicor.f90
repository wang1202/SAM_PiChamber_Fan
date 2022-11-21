! Create file for Licor
! FY @ MTU 2017
subroutine detector_inilicor()
use vars
use params
implicit none
integer :: i
integer :: rki
character(len=5) :: filenm
if (nrestart.eq.0) then
 do i=1,ndet_licor
  rki = plocation_licor(i,2)
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Licor'//trim(filenm)//'.txt',status='new')
   close(69)
  end if
 end do
end if
end subroutine
