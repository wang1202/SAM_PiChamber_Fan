! Create file for Sonic
! FY @ MTU 2017
subroutine detector_inisonic()
use vars
use params
implicit none
integer :: i
integer :: rki
character(len=5) :: filenm
if (nrestart.eq.0) then
 do i=1,ndet_sonicu
  rki = plocation_sonicu(i,2)
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Sonicu'//trim(filenm)//'.txt',status='new')
   close(69)
  end if
 end do
 do i=1,ndet_sonicv
  rki = plocation_sonicv(i,2)
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Sonicv'//trim(filenm)//'.txt',status='new')
   close(69)
  end if
 end do
 do i=1,ndet_sonicw
  rki = plocation_sonicw(i,2)
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Sonicw'//trim(filenm)//'.txt',status='new')
   close(69)
  end if
 end do
end if
end subroutine
