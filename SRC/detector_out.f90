subroutine detector_out()
! Output for detector
! FY @ MTU 2017
use vars
use params
implicit none
integer :: i,j
integer :: rki
integer :: nbuf
character(len=5) :: filenm
if (nrestart.eq.0) then
! RTD
 do i=1,ndet_rtd
  rki=plocation_rtd(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/RTD'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_rtd(i,j,:)
   end do
   close(69)
  end if
 end do
! Sonic u
 do i=1,ndet_sonicu
  rki=plocation_sonicu(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Sonicu'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_sonicu(i,j,:)
   end do
   close(69)
  end if
 end do
! Sonic v
 do i=1,ndet_sonicv
  rki=plocation_sonicv(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Sonicv'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,105E14.5)') det_sonicv(i,j,:)
   end do
   close(69)
  end if
 end do
! Sonic w
 do i=1,ndet_sonicw
  rki=plocation_sonicw(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Sonicw'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_sonicw(i,j,:)
   end do
   close(69)
  end if
 end do
! Licor
 do i=1,ndet_licor
  rki=plocation_licor(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Licor'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_licor(i,j,:)
   end do
   close(69)
  end if
 end do
! Thermistors
 do i=1,ndet_thermistors
  rki=plocation_thermistors(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Thermistors'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_thermistors(i,j,:)
   end do
   close(69)
  end if
 end do
! Virtual
 do i=1,ndet_virtual
  rki=plocation_virtual(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Virtual'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_virtual(i,j,:)
   end do
   close(69)
  end if
 end do
end if

if (nrestart.eq.1) then
! RTD
 do i=1,ndet_rtd
  rki=plocation_rtd(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/RTD'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_rtd(i,j,:)
   end do
   close(69)
  end if
 end do
! Sonic u
 do i=1,ndet_sonicu
  rki=plocation_sonicu(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Sonicu'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_sonicu(i,j,:)
   end do
   close(69)
  end if
 end do
! Sonic v
 do i=1,ndet_sonicv
  rki=plocation_sonicv(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Sonicv'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_sonicv(i,j,:)
   end do
   close(69)
  end if
 end do
! Sonic w
 do i=1,ndet_sonicw
  rki=plocation_sonicw(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Sonicw'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_sonicw(i,j,:)
   end do
   close(69)
  end if
 end do
! Licor
 do i=1,ndet_licor
  rki=plocation_licor(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Licor'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_licor(i,j,:)
   end do
   close(69)
  end if
 end do
! Thermistors
 do i=1,ndet_thermistors
  rki=plocation_thermistors(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Thermistors'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_thermistors(i,j,:)
   end do
   close(69)
  end if
 end do
! Virtual
 do i=1,ndet_virtual
  rki=plocation_virtual(i,2)
  nbuf=nstat/ndetout
  if (rank.eq.rki) then
   write(filenm,"(I3.3)") i
   open(69,file='./OUT_DET/Virtual'//trim(filenm)//'.txt',position='append',status='old')
   do j=1,nbuf
     write(69,'(E14.7,10E14.5)') det_virtual(i,j,:)
   end do
   close(69)
  end if
 end do
end if

end subroutine
