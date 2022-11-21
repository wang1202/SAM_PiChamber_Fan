! main code for detector
! FY @ MTU 2017
subroutine detector(dbuf)
use vars
use params
use microphysics
implicit none
integer i,j
integer :: rki,nxi,nyi,nzi
integer :: dbuf
real ffncns, ffcds
ffncns = 0.d0
ffcds = 0.d0
call t_startf('detector')
! RTD
do i=1,ndet_rtd
 rki=plocation_rtd(i,2)
 nxi=plocation_rtd(i,3)
 nyi=plocation_rtd(i,4)
 nzi=plocation_rtd(i,5)
 if (rank.eq.rki) then
  det_rtd(i,dbuf,1)=time
  det_rtd(i,dbuf,2)=0.5*(u(nxi,nyi,nzi)+u(nxi+1,nyi,nzi))
  det_rtd(i,dbuf,3)=0.5*(v(nxi,nyi,nzi)+v(nxi,nyi+1,nzi))
  det_rtd(i,dbuf,4)=0.5*(w(nxi,nyi,nzi)+w(nxi,nyi,nzi+1))
  det_rtd(i,dbuf,5)=tabs(nxi,nyi,nzi)
  det_rtd(i,dbuf,6)=qv(nxi,nyi,nzi)
  det_rtd(i,dbuf,7)=qna(nxi,nyi,nzi)
  det_rtd(i,dbuf,8)=qnc(nxi,nyi,nzi)
  det_rtd(i,dbuf,9)=rmean(nxi,nyi,nzi)
  det_rtd(i,dbuf,10)=reffc(nxi,nyi,nzi)
 end if
end do
! Sonic u
do i=1,ndet_sonicu
 rki=plocation_sonicu(i,2)
 nxi=plocation_sonicu(i,3)
 nyi=plocation_sonicu(i,4)
 nzi=plocation_sonicu(i,5)
 if (rank.eq.rki) then
  det_sonicu(i,dbuf,1)=time
  det_sonicu(i,dbuf,2)=0.5*(u(nxi,nyi,nzi)+u(nxi+1,nyi,nzi))
  det_sonicu(i,dbuf,3)=0.5*(v(nxi,nyi,nzi)+v(nxi,nyi+1,nzi))
  det_sonicu(i,dbuf,4)=0.5*(w(nxi,nyi,nzi)+w(nxi,nyi,nzi+1))
  det_sonicu(i,dbuf,5)=tabs(nxi,nyi,nzi)
  det_sonicu(i,dbuf,6)=qv(nxi,nyi,nzi)
  det_sonicu(i,dbuf,7)=qna(nxi,nyi,nzi)
  det_sonicu(i,dbuf,8)=qnc(nxi,nyi,nzi)
  det_sonicu(i,dbuf,9)=rmean(nxi,nyi,nzi)
  det_sonicu(i,dbuf,10)=reffc(nxi,nyi,nzi)
 end if
end do
! Sonic v
do i=1,ndet_sonicv
 rki=plocation_sonicv(i,2)
 nxi=plocation_sonicv(i,3)
 nyi=plocation_sonicv(i,4)
 nzi=plocation_sonicv(i,5)
 if (rank.eq.rki) then
  det_sonicv(i,dbuf,1)=time
  det_sonicv(i,dbuf,2)=0.5*(u(nxi,nyi,nzi)+u(nxi+1,nyi,nzi))
  det_sonicv(i,dbuf,3)=0.5*(v(nxi,nyi,nzi)+v(nxi,nyi+1,nzi))
  det_sonicv(i,dbuf,4)=0.5*(w(nxi,nyi,nzi)+w(nxi,nyi,nzi+1))
  det_sonicv(i,dbuf,5)=tabs(nxi,nyi,nzi)
  det_sonicv(i,dbuf,6)=qv(nxi,nyi,nzi)
  det_sonicv(i,dbuf,7)=qna(nxi,nyi,nzi)
  det_sonicv(i,dbuf,8)=qnc(nxi,nyi,nzi)
  det_sonicv(i,dbuf,9)=rmean(nxi,nyi,nzi)
  det_sonicv(i,dbuf,10)=reffc(nxi,nyi,nzi)
 end if
end do
! Sonic w
do i=1,ndet_sonicw
 rki=plocation_sonicw(i,2)
 nxi=plocation_sonicw(i,3)
 nyi=plocation_sonicw(i,4)
 nzi=plocation_sonicw(i,5)
 if (rank.eq.rki) then
  det_sonicw(i,dbuf,1)=time
  det_sonicw(i,dbuf,2)=0.5*(u(nxi,nyi,nzi)+u(nxi+1,nyi,nzi))
  det_sonicw(i,dbuf,3)=0.5*(v(nxi,nyi,nzi)+v(nxi,nyi+1,nzi))
  det_sonicw(i,dbuf,4)=0.5*(w(nxi,nyi,nzi)+w(nxi,nyi,nzi+1))
  det_sonicw(i,dbuf,5)=tabs(nxi,nyi,nzi)
  det_sonicw(i,dbuf,6)=qv(nxi,nyi,nzi)
  det_sonicw(i,dbuf,7)=qna(nxi,nyi,nzi)
  det_sonicw(i,dbuf,8)=qnc(nxi,nyi,nzi)
  det_sonicw(i,dbuf,9)=rmean(nxi,nyi,nzi)
  det_sonicw(i,dbuf,10)=reffc(nxi,nyi,nzi)
 end if
end do
! Licor
do i=1,ndet_licor
 rki=plocation_licor(i,2)
 nxi=plocation_licor(i,3)
 nyi=plocation_licor(i,4)
 nzi=plocation_licor(i,5)
 if (rank.eq.rki) then
  det_licor(i,dbuf,1)=time
  det_licor(i,dbuf,2)=0.5*(u(nxi,nyi,nzi)+u(nxi+1,nyi,nzi))
  det_licor(i,dbuf,3)=0.5*(v(nxi,nyi,nzi)+v(nxi,nyi+1,nzi))
  det_licor(i,dbuf,4)=0.5*(w(nxi,nyi,nzi)+w(nxi,nyi,nzi+1))
  det_licor(i,dbuf,5)=tabs(nxi,nyi,nzi)
  det_licor(i,dbuf,6)=qv(nxi,nyi,nzi)
  det_licor(i,dbuf,7)=qna(nxi,nyi,nzi)
  det_licor(i,dbuf,8)=qnc(nxi,nyi,nzi)
  det_licor(i,dbuf,9)=rmean(nxi,nyi,nzi)
  det_licor(i,dbuf,10)=reffc(nxi,nyi,nzi)
 end if
end do
! Thermistors
do i=1,ndet_thermistors
 rki=plocation_thermistors(i,2)
 nxi=plocation_thermistors(i,3)
 nyi=plocation_thermistors(i,4)
 nzi=plocation_thermistors(i,5)
 if (rank.eq.rki) then
  det_thermistors(i,dbuf,1)=time
  det_thermistors(i,dbuf,2)=0.5*(u(nxi,nyi,nzi)+u(nxi+1,nyi,nzi))
  det_thermistors(i,dbuf,3)=0.5*(v(nxi,nyi,nzi)+v(nxi,nyi+1,nzi))
  det_thermistors(i,dbuf,4)=0.5*(w(nxi,nyi,nzi)+w(nxi,nyi,nzi+1))
  det_thermistors(i,dbuf,5)=tabs(nxi,nyi,nzi)
  det_thermistors(i,dbuf,6)=qv(nxi,nyi,nzi)
  det_thermistors(i,dbuf,7)=qna(nxi,nyi,nzi)
  det_thermistors(i,dbuf,8)=qnc(nxi,nyi,nzi)
  det_thermistors(i,dbuf,9)=rmean(nxi,nyi,nzi)
  det_thermistors(i,dbuf,10)=reffc(nxi,nyi,nzi)
 end if
end do
! Virtual
do i=1,ndet_virtual
 rki=plocation_virtual(i,2)
 nxi=plocation_virtual(i,3)
 nyi=plocation_virtual(i,4)
 nzi=plocation_virtual(i,5)
 if (rank.eq.rki) then
  det_virtual(i,dbuf,1)=time
  det_virtual(i,dbuf,2)=0.5*(u(nxi,nyi,nzi)+u(nxi+1,nyi,nzi))
  det_virtual(i,dbuf,3)=0.5*(v(nxi,nyi,nzi)+v(nxi,nyi+1,nzi))
  det_virtual(i,dbuf,4)=0.5*(w(nxi,nyi,nzi)+w(nxi,nyi,nzi+1))
  det_virtual(i,dbuf,5)=tabs(nxi,nyi,nzi)
  det_virtual(i,dbuf,6)=qv(nxi,nyi,nzi)
  det_virtual(i,dbuf,7)=qna(nxi,nyi,nzi)
  det_virtual(i,dbuf,8)=qnc(nxi,nyi,nzi)
  det_virtual(i,dbuf,9)=rmean(nxi,nyi,nzi)
  det_virtual(i,dbuf,10)=reffc(nxi,nyi,nzi)
 end if
end do
call t_stopf('detector')

end subroutine
