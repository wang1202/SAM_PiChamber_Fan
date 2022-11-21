subroutine setdetector_virtual()
! Initialize Virtual Detector
! FY @ MTU 2017
use vars
use params
implicit none
integer :: i
integer :: rki,nxi,nyi,nzi,txi,tyi,tzi
integer :: rkxi,rkyi
integer :: nbuf
real :: px, py, pz ! detector location
if (masterproc) print*,'find location for virtual detectors'
idet = 5
pdet = 10 
open(78,file='./'//trim(case)//'/Virtual',status='old',form='formatted')
read(78,*)
ndet_virtual=0
read(78,fmt=*) ndet_virtual
if (ndet_virtual>0) then
 nbuf = nstat/ndetout 
 if (mod(nstat,ndetout).ne.0) then
  print*,'nstat, ndetout not correct!'
  stop
 end if
 allocate(plocation_virtual(ndet_virtual,idet))
 allocate(det_virtual(ndet_virtual,nbuf,pdet))
 det_virtual(:,:,:)=0.0
 do i=1,ndet_virtual
  read(78,fmt=*) px, py, pz
  txi = max(min(nint((px-dx/2.0)/dx)+1,nx_gl),YES3D)
  tyi = max(min(nint((py-dy/2.0)/dy)+1,ny_gl),YES3D)
  tzi = max(min(nint((pz-dz/2.0)/dz)+1,nz_gl),YES3D)
  if (mod(txi,nx).eq.0) then
   rkxi = int(txi/nx)
   nxi = nx
  else
   rkxi = int(txi/nx) + 1
   nxi = mod(txi,nx)
  end if
  if (mod(tyi,ny).eq.0) then
   rkyi = int(tyi/ny)
   nyi = ny
  else
   rkyi = int(tyi/ny) + 1
   nyi = mod(tyi,ny)
  end if
  rki = (nsubdomains_x-1) * (rkyi-1) + rkxi - 1
  nzi = tzi

  plocation_virtual(i,1)=i
  plocation_virtual(i,2)=rki
  plocation_virtual(i,3)=nxi
  plocation_virtual(i,4)=nyi
  plocation_virtual(i,5)=nzi
 end do

call detector_inivirtual() ! initial output file

else ! no Virtual Detector
84 continue
85 continue
  print*,'No Virtual Detector'
  return
end if

return
end subroutine
