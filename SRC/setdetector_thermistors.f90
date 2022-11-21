subroutine setdetector_thermistors()
! Initialize Thermistors in pi chamber
! FY @ MTU 2017
use vars
use params
implicit none
integer :: i,j
integer :: rki,nxi,nyi,nzi,txi,tyi,tzi
integer :: rkxi,rkyi
integer :: nbuf
integer :: ndet_rd
integer :: nnx,nny,nnz,nnn
real :: px,py,pz
real :: px1,py1,pz1
real :: px2,py2,pz2
if (masterproc) print*,'find path location for thermistors'
idet = 5
pdet = 10 
open(78,file='./'//trim(case)//'/Thermistors',status='old',form='formatted')
read(78,*)
ndet_rd=0 ! detector number read from files
read(78,fmt=*) ndet_rd
if (ndet_rd.ne.8) then
  print*,'check thermistors file'
  stop
end if
if (ndet_rd.eq.8) then
! calculate ndet_thermistors
 nbuf = nstat/ndetout 
 if (mod(nstat,ndetout).ne.0) then
  print*,'nstat, ndetout not correct!'
  stop
 end if
 read(78,fmt=*) px1,py1,pz1
 read(78,fmt=*) px,py,pz
 read(78,fmt=*) px,py,pz
 read(78,fmt=*) px,py,pz
 read(78,fmt=*) px,py,pz
 read(78,fmt=*) px,py,pz
 read(78,fmt=*) px,py,pz
 read(78,fmt=*) px2,py2,pz2
 nnx=int(abs((px2-px1))/dx)
 nny=int(abs((py2-py1))/dy)
 nnz=int(abs((pz2-pz1))/dz)
 if (nnx>0.and.nny.eq.0.and.nnz.eq.0) then
   ndet_thermistors = nnx
   nnn = 1
 elseif (nnx.eq.0.and.nny>0.and.nnz.eq.0) then
   ndet_thermistors = nny
   nnn = 2
 elseif (nnx.eq.0.and.nny.eq.0.and.nnz>0) then
   ndet_thermistors = nnz
   nnn = 3
 else
   print*,'check thermistors file'
   stop
 end if

! find ndet_thermistors
 allocate(plocation_thermistors(ndet_thermistors,idet))
 allocate(det_thermistors(ndet_thermistors,nbuf,pdet))
 det_thermistors(:,:,:)=0.0
 do i=1,1
     px=min(px1,px2)
     py=min(py1,py2)
     pz=min(pz1,pz2)
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
! plocation_thermistors
  if (nnn.eq.1) then
    nxi=nxi-1
    do j=1,ndet_thermistors
     nxi=nxi+1
     if (nxi>nx) then
       nxi=1
       rki=rki+1
     end if
     plocation_thermistors(j,1)=j
     plocation_thermistors(j,2)=rki
     plocation_thermistors(j,3)=nxi
     plocation_thermistors(j,4)=nyi
     plocation_thermistors(j,5)=nzi
    end do
  elseif (nnn.eq.2) then
    nyi=nyi-1
    do j=1,ndet_thermistors
     nyi=nyi+1
     if (nyi>ny) then
       nyi=1
       rki=rki+nsubdomains_x
     end if
     plocation_thermistors(j,1)=j
     plocation_thermistors(j,2)=rki
     plocation_thermistors(j,3)=nxi
     plocation_thermistors(j,4)=nyi
     plocation_thermistors(j,5)=nzi
    end do
  elseif (nnn.eq.3) then
    nzi=nzi-1
    do j=1,ndet_thermistors
     nzi=nzi+1
     plocation_thermistors(j,1)=j
     plocation_thermistors(j,2)=rki
     plocation_thermistors(j,3)=nxi
     plocation_thermistors(j,4)=nyi
     plocation_thermistors(j,5)=nzi
    end do
  end if
 end do

call detector_inithermistors() ! initial output file

else ! no Thermistors
74 continue
75 continue
  print*,'No Thermistors'
  return
end if
return
end subroutine
