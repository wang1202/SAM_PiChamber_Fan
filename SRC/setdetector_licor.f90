subroutine setdetector_licor()
! Initialize Licor in pi chamber
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
if (masterproc) print*,'find path location for licor'
idet = 5
pdet = 10 
open(78,file='./'//trim(case)//'/Licor',status='old',form='formatted')
read(78,*)
ndet_rd=0 ! detector number read from files
read(78,fmt=*) ndet_rd
if (ndet_rd.ne.2) then
  print*,'check licor file'
  stop
end if
if (ndet_rd.eq.2) then
! calculate ndet_licor
 nbuf = nstat/ndetout 
 if (mod(nstat,ndetout).ne.0) then
  print*,'nstat, ndetout not correct!'
  stop
 end if
 read(78,fmt=*) px1,py1,pz1
 read(78,fmt=*) px2,py2,pz2
 nnx=int((px2-px1)/dx)
 nny=int((py2-py1)/dy)
 nnz=int((pz2-pz1)/dz)
 if (nnx>0.and.nny.eq.0.and.nnz.eq.0) then
   ndet_licor = nnx
   nnn = 1
 elseif (nnx.eq.0.and.nny>0.and.nnz.eq.0) then
   ndet_licor = nny
   nnn = 2
 elseif (nnx.eq.0.and.nny.eq.0.and.nnz>0) then
   ndet_licor = nnz
   nnn = 3
 else
   print*,'check licor file'
   stop
 end if

! find ndet_licor
 allocate(plocation_licor(ndet_licor,idet))
 allocate(det_licor(ndet_licor,nbuf,pdet))
 det_licor(:,:,:)=0.0
 do i=1,ndet_rd/2
     px=px1
     py=py1
     pz=pz1
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
! plocation_licor
  if (nnn.eq.1) then
    nxi=nxi-1
    do j=1,ndet_licor
     nxi=nxi+1
     if (nxi>nx) then
       nxi=1
       rki=rki+1
     end if
     plocation_licor(j,1)=j
     plocation_licor(j,2)=rki
     plocation_licor(j,3)=nxi
     plocation_licor(j,4)=nyi
     plocation_licor(j,5)=nzi
    end do
  elseif (nnn.eq.2) then
    nyi=nyi-1
    do j=1,ndet_licor
     nyi=nyi+1
     if (nyi>ny) then
       nyi=1
       rki=rki+nsubdomains_x
     end if
     plocation_licor(j,1)=j
     plocation_licor(j,2)=rki
     plocation_licor(j,3)=nxi
     plocation_licor(j,4)=nyi
     plocation_licor(j,5)=nzi
    end do
  elseif (nnn.eq.3) then
    nzi=nzi-1
    do j=1,ndet_licor
     nzi=nzi+1
     plocation_licor(j,1)=j
     plocation_licor(j,2)=rki
     plocation_licor(j,3)=nxi
     plocation_licor(j,4)=nyi
     plocation_licor(j,5)=nzi

    end do
  end if
 end do

call detector_inilicor() ! initial output file

else ! no Licor
64 continue
65 continue
  print*,'No Licor'
  return
end if
return
end subroutine
