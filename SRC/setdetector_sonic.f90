subroutine setdetector_sonic()
! Initialize Sonic in pi chamber
! FY @ MTU 2017
use vars
use params
implicit none
integer :: i,j
integer :: rki,nxi,nyi,nzi,txi,tyi,tzi
integer :: rkxi,rkyi
integer :: nbuf
integer :: ndet_rd
real :: px,py,pz
real :: pxu_low, pxu_high,pyu,pzu
real :: pxv,pyv_low, pyv_high,pzv
real :: pxw,pyw,pzw_low, pzw_high
if (masterproc) print*,'find path location for sonic u,v,w'
idet = 5
pdet = 10 
open(78,file='./'//trim(case)//'/Sonic',status='old',form='formatted')
read(78,*)
ndet_rd=0 ! detector number read from files
read(78,fmt=*) ndet_rd
if (ndet_rd.ne.6) then
  print*,'check Sonic file'
  stop
end if
if (ndet_rd.eq.6) then
! calculate ndet_sonicu, ndet_sonicv, ndet_sonicw 
 nbuf = nstat/ndetout 
 if (mod(nstat,ndetout).ne.0) then
  print*,'nstat, ndetout not correct!'
  stop
 end if
 read(78,fmt=*) pxu_low,pyu,pzu
 read(78,fmt=*) pxu_high,pyu,pzu
 read(78,fmt=*) pxv,pyv_low,pzv
 read(78,fmt=*) pxv,pyv_high,pzv
 read(78,fmt=*) pxw,pyw,pzw_low
 read(78,fmt=*) pxw,pyw,pzw_high
 ndet_sonicu=int((pxu_high-pxu_low)/dx)
 ndet_sonicv=int((pyv_high-pyv_low)/dy)
 ndet_sonicw=int((pzw_high-pzw_low)/dz)

! find ndet_sonicu, ndet_sonicv, ndet_sonicw

 allocate(plocation_sonicu(ndet_sonicu,idet))
 allocate(plocation_sonicv(ndet_sonicv,idet))
 allocate(plocation_sonicw(ndet_sonicw,idet))
 allocate(det_sonicu(ndet_sonicu,nbuf,pdet))
 allocate(det_sonicv(ndet_sonicv,nbuf,pdet))
 allocate(det_sonicw(ndet_sonicw,nbuf,pdet))
 det_sonicu(:,:,:)=0.0
 det_sonicv(:,:,:)=0.0
 det_sonicw(:,:,:)=0.0
 do i=1,ndet_rd/2
   if(i.eq.1) then
     px=pxu_low
     py=pyu
     pz=pzu
   elseif (i.eq.2) then
     px=pxv
     py=pyv_low
     pz=pzv
   elseif (i.eq.3) then
     px=pxw
     py=pyw
     pz=pzw_low
   end if
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
! plocation_sonicu,plocation_sonicv,plocation_sonicw
  if (i.eq.1) then
    nxi=nxi-1
    do j=1,ndet_sonicu
     nxi=nxi+1
     if (nxi>nx) then
       nxi=1
       rki=rki+1
     end if
     plocation_sonicu(j,1)=j
     plocation_sonicu(j,2)=rki
     plocation_sonicu(j,3)=nxi
     plocation_sonicu(j,4)=nyi
     plocation_sonicu(j,5)=nzi
    end do
  elseif (i.eq.2) then
    nyi=nyi-1
    do j=1,ndet_sonicv
     nyi=nyi+1
     if (nyi>ny) then
       nyi=1
       rki=rki+nsubdomains_x
     end if
     plocation_sonicv(j,1)=j
     plocation_sonicv(j,2)=rki
     plocation_sonicv(j,3)=nxi
     plocation_sonicv(j,4)=nyi
     plocation_sonicv(j,5)=nzi
    end do
  elseif (i.eq.3) then
    nzi=nzi-1
    do j=1,ndet_sonicw
     nzi=nzi+1
     plocation_sonicw(j,1)=j
     plocation_sonicw(j,2)=rki
     plocation_sonicw(j,3)=nxi
     plocation_sonicw(j,4)=nyi
     plocation_sonicw(j,5)=nzi

    end do
  end if
 end do

call detector_inisonic() ! initial output file

else ! no Sonic
54 continue
55 continue
  print*,'No Sonic'
  return
end if
return
end subroutine
