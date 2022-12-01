subroutine surface()
	
use vars
use params
use microphysics, only: micro_field, index_water_vapor
implicit none
	
real qvs(0:nx,1-YES3D:ny),t_s, q_s, u_h0, t_f, q_f
real qvx(0:nx,nzm),qvy(1-YES3D:ny,nzm) 
real taux0, tauy0, xlmo
real diag_ustar, coef, coef1
integer i,j,k
integer ib, it, jb, jt, kb, kt
real(8) buffer(2), buffer1(2)

! LES mode: 

call t_startf ('surface')


if(.not.SFC_FLX_FXD) then

  if(OCEAN) then

    if(LES) then

!      surface flux

       call oceflx(pres(1),u0(1)+ug, v0(1)+vg,t0(1)-gamaz(1), q0(1),t0(1),z(1),&
                          sstxy(1,1)+t00, fluxt0, fluxq0, taux0, tauy0, q_s)
       if(SFC_TAU_FXD) then
         u_h0 = max(1.,sqrt((u0(1)+ug)**2+(v0(1)+vg)**2))
         taux0 = -(u0(1)+ug)/u_h0*tau0*rhow(1)
         tauy0 = -(v0(1)+vg)/u_h0*tau0*rhow(1)
       else
         tau0=sqrt( taux0**2 +  tauy0**2)/rhow(1)
       end if

       fluxbt(:,:) = fluxt0
       fluxbq(:,:) = fluxq0
       fluxbu(:,:) = taux0/rhow(1)
       fluxbv(:,:) = tauy0/rhow(1)

!  top flux
       call oceflx(pres(nzm),u0(nzm)+ug, v0(nzm)+vg,t0(nzm)-gamaz(nzm), q0(nzm),t0(nzm),z(nzm),&
                          sttxy(1,1)+t00, fluxt0, fluxq0, taux0, tauy0, q_s)
       if(SFC_TAU_FXD) then
         u_h0 = max(1.,sqrt((u0(nzm)+ug)**2+(v0(nzm)+vg)**2))
         taux0 = -(u0(nzm)+ug)/u_h0*tau0*rhow(nz)
         tauy0 = -(v0(nzm)+vg)/u_h0*tau0*rhow(nz)
       else
         tau0=sqrt( taux0**2 +  tauy0**2)/rhow(nz)
       end if

       fluxtt(:,:) = fluxt0
       fluxtq(:,:) = fluxq0
       fluxtu(:,:) = taux0/rhow(nz)
       fluxtv(:,:) = tauy0/rhow(nz)
    end if ! LES

    if(CEM) then

       qvs(0:nx,1-YES3D:ny) = micro_field(0:nx,1-YES3D:ny,1,index_water_vapor)

       do j=1,ny
         do i=1,nx

           call oceflx(pres(1),0.5*(u(i+1,j,1)+u(i,j,1))+ug, &
                       0.5*(v(i,j+YES3D,1)+v(i,j,1))+vg, &
                       t(i,j,1)-gamaz(1),qv(i,j,1),t(i,j,1),z(1), &
                       sstxy(i,j)+t00, fluxt0, fluxq0, taux0, tauy0, q_s)
           fluxbt(i,j) = fluxt0
           fluxbq(i,j) = fluxq0

           call oceflx(pres(1),u(i,j,1)+ug, &
                       0.25*(v(i-1,j+YES3D,1)+v(i-1,j,1)+v(i,j+YES3D,1)+v(i,j,1))+vg, &
                       0.5*(t(i-1,j,1)+t(i,j,1))-gamaz(1),0.5*(qvs(i-1,j)+qvs(i,j)), &
                       0.5*(t(i-1,j,1)+t(i,j,1)),z(1), &
                       0.5*(sstxy(i-1,j)+sstxy(i,j))+t00, fluxt0, fluxq0, taux0, tauy0, q_s)
           if(SFC_TAU_FXD) then
             u_h0 = max(1.,sqrt((u(i,j,1)+ug)**2+ &
                     (0.25*(v(i-1,j+YES3D,1)+v(i-1,j,1)+v(i,j+YES3D,1)+v(i,j,1))+vg)**2))
             taux0 = -(u(i,j,1)+ug)/u_h0*tau0*rhow(1)
           end if
           fluxbu(i,j) = taux0/rhow(1)

           call oceflx(pres(1),0.25*(u(i+1,j-YES3D,1)+u(i,j-YES3D,1)+u(i+1,j,1)+u(i,j,1))+ug, &
                       v(i,j,1)+vg, &
                       0.5*(t(i,j-YES3D,1)+t(i,j,1))-gamaz(1),0.5*(qvs(i,j-YES3D)+qvs(i,j)), &
                       0.5*(t(i,j-YES3D,1)+t(i,j,1)),z(1), &
                       0.5*(sstxy(i,j-YES3D)+sstxy(i,j))+t00, fluxt0, fluxq0, taux0, tauy0, q_s)
           if(SFC_TAU_FXD) then
             u_h0 = max(1.,sqrt( &
                       (0.25*(u(i+1,j-YES3D,1)+u(i,j-YES3D,1)+u(i+1,j,1)+u(i,j,1))+ug)**2+ &
                       (v(i,j,1)+vg)**2))
             tauy0 = -(v(i,j,1)+vg)/u_h0*tau0*rhow(1)
           end if
           fluxbv(i,j) = tauy0/rhow(1)


         end do
       end do
	
    end if ! CEM

  end if ! OCEAN


  if(LAND) then

            if(LES) then    
!   surface flux
               soil_wetness=1.0
               coef = (1000./pres0)**(rgas/cp)
               coef1 = (1000./pres(1))**(rgas/cp)
               t_s = (sstxy(1,1)+t00)*coef
               q_s = soil_wetness*qsatw(sstxy(1,1)+t00,pres(1))

               if (CNS_FLX) then
! constant surface flux
                 call landflx((t0(1)-gamaz(1))*coef1, t_s,     &
                      q0(1), q_s, u0(1)+ug, v0(1)+vg, z(1), z0,      &
                      fluxt0, fluxq0, taux0, tauy0)
                 if(SFC_TAU_FXD) then
                   u_h0 = max(1.,sqrt((u0(1)+ug)**2+(v0(1)+vg)**2))
                   taux0 = -(u0(1)+ug)/u_h0*tau0*rhow(1)
                   tauy0 = -(v0(1)+vg)/u_h0*tau0*rhow(1)
                 else
                   tau0=sqrt( taux0**2 +  tauy0**2)/rhow(1)
                 end if
                 fluxbt(:,:) = fluxt0
                 fluxbq(:,:) = fluxq0
                 fluxbu(:,:) = taux0
                 fluxbv(:,:) = tauy0
                
               else
! non-constant surface flux
                 qvs(0:nx,1-YES3D:ny) = micro_field(0:nx,1-YES3D:ny,1,index_water_vapor) ! index_water_vapor = 1, which is total water instead of qv.  AW@PNNL
                 do j=1,ny
                   do i=1,nx
                     call landflx((t(i,j,1)-gamaz(1))*coef1, t_s,  &
                       qv(i,j,1), q_s, 0.5*(u(i+1,j,1)+u(i,j,1))+ug,       &
                       0.5*(v(i,j+YES3D,1)+v(i,j,1))+vg, z(1), z0,         &
                       fluxt0, fluxq0, taux0, tauy0)
                     fluxbt(i,j) = fluxt0
                     fluxbq(i,j) = fluxq0

               call landflx((0.5*(t(i,j,1)+t(i-1,j,1))-gamaz(1))*coef1, & 
                      t_s, 0.5*(qv(i,j,1)+qv(i-1,j,1)), q_s, u(i,j,1)+ug,     & ! qv instead of qvs should be used here.  AW@PNNL
                      0.25*(v(i-1,j+YES3D,1)+v(i-1,j,1)+v(i,j+YES3D,1)+v(i,j,1))+vg, &
                      z(1), z0,fluxt0, fluxq0, taux0, tauy0)
               fluxbu(i,j)=taux0
               
               call landflx((0.5*(t(i,j-YES3D,1)+t(i,j,1))-gamaz(1))*coef1, & 
                      t_s, 0.5*(qv(i,j-YES3D,1)+qv(i,j,1)), q_s, & ! qv instead of qvs should be used here.  AW@PNNL
                      0.25*(u(i,j,1)+u(i+1,j,1)+u(i,j-YES3D,1)+u(i+1,j-YES3D,1))+ug, &
                      v(i,j,1)+vg, &
                      z(1), z0,fluxt0, fluxq0, taux0, tauy0)
               fluxbv(i,j)=tauy0
               end do
               end do
               end if
!---------------------------------------------------------------
! top flux add for pi chamber
! FY @ MTU 2017
               soil_wetness=1.0
               coef = (1000./(pres(nzm)+0.5*(pres(nzm)-pres(nzm-1))))**(rgas/cp)
               coef1 = (1000./pres(nzm))**(rgas/cp)
               t_s = (sttxy(1,1)+t00)*coef
               q_s = soil_wetness*qsatw(sttxy(1,1)+t00,pres(nzm))
               if (CNS_FLX) then
! constant top surface flux
                 call landflx((t0(nzm)-gamaz(nzm))*coef1, t_s,     &
                      q0(nzm), q_s, u0(nzm)+ug, v0(nzm)+vg, z(1), z0,      &
                      fluxt0, fluxq0, taux0, tauy0)
                 if(SFC_TAU_FXD) then
                   u_h0 = max(1.,sqrt((u0(nzm)+ug)**2+(v0(nzm)+vg)**2))
                   taux0 = -(u0(nzm)+ug)/u_h0*tau0
                   tauy0 = -(v0(nzm)+vg)/u_h0*tau0
                 else
                   tau0=sqrt( taux0**2 +  tauy0**2)
                 end if
                 fluxtt(:,:) = - fluxt0
                 fluxtq(:,:) = - fluxq0
                 fluxtu(:,:) = - taux0
                 fluxtv(:,:) = - tauy0
               else
! non-constant top surface flux
                 qvs(0:nx,1-YES3D:ny) = micro_field(0:nx,1-YES3D:ny,nzm,index_water_vapor) ! index_water_vapor = 1, which is total water instead of qv.  AW@PNNL
                 do j=1,ny
                   jb = j-YES3D
                   jt = j+YES3D
                   do i=1,nx
                     ib = i-1
                     it = i+1
                     call landflx(t_s,(t(i,j,nzm)-gamaz(nzm))*coef1,  &
                       q_s,qv(i,j,nzm), 0.5*(u(it,j,nzm)+u(i,j,nzm))+ug,       &
                       0.5*(v(i,jt,nzm)+v(i,j,nzm))+vg, z(1), z0,          &
                       fluxt0, fluxq0, taux0, tauy0)
                     fluxtt(i,j) = fluxt0
                     fluxtq(i,j) = fluxq0
                     call landflx(t_s,(0.5*(t(i,j,nzm)+t(ib,j,nzm))-gamaz(nzm))*coef1, & 
                       q_s, 0.5*(qv(i,j,nzm)+qv(i-1,j,nzm)), u(i,j,nzm)+ug,     & ! qv instead of qvs should be used here.  AW@PNNL
                       0.25*(v(ib,jt,nzm)+v(ib,j,nzm)+v(i,jt,nzm)+v(i,j,nzm))+vg, &
                       z(1), z0,fluxt0, fluxq0, taux0, tauy0)
                     fluxtu(i,j)= -taux0
               
                     call landflx(t_s,(0.5*(t(i,jb,nzm)+t(i,j,nzm))-gamaz(nzm))*coef1, & 
                       q_s, 0.5*(qv(i,jb,nzm)+qv(i,j,nzm)), & ! qv instead of qvs should be used here.  AW@PNNL
                       0.25*(u(i,j,nzm)+u(it,j,nzm)+u(i,jb,nzm)+u(it,jb,nzm))+ug, &
                       v(i,j,nzm)+vg, &
                       z(1), z0,fluxt0, fluxq0, taux0, tauy0)
                       fluxtv(i,j)= -tauy0
                     end do
                   end do
                 end if

!---------------------------------------------------------------
! wall flux add for pi chamber
! FY @ MTU 2017
if(dowallx) then
! left wall flux
   qvy(1-YES3D:ny,:) = micro_field(1,1-YES3D:ny,:,index_water_vapor) ! index_water_vapor = 1, which is total water instead of qv.  AW@PNNL
! set soil_wetness to 0.0 for dry side walls
  soil_wetness= 0.61
  if(mod(rank,nsubdomains_x).eq.0) then
    do k=1,nzm
     kb = max(1,k-1)   
     do j=1,ny
       jb = j-YES3D
       jt = j+YES3D
       coef = (1000./pres(k))**(rgas/cp)
       coef1 = (1000./pres(k))**(rgas/cp)
       t_s = tabs_w*coef
       q_s = soil_wetness*qsatw(tabs_w,pres(k))
       call landflxSW((t(1,j,k)-gamaz(k))*coef1, t_s, &
                   qv(1,j,k), q_s, 0.5*(v(1,j,k)+v(1,jt,k)), &
                   0.5*(w(1,j,k)+w(1,j,k+1)),z(1),z0, &
                   fluxt0, fluxq0, taux0, tauy0)
       fluxlt(j,k) = fluxt0
       fluxlq(j,k) = fluxq0
       call landflxSW((0.5*(t(1,j,k)+t(1,jb,k))-gamaz(k))*coef1, &
            t_s, 0.5*(qv(1,j,k)+qv(1,j-YES3D,k)), q_s, v(1,j,k)+vg,     & ! qv instead of qvy should be used here.  AW@PNNL
            0.25*(w(1,jb,k)+w(1,jb,k+1)+w(1,j,k+1)+w(1,j,k)), &
            z(1), z0,fluxt0, fluxq0, taux0, tauy0)
       fluxlv(j,k)=taux0

       call landflxSW((0.5*(t(1,j,k)+t(1,j,kb))-gamaz(k))*coef1, &
            t_s, 0.5*(qv(1,j,k)+qv(1,j,kb)), q_s, & ! qv instead of qvy should be used here.  AW@PNNL
            0.25*(v(1,j,k)+v(1,j,kb)+v(1,jt,k)+v(1,jt,kb))+vg, &
            w(1,j,k), &
            z(1), z0,fluxt0, fluxq0, taux0, tauy0)
       fluxlw(j,k)=tauy0
     end do
    end do
  end if

! right wall flux
  if(mod(rank,nsubdomains_x).eq.nsubdomains_x-1) then
   qvy(1-YES3D:ny,:) = micro_field(nx,1-YES3D:ny,:,index_water_vapor) ! index_water_vapor = 1, which is total water instead of qv.  AW@PNNL
   do k=1,nzm
     kb = max(1,k-1) 
     do j=1,ny
       jb = j-YES3D
       jt = j+YES3D
       coef = (1000./pres(k))**(rgas/cp)
       coef1 = (1000./pres(k))**(rgas/cp)
       t_s = tabs_w*coef
       q_s = soil_wetness*qsatw(tabs_w,pres(k))
       t_f = max(t_s,(t(nx,j,k)-gamaz(k))*coef1)
       q_f = max(q_s,qv(nx,j,k))
       call landflxSW((t(nx,j,k)-gamaz(k))*coef1, t_s, &
                   qv(nx,j,k), q_s, 0.5*(v(nx,j,k)+v(nx,jt,k)), &
                   0.5*(w(nx,j,k)+w(nx,j,k+1)),z(1),z0, &
                   fluxt0, fluxq0, taux0, tauy0)
       fluxrt(j,k) = - fluxt0
       fluxrq(j,k) = - fluxq0
       call landflxSW((0.5*(t(nx,j,k)+t(nx,jb,k))-gamaz(k)*coef1), & 
            t_s, 0.5*(qv(nx,j,k)+qv(nx,j-YES3D,k)), q_s, v(nx,j,k)+vg,     & ! qv instead of qvy should be used here.  AW@PNNL
            0.25*(w(nx,jb,k)+w(nx,jb,k+1)+w(nx,j,k+1)+w(nx,j,k)), &
            z(1), z0,fluxt0, fluxq0, taux0, tauy0)
       fluxrv(j,k)=-taux0

       call landflxSW((0.5*(t(nx,j,k)+t(nx,j,kb))-gamaz(k))*coef1, & 
            t_s, 0.5*(qv(nx,j,k)+qv(nx,j,kb)), q_s, & ! qv instead of qvy should be used here.  AW@PNNL
            0.25*(v(nx,j,k)+v(nx,j,kb)+v(nx,jt,k)+v(nx,jt,kb))+vg, &
            w(nx,j,k), &
            z(1), z0,fluxt0, fluxq0, taux0, tauy0)
       fluxrw(j,k)=-tauy0
     end do
    end do
  end if

end if

if(dowally) then

! front wall flux
! soil_Wetness set to 0.0 for side walls, to assume it is not saturated
! ST 2018
  soil_wetness= 0.61
  if(rank.lt.nsubdomains_x) then   
    qvx(0:nx,:) = micro_field(0:nx,1,:,index_water_vapor) ! index_water_vapor = 1, which is total water instead of qv.  AW@PNNL

    do k=1,nzm
     kb = max(1,k-1)
     do i=1,nx
       ib = i-1
       it = i+1
       coef = (1000./pres0)**(rgas/cp)
       coef1 = (1000./pres(k))**(rgas/cp)
       t_s = tabs_w*coef
       q_s = soil_wetness*qsatw(tabs_w,pres(k))
       call landflxSW((t(i,1,k)-gamaz(k))*coef1, t_s, &
                   qv(i,1,k), q_s, 0.5*(w(i,1,k)+w(i,1,k+1)), &
                   0.5*(u(i,1,k)+u(it,1,k)),z(1),z0, &
                   fluxt0, fluxq0, taux0, tauy0)
       fluxqt(i,k) = fluxt0
       fluxqq(i,k) = fluxq0
       call landflxSW((0.5*(t(i,1,k)+t(i,1,kb))-gamaz(k))*coef1, & 
            t_s, 0.5*(qv(i,1,k)+qv(i,1,kb)), q_s, w(i,1,k),     & ! qv instead of qvx should be used here.  AW@PNNL
            0.25*(u(i,1,k)+u(it,1,k)+u(it,1,kb)+u(i,1,kb))+ug, &
            z(1), z0,fluxt0, fluxq0, taux0, tauy0)
       fluxqw(i,k)=taux0
       call landflxSW((0.5*(t(i,1,k)+t(i-1,1,k))-gamaz(k))*coef1, & 
            t_s, 0.5*(qv(i,1,k)+qv(i-1,1,k)), q_s, & ! qv has been correctly applied here...  AW@PNNL
            0.25*(w(i,1,k)+w(i,1,k+1)+w(ib,1,k)+w(ib,1,k+1)), &
            u(i,1,k)+ug, &
            z(1), z0,fluxt0, fluxq0, taux0, tauy0)
       fluxqu(i,k)=tauy0
     end do
    end do
  end if
! back wall flux
  if(rank.gt.nsubdomains-nsubdomains_x-1) then
    qvx(0:nx,:) = micro_field(0:nx,ny,:,index_water_vapor) ! index_water_vapor = 1, which is total water instead of qv.  AW@PNNL
    do k=1,nzm
     kb = max(1,k-1)
     do i=1,nx
       ib = i-1
       it = i+1
       coef = (1000./pres0)**(rgas/cp)
       coef1 = (1000./pres(k))**(rgas/cp)
       t_s = tabs_w*coef
       q_s = soil_wetness*qsatw(tabs_w,pres(k))
       call landflxSW((t(i,ny,k)-gamaz(k))*coef1, t_s, &
                   qv(i,ny,k), q_s, 0.5*(w(i,ny,k)+w(i,ny,k+1)), &
                   0.5*(u(i,ny,k)+u(it,ny,k)),z(1),z0, &
                   fluxt0, fluxq0, taux0, tauy0)
       fluxht(i,k) = - fluxt0
       fluxhq(i,k) = - fluxq0
       call landflxSW((0.5*(t(i,ny,k)+t(i,ny,kb))-gamaz(k))*coef1, & 
            t_s, 0.5*(qv(i,ny,k)+qv(i,ny,kb)), q_s, w(i,ny,k),     & ! qv instead of qvx should be used here.  AW@PNNL
            0.25*(u(i,ny,k)+u(it,ny,k)+u(it,ny,kb)+u(i,ny,kb))+ug, &
            z(1), z0,fluxt0, fluxq0, taux0, tauy0)
       fluxhw(i,k)= - taux0
       call landflxSW((0.5*(t(ib,ny,k)+t(i,ny,k))-gamaz(k))*coef1, & 
            t_s, 0.5*(qv(i-1,ny,k)+qv(i,ny,k)), q_s, & ! qv instead of qvx should be used here.  AW@PNNL
            0.25*(w(i,ny,k)+w(i,ny,k+1)+w(ib,ny,k)+w(ib,ny,k+1)), &
            u(i,ny,k)+ug, &
            z(1), z0,fluxt0, fluxq0, taux0, tauy0)
       fluxhu(i,k)= - tauy0
     end do
    end do
  end if

end if

!-----------------------------------------
            end if ! LES

            if(CEM) then

              coef = (1000./pres0)**(rgas/cp)
              coef1 = (1000./pres(1))**(rgas/cp)
              qvs(0:nx,1-YES3D:ny) = micro_field(0:nx,1-YES3D:ny,1,index_water_vapor)

              do j=1,ny  
               do i=1,nx

               t_s = (sstxy(i,j)+t00)*coef
               q_s = soil_wetness*qsatw(sstxy(i,j)+t00,pres(1))
               call landflx((t(i,j,1)-gamaz(1))*coef1, t_s,   &
                      qv(i,j,1), q_s, 0.5*(u(i+1,j,1)+u(i,j,1))+ug,     &
                        0.5*(v(i,j+YES3D,1)+v(i,j,1))+vg, z(1), z0,        &
                      fluxt0, fluxq0, taux0, tauy0)
               fluxbt(i,j) = fluxt0
               fluxbq(i,j) = fluxq0

               t_s = (0.5*(sstxy(i-1,j)+sstxy(i,j))+t00)*coef
               q_s = soil_wetness*qsatw(0.5*(sstxy(i-1,j)+sstxy(i,j))+t00,pres(1))
               call landflx((0.5*(t(i-1,j,1)+t(i,j,1))-gamaz(1))*coef1, t_s,   &
                      0.5*(qvs(i-1,j)+qvs(i,j)), q_s, u(i,j,1)+ug,     &
                        0.25*(v(i-1,j+YES3D,1)+v(i-1,j,1)+v(i,j+YES3D,1)+v(i,j,1))+vg, &
                       z(1), z0, fluxt0, fluxq0, taux0, tauy0)
               if(SFC_TAU_FXD) then
                   u_h0 = max(1.,sqrt((u(i,j,1)+ug)**2+ &
                        (0.25*(v(i-1,j+YES3D,1)+v(i-1,j,1)+v(i,j+YES3D,1)+v(i,j,1))+vg)**2))
                   taux0 = -(u(i,j,1)+ug)/u_h0*tau0*rhow(1)
               end if
               fluxbu(i,j) = taux0

               t_s = (0.5*(sstxy(i,j-YES3D)+sstxy(i,j))+t00)*coef
               q_s = soil_wetness*qsatw(0.5*(sstxy(i,j-YES3D)+sstxy(i,j))+t00,pres(1))
               call landflx((0.5*(t(i,j-YES3D,1)+t(i,j,1))-gamaz(1))*coef1, t_s,   &
                      0.5*(qvs(i,j-YES3D)+qvs(i,j)), q_s,  &
                      0.25*(u(i+1,j-YES3D,1)+u(i,j-YES3D,1)+u(i+1,j,1)+u(i,j,1))+ug,     &
                      v(i,j,1)+vg, &
                      z(1), z0, fluxt0, fluxq0, taux0, tauy0)
               if(SFC_TAU_FXD) then
                  u_h0 = max(1.,sqrt( &
                       (0.25*(u(i+1,j-YES3D,1)+u(i,j-YES3D,1)+u(i+1,j,1)+u(i,j,1))+ug)**2+ &
                       (v(i,j,1)+vg)**2))
                  tauy0 = -(v(i,j,1)+vg)/u_h0*tau0*rhow(1)
               end if
               fluxbv(i,j) = tauy0

               end do
              end do

            end if ! CEM


  end if ! LAND

end if! .not.SFC_FLX_FXD



if(SFC_FLX_FXD) then

  u_h0 = max(1.,sqrt((u0(1)+ug)**2+(v0(1)+vg)**2))

  if(.not.SFC_TAU_FXD) then
    if(OCEAN) z0 = 0.0001  ! for LAND z0 should be set in namelist (default z0=0.035)

    tau0 = diag_ustar(z(1),  &
                bet(1)*(fluxt0+epsv*(t0(1)-gamaz(1))*fluxq0),u_h0,z0)**2  

  end if ! .not.SFC_TAU_FXD

  if(LES) then
    taux0 = -(u0(1)+ug)/u_h0*tau0
    tauy0 = -(v0(1)+vg)/u_h0*tau0
    fluxbu(:,:) = taux0
    fluxbv(:,:) = tauy0
  else
    fluxbu(:,:) = -(u(1:nx,1:ny,1)+ug)/u_h0*tau0
    fluxbv(:,:) = -(v(1:nx,1:ny,1)+vg)/u_h0*tau0
  end if

  fluxbt(:,:) = fluxt0
  fluxbq(:,:) = fluxq0

end if ! SFC_FLX_FXD

!
! Homogenize the surface scalar fluxes if needed for sensitivity studies
!
   if(dosfchomo) then

	fluxt0 = 0.
	fluxq0 = 0.
	do j=1,ny
         do i=1,nx
	   fluxt0 = fluxt0 + fluxbt(i,j)
	   fluxq0 = fluxq0 + fluxbq(i,j)
         end do
        end do
	fluxt0 = fluxt0 / float(nx*ny)
	fluxq0 = fluxq0 / float(nx*ny)
        if(dompi) then
            buffer(1) = fluxt0
            buffer(2) = fluxq0
            call task_sum_real8(buffer,buffer1,2)
	    fluxt0 = buffer1(1) /float(nsubdomains)
	    fluxq0 = buffer1(2) /float(nsubdomains)
        end if ! dompi
	fluxbt(:,:) = fluxt0
	fluxbq(:,:) = fluxq0

   end if

shf_xy(:,:) = shf_xy(:,:) + fluxbt(:,:) * dtfactor
lhf_xy(:,:) = lhf_xy(:,:) + fluxbq(:,:) * dtfactor
! Ssve top and side wall's flux. Aaron Wang @ PNNL
shft_xy(:,:) = shft_xy(:,:) + fluxtt(:,:) * dtfactor
lhft_xy(:,:) = lhft_xy(:,:) + fluxtq(:,:) * dtfactor
! Left wall
if(mod(rank,nsubdomains_x).eq.0) then
  shfl_yz(:,:) = shfl_yz(:,:) + fluxlt(:,:) * dtfactor
  lhfl_yz(:,:) = lhfl_yz(:,:) + fluxlq(:,:) * dtfactor
end if
! Right wall
if(mod(rank,nsubdomains_x).eq.nsubdomains_x-1) then
  shfr_yz(:,:) = shfr_yz(:,:) + fluxrt(:,:) * dtfactor
  lhfr_yz(:,:) = lhfr_yz(:,:) + fluxrq(:,:) * dtfactor
end if
! Front wall
if(rank.lt.nsubdomains_x) then
  shfq_xz(:,:) = shfq_xz(:,:) + fluxqt(:,:) * dtfactor
  lhfq_xz(:,:) = lhfq_xz(:,:) + fluxqq(:,:) * dtfactor
end if
! Back wall
if(rank.gt.nsubdomains-nsubdomains_x-1) then
  shfh_xz(:,:) = shfh_xz(:,:) + fluxht(:,:) * dtfactor
  lhfh_xz(:,:) = lhfh_xz(:,:) + fluxhq(:,:) * dtfactor
end if
! End of editing. Aaron Wang @ PNNL

call t_stopf ('surface')

end




! ----------------------------------------------------------------------
!
! DISCLAIMER : this code appears to be correct but has not been
!              very thouroughly tested. If you do notice any
!              anomalous behaviour then please contact Andy and/or
!              Bjorn
!
! Function diag_ustar:  returns value of ustar using the below 
! similarity functions and a specified buoyancy flux (bflx) given in
! kinematic units
!
! phi_m (zeta > 0) =  (1 + am * zeta)
! phi_m (zeta < 0) =  (1 - bm * zeta)^(-1/4)
!
! where zeta = z/lmo and lmo = (theta_rev/g*vonk) * (ustar^2/tstar)
!
! Ref: Businger, 1973, Turbulent Transfer in the Atmospheric Surface 
! Layer, in Workshop on Micormeteorology, pages 67-100.
!
! Code writen March, 1999 by Bjorn Stevens
!
! Code corrected 8th June 1999 (obukhov length was wrong way up,
! so now used as reciprocal of obukhov length)

      real function diag_ustar(z,bflx,wnd,z0)

      implicit none
      real, parameter      :: vonk =  0.4   ! von Karmans constant
      real, parameter      :: g    = 9.81   ! gravitational acceleration
      real, parameter      :: am   =  4.8   !   "          "         "
      real, parameter      :: bm   = 19.3   !   "          "         "
      real, parameter      :: eps  = 1.e-10 ! non-zero, small number

      real, intent (in)    :: z             ! height where u locates
      real, intent (in)    :: bflx          ! surface buoyancy flux (m^2/s^3)
      real, intent (in)    :: wnd           ! wind speed at z
      real, intent (in)    :: z0            ! momentum roughness height

      integer :: iterate
      real    :: lnz, klnz, c1, x, psi1, zeta, rlmo, ustar

      lnz   = log(z/z0) 
      klnz  = vonk/lnz              
      c1    = 3.14159/2. - 3.*log(2.)

      ustar =  wnd*klnz
      if (bflx /= 0.0) then 
        do iterate=1,4
          rlmo   = -bflx * vonk/(ustar**3 + eps)   !reciprocal of
                                                   !obukhov length
          zeta  = z*rlmo
          if (zeta > 0.) then
            ustar =  vonk*wnd  /(lnz + am*zeta)
          else
            x     = sqrt( sqrt( 1.0 - bm*zeta ) )
            psi1  = 2.*log(1.0+x) + log(1.0+x*x) - 2.*atan(x) + c1
            ustar = wnd*vonk/(lnz - psi1)
          end if
        end do
      end if

      diag_ustar = ustar

      return
      end function diag_ustar
! ----------------------------------------------------------------------

