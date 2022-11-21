subroutine compress3DSWlr (f,nx,ny,nz,name, long_name, units, &
       	               savebin, dompi, rank, nsubdomains, nsubdomains_x, nsubdomains_y, SW)

! Compress3D: Compresses a given 3D array into the byte-array
! and writes the latter into a file.
! Modified for different MPI data gathering at the left/right side walls. Aaron Wang @ PNNL

use grid, only: masterproc,output_sep
	implicit none
! Input:

integer nx,ny,nz
real(8) f(nx,ny,nz)
character*(*) name,long_name,units
integer rank,rrr,ttt,irank,irank2,nsubdomains,nsubdomains_x,nsubdomains_y
logical savebin, dompi
integer SW ! used to specify which side wall.  
           ! 1: left; 2: right
           ! Aaron Wang @ PNNL

! Local:

integer(2), allocatable :: byte(:)
real(8), allocatable :: byte4(:)
integer size,count

character(10) value_min(nx), value_max(nx)
character(7) form
integer int_fac, integer_max, integer_min
parameter (int_fac=2,integer_min=-32000, integer_max=32000)
!	parameter (int_fac=1,integer_min=-127, integer_max=127)
real(8) f_max,f_min, f_max1, f_min1, scale
integer i,j,k,req

! Allocate byte array:

size=nx*ny*nz
if(savebin) then
  allocate (byte4(size))
else
  allocate (byte(size))
end if
count = 0

if(savebin) then	

  do k=1,nz
   do j=1,ny
    do i=1,nx
     count = count+1
     byte4(count) = f(i,j,k)
    end do
   end do
  end do

  if(SW.eq.1) then ! left wall
    if(masterproc) then
      write(46) name,' ',long_name,' ',units
      write(46) (byte4(k),k=1,count)
    end if 

    if(output_sep) then ! output_sep is not tested, yet
      if(.not.masterproc) write(46) (byte4(k),k=1,count)
    else
      do irank = 1, nsubdomains_y-1
        irank2 = nsubdomains_x*irank
        call task_barrier()
        if(irank2.eq.rank) then
          call task_bsend_float4(0,byte4,count,irank2)
        end if
        
        if(masterproc) then
          call task_receive_float4(byte4,count,req)
          call task_wait(req,rrr,ttt)
          write(46) (byte4(k),k=1,count)
        end if
      end do
    end if
  else ! right wall
    if(masterproc) then
      write(46) name,' ',long_name,' ',units
    end if 
    if(output_sep) then ! output_sep is not tested, yet
      if(.not.masterproc) write(46) (byte4(k),k=1,count)
    else
      do irank = 1, nsubdomains_y
        irank2 = nsubdomains_x*irank - 1
        call task_barrier()
        if(irank2.eq.rank) then
          call task_bsend_float4(0,byte4,count,irank2)
        end if
        
        if(masterproc) then
          call task_receive_float4(byte4,count,req)
          call task_wait(req,rrr,ttt)
          write(46) (byte4(k),k=1,count)
        end if
      end do
    end if
  end if
else ! .not. savebin hasn't been touched

  do k=1,nx

   f_max=-1.e30
   f_min= 1.e30
   do j=1,ny
    do i=1,nx
     f_max = max(f_max,f(i,j,k)) 	    
     f_min = min(f_min,f(i,j,k)) 	    
    end do
   end do
   if(dompi) then
     f_max1=f_max
     f_min1=f_min
     call task_max_real4(f_max1,f_max,1)
     call task_min_real4(f_min1,f_min,1)
   endif
   if(abs(f_max).lt.10..and.abs(f_min).lt.10.) then
          form='(f10.7)'
   else if(abs(f_max).lt.100..and.abs(f_min).lt.100.) then
          form='(f10.6)'
   else if(abs(f_max).lt.1000..and.abs(f_min).lt.1000.) then
          form='(f10.5)'
   else if(abs(f_max).lt.10000..and.abs(f_min).lt.10000.) then
          form='(f10.4)'
   else if(abs(f_max).lt.100000..and.abs(f_min).lt.100000.) then
          form='(f10.3)'
   else if(abs(f_max).lt.1000000..and.abs(f_min).lt.1000000.) then
          form='(f10.2)'
   else if(abs(f_max).lt.10000000..and.abs(f_min).lt.10000000.) then
          form='(f10.1)'
   else if(abs(f_max).lt.100000000..and.abs(f_min).lt.100000000.) then
          form='(f10.0)'
   else
          form='(f10.0)'
          f_min=-999.
          f_max= 999.
   end if

   write(value_max(k),form) f_max
   write(value_min(k),form) f_min

   scale = float(integer_max-integer_min)/(f_max-f_min+1.e-20)
   do j=1,ny
    do i=1,nx
      count=count+1
      byte(count)= integer_min+scale*(f(i,j,k)-f_min)   
    end do
   end do

  end do ! k
        
  if(masterproc) then
     write(46) name,' ',long_name,' ',units,' ',value_max,value_min
     write(46) (byte(k),k=1,count)
  end if

  if(output_sep) then
    if(.not.masterproc) write(46) (byte(k),k=1,count)
  else
    do irank = 1, nsubdomains-1
      call task_barrier()
      if(irank.eq.rank) then
        call task_send_character(0,byte,int_fac*count,irank,req)
        call task_wait(req,rrr,ttt)
      end if
      if(masterproc) then
        call task_receive_character(byte,int_fac*count,req)
        call task_wait(req,rrr,ttt)
        write(46) (byte(k),k=1,count)
      end if
    end do
  end if

  deallocate(byte)

end if ! savebin


call task_barrier()

end subroutine compress3DSWlr
	
