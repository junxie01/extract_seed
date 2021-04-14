! extract three components of sac file from seeds file or miniseed file
! 2016/10/04 --introduce sacio
! 2018/03/09 -- using only one parameter file
! 2018/04/28 -- using allocatable array
! 2018/04/29 -- extract specific day
program main
use sacio
implicit none
type(sac_head):: sachead,sachead1
!integer,parameter :: nn=4000000,nstmax=1000,nfmax=1000
integer,parameter :: nstmax=2000,nfmax=1000
integer seed_type ! 1 for seed; 0 for miniseed
integer ibeg,iend,nloc,nn
integer nh1,nh2,npart,nnh1,nseg
integer nzhour1,nzmin1,nzsec1,nzmsec1
integer i,j,nsta,error,ifile,ic,dsec,iseg
integer year_b,day_b,year_e,day_e,dhour,dseg,swap
integer do_rm_response,do_decimate,imark,npts,multpt
integer nlen,nzhour,nzmin,nzsec,nzmsec,nerr,nzero
integer is,ih,iy,id,dayb,daye,jday,nh,nnh,nhh1,icom
character(80)sta(nstmax)
character(80)net(nstmax)
character(3)com(3),co
character(10)nd,year_day
character(80)str,tra,bash,resp,mdata
character(80)para,dirin,dirout,list
character(180)command,saclist,file,tmp
character(80)seed,sac,dir,sac1,sac2,sac_tmp
character(80)dir_seed,dir_sac,dir_resp,dir_mdata
character(8)kh,kho
character(8)khole
!real sig(nn),sigall(nn),dt,beg,stla,stlo,sigo(nn)
real :: dt,beg,stla,stlo
real,allocatable,dimension(:) :: sig,sigall,sigo,sigout
!real sigout(nn),multpt
real f1,f2,dt0,mean,avg
logical ext,ext1,ext2

interface
   function av_sig(sig, i, npts, nwin )
   integer,intent(in) :: i,npts,nwin
   real,dimension(4000000):: sig
   end function
end interface

str='extract.bash'
inquire(file='for_cc',exist=ext)
if(.not.ext)stop "for_cc not exit"
if(iargc().ne.3)then
   call usage
   stop
endif
i=1
call getarg(1,list)
call getarg(2,tmp)
read(tmp,'(bn,i20)')year_b
call getarg(3,tmp)
read(tmp,'(bn,i20)')day_b
open(99,file='for_cc')
read(99,*)co,icom
read(99,*)seed_type,dsec,multpt
read(99,*)f1,f2,do_rm_response,do_decimate
read(99,'(a80)')dir_seed
read(99,'(a80)')dir_resp
read(99,'(a80)')dir_mdata
read(99,'(a80)')dir_sac
close(99)
if(multpt.ge.100)stop 'Please check multpt'
if(icom.eq.1)then
   com(1)=co
else
   com(1)=trim(co)//'Z'
   com(2)=trim(co)//'N'
   com(3)=trim(co)//'E'
endif
open(11,file=list)
do i=1,nstmax
   read(11,*,end=12,err=12)net(i),sta(i)
enddo
12  close(11)
nsta=i-1
!write(*,*)'Number of station is ',nsta
!nh=24/dhour ! number of segments
!nseg=(1-multp/100)*dsec
dseg=int((1-real(multpt)/100.0)*dsec) ! the left points without overlapping
nseg=int((86400-dsec)/dseg)+1 
write(year_day,'(i4.4,"_",i3.3)')year_b,day_b
seed=trim(dir_seed)//'/'//trim(year_day)//'.seed'
inquire(file=seed,exist=ext)
if (.not.ext)stop                          ! if the seed file exists
write(command,'("mkdir -p",1x,1a,1x,"2>/dev/null")')trim(dir_sac)//'/'//trim(year_day)
call system(command)                        ! make output directory
if(seed_type.eq.1)then                      ! if it is the seed file
   do iseg=1,nseg                           ! loop over each segments
      !write(*,*)'deal with ',iy, id
      nzhour=(iseg-1)*dseg/3600
      nzmin=mod((iseg-1)*dseg,3600)/60
      nzsec=mod(mod((iseg-1)*dseg,3600),60)

      nzhour1=(iseg*dseg+dsec-dseg)/3600
      nzmin1=mod((iseg*dseg+dsec-dseg),3600)/60
      nzsec1=mod(mod((iseg*dseg+dsec-dseg),3600),60)
      do is=1,nsta            ! loop over station
         do ic=1,icom    ! loop over component
            sigall=1e30
            write(saclist,'(1a,"_",1a,"_",1a,".list")')trim(year_day),trim(sta(is)),com(ic)
            write(sac1,'(i4.4,".",i3.3,".*",1a,"*",1a,"*SAC")')year_b,day_b,trim(sta(is)),com(ic)
            write(sac,'(1a,"/",i4.4,"_",i3.3,"/",i4.4,"_",i3.3,"_",i2.2,"_",i2.2,"_",i2.2,"_",&
            1a,"_",1a,"_",1a,".SAC")')trim(dir_sac),year_b,day_b,year_b,day_b,nzhour,nzmin,nzsec,trim(net(is)),trim(sta(is)),com(ic)
            inquire(file=sac,exist=ext)
            if(ext)cycle
            write(bash,'(i4.4,"_",i3.3,"_",1a,"_",1a,"_",1a)')year_b,day_b,trim(net(is)),trim(sta(is)),trim(str)
            open(20,file=bash)
            write(20,'("#!/bin/bash")')
            write(20,'("rdseed.linux <<eof 1>/dev/null 2>&1")')
            write(20,'(1a)')trim(seed)
            write(20,'("")')
            write(20,'("")')
            write(20,'("d")')
            write(20,'("")')
            write(20,*)trim(sta(is))
            write(20,'(a3)')com(ic)
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'("")')
            write(20,'(i0,",",i0,",",i0,":",i0,":",i0,".0000")')year_b,day_b,nzhour,nzmin,nzsec
            write(20,'(i0,",",i0,",",i0,":",i0,":",i0,".9999")')year_b,day_b,nzhour1,nzmin1,nzsec1
            write(20,'("")')
            write(20,'("Y")')
            write(20,'("Quit")')
            write(20,'("eof")') 
            close(20)
            write(command,'("bash",1x,1a)')trim(bash)
            call system(command) ! extract sac file
            open(20,file=bash) 
            write(20,'("#!/bin/bash")')
            write(20,'("rm",1x,1a,1x,"2>/dev/null")')trim(saclist)
            write(20,'("n=`ls ",1a,1x,"2>/dev/null | wc -l`")')trim(sac1)
            write(20,'("if [ $n -ge 1 ];then")')
            write(20,'("   ls",1x,1a,1x,">",1a)')trim(sac1),trim(saclist)
            write(20,'("fi")')
            close(20)
            write(command,'("bash",1x,1a)')trim(bash)
            call system(command)  ! write all sac in to saclist
            write(command,'("rm",1x,1a)')trim(bash)
            call system(command)  ! write all sac in to saclist
            inquire(file=trim(saclist),exist=ext)  
            imark=0;nlen=0;dt=0;beg=0
            sig=0;stla=0;stlo=0;
            if(.not.ext)cycle     ! if the sac file list not exists
            open(80,file=trim(saclist))
            do ifile=1,nfmax
               read(80,'(1a180)',err=20,end=20)file
               call read_sachead(trim(file),sachead,swap,nerr)
               if(nerr.eq.-1)cycle ! read file uncorrectly
               if(imark==0)then
                  kho=trim(sachead%khole(1:2))
                  dt0=sachead%delta
                  imark=imark+1
               endif
               if(sachead%khole(1:2).ne.kho)cycle
               if(dt0.ne.sachead%delta)cycle
               if(do_rm_response.eq.1)then
                  write(resp,'("RESP.",1a,".",1a,".",1a,".",1a)')trim(net(is)),trim(sta(is)),trim(kho),trim(com(ic))
                  write(*,'("Remove resp from",1x,1a)')trim(resp)
                  if(trim(kho).eq."-1")write(resp,'("RESP.",1a,".",1a,"..",1a)')&
                  trim(net(is)),trim(sta(is)),trim(com(ic))
                  call rm_resp(trim(bash),trim(file),trim(resp),f1,f2,error)
                  if(error.eq.-1)cycle  ! remove response wrong
               endif
               write(*,*)"Read file ",trim(file)
               call read_sac(trim(file),sig,sachead,swap,nerr)
               call abstime(nloc,nzhour,nzmin,nzsec,sachead,sig,sigo)
               do i=1,sachead%npts 
                  if(nloc.ge.1.and.sigall(i+nloc-1).ge.1e29)sigall(i+nloc-1)=sigo(i)
               enddo
            enddo ! loop over file
            20 close(80)
            write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(saclist)
            call system(command)
            write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(bash)
            call system(command)
            write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(sac1)
            call system(command)
            npts=int(dsec/dt0)+1
            nzero=0 
            do i=1,npts
               if(sigall(i).gt.1e29)nzero=nzero+1 
            enddo
            if(nzero.gt.npts/2)cycle ! ignore the data with no. zeros bigger than half of the npts
            do i=1,npts
               npart=16
               avg=sigall(i)
               do while (avg.gt.1.e29.and.npart.gt.1)
                  avg=av_sig(sigall,i,npts,npts/npart)
                  npart = npart/2
               enddo
               sigall(i) = avg
               if ( npart.eq.1 )then
                  avg = 0.
               endif 
            enddo ! end loop over points
            write(*,*)'Write to file ',trim(sac)
            call write_ext_sac(trim(sac),sigall,sachead,npts,year_b,day_b,nzhour,nzmin,nzsec,0,kho,nerr)
            if(do_decimate.eq.1)call decimate(trim(sac),dt0) ! decimate the data to 10Hz
            write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(saclist)
            call system(command)
            write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(bash)
            call system(command)
            write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(sac1)
            call system(command)
         enddo      ! loop over comopnonet             
      enddo         ! loop over station
   enddo            ! loop over daily segments
else                ! if the file is miniseed
   write(mdata,'(1a,"/",i4.4,"_",i3.3,".mdata")')trim(dir_mdata),year_b,day_b
   inquire(file=mdata,exist=ext1)
   if(.not.ext1)stop
   write(command,'("mseed2sac ",1a,1x,"-m",1x,1a,1x,"1>/dev/null 2>&1")')trim(seed),trim(mdata) 
   call system(command)  ! extract miniseed file
   do is=1,nsta          ! loop over station
      do ic=1,icom       ! loop over component
         write(saclist,'(1a,"_",1a,"_",1a,".list")')trim(year_day),trim(sta(is)),com(ic)
         write(bash,'(1a,"_",1a,"_",1a,".bash")')trim(year_day),trim(sta(is)),trim(com(ic))
         write(sac_tmp,'(1a,".",1a,".*.",1a,".*.",i4.4,".",i3.3,".*.SAC")')&
         trim(net(is)),trim(sta(is)),trim(com(ic)),year_b,day_b
         open(20,file=bash) 
         write(20,'("#!/bin/bash")')
         write(20,'("rm",1x,1a,1x,"2>/dev/null")')trim(saclist)
         write(20,'("n=`ls",1x,1a,1x,"2>/dev/null | wc -l`")')trim(sac_tmp)
         write(20,'("if [ $n -ge 1 ];then")')
         write(20,'("     ls",1x,1a,1x,">",1a)')trim(sac_tmp),trim(saclist)
         write(20,'("fi")')
         close(20)
         write(command,'("bash",1x,1a)')trim(bash)
         call system(command)     ! write sac file to saclist
         write(command,'("rm",1x,1a," 2>/dev/null")')trim(bash)
         call system(command)     ! rmove the bash
         inquire(file=trim(saclist),exist=ext) 
         if(.not.ext)cycle ! if the sac file list exists
         imark=0
         open(100,file=trim(saclist))
         do ifile=1,nfmax
            read(100,'(1a180)',err=16,end=16)file !read sac file list  
            call read_sachead(trim(file),sachead,swap,nerr)
            if(imark==0)then
               if(nerr.eq.-1)cycle
               kho=sachead%khole(1:2)
               dt0=sachead%delta
               imark=imark+1
               nn=int(86400/dt0)+1
               allocate(sigall(nn))
               sigall=1e30
            endif
            if(sachead%delta.ne.dt0)cycle
            if(sachead%khole(1:2).ne.kho)cycle
!      ******************************************** remove response before paste
            if(do_rm_response.eq.1)then
               write(resp,'(1a,"/resp_",1a,"/RESP.",1a,".",1a,".",1a,".",1a)')&
               trim(dir_resp),trim(year_day),trim(net(is)),trim(sta(is)),trim(kho),trim(com(ic))
               if(kho.eq."-1")write(resp,'(1a,"/resp_",1a,"/RESP.",1a,".",1a,"..",1a)')&
               trim(dir_resp),trim(year_day),trim(net(is)),trim(sta(is)),trim(com(ic))
               write(*,*)'Remove response ',trim(resp), " from ",trim(file)
               call rm_resp(bash,trim(file),trim(resp),f1,f2,error)
               if(error.eq.-1)cycle !  response wrong
            endif
!      ******************************************** remove response done
            allocate(sig(sachead%npts),sigo(sachead%npts))
            call read_sac(trim(file),sig,sachead,swap,nerr)
            if (nerr.eq.-1)cycle
            call abstime(nloc,0,0,0,sachead,sig,sigo) 
            do i=1,sachead%npts
               if(i+nloc-1.gt.0.and.sigall(i+nloc-1).gt.1e29)then
                  sigall(i+nloc-1)=sigo(i)
               endif
            enddo  ! loop over points
            deallocate(sig,sigo)
         enddo     ! loop over sacfile 
      16 close(100) ! done reading the temporary sac files
         npts=int(dsec/dt0)+1  ! number of points of each segments 
         allocate(sigout(npts))
         dseg=int((1-multpt/100)*npts)
         if(multpt.eq.0)dseg=npts-1 ! added at 2017/06/22 
         do iseg=1,nseg
            ibeg=(iseg-1)*(dseg)+1
            iend=ibeg+npts-1
            nnh=(ih-1)*dhour
            nzhour=int((ibeg-1)*dt0)/3600
            nzmin=mod(int((ibeg-1)*dt0),3600)/60
            nzsec=mod(mod(int((ibeg-1)*dt0),3600),60)
            !write(*,*)iseg,dseg,dt0,ibeg,nzhour,nzmin,nzsec
            write(sac,'(1a,"/",1a,"/",1a,"_",i2.2,"_",i2.2,"_",i2.2,"_",1a,"_",1a,"_",1a,".SAC")')&
            trim(dir_sac),trim(year_day),trim(year_day),nzhour,nzmin,nzsec,trim(net(is)),trim(sta(is)),trim(com(ic))
            inquire(file=sac,exist=ext)
            if(ext)cycle
            nzero=0
            do i=ibeg,iend
               if(sigall(i).gt.1.e29) nzero=nzero+1
            enddo
            if(real(nzero).gt.real(npts/2))cycle
            write(*,*)'Write to file ',trim(sac)
            sigout(1:npts)=sigall(ibeg:iend)
            do i=1,npts
               avg=sigout(i)
               npart=16
               do while(avg.gt.1.e29.and.npart.gt.1)
                  avg=av_sig(sigout,i,npts,npts/npart)
                  npart = npart/2
               enddo
               if(npart.eq.1)avg = 0.
               sigout(i) = avg
            enddo        ! loop over points
            sachead%delta=dt0
            call write_ext_sac(trim(sac),sigout,sachead,npts,year_b,day_b,nzhour,nzmin,nzsec,0,kho,nerr)
            if(do_decimate.eq.1)call decimate(trim(sac),dt0) ! decimate the data to 1Hz
         enddo       ! loop over each segments
         deallocate(sigout,sigall)
         write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(saclist)
         call system(command) ! remove sac file list
         write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(bash)
         call system(command) ! remove command scripts
         write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(sac_tmp)
         call system(command) ! remove temporary file
      enddo                   ! loop over components
   enddo                      ! loop over station
endif                         ! different seed type
end program
