! extract three components of sac file from seeds file or miniseed file
! 2016/10/04 --introduce sacio
! 2018/03/09 -- using only one parameter file
! 2018/04/28 -- using allocatable array
! 2021/04/02 -- suppose the data has been extracted from mseed
!            -- mseed are downloaded from
!            https://pds-geosciences.wustl.edu/insight/urn-nasa-pds-insight_seis/data/xb/continuous_waveform/elyse/2019/045/, these
!            sac file has b with value 0.
program main
use sacio
implicit none
type(sac_head):: sachead,sachead1
!integer,parameter :: nn=4000000,nstmax=1000,nfmax=1000
integer,parameter :: nstmax=2000,nfmax=1000
integer seed_type ! 1 for seed; 0 for miniseed
integer ibeg,iend,nloc,nn,ii
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
if(iargc().ne.5)then
   call usage
   stop
endif
i=1
call getarg(1,list)
call getarg(2,tmp)
read(tmp,'(bn,i20)')year_b
call getarg(3,tmp)
read(tmp,'(bn,i20)')day_b
call getarg(4,tmp)
read(tmp,'(bn,i20)')year_e
call getarg(5,tmp)
read(tmp,'(bn,i20)')day_e
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
   com(1)=trim(co)//'U'
   com(2)=trim(co)//'V'
   com(3)=trim(co)//'W'
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
!write(*,*)'nh=',nh
write(*,'("Extract data from ",i4.4,"/",i3.3," to ",i4.4,"/",i3.3)')year_b,day_b,year_e,day_e
do iy=year_b,year_e                               ! loop over year
   jday=365
   if(mod(iy,4).eq.0.and.mod(iy,100).ne.0.or.mod(iy,400).eq.0)jday=366  !leap year
   dayb=day_b
   if(iy.ne.year_b)dayb=1
   daye=day_e
   if(iy.ne.year_e)daye=jday
   do id=dayb,daye                                ! loop over day
      write(year_day,'(i4.4,"_",i3.3)')iy,id
      write(command,'("mkdir -p",1x,1a,1x,"2>/dev/null")')trim(dir_sac)//'/'//trim(year_day)
      call system(command)                        ! make output directory
      write(mdata,'(1a,"/",i4.4,"_",i3.3,".mdata")')trim(dir_mdata),iy,id
      inquire(file=mdata,exist=ext1)
      if(.not.ext1)cycle
      do is=1,nsta          ! loop over station
         do ic=1,icom       ! loop over component
            write(saclist,'(1a,"_",1a,"_",1a,".list")')trim(year_day),trim(sta(is)),com(ic)
            write(bash,'(1a,"_",1a,"_",1a,".bash")')trim(year_day),trim(sta(is)),trim(com(ic))
            write(sac_tmp,'(1a,"/",1a,".",1a,".*.",1a,".*.",i4.4,".",i3.3,".*.SAC")')&
            trim(dir_seed),trim(net(is)),trim(sta(is)),trim(com(ic)),iy,id
            write(*,*)'for day',id,'file:',trim(sac_tmp)
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
            do ifile=1,nfmax                         ! loop over temprary sac files
               read(100,'(1a180)',err=16,end=16)file !read sac file list  
               write(*,*)'Read in file ',trim(file)
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
               if(abs(sachead%delta-dt0).gt.1e-6)cycle
               if(sachead%khole(1:2).ne.kho)cycle
!      ******************************************** remove response before paste
               if(do_rm_response.eq.1)then
                  write(resp,'(1a,"/resp_",1a,"/RESP.",1a,".",1a,".",1a,".",1a)')&
                  trim(dir_resp),trim(year_day),trim(net(is)),trim(sta(is)),trim(kho),trim(com(ic))
                  if(kho.eq."-1")write(resp,'(1a,"/resp_",1a,"/RESP.",1a,".",1a,"..",1a)')&
                  trim(dir_resp),trim(year_day),trim(net(is)),trim(sta(is)),trim(com(ic))

                     !write(resp,'(1a,"/RESP.",1a,".",1a,".",1a,".",1a)')&
                     !trim(dir_resp),trim(net(is)),trim(sta(is)),trim(kho),trim(com(ic))
                     !if(kho.eq."-1")write(resp,'(1a,"/RESP.",1a,".",1a,"..",1a)')&
                     !trim(dir_resp),trim(net(is)),trim(sta(is)),trim(com(ic))
                     !if(kho.eq."-1")write(resp,'(1a,"/RESP.",1a,".",1a,"..",1a)')&
     !             write(*,*)'Remove response ',trim(resp), " from ",trim(file)
                   !trim(dir_resp),trim(net(is)),trim(sta(is)),trim(com(ic))
                     !write(*,*)'Remove response ',trim(resp), " from ",trim(file)
      !               write(*,*) 'bash is ',trim(bash)
                  call rm_resp(bash,trim(file),trim(resp),f1,f2,error)
                  if(error.eq.-1)then
                     open(50,file='resp_missing.log',position='append')
                     write(50,*)iy,id
                     close(50)
                  endif
                  if(error.eq.-1)cycle !  response wrong
               endif
!      ******************************************** remove response done
               allocate(sig(sachead%npts),sigo(sachead%npts))
               call read_sac(trim(file),sig,sachead,swap,nerr)
               if (nerr.eq.-1)cycle
               call abstime(nloc,0,0,0,sachead,sig,sigo) 
               write(*,'("nloc of file file ",1a, " is ",i8.8)')trim(file),nloc
               !find absolute location of the first dot
               do i=1,sachead%npts
                  ii=i+nloc-1 
                  if(ii.gt.0.and.ii.le.nn)then
                     if(sigall(ii).gt.1e29)sigall(ii)=sigo(i)
                  endif
               enddo  ! loop over points
               deallocate(sig,sigo)
            enddo     ! loop over sacfile 
         16 close(100) ! done reading the temporary sac files
            npts=int(dsec/dt0)+1  ! number of points of each segments 
            allocate(sigout(npts))
            dseg=int((1-multpt/100.0)*npts)
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
               write(*,'(" Percentage of zero points is ",f5.2,"%")')real(nzero)/real(npts)*100.0
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
               enddo             ! loop over points
               sachead%delta=dt0
               call write_ext_sac(trim(sac),sigout,sachead,npts,iy,id,nzhour,nzmin,nzsec,0,kho,nerr)
               if(do_decimate.eq.1)call decimate(trim(sac),dt0) ! decimate the data to 1Hz
            enddo                ! loop over each segments
            deallocate(sigout,sigall)
            write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(saclist)
            call system(command) ! remove sac file list
            write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(bash)
            call system(command) ! remove command scripts
               !write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(sac_tmp)
               !call system(command) ! remove temporary file
         enddo                   ! loop over components
      enddo                      ! loop over station
   enddo                            ! loop over day
enddo                               ! loop over year
end program
