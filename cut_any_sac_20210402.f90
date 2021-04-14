! cut three components of sac file into segments with any given length
! 2016/10/04 -- introduce sacio
! 2018/03/09 -- using only one parameter file
! 2018/04/28 -- using allocatable array
! 2020/10/29 -- extract data in any length with any overlapping percentage
!            -- suppose daily SAC files are ready restored in seed_dir/
!            -- data will be stored like dir_sac/net_sta/year_jday_nzhour_nzmin_nzsec.SAC
!            -- this code will be more effective if the output segement is at least one day
! 2021/04/02 -- for mars data which has u v w components
!            -- original sac files have the name like : XB.ELYSE.02.BHV.R.2020.351.000000.SAC
program main
use sacio
implicit none
type(sac_head):: sachead,sachead1
integer,parameter :: nstmax=2000,nfmax=1000,nmax=4000000
integer seed_type            ! 1 for seed; 0 for miniseed
integer ibeg,iend,nloc,nn,ii
integer nh1,nh2,npart,nnh1,nseg
integer i,j,nsta,error,ifile,ic,dsec,iseg
integer year_b,day_b,year_e,day_e,dhour,dseg,swap
integer do_rm_response,do_decimate,imark,npts,multpt
integer nlen,nzhour,nzmin,nzsec,nzmsec,nerr,nzero
integer is,ih,iy,id,dayb,daye,jday,nh,nnh,nhh1,icom
integer nsec_total,d1,d2,tt
integer nzyear1,nzjday1,nzhour1,nzmin1,nzsec1
integer nzyear2,nzjday2,nzhour2,nzmin2,nzsec2
integer mark(nfmax)
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
real :: dt,beg,stla,stlo
real,allocatable,dimension(:) :: sig,sigo
real,dimension(nmax) :: sigall
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
   !com(1)=trim(co)//'Z'
   !com(2)=trim(co)//'1'
   !com(3)=trim(co)//'2'
   com(1)=trim(co)//'U'
   com(2)=trim(co)//'V'
   com(3)=trim(co)//'W'
endif
! read station list 
open(21,file=list)
do i=1,nstmax
   read(21,*,end=12,err=12)net(i),sta(i)
enddo
12  close(21)
nsta=i-1
! read station list done

dseg=int((1-real(multpt)/100.0)*dsec)            ! the left points without overlapping
!write(*,'("Extract data from ",i4.4,"/",i3.3," to ",i4.4,"/",i3.3)')year_b,day_b,year_e,day_e
nsec_total=0;mark=0
do iy=year_b,year_e                                                     ! loop over year
   jday=365
   if(mod(iy,4).eq.0.and.mod(iy,100).ne.0.or.mod(iy,400).eq.0)jday=366  ! leap year
   dayb=day_b
   if(iy.ne.year_b)dayb=1
   daye=day_e
   if(iy.ne.year_e)daye=jday
   do id=dayb,daye                                ! loop over day
      nsec_total=nsec_total+1 
   enddo
enddo
nseg=int(nsec_total*86400/dseg)                   ! number of segments
write(*,*)'number of sac will be ',nseg
do is=1,nsta                                      ! loop over station
   write(command,'("mkdir -p ",1a,"/",1a,"_",1a)')trim(dir_sac),trim(net(is)),trim(sta(is))
   call system(command)
   do iseg=1,nseg
      do ic=1,icom                                ! loop over component
         sigall=1e30
         tt=(iseg-1)*dseg
         d1=int(tt/86400)
         nzjday1=d1+day_b
         nzyear1=year_b
         jday=365
         if(mod(nzyear1,4).eq.0.and.mod(nzyear1,100).ne.0.or.mod(nzyear1,400).eq.0)jday=366
         do while (nzjday1.gt.jday)
            nzyear1=nzyear1+1
            nzjday1=nzjday1-jday
            jday=365
            if(mod(nzyear1,4).eq.0.and.mod(nzyear1,100).ne.0.or.mod(nzyear1,400).eq.0)jday=366
         enddo
         nzhour1=int((tt-d1*86400)/3600)
         nzmin1=int((tt-d1*86400-nzhour1*3600)/60)
         nzsec1=int(tt-d1*86400-nzhour1*3600-nzmin1*60)

         tt=tt+dsec
         d2=int((tt-1)/86400)
         nzjday2=d2+day_b
         nzyear2=year_b
         jday=365
         if(mod(nzyear2,4).eq.0.and.mod(nzyear2,100).ne.0.or.mod(nzyear2,400).eq.0)jday=366
         do while (nzjday2.gt.jday)
            nzyear2=nzyear2+1
            nzjday2=nzjday2-jday
            jday=365
            if(mod(nzyear2,4).eq.0.and.mod(nzyear2,100).ne.0.or.mod(nzyear2,400).eq.0)jday=366
         enddo
         write(*,*)'nzyear1=',nzyear1,'nzjday1=',nzjday1,'nzyear2=',nzyear2,'nzjday2=',nzjday2
         write(sac,'(1a,"/",1a,"_",1a,"/",i4.4,"_",i3.3,"_",i2.2,"_",i2.2,"_",i2.2,"_",1a,".SAC")')&
         trim(dir_sac),trim(net(is)),trim(sta(is)),nzyear1,nzjday1,nzhour1,nzmin1,nzsec1,trim(com(ic))
         inquire(file=sac,exist=ext)
         if(ext)cycle
         write(saclist,'(1a,"_",1a,"_",i4.4,"_",i3.3,".list")')trim(sta(is)),com(ic),nzyear1,nzjday1
         write(bash,'(   1a,"_",1a,"_",i4.4,"_",i3.3,".bash")')trim(sta(is)),com(ic),nzyear1,nzjday1
         do iy=nzyear1,nzyear2                                                    ! loop over year
            jday=365
            if(mod(iy,4).eq.0.and.mod(iy,100).ne.0.or.mod(iy,400).eq.0)jday=366  ! leap year
            dayb=nzjday1
            if(iy.ne.nzyear1)dayb=1
            daye=nzjday2
            if(iy.ne.nzyear2)daye=jday
            write(*,*)'dayb=',dayb,'daye=',daye
            do id=dayb,daye                                ! loop over day
               write(sac_tmp,'(1a,"/",1a,".",1a,".*.",1a,".*.",i4.4,".",i3.3,".*.SAC")')&
               trim(dir_seed),trim(net(is)),trim(sta(is)),trim(com(ic)),iy,id
               open( 20,file=bash)                ! put all sacfiles in a list
               write(20,'("#!/bin/bash")')
               !if(mark(id+1).ne.0)cycle
               write(20,'("n=`ls",1x,1a,1x,"2>/dev/null | wc -l`")')trim(sac_tmp)
               write(20,'("if [ $n -ge 1 ];then")')
               write(20,'("     ls",1x,1a,1x,">",1a)')trim(sac_tmp),trim(saclist)
               write(20,'("fi")')
               close(20)
               write(command,'("bash",1x,1a)')trim(bash)
               call system(command)                        ! write sac file to saclist
               write(command,'("rm",1x,1a," 2>/dev/null")')trim(bash)
               call system(command)                        ! rmove the bash file
               inquire(file=trim(saclist),exist=ext) 
               if(.not.ext)cycle                           ! if the sac file list exists
               open(100,file=trim(saclist))
               imark=0
               do ifile=1,nfmax                            ! loop over temprary sac files
                  read(100,'(1a180)',err=16,end=16)file    ! read sac file list  
                  call read_sachead(trim(file),sachead,swap,nerr)
                  write(*,*)'read in file ',trim(file)
                  if(imark==0)then
                     if(nerr.eq.-1)cycle
                     kho=sachead%khole(1:2)
                     dt0=sachead%delta
                     imark=imark+1
                     npts=int(dsec/dt0)+1                    ! no. of points for target segment
                  endif
                  if(sachead%khole(1:2)/=kho)cycle
                  if(sachead%delta/=dt0)cycle
!      ******************************************** remove response before paste
                  !write(resp,'(1a,"/resp_",i4.4,"_",i3.3,"/RESP.",1a,".",1a,".",1a,".",1a)')&
                  !trim(dir_resp),iy,id,trim(net(is)),trim(sta(is)),trim(kho),trim(com(ic))
                  write(resp,'(1a,"/RESP.",1a,".",1a,".",1a,".",1a)')&
                  trim(dir_resp),trim(net(is)),trim(sta(is)),trim(kho),trim(com(ic))
                  !if(kho.eq."-1")write(resp,'(1a,"/resp_",i4.4,"_",i3.3,"/RESP.",1a,".",1a,"..",1a)')&
                  !trim(dir_resp),iy,id,trim(net(is)),trim(sta(is)),trim(com(ic))
                  if(kho.eq."-1")write(resp,'(1a,"/RESP.",1a,".",1a,"..",1a)')&
                  trim(dir_resp),iy,id,trim(net(is)),trim(sta(is)),trim(com(ic))
                  !write(*,*)'Remove response ',trim(resp), " from ",trim(file)
                  call rm_resp(bash,trim(file),trim(resp),f1,f2,error)
                  if(error.eq.-1)cycle                     !  response wrong
!      ******************************************** remove response done
                  allocate(sig(sachead%npts),sigo(sachead%npts))
                  !call read_sac(trim(file),sig,sachead,swap,nerr)
                  call read_sac('temp.sac',sig,sachead,swap,nerr)
                  call system("rm temp.sac")
                  if (nerr.eq.-1)cycle
                  call locate(nloc,nzyear1,nzjday1,nzhour1,nzmin1,nzsec1,sachead,sig,sigo) 
                  do i=1,sachead%npts
                     ii=i+nloc-1 
                     if(ii.gt.0.and.ii.le.npts)then
                        if(sigall(ii).gt.1e29)sigall(ii)=sigo(i)
                     endif
                  enddo                ! loop done over points
                  deallocate(sig,sigo)
               enddo                   ! loop done over sacfiles
            16 close(100)              ! done reading the temporary sac files
               write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(saclist)
               call system(command)    ! remove saclist
            enddo                      ! loop done over jday
         enddo                         ! loop done over year
         nzero=0
         do i=1,npts
            if(sigall(i).gt.1.e29) nzero=nzero+1
         enddo
         write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(bash)
         call system(command) ! remove command scripts
         write(*,'(" Percentage of zero points is ",f5.2,"%")')real(nzero)/real(npts)*100.0
         if(real(nzero).gt.real(npts/2))cycle
         write(*,*)'Write to file ',trim(sac)
!         do i=1,npts
!            avg=sigall(i)
!            npart=16
!            do while(avg.gt.1.e29.and.npart.gt.1)
!               avg=av_sig(sigall,i,npts,npts/npart)
!               npart = npart/2
!            enddo
!            if(npart.eq.1)avg = 0.
!            sigall(i) = avg
!         enddo             ! loop over points
         do i=1,npts
            if (sigall(i).gt.1.e29)sigall(i)=0
         enddo
         sachead%delta=dt0
         call write_ext_sac(trim(sac),sigall,sachead,npts,nzyear1,nzjday1,nzhour1,nzmin1,nzsec1,0,kho,nerr)
         if(do_decimate.eq.1)call decimate(trim(sac),dt0) ! decimate the data to 1Hz
      enddo                   ! loop done over segements 
   enddo                      ! loop done over components
enddo                         ! loop done over stations
end program
