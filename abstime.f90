!subroutine abstime(n1,nnh,nzhour,nzmin,nzsec,nzmsec,npts,dt,sig,sigo)
subroutine abstime(nloc,nzhour,nzmin,nzsec,sachead,sig,sigo)
use sacio
implicit none
type(sac_head) :: sachead
real:: t1,t2,dt
real,dimension (sachead%npts):: sig,sigo
integer :: i,nloc,nzhour,nzmin,nzsec,nzmsec,npts
integer :: nzmsecb,nzmsece,nb,ne
dt=sachead%delta
nzmsecb=sachead%nzmsec
nzmsece=int(nzmsecb/dt/1000.0)*1000
t2=mod(nzmsecb,int(dt*1000))/1000.0
t1=mod(1000-nzmsecb,int(dt*1000))/1000.0
npts=sachead%npts
sigo(1:npts)=sig(1:npts)
if(sachead%nzmsec.ne.0)then
   sigo(1)=(sig(2)-sig(1))*t2/dt+sig(1)
   do i=2,npts
      sigo(i)= (t2*sig(i-1) / t1 + t1*sig(i)/t2) / (t2/t1+t1/t2)
   enddo
endif
nloc=int(((sachead%nzhour-nzhour)*60.0*60.0+(sachead%nzmin-nzmin)*60.0+(sachead%nzsec-nzsec)*1.0+sachead%nzmsec*1.0/1000.0)/dt)+1
end subroutine
