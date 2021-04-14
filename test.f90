program test
parameter(nn=1000000)
real sig(nn)
integer nlen,nerr
real beg,dt
character(2) khole,kh
print *, mod(0.1*1000,0.025*1000)/1000.0
print *, mod(994,int(0.025*1000))/1000.0
print *, int(((2-3)*60.0*60.0+59*60.0+59+994/1000.0)/0.025)+1
print *, int(994/0.025/1000)*0.025*1000
call rsac1('test.sac',sig,nlen,beg,dt,nn,nerr)
call getkhv('khole', khole, nerr)
write(*,'("The khole is ",1a)')khole
kh=trim(khole)//"0"
write(kh,'(1a1,"0")')khole
if(kh.eq."10")write(*,'("The khole is ",1a)')kh
end program
