program kidou_ex2_7b
  implicit none
  integer::n
  real(8)::pi,e,l,u

  pi=4.0d0*atan(1.0d0)
  e=0.967
  l=(10d0/180)*pi
  
  u=l
  n=0
  write(*,*) n,(u/pi)*180
  do n=1,50
     u=l+e*sin(u)
     write(*,*) n,(u/pi)*180
  end do

end program kidou_ex2_7b

