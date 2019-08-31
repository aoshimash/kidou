program kidou_ex2_7a
  !天体と軌道の力学(p.56)[例2.7a]
  !iとu_iの値を出力する
  implicit none
  integer::n
  real(8)::pi,e,l,u

  pi=4.0d0*atan(1.0d0)
  e=0.249
  l=(12.14/180)*pi
  
  u=l
  n=0
  write(*,*) n,(u/pi)*180
  do n=1,9
     u=l+e*sin(u)
     write(*,*) n,(u/pi)*180
  end do

end program kidou_ex2_7a
