program kidou_ex2_7c
  !天体と軌道の力学(p.56)[例2.7c]
  
  implicit none
  integer::n
  real(8)::pi,e,l,u

  pi=4.0d0*atan(1.0d0)

  ![例2.7a]冥王星の離心近点離角uをニュートン・ラプソン法で求める。
  write(*,*)"冥王星"
  e=0.249d0 !離心率（文献値）
  l=(12.14d0/180)*pi !平均近点離角をラジアン表記へ変更
  
  u=l
  n=0
  write(*,*) n,(u/pi)*180d0
  
  do n=1,9
     u=u-((u-e*sin(u)-l)/(1-e*cos(u)))
     write(*,*) n,(u/pi)*180d0
  end do

  ![例2.7b]ハレー彗星の離心近点離角uをニュートン・ラプソン法で求める。
  write(*,*)" ハレー彗星"
  e=0.967d0 !離心率（文献値）
  l=(10d0/180)*pi !平均近点離角をラジアン表記へ変更
  
  u=l
  n=0
  write(*,*) n,(u/pi)*180d0
  
  do n=1,20
     u=u-((u-e*sin(u)-l)/(1-e*cos(u)))
     write(*,*) n,(u/pi)*180d0
  end do

end program kidou_ex2_7c
