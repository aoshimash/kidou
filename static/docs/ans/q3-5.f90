program kidou_q3_5
  !ホーマン軌道を使って太陽から離れていく場合に必要な加速量の合計が最大となるa2を求める。
  implicit none
  integer:: i
  real(8):: a1,a2,e,dv,x,dv_max,a2_max
  
  a1=1d0 !太陽から地球までの距離（天文単位）
  x=1d0 !μの値 （なんでもいい:a2の最大値を求めるだけなので、計算しやすいように規格化）
  dv_max=0d0 !今回はdvの最大値が0以上になることがわかってるので、適当に0にした
  a2_max=0d0
  
  do i=1,100000
     a2=i*0.001d0
     dv=sqrt((2d0*a2*x)/(1d0+a2))-sqrt(x)+sqrt(x/a2)-sqrt(2*x/(a2*(1d0+a2)))
     if (dv>dv_max) then
        dv_max=dv
        a2_max=a2
     end if
  end do

  write(*,*) a2_max

end program kidou_q3_5
