!**********************************************************************************************************************************
!
!  Name        : RTB
!  Language    : Fortran 90
!
!  Description : 制限三体問題のゼロ速度曲線を描く
!
!
!  Input
!    Arguments   a  : 中心星と惑星の距離
!                nu : 質量比
!    Terminal  : none
!    File      : none
!
!  Output
!    Arguments : none
!    Terminal  : none
!    File      : out.dat, out1.dat-out7.dat
!
!  Invocation  : none
!
!  Notes       : none
!**********************************************************************************************************************************

PROGRAM RTB
  IMPLICIT NONE

! Constants
  REAL,PARAMETER             :: a  = 1E0                     ! 中心星から惑星までの距離
  REAL,PARAMETER             :: nu = 0.2E0                   ! m2/(m1+m2)

! Internal
  DOUBLE PRECISION :: r1,r2     ! 中心星(惑星)からポテンシャルを求める座標までの距離
	DOUBLE PRECISION :: x,y       ! 位置座標
  DOUBLE PRECISION :: u         ! 回転座標系でのポテンシャル
  INTEGER          :: i,j,num
! Dimension
  REAL,DIMENSION(160000,4) :: S = 0D0

  num=0
  do i = 1,400
    do j = 1,400
      num = num + 1
      x = -2.00D0 + 0.01D0*i
      y = -2.00D0 + 0.01D0*j

      r1 = sqrt( (x + nu*a )**2 + y**2 )
      r2 = sqrt( (x - (1-nu)*a )**2 + y**2 )
      u = ( (1-nu)*(a**3/r1) + nu*(a**3/r2) + 0.5*(x**2+y**2) )
      S(num,2) = x
      S(num,3) = y
      S(num,4) = u
    end do
  end do

num=0
open(10, file="out.dat", status="unknown")
open(11, file="out1.dat", status="unknown")
open(12, file="out2.dat", status="unknown")
open(13, file="out3.dat", status="unknown")
open(14, file="out4.dat", status="unknown")
open(15, file="out5.dat", status="unknown")
open(16, file="out6.dat", status="unknown")
open(17, file="out7.dat", status="unknown")

do num=1,160000
  write(10,*) S(num,2),S(num,3),S(num,4)
  if(S(num,4)>2.2) then
    write(11,*) S(num,2),S(num,3)
  end if
  if(S(num,4)>2.0) then
    write(12,*) S(num,2),S(num,3)
  end if
  if(S(num,4)>1.9) then
    write(13,*) S(num,2),S(num,3)
  end if
  if(S(num,4)>1.8) then
    write(14,*) S(num,2),S(num,3)
  end if
  if(S(num,4)>1.7) then
    write(15,*) S(num,2),S(num,3)
  end if
  if(S(num,4)>1.6) then
    write(16,*) S(num,2),S(num,3)
  end if
  if(S(num,4)>1.5) then
    write(17,*) S(num,2),S(num,3)
  end if
end do

close(10)
close(11)
close(12)
close(13)
close(14)
close(15)
close(16)
close(17)


END PROGRAM
