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
  REAL                       :: nu                   ! m2/(m1+m2)

  ! Internal
  DOUBLE PRECISION :: r1,r2     ! 中心星(惑星)からポテンシャルを求める座標までの距離
  DOUBLE PRECISION :: x         ! 位置座標
  DOUBLE PRECISION :: u         ! 回転座標系でのポテンシャル
  INTEGER          :: i,j,num
  ! Dimension
  DOUBLE PRECISION,DIMENSION(4000,2) :: S = 0D0

  write(*,*) "m1/(m1+m2)"
  read(*,*) nu

  num=0
  do i = 1,4000
    num = num + 1
    x = -2.00D0 + 0.001D0*i
    r1 = abs(x + nu)
    r2 = abs(x - (1-nu))
    u = ( (1-nu)/r1 + nu/r2 + 0.5*(x**2) )
    S(num,1) = x
    S(num,2) = u
  end do

  num=0
  open(10, file="out.dat", status="unknown")
  do num=1,4000
    write(10,*) S(num,1),S(num,2)
  end do
  close(10)

END PROGRAM
