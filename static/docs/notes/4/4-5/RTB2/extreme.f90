!**********************************************************************************************************************************
!
!  Name        : extreme
!  Language    : Fortran 90
!
!  Description : 極値を求める
!
!
!  Input
!    Arguments : none
!    Terminal  : none
!    File      : out.dat
!
!  Output
!    Arguments : none
!    Terminal  : none
!    File      : extreme.txt
!
!  Invocation  : none
!
!  Notes       : none
!**********************************************************************************************************************************

PROGRAM extreme
  IMPLICIT NONE
  DOUBLEPRECISION,DIMENSION(4000,2) :: S = 0D0
  INTEGER :: i

open (10, file="out.dat",status="old")
do i=1,4000
  read(10,*) S(i,1), S(i,2)
end do
close(10)

open(11, file="extreme.txt",status="unknown",position="append")
do i=1,3998
  if (S(i+1,2)>S(i,2)) then
    if (S(i+1,2)>S(i+2,2)) then
      write(11,*) S(i+1,1),S(i+1,2),"lmax"
    end if
  else if (S(i+1,2)<S(i,2)) then
    if (S(i+1,2)<S(i+2,2)) then
      write(11,*) S(i+1,1),S(i+1,2),"lmin"
    end if
  end if
end do
close(11)

END PROGRAM
