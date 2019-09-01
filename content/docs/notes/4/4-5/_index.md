# ゼロ速度曲線の描画

P.111の表はどのように規格化されているのか明示されていないのでなんだかよくわからない。だから自分で書き直す。

## ヤコビ積分の \\( \Omega \\) を無次元化

ヤコビ積分

{{< katex >}}
\frac{1}{2}\left( \dot{X}^2 + \dot{Y}^2 + \dot{Z}^2 \right) - \Omega = -C
{{< /katex >}}

の位置だけの関数 \\( \Omega \\) を無次元化する。
(4.26)式を(4.17)と(4.23)を合わせた形に書き直す。

{{< katex >}}
\Omega = n^{\prime2} \left[ \frac{m_1}{m_1+m_2} \frac{a^{\prime3}}{r_1} + \frac{m_2}{m_1+m_2} \frac{a^{\prime 3}}{r_2} + \frac{1}{2}\left( X^2 + Y^2 \right) \right]
{{< /katex >}}

ちなみに、このヤコビ積分は制限三体問題の保存量。単位はエネルギーとは違う。制限三体問題のエネルギー保存しない。(もちろん系全体では保存している)
さらに、\\( \nu = \frac{m_2}{m_1+m_2} \\) を使うと、上式は次のように書き換えられる。

{{< katex >}}
\Omega = n^{\prime2} \left[ \left( 1-\nu \right) \frac{a^{\prime3}}{r_1} + \nu \frac{a^{\prime3}}{r_2} + \frac{1}{2}\left( X^2 + Y^2 \right) \right]
{{< /katex >}}

万有引力定数はケプラーの第三法則より \\( n^\prime \\) に含まれており、陽には出てこない。距離に関しては太陽と惑星の間の距離 \\( a^\prime \\) で規格化することとすると、上式の変数は以下のように変換される。

{{< katex >}}
X = a^\prime \cdot X^\prime \\
Y = a^\prime \cdot Y^\prime \\
r_1 = a^\prime \cdot r_1^\prime \\
r_2 = a^\prime \cdot r_2^\prime
{{< /katex >}}


以上を用いてさらに変換すると、

{{< katex >}}
\Omega = n^{\prime2}a^{\prime2} \left[ \left( 1-\nu \right) \frac{1}{r^\prime_1} + \nu \frac{1}{r^\prime_2} + \frac{1}{2}\left( X^{\prime2} + Y^{\prime2} \right) \right]
{{< /katex >}}


## プログラム

この式を \\( n^{\prime}a^{\prime}=1 \\) としてゼロ速度曲線を描いたプログラムが以下。結局教科書はどのような規格化を行っているのかわからなかった。何か現実の系を再現しているのかとも考えたが \\( \nu=0.2 \\) 程大きな惑星は太陽系にはない。規格化の仕方をかえたとしても \\( Z \\) 軸方向に曲面が平行移動するだけで、曲面の形自体は変わらない。

``` fortran
!**************************************************
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
!**************************************************

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

```

## (図)ゼロ速度曲線と運動可能領域( \\( \nu=0.2 \\) )

色がついている部分が運動可能領域

\\( n^{\prime}a^{\prime}=1 \\) で規格化

{{< figure src="./RTB/C=2_2.jpg" title="(a) C=2.2" >}}
{{< figure src="./RTB/C=2_1.jpg" title="(b) C=2.1" >}}
{{< figure src="./RTB/C=1_9.jpg" title="(c) C=1.9" >}}
{{< figure src="./RTB/C=1_8.jpg" title="(d) C=1.8" >}}
{{< figure src="./RTB/C=1_7.jpg" title="(e) C=1.7" >}}
{{< figure src="./RTB/C=1_6.jpg" title="(f) C=1.6" >}}


## \\( Y=0 \\) での \\( \Omega \\) 値

なぜ教科書と上図ではCの値が少しずれるのかわからなかったので、\\( L_1,L_2,L_3 \\) の値を求めるために、以下のようなコードを書いた。
ここでも規格化は \\( n^{\prime}a^{\prime}=1 \\)。

### \\( \Omega \\) の値を求める

上のコードを一次元にしただけだからほぼ同じ。

``` Fortran
!**************************************************
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
!**************************************************

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
```


### 極値の抜き出し

``` Fortran
!**************************************************
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
!**************************************************

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
```

### 極値抜き出しプログラムの結果

<img src="./RTB2/1d.jpg" width="100%">
<p class="text-center"> 図. 横軸$x$, 縦軸$\Omega$ (Y=0)</p>

``` text
-1.0830000000000000        1.5986602715874194      lmin   ← L3
-0.19999999999999996       268435456.21999997      lmax   ← m1
0.43800000000000017        1.9023266860095391      lmin   ← L1
0.80000000000000027        16777217.745000023      lmax   ← m2
 1.2709999999999999        1.7761966968206810      lmin   ← L2
```

ゼロ速度曲線がラグランジュ点で交差する \\( \Omega \\) の値は

<ul>
<li> 教科書: &nbsp;&nbsp;&nbsp; \\( L_1:1.9823,L_2:1.8562,L_3:1.6787 \\) </li>
<li> 計算結果: \\( L_1:1.9023,L_2:1.7761,L_3:1.5985 \\) </li>
</ul>

となり、若干違う。理由はわからない。


## (おまけ) $\Omega$の3Dグラフ

<img src="./RTB/gradient1.jpg" width="100%">

## 付録. 描画に使ったpltファイル

``` plt
set xrange[-2:2]
set yrange[-2:2]
set xlabel "X"
set ylabel "Y"
set size square

set terminal jpeg

set output "C=2_2.jpg"
plot "out1.dat"

set output "C=2_1.jpg"
plot "out2.dat"

set output "C=2_0.jpg"
plot "out3.dat"

set output "C=1_9.jpg"
plot "out4.dat"

set output "C=1_8.jpg"
plot "out5.dat"

set output "C=1_7.jpg"
plot "out6.dat"

set output "C=1_6.jpg"
plot "out7.dat"


set isosamples 50
f(x,y) = ( (1/(1+0.25))*(1**3/sqrt( (x + (0.25/(1+0.25) )*1 )**2 + y**2 ) )
         + (0.25/(1+0.25))*(1**3/sqrt( (x - (1/(1+0.25) )*1 )**2 + y**2 ) )
         + 0.5*(x**2+y**2) ) * 1
set xrange[-2:2]
set yrange[-2:2]

set output "gradient1.jpg"
set zrange[0:10]
splot f(x,y) with line
```
