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
f(x,y) = ( (1/(1+0.25))*(1**3/sqrt( (x + (0.25/(1+0.25) )*1 )**2 + y**2 ) ) + (0.25/(1+0.25))*(1**3/sqrt( (x - (1/(1+0.25) )*1 )**2 + y**2 ) ) + 0.5*(x**2+y**2) ) * 1
set xrange[-2:2]
set yrange[-2:2]

set output "gradient1.jpg"
set zrange[0:10]
splot f(x,y) with line
