set xrange[-2:2]
set yrange[0:10]
set xlabel "X"
set ylabel "Z"

set terminal jpeg

set output "1d.jpg"
plot "out.dat"
