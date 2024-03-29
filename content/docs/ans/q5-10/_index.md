# 問題5.11

摂動力Nによる離心率の増減、近点方向の前進・後退が質点Pの軌道上の位置によってどうするようになるか考察せよ。

## 離心率(e)の増減について

離心率についての運動方程式は、

{{< katex >}}
\frac{de}{dt}=-\frac{\eta r\sin{f}}{na^2A}N
{{< /katex >}}

となるので、

- \\( f \gt 0 \\) かつ \\( N \gt 0 \\) のとき、離心率は減少する
- \\( f \gt 0 \\) かつ \\( N \lt 0 \\) のとき、離心率は増加する
- \\( f \lt 0 \\) かつ \\( N \gt 0 \\) のとき、離心率は増加する
- \\( f \lt 0 \\) かつ \\( N \lt 0 \\) のとき、離心率は減少する

## 近点方向( \\( \omega \\) )の前進・後退について

近点引数 \\( \omega \\) についての運動方程式は、

{{< katex >}}
\frac{d\omega}{dt}=\frac{\eta (\cos{u}+e)}{naeA}N
{{< /katex >}}

となるので、

- \\( \cos{u}+e \gt 0 \\) かつ \\( N \gt 0 \\) で \\( \omega \\) は増加、つまり近点方向は前進にする。
- \\( \cos{u}+e \lt 0 \\) かつ \\( N \gt 0 \\) で \\( \omega \\) は減少、つまり近点方向は後退にする。
- \\( \cos{u}+e \gt 0 \\) かつ \\( N \lt 0 \\) で \\( \omega \\) は減少、つまり近点方向は後退にする。
- \\( \cos{u}+e \lt 0 \\) かつ \\( N \lt 0 \\) で \\( \omega \\) は増加、つまり近点方向は前進にする。
