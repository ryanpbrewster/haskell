pwr(m,0)=1
pwr(m,n)=
	if n>0
	then m*pwr(m,n-1)
	else pwr(m,n+1)/m

lg(n)=
	if n<=1
	then 0
	else 1+lg(n/2)

twiddle(a:b:r)=b:a:r
twiddle(l)=l

enum(l)=enum_h(l,0)
enum_h(x:xs,i)=(i,x):enum_h(xs,i+1)
enum_h(_,_)=[]

rem_all(e,x:xs)=
	if e==x
	then rem_all(e,xs)
	else x:rem_all(e,xs)
rem_all(e,[])=[]

uniq(x:xs)=x:rem_all(x,xs)
uniq([])=0


