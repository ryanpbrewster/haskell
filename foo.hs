sq(x)=x*x
pow(x,0)=1
pow(x,n)=
	if odd(n) then p*p*x else p*p
	where p=pow(x,n`quot`2)

len([])=0
len(f:l)=1+len(l)

leng(l)=foldr (\x y->1+y) 0 l

fact(n)=foldr(*)(1)([1..n])

m(y,f:r)=y(f):m(y,r)
m(y,[])=[]

func(n) = (\x -> x+n)

foo(n)
	| n > 10	= True
	| otherwise	= False
