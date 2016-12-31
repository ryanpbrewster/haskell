powmod(x,0,m)=1
powmod(x,n,m)=
	if odd(n) then (p*p*x)`mod`m else (p*p)`mod`m
	where p=powmod(x,n`quot`2,m)
