-- divisors(n)=[k|k<-[1..n],n `mod` k==0]
smallest_factor(n,i) = 
	if (i*i>n) then n
	else (if n `mod` i==0 then i else smallest_factor(n,i+1))
factor(n) =
	let p = smallest_factor(n,2) in
	if p == n then [n]
		else p:factor(n `quot` p)
