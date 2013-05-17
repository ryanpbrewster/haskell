-- digits(n,b) :: Integer -> [Integer]
digits(n,b)=
	if n==0 then []
	else digits(quot n b)++[(n `mod` b)]
int_digits(n)=digits(n,10)
digisum(n)=sum(int_digits(n))
