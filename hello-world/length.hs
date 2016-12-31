len([])=0
len(f:r)=1+len(r)
leng(l)=foldr (\x y->1+y) 0 l
main=print(length([1..100000]))
