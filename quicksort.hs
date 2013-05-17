quicksort([])=[]
quicksort(p:r)=
	let	lo=[x|x<-r,x<p];
		hi=[x|x<-r,x>=p];
	in quicksort(lo)++quicksort(hi)

main=print(length(quicksort([1..10000])))
