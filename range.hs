inc([],[])=[0] -- cur is now > target, let's make cur longer than tar so rangeH knows to stop
inc(cur,tar)=
	if head(cur) < head(tar) then (head(cur)+1):tail(cur)
	else 0:inc(tail(cur),tail(tar))
rangeH(cur,tar)=
	if length(cur)/=length(tar) then []
	else cur:rangeH(inc(cur,tar),tar)
range(s,e)=
	map reverse (rangeH(s,e))
