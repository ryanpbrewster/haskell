is_pythag a b c = a*a + b*b == c*c

triples n = [(a,b,c) | a<-[1..n], b<-[a..n], c<-[b..n], is_pythag a b c]
