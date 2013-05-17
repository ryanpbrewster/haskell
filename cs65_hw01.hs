heron(a,b,c)=sqrt(s*(s-a)*(s-b)*(s-c)) where s=(a+b+c)/2

heron_s(x,y,z)=
        let     a=maximum([x,y,z])
                c=minimum([x,y,z])
                b=x+y+z-a-c
        in sqrt((a+(b+c))*(c-(a-b))*(c+(a-b))*(a+(b-c)))/4

heron_d(x,y,z)=
        heron(x,y,z)/=heron_s(x,y,z)

zeller(year,month,day)=
        let     q=day
                m=if month<3 then month+13 else month+1
                y=if month<3 then year-1 else year
                j=y `div` 100
                k=y `mod` 100
        in (q + (26*m)`div`10 + k + k`div`4 + j`div`4 + 5*j) `mod` 7


extract(g,f:r)=if g(f) then f else extract(g,r)

months=[("jan",1), ("feb",2), ("mar",3),
        ("apr",4), ("may",5), ("jun",6),
        ("jul",7), ("aug",8), ("sep",9),
        ("oct",10), ("nov",11), ("dec",12)]

mon2int(month)=snd $ extract((\x -> fst(x)==month),months)


days=[  (0,"sat"),
        (1,"sun"),
        (2,"mon"),
        (3,"tue"),
        (4,"wed"),
        (5,"thu"),
        (6,"fri")]

int2day(int)=snd $ extract((\x -> fst(x)==int),days)

day_of_week(year,month,day)=
        int2day $ zeller(year,mon2int(month),day)

letter_scores=[ ('a',1), ('b',3),
                ('c',3), ('d',2),
                ('e',1), ('f',4),
                ('g',2), ('h',4),
                ('i',1), ('j',8),
                ('k',5), ('l',1),
                ('m',3), ('n',1),
                ('o',1), ('p',3),
                ('q',10),('r',1),
                ('s',1), ('t',1),
                ('u',1), ('v',4),
                ('w',4), ('x',8),
                ('y',4), ('z',10)]
letter_score(letter)=snd $ extract((\x->fst(x)==letter),letter_scores)
word_score(word)=foldr (+) 0 (map letter_score word)

higher_word(word1,word2)=if word_score(word1)>word_score(word2) then word1 else word2

highest_word([])=""
highest_word(f:r)=higher_word(f,highest_word(r))
