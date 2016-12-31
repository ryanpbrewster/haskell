splitStream (x:y:rest) = let (xs,ys) = splitStream rest
                         in (x:xs, y:ys)
