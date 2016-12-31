data Property = Even | Odd | GT10 | Positive | Negative | Square deriving (Show)

properties1 xs =
  (if any even     xs then [Even]     else []) ++
  (if any odd      xs then [Odd]      else []) ++
  (if any (> 10)   xs then [GT10]     else []) ++
  (if any (> 0)    xs then [Positive] else []) ++
  (if any (< 0)    xs then [Negative] else []) ++
  (if any isSquare xs then [Square]   else [])

properties2 xs =
  let predMap = [ (even, Even)
                , (odd, Odd)
                , ((> 10), GT10)
                , ((> 0), Positive)
                , ((< 0), Negative)
                , (isSquare, Square)
                ]
  in [ prop | (p, prop) <- predMap, any p xs ]

isSquare x =
  let r = round $ sqrt $ fromIntegral x
  in r*r == x
