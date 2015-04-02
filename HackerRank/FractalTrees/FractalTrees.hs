main = do
    n <- readLn :: IO Int
    mapM_ putStrLn $ makeFractal 63 100 50 16 n

emptyChar = '_'
activeChar = '1'

makeRow :: Int -> [Int] -> String
makeRow cols actives =
    [ if i `elem` actives then activeChar else emptyChar | i <- [0..cols-1] ]

emptyRow :: Int -> String
emptyRow cols = makeRow cols []

makeFractal rows cols start size iters =
    let fractal = makeFractalHelper cols [start] size iters
    in (replicate (rows - length fractal) (emptyRow cols)) ++
        reverse fractal

--makeFractalHelper cols actives size iters
makeFractalHelper cols actives size 0 = []
makeFractalHelper cols actives size iters =
    let verts  = replicate size actives
        splits = [ concat $ map (split i) actives | i <- [1..size] ]
        cur_iter = [ makeRow cols row | row <- (verts ++ splits) ]
        rest_iters = makeFractalHelper cols (last splits) (size `div` 2) (iters-1)
    in cur_iter ++ rest_iters

split dx x = [x-dx, x+dx]

