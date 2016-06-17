import Control.Monad

main = do
  inps <- fmap (tail . lines) getContents
  forM_ (zip inps [1..])  $ \(inp, i) -> do
    putStrLn $ "Case #" ++ show i ++ ": " ++ lastWord inp

lastWord "" = ""
lastWord s =
  let
    (front, maxChar, back) = splitAtMax s
  in maxChar : (lastWord front) ++ back


splitAtMax :: (Eq a, Ord a) => [a] -> ([a], a, [a])
splitAtMax xs =
  let
    x = maximum xs
    idx = lastIndexOf x xs
  in (take idx xs, xs !! idx, drop (idx+1) xs)

lastIndexOf :: Eq a => a -> [a] -> Int
lastIndexOf e xs = last [ idx | (x, idx) <- zip xs [0..], x == e ]
