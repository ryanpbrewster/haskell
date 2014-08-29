import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (Line)


data Vector = Vector Double Double deriving (Eq)
instance Show Vector where
    show (Vector x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

(Vector x1 y1) `plus`  (Vector x2 y2) = Vector (x1+x2) (y1+y2)
(Vector x1 y1) `minus` (Vector x2 y2) = Vector (x1-x2) (y1-y2)
a `times` (Vector x y) = Vector (a*x) (a*y)

cross :: Vector -> Vector -> Double
cross (Vector x1 y1) (Vector x2 y2) = x1*y2 - x2*y1

data Line = Line Vector Vector deriving (Show)


data Polygon = Polygon [Line] deriving (Show)

fromPoints :: [Vector] -> Polygon
fromPoints pts =
    Polygon [ Line p1 p2 | (p1,p2) <- zip pts (tail pts ++ [head pts]) ]

intersection :: Line -> Line -> Vector
intersection (Line p p') (Line q q') =
    let r = p' `minus` p
        s = q' `minus` q
        rcs = r `cross` s
        u = (q `minus` p) `cross` r / rcs
    in q `plus` (u `times` s)

hitsSegment :: Line -> Line -> Bool
(Line p p') `hitsSegment` (Line q q') =
    let r = p' `minus` p
        s = q' `minus` q
        rcs = r `cross` s
        u = (q `minus` p) `cross` r / rcs
        t = (q `minus` p) `cross` s / rcs
    in rcs /= 0 && 0 <= u && u <= 1 && t >= 0

hitsPolygon :: Line -> Polygon -> Bool
ln `hitsPolygon` (Polygon segs) =
    any (ln `hitsSegment`) segs

pt `inside` (Polygon segs) =
    let ray = Line pt (pt `plus` (Vector 1 0))
        crossings = sum [ if ray `hitsSegment` s then 1 else 0 | s <- segs ]
    in odd crossings


data Building = Building String Polygon deriving (Show)

data Router = Router String Vector deriving (Show)
data RadarLog = RadarLog Vector [(String,Double)] deriving (Show)

-- This will throw a pattern match error if there is a router with
-- fewer than two readings
locateRouters :: M.Map String [Line] -> [Router]
locateRouters mp =
    [ Router str loc | (str, lns) <- M.toList mp
                     , let (ln1:ln2:_) = lns
                     , let loc = intersection ln1 ln2 ]

processRadarLog :: [RadarLog] -> M.Map String [Line]
processRadarLog logs =
    foldl (M.unionWith (++)) M.empty $ map logToMap logs

logToMap :: RadarLog -> M.Map String [Line]
logToMap (RadarLog pt readings) =
    M.fromList [(str, [angleLine pt theta]) | (str, theta) <- readings ]

angleLine v@(Vector x y) theta =
    Line v (Vector (x + cos theta) (y + sin theta))

{--------------------}
{- Parsing nonsense -}
{--------------------}

parseRadarLog :: String -> RadarLog
parseRadarLog input = case parse pRadarLog "" input of
    Right v -> v
    Left _  -> error $ "Could not parse RadarLog from: " ++ show input

parseBuilding :: String -> Building
parseBuilding input = case parse pBuilding "" input of
    Right v -> v
    Left _  -> error $ "Could not parse Building from: " ++ show input

pBuilding = do
    str <- many (noneOf " ")
    pts <- many pVector
    return $ Building str (fromPoints pts)

pVector = do
    spaces
    x <- pNum
    char ';'
    y <- pNum
    return $ Vector x y

pNum = do
    n <- many (oneOf "0123456789.")
    return $ read n

pRadarLog = do
    pt <- pVector
    readings <- many pReading
    return $ RadarLog pt readings

pReading = do
    spaces
    str <- many (noneOf ";")
    char ';'
    theta <- pNum
    return (str, theta)

{- Example -}
foo_b = "B001 14.88;8.94 14.88;33.23 25.29;33.23 25.29;15.88 32.23;15.88 32.23;8.94 14.88;8.94"
foo_rl = "56.51;5.47 56-4c-18-eb-13-8b;59.3493 88-fe-14-a4-aa-2a;303.0239"

foo_rls = "28.76;5.47 88-fe-14-a4-aa-2a;56.9761\n14.88;5.47 88-fe-14-a4-aa-2a;71.9958"
