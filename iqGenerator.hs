import Data.List (transpose, intercalate, intersperse)
import GHC.Parser.CharClass (is_upper)
import Data.Char (toLower, toUpper, isUpper)
import System.Random.Stateful (StdGen, uniformR, uniform, mkStdGen)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Data.Time.Clock.System (getSystemTime, SystemTime (systemSeconds))

main :: IO ()
main = do
    mSize <- getMatrixSize
    d <- difficultyScaler <$> getDifficulty
    sysTime <- getSystemTime
    let secs = fromIntegral $ systemSeconds sysTime
    let g = mkStdGen secs

    let (m1, g') = randMatrix mSize g
    let (ts, _) = genTransforms mSize d g'
    let t = foldl1 (.) (getTransform <$> ts)

    let m2 = t m1
    let m3 = t m2
    let m4 = t m3

    putLn
    print m1
    putLn
    print m2
    putLn
    print m3
    putLn
    putStr' "Press Enter to see solution: "
    _ <- getLine
    putLn
    putStrLn $ "Using transforms: " ++ show ts
    putLn
    print m4
    putLn

type Row = [Char]
newtype Matrix = ToM { froM :: [Row] }

instance Show Matrix
    where show (ToM rows) = ' ' : intercalate "\n " [intersperse ' ' r | r <- rows]

data Rotation = R90 | R180 | R270 deriving (Eq, Show)
data Axis = X | Y | XY | NXY deriving (Eq, Show)
data TransformT =
    RotateT
    | ReflectT
    | HorizontalShiftT
    | VerticalShiftT
    | RowShiftT
    | ColShiftT
    | SwapCaseT
    deriving (Eq)

data Transform =
    Rotate Rotation
    | Reflect Axis
    | HorizontalShift Int
    | VerticalShift Int
    | RowShift Int Int
    | ColShift Int Int
    | SwapCase Int Int
    deriving (Eq, Show)

data Difficulty = Easy | Medium | Hard

getMatrixSize :: IO Int
getMatrixSize = do
    putStr' "Enter Matrix Size >= 3: "
    sizeString <- getLine
    let maybeSize = readMaybe sizeString :: Maybe Int
    mSize <- maybe getMatrixSize return maybeSize
    if mSize >= 3
        then return mSize
        else getMatrixSize

getDifficulty :: IO Difficulty
getDifficulty = do
    putStr' "Easy (e) (default), Medium (m), or Hard (h)?: "
    dStr <- getLine
    let d = parseDifficulty dStr
    maybe getDifficulty return d

parseDifficulty :: String -> Maybe Difficulty
parseDifficulty d
    | d' == "e" || d' == "easy" || d' == "" = Just Easy
    | d' == "m" || d' == "medium" = Just Medium
    | d' == "h" || d' == "hard" = Just Hard
    | otherwise = Nothing
    where d' = toLower <$> d

difficultyScaler :: Difficulty -> Float
difficultyScaler Easy = 2.5
difficultyScaler Medium = 4
difficultyScaler Hard = 6

putStr' :: String -> IO ()
putStr' s = do
    putStr s
    hFlush stdout

putLn :: IO ()
putLn = putStrLn ""

genTransforms :: Int -> Float -> StdGen -> ([Transform], StdGen)
genTransforms mSize targetD = genTransforms' mSize targetD []

genTransforms' :: Int -> Float -> [Transform] -> StdGen -> ([Transform], StdGen)
genTransforms' mSize targetD priorTs g = do
    let transformTs = [RotateT, ReflectT, HorizontalShiftT, VerticalShiftT, RowShiftT, ColShiftT, SwapCaseT]
    let (tt, g') = selectRandom g transformTs
    let (t, g'') = genTransform g' mSize tt
    let ts = t:priorTs
    let d = cumulativeDifficulty ts
    let overshot = d > Just (targetD + 0.5)
    let reached = (abs . (-) targetD <$> d) <= Just 0.5
    if isNothing d || overshot then
        genTransforms' mSize targetD priorTs g''
    else if reached then
        (ts, g'')
    else
        genTransforms' mSize targetD ts g''

genTransform :: StdGen -> Int -> TransformT -> (Transform, StdGen)
genTransform g _ RotateT =
    let (r, g') = selectRandom g [R90, R180, R270]
    in (Rotate r, g')
genTransform g _ ReflectT =
    let (a, g') = selectRandom g [X,Y, XY, NXY]
    in (Reflect a, g')
genTransform g mSize HorizontalShiftT =
    let (s, g') = uniformR (1, mSize - 1) g
    in (HorizontalShift s, g')
genTransform g mSize VerticalShiftT =
    let (s, g') = uniformR (1, mSize - 1) g
    in (VerticalShift s, g')
genTransform g mSize RowShiftT =
    let (row, g') = uniformR (1, mSize) g
        (s, g'') = uniformR (1, mSize - 1) g'
    in (RowShift row s, g'')
genTransform g mSize ColShiftT =
    let (col, g') = uniformR (1, mSize) g
        (s, g'') = uniformR (1, mSize - 1) g'
    in (ColShift col s, g'')
genTransform g mSize SwapCaseT =
    let (row, g') = uniformR (1, mSize) g
        (col, g'') = uniformR (1, mSize) g'
    in (SwapCase row col, g'')

getType :: Transform -> TransformT
getType (Rotate _) = RotateT
getType (Reflect _) = ReflectT
getType (HorizontalShift _) = HorizontalShiftT
getType (VerticalShift _) = VerticalShiftT
getType (RowShift _ _) = RowShiftT
getType (ColShift _ _) = ColShiftT
getType (SwapCase _ _) = SwapCaseT

difficulty :: Transform -> Float
difficulty (Rotate degree) = if degree == R180 then 1 else 2
difficulty (Reflect _) = 1
difficulty (HorizontalShift _) = 1
difficulty (VerticalShift _) = 1
difficulty (RowShift _ s) = if abs s == 1 then 1 else 2
difficulty (ColShift _ s) = if abs s == 1 then 1 else 2
difficulty (SwapCase _ _) = 0.25

getTransform :: Transform -> (Matrix -> Matrix)
getTransform (Rotate R90) = rotate90
getTransform (Rotate R180) = rotate180
getTransform (Rotate R270) = rotate270
getTransform (Reflect X) = xreflect
getTransform (Reflect Y) = yreflect
getTransform (Reflect XY) = xyreflect
getTransform (Reflect NXY) = nxyreflect
getTransform (HorizontalShift s) = hshift s
getTransform (VerticalShift s) = vshift s
getTransform (RowShift row s) = rshift row s
getTransform (ColShift col s) = cshift col s
getTransform (SwapCase row col) = swapcase' row col

cumulativeDifficulty :: [Transform] -> Maybe Float
cumulativeDifficulty [] = Just 0
cumulativeDifficulty [x] = Just $ difficulty x
cumulativeDifficulty (x:xs) = (+)
    <$> additionalDifficulty x xs
    <*> cumulativeDifficulty xs

additionalDifficulty :: Transform -> [Transform] -> Maybe Float
additionalDifficulty t [] = Just $ difficulty t
additionalDifficulty t ts =
    if tDifficulty > minDifficulty
        then minDifficulty
        else maxDifficulty
    where condDifficulties = fmap (conditionalDifficulty t) ts
          maxDifficulty = foldl1 maxFail condDifficulties
          minDifficulty = foldl1 minFail condDifficulties
          tDifficulty = Just $ difficulty t

maxFail :: (Ord a) => Maybe a -> Maybe a -> Maybe a
maxFail a b = max <$> a <*> b

minFail :: (Ord a) => Maybe a -> Maybe a -> Maybe a
minFail a b = min <$> a <*> b

conditionalDifficulty :: Transform -> Transform -> Maybe Float
conditionalDifficulty newT oldT =
    let nd = difficulty newT
        cs = (getType <$> [oldT, newT])
        anyC c = elem c cs
        isCombinedShift (RowShift r1 s1) (RowShift r2 s2) =
            abs (r1 - r2) == 1 && s1 == s2
        isCombinedShift (ColShift c1 s1) (ColShift c2 s2) =
            abs (c1 - c2) == 1 && s1 == s2
        isCombinedShift _ _ = False
    in
        if oldT == newT then
            Nothing
        else if anyC SwapCaseT then
            Just nd
        else if anyC RotateT then
            if anyC ReflectT then
                Nothing
            else if anyC HorizontalShiftT || anyC VerticalShiftT then
                Just $ nd + 1.5
            else
                Just $ nd + 2
        else if anyC ReflectT then
            if anyC HorizontalShiftT || anyC VerticalShiftT then
                Just $ nd + 1.5
            else
                Just $ nd + 2
        else if anyC HorizontalShiftT then
            if anyC VerticalShiftT then
                Just nd
            else
                Just $ nd + 1
        else if anyC VerticalShiftT then
            Just $ nd + 1
        else if anyC RowShiftT then
            if anyC ColShiftT then
                Just $ nd + 1
            else if isCombinedShift oldT newT then
                Just 0.1
            else
                Just nd
        else if isCombinedShift oldT newT then
            Just 0.1
        else
            Just nd

selectRandom :: StdGen -> [a] -> (a, StdGen)
selectRandom g xs =
    let l = length xs
        (i, g') = uniformR (0, l - 1) g
    in (xs !! i, g')

randRow ::  Int -> StdGen -> ([Char], StdGen)
randRow n = randReplicate n randChar

randReplicate :: Int -> (StdGen -> (a, StdGen)) -> StdGen -> ([a], StdGen)
randReplicate n f g
    | n > 0 =
        let (a, g') = f g
            (as, g'') = randReplicate (n-1) f g'
        in (a:as, g'')
    | otherwise = ([], g)

randChar :: StdGen -> (Char, StdGen)
randChar g =
    let (c, g') = uniformR ('a', 'z') g
    in randSwapCase g' c

randSwapCase :: StdGen -> Char -> (Char, StdGen)
randSwapCase g c =
    let (b, g') = randBool g
        c' = condSwap c b
    in (c', g')

randBool :: StdGen -> (Bool, StdGen)
randBool = uniform

condSwap :: Char -> Bool -> Char
condSwap c b
    | b = swapcase c
    | otherwise = c

swapcase :: Char -> Char
swapcase c
    | isUpper c = toLower c
    | otherwise = toUpper c

randMatrix :: Int -> StdGen -> (Matrix, StdGen)
randMatrix n g =
    let (rows, g') = randReplicate n (randRow n) g
    in (ToM rows, g')

shift :: Int -> [a] -> [a]
shift n xs = drop n xs ++ take n xs

posop :: Int -> (a -> a) -> [a] -> [a]
posop p op as = start ++ rop nth ++ end
    where start = take (p-1) as
          nth = drop (p-1) (take p as)
          end = drop p as
          rop [] = []
          rop (x:xs) = op x : xs

rowop :: Int -> (Row -> Row) -> Matrix -> Matrix
rowop r op = ToM . posop r op . froM

colop :: Int -> (Row -> Row) -> Matrix -> Matrix
colop c op = nxyreflect . rowop c op . nxyreflect

cellop :: Int -> Int -> (Char -> Char) -> Matrix -> Matrix
cellop r c op = rowop r (posop c op)

hshift :: Int -> Matrix -> Matrix
hshift n = ToM . map (shift n) . froM

vshift :: Int -> Matrix -> Matrix
vshift n = ToM . shift n . froM

rshift :: Int -> Int -> Matrix -> Matrix
rshift r s = rowop r (shift s)

cshift :: Int -> Int -> Matrix -> Matrix
cshift c s = colop c (shift s)

xreflect :: Matrix -> Matrix
xreflect = ToM . reverse . froM

yreflect :: Matrix -> Matrix
yreflect = ToM . map reverse . froM

nxyreflect :: Matrix -> Matrix
nxyreflect = ToM . transpose . froM

xyreflect :: Matrix -> Matrix
xyreflect = xreflect . nxyreflect . xreflect

rotate180 :: Matrix -> Matrix
rotate180 = xreflect . yreflect

rotate90 :: Matrix -> Matrix
rotate90 = nxyreflect . xreflect

rotate270 :: Matrix -> Matrix
rotate270 = xreflect . nxyreflect

swapcase' :: Int -> Int -> Matrix -> Matrix
swapcase' r c = cellop r c swpcase
    where swpcase ch
              | is_upper ch = toLower ch
              | otherwise = toUpper ch