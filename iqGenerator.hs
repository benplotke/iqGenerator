import Data.List (transpose, intercalate, intersperse)
import GHC.Parser.CharClass (is_upper)
import Data.Char (toLower, toUpper, isUpper)
import System.Random.Stateful (StdGen, uniformR, uniform, mkStdGen)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Data.Time.Clock.System (getSystemTime, SystemTime (systemSeconds))
import Control.Monad.Trans.State ( State, state, runState )
import Control.Monad (replicateM)

main :: IO ()
main = do
    mSize <- getMatrixSize
    d <- difficultyScaler <$> getDifficulty
    sysTime <- getSystemTime
    let secs = fromIntegral $ systemSeconds sysTime
    let g = mkStdGen secs

    let (m1, g') = runState (randMatrix mSize) g
    let (ts, _) = runState (genTransforms mSize d) g'
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

genTransforms :: Int -> Float -> State StdGen [Transform]
genTransforms mSize targetD = genTransforms' mSize targetD []

genTransforms' :: Int -> Float -> [Transform] -> State StdGen [Transform]
genTransforms' mSize targetD priorTs = do
    let transformTs = [RotateT, ReflectT, HorizontalShiftT, VerticalShiftT, RowShiftT, ColShiftT, SwapCaseT]
    tt <- selectRandom transformTs
    t <- genTransform mSize tt
    let ts = t:priorTs
    let d = cumulativeDifficulty ts
    let overshot = d > Just (targetD + 0.5)
    let reached = (abs . (-) targetD <$> d) <= Just 0.5
    if isNothing d || overshot then
        genTransforms' mSize targetD priorTs
    else if reached then
        return ts
    else
        genTransforms' mSize targetD ts

genTransform :: Int -> TransformT -> State StdGen Transform
genTransform _ RotateT = do
    r <- selectRandom [R90, R180, R270]
    return $ Rotate r
genTransform _ ReflectT = do
    a <- selectRandom [X,Y, XY, NXY]
    return $ Reflect a
genTransform mSize HorizontalShiftT = do
    s <- state $ uniformR (1, mSize - 1)
    return $ HorizontalShift s
genTransform mSize VerticalShiftT = do
    s <- state $ uniformR (1, mSize - 1)
    return $ VerticalShift s
genTransform mSize RowShiftT = do
    row <- state $ uniformR (1, mSize)
    s <- state $ uniformR (1, mSize - 1)
    return $ RowShift row s
genTransform mSize ColShiftT = do
    col <- state $ uniformR (1, mSize)
    s <- state $ uniformR (1, mSize - 1)
    return $ ColShift col s
genTransform mSize SwapCaseT = do
    row <- state $ uniformR (1, mSize)
    col <- state $ uniformR (1, mSize)
    return $ SwapCase row col

getType :: Transform -> TransformT
getType t = case t of
    Rotate _          -> RotateT
    Reflect _         -> ReflectT
    HorizontalShift _ -> HorizontalShiftT
    VerticalShift _   -> VerticalShiftT
    RowShift _ _      -> RowShiftT
    ColShift _ _      -> ColShiftT
    SwapCase _ _      -> SwapCaseT

difficulty :: Transform -> Float
difficulty t = case t of
    Rotate degree -> if degree == R180 then 1 else 2
    Reflect _         -> 1
    HorizontalShift _ -> 1
    VerticalShift _   -> 1
    RowShift _ s      -> if abs s == 1 then 1 else 2
    ColShift _ s      -> if abs s == 1 then 1 else 2
    SwapCase _ _      -> 0.25

getTransform :: Transform -> (Matrix -> Matrix)
getTransform t = case t of
    Rotate R90        -> rotate90
    Rotate R180       -> rotate180
    Rotate R270       -> rotate270
    Reflect X         -> xreflect
    Reflect Y         -> yreflect
    Reflect XY        -> xyreflect
    Reflect NXY       -> nxyreflect
    HorizontalShift s -> hshift s
    VerticalShift s   -> vshift s
    RowShift row s    -> rshift row s
    ColShift col s    -> cshift col s
    SwapCase row col  -> swapcase' row col

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
conditionalDifficulty newT oldT
    | oldT == newT                            = Nothing
    | hasOne  SwapCaseT                       = Just nd
    | hasBoth RotateT ReflectT                = Nothing
    | hasBoth RotateT HorizontalShiftT        = Just $ nd + 1.5
    | hasBoth RotateT VerticalShiftT          = Just $ nd + 1.5
    | hasOne  RotateT                         = Just $ nd + 2
    | hasBoth ReflectT HorizontalShiftT       = Just $ nd + 1.5
    | hasBoth ReflectT VerticalShiftT         = Just $ nd + 1.5
    | hasOne  ReflectT                        = Just $ nd + 2
    | hasBoth HorizontalShiftT VerticalShiftT = Just nd
    | hasOne  HorizontalShiftT                = Just $ nd + 1
    | hasOne  VerticalShiftT                  = Just $ nd + 1
    | hasBoth RowShiftT ColShiftT             = Just $ nd + 1
    | isCombinedShift oldT newT               = Just 0.1
    | otherwise = Nothing
    where
        nd = difficulty newT
        cs = getType <$> [oldT, newT]
        hasOne c = elem c cs
        hasBoth t1 t2 = hasOne t1 && hasOne t2
        isCombinedShift (RowShift r1 s1) (RowShift r2 s2) =
            abs (r1 - r2) == 1 && s1 == s2
        isCombinedShift (ColShift c1 s1) (ColShift c2 s2) =
            abs (c1 - c2) == 1 && s1 == s2
        isCombinedShift _ _ = False

selectRandom :: [a] -> State StdGen a
selectRandom xs = do
    let l = length xs
    i <- state $ uniformR (0, l - 1)
    return $ xs !! i

randMatrix :: Int -> State StdGen Matrix
randMatrix n = do
    rows <- replicateM n (randRow n)
    return $ ToM rows

randRow ::  Int -> State StdGen [Char]
randRow n = replicateM n randChar

randChar :: State StdGen Char
randChar = do
    c <- state $ uniformR ('a', 'z')
    randSwapCase c

randSwapCase :: Char -> State StdGen Char
randSwapCase c = do
    condSwap c <$> randBool

randBool :: State StdGen Bool
randBool = state uniform

condSwap :: Char -> Bool -> Char
condSwap c b = if b then swapcase c else c

swapcase :: Char -> Char
swapcase c
    | isUpper c = toLower c
    | otherwise = toUpper c

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