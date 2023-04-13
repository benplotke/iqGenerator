import Data.List (transpose, intercalate, intersperse)
import GHC.Parser.CharClass (is_upper)
import Data.Char (toLower, toUpper, isUpper)
import System.Random.Stateful (randomIO, newStdGen, Random (randomRs, randomR))
import Control.Monad (replicateM)
import Control.Applicative (Applicative(liftA2))
import Data.Maybe (isNothing)

main :: IO ()
main = do
    putStrLn "Enter Matrix Size: "
    sizeString <- getLine
    let mSize = read sizeString :: Int
    m1 <- randMatrix mSize

    putStrLn "Enter Difficulty: "
    dString <- getLine
    let d = read dString :: Float
    ts <- genTransforms mSize d
    let t = foldl1 (.) (getTransform <$> ts)

    let m2 = t m1
    let m3 = t m2
    let m4 = t m3

    print m1
    putStrLn ""
    print m2
    putStrLn ""
    print m3
    putStrLn ""
    putStrLn "Press Enter to see solution: "
    _ <- getLine
    print m4

type Row = [Char]
newtype Matrix = ToM { froM :: [Row] }

instance Show Matrix
    where show (ToM rows) = ' ' : intercalate "\n " [intersperse ' ' r | r <- rows]

data Rotation = R90 | R180 | R270 deriving (Eq)
data Axis = X | Y | XY | NXY deriving (Eq)
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
    deriving (Eq)

genTransforms :: Int -> Float -> IO [Transform]
genTransforms mSize targetD = genTransforms' mSize targetD []

genTransforms' :: Int -> Float -> [Transform] -> IO [Transform]
genTransforms' mSize targetD priorTs = do
    let transformTs = [RotateT, ReflectT, HorizontalShiftT, VerticalShiftT, RowShiftT, ColShiftT, SwapCaseT]
    tt <- selectRandom transformTs
    t <- genTransform mSize tt
    let ts = t:priorTs
    let d = cumulativeDifficulty ts
    let overshot = d > Just (targetD + 0.5)
    let reached = (abs . (-) targetD <$> d) < Just 0.5
    if isNothing d || overshot then
        genTransforms' mSize targetD priorTs
    else if reached then
        return ts
    else
        genTransforms' mSize targetD ts


genTransform :: Int -> TransformT -> IO Transform
genTransform _ RotateT = do
    r <- selectRandom [R90, R180, R270]
    return (Rotate r)
genTransform _ ReflectT = do
    a <- selectRandom [X,Y, XY, NXY]
    return (Reflect a)
genTransform mSize HorizontalShiftT = do
    s <- randRange 1 (mSize - 1)
    return (HorizontalShift s)
genTransform mSize VerticalShiftT = do
    s <- randRange 1 (mSize - 1)
    return (VerticalShift s)
genTransform mSize RowShiftT = do
    row <- randRange 1 mSize
    s <- randRange 1 (mSize - 1)
    return (RowShift row s)
genTransform mSize ColShiftT = do
    col <- randRange 1 mSize
    s <- randRange 1 (mSize - 1)
    return (ColShift col s)
genTransform mSize SwapCaseT = do
    row <- randRange 1 mSize
    col <- randRange 1 mSize
    return (SwapCase row col)

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
cumulativeDifficulty (x:xs) = liftA2 (+)
    (additionalDifficulty x xs) (cumulativeDifficulty xs)

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
maxFail Nothing _ = Nothing
maxFail _ Nothing = Nothing
maxFail a b = max a b

minFail :: (Ord a) => Maybe a -> Maybe a -> Maybe a
minFail Nothing _ = Nothing
minFail _ Nothing = Nothing
minFail a b = min a b

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
        if anyC SwapCaseT then
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

selectRandom :: [a] -> IO a
selectRandom xs = do
    let l = length xs
    g <- newStdGen
    let i = fst $ randomR (0, l - 1) g
    return (xs !! i)

randRange :: Int -> Int -> IO Int
randRange s e = fst . randomR (s, e) <$> newStdGen

randRow :: Int -> IO [Char]
randRow n = newStdGen >>= randSwapCases . take n . randomRs ('a', 'z')

randSwapCase :: Char -> IO Char
randSwapCase c = fmap (condSwap c) randBool

randSwapCases :: [Char] -> IO [Char]
randSwapCases = mapM randSwapCase

randBool :: IO Bool
randBool = randomIO

condSwap :: Char -> Bool -> Char
condSwap c b
    | b = swapcase c
    | otherwise = c

swapcase :: Char -> Char
swapcase c
    | isUpper c = toLower c
    | otherwise = toUpper c

randMatrix :: Int -> IO Matrix
randMatrix n = ToM <$> replicateM n (randRow n)

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