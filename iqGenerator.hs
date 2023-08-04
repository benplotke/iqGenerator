import Data.List (transpose, intercalate, intersperse)
import Data.Char (toLower, toUpper, isUpper)
import System.Random.Stateful (StdGen, uniformR, uniform, mkStdGen)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Data.Time.Clock.System (getSystemTime, SystemTime (systemSeconds))
import Control.Monad.Trans.State ( State, state, runState )
import Control.Monad (replicateM)
import Control.Applicative (Applicative(liftA2))

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

    putLn
    print m1
    putLn
    print m2
    putLn
    putStr' "Press Enter to see solution: "
    _ <- getLine
    putLn
    putStrLn $ "Using transforms: " ++ show ts
    putLn
    print m3
    putLn


-- ### Data Structures ###

type Row = [Char]

newtype Matrix = ToM { froM :: [Row] }
instance Show Matrix
    where show (ToM rows) = ' ' : intercalate "\n " [intersperse ' ' r | r <- rows]

data Rotation = R90 | R180 | R270 deriving (Eq, Show, Enum)

data Axis = X | Y | XY | NXY deriving (Eq, Show, Bounded, Enum)

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


-- ### Matrix Transformations ###

-- | Transform datum to transforming function
getTransform :: Transform -> (Matrix -> Matrix)
getTransform t = case t of
    Rotate R90        -> rotate90
    Rotate R180       -> rotate180
    Rotate R270       -> rotate270
    Reflect X         -> xReflect
    Reflect Y         -> yReflect
    Reflect XY        -> xyReflect
    Reflect NXY       -> nxyReflect
    HorizontalShift s -> hShift s
    VerticalShift s   -> vShift s
    RowShift row s    -> rShift row s
    ColShift col s    -> cShift col s
    SwapCase row col  -> swapCase' row col

-- | Perform row operation op on one based index row r of matrix
rowOp :: Int -> (Row -> Row) -> Matrix -> Matrix
rowOp r op = ToM . elemOp r op . froM

-- | Perform "row" operation on one based index column c of matrix
colOp :: Int -> (Row -> Row) -> Matrix -> Matrix
colOp c op = nxyReflect . rowOp c op . nxyReflect

-- | Perform cell operation on one based indecies row r column c of matrix
cellOp :: Int -> Int -> (Char -> Char) -> Matrix -> Matrix
cellOp r c op = rowOp r (elemOp c op)

-- | Shift matrix n places to the left. Use negative n for right shifts
hShift :: Int -> Matrix -> Matrix
hShift n = ToM . map (shift n) . froM

-- | Shift matrix n places up. Use negative n to shift down
vShift :: Int -> Matrix -> Matrix
vShift n = ToM . shift n . froM

-- | Shift row r s places to the left
rShift :: Int -> Int -> Matrix -> Matrix
rShift r s = rowOp r (shift s)

-- | Shift column c s places up
cShift :: Int -> Int -> Matrix -> Matrix
cShift c s = colOp c (shift s)

-- | Reflect matrix over the x-axis
xReflect :: Matrix -> Matrix
xReflect = ToM . reverse . froM

-- | Reflect matrix over the y-axis
yReflect :: Matrix -> Matrix
yReflect = ToM . map reverse . froM

-- | Reflect matrix over the y=-x axis
nxyReflect :: Matrix -> Matrix
nxyReflect = ToM . transpose . froM

-- | Reflect matrix over the y=x axis
xyReflect :: Matrix -> Matrix
xyReflect = xReflect . nxyReflect . xReflect

rotate180 :: Matrix -> Matrix
rotate180 = xReflect . yReflect

rotate90 :: Matrix -> Matrix
rotate90 = nxyReflect . xReflect

rotate270 :: Matrix -> Matrix
rotate270 = xReflect . nxyReflect

-- | Swap the case of the char at row r column c
swapCase' :: Int -> Int -> Matrix -> Matrix
swapCase' r c = cellOp r c swapCase


-- ### Difficulty Calculation ###

-- | Difficulty setting to difficulty number
difficultyScaler :: Difficulty -> Float
difficultyScaler Easy = 2.5
difficultyScaler Medium = 4
difficultyScaler Hard = 6

-- | Independent difficulty of a transform t
difficulty :: Transform -> Float
difficulty t = case t of
    Rotate degree     -> if degree == R180 then 1 else 2
    Reflect _         -> 1
    HorizontalShift _ -> 1
    VerticalShift _   -> 1
    RowShift _ s      -> if abs s == 1 then 1 else 2
    ColShift _ s      -> if abs s == 1 then 1 else 2
    SwapCase _ _      -> 0.25

-- | Difficulty of all the transforms together
--  If any transform is incompatible with any other, we get Nothing
cumulativeDifficulty :: [Transform] -> Maybe Float
cumulativeDifficulty [] = Just 0
cumulativeDifficulty [t] = Just $ difficulty t
cumulativeDifficulty (t:ts) = (+)
    <$> additionalDifficulty t ts
    <*> cumulativeDifficulty ts

-- | Calculate the marginal difficulty of adding transform t to a set of existing transforms ts
--  Return is maybe because some transforms are incompatible. If t is incompatible with any of ts, we get Nothing
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

-- | Calculate the marginal difficulty of adding a transform newT given there is already a transform oldT
--  Return is maybe because some transforms are incompatible, in which case we get Nothing
conditionalDifficulty :: Transform -> Transform -> Maybe Float
conditionalDifficulty newT oldT
    | oldT == newT             = Nothing
    | shiftsOverlap oldT newT  = Nothing
    | shiftsCoincide oldT newT = Just 0.1
    | otherwise                = case (newT, oldT) of

        -- SwapCase.
        -- Swapping the case of a cell does not interact significantly with other transformations
        (SwapCase{}, _)                        -> Just newDifficulty
        (_, SwapCase{})                        -> Just newDifficulty

        -- Rotate
        (Rotate{}, Rotate{})                   -> Nothing
        -- Combining a rotation and a reflection results in a different reflection, so we do not combine them 
        (Rotate{}, Reflect{})                  -> Nothing
        (Reflect{}, Rotate{})                  -> Nothing
        (Rotate{}, HorizontalShift{})          -> Just $ newDifficulty + 1.5
        (HorizontalShift{}, Rotate{})          -> Just $ newDifficulty + 1.5
        (Rotate{}, VerticalShift{})            -> Just $ newDifficulty + 1.5
        (VerticalShift{}, Rotate{})            -> Just $ newDifficulty + 1.5
        -- I do not recall why I decided rotation combined with row or column shifts is harder. Note, the same for Reflect below
        (Rotate{}, _)                          -> Just $ newDifficulty + 2
        (_, Rotate{})                          -> Just $ newDifficulty + 2

        -- Reflect
        (Reflect{}, Reflect{})                 -> Nothing
        (Reflect{}, HorizontalShift{})         -> Just $ newDifficulty + 1.5
        (HorizontalShift{}, Reflect{})         -> Just $ newDifficulty + 1.5
        (Reflect{}, VerticalShift{})           -> Just $ newDifficulty + 1.5
        (VerticalShift{}, Reflect{})           -> Just $ newDifficulty + 1.5
        (Reflect{}, _)                         -> Just $ newDifficulty + 2
        (_, Reflect{})                         -> Just $ newDifficulty + 2

        -- HorizontalShift
        (HorizontalShift{}, HorizontalShift{}) -> Nothing
        (HorizontalShift{}, VerticalShift{})   -> Just newDifficulty
        (VerticalShift{}, HorizontalShift{})   -> Just newDifficulty
        (HorizontalShift{}, _)                 -> Just $ newDifficulty + 1
        (_, HorizontalShift{})                 -> Just $ newDifficulty + 1

        -- VerticalShift
        (VerticalShift{}, VerticalShift{})     -> Nothing
        (VerticalShift{}, _)                   -> Just $ newDifficulty + 1
        (_, VerticalShift{})                   -> Just $ newDifficulty + 1

        -- RowShift
        (RowShift{}, _)                        -> Just $ newDifficulty + 1
        (_, RowShift{})                        -> Just $ newDifficulty + 1

        -- ColShift
        (ColShift{}, _)                        -> Just $ newDifficulty + 1

    where
        newDifficulty = difficulty newT
        shiftsOverlap t1 t2 = case (t1, t2) of
            (HorizontalShift{}, HorizontalShift{}) -> True
            (VerticalShift{}, VerticalShift{})     -> True
            (RowShift r1 _, RowShift r2 _)         -> r1 == r2
            (ColShift c1 _, ColShift c2 _)         -> c1 == c2
            (_, _)                                 -> False
        shiftsCoincide t1 t2 = case (t1, t2) of
            (RowShift r1 s1, RowShift r2 s2) -> abs (r1 - r2) == 1 && s1 == s2
            (ColShift c1 s1, ColShift c2 s2) -> abs (c1 - c2) == 1 && s1 == s2
            (_, _)                           -> False


-- ### Random Board and Transform Generation ###

genTransforms :: Int -> Float -> State StdGen [Transform]
genTransforms mSize targetD = genTransforms' mSize targetD []

genTransforms' :: Int -> Float -> [Transform] -> State StdGen [Transform]
genTransforms' mSize targetDifficulty priorTs = do
    generator <- selectRandom transformGenerators
    newT <- generator mSize
    let ts = newT:priorTs
    let d = cumulativeDifficulty ts
    let overshot = d > Just (targetDifficulty + 0.5)
    let reached = (abs . (-) targetDifficulty <$> d) <= Just 0.5
    if isNothing d || overshot then
        genTransforms' mSize targetDifficulty priorTs
    else if reached then
        return ts
    else
        genTransforms' mSize targetDifficulty ts

transformGenerators :: [Int -> State StdGen Transform]
transformGenerators = 
    [\_ -> do
        r <- selectRandom [R90, R180, R270]
        return $ Rotate r
    ,\_ -> do
        a <- selectRandom [X,Y, XY, NXY]
        return $ Reflect a
    ,\mSize -> do
        s <- state $ uniformR (1, mSize - 1)
        return $ HorizontalShift s
    ,\mSize -> do
        s <- state $ uniformR (1, mSize - 1)
        return $ VerticalShift s
    ,\mSize -> do
        row <- state $ uniformR (1, mSize)
        s <- state $ uniformR (1, mSize - 1)
        return $ RowShift row s
    ,\mSize -> do
        col <- state $ uniformR (1, mSize)
        s <- state $ uniformR (1, mSize - 1)
        return $ ColShift col s
    ,\mSize -> do
        row <- state $ uniformR (1, mSize)
        col <- state $ uniformR (1, mSize)
        return $ SwapCase row col]

-- | Returns a randomly selected item from a list. Beware of passing the empty list
selectRandom :: [a] -> State StdGen a
selectRandom xs = do
    let l = length xs
    i <- state $ uniformR (0, l - 1)
    return $ xs !! i

-- | Returns a randomly generated square matrix of size n
randMatrix :: Int -> State StdGen Matrix
randMatrix n = do
    rows <- replicateM n (randRow n)
    return $ ToM rows

-- | Retruns a randomly generated string of length n
randRow ::  Int -> State StdGen [Char]
randRow n = replicateM n randChar

-- | Returns a randomly selected letter from A-Z or a-z
randChar :: State StdGen Char
randChar = do
    c <- state $ uniformR ('a', 'z')
    randSwapCase c

-- | Randomizes the case of c
randSwapCase :: Char -> State StdGen Char
randSwapCase c = do
    condSwap c <$> randBool

randBool :: State StdGen Bool
randBool = state uniform


-- ### CLI IO ###

-- | Prompt the user to enter a matrix size. Must be 3 or larger
getMatrixSize :: IO Int
getMatrixSize = do
    putStr' "Enter Matrix Size >= 3: "
    sizeString <- getLine
    let maybeSize = readMaybe sizeString :: Maybe Int
    mSize <- maybe getMatrixSize return maybeSize
    if mSize >= 3
        then return mSize
        else getMatrixSize

-- | Prompt the user to enter a difficulty of easy, medium, or hard
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

-- | Prints the string and forces stdout flush
putStr' :: String -> IO ()
putStr' s = do
    putStr s
    hFlush stdout

-- | Prints a newline
putLn :: IO ()
putLn = putStrLn ""


-- ### Helper Functions ###

-- | If b, the swap the case of char c. Otherwise, return c
condSwap :: Char -> Bool -> Char
condSwap c b = if b then swapCase c else c

swapCase :: Char -> Char
swapCase c
    | isUpper c = toLower c
    | otherwise = toUpper c

-- ### Shift list xs n places to the left
shift :: Int -> [a] -> [a]
shift n xs = drop n xs ++ take n xs

-- | Peform operation op on element at one based index op
elemOp :: Int -> (a -> a) -> [a] -> [a]
elemOp idx op xs = start ++ op nth : end
    where start = take (idx-1) xs
          nth = xs !! (idx-1)
          end = drop idx xs

-- | If both inputs are Just, return max. Otherwise, return nothing
maxFail :: (Ord a) => Maybe a -> Maybe a -> Maybe a
maxFail = liftA2 max

-- | If both inputs are Just, return min. Otherwise, return nothing
minFail :: (Ord a) => Maybe a -> Maybe a -> Maybe a
minFail = liftA2 min