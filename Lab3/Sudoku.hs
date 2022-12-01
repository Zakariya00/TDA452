module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe
import Data.List

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = validRows && validColAndValues
  where
     validRows          = length (rows s) == 9
     validColAndValues  = and (map (\row -> length row == 9 &&
                           and (map validElement row)) (rows s))

-- | Helper function, checks if cell values are valid for Sudoku
validElement :: Maybe Int -> Bool
validElement Nothing   = True
validElement (Just e)  = elem e [1..9]

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled s = and (map (\row -> and (map filledElement row)) (rows s))

filledElement :: Maybe Int -> Bool
filledElement (Just e)  = elem e [1..9]
filledElement Nothing   = False

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStrLn (printRow (rows s))

-- | Helper function, prints each row recursively
printRow:: [[Maybe Int]] -> String
printRow []      = ""
printRow [x]     = map toChar x
printRow (x:xs)  = map toChar x ++ "\n" ++ printRow xs

-- | Helper function, for converting Maybe Int to char
toChar :: Maybe Int -> Char
toChar Nothing   = '.'
toChar (Just e)  = intToDigit e

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fPath =
                   do
                      fileData <- readFile fPath
                      let fLines = lines fileData
                      let s      = Sudoku $ map (map fromChar) fLines
                      if (isSudoku s) then return s
                      else error "Not a valid Sudoku <Failed isSudoku Check>"

-- | Helper function, for converting Char to Maybe Int
fromChar :: Char -> Maybe Int
fromChar '.' = Nothing
fromChar  c
    | isDigit c   = Just (digitToInt c)
    | otherwise   = error "Not a valid Sudoku <Failed fromChar Check>"
------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [ (9, return Nothing),
                    (1,  do
                           n <- choose (1,9)
                           let e = Just n
                           return e)
                           ]


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
               do
                 r <- vectorOf 9 (vectorOf 9 cell)
                 let rs = Sudoku r
                 return rs
 -- hint: get to know the QuickCheck function vectorOf

-- * C3

-- | Property that expresses that each generated Sudoku actually is a Sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

-- |  checks if supplied block does not contain the same digit twice
isOkayBlock :: Block -> Bool
isOkayBlock []            = True
isOkayBlock (Nothing:r)   = isOkayBlock r
isOkayBlock (c:r)
    | elem c r     = False
    | otherwise    = isOkayBlock r


-- * D2

 -- |  Given a Sudoku, creates a list of all blocks of that Sudoku. [9 rows, 9 col, 27 blocks]
blocks :: Sudoku -> [Block]
blocks s  = rows s ++ transpose (rows s) ++ miniblocks (rows s) []

 -- |  Helper function for creating 3x3 blocks out of given Sudoku rows
miniblocks:: [Row] -> [Block] -> [Block]
miniblocks [] acc             = acc
miniblocks (r1:r2:r3:rs) acc  = miniblocks rs (
                                                (c11++c21++c31):(c12++c22++c32):(c13++c23++c33):acc
                                                )
  where
     [c11, c12, c13]  = splitrow r1
     [c21, c22, c23]  = splitrow r2
     [c31, c32, c33]  = splitrow r3

 -- |  Helper function for splitting a row into 3 with 3x1
splitrow:: Row -> [[Maybe Int]]
splitrow r = [r1,r2,r3]
  where
     (r1, rem)   = splitAt 3 r
     (r2, rem1)  = splitAt 3 rem
     (r3, _   )  = splitAt 3 rem1

 -- |  Property that states that, for each Sudoku, there are 27 blocks, and each block has exactly 9 cells
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s = validBlocks && validBlockCells
  where
     validBlocks          = length (blocks s) == 27
     validBlockCells      = and (map (\block -> length block == 9 ) (blocks s))


-- * D3

 -- | Given a Sudoku, checks that all rows, columns, and 3x3 blocks do not contain repeated digits
isOkay :: Sudoku -> Bool
isOkay s = and (map isOkayBlock (blocks s))


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

 -- | Given a Sudoku returns a list of the positions in the Sudoku that are still blank
blanks :: Sudoku -> [Pos]
blanks s = [(x,y) | x <- [0..8],
                    y <- [0..8],
                    ((rows s !! x)!!y) == Nothing]

 -- | Property that checks that the blanks of allBlankSudoku are indeed all of the expected 9x9 cells
prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks  =  length (rows s) == 9 &&
                            and (map (\row -> length row == 9 &&
                             and (map (\x -> x == Nothing) row)) (rows s))
 where s = allBlankSudoku


-- * E2

 -- | Given a list, and a tuple containing an index in the list and a new value,
 --   updates the given list with the new value at the given index
(!!=) :: [a] -> (Int,a) -> [a]
[] !!= (_,_)     = error "Empty, index automatically out of bound"
[x] !!= (0,y)    = [y]
xs !!= (i,y)
    | i > (length xs - 1)  || (i < 0) = error "Index Out Of Bound"
    | otherwise                       = newL
  where
     (h,(_:t))      = splitAt i xs
     newL           = h ++ y:t

 -- | Property that state(s) the expected properties of this function
prop_bangBangEquals_correct :: Sudoku -> (Int,Row) -> Bool
prop_bangBangEquals_correct xs (i,e) = length (rows xs) == length (rows ys) && ((rows ys)!!abs i') == e
  where ys = Sudoku ((rows xs) !!= (abs i',e))
        i' = mod i 8


-- * E3

 -- | Given a Sudoku, a position, and a new cell value,
 --   updates the given Sudoku at the given position with the new value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s (i,j) x
    | (i > 8) || (j > 8) || (i < 0) || (j < 0)   = error "Index Out Of Bound"
    | otherwise = updatedS
  where
    row            = (rows s)!!i
    updatedR       = row !!= (j,x)
    updatedS       = Sudoku ((rows s) !!= (i,updatedR))

 -- | Property that checks that the updated position really has gotten the new value
prop_update_updated :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update_updated s (i,j) e = ((rows s'!!abs i')!!abs j') == e
  where s' = update s (abs i', abs j') e
        i' = mod i 8
        j' = mod j 8


------------------------------------------------------------------------------

-- * F1

 -- | Solves a given Sudoku
solve :: Sudoku -> Maybe Sudoku
solve s
     | not (isSudoku s || isOkay s) = Nothing
     | isFilled s                   = Just s
     | otherwise                    = solve' s (blanks s)

solve' :: Sudoku -> [Pos] -> Maybe Sudoku
solve' s [] = Just s
solve' s (p:ps) = listToMaybe (catMaybes (map solve updatedL))
 where updatedL = [update s p (Just x) | x <- [1..9], isOkay (update s p (Just x))]


-- * F2

 -- | Produces instructions for reading the Sudoku from the given file,
 --  solving it, and printing the answer
readAndSolve :: FilePath -> IO ()
readAndSolve fPath =
                   do
                   s <- readSudoku fPath
                   case solve s of
                    Nothing -> putStrLn "(no solution)"
                    Just s -> printSudoku s


-- * F3

 -- | Checks, given two Sudokus, whether the first one is a solution,
 --   and also whether the first one is a solution of the second one.
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol s = isOkay sol && isFilled sol && isSolOf
 where isSolOf = and [(((rows sol!!x)!!y) == ((rows s!!x)!!y)) | x <- [0..8],
                                                                 y <- [0..8],
                                                                 ((rows s !! x)!!y) /= Nothing]


-- * F4

 -- | Property that says that the function solve is sound. Soundness means that every supposed
 --   solution produced by solve actually is a valid solution to the original problem.
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isSudoku s && isOkay s ==> isSolutionOf sol s
  where Just sol = solve s

fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 10 } prop
