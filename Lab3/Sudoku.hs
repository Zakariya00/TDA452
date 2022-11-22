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
cell = frequency [ (1, return Nothing),
                    (9,  do
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

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
