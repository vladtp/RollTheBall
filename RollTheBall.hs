{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import qualified Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
type Pipe = Char
data Cell = EmptyCell | Cell Pipe | StartCell Pipe | EndCell Pipe
    deriving (Eq, Ord)
instance Show Cell where
    show EmptyCell = show emptySpace
    show (Cell x) = show x
    show (StartCell x) = show x
    show (EndCell x) = show x
{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level (A.Array Position Cell)
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}
cellToChar :: Cell -> Char
cellToChar EmptyCell = emptySpace
cellToChar (Cell x) = x
cellToChar (StartCell x) = x
cellToChar (EndCell x) = x

-- concateneaza linia unui Array intr-un string
concatLine :: A.Array Position Cell -> Position -> Int -> String
concatLine matrix (x, y) maxY
    | y == maxY = [cellToChar $ matrix A.! (x, y)]
    | y < maxY = [cellToChar $ matrix A.! (x, y)] ++ concatLine matrix (x, y + 1) maxY  

instance Show Level where 
    show (Level matrix) = "\n" ++ unlines [concatLine matrix (i, 0) maxY | i <- [0..maxX]]
        where
            (maxX, maxY) = snd $ A.bounds matrix
{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel pos = Level (A.array ((0, 0), maxPos) [((i, j), EmptyCell) | i <- [0..maxX], j <- [0..maxY]])
    where
        maxX = fst pos
        maxY = snd pos
        maxPos = (maxX, maxY)
{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

createCell :: Char -> Cell
createCell pipe
    | pipe == startDown || pipe == startLeft || pipe == startRight || pipe == startUp
    = StartCell pipe
    | pipe == winDown || pipe == winLeft || pipe == winRight || pipe == winUp
    = EndCell pipe
    | pipe == emptySpace = EmptyCell
    | otherwise = Cell pipe

addCell :: (Char, Position) -> Level -> Level
addCell (c, pos) level
    | (x < 0) || (y < 0) || (x > maxX) || (y > maxY) = level
    | (matrix A.! pos) /= EmptyCell = level
    | otherwise = Level $ matrix A.// [(pos, pipe)]
    where
        (Level matrix) = level
        pipe = createCell c
        (x, y) = pos
        (maxX, maxY) = snd $ A.bounds matrix



{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos = foldr addCell (emptyLevel pos)


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}
newPos :: Position -> Directions -> Position
newPos (x, y) dir 
    | dir == North = (x - 1, y)
    | dir == South = (x + 1, y)
    | dir == West = (x, y - 1)
    | dir == East = (x, y + 1) 

canMove :: Level -> Position -> Bool
canMove lvl (x, y) = (x >= 0) && (y >= 0) && (x <= maxX) && (y <= maxY) && (cell == EmptyCell)
    where
        (Level matrix) = lvl
        (maxX, maxY) = snd $ A.bounds matrix
        cell = matrix A.! (x, y)

isEndCell :: Cell -> Bool
isEndCell cell = cell == EndCell winUp || cell == EndCell winRight 
    || cell == EndCell winLeft || cell == EndCell winDown

isStartCell :: Cell -> Bool
isStartCell cell = cell == StartCell startDown || cell == StartCell startUp
    || cell == StartCell startLeft || cell == StartCell startRight

moveCell :: Position -> Directions -> Level -> Level
moveCell pos dir lvl 
    | cell == EmptyCell || isStartCell cell || isEndCell cell = lvl
    | canMove lvl nextPos = newLvl
    | otherwise = lvl
    where
        (Level matrix) = lvl
        cell = matrix A.! pos
        nextPos = newPos pos dir
        newLvl = Level $ matrix A.// [(pos, EmptyCell), (nextPos, cell)]

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}

antiDir :: Directions -> Directions
antiDir d
    | d == North = South
    | d == South = North
    | d == West = East
    | d == East = West

pipeDirection :: Char -> [Directions]
pipeDirection pipe
    | pipe == horPipe = [West, East]
    | pipe == verPipe = [North, South]
    | pipe == topLeft = [South, East]
    | pipe == botLeft = [North, East]
    | pipe == botRight = [West, North]
    | pipe == topRight = [West, South]
    | pipe == startDown = [South]
    | pipe == startRight = [East]
    | pipe == startUp = [North]
    | pipe == startLeft = [West]
    | pipe == winDown = [South]
    | pipe == winUp = [North]
    | pipe == winLeft = [West]
    | pipe == winRight = [East]
    | otherwise = []

getPipe :: Cell -> Char
getPipe (Cell c) = c
getPipe (StartCell c) = c
getPipe (EndCell c) = c
getPipe EmptyCell = emptySpace

hasConnection :: Cell -> Directions -> Bool
hasConnection cell dir
    | cell == EmptyCell = False
    | otherwise = dir `elem` pipeDirection pipe
    where
        pipe = getPipe cell
    

connection :: Cell -> Cell -> Directions -> Bool
connection c1 c2 dir = hasConnection c1 dir && hasConnection c2 opDir
    where
        opDir = antiDir dir

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

isOnBoard :: A.Array Position Cell -> Position -> Bool
isOnBoard matrix (x, y) = (x >= 0) && (y >= 0) && (x <= maxX) && (y <= maxY)
    where
        (maxX, maxY) = snd $ A.bounds matrix

checkConnection :: A.Array Position Cell -> Position -> Directions -> Bool
checkConnection matrix pos dir 
    | isEndCell cell = True
    | isOnBoard matrix nextPos = connection cell nextCell nextDir && checkConnection matrix nextPos (antiDir nextDir)
    | otherwise = False
    where
        nextDir = head $ filter (/= dir) (pipeDirection pipe)
        pipe = getPipe cell
        cell = matrix A.! pos
        nextCell = matrix A.! nextPos
        nextPos = newPos pos nextDir


wonLevel :: Level -> Bool
wonLevel level = checkConnection matrix startPos dir 
    where
        (Level matrix) = level
        startPos = head [(i, j) | i <- [0..maxX], j <- [0..maxY], isStartCell $ matrix A.! (i, j)]
        (maxX, maxY) = snd $ A.bounds matrix
        dir = antiDir $ head $ pipeDirection $ getPipe $ matrix A.! startPos 

generateMoves :: Level -> Position -> [((Position, Directions), Level)]
generateMoves level pos
    | cell == EmptyCell || isEndCell cell || isStartCell cell = []
    | otherwise = [((pos, dir), moveCell pos dir level) | dir <- [North, South, East, West], let nextPos = newPos pos dir, canMove level nextPos]
    where
        cell = matrix A.! pos
        (Level matrix) = level


instance ProblemState Level (Position, Directions) where
    successors lvl = concat [generateMoves lvl (i, j) | i <- [0..maxX], j <- [0..maxY]]
        where
            (Level matrix) = lvl
            (maxX, maxY) = snd $ A.bounds matrix
    isGoal = wonLevel
    reverseAction ((pos, dir), lvl) = ((nextPos, nextDir), nextLvl)
        where
            nextPos = newPos pos dir 
            nextDir = antiDir dir
            nextLvl = moveCell nextPos nextDir lvl