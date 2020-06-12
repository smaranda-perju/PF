
import Data.List
type Board =[[Piece]]



data Piece = Piece PType deriving (Show, Eq)
data PType = Fox | Dog1 | Dog2 | Dog3 | Dog4 | Empty deriving (Show, Eq)



showPiece :: Piece -> Char
showPiece (Piece Fox) = 'F'
showPiece (Piece Dog1) = '1'
showPiece (Piece Dog2) = '2'
showPiece (Piece Dog3) = '3'
showPiece (Piece Dog4) = '4'
showPiece (Piece Empty) = 'e'

showBoard :: Board -> String
showBoard []= ""
showBoard (l:rest) = showLine l ++ "\n" ++ showBoard rest

showLine :: [Piece] -> String
showLine [] = ""
showLine (p:rest) = [showPiece p] ++ showLine rest


-- Tabla de joc de la inceputul jocului
officialBoard = [[Piece Empty, Piece Empty, Piece Empty, Piece Fox, Piece Empty, Piece Empty, Piece Empty, Piece Empty], [Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty], [Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty], [Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty], [Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty], [Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty], [Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty, Piece Empty], [Piece Dog1, Piece Empty, Piece Dog2, Piece Empty, Piece Dog3, Piece Empty, Piece Dog4, Piece Empty]]

-- Fox move: jucatorul va introduce linia si coloana pe care se afla vulpea 
-- si pozitia pe care doreste sa o mute:
---- 4-3
---- -F-
---- 1-2

foxMove :: Int -> Int -> Int -> Board -> Board
foxMove l c 1 board = findLine (l+1) (c-1) (Piece Fox) board
foxMove l c 2 board = findLine (l+1) (c+1) (Piece Fox) board
foxMove l c 3 board = findLine (l-1) (c+1) (Piece Fox) board
foxMove l c 4 board = findLine (l-1) (c-1) (Piece Fox) board


-- Dog move: jucatorul va introduce cainele pe care vrea sa il mute (1, 2, 3 sau 4),
-- linia si coloana pe care se afla si pozitia pe care doreste sa il mute:
---- 1-2
---- -D-

dogMove :: Int -> Int -> Int -> Int -> Board -> Board
dogMove 1 l c x board = checkDogMove (Piece Dog1) l c x board
dogMove 2 l c x board = checkDogMove (Piece Dog2) l c x board
dogMove 3 l c x board = checkDogMove (Piece Dog3) l c x board
dogMove 4 l c x board = checkDogMove (Piece Dog4) l c x board

checkDogMove :: Piece -> Int -> Int -> Int -> Board -> Board
checkDogMove d l c 1 board = findLine (l-1) (c-1) d board
checkDogMove d l c 2 board = findLine (l-1) (c+1) d board


-- Gaseste linia si apoi coloana pe care se va muta piesa
findLine :: Int -> Int -> Piece -> Board -> Board
findLine 1 c a (h:rest) = (findCol 1 c a h):rest
findLine l c a (h:rest) = h:(findLine (l-1) c a rest)

findCol :: Int -> Int -> Piece -> [Piece] -> [Piece]
findCol l 1 a (p:rest) = a:rest
findCol l c a (p:rest) = p:(findCol l (c-1) a rest)

-- Sterge piesa dupa ce a fost mutata
findPiece :: Int -> Int -> Piece -> Board -> Board
findPiece l c a (h:rest) = if l > 1 then h:(findPiece (l-1) c a rest) else (removePiece l c a h):rest

removePiece :: Int -> Int -> Piece -> [Piece] -> [Piece]
removePiece l 1 a (p:rest) = a:rest
removePiece l c a (p:rest) = p:(removePiece l (c-1) a rest)


-- Au castigat cainii? Returneaza True daca da.
checkWinD :: Int -> Int -> Board -> Bool
checkWinD 1 c board = (findLineF (2) (c+1) board) && (findLineF (2) (c-1) board)
checkWinD l 1 board = (findLineF (l+1) (2) board) && (findLineF (l-1) (2) board)
checkWinD l 8 board = (findLineF (l-1) (7) board) && (findLineF (l+1) (7) board)
checkWinD 8 c board = (findLineF (7) (c+1) board) && (findLineF (7) (c-1) board)
checkWinD l c board = (findLineF (l-1) (c+1) board) && (findLineF (l-1) (c-1) board) && (findLineF (l+1) (c+1) board) && (findLineF (l+1) (c-1) board)


findLineF :: Int -> Int -> Board -> Bool
findLineF 1 c (h:rest) = findColF 1 c h
findLineF l c (h:rest) = (findLineF (l-1) c  rest)

findColF :: Int -> Int ->  [Piece] -> Bool
findColF l 1 (p:rest) = if p /= Piece Empty then True else False
findColF l c (p:rest) = (findColF l (c-1)  rest)


-- A castigat vulpea? Returneaza True daca da.
checkWinF :: Int -> Board -> Bool
checkWinF lin board = lin > (findD 1 (Piece Dog1) board) && lin > (findD 1 (Piece Dog2) board) && lin > (findD 1 (Piece Dog3) board) && lin > (findD 1 (Piece Dog4) board)

findD :: Int -> Piece -> Board -> Int
findD s p (h:rest) = check s 1 rest p h

check :: Int -> Int -> Board -> Piece -> [Piece] -> Int
check s 8 board p (h:rest) = if p == h then s else findD (s+1) p board
check s c board p (h:rest) = if p == h then s else check s (c+1) board p rest




playF :: Board -> IO()
playF board = do putStrLn $ showBoard board
                 putStrLn "introdu linia pe care se afla F "
                 first <- getLine
                 let x = read first :: Int
                 putStrLn "acum coloana "
                 second <- getLine
                 let y = read second :: Int
                 putStrLn "ce mutare doresti sa faci? "
                 third <- getLine
                 let z = read third :: Int
                 let w = if ( z == 1 || z == 2) then x + 1 else x - 1
                 if (checkWinD x y board) == False then playD w (findPiece x y (Piece Empty) (foxMove x y z board)) else putStrLn "Cainii au castigat!"


playD :: Int -> Board -> IO()
playD x board = do putStrLn $ showBoard board
                   putStrLn "introdu cainele pe care doresti sa il muti "
                   caine <- getLine
                   let a = read caine :: Int
                   putStrLn "acum linia pe care se afla "
                   ln <- getLine
                   let b = read ln :: Int
                   putStrLn "acum coloana "
                   col <- getLine
                   let c = read col :: Int
                   putStrLn "ce mutare doresti sa faci? "
                   mut <- getLine
                   let d = read mut :: Int
                   if (checkWinF x board) == False then playF (findPiece b c (Piece Empty) (dogMove a b c d board)) else putStrLn "Vulpea a castigat!"
                 

main :: IO()
main = playF officialBoard
          

          




         
