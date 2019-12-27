module Extensions where 

import Types

findLastId :: [Game] -> Int
findLastId [] = 1
findLastId (game:games) = (+) 1 $ gameId game

returnOnlyAvailableGames :: [Game] -> [Game]
returnOnlyAvailableGames [] = []
returnOnlyAvailableGames (g:gs)
  | (isGameStarted $ info g) == False && (playersCount $ info g) < 2 = g : returnOnlyAvailableGames gs
  | otherwise = returnOnlyAvailableGames gs

changeGames :: Int -> [Game] -> [Game]
changeGames _ [] = []
changeGames gameId ((Game id (ResponseForWhileTrue isStarted isEnded playersTurn playersCount changes) field):xs)
  | gameId == id = (Game id (ResponseForWhileTrue isStarted isEnded playersTurn (playersCount + 1) changes) field) : xs
  | otherwise = (Game id (ResponseForWhileTrue isStarted isEnded playersTurn playersCount changes) field) : changeGames gameId xs

startOneGame :: Int -> [Game] -> [Game]
startOneGame _ [] = []
startOneGame gameId ((Game id (ResponseForWhileTrue isStarted isEnded playersTurn playersCount changes) field):xs)
  | gameId == id = (Game id (ResponseForWhileTrue True isEnded playersTurn playersCount changes) field) : xs
  | otherwise = (Game id (ResponseForWhileTrue isStarted isEnded playersTurn playersCount changes) field) : changeGames gameId xs

findGame :: Int -> [Game] -> Maybe Game
findGame _ [] = Nothing
findGame findId (game:games)
  | findId == (gameId game) = Just game
  | otherwise = findGame findId games

emptyBoard :: Int -> [String]
emptyBoard 0 = []
emptyBoard size = generateRow size : emptyBoard (size - 1)

generateRow :: Int -> String
generateRow 0 = []
generateRow n = 'n':generateRow (n - 1)

changeBoardState :: MakeTurnChanges -> [Game] -> [Game]
changeBoardState makeTurnChanges ((Game id (ResponseForWhileTrue isStarted isEnded playersTurn playersCount changes) field):gs)
  | (gameNumber (turnInfo makeTurnChanges)) == id = do
    let game = (Game id (ResponseForWhileTrue isStarted isEnded (getNextPlayer playersTurn) playersCount (turnChanges makeTurnChanges)) (setChanges (turnChanges makeTurnChanges) field))
    let newGame = (Game id (ResponseForWhileTrue isStarted (checkIsGameEnded game $ show playersTurn !! 0) (getNextPlayer playersTurn) playersCount (turnChanges makeTurnChanges)) (setChanges (turnChanges makeTurnChanges) field))
    newGame : gs
  | otherwise = (Game id (ResponseForWhileTrue isStarted isEnded playersTurn playersCount changes) field) : changeBoardState makeTurnChanges gs

setChanges :: Changes -> Field -> Field
setChanges changes field = do
  let (Field size board) = field
  Field size $ changeBoard changes board

changeBoard :: Changes -> [String] -> [String]
changeBoard _ [] = []
changeBoard (Changes x y l) (row:rows)
  | x == 0 = (changeRow y l row) : rows
  | otherwise = row : changeBoard (Changes (x - 1) y l) rows

getNextPlayer :: Int -> Int
getNextPlayer playerTurnNumber
    | playerTurnNumber == 2 = 1
    | otherwise = playerTurnNumber + 1

changeRow :: Int -> Char -> String -> String
changeRow y l (cell:cells)
  | y == 0 = l : cells
  | otherwise = cell : changeRow (y - 1) l cells

isTurnAvailable :: Changes -> Field -> Bool
isTurnAvailable (Changes x y _) (Field _ board) = (board !! x) !! y == 'n'

checkIsGameEnded :: Game -> Char -> Bool
checkIsGameEnded (Game _ _ (Field size board)) playerTurnNumber = do
  let starts = findStartPoints (size - 1) playerTurnNumber (board !! (size - 1)) 0
  if starts == []
    then False
    else checkIsGameEnded' (collectAllPaths (head starts) playerTurnNumber starts board)

checkIsGameEnded' :: [[Point]] -> Bool
checkIsGameEnded' [] = False
checkIsGameEnded' (points:arrays)
  | (checkSequence points False False) == True = True
  | otherwise = checkIsGameEnded' arrays

checkSequence :: [Point] -> Bool -> Bool -> Bool
checkSequence [] firstCondition secondCondition = firstCondition && secondCondition
checkSequence ((Point x y):points) firstCondition secondCondition
  | y == 0 && x == y = checkSequence points True True
  | y == 0 && x /= y = checkSequence points True secondCondition
  | x == y && y /= 0 = checkSequence points firstCondition True
  | otherwise = checkSequence points firstCondition secondCondition

findStartPoints :: Int -> Char -> String -> Int -> [[Point]]
findStartPoints _ _ [] _ = []
findStartPoints x l (c:cs) index
  | l == c = ((Point x index) : []) : (findStartPoints x l cs $ index + 1)
  | otherwise = findStartPoints x l cs $ index + 1

collectAllPaths :: [Point] -> Char -> [[Point]] -> [String] -> [[Point]]
collectAllPaths _ _ [] _ = []
collectAllPaths prev l ([]:arrays) board = collectAllPaths (head arrays) l arrays board
collectAllPaths prev l (((Point x y):points):arrays) board
  | checkFirst prev x y l points board = collectAllPaths ((Point (x + 1) y) : prev) l (((Point x y):(Point (x + 1) y):points):arrays) board
  | checkSecond prev x y l points board = collectAllPaths ((Point (x - 1) y):prev) l (((Point x y):(Point (x - 1) y):points):arrays) board
  | checkThird prev x y l points board = collectAllPaths ((Point x (y + 1)):prev) l (((Point x y):(Point x (y + 1)):points):arrays) board
  | checkFourth prev x y l points board = collectAllPaths ((Point x (y - 1)):prev) l (((Point x y):(Point x (y - 1)):points):arrays) board
  | checkFifth prev x y l points board = collectAllPaths ((Point (x + 1) (y + 1)):prev) l (((Point x y):(Point (x + 1) (y + 1)):points):arrays) board
  | checkSixth prev x y l points board = collectAllPaths ((Point (x - 1) (y - 1)):prev) l (((Point x y):(Point (x - 1) (y - 1)):points):arrays) board
  | otherwise = ((Point x y):points):(collectAllPaths prev l (points:arrays) board)

checkFirst :: [Point] -> Int -> Int -> Char -> [Point] -> [String] -> Bool
checkFirst prev x y l points board
  | x < ((length board) - 1) = ((board !! (x + 1)) !! y) == l && not ((Point (x + 1) y) `elem` points) && not ((Point (x + 1) y) `elem` prev)
  | otherwise = False

checkSecond :: [Point] -> Int -> Int -> Char -> [Point] -> [String] -> Bool
checkSecond prev x y l points board
  | y < ((length (board !! x)) - 1) = ((board !! (x - 1)) !! y) == l && not ((Point (x - 1) y) `elem` points) && not ((Point (x - 1) y) `elem` prev)
  | otherwise = False

checkThird :: [Point] -> Int -> Int -> Char -> [Point] -> [String] -> Bool
checkThird prev x y l points board
  | y < ((length (board !! x)) - 1) = ((board !! x) !! (y + 1)) == l && not ((Point x (y + 1)) `elem` points) && not ((Point x (y + 1)) `elem` prev)
  | otherwise = False

checkFourth :: [Point] -> Int -> Int -> Char -> [Point] -> [String] -> Bool
checkFourth prev x y l points board
  | y /= 0 = ((board !! x) !! (y - 1)) == l && not ((Point x (y - 1)) `elem` points) && not ((Point x (y - 1)) `elem` prev)
  | otherwise = False

checkFifth :: [Point] -> Int -> Int -> Char -> [Point] -> [String] -> Bool
checkFifth prev x y l points board
  | x < ((length board) - 1) && y < ((length board) - 1) = ((board !! (x + 1)) !! (y + 1)) == l && not ((Point (x + 1) (y + 1)) `elem` points) && not ((Point (x + 1) (y + 1)) `elem` prev)
  | otherwise = False

checkSixth :: [Point] -> Int -> Int -> Char -> [Point] -> [String] -> Bool
checkSixth prev x y l points board
  | x /= 0 && y /= 0 = ((board !! (x - 1)) !! (y - 1)) == l && not ((Point (x - 1) (y - 1)) `elem` points) && not ((Point (x - 1) (y - 1)) `elem` prev)
  | otherwise = False


