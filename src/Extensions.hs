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
changeGames gameId ((Game id (ResponseForWhileTrue isStarted playersTurn playersCount changes) field):xs)
  | gameId == id = (Game id (ResponseForWhileTrue isStarted playersTurn (playersCount + 1) changes) field) : xs
  | otherwise = (Game id (ResponseForWhileTrue isStarted playersTurn playersCount changes) field) : changeGames gameId xs

startOneGame :: Int -> [Game] -> [Game]
startOneGame _ [] = []
startOneGame gameId ((Game id (ResponseForWhileTrue isStarted playersTurn playersCount changes) field):xs)
  | gameId == id = (Game id (ResponseForWhileTrue True playersTurn playersCount changes) field) : xs
  | otherwise = (Game id (ResponseForWhileTrue isStarted playersTurn playersCount changes) field) : changeGames gameId xs

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
changeBoardState makeTurnChanges ((Game id (ResponseForWhileTrue isStarted playersTurn playersCount changes) field):gs)
  | (gameNumber (turnInfo makeTurnChanges)) == id = (Game id (ResponseForWhileTrue isStarted playersTurn playersCount changes) (setChanges (turnChanges makeTurnChanges) field)) : gs
  | otherwise = (Game id (ResponseForWhileTrue isStarted playersTurn playersCount changes) field) : changeBoardState makeTurnChanges gs

setChanges :: Changes -> Field -> Field
setChanges changes field = do
  let (Field size board) = field
  Field size $ changeBoard changes board

changeBoard :: Changes -> [String] -> [String]
changeBoard _ [] = []
changeBoard (Changes x y l) (row:rows)
  | x == 0 = (changeRow y l row) : rows
  | otherwise = row : changeBoard (Changes (x - 1) y l) rows


changeRow :: Int -> Char -> String -> String
changeRow y l (cell:cells)
  | y == 0 = l : cells
  | otherwise = cell : changeRow (y - 1) l cells