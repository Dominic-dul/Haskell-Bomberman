{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}

module Lib3 where

import Data.Either as E (Either (..), either)
import Data.Function ((&))
import Data.List as L (lookup, delete)
import Lib2 (JsonLike (..), parseJsonMessage, jsonLikeObjectToList, extractIntListUntilNull, jsonToArray, findInMap)
import Control.Monad.Trans.State.Strict as M (runState)
import Control.Monad.Trans.Except (runExceptT)


-- Keep this type as is
type GameId = String

-- Keep these classes as is:
-- You will want to implement instances for them
class ToJsonLike a where
  toJsonLike :: a -> Either String JsonLike

class FromJsonLike a where
  fromJsonLike :: JsonLike -> Either String a

class ContainsGameId a where
  gameId :: a -> GameId

-- Further it is your code: you can change whatever you wish

instance FromJsonLike String where 
  fromJsonLike obj = do
    convertJsonToString obj


convertJsonToString :: JsonLike -> Either String String
convertJsonToString (JsonLikeString str) = E.Right (show str)
convertJsonToString (JsonLikeNull) = E.Right "null"
convertJsonToString (JsonLikeInteger num) = E.Right $ show (fromInteger num) 
convertJsonToString (JsonLikeList []) = E.Right $ "[]"
convertJsonToString (JsonLikeList list) = 
  let
    (head:tail) = list
    jsonString = if null tail
      then "[" ++ (convertJsonToString head & rightValue) ++ "]"
      else "[" ++ (convertJsonToString head & rightValue) ++ "," ++ (convertListOfJsonToString tail) ++ "]"
  in
    E.Right jsonString
convertJsonToString (JsonLikeObject obj) =
    let
      (head:tail) = obj
      (key, value) = head
      jsonString = if null tail
          then "{" ++ show key ++ ":" ++ (convertJsonToString value & rightValue) ++ "}"
          else "{" ++ show key ++ ":" ++ (convertJsonToString value & rightValue) ++ "," ++ (convertJsonLikeObjToString tail & rightValue) ++ "}"
    in
      E.Right jsonString

convertJsonLikeObjToString :: [(String, JsonLike)] -> Either String String
convertJsonLikeObjToString list = 
  let
    (head:tail) = list
    (key, value) = head
    jsonString = if null tail
        then show key ++ ":" ++ (convertJsonToString value & rightValue)
        else show key ++ ":" ++ (convertJsonToString value & rightValue) ++ "," ++ (convertJsonLikeObjToString tail & rightValue)
  in 
    E.Right jsonString

convertListOfJsonToString :: [JsonLike] -> String
convertListOfJsonToString [] = []
convertListOfJsonToString list =
  let
    (head:tail) = list
    result = if null tail
      then (convertJsonToString head & rightValue)
      else (convertJsonToString head & rightValue) ++ "," ++ convertListOfJsonToString tail
  in
    result

-- Acts as a parser from a String
instance ToJsonLike String where
  toJsonLike str = do 
    let (result, rest) = M.runState (runExceptT Lib2.parseJsonMessage) str
    pureValue <- result
    return pureValue

data NewGame = NewGame {
  gid :: GameId,
  width :: Int,
  height :: Int
}
  deriving (Show)

instance ContainsGameId NewGame where
  gameId (NewGame gid height width) = gid

instance FromJsonLike NewGame where
  fromJsonLike o@(JsonLikeObject m) = do
    let gid = do
          L.lookup "uuid" m
    let height = do
          L.lookup "height" m
    let width = do
          L.lookup "width" m
    return $ NewGame (gid & justValue & rightValue) (read (width & justValue & rightValue)) (read (height & justValue & rightValue))
  fromJsonLike v = E.Left $ "Unexpected value: " ++ show v

justValue :: Maybe JsonLike -> Either String String
justValue (Just (JsonLikeString str)) = E.Right str
justValue (Just (JsonLikeInteger num)) = E.Right (show (fromInteger num))
justValue _ = E.Left "Not a Just Json String/Integer passed"

rightValue :: Either String a -> a
rightValue = E.either error id

data Direction = Right | Left | Up | Down
  deriving (Show)

data Command
  = MoveBomberman Direction
  | FetchSurrounding
  | PlantBomb
  | FetchBombStatus
  | FetchBombSurrounding
  deriving (Show)

data Commands = Commands
  { command :: Command,
    additional :: Maybe Commands
  }
  deriving (Show)

instance ToJsonLike Commands where 
 toJsonLike commands = E.Right (commandsToJsonLike commands)


commandsToJsonLike :: Commands -> JsonLike
commandsToJsonLike (Commands command additional) =
    case additional of
      Nothing -> case command of
        MoveBomberman direction ->  JsonLikeObject [("command", JsonLikeObject [("name", JsonLikeString "MoveBomberman"), ("direction", JsonLikeString (show direction))]) ]
        cmd -> JsonLikeObject [("command", JsonLikeObject [("name", JsonLikeString (show cmd))]) ]
      Just addCommands ->
        let
          jsonLikeCmdObject = commandsToJsonLike addCommands
          cmdJsonLikeObj = case command of
            MoveBomberman direction ->  JsonLikeObject [("command", JsonLikeObject [("name", JsonLikeString "MoveBomberman"), ("direction", JsonLikeString (show direction))]), ("additional", jsonLikeCmdObject) ]
            cmd -> JsonLikeObject [("command", JsonLikeObject [("name", JsonLikeString (show cmd))]), ("additional", jsonLikeCmdObject) ]
        in
          cmdJsonLikeObj       

instance FromJsonLike Commands where
  fromJsonLike obj = do convertJsonLikeToCommands obj

instance FromJsonLike Command where
  fromJsonLike obj = do convertJsonLikeToCommand obj

instance FromJsonLike Direction where
  fromJsonLike dir = do convertJsonLikeToDirection dir

convertJsonLikeToDirection :: JsonLike -> Either String Direction
convertJsonLikeToDirection (JsonLikeString str) =
  case str of
    "Up" -> E.Right Up
    "Down" -> E.Right Down
    "Right" -> E.Right Lib3.Right
    "Left" -> E.Right Lib3.Left
    _ -> E.Left "Wrong Direction passed in JSON"

convertJsonLikeToCommand :: JsonLike -> Either String Command
convertJsonLikeToCommand (JsonLikeString str) =
  case str of
    "FetchSurrounding" -> E.Right FetchSurrounding
    "FetchBombSurrounding" -> E.Right FetchBombSurrounding
    "FetchBombStatus" -> E.Right FetchBombStatus
    "PlantBomb" -> E.Right PlantBomb
    _ -> E.Left "Wrong Command passed in JSON"
convertJsonLikeToCommand (JsonLikeObject obj) =
  let
    (head:tail) = obj
    (key, value) = head
    finalResult = if Prelude.null tail
                    then convertJsonLikeToCommand value & rightValue
                    else 
                      let
                        (head2:tail2) = tail
                        (key2, value2) = head2
                        dir = fromJsonLike value2 & rightValue
                      in
                        MoveBomberman dir
  in
    E.Right finalResult

convertJsonLikeToCommands :: JsonLike -> Either String Commands
convertJsonLikeToCommands (JsonLikeObject obj) = 
  let
    (head:tail) = obj
    (key, value) = head
    result = if null tail
              then Commands {command = fromJsonLike value & rightValue, additional = Nothing}
              else 
                let
                  (head2:tail2) = tail
                  (key2, value2) = head2
                  add = fromJsonLike value2 & rightValue
                in
                  Commands {command = fromJsonLike value & rightValue, additional = Just (add)}
  in 
    E.Right result

data Dimensions = Dimensions {
  gameWidth :: Int,
  gameHeight :: Int
  }
  deriving (Show)

data Surrounding = Surrounding {
  bombermans :: [[Int]],
  bricks :: [[Int]],
  gates :: [[Int]],
  ghosts :: [[Int]],
  wall :: [[Int]]
  }
  deriving (Show)

data BombSurrounding = BombSurrounding {
  bomb_bricks:: [[Int]],
  bomb_wall :: [[Int]]
  }
  deriving (Show)

data CommandsResponse = CommandsResponse {
  bomb :: [[Int]],
  bomb_surrounding :: BombSurrounding,
  surrounding :: Surrounding
  }
  deriving (Show)

data State = State{
  initialResponse :: CommandsResponse,
  initialData :: Dimensions
  }
  deriving (Show)

instance FromJsonLike CommandsResponse where 
  fromJsonLike jsonLikeObj = do jsonObjectToResponse jsonLikeObj

instance ToJsonLike CommandsResponse where 
  toJsonLike _ = E.Right JsonLikeNull

jsonObjectToResponse :: JsonLike -> Either String CommandsResponse
jsonObjectToResponse (JsonLikeObject list) =
  let
    (bomb:bomb_surrounding:surrounding) = list
    (bombK, bombV) = bomb
    (bomb_surroundingK, bomb_surroundingV) = bomb_surrounding
    (surroundingK, surroundingV) = head surrounding
    bombArray = Lib2.extractIntListUntilNull bombV
    bomb_surroundingArray = Lib2.jsonToArray bomb_surroundingV
    surroundingArray = Lib2.jsonToArray surroundingV
    result = (bombK, bombArray) : bomb_surroundingArray ++ surroundingArray

    bomb_surroundingState = BombSurrounding (Lib2.findInMap "bricks" bomb_surroundingArray) (Lib2.findInMap "wall" bomb_surroundingArray)
    surroundingState = Surrounding (Lib2.findInMap "bombermans" surroundingArray) (Lib2.findInMap "bricks" surroundingArray) (Lib2.findInMap "gates" surroundingArray) (Lib2.findInMap "ghosts" surroundingArray) (Lib2.findInMap "wall" surroundingArray)
  in
    E.Right (CommandsResponse bombArray bomb_surroundingState surroundingState)
jsonObjectToResponse _ = E.Left "passed not a JsonLikeObject to jsonObjectToResponse"    


update :: State -> State -> State
update currState newState = newState


-- Takes lists A and B
-- Returns A \ B
removeElementsFromList :: [[Int]] -> [[Int]] -> [[Int]]
removeElementsFromList bricks [] = bricks
removeElementsFromList bricks explodedBricks =
  let
    (coords:tail) = explodedBricks
    newBricks = L.delete coords bricks
    answer = removeElementsFromList newBricks tail
  in
    answer


-- Merges two lists and eliminates duplicates 
-- [[1,1],[2,2]] -> [[1,1],[3,3]] -> [[1,1],[2,2],[3,3]]
updateValues :: [[Int]] -> [[Int]] -> [[Int]]
updateValues [] arr = arr
updateValues old new =
  let
    first = take 1 old
  in
    if checkIfEx first new
      then updateValues (drop 1 old) new
      else first ++ updateValues (drop 1 old) new


-- checks if given element is in the list
-- 
checkIfEx :: [[Int]] -> [[Int]] -> Bool
checkIfEx _ [] = False
checkIfEx key array
  |key == take 1 array = True
  |otherwise = checkIfEx key (drop 1 array)


-- | Renders the current state
render :: State -> String
render currState =
  let
    matrix = createMatrix currState
    result0 = paint currState matrix (bomb (initialResponse currState)) 'b'
    result1 = paint currState result0 (bombermans (surrounding (initialResponse currState))) 'p'
    result2 = paint currState result1 (bricks (surrounding (initialResponse currState))) '░'
    result3 = paint currState result2 (gates (surrounding (initialResponse currState))) '$'
    result4 = paint currState result3 (ghosts (surrounding (initialResponse currState))) '@'
    result5 = paint currState result4 (wall (surrounding (initialResponse currState))) '█'
  in
    result5


-- Creates a 2D empty matrix
createMatrix :: State -> String
createMatrix currState =
  let
   matrix = take (gameWidth (initialData currState) * gameHeight (initialData currState)) [' ',' '..]
   result = putSpacingMatrix currState matrix
   in
     result


--Puts new lines in matrix
putSpacingMatrix :: State -> String -> String
putSpacingMatrix _ [] = []
putSpacingMatrix currState matrixOld =
  let
    gameWidth2 = gameWidth(initialData currState)
    gameHeight2 = gameHeight(initialData currState)
    matrixNew = take gameWidth2 matrixOld ++ ['\n']
    remainder = drop gameWidth2 matrixOld
  in
    matrixNew ++ putSpacingMatrix currState remainder


-- Takes state, matrix, list of coordinates and symbol and changes the matrix
paint :: State -> String -> [[Int]] -> Char -> String
paint _ str [] _ = str
paint currState matrix listOfCoords symbol =
  let
    (first:second:rest) = head listOfCoords
    listOfCoordsNew = drop 1 listOfCoords
    result = changeMatrix matrix first second (initialData currState) symbol
    in
      paint currState result listOfCoordsNew symbol


-- Changes single element of matrix with the given symbol
changeMatrix :: String -> Int -> Int -> Dimensions -> Char -> String
changeMatrix matrix i j dimensions symbol =
  let
    move = (i * (gameWidth dimensions + 1)) + j
  in
    take move matrix ++ [symbol] ++ drop (move + 1) matrix