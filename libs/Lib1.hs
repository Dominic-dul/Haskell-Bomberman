{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib1 where

import GHC.StableName (StableName)

import Data.Char as C
import GHC.Base (String)

data InitData = InitData
  {
    gameWidth :: Int,
    gameHeight :: Int
  }
  deriving (Show)

data State = State {
   bombermans :: [[Int]],
   bricks :: [[Int]],
   gates :: [[Int]],
   ghosts :: [[Int]],
   wall :: [[Int]],
   dimensions :: InitData
  }
  deriving (Show)

-- Counts how many chars of list to eliminate
countCharsInList :: [[Int]] -> Int
countCharsInList [] = 1
countCharsInList array =
  let
    arrayString = show array
    stringParsed = parseString arrayString
    result = length stringParsed + 1
  in
    result

--Takes json string and extracts only letters, numbers and commas
-- "{bombermans:[[1,1]]}" -> "bombermans1,1}"
parseString :: String -> String
parseString [] = []
parseString (head:tail) =
  if isLetter head || isDigit head || head == ',' || head == '}'
  then head : parseString tail
  else parseString tail

-- Eliminates given word from a string and returns given coordinates in pairs
separateBombermans :: String -> [[Int]]
separateBombermans str =
  case str of
    ('b':'o':'m':'b':'e':'r':'m':'a':'n':'s':',':tail) -> []
    ('b':'o':'m':'b':'e':'r':'m':'a':'n':'s':tail) -> parseInts tail
    _ -> error "Wrong word passed"

separateBricks :: String -> [[Int]]
separateBricks str =
  case str of
    ('b':'r':'i':'c':'k':'s':',':tail) -> []
    ('b':'r':'i':'c':'k':'s':tail) -> parseInts tail
    _ -> error "Wrong word passed"

separateGates :: String -> [[Int]]
separateGates str =
  case str of
  ('g':'a':'t':'e':'s':',':tail) -> []
  ('g':'a':'t':'e':'s':tail) -> parseInts tail
  _ -> error "Wrong word passed"

separateGhosts :: String -> [[Int]]
separateGhosts str =
  case str of
  ('g':'h':'o':'s':'t':'s':',':tail) -> []
  ('g':'h':'o':'s':'t':'s':tail) -> parseInts tail
  _ -> error "Wrong word passed"

separateWall :: String -> [[Int]]
separateWall str =
  case str of
  ('w':'a':'l':'l':',':tail) -> []
  ('w':'a':'l':'l':tail) -> parseInts tail
  _ -> error "Wrong word passed"

-- Extracts pair of numbers from string
-- "1,2,3,4,bricks" -> ([1,2], ",3,4,bricks")
parseIntPair :: String -> ([Int], String)
parseIntPair str = 
  let
    firstNumber = takeWhile C.isDigit str
    len = length firstNumber
    str2 = drop (len+1) str
    secondNumber = takeWhile C.isDigit str2
    len2 = length secondNumber
  in
    ([read firstNumber, read secondNumber], drop len2 str2)

-- Extracts all pairs from string
-- "1,2,3,4,bricks" -> [[1,2],[3,4]]
parseInts :: String -> [[Int]]
parseInts str =
  let
    (value, rest) = parseIntPair str
  in
    case rest of
      ('}':tail) -> [value]
      (head:tail) -> if isLetter head then [] else value : parseInts tail

-- takes json and transfers it to our State
init :: InitData -> String -> State
init dimensions str =
  let
    naujas = parseString str 
    naujas' = drop 11 naujas 

    bombermans = separateBombermans naujas'
    str1 = drop (countCharsInList bombermans + 10) naujas'

    bricks = separateBricks str1
    str2 = drop (countCharsInList bricks + 6) str1

    gates = separateGates str2
    str3 = drop (countCharsInList gates + 5) str2

    ghosts = separateGhosts str3
    str4 = drop (countCharsInList ghosts + 6) str3

    wall = separateWall str4  
  in
    State bombermans bricks gates ghosts wall dimensions

-- Updates our state and saves already visited areas
update :: State -> String -> State
update currState j = 
  let
    newState = Lib1.init (dimensions currState) j
    bricks' = updateValues (bricks currState) (bricks newState)
    gates' = updateValues (gates currState) (gates newState)
    ghosts' = updateValues (ghosts currState) (ghosts newState)
    wall' = updateValues (wall currState) (wall newState)
  in
    State (bombermans newState) bricks' gates' ghosts' wall' (dimensions newState)

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
  |(key == (take 1 array)) = True
  |otherwise = checkIfEx key (drop 1 array)

-- | Renders the current state
render :: State -> String
render currState = 
  let
    matrix = createMatrix currState
    result1 = paint currState matrix (bombermans currState) 'p'
    result2 = paint currState result1 (bricks currState) '░'
    result3 = paint currState result2 (gates currState) '$'
    result4 = paint currState result3 (ghosts currState) '@'
    result5 = paint currState result4 (wall currState) '█'
  in
    result5

-- Creates a 2D empty matrix
createMatrix :: State -> String 
createMatrix currState =
  let
   matrix = take (gameWidth (dimensions currState) * gameHeight (dimensions currState)) [' ',' '..]
   result = putSpacingMatrix currState matrix
   in
     result 
  
--Puts new lines in matrix
putSpacingMatrix :: State -> String -> String 
putSpacingMatrix _ [] = []
putSpacingMatrix currState matrixOld = 
  let
    gameWidth2 = gameWidth(dimensions currState)
    gameHeight2 = gameHeight(dimensions currState)
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
    result = changeMatrix matrix first second (dimensions currState) symbol
    in
      paint currState result listOfCoordsNew symbol

-- Changes single element of matrix with the given symbol
changeMatrix :: String -> Int -> Int -> InitData -> Char -> String
changeMatrix matrix i j dimensions symbol =
  let
    move = (i * (gameWidth dimensions + 1)) + j
  in
    take move matrix ++ [symbol] ++ drop (move + 1) matrix