{-# LANGUAGE BlockArguments #-}

module Lib2 where
import Data.Char (isDigit)
import Data.List
import qualified Data.Char as C
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict as M (State, StateT, get, put, runState)

type Parser a = ExceptT String (M.State String) a

data InitData = InitData
  { gameWidth :: Int,
    gameHeight :: Int
  }
  deriving (Show)

data JsonLike
  = JsonLikeInteger Integer
  | JsonLikeString String
  | JsonLikeObject [(String, JsonLike)]
  | JsonLikeList [JsonLike]
  | JsonLikeNull
  deriving (Show, Eq)

data State = State {
   bomb :: [[Int]],
   bombermans :: [[Int]],
   bricks :: [[Int]],
   gates :: [[Int]],
   ghosts :: [[Int]],
   wall :: [[Int]],
   dimensions :: InitData
  }
  deriving (Show)

imitateJsonCorrect :: String
imitateJsonCorrect = "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"head\":[0,0],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"

--Tests for incorrect json messages
jsonTest1 :: String
jsonTest1 = "{\"bomb\":null#\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"head\":[0,0],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"

jsonTest2 :: String
jsonTest2 = "{\"bomb\":null,\"surrounding\":{\"bombermans\":&\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"head\":[0,0],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"

jsonTest3 :: String
jsonTest3 = "{\"bomb\":null,\"surrounding\":{\"bombermans\":{head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"head\":[0,0],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"

jsonTest4 :: String
jsonTest4 = "{\"bomb\":nul,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"head\":[0,0],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"



-- Converts a list to a JsonLikeObject 
listToJsonLikeObject :: [(String, JsonLike)] -> JsonLike
listToJsonLikeObject = JsonLikeObject


-- Takes json string message
-- Returns Either a parsed list of JsonLikeObjects
--         Or an error message if the json is invalid
parseMainObjectList :: Parser [(String, JsonLike)] --String -> [(String, JsonLike)]
parseMainObjectList = do
  str <- lift $ M.get
  case str of
    [] -> do
      lift $ M.put str
      return []
    ('{':tail) -> do
      lift $ M.put tail
      res <- parseMainObjectList
      return res
    ('}':tail) -> do
      skipCurlyBrackets
      res <- parseMainObjectList
      return res
    (',':tail) -> do
      lift $ M.put tail
      res <- parseMainObjectList
      return res

    ('"':tail) -> do
      --parsedObject <- parseObject
      (key, value) <- parseObject
      goodResult <- parseMainObjectList
      return $ (key, value) : goodResult
    _ -> throwE "Invalid value passed to parseMainObjectList"


-- parseMainObjectList [] = []
-- parseMainObjectList str =
--   case str of
--     ('{':tail) -> parseMainObjectList tail
--     ('}':tail) -> parseMainObjectList $ skipCurlyBrackets tail
--     (',':tail) -> parseMainObjectList tail

--     ('"':tail) -> do
--       parsedObject <- parseObject str
--       let ((key, value), rest) = parsedObject
--       do
--         goodResult <- parseMainObjectList rest
--         return $ (key, value) : goodResult
--     _ -> error "Invalid value passed to parseMainObjectList"


-- Takes string of json message
-- Returns Either a single object which can have nested objects
--         Or an error message if the json is invalid
parseObject :: Parser (String, JsonLike) --String -> ((String, JsonLike), String)
parseObject = do
  str <- lift $ M.get
  lift $ M.put $ drop 1 str
  key <- parseSingleString
  str1 <- lift $ M.get
  lift $ M.put $ drop 1 str1
  value <- parseValue 
  return (key, value)

-- Takes string of json message
-- Returns Either a parsed value of object which can be another nested object
--         Or an error message if the json is invalid
parseValue :: Parser JsonLike   --String -> (JsonLike, String)
parseValue = do
  str <- lift $ M.get
  case str of
    ('n':'u':'l':'l':tail) -> do
      lift $ M.put tail
      return JsonLikeNull
    ('{':tail) -> do
      lift $ M.put tail
      list <- parseListOfTuples
      return $ JsonLikeObject list
    ('[':tail) -> do
      let value = case tail of
                    ('[':tail2) -> do
                      lift $ M.put tail2
                      res <- parseIntegerLists
                      return res
                    (']':tail2) -> do
                      lift $ M.put tail2
                      return []
                    _ -> do
                      lift $ M.put tail
                      value2 <- parseIntegerList
                      return [value2]
      pureValue <- value
      let value1 = intArrayToJsonLikeList pureValue
      return value1
    ('0':tail) -> do
      res <- parseIntToJsonLike
      return res
    ('1':tail) -> do
      res <- parseIntToJsonLike
      return res
    ('2':tail) -> do
      res <- parseIntToJsonLike
      return res
    ('3':tail) -> do
      res <- parseIntToJsonLike
      return res
    ('4':tail) -> do
      res <- parseIntToJsonLike
      return res
    ('5':tail) -> do
      res <- parseIntToJsonLike
      return res
    ('6':tail) -> do
      res <- parseIntToJsonLike
      return res
    ('7':tail) -> do
      res <- parseIntToJsonLike
      return res
    ('8':tail) -> do
      res <- parseIntToJsonLike
      return res
    ('9':tail) -> do
      res <- parseIntToJsonLike
      return res
    ('"':tail) -> do
      lift $ M.put tail
      strValue <- parseSingleString
      return $ JsonLikeString strValue
    _ -> throwE "invalid value passed to parseValue"
-- parseValue str =
--   case str of
--     ('n':'u':'l':'l':tail) -> (JsonLikeNull, drop 4 str)
--     ('{':tail) -> do
--       (list, rest) <- parseListOfTuples tail
--       return (JsonLikeObject list, rest)
--     ('[':tail) ->
--       let
--         (value, rest) = case tail of
--           ('[':tail2) -> parseIntegerLists tail2
--           (']':tail2) -> ([], tail2)
--           _ ->
--             let
--               (value2, rest2) = parseIntegerList tail
--             in
--               ([value2], rest2)

--         value1 = intArrayToJsonLikeList value
--       in
--         (value1, rest)
--     ('0':tail) -> parseIntToJsonLike str
--     ('1':tail) -> parseIntToJsonLike str
--     ('2':tail) -> parseIntToJsonLike str
--     ('3':tail) -> parseIntToJsonLike str
--     ('4':tail) -> parseIntToJsonLike str
--     ('5':tail) -> parseIntToJsonLike str
--     ('6':tail) -> parseIntToJsonLike str
--     ('7':tail) -> parseIntToJsonLike str
--     ('8':tail) -> parseIntToJsonLike str
--     ('9':tail) -> parseIntToJsonLike str
--     ('"':tail) -> do
--       strValue <- parseSingleString tail
--       let rest = drop(length strValue + 2) str
--       return (JsonLikeString strValue, rest)
--     _ -> error "invalid value passed to parseValue"


-- Takes string of json message
-- Return Either list of JsonLikeObjects and the remaining json string
--        Or an error message if the json is invalid
parseListOfTuples :: Parser [(String, JsonLike)]  -- String -> ([(String, JsonLike)], String)
parseListOfTuples = do
  str <- lift $ M.get
  case str of
    ('{':tail) -> do
      lift $ M.put tail
      res <- parseListOfTuples
      return res
    ('}':tail) -> do
      lift $ M.put tail
      return []
    (',':tail) -> do
      lift $ M.put tail
      res <- parseListOfTuples
      return res
    ('"':tail) -> do
      parsedObject <- parseObject
      let (key, value) = parsedObject
      listOfTuples <- parseListOfTuples
      let list = listOfTuples
      let result = (key, value) : list
      return result
    _ -> throwE "Incorrect Json String passed to parseListOfTuples"

-- parseListOfTuples str =
--   case str of
--     ('{':tail) -> parseListOfTuples tail
--     ('}':tail) -> ([], tail)
--     (',':tail) -> parseListOfTuples tail
--     ('"':tail) -> do
--       parsedObject <- parseObject str
--       let ((key, value), rest) = parsedObject
--       listOfTuples <- parseListOfTuples rest
--       let (list, remaining) = listOfTuples
--       let result = (key, value) : list
--       return (result, remaining)
--     _ -> error "Incorrect Json String passed to parseListOfTuples"


-- Takes a string of numbers
-- Returns a tuple of the number pair and remaining string
parseIntegerList :: Parser [Int]    --String -> ([Int], String)
parseIntegerList = do
  str <- lift $ M.get
  let firstNumber = takeWhile C.isDigit str
  let len = length firstNumber
  let str2 = drop (len+1) str
  let secondNumber = takeWhile C.isDigit str2
  let len2 = length secondNumber
  let rest1 = drop (len2+1) str2
  lift $ M.put rest1
  return [read firstNumber, read secondNumber]

-- parseIntegerList str =
--   let
--     firstNumber = takeWhile C.isDigit str
--     len = length firstNumber
--     str2 = drop (len+1) str
--     secondNumber = takeWhile C.isDigit str2
--     len2 = length secondNumber
--     rest1 = drop (len2+1) str2
--   in
--     ([read firstNumber, read secondNumber], rest1)

parseIntegerLists :: Parser [[Int]]   --String -> ([[Int]], String)
parseIntegerLists = do
  str <- lift $ M.get
  firstPair <- parseIntegerList
  str1 <- lift $ M.get
  let result = case str1 of
                (',':'[':tail) -> do
                  lift $ M.put tail
                  pair <- parseIntegerLists
                  return $ firstPair : pair
                (']':tail) -> do
                  lift $ M.put tail
                  return [firstPair]
                _ -> throwE "error parseIntegerLists"
  pureValue <- result
  return pureValue

-- parseIntegerLists str =
--   let
--     (firstPair, rest) = parseIntegerList str
--     result = case rest of
--       (',':'[':tail) ->
--         let
--           (pair, restList) = parseIntegerLists tail
--         in
--           (firstPair : pair, restList)
--       (']':tail) -> ([firstPair], tail)
--       _ -> error "error parseIntegerLists"
--   in
--     result

-- Takes a single string of the key
-- Returns Either a single word - the key
--         Or an error message if the json is invalid
parseSingleString :: Parser String --String -> String 
parseSingleString = do
  str <- lift $ M.get
  case str of
    ('"':tail) -> do
      lift $ M.put tail
      return []
    (head:tail) -> do
      lift $ M.put tail
      str1 <- parseSingleString
      return (head : str1)

--
parseSingleInt :: String -> Int
parseSingleInt str =
  let
    firstNumber = takeWhile C.isDigit str
  in
    read firstNumber

--
parseIntToJsonLike :: Parser JsonLike     --String -> (JsonLike, String)
parseIntToJsonLike = do
  str <- lift $ M.get
  let number = parseSingleInt str
  let len = length $ show number
  let rest = drop len str
  lift $ M.put rest
  return $ JsonLikeInteger (toInteger number)
-- parseIntToJsonLike str =
--   let
--     number = parseSingleInt str
--     len = length $ show number
--     rest = drop len str
--   in
--     (JsonLikeInteger (toInteger number), rest)


-- Takes json string message
-- Returns json string message with skipped curly brackets
skipCurlyBrackets :: Parser () --String -> String
skipCurlyBrackets = do
  str <- lift $ M.get
  let brackets = takeWhile ('}'==) str
  let len = length brackets
  lift $ M.put $ drop len str
  return ()


-- Takes Int List
-- Returns JsonLikeList with a JsonLikeInteger pair
intArrayToJsonLikeList :: [[Int]] -> JsonLike
intArrayToJsonLikeList [] = JsonLikeList []
intArrayToJsonLikeList array =
  let
    len = length array
    result = if len > 1
      then
        let
          result2 = intToJsonLikeInteger array
        in
          JsonLikeList result2

      else JsonLikeList [ JsonLikeList [JsonLikeInteger (toInteger (head $ head array) ), JsonLikeInteger (toInteger (head $ tail $ head array))] ]

  in
     result


intToJsonLikeInteger :: [[Int]] -> [JsonLike]
intToJsonLikeInteger array =
  let
    (onePair:restOfPairs) = array
    list = case restOfPairs of
      [] -> [ JsonLikeList [JsonLikeInteger (toInteger (head onePair)), JsonLikeInteger (toInteger(head $ tail onePair))] ]
      _ -> JsonLikeList [JsonLikeInteger (toInteger (head onePair)), JsonLikeInteger (toInteger(head $ tail onePair))] : intToJsonLikeInteger restOfPairs
  in
    list

-- Takes json message
-- Returns Either parsed json to a JsonLikeObject
--         Or an error message if the json is invalid
parseJsonMessage :: Parser JsonLike
parseJsonMessage = do
  jsonMessage <- parseMainObjectList
  return $ JsonLikeObject jsonMessage

----------------------
----End Of Parsing----
----------------------


-- Takes JsonLikeObject
-- Returns List of Tuples which are (String, JsonLike)
jsonLikeObjectToList :: JsonLike -> [(String, JsonLike)]
jsonLikeObjectToList (JsonLikeObject list) = list
jsonLikeObjectToList _ = error "Incorrect value passed to JsonLikeObject List extractor"


-- Takes JsonLikeObject
-- Returns extracted tuples from the JsonLikeObject (remove surrounding keyword)
extractTuples :: JsonLike -> JsonLike
extractTuples (JsonLikeObject list) =
  let
    bomb = head list
    surroundingList = tail list
    [(key, value)] = surroundingList
    valueList = jsonLikeObjectToList value
    result = bomb : valueList
  in
    JsonLikeObject result
extractTuples _ = error "Not JsonLikeObject"


-- Takes JsonLikeObject
-- Returns a list of tuples like (String, [[Int]]) (key, coordinates)
jsonToArray :: JsonLike -> [(String, [[Int]])]
jsonToArray (JsonLikeObject []) = []
jsonToArray (JsonLikeObject (head:tail)) =
  let
    result = case head of
      (key, value) ->
        let
          processed = (key, extractIntListUntilNull value)
        in
          processed : jsonToArray (JsonLikeObject tail)

  in
    result
jsonToArray JsonLikeNull = []
jsonToArray err = error ("Incorrect JsonLike passed to jsonToArray" ++ show err)


-- Takes JsonLikeObject which is represented as a single linked list
-- Returns a list of all number pairs till the end of single linked list
extractIntListUntilNull :: JsonLike -> [[Int]]
extractIntListUntilNull JsonLikeNull = [] --buvo [[]] -----------------------------------------------------------------------------------------------
extractIntListUntilNull (JsonLikeList list) = parseIntValue (JsonLikeList list)
extractIntListUntilNull (JsonLikeObject list) =
  let
    (key, value) = head list
    valueException = case value of
       JsonLikeObject list2 ->
         let
           answer = extractIntListUntilNull value
         in
           answer
       JsonLikeList list2 -> parseIntValue value
       JsonLikeNull -> []
       _ -> error "Invalid type of JsonLike in Value"

    rest = tail list
    recursionTemp = if null rest
      then []
      else extractIntListUntilNull (JsonLikeObject rest)

    result = valueException ++ recursionTemp
  in
    result
extractIntListUntilNull _ = error "Not Json Like Integer List"


-- Takes keyword of an object and the list (which is an json object)
-- Returns the corresponding list of number pairs
findInMap :: String -> [(String, [[Int]])] -> [[Int]]
findInMap _ [] = []
findInMap str ((key, value) : tail) =
  if key == str
    then value
    else findInMap str tail


-- Takes JsonLikeList with a pair of JsonLikeIntegers
-- Returns a list with a single number pair
parseIntValue :: JsonLike -> [[Int]]
parseIntValue (JsonLikeList []) = []
parseIntValue (JsonLikeList list) =
  let
    (firstPair:tail) = list
    extractedPair = parseSingleIntValue firstPair
    result = if null tail
      then [extractedPair]
      else extractedPair : parseIntValue (JsonLikeList tail)
  in
    result
parseIntValue _ = error "Not a JsonLikeList"


parseSingleIntValue :: JsonLike -> [Int]
parseSingleIntValue (JsonLikeList (first:second)) = [fromJsonLikeInteger first, fromJsonLikeInteger $ head second]
parseSingleIntValue _ = error "Not a JsonLikeList"

-- Takes JsonLikeInteger, converts it to Integer
-- Returns Int
fromJsonLikeInteger :: JsonLike -> Int
fromJsonLikeInteger (JsonLikeInteger integer) = fromInteger integer
fromJsonLikeInteger _ = error "Incorrect value passed to JsonLikeInteger -> Int converter"


-- | Is called in a very beginning of a game
init :: InitData -> JsonLike -> Lib2.State
init dimensions message =
  let
    extractedMessage = extractTuples message
    array = jsonToArray extractedMessage
    bomb' = findInMap "bomb" array
    bomb = if bomb' == [[]]
      then tail bomb'
      else bomb'
    bombermans = findInMap "bombermans" array
    bricks = findInMap "bricks" array
    gates = findInMap "gates" array
    ghosts = findInMap "ghosts" array
    wall = findInMap "wall" array
  in
    Lib2.State bomb bombermans bricks gates ghosts wall dimensions


-- | Is called after every user interaction (key pressed)
update :: Lib2.State -> JsonLike -> Lib2.State
update currState j =
  let
    newState = Lib2.init (dimensions currState) j         -- if bomb not null then bricks check 4 tiles around bomb coordinate
    bomb' = updateValues (bomb currState) (bomb newState) -- if coordinate exists, remove it, else Nothing
    bricksTemp = updateValues (bricks currState) (bricks newState)
    (bombX, bombY) = if null bomb'
                      then (-1, -1)
                      else (head $ head bomb', head $ tail $ head bomb')
    explodedBricks = if bombX /= (-1) && bombY /= (-1)
                      then
                        let
                          allNewCoords = bricks currState \\ bricks newState
                          bomb1 = [bombX + 1, bombY]
                          bomb2 = [bombX - 1, bombY]
                          bomb3 = [bombX, bombY + 1]
                          bomb4 = [bombX, bombY - 1]
                          result1 = [bomb1 | bomb1 `elem` allNewCoords]
                          result2 = [bomb2 | bomb2 `elem` allNewCoords]
                          result3 = [bomb3 | bomb3 `elem` allNewCoords]
                          result4 = [bomb4 | bomb4 `elem` allNewCoords]

                        in
                          result1 ++ result2 ++ result3 ++ result4
                      else []
    gates' = updateValues (gates currState) (gates newState)
    ghosts' = updateValues (ghosts currState) (ghosts newState)
    wall' = updateValues (wall currState) (wall newState)
    result = Lib2.State bomb' (bombermans newState) bricksTemp gates' ghosts' wall' (dimensions newState)
    answer =  if null explodedBricks
              then result
              else  Lib2.State [] (bombermans newState) (removeElementsFromList bricksTemp explodedBricks) gates' ghosts' wall' (dimensions newState)
  in
    answer


-- Takes lists A and B
-- Returns A \ B
removeElementsFromList :: [[Int]] -> [[Int]] -> [[Int]]
removeElementsFromList bricks [] = bricks
removeElementsFromList bricks explodedBricks =
  let
    (coords:tail) = explodedBricks
    newBricks = delete coords bricks
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
  |(key == (take 1 array)) = True
  |otherwise = checkIfEx key (drop 1 array)

-- | Renders the current state
render :: Lib2.State -> String
render currState =
  let
    matrix = createMatrix currState
    result0 = paint currState matrix (bomb currState) 'b'
    result1 = paint currState result0 (bombermans currState) 'p'
    result2 = paint currState result1 (bricks currState) '░'
    result3 = paint currState result2 (gates currState) '$'
    result4 = paint currState result3 (ghosts currState) '@'
    result5 = paint currState result4 (wall currState) '█'
  in
    result5


-- Creates a 2D empty matrix
createMatrix :: Lib2.State -> String
createMatrix currState =
  let
   matrix = take (gameWidth (dimensions currState) * gameHeight (dimensions currState)) [' ',' '..]
   result = putSpacingMatrix currState matrix
   in
     result


--Puts new lines in matrix
putSpacingMatrix :: Lib2.State -> String -> String
putSpacingMatrix _ [] = []
putSpacingMatrix currState matrixOld =
  let
    gameWidth2 = gameWidth(dimensions currState)
    gameHeight2 = gameHeight(dimensions currState)
    matrixNew = take gameWidth2 matrixOld ++ ['\n']
    remainder = drop gameWidth2 matrixOld
  in
    matrixNew ++ putSpacingMatrix currState remainder


-- Takes Lib2.State, matrix, list of coordinates and symbol and changes the matrix
paint :: Lib2.State -> String -> [[Int]] -> Char -> String
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