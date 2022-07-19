{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns, DeriveGeneric #-}

module Main where

import Control.Concurrent.STM
import Yesod
import GHC.Generics
import Control.Concurrent
import GameModel
import Data.Text
import Data.List
import Data.Time.Clock
import Data.Map.Internal
import qualified Data.Text as Text
import Data.Map
import Lib3(rightValue, fromJsonLike, toJsonLike, updateValues, FromJsonLike (..), Commands (..), Command (..), Direction (..), Dimensions (..))
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Conduit ((=$), ($$))
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL

data App = App (TVar Int) (TVar (Map Text GameState))

mkYesod "App" [parseRoutes|
/newGame NewGameR POST
/game/#Text GameR POST
|]

instance Yesod App


postNewGameR :: Handler TypedContent
postNewGameR = do
  (gameId, GameState id state dimensions bombExpl) <- createNewGame
  let initInfo = InitialInfo (gameWidth dimensions) gameId (gameHeight dimensions)
  let initInfoJsonLike = GameModel.toJsonLike initInfo & rightValue
  let initInfoJsonString = Lib3.fromJsonLike initInfoJsonLike & rightValue :: String
  return $ TypedContent mimeType $ toContent $ initInfoJsonString

mimeType :: ContentType
mimeType = "application/json"

postGameR :: Text -> Handler TypedContent
postGameR gameId = do
   App gameCount tGamesMap <- getYesod
   existing <- getGameForSession gameId
   commandsStringArray <- rawRequestBody $$ CT.decode CT.utf8 =$ CL.consume
   let commandsString = Data.List.head commandsStringArray
   case existing of
      Just (_, gameState) -> do
         let commandsJsonLike = Lib3.toJsonLike (unpack commandsString) & rightValue
         let commands = Lib3.fromJsonLike commandsJsonLike & rightValue :: Commands

         currTime <- liftIO getCurrentTime
         let newGameState = executeCommands currTime commands gameState
         let GameState id state dimensions bombExpl = newGameState

         liftIO $ setGameStateForGameId tGamesMap id newGameState

         let stateJsonLike = GameModel.toJsonLike state & rightValue
         let stateJsonString = Lib3.fromJsonLike stateJsonLike & rightValue :: String
         
         return $ TypedContent mimeType $ toContent $ stateJsonString    

      Nothing -> error "error on POST game/{gameId}"

-- Helping Functions

-- Gaunam TVar su visais game map'ais, gameId ir nauja game state kuri reikia overwritint
-- Overwritinam ir irasom nauja game
createNewGame :: Handler (Text, GameState)
createNewGame = do
  app@(App tIdCounter tGamesMap) <- getYesod
  newGameId <- liftIO $ getNextIdAsText tIdCounter
  currTime <- liftIO getCurrentTime
  let newGame = GameState { gameId = newGameId, gameState = seed1, dimensions = Dimensions {gameWidth = 15, gameHeight = 15}, bombPlantTime = currTime } -- padaryt random
  liftIO $ setGameStateForGameId tGamesMap newGameId newGame
  return (newGameId, newGame)

getNextId :: TVar Int -> IO Int
getNextId tIdCounter = atomically $ do
  modifyTVar tIdCounter (+1)
  readTVar tIdCounter

getNextIdAsText :: TVar Int -> IO Text
getNextIdAsText tIdCounter = do
  gameId <- getNextId tIdCounter
  return $ pack $ show gameId

--gauna mapsa visu gamestate'u, gauna id, gauna nauja state ir pagal id pakeicia ta state i nauja
setGameStateForGameId :: TVar (Map Text GameState) -> Text -> GameState -> IO (Map Text GameState)
setGameStateForGameId tGamesMap gameId gameState = atomically $ do
   games <- readTVar tGamesMap
   let newMap = Map.insert gameId gameState games
   swapTVar tGamesMap newMap


getGameForSession :: Text -> Handler (Maybe (Text, GameState))
getGameForSession gameId = do
   app@(App tIdCounter tGamesMap) <- getYesod
   existing <- liftIO $ getById tGamesMap gameId
   return $ fmap (\x -> (gameId, x)) existing


executeCommands :: UTCTime -> Commands -> GameState -> GameState
executeCommands currTime (Commands command additional) (GameState id state dim bombPlantTime) =
   case additional of
      Nothing -> case command of
         MoveBomberman direction -> GameState id (moveBomberman direction state) dim bombPlantTime
          
         PlantBomb -> GameState id (plantBomb state) dim currTime
         _ -> GameState id state dim bombPlantTime
      Just addCommands ->
         let
            newGameState = executeCommands currTime (Commands command Nothing) (GameState id state dim bombPlantTime)
         in
            executeCommands currTime addCommands newGameState

doExplosion :: GameState -> GameState
doExplosion (GameState gameId state dimensions bombTime) =
   let
      bombBricks = bomb_bricks (bomb_surrounding state)
      allBricks = bricks $ surrounding state
      newBricks = removeListElements bombBricks allBricks
   in
      GameState gameId (State {bomb = [], bomb_surrounding = BombSurrounding {bomb_bricks = [], bomb_wall = []}, surrounding = Surrounding {bombermans = bombermans $ surrounding state, bricks = newBricks, gates = gates $ surrounding state, ghosts = ghosts $ surrounding state, wall = wall $ surrounding state }}) dimensions bombTime
      
checkForBombs :: TVar (Map Text GameState) -> IO ()
checkForBombs tGamesMap = do
   threadDelay 500000
   now <- getCurrentTime
   traverseMaps now tGamesMap
   checkForBombs tGamesMap


traverseMaps :: UTCTime -> TVar (Map Text GameState) -> IO (Map Text GameState)
traverseMaps currTime tGamesMap = atomically $ do
   gamesMap <- readTVar tGamesMap
   let newMap = Data.Map.Internal.map (checkBombTime currTime) gamesMap
   swapTVar tGamesMap newMap


checkBombTime :: UTCTime -> GameState -> GameState
checkBombTime currTime gameStateMain
  | Data.List.null (bomb (gameState gameStateMain)) = gameStateMain
  | diffUTCTime (currTime) (bombPlantTime gameStateMain) >= 4 = doExplosion gameStateMain
  | otherwise = gameStateMain


removeListElements :: [[Int]] -> [[Int]] -> [[Int]]
removeListElements [] final = final
removeListElements explosionBricks allBricks =
   let
      (head:tail) = explosionBricks
      finalBricks = removeListElements tail (Data.List.delete head allBricks)
   in
      finalBricks

moveBomberman :: Direction -> State -> State
moveBomberman direction (State bomb bomb_surrounding (Surrounding bombermans bricks gates ghosts wall)) = if Prelude.null bombermans
         then
            (State bomb bomb_surrounding (Surrounding bombermans bricks gates ghosts wall))
         else
            let
               bombermanPos = Prelude.head (bombermans)
               bombermanX = Prelude.head bombermanPos
               bombermanY = Prelude.head $ Prelude.tail bombermanPos
               bombermanNewPos = case direction of
                  Lib3.Left -> [bombermanX, bombermanY - 1]
                  Up -> [bombermanX - 1, bombermanY]
                  Lib3.Right -> [bombermanX, bombermanY + 1]
                  Down -> [bombermanX + 1, bombermanY]
               newState = if elem bombermanNewPos bricks || elem bombermanNewPos wall
                  then (State bomb bomb_surrounding (Surrounding bombermans bricks gates ghosts wall))
                  else if elem bombermanNewPos ghosts || elem bombermanNewPos gates
                     then
                        State {bomb = bomb, bomb_surrounding = bomb_surrounding, surrounding = Surrounding { bombermans = [], bricks = bricks, gates = gates, ghosts = ghosts, wall = wall}}
                     else
                        State {bomb = bomb, bomb_surrounding = bomb_surrounding, surrounding = Surrounding { bombermans = [bombermanNewPos], bricks = bricks, gates = gates, ghosts = ghosts, wall = wall}}
            in
               newState


plantBomb :: State -> State
plantBomb state =
   let
      newState = if Prelude.null (bomb state)
         then State {bomb = bombermans (surrounding state), bomb_surrounding = (checkBombRadius (bombermans (surrounding state)) state), surrounding = surrounding state}
         else state
   in
      newState


checkBombRadius :: [[Int]] -> State -> BombSurrounding
checkBombRadius bombPos state =
   let
      bombX = Prelude.head $ Prelude.head bombPos
      bombY = Prelude.head $ Prelude.tail $ Prelude.head bombPos
      bombLeft = [bombX, bombY - 1]
      bombUp = [bombX - 1, bombY]
      bombRight = [bombX, bombY + 1]
      bombDown = [bombX + 1, bombY]
      leftCheckWall = [bombLeft | elem bombLeft (wall (surrounding state))]
      upCheckWall = [bombUp | elem bombUp (wall (surrounding state))]
      rightCheckWall = [bombRight | elem bombRight (wall (surrounding state))]
      downCheckWall = [bombDown | elem bombDown (wall (surrounding state))]

      leftCheckBricks = [bombLeft | elem bombLeft (bricks (surrounding state))]
      upCheckBricks = [bombUp | elem bombUp (bricks (surrounding state))]
      rightCheckBricks = [bombRight | elem bombRight (bricks (surrounding state))]
      downCheckBricks = [bombDown | elem bombDown (bricks (surrounding state))]
   in
      BombSurrounding {bomb_bricks = leftCheckBricks ++ upCheckBricks ++ rightCheckBricks ++ downCheckBricks, bomb_wall = leftCheckWall ++ upCheckWall ++ rightCheckWall ++ downCheckWall}



getById :: TVar (Map Text GameState) -> Text -> IO (Maybe GameState)
getById tGamesMap gameId = do
    games <- readTVarIO tGamesMap
    return $ Map.lookup gameId games
   
moveGhostsThread :: TVar (Map Text GameState) -> IO ()
moveGhostsThread tGamesMap = do
   threadDelay 1000000
   traverseMapsForGhosts tGamesMap
   moveGhostsThread tGamesMap

traverseMapsForGhosts :: TVar (Map Text GameState) -> IO (Map Text GameState)
traverseMapsForGhosts tGamesMap = atomically $ do
   gamesMap <- readTVar tGamesMap
   let newMap = Data.Map.Internal.map (checkForGhosts) gamesMap
   swapTVar tGamesMap newMap

checkForGhosts :: GameState -> GameState
checkForGhosts gameState =
   let
      GameState id state dimensions bombExpl = gameState
      State bomb bomb_surrounding surrounding = state
      newSurrounding = moveGhosts surrounding
   in
      GameState id (State bomb bomb_surrounding newSurrounding) dimensions bombExpl


moveGhosts :: Surrounding -> Surrounding
moveGhosts surrounding = 
   let
      Surrounding bombermans bricks gates ghosts wall = surrounding
      obstacles = updateValues bricks wall
      ghostCoords = Data.List.head ghosts
      newGhostCoords = if Data.List.null ghostCoords
         then []
         else moveGhost ghostCoords obstacles
   in
      Surrounding bombermans bricks gates [newGhostCoords] wall


moveGhost :: [Int] -> [[Int]] -> [Int]
moveGhost (ghostX:ghostY:tail) obstacles = 
   if [ghostX, ghostY - 1] `elem` obstacles
      then if [ghostX - 1, ghostY] `elem` obstacles
         then if [ghostX, ghostY + 1] `elem` obstacles
            then if [ghostX + 1, ghostY] `elem` obstacles
               then [ghostX, ghostY]
               else [ghostX + 1, ghostY]
         else [ghostX, ghostY + 1]
      else [ghostX - 1, ghostY]
   else [ghostX, ghostY - 1]


main :: IO ()
main = do
    counter <- newTVarIO 0
    games <- newTVarIO Map.empty
    forkIO $ checkForBombs games
    forkIO $ moveGhostsThread games
    warp 3000 (App counter games)