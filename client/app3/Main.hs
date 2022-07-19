module Main where

import Control.Exception (bracket)
import Control.Lens ((^.), (.~))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Either as E (either)
import Data.Function ((&))
import Data.List as L (concat, (++))
import Data.String.Conversions (cs)
import Lib3
import Network.Wreq (post, responseBody, header, defaults)
import Network.Wreq.Lens (Response (..))
import Network.HTTP.Types.Header (hContentType)
import qualified Network.Wreq.Session as Sess
import System.Console.ANSI as ANSI
  ( clearScreen,
    hideCursor,
    setCursorPosition,
    showCursor,
  )
import System.IO (BufferMode (..), hSetBuffering, hSetEcho, stderr, stdin, stdout)
import Prelude hiding (Left, Right)
import Control.Concurrent

-- MANDATORY CODE
host :: String
host = "http://localhost:3000"


createGame :: (FromJsonLike a) => Sess.Session -> IO a
createGame sess = do
  r <- Sess.post sess (host ++ "/newGame") B.empty
  let resp = cs $ r ^. responseBody :: String
  return $ toJsonLike resp & e & fromJsonLike & e

postCommands :: (FromJsonLike a, ToJsonLike a, FromJsonLike b, ToJsonLike b) => GameId -> Sess.Session -> a -> IO b
postCommands uuid sess commands = do
  let opts = defaults & header hContentType  .~ [cs "application/json"]
  let str = toJsonLike commands & e & fromJsonLike & e :: String
  let req = cs str :: B.ByteString
  r <- Sess.postWith opts sess (L.concat [host, "/game/", uuid]) req
  let respStr = cs $ r ^. responseBody :: String
  return $ toJsonLike respStr & e & fromJsonLike & e

e :: Either String a -> a
e = E.either error id

-- MANDATORY CODE END

getAllInfo :: Commands
getAllInfo = Commands FetchSurrounding (Just (Commands FetchBombStatus (Just (Commands FetchBombSurrounding Nothing))))

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stderr NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  bracket
    (ANSI.hideCursor >> Sess.newAPISession)
    (const showCursor)
    ( \sess -> do
        game <- createGame sess :: IO NewGame
        let dimensions = Dimensions {gameWidth = width game, gameHeight = height game} :: Dimensions
        initialStr <- postCommands (gameId game) sess (Commands FetchSurrounding Nothing)  :: IO CommandsResponse
        let initialState = Lib3.State initialStr dimensions
        
        bombCheckThread <- newChan
        drawThread <- newChan
        forkIO $ checkBombExplosion bombCheckThread drawThread (gid game) sess
        forkIO $ draw drawThread
        writeChan bombCheckThread initialState
        writeChan drawThread initialState
        
        loop (gid game) sess bombCheckThread
    )

loop :: String -> Sess.Session -> Chan Lib3.State -> IO ()
loop uuid sess bombCheckThread = do
  c <- getChar
  let commands = case c of
        'a' -> Commands (MoveBomberman Left) (Just getAllInfo)
        's' -> Commands (MoveBomberman Down) (Just getAllInfo)
        'd' -> Commands (MoveBomberman Right) (Just getAllInfo)
        'w' -> Commands (MoveBomberman Up) (Just getAllInfo)
        'b' -> Commands PlantBomb (Just getAllInfo)
        _ -> Commands FetchSurrounding Nothing
  updatedCommandResponse <- postCommands uuid sess commands -- turim IO CommandsResponse
  oldState <- readChan bombCheckThread
  let updatedState = Lib3.State updatedCommandResponse (initialData oldState)
  let newState = Lib3.update oldState updatedState
  writeChan bombCheckThread newState
  loop uuid sess bombCheckThread

draw :: Chan Lib3.State -> IO ()
draw drawThread = do
  _ <- ANSI.clearScreen
  _ <- ANSI.setCursorPosition 0 0
  state <- readChan drawThread
  putStrLn $ Lib3.render state
  threadDelay 500000
  draw drawThread

checkBombExplosion :: Chan Lib3.State -> Chan Lib3.State -> String -> Sess.Session -> IO ()
checkBombExplosion oldState drawThread uuid sess = do
  currCommandResponse <- postCommands uuid sess getAllInfo
  strippedState <- readChan oldState
  let currState = Lib3.State currCommandResponse (initialData strippedState)
  let newState = Lib3.update strippedState currState
  writeChan oldState newState
  writeChan drawThread newState
  threadDelay 500000
  checkBombExplosion oldState drawThread uuid sess
