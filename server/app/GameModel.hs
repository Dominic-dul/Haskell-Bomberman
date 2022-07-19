{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module GameModel where

import Data.List
import GHC.Generics
import Data.Either as E (Either (..), either)
import Data.Aeson
import Data.Text (Text, pack, unpack, empty)
import Data.Time.Clock
import Lib2(JsonLike (..), intArrayToJsonLikeList)
import Lib3(rightValue, FromJsonLike (..), Commands (..), Command (..), Direction (..), Dimensions (..))
import Data.Function ((&))
import Control.Monad
import Data.HashMap.Strict as H (lookup)
import Control.Monad.IO.Class


data Surrounding = Surrounding {
  bombermans :: [[Int]],
  bricks :: [[Int]],
  gates :: [[Int]],
  ghosts :: [[Int]],
  wall :: [[Int]]
  }
  deriving (Show)

data BombSurrounding = BombSurrounding {
  bomb_bricks :: [[Int]],
  bomb_wall :: [[Int]]
  }
  deriving (Show)

data State = State {
  bomb :: [[Int]],
  bomb_surrounding :: BombSurrounding,
  surrounding :: Surrounding
  }
  deriving (Show)

data GameState = GameState {
   gameId :: Text,
   gameState :: State,
   dimensions :: Dimensions,
   bombPlantTime :: UTCTime
} deriving (Show)

instance ToJsonLike State where
  toJsonLike (State bomb (BombSurrounding bomb_bricks bomb_wall) (Surrounding bombermans bricks gates ghosts wall)) = Prelude.Right $ JsonLikeObject[("bomb", (intArrayToJsonLikeList bomb)), ("bomb_surrounding", JsonLikeObject [("bomb_bricks", (intArrayToJsonLikeList bomb_bricks) ), ("bomb_wall", (intArrayToJsonLikeList bomb_wall) )]), ("surrounding", JsonLikeObject [("bombermans", (intArrayToJsonLikeList bombermans) ), ("bricks", (intArrayToJsonLikeList bricks) ), ("gates", (intArrayToJsonLikeList gates) ), ("ghosts", (intArrayToJsonLikeList ghosts) ), ("wall", (intArrayToJsonLikeList wall) )])]


data InitialInfo = InitialInfo {
  width :: Int,
  uuid :: Text,
  height :: Int
} deriving (Show)

instance ToJSON InitialInfo where
  toJSON (InitialInfo width uuid height) = object [pack "width" .= width, pack "uuid" .= unpack uuid, pack "height" .= height]


class ToJsonLike a where
  toJsonLike :: a -> Either String JsonLike


instance ToJsonLike InitialInfo where
  toJsonLike (InitialInfo width uuid height) = Prelude.Right $ JsonLikeObject [("width", JsonLikeInteger (toInteger width)), ("uuid", JsonLikeString (unpack uuid)), ("height", JsonLikeInteger (toInteger height))]


seed1 :: State
seed1 = State {bomb = [], bomb_surrounding = BombSurrounding {bomb_bricks = [], bomb_wall = []}, surrounding = Surrounding {bombermans = [[1,1]], bricks = [[12,13],[8,13],[6,13],[4,13],[1,13],[12,11],[12,9],[12,7],[12,5],[12,3],[11,6],[10,11],[10,9],[10,7],[10,3],[10,1],[9,6],[8,11],[8,9],[8,7],[8,3],[8,1],[7,12],[7,10],[6,11],[6,7],[6,5],[5,8],[5,4],[4,11],[3,10],[3,6],[3,4],[2,3],[2,1],[1,12],[1,11],[1,10],[1,9],[1,8],[1,7],[1,6]], gates = [[13,13]], ghosts = [[9,7]], wall = [[14,14],[13,14],[12,14],[11,14],[10,14],[9,14],[8,14],[7,14],[6,14],[5,14],[4,14],[3,14],[2,14],[1,14],[0,14],[14,13],[0,13],[14,12],[14,11],[14,10],[14,9],[14,8],[14,7],[14,6],[14,5],[14,4],[14,3],[14,2],[14,1],[14,0],[13,0],[12,12],[12,10],[12,8],[12,6],[12,4],[12,2],[12,0],[11,0],[10,12],[10,10],[10,8],[10,6],[10,4],[10,2],[10,0],[9,0],[8,12],[8,10],[8,8],[8,6],[8,4],[8,2],[8,0],[7,0],[6,12],[6,10],[6,8],[6,6],[6,4],[6,2],[6,0],[5,0],[4,12],[4,10],[4,8],[4,6],[4,4],[4,2],[4,0],[3,0],[2,12],[2,10],[2,8],[2,6],[2,4],[2,2],[2,0],[1,0],[0,12],[0,11],[0,10],[0,9],[0,8],[0,7],[0,6],[0,5],[0,4],[0,3],[0,2],[0,1],[0,0]]}}
