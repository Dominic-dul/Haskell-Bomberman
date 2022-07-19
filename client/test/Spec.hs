{-# LANGUAGE BlockArguments #-}
import Test.HUnit
import System.Exit
import Data.Either as E

import Lib2
import Lib3

test1 = TestCase (assertEqual "for parseJsonMessage()" (E.Right (JsonLikeObject [("name", JsonLikeString "value")])) (Lib2.parseJsonMessage "{\"name\":\"value\"}") )
test2 = TestCase (assertEqual "for parseJsonMessage()" (E.Left "invalid value passed to parseValue") (Lib2.parseJsonMessage "{\"name\":{\"value\"}") )
test3 = TestCase (assertEqual "for convertJsonToString()" (E.Right "{\"command\":{\"name\":\"MoveBomberman\",\"direction\":\"Right\"}}") (Lib3.convertJsonToString (JsonLikeObject [("command", JsonLikeObject [("name", JsonLikeString "MoveBomberman"), ("direction", JsonLikeString ("Right"))]) ])) )
test4 = TestCase (assertEqual "for convertJsonToString()" (E.Left "other JsonLike value passed") (Lib3.convertJsonToString JsonLikeNull))
test5 = TestCase (assertEqual "for commandsToJsonLike()" (JsonLikeObject [("command",JsonLikeObject [("name",JsonLikeString "FetchSurrounding")])]) (Lib3.commandsToJsonLike (Commands FetchSurrounding Nothing)) )

main :: IO ()
main = do
    counts <- runTestTT ( test [
        test1,
        test2,
        test3,
        test4,
        test5
        ])
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure