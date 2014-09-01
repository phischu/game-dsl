{-# LANGUAGE StandaloneDeriving,DeriveFunctor #-}
module Main where

import GameDSL hiding (Rule,Action)
import qualified GameDSL as GameDSL (Rule,Action)

import Graphics.Gloss
import Control.Monad (guard)

data Tag = Field | Turn | X | O

deriving instance Eq Tag
deriving instance Ord Tag
deriving instance Show Tag

data Attribute = XPosition | YPosition

deriving instance Eq Attribute
deriving instance Ord Attribute
deriving instance Show Attribute

type Rule = GameDSL.Rule Tag Attribute
type Action = GameDSL.Action Tag Attribute

setupBoard :: Action ()
setupBoard = do
    mapM_ (uncurry newField) [(x,y) | x <- [-1,0,1], y <- [-1,0,1]]
    turn <- new
    setTag Turn turn True
    setTag X turn True

newField :: Value -> Value -> Action ()
newField x y = do
    field <- new
    setTag Field field True
    setAttribute XPosition field x
    setAttribute YPosition field y

setStone :: Rule
setStone = do
    field <- entityTagged Field
    getTag X field >>= guard . not
    getTag O field >>= guard . not
    x <- getAttribute XPosition field
    y <- getAttribute YPosition field
    turn <- entityTagged Turn
    player <- for [X,O]
    getTag player turn >>= guard
    return (trigger (fieldRect x y) (do
        setTag player field True
        setTag player turn False
        setTag (other player) turn True))

other :: Tag -> Tag
other X = O
other O = X
other u = u

renderField :: Rule
renderField = do
    field <- entityTagged Field
    x <- getAttribute XPosition field
    y <- getAttribute YPosition field
    return (draw (translate (fieldCoordinate x) (fieldCoordinate y) (rectangleWire 60 60)))

renderX :: Rule
renderX = do
    field <- entityTagged Field
    x <- getAttribute XPosition field
    y <- getAttribute YPosition field
    getTag X field >>= guard
    return (draw (translate (fieldCoordinate x) (fieldCoordinate y) cross))

renderO :: Rule
renderO = do
    field <- entityTagged Field
    x <- getAttribute XPosition field
    y <- getAttribute YPosition field
    getTag O field >>= guard
    return (draw (translate (fieldCoordinate x) (fieldCoordinate y) (circleSolid 25)))

cross :: Picture
cross = pictures [rotate degrees (rectangleSolid 60 15) | degrees <- [-45,45]]

fieldRect :: Integer -> Integer -> Trigger
fieldRect x y = ClickableRect (fieldCoordinate x) (fieldCoordinate y) 60 60

fieldCoordinate :: Integer -> Float
fieldCoordinate x = fromIntegral x * 60

main :: IO ()
main = runGame setupBoard [setStone,renderField,renderX,renderO]
