{-# LANGUAGE StandaloneDeriving,DeriveFunctor #-}
module Main where

import GameDSL hiding (Rule,Action,Render)
import qualified GameDSL as GameDSL (Rule,Action,Render)

import Graphics.Gloss

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
type Render = GameDSL.Render Tag Attribute

setupBoard :: Action ()
setupBoard = do
    mapM_ (uncurry newField) [(x,y) | x <- [-1,0,1], y <- [-1,0,1]]
    turn <- new
    tag Turn turn
    tag X turn

newField :: Value -> Value -> Action ()
newField x y = do
    field <- new
    tag Field field
    set XPosition field x
    set YPosition field y

setStone :: Rule
setStone = do
    field <- tagged Field
    no (hasTag X field)
    no (hasTag O field)
    x <- get XPosition field
    y <- get YPosition field
    turn <- tagged Turn
    player <- for [X,O]
    hasTag player turn
    return (Clickable (fieldRect x y) (do
        tag player field
        untag player turn
        tag (other player) turn))

other :: Tag -> Tag
other X = O
other O = X
other u = u

renderField :: Render
renderField = do
    field <- tagged Field
    x <- get XPosition field
    y <- get YPosition field
    return (translate (fieldCoordinate x) (fieldCoordinate y) (rectangleWire 60 60))

renderX :: Render
renderX = do
    field <- tagged Field
    x <- get XPosition field
    y <- get YPosition field
    hasTag X field
    return (translate (fieldCoordinate x) (fieldCoordinate y) cross)

renderO :: Render
renderO = do
    field <- tagged Field
    x <- get XPosition field
    y <- get YPosition field
    hasTag O field
    return (translate (fieldCoordinate x) (fieldCoordinate y) (circleSolid 25))

cross :: Picture
cross = pictures [rotate degrees (rectangleSolid 60 15) | degrees <- [-45,45]]

fieldRect :: Integer -> Integer -> Area
fieldRect x y = Rect (fieldCoordinate x) (fieldCoordinate y) 60 60

fieldCoordinate :: Integer -> Float
fieldCoordinate x = fromIntegral x * 60

main :: IO ()
main = runGame setupBoard [setStone] [renderField,renderX,renderO]
