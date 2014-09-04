{-# LANGUAGE StandaloneDeriving,DeriveFunctor #-}
module Main where

import GameDSL hiding (Rule,Action)
import qualified GameDSL as GameDSL (Rule,Action)

import Graphics.Gloss

import Control.Monad (guard)

data Tag = Selected | Stone

deriving instance Eq Tag
deriving instance Ord Tag
deriving instance Show Tag

data Attribute = XPosition | YPosition

deriving instance Eq Attribute
deriving instance Ord Attribute
deriving instance Show Attribute

type Rule = GameDSL.Rule Tag Attribute
type Action = GameDSL.Action Tag Attribute

move :: Rule
move = do
    selected <- entityTagged Selected
    x <- getAttribute XPosition selected
    y <- getAttribute YPosition selected
    (dx,dy) <- for [(1,0),(-1,0),(0,1),(0,-1)]
    let (nx,ny) = (x + dx, y + dy)
        (tx,ty) = (nx + dx, ny + dy)
    other <- stoneAt nx ny
    ensureNot (stoneAt tx ty)
    return (trigger (clickInRect (fieldRect tx ty) (do
        delete other
        setAttribute XPosition selected tx
        setAttribute YPosition selected ty)))

stoneAt :: Integer -> Integer -> Query Tag Attribute Entity
stoneAt x y = do
    stone <- entityTagged Stone
    x' <- getAttribute XPosition stone
    y' <- getAttribute YPosition stone
    guard (x == x')
    guard (y == y')
    return stone

select :: Rule
select = do
    stone <- entityTagged Stone
    x <- getAttribute XPosition stone
    y <- getAttribute YPosition stone
    selected <- entityTagged Selected
    return (trigger (clickInRect (fieldRect x y) (do
        unsetTag Selected selected
        setTag Selected stone)))

setupBoard :: Action ()
setupBoard = do
    mapM_ (uncurry newStone) (
        [(x,y) | x <- [-1,0,1], y <- [-1,0,1], not (x == 0 && y == 0)] ++
        [(x,y) | x <- [-1,0,1], y <- [2,3]] ++
        [(x,y) | x <- [-1,0,1], y <- [-2,-3]] ++
        [(x,y) | x <- [2,3], y <- [-1,0,1]] ++
        [(x,y) | x <- [-2,-3], y <- [-1,0,1]])
    setTag Selected 0

newStone :: Integer -> Integer -> Action ()
newStone x y = do
    stone <- new
    setTag Stone stone
    setAttribute XPosition stone x
    setAttribute YPosition stone y


renderStone :: Rule
renderStone = do
    stone <- entityTagged Stone
    x <- getAttribute XPosition stone
    y <- getAttribute YPosition stone
    return (draw (translate (fieldCoordinate x) (fieldCoordinate y) (circleSolid 20)))

renderSelected :: Rule
renderSelected = do
    selected <- entityTagged Selected
    x <- getAttribute XPosition selected
    y <- getAttribute YPosition selected
    return (draw (translate (fieldCoordinate x) (fieldCoordinate y) (color red (circleSolid 18))))

fieldRect :: Integer -> Integer -> Rect
fieldRect x y = Rect (fieldCoordinate x) (fieldCoordinate y) 40 40

fieldCoordinate :: Integer -> Float
fieldCoordinate x = fromIntegral x * 40

main :: IO ()
main = runGame setupBoard [select,move,renderStone,renderSelected]
