{-# LANGUAGE StandaloneDeriving,DeriveFunctor #-}
module Main where

import GameDSL hiding (Rule,Action,Render)
import qualified GameDSL as GameDSL (Rule,Action,Render)

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
type Render = GameDSL.Render Tag Attribute

move :: Rule
move = do
    selected <- tagged Selected
    x <- get XPosition selected
    y <- get YPosition selected
    (dx,dy) <- for [(1,0),(-1,0),(0,1),(0,-1)]
    let (nx,ny) = (x + dx, y + dy)
        (tx,ty) = (nx + dx, ny + dy)
    other <- stoneAt nx ny
    no (stoneAt tx ty)
    return (Clickable (fieldRect tx ty) (do
        delete other
        set XPosition selected tx
        set YPosition selected ty))

stoneAt :: Integer -> Integer -> Query Tag Attribute Entity
stoneAt x y = do
    stone <- tagged Stone
    x' <- get XPosition stone
    y' <- get YPosition stone
    guard (x == x')
    guard (y == y')
    return stone

select :: Rule
select = do
    stone <- tagged Stone
    x <- get XPosition stone
    y <- get YPosition stone
    selected <- tagged Selected
    return (Clickable (fieldRect x y) (do
        untag Selected selected
        tag Selected stone))

setupBoard :: Action ()
setupBoard = do
    mapM_ (uncurry newStone) (
        [(x,y) | x <- [-1,0,1], y <- [-1,0,1], not (x == 0 && y == 0)] ++
        [(x,y) | x <- [-1,0,1], y <- [2,3]] ++
        [(x,y) | x <- [-1,0,1], y <- [-2,-3]] ++
        [(x,y) | x <- [2,3], y <- [-1,0,1]] ++
        [(x,y) | x <- [-2,-3], y <- [-1,0,1]])
    tag Selected 0

newStone :: Integer -> Integer -> Action ()
newStone x y = do
    stone <- new
    tag Stone stone
    set XPosition stone x
    set YPosition stone y


renderStone :: Render
renderStone = do
    stone <- tagged Stone
    x <- get XPosition stone
    y <- get YPosition stone
    return (translate (fieldCoordinate x) (fieldCoordinate y) (circleSolid 20))

renderSelected :: Render
renderSelected = do
    selected <- tagged Selected
    x <- get XPosition selected
    y <- get YPosition selected
    return (translate (fieldCoordinate x) (fieldCoordinate y) (color red (circleSolid 18)))

fieldRect :: Integer -> Integer -> Area
fieldRect x y = Rect (fieldCoordinate x) (fieldCoordinate y) 40 40

fieldCoordinate :: Integer -> Float
fieldCoordinate x = fromIntegral x * 40

main :: IO ()
main = runGame setupBoard [select,move] [renderStone,renderSelected]
