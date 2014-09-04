{-# LANGUAGE StandaloneDeriving,DeriveFunctor #-}
module Main where

import GameDSL hiding (Rule,Action,Element,render)
import qualified GameDSL as GameDSL (Rule,Action,Element)

import Graphics.Gloss

data Tag = Enemy | Site | Turret | Bullet | North | South | East | West

deriving instance Eq Tag
deriving instance Ord Tag
deriving instance Show Tag

data Attribute = XPosition | YPosition | MoveTime | MoveCounter | Reload

deriving instance Eq Attribute
deriving instance Ord Attribute
deriving instance Show Attribute

type Rule = GameDSL.Rule Tag Attribute
type Action = GameDSL.Action Tag Attribute
type Element = GameDSL.Element Tag Attribute

setupBoard :: Action ()
setupBoard = do
    sequence_ (do
        x <- [-20..0]
        y <- [-2..2]
        return (newEnemy x y))
    sequence_ (do
        x <- [-9,-6..9]
        [newSite South x 9,newSite North x (-9)])

newEnemy :: Value -> Value -> Action ()
newEnemy x y = do
    enemy <- new
    setTag Enemy enemy
    setTag East enemy
    setAttribute XPosition enemy x
    setAttribute YPosition enemy y
    setAttribute MoveCounter enemy 0
    setAttribute MoveTime enemy 20

newSite :: Tag -> Value -> Value -> Action ()
newSite direction x y = do
    site <- new
    setTag Site site
    setTag direction site
    setAttribute XPosition site x
    setAttribute YPosition site y

move :: Rule
move = do
    entity <- getEntity
    x <- getAttribute XPosition entity
    y <- getAttribute YPosition entity
    mt <- getAttribute MoveTime entity
    xc <- getAttribute MoveCounter entity
    direction <- for [North,South,East,West]
    ensure (getTag direction entity)
    return (trigger (tick (if xc <= 0
        then do
            case direction of
                East -> setAttribute XPosition entity (x + 1)
                West -> setAttribute XPosition entity (x - 1)
                North -> setAttribute YPosition entity (y + 1)
                South -> setAttribute YPosition entity (y - 1)
            setAttribute MoveCounter entity mt
        else do
            setAttribute MoveCounter entity (xc - 1))))

build :: Rule
build = do
    site <- entityTagged Site
    x <- getAttribute XPosition site
    y <- getAttribute YPosition site
    ensureNot (getTag Turret site)
    return (trigger (clickInRect (Rect (fieldCoordinate x) (fieldCoordinate y) siteSize siteSize) (do
        setTag Turret site
        setAttribute Reload site 0)))

fire :: Rule
fire = do
    turret <- entityTagged Turret
    x <- getAttribute XPosition turret
    y <- getAttribute YPosition turret
    direction <- for [North,South]
    ensure (getTag direction turret)
    reload <- getAttribute Reload turret
    return (trigger (tick (if reload <= 0
        then do
            setAttribute Reload turret reloadTime
            newBullet direction x y
        else do
            setAttribute Reload turret (reload - 1))))

newBullet :: Tag -> Value -> Value -> Action ()
newBullet direction x y = do
    bullet <- new
    setTag Bullet bullet
    setAttribute XPosition bullet x
    setAttribute YPosition bullet y
    setTag direction bullet
    setAttribute MoveTime bullet 5
    setAttribute MoveCounter bullet 0

reloadTime :: Value
reloadTime = 40

render :: Tag -> (Float -> Float -> Picture) -> Rule
render tag pic = do
    entity <- entityTagged tag
    x <- getAttribute XPosition entity
    y <- getAttribute YPosition entity
    return (draw (pic (fieldCoordinate x) (fieldCoordinate y)))

renderEnemy :: Rule
renderEnemy = render Enemy (\x y -> 
    color green (
        translate x y (
            rectangleSolid (fieldSize - 2) (fieldSize - 2))))

renderSite :: Rule
renderSite = render Site (\x y ->
    translate x y (rectangleWire siteSize siteSize))

renderTurret :: Rule
renderTurret = render Turret (\x y ->
    translate x y (
            color blue (
                circleSolid (0.5 * siteSize))))

renderBullet :: Rule
renderBullet = render Bullet (\x y ->
    translate x y (
        color black (
            circleSolid (0.5 * fieldSize))))

siteSize :: Float
siteSize = 3 * fieldSize

fieldSize :: Float
fieldSize = 15

fieldRect :: Integer -> Integer -> Rect
fieldRect x y = Rect (fieldCoordinate x) (fieldCoordinate y) fieldSize fieldSize

fieldCoordinate :: Integer -> Float
fieldCoordinate x = fromIntegral x * fieldSize

main :: IO ()
main = runGame setupBoard [renderEnemy,renderSite,renderTurret,renderBullet,build,fire,move]
