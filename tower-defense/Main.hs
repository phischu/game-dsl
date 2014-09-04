{-# LANGUAGE StandaloneDeriving,DeriveFunctor #-}
module Main where

import GameDSL hiding (Rule,Query,Action,Element,render)
import qualified GameDSL as GameDSL (Rule,Action,Query,Element)

import Graphics.Gloss

import Control.Monad (guard,forM_)

data Tag = Enemy | Site | Turret | Launcher | Bullet | Rocket | North | South | East | West

deriving instance Eq Tag
deriving instance Ord Tag
deriving instance Show Tag

data Attribute = XPosition | YPosition | MoveTime | MoveCounter | Reload

deriving instance Eq Attribute
deriving instance Ord Attribute
deriving instance Show Attribute

type Rule = GameDSL.Rule Tag Attribute
type Query = GameDSL.Query Tag Attribute
type Action = GameDSL.Action Tag Attribute
type Element = GameDSL.Element Tag Attribute

setupBoard :: Action ()
setupBoard = do
    sequence_ (do
        x <- [-40 .. -20]
        y <- [-2 .. 2]
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

upgrade :: Rule
upgrade = do
    turret <- entityTagged Turret
    (x,y) <- getXY turret
    ensureNot (getTag Launcher turret)
    return (trigger (clickInRect (Rect (fieldCoordinate x) (fieldCoordinate y) siteSize siteSize) (do
        setTag Launcher turret)))

fire :: Rule
fire = do
    turret <- entityTagged Turret
    (x,y) <- getXY turret
    direction <- for [North,South]
    ensure (getTag direction turret)
    reload <- getAttribute Reload turret
    isLauncher <- results (getTag Launcher turret) >>= return . not . null
    return (trigger (tick (if reload <= 0
        then do
            setAttribute Reload turret reloadTime
            if isLauncher
                then newShot Rocket 10 direction x y
                else newShot Bullet 5 direction x y
        else do
            setAttribute Reload turret (reload - 1))))

hit :: Rule
hit = do
    bullet <- entityTagged Bullet
    (bx,by) <- getXY bullet
    enemy <- entityTagged Enemy
    (ex,ey) <- getXY enemy
    guard (bx == ex)
    guard (by == ey)
    return (trigger (tick (do
        delete bullet
        delete enemy)))

explode :: Rule
explode = do
    rocket <- entityTagged Rocket
    (rx,ry) <- getXY rocket
    enemy <- entityTagged Enemy
    (ex,ey) <- getXY enemy
    guard (rx == ex)
    guard (ry == ey)
    enemies <- results (do
        enemy' <- entityTagged Enemy
        (ex',ey') <- getXY enemy'
        guard (abs (rx - ex') < 2)
        guard (abs (ry - ey') < 2)
        return enemy')
    return (trigger (tick (do
        delete rocket
        forM_ enemies delete)))

getXY :: Entity -> Query (Value,Value)
getXY entity = do
    x <- getAttribute XPosition entity
    y <- getAttribute YPosition entity
    return (x,y)

newShot :: Tag -> Value -> Tag -> Value -> Value -> Action ()
newShot tag movetime direction x y = do
    bullet <- new
    setTag tag bullet
    setAttribute XPosition bullet x
    setAttribute YPosition bullet y
    setTag direction bullet
    setAttribute MoveTime bullet movetime
    setAttribute MoveCounter bullet 0

reloadTime :: Value
reloadTime = 100

render :: Tag -> Picture -> Rule
render tag pic = do
    entity <- entityTagged tag
    x <- getAttribute XPosition entity
    y <- getAttribute YPosition entity
    return (draw (translate (fieldCoordinate x) (fieldCoordinate y) pic))

renderEnemy :: Rule
renderEnemy = render Enemy (color green (rectangleSolid (fieldSize - 2) (fieldSize - 2)))

renderSite :: Rule
renderSite = render Site (rectangleWire siteSize siteSize)

renderTurret :: Rule
renderTurret = render Turret (color blue (circleSolid (0.3 * siteSize)))

renderLauncher :: Rule
renderLauncher = render Launcher (color orange (rectangleWire siteSize (0.1 * siteSize)))

renderBullet :: Rule
renderBullet = render Bullet (color black (circleSolid (0.3 * fieldSize)))

renderRocket :: Rule
renderRocket = render Rocket (color black (circleSolid (0.6 * fieldSize)))

siteSize :: Float
siteSize = 3 * fieldSize

fieldSize :: Float
fieldSize = 15

fieldRect :: Integer -> Integer -> Rect
fieldRect x y = Rect (fieldCoordinate x) (fieldCoordinate y) fieldSize fieldSize

fieldCoordinate :: Integer -> Float
fieldCoordinate x = fromIntegral x * fieldSize

main :: IO ()
main = runGame setupBoard [
    renderEnemy,renderBullet,renderRocket,renderSite,renderTurret,renderLauncher,
    hit,build,upgrade,fire,move,explode]
