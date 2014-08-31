module Main where

data Classes = Selected | Stone

data Attributes = XPosition | YPosition

move :: Action
move = do
    selected <- a Selected
    x <- get XPosition selected
    y <- get YPosition selected
    (dx,dy) <- for [(1,0),(-1,0),(0,1),(0,-1)]
    let (nx,ny) = (x + dx, y + dy)
        (tx,ty) = (nx + dx, ny + dy)
    other <- stoneAt nx ny
    no (stoneAt tx ty)
    field tx ty
    clickable (fieldRect tx ty) (do
        delete other
        set XPosition selected tx
        set YPosition selected ty)

select :: Action
select = do
    stone <- a Stone
    x <- get XPosition stone
    y <- get YPosition stone
    selected <- a Selected
    clickable (fieldRect x y) (do
        retract Selected selected
        assure Selected stone)

renderStone :: Render
renderStone = do
    stone <- a Stone
    x <- get XPosition stone
    y <- get YPosition stone
    return (translate (fieldCoordinate x) (fieldCoordinate y) (circle 20))

renderSelected :: Render
renderSelected = do
    selected <- a Selected
    x <- get XPosition selected
    y <- get YPosition selected
    return (translate (fieldCoordinate x) (fieldCoordinate y) (rect 20 20))


main :: IO ()
main = putStrLn "hallo"








