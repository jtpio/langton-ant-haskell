module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Data.IORef
import Test.QuickCheck

-- #### Parameters -----------------------------------------------------------

width, height, squareSize, canvasWidth, canvasHeight :: Int
width  = 200 -- number of squares
height  = 150 -- number of squares
squareSize = 5 -- px
canvasWidth = width * squareSize -- px
canvasHeight = height * squareSize -- px

-- #### Data types ant types -------------------------------------------------

-- Direction: North, South, West, East

data Direction = N | S | W | E
  deriving(Eq, Show)

-- Grid, a matrix of colors

data Grid = Grid { rows :: [[Color]] }
    deriving (Eq, Show)

-- Types

type Pos = (Int, Int)

-- Position of the Ant is set

type Ant  = (Pos, Direction)
type Ants = [Ant]

-- #### Grid related functions -----------------------------------------------

-- Init a new grid with the same color everywhere

initGrid :: Grid
initGrid = Grid (replicate width [ grass | x <- [0..height] ])

-- #### Ant related functions ------------------------------------------------

-- Create an ant in the middle of the screen and going to the West direction

middleAnt :: Ant
middleAnt = ((width `div` 2, height `div` 2), W)

-- Find next direction when turning left or right

left :: Direction -> Direction
left N = E
left E = S
left S = W
left W = N

right :: Direction -> Direction
right N = W
right W = S
right S = E
right E = N

-- Move the ant

move :: Ant -> Ant
move ((x,y), dir) = case dir of
  N -> ((x, mod (y-1) height), dir)
  S -> ((x, mod (y+1) height), dir)
  W -> ((mod (x-1) width, y), dir)
  E -> ((mod (x+1) width, y), dir)

-- Add a new ant at the click position

addAnt :: DrawingArea -> IORef Ants -> IO Bool
addAnt canvas ants =
  do
    (x,y) <- widgetGetPointer canvas
    prevAnts <- readIORef ants
    writeIORef ants (((scale x, scale y), W) : prevAnts)
    return True

    where
      scale x = x `div` squareSize

-- #### Tick, update and step ------------------------------------------------

tick :: DrawingArea-> IORef Ants -> IORef Grid -> IO Bool
tick canvas antsRef gridRef =
  do
    dw <- widgetGetDrawWindow canvas
    -- Get previous values
    ants <- readIORef antsRef
    grid <- readIORef gridRef
    -- New step for all the ants
    (newAnts, newGrid) <- update dw ants grid []
    -- Save new values
    writeIORef antsRef newAnts
    writeIORef gridRef newGrid

    return True

update :: DrawWindow -> Ants -> Grid -> Ants -> IO (Ants,Grid)
update dw (ant:ants') grid nextAnts =
  do
    (newAnt, newGrid) <- step dw ant grid
    update dw ants' newGrid (newAnt:nextAnts)

update dw [] grid nextAnts =
  do
    return (nextAnts, grid)

step :: DrawWindow -> Ant -> Grid -> IO (Ant, Grid)
step dw ((x,y),dir) grid =
  do
    let currColor = (rows grid) !! x !! y in
      case currColor of
        Color 1792 36608 0 -> do
              drawSquare dw (x,y) black
              return (move ((x,y), right dir), updateGrid grid (x,y) black)
        Color 0 0 0 -> do
              drawSquare dw (x,y) grass
              return (move ((x,y), left dir), updateGrid grid (x,y) grass)

-- Update the grid

updateGrid :: Grid -> Pos -> Color -> Grid
updateGrid (Grid grid) (x,y) color = Grid (grid !!= (x, newValue))
    where newValue = (grid !! x) !!= (y, color)

-- #### Drawing function -----------------------------------------------------

-- Render the scene

render :: DrawingArea -> event -> IO Bool
render canvas _evt =
  do dw <- widgetGetDrawWindow canvas
     drawWindowClear dw
     drawGrid dw initGrid
     return True

-- Draw a square on the given canvas, at the given position with the given
-- color

drawSquare :: DrawWindow -> Pos -> Color -> IO Bool
drawSquare dw (x,y) color =
  do
    currStyle <- gcNewWithValues dw newGCValues{ foreground = color }
    drawRectangle dw currStyle True (scale x) (scale y) squareSize squareSize
    return True
    where
      scale x = x * squareSize

-- Draw the grid

drawGrid :: DrawWindow -> Grid -> IO Bool
drawGrid dw (Grid grid) =
  do
    sequence_ $
      map
        (\(i,j) -> drawSquare dw (i,j)
        (grid !! i !! j)) [ (x,y) | x <- [0..width], y <- [0..height] ]

    return True


-- #### UI main function -----------------------------------------------------

main :: IO ()
main =
  do
    initGUI

    -- window
    win <- windowNew
    windowSetTitle win "Langton's Ant"
    win `onDestroy` mainQuit

    -- Create references to the list of ants and the grid
    ants <- newIORef [] -- List of ants
    grid <- newIORef initGrid -- Only one grid for all the ants

    writeIORef ants (middleAnt : [])

    -- Canvas
    canvas <- drawingAreaNew
    canvas `onSizeRequest` return (Requisition canvasWidth canvasHeight)
    canvas `onExpose` render canvas
    canvas `onButtonPress` \_ -> addAnt canvas ants

    -- Reset Button
    playButton <- buttonNewWithLabel "Reset"
    playButton `onClicked` reset canvas ants grid

    -- Layout Global
    lay <- vBoxNew False 5
    containerAdd lay canvas
    containerAdd lay playButton

    -- Add layouts to window and render
    containerAdd win lay
    widgetShowAll win

    -- Launch simulation
    timeoutAdd (tick canvas ants grid) 1

    mainGUI

reset :: DrawingArea -> IORef Ants -> IORef Grid -> IO ()
reset canvas antsRef gridRef =
  do
    dw <- widgetGetDrawWindow canvas
    drawWindowClear dw
    drawGrid dw initGrid
    writeIORef antsRef (middleAnt:[])
    writeIORef gridRef initGrid

-- #### Utils ----------------------------------------------------------------

-- Colors
white, black, red, green, grass :: Color
white = Color 65535 65535 65535
black = Color 0 0 0
red   = Color 65535 0 0
green = Color 0 65535 0
grass = Color 1792 36608 0

-- Updates the given list with the new value at the given index

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _ = []
(!!=) ls (index, newValue)
      | index < length ls && index >= 0 =
        (take index ls) ++ [newValue] ++ (drop (index+1) ls)
      | otherwise = ls
