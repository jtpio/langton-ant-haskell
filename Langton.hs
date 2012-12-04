module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Data.IORef
import Test.QuickCheck

width, height, squareSize, canvasWidth, canvasHeight :: Int
width  = 200 -- number of squares
height  = 150 -- number of squares
squareSize = 5 -- px
canvasWidth = width * squareSize -- px
canvasHeight = height * squareSize -- px

-- Data types

-- Direction: North, South, West, East
data Direction = N | S | W | E
  deriving(Eq, Show)

-- Grid
data Grid = Grid { rows :: [[Color]] }
    deriving (Eq, Show)

-- Types
type Pos = (Int, Int)

-- Position of the Ant is set
type Ant = (Pos, Direction)

type State = (Ant,Grid)

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


-- Init a new grid
initGrid :: Grid
initGrid = Grid (replicate width [ grass | x <- [0..height] ])

-- Create an ant in the middle of the screen and going to the west direction
ant :: Ant
ant = ((width `div` 2, height `div` 2), W)

-- #### Tick and update ------------------------------------------------------

tick :: DrawingArea-> Ant -> Grid -> IO Bool
tick canvas ant grid =
  do
    putStrLn "Tick!"
    dw <- widgetGetDrawWindow canvas
    (newGrid, newAnt) <- step dw ant grid
    return True


step :: DrawWindow -> Ant -> Grid -> IO (Grid, Ant)
step dw ((x,y),dir) grid =
  do
    let currColor = (rows grid) !! x !! y in
      case currColor of
        grass -> do
                  drawSquare dw (x,y) black
                  return (updateGrid grid (x,y) black, move ((x,y), right dir))
        black -> do
                  drawSquare dw (x,y) grass
                  return (updateGrid grid (x,y) grass, move ((x,y), left dir))

-- Update the grid
-- (from our lab 3 Sudoku)

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
      map (\(i,j) -> drawSquare dw (i,j) (grid !! i !! j)) [ (x,y) | x <- [0..width], y <- [0..height] ]

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

    ---- Canvas
    canvas <- drawingAreaNew
    canvas `onSizeRequest` return (Requisition canvasWidth canvasHeight)
    canvas `onExpose` render canvas

    -- Play Button
    playButton <- buttonNewWithLabel "Play"
    -- playButton `onClicked` tick canvas ant initGrid

    -- Layout Global
    lay <- vBoxNew False 5
    containerAdd lay canvas
    containerAdd lay playButton

    -- Add layouts to window and render
    containerAdd win lay
    widgetShowAll win

    -- Launch simulation
    timeoutAdd (tick canvas ant initGrid) 500

    mainGUI

-- #### Utils ----------------------------------------------------------------

-- Colors
white, black :: Color
white = Color 65535 65535 65535
black = Color 0 0 0
red   = Color 65535 0 0
green = Color 0 65535 0
grass = Color 1792 36608 0

-- Updates the given list with the new value at the given index
-- (from our lab 3 Sudoku)

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _ = []
(!!=) ls (index, newValue)
      | index < length ls && index >= 0 =
        (take index ls) ++ [newValue] ++ (drop (index+1) ls)
      | otherwise = ls
