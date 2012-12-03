module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
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

-- Moving functions

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


-- Init a new grid with only white squares
initGrid :: Grid
initGrid = Grid (replicate width [ white | x <- [1..height] ])


-- UI main function

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
    playButton <- buttonNewWithLabel "Clear"
    playButton `onClicked` putStrLn "Clear clicked"


    -- Layout Global
    lay <- vBoxNew False 5
    containerAdd lay canvas
    containerAdd lay playButton

    -- Add layouts to window and render
    containerAdd win lay
    widgetShowAll win
    mainGUI

-- rendering the scene

render :: DrawingArea -> event -> IO Bool
render canvas _evt =
  do dw <- widgetGetDrawWindow canvas
     drawWindowClear dw
     gc <- gcNew dw
     drawSquare dw (100,100) black
     drawSquare dw (42,42) black
     drawSquare dw (67,99) black
     return True

-- draw a square on the given canvas, at the given position with the given
-- color

drawSquare :: DrawWindow -> Pos -> Color -> IO Bool
drawSquare dw (x,y) color =
  do
    currStyle <- gcNewWithValues dw newGCValues{ foreground = color }
    drawRectangle dw currStyle True (scale x) (scale y) squareSize squareSize
    return True

    where
      scale x = x * squareSize


-- ### Utils ###

-- Colors
white, black :: Color
white = Color 65535 65535 65535
black = Color 0 0 0
