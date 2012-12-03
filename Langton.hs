module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC

width, height, nbSquares, squareSize :: Int
width  = 500
height  = width
nbSquares = 20
squareSize = width `div` nbSquares

-- Data types


-- Types
type Pos = (Int, Int)


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
    canvas `onSizeRequest` return (Requisition width height)
    canvas `onExpose` render canvas

    -- Play Button
    playButton <- buttonNewWithLabel "Play"
    playButton `onClicked` putStrLn "Play clicked"


    -- Layout Global
    lay <- vBoxNew False 5
    containerAdd lay canvas
    containerAdd lay playButton

    -- Add layouts to window
    containerAdd win lay

    widgetShowAll win

    mainGUI

render :: DrawingArea -> event -> IO Bool
render canvas _evt =
  do dw <- widgetGetDrawWindow canvas
     drawWindowClear dw
     gc <- gcNew dw
     drawRectangle dw gc True 0 0 squareSize squareSize
     return True
