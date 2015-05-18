module Demo
import Effects
import Canvas
import Canvas.JS
import Canvas.State
import Canvas.Utils
import Canvas.Style
import Data.List

drawDemoStuff : { [WANT_CANVAS] ==> [CANVAS] } Eff ()
drawDemoStuff = do
  init "democanvas"
  (w, h) <- dimensions
  beginPath
  moveTo (0, 0)
  lineTo (w, h)
  setStroke (Solid (Named "ALICEBLUE"))
  stroke
  beginPath
  moveTo (w, 0)
  lineTo (0, h)
  setStroke (Solid (RGB 0 100 100))
  stroke
  rectPath (CenterRadius (w/2) (h-30) 40 10)
  setFill (Solid (RGBA 255 80 80 0.5))
  fill

namespace Main
  main : JS_IO ()
  main = un $ run drawDemoStuff
