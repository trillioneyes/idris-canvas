module Demo
import IdrisScript
import Effects
import Canvas
import Canvas.JS
import Canvas.State
import Canvas.Utils

drawDemoStuff : { [WANT_CANVAS] ==> [CANVAS] } Eff ()
drawDemoStuff = do
  init "democanvas"
  (w, h) <- dimensions
  beginPath
  moveTo (0, 0)
  lineTo (w, h)
  stroke
  rectPath (CenterRadius (w/2) (h-30) 40 10)
  fill

namespace Main
  main : JS_IO ()
  main = un $ run drawDemoStuff
