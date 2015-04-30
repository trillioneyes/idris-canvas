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
  rectPath (CornerAndDims 5 5 (w-10) (h-10))
  fill

namespace Main
  main : JS_IO ()
  main = un $ run drawDemoStuff
