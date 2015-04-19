module Demo
import IdrisScript
import JS

withCanvas : (Context -> JS_IO ()) -> JS_IO ()
withCanvas f = do
  obj <- jscall "document.getElementById('democanvas')" (JS_IO Ptr)
  c <- canvas obj
  case c of
    Left err => putStr "Couldn't load the canvas"
    Right ctx => f ctx

drawDemoStuff : Context -> JS_IO ()
drawDemoStuff ctx = do
  w <- getWidth ctx
  h <- getHeight ctx
  putStr ("(" ++ show w ++ "," ++ show h ++ ")")
  rect ctx 5 5 (w-10) (h-10)
  fill ctx

namespace Main
  main : JS_IO ()
  main = do
    putStr "Attempting to load canvas..."
    withCanvas drawDemoStuff
    putStr "Demo picture drawn!"
