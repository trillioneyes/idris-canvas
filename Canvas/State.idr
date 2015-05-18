module State
import Canvas.JS

data PathPrim : Type where
  ||| A line segment with the given endpoints
  Seg : (Float, Float) -> (Float, Float) -> PathPrim

Path : Type
Path = List PathPrim

||| A data type representing canvas state
record CanvasState where
  constructor MkC
  context : Context
  path : Path
  -- transformation : Transform
  width : Float
  height : Float
  -- fillStyle : Fill
  -- strokeStyle : Stroke
  -- shadowStyle : Shadow

canvasById : String -> JS_IO (Either CanvasLoadingError CanvasState)
canvasById name = do
  jsObj <- jscall "document.getElementById(%0)"
                  (String -> JS_IO Ptr)
                  name
  context <- canvas jsObj
  either (map Left . return) (map Right . toCState) context
 where toCState : Context -> JS_IO CanvasState
       toCState c = do
         w <- getWidth c
         h <- getHeight c
         beginPath c
         return (MkC c [] w h)
