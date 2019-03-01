module State
import Canvas.JS

data PathPrim : Type where
  ||| A line segment with the given endpoints
  Seg : (Double, Double) -> (Double, Double) -> PathPrim

export
Path : Type
Path = List PathPrim

||| A data type representing canvas state
public export
record CanvasState where
  constructor MkC
  context : Context
  path : Path
  -- transformation : Transform
  width : Double
  height : Double
  -- fillStyle : Fill
  -- strokeStyle : Stroke
  -- shadowStyle : Shadow

export
canvasById : String -> JS_IO (Either CanvasLoadingError CanvasState)
canvasById name = do
  jsObj <- jscall "document.getElementById(%0)"
                  (String -> JS_IO Ptr)
                  name
  context <- canvas jsObj
  either (map Left . pure) (map Right . toCState) context
 where toCState : Context -> JS_IO CanvasState
       toCState c = do
         w <- getWidth c
         h <- getHeight c
         beginPath c
         pure (MkC c [] w h)
