||| Low-level Javascript primitives for accessing a canvas
module Canvas.JS
import IdrisScript

abstract
record Context : Type where
  CO : (can : Ptr) -> (ctx : Ptr) -> Context

public
data CanvasLoadingError = NotACanvas

public
canvas : Ptr -> JS_IO (Either CanvasLoadingError Context)
canvas p = do
  conName <- jscall "%0.constructor.name" (Ptr -> JS_IO String) p
  if conName == "HTMLCanvasElement"
    then map (Right . CO p) (jscall "%0.getContext('2d')" (Ptr -> JS_IO Ptr) p)
    else return (Left NotACanvas)

public
getWidth : Context -> JS_IO Float
getWidth c = jscall "%0.width" (Ptr -> JS_IO Float) (can c)

public
getHeight : Context -> JS_IO Float
getHeight c = jscall "%0.height" (Ptr -> JS_IO Float) (can c)

public
rect : Context -> Float -> Float -> Float -> Float -> JS_IO ()
rect c tlx tly w h = jscall "%0.rect(%1,%2,%3,%4)"
                            (Ptr -> Float -> Float -> Float -> Float -> JS_IO ())
                            (ctx c) tlx tly w h

public
fill : Context -> JS_IO ()
fill c = jscall "%0.fill()"
                (Ptr -> JS_IO ())
                (ctx c)

public
stroke : Context -> JS_IO ()
stroke c = jscall "%0.stroke()"
                  (Ptr -> JS_IO ())
                  (ctx c)
