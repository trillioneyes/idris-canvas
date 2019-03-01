||| Low-level Javascript primitives for accessing a canvas
module Canvas.JS
import Canvas.Style

%inline
export
jscall : String -> (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jscall name ty = foreign FFI_JS name ty

export
record Context where
  constructor CO
  can : Ptr
  ctx : Ptr

public export
data CanvasLoadingError = NotACanvas

export
canvas : Ptr -> JS_IO (Either CanvasLoadingError Context)
canvas p = do
  conName <- jscall "%0.constructor.name" (Ptr -> JS_IO String) p
  if conName == "HTMLCanvasElement"
    then map (Right . CO p) (jscall "%0.getContext('2d')" (Ptr -> JS_IO Ptr) p)
    else pure (Left NotACanvas)

export
getWidth : Context -> JS_IO Double
getWidth c = jscall "%0.width" (Ptr -> JS_IO Double) (can c)

export
getHeight : Context -> JS_IO Double
getHeight c = jscall "%0.height" (Ptr -> JS_IO Double) (can c)

export
rect : Context -> Double -> Double -> Double -> Double -> JS_IO ()
rect c tlx tly w h = jscall "%0.rect(%1,%2,%3,%4)"
                            (Ptr -> Double -> Double -> Double -> Double -> JS_IO ())
                            (ctx c) tlx tly w h

export
fill : Context -> JS_IO ()
fill c = jscall "%0.fill()"
                (Ptr -> JS_IO ())
                (ctx c)

export
stroke : Context -> JS_IO ()
stroke c = jscall "%0.stroke()"
                  (Ptr -> JS_IO ())
                  (ctx c)

export
beginPath : Context -> JS_IO ()
beginPath c = jscall "%0.beginPath()"
                     (Ptr -> JS_IO ())
                     (ctx c)

export
moveTo : Context -> Double -> Double -> JS_IO ()
moveTo c x y = jscall "%0.moveTo(%1, %2)"
                      (Ptr -> Double -> Double -> JS_IO ())
                      (ctx c) x y

export
lineTo : Context -> Double -> Double -> JS_IO ()
lineTo c x y = jscall "%0.lineTo(%1, %2)"
                      (Ptr -> Double -> Double -> JS_IO ())
                      (ctx c) x y

export
closePath : Context -> JS_IO ()
closePath c = jscall "%0.closePath()"
                     (Ptr -> JS_IO ())
                     (ctx c)

export
setFill : Context -> ColorStyle -> JS_IO ()
setFill c s = jscall "%0.fillStyle = %1"
                     (Ptr -> String -> JS_IO ())
                     (ctx c) (colorProperty s)

export
setStroke : Context -> ColorStyle -> JS_IO ()
setStroke c s = jscall "%0.strokeStyle = %1"
                       (Ptr -> String -> JS_IO ())
                       (ctx c) (colorProperty s)
