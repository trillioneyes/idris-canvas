module Canvas
import Effects
import Canvas.State
import Canvas.JS
import Canvas.Utils
import Canvas.Style

||| The representation of a rectangle used internally by a canvas
data RectRep : Type where
  Rep : (topleftX : Double) -> (topleftY : Double) -> (width : Double) -> (height : Double)
        -> RectRep

||| A specification of a rectangle
public export
data Rect : Type where
  ||| Specify a rectangle by giving one of its corners and offsets for the other
  ||| corner.
  CornerAndDims : (x, y, width, height : Double) -> Rect
  ||| Specify a rectangle by giving horizontal and vertical bounding regions, or
  ||| equivalently, by giving both corners.
  Bounds : (x1, x2, y1, y2 : Double) -> Rect
  ||| Specify a rectangle by giving the coordinates of its center, and then a radius
  ||| (i.e. half the length) for each axis.
  CenterRadius : (x, y, rx, ry : Double) -> Rect
  ||| Specify a rectangle by giving the coordinates of its center and the length of
  ||| each axis.
  CenterLength : (x, y, width, height : Double) -> Rect

rectRep : Rect -> RectRep
rectRep (CornerAndDims x y width height) = Rep x y width height
rectRep (Bounds x1 x2 y1 y2) = Rep x1 y1 (x2 - x1) (y2 - y1)
rectRep (CenterRadius x y rx ry) = Rep (x - rx) (y - ry) (2 * rx) (2 * ry)
rectRep (CenterLength x y width height) = Rep tlX tlY width height where
  tlX = x - (width / 2)
  tlY = y - (height / 2)

export
rectCenter : Rect -> (Double, Double)
rectCenter rect = case rectRep rect of
  Rep tlX tlY w h => (tlX + w/2, tlY + h/2)

||| A data type representing the 2D transformation matrix used by the canvas
record Transform where
  constructor MkT
  scaleX : Double
  shearX : Double
  shearY : Double
  scaleY : Double
  shiftX : Double
  shiftY : Double

mutual
  -- canvas operation types
  export
  CanvasOp : Type -> Type
  CanvasOp result = { CanvasState } Canvas result
  Draw : Type
  Draw = CanvasOp ()

  export
  data Canvas : Effect where
    Init : String ->
           { () ==> CanvasState } Canvas ()
    Dimensions : CanvasOp (Double, Double)
    RectPath : Rect -> Draw
    BeginPath : Draw
    MoveTo : (Double, Double) -> Draw
    LineTo : (Double, Double) -> Draw
    QuadraticTo : (Double, Double) -> (Double, Double) -> Draw
    ClosePath : Draw
    Fill : Draw
    FillText : String -> (Double, Double) -> Draw
    Stroke : Draw
    SetFill : ColorStyle -> Draw
    SetStroke : ColorStyle -> Draw

(>>) : JS_IO () -> SideEffect a -> SideEffect a
(>>) io se = SE (io *> un se)

export
Handler Canvas SideEffect where
  handle () (Init name) k = SE $ do
    c <- canvasById name
    case c of
      Left err => pure ()
      Right real => un $ k () real
  handle st Dimensions k = k (width st, height st) st
  handle st (RectPath r) k with (rectRep r)
    | (Rep topleftX topleftY width height) = SE $ do
      rect (context st) topleftX topleftY width height
      un $ k () st
  handle st Fill k = fill (context st) >> k () st
  handle st (FillText text (x, y)) k = fillText (context st) text x y >> k () st
  handle st Stroke k = stroke (context st) >>  k () st
  handle st ClosePath k = closePath (context st) >> k () st
  handle st (LineTo (x, y)) k = lineTo (context st) x y >> k () st
  handle st (QuadraticTo (cx, cy) (x, y)) k =
         quadraticTo (context st) cx cy x y >> k () st
  handle st (MoveTo (x, y)) k = moveTo (context st) x y >> k () st
  handle st BeginPath k = beginPath (context st) >> k () st
  handle st (SetStroke c) k = setStroke (context st) c >> k () st
  handle st (SetFill c) k = setFill (context st) c >> k () st

export
WANT_CANVAS : EFFECT
WANT_CANVAS = MkEff () Canvas

export
WantCanvas : Handler Canvas m => Env m [WANT_CANVAS]
WantCanvas = [()]

export
CANVAS : EFFECT
CANVAS = MkEff CanvasState Canvas

export
init : String -> { [WANT_CANVAS] ==> [CANVAS] } Eff ()
init name = call (Init name)

export
dimensions : { [CANVAS] } Eff (Double, Double)
dimensions = call Dimensions

export
rectPath : Rect -> { [CANVAS] } Eff ()
rectPath r = call (RectPath r)

export
fill : { [CANVAS] } Eff ()
fill = call Canvas.Fill

export
fillText : String -> (Double, Double) -> { [CANVAS] } Eff ()
fillText text (x, y) = call (FillText text (x, y))

export
stroke : { [CANVAS] } Eff ()
stroke = call Stroke

export
closePath : { [CANVAS] } Eff ()
closePath = call ClosePath

export
lineTo : (Double, Double) -> { [CANVAS] } Eff ()
lineTo pt = call (LineTo pt)

export
quadraticTo : (Double, Double) -> (Double, Double) -> { [CANVAS] } Eff ()
quadraticTo control pt = call (QuadraticTo control pt)

export
moveTo : (Double, Double) -> { [CANVAS] } Eff ()
moveTo pt = call (MoveTo pt)

export
beginPath : { [CANVAS] } Eff ()
beginPath = call BeginPath

export
setFill : ColorStyle -> { [CANVAS] } Eff ()
setFill c = call (SetFill c)

export
setStroke : ColorStyle -> { [CANVAS] } Eff ()
setStroke c = call (SetStroke c)
