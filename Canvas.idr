module Canvas
import Effects
import Canvas.State
import Canvas.JS
import Canvas.Utils
import Canvas.Style

||| The representation of a rectangle used internally by a canvas
data RectRep : Type where
  Rep : (topleftX : Float) -> (topleftY : Float) -> (width : Float) -> (height : Float)
        -> RectRep

||| A specification of a rectangle
data Rect : Type where
  ||| Specify a rectangle by giving one of its corners and offsets for the other
  ||| corner.
  CornerAndDims : (x, y, width, height : Float) -> Rect
  ||| Specify a rectangle by giving horizontal and vertical bounding regions, or
  ||| equivalently, by giving both corners.
  Bounds : (x1, x2, y1, y2 : Float) -> Rect
  ||| Specify a rectangle by giving the coordinates of its center, and then a radius
  ||| (i.e. half the length) for each axis.
  CenterRadius : (x, y, rx, ry : Float) -> Rect
  ||| Specify a rectangle by giving the coordinates of its center and the length of
  ||| each axis.
  CenterLength : (x, y, width, height : Float) -> Rect

rectRep : Rect -> RectRep
rectRep (CornerAndDims x y width height) = Rep x y width height
rectRep (Bounds x1 x2 y1 y2) = Rep x1 y1 (x2 - x1) (y2 - y1)
rectRep (CenterRadius x y rx ry) = Rep (x - rx) (y - ry) (2 * rx) (2 * ry)
rectRep (CenterLength x y width height) = Rep tlX tlY width height where
  tlX = x - (width / 2)
  tlY = y - (height / 2)

||| A data type representing the 2D transformation matrix used by the canvas
record Transform where
  constructor MkT
  scaleX : Float
  shearX : Float
  shearY : Float
  scaleY : Float
  shiftX : Float
  shiftY : Float

mutual
  -- canvas operation types
  CanvasOp : Type -> Type
  CanvasOp result = { CanvasState } Canvas result
  Draw : Type
  Draw = CanvasOp ()

  data Canvas : Effect where
    Init : String ->
           { () ==> CanvasState } Canvas ()
    Dimensions : CanvasOp (Float, Float)
    RectPath : Rect -> Draw
    BeginPath : Draw
    MoveTo : (Float, Float) -> Draw
    LineTo : (Float, Float) -> Draw
    ClosePath : Draw
    Fill : Draw
    Stroke : Draw
    SetFill : ColorStyle -> Draw
    SetStroke : ColorStyle -> Draw

(>>) : JS_IO () -> SideEffect a -> SideEffect a
(>>) io se = SE (io *> un se)

instance Handler Canvas SideEffect where
  handle () (Init name) k = SE $ do
    c <- canvasById name
    case c of
      Left err => putStr "Couldn't load canvas"
      Right real => un $ k () real
  handle st Dimensions k = k (width st, height st) st
  handle st (RectPath r) k with (rectRep r)
    | (Rep topleftX topleftY width height) = SE $ do
      rect (context st) topleftX topleftY width height
      un $ k () st
  handle st Fill k = fill (context st) >> k () st
  handle st Stroke k = stroke (context st) >>  k () st
  handle st ClosePath k = closePath (context st) >> k () st
  handle st (LineTo (x, y)) k = lineTo (context st) x y >> k () st
  handle st (MoveTo (x, y)) k = moveTo (context st) x y >> k () st
  handle st BeginPath k = beginPath (context st) >> k () st
  handle st (SetStroke c) k = setStroke (context st) c >> k () st
  handle st (SetFill c) k = setFill (context st) c >> k () st

WANT_CANVAS : EFFECT
WANT_CANVAS = MkEff () Canvas

CANVAS : EFFECT
CANVAS = MkEff CanvasState Canvas

init : String -> { [WANT_CANVAS] ==> [CANVAS] } Eff ()
init name = call (Init name)

dimensions : { [CANVAS] } Eff (Float, Float)
dimensions = call Dimensions

rectPath : Rect -> { [CANVAS] } Eff ()
rectPath r = call (RectPath r)

fill : { [CANVAS] } Eff ()
fill = call Canvas.Fill

stroke : { [CANVAS] } Eff ()
stroke = call Stroke

closePath : { [CANVAS] } Eff ()
closePath = call ClosePath

lineTo : (Float, Float) -> { [CANVAS] } Eff ()
lineTo pt = call (LineTo pt)

moveTo : (Float, Float) -> { [CANVAS] } Eff ()
moveTo pt = call (MoveTo pt)

beginPath : { [CANVAS] } Eff ()
beginPath = call BeginPath

setFill : ColorStyle -> { [CANVAS] } Eff ()
setFill c = call (SetFill c)

setStroke : ColorStyle -> { [CANVAS] } Eff ()
setStroke c = call (SetStroke c)
