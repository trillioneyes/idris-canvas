module Canvas
import Effects
import Canvas.State
import Canvas.JS
import Canvas.Utils

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
  CenterRadius : (x, y, rx, y : Float) -> Rect
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
record Transform : Type where
  MkT : (scaleX, shearX, shearY, scaleY, shiftX, shiftY : Float) ->
        Transform

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
    Fill : Draw
    Stroke : Draw

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
  handle st Fill k = SE $ fill (context st) *> un (k () st)
  handle st Stroke k = SE $ stroke (context st) *> un (k () st)

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
