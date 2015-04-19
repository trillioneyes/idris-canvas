module Canvas
import Effects

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

||| A data type representing canvas state
record CanvasState : Type where
  MkC : (transformation : Transform) ->
        (width, height : Float) ->
        -- (fillStyle : Fill) ->
        -- (strokeStyle : Stroke) ->
        -- (shadowStyle : Shadow) ->
        CanvasState

mutual
  -- canvas operation types
  CanvasOp : Type -> Type
  CanvasOp result = { CanvasState } Canvas result
  Draw : Type
  Draw = CanvasOp ()

  data Canvas : Effect where
    Dimensions : CanvasOp (Float, Float)
    RectPath : Rect -> Draw
    FillRect : Rect -> Draw
    StrokeRect : Rect -> Draw
    ClearRect : Rect -> Draw
