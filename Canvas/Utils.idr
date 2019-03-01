||| Instances for SideEffect
module Utils
%access public export

record SideEffect' (ffi : FFI) (a : Type) where
  constructor SE'
  un : IO' ffi ()

SideEffect : Type -> Type
SideEffect = SideEffect' FFI_JS

SE : JS_IO () -> SideEffect a
SE = SE'

Functor (SideEffect' ffi) where
  map {b} f (SE' act) = SE' {a=b} act

Applicative (SideEffect' ffi) where
  pure x = SE' (pure ())
  (<*>) (SE' f) (SE' x) = SE' $ x *> pure ()
