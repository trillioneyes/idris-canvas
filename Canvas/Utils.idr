||| Instances for SideEffect
module Utils

record SideEffect' (ffi : FFI) (a : Type) where
  constructor SE'
  un : IO' ffi ()

SideEffect : Type -> Type
SideEffect = SideEffect' FFI_JS

SE : JS_IO () -> SideEffect a
SE = SE'

instance Functor (SideEffect' ffi) where
  map {b} f (SE' act) = SE' {a=b} act

instance Applicative (SideEffect' ffi) where
  pure x = SE' (return ())
  (<*>) (SE' f) (SE' x) = SE' $ x *> return ()
