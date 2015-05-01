module Canvas.Style
import Data.List

namespace Rep
  data ColorRep = RGBA Nat Nat Nat Float

CSSColors : List String
CSSColors = ["blue"]

data Color : Type where
  RGB : Nat -> Nat -> Nat -> Color
  RGBA : Nat -> Nat -> Nat -> Float -> Color
  HSL : Nat -> Nat -> Nat -> Color
  HSLA : Nat -> Nat -> Nat -> Float -> Color
  Named : (name : String) -> {auto prf : Elem name CSSColors} -> Color

rep : Color -> ColorRep
rep (RGB r g b) = RGBA r g b 255
rep (RGBA r g b a) = RGBA r g b a
rep (HSL h s l) = ?hslToRGBA
rep (HSLA h s l a) = ?hslaToRGBA
rep (Named name) = ?nameToRGBA

str : List String -> String
str xs = foldr (++) "" xs

call : String -> List String -> String
call fn args = str ([fn, "("] ++ intersperse "," args ++ [")"])

toCSS : Color -> String
toCSS (RGB r g b) = call "rgb" [show r, show g, show b]
toCSS (RGBA r g b a) = call "rgba" [show r, show g, show b, show a]
toCSS (HSL h s l) = call "hsl" [show h, show s ++ "%", show l ++ "%"]
toCSS (HSLA h s l a) = call "hsla" [show h, show s ++ "%", show l ++ "%", show a]
toCSS (Named name) = name
