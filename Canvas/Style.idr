module Canvas.Style
import Data.List

%language TypeProviders

namespace Rep
  export
  data ColorRep = RGBA Nat Nat Nat Double

hexDigit : Char -> Maybe Nat
hexDigit c =
  if c >= '0' && c <= '9'
    then Just (toNat $ ord c - ord '0')
    else if c >= 'a' && c <= 'f'
            then Just $ toNat $ 10 + ord c - ord 'a'
            else if c >= 'A' && c <= 'F'
                    then Just $ toNat $ 10 + ord c - ord 'A'
                    else Nothing

hex : Char -> Char -> Maybe Nat
hex x y = [| (map (*16) (hexDigit x)) + hexDigit y |]

parseVal : List Char -> Maybe ColorRep
parseVal [',', '#', r1, r2, g1, g2, b1, b2] =
  [| RGBA (hex r1 r2) (hex g1 g2) (hex b1 b2) (pure 1) |]
parseVal _ = Nothing

parseColor : List Char -> Maybe (String, ColorRep)
parseColor line with (break (==',') line)
  | (name, val) = map (\rep => (pack name, rep)) (parseVal val)

parseColors : String -> Maybe (List (String, ColorRep))
parseColors = traverse parseColor . lines' . unpack

readColors : String -> IO (Provider (List (String, ColorRep)))
readColors path = do
  Right csvData <- readFile path
        | Left fileError => pure (Error (show fileError))
  Just colorReps <- pure $ parseColors csvData
       | Nothing => pure (Error "malformed colors.csv")
  pure (Provide colorReps)
  -- map (maybe (Error "malformed colors.csv") Provide . parseColors) (readFile path)

%provide (rawColors : List (String, ColorRep)) with (readColors "Canvas/colors.csv")

public export
CSSColors : List String
CSSColors = [colorName | (colorName, _) <- rawColors]

elemMap : Elem x xs -> Elem (f x) (map f xs)
elemMap Here = Here
elemMap (There x) = There (elemMap x)

mapElem : Elem a (map f xs) -> (x ** Elem x xs)
mapElem {xs = []} prf = absurd prf
mapElem {xs = (y :: xs)} Here = MkDPair y Here
mapElem {xs = (y :: xs)} (There prf) with (mapElem prf)
  mapElem {xs = (y :: xs)} (There prf) | (MkDPair x pf) = (x ** There pf)

CSSRaw : {name : String} -> Elem name CSSColors -> ColorRep
CSSRaw prf with (mapElem {f=fst} {xs=rawColors} prf)
  | ((n, v) ** prf') = v

nameColor : (name : String) -> Elem name CSSColors -> ColorRep
nameColor name prf = CSSRaw prf

public export
data Color : Type where
  RGB : Nat -> Nat -> Nat -> Color
  RGBA : Nat -> Nat -> Nat -> Double -> Color
  --HSL : Nat -> Nat -> Nat -> Color
  --HSLA : Nat -> Nat -> Nat -> Double -> Color
  Named : (name : String) -> {auto prf : Elem name CSSColors} -> Color

-- hslToRGB : (Nat, Nat, Nat) -> (Nat, Nat, Nat)
-- hslToRGB (h, s, l) =
--   let h' = mod h 360
--       c = (100 - abs (2 * l - 100)) * s
--       x = c * (100 - abs ((h' `div` 60) `mod` 2))
--       m = l - (c `div` 2)

--   in (r + m, g + m, b + m)

rep : Color -> ColorRep
rep (RGB r g b) = RGBA r g b 255
rep (RGBA r g b a) = RGBA r g b a
--rep (HSL h s l) = ?hslToRGBA
--rep (HSLA h s l a) = ?hslaToRGBA
rep (Named n {prf}) = nameColor n prf

str : List String -> String
str xs = foldr (++) "" xs

call : String -> List String -> String
call fn args = str ([fn, "("] ++ intersperse "," args ++ [")"])

toCSS : Color -> String
toCSS (RGB r g b) = call "rgb" [show r, show g, show b]
toCSS (RGBA r g b a) = call "rgba" [show r, show g, show b, show a]
-- toCSS (HSL h s l) = call "hsl" [show h, show s ++ "%", show l ++ "%"]
-- toCSS (HSLA h s l a) = call "hsla" [show h, show s ++ "%", show l ++ "%", show a]
toCSS (Named name) = name

public export
data ColorStyle = Solid Color -- | Grad Gradient | Pat Pattern

export
colorProperty : ColorStyle -> String
colorProperty (Solid c) = toCSS c

public export
record Style where
  constructor S
  fill : ColorStyle
  stroke : ColorStyle
  -- text : TextStyle
  -- line : LineStyle
  -- shadow : ShadowStyle
