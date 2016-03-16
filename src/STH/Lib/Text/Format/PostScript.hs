module STH.Lib.Text.Format.PostScript (
  unicodeToPS
) where

import Data.List (unfoldr)


unicodeToPS :: String -> String
unicodeToPS = renderPSText . toPSText


data PSChar
  = Plain String
  | Glyph String
  deriving (Show)

type PSText = [PSChar]


renderPSText :: PSText -> String
renderPSText = concatMap renderPSChar
  where
    renderPSChar :: PSChar -> String
    renderPSChar (Plain s)
      = "(" ++ s ++ ") show\n"
    renderPSChar (Glyph g)
      = "/" ++ g ++ " glyphshow\n"


toPSText :: String -> PSText
toPSText = condense . map toPSChar
  where
    condense :: PSText -> PSText
    condense = unfoldr first

    first :: PSText -> Maybe (PSChar, PSText)
    first [] = Nothing
    first (x:xs) = case x of
      Glyph g -> Just (Glyph g, xs)
      Plain c -> do
        let (ps,rest) = span isPlain xs
        cs <- fmap concat $ sequence $ map fromPlain ps
        Just (Plain (c++cs), rest)

    isPlain :: PSChar -> Bool
    isPlain (Plain _) = True
    isPlain (Glyph _) = False

    fromPlain :: PSChar -> Maybe String
    fromPlain (Plain s) = Just s
    fromPlain (Glyph _) = Nothing


-- hold on to your butts
toPSChar :: Char -> PSChar
toPSChar x = case x of
  '\\'     -> Plain "\\\\"

  -- Greek Letters: Uppercase
  '\x0391' -> Glyph "Alpha"
  '\x0392' -> Glyph "Beta"
  '\x0393' -> Glyph "Gamma"
  '\x0394' -> Glyph "Deltagreek"
  '\x0395' -> Glyph "Epsilon"
  '\x0396' -> Glyph "Zeta"
  '\x0397' -> Glyph "Eta"
  '\x0398' -> Glyph "Theta"
  '\x0399' -> Glyph "Iota"
  '\x039a' -> Glyph "Kappa"
  '\x039b' -> Glyph "Lambda"
  '\x039c' -> Glyph "Mu"
  '\x039d' -> Glyph "Nu"
  '\x039e' -> Glyph "Xi"
  '\x039f' -> Glyph "Omicron"
  '\x03a0' -> Glyph "Pi"
  '\x03a1' -> Glyph "Rho"
  '\x03a3' -> Glyph "Sigma"
  '\x03a4' -> Glyph "Tau"
  '\x03a5' -> Glyph "Upsilon"
  '\x03a6' -> Glyph "Phi"
  '\x03a7' -> Glyph "Chi"
  '\x03a8' -> Glyph "Psi"
  '\x03a9' -> Glyph "Omegagreek"

  -- Greek letters: Lowercase
  '\x03b1' -> Glyph "alpha"
  '\x03b2' -> Glyph "beta"
  '\x03b3' -> Glyph "gamma"
  '\x03b4' -> Glyph "delta"
  '\x03b5' -> Glyph "epsilon"
  '\x03b6' -> Glyph "zeta"
  '\x03b7' -> Glyph "eta"
  '\x03b8' -> Glyph "theta"
  '\x03b9' -> Glyph "iota"
  '\x03ba' -> Glyph "kappa"
  '\x03bb' -> Glyph "lambda"
  '\x03bc' -> Glyph "mugreek"
  '\x03bd' -> Glyph "nu"
  '\x03be' -> Glyph "xi"
  '\x03bf' -> Glyph "omicron"
  '\x03c0' -> Glyph "pi"
  '\x03c1' -> Glyph "rho"
  '\x03c2' -> Glyph "sigmafinal"
  '\x03c3' -> Glyph "sigma"
  '\x03c4' -> Glyph "tau"
  '\x03c5' -> Glyph "upsilon"
  '\x03c6' -> Glyph "phi"
  '\x03c7' -> Glyph "chi"
  '\x03c8' -> Glyph "psi"
  '\x03c9' -> Glyph "omega"

  -- Otherwise
  _        -> Plain [x]
