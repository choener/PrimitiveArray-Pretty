
-- | Probability grid square drawing routines.

module Diagrams.TwoD.ProbabilityGrid where

import Data.List (genericLength)
import Data.List.Split (chunksOf)
import Diagrams.Backend.Postscript
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD
import Diagrams.TwoD.Text
import Numeric.Log



-- | Fill weight for our grid. If the fill weight is @logarithmic@, then
-- the line length is @1 / (1 + log value)@ otherwise it is @value@.

data FillWeight = FWlog | FWlinear

-- | A single square in our grid.

-- gridSquare :: FillWeight -> Log Double
gridSquare
  :: (Monoid m, Semigroup m, TrailLike (QDiagram b V2 Double m))
  => FillWeight -> Log Double -> QDiagram b V2 Double m
gridSquare (fw :: FillWeight) (v :: Log Double) = g `beneath` (z # scale s)
  where s = case fw of
              FWlog     -> 1 / (1 - ln v)
              FWlinear  -> exp $ ln v
        z = square 1 # lw 0 # fc blue # centerXY
        g = square 1 # lc black

-- | Draw the actual grid.

grid
  :: ( Renderable (Diagrams.TwoD.Text.Text Double) b
     , Renderable (Path V2 Double) b)
  => FillWeight
  -> t
  -> Int
  -> [String]
  -> [String]
  -> [Log Double]
  -> QDiagram b V2 Double Any
grid (fw :: FillWeight) n m (ns :: [String]) (ms :: [String]) (vs :: [Log Double])
  | null ns && null ms = grd
  | otherwise =  (grd ||| ns') === ms'
  where ns' = if null ns then mempty else vcat $ map (\t -> (square 1) `beneath` (text t # scale (0.9 / genericLength t))) ns
        ms' = if null ms then mempty else hcat $ map (\t -> (square 1) `beneath` (text t # scale (0.9 / genericLength t))) ms
        grd = vcat $ map hcat $ map (map (gridSquare fw)) $ chunksOf m $ vs

-- | Render as @svg@.

svgGridFile :: FilePath -> FillWeight -> Int -> Int -> [String] -> [String] -> [Log Double] -> IO ()
svgGridFile fname fw n m ns ms vs = renderPretty fname size $ g
  where size = ((*100) . fromIntegral) <$> mkSizeSpec2D (Just m) (Just n) -- Nothing Nothing -- n n
        g = grid fw n m ns ms vs

-- | Render as @eps@.

epsGridFile :: String -> FillWeight -> Int -> Int -> [String] -> [String] -> [Log Double] -> IO ()
epsGridFile fname fw n m ns ms vs = renderDia Postscript (PostscriptOptions fname size EPS) g
  where size = ((*100) . fromIntegral) <$> mkSizeSpec2D (Just m) (Just n)
        g = grid fw n m ns ms vs
