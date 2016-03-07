
-- | Sensible prettyfication of Inside/Outside combinations without having
-- to invoke a third grammar (that would be the same for many problems
-- anyway).

module Data.PrimitiveArray.Pretty.InOut where

--import Graphics.Rendering.Chart.Easy
--import Graphics.Rendering.Chart.Backend.Cairo
import Numeric.Log
import Diagrams.TwoD
import Diagrams.Prelude
import Data.List.Split (chunksOf)
import Diagrams.Backend.SVG
import Diagrams.Backend.Postscript
import Data.List (genericLength)



--toPlot :: (Default r, ToRenderable r) => [(Int,Int,Log Double)] -> EC r ()
--toPlot xs = do
--  layout_title .= "Match Probabilities"
--  plot $ liftEC $ do
--    undefined



-- | Fill weight for our grid. If the fill weight is @logarithmic@, then
-- the line length is @1 / (1 + log value)@ otherwise it is @value@.

data FillWeight = FWlog | FWlinear

-- | A single square in our grid.

-- gridSquare :: FillWeight -> Log Double
gridSquare (fw :: FillWeight) (v :: Log Double) = g `beneath` (z # scale s)
  where s = case fw of
              FWlog     -> 1 / (1 - ln v)
              FWlinear  -> exp $ ln v
        z = square 1 # lw 0 # fc blue # centerXY
        g = square 1 # lc black

grid (fw :: FillWeight) n m (ns :: [String]) (ms :: [String]) (vs :: [Log Double])
--  | null ns   = vcat $ map hcat $ map (map (gridSquare fw)) $ chunksOf n $ vs
--  | length ns /= n = error "grid: annotation length not equal to grid size"
  | null ns && null ms = grd
  | otherwise =  (grd ||| ns') === ms'
  where ns' = if null ns then mempty else vcat $ map (\t -> (square 1) `beneath` (text t # scale (0.9 / genericLength t))) ns
        ms' = if null ms then mempty else hcat $ map (\t -> (square 1) `beneath` (text t # scale (0.9 / genericLength t))) ms
        grd = vcat $ map hcat $ map (map (gridSquare fw)) $ chunksOf m $ vs

svgGridFile fname fw n m ns ms vs = renderPretty fname size $ g
  where size = ((*100) . fromIntegral) <$> mkSizeSpec2D (Just m) (Just n) -- Nothing Nothing -- n n
        g = grid fw n m ns ms vs

epsGridFile fname fw n m ns ms vs = renderDia Postscript (PostscriptOptions fname size EPS) g
  where size = ((*100) . fromIntegral) <$> mkSizeSpec2D (Just m) (Just n)
        g = grid fw n m ns ms vs

