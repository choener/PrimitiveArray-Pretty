
-- | Probability grid square drawing routines.

module Diagrams.TwoD.ProbabilityGrid where

import           Data.Data
import           Data.List (genericLength)
import           Data.List.Split (chunksOf)
import           Data.Typeable
import           Debug.Trace
import           Diagrams.Backend.Postscript hiding (EPS)
import           Diagrams.Backend.SVG hiding (SVG)
import           Diagrams.Prelude
import           Diagrams.TwoD
import           Diagrams.TwoD.Text
import           Numeric.Log
import qualified Diagrams.Backend.Postscript as DBP
import qualified Diagrams.Backend.SVG as DBS
import           System.FilePath (replaceExtension)



-- | Fill weight for our grid. If the fill weight is @logarithmic@, then
-- the line length is @1 / (1 + log value)@ otherwise it is @value@.

data FillWeight = FWlog | FWlinear | FWfill
  deriving (Eq,Show,Data,Typeable)

data FillStyle = FSopacityLog | FSopacityLinear | FSfull
  deriving (Eq,Show,Data,Typeable)

-- | A single square in our grid.

-- gridSquare :: FillWeight -> Log Double
gridSquare
  :: (Monoid m, Semigroup m, TrailLike (QDiagram b V2 Double m))
  => FillWeight -> FillStyle -> Log Double -> QDiagram b V2 Double m
gridSquare fw fs v
  | s >= 0.001 = g `beneath` (z # scale s)
  | otherwise  = g
  where s = case fw of
              FWlog     -> 1 / (1 - ln v)
              FWlinear  -> exp $ ln v
              FWfill    -> 1
        o = case fs of
              FSopacityLog    -> 1 / (1 - ln v)
              FSopacityLinear -> exp $ ln v
              FSfull          -> 1.0 :: Double
        z = square 1 # lw 0 # ((if fs==FSfull then fc else fcA . flip withOpacity o) blue) # centerXY
        g = square 1 # lc black

-- | Draw the actual grid.

grid
  :: ( Renderable (Diagrams.TwoD.Text.Text Double) b
     , Renderable (Path V2 Double) b)
  => FillWeight
  -> FillStyle
  -> t
  -> Int
  -> [String]
  -> [String]
  -> [Log Double]
  -> QDiagram b V2 Double Any
grid fw fs n m (ns :: [String]) (ms :: [String]) (vs :: [Log Double])
  | null ns && null ms = grd
  | otherwise =  (grd ||| ns') === ms'
  where ns' = if null ns then mempty else vcat $ map (\t -> (square 1) `beneath` (text t # scale (0.9 / genericLength t))) ns
        ms' = if null ms then mempty else hcat $ map (\t -> (square 1) `beneath` (text t # scale (0.9 / genericLength t))) ms
        grd = vcat $ map hcat $ map (map (gridSquare fw fs)) $ chunksOf m $ vs

-- | Render as @svg@.

svgGridFile :: FilePath -> FillWeight -> FillStyle -> Int -> Int -> [String] -> [String] -> [Log Double] -> IO ()
svgGridFile fname fw fs n m ns ms vs = renderPretty fname size $ g
  where size = ((*100) . fromIntegral) <$> mkSizeSpec2D (Just m) (Just n) -- Nothing Nothing -- n n
        g = grid fw fs n m ns ms vs

-- | Render as @eps@.

epsGridFile :: String -> FillWeight -> FillStyle -> Int -> Int -> [String] -> [String] -> [Log Double] -> IO ()
epsGridFile fname fw fs n m ns ms vs = renderDia Postscript (PostscriptOptions fname size DBP.EPS) g
  where size = ((*100) . fromIntegral) <$> mkSizeSpec2D (Just m) (Just n)
        g = grid fw fs n m ns ms vs

data RenderChoice
  = SVG
  | EPS
  deriving (Eq,Show,Data,Typeable)

-- | Choose a renderer with appropriate file name suffix

gridFile :: [RenderChoice] -> String -> FillWeight -> FillStyle -> Int -> Int -> [String] -> [String] -> [Log Double] -> IO ()
gridFile cs fname fw fs n m ns ms vs = go cs
  where go [] = return ()
        go (c:cs) = case c of
          SVG -> svgGridFile (fname ++ ".svg") fw fs n m ns ms vs >> go cs
          EPS -> epsGridFile (fname ++ ".eps") fw fs n m ns ms vs >> go cs

