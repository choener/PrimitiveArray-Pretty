
-- | Probability grid square drawing routines.

module Diagrams.TwoD.ProbabilityGrid
  ( module Diagrams.TwoD.ProbabilityGrid
  , blue, red, green, yellow, cyan, magenta, black
  ) where

import           Data.Data
import           Data.List (genericLength,sortBy)
import           Data.List.Split (chunksOf)
import           Data.Map.Strict (Map)
import           Data.Ord (comparing, Down(..))
import           Data.Typeable
import           Debug.Trace
import           Diagrams.Backend.Postscript hiding (EPS)
import           Diagrams.Backend.SVG hiding (SVG)
import           Diagrams.Prelude
import           Diagrams.TwoD
import           Diagrams.TwoD.Text
import           Numeric.Log
import qualified Data.Map.Strict as M
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
  => FillWeight -> FillStyle -> [(Colour Double, Log Double)] -> QDiagram b V2 Double m
gridSquare fw fs cv = foldl beneath g ds
  where
    g = strut unitX `beneath` strut unitY -- mempty -- square 1 # lc black # lw 0.01
    xs = sortBy (comparing (Down . snd)) [ (c,v) | (c,v) ← cv, v >= 0.001 ]
    ds = take 1 $ map gen xs
    gen (c,v) =
      let s = case fw of
                FWlog     -> 1 / (1 - ln v)
                FWlinear  -> exp $ ln v
                FWfill    -> 1
          o = case fs of
                FSopacityLog    -> 1 / (1 - ln v)
                FSopacityLinear -> exp $ ln v
                FSfull          -> 1.0 :: Double
      in  square 1 # lw 0 # ((if fs==FSfull then fc else fcA . flip withOpacity o) c) # centerXY # scale s

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
        grd = vcat $ map hcat $ map (map (gridSquare fw fs . (:[]) . (blue,) )) $ chunksOf m $ vs

gridNew
  ∷ ( Renderable (Diagrams.TwoD.Text.Text Double) b
     , Renderable (Path V2 Double) b)
  ⇒ FillWeight
  → FillStyle
  → (Int,Int)
  -- ^ row min max
  → (Int,Int)
  -- ^ col min max
  → (Int → String)
  -- ^ render row names
  → (Int → String)
  -- ^ render column names
  → [(Colour Double, Map (Int,Int) (Log Double))]
  -- ^ set of values to render
  → QDiagram b V2 Double Any
gridNew fw fs (minRow,maxRow) (minCol,maxCol) renderRow renderCol xs = vcat $ colNames : rows
  where rows        = [ genRow i | i ← [minRow .. maxRow] ]
        colNames    = hcat [ name (renderCol j) j | j ← [minCol .. maxCol]]
        genRow i    = hcat [ genCell i j | j ← [minCol .. maxCol ] ] ||| name (renderRow i) i
        genCell i j = gridSquare fw fs [ (c,v) | (c,x) ← xs, Just v ← [x M.!? (i,j)] ]
        name t i    = (square 1 # lw 0.01) `beneath` (text t # scale (0.9 / genericLength t))
        name t j    = (square 1 # lw 0.01) `beneath` (text t # scale (0.9 / genericLength t))


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

epsNewGridFile, svgNewGridFile
  ∷ FilePath
  → FillWeight
  → FillStyle
  → (Int,Int)
  -- ^ row min max
  → (Int,Int)
  -- ^ col min max
  → (Int → String)
  → (Int → String)
  → [(Colour Double, Map (Int,Int) (Log Double))]
  → IO ()
epsNewGridFile fname fw fs r@(minRow,maxRow) c@(minCol,maxCol) renderRow renderCol values = renderDia Postscript (PostscriptOptions fname size DBP.EPS) g
  where size = ((*100) . fromIntegral) <$> mkSizeSpec2D (Just $ maxCol - minCol) (Just $ maxRow - minRow)
        g = gridNew fw fs r c renderRow renderCol values

svgNewGridFile fname fw fs r@(minRow,maxRow) c@(minCol,maxCol) renderRow renderCol values = renderPretty fname size g
  where size = ((*100) . fromIntegral) <$> mkSizeSpec2D (Just $ maxCol - minCol) (Just $ maxRow - minRow)
        g = gridNew fw fs r c renderRow renderCol values

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

