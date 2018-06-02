module Main where

import Data.List ( unzip4 )
import Data.Semigroup ( (<>) )

import Graphics.Rendering.Chart.Easy ( layout_title, def
                                     , setColors, opaque, blue, red, green
                                     , points, plot, (.=)
                                     )
import Graphics.Rendering.Chart.Backend.Cairo ( toFile, FileOptions(_fo_size) )

import Hyperbolic

{- Generate plots -}

makeErrorPlots :: IO ()
makeErrorPlots = do
  plotErrors "asinh" compareAsinh asinhSamples
  plotErrors "atanh" compareAtanh atanhSamples
  plotErrors "log1p" compareLog1p log1pSamples
  plotUlpErrors "asinh" compareAsinh asinhSamples
  plotUlpErrors "atanh" compareAtanh atanhSamples
  plotUlpErrors "log1p" compareLog1p log1pSamples

plotErrors :: String -> (Double -> Error) -> [Double] -> IO ()
plotErrors funName f numbers = toFile opts (funName <> ".png") $ do
  layout_title .=
    "Log " <> funName <> " error vs. log argument (vs. MPFR with 1000-bit mantissa)"
  setColors colors
  plot (points "GHC.Float" (zip inputs errGhc))
  plot (points "glibc libm" (zip inputs errLibm))
  plot (points "MP candidate" (zip inputs errMP))
  where
    opts = def { _fo_size = (1920, 1080) }
    colors = [opaque blue, opaque red, opaque green]
    (inputs, errGhc, errLibm, errMP) =
      unzip4 $ fmap (logErrorAsDoubles . f) numbers

plotUlpErrors :: String -> (Double -> Error) -> [Double] -> IO ()
plotUlpErrors funName f numbers = toFile opts (funName <> "_ulp.png") $ do
  layout_title .=
    funName <> " error (log ULP) vs. log argument (vs. MPFR with 1000-bit mantissa)"
  setColors colors
  plot (points "GHC.Float" (zip inputs errGhc))
  plot (points "glibc libm" (zip inputs errLibm))
  plot (points "MP candidate" (zip inputs errMP))
  where
    opts = def { _fo_size = (1920, 1080) }
    colors = [opaque blue, opaque red, opaque green]
    (inputs, errGhc, errLibm, errMP) =
      unzip4 $ fmap (ulpErrorAsDoubles . f) numbers

main :: IO ()
main = makeErrorPlots
