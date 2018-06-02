{-# OPTIONS_GHC -Wall -Wextra -Wcompat -Wincomplete-uni-patterns -Wincomplete-record-updates #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hyperbolic
  ( compareAsinh
  , compareAtanh
  , compareLog1p
  , asinhSamples
  , atanhSamples
  , log1pSamples
  , logErrorAsDoubles
  , ulpErrorAsDoubles
  , Error(..)
  ) where

import Foreign.C.Types ( CDouble(..) )
import Data.Bits ( Bits(shiftR) )
import Data.Coerce ( coerce )
import Data.Int ( Int64 )
import Data.List ( sort, nub )
import Unsafe.Coerce ( unsafeCoerce )

import Data.Number.MPFR ( MPFR )
import Data.Number.MPFR.Instances.Near ()
import qualified Data.Number.MPFR as MPFR ( asinh, atanh, log10, log1p, add
                                          , RoundMode(Near)
                                          , Precision
                                          , fromDouble, toDouble
                                          )

{- Proposed function implementations -}

-- This comes from the inimitable Mark Adler:
-- https://mathematica.stackexchange.com/a/39133
mpLog1p :: (Floating a, Ord a) => a -> a
mpLog1p x
  | x < 0.5 = go
  | otherwise = log (1 + x)
  where
    w = 1 + x
    go =
      if w - 1 == 0
      then x
      else x * log w / (w - 1)
{-# INLINEABLE mpLog1p #-}
{-# SPECIALIZE mpLog1p :: Float -> Float #-}
{-# SPECIALIZE mpLog1p :: Double -> Double #-}

-- This uses approximately the same algorithm as GNU libm:
-- https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/ieee754/dbl-64/s_asinh.c;hb=HEAD
mpAsinh :: (Show a, RealFloat a) => a -> a
mpAsinh x
  | xa < verySmall
    -- This first case should include forcing an underflow exception,
    -- see the macro `math_check_force_underflow()`.
    --
    -- Do we really want to do the 'huge' comparison here?  The C code
    -- is written as if this check can never fail.  Perhaps because
    -- the only way for it to fail is for it to trigger `SIGFPE`?
    && huge + x > 1  = x
  | xa > veryLarge =
      if isNaN x || isInfinite x
      then x + x
      else handleW $ log xa + log 2
  | otherwise = handleW $
    if veryLarge >= xa && xa > 2.0
    then log $ 2 * xa + 1.0 / (sqrt (xa2 + 1.0) + xa)
    else  mpLog1p $ xa + xa2 / (1.0 + sqrt (1.0 + xa2))
  where
    xa = abs x
    xa2 = xa * xa
    verySmall = 2 ** (-28)
    veryLarge = 2 ** 28
    huge = 1e300

    handleW w = if signum w == signum x then w else negate w
{-# INLINEABLE mpAsinh #-}
{-# SPECIALIZE mpAsinh :: Float -> Float #-}
{-# SPECIALIZE mpAsinh :: Double -> Double #-}

-- This uses approximately the same algorithm as GNU libm:
-- https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/ieee754/dbl-64/e_atanh.c;hb=HEAD
mpAtanh :: (Show a, RealFloat a) => a -> a
mpAtanh x
  | xa < 0.5
  , t <- xa + xa =
      if xa < verySmall
      then x
      else handleT $ 0.5 * mpLog1p (t + t * xa / (1.0 - xa))
  | xa < 1.0 = handleT $ 0.5 * mpLog1p ((xa + xa) / (1.0 - xa))
  -- Domain errors
  | xa > 1.0 = (x - x) / (x - x)
  | otherwise = x / 0.0
  where
    xa = abs x
    verySmall = 2 ** (-28)
    handleT t = if signum t == signum x then t else negate t
{-# INLINEABLE mpAtanh #-}
{-# SPECIALIZE mpAtanh :: Float -> Float #-}
{-# SPECIALIZE mpAtanh :: Double -> Double #-}

{- GNU libm implementations for comparison -}

foreign import ccall unsafe "math.h asinh" libm_asinh :: CDouble -> CDouble
foreign import ccall unsafe "math.h atanh" libm_atanh :: CDouble -> CDouble
foreign import ccall unsafe "math.h log1p" libm_log1p :: CDouble -> CDouble

{- MPFR configuration -}

mpfrPrecision :: MPFR.Precision
mpfrPrecision = 1000

{- Assessing errors -}

data Error = Error
  { inputValue :: Double
  , mpfrResult :: MPFR
  , errorGhc :: MPFR
  , errorCandidate :: MPFR
  , errorLibm :: MPFR
  } deriving (Show)

ulpCompare :: Double -> Double -> Maybe Int64
ulpCompare x' y'
  | signBit x /= signBit y = Nothing
  | otherwise              = Just . abs $ y - x
  where
    x = unsafeCoerce x' :: Int64
    y = unsafeCoerce y' :: Int64
    signBit = (`shiftR` 63)

ulpErrorAsDoubles :: Error -> (Double, Double, Double, Double)
ulpErrorAsDoubles (Error x res eh emp ec) = (logBase 10 $ abs x
                                            , cmp eh
                                            , cmp emp
                                            , cmp ec
                                            )
  where
    cmp = logBase 10 . cmp' . abs . trunc . mpfrSum res
    cmp' a = case ulpCompare (abs $ trunc res) a of
      Nothing -> error "should only be called on the abs of numbers!"
      Just v  -> fromIntegral v

    mpfrSum = MPFR.add MPFR.Near (2 * mpfrPrecision)
    trunc = MPFR.toDouble MPFR.Near

logErrorAsDoubles :: Error -> (Double, Double, Double, Double)
logErrorAsDoubles (Error x _ eh emp ec) = ( logBase 10 $ abs x
                                          , trunc $ mpfrLog eh
                                          , trunc $ mpfrLog ec
                                          , trunc $ mpfrLog emp
                                          )
  where
    mpfrLog = MPFR.log10 MPFR.Near mpfrPrecision . abs
    trunc = MPFR.toDouble MPFR.Near

doubleCompare :: (CDouble -> CDouble)
              -> (Double -> Double)
              -> (Double -> Double)
              -> (MPFR.RoundMode -> MPFR.Precision -> MPFR -> MPFR)
              -> Double
              -> Error
doubleCompare cf' hf mpf mf' x = Error x my (hy - my) (mpy - my) (cy - my)
  where
    cf :: Double -> Double
    cf = coerce . cf' . coerce

    mf = mf' MPFR.Near mpfrPrecision

    toMPFR = MPFR.fromDouble MPFR.Near mpfrPrecision

    cy = toMPFR $ cf x
    hy = toMPFR $ hf x
    mpy = toMPFR $ mpf x
    my = mf $ toMPFR x

samples :: [Int] -> [Int] -> [Double]
samples bases exps = sort . nub $ expStr <$> bases <*> exps
  where
    expStr base ex = fromIntegral base * 10 ** fromIntegral ex

{- Configure the comparisons -}

compareAsinh :: Double -> Error
compareAsinh = doubleCompare libm_asinh asinh mpAsinh MPFR.asinh
compareAtanh :: Double -> Error
compareAtanh = doubleCompare libm_atanh atanh mpAtanh MPFR.atanh
compareLog1p :: Double -> Error
compareLog1p = doubleCompare libm_log1p (log . (1+)) mpLog1p MPFR.log1p

asinhSamples :: [Double]
asinhSamples = samples [-9..0] [-15..15]
atanhSamples :: [Double]
atanhSamples = samples [0..9] [-15 .. -1]
log1pSamples :: [Double]
log1pSamples = samples [0..9] [-15..15]
