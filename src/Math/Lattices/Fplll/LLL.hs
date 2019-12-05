module Math.Lattices.Fplll.LLL
  ( lllReduce
  , lllReduceTrack
  , lllReduceTrackInv

  , LLLOptions(..)
  , defaultLLL
  ) where

import Control.Applicative
import Foreign.C.Types
import Foreign.Storable
import Math.Lattices.Fplll.Internal
import Math.Lattices.Fplll.Types
import System.IO.Unsafe

data LLLOptions =
  LLLOptions
    { delta :: Double
    , eta :: Double
    , method :: LLLMethod
    , floatType :: FloatType
    , precision :: Int
    , flags :: LLLFlags
    }
  deriving (Eq, Ord, Show)

defaultLLL =
  LLLOptions
    { delta = lllDefaultDelta
    , eta = lllDefaultEta
    , method = lmWrapper
    , floatType = ftDefault
    , precision = 0
    , flags = lllDefault
    }


lllReduce :: LLLOptions -> [[Integer]] -> Either RedStatus [[Integer]]
lllReduce opts b =
  unsafePerformIO $
  allocaAndPokeBasis b $ \vecs len bArr -> do
    status <-
      c_lll_reduction
        (fromIntegral vecs)
        (fromIntegral len)
        bArr
        (CDouble $ delta opts)
        (CDouble $ eta opts)
        (method opts)
        (floatType opts)
        (fromIntegral $ precision opts)
        (flags opts)
    if status /= redSuccess
      then return $ Left status
      else Right <$> peekBasis vecs len bArr

lllReduceTrack :: LLLOptions -> [[Integer]] -> Either RedStatus ([[Integer]], [[Integer]])
lllReduceTrack opts b =
  unsafePerformIO $
  allocaAndPokeBasis b $ \vecs len bArr ->
    allocaMpz (vecs * vecs) $ \uArr -> do
      status <-
        c_lll_reduction_u_id
          (fromIntegral vecs)
          (fromIntegral len)
          bArr
          uArr
          (CDouble $ delta opts)
          (CDouble $ eta opts)
          (method opts)
          (floatType opts)
          (fromIntegral $ precision opts)
          (flags opts)
      if status /= redSuccess
        then return $ Left status
        else Right <$> liftA2 (,) (peekBasis vecs len bArr) (peekBasis vecs vecs uArr)

lllReduceTrackInv :: LLLOptions -> [[Integer]]
                  -> Either RedStatus ([[Integer]], [[Integer]], [[Integer]])
lllReduceTrackInv opts b =
  unsafePerformIO $
  allocaAndPokeBasis b $ \vecs len bArr ->
    allocaMpz (vecs * vecs) $ \uArr ->
      allocaMpz (vecs * vecs) $ \uInvArr -> do
        status <-
          c_lll_reduction_uinv_id
            (fromIntegral vecs)
            (fromIntegral len)
            bArr
            uArr
            uInvArr
            (CDouble $ delta opts)
            (CDouble $ eta opts)
            (method opts)
            (floatType opts)
            (fromIntegral $ precision opts)
            (flags opts)
        if status /= redSuccess
          then return $ Left status
          else Right <$>
               liftA3
                 (,,)
                 (peekBasis vecs len bArr)
                 (peekBasis vecs vecs uArr)
                 (peekBasis vecs vecs uInvArr)

lllDefaultDelta :: Double
lllDefaultDelta = x
  where (CDouble x) = unsafePerformIO $ peek c_lllDefaultDelta
lllDefaultEta :: Double
lllDefaultEta = x
  where (CDouble x) = unsafePerformIO $ peek c_lllDefaultEta
