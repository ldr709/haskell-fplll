{-|
  Bindings to FPLLL's implementation of the LLL Algorithm.
-}

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

-- | Options to configure the LLL reduction algorithm.
data LLLOptions =
  LLLOptions
    { delta :: Double         -- ^ δ controls the Lovász condition, i.e. how much the length of consecutive Gram-Schmidt orthogonal basis vectors can decrease.
    , eta :: Double           -- ^ η is an upper bound on the largest Gram-Schmidt coefficient, i.e. how far from orthogonal the reduced basis can be.
    , method :: LLLMethod     -- ^ LLL reduction method.
    , floatType :: FloatType  -- ^ What sort of floating point to use for orthogonalization.
    , precision :: Int        -- ^ Bits of precision for floating point if ftMpfr is used. Chooses automatically if set to zero.
    , flags :: LLLFlags       -- ^ Extra options for the LLL reduction.
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

-- | Compute an LLL-reduced basis for the given lattice. Each item of the list is a basis vector.
-- Returns a @Left 'RedStatus'@ on failure.
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

-- | Similar to 'lllReduce', but additionally return (in the second output) the operations that were
-- applied to the basis vectors. In other words, the second return value tracks the operations
-- applied to the basis vectors by applying them to the identity matrix as well.
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

-- | Like 'lllReduceTrackInv', but return the inverse matrix of the applied operations in the third
-- output.
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
