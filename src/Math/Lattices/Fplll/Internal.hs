{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Math.Lattices.Fplll.Internal where

import Control.Exception
import Data.Foldable
import Data.Maybe
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Numeric.GMP.Raw.Unsafe
import Numeric.GMP.Types
import Numeric.GMP.Utils

newtype LLLMethod = LLLMethod CInt deriving (Eq, Ord, Storable)
newtype FloatType = FloatType CInt deriving (Eq, Ord, Storable)
newtype LLLFlags  = LLLFlags  CInt deriving (Eq, Ord, Storable)
newtype RedStatus = RedStatus CInt deriving (Eq, Ord, Storable)


allocaMpz :: Int -> (Ptr MPZ -> IO a) -> IO a
allocaMpz len f =
  allocaArray len $ \p ->
    bracket_
      (traverse_ (mpz_init . advancePtr p) [0 .. len-1])
      (traverse_ (mpz_clear . advancePtr p) [0 .. len-1])
      (f p)

peekBasis :: Int -> Int -> Ptr MPZ -> IO [[Integer]]
peekBasis vecs len bArr =
  sequenceA [sequenceA [readVal i j | j <- [0 .. len-1]] | i <- [0 .. vecs-1]]
  where
    readVal i j = peekInteger (advancePtr bArr (i * len + j))

pokeBasis :: Int -> Int -> Ptr MPZ -> [[Integer]] -> IO ()
pokeBasis vecs len bArr b =
  sequenceA_ $ concat $ zipWith (\i vec -> zipWith (writeVal i) [0 .. len-1] vec) [0 .. vecs-1] b
  where
    writeVal i j = pokeInteger (advancePtr bArr (i * len + j))

allocaAndPokeBasis :: [[Integer]] -> (Int -> Int -> Ptr MPZ -> IO a) -> IO a
allocaAndPokeBasis b f
  | allEqual $ length <$> b =
    allocaMpz (vecs * len) $ \bArr -> do
      sequenceA_ $ concat $ zipWith (\i vec -> zipWith (writeVal bArr i) [0 ..] vec) [0 ..] b
      f vecs len bArr
  where
    writeVal bArr i j = pokeInteger (advancePtr bArr (i * len + j))
    vecs = length b
    len = maybe 0 length (listToMaybe b)
allocaAndPokeBasis _ _ = error "Basis vectors aren't all the same length"

allEqual :: (Eq a) => [a] -> Bool
allEqual x = all (== head x) (drop 1 x)


foreign import ccall "&lllDefaultDelta" c_lllDefaultDelta :: Ptr CDouble
foreign import ccall "&lllDefaultEta" c_lllDefaultEta :: Ptr CDouble

foreign import ccall "&lmWrapper" c_lmWrapper :: Ptr CInt
foreign import ccall "&lmProved" c_lmProved :: Ptr CInt
foreign import ccall "&lmHeuristic" c_lmHeuristic :: Ptr CInt
foreign import ccall "&lmFast" c_lmFast :: Ptr CInt

foreign import ccall "&lllMethodStr" c_lllMethodStr :: Ptr (Ptr CString)

foreign import ccall "&lllVerbose" c_lllVerbose :: Ptr CInt
foreign import ccall "&lllEarlyRed" c_lllEarlyRed :: Ptr CInt
foreign import ccall "&lllSiegel" c_lllSiegel :: Ptr CInt
foreign import ccall "&lllDefault" c_lllDefault :: Ptr CInt

foreign import ccall "&ftDefault" c_ftDefault :: Ptr CInt
foreign import ccall "&ftDouble" c_ftDouble :: Ptr CInt
foreign import ccall "&ftLongDouble" c_ftLongDouble :: Ptr CInt
foreign import ccall "&ftDpe" c_ftDpe :: Ptr CInt
foreign import ccall "&ftDD" c_ftDD :: Ptr CInt
foreign import ccall "&ftQD" c_ftQD :: Ptr CInt
foreign import ccall "&ftMpfr" c_ftMpfr :: Ptr CInt

foreign import ccall "&floatTypeStr" c_floatTypeStr :: Ptr (Ptr CString)

foreign import ccall "&redSuccess" c_redSuccess :: Ptr CInt
foreign import ccall "&redGsoFailure" c_redGsoFailure :: Ptr CInt
foreign import ccall "&redBabaiFailure" c_redBabaiFailure :: Ptr CInt
foreign import ccall "&redLllFailure" c_redLllFailure :: Ptr CInt
foreign import ccall "&redEnumFailure" c_redEnumFailure :: Ptr CInt
foreign import ccall "&redBkzFailure" c_redBkzFailure :: Ptr CInt
foreign import ccall "&redBkzTimeLimit" c_redBkzTimeLimit :: Ptr CInt
foreign import ccall "&redBkzLoopsLimit" c_redBkzLoopsLimit :: Ptr CInt
foreign import ccall "&redHlllFailure" c_redHlllFailure :: Ptr CInt
foreign import ccall "&redHlllNormFailure" c_redHlllNormFailure :: Ptr CInt
foreign import ccall "&redHlllSrFailure" c_redHlllSrFailure :: Ptr CInt

foreign import ccall "&redStatusStr" c_redStatusStr :: Ptr (Ptr CString)

foreign import ccall unsafe "hs_ffi_lll_reduction"
  c_lll_reduction :: CInt -> CInt -> Ptr MPZ -> CDouble -> CDouble -> LLLMethod -> FloatType -> CInt
                     -> LLLFlags -> IO RedStatus
foreign import ccall unsafe "hs_ffi_lll_reduction_u_id"
  c_lll_reduction_u_id :: CInt -> CInt -> Ptr MPZ -> Ptr MPZ -> CDouble -> CDouble -> LLLMethod
                       -> FloatType -> CInt -> LLLFlags -> IO RedStatus
foreign import ccall unsafe "hs_ffi_lll_reduction_uinv_id"
  c_lll_reduction_uinv_id :: CInt -> CInt -> Ptr MPZ -> Ptr MPZ -> Ptr MPZ -> CDouble -> CDouble
                          -> LLLMethod -> FloatType -> CInt -> LLLFlags -> IO RedStatus
