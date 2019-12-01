{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Math.Lattices.Fplll where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Numeric.GMP.Types
import System.IO.Unsafe

newtype LLLMethod = LLLMethod CInt deriving (Eq, Ord, Storable)
newtype FloatType = FloatType CInt deriving (Eq, Ord, Storable)
newtype RedStatus = RedStatus CInt deriving (Eq, Ord, Storable)

instance Show LLLMethod where
  show (LLLMethod x) = unsafePerformIO $ peekElemOff lllMethodStr (fromIntegral x) >>= peekCString
instance Show FloatType where
  show (FloatType x) = unsafePerformIO $ peekElemOff floatTypeStr (fromIntegral x) >>= peekCString
instance Show RedStatus where
  show (RedStatus x) = unsafePerformIO $ peekElemOff redStatusStr (fromIntegral x) >>= peekCString

lmWrapper :: LLLMethod
lmWrapper = LLLMethod $ unsafePerformIO $ peek c_lmWrapper
lmProved :: LLLMethod
lmProved = LLLMethod $ unsafePerformIO $ peek c_lmProved
lmHeuristic :: LLLMethod
lmHeuristic = LLLMethod $ unsafePerformIO $ peek c_lmHeuristic
lmFast :: LLLMethod
lmFast = LLLMethod $ unsafePerformIO $ peek c_lmFast

lllMethodStr :: Ptr CString
lllMethodStr = unsafePerformIO $ peek c_lllMethodStr

ftDefault :: FloatType
ftDefault = FloatType $ unsafePerformIO $ peek c_ftDefault
ftDouble :: FloatType
ftDouble = FloatType $ unsafePerformIO $ peek c_ftDouble
ftLongDouble :: FloatType
ftLongDouble = FloatType $ unsafePerformIO $ peek c_ftLongDouble
ftDpe :: FloatType
ftDpe = FloatType $ unsafePerformIO $ peek c_ftDpe
ftDD :: FloatType
ftDD = FloatType $ unsafePerformIO $ peek c_ftDD
ftQD :: FloatType
ftQD = FloatType $ unsafePerformIO $ peek c_ftQD
ftMpfr :: FloatType
ftMpfr = FloatType $ unsafePerformIO $ peek c_ftMpfr

floatTypeStr :: Ptr CString
floatTypeStr = unsafePerformIO $ peek c_floatTypeStr

redSuccess :: RedStatus
redSuccess = RedStatus $ unsafePerformIO $ peek c_redSuccess
redGsoFailure :: RedStatus
redGsoFailure = RedStatus $ unsafePerformIO $ peek c_redGsoFailure
redBabaiFailure :: RedStatus
redBabaiFailure = RedStatus $ unsafePerformIO $ peek c_redBabaiFailure
redLllFailure :: RedStatus
redLllFailure = RedStatus $ unsafePerformIO $ peek c_redLllFailure
redEnumFailure :: RedStatus
redEnumFailure = RedStatus $ unsafePerformIO $ peek c_redEnumFailure
redBkzFailure :: RedStatus
redBkzFailure = RedStatus $ unsafePerformIO $ peek c_redBkzFailure
redBkzTimeLimit :: RedStatus
redBkzTimeLimit = RedStatus $ unsafePerformIO $ peek c_redBkzTimeLimit
redBkzLoopsLimit :: RedStatus
redBkzLoopsLimit = RedStatus $ unsafePerformIO $ peek c_redBkzLoopsLimit
redHlllFailure :: RedStatus
redHlllFailure = RedStatus $ unsafePerformIO $ peek c_redHlllFailure
redHlllNormFailure :: RedStatus
redHlllNormFailure = RedStatus $ unsafePerformIO $ peek c_redHlllNormFailure
redHlllSrFailure :: RedStatus
redHlllSrFailure = RedStatus $ unsafePerformIO $ peek c_redHlllSrFailure

redStatusStr :: Ptr CString
redStatusStr = unsafePerformIO $ peek c_redStatusStr

foreign import ccall "&lmWrapper" c_lmWrapper :: Ptr CInt
foreign import ccall "&lmProved" c_lmProved :: Ptr CInt
foreign import ccall "&lmHeuristic" c_lmHeuristic :: Ptr CInt
foreign import ccall "&lmFast" c_lmFast :: Ptr CInt

foreign import ccall "&lllMethodStr" c_lllMethodStr :: Ptr (Ptr CString)

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
                     -> CInt -> IO RedStatus
foreign import ccall unsafe "hs_ffi_lll_reduction"
  c_lll_reduction_u :: CInt -> CInt -> Ptr MPZ -> CInt -> Ptr MPZ -> CDouble -> CDouble -> LLLMethod
                       -> FloatType -> CInt -> CInt -> IO RedStatus
foreign import ccall unsafe "hs_ffi_lll_reduction"
  c_lll_reduction_uinv :: CInt -> CInt -> Ptr MPZ -> CInt -> Ptr MPZ -> Ptr MPZ -> CDouble -> CDouble
                          -> LLLMethod -> FloatType -> CInt -> CInt -> IO RedStatus
