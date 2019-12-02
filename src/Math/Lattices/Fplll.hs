{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Math.Lattices.Fplll
  ( LLLMethod()
  , FloatType()
  , RedStatus()
  , LLLFlags()

  , LLLOptions(..)
  , defaultLLL

  , lllReduce

  , lmWrapper
  , lmProved
  , lmHeuristic
  , lmFast

  , ftDefault
  , ftDouble
  , ftLongDouble
  , ftDpe
  , ftDD
  , ftQD
  , ftMpfr

  , lllVerbose
  , lllEarlyRed
  , lllSiegel
  , lllDefault

  , redSuccess
  , redGsoFailure
  , redBabaiFailure
  , redLllFailure
  , redEnumFailure
  , redBkzFailure
  , redBkzTimeLimit
  , redBkzLoopsLimit
  , redHlllFailure
  , redHlllNormFailure
  , redHlllSrFailure
  ) where

import Algebra.Lattice
import Algebra.SemiBoundedLattice hiding (complement)
import qualified Algebra.SemiBoundedLattice as SBL
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Foldable
import Data.Functor
import Data.List
import Data.Maybe
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Numeric.GMP.Raw.Unsafe
import Numeric.GMP.Types
import Numeric.GMP.Utils
import System.IO.Unsafe

newtype LLLMethod = LLLMethod CInt deriving (Eq, Ord, Storable)
newtype FloatType = FloatType CInt deriving (Eq, Ord, Storable)
newtype LLLFlags  = LLLFlags  CInt deriving (Eq, Ord, Storable)
newtype RedStatus = RedStatus CInt deriving (Eq, Ord, Storable)

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

lllReduceTrackFrom :: LLLOptions -> [[Integer]] -> [[Integer]]
                   -> Either RedStatus ([[Integer]], [[Integer]])
lllReduceTrackFrom opts b u =
  unsafePerformIO $
  allocaAndPokeBasis b $ \vecs len bArr ->
    allocaAndPokeBasis u $ \uVecs uLen uArr -> do
      if vecs /= uVecs
        then error "u and b have different numbers of basis vectors"
        else return ()
      status <-
        c_lll_reduction_u
          (fromIntegral vecs)
          (fromIntegral len)
          bArr
          (fromIntegral uLen)
          uArr
          (CDouble $ delta opts)
          (CDouble $ eta opts)
          (method opts)
          (floatType opts)
          (fromIntegral $ precision opts)
          (flags opts)
      if status /= redSuccess
        then return $ Left status
        else Right <$> liftA2 (,) (peekBasis vecs len bArr) (peekBasis vecs uLen uArr)

lllReduceTrackFromInv :: LLLOptions -> [[Integer]] -> [[Integer]]
                      -> Either RedStatus ([[Integer]], [[Integer]], [[Integer]])
lllReduceTrackFromInv opts b u =
  unsafePerformIO $
  allocaAndPokeBasis b $ \vecs len bArr ->
    allocaAndPokeBasis u $ \uVecs uLen uArr ->
      allocaMpz (vecs * vecs) $ \uInvArr -> do
        if vecs /= uVecs
          then error "u and b have different numbers of basis vectors"
          else return ()
        status <-
          c_lll_reduction_uinv
            (fromIntegral vecs)
            (fromIntegral len)
            bArr
            (fromIntegral uLen)
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
                 (peekBasis vecs uLen uArr)
                 (peekBasis vecs vecs uInvArr)


allocaMpz :: Int -> (Ptr MPZ -> IO a) -> IO a
allocaMpz len f =
  allocaArray len $ \p ->
    bracket_
      (traverse_ (\i -> mpz_init (advancePtr p i)) [0 .. len - 1])
      (traverse_ (\i -> mpz_clear (advancePtr p i)) [0 .. len - 1])
      (f p)

peekBasis :: Int -> Int -> Ptr MPZ -> IO [[Integer]]
peekBasis vecs len bArr =
  sequenceA [sequenceA [readVal i j | j <- [0 .. len - 1]] | i <- [0 .. vecs - 1]]
  where
    readVal i j = peekInteger (advancePtr bArr (i * len + j))

pokeBasis :: Int -> Int -> Ptr MPZ -> [[Integer]] -> IO ()
pokeBasis vecs len bArr b =
  sequenceA_ $ concat $ zipWith (\i vec -> zipWith (writeVal i) [0 ..] vec) [0 ..] b
  where
    writeVal i j val = pokeInteger (advancePtr bArr (i * len + j)) val

allocaAndPokeBasis :: [[Integer]] -> (Int -> Int -> Ptr MPZ -> IO a) -> IO a
allocaAndPokeBasis b f
  | allEqual $ length <$> b =
    allocaMpz (vecs * len) $ \bArr -> do
      sequenceA_ $ concat $ zipWith (\i vec -> zipWith (writeVal bArr i) [0 ..] vec) [0 ..] b
      f vecs len bArr
  where
    writeVal bArr i j val = pokeInteger (advancePtr bArr (i * len + j)) val
    vecs = length b
    len = fromMaybe 0 $ length <$> listToMaybe b
allocaAndPokeBasis _ _ = error "Basis vectors aren't all the same length"

allEqual :: (Eq a) => [a] -> Bool
allEqual x = all (== head x) (drop 1 x)


instance Show LLLMethod where
  show (LLLMethod x) = unsafePerformIO $ peekElemOff lllMethodStr (fromIntegral x) >>= peekCString
instance Show FloatType where
  show x | x == ftDefault = "default"
  show (FloatType x) = unsafePerformIO $ peekElemOff floatTypeStr (fromIntegral x) >>= peekCString
instance Show RedStatus where
  show (RedStatus x) = unsafePerformIO $ peekElemOff redStatusStr (fromIntegral x) >>= peekCString
instance Show LLLFlags where
  show x | x == lllDefault = "lllDefault"
  show x = intercalate " \\/ " $ do
    (y, name) <- [(lllVerbose, "lllVerbose"), (lllEarlyRed, "lllEarlyRed"), (lllSiegel, "lllSiegel")]
    guard $ x /\ y /= lllDefault
    return name

instance JoinSemiLattice LLLFlags where
  (LLLFlags x) \/ (LLLFlags y) = LLLFlags $ x .|. y
instance MeetSemiLattice LLLFlags where
  (LLLFlags x) /\ (LLLFlags y) = LLLFlags $ x .&. y
instance Lattice LLLFlags

instance BoundedJoinSemiLattice LLLFlags where
  bottom = lllDefault
instance BoundedMeetSemiLattice LLLFlags where
  top = lllVerbose \/ lllEarlyRed \/ lllSiegel
instance LowerBoundedLattice LLLFlags
instance UpperBoundedLattice LLLFlags
instance BoundedLattice LLLFlags

instance DistributiveLattice LLLFlags
instance LowerBoundedDistributiveLattice LLLFlags
instance UpperBoundedDistributiveLattice LLLFlags

instance SemiHeytingAlgebra LLLFlags where
  x --> y = SBL.complement x \/ y
instance SemiCoHeytingAlgebra LLLFlags where
  (LLLFlags x) \\\ (LLLFlags y) = LLLFlags $ x .&. complement y

instance HeytingAlgebra LLLFlags
instance CoHeytingAlgebra LLLFlags
instance BiHeytingAlgebra LLLFlags
instance BooleanAlgebra LLLFlags

lmWrapper :: LLLMethod
lmWrapper = LLLMethod $ unsafePerformIO $ peek c_lmWrapper
lmProved :: LLLMethod
lmProved = LLLMethod $ unsafePerformIO $ peek c_lmProved
lmHeuristic :: LLLMethod
lmHeuristic = LLLMethod $ unsafePerformIO $ peek c_lmHeuristic
lmFast :: LLLMethod
lmFast = LLLMethod $ unsafePerformIO $ peek c_lmFast

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

lllVerbose :: LLLFlags
lllVerbose = LLLFlags $ unsafePerformIO $ peek c_lllVerbose
lllEarlyRed :: LLLFlags
lllEarlyRed = LLLFlags $ unsafePerformIO $ peek c_lllEarlyRed
lllSiegel :: LLLFlags
lllSiegel = LLLFlags $ unsafePerformIO $ peek c_lllSiegel
lllDefault :: LLLFlags
lllDefault = LLLFlags $ unsafePerformIO $ peek c_lllDefault

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

lllMethodStr :: Ptr CString
lllMethodStr = unsafePerformIO $ peek c_lllMethodStr

floatTypeStr :: Ptr CString
floatTypeStr = unsafePerformIO $ peek c_floatTypeStr

redStatusStr :: Ptr CString
redStatusStr = unsafePerformIO $ peek c_redStatusStr

lllDefaultDelta :: Double
lllDefaultDelta = x
  where (CDouble x) = unsafePerformIO $ peek c_lllDefaultDelta
lllDefaultEta :: Double
lllDefaultEta = x
  where (CDouble x) = unsafePerformIO $ peek c_lllDefaultEta


-- FFI imports

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
foreign import ccall unsafe "hs_ffi_lll_reduction_u"
  c_lll_reduction_u :: CInt -> CInt -> Ptr MPZ -> CInt -> Ptr MPZ -> CDouble -> CDouble -> LLLMethod
                       -> FloatType -> CInt -> LLLFlags -> IO RedStatus
foreign import ccall unsafe "hs_ffi_lll_reduction_uinv"
  c_lll_reduction_uinv :: CInt -> CInt -> Ptr MPZ -> CInt -> Ptr MPZ -> Ptr MPZ -> CDouble -> CDouble
                          -> LLLMethod -> FloatType -> CInt -> LLLFlags -> IO RedStatus
foreign import ccall unsafe "hs_ffi_lll_reduction_u_id"
  c_lll_reduction_u_id :: CInt -> CInt -> Ptr MPZ -> Ptr MPZ -> CDouble -> CDouble -> LLLMethod
                       -> FloatType -> CInt -> LLLFlags -> IO RedStatus
foreign import ccall unsafe "hs_ffi_lll_reduction_uinv_id"
  c_lll_reduction_uinv_id :: CInt -> CInt -> Ptr MPZ -> Ptr MPZ -> Ptr MPZ -> CDouble -> CDouble
                          -> LLLMethod -> FloatType -> CInt -> LLLFlags -> IO RedStatus
