module Math.Lattices.Fplll.Types
  ( LLLMethod()
  , lmWrapper
  , lmProved
  , lmHeuristic
  , lmFast

  , FloatType()
  , ftDefault
  , ftDouble
  , ftLongDouble
  , ftDpe
  , ftDD
  , ftQD
  , ftMpfr

  , LLLFlags()
  , lllVerbose
  , lllEarlyRed
  , lllSiegel
  , lllDefault

  , RedStatus()
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
import Control.Monad
import Data.Bits
import Data.List
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Math.Lattices.Fplll.Internal
import System.IO.Unsafe

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
