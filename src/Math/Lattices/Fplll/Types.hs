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
  , lllDefault
  , lllVerbose
  , lllEarlyRed
  , lllSiegel

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

-- | Automatically select the LLL implementation.
lmWrapper = LLLMethod $ unsafePerformIO $ peek c_lmWrapper

-- | Use a slower method that has proven precision.
lmProved = LLLMethod $ unsafePerformIO $ peek c_lmProved

-- | Use the heuristic method.
lmHeuristic = LLLMethod $ unsafePerformIO $ peek c_lmHeuristic

-- | Use the fast but less precise LLL method.
lmFast = LLLMethod $ unsafePerformIO $ peek c_lmFast

-- | Automatically select floating point type.
ftDefault = FloatType $ unsafePerformIO $ peek c_ftDefault

-- | Use @double@ precision.
ftDouble = FloatType $ unsafePerformIO $ peek c_ftDouble

-- | Use the @long double@ type.
ftLongDouble = FloatType $ unsafePerformIO $ peek c_ftLongDouble

-- | Use DPE (Double Plus Exponent) floating point representation, which can represent values with
-- extra large exponents.
ftDpe = FloatType $ unsafePerformIO $ peek c_ftDpe

-- | Use double-double arithmetic, where each value is represented as the sum of two double values,
-- representing the most and least significant bits, respectively.
ftDD = FloatType $ unsafePerformIO $ peek c_ftDD

-- | Use quad-double arithmetic. Values are represented as the sum of four doubles.
ftQD = FloatType $ unsafePerformIO $ peek c_ftQD

-- | Use MPFR for arbitrary precision arithmetic.
ftMpfr = FloatType $ unsafePerformIO $ peek c_ftMpfr

lllVerbose = LLLFlags $ unsafePerformIO $ peek c_lllVerbose
lllEarlyRed = LLLFlags $ unsafePerformIO $ peek c_lllEarlyRed
lllSiegel = LLLFlags $ unsafePerformIO $ peek c_lllSiegel

-- | Default options, i.e. no flags.
lllDefault = LLLFlags $ unsafePerformIO $ peek c_lllDefault

-- | Algorithm returned successfully. In some cases a 'RedStatus' is only returned in case of an
-- error, such as with 'Math.Lattices.Fplll.LLL.lllReduce', in which case this value will never be
-- returned.
redSuccess = RedStatus $ unsafePerformIO $ peek c_redSuccess
redGsoFailure = RedStatus $ unsafePerformIO $ peek c_redGsoFailure
redBabaiFailure = RedStatus $ unsafePerformIO $ peek c_redBabaiFailure
redLllFailure = RedStatus $ unsafePerformIO $ peek c_redLllFailure
redEnumFailure = RedStatus $ unsafePerformIO $ peek c_redEnumFailure
redBkzFailure = RedStatus $ unsafePerformIO $ peek c_redBkzFailure
redBkzTimeLimit = RedStatus $ unsafePerformIO $ peek c_redBkzTimeLimit
redBkzLoopsLimit = RedStatus $ unsafePerformIO $ peek c_redBkzLoopsLimit
redHlllFailure = RedStatus $ unsafePerformIO $ peek c_redHlllFailure
redHlllNormFailure = RedStatus $ unsafePerformIO $ peek c_redHlllNormFailure
redHlllSrFailure = RedStatus $ unsafePerformIO $ peek c_redHlllSrFailure

lllMethodStr :: Ptr CString
lllMethodStr = unsafePerformIO $ peek c_lllMethodStr

floatTypeStr :: Ptr CString
floatTypeStr = unsafePerformIO $ peek c_floatTypeStr

redStatusStr :: Ptr CString
redStatusStr = unsafePerformIO $ peek c_redStatusStr
