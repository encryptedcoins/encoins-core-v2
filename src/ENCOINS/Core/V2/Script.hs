module ENCOINS.Core.V2.Script where

import           Prelude                                        hiding (Bool, Eq (..), all, length, splitAt, zip, (&&),
                                                                 (*), (+))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.Vector                        (Vector (..))
import           ZkFold.Symbolic.Algorithms.Hash.MiMC           (mimcHash)
import           ZkFold.Symbolic.Algorithms.Hash.MiMC.Constants (mimcConstants)
import           ZkFold.Symbolic.Cardano.Types                  (Input, Output, Transaction, Value(..), Address)
import           ZkFold.Symbolic.Compiler                       (ArithmeticCircuit)
import           ZkFold.Symbolic.Compiler.Arithmetizable
import           ZkFold.Symbolic.Data.Bool                      (Bool, true)
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.UInt
import           ZkFold.Symbolic.Types                          (Symbolic)

type TxOut a = Output 10 () a
type TxIn a  = Input 10 () a
type Tx a = Transaction 6 0 11 10 () a

class Hash a x where
    hash :: x -> a

instance SymbolicData a x => Hash (ArithmeticCircuit a) x where
    hash datum = case pieces datum of
        []         -> zero
        [x]        -> mimcHash mimcConstants zero zero x
        [xL, xR]   -> mimcHash mimcConstants zero xL xR
        (xL:xR:xZ) -> mimcHash (zero : xZ ++ [zero]) zero xL xR

type Sig a = (StrictConv a (UInt 256 a),
    MultiplicativeSemigroup (UInt 256 a),
    Eq (Bool a) (UInt 256 a),
    Iso (UInt 256 a) (ByteString 256 a),
    Extend (ByteString 224 a) (ByteString 256 a),
    Hash a (TxOut a))

data Coin x = Coin
    { coinValue      :: Value 5 x
    , coinScriptHash :: x
    , coinNonce      :: x
    }

coinName :: forall a . Arithmetic a => Coin (ArithmeticCircuit a) -> ArithmeticCircuit a
coinName (Coin v h n) = hash (v, h, n)

data EncoinsInput x = EncoinsInput
    { encoinsInputAddressAssets  :: Address x
    , encoinsInputAddressCoins   :: Address x
    , encoinsInputSymbol         :: ByteString 224 x
    }

-- TODO: Replace the script component with S_0
coinZero :: forall a . Arithmetic a => Coin (ArithmeticCircuit a)
coinZero = Coin (Value (Vector @5 (replicate 5 (from @(UInt 224 (ArithmeticCircuit a)) zero, (from @(UInt 256 (ArithmeticCircuit a)) zero, zero))))) zero zero

data EncoinsValueUpdate x = EncoinsValueUpdate
    { encoinsValueUpdateT  :: ByteString 256 x
    , encoinsValueUpdateA' :: Address x
    , encoinsValueUpdateT' :: ByteString 256 x
    , encoinsValueUpdateD  :: Value 5 x
    , encoinsValueUpdateA  :: Address x
    , encoinsValueUpdateV  :: Value 5 x
    , encoinsValueUpdateS  :: x
    }

checkValueUpdate :: Symbolic x => EncoinsValueUpdate x -> () -> Bool x
checkValueUpdate _ () =
    let 
    in true

encoinsTransaction :: Tx x -> EncoinsInput x -> Bool x
encoinsTransaction _ _ =
    let
    in undefined