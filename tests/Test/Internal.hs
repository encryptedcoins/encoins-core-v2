{-# LANGUAGE UndecidableInstances #-}

module Test.Internal where

import           GHC.Natural                                    (Natural)
import           Prelude                                        hiding (Num(..), negate, Bool, Eq (..), all, sum, length, splitAt, replicate, zip, (&&), (==),
                                                                 (*), (+), (!!), (||))
import qualified Prelude                                        as Haskell
import           Test.QuickCheck                                (Arbitrary(..))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number               (value, KnownNat)
import           ZkFold.Base.Data.Vector                        (fromVector, Vector (..))
import           ZkFold.Prelude                                 (replicate)
import           ZkFold.Symbolic.Cardano.Types
import           ZkFold.Symbolic.Data.Bool                      (true)

import           ENCOINS.Core.V2.Script

data EncoinsContractTest context = EncoinsContractTest [TxIn context] (Tx context) (ProtocolInput context) Haskell.Bool

instance Show (EncoinsContractTest context) where
    show _ = "EncoinsContractTest"

instance Sig context => Arbitrary (EncoinsContractTest context) where
    arbitrary = do
        let ins   = fromVector $ txInputs tx
            input = ProtocolInput
                { inputEncoinsAddress = Address (fromConstant @Natural 3, ("", ""))
                , inputEncoinsSymbol  = ""
                , inputValueUpdates   = (ValueUpdate
                    { valueUpdateT = ""
                    , valueUpdateT' = ""
                    , valueUpdateVu = zero
                    , valueUpdateVr = zero
                    , valueUpdateA' = Address (fromConstant @Natural 3, ("", ""))
                    , valueUpdateAr = Address (fromConstant @Natural 3, ("", ""))
                    }, ValueUpdate
                    { valueUpdateT = ""
                    , valueUpdateT' = ""
                    , valueUpdateVu = zero
                    , valueUpdateVr = zero
                    , valueUpdateA' = Address (fromConstant @Natural 3, ("", ""))
                    , valueUpdateAr = Address (fromConstant @Natural 3, ("", ""))
                    })
                , inputProofReference = (OutputRef ("", fromConstant @Natural 5), OutputRef ("", fromConstant @Natural 5))
                }
            tx = Transaction
                ( Vector [arbTxIn, arbTxIn, arbTxIn], (
                  Vector [arbTxIn, arbTxIn, arbTxIn], (
                  Vector [arbTxOut, arbTxOut, arbTxOut, arbTxOut, arbTxOut], (
                  arbValue,
                  (fromConstant @Natural 0, fromConstant @Natural 0))))) :: Tx context
        return $ EncoinsContractTest ins tx input true

arbTxOutRef :: Sig context => OutputRef context
arbTxOutRef = OutputRef ("", fromConstant @Natural 0)

arbValue :: forall n context . (KnownNat n, Sig context) => Value n context
arbValue = Value $ Vector $ replicate (value @n) ("", ("", fromConstant @Natural 0))

arbTxOut :: Sig context => TxOut context
arbTxOut = Output (Address (fromConstant @Natural 3, ("", "")), (arbValue, emptyDatumHash))

arbTxIn :: Sig context => TxIn context
arbTxIn = Input (arbTxOutRef, arbTxOut)