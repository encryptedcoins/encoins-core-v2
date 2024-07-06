{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE  TypeApplications   #-}

module Test where

import           Data.List                                   (find)
import           Data.Maybe                                  (isJust)
import           Prelude                                     hiding (Fractional (..), Num (..), sum)
import           Test.Hspec                                  (hspec, describe, it)
import           Test.QuickCheck                             (property)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.Vector                     (fromVector)
import           ZkFold.Symbolic.Cardano.Types
import qualified ZkFold.Symbolic.Data.Bool                   as ZK
import qualified ZkFold.Symbolic.Data.Eq                     as ZK

import           ENCOINS.Core.V2.Script

propInputsAreValid :: forall a b . (Eq (TxIn b a))
    => EncoinsContractTest b a -> Bool
propInputsAreValid (EncoinsContractTest ins tx _) =
    let
        txIns = txInputs tx

        conditionInputsAreValid = all (\i -> isJust $ find (== i) ins) txIns
    in
        conditionInputsAreValid

propEncoinsTx :: forall a b . (Sig b a, Eq (b 1 a))
    => EncoinsContractTest b a -> Bool
propEncoinsTx (EncoinsContractTest _ tx input) =
    let
        conditionEncoinsTransaction = encoinsTransaction tx input == ZK.true
    in
        conditionEncoinsTransaction

propBalanceIsValid :: forall a b . (Sig b a, Eq (Value MaxTokens b a))
    => EncoinsContractTest b a -> Bool
propBalanceIsValid (EncoinsContractTest _ tx input) =
    let
        encoinsAddress = inputEncoinsAddress input
        outs = txOutputs tx
        outputsAtEncoins = (\o -> txoAddress o ZK.== encoinsAddress) <$> fromVector outs
        valueAtEncoins = sum ((\(o, ZK.Bool b) -> scale @(b 1 a) b (txoTokens o)) <$> zip (fromVector outs) outputsAtEncoins)
        conditionValueAtEncoinsContract = valueAtEncoins /= zero
    in
        conditionValueAtEncoinsContract

specEncoinsV2 :: forall a b . (Sig b a, Eq (TxIn b a), Eq (Value MaxTokens b a), Eq (b 1 a)) =>  IO ()
specEncoinsV2 = hspec $ do
    describe "Encoins V2 specification" $ do
        it "should have valid inputs" $ 
            property $ propInputsAreValid @a @b
        it "should be a valid encoins transaction" $
            property $ propEncoinsTx @a @b
        it "should have a positive final balance at the Encoins address" $
            property $ propBalanceIsValid @a @b