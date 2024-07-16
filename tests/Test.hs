{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

module Test where

import           Data.List                                   (find)
import           Data.Maybe                                  (isJust)
import           Prelude                                     hiding (Fractional (..), Num (..), sum)
import           Test.Hspec                                  (hspec, describe, it)
import           Test.QuickCheck                             (property)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.Vector                     (fromVector)
import           ZkFold.Symbolic.Cardano.Types               hiding (Bool)
import qualified ZkFold.Symbolic.Data.Bool                   as ZK
import           ZkFold.Symbolic.Data.Conditional            (bool)
import qualified ZkFold.Symbolic.Data.Eq                     as ZK

import           ENCOINS.Core.V2.Script
import           Test.Internal

propInputsAreValid :: forall context . (Eq (TxIn context))
    => EncoinsContractTest context -> Bool
propInputsAreValid (EncoinsContractTest ins tx _ _) =
    let
        txIns = txInputs tx

        conditionInputsAreValid = all (\i -> isJust $ find (== i) ins) txIns
    in
        conditionInputsAreValid

propEncoinsTx :: forall context . (Sig context, Eq (context 1 F))
    => EncoinsContractTest context -> Bool
propEncoinsTx (EncoinsContractTest _ tx input mt) =
    let
        conditionEncoinsTransaction = encoinsTransaction tx input == ZK.true || mt
    in
        conditionEncoinsTransaction

propBalanceIsValid :: forall context .
    ( Sig context
    , Eq (Value MaxTokens context)
    , Eq (ByteString 4 context)
    , Eq (ByteString 224 context)
    ) => EncoinsContractTest context -> Bool
propBalanceIsValid (EncoinsContractTest _ tx input mt) =
    let
        encoinsAddress = inputEncoinsAddress input
        outs = txOutputs tx
        outputsAtEncoins = (\o -> txoAddress o ZK.== encoinsAddress) <$> fromVector outs :: [Bool]
        valueAtEncoins = sum ((\(o, b) -> bool zero (txoTokens o) b) <$> zip (fromVector outs) outputsAtEncoins)
        conditionValueAtEncoinsContract = valueAtEncoins /= zero || mt
    in
        conditionValueAtEncoinsContract

specEncoinsV2 :: forall context .
    ( Sig context
    , Eq (TxIn context)
    , Eq (Value MaxTokens context)
    , Eq (context 1 F)
    , Eq (ByteString 4 context)
    , Eq (ByteString 224 context)) =>  IO ()
specEncoinsV2 = hspec $ do
    describe "Encoins V2 specification" $ do
        it "should have valid inputs" $ 
            property $ propInputsAreValid  @context
        it "should be a valid encoins transaction" $
            property $ propEncoinsTx @context
        it "should have a positive final balance at the Encoins address" $
            property $ propBalanceIsValid @context