module ENCOINS.Core.V2.Script where

import           Data.Zip                                       (Zip(..))
import           GHC.Natural (Natural)
import           Prelude                                        hiding (Num(..), negate, Bool, Eq (..), all, sum, length, splitAt, replicate, zip, (&&), (==),
                                                                 (*), (+), (!!), (||))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number               (value)
import           ZkFold.Base.Data.Vector                        ((!!), uncons, unsafeToVector, Vector)
import           ZkFold.Prelude                                 (replicate)
import           ZkFold.Symbolic.Algorithms.Hash.Blake2b        (blake2b_256)
import           ZkFold.Symbolic.Algorithms.Hash.MiMC
import           ZkFold.Symbolic.Algorithms.Hash.MiMC.Constants (mimcConstants)
import           ZkFold.Symbolic.Cardano.Types
import           ZkFold.Symbolic.Data.Bool                      hiding (Bool(..))
import           ZkFold.Symbolic.Data.ByteString                hiding (ByteString(..))
import qualified ZkFold.Symbolic.Data.ByteString                as Symbolic
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Conditional               (bool, Conditional)
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.FieldElement              (fromFieldElement)
import           ZkFold.Symbolic.Data.UInt                      hiding (UInt(..))

type MaxTokens = 2
type MaxTokenMints = 4

type TxOut context = Output MaxTokens () context
type TxIn context  = Input MaxTokens () context
type Tx context = Transaction 3 3 5 MaxTokens MaxTokenMints () context

type Coin context = (Value MaxTokens context, FieldElement context)

-- TODO: check correctness
toByteString :: forall context n . Extend (ByteString 1 context) (ByteString n context) => FieldElement context -> ByteString n context
toByteString = extend . Symbolic.ByteString . fromFieldElement

hash :: forall c x . MiMCHash F c x => x -> FieldElement c
hash = mimcHash mimcConstants zero

type Sig context =
    ( FromConstant Natural (ByteString 0 context)
    , FromConstant Natural (ByteString 4 context)
    , FromConstant Natural (ByteString 8 context)
    , FromConstant Natural (UInt 32 context)
    , FromConstant Natural (UInt 64 context)
    , FromConstant Natural (UTCTime context)
    , Concat (ByteString 8 context) (ByteString 224 context)
    , Concat (ByteString 8 context) (ByteString 256 context)
    , StrictConv F (UInt 256 context)
    , AdditiveGroup (UInt 64 context)
    , AdditiveSemigroup (Value MaxTokens context)
    , MultiplicativeSemigroup (UInt 256 context)    
    , BoolType (ByteString 64 context)
    , Conditional (Bool context) (Value MaxTokens context)
    , Eq (Bool context) (ByteString 4 context)
    , Eq (Bool context) (ByteString 224 context)
    , Eq (Bool context) (ByteString 256 context)
    , Eq (Bool context) (UInt 256 context)
    , Eq (Bool context) (Value MaxTokens context)
    , Eq (Bool context) (Address context)
    , Eq (Bool context) (Output MaxTokens () context)
    , Eq (Bool context) (UInt 64 context)
    , Scale Natural (Value 5 context)
    , Iso (UInt 256 context) (ByteString 256 context)
    , Iso (ByteString 64 context) (UInt 64 context)
    , Iso (ByteString 512 context) (UInt 512 context)
    , Extend (ByteString 0 context) (ByteString 256 context)
    , Extend (ByteString 0 context) (ByteString 1024 context)
    , Extend (ByteString 1 context) (ByteString 256 context)
    , Extend (ByteString 224 context) (ByteString 256 context)
    , Extend (ByteString 256 context) (ByteString 256 context)
    , Extend (ByteString 256 context) (ByteString 1024 context)
    , ToWords (ByteString 1024 context) (ByteString 64 context)
    , Truncate (ByteString 512 context) (ByteString 256 context)
    , ShiftBits (ByteString 64 context), ShiftBits (ByteString 1024 context)
    , Concat (ByteString 64 context) (ByteString 512 context)
    , ReverseEndianness 64 (ByteString 512 context)
    , ReverseEndianness 64 (ByteString 1024 context)
    , MultiplicativeMonoid (UInt 64 context)
    , MiMCHash F context (Coin context)
    , MiMCHash F context (Address context)
    , MiMCHash F context (Address context, ByteString 224 context)
    , MiMCHash F context (ByteString 256 context, (ByteString 256 context, (Value MaxTokens context, (Value MaxTokens context, (Address context, Address context)))))
    )

coinName :: forall context . Sig context => Coin context -> FieldElement context
coinName = hash

data ValueUpdate context = ValueUpdate
    { valueUpdateT         :: ByteString 256 context
    , valueUpdateT'        :: ByteString 256 context
    , valueUpdateVu        :: Value MaxTokens context
    , valueUpdateVr        :: Value MaxTokens context
    , valueUpdateA'        :: Address context
    , valueUpdateAr        :: Address context
    }

checkValueUpdate :: forall context . Sig context => ValueUpdate context -> (Coin context, Coin context) -> Bool context
checkValueUpdate (ValueUpdate t t' vu _ _ _ ) (c@(v, _), c'@(v', _)) =
    let
        conditionOldHash = t == toByteString (hash @context c)

        conditionNewHash = t' == toByteString (hash @context c')

        conditionBalance = v' == v + vu
    in conditionOldHash && conditionNewHash && conditionBalance

data ProtocolInput context = ProtocolInput
    { inputEncoinsAddress :: Address context
    , inputEncoinsSymbol  :: ByteString 224 context
    , inputValueUpdates   :: (ValueUpdate context, ValueUpdate context)
    , inputProofReference :: (OutputRef context, OutputRef context)
    }

-- TODO: replace with the actual values (testnet, mainnet)
encoinsBeaconSymbol :: Sig context => PolicyId context
encoinsBeaconSymbol = ""

-- TODO: replace with the actual values (testnet, mainnet)
encoinsProofSymbol :: Sig context => PolicyId context
encoinsProofSymbol = ""

encoinsTransaction :: forall context . Sig context => Tx context -> ProtocolInput context -> Bool context
encoinsTransaction tx input =
    let
        encoinsAddress = inputEncoinsAddress input
        encoinsSymbol  = inputEncoinsSymbol input
        (valueUpdate1, valueUpdate2) = inputValueUpdates input

        ins = txInputs tx
        outs = txOutputs tx
        refIns = txRefInputs tx
        
        refBeacon     = txiOutput $ refIns !! 0
        expectedDatum = toByteString @context @256 $ hash @context (encoinsAddress, encoinsSymbol)
        conditionBeaconIsPresent    = fst (getValue (txoTokens refBeacon) !! 1) == encoinsBeaconSymbol
        conditionBeaconDatumMatches = txoDatumHash refBeacon == blake2b_256 expectedDatum

        (outEncoins, outs') = uncons outs
        conditionEncoinsOutputExists = txoAddress outEncoins == encoinsAddress && txoDatumHash outEncoins == emptyDatumHash
        conditionEncoinsOutputIsUnique = all (\out -> txoAddress out /= encoinsAddress) outs'

        refCoin = getValue (txoTokens $ txiOutput $ refIns !! 1) !! 1
        refCoinPolicyId = fst refCoin
        refCoinName1 = fst $ snd refCoin
        valueUpdateHash1 = hash @context
            (valueUpdateT valueUpdate1,
            (valueUpdateT' valueUpdate1,
            (valueUpdateVu valueUpdate1,
            (valueUpdateVr valueUpdate1,
            (valueUpdateA' valueUpdate1,
            valueUpdateAr valueUpdate1)))))
        conditionValueUpdate1 = refCoinPolicyId == encoinsProofSymbol && refCoinName1 == toByteString valueUpdateHash1

        refCoin2 = getValue (txoTokens $ txiOutput $ refIns !! 2) !! 1
        refCoinPolicyId2 = fst refCoin2
        refCoinName2 = fst $ snd refCoin2
        valueUpdateHash2 = hash @context
            (valueUpdateT valueUpdate2,
            (valueUpdateT' valueUpdate2,
            (valueUpdateVu valueUpdate2,
            (valueUpdateVr valueUpdate2,
            (valueUpdateA' valueUpdate2,
            valueUpdateAr valueUpdate2)))))
        conditionValueUpdate2 = refCoinPolicyId2 == encoinsProofSymbol && refCoinName2 == toByteString valueUpdateHash2

        inputAtEncoins = fmap (\i -> txoAddress (txiOutput i) == encoinsAddress) ins :: Vector 3 (Bool context)
        valueAtEncoins = sum ((\(i, b) -> bool zero (txoTokens $ txiOutput i) b) <$> zip ins inputAtEncoins)
        valueAtEncoins' = txoTokens outEncoins
        valueInUpdates  = valueUpdateVu valueUpdate1 + valueUpdateVu valueUpdate2
        conditionBalance = valueAtEncoins' == valueAtEncoins + valueInUpdates
        
        outAtAr1 = Output (valueUpdateAr valueUpdate1, (valueUpdateVr valueUpdate1, emptyDatumHash))
        conditionRedemption1 = outs !! 1 == outAtAr1 || valueUpdateVr valueUpdate1 == zero

        outAtAr2 = Output (valueUpdateAr valueUpdate2, (valueUpdateVr valueUpdate2, emptyDatumHash))
        conditionRedemption2 = outs !! 2 == outAtAr2 || valueUpdateVr valueUpdate2 == zero

        mints = getValue $ txMint tx

        t1  = valueUpdateT valueUpdate1
        conditionCoinBurn1 = t1 ==
                extend (emptyByteString @F @context)
            || (t1 == fst (snd (mints !! 0)) && snd (snd (mints !! 0)) == negate one)
        t1' = valueUpdateT' valueUpdate1
        conditionCoinMint1 = t1' ==
                extend (emptyByteString @F @context)
            || (t1' == fst (snd (mints !! 1)) && snd (snd (mints !! 1)) == one)

        -- TODO: check correctness
        c1 = Value $ unsafeToVector $ replicate (value @MaxTokens) (encoinsSymbol, (t1', one))
        outAtA1 = Output (valueUpdateA' valueUpdate1, (c1, emptyDatumHash))
        conditionCoinSend1 = outs !! 3 == outAtA1 || valueUpdateVu valueUpdate1 == zero

        t2  = valueUpdateT valueUpdate2
        conditionTokenBurn2 = t2 ==
                extend (emptyByteString @F @context)
            || (t2 == fst (snd (mints !! 2)) && snd (snd (mints !! 2)) == negate one)
        t2' = valueUpdateT' valueUpdate2
        conditionTokenMint2 = t2' ==
                extend (emptyByteString @F @context)
            || (t2' == fst (snd (mints !! 3)) && snd (snd (mints !! 3)) == one)

        -- TODO: check correctness
        c2 = Value $ unsafeToVector $ replicate (value @MaxTokens) (encoinsSymbol, (t2', one))
        outAtA2 = Output (valueUpdateA' valueUpdate2, (c2, emptyDatumHash))
        conditionTokenSend2 = outs !! 4 == outAtA2 || valueUpdateVu valueUpdate2 == zero

    in conditionBeaconIsPresent
    && conditionBeaconDatumMatches
    && conditionEncoinsOutputExists
    && conditionEncoinsOutputIsUnique
    && conditionValueUpdate1
    && conditionValueUpdate2
    && conditionBalance
    && conditionRedemption1
    && conditionRedemption2
    && conditionCoinBurn1
    && conditionCoinMint1
    && conditionCoinSend1
    && conditionTokenBurn2
    && conditionTokenMint2
    && conditionTokenSend2