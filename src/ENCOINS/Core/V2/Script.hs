module ENCOINS.Core.V2.Script where

import           Data.Zip                                       (Zip(..))
import           GHC.Natural (Natural)
import           Prelude                                        hiding (negate, Bool, Eq (..), all, sum, length, splitAt, zip, (&&), (==),
                                                                 (*), (+), (!!), (||))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.Vector                        ((!!), uncons, unsafeToVector)
import           ZkFold.Symbolic.Algorithms.Hash.Blake2b        (blake2b_256)
import           ZkFold.Symbolic.Algorithms.Hash.MiMC
import           ZkFold.Symbolic.Algorithms.Hash.MiMC.Constants (mimcConstants)
import           ZkFold.Symbolic.Cardano.Types
import           ZkFold.Symbolic.Compiler.Arithmetizable
import           ZkFold.Symbolic.Data.Bool                      (Bool (..), BoolType (..), all)
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.UInt

type MaxTokens = 5
type MaxTokenMints = 4

type TxOut a = Output MaxTokens () a
type TxIn a  = Input MaxTokens () a
type Tx a = Transaction 3 3 11 MaxTokens MaxTokenMints () a

type Coin b a = (Value MaxTokens b a, b 1 a)

toByteString :: forall a b n x . (BinaryExpansion x, Extend (Bits x) (b n a)) => x -> ByteString n b a
toByteString = ByteString . extend @_ @(b n a) . binaryExpansion

hash :: forall a b x . (Arithmetic a, MiMCHash a b x) => x -> b 1 a
hash = mimcHash @a mimcConstants zero

type Sig b a =
    ( Arithmetic a
    , FromConstant Natural (b 224 a)
    , StrictConv a (UInt 256 b a)
    , AdditiveGroup (UInt 64 b a)
    , AdditiveSemigroup (Value MaxTokens b a)
    , MultiplicativeSemigroup (UInt 256 b a)
    , BoolType (ByteString 64 b a)
    , Eq (Bool (b 1 a)) (ByteString 224 b a)
    , Eq (Bool (b 1 a)) (UInt 256 b a)
    , Eq (Bool (b 1 a)) (ByteString 256 b a)
    , Eq (Bool (b 1 a)) (Value MaxTokens b a)
    , Eq (Bool (b 1 a)) (Address b a)
    , Eq (Bool (b 1 a)) (Output MaxTokens () b a)
    , Eq (Bool (b 1 a)) (UInt 64 b a)
    , Functor (Value MaxTokens b)
    , Scale (b 1 a) a
    , Iso (UInt 256 b a) (ByteString 256 b a)
    , Iso (ByteString 64 b a) (UInt 64 b a)
    , Iso (ByteString 512 b a) (UInt 512 b a)
    , Extend (Bits (b 1 a)) (b 256 a)
    , Extend (ByteString 0 b a) (ByteString 256 b a)
    , Extend (ByteString 224 b a) (ByteString 256 b a)
    , Extend (ByteString 256 b a) (ByteString 1024 b a)
    , ToWords (ByteString 1024 b a) (ByteString 64 b a)
    , Truncate (ByteString 512 b a) (ByteString 256 b a)
    , ShiftBits (ByteString 64 b a), ShiftBits (ByteString 1024 b a)
    , Concat (ByteString 64 b a) (ByteString 512 b a)
    , ReverseEndianness 64 (ByteString 512 b a)
    , ReverseEndianness 64 (ByteString 1024 b a)
    , FromConstant Natural (b 0 a)
    , FromConstant Natural (UInt 64 b a), MultiplicativeMonoid (UInt 64 b a)
    , FromConstant Natural (ByteString 0 b a), Extend (ByteString 0 b a) (ByteString 1024 b a)
    , MiMCHash a b (Coin b a)
    , MiMCHash a b (Address b a)
    , MiMCHash a b (Address b a, ByteString 224 b a)
    , MiMCHash a b (ByteString 256 b a, (ByteString 256 b a, (Value MaxTokens b a, (Value MaxTokens b a, (Address b a, Address b a)))))
    , BinaryExpansion (b 1 a))

coinName :: forall b a . Sig b a => Coin b a -> b 1 a
coinName = hash

data ValueUpdate b a = ValueUpdate
    { valueUpdateT         :: ByteString 256 b a
    , valueUpdateT'        :: ByteString 256 b a
    , valueUpdateVu        :: Value MaxTokens b a
    , valueUpdateVr        :: Value MaxTokens b a
    , valueUpdateA'        :: Address b a
    , valueUpdateAr        :: Address b a
    }

checkValueUpdate :: forall a b . Sig b a => ValueUpdate b a -> (Coin b a, Coin b a) -> Bool (b 1 a)
checkValueUpdate (ValueUpdate t t' vu _ _ _ ) (c@(v, _), c'@(v', _)) =
    let
        conditionOldHash = t == toByteString (hash @a @b c)

        conditionNewHash = t' == toByteString (hash @a @b c')

        conditionBalance = v' == v + vu
    in conditionOldHash && conditionNewHash && conditionBalance

data ProtocolInput b a = Input
    { inputEncoinsAddress :: Address b a
    , inputEncoinsSymbol  :: ByteString 224 b a
    , inputValueUpdates   :: (ValueUpdate b a, ValueUpdate b a)
    , inputProofReference :: (OutputRef b a, OutputRef b a)
    }

-- TODO: replace with the actual values (testnet, mainnet)
encoinsBeaconSymbol :: Sig b a => PolicyId b a
encoinsBeaconSymbol = ByteString $ fromConstant @Natural 12407958170701254809810251256235325962359823759

-- TODO: replace with the actual values (testnet, mainnet)
encoinsProofSymbol :: Sig b a => PolicyId b a
encoinsProofSymbol = ByteString $ fromConstant @Natural 98237578236573619851682415165263356158158158163

emptyByteString :: forall a b . Sig b a => ByteString 0 b a
emptyByteString = ByteString $ fromConstant @Natural 0

emptyDatumHash :: forall a b . Sig b a => ByteString 256 b a
emptyDatumHash = blake2b_256 emptyByteString

encoinsTransaction :: forall a b . Sig b a => Tx b a -> ProtocolInput b a -> Bool (b 1 a)
encoinsTransaction tx input =
    let
        encoinsAddress = inputEncoinsAddress input
        encoinsSymbol  = inputEncoinsSymbol input
        (valueUpdate1, valueUpdate2) = inputValueUpdates input

        ins = txInputs tx
        outs = txOutputs tx
        refIns = txRefInputs tx
        
        refBeacon     = txiOutput $ refIns !! 0
        expectedDatum = toByteString @a @b @256 $ hash @a @b (encoinsAddress, encoinsSymbol)
        conditionBeaconIsPresent    = fst (getValue (txoTokens refBeacon) !! 1) == encoinsBeaconSymbol
        conditionBeaconDatumMatches = txoDatumHash refBeacon == blake2b_256 expectedDatum

        (outEncoins, outs') = uncons outs
        conditionEncoinsOutputExists = txoAddress outEncoins == encoinsAddress && txoDatumHash outEncoins == emptyDatumHash
        conditionEncoinsOutputIsUnique = all (\out -> txoAddress out /= encoinsAddress) outs'

        refCoin = getValue (txoTokens $ txiOutput $ refIns !! 1) !! 1
        refCoinPolicyId = fst refCoin
        refCoinName1 = fst $ snd refCoin
        valueUpdateHash1 = hash @a @b
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
        valueUpdateHash2 = hash @a @b
            (valueUpdateT valueUpdate2,
            (valueUpdateT' valueUpdate2,
            (valueUpdateVu valueUpdate2,
            (valueUpdateVr valueUpdate2,
            (valueUpdateA' valueUpdate2,
            valueUpdateAr valueUpdate2)))))
        conditionValueUpdate2 = refCoinPolicyId2 == encoinsProofSymbol && refCoinName2 == toByteString valueUpdateHash2

        inputAtEncoins = fmap (\i -> txoAddress (txiOutput i) == encoinsAddress) ins
        valueAtEncoins = sum ((\(i, Bool b) -> scale @(b 1 a) b (txoTokens $ txiOutput i)) <$> zip ins inputAtEncoins)
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
                extend (emptyByteString @a @b)
            || (t1 == fst (snd (mints !! 0)) && snd (snd (mints !! 0)) == negate one)
        t1' = valueUpdateT' valueUpdate1
        conditionCoinMint1 = t1' ==
                extend (emptyByteString @a @b)
            || (t1' == fst (snd (mints !! 1)) && snd (snd (mints !! 1)) == one)

        c1 = Value $ unsafeToVector [(encoinsSymbol, (t1', one))]
        outAtA1 = Output (valueUpdateA' valueUpdate1, (c1, emptyDatumHash))
        conditionCoinSend1 = outs !! 3 == outAtA1 || valueUpdateVu valueUpdate1 == zero

        t2  = valueUpdateT valueUpdate2
        conditionTokenBurn2 = t2 ==
                extend (emptyByteString @a @b)
            || (t2 == fst (snd (mints !! 2)) && snd (snd (mints !! 2)) == negate one)
        t2' = valueUpdateT' valueUpdate2
        conditionTokenMint2 = t2' ==
                extend (emptyByteString @a @b)
            || (t2' == fst (snd (mints !! 3)) && snd (snd (mints !! 3)) == one)

        c2 = Value $ unsafeToVector [(encoinsSymbol, (t2', one))]
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