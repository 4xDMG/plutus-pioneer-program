# Week 3 notes

[Lecture](https://www.youtube.com/watch?v=Lk1eIVm_ZTQ)

This week moved to a new commit hash of the Plutus repository. The `IsData.hs` example has been ported from last week to show the changes to Plutus with this update. Some of the changes are:

- ValidatorContext has been changed to ScriptContext
- the ScriptAddress constructor which took a validator hash has changed to scriptAddress which takes a validator
- it is no longer needed to remove the module header when copying code into the Plutus Playground editor
- fees are now considered in the Playground. The fees aren't realistic at this point and are always 10 Lovelace

The definition of CcriptContext can be found [here](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs#L116) in the Plutus repo.

```
data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
```

It is a record type with two fields, TxInfo and ScriptPurpose.

```
data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert
```

Above are the currently available script purposes. The most important one for this course will be the Spending purpose. Minting covers creating and burning tokens. There are two new purposes with this update to the Plutus repo. Rewarding which is related to staking rewards and Certifying which is related to delegation certificates.

```
data TxInfo = TxInfo
    { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
    , txInfoInputsFees  :: [TxInInfo]     -- ^ Transaction inputs designated to pay fees
    , txInfoOutputs     :: [TxOut] -- ^ Transaction outputs
    , txInfoFee         :: Value -- ^ The fee paid by this transaction.
    , txInfoForge       :: Value -- ^ The 'Value' forged by this transaction.
    , txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
    , txInfoValidRange  :: SlotRange -- ^ The valid range for the transaction.
    , txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoData        :: [(DatumHash, Datum)]
    , txInfoId          :: TxId
    -- ^ Hash of the pending transaction (excluding witnesses)
    } deriving (Generic)
```

Above is the definition of `TxInfo`. At the top you can see the context of the transaction in terms of its inputs and outputs. Below the top three you can see more pieces of data that are "global" to the transaction. Fee obviously represents the fees associated with it. Forge handles the minting or burning of any tokens related to this transaction. DCert shows a list of related certificates. Wdrl shows some staking related info. ValidRange (SlotRange) defines at which time the transaction is valid. Signatories is the list of signatures attached to the transaction. Data contains a list of pairs which match the DatumHash to the Datum. Finally the Id which is basically the hash of the transaction.

Restating one of the advantages of the EUTxO model over something like the Ethereum model (covered in lecture 1) is that validators can be run off chain (in the wallet) before ever making their way to the on chain validator. This means, under normal circumstances, if a transaction will fail it should never be run on the Cardano blockchain which means no fees will need to be paid. There is an issue with this that needs explaining, the time at which validation runs in the wallet will be different from the time at which validation runs on a Cardano node meaning things could have happened during that time difference and possibly the state of the transaction has changed. Cardano solves this with the `txInfoValidRange` (`SlotRange`) field which you can see in the definition of `TxInfo` above. This says that a transaction is valid between "this slot and that slot" and is specified in the transaction before sending it to the blockchain. Before any script is run some general checks are done about the transaction such as the inputs are valid, fees are included and that the slot range is correct. If the slot range has passed the transaction will immediately fail.

The definition for a Slot can be found [here](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Slot.hs).

```
newtype Slot = Slot { getSlot :: Integer }
    deriving stock (Haskell.Eq, Haskell.Ord, Show, Generic)
    deriving anyclass (FromJSON, FromJSONKey, ToJSON, ToJSONKey, NFData)
    deriving newtype (Haskell.Num, AdditiveSemigroup, AdditiveMonoid, AdditiveGroup, Enum, Eq, Ord, Real, Integral, Serialise, Hashable, PlutusTx.IsData)
```

Slot is a wrapper around the interger type and can be defined using the `Slot` constructor of by using a Integer literal.

```
type SlotRange = Interval Slot
```

A SlotRange is defined as an [Interval](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Interval.hs) of Slots. An interval is defined by having a lower bound and an upper bound which can either be inclusive or exclusive of the defined bounds. In the definition of Interval you can see a variety of helper functions for defining (e.g. `always` is the default for an Intervals upper bound and is any slot till the end of time) and working with (e.g. `before` checks if a value is before the lower bound, `after` checks if a value is above the upper bound) Intervals.

In the `Vesting.hs` file there is an example of a smart contract that could be used for giving some ADA to a child with the condition that they will only have access to it when they turn 18. For the Datum we would need to know who to send the ADA to (wallet address) and at what time the ADA should be sent.

```
data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: Slot
    } deriving Show
```

The type definition for the validator

```
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
```

Firstly error messages need to be created.

```
traceIfFalse "beneficiary's signature missing" checkSig      &&
traceIfFalse "deadline not reached"            checkDeadline
```

The checking of the conditions which trigger these errors has been delegated to some helper functions

```
where
    -- just return TxInfo to us because it contains all the information we need from the script context
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- txInfoSignatories contains a list of public keys that have signed a transaction.
    -- We need to check if the beneficiaries key is in this list
    checkSig :: Bool
    checkSig = beneficiary dat `elem` txInfoSignatories info

    -- txInfoValidRange contains the valid range of a transaction. The current slot is included in this range.
    -- We must check that all slots within this range are after the deadline
    checkDeadline :: Bool
    checkDeadline = from (deadline dat) `contains` txInfoValidRange info
```

In the playground there is currently no way to get the pubic key hash of a wallet. There are a few helper functions you can use to generate one though.

`walletPubKey` takes a Wallet and returns it's Public Key

```
walletPubKey :: Wallet -> PubKey
```

`pubKeyHash` takes the Public Key of a Wallet and returns its hash

```
pubKeyHash :: PubKey -> PubKeyHash
```

`Wallet` is defined by an Integer

```
type Wallet :: *
newtype Wallet = Wallet {getWallet :: Integer}
  	-- Defined in ‘Wallet.Emulator.Wallet’
instance Eq Wallet -- Defined in ‘Wallet.Emulator.Wallet’
instance Ord Wallet -- Defined in ‘Wallet.Emulator.Wallet’
instance Show Wallet -- Defined in ‘Wallet.Emulator.Wallet’
```

These functions can be chained together in order to get a `PubKeyHash` which is usable in the playground

```
pubKeyHash $ walletPubKey $ Wallet 2
-- returns 39f713d0a644253f04529421b9f51b9b08979d08295959c4f3990ee617f5139f
```

Another way to write the above script would be to create a Parameterized script. Writing a Parameterized script means when you instantiate the script with different values you get a different script. The difference with this and the example above is that if you make two vesting transaction with the example above, they will both have the same script address. A Parameterized script will make these seperate scripts running at different addresses. `Parameterized.hs` is a parameterized example of the `Vesting.hs` script.

To achieve this the `mkValidator` function now takes four arguments

```
mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
```

The Datum is no longer used. Calling the `mkValidator` function now looks like this

```
inst :: VestingParam -> Scripts.ScriptInstance Vesting
inst p = Scripts.validator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()
```

A full explanation of all that changes that are needed above to turn this into a parameterized script can be found [here](https://youtu.be/Lk1eIVm_ZTQ?t=3206).
