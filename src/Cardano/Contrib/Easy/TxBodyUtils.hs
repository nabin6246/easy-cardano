-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DerivingVia #-}
-- {-# LANGUAGE DisambiguateRecordFields #-}
-- {-# LANGUAGE EmptyCase #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE ViewPatterns #-}


-- -- | Transaction bodies
-- --
module Cardano.Contrib.Easy.TxBodyUtils (

  ) where

-- import           Prelude

-- import Cardano.Api hiding (TxOutDatum(..),TxOutDatumHash(..))
-- import Cardano.Api.Shelley hiding (TxOutDatum(..),TxOutDatumHash(..))

-- import           Control.Monad (guard)
-- import           Data.Aeson (object, withObject, withText, (.:), (.:?), (.=),FromJSONKey)
-- import qualified Data.Aeson as Aeson
-- import           Data.Aeson.Types (ToJSONKey (..), toJSONKeyText, toJSON)
-- import qualified Data.Aeson.Types as Aeson
-- import           Data.Bifunctor (first)
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as BSC
-- import qualified Data.ByteString.Lazy as LBS
-- import           Data.Foldable (for_, toList)
-- import           Data.Function (on)
-- import           Data.List (intercalate, sortBy)
-- import qualified Data.List.NonEmpty as NonEmpty
-- import           Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import           Data.Maybe (fromMaybe, maybeToList)
-- import           Data.Set (Set)
-- import qualified Data.Set as Set
-- import           Data.String
-- import           Data.Text (Text)
-- import qualified Data.Text as Text
-- import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))
-- import           Data.Word (Word32, Word64)
-- import           GHC.Generics
-- -- import qualified Text.Parsec as Parsec
-- -- import qualified Text.Parsec.Language as Parsec
-- -- import qualified Text.Parsec.String as Parsec
-- -- import qualified Text.Parsec.Token as Parsec

-- -- import           Cardano.Binary (Annotated (..), reAnnotate, recoverBytes)
-- -- import qualified Cardano.Binary as CBOR
-- import qualified Cardano.Crypto.Hash.Class as Crypto
-- -- import qualified Cardano.Ledger.Serialization as CBOR (decodeNullMaybe, encodeNullMaybe)
-- -- import           Cardano.Slotting.Slot (SlotNo (..))

-- -- import qualified Cardano.Chain.Common as Byron
-- -- import qualified Cardano.Chain.UTxO as Byron
-- -- import qualified Cardano.Crypto.Hashing as Byron

-- import qualified Cardano.Ledger.Address as Shelley
-- import qualified Cardano.Ledger.AuxiliaryData as Ledger (hashAuxiliaryData)
-- import           Cardano.Ledger.BaseTypes (StrictMaybe (..), maybeToStrictMaybe)
-- import qualified Cardano.Ledger.Core as Core
-- import qualified Cardano.Ledger.Core as Ledger
-- import qualified Cardano.Ledger.Credential as Shelley
-- import qualified Cardano.Ledger.Era as Ledger
-- import qualified Cardano.Ledger.Keys as Shelley
-- import qualified Cardano.Ledger.SafeHash as SafeHash
-- import qualified Cardano.Ledger.Shelley.Constraints as Ledger

-- import qualified Cardano.Ledger.Shelley.Genesis as Shelley
-- import qualified Cardano.Ledger.Shelley.Metadata as Shelley
-- import qualified Cardano.Ledger.Shelley.Tx as Shelley
-- import qualified Cardano.Ledger.Shelley.TxBody as Shelley
-- import qualified Cardano.Ledger.TxIn as Ledger

-- import qualified Cardano.Ledger.Mary.Value as Mary
-- import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Allegra
-- import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Mary
-- import qualified Cardano.Ledger.ShelleyMA.TxBody as Allegra
-- import qualified Cardano.Ledger.ShelleyMA.TxBody as Mary
-- import           Cardano.Ledger.Val (isZero)

-- import qualified Cardano.Ledger.Alonzo as Alonzo
-- import qualified Cardano.Ledger.Alonzo.Data as Alonzo
-- import qualified Cardano.Ledger.Alonzo.Language as Alonzo
-- import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
-- import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
-- import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
-- import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
-- import qualified Cardano.Ledger.SafeHash as Ledger

-- import qualified Data.Text.Encoding as Text
-- import           Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)

-- import           Data.Proxy (Proxy (..))

-- import          qualified Ouroboros.Consensus.Shelley.Eras as Ledger
-- import qualified Data.ByteString.Base16 as Base16

-- -- import           Ouroboros.Consensus.Shelley.Eras (StandardAllegra, StandardAlonzo, StandardMary,
-- --                    StandardShelley)

-- -- import           Cardano.Api.Address
-- -- import           Cardano.Api.Certificate
-- -- import           Cardano.Api.Eras
-- -- import           Cardano.Api.Error
-- -- import           Cardano.Api.HasTypeProxy
-- -- import           Cardano.Api.Hash
-- -- import           Cardano.Api.KeysByron
-- -- import           Cardano.Api.KeysShelley
-- -- import           Cardano.Api.NetworkId
-- -- import           Cardano.Api.ProtocolParameters
-- -- import           Cardano.Api.Script
-- -- import           Cardano.Api.ScriptData
-- -- import           Cardano.Api.SerialiseCBOR
-- -- import           Cardano.Api.SerialiseJSON
-- -- import           Cardano.Api.SerialiseRaw
-- -- import           Cardano.Api.SerialiseTextEnvelope
-- -- import           Cardano.Api.SerialiseUsing
-- -- import           Cardano.Api.TxMetadata
-- -- import           Cardano.Api.Utils
-- -- import           Cardano.Api.Value
-- -- import           Cardano.Api.ValueParser
-- import           Cardano.Ledger.Crypto (StandardCrypto)


-- toShelleyTxOutAny :: forall ctx era ledgerera.
--                    ShelleyLedgerEra era ~ ledgerera
--                 => ShelleyBasedEra era
--                 -> TxOut ctx era
--                 -> Ledger.TxOut ledgerera
-- toShelleyTxOutAny era (TxOut _ (TxOutAdaOnly AdaOnlyInByronEra _) _) =
--     case era of {}

-- toShelleyTxOutAny _ (TxOut addr (TxOutAdaOnly AdaOnlyInShelleyEra value) _) =
--     Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

-- toShelleyTxOutAny _ (TxOut addr (TxOutAdaOnly AdaOnlyInAllegraEra value) _) =
--     Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

-- toShelleyTxOutAny _ (TxOut addr (TxOutValue MultiAssetInMaryEra value) _) =
--     Shelley.TxOut (toShelleyAddr addr) (toMaryValue value)

-- toShelleyTxOutAny _ (TxOut addr (TxOutValue MultiAssetInAlonzoEra value) txoutdata) =
--     Alonzo.TxOut (toShelleyAddr addr) (toMaryValue value)
--                  (toAlonzoTxOutDataHash' txoutdata)

-- toAlonzoTxOutDataHash' :: TxOutDatum ctx era
--                       -> StrictMaybe (Alonzo.DataHash StandardCrypto)
-- toAlonzoTxOutDataHash'  TxOutDatumNone                          = SNothing
-- toAlonzoTxOutDataHash' (TxOutDatumHash _ (ScriptDataHash dh))   = SJust dh
-- toAlonzoTxOutDataHash' (TxOutDatum'    _ (ScriptDataHash dh) _) = SJust dh

-- data TxOutDatum ctx era where

--      TxOutDatumNone :: TxOutDatum ctx era

--      -- | A transaction output that only specifies the hash of the datum, but
--      -- not the full datum value.
--      --
--      TxOutDatumHash :: ScriptDataSupportedInEra era
--                     -> Hash ScriptData
--                     -> TxOutDatum ctx era

--      -- | A transaction output that specifies the whole datum value. This can
--      -- only be used in the context of the transaction body, and does not occur
--      -- in the UTxO. The UTxO only contains the datum hash.
--      --
--      TxOutDatum'    :: ScriptDataSupportedInEra era
--                     -> Hash ScriptData
--                     -> ScriptData
--                     -> TxOutDatum CtxTx era

-- deriving instance Eq   (TxOutDatum ctx era)
-- deriving instance Show (TxOutDatum ctx era)

-- pattern TxOutDatum :: ScriptDataSupportedInEra era
--                    -> ScriptData
--                    -> TxOutDatum CtxTx era
-- pattern TxOutDatum s d  <- TxOutDatum' s _ d
--   where
--     TxOutDatum s d = TxOutDatum' s (hashScriptData d) d

-- {-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutDatum' #-}
-- {-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutDatum  #-}


-- -- newtype instance Hash ScriptData =
-- --     ScriptDataHash (Alonzo.DataHash StandardCrypto)
-- --   deriving stock (Eq, Ord)
-- --   deriving (Show, IsString)         via UsingRawBytesHex (Hash ScriptData)
-- --   deriving (ToJSON, FromJSON)       via UsingRawBytesHex (Hash ScriptData)
-- --   deriving (ToJSONKey, FromJSONKey) via UsingRawBytesHex (Hash ScriptData)

-- -- instance SerialiseAsRawBytes (Hash ScriptData) where
-- --     serialiseToRawBytes (ScriptDataHash dh) =
-- --       Crypto.hashToBytes (Ledger.extractHash dh)

-- --     deserialiseFromRawBytes (AsHash AsScriptData) bs =
-- --       ScriptDataHash . Ledger.unsafeMakeSafeHash <$> Crypto.hashFromBytes bs


-- -- newtype UsingRawBytesHex a = UsingRawBytesHex a

-- -- instance SerialiseAsRawBytes a => Show (UsingRawBytesHex a) where
-- --     show (UsingRawBytesHex x) = show (serialiseToRawBytesHex x)

-- -- instance SerialiseAsRawBytes a => IsString (UsingRawBytesHex a) where
-- --     fromString = either error id . deserialiseFromRawBytesBase16 . BSC.pack

-- -- instance SerialiseAsRawBytes a => ToJSON (UsingRawBytesHex a) where
-- --     toJSON (UsingRawBytesHex x) = toJSON (serialiseToRawBytesHexText x)

-- -- instance (SerialiseAsRawBytes a, Typeable a) => FromJSON (UsingRawBytesHex a) where
-- --   parseJSON =
-- --     Aeson.withText tname $
-- --       either fail pure . deserialiseFromRawBytesBase16 . Text.encodeUtf8
-- --     where
-- --       tname  = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

-- -- instance SerialiseAsRawBytes a => ToJSONKey (UsingRawBytesHex a) where
-- --   toJSONKey =
-- --     Aeson.toJSONKeyText $ \(UsingRawBytesHex x) -> serialiseToRawBytesHexText x

-- -- instance
-- --   (SerialiseAsRawBytes a, Typeable a) => FromJSONKey (UsingRawBytesHex a) where

-- --   fromJSONKey =
-- --     Aeson.FromJSONKeyTextParser $
-- --     either fail pure . deserialiseFromRawBytesBase16 . Text.encodeUtf8


-- -- deserialiseFromRawBytesBase16 ::
-- --   SerialiseAsRawBytes a => ByteString -> Either String (UsingRawBytesHex a)
-- -- deserialiseFromRawBytesBase16 str =
-- --   case Base16.decode str of
-- --     Right raw -> case deserialiseFromRawBytes ttoken raw of
-- --       Just x  -> Right (UsingRawBytesHex x)
-- --       Nothing -> Left ("cannot deserialise " ++ show str)
-- --     Left msg  -> Left ("invalid hex " ++ show str ++ ", " ++ msg)
-- --   where
-- --     ttoken = proxyToAsType (Proxy :: Proxy a)

-- -- type family ShelleyLedgerEra era where

-- --   ShelleyLedgerEra ShelleyEra = Ledger.StandardShelley
-- --   ShelleyLedgerEra AllegraEra = Ledger.StandardAllegra
-- --   ShelleyLedgerEra MaryEra    = Ledger.StandardMary
-- --   ShelleyLedgerEra AlonzoEra  = Ledger.StandardAlonzo

