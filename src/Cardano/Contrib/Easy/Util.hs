{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Cardano.Contrib.Easy.Util
where

import Cardano.Api
import Data.ByteString (ByteString,readFile)
import qualified Cardano.Api.Shelley as ShelleyApi
import Ledger (PubKeyHash (PubKeyHash), AssetClass)
import qualified Data.Set as Set
import Control.Exception (try, throw)
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import Cardano.Contrib.Easy.Error
import Plutus.V1.Ledger.Api (fromBuiltin, toBuiltin, ToData, toData, CurrencySymbol (CurrencySymbol), TokenName (TokenName))
import System.FilePath (joinPath)
import Cardano.Api.Shelley (ProtocolParameters (protocolParamUTxOCostPerWord), fromPlutusData, TxBody (ShelleyTxBody), Lovelace (Lovelace), toShelleyTxOut, Address (ShelleyAddress), fromShelleyStakeCredential, fromShelleyStakeReference, fromShelleyAddr, toShelleyAddr)
import qualified Cardano.Ledger.Alonzo.Tx as LedgerBody
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(SubmitSuccess, SubmitFail))
import Data.Text.Conversions (convertText, Base16 (unBase16), FromText (fromText), ToText (toText))
import Data.Functor ((<&>))

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Text.Encoding
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text.IO as TextIO
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo
import Plutus.V1.Ledger.Value (AssetClass(AssetClass))
import Data.String (fromString)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Shelley.Spec.Ledger.API (Credential(ScriptHashObj, KeyHashObj), KeyHash (KeyHash), StakeReference (StakeRefNull))
import Codec.Serialise (serialise)
import Cardano.Api.Byron (Address(ByronAddress))
import qualified Data.Aeson as JSON
import qualified Data.Text.Encoding as TSE
import Cardano.Contrib.Easy.Parsers
import qualified Data.Map as Map
import Data.Char (toLower)
import Data.Set (Set)

localNodeConnInfo :: NetworkId -> FilePath   -> LocalNodeConnectInfo CardanoMode
localNodeConnInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600))

readSignKey :: FilePath -> IO (SigningKey PaymentKey)
readSignKey file = do
  eitherSkey<-try  readSkeyFromFile
  case eitherSkey of
    Left (e::IOError )-> fail  "There was error reading skey file"
    Right sk -> pure sk
  where
    readSkeyFromFile=do
      exists<-doesFileExist file
      if exists then pure () else  fail $ file ++ "  doesn't exist"
      content <-readBs file
      parseSignKey content

    readBs:: FilePath -> IO T.Text
    readBs  = TextIO.readFile


getDefaultSignKey :: IO (SigningKey PaymentKey)
getDefaultSignKey= getWorkPath ["default.skey"] >>= readSignKey

skeyToAddr:: SigningKey PaymentKey -> NetworkId -> ShelleyApi.Address ShelleyAddr
skeyToAddr skey network =
  makeShelleyAddress  network  credential NoStakeAddress
  where
    credential=PaymentCredentialByKey  $ verificationKeyHash   $ getVerificationKey  skey

skeyToAddrInEra ::  SigningKey PaymentKey -> NetworkId -> AddressInEra AlonzoEra
skeyToAddrInEra skey network=makeShelleyAddressInEra network   credential NoStakeAddress
  where
    credential=PaymentCredentialByKey  $ verificationKeyHash   $ getVerificationKey  skey



sKeyToPkh:: SigningKey PaymentKey -> PubKeyHash
sKeyToPkh skey= PubKeyHash (toBuiltin  $  serialiseToRawBytes  vkh)
  where
    vkh=verificationKeyHash   $ getVerificationKey  skey

pkhToMaybeAddr:: NetworkId -> PubKeyHash -> Maybe (AddressInEra  AlonzoEra)
pkhToMaybeAddr network (PubKeyHash pkh) =do
    key <- vKey
    Just $ makeShelleyAddressInEra  network (PaymentCredentialByKey key)  NoStakeAddress
  where
    paymentCredential _vkey=PaymentCredentialByKey _vkey
    vKey= deserialiseFromRawBytes (AsHash AsPaymentKey) $fromBuiltin pkh

addrToMaybePkh :: Cardano.Api.Shelley.Address ShelleyAddr -> Maybe PubKeyHash
addrToMaybePkh (ShelleyAddress net cre sr) = do
  PubKeyHash . toBuiltin <$> hash
  where
    hash= case cre of
      ScriptHashObj _ ->Nothing
      KeyHashObj kh -> case kh of { KeyHash ha -> unHex $ init $ tail$ show  ha }

    unHex ::  ToText a => a -> Maybe  ByteString
    unHex v = convertText (toText v) <&> unBase16

addrInEraToPkh :: MonadFail m =>AddressInEra AlonzoEra -> m PubKeyHash
addrInEraToPkh a = case a of { AddressInEra atie ad -> case ad of
                                      ByronAddress ad' -> fail "Byron address is not supported"
                                      ShelleyAddress net cre sr -> case cre of
                                        ScriptHashObj sh -> fail "Expected PublicKey address got Script Address"
                                        KeyHashObj kh -> case kh of { KeyHash ha -> case unHex $ init $ tail $ show ha of
                                                                        Nothing -> fail "Unexpected"
                                                                        Just bs -> pure $ PubKeyHash (toBuiltin bs)
                                                                    }
                                        }
    where
    unHex ::  ToText a => a -> Maybe  ByteString
    unHex v = convertText (toText v) <&> unBase16

unstakeAddr :: AddressInEra AlonzoEra -> AddressInEra AlonzoEra
unstakeAddr a = case a of { AddressInEra atie ad -> case ad of
                                      ByronAddress ad' ->a
                                      ShelleyAddress net cre sr ->  shelleyAddressInEra $ ShelleyAddress net cre StakeRefNull }

queryUtxos :: LocalNodeConnectInfo CardanoMode-> AddressAny -> IO (UTxO AlonzoEra)
queryUtxos conn addr=do
  a <-queryNodeLocalState conn Nothing $ utxoQuery [addr]
  case a of
    Left af -> throw $ SomeError $ show af
    Right e -> case e of
      Left em -> throw $ SomeError $ show em
      Right uto -> return uto

  where
  utxoQuery qfilter= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo (QueryUTxO (QueryUTxOByAddress (Set.fromList qfilter)) )

resolveTxins :: LocalNodeConnectInfo CardanoMode -> Set TxIn -> IO (UTxO AlonzoEra)
resolveTxins conn ins= do
  a <- queryNodeLocalState conn Nothing (utxoQuery ins)
  case a of
    Left af -> throw $SomeError $ show af
    Right e -> case e of
      Left em -> throw $ SomeError $ show em
      Right uto -> return uto

    where
      utxoQuery qfilter = QueryInEra  AlonzoEraInCardanoMode
        $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo (QueryUTxO  $ QueryUTxOByTxIn qfilter )

getDefaultConnection :: String -> NetworkId ->  IO (LocalNodeConnectInfo CardanoMode)
getDefaultConnection networkName networkId= do
  sockEnv <- try $ getEnv "CARDANO_NODE_SOCKET_PATH"
  socketPath <-case  sockEnv of
    Left (e::IOError) -> do
          defaultSockPath<- getWorkPath ( if null networkName then ["node.socket"] else [networkName,"node.socket"])
          exists<-doesFileExist defaultSockPath
          if exists then return defaultSockPath else throw (SomeError $ "Socket File is Missing: "++defaultSockPath ++"\n\tSet environment variable CARDANO_NODE_SOCKET_PATH  to use different path")
    Right s -> pure s
  pure (localNodeConnInfo networkId socketPath )

getNetworkFromEnv :: String -> IO NetworkId
getNetworkFromEnv envKey =  do
  networkEnv <- try $ getEnv envKey
  case  networkEnv of
    Left (e::IOError) -> do
          pure (Testnet  (NetworkMagic 1097911063))
    Right s ->  case map toLower s of
      "mainnet" -> pure  Mainnet
      "testnet" -> pure $ Testnet  (NetworkMagic 1097911063)
      _  -> case read s of
        Just v -> pure (Testnet  (NetworkMagic v))
        _ -> fail "Invalid network id"

queryProtocolParam :: LocalNodeConnectInfo CardanoMode -> IO ProtocolParameters
queryProtocolParam conn=do
  paramQueryResult<-queryNodeLocalState conn Nothing $
            QueryInEra AlonzoEraInCardanoMode
                  $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
  case paramQueryResult of
    Left af -> throw $ SomeError  "QueryProtocolParam: Acquire Failure"
    Right e -> case e of
      Left em -> throw $ SomeError "QueryrotocolParam: Missmatched Era"
      Right pp -> return pp

querySystemStart conn=do
  result<-queryNodeLocalState conn Nothing QuerySystemStart
  case result of
    Left af -> throw $ SomeError "Acquire Failure"
    Right ss -> pure ss

queryEraHistory :: LocalNodeConnectInfo CardanoMode -> IO (EraHistory CardanoMode)
queryEraHistory conn=do
  result <- queryNodeLocalState conn Nothing (QueryEraHistory CardanoModeIsMultiEra)
  case result of
    Left af -> throw $ SomeError "Acquire Failure"
    Right eh -> pure eh

getWorkPath :: [FilePath] -> IO  FilePath
getWorkPath paths= do
  f <- getWorkPathFunc
  pure $ f paths

getWorkPathFunc :: IO( [FilePath] -> FilePath )
getWorkPathFunc = do
  eitherHome <-try $ getEnv "HOME"
  eitherCardanoHome <- try $ getEnv "CARDANO_HOME"
  case eitherCardanoHome of
    Left (e::IOError) ->   case eitherHome of
        Left (e::IOError) -> throw $ SomeError "Can't get Home directory. Missing   HOME and CARDANO_HOME"
        Right home -> pure $ f [home,".cardano"]
    Right home ->  pure $ f  [home]
    where
      f a b = joinPath $ a ++ b

dataToScriptData :: (ToData a1) => a1 -> ScriptData
dataToScriptData sData =  fromPlutusData $ toData sData

signAndSubmitTxBody :: LocalNodeConnectInfo CardanoMode
  -> TxBody AlonzoEra -> [SigningKey PaymentKey] -> IO (Tx AlonzoEra)
signAndSubmitTxBody conn txBody skeys= do
      let (ins,outs)=case txBody of { ShelleyTxBody sbe (LedgerBody.TxBody ins outs _ _ _ _ _ _ _ _ _ _ _ ) scs tbsd m_ad tsv -> (ins,outs) }
          tx = makeSignedTransaction (map toWitness skeys) txBody -- witness and txBody
      executeSubmitTx conn tx
      pure tx
  where
    toWitness skey = makeShelleyKeyWitness txBody (WitnessPaymentKey skey)

executeSubmitTx :: LocalNodeConnectInfo CardanoMode -> Tx AlonzoEra -> IO ()
executeSubmitTx conn  tx= do 
      res <-submitTxToNodeLocal conn $  TxInMode tx AlonzoEraInCardanoMode
      case res of
        SubmitSuccess ->  pure ()
        SubmitFail reason ->
          case reason of
            TxValidationErrorInMode err _eraInMode ->  throw$ SomeError $ "SubmitTx: " ++ show  err
            TxValidationEraMismatch mismatchErr -> throw $ SomeError $ "SubmitTx: " ++ show  mismatchErr

queryTxins :: LocalNodeConnectInfo CardanoMode-> [TxIn] -> IO (UTxO AlonzoEra)
queryTxins conn txin=do
  a <-queryNodeLocalState conn Nothing $ utxoQuery txin
  case a of
    Left af -> throw $ SomeError $ show af
    Right e -> case e of
      Left em -> throw $ SomeError $ show em
      Right uto -> return uto

  where
  utxoQuery qfilter= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo (QueryUTxO (QueryUTxOByTxIn  (Set.fromList qfilter)) )


nullValue :: Value -> Bool
nullValue v = not $ any (\(aid,Quantity q) -> q>0) (valueToList v)

positiveValue :: Value -> Bool
positiveValue v = not $ any (\(aid,Quantity q) -> q<0) (valueToList v)

-- calculateTxoutMinLovelace :: TxOut CtxTx AlonzoEra -> ProtocolParameters -> Maybe Lovelace
-- calculateTxoutMinLovelace txout pParams=do
--   Lovelace costPerWord <- protocolParamUTxOCostPerWord pParams
--   Just $ Lovelace  $ Alonzo.utxoEntrySize (toShelleyTxOut ShelleyBasedEraAlonzo  txout) * costPerWord

calculateTxoutMinLovelaceFunc :: ProtocolParameters  -> Maybe ( TxOut CtxTx AlonzoEra -> Lovelace)
calculateTxoutMinLovelaceFunc pParams = do
  Lovelace costPerWord <- protocolParamUTxOCostPerWord pParams
  pure $ f costPerWord
  where
    f cpw txout =Lovelace  $ Alonzo.utxoEntrySize (toShelleyTxOutAny ShelleyBasedEraAlonzo txout) * cpw

toPlutusAssetClass :: AssetId -> AssetClass
toPlutusAssetClass (AssetId (PolicyId hash) (AssetName name)) = AssetClass (CurrencySymbol $ toBuiltin $ serialiseToRawBytes hash , TokenName $ toBuiltin name)
toPlutusAssetClass AdaAssetId  =AssetClass (CurrencySymbol $ fromString "", TokenName $ fromString "")


utxoValueSum :: UTxO AlonzoEra  -> Value
utxoValueSum (UTxO uMap)= foldMap toValue $ Map.elems uMap
  where
    toValue (TxOut _ val _)= case val of
      TxOutValue masie va -> va