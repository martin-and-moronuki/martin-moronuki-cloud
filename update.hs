#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.Aeson.Encode.Pretty  as AP
import           Data.Aeson.Optics
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as T
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import           Optics
import           Relude
import           System.Process

main :: IO ()
main = read file >>= either fail pure >>= traverse update >>= write file
  where
    file = "versions.json"

read :: FilePath -> IO (Either String (Map Text Value))
read = eitherDecodeFileStrict

write :: FilePath -> (Map Text Value) -> IO ()
write file = LBS.writeFile file . encodePretty' conf
  where
    conf = AP.defConfig{ AP.confIndent = AP.Spaces 2, AP.confTrailingNewline = True }

update :: Value -> IO Value
update = updateRev >=> updateHash

updateRev :: Value -> IO Value
updateRev x =
    forceKey "owner" _String x >>= \owner ->
    forceKey "repo" _String x >>= \repo ->
    forceKey "branch" _String x >>= \branch ->
    httpJSON (gitHubRequest owner repo branch) >>= \response ->
    gitHubResponseRev response >>= \rev ->
    pure (x & set (key "rev") (review _String rev))

updateHash :: Value -> IO Value
updateHash x =
    forceKey "owner" _String x >>= \owner ->
    forceKey "repo" _String x >>= \repo ->
    forceKey "rev" _String x >>= \rev ->
    nixPrefetchUrl (gitHubArchive owner repo rev) >>= \hash ->
    pure (x & set (key "sha256") (review _String (toText hash)))

nixPrefetchUrl :: Text -> IO Text
nixPrefetchUrl url = T.strip . toText <$> readProcess "nix-prefetch-url" ["--unpack", toString url] ""

gitHubRequest :: Text -> Text -> Text -> Request
gitHubRequest owner repo branch = defaultRequest
    & setRequestCheckStatus & setRequestSecure True & setRequestPort 443
    & setRequestHost (encodeUtf8 ("api.github.com" :: Text))
    & setRequestPath (encodeUtf8 $ foldMap ("/" <> ) ["repos", owner, repo, "branches", branch])
    & addRequestHeader hUserAgent (encodeUtf8 gitHubUserAgent)

-- https://developer.github.com/v3/#user-agent-required
gitHubUserAgent :: Text
gitHubUserAgent = "github.com/martin-and-moronuki/martin-moronuki-cloud"

gitHubResponseRev :: Response Value -> IO Text
gitHubResponseRev = forceKey' ["commit", "sha"] _String . getResponseBody

gitHubArchive :: Text -> Text -> Text -> Text
gitHubArchive owner repo rev = "https://github.com" <> foldMap ("/" <> ) [owner, repo, "archive", rev <> ".tar.gz"]

forceKey :: Text -> Prism' Value a -> Value -> IO a
forceKey k p v = orErr $ preview (key k % p) v
  where
    orErr = maybe (fail err) pure
    err = "JSON problem at" ! show k ! "in value" ! show v

forceKey' :: [Text] -> Prism' Value a -> Value -> IO a
forceKey' ks p v = orErr $ preview (pathOptic % p) v
  where
    pathOptic :: AffineTraversal' Value Value
    pathOptic = foldr (\k o -> key k % o) (castOptic simple) ks
    orErr = maybe (fail err) pure
    err = "JSON problem at" ! intercalate "." (map show ks) ! "in value" ! show v

(!) :: (IsString a, Semigroup a) => a -> a -> a
a ! b = a <> " " <> b
