#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import qualified Data.Text as T
import Relude
import System.Environment
import System.Process

main :: IO ()
main = buildNixos >>= \nixos -> copy host nixos *> setProfile host nixos *> switch host
  where
    host = "chris-martin.org"

buildNixos :: IO Text
buildNixos = T.strip . toText <$> readProcess "nix-build" ["nixos.nix"] ""

copy :: Text -> Text -> IO ()
copy host nixos = callProcess "nix-copy-closure" ["--use-substitutes", "--to", toString $ "ssh://" <> host, toString nixos]

setProfile :: Text -> Text -> IO ()
setProfile host nixos = callProcess "ssh" [toString host, "nix-env", "--profile", "/nix/var/nix/profiles/system", "--set", toString nixos]

switch :: Text -> IO ()
switch host = callProcess "ssh" [toString host, "sudo", "/nix/var/nix/profiles/system/bin/switch-to-configuration", "switch"]
