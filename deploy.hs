#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import qualified Data.Text as T
import Relude
import System.Environment
import System.Process

main :: IO ()
main = build >>= deploy "chris-martin.org"

build :: IO Text
build = T.strip . toText <$> readProcess "nix-build" ["nixos.nix", "--no-out-link"] ""

deploy :: Text -> Text -> IO ()
deploy host nixos = copy host nixos *> setProfile host nixos *> switch host

copy :: Text -> Text -> IO ()
copy host nixos = callProcess "nix-copy-closure" ["-v", "--use-substitutes", "--to", toString host, toString nixos]

setProfile :: Text -> Text -> IO ()
setProfile host nixos = callProcess "ssh" [toString host, "sudo", "nix-env", "--profile", "/nix/var/nix/profiles/system", "--set", toString nixos]

switch :: Text -> IO ()
switch host = callProcess "ssh" [toString host, "sudo", "/nix/var/nix/profiles/system/bin/switch-to-configuration", "switch"]
