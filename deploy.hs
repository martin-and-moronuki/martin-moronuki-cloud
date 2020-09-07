#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

import System.Process

main :: IO ()
main =
  _ >>= \nixos ->
    callProcess "nix-copy-closure" ["--use-substitutes", "--to", "ssh://" <> host, nixos]
      *> callProcess "ssh" [host, "nix-env", "--profile", "/nix/var/nix/profiles/system", "--set", nixos]
      *> callProcess "ssh" [host, "sudo", "/nix/var/nix/profiles/system/bin/switch-to-configuration", "switch"]

host :: String
host = "chris-martin.org"
