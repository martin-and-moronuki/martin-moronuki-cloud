# https://vaibhavsagar.com/blog/2018/05/27/quick-easy-nixpkgs-pinning/

let
    fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
        inherit sha256;
        url = "https://github.com/${owner}/${repo}/tarball/${rev}";
    };
    nixpkgs = import (fetcher (fromJSON (builtins.readFile ./versions.json)).nixpkgs) {};
    inherit (nixpkgs) lib;
    inherit (lib) mapAttrs;
    inherit (builtins) readFile fromJSON;
in
    mapAttrs (_: x: fetcher x) (fromJSON (readFile ./versions.json))
