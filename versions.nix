let
    inherit (builtins) readFile fromJSON;
    inherit (nixpkgs) lib;
    inherit (lib) mapAttrs;

    nixpkgs = import (resolve versionData.nixpkgs) {};
    versionData = fromJSON (readFile ./versions.json);

    resolve = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
        url = "https://github.com/${owner}/${repo}/tarball/${rev}";
        inherit sha256;
    };

in
    mapAttrs (_: resolve) (fromJSON (readFile ./versions.json))
