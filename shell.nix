let
    pkgs = (import (import ./versions.nix).nixpkgs) {};
    haskell = pkgs.haskellPackages.ghcWithPackages hsPackageSelection;
    hsPackageSelection = haskellPackages: with haskellPackages; [
        aeson aeson-optics aeson-pretty http-conduit optics ormolu relude unordered-containers
    ];
in
    pkgs.mkShell {
        buildInputs = [ haskell pkgs.cacert pkgs.openssh pkgs.cabal-install pkgs.nix ];
    }
