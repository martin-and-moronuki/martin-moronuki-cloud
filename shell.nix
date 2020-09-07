let
    pkgs = (import (import ./versions.nix).nixpkgs) {};
    haskell = pkgs.haskellPackages.ghcWithPackages hsPackageSelection;
    hsPackageSelection = haskellPackages: with haskellPackages; [
        aeson aeson-optics aeson-pretty http-conduit optics ormolu relude
    ];
in
    pkgs.mkShell {
        buildInputs = [ haskell ];
    }
