let
    versions = import ./versions.nix;
    inherit (versions) nixpkgs;
    pkgs = import nixpkgs {};
in
    (import "${nixpkgs}/nixos" {
        configuration = import ./nixos-config.nix {
            inherit nixpkgs pkgs;
        };
    }).system
