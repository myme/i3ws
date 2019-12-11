let
  pkgs = import ./nix/nixpkgs.nix;
  derivations = pkgs.callPackage (import ./nix/derivations.nix) {};
in
  derivations
