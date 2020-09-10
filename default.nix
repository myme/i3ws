{
  pkgs ? import ./nix/nixpkgs.nix,
}:

let
  derivations = pkgs.callPackage (import ./nix/derivations.nix) {};

in
  derivations
