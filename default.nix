let
  nixpkgs = import ./nix/nixpkgs.nix;
  derivations = import ./nix/derivations.nix;
in
  derivations.i3ws
