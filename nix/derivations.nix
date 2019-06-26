let
  nixpkgs = import ./nixpkgs.nix;
  packages = import ./packages.nix;
in
  builtins.mapAttrs (name: path: builtins.getAttr name nixpkgs.haskellPackages) packages
