let
  nixpkgs = import ./nix/nixpkgs.nix;
  packages = import ./nix/packages.nix;
in nixpkgs.haskellPackages.shellFor {
  packages = with builtins; ps: map (p: getAttr p ps) (attrNames packages);
  buildInputs = [
    nixpkgs.haskellPackages.cabal-install
  ];
}
