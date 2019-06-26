let
  findHaskellPackages = (import ./utils.nix).findHaskellPackages;
in
  findHaskellPackages ./..
