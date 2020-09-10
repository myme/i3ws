{
  pkgs ? import ./nix/nixpkgs.nix,
}:

pkgs.haskellPackages.shellFor {
  packages = ps: with ps; [
    fa
    i3ipc
    i3ws
  ];
  buildInputs = with pkgs.haskellPackages; [
    cabal-install
    ghcid
    hlint
  ];
}
