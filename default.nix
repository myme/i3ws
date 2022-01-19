let overlay = import ./overlay.nix;
in {
  pkgs ? import <nixpkgs> {
    overlays = [overlay];
  },
}:
{
  inherit (pkgs.haskellPackages) fa i3ipc i3ws;
}
