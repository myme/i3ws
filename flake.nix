{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "i3ws - Dynamic i3 wm workspace labeling";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    let overlay = import ./overlay.nix;
    in {
      inherit overlay;
    } // (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };

      in rec {
        packages = { inherit (pkgs.haskellPackages) fa i3ipc i3ws; };
        defaultPackage = packages.i3ws;
        checks = defaultPackage;
        devShell = pkgs.haskellPackages.shellFor {
          packages = ps: with ps; [ fa i3ipc i3ws ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            ghcid
            hlint
          ];
        };
      }));
}
