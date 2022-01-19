{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "i3ws - Dynamic i3 wm workspace labeling";

  inputs.nixpkgs.url = "nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = import ./overlay.nix;
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };

      in rec {
        inherit overlay;
        packages = { inherit (pkgs.haskellPackages) fa i3ipc i3ws; };
        defaultPackage = packages.i3ws;
        checks = defaultPackage;
        devShell = pkgs.haskellPackages.shellFor {
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
        };
      });
}
