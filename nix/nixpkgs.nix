let
  nixpkgs = import <nixpkgs> { overlays = [pkgs]; };
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ../.gitignore ];
  pkgs = self: super: {
    haskellPackages = super.haskell.packages.ghc864.override {
      overrides = self: super: (
        builtins.mapAttrs (
          name: path: super.callCabal2nix name (gitignore path) {}) (import ./packages.nix)
      );
    };
  };
in
  nixpkgs
