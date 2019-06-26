let
  nixpkgs = import <nixpkgs> { inherit config; };
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ../.gitignore ];
  config = {
    # allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc864.override {
        overrides = self: super: (
          builtins.mapAttrs (
            name: path: super.callCabal2nix name (gitignore path) {}) (import ./packages.nix)
        );
      };
    };
  };
in
  nixpkgs
