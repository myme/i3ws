let
  nixpkgs = import <nixpkgs> { overlays = [pkgs]; };
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ../.gitignore ];
  pkgs = _: super: {
    haskellPackages = super.haskell.packages.ghc865.override {
      overrides = _: hsuper: with hsuper; {
        fa = callCabal2nix "fa" (gitignore ../fa) {};
        i3ipc = callCabal2nix "i3ipc" (gitignore ../i3ipc) {};
        i3ws = callCabal2nix "i3ws" (gitignore ../i3ws) {};
      };
    };
  };
in
  nixpkgs
