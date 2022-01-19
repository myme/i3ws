final: prev:
let gitignore = prev.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
in {
  haskellPackages = prev.haskellPackages.override {
    overrides = _: hs: {
      fa = hs.callCabal2nix "fa" (gitignore ./fa) {};
      i3ipc = hs.callCabal2nix "i3ipc" (gitignore ./i3ipc) {};
      i3ws = hs.callCabal2nix "i3ws" (gitignore ./i3ws) {};
    };
  };
}
