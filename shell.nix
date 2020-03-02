{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "frost";
  buildInputs = [ python3 zlib ghcid ];
}
