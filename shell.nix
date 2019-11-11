{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "frost-env";
  buildInputs = [ zlib ];
}
