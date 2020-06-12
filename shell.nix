{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "frost";
  # Stack with Nix enabled does not work on OSX (:shrug:)
  # Workaround that I found on Idris https://github.com/idris-lang/Idris-dev/pull/2938 fixes the issue
  buildInputs = [ python3 zlib ghcid ] ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
  ]);
}
