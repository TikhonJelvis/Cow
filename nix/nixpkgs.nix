# Pinnable versions of Nixpkgs and all-cabal-hashes.
#
# If you do not specify any arguments, this will default to whatever
# version of Nixpkgs you have in your configured channel.
#
# The nixpkgs-version argument should have two fields:
#  * rev: the Git revision to pin
#  * sha256: a SHA-256 hash to check the fetched version of Nixpkgs is correct
{ nixpkgs-revision ? null
, all-cabal-hashes-rev ? null
}:
let
  nixpkgs =
    if nixpkgs-revision == null
    then <nixpkgs>
    else (import <nixpkgs> {}).fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = nixpkgs-revision.rev;
      sha256 = nixpkgs-revision.sha256;
    };

  haskell-overlay = self: super: {
    all-cabal-hashes =
      if all-cabal-hashes-rev == null
      then super.all-cabal-hashes
      else builtins.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${all-cabal-hashes-rev}.tar.gz";
        name = "all-cabal-hashes-${all-cabal-hashes-rev}.tar.gz";
      };
  };
in
import nixpkgs { overlays = [haskell-overlay]; }
