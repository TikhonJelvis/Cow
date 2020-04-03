# Pinned version of Nixpkgs
#  * Nixpkgs pinned to nixpkgs-unstable on 2020-04-03
#  * all-cabal-hashes pinned to latest version on 2020-04-03

{}:
import ./nixpkgs.nix {
  nixpkgs-revision = {
    rev = "05f0934825c2a0750d4888c4735f9420c906b388";
    sha256 = "1g8c2w0661qn89ajp44znmwfmghbbiygvdzq0rzlvlpdiz28v6gy";
  };
  all-cabal-hashes-rev = "90b24a91103dca4f0df6cb28cecb205a7d7ab650";
}
