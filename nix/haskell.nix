{ pkgs ? import ./nixpkgs.nix {}
, compiler ? null
}:
if compiler == null
then pkgs.haskellPackages
else pkgs.haskell.packages.${compiler}
