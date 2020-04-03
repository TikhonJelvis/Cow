{ pkgs ? import nix/nixpkgs.nix {}
, compiler ? null
, haskellPackages ? import ./haskell.nix { compiler = compiler; }
, cow ? import ./. { compiler = compiler; }
}:
let
  haskellPackages =
    if compiler == null
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  # Extra development tools to expose inside a Nix shell:
  haskellDevelopmentTools = with haskellPackages;
    [ cabal-install stylish-haskell ];
in    
pkgs.lib.overrideDerivation cow.env (old: {
  nativeBuildInputs = old.nativeBuildInputs ++ haskellDevelopmentTools;
})
