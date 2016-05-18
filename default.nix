{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, containers, diagrams-contrib
      , diagrams-lib, diagrams-svg, lens, mtl, parsec, stdenv
      }:
      mkDerivation {
        pname = "cow";
        version = "0.3.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          array base containers diagrams-contrib diagrams-lib diagrams-svg
          lens mtl parsec
        ];
        executableHaskellDepends = [ base diagrams-svg lens parsec ];
        homepage = "http://jelv.is/cow";
        description = "Semantic version control: language-aware diff and merge";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
