{ pkgs ? import nix/nixpkgs.nix {}
, compiler ? null
, haskellPackages ? import nix/haskell.nix { compiler = compiler; }
}:
haskellPackages.developPackage {
  name = "cow";
  root = (pkgs.lib.cleanSourceWith {
    src = ./.;
    filter = path: type:
      let
        name = baseNameOf (toString path);
        ignored = ["dist" "dist-newstyle"];
      in
        builtins.all (ignored-file: name != ignored-file) ignored &&
        !pkgs.lib.hasPrefix ".ghc.environment" name &&
        pkgs.lib.cleanSourceFilter path type;
  }).outPath;

  # Disable "smart" Nix-shell detection because it is impure (depends
  # on the IN_NIX_SHELL environment variable), which can cause
  # hard-to-debug Nix issues.
  #
  # Instead, we have an explicit shell.nix with extra shell-specific
  # settings.
  returnShellEnv = false;
}
