{ pkgs ? import ./nix/pkgs.nix { }
, withProfiling ? false
}:
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "pretty-types";
    src = ./.;
  };
  projectFileName = "cabal.project";
  compiler-nix-name = "ghc8103";
  pkg-def-extras = [ ];
}

