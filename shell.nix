# shell.nix
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  ormolu = import sources.ormolu {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell.compiler.ghc8101
    pkgs.zlib
    pkgs.hlint
    ormolu.ormolu
  ];
}
