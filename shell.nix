{ pkgs ? import <nixpkgs> {} }:

let
  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.parsec
    p.X11
    p.xmonad
    p.xmonad-contrib
  ]);
in

pkgs.mkShell {
  buildInputs = [
    ghc
    pkgs.texlive.combined.scheme-full
    pkgs.haskellPackages.lhs2tex
  ];
}
