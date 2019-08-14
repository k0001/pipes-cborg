{ pkgs ? import ./pkgs.nix {} }:
{ inherit (pkgs._here.ghc) pipes-cborg; }
