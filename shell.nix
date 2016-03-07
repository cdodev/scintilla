{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:

let

  inherit (nixpkgs) pkgs;

  drv = pkgs.haskell.packages.${compiler}.callPackage ./. {};

in

  if pkgs.lib.inNixShell then drv.env else drv
