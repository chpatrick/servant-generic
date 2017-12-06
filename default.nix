{ pkgs ? import <nixpkgs> {} }:
let drv = pkgs.haskell.packages.ghc821.callCabal2nix "servant-generic" ./. {};
in if pkgs.lib.inNixShell then drv.env else drv