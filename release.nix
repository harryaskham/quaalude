{ pkgs ? import <nixpkgs> { config = { allowBroken = true; }; } }:
pkgs.haskellPackages.callCabal2nix "quaalude" ./. {  }
