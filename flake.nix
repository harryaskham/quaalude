{
  description = "quaalude";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        packageName = "quaalude";
      in with pkgs; rec {
        packages.${packageName} = haskellPackages.callCabal2nix "${packageName}" ./. {  };
        defaultPackage = packages.${packageName};
      }
    );
}
