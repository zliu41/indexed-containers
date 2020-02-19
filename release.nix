{ compiler ? "default" }:

let
  pkgs = import <nixpkgs> { };

in
  { indexed-containers = pkgs.haskellPackages.callPackage ./default.nix { inherit compiler; };
  }
