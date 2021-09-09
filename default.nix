let nixpkgs = import <nixpkgs> {};
in nixpkgs.haskellPackages.callPackage ./project.nix {}
