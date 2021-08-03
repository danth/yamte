{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      with nixpkgs.lib;
      with nixpkgs.legacyPackages.${system}.haskellPackages;
      rec {
        packages = {
          yamte = mkDerivation {
            pname = "yamte";
            version = "0.1.0.0";
            src = ./.;
            isLibrary = false;
            isExecutable = true;
            executableHaskellDepends = [ base ilist ncurses skylighting ];
            license = "AGPL-3.0";
          };
        };
        defaultPackage = packages.yamte;

        apps.yamte = utils.lib.mkApp { drv = packages.yamte; };
        defaultApp = apps.yamte;

        checks = packages;
      });
}
