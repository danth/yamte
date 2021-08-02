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
      with nixpkgs.legacyPackages.${system};
      let
        configurePackages = packages:
          let
            packageConf = package:
              "${package}/lib/ghc-${ghc.version}/package.conf.d/*";
            packageConfs = concatStringsSep " " (map packageConf packages);
          in ''
            nixPackages="$PWD/package.conf.d"
            mkdir -p "$nixPackages"
            cp ${packageConfs} "$nixPackages/"
            ${ghc}/bin/ghc-pkg recache -f "$nixPackages"
            export GHC_PACKAGE_PATH="$nixPackages:"
            ${ghc}/bin/ghc-pkg list
          '';
      in rec {
        packages = {
          yamte = stdenv.mkDerivation {
            name = "yamte";
            src = ./app;
            configurePhase = configurePackages (with haskellPackages; [ ncurses ]);
            buildPhase = "${ghc}/bin/ghc -O -o yamte Main.hs";
            installPhase = "install -D yamte $out/bin/yamte";
          };
        };
        defaultPackage = packages.yamte;

        apps.yamte = utils.lib.mkApp { drv = packages.yamte; };
        defaultApp = apps.yamte;

        checks = packages;
      });
}
