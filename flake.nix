{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages = {
          yamte = pkgs.stdenv.mkDerivation {
            name = "yamte";
            src = ./.;
            buildInputs = with pkgs; [ ncurses ];
            buildPhase = "g++ src/*.cpp -o yamte -Wall -Wextra -pedantic -lncurses";
            installPhase = "install -D yamte $out/bin/yamte";
          };
          yamte-debug = pkgs.stdenv.mkDerivation {
            name = "yamte-debug";
            src = ./.;
            buildInputs = with pkgs; [ ncurses ];
            buildPhase = "g++ src/*.cpp -o yamte -ggdb -O0 -Wall -Wextra -pedantic -lncurses";
            installPhase = "install -D yamte $out/bin/yamte";
            dontStrip = true;
          };
        };
        defaultPackage = packages.yamte;

        apps.yamte = utils.lib.mkApp { drv = packages.yamte; };
        defaultApp = apps.yamte;

        checks = packages;
      });
}
