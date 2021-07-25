{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages.yamte = pkgs.stdenv.mkDerivation {
          name = "yamte";
          src = ./.;
          buildInputs = with pkgs; [ ncurses ];
          buildPhase = "cc yamte.c -o yamte -Wall -Wextra -pedantic -std=c99 -lncurses";
          installPhase = "install -D yamte $out/bin/yamte";
        };
        defaultPackage = packages.yamte;

        apps.yamte = utils.lib.mkApp { drv = packages.yamte; };
        defaultApp = apps.yamte;

        checks = packages;
      });
}
