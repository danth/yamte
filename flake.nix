{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachSystem ["aarch64-linux" "i686-linux" "x86_64-linux"]
    (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        ghc = pkgs.haskell.packages.ghc901.ghcWithPackages
          (haskellPackages: with haskellPackages; [
            brick
            data-default-class
            ilist
            microlens
            microlens-th
            skylighting
            vty
          ]);

        yamte = pkgs.stdenvNoCC.mkDerivation {
          name = "yamte";
          src = ./.;
          buildInputs = [ ghc ];
          buildPhase = ''
            cd app
            ghc -O -threaded -Wall Main.hs
          '';
          installPhase = ''
            install -D Main $out/bin/yamte
          '';
        };

        yamteApp = utils.lib.mkApp {
          drv = yamte;
          name = "yamte";
        };

      in {
        packages.yamte = yamte;
        defaultPackage = yamte;
        apps.yamte = yamteApp;
        defaultApp = yamteApp;
      });
}
