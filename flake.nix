{
  inputs = {
    haskell.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { haskell, nixpkgs, utils, ... }:
    # Cannot support i686-linux as haskell.nix does not support it
    # Cannot support aarch64-linux due to input-output-hk/haskell.nix#1189
    utils.lib.eachSystem ["x86_64-linux"]
    (system:
      let
        overlays = [
          haskell.overlay
          (final: prev: {
            yamte = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc901";
              modules = [{
                packages.ncurses.components.library.libs =
                  final.lib.mkForce [ final.ncurses ];
              }];
            };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        flake = pkgs.yamte.flake {};
      in rec {
        inherit (flake) packages apps;
        defaultPackage = packages."yamte:exe:yamte";
        defaultApp = apps."yamte:exe:yamte";
      });
}
