{
  description = "cursor-dirforest";
  nixConfig = {
    extra-substituters = "https://cursor-dirforest.cachix.org";
    extra-trusted-public-keys = "cursor-dirforest.cachix.org-1:f/YRo6J0qWCFwGEzoONSWXs0hIYYZdPFuZaN6zm0nzQ=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    cursor.url = "github:NorfairKing/cursor?ref=flake";
    cursor.flake = false;
    dirforest.url = "github:NorfairKing/dirforest?ref=flake";
    dirforest.flake = false;
    cursor-brick.url = "github:NorfairKing/cursor-brick?ref=flake";
    cursor-brick.flake = false;
    nixpkgs-22_05.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    nixpkgs-21_11.url = "github:NixOS/nixpkgs?ref=nixos-21.11";
    nixpkgs-21_05.url = "github:NixOS/nixpkgs?ref=nixos-21.05";
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-22_05
    , nixpkgs-21_11
    , nixpkgs-21_05
    , flake-utils
    , pre-commit-hooks
    , validity
    , dirforest
    , cursor
    , cursor-brick
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
      let
        pkgsFor = nixpkgs: import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.${system}
            (import (validity + "/nix/overlay.nix"))
            (import (dirforest + "/nix/overlay.nix"))
            (import (cursor + "/nix/overlay.nix"))
            (import (cursor-brick + "/nix/overlay.nix"))
          ];
        };
        pkgs = pkgsFor nixpkgs;
      in
      {
        overlays = import ./nix/overlay.nix;
        packages.default = pkgs.cursorDirforestRelease;
        checks =
          let
            backwardCompatibilityCheckFor = nixpkgs:
              let pkgs' = pkgsFor nixpkgs;
              in pkgs'.cursorDirforestRelease;
            allNixpkgs = {
              inherit
                nixpkgs-22_05
                nixpkgs-21_11
                nixpkgs-21_05;
            };
            backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
          in
          backwardCompatibilityChecks // {
            pre-commit = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                hlint.enable = true;
                hpack.enable = true;
                ormolu.enable = true;
                nixpkgs-fmt.enable = true;
                nixpkgs-fmt.excludes = [ ".*/default.nix" ];
                cabal2nix.enable = true;
              };
            };
          };
        devShells.default = pkgs.haskellPackages.shellFor {
          name = "cursor-dirforest-shell";
          packages = (p:
            (builtins.attrValues p.cursorDirforestPackages)
          );
          withHoogle = true;
          doBenchmark = true;
          buildInputs = with pkgs; [
            niv
            zlib
            cabal-install
          ] ++ (with pre-commit-hooks;
            [
              hlint
              hpack
              nixpkgs-fmt
              ormolu
              cabal2nix
            ]);
          shellHook = self.checks.${system}.pre-commit.shellHook;
        };
      });
}
