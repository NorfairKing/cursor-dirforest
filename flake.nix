{
  description = "cursorDirforest";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.05";
    nixpkgs-24_11.url = "github:NixOS/nixpkgs?ref=nixos-24.11";
    nixpkgs-24_05.url = "github:NixOS/nixpkgs?ref=nixos-24.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    cursor.url = "github:NorfairKing/cursor";
    cursor.flake = false;
    dirforest.url = "github:NorfairKing/dirforest";
    dirforest.flake = false;
    cursor-brick.url = "github:NorfairKing/cursor-brick";
    cursor-brick.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-24_11
    , nixpkgs-24_05
    , pre-commit-hooks
    , cursor
    , dirforest
    , cursor-brick
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
          (import (cursor + "/nix/overlay.nix"))
          (import (dirforest + "/nix/overlay.nix"))
          (import (cursor-brick + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.cursorDirforestRelease;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs:
            let pkgs' = pkgsFor nixpkgs;
            in pkgs'.cursorDirforestRelease;
          allNixpkgs = {
            inherit
              nixpkgs-24_11
              nixpkgs-24_05;
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
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "cursor-dirforest-shell";
        packages = p: (builtins.attrValues p.cursorDirforestPackages);
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          cabal-install
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
