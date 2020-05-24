let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay =
    import (
      (
        pkgs.fetchFromGitHub (import ./validity-version.nix)
        + "/nix/overlay.nix"
      )
    );
  cursor-overlay =
    import (
      (
        pkgs.fetchFromGitHub (import ./cursor-version.nix)
        + "/nix/overlay.nix"
      )
    );
  dirforest-overlay =
    import (
      (
        pkgs.fetchFromGitHub (import ./dirforest-version.nix)
        + "/nix/overlay.nix"
      )
    );
  cursor-brick-overlay =
    import (
      (
        pkgs.fetchFromGitHub (import ./cursor-brick-version.nix)
        + "/nix/overlay.nix"
      )
    );
in
pkgsv {
  overlays =
    [
      validity-overlay
      cursor-overlay
      dirforest-overlay
      cursor-brick-overlay
      (import ./overlay.nix)
      (import ./gitignore-src.nix)

    ];
}
