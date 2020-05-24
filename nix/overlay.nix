final: previous:
with final.haskell.lib;

{
  cursorDirforestPackages =
    {
      cursor-dirforest =
        failOnAllWarnings (
          final.haskellPackages.callCabal2nix "cursor-dirforest" (final.gitignoreSource ../cursor-dirforest) {}
        );
      cursor-dirforest-gen =
        doBenchmark (
          failOnAllWarnings (
            final.haskellPackages.callCabal2nix "cursor-dirforest-gen" (final.gitignoreSource ../cursor-dirforest-gen) {}
          )
        );
      cursor-dirforest-brick =
        failOnAllWarnings (
          final.haskellPackages.callCabal2nix "cursor-dirforest-brick" (final.gitignoreSource ../cursor-dirforest-brick) {}
        );
    };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (old.overrides or (_: _: {})) (
              self: super: final.cursorDirforestPackages
            );
        }
    );
}
