final: prev:
with final.lib;
with final.haskell.lib;
{
  cursorDirforestRelease = final.symlinkJoin {
    name = "cursor-dirforest-release";
    paths = attrValues final.haskellPackages.cursorDirforestPackages;
  };

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        let
          cursorDirforestPackages =
            let cursorDirforestPkg = name:
              buildStrictly (self.callPackage (../${name}) { });
            in
            final.lib.genAttrs [
              "cursor-dirforest"
              "cursor-dirforest-brick"
              "cursor-dirforest-demo"
              "cursor-dirforest-gen"
            ]
              cursorDirforestPkg;
        in
        { inherit cursorDirforestPackages; } // cursorDirforestPackages
    );
  });
}
