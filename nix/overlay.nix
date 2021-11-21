final:
previous:
with final.haskell.lib;
{
  cursorDirforestPackages =
    let cursorDirforestPkg = name:
      (buildStrictly (final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }));
    in
    final.lib.genAttrs [
      "cursor-dirforest"
      "cursor-dirforest-brick"
      "cursor-dirforest-demo"
      "cursor-dirforest-gen"
    ]
      cursorDirforestPkg;
  haskellPackages = previous.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super: final.cursorDirforestPackages
    );
  });
}
