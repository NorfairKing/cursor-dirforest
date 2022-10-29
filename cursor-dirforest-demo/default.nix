{ mkDerivation, base, brick, containers, cursor, cursor-brick
, cursor-dirforest, cursor-dirforest-brick, directory, dirforest
, lib, microlens, path, path-io, pretty-show, unix, vty
}:
mkDerivation {
  pname = "cursor-dirforest-demo";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick containers cursor cursor-brick cursor-dirforest
    cursor-dirforest-brick directory dirforest microlens path path-io
    pretty-show unix vty
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/cursor-dirforest#readme";
  license = lib.licenses.mit;
}
