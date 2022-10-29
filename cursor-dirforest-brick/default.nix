{ mkDerivation, base, brick, containers, cursor, cursor-brick
, cursor-dirforest, dirforest, lib, vty
}:
mkDerivation {
  pname = "cursor-dirforest-brick";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base brick containers cursor cursor-brick cursor-dirforest
    dirforest vty
  ];
  homepage = "https://github.com/NorfairKing/cursor-dirforest#readme";
  license = lib.licenses.mit;
}
