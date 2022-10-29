{ mkDerivation, base, containers, cursor, deepseq, dirforest
, filepath, lib, microlens, path, text, validity, validity-path
}:
mkDerivation {
  pname = "cursor-dirforest";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers cursor deepseq dirforest filepath microlens path
    text validity validity-path
  ];
  homepage = "https://github.com/NorfairKing/cursor-dirforest#readme";
  license = lib.licenses.mit;
}
