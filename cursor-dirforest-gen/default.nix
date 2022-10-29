{ mkDerivation, base, containers, criterion, cursor
, cursor-dirforest, cursor-gen, dirforest, filepath, genvalidity
, genvalidity-containers, genvalidity-criterion
, genvalidity-dirforest, genvalidity-hspec
, genvalidity-hspec-optics, hspec, lib, path, QuickCheck
}:
mkDerivation {
  pname = "cursor-dirforest-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers cursor cursor-dirforest cursor-gen dirforest
    filepath genvalidity genvalidity-containers genvalidity-dirforest
    path QuickCheck
  ];
  testHaskellDepends = [
    base cursor cursor-dirforest cursor-gen dirforest genvalidity-hspec
    genvalidity-hspec-optics hspec path QuickCheck
  ];
  benchmarkHaskellDepends = [
    base criterion cursor-dirforest genvalidity-criterion QuickCheck
  ];
  homepage = "https://github.com/NorfairKing/cursor-dirforest#readme";
  license = lib.licenses.mit;
}
