{ mkDerivation, aeson, attoparsec, base, binary, BiobaseTypes
, BiobaseXNA, bytestring, cereal, cereal-text, cereal-vector
, cmdargs, containers, criterion, data-default, deepseq, DPutils
, filepath, hashable, HUnit, lens, lib, parallel, pipes
, pipes-attoparsec, pipes-bytestring, pipes-parse, pipes-safe
, pipes-zlib, primitive, PrimitiveArray, QuickCheck, strict
, string-conversions, tasty, tasty-hunit, tasty-quickcheck
, tasty-th, text, text-binary, transformers, tuple
, unordered-containers, utf8-string, vector, vector-th-unbox, zlib
}:
mkDerivation {
  pname = "BiobaseInfernal";
  version = "0.8.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base binary BiobaseTypes BiobaseXNA bytestring
    cereal cereal-text cereal-vector containers data-default deepseq
    DPutils filepath hashable lens parallel pipes pipes-attoparsec
    pipes-bytestring pipes-parse pipes-safe pipes-zlib primitive
    PrimitiveArray strict string-conversions text text-binary
    transformers tuple unordered-containers utf8-string vector
    vector-th-unbox zlib
  ];
  executableHaskellDepends = [ base cmdargs ];
  testHaskellDepends = [
    base HUnit lens QuickCheck tasty tasty-hunit tasty-quickcheck
    tasty-th
  ];
  benchmarkHaskellDepends = [
    base criterion lens text transformers
  ];
  homepage = "https://github.com/choener/BiobaseInfernal";
  description = "Infernal data structures and tools";
  license = lib.licenses.gpl3Only;
}
