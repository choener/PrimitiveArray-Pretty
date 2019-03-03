{ mkDerivation, base, containers, diagrams, diagrams-contrib
, diagrams-lib, diagrams-postscript, diagrams-svg, filepath
, log-domain, QuickCheck, smallcheck, split, stdenv, tasty
, tasty-quickcheck, tasty-smallcheck, tasty-th
}:
mkDerivation {
  pname = "PrimitiveArray-Pretty";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers diagrams diagrams-contrib diagrams-lib
    diagrams-postscript diagrams-svg filepath log-domain split
  ];
  testHaskellDepends = [
    base QuickCheck smallcheck tasty tasty-quickcheck tasty-smallcheck
    tasty-th
  ];
  homepage = "https://github.com/choener/PrimitiveArray-Pretty";
  description = "Pretty-printing for primitive arrays";
  license = stdenv.lib.licenses.bsd3;
}
