{ mkDerivation, base, bytestring, cborg, ghc-prim, pipes
, pipes-bytestring, pipes-parse, QuickCheck, serialise, stdenv
, tasty, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "pipes-cborg";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cborg ghc-prim pipes pipes-bytestring pipes-parse
    serialise transformers
  ];
  testHaskellDepends = [
    base bytestring cborg pipes pipes-bytestring QuickCheck serialise
    tasty tasty-quickcheck transformers
  ];
  homepage = "https://github.com/k0001/pipes-cborg";
  description = "Encode and decode cborg streams using the pipes and cborg libraries";
  license = stdenv.lib.licenses.bsd3;
}
