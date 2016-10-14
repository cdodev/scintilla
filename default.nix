{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, either, exceptions, free, hedis, lens, mmorph, mtl, opaleye
, pipes, pipes-concurrency, postgresql-simple, product-profunctors
, stdenv, tasty, tasty-hspec, tasty-quickcheck, text, thyme, tisch
, transformers, unordered-containers, uuid
}:
mkDerivation {
  pname = "scintilla";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers either exceptions free
    hedis lens mmorph mtl opaleye pipes pipes-concurrency
    postgresql-simple product-profunctors text thyme tisch transformers
    unordered-containers uuid
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers either exceptions free
    hedis lens mmorph mtl pipes pipes-concurrency postgresql-simple
    product-profunctors text transformers unordered-containers uuid
  ];
  testHaskellDepends = [
    base containers mmorph tasty tasty-hspec tasty-quickcheck
    unordered-containers
  ];
  homepage = "http://github.com/cdodev/scintilla";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
