{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, either, errors, free, hedis, lens, mmorph, mtl, opaleye
, opaleye-sot, pipes, pipes-concurrency, postgresql-simple, stdenv
, tasty, tasty-hspec, tasty-quickcheck, text, thyme, transformers
, unordered-containers, uuid
}:
mkDerivation {
  pname = "scintilla";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec base bytestring containers either errors free
    hedis lens mmorph mtl opaleye opaleye-sot pipes pipes-concurrency
    postgresql-simple text thyme transformers unordered-containers uuid
  ];
  testDepends = [
    base containers mmorph tasty tasty-hspec tasty-quickcheck
    unordered-containers
  ];
  homepage = "http://github.com/cdodev/scintilla";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
