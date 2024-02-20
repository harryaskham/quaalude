{ mkDerivation, aeson, aeson-casing, array, base, benchpress, bimap
, bitwise, bytestring, containers, distributive, extra, file-embed
, fin, hashable, lens, lib, linear, megaparsec, MissingH, mod
, monad-memo, MonadRandom, mtl, parsec, pqueue, random
, random-shuffle, relude, replace-megaparsec, safe, semirings
, split, string-qq, template-haskell, text, unordered-containers
, utility-ht, vector, z3
}:
mkDerivation {
  pname = "quaalude";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-casing array base benchpress bimap bitwise bytestring
    containers distributive extra file-embed fin hashable lens linear
    megaparsec MissingH mod monad-memo MonadRandom mtl parsec pqueue
    random random-shuffle relude replace-megaparsec safe semirings
    split string-qq template-haskell text unordered-containers
    utility-ht vector z3
  ];
  license = "unknown";
}
