# GHC 9.10 with every dependency in package.yaml, used to build quaalude and record the site's
# GHCi output. Pinned to one nixpkgs revision so the reference regenerates identically.
# Packages nixpkgs marks broken are built with bounds relaxed and their tests skipped.
let
  src = builtins.fetchTree {
    type = "github"; owner = "NixOS"; repo = "nixpkgs";
    rev = "c043004d1c6985732bcc1cbc5a9c9aecbbb4e0f0";
    narHash = "sha256-7QYjT2vHLuX9Z1pdxHXDKCbh1CR3D/2rywB9Tx0MPRg=";
  };
  pkgs = import src { config.allowBroken = true; };
  hl = pkgs.haskell.lib.compose;
  fix = p: hl.dontCheck (hl.doJailbreak (hl.markUnbroken p));
  hp = pkgs.haskellPackages.override {
    overrides = self: super: builtins.mapAttrs (_: fix) {
      inherit (super) groupBy variant HList overhang benchpress z3 aeson-gadt-th named bitwise;
    };
  };
in hp.ghcWithPackages (p: with p; [
  relude pqueue bytestring parsec megaparsec replace-megaparsec groupBy vector vector-space array extra
  split data-default text containers unordered-containers bifunctors mtl bimap mod semirings distributive
  OneTuple template-haskell file-embed utility-ht monad-memo benchpress fin MissingH safe linear lens
  bitwise random ansi-terminal random-shuffle MonadRandom string-qq binary-search variant hashable aeson
  deriving-aeson aeson-gadt-th some dependent-sum-template raw-strings-qq aeson-casing barbies req HList
  tagged named overhang z3 mono-traversable hspec
])
