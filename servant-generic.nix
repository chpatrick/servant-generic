{ mkDerivation, base, servant, servant-server, text, warp, stdenv }:
mkDerivation {
  pname = "servant-generic";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base servant servant-server text warp ];
  license = stdenv.lib.licenses.bsd3;
}
