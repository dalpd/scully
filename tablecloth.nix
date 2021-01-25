{ mkDerivation, base, ghcid, nixfmt, stdenv }:
mkDerivation {
  pname = "tablecloth";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ghcid nixfmt ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
  shellHook = ''
      hpack
    '';
}
