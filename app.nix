{ mkDerivation, base, ghcjs-base, lens, linear, miso, stdenv }:
mkDerivation {
  pname = "miso-test";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ghcjs-base lens linear miso ];
  description = "First miso app";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
