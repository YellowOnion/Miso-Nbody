{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
	owner = "NixOS";
	repo = "nixpkgs";
	rev = "a0aeb23";
	sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
}:
let
  overrides =
            (self: super:
              {
              doctest = null;
              linear = pkgs.haskell.lib.dontCheck (super.linear.override { doctest = null; });
              bytes = pkgs.haskell.lib.dontCheck (super.bytes.override { doctest = null; });
              }
            );
  ghcjsPkgs = pkgs.haskell.packages.ghcjs.override { inherit overrides; };
  result = import (pkgs.fetchFromGitHub {
	owner = "dmjio";
	repo = "miso";
	sha256 = "1l1gwzzqlvvcmg70jjrwc5ijv1vb6y5ljqkh7rxxq7hkyxpjyx9q";
	rev = "95f6bc9b1ae6230b110358a82b6a573806f272c2";
  }) {};
in ghcjsPkgs.callPackage ./app.nix {
  miso = result.miso-ghcjs;
  }
