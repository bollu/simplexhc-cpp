{ }:

let
  pkgs = import <nixpkgs> { };
in
  pkgs.stdenv.mkDerivation {
    name = "simplexhc-1.0.0";
    src = ./.;
    buildInputs = [ pkgs.cmake pkgs.flex pkgs.bison pkgs.boost pkgs.llvm ];
  }
