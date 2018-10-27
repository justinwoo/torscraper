let
  pkgs = import <nixpkgs> {};

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "45050a69fc60b9e7259053a9d68d42a0e47dbf33";
    sha256 = "1ajnsiclzv4xcjd6dn6p8bwqmyjib7rjrya0m57gj1dwkzn9z3lk";
  });
in pkgs.stdenv.mkDerivation {
  name = "easy-purescript";
  src = ./.;

  buildInputs = [
    easy-ps.inputs.purs
    easy-ps.inputs.purp
    easy-ps.inputs.psc-package-simple
  ];
}
