with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "teambot";
  buildInputs = [
    git
    elmPackages.elm
    elmPackages.elm-format
  ];
}
