with import (builtins.fetchGit {
  name = "nixos-unstable-2018-11-16";
  url = https://github.com/nixos/nixpkgs/;
  rev = "6141939d6e0a77c84905efd560c03c3032164ef1";
}) {};

stdenv.mkDerivation {
  name = "elm-particle";
  buildInputs = [
    bash
    elmPackages.elm
    elmPackages.elm-format
    git
    gnumake
  ];
}
