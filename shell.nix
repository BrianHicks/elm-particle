with import (builtins.fetchTarball rec {
  # grab a hash from here: https://nixos.org/channels/
  name = "nixpkgs-darwin-18.09pre153446.836d1d7c1cc";
  url = "https://github.com/nixos/nixpkgs/archive/836d1d7c1cce26eda233dc925c8093e5a5d86ad3.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "1vibh9k2swmsq07v2fsqxhc0lxn0gnk48jsykxbb42bvlsxisxdi";
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
