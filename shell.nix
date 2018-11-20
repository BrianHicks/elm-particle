with import (builtins.fetchTarball rec {
  name = "nixpkgs-18.09";
  url = "https://github.com/nixos/nixpkgs/archive/18.09.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
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
