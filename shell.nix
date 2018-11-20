with import (builtins.fetchTarball rec {
  # grab a hash from here: https://nixos.org/channels/
  name = "nixpkgs-18.09-darwin-2018-11-20";
  url = "https://github.com/nixos/nixpkgs/archive/f7da99c4b9680254e05aa67b5aa3a60a2cbdbccd.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "1322q71zs6c791bmniv9n21dds5l1yki111i24syypky2529s7m7";
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
