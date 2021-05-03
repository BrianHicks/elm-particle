{ ... }:
let
  sources = import ./nix/sources.nix { };
  niv = import sources.niv { };
  pkgs = import sources.nixpkgs { };
in pkgs.mkShell {
  buildInputs = [
    niv.niv
    pkgs.bash
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.git
    pkgs.gnumake
  ];
}
