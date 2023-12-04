{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs.buildPackages;
    [
      chicken
      chickenPackages_5.chickenEggs.chicken-doc
      chickenPackages_5.chickenEggs.breadline
      chickenPackages_5.chickenEggs.defstruct
      chickenPackages_5.chickenEggs.srfi-113
    ];
}
