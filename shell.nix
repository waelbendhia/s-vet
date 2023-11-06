{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {

  NODE_OPTIONS = "--openssl-legacy-provider";
  nativeBuildInputs = with pkgs; [
    zlib
  ];
}
