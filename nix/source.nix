{ lib, fetchFromGitHub, fetchgit, fetchzip, ... }:
with lib;
let
  make-src-filter = src: with lib;
    cleanSourceWith {
      inherit src;
      filter =
        name: type: let baseName = baseNameOf (toString name); in
         ((type == "regular" && hasSuffix ".hs" baseName) ||
         (hasSuffix ".yaml" baseName) ||
         (hasSuffix ".cabal" baseName) ||
         (hasSuffix ".css" baseName) ||
         (hasSuffix ".html" baseName) ||
         (hasSuffix ".png" baseName) ||
         (hasSuffix ".js" baseName) ||
         (baseName == "README.md") ||
         (baseName == "LICENSE") ||
         (type == "directory" && baseName != "examples") ||
         (type == "directory" && baseName != "dist"));
    };
in
{
  sse              = make-src-filter ../examples/sse;
  miso             = make-src-filter ../.;
  examples         = make-src-filter ../examples;
  sample-app       = make-src-filter ../sample-app;
  haskell-miso     = make-src-filter ../haskell-miso.org;
  miso-from-html = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-from-html";
    rev = "8c7635889ca0a5aaac36a8b21db7f5e5ec0ae4c9";
    sha256 = "0s6kzqxbshsnqbqfj7rblqkrr5mzkjxknb6k8m8z4h10mcv1zh7j";
  };
  jsaddle = fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "0d5e427cb99391179b143dc93dfbac9c1019237b";
    sha256 = "sha256-jyJ7bdz0gNLOSzRxOWcv7eWGIwo3N/O4PcY7HyNF8Fo=";
  };
  flatris = fetchFromGitHub {
    owner = "dmjio";
    repo = "hs-flatris";
    rev = "4d8ea97";
    sha256 = "sha256-VLbVdatgIxF680LVQ/nC9tVb9p9hlTCpXKp8U9vkss8=";
  };
  miso-plane = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-plane";
    rev = "cbeb74e";
    sha256 = "sha256-QhoeeTVO+zWwgUmLXvpd3mgtLa5+G36Eryxdx/ofgp4=";
  };
  the2048 = fetchFromGitHub {
    owner = "dmjio";
    repo = "hs2048";
    rev = "c8c19d6";
    sha256 = "sha256-SwLZX//1vj4qsPun7gz23gHWcyWKYl++Sw34zJLqKp4=";
  };
  snake = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-snake";
    rev = "d26c0a3";
    sha256 = "sha256-jD1kBqOMFrhxt+yUcXLTLL+qv71X4/uOI4qdkpO6nGA=";
  };
  todomvc-common = fetchFromGitHub {
    owner = "tastejs";
    repo = "todomvc-common";
    rev = "3b6156d98fdc7070fc284038ac10e4ec4a5d1d1f";
    sha256 = "1hvjvvrvbcfqgs77vg923w409cpkjisdyzxsd7b9zibrcqxz15l7";
  };
  todomvc-app-css = fetchFromGitHub {
    owner = "tastejs";
    repo = "todomvc-app-css";
    rev = "d99f1965654568c0ced7bcd54c0d5083e08fce9c";
    sha256 = "1fz4wac1if0lc54gvga640dybwlchi1ybzxw8yjz5836674mdkid";
  };
}
