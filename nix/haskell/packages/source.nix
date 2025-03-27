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
  miso = make-src-filter ../../..;
  examples = make-src-filter ../../../examples;
  haskell-miso-src = make-src-filter ../../../haskell-miso.org;
  sample-app = make-src-filter ../../../sample-app;
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
    rev = "0b0e9ca";
    sha256 = "sha256-lCVbACOAAz77k1tI4DXRAMDJLJiWaPb7o7uio1YGAIk=";
  };
  miso-plane = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-plane";
    rev = "55e8445";
    sha256 = "sha256-6QU+SvE5Rpr/YJpp4+A0iwu3CwMYtZ0OdEdMJHHeo5U=";
  };
  the2048 = fetchFromGitHub {
    owner = "dmjio";
    repo = "hs2048";
    rev = "f5729cb";
    sha256 = "sha256-EJWAIFMhplF97jhY0wQ0ms0ZHrgnYWszPp8GE23RkC0=";
  };
  snake = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-snake";
    rev = "e83f9be";
    sha256 = "sha256-QvekR9JjHtHJW+52No2q8cZ9niXkakgFHmdOX3fd/n8=";
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
