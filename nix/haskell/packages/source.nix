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
    rev = "088a95d";
    sha256 = "sha256-5R/LTgH+jVHQWT1Mk9erC7wJPj2R1z7snrNQeun7eCs=";
  };
  miso-plane = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-plane";
    rev = "225ce5b7ddd57e716b96f8029dbef16d7dda15fd";
    sha256 = "sha256-DuDDM0ePZCIASX8TB0YxIivXMhV7x7uaPb/HbJJILUs=";
  };
  the2048 = fetchFromGitHub {
    owner = "dmjio";
    repo = "hs2048";
    rev = "8a6c951e75bea7502287b809e66bfe8f1db77a7c";
    sha256 = "sha256-O3AqxsyrwvUMb7Nnz3KDUgRYJtgJouunnONqR3SIgNQ=";
  };
  snake = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-snake";
    rev = "5740e0f";
    sha256 = "sha256-qStRWDP0jFccPhVUWoP7o1P8+MvJq7cJSDUuBmUfsyg=";
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
