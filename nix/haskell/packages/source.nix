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
  haskell-miso-src = make-src-filter ../../../examples/haskell-miso.org;
  jsaddle = fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "c4b7c6cebf09447a25105d7c839914d7378bf5a7";
    sha256 = "0p57n94w196cigncppq8wldha5rrslhgp5i5pfnd2klj6nwq77dv";
  };
  jsaddle-dom = fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle-dom";
    rev = "fa40dd223c3128e7f2560e4f49486ca1673b78f6";
    sha256 = "0zczgvmg3vvvdf9q0h7z3yx6fj117rhf0ljcz68g2v4w6a6kmlkb";
  };
  ghcjs-dom = fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-dom";
    rev = "90779f212a6fdc6a4884aff6a0e6b8d2e8850513";
    sha256 = "0py54fyp4zrnabgdnvwvdliyz65hi20ii1qyalwzji4f9dd3cqlf";
  };
  webkit2gtk3-javascriptcore = fetchFromGitHub {
    owner = "gtk2hs";
    repo = "webkit-javascriptcore";
    rev = "b092a700f04b1001f7f7b58c2f90392a3da5c410";
    sha256 = "0yshacbjxgkdv8hd83chf4qshf9rv5my14vz66ja1883dwignsbj";
  };
  ref-tf = fetchFromGitHub {
    owner = "mainland";
    repo = "ref-tf";
    rev = "2405763a78cb49839a25e0da5e15bb7524c8b224";
    sha256 = "0j28rq8706yc5mm2753jnxn81p43j2bvwblc3ys790njxarc03v5";
  };
  flatris = fetchFromGitHub {
    repo = "hs-flatris";
    owner = "ptigwe";
    rev = "5b386e35db143205b4bd8d45cdf98423ed51b713";
    sha256 = "0wll5fizkdmj2hgd71v9klnnr6wxvvf36imchh2chm1slqm78zca";
  };
  miso-plane = fetchFromGitHub {
    repo = "miso-plane";
    owner = "dmjio";
    rev = "9fc9a913a5cf702e65d37068c95d955b001fe3ec";
    sha256 = "1k112f2gnk3q3j6c3zkql9wdgkv2mbbyb2jrn5192w5vy09vqh40";
  };
  the2048 = fetchFromGitHub {
    repo = "hs2048";
    owner = "dmjio";
    rev = "0d91626e01aba742874e5ae6ba1405d2ce15e68d";
    sha256 = "1fbrs1dmd8s7l1p34g1nmignga9a9ifr8jhgmckzdrywx6f5k583";
  };
  snake = fetchFromGitHub {
    repo = "miso-snake";
    owner = "dmjio";
    rev = "c38947cd9417ab8bf8a8d3652d8bf549e35f14af";
    sha256 = "17rdc7fisqgf8zq90c3cw9c08b1qac6wirqmwifw2a0xxbigz4qc";
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
