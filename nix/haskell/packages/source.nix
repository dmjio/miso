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
    rev = "d569be43f92b9b8c01dc3ee4c41401ab406a2076";
    sha256 = "1m1xxy4l9ii91k1k504qkxh9k1ybprm1m66mkb9dqlwcpyhcccmv";
  };
  jsaddle-dom = fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle-dom";
    rev = "6ce23c5";
    sha256 = "1wpwf025czibkw6770c99zk7r30j6nh7jdzhzqbi2z824qyqzbnw";
  };
  ghcjs-dom = fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-dom";
    rev = "b8e483a";
    sha256 = "06qlbbhjd0mlv5cymp5q0rb69a333l0fcif5zwa83h94dh25c1g7";
  };
  webkit2gtk3-javascriptcore = fetchFromGitHub {
    owner = "gtk2hs";
    repo = "webkit-javascriptcore";
    rev = "5868624";
    sha256 = "0aj0cvcbnzrdi1ynahpb4034vadfrs99n5qa8dar1srhijv55g8b";
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
    rev = "07dbed79a012240bfe19b836b6d445bb16a0602a";
    sha256 = "00rqix5g8s8y6ngxnjskvcyj19g639havn9pgpkdpxp8ni6g7xsm";
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
