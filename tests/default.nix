{ misoPkgs ? import ../. {} }:

with misoPkgs;

let
  yarn2nixSrc = pkgs.fetchFromGitHub {
    owner = "mayflower";
    repo = "yarn2nix-moretea";
    rev = "2ecd70bef182cd1ad5f9ee0d9004b5bd1cbbd023";
    sha256 = "0ayrcayly97apl9cw74lxr53f6fgyjc8rzijgc75kvwxhq7fyhxf";
  };
  yarn2nix = import yarn2nixSrc { inherit pkgs; };
  deps =
    yarn2nix.mkYarnPackage {
      name = "deps";
      src = ./.;
      packageJSON = ./package.json;
      yarnLock = ./yarn.lock;
    };
in
  pkgs.stdenv.mkDerivation rec {
    name = "coverage";
    src = ./.;
    buildCommand = ''
      mkdir -p $out
      cp ${miso-ghcjs.src}/jsbits/diff.js $out
      chmod +w $out/diff.js
      cp ${miso-ghcjs.src}/jsbits/isomorphic.js $out
      cp ${src}/package.json $out
      cp ${src}/diff.test.js $out
      cp -r ${deps}/libexec/miso/node_modules $out
      echo 'module.exports = diff;' >> $out/diff.js
      cd $out
      ${deps}/libexec/miso/node_modules/jest/./bin/jest.js --collectCoverage=true
      cp -rv $out/coverage/lcov-report $out
    '';
  }
