/* stackage-packages.nix is an auto-generated file -- DO NOT EDIT! */
{ pkgs, stdenv, callPackage }:
self: {
"BoundedChan" = callPackage ({ mkDerivation, array, base, stdenv }:
mkDerivation {
  pname = "BoundedChan";
  version = "1.0.3.0";
  sha256 = "0vf4mlw08n056g5256cf46m5xsijng5gvjx7ccm4r132gznyl72k";
  libraryHaskellDepends = [ array base ];
  doHaddock = false;
  doCheck = false;
  description = "Implementation of bounded channels";
  license = stdenv.lib.licenses.bsd3;
}){};
"aeson" = callPackage ({ mkDerivation, attoparsec, base, bytestring, containers, deepseq
, dlist, fail, ghc-prim, hashable, mtl, nats, scientific
, semigroups, stdenv, syb, tagged, template-haskell, text, time
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "aeson";
  version = "0.11.2.1";
  sha256 = "0k5p06pik7iyjm1jjkjbpqqn0mqps6b8mz9p9sp9hmganl4cffyc";
  revision = "1";
  editedCabalFile = "04sydhx056gpakm39xk7s849qjr218ai1sjj2zr7n0yxxm1sqzz9";
  libraryHaskellDepends = [
    attoparsec base bytestring containers deepseq dlist fail ghc-prim
    hashable mtl nats scientific semigroups syb tagged template-haskell
    text time transformers unordered-containers vector
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}){};
"array" = callPackage ({ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "array";
  version = "0.5.1.0";
  sha256 = "18hz1jcshdj6c10lsxq86rs6rbx77g91w4ay2s58h9j5rnkchjxq";
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  doCheck = false;
  description = "Mutable and immutable arrays";
  license = stdenv.lib.licenses.bsd3;
}){};
"attoparsec" = callPackage ({ mkDerivation, array, base, bytestring, containers, deepseq, fail
, scientific, semigroups, stdenv, text, transformers
}:
mkDerivation {
  pname = "attoparsec";
  version = "0.13.1.0";
  sha256 = "0r1zrrkbqv8w4pb459fj5izd1h85p9nrsp3gyzj7qiayjpa79p2j";
  libraryHaskellDepends = [
    array base bytestring containers deepseq fail scientific semigroups
    text transformers
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/bos/attoparsec";
  description = "Fast combinator parsing for bytestrings and text";
  license = stdenv.lib.licenses.bsd3;
}){};
"base" = callPackage ({ mkDerivation, ghc-prim, rts, stdenv }:
mkDerivation {
  pname = "base";
  version = "4.8.2.0";
  sha256 = "137yz9jd7phd2khvcr4hdg6cq15p8wxxqcjzl8qw4x1zfyr9xg7j";
  libraryHaskellDepends = [ ghc-prim rts ];
  doHaddock = false;
  doCheck = false;
  description = "Basic libraries";
  license = stdenv.lib.licenses.bsd3;
}){};
"base-compat" = callPackage ({ mkDerivation, base, stdenv, unix }:
mkDerivation {
  pname = "base-compat";
  version = "0.9.1";
  sha256 = "0jj6nq0vb8ap3724c3r3cavc298m1gm238vmgi7wzzxr8s0v8cqh";
  libraryHaskellDepends = [ base unix ];
  doHaddock = false;
  doCheck = false;
  description = "A compatibility layer for base";
  license = stdenv.lib.licenses.mit;
}){};
"binary" = callPackage ({ mkDerivation, array, base, bytestring, containers, stdenv }:
mkDerivation {
  pname = "binary";
  version = "0.7.5.0";
  sha256 = "06gg61srfva7rvzf4s63c068s838i5jf33d6cnjb9769gjmca2a7";
  libraryHaskellDepends = [ array base bytestring containers ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/kolmodin/binary";
  description = "Binary serialisation for Haskell values using lazy ByteStrings";
  license = stdenv.lib.licenses.bsd3;
}){};
"blaze-builder" = callPackage ({ mkDerivation, base, bytestring, deepseq, stdenv, text }:
mkDerivation {
  pname = "blaze-builder";
  version = "0.4.0.2";
  sha256 = "1m33y6p5xldni8p4fzg8fmsyqvkfmnimdamr1xjnsmgm3dkf9lws";
  libraryHaskellDepends = [ base bytestring deepseq text ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://github.com/lpsmith/blaze-builder";
  description = "Efficient buffered output";
  license = stdenv.lib.licenses.bsd3;
}){};
"bytestring" = callPackage ({ mkDerivation, base, deepseq, ghc-prim, stdenv }:
mkDerivation {
  pname = "bytestring";
  version = "0.10.6.0";
  sha256 = "0xw924djdbs15r4dh2zyn209b0ji94si4ywliagjbg41gdmrl6r7";
  libraryHaskellDepends = [ base deepseq ghc-prim ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/haskell/bytestring";
  description = "Fast, compact, strict and lazy byte strings with a list interface";
  license = stdenv.lib.licenses.bsd3;
}){};
"bytestring-conversion" = callPackage ({ mkDerivation, attoparsec, base, bytestring, case-insensitive
, double-conversion, stdenv, text
}:
mkDerivation {
  pname = "bytestring-conversion";
  version = "0.3.1";
  sha256 = "1y2fhwz632sp7n0iw87lz2g8vks4jgxz2kw99kysgivxfd4fmdqk";
  revision = "2";
  editedCabalFile = "1x8c42cfzb6fdvgkxxdxcpdf16csimlzsgahb1axnplmr6b3ba63";
  libraryHaskellDepends = [
    attoparsec base bytestring case-insensitive double-conversion text
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/twittner/bytestring-conversion/";
  description = "Type-classes to convert values to and from ByteString";
  license = stdenv.lib.licenses.mpl20;
}){};
"cabal-doctest" = callPackage ({ mkDerivation, base, Cabal, directory, filepath, stdenv }:
mkDerivation {
  pname = "cabal-doctest";
  version = "1.0.2";
  sha256 = "0h3wsjf2mg8kw1zvxc0f9nzchj5kzvza9z0arcyixkd9rkgqq6sa";
  libraryHaskellDepends = [ base Cabal directory filepath ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/phadej/cabal-doctest";
  description = "A Setup.hs helper for doctests running";
  license = stdenv.lib.licenses.bsd3;
}){};
"case-insensitive" = callPackage ({ mkDerivation, base, bytestring, deepseq, hashable, semigroups
, stdenv, text
}:
mkDerivation {
  pname = "case-insensitive";
  version = "1.2.0.7";
  sha256 = "1j6ahvrz1g5q89y2difyk838yhwjc8z67zr0v2z512qdznc3h38n";
  libraryHaskellDepends = [
    base bytestring deepseq hashable semigroups text
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/basvandijk/case-insensitive";
  description = "Case insensitive string comparison";
  license = stdenv.lib.licenses.bsd3;
}){};
"containers" = callPackage ({ mkDerivation, array, base, deepseq, stdenv }:
mkDerivation {
  pname = "containers";
  version = "0.5.6.2";
  sha256 = "1r9dipm2bw1dvdjyb2s1j9qmqy8xzbldgiz7a885fz0p1ygy9bdi";
  revision = "1";
  editedCabalFile = "1z5i6xxkq88vwd0xir5nzqrg0ip665wagcivnnrchy8flypdvmyi";
  libraryHaskellDepends = [ array base deepseq ];
  doHaddock = false;
  doCheck = false;
  description = "Assorted concrete container types";
  license = stdenv.lib.licenses.bsd3;
}){};
"deepseq" = callPackage ({ mkDerivation, array, base, stdenv }:
mkDerivation {
  pname = "deepseq";
  version = "1.4.1.1";
  sha256 = "1gxk6bdqfplvq0lma2sjcxl8ibix5k60g71dncpvwsmc63mmi0ch";
  revision = "1";
  editedCabalFile = "095j5l1p955ksmkr0fx7554kd96457g7ad61i2w619m03vh397db";
  libraryHaskellDepends = [ array base ];
  doHaddock = false;
  doCheck = false;
  description = "Deep evaluation of data structures";
  license = stdenv.lib.licenses.bsd3;
}){};
"dlist" = callPackage ({ mkDerivation, base, deepseq, stdenv }:
mkDerivation {
  pname = "dlist";
  version = "0.7.1.2";
  sha256 = "10rp96rryij7d8gz5kv8ygc6chm1624ck5mbnqs2a3fkdzqj2b9k";
  libraryHaskellDepends = [ base deepseq ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/spl/dlist";
  description = "Difference lists";
  license = stdenv.lib.licenses.bsd3;
}){};
"double-conversion" = callPackage ({ mkDerivation, base, bytestring, ghc-prim, stdenv, text }:
mkDerivation {
  pname = "double-conversion";
  version = "2.0.1.0";
  sha256 = "034ji9jgf3jl0n5pp1nki3lsg173c3b9vniwnwp1q21iasqbawh0";
  libraryHaskellDepends = [ base bytestring ghc-prim text ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/bos/double-conversion";
  description = "Fast conversion between double precision floating point and text";
  license = stdenv.lib.licenses.bsd3;
}){};
"fail" = callPackage ({ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "fail";
  version = "4.9.0.0";
  sha256 = "18nlj6xvnggy61gwbyrpmvbdkq928wv0wx2zcsljb52kbhddnp3d";
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail";
  description = "Forward-compatible MonadFail class";
  license = stdenv.lib.licenses.bsd3;
}){};
"ghc-prim" = callPackage ({ mkDerivation, rts, stdenv }:
mkDerivation {
  pname = "ghc-prim";
  version = "0.4.0.0";
  sha256 = "1w3hkl1xyfi67kh65gqn99pinxrfqjl2s08yg0010r907w3qys31";
  libraryHaskellDepends = [ rts ];
  doHaddock = false;
  doCheck = false;
  description = "GHC primitives";
  license = stdenv.lib.licenses.bsd3;
}){};
"happy" = callPackage ({ mkDerivation, array, base, Cabal, containers, directory, filepath
, mtl, stdenv
}:
mkDerivation {
  pname = "happy";
  version = "1.19.5";
  sha256 = "1nj353q4z1g186fpjzf0dnsg71qhxqpamx8jy89rjjvv3p0kmw32";
  revision = "2";
  editedCabalFile = "1dvhv94lzmya938i5crshg9qbx7dxvkyxkhfbqimxkydxn7l2w7w";
  isLibrary = false;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal directory filepath ];
  executableHaskellDepends = [ array base containers mtl ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://www.haskell.org/happy/";
  description = "Happy is a parser generator for Haskell";
  license = stdenv.lib.licenses.bsd3;
}){};
"hashable" = callPackage ({ mkDerivation, base, bytestring, stdenv }:
mkDerivation {
  pname = "hashable";
  version = "1.2.4.0";
  sha256 = "1wrwpchksxd1i63ydpqy6jkdzxrb5qsy64rfiv9lik9r1kdp35pv";
  libraryHaskellDepends = [ base bytestring ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://github.com/tibbe/hashable";
  description = "A class for types that can be converted to a hash value";
  license = stdenv.lib.licenses.bsd3;
}){};
"hscolour" = callPackage ({ mkDerivation, base, containers, stdenv }:
mkDerivation {
  pname = "hscolour";
  version = "1.24.1";
  sha256 = "1j3rpzjygh3igvnd1n2xn63bq68rs047cjxr2qi6xyfnivgf6vz4";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers ];
  executableHaskellDepends = [ base containers ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://code.haskell.org/~malcolm/hscolour/";
  description = "Colourise Haskell code";
  license = "LGPL";
}){};
"http-api-data" = callPackage ({ mkDerivation, base, bytestring, stdenv, text, time
, time-locale-compat
}:
mkDerivation {
  pname = "http-api-data";
  version = "0.2.4";
  sha256 = "0db6brf5rbd3ah3cz9is6j3l42yv86dcxkz8dv7bj5rv6iihifbb";
  libraryHaskellDepends = [
    base bytestring text time time-locale-compat
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://github.com/fizruk/http-api-data";
  description = "Converting to/from HTTP API data like URL pieces, headers and query parameters";
  license = stdenv.lib.licenses.bsd3;
}){};
"http-media" = callPackage ({ mkDerivation, base, bytestring, case-insensitive, containers
, stdenv
}:
mkDerivation {
  pname = "http-media";
  version = "0.6.4";
  sha256 = "1ly93k3d6kilma8gv6y1vf4d3lz4xg5xwi5p8x10w9al13sjqxpg";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/zmthy/http-media";
  description = "Processing HTTP Content-Type and Accept headers";
  license = stdenv.lib.licenses.mit;
}){};
"http-types" = callPackage ({ mkDerivation, array, base, blaze-builder, bytestring
, case-insensitive, stdenv, text
}:
mkDerivation {
  pname = "http-types";
  version = "0.9.1";
  sha256 = "0l7mnvqyppxpnq6ds4a9f395zdbl22z3sxiry1myfs8wvj669vbv";
  libraryHaskellDepends = [
    array base blaze-builder bytestring case-insensitive text
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/aristidb/http-types";
  description = "Generic HTTP types for Haskell (for both client and server code)";
  license = stdenv.lib.licenses.bsd3;
}){};
"integer-gmp" = callPackage ({ mkDerivation, ghc-prim, stdenv }:
mkDerivation {
  pname = "integer-gmp";
  version = "1.0.0.0";
  sha256 = "0sh01sbib7z0bx934a7gq6583hdz8yncaxpfi9k8y4v18gm8j55f";
  revision = "1";
  editedCabalFile = "0qsnmvn70lvcivx3sn5pqaxfya0j6q6dq6qm7mqlwjy9lywzlqsx";
  libraryHaskellDepends = [ ghc-prim ];
  doHaddock = false;
  doCheck = false;
  description = "Integer library based on GMP";
  license = stdenv.lib.licenses.bsd3;
}){};
"jailbreak-cabal" = callPackage ({ mkDerivation, base, Cabal, stdenv }:
mkDerivation {
  pname = "jailbreak-cabal";
  version = "1.3.2";
  sha256 = "1x2h54sx4ycik34q8f9g698xc2b7fai18918cd08qx7w7ny8nai1";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base Cabal ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/peti/jailbreak-cabal#readme";
  description = "Strip version restrictions from build dependencies in Cabal files";
  license = stdenv.lib.licenses.bsd3;
}){};
"mmorph" = callPackage ({ mkDerivation, base, mtl, stdenv, transformers
, transformers-compat
}:
mkDerivation {
  pname = "mmorph";
  version = "1.0.6";
  sha256 = "1i8dzrc5qi3ryc9vrrmpn3sihmramsbhhd592w4w2k5g26qr3hql";
  revision = "1";
  editedCabalFile = "081g39qv8lzmavv8q4sr24liiy56fwrbngyg7j67ah1zgwld8ss8";
  libraryHaskellDepends = [
    base mtl transformers transformers-compat
  ];
  doHaddock = false;
  doCheck = false;
  description = "Monad morphisms";
  license = stdenv.lib.licenses.bsd3;
}){};
"mtl" = callPackage ({ mkDerivation, base, stdenv, transformers }:
mkDerivation {
  pname = "mtl";
  version = "2.2.1";
  sha256 = "1icdbj2rshzn0m1zz5wa7v3xvkf6qw811p4s7jgqwvx1ydwrvrfa";
  revision = "1";
  editedCabalFile = "0fsa965g9h23mlfjzghmmhcb9dmaq8zpm374gby6iwgdx47q0njb";
  libraryHaskellDepends = [ base transformers ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://github.com/ekmett/mtl";
  description = "Monad classes, using functional dependencies";
  license = stdenv.lib.licenses.bsd3;
}){};
"network-uri" = callPackage ({ mkDerivation, base, deepseq, parsec, stdenv }:
mkDerivation {
  pname = "network-uri";
  version = "2.6.1.0";
  sha256 = "1w27zkvn39kjr9lmw9421y8w43h572ycsfafsb7kyvr3a4ihlgj2";
  revision = "1";
  editedCabalFile = "141nj7q0p9wkn5gr41ayc63cgaanr9m59yym47wpxqr3c334bk32";
  libraryHaskellDepends = [ base deepseq parsec ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/haskell/network-uri";
  description = "URI manipulation";
  license = stdenv.lib.licenses.bsd3;
}){};
"parsec" = callPackage ({ mkDerivation, base, bytestring, mtl, stdenv, text }:
mkDerivation {
  pname = "parsec";
  version = "3.1.11";
  sha256 = "0vk7q9j2128q191zf1sg0ylj9s9djwayqk9747k0a5fin4f2b1vg";
  libraryHaskellDepends = [ base bytestring mtl text ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/aslatter/parsec";
  description = "Monadic parser combinators";
  license = stdenv.lib.licenses.bsd3;
}){};
"pretty" = callPackage ({ mkDerivation, base, deepseq, ghc-prim, stdenv }:
mkDerivation {
  pname = "pretty";
  version = "1.1.2.0";
  sha256 = "043kcl2wjip51al5kx3r9qgazq5w002q520wdgdlv2c9xr74fabw";
  revision = "1";
  editedCabalFile = "0jq9lq4i4dpwpgl12py9h1fixcml477vca5ak4gn1brl547bgvvp";
  libraryHaskellDepends = [ base deepseq ghc-prim ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://github.com/haskell/pretty";
  description = "Pretty-printing library";
  license = stdenv.lib.licenses.bsd3;
}){};
"primitive" = callPackage ({ mkDerivation, base, ghc-prim, stdenv, transformers }:
mkDerivation {
  pname = "primitive";
  version = "0.6.1.0";
  sha256 = "1j1q7l21rdm8kfs93vibr3xwkkhqis181w2k6klfhx5g5skiywwk";
  revision = "1";
  editedCabalFile = "0gb8lcn6bd6ilfln7ah9jmqq6324vgkrgdsnz1qvlyj3bi2w5ivf";
  libraryHaskellDepends = [ base ghc-prim transformers ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = stdenv.lib.licenses.bsd3;
}){};
"scientific" = callPackage ({ mkDerivation, base, binary, bytestring, containers, deepseq
, ghc-prim, hashable, integer-gmp, stdenv, text, vector
}:
mkDerivation {
  pname = "scientific";
  version = "0.3.4.9";
  sha256 = "1a0q15kq0pk3pabxh536wgphh8713hhn8n55gm6s1y8a5dk310qh";
  revision = "1";
  editedCabalFile = "0q8xnyyyl4llmfi0ph7cxi7cqyq3v8w7am027in39ir2wrh5jgw3";
  libraryHaskellDepends = [
    base binary bytestring containers deepseq ghc-prim hashable
    integer-gmp text vector
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/basvandijk/scientific";
  description = "Numbers represented using scientific notation";
  license = stdenv.lib.licenses.bsd3;
}){};
"semigroups" = callPackage ({ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "semigroups";
  version = "0.18.2";
  sha256 = "1r6hsn3am3dpf4rprrj4m04d9318v9iq02bin0pl29dg4a3gzjax";
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://github.com/ekmett/semigroups/";
  description = "Anything that associates";
  license = stdenv.lib.licenses.bsd3;
}){};
"servant" = callPackage ({ mkDerivation, aeson, attoparsec, base, base-compat, bytestring
, bytestring-conversion, case-insensitive, http-api-data
, http-media, http-types, mmorph, mtl, network-uri, stdenv
, string-conversions, text, vault
}:
mkDerivation {
  pname = "servant";
  version = "0.7.1";
  sha256 = "0v0jqq8zv98c9xvbp559b82f8nfcyb5p0xcxf4pifxhg6kglgs74";
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bytestring bytestring-conversion
    case-insensitive http-api-data http-media http-types mmorph mtl
    network-uri string-conversions text vault
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "A family of combinators for defining webservices APIs";
  license = stdenv.lib.licenses.bsd3;
}){};
"string-conversions" = callPackage ({ mkDerivation, base, bytestring, stdenv, text, utf8-string }:
mkDerivation {
  pname = "string-conversions";
  version = "0.4";
  sha256 = "1bi4mjnz0srb01n0k73asizp5h2ir7j3whxai9wprqvz7kdscr0s";
  libraryHaskellDepends = [ base bytestring text utf8-string ];
  doHaddock = false;
  doCheck = false;
  description = "Simplifies dealing with different types for strings";
  license = stdenv.lib.licenses.bsd3;
}){};
"stringbuilder" = callPackage ({ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "stringbuilder";
  version = "0.5.0";
  sha256 = "1ap95xphqnrhv64c2a137wqslkdmb2jjd9ldb17gs1pw48k8hrl9";
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  doCheck = false;
  description = "A writer monad for multi-line string literals";
  license = stdenv.lib.licenses.mit;
}){};
"syb" = callPackage ({ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "syb";
  version = "0.6";
  sha256 = "1p3cnqjm13677r4a966zffzhi9b3a321aln8zs8ckqj0d9z1z3d3";
  revision = "1";
  editedCabalFile = "158ngdnlq9n1mil7cq2bzy4zkgx73zzms9q56wp6ll93m5mc4nlx";
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://www.cs.uu.nl/wiki/GenericProgramming/SYB";
  description = "Scrap Your Boilerplate";
  license = stdenv.lib.licenses.bsd3;
}){};
"tagged" = callPackage ({ mkDerivation, base, deepseq, stdenv }:
mkDerivation {
  pname = "tagged";
  version = "0.8.4";
  sha256 = "0pjd4cx4kkcc2vxs8w829lpqdcxhkx9d1n9rp88ahpj4k7963j10";
  libraryHaskellDepends = [ base deepseq ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://github.com/ekmett/tagged";
  description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
  license = stdenv.lib.licenses.bsd3;
}){};
"template-haskell" = callPackage ({ mkDerivation, base, pretty, stdenv }:
mkDerivation {
  pname = "template-haskell";
  version = "2.10.0.0";
  sha256 = "1y0dikbpy98n9g1rwb6swng86cch815x5ipj8kfjgpjgs0c3i2im";
  libraryHaskellDepends = [ base pretty ];
  doHaddock = false;
  doCheck = false;
  description = "Support library for Template Haskell";
  license = stdenv.lib.licenses.bsd3;
}){};
"text" = callPackage ({ mkDerivation, array, base, binary, bytestring, deepseq, ghc-prim
, integer-gmp, stdenv
}:
mkDerivation {
  pname = "text";
  version = "1.2.2.1";
  sha256 = "0nrrzx0ws7pv4dx9jbc6jm2734al1cr0m6iwcnbck4v2yfyv3p8s";
  libraryHaskellDepends = [
    array base binary bytestring deepseq ghc-prim integer-gmp
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/bos/text";
  description = "An efficient packed Unicode text type";
  license = stdenv.lib.licenses.bsd3;
}){};
"time" = callPackage ({ mkDerivation, base, deepseq, stdenv }:
mkDerivation {
  pname = "time";
  version = "1.5.0.1";
  sha256 = "0knixcmdsl2jhjw0x6is02yrw6dhjn4gr3fh06adc003gc3wr894";
  revision = "1";
  editedCabalFile = "0pv1bkv0x8hw6qszj7sjg1g8hms6hsb6c3dz9h8ypbadcb85v829";
  libraryHaskellDepends = [ base deepseq ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/haskell/time";
  description = "A time library";
  license = stdenv.lib.licenses.bsd3;
}){};
"time-locale-compat" = callPackage ({ mkDerivation, base, old-locale, stdenv, time }:
mkDerivation {
  pname = "time-locale-compat";
  version = "0.1.1.3";
  sha256 = "1vdcfr2hp9qh3ag90x6ikbdf42wiqpdylnplffna54bpnilbyi4i";
  libraryHaskellDepends = [ base old-locale time ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/khibino/haskell-time-locale-compat";
  description = "Compatibility of TimeLocale between old-locale and time-1.5";
  license = stdenv.lib.licenses.bsd3;
}){};
"transformers" = callPackage ({ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "transformers";
  version = "0.4.2.0";
  sha256 = "0a364zfcm17mhpy0c4ms2j88sys4yvgd6071qsgk93la2wjm8mkr";
  revision = "1";
  editedCabalFile = "1q7y5mh3bxrnxinkvgwyssgrbbl4pp183ncww8dwzgsplf0zav0n";
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  doCheck = false;
  description = "Concrete functor and monad transformers";
  license = stdenv.lib.licenses.bsd3;
}){};
"transformers-compat" = callPackage ({ mkDerivation, base, stdenv, transformers }:
mkDerivation {
  pname = "transformers-compat";
  version = "0.4.0.4";
  sha256 = "0lmg8ry6bgigb0v2lg0n74lxi8z5m85qq0qi4h1k9llyjb4in8ym";
  libraryHaskellDepends = [ base transformers ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://github.com/ekmett/transformers-compat/";
  description = "A small compatibility shim exposing the new types from transformers 0.3 and 0.4 to older Haskell platforms.";
  license = stdenv.lib.licenses.bsd3;
}){};
"unix" = callPackage ({ mkDerivation, base, bytestring, stdenv, time }:
mkDerivation {
  pname = "unix";
  version = "2.7.1.0";
  sha256 = "0p74ljsl1zgwnyl69pg4l15z5rqidam9fw9il4siam2m700ydm3b";
  revision = "1";
  editedCabalFile = "00nqyvc34cn73gd829cl2cfkg6c3jb6qdrwf3ssz0l4d2apk4cpf";
  libraryHaskellDepends = [ base bytestring time ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/haskell/unix";
  description = "POSIX functionality";
  license = stdenv.lib.licenses.bsd3;
}){};
"unordered-containers" = callPackage ({ mkDerivation, base, deepseq, hashable, stdenv }:
mkDerivation {
  pname = "unordered-containers";
  version = "0.2.7.1";
  sha256 = "00npqiphivjp2d7ryqsdavfn4m5v3w1lq2azhdsrfh0wsvqpg4ig";
  libraryHaskellDepends = [ base deepseq hashable ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/tibbe/unordered-containers";
  description = "Efficient hashing-based container types";
  license = stdenv.lib.licenses.bsd3;
}){};
"utf8-string" = callPackage ({ mkDerivation, base, bytestring, stdenv }:
mkDerivation {
  pname = "utf8-string";
  version = "1.0.1.1";
  sha256 = "0h7imvxkahiy8pzr8cpsimifdfvv18lizrb33k6mnq70rcx9w2zv";
  revision = "2";
  editedCabalFile = "1b97s9picjl689hcz8scinv7c8k5iaal1livqr0l1l8yc4h0imhr";
  libraryHaskellDepends = [ base bytestring ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://github.com/glguy/utf8-string/";
  description = "Support for reading and writing UTF8 Strings";
  license = stdenv.lib.licenses.bsd3;
}){};
"vault" = callPackage ({ mkDerivation, base, containers, hashable, stdenv
, unordered-containers
}:
mkDerivation {
  pname = "vault";
  version = "0.3.0.6";
  sha256 = "0j7gcs440q7qlgzi2hn36crgp2c0w69k40g6vj9hxlm31zk3shqb";
  libraryHaskellDepends = [
    base containers hashable unordered-containers
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/HeinrichApfelmus/vault";
  description = "a persistent store for values of arbitrary types";
  license = stdenv.lib.licenses.bsd3;
}){};
"vector" = callPackage ({ mkDerivation, base, deepseq, ghc-prim, primitive, stdenv }:
mkDerivation {
  pname = "vector";
  version = "0.11.0.0";
  sha256 = "1r1jlksy7b0kb0fy00g64isk6nyd9wzzdq31gx5v1wn38knj0lqa";
  revision = "2";
  editedCabalFile = "1kjafhgsyjqlvrpfv2vj17hipyv0zw56a2kbl6khzn5li9szvyib";
  libraryHaskellDepends = [ base deepseq ghc-prim primitive ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/haskell/vector";
  description = "Efficient Arrays";
  license = stdenv.lib.licenses.bsd3;
}){};
}
