{
  ### Welcome to the Haskell Miso Flake ###
  description = "🍜 Haskell Miso flake 🍜";

  ## Config
  nixConfig = {

    # Miso's cachix cache
    extra-substituters = [
      "https://haskell-miso-cachix.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-miso-cachix.cachix.org-1:m8hN1cvFMJtYib4tj+06xkKt5ABMSGfe8W7s40x1kQ0="
    ];

  };

  # Miso's flake inputs
  inputs = {

    # Miso's nixpkgs hash, this is used for acquiring the GHCJS-9122 backend
    # and native backend
    nixpkgs.url =
      "github:nixos/nixpkgs?rev=9e2e8a7878573d312db421d69e071690ec34e98c";

    # Miso uses this for FFI
    jsaddle.url =
      "github:ghcjs/jsaddle?rev=2513cd19184376ac8a2f0e3797a1ae7d2e522e87";

    # Miso uses this for routing
    servant.url =
      "github:haskell-servant/servant?rev=e07e92abd62641fc0f199a33e5131de273140cb0";

    # Miso uses this compiling for WebAssembly
    ghc-wasm-meta.url =
      "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";

  };

  # Miso's flake outputs
  outputs = { self, nixpkgs, ... } @ inputs : {

     # Miso's overlays
    overlays =
      [ (import ./nix/overlay.nix)
      ];

     # Packages
      packages = with nixpkgs.legacyPackages; {

       # Linux x86_64 is the default, for now
       default = self.packages.x86_64-linux;

       ### x86
       x86_64-linux = with x86_64-linux.haskell.packages.ghc9122; {
         default = miso;
         inherit miso miso-examples;
       };
       x86_64-darwin = with x86_64-darwin.haskell.packages.ghc9122; {
         default = miso;
         inherit miso miso-examples;
       };

       ### ARM
       aarch64-linux = with aarch64-linux.haskell.packages.ghc9122; {
         default = miso;
         inherit miso miso-examples;
       };
       aarch64_64-darwin = with aarch64_64-darwin.haskell.packages.ghc9122; {
         default = miso;
         inherit miso miso-examples;
       };

     };

     # Miso development Shells
     devShells = with nixpkgs.legacyPackages; {

       # The default shell is for Linux x86_64 and includes GHC, ghcid, ghciwatch
       default = self.devShells.x86_64-linux;

       # Linux
       x86_64-linux.default =
         pkgs.mkShell {
           name = "The miso linux x86 ghc9122 shell";
           shellHook = ''
             echo test
           '';

         };

       # Darwin
       x86_64-darwin.default =
         pkgs.mkShell {
           name = "The miso linux x86 ghc9122 shell";
           shellHook = ''
             echo test
           '';
           buildInputs = with pkgs; [
             pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.miso
           ];
         };
     };
   };
}
