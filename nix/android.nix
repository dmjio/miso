# Build the sample-app-native Android host APK (Kotlin + Lynx SDK) reproducibly.
#
# Uses nixpkgs' androidenv for the SDK (platform-34 + build-tools 34.0.0) and the
# gradle mitm-cache fixed-output dependency fetcher for all Maven/Gradle
# artifacts (Lynx, Fresco, OkHttp, androidx, AGP, Kotlin plugin). The Lynx bundle
# is taken from `bundle` (the sample-app-native-bundle derivation) and embedded
# in app/src/main/assets, so the APK always ships a freshly built bundle.
#
# The app has no native C/C++ of its own — the .so's ship prebuilt inside the
# Lynx Maven AARs — so no NDK / pkgsCross toolchain is required here.
#
# Regenerating the dep lock (nix/android-deps.json), e.g. after changing
# app/build.gradle.kts:
#
#     ./$(nix-build --no-out-link -A sample-app-native-android.mitmCache.updateScript)
#
# then rebuild with `nix-build -A sample-app-native-android`.
{ pkgs, bundle }:

let
  androidComposition = pkgs.androidenv.composeAndroidPackages {
    platformVersions    = [ "34" ];
    buildToolsVersions  = [ "34.0.0" ];
    includeEmulator     = false;
    includeSystemImages = false;
    includeNDK          = false;
  };

  sdkRoot = "${androidComposition.androidsdk}/libexec/android-sdk";
  aapt2   = "${sdkRoot}/build-tools/34.0.0/aapt2";

  jdk = pkgs.jdk17;

  # Android project tree with the freshly-built Lynx bundle embedded in assets.
  androidSrc = pkgs.runCommand "miso-android-src" { } ''
    cp -r ${../sample-app-native/android} $out
    chmod -R u+w $out
    mkdir -p $out/app/src/main/assets
    cp ${bundle}/main.lynx.bundle $out/app/src/main/assets/main.lynx.bundle
    # SDK location comes from ANDROID_HOME, not a checked-in local.properties.
    rm -f $out/local.properties
  '';
in
pkgs.stdenv.mkDerivation (finalAttrs: {
  pname = "miso-sample-app-native-android";
  version = "1.0";

  src = androidSrc;

  nativeBuildInputs = [ pkgs.gradle jdk ];

  # Fixed-output dep cache; regenerate nix/android-deps.json via the updateScript
  # (see header). `data` is a string so it resolves relative to this .nix file.
  mitmCache = pkgs.gradle.fetchDeps {
    pkg = finalAttrs.finalPackage;
    data = "android-deps.json";
  };

  # AGP resolves the SDK from these.
  ANDROID_HOME = sdkRoot;
  ANDROID_SDK_ROOT = sdkRoot;
  JAVA_HOME = jdk.home;

  # AGP downloads a prebuilt aapt2 from Maven whose dynamic loader doesn't exist
  # under the nix sandbox; use androidenv's autoPatchelf'd build-tools aapt2.
  gradleFlags = [ "-Pandroid.aapt2FromMavenOverride=${aapt2}" ];

  gradleBuildTask = ":app:assembleDebug";

  # The default dep-fetch task (nixDownloadDeps) resolves *all* configurations
  # generically, which fails on AGP's variant-aware Android configurations
  # (variant ambiguity). Run the real build during the update so the mitm proxy
  # records exactly the artifacts the build needs.
  gradleUpdateTask = ":app:assembleDebug";

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp app/build/outputs/apk/debug/app-debug.apk $out/
    runHook postInstall
  '';

  meta = {
    description = "Android host APK embedding the sample-app-native Lynx bundle";
    platforms = pkgs.lib.platforms.linux;
    license = pkgs.lib.licenses.bsd3;
  };
})
