# Android host (Lynx)

Thin Android host that loads and renders the same `main.lynx.bundle` as the iOS
host. Modeled on the official
[integrating-lynx-demo-projects](https://github.com/lynx-family/integrating-lynx-demo-projects)
`KotlinEmptyProject`. **Host-only — no native modules** (add a `LynxModule` +
`SharedPreferences` later if you want the `Miso.Storage` bridge).

The Nix Android build and CI both inject the generated bundle; the repository
does not carry a prebuilt binary. A direct Gradle build verifies the generated
SHA-256 sidecar and fails before packaging if the bundle is missing or stale.

## What's here
```
settings.gradle.kts                        # repos (Lynx = Maven Central) + include(:app)
build.gradle.kts                           # root: AGP 8.5.0 / Kotlin 1.9.0
gradle.properties                          # androidx etc.
gradle/wrapper/gradle-wrapper.properties   # Gradle 8.7
app/build.gradle.kts                       # Lynx SDK 3.8.0 deps
app/src/main/AndroidManifest.xml
app/src/main/java/io/dmj/miso/
  MisoApplication.kt                        # LynxEnv.init + service registration
  MainActivity.kt                           # LynxView + renderTemplateUrl
  MisoTemplateProvider.kt                   # loads bundle from assets/
app/src/main/assets/main.lynx.bundle        # generated, ignored by Git
app/src/main/assets/main.lynx.bundle.sha256 # generated provenance sidecar
```

## Build & run
1. Preferred reproducible build: `nix-build -A sample-app-native-android`.
2. For a direct Gradle/Android Studio build, first run
   `nix-build -A sample-app-native-bundle`, then copy both
   `result/main.lynx.bundle` and `result/main.lynx.bundle.sha256` into
   `app/src/main/assets/`.
3. Open `sample-app-native/android` in Android Studio → **Sync** (it downloads
   Gradle 8.7 and the `com.android.application` plugin from the repos in
   `settings.gradle.kts`) → run on a device/emulator (minSdk 24).

The bundle is platform-agnostic — the exact artifact the iOS host loads.
