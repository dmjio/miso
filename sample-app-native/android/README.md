# Android host (Lynx)

Thin Android host that loads and renders the same `main.lynx.bundle` as the iOS
host. Modeled on the official
[integrating-lynx-demo-projects](https://github.com/lynx-family/integrating-lynx-demo-projects)
`KotlinEmptyProject`. **Host-only — no native modules** (add a `LynxModule` +
`SharedPreferences` later if you want the `Miso.Storage` bridge).

> Status: hand-authored integration files. This is **not verified to build** in
> this repo's CI — it needs the Android toolchain. Generate the Gradle wrapper
> and open in Android Studio to complete/run it.

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
app/src/main/assets/main.lynx.bundle        # <-- copy the built bundle here
```

## Still needed (one-time, IDE-generated)
- **Gradle wrapper jar + scripts** (`gradle/wrapper/gradle-wrapper.jar`,
  `gradlew`, `gradlew.bat`) — binary, not checked in here. Android Studio
  generates them on first sync, or run `gradle wrapper --gradle-version 8.7`.
- an app icon / theme if you want more than a bare activity.

## Build & run
1. Build the bundle: `bun run js && nix-build -A sample-app-native-bundle`
2. Copy it into **assets/** (not the module root):
   `mkdir -p app/src/main/assets && cp result/main.lynx.bundle app/src/main/assets/`
3. Open `sample-app-native/android` in Android Studio → **Sync** (it downloads
   Gradle 8.7 and the `com.android.application` plugin from the repos in
   `settings.gradle.kts`) → run on a device/emulator (minSdk 24).

The bundle is platform-agnostic — the exact artifact the iOS host loads.
