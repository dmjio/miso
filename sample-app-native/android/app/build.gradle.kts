import java.security.MessageDigest

// Android host for the miso-native Lynx bundle. Artifacts mirror the official
// lynx-family integrating-lynx-demo-projects KotlinEmptyProject; the Lynx SDK is
// pinned to 4.0.3 to match sample-app-native/ios/Podfile, since that is the newest
// stable release published on both Maven and CocoaPods. Native modules are
// intentionally omitted — this host only loads and renders main.lynx.bundle.
plugins {
  id("com.android.application")
  id("org.jetbrains.kotlin.android")
}

android {
  namespace = "io.dmj.miso"
  compileSdk = 34

  defaultConfig {
    applicationId = "io.dmj.miso"
    minSdk = 24
    targetSdk = 34
    versionCode = 1
    versionName = "1.0"
  }

  packaging {
    resources {
      excludes += "/META-INF/{AL2.0,LGPL2.1}"
    }
  }

  compileOptions {
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17
  }
  kotlinOptions { jvmTarget = "17" }
}

dependencies {
  // Core Lynx runtime + JS engine
  implementation("org.lynxsdk.lynx:lynx:4.0.3")
  implementation("org.lynxsdk.lynx:lynx-jssdk:4.0.3")
  implementation("org.lynxsdk.lynx:lynx-trace:4.0.3")
  implementation("org.lynxsdk.lynx:primjs:4.0.0")

  // Image service (<image>) + its required Fresco deps (not transitive).
  implementation("org.lynxsdk.lynx:lynx-service-image:4.0.3")
  implementation("com.facebook.fresco:fresco:2.3.0")
  implementation("com.facebook.fresco:animated-gif:2.3.0")
  implementation("com.facebook.fresco:animated-webp:2.3.0")
  implementation("com.facebook.fresco:webpsupport:2.3.0")
  implementation("com.facebook.fresco:animated-base:2.3.0")

  // Log service
  implementation("org.lynxsdk.lynx:lynx-service-log:4.0.3")

  // Http service (fetch) + its OkHttp dep
  implementation("org.lynxsdk.lynx:lynx-service-http:4.0.3")
  implementation("com.squareup.okhttp3:okhttp:4.9.0")

  // Extended (X) elements exercised by the gallery
  implementation("org.lynxsdk.lynx:xelement:4.0.3")
  implementation("org.lynxsdk.lynx:xelement-input:4.0.3")
  implementation("org.lynxsdk.lynx:xelement-overlay:4.0.3")
  implementation("org.lynxsdk.lynx:xelement-svg:4.0.3")
  implementation("org.lynxsdk.lynx:servalsvg:0.0.2")
  implementation("org.lynxsdk.lynx:xelement-refresh:4.0.3")
}

val lynxBundle = layout.projectDirectory.file("src/main/assets/main.lynx.bundle").asFile
val lynxBundleChecksum =
  layout.projectDirectory.file("src/main/assets/main.lynx.bundle.sha256").asFile

tasks.named("preBuild").configure {
  doFirst {
    check(lynxBundle.isFile && lynxBundleChecksum.isFile) {
      "Missing generated Lynx bundle/checksum. Build with " +
        "`nix-build -A sample-app-native-android`, or copy both files from " +
        "`nix-build -A sample-app-native-bundle`."
    }
    val expected = lynxBundleChecksum.readText().trim().substringBefore(' ')
    val actual = MessageDigest.getInstance("SHA-256")
      .digest(lynxBundle.readBytes())
      .joinToString("") { "%02x".format(it) }
    check(expected == actual) {
      "Stale main.lynx.bundle: expected SHA-256 $expected, got $actual. " +
        "Copy the bundle and checksum from the same generated artifact."
    }
  }
}
