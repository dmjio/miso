// Android host for the miso-native Lynx bundle. Versions/artifacts mirror the
// official lynx-family integrating-lynx-demo-projects KotlinEmptyProject
// (Lynx SDK 3.8.0). Native modules are intentionally omitted — this host only
// loads and renders main.lynx.bundle.
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
  implementation("org.lynxsdk.lynx:lynx:3.8.0")
  implementation("org.lynxsdk.lynx:lynx-jssdk:3.8.0")
  implementation("org.lynxsdk.lynx:lynx-trace:3.8.0")
  implementation("org.lynxsdk.lynx:primjs:3.8.0")

  // Image service (<image>) + its required Fresco deps (not transitive).
  implementation("org.lynxsdk.lynx:lynx-service-image:3.8.0")
  implementation("com.facebook.fresco:fresco:2.3.0")
  implementation("com.facebook.fresco:animated-gif:2.3.0")
  implementation("com.facebook.fresco:animated-webp:2.3.0")
  implementation("com.facebook.fresco:webpsupport:2.3.0")
  implementation("com.facebook.fresco:animated-base:2.3.0")

  // Log service
  implementation("org.lynxsdk.lynx:lynx-service-log:3.8.0")

  // Http service (fetch) + its OkHttp dep
  implementation("org.lynxsdk.lynx:lynx-service-http:3.8.0")
  implementation("com.squareup.okhttp3:okhttp:4.9.0")

  // Extended (X) elements exercised by the gallery
  implementation("org.lynxsdk.lynx:xelement:3.8.0")
  implementation("org.lynxsdk.lynx:xelement-input:3.8.0")
  implementation("org.lynxsdk.lynx:xelement-overlay:3.8.0")
  implementation("org.lynxsdk.lynx:xelement-svg:3.8.0")
  implementation("org.lynxsdk.lynx:servalsvg:0.0.1-alpha.3")
  implementation("org.lynxsdk.lynx:xelement-refresh:3.8.0")
}
