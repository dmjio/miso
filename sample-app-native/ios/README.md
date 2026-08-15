# MisoLynx — iOS host app

A minimal native iOS app that renders the miso `main.lynx.bundle` (the
`sample-app-native` Lynx app) on an iPhone. It mirrors the official
[Lynx iOS integration](https://lynxjs.org/guide/start/integrate-with-existing-apps.html)
and the LynxExplorer's `TemplateProvider` — just enough to load and show one bundle.

> Written in Objective-C (mirrors the reference explorer for reliability). It's a
> plain `UIWindow` + `UIViewController` hosting a `LynxView`.

## Prerequisites (macOS)

```bash
brew install xcodegen cocoapods   # Xcode 15+ also required
```

## Steps

**1. Build the Lynx bundle** (repo root, in the native toolchain shell):

```bash
nix develop .#native
just rebuild-native          # -> sample-app-native/build/dist/main.lynx.bundle
```

**2. Copy the bundle into this app's resources:**

```bash
just bundle-ios              # -> sample-app-native/ios/Resource/main.lynx.bundle
```

**3. Generate the Xcode project and install pods** (from this `ios/` dir):

```bash
cd sample-app-native/ios
xcodegen generate            # -> MisoLynx.xcodeproj
pod install                  # -> MisoLynx.xcworkspace
```

**4. Open, sign, run:**

```bash
open MisoLynx.xcworkspace     # NOT the .xcodeproj
```

- In Xcode: **Signing & Capabilities** → select your **Personal Team** (Apple ID)
  and set a unique **Bundle Identifier** (e.g. `com.<you>.MisoLynx`), enable
  *Automatically manage signing*.
- Select your iPhone as the run destination, press **⌘R**.
- First run on the device: **Settings → General → VPN & Device Management** →
  trust your developer profile.

You should see the gallery: a scrolling showcase of native Lynx elements (view
gestures, a tappable heart "like" animation, text, image, list, input,
scroll-coordinator, viewpager, refresh, svg, blur-view, webview, overlay, and
main-thread tap/scroll handlers), with an event log at the top that fills in
as you interact with each section.

## Iterating

- **Re-embed after a code change:** `just rebuild-native && just bundle-ios`, then
  re-run in Xcode.
- **Load from a dev server instead of embedding** (faster loop, no rebuild of the
  app): run `just serve-native`, then set `kTemplateURL` in
  `App/ViewController.m` to `http://<your-mac-ip>:8080/main.lynx.bundle`. (ATS
  arbitrary loads is already enabled in `Info.plist` for local HTTP.)

## Notes / caveats

- Untested on-device by the author (developed on Linux); the Lynx API calls are
  copied from the reference explorer, but pod versions (`4.0.0`) and signing are
  yours to confirm on your Mac.
- Uses `LynxThreadStrategyForRenderAllOnUI` (the explorer default the bundle was
  verified under). If rendering misbehaves, that's the first knob to try.
- Regenerated files (`*.xcodeproj`, `*.xcworkspace`, `Pods/`, the copied bundle)
  are git-ignored; `project.yml`, `Podfile`, and `App/` sources are the source of
  truth.
