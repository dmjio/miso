// Persistent key/value storage native module, backed by NSUserDefaults.
// Exposed to miso's background-thread JS (where the Haskell runtime runs) as
// `NativeModules.NativeLocalStorageModule.<method>(...)`. This is the module
// Miso.Storage's NATIVE path calls (getStorageItem / setStorageItem /
// clearStorage) — Lynx has no built-in `window.localStorage`.
#import <Foundation/Foundation.h>
#import <Lynx/LynxModule.h>

@interface NativeLocalStorageModule : NSObject <LynxModule>
@end
