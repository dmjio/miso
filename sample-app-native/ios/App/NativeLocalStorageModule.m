#import "NativeLocalStorageModule.h"

// Backs Miso.Storage (NATIVE). The Haskell side calls:
//   setLocalStorage k v  -> setStorageItem(k, v)          (fire-and-forget)
//   getLocalStorage k    -> getStorageItem(k, callback)   (async, value via cb)
//   clearLocalStorage    -> clearStorage()                (fire-and-forget)
// Values arrive already JSON-encoded by miso (toJSON) and are decoded on the
// way back (FromJSON), so we just persist/return them as opaque strings.
@implementation NativeLocalStorageModule

// The name JS uses: NativeModules.NativeLocalStorageModule
+ (NSString*)name {
  return @"NativeLocalStorageModule";
}

// Map JS method names -> Objective-C selectors.
+ (NSDictionary<NSString*, NSString*>*)methodLookup {
  return @{
    @"setStorageItem" : NSStringFromSelector(@selector(setStorageItem:value:)),
    @"getStorageItem" : NSStringFromSelector(@selector(getStorageItem:callback:)),
    @"clearStorage"   : NSStringFromSelector(@selector(clearStorage)),
  };
}

- (NSUserDefaults*)store {
  return [NSUserDefaults standardUserDefaults];
}

// NativeModules.NativeLocalStorageModule.setStorageItem(key, value)
- (void)setStorageItem:(NSString*)key value:(NSString*)value {
  [[self store] setObject:value forKey:key];
}

// NativeModules.NativeLocalStorageModule.getStorageItem(key, v => ...)
// The JS callback is delivered as a block; call it exactly once to hand the
// value back to JS. A missing key yields nil (JS null). N.B.: miso blocks on an
// MVar awaiting this callback, so it MUST fire exactly once on every path.
- (void)getStorageItem:(NSString*)key callback:(void (^)(NSString*))callback {
  callback([[self store] stringForKey:key]);
}

// NativeModules.NativeLocalStorageModule.clearStorage()
- (void)clearStorage {
  NSString* domain = [[NSBundle mainBundle] bundleIdentifier];
  [[self store] removePersistentDomainForName:domain];
}

@end
