#import "MisoNativeModule.h"
#import <UIKit/UIKit.h>

@implementation MisoNativeModule

// The name JS uses: NativeModules.MisoNativeModule
+ (NSString*)name {
  return @"MisoNativeModule";
}

// Map JS method names -> Objective-C selectors.
+ (NSDictionary<NSString*, NSString*>*)methodLookup {
  return @{
    @"log" : NSStringFromSelector(@selector(log:)),
    @"deviceInfo" : NSStringFromSelector(@selector(deviceInfo:)),
  };
}

// Fire-and-forget: NativeModules.MisoNativeModule.log("hi")
- (void)log:(NSString*)message {
  NSLog(@"[MisoNativeModule] %@", message);
}

// Async: NativeModules.MisoNativeModule.deviceInfo(v => ...). The JS callback is
// delivered as a block; call it to return a value to JS.
- (void)deviceInfo:(void (^)(NSString*))callback {
  UIDevice* d = [UIDevice currentDevice];
  NSString* info =
      [NSString stringWithFormat:@"%@ / %@ %@", d.model, d.systemName, d.systemVersion];
  callback(info);
}

@end
