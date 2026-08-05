// Example custom Lynx native module. Exposes methods to the background-thread
// JS runtime (where miso's Haskell runs), reachable as
// `NativeModules.MisoNativeModule.<method>(...)`.
#import <Foundation/Foundation.h>
#import <Lynx/LynxModule.h>

@interface MisoNativeModule : NSObject <LynxModule>
@end
