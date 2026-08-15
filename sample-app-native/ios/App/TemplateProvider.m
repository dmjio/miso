// Adapted from lynx/explorer/darwin/ios/.../provider/TemplateProvider.m
#import "TemplateProvider.h"

@implementation TemplateProvider

- (void)loadTemplateWithUrl:(NSString*)url onComplete:(LynxTemplateLoadBlock)callback {
  // 1) Local bundle in the app's Resource/ dir (e.g. "main.lynx.bundle").
  if ([url hasSuffix:@".bundle"]) {
    NSString* bundleName = [url stringByDeletingPathExtension];  // "main.lynx.bundle" -> "main.lynx"
    NSString* bundlePath = [[NSBundle mainBundle] pathForResource:bundleName
                                                           ofType:@"bundle"
                                                      inDirectory:@"Resource"];
    if (bundlePath) {
      NSData* data = [NSData dataWithContentsOfFile:bundlePath];
      if (data) {
        dispatch_async(dispatch_get_main_queue(), ^{ callback(data, nil); });
        return;
      }
    }
  }

  // 2) Fallback: fetch over the network, e.g. http://<mac-ip>:8080/main.lynx.bundle
  NSString* encoded =
      [url stringByAddingPercentEncodingWithAllowedCharacters:[NSCharacterSet
                                                                  URLFragmentAllowedCharacterSet]];
  NSURL* nsUrl = [NSURL URLWithString:encoded];
  if (!nsUrl) {
    NSError* err = [NSError errorWithDomain:@"io.dmj.miso"
                                       code:400
                                   userInfo:@{NSLocalizedDescriptionKey : @"Invalid template URL"}];
    dispatch_async(dispatch_get_main_queue(), ^{ callback(nil, err); });
    return;
  }
  NSURLSessionDataTask* task = [[NSURLSession sharedSession]
        dataTaskWithURL:nsUrl
      completionHandler:^(NSData* _Nullable data, NSURLResponse* _Nullable response,
                          NSError* _Nullable error) {
        dispatch_async(dispatch_get_main_queue(), ^{ callback(data, error); });
      }];
  [task resume];
}

@end
