#import "AppDelegate.h"
#import "ViewController.h"
#import "TemplateProvider.h"
#import <Lynx/LynxConfig.h>
#import <Lynx/LynxEnv.h>

@implementation AppDelegate

- (BOOL)application:(UIApplication*)application
    didFinishLaunchingWithOptions:(NSDictionary*)launchOptions {
  // Lynx global setup: initialise the environment and register a template
  // provider (loads main.lynx.bundle from the app's Resource/ dir or a URL).
  LynxEnv* env = [LynxEnv sharedInstance];
  LynxConfig* config = [[LynxConfig alloc] initWithProvider:[TemplateProvider new]];
  [env prepareConfig:config];

  self.window = [[UIWindow alloc] initWithFrame:[UIScreen mainScreen].bounds];
  self.window.rootViewController = [[ViewController alloc] init];
  [self.window makeKeyAndVisible];
  return YES;
}

@end
