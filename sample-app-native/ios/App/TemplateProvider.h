// Loads a Lynx template (.lynx.bundle) for the app: first from the bundled
// `Resource/` directory, then falling back to a network URL (dev server).
#import <Foundation/Foundation.h>
#import <Lynx/LynxTemplateProvider.h>

@interface TemplateProvider : NSObject <LynxTemplateProvider>
@end
