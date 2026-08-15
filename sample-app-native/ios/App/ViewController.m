#import "ViewController.h"
#import "TemplateProvider.h"
#import <Lynx/LynxConfig.h>
#import <Lynx/LynxView.h>
#import <Lynx/LynxViewBuilder.h>

// The bundle to load. The provider strips ".bundle" and looks up
// "main.lynx.bundle" in the app's Resource/ dir. To load from a dev server
// instead, use e.g. @"http://192.168.1.23:8080/main.lynx.bundle".
static NSString* const kTemplateURL = @"main.lynx.bundle";

@interface ViewController ()
@property(nonatomic, strong) LynxView* lynxView;
@end

@implementation ViewController

- (void)viewDidLoad {
  [super viewDidLoad];
  // Black so the safe-area bands (status bar / home indicator) around the
  // safe-area-inset LynxView match the gallery's black background instead of
  // showing white.
  self.view.backgroundColor = [UIColor blackColor];

  CGSize screenSize = self.view.bounds.size;

  LynxView* lynxView = [[LynxView alloc] initWithBuilderBlock:^(LynxViewBuilder* builder) {
    LynxConfig* config = [[LynxConfig alloc] initWithProvider:[TemplateProvider new]];
    builder.config = config;
    builder.screenSize = screenSize;
    builder.fontScale = 1.0;
    // miso runs a dual-thread (MTS/BTS) app; AllOnUI is the explorer default and
    // the strategy the bundle was verified under.
    [builder setThreadStrategyForRender:LynxThreadStrategyForRenderAllOnUI];
  }];

  lynxView.layoutWidthMode = LynxViewSizeModeExact;
  lynxView.layoutHeightMode = LynxViewSizeModeExact;
  [self.view addSubview:lynxView];
  self.lynxView = lynxView;

  [lynxView loadTemplateFromURL:kTemplateURL initData:nil];
}

// Inset the LynxView to the safe area so the app's 100vh viewport sits below the
// status bar / notch and above the home indicator, rather than under them.
// safeAreaInsets is only valid once the view has laid out, so we size here (not
// in viewDidLoad) and keep the Lynx layout viewport in sync on rotation, etc.
- (void)viewDidLayoutSubviews {
  [super viewDidLayoutSubviews];
  CGRect safeFrame = UIEdgeInsetsInsetRect(self.view.bounds, self.view.safeAreaInsets);
  self.lynxView.frame = safeFrame;
  self.lynxView.preferredLayoutWidth = safeFrame.size.width;
  self.lynxView.preferredLayoutHeight = safeFrame.size.height;
  [self.lynxView triggerLayout];
}

@end
