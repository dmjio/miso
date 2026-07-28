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
  self.view.backgroundColor = [UIColor whiteColor];

  CGSize screenSize = self.view.bounds.size;

  LynxView* lynxView = [[LynxView alloc] initWithBuilderBlock:^(LynxViewBuilder* builder) {
    builder.config = [[LynxConfig alloc] initWithProvider:[TemplateProvider new]];
    builder.screenSize = screenSize;
    builder.fontScale = 1.0;
    // miso runs a dual-thread (MTS/BTS) app; AllOnUI is the explorer default and
    // the strategy the bundle was verified under.
    [builder setThreadStrategyForRender:LynxThreadStrategyForRenderAllOnUI];
  }];

  lynxView.frame = self.view.bounds;
  lynxView.preferredLayoutWidth = screenSize.width;
  lynxView.preferredLayoutHeight = screenSize.height;
  lynxView.layoutWidthMode = LynxViewSizeModeExact;
  lynxView.layoutHeightMode = LynxViewSizeModeExact;
  [self.view addSubview:lynxView];
  self.lynxView = lynxView;

  [lynxView loadTemplateFromURL:kTemplateURL initData:nil];
  [lynxView triggerLayout];
}

- (void)viewDidLayoutSubviews {
  [super viewDidLayoutSubviews];
  self.lynxView.frame = self.view.bounds;
}

@end
