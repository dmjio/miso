package io.dmj.miso

import android.app.Application
import com.lynx.tasm.LynxEnv
import com.lynx.tasm.service.LynxServiceCenter
import com.lynx.service.image.LynxImageService
import com.lynx.service.http.LynxHttpService
import com.lynx.service.log.LynxLogService

// Lynx global setup. Mirrors the official KotlinEmptyProject "YourApplication":
// initialise LynxEnv once per process and register the core services the runtime
// needs (image for <image>, http for fetch, log). Native modules are registered
// per-LynxView in MainActivity, not here.
class MisoApplication : Application() {
  override fun onCreate() {
    super.onCreate()

    // Services must be registered before LynxEnv.init.
    LynxServiceCenter.inst().registerService(LynxImageService.getInstance())
    LynxServiceCenter.inst().registerService(LynxHttpService)
    LynxServiceCenter.inst().registerService(LynxLogService)

    // args: (context, behaviorBundle, ttNetPackage, apiActor) — nulls are fine
    // for a plain host; behaviors are added per-view via LynxViewBuilder instead.
    LynxEnv.inst().init(this, null, null, null)
  }
}
