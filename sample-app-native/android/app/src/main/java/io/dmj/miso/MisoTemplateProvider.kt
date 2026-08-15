package io.dmj.miso

import android.content.Context
import com.lynx.tasm.provider.AbsTemplateProvider

// Loads main.lynx.bundle from the app's assets/ dir and hands the raw bytes to
// the Lynx runtime. Equivalent to the iOS TemplateProvider. `renderTemplateUrl`
// in MainActivity passes the uri ("main.lynx.bundle") through to loadTemplate.
class MisoTemplateProvider(private val context: Context) : AbsTemplateProvider() {
  override fun loadTemplate(uri: String, callback: Callback) {
    try {
      context.assets.open(uri).use { input ->
        callback.onSuccess(input.readBytes())
      }
    } catch (e: Exception) {
      callback.onFailed(e.message ?: "failed to load $uri from assets")
    }
  }
}
