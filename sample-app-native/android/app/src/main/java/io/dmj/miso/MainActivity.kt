package io.dmj.miso

import android.app.Activity
import android.os.Bundle
import android.view.ViewGroup
import com.lynx.tasm.LynxView
import com.lynx.tasm.LynxViewBuilder
import com.lynx.xelement.XElementBehaviors

// Thin host: build a LynxView, register the extended (X) element behaviors the
// gallery uses, point it at the assets template provider, and render the same
// main.lynx.bundle the iOS host loads. Mirrors KotlinEmptyProject's MainActivity.
class MainActivity : Activity() {
  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)

    val builder = LynxViewBuilder()
    builder.addBehaviors(XElementBehaviors().create())
    builder.setTemplateProvider(MisoTemplateProvider(this))

    val lynxView: LynxView = builder.build(this)
    lynxView.layoutParams = ViewGroup.LayoutParams(
      ViewGroup.LayoutParams.MATCH_PARENT,
      ViewGroup.LayoutParams.MATCH_PARENT,
    )
    setContentView(lynxView)

    // Resolved by MisoTemplateProvider against assets/main.lynx.bundle.
    lynxView.renderTemplateUrl("main.lynx.bundle", "")
  }
}
