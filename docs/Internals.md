Internals
===========================

## Overview

Miso’s external API has three main parts. The `model` (parameterized by `action`), the `view` function and `update` function. 

## Concurrency

Under the hood miso’s concurrency model centers around an atomically updated `IORef (Seq action)` (known as queue). The queue is used to capture events and update the user-defined `model` [^1]. Each captured browser event causes a green thread [^2] to be forked, and an `action` appended to the queue.

## Event Loop

`miso` operates in an event [loop](https://github.com/dmjio/miso/blob/master/src/Miso.hs#L124) that blocks on [requestAnimationFrame](https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame). The `model` lives on the stack of the event loop function. Inside this loop we drain the queue of `action`s, and fold them against the `model` (known as “batching”). We then diff the new `model` against the old `model`. Finally, if the `model` has been updated (is dirty), `view` is invoked on the updated `model` to get a `VTree` [^3]. This `VTree` gets converted to a JS tree and is diffed + patched against the existing tree (that lives on the JS heap) and the DOM gets updated. The loop recurses on the new `model`.

## Diffing and patching (simultaneously)

During [diffing](https://github.com/dmjio/miso/blob/master/jsbits/miso.js#L3) we mutually recurse over the parent and child nodes of both the old and new virtual DOM structures. During this process we diff the old tree against the new tree (while simultaneously updating the DOM to reflect the new structure) [^4]. We apply various new optimizations not seen in other frameworks (like react) on the child lists (see [syncChildren](https://github.com/dmjio/miso/blob/master/jsbits/miso.js#L187)). During diffing we also invoke node creation / destruction life cycle hooks.

## Events

While the event loop is executing, browser events are raised asynchronously and delegated into Haskell callbacks that live on the virtual DOM in the JS heap [^6]. The event body is parsed into a Haskell structure via JSON (FromJSON), the update function is invoked to produce an Action that gets written to the queue atomically. Event delegation and DOM diffing occur simultaneously.

## Misc. concurrency

`subs` and `sink` are ways to write into the actions queue externally (useful for integration with third party components). There are some predefined `subs` in `Miso.Subscription` for conveniently working with the History, Websocket, Keyboard and Mouse APIs.

## Pre-rendering

Pre-rendering (using the `miso` function) on application startup will traverse the DOM and copy pointers into miso’s virtual DOM structure (this process is known as hydration). This is necessary for events to work, since event delegation works by DOM pointer traversal on the virtual DOM to find the correct node to dispatch the event [^6].

## View

The `view` function is how templating works in miso. The `View` is a rose tree that represents the DOM. This function is used in the event loop to construct new virtual DOMs in response to browser events.

[^1]: `Seq` is used to aid event ordering and avoid excessive redraws.

[^2]: Green threads are very cheap in Haskell. The GHCJS and GHC RTS (w/ WASM backend) should have equivalent operating semantics for threads.

[^3]: Since events can be no-ops we want to avoid generating a tree if the model hasn’t changed. Miso uses both `Eq` and `StablePtr` equality to determine if a draw is necessary. The `StablePtr` equality is an optimization that avoids expensive calls to `(==)` on large models.

[^4]: This diff + patch approach is responsible for a lot of performance gains. We don't generate a list of patches and apply them in a separate phase like some other frameworks.

[^5]: `VTree` is the Haskell AST version of a JS virtual DOM. The `view` function constructs terms in this AST, it is then lowered into a `JSVal`. The `JSVal` is a virtual DOM tree structure that lives in the JS heap that is used for diffing. Once lowered, we diff against the existing virtual DOM that already lives in the JS heap.

[^6]: The event handlers that live on the JS virtual DOM are how miso calls back into the Haskell heap from the JS heap to write to the `Action` queue. Events are defined on the `View` using the `onWithOptions` function. Lifecycle are also defined using the View DSL. 
