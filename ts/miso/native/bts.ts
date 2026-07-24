/*

[BTS notes]
  - Setup event listener for main thread communication (requires abstracting out delegator into context object).
  - Add handler for receiving events from main thread (meant for processing on bg thread)
    - Receives stack then dispatches through VDOM, causes diff which creates patches sent to main thread
    - Send patches from post-diff `flush`() w/ `componentIds` to main thread
  - Once `miso` is on `npm` then consume `miso` typescript from `miso-native` (to avoid type duplication).

*/

export function bts () {
  'background only'
}
