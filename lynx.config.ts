import { defineConfig } from '@lynx-js/rspeedy';
import { pluginReactLynx } from '@lynx-js/react-rsbuild-plugin';
export default defineConfig({ source: { entry: './all.js' }, plugins: [ pluginReactLynx() ] });
