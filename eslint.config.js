import { defineConfig } from "eslint/config";
import globals from "globals";
import js from "@eslint/js";
import jestPlugin from "eslint-plugin-jest";

export default defineConfig([
  { files: ["**/*.{js,mjs,cjs}"] },
  { files: ["**/*.js"], languageOptions: { sourceType: "script" } },
  { files: ["**/*.{js,mjs,cjs}"], languageOptions: { globals: {...globals.browser, ...globals.node, ...globals.jest} } },
  { files: ["**/*.{js,mjs,cjs}"], plugins: { js, jestPlugin }, extends: ["js/recommended"] },
  { rules: {
      "no-global-assign": ["error", {"exceptions": ["module"]}]
	},
  },
]);
