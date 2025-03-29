import { defineConfig } from "eslint/config";
import globals from "globals";
import js from "@eslint/js";

export default defineConfig([
  { files: ["**/*.{js,mjs,cjs,ts}"] },
  { files: ["**/*.js","**/*.ts"], languageOptions: { sourceType: "script" } },
  {
    files: ["**/*.{js,mjs,cjs,ts}"],
    languageOptions: {
      globals: { ...globals.browser, ...globals.node },
    },
  },
  {
    files: ["**/*.{js,mjs,cjs,ts}"],
    plugins: { js },
    extends: ["js/recommended"],
  },
  {
    rules: {
      "no-global-assign": ["error", { exceptions: ["miso"] }],
      "arrow-body-style": ["error", "always"],
    },
  },
]);
