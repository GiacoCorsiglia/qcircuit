// @ts-expect-error Node types are not included.
import { resolve } from "path";
import { defineConfig } from "vite";

const extensions = {
  es: "mjs",
  cjs: "cjs",
  umd: "js",
};

export default defineConfig({
  build: {
    lib: {
      // @ts-expect-error Node types are not included.
      entry: resolve(__dirname, "/src/qcircuit.ts"),
      name: "qcircuit",
      fileName(format, entryName) {
        if (!(format in extensions)) {
          throw new Error("Unknown extension");
        }
        return `${entryName}.${extensions[format]}`;
      },
    },
    rollupOptions: {
      // Force the CSS file to also be called "qcircuit.css".
      output: {
        assetFileNames: "qcircuit.[ext]",
      },
    },
  },
});
