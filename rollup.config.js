const commonjs = require("@rollup/plugin-commonjs");
const json = require("@rollup/plugin-json");
const modify = require("rollup-plugin-modify");
const { nodeResolve } = require("@rollup/plugin-node-resolve");
const path = require("path");
const { terser } = require("rollup-plugin-terser");

const bundleDefaults = f => ({
  file: path.join(__dirname, "dist", f),
  format: "umd",
  name: "hacss"
});

module.exports = {
  input: "./output/Hacss",
  output: [
    bundleDefaults("hacss.umd.js"),
    {
      ...bundleDefaults("hacss.umd.min.js"),
      sourcemap: true,
      plugins: [terser()],
    },
  ],
  plugins: [
    modify({
      find: /module\.exports\s*=\s*\{[^\}]+\}/m,
      replace: "module.exports = unsafeForeignHacss",
    }),
    commonjs(),
    nodeResolve(),
    json(),
  ],
}
