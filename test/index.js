var Diff = require("line-diff");
var assert = require("assert");
var fs = require("fs");
var path = require("path");
var hacss = require("../dist/hacss.umd.js");

process.stdout.write(
  "* Context classes can contain word characters and hyphens...",
);
assert.equal(hacss("Foo-_bar+color:red;").match(/\S+/)[0], ".Foo-_bar");
process.stdout.write("success.\n");

process.stdout.write(
  "* Media query aliases can contain word characters and hyphens...",
);
assert.equal(
  hacss("@Foo-_bar{color:red;}", {
    mediaQueries: { "Foo-_bar": "foobar" },
  }).substring(0, 13),
  "@media foobar",
);
process.stdout.write("success.\n");

var config = {
  mediaQueries: {
    medium: "only screen and (min-width: 600px) and (max-width: 1199px)",
    large: "only screen and (min-width: 1200px)",
  },
  plugins: [
    function (decls) {
      if ("font-family" in decls) {
        decls["font-family"] = decls["font-family"].replace(
          "sans-serif",
          "'Inter', sans-serif",
        );
      }
      return decls;
    },
  ],
};

fs.readFile(path.join(__dirname, "index.html"), "utf8", function (err, code) {
  if (err) throw err;
  fs.readFile(path.join(__dirname, "styles.css"), "utf8", function (
    err,
    expected,
  ) {
    if (err) throw err;
    var actual = hacss(code, config);
    var diff = new Diff(expected, actual);
    console.log(diff.toString());
    if (process.argv.indexOf("--update") !== -1) {
      fs.writeFile(path.join(__dirname, "styles.css"), actual, function (err) {
        if (err) throw err;
        console.info("Updated reference style sheet.");
      });
    } else {
      assert.equal(
        0,
        diff.changes.filter(function (c) {
          return c.modified;
        }).length,
      );
    }
  });
});
