var Diff = require("line-diff");
var assert = require("assert");
var fs = require("fs");
var path = require("path");
var hacss = require("../dist/hacss.umd.js");

process.stdout.write(
  "* Context classes can contain word characters and hyphens...",
);
assert.equal(hacss("Foo-_bar+color:red;").css.match(/\S+/)[0], ".Foo-_bar");
process.stdout.write("success.\n");

process.stdout.write(
  "* Media query aliases can contain word characters and hyphens...",
);
assert.equal(
  hacss("@Foo-_bar{color:red;}", {
    mediaQueries: { "Foo-_bar": "foobar" },
  }).css.substring(0, 13),
  "@media foobar",
);
process.stdout.write("success.\n");

process.stdout.write(
  "* Media query rules appear according to configured order...",
);

{
  const actual = hacss(
    `
    <div class="
      @csmall{font-size:10px;}
      @blarge{font-size:18px;}
      @amedium{font-size:14px;}
    ">
      Hello
    </div>
  `,
    {
      mediaQueries: {
        blarge: "LARGE",
        amedium: "MEDIUM",
        csmall: "SMALL",
      },
    },
  )
    .css.split("\n")
    .map(x => x.match(/[^{]+/)[0]);

  assert.deepEqual(actual, ["@media LARGE", "@media MEDIUM", "@media SMALL"]);
}
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

process.stdout.write("* Invalid rules are returned as 'ignored'...");
assert.equal(hacss('color:re">d;').ignored[0].className, 'color:re">d;');
process.stdout.write("success.\n");

process.stdout.write("* Vanilla plugins affect declarations...");
assert.equal(
  hacss("color:blue;", {
    plugins: [
      decls => {
        decls["background"] = decls["color"];
        delete decls["color"];
        return decls;
      },
    ],
  }).css,
  ".color\\:blue\\;{background:blue;}",
);
process.stdout.write("success.\n");

process.stdout.write("* Invalid plugins have no effect...");
assert.equal(
  hacss("color:red;", { plugins: ["foo", null, () => "foo"] }).css,
  ".color\\:red\\;{color:red;}",
);
process.stdout.write("success.\n");

process.stdout.write("* A scope is added to each rule if specified...");
assert.equal(
  hacss("color:red;", { scope: "#foo" }).css.match(/\S+/)[0],
  "#foo",
);
process.stdout.write("success.\n");

process.stdout.write(
  "* &gt; HTML entity is decoded for immediate-child selection...",
);
assert.equal(
  hacss("item&gt;color:red;").css,
  ".item > .item\\>color\\:red\\;{color:red;}",
);
process.stdout.write("success.\n");

process.stdout.write(
  "* Arbitrary vendor-prefixed pseudo-elements are allowed...",
);
["ms", "moz", "o", "webkit"]
  .flatMap(x => [`:-${x}-foo-bar`, `::-${x}-something`])
  .forEach(x => {
    assert.equal(
      hacss(`${x}{color:red;}`).css,
      `.${x.replace(/:/g, "\\:")}\\{color\\:red\\;\\}${x}{color:red;}`,
    );
  });
process.stdout.write("success.\n");

process.stdout.write(
  "* Allows empty string values for e.g. pseudo-element content.",
);
assert.equal(
  hacss("::after{content:'';}").css,
  ".\\:\\:after\\{content\\:\\'\\'\\;\\}::after{content:'';}",
);
process.stdout.write("success.\n");

process.stdout.write("* Intersection selectors can be used for context.");
assert.equal(
  hacss("input:not(:intersection(success)):intersection(error)~color:red;").css,
  ".input.error:not(.success) ~ .input\\:not\\(\\:intersection\\(success\\)\\)\\:intersection\\(error\\)\\~color\\:red\\;{color:red;}",
);
process.stdout.write("...success.\n");

process.stdout.write("\n");

fs.readFile(path.join(__dirname, "index.html"), "utf8", function (err, code) {
  if (err) throw err;
  fs.readFile(path.join(__dirname, "styles.css"), "utf8", function (
    err,
    expected,
  ) {
    if (err) throw err;
    var actual = hacss(code, config).css;
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
