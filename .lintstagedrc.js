module.exports = {
  "**/*.purs": [
    "purty format --write",
    () => "spago test",
  ],
};
