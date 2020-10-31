module.exports = {
  "**/*.purs": [
    files => files.map(f => `purty format --write ${f}`),
    () => "spago test",
  ],
};
