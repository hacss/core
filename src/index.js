const { all: knownProperties } = require("known-css-properties");

import {
  addIndex,
  adjust,
  always,
  apply,
  ascend,
  assoc,
  call,
  concat,
  contains,
  defaultTo,
  drop,
  equals,
  filter,
  flatten,
  flip,
  fromPairs,
  head,
  identity,
  ifElse,
  indexOf,
  insert,
  is,
  isEmpty,
  isNil,
  join,
  keys,
  length,
  map,
  mapObjIndexed,
  match,
  max,
  mergeRight,
  not,
  nth,
  nthArg,
  o,
  pair,
  pipe,
  prepend,
  prop,
  props,
  reduce,
  repeat,
  replace,
  reverse,
  sortWith,
  split,
  take,
  toPairs,
  tryCatch,
  unapply,
  uniqBy,
  when,
  zip,
} from "ramda";

import composePlugins from "@hacss/compose-plugins";
import assertValidRule from "postcss/lib/parse.js";

const DEFAULT_MEDIA_QUERIES = {
  small: "only screen and (max-width: 599px)",
  medium: "only screen and (min-width: 600px) and (max-width: 999px)",
  large: "only screen and (min-width: 1000px)",
};

const applyRecord = f =>
  mapObjIndexed(
    pipe(
      unapply(identity),
      take(2),
      reverse,
      adjust(0, pipe(flip(prop)(f), defaultTo(identity))),
      apply(call),
    ),
  );

const computeField = (key, fn) =>
  pipe(flip(repeat)(2), adjust(0, fn), insert(0, key), apply(assoc));

const matchAll = pattern => str => {
  if (!pattern || !str) {
    return [];
  }
  const matches = [];
  for (
    let s = str, match = s.match(pattern);
    s && match;
    s = s.substring(match.index + match[0].length), match = s.match(pattern)
  ) {
    matches.push(match);
  }
  return matches;
};

const balanced = pipe(
  flip(repeat)(2),
  adjust(0, pipe(match(/\{/g), length)),
  adjust(1, pipe(match(/\}/g), length)),
  apply(equals),
);

const pseudoWeight = pipe(
  map(
    flip(indexOf)([
      ":link",
      ":visited",
      ":focus",
      ":hover",
      ":active",
      ":disabled",
    ]),
  ),
  reduce(max, -1),
);

const parseDeclarations = pipe(
  split(";"),
  filter(o(not, isEmpty)),
  map(pipe(match(/([^:]+):(.+)/), drop(1))),
  fromPairs,
);

const applyFixes = map(
  pipe(
    replace(
      /calc\(.+\)/g,
      replace(/[+\-*/]/g, o(concat(" "), flip(concat)(" "))),
    ),
    replace(/__/g, " "),
  ),
);

const stringifyDeclarations = pipe(
  toPairs,
  map(o(flip(concat)(";"), join(":"))),
  join(""),
);

const build = config => {
  const [applyPlugins, properties] = call(
    pipe(
      prop("plugins"),
      defaultTo([]),
      concat([[identity, knownProperties]]),
      apply(composePlugins),
    ),
    config,
  );

  const prependScope = call(
    pipe(
      prop("scope"),
      ifElse(isNil, always(identity), pipe(flip(concat)(" "), prepend)),
    ),
    config,
  );

  const mediaQueries = mergeRight(
    DEFAULT_MEDIA_QUERIES,
    config.mediaQueries || [],
  );

  const pattern = new RegExp(
    `(@([\\-\\w]+){)?(([\\-\\w]+(:[^\\s]+)?)([_+>]))?((:[^\\s{]+){)?(((${join(
      "|",
      properties,
    )}):[^\\s\\{\\};]+;)+)[}]*`,
  );

  return pipe(
    matchAll(pattern),
    filter(o(balanced, head)),
    uniqBy(head),
    map(
      pipe(
        addIndex(filter)(pipe(nthArg(1), flip(contains)([0, 2, 4, 6, 8, 9]))),
        zip([
          "className",
          "mediaQuery",
          "context",
          "operator",
          "pseudos",
          "declarations",
        ]),
        fromPairs,
      ),
    ),
    sortWith([
      ascend(
        pipe(
          prop("mediaQuery"),
          ifElse(isNil, always(-1), flip(indexOf)(keys(mediaQueries))),
        ),
      ),
      ascend(
        pipe(prop("context"), defaultTo(""), match(/:[^:]+/g), pseudoWeight),
      ),
      ascend(
        pipe(prop("pseudos"), defaultTo(""), match(/:[^:]+/g), pseudoWeight),
      ),
      ascend(prop("className")),
    ]),
    map(
      pipe(
        flip(repeat)(2),
        adjust(0, prop("className")),
        adjust(
          1,
          pipe(
            applyRecord({
              className: pipe(o(concat("."), CSS.escape), pair(null)),
              context: pipe(
                when(o(not, isNil), concat(".")),
                defaultTo(null),
                pair(null),
              ),
              declarations: pipe(
                parseDeclarations,
                applyFixes,
                tryCatch(
                  pipe(applyPlugins, stringifyDeclarations, pair(null)),
                  pipe(prop("message"), flip(pair)(null)),
                ),
              ),
              mediaQuery: ifElse(
                isNil,
                always([null, null]),
                pipe(
                  flip(repeat)(2),
                  adjust(0, flip(prop)(mediaQueries)),
                  ifElse(
                    pipe(nth(0), is(String)),
                    pipe(nth(0), concat("@media "), pair(null)),
                    pipe(
                      nth(1),
                      concat("Unrecognized media query: "),
                      flip(pair)(null),
                    ),
                  ),
                ),
              ),
              operator: pipe(
                flip(prop)({ _: " ", "+": " + ", ">": " > " }),
                defaultTo(null),
                pair(null),
              ),
              pseudos: pair(null),
            }),
            when(
              pipe(filter(nth(0)), keys, length, equals(0)),
              pipe(
                computeField(
                  "css",
                  pipe(
                    map(nth(1)),
                    flip(repeat)(2),
                    adjust(
                      0,
                      pipe(
                        prop("mediaQuery"),
                        ifElse(isNil, always(identity), mq => x =>
                          `${mq}{${x}}`,
                        ),
                      ),
                    ),
                    adjust(
                      1,
                      pipe(
                        applyRecord({
                          declarations: pipe(concat("{"), flip(concat)("}")),
                        }),
                        props([
                          "context",
                          "operator",
                          "className",
                          "pseudos",
                          "declarations",
                        ]),
                        prependScope,
                        filter(identity),
                        join(""),
                      ),
                    ),
                    apply(call),
                    tryCatch(
                      pipe(assertValidRule, pair(null)),
                      pipe(
                        props(["message", "source"]),
                        join(" in "),
                        flip(pair)(null),
                      ),
                    ),
                  ),
                ),
              ),
            ),
            ifElse(
              pipe(filter(nth(0)), keys, length, equals(0)),
              pipe(prop("css"), nth(1), pair(null)),
              pipe(
                map(nth(0)),
                filter(identity),
                toPairs,
                map(
                  pipe(
                    adjust(
                      0,
                      pipe(
                        flip(prop)({
                          className: "class name",
                          context: "context class",
                          css: "CSS output",
                          declarations: "declarations",
                          mediaQuery: "media query",
                          operator: "context operator",
                          pseudos: "pseudos",
                        }),
                        when(o(not, isNil), concat("Error in ")),
                        defaultTo("Error"),
                      ),
                    ),
                    join(": "),
                  ),
                ),
                head,
                flip(pair)(null),
              ),
            ),
          ),
        ),
        flatten,
      ),
    ),
    flip(repeat)(2),
    adjust(0, pipe(map(nth(2)), filter(identity), join("\n"))),
    adjust(
      1,
      pipe(
        filter(nth(1)),
        map(pipe(take(2), zip(["className", "error"]), fromPairs)),
      ),
    ),
    zip(["css", "ignored"]),
    fromPairs,
  );
};

export default (code, config = {}) => build(config)(code);
