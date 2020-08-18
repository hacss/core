/* eslint no-unused-vars: "error" */
const { all: knownProperties } = require("known-css-properties");

import {
  adjust,
  always,
  apply,
  ascend,
  assoc,
  call,
  concat,
  defaultTo,
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
  startsWith,
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

const parseDeclarations = properties => {
  const property = join("|", properties);
  const value = "(([^\\s'{};]+)|'[^\\s']*'|\"[^\\s\"]*\")+";
  const pattern = new RegExp(`(${property}):(${value})`);
  return pipe(matchAll(pattern), map(props([1, 2])), fromPairs);
};

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

const mkPattern = (properties, mediaQueries = []) => {
  const nexpr = "[0-9\\+\\-n]+";
  const pseudoClassBase = [
    ":active",
    ":checked",
    ":disabled",
    ":empty",
    ":enabled",
    ":first-child",
    ":first-of-type",
    ":focus",
    ":focus-within",
    ":hover",
    ":in-range",
    ":invalid",
    ":last-child",
    ":last-of-type",
    ":link",
    ":only-of-type",
    ":only-child",
    ":optional",
    ":out-of-range",
    ":read-only",
    ":read-write",
    ":required",
    ":root",
    ":target",
    ":valid",
    ":visited",
    ...["child", "last-child", "last-of-type", "of-type"].map(
      x => `:nth-${x}\\(${nexpr}\\)`,
    ),
    ":lang([a-z]{2}([A-Za-z]{2})?)",
    ":intersection\\([\\w\\-]+\\)",
  ];
  const mkPseudoClasses = x => `${join("|", x)}|:not\\((${join("|", x)})+\\)`;
  const pseudoElements = join(
    "|",
    concat(
      map(concat("::"), [
        "after",
        "before",
        "first-letter",
        "first-line",
        "placeholder",
        "selection",
      ]),
      map(x => `::?-${x}-[a-z][a-z\\-]+[a-z]`, ["moz", "ms", "o", "webkit"]),
    ),
  );
  const property = join("|", properties);
  const value = "(([^\\s'{};]+)|'[^\\s']*'|\"[^\\s\"]*\")+";
  const declaration = `(${property}):(${value})`;
  const declarations = `(${declaration};)+`;
  const base = `((${mkPseudoClasses(
    pseudoClassBase,
  )}|${pseudoElements})+){(${declarations})}|(${declarations})`;
  const context = `([\\w\\-]+)((${mkPseudoClasses(pseudoClassBase)})*)([~_+>])`;
  const baseWithContext = `(${context})?(${base})`;
  return new RegExp(
    `@(${join("|", mediaQueries)}){${baseWithContext}}|${baseWithContext}`,
  );
};

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

  const pattern = mkPattern(properties, keys(mediaQueries));

  return pipe(
    replace(/&gt;/g, ">"),
    matchAll(pattern),
    uniqBy(head),
    map(
      pipe(
        flip(repeat)(7),
        adjust(0, prop(0)),
        adjust(1, prop(1)),
        adjust(2, pipe(props([3, 33]), filter(identity), head)),
        adjust(3, pipe(props([4, 34]), filter(identity), head)),
        adjust(4, pipe(props([11, 39, 41]), filter(identity), head)),
        adjust(5, pipe(props([13, 43]), filter(identity), head)),
        adjust(6, pipe(props([20, 26, 50, 56]), filter(identity), head)),
        zip([
          "className",
          "mediaQuery",
          "contextName",
          "contextPseudos",
          "combinator",
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
        pipe(
          prop("contextPseudos"),
          defaultTo(""),
          match(/:[^:]+/g),
          pseudoWeight,
        ),
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
              contextName: pair(null),
              contextPseudos: pipe(
                when(o(not, isNil), match(/:not\(:[^:]+\)|:{1,2}[^:]+/g)),
                pair(null),
              ),
              className: pipe(o(concat("."), CSS.escape), pair(null)),
              declarations: pipe(
                parseDeclarations(properties),
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
              combinator: pipe(
                flip(prop)({ _: " ", "~": " ~ ", "+": " + ", ">": " > " }),
                defaultTo(null),
                pair(null),
              ),
              pseudos: pipe(
                when(o(not, isNil), match(/:not\(:[^:]+\)|:{1,2}[^:]+/g)),
                pair(null),
              ),
            }),
            when(
              pipe(filter(nth(0)), keys, length, equals(0)),
              pipe(
                computeField(
                  "css",
                  pipe(
                    identity,
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
                        computeField(
                          "contextIntersections",
                          pipe(
                            prop("contextPseudos"),
                            defaultTo([]),
                            filter(startsWith(":intersection")),
                            map(
                              pipe(
                                match(/:intersection\((.*)\)/),
                                nth(1),
                                concat("."),
                              ),
                            ),
                            ifElse(
                              pipe(length, equals(0)),
                              always(null),
                              join(""),
                            ),
                          ),
                        ),
                        computeField(
                          "intersections",
                          pipe(
                            prop("pseudos"),
                            defaultTo([]),
                            filter(startsWith(":intersection")),
                            map(
                              pipe(
                                match(/:intersection\((.*)\)/),
                                nth(1),
                                concat("."),
                              ),
                            ),
                            ifElse(
                              pipe(length, equals(0)),
                              always(null),
                              join(""),
                            ),
                          ),
                        ),
                        applyRecord({
                          contextPseudos: when(
                            o(not, isNil),
                            pipe(
                              map(
                                when(
                                  startsWith(":not(:intersection("),
                                  pipe(
                                    match(/:intersection\((.*?)\)/),
                                    nth(1),
                                    concat(":not(."),
                                    flip(concat)(")"),
                                  ),
                                ),
                              ),
                              filter(o(not, startsWith(":intersection"))),
                              join(""),
                            ),
                          ),
                          pseudos: when(
                            o(not, isNil),
                            pipe(
                              map(
                                when(
                                  startsWith(":not(:intersection("),
                                  pipe(
                                    match(/:intersection\((.*?)\)/),
                                    nth(1),
                                    concat(":not(."),
                                    flip(concat)(")"),
                                  ),
                                ),
                              ),
                              filter(o(not, startsWith(":intersection"))),
                              join(""),
                            ),
                          ),
                          declarations: pipe(concat("{"), flip(concat)("}")),
                        }),
                        computeField(
                          "context",
                          pipe(
                            props([
                              "contextName",
                              "contextIntersections",
                              "contextPseudos",
                            ]),
                            join(""),
                            when(o(not, isEmpty), concat(".")),
                          ),
                        ),
                        props([
                          "context",
                          "combinator",
                          "className",
                          "intersections",
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
                          combinator: "combinator",
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
