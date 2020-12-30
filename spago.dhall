{ name = "hacss"
, dependencies =
  [ "console"
  , "effect"
  , "foreign"
  , "generics-rep"
  , "newtype"
  , "profunctor-lenses"
  , "psci-support"
  , "spec"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
