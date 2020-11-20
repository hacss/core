{ name = "hacss"
, dependencies =
  [ "console"
  , "effect"
  , "foreign-object"
  , "generics-rep"
  , "newtype"
  , "nullable"
  , "profunctor-lenses"
  , "psci-support"
  , "spec"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
