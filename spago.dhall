{ name = "hacss"
, dependencies =
  [ "console"
  , "effect"
  , "foreign-object"
  , "generics-rep"
  , "newtype"
  , "nullable"
  , "option"
  , "parsing"
  , "psci-support"
  , "spec"
  , "spec-quickcheck"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
