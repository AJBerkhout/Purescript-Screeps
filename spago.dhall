{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "argonaut"
    , "console"
    , "debug"
    , "effect"
    , "either"
    , "exceptions"
    , "foreign-object"
    , "functions"
    , "generics-rep"
    , "maybe"
    , "ordered-collections"
    , "partial"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
