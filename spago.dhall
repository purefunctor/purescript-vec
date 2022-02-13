let test-dependencies = [ "aff", "spec" ]

in  { name = "vec"
    , dependencies =
          [ "effect"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "prelude"
          , "typelevel-prelude"
          ]
        # test-dependencies
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    }
