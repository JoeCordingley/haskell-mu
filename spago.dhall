{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ "console", "effect", "psci-support", "halogen" ]
, packages = ./packages.dhall
, sources = [ "src/purescript/**/*.purs", "test/purescript/**/*.purs" ]
}
