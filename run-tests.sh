#! /bin/sh

set -e
elm-test
cd examples
elm-make --yes src/Basic.elm
elm-make --yes src/Messages.elm
