#! /bin/sh

set -e
elm-test
cd examples
elm-make --yes --output build/Basic.html src/Basic.elm
elm-make --yes --output build/Messages.html src/Messages.elm
