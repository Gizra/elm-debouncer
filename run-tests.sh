#! /bin/sh

set -e
elm-doc-test
elm-test
cd examples
elm make --output build/Basic.html src/Basic.elm
elm make --output build/Messages.html src/Messages.elm
