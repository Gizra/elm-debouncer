#! /bin/sh

set -e
elm-test
pushd examples
elm-make --yes src/Basic.elm
elm-make --yes src/Messages.elm
popd
