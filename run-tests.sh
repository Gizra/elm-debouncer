#! /bin/sh

set -e
elm-test
pushd examples
elm-make --yes Basic.elm
popd
