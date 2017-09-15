module InternalTest exposing (..)

import Debouncer.Internal exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Fuzzers exposing (..)
import Test exposing (..)


testCancel : Test
testCancel =
    fuzz2 (fuzzSimpleDebouncer int)
        fuzzCheck
        "if you cancel a debouncer, and then check it, it won't emit anything"
        (\debouncer msg ->
            cancel debouncer
                |> update msg
                |> (\( _, _, emitted ) ->
                        Expect.equal emitted Nothing
                   )
        )
