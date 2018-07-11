module InternalTest exposing (..)

import Debouncer.Internal exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Fuzzers exposing (..)
import Test exposing (..)
import Time exposing (second)


expectEmitted : c -> ( a, b, Maybe c ) -> Expectation
expectEmitted expected ( state, times, emitted ) =
    Expect.equal (Just expected) emitted


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


testThrottle : Test
testThrottle =
    describe "throttle"
        [ test "Always emits first input" <|
            \_ ->
                throttle (3 * second)
                    |> toDebouncer
                    |> update (InputProvidedAt 1 2000)
                    |> expectEmitted 1
        ]
