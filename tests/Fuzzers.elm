module Fuzzers exposing (..)

import Debouncer.Internal exposing (..)
import Fuzz exposing (Fuzzer)
import Time exposing (Time)


{-| We just need a kind of arbitrary time range to work within. Note that
this is for times, not intervals. We make this relatively small so that
our random intervals have a reasonable chance of tripping over the times.
-}
fuzzTime : Fuzzer Time
fuzzTime =
    Fuzz.floatRange 10000 11000


{-| Fuzz the kind of intervals you'd expect.
-}
fuzzInterval : Fuzzer Time
fuzzInterval =
    Fuzz.floatRange 0 500


fuzzState : Fuzzer o -> Fuzzer (State o)
fuzzState outputFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Settled
        , Fuzz.map Unsettled <|
            Fuzz.map4 UnsettledState
                fuzzTime
                fuzzTime
                (Fuzz.maybe fuzzTime)
                (Fuzz.maybe outputFuzzer)
        ]


fuzzSimpleConfig : Fuzzer (Config i i)
fuzzSimpleConfig =
    Fuzz.map Config <|
        Fuzz.map4 ConfigRecord
            (Fuzz.maybe fuzzInterval)
            (Fuzz.maybe fuzzInterval)
            fuzzInterval
            fuzzSimpleAccumulator


fuzzSimpleAccumulator : Fuzzer (i -> Maybe i -> Maybe i)
fuzzSimpleAccumulator =
    Fuzz.oneOf
        [ Fuzz.constant lastInput
        , Fuzz.constant firstInput
        ]


{-| Because we're fuzzing the config and the state separately, we could
actually end up with impossible states here, I suppose. In a way, it would be
better to fuzz a Config, initialize a Debouncer, then fuzz some Msgs, and apply
them to the debouncer. But this will do for now.
-}
fuzzSimpleDebouncer : Fuzzer i -> Fuzzer (Debouncer i i)
fuzzSimpleDebouncer fuzzInput =
    Fuzz.map2 Debouncer fuzzSimpleConfig (fuzzState fuzzInput)


{-| I suppose it would be nice to have a mechanism for fuzzing the times with
guarantees that time moves forwards, not backwards! But this will do for now.
-}
fuzzMsg : Fuzzer i -> Fuzzer (Msg i)
fuzzMsg inputFuzzer =
    Fuzz.oneOf
        [ fuzzCheck
        , fuzzInputProvidedAt inputFuzzer
        ]


fuzzCheck : Fuzzer (Msg i)
fuzzCheck =
    Fuzz.map Check fuzzTime


fuzzInputProvidedAt : Fuzzer i -> Fuzzer (Msg i)
fuzzInputProvidedAt inputFuzzer =
    Fuzz.map2 InputProvidedAt inputFuzzer fuzzTime
