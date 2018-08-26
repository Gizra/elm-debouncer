module Debouncer.Internal exposing (Accumulator, Config(..), ConfigRecord, Debouncer(..), Milliseconds, Msg(..), State(..), UnsettledState, accumulateWith, addInputs, allInputs, appendInputToOutput, appendOutputToInput, cancel, debounce, emitFirstInput, emitWhenUnsettled, emitWhileUnsettled, firstInput, lastInput, manual, nothingIfNegative, sanitizeConfig, settleWhenQuietFor, throttle, toDebouncer, update)

{-| The purpose of this module is to expose the "guts" of the logic so that it
can be tested. It is not in the "exposed-modules" of the elm-package.json file,
so it's not directly accessible to clients of the package.

Testing just the things exposed to clients of the package would be awkward,
since it would require asynchronous tests. Here, we don't do anything actually
asynchronous:

  - You provide the `Time` with all messages (so it's just data).

  - When we want to schedule a `Check`, we just return a `List Time` ... it's
    the caller's job to actually schedule it.

The user-facing documentation for these types is in `Debouncer.Basic` and
`Debouncer.Messages`, since those are the exposed modules.

-}

import List.Extra


{-| A useful reminder of what our intervals mean.
-}
type alias Milliseconds =
    Int


{-| Our basic type. Pairs a configuration with some state that changes as
inputs are received and outputs are provided.
-}
type Debouncer i o
    = Debouncer (Config i o) (State o)


{-| Cancelation is one way we can directly modify a debouncer. Basically, we
just forget our state. There may be future `Check ...` messages coming, but
they will consider what to do based solely on our state at that time. So,
forgetting our state is sufficient to "cancel" any pending output.
-}
cancel : Debouncer i o -> Debouncer i o
cancel (Debouncer config state) =
    Debouncer config Settled


{-| A type which represents a function which incorporates an input into
the output so far.
-}
type alias Accumulator i o =
    i -> Maybe o -> Maybe o


{-| Accumulates by just keeping the last input.
-}
lastInput : Accumulator i i
lastInput i o =
    Just i


{-| Accumulates by just keeping the first input.
-}
firstInput : Accumulator i i
firstInput i =
    Just << Maybe.withDefault i


{-| Accumulates all inputs in a list.

Note that the output list will have the most recent input first.

-}
allInputs : Accumulator i (List i)
allInputs i o =
    Just (i :: Maybe.withDefault [] o)


{-| Accumulates by taking a sum of the inputs.
-}
addInputs : Accumulator number number
addInputs i o =
    Just (i + Maybe.withDefault 0 o)


{-| Accumulates by appending the input to the output.
-}
appendInputToOutput : Accumulator appendable appendable
appendInputToOutput i o =
    Just <|
        Maybe.withDefault i <|
            Maybe.map (\acc -> acc ++ i) o


{-| Accumulates by appending the output to the input.
-}
appendOutputToInput : Accumulator appendable appendable
appendOutputToInput i o =
    Just <|
        Maybe.withDefault i <|
            Maybe.map (\acc -> i ++ acc) o


{-| An opaque type representing the configuration for a debouncer.

  - We provide various "constructor" functions (rather than exposing the
    type to clients) so that we can change the type in future without
    a major version bump.

  - We require a `Config` that is separate from the `Debouncer` itself,
    because changes to the configuration once the debouncer is actually
    operating would need to be handled specially, via messages.

-}
type Config i o
    = Config (ConfigRecord i o)


{-| Each of the times is a `Maybe`, since you don't necessarily want to
automatically emit at all for one kind of event or another.

If all three are `Nothing`, then the debouncer will never automatically
emit -- you would have to tell it to emit at some point, based on some
external event.

-}
type alias ConfigRecord i o =
    { emitWhenUnsettled : Maybe Milliseconds
    , emitWhileUnsettled : Maybe Milliseconds
    , settleWhenQuietFor : Maybe Milliseconds
    , accumulator : Accumulator i o
    }


{-| A starting point for a "manual" configuration.

By default, it:

  - never settles
  - does not emit when unsettled
  - does not emit while unsettled
  - accumulates only the last input

So, without more, you would need to tell this debouncer when to emit something
-- it would never happen automatically.

To change any of those parameters, use the various functions that alter a
`Config` (i.e. `settleWhenQuietFor`, `emitWhenUnsettled`, `emitWhileUnsettled`).

By default, the output type is the same as the input type. However, you can
change that by using the `accumulateWith` function to provide a different
accumulator.

-}
manual : Config i i
manual =
    Config
        { emitWhenUnsettled = Nothing
        , emitWhileUnsettled = Nothing
        , settleWhenQuietFor = Nothing
        , accumulator = lastInput
        }


{-| A starting point for a configuration which debounces -- that is,
which will emit once quiet for the time you specify.

So, `debounce (2 * Time.second)` is equivalent to

    manual
        |> settleWhenQuietFor (Just (2 * Time.second))

If you also want to emit using the first input, then you can use
`emitWhenSettled`. For instance, the following configuration would
emit the first input immediately when becoming unsettled, and then
emit any subsequent input once the debouncer was quiet for 2 seconds.

    debounce (2 * Time.second)
        |> emitWhenUnsettled (Just 0)

-}
debounce : Milliseconds -> Config i i
debounce interval =
    settleWhenQuietFor (Just interval) manual


{-| A starting point for a configuration which throttles -- that is,
which will emit the first input immediately, and then accmulate and
emit no more often than the specified interval.

So, `throttle (2 * Time.second)` is equivalent to

    manual
        |> emitWhileUnsettled (Just (2 * Time.second))
        |> emitWhenUnsettled (Just 0)

-}
throttle : Milliseconds -> Config i i
throttle interval =
    manual
        |> emitWhileUnsettled (Just interval)
        |> emitWhenUnsettled (Just 0)


emitWhenUnsettled : Maybe Milliseconds -> Config i o -> Config i o
emitWhenUnsettled time (Config config) =
    Config
        { config | emitWhenUnsettled = time }


{-| Modify a `Config` by controlling whether to emit the first
input when becoming unsettled.

`emitFirstInput True config` is equivalent to

    emitWhenUnsettled (Just 0) config

`emitFirstInput False config` is equivalent to

    emitWhenUnsettled Nothing config

For more complex cases, where you want to emit the first input on
a different interval than others, but not immediately, you can
use `emitWhenSettled` directly.

-}
emitFirstInput : Bool -> Config i o -> Config i o
emitFirstInput choice =
    if choice then
        emitWhenUnsettled (Just 0)

    else
        emitWhenUnsettled Nothing


emitWhileUnsettled : Maybe Milliseconds -> Config i o -> Config i o
emitWhileUnsettled time (Config config) =
    Config
        { config | emitWhileUnsettled = time }


settleWhenQuietFor : Maybe Milliseconds -> Config i o -> Config i o
settleWhenQuietFor time (Config config) =
    Config
        { config | settleWhenQuietFor = time }


{-| Note that this changes the type of the `Config` to match the
type of the acummulator provided.
-}
accumulateWith : Accumulator i o -> Config a b -> Config i o
accumulateWith accumulator (Config config) =
    Config
        { accumulator = accumulator
        , emitWhenUnsettled = config.emitWhenUnsettled
        , emitWhileUnsettled = config.emitWhileUnsettled
        , settleWhenQuietFor = config.settleWhenQuietFor
        }


toDebouncer : Config i o -> Debouncer i o
toDebouncer config =
    Debouncer (sanitizeConfig config) Settled


{-| Sanitize the config to simplify some of the logic.
-}
sanitizeConfig : Config i o -> Config i o
sanitizeConfig (Config config) =
    Config
        { emitWhenUnsettled = nothingIfNegative config.emitWhenUnsettled
        , emitWhileUnsettled = nothingIfNegative config.emitWhileUnsettled
        , settleWhenQuietFor = nothingIfNegative config.settleWhenQuietFor
        , accumulator = config.accumulator
        }


nothingIfNegative : Maybe number -> Maybe number
nothingIfNegative =
    Maybe.andThen
        (\num ->
            if num < 0 then
                Nothing

            else
                Just num
        )


type State o
    = Settled
    | Unsettled (UnsettledState o)


{-| `output` needs to be a `Maybe` because we may have emitted while
unsettled, due to `emitWhileUnsettled`. And, we could arrive at
`settleWhenQuietFor` without any further input, so we don't
necessarily emit anything when we become settled.
-}
type alias UnsettledState o =
    { unsettledAt : Milliseconds
    , lastInputProvidedAt : Milliseconds
    , lastEmittedAt : Maybe Milliseconds
    , output : Maybe o
    }


type Msg i
    = InputProvidedAt i Milliseconds
    | ManualCancel
    | ManualSettle
    | ManualEmitAt Milliseconds
    | Check Milliseconds


{-| The second return parameter is not a `Cmd`, but instead a list of intervals
at which we ought to check whether to emit something. This assists with
testability (since we can test whether that list is correct). The caller is
responsible for actually turning that list into commands.
-}
update : Msg i -> Debouncer i o -> ( Debouncer i o, List Milliseconds, Maybe o )
update msg ((Debouncer ((Config config) as wrappedConfig) state) as debouncer) =
    case msg of
        InputProvidedAt input time ->
            let
                newState =
                    case state of
                        Settled ->
                            Unsettled
                                { unsettledAt = time
                                , lastInputProvidedAt = time
                                , lastEmittedAt = Nothing
                                , output = config.accumulator input Nothing
                                }

                        Unsettled unsettled ->
                            Unsettled
                                { unsettled
                                    | output = config.accumulator input unsettled.output
                                    , lastInputProvidedAt = time
                                }

                newDebouncer =
                    Debouncer wrappedConfig newState

                checks =
                    case state of
                        Settled ->
                            -- If we are moving from settled to unsettled, then all three
                            -- checks are relevant. In theory, we might be able to deduce
                            -- that some are not necessary, but it's easier to start this
                            -- way and optimize later. We do at least check that they
                            -- are unique, since we hardly need to set two timeouts for
                            -- the same value.
                            [ config.emitWhenUnsettled
                            , config.emitWhileUnsettled
                            , config.settleWhenQuietFor
                            ]
                                |> List.filterMap identity
                                |> List.Extra.unique

                        Unsettled _ ->
                            -- If we were already unsettled, the only fresh check that
                            -- could now be needed is settleWhenQuietFor
                            List.filterMap identity [ config.settleWhenQuietFor ]

                ( checkNow, checkLater ) =
                    -- We do see whether an immediate check is called for. If so, we
                    -- do it recursively, since we don't want to create a race with
                    -- some further input. Practically, this optimizes the case where
                    -- `emitWhenUnsettled` is `Just 0`, which would be a common case.
                    checks
                        |> List.partition (\interval -> interval <= 0)
                        |> Tuple.mapFirst (not << List.isEmpty)

                ( checkedDebouncer, checkedIntervals, emit ) =
                    if checkNow then
                        update (Check time) newDebouncer

                    else
                        ( newDebouncer, [], Nothing )
            in
            ( checkedDebouncer
            , checkedIntervals ++ checkLater
            , emit
            )

        ManualCancel ->
            -- This is actually equivalent to `cancel`, but it's here as a convenience
            -- in case people want a msg-oriented way to cancel.
            ( cancel debouncer
            , []
            , Nothing
            )

        ManualSettle ->
            -- We settle now, even though we wouldn't normally.
            --
            -- We end up in the same state as `ManualCancel`, except that we also
            -- emit anything we've accumulated.
            let
                emit =
                    case state of
                        Settled ->
                            Nothing

                        Unsettled unsettled ->
                            unsettled.output
            in
            ( cancel debouncer
            , []
            , emit
            )

        ManualEmitAt time ->
            -- We've been told to emit at this time, even if we wouldn't normally.
            -- So, this doesn't affect whether we're settled or not.
            case state of
                Settled ->
                    -- If we're settled, then there isn't anything to do ... we
                    -- have nothing to emit, and no state change is required.
                    ( debouncer, [], Nothing )

                Unsettled unsettled ->
                    case unsettled.output of
                        Just _ ->
                            let
                                newState =
                                    -- Since we're emitting, we record that fact. Whether
                                    -- we become settled depends on inputs, not outputs,
                                    -- so that can't change here.
                                    Unsettled
                                        { unsettled
                                            | lastEmittedAt = Just time
                                            , output = Nothing
                                        }

                                intervals =
                                    -- Since we're emitting, we should schedule a check
                                    -- if config.emitWhileUnsettled is set.
                                    case config.emitWhenUnsettled of
                                        Just emit ->
                                            [ emit ]

                                        Nothing ->
                                            []
                            in
                            ( Debouncer wrappedConfig newState
                            , intervals
                            , unsettled.output
                            )

                        Nothing ->
                            -- If we have nothing to emit, then our state stays
                            -- the same.  That is, we only update our state if
                            -- we have something to emit.
                            ( debouncer, [], Nothing )

        Check time ->
            -- We have arrived at a moment which, in the past, we thought would
            -- need checking to see whether to emit some output. So, take a
            -- look.
            --
            -- Note that we don't infer anything from the fact that we thought,
            -- in the past, that we should check now. Whatever we do now is
            -- purely a function of our current state and the current time.
            -- This simplifies the code a great deal, since we just have to
            -- keep our current state sensible ...  we don't have to worry
            -- about information embedded in upcoming checks.
            --
            -- To put it another way, something may have happened between
            -- scheduling this check and arriving at this check which means
            -- that we don't actually want to emit something now. In some
            -- cases, it would be possible to know that in advance ... that is,
            -- in some cases, it might be a nice optimization to be able to
            -- "cancel" a scheduled check. However, the Elm API for
            -- `Process.sleep` doesn't currently allow access to the Javascript
            -- `cancelTimeout`, so you'd need a native code version of it to do
            -- that.
            --
            -- In theory, we could keep a `Dict` of upcoming checks as part of
            -- our state, and plan to short-circuit a check when we know, in
            -- advance, it won't be needed any longer. Also, we could consult
            -- that `Dict` when scheduling checks, since it might be possible
            -- to know that some checks don't need to be scheduled at all,
            -- given what is already scheduled. (Checks scheduled for the same
            -- time would be low-hanging fruit).
            case state of
                Settled ->
                    -- If we're settled now, then clearly we have nothing to
                    -- emit, and nothing to do.
                    ( debouncer, [], Nothing )

                Unsettled unsettled ->
                    let
                        -- Should we emit because of emitWhenUnsettled?
                        becauseEmitWhenUnsettled =
                            case config.emitWhenUnsettled of
                                Just interval ->
                                    case unsettled.lastEmittedAt of
                                        Just _ ->
                                            -- If we've emitted since becomming unsettled, then
                                            -- clearly we don't need to emit for this reason
                                            False

                                        Nothing ->
                                            -- If not, see if enough time has passed since we
                                            -- became unsettled.
                                            unsettled.unsettledAt + interval <= time

                                Nothing ->
                                    -- We're not configured to emit for this reason
                                    False

                        -- Should we emit because of emitWhileUnsettled?
                        becauseEmitWhileUnsettled =
                            case config.emitWhileUnsettled of
                                Just interval ->
                                    case unsettled.lastEmittedAt of
                                        Just lastEmittedAt ->
                                            -- Check if enough time has passed since we last
                                            -- emitted.
                                            lastEmittedAt + interval <= time

                                        Nothing ->
                                            -- We haven't emitted since becoming unsettled,
                                            -- so check whether enough time has passed since
                                            -- becoming unsettled.
                                            unsettled.unsettledAt + interval <= time

                                Nothing ->
                                    -- We're not configured to emit for this reason
                                    False

                        shouldSettle =
                            config.settleWhenQuietFor
                                |> Maybe.map (\interval -> unsettled.lastInputProvidedAt + interval <= time)
                                |> Maybe.withDefault False

                        shouldEmit =
                            -- Now, if our output is Nothing, we won't emit, no matter what.
                            -- This could happen even if `shouldSettle` is true, because we
                            -- may have received no new input since emitting while unsettled.
                            (unsettled.output /= Nothing)
                                && (shouldSettle || becauseEmitWhenUnsettled || becauseEmitWhileUnsettled)

                        emit =
                            if shouldEmit then
                                unsettled.output

                            else
                                Nothing

                        newState =
                            if shouldSettle then
                                -- Nice and simple ... if we're settling, we can throw away our
                                -- state.
                                --
                                -- There may still be future "checks" scheduled that it would
                                -- actually be nice to cancel, I suppose. But they can't do any
                                -- harm, since they will evaluate what to do based on state at
                                -- that time. To put it another way, there can be no harm in a
                                -- "spurious" check ... it will just conclude that nothing need
                                -- be done. But it would be nice to optimize away previously-scheduled
                                -- checks that are now known to be spurious.
                                Settled

                            else if shouldEmit then
                                -- We're actually emitting something, so reset our `output`
                                -- to `Nothing`, and record the time.
                                Unsettled
                                    { unsettled
                                        | lastEmittedAt = Just time
                                        , output = Nothing
                                    }

                            else
                                -- If we're neither settling nor emitting, then nothing has changed
                                state

                        intervals =
                            if shouldEmit && not shouldSettle then
                                -- If we're emitting but not settling, then check emitWhileUnsettled
                                -- to see whether we need to schedule a check
                                case config.emitWhileUnsettled of
                                    Just interval ->
                                        [ interval ]

                                    Nothing ->
                                        []

                            else
                                []
                    in
                    ( Debouncer wrappedConfig newState
                    , intervals
                    , emit
                    )
