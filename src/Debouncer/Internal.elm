module Debouncer.Internal exposing (..)

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
import Time exposing (Time)


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


type alias Accumulator i o =
    i -> Maybe o -> Maybe o


lastInput : Accumulator i i
lastInput i o =
    Just i


firstInput : Accumulator i i
firstInput i =
    Just << Maybe.withDefault i


{-| Note that the output list will have the most recent input first.
-}
allInputs : Accumulator i (List i)
allInputs i o =
    Just (i :: Maybe.withDefault [] o)


addInputs : Accumulator number number
addInputs i o =
    Just (i + Maybe.withDefault 0 o)


appendInputToOutput : Accumulator appendable appendable
appendInputToOutput i o =
    Just <|
        Maybe.withDefault i <|
            Maybe.map (\acc -> acc ++ i) o


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


type alias ConfigRecord i o =
    { emitWhenUnsettled : Maybe Time
    , emitWhileUnsettled : Maybe Time
    , settleWhenQuietFor : Time
    , accumulator : Accumulator i o
    }


{-| This is the most basic constructor for a `Config`.

By default, it:

  - settles when quiet for 1 second
  - does not emit when unsettled
  - does not emit while unsettled
  - emits the last input

To change any of those parameters, use the various functions
that alter a `Config`.

So, by default, the output type is the same as the input type. However,
you can change that by using the `accumulateWith` function to provide
a different accumulator.

-}
config : Config i i
config =
    Config
        { emitWhenUnsettled = Nothing
        , emitWhileUnsettled = Nothing
        , settleWhenQuietFor = 1 * Time.second
        , accumulator = lastInput
        }


emitWhenUnsettled : Maybe Time -> Config i o -> Config i o
emitWhenUnsettled time (Config config) =
    Config
        { config | emitWhenUnsettled = time }


emitWhileUnsettled : Maybe Time -> Config i o -> Config i o
emitWhileUnsettled time (Config config) =
    Config
        { config | emitWhileUnsettled = time }


settleWhenQuietFor : Time -> Config i o -> Config i o
settleWhenQuietFor time (Config config) =
    Config
        { config | settleWhenQuietFor = time }


{-| Note that this changes the type of the `Config` to match the
type of the acummulator provided.
-}
accumulateWith : Accumulator i o -> Config a b -> Config i o
accumulateWith accumulator (Config config) =
    Config
        { config | accumulator = accumulator }


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
        , settleWhenQuietFor = zeroIfNegative config.settleWhenQuietFor
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


zeroIfNegative : number -> number
zeroIfNegative =
    max 0


type State o
    = Settled
    | Unsettled (UnsettledState o)


{-| `output` needs to be a `Maybe` because we may have emitted while
unsettled, due to `emitWhileUnsettled`. And, we could arrive at
`settleWhenQuietFor` without any further input, so we don't
necessarily emit anything when we become settled.
-}
type alias UnsettledState o =
    { unsettledAt : Time
    , lastInputProvidedAt : Time
    , lastEmittedAt : Maybe Time
    , output : Maybe o
    }


type Msg i
    = InputProvidedAt i Time
    | Check Time


{-| The second return parameter is not a `Cmd`, but instead a list of intervals
at which we ought to check whether to emit something. This assists with
testability (since we can test whether that list is correct). The caller is
responsible for actually turning that list into commands.
-}
update : Msg i -> Debouncer i o -> ( Debouncer i o, List Time, Maybe o )
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
                            , Just config.settleWhenQuietFor
                            ]
                                |> List.filterMap identity
                                |> List.Extra.unique

                        Unsettled _ ->
                            -- If we were already unsettled, the only fresh check that
                            -- could now be needed is settleWhenQuietFor
                            [ config.settleWhenQuietFor
                            ]

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
                                Just emitWhenUnsettled ->
                                    case unsettled.lastEmittedAt of
                                        Just _ ->
                                            -- If we've emitted since becomming unsettled, then
                                            -- clearly we don't need to emit for this reason
                                            False

                                        Nothing ->
                                            -- If not, see if enough time has passed since we
                                            -- became unsettled.
                                            unsettled.unsettledAt + emitWhenUnsettled <= time

                                Nothing ->
                                    -- We're not configured to emit for this reason
                                    False

                        -- Should we emit because of emitWhileUnsettled?
                        becauseEmitWhileUnsettled =
                            case config.emitWhileUnsettled of
                                Just emitWhileUnsettled ->
                                    case unsettled.lastEmittedAt of
                                        Just lastEmittedAt ->
                                            -- Check if enough time has passed since we last
                                            -- emitted.
                                            lastEmittedAt + emitWhileUnsettled <= time

                                        Nothing ->
                                            -- We haven't emitted since becoming unsettled,
                                            -- so check whether enough time has passed since
                                            -- becoming unsettled.
                                            unsettled.unsettledAt + emitWhileUnsettled <= time

                                Nothing ->
                                    -- We're not configured to emit for this reason
                                    False

                        shouldSettle =
                            unsettled.lastInputProvidedAt + config.settleWhenQuietFor <= time

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
                                    Just emitWhileUnsettled ->
                                        [ emitWhileUnsettled ]

                                    Nothing ->
                                        []
                            else
                                []
                    in
                        ( Debouncer wrappedConfig newState
                        , intervals
                        , emit
                        )
