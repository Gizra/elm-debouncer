module Debouncer
    exposing
        ( Debouncer
        , Config
        , Msg
        , update
        , provideInput
        , checkNow
        , cancel
        , init
        )

{-| See the README for discussion. This is a complete low-level
work-in-progress. Next step is to create some higher-level
conveniences on top of this.

@docs Debouncer, Config, init

@docs Msg, provideInput, update

@docs cancel, checkNow
-}

import List.Extra
import Process
import Time exposing (Time, second)
import Task as Task18
import Maybe as Maybe18
import Tuple as Tuple18


{-| A type which holds the state of a debouncer.
-}
type Debouncer i o
    = Debouncer (Config i o) (State o)


{-| Configuration for a debouncer.

- `emitWhenUnsettled`

  When becoming unsettled, should we emit at a different interval the first
  time? For instance, you could provide `Just 0` in order to emit the first
  input immediately, and then start accumulating using the other parameters.

- `emitWhileUnsettled`

  While we're accumulating inputs, how often should we emit them? If
  `Nothing`, we wait until we become settled before we emit anything. If
  `Just time`, then emit every specified interval while we're collecting
  inputs, even if we haven't settled yet.

- `settleWhenQuietFor`

  How long should we wait without inputs before considering the debouncer to
  be "settled"?

- `accumulator`

  When given some input, how should we aggregate it with other input we
  haven't emitted yet?
-}
type alias Config i o =
    { emitWhenUnsettled : Maybe Time
    , emitWhileUnsettled : Maybe Time
    , settleWhenQuietFor : Time
    , accumulator : i -> Maybe o -> Maybe o
    }


type State o
    = Settled
    | Unsettled
        -- `output` needs to be a `Maybe` because we may have emitted while
        -- unsettled, due to `emitWhileUnsettled`. And, we could arrive at
        -- `settleWhenQuietFor` without any further input, so we don't
        -- necessarily emit anything when we become settled.
        { unsettledAt : Time
        , lastInputProvidedAt : Time
        , lastEmittedAt : Maybe Time
        , output : Maybe o
        }


{-| Initialize a debouncer using the supplied Config.
-}
init : Config i o -> Debouncer i o
init config =
    Debouncer (sanitizeConfig config) Settled


{-| Sanitize the config to simplify some of the logic.
-}
sanitizeConfig : Config i o -> Config i o
sanitizeConfig config =
    { emitWhenUnsettled = nothingIfNegative config.emitWhenUnsettled
    , emitWhileUnsettled = nothingIfNegative config.emitWhileUnsettled
    , settleWhenQuietFor = zeroIfNegative config.settleWhenQuietFor
    , accumulator = config.accumulator
    }


nothingIfNegative : Maybe number -> Maybe number
nothingIfNegative =
    Maybe18.andThen
        (\num ->
            if num < 0 then
                Nothing
            else
                Just num
        )


zeroIfNegative : number -> number
zeroIfNegative =
    max 0


{-| Messages which the debouncer can handle.
-}
type Msg i
    = ProvideInput i
    | InputProvidedAt i Time
    | CheckNow
    | Check Time



-- You could imagine some messages for changing the configuration. They would
-- need to be messages, rather than just modifying the debouncer directly,
-- since changes to the configuration would potentially require checks to be
-- scheduled at new times.


{-| Construct a message that provides input to a debouncer. The input type is
parameterized, so it can be anything you like. Most often, you'll want to
provide your message type, or possibly a `Task`.
-}
provideInput : i -> Msg i
provideInput =
    ProvideInput


{-| Check whether the debouncer should emit something now.

Normally, you don't need to send this message ... the debouncer will schedule
checks appropriately. However, it could come in handy for testing.
-}
checkNow : Msg i
checkNow =
    CheckNow


{-| Cancel any accumulated input. Throws away whatever input has been provided
in the past and not emitted yet.
-}
cancel : Debouncer i o -> Debouncer i o
cancel (Debouncer config state) =
    Debouncer config Settled


{-| Handles a message for the debouncer.

The extra return parameter is the emitted output if this update results in
emitting something. So, you should do whatever it is you want to do with the
output if you get it. For instance, if messages, you should feed them into your
`update` function.
-}
update : Msg i -> Debouncer i o -> ( Debouncer i o, Cmd (Msg i), Maybe o )
update msg debouncer =
    case ( msg, debouncer ) of
        ( ProvideInput input, _ ) ->
            ( debouncer
            , Task18.perform (InputProvidedAt input) Time.now
            , Nothing
            )

        ( InputProvidedAt input time, Debouncer config state ) ->
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
                    Debouncer config newState

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
                        |> Tuple18.mapFirst (not << List.isEmpty)

                ( checkedDebouncer, checkedCmd, emit ) =
                    if checkNow then
                        update (Check time) newDebouncer
                    else
                        ( newDebouncer, Cmd.none, Nothing )

                cmd =
                    checkLater
                        |> List.map
                            (\interval ->
                                Process.sleep interval
                                    |> Task18.andThen (always Time.now)
                                    |> Task18.perform Check
                            )
                        |> (::) checkedCmd
                        |> Cmd.batch
            in
                ( checkedDebouncer
                , cmd
                , emit
                )

        ( CheckNow, _ ) ->
            ( debouncer
            , Task18.perform Check Time.now
            , Nothing
            )

        ( Check time, Debouncer config state ) ->
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
                    ( debouncer, Cmd.none, Nothing )

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

                        cmd =
                            if shouldEmit && not shouldSettle then
                                -- If we're emitting but not settling, then check emitWhileUnsettled
                                -- to see whether we need to schedule a check
                                case config.emitWhileUnsettled of
                                    Just emitWhileUnsettled ->
                                        Process.sleep emitWhileUnsettled
                                            |> Task18.andThen (always Time.now)
                                            |> Task18.perform Check

                                    Nothing ->
                                        Cmd.none
                            else
                                Cmd.none
                    in
                        ( Debouncer config newState
                        , cmd
                        , emit
                        )
