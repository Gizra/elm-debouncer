module Debouncer.LowLevel
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

{-| Ths module allows you to managing inputs that occur over time, so that they
get handed back to you, at future moments, grouped in a way that you have
configured. Depending on the configuration you provide, you can use this to
implemented debouncing, throttling, or other ways of managing inputs that occur
over time.

This is a "low-level" implementation, in the sense that it is the most general
implementation. The other modules provide simpler, pre-configured interfaces
for particular debouncing strategies. So, you can use the other modules if they
suit you, leaving this module for situations that aren't handled by the others.

To use this module, you will need to integrate it into your `Model` and `Msg`
type, and handle it in your `update` function. Here's one example, where the
"input" you're providing is your own `Msg` type, and the output is also your
`Msg` type. (To avoid some of the verbosity below, you can use one of the
more specialized modules, assuming the specialization suits you).

    import Debouncer.LowLevel exposing (Debouncer) as Debouncer

    type alias Model =
        { quietForOneSecond : Debouncer Msg Msg
        , ...
        }

    -- A configuration that emits the last message provided, once no message
    -- has been provided for 1 second.
    quietForOneSecondConfig : Debouncer.Config Msg Msg
    quietForOneSecondConfig =
        { emitWhenUnsettled = Nothing
        , emitWhileUnsettled = Nothing
        , settleWhenQuietFor = 1 * Time.second
        , accumulator = \input accum -> Just input
        }

    initialModel : Model
    initialModel =
        { quietForOneSecond : Debouncer.init quietForOneSecondConfig
        , ...
        }

    type Msg
        = MsgQuietForOneSecond (Debouncer.Msg Msg)
        | DoSomething
        | ...

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            MsgQuietForOneSecond subMsg ->
                let
                    ( subModel, subCmd, emittedMsg ) =
                        Debouncer.update subMsg model.quietForOneSecond

                    mappedCmd =
                        Cmd.map MsgQuietForOneSecond subCmd

                    updatedModel =
                        { model | quietForOneSecond = subModel }
                in
                    case emittedMsg of
                        Just emitted ->
                            -- We got a `Msg` back to execute, so go ahead and
                            -- do that recursively.
                            update emitted updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, mappedCmd ])

                        Nothing ->
                            ( updatedMdel, mappedCmd)

            DoSomething ->
                ...

            ... ->

Then, to provide inputs, you can use `provideInput` something like this:

    div
        [ onClick <| MsgQuietForOneSecond (provideInput DoSomething) ]
        [ text "Do something after you stop clicking for 1 second." ]

And the rest is magic!

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


{-| An opaque type which holds the state of a debouncer. You will need to
integrate this into your `Model` type to keep the state.

The `i` type parameter represents the type of the inputs that you will provide.
One common case will be that your inputs will be your `Msg` type -- that is,
you'll be providing messages to "smooth out" over time.  However, you're not
limited to that -- you can provide any kind of input you like.

The `o` type parameter represents the type of the things which will be emitted
by the debouncer. In many cases, this will be the same thing as your input type
-- for instance, you might provide your `Msg` type and get your `Msg` type
back. However, you're not limited to that -- you can get a different type back
if you like.
-}
type Debouncer i o
    = Debouncer (Config i o) (State o)


{-| The configuration needed for a debouncer.

A note on terminology: the debouncer is "unsettled" while it is collecting
inputs and considering when to emit them. It becomes "settled" again once
a specified period has passed without any inputs.

- `settleWhenQuietFor`

  How long should we wait without inputs before considering the debouncer to
  be "settled" again?

- `emitWhileUnsettled`

  While we're collecting inputs, how often should we emit something? If
  `Nothing`, we wait until we become settled before we emit anything. If
  `Just time`, then emit every specified interval while we're collecting
  inputs, even if we haven't settled yet.

- `emitWhenUnsettled`

  When becoming unsettled, should we emit at a different interval the first
  time? For instance, you could provide `Just 0` in order to emit the first
  input immediately, and then start collecting inputs using the other
  parameters.  If `Nothing`, then we wn't emit immediately -- we'll wait
  until the other parameters tell us to emit something.

- `accumulator`

  When given some input, how should we aggregate it with other input we
  have collected so far? One simple strategy is just to throw away the
  previous inputs and remember the latest one:

    accumulator = \input acc -> Just input

  But other strategies would be possible ... for instance, you could just
  remember the first input and use later inputs just for timing, not their
  values:

    accumulator = \input acc -> Just (Maybe.withDefault input acc)

  Or, perhaps your output type is actually a list of your input types, so
  you can collect all the inputs.

    accumulator = \input acc -> input :: Maybe.withDefault [] acc

  In any event, it's up to you to decide what your output type is, and how
  to aggregate the input types into the output type.
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


{-| Initialize a `Debouncer` using the supplied `Config`. You'll typically
need to do this in order to put the debouncer in your `Model`.
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

You will need to integrate this into your own `Msg` type, and then handle it
in your `update` method (see code example above).

The type parameter represents the type of the input to be provided to
the debouncer. Thus, it should match the `i` in `Debouncer i o`.

The only message you will typically need to send to the debouncer explicitly
is the message that provides input. You can construct such a message with the
`provideInput` function. Other messages are used internally by the debouncer.
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


{-| Construct a message that provides input to a debouncer.

The type parameter represents the type of the input to be provided. One typical
example would be your own `Msg` type, but it can be anything you like. It would
need to match the `i` in your `Debouncer i o` type.
-}
provideInput : i -> Msg i
provideInput =
    ProvideInput


{-| Constructs a message which will check whether the debouncer should emit
something now.

Normally, you don't need to send this message ... the debouncer will schedule
checks appropriately. However, it could come in handy for testing.
-}
checkNow : Msg i
checkNow =
    CheckNow


{-| Cancel any input collected so far (and not yet emitted). This throws away
whatever input has been provided in the past, and forces the debouncer back to
a "settled" state without emitting anything further.
-}
cancel : Debouncer i o -> Debouncer i o
cancel (Debouncer config state) =
    Debouncer config Settled


{-| Handle a message for the debouncer.

You will need to integrate this into your `update` function, so that the debouncer
can act on its messages. (There is a code example at the top of the module docs).

If we have arrived at a time for output to be emitted, then the extra return
parameter will be the output. So, you should do whatever it is you want to do
with the output, if it is provided. For instance, if the output is your `Msg`
type, you should recursively feed it into your `update` function. (Again, see
the example above for that case). But that is just one example -- the output
type can be whatever you wish, and you can decide what to do with it.
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
