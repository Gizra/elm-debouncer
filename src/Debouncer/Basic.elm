module Debouncer.Basic
    exposing
        ( Debouncer
        , Config
        , Msg
        , update
        , provideInput
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

    type alias Model =
        { quietForOneSecond : Debouncer Msg Msg
        , messages : List String
        }

    quietForOneSecondConfig : Debouncer.Config Msg Msg
    quietForOneSecondConfig =
        { emitWhenUnsettled = Nothing
        , emitWhileUnsettled = Nothing
        , settleWhenQuietFor = 1 * Time.second
        , accumulator = \input accum -> Just input
        }

    init : ( Model, Cmd Msg )
    init =
        ( { quietForOneSecond = Debouncer.init quietForOneSecondConfig
          , messages = []
          }
        , Cmd.none
        )

    type Msg
        = MsgQuietForOneSecond (Debouncer.Msg Msg)
        | DoSomething

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
                            update emitted updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, mappedCmd ])

                        Nothing ->
                            ( updatedModel, mappedCmd )

            DoSomething ->
                ( { model | messages = model.messages ++ [ "I did something" ] }
                , Cmd.none
                )

    view : Model -> Html Msg
    view model =
        div [ style [ ( "margin", "1em" ) ] ]
            [ button
                [ DoSomething
                    |> provideInput
                    |> MsgQuietForOneSecond
                    |> onClick
                ]
                [ text "Click here repeatedly." ]
            , p [] [ text " I'll add a message below once you stop clicking for one second." ]
            , model.messages
                |> List.map (\message -> p [] [ text message ])
                |> div []
            ]

    main : Program Never Model Msg
    main =
        Html.program
            { init = init
            , view = view
            , update = update
            , subscriptions = always Sub.none
            }

@docs Debouncer, Config, init

@docs Msg, provideInput, update

@docs cancel

-}

import Debouncer.Internal
import List.Extra
import Process
import Time exposing (Time, second)
import Task
import Maybe
import Tuple


{-| An opaque type which holds the state of a debouncer. You will need to
integrate this into your `Model` type to keep the state.

The `i` type parameter represents the type of the inputs that you will provide.
One common case will be that your inputs will be your `Msg` type -- that is,
you'll be providing messages to "smooth out" over time. However, you're not
limited to that -- you can provide any kind of input you like.

The `o` type parameter represents the type of the things which will be emitted
by the debouncer. In many cases, this will be the same thing as your input type
-- for instance, you might provide your `Msg` type and get your `Msg` type
back. However, you're not limited to that -- you can get a different type back
if you like.

-}
type alias Debouncer i o =
    Debouncer.Internal.Debouncer i o


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
    parameters. If `Nothing`, then we wn't emit immediately -- we'll wait
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


{-| Initialize a `Debouncer` using the supplied `Config`. You'll typically
need to do this in order to put the debouncer in your `Model`.
-}
init : Config i o -> Debouncer i o
init config =
    Debouncer.Internal.Debouncer (sanitizeConfig config) Debouncer.Internal.Settled


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
    | MsgInternal (Debouncer.Internal.Msg i)



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


{-| Cancel any input collected so far (and not yet emitted). This throws away
whatever input has been provided in the past, and forces the debouncer back to
a "settled" state without emitting anything further.
-}
cancel : Debouncer i o -> Debouncer i o
cancel (Debouncer.Internal.Debouncer config state) =
    Debouncer.Internal.Debouncer config Debouncer.Internal.Settled


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
    case msg of
        ProvideInput input ->
            -- Basically, this just gets the current time, and then hands the
            -- input and the time to our internal "update" function.
            ( debouncer
            , Task.perform (MsgInternal << Debouncer.Internal.InputProvidedAt input) Time.now
            , Nothing
            )

        MsgInternal subMsg ->
            -- The way the code is organized here is mostly for the purpose of
            -- facilitating testing. So, the "internal" update function does
            -- not return actual commands. Instead, it returns a list of
            -- intervals at which we should schedule checks. That way, we can
            -- test its behaviour. Then, it's our job here to turn that into
            -- actual commands.
            let
                ( updatedDebouncer, intervals, output ) =
                    Debouncer.Internal.update subMsg debouncer

                cmds =
                    intervals
                        |> List.map
                            (\interval ->
                                Process.sleep interval
                                    |> Task.andThen (always Time.now)
                                    |> Task.perform (MsgInternal << Debouncer.Internal.Check)
                            )
                        |> Cmd.batch
            in
                ( updatedDebouncer
                , cmds
                , output
                )
