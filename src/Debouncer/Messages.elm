module Debouncer.Messages
    exposing
        ( Debouncer
        , Config
        , Msg
        , update
        , provideInput
        , cancel
        , init
        )

{-| Ths module allows you to "smooth out" messages over time, so that they
don't get applied immediately -- instead, they are applied at a future moment.
Depending on the configuration you provide, you can use this to implemented
debouncing, throttling, or other ways of managing messages.

This module is simpler than `Debouncer.Basic`, because it is specialized for
the common case where the "input" is your `Msg` type, and what you ultimately
want do do is have the messages applied. You can use `Debouncer.Basic` instead
if you want to do something more complex.

To use this module, you will need to integrate it into your `Model` and `Msg`
type, and handle it in your `update` function. Here's one example -- it's the
same example as given in `Debouncer.Basic`, but you can see that it's simpler
here.

    type alias Model =
        { quietForOneSecond : Debouncer Msg
        , messages : List String
        }

    quietForOneSecondConfig : Debouncer.Config Msg
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
                    ( subModel, cmd, emittedMsg ) =
                        Debouncer.update MsgQuietForOneSecond subMsg model.quietForOneSecond

                    updatedModel =
                        { model | quietForOneSecond = subModel }
                in
                    case emittedMsg of
                        Nothing ->
                            ( updatedModel, cmd )

                        Just emitted ->
                            update emitted updatedModel
                                |> Tuple.mapSecond (\cmd2 -> Cmd.batch [ cmd, cmd2 ])

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

The type parameter represents your `Msg` type, which will be used both
for the inputs and the outputs. If that doesn't suit you, `Debouncer.Basic`
allows you to use any input or output type you wish.

-}
type alias Debouncer msg =
    Debouncer.Internal.Debouncer msg msg


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
type alias Config msg =
    { emitWhenUnsettled : Maybe Time
    , emitWhileUnsettled : Maybe Time
    , settleWhenQuietFor : Time
    , accumulator : msg -> Maybe msg -> Maybe msg
    }


{-| Initialize a `Debouncer` using the supplied `Config`. You'll typically
need to do this in order to put the debouncer in your `Model`.
-}
init : Config msg -> Debouncer msg
init config =
    Debouncer.Internal.init config


{-| Messages which the debouncer can handle.

You will need to integrate this into your own `Msg` type, and then handle it
in your `update` method (see code example above).

The type parameter represents your own `Msg` type.

The only message you will typically need to send to the debouncer explicitly
is the message that provides input. You can construct such a message with the
`provideInput` function. Other messages are used internally by the debouncer.

-}
type Msg msg
    = ProvideInput msg
    | MsgInternal (Debouncer.Internal.Msg msg)



-- You could imagine some messages for changing the configuration. They would
-- need to be messages, rather than just modifying the debouncer directly,
-- since changes to the configuration would potentially require checks to be
-- scheduled at new times.


{-| Construct a message that provides input to a debouncer.

The type parameter represents your own `Msg` type.

-}
provideInput : msg -> Msg msg
provideInput =
    ProvideInput


{-| Cancel any input collected so far (and not yet emitted). This throws away
whatever messages been provided in the past, and forces the debouncer back to
a "settled" state without emitting anything further.
-}
cancel : Debouncer msg -> Debouncer msg
cancel =
    Debouncer.Internal.cancel


{-| Handle a message for the debouncer.

You will need to integrate this into your `update` function, so that the debouncer
can act on its messages. (There is a code example at the top of the module docs).

The first parameter is the "tag" that maps the debouncer's internal messages back
into your own message type.

-}
update : (Msg msg -> msg) -> Msg msg -> Debouncer msg -> ( Debouncer msg, Cmd msg, Maybe msg )
update mapper msg debouncer =
    case msg of
        ProvideInput input ->
            -- Basically, this just gets the current time, and then hands the
            -- input and the time to our internal "update" function.
            ( debouncer
            , Task.perform (mapper << MsgInternal << Debouncer.Internal.InputProvidedAt input) Time.now
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
                                    |> Task.perform (mapper << MsgInternal << Debouncer.Internal.Check)
                            )
                        |> Cmd.batch
            in
                ( updatedDebouncer
                , cmds
                , output
                )
