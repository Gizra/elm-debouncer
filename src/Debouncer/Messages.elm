module Debouncer.Messages
    exposing
        ( accumulateWith
        , cancel
        , config
        , Debouncer
        , DebouncerConfig
        , emitWhenUnsettled
        , emitWhileUnsettled
        , firstInput
        , lastInput
        , Msg
        , provideInput
        , settleWhenQuietFor
        , toDebouncer
        , update
        , UpdateConfig
        )

{-| Ths module allows you to "smooth out" messages over time, so that they
don't get applied immediately -- instead, they are applied at a future moment.
Depending on the configuration you provide, you can use this to implemented
debouncing, throttling, or other ways of managing messages.

This module is simpler than `Debouncer.Basic`, because it is specialized for
the common case where the "input" is your `Msg` type, and what you ultimately
want do do is have the messages applied. You can use `Debouncer.Basic` instead
if you want to do something more complex.

A quick note on terminology: the debouncer is said to be "unsettled" while it
is collecting inputs and considering when to emit output. It becomes "settled"
again once a specified period has passed without any inputs (see
`settleWhenQuietFor`).

To use this module, you will need to integrate it into your `Model` and `Msg`
type, and handle it in your `update` function. Here's one example -- it's the
same example as given in `Debouncer.Basic`, but you can see that it's simpler
here.

    import Debouncer.Messages as Debouncer exposing (Debouncer, settleWhenQuietFor, provideInput, toDebouncer)
    import Html exposing (..)
    import Html.Attributes exposing (..)
    import Html.Events exposing (..)
    import Time exposing (Time)

    type alias Model =
        { quietForOneSecond : Debouncer Msg
        , messages : List String
        }

    init : ( Model, Cmd Msg )
    init =
        ( { quietForOneSecond =
                Debouncer.config
                    |> settleWhenQuietFor (1 * Time.second)
                    |> toDebouncer
          , messages = []
          }
        , Cmd.none
        )

    type Msg
        = MsgQuietForOneSecond (Debouncer.Msg Msg)
        | DoSomething

    updateDebouncer : Debouncer.UpdateConfig Msg Model
    updateDebouncer =
        { mapMsg = MsgQuietForOneSecond
        , getDebouncer = .quietForOneSecond
        , setDebouncer = \debouncer model -> { model | quietForOneSecond = debouncer }
        }

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            MsgQuietForOneSecond subMsg ->
                Debouncer.update update updateDebouncer subMsg model

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


## Creating a debouncer

@docs Debouncer, DebouncerConfig, toDebouncer


## Creating a configuration

@docs config
@docs settleWhenQuietFor, emitWhenUnsettled, emitWhileUnsettled
@docs accumulateWith, lastInput, firstInput

@docs Debouncer, Config, toDebouncer


## Running a debouncer

@docs Msg, provideInput, cancel, UpdateConfig, update

-}

import Debouncer.Basic exposing (Accumulator)
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

To create a `Debouncer`, start with a `config`, modify the config as needed,
and then use `toDebouncer`. For instance:

    config
        |> settleWhenQuietFor (0.5 * Time.second)
        |> toDebouncer

-}
type alias Debouncer msg =
    Debouncer.Basic.Debouncer msg msg


{-| An opaque type representing the configuration needed for a debouncer.

To create a debouncer, start with `config`, modify it as needed, and then
use `toDebouncer`. For instance:

    config
        |> settleWhenQuietFor (2.0 * Time.second)
        |> emitWhileUnsettled (0.5 * Time.second)
        |> toDebouncer

-}
type alias DebouncerConfig msg =
    Debouncer.Basic.Config msg msg


{-| A starting point for configuring a debouncer. By default, it:

  - settles when quiet for 1 second
  - emits the last input when it becomes settled
  - does not emit when it becomes unsettled
  - does not emit while unsettled

So, this amounts to "debouncing" by default (as opposed to "throttling").
To change any of those parameters, use the various functions that alter a
`Config`.

-}
config : DebouncerConfig msg
config =
    Debouncer.Basic.config


{-| What should the debouncer do with the first input when it becomes unsettled?

If `Nothing` (the default), the debouncer emits only on the "trailing edge."
That is, it won't do anything special with the first input -- it will just
keep it and do something with it later.

If `Just 0`, the debouncer will immediately emit the first input when becoming
unsettled. It will then remain unsettled, accumulating further input and
eventually emitting it.

If `Just interval`, the debouncer will use the provided interval to emit after
becoming unsettled. It will then remain unsettled, collecting further input and
eventually emitting it.

-}
emitWhenUnsettled : Maybe Time -> DebouncerConfig msg -> DebouncerConfig msg
emitWhenUnsettled =
    Debouncer.Basic.emitWhenUnsettled


{-| Should the debouncer emit while it is unsettled?

If `Nothing` (the default), the debouncer will wait until it becomes settled
again before emitting. This is what you might refer to as "debouncing".

If `Just interval`, the debouncer will emit at the provided interval while it
is unsettled. This is what you might refer to as "throttling".

-}
emitWhileUnsettled : Maybe Time -> DebouncerConfig msg -> DebouncerConfig msg
emitWhileUnsettled =
    Debouncer.Basic.emitWhileUnsettled


{-| How long should the debouncer wait without input before becoming "settled"
again?

If you are "debouncing" (i.e. `emitWhileUnsettled` is `Nothing`), then this is
the key parameter controlling when you will receive output -- you'll receive
the output after no inputs have been provided for the specified time.

If you are "throttling" (i.e. `emitWhileUnsettled is not`Nothing`), then this
parameter won't make much difference, unless you are also specifying
emitWhenUnsettled` in order to do something with the initial input.

-}
settleWhenQuietFor : Time -> DebouncerConfig msg -> DebouncerConfig msg
settleWhenQuietFor =
    Debouncer.Basic.settleWhenQuietFor


{-| How should the debouncer combine new input with the output collected
so far?

You can use one of the pre-built accumulators:

    - `lastInput` (the default)
    - `firstInput`

Or, if you need to do something more complex, you can provide your own function
of the form

    msg -> Maybe msg -> Maybe msg

-}
accumulateWith : Accumulator msg msg -> DebouncerConfig msg -> DebouncerConfig msg
accumulateWith =
    Debouncer.Basic.accumulateWith


{-| An accumulator which just keeps the last provided input. This is
probably the one you'll want most often (and is the default), but you do have
other choices.
-}
lastInput : Accumulator msg msg
lastInput =
    Debouncer.Basic.lastInput


{-| An accumulator which just keeps the first provided input. Thus, the
remaining inputs are only used for timing purposes -- their values aren't
actually remembered.
-}
firstInput : Accumulator msg msg
firstInput =
    Debouncer.Basic.firstInput


{-| Initialize a `Debouncer` using the supplied `Config`.

For now, a debouncer's configuration cannot be changed once the debouncer is
created. (This is a feature that could be provided in future, if desirable).

-}
toDebouncer : DebouncerConfig msg -> Debouncer msg
toDebouncer =
    Debouncer.Basic.toDebouncer


{-| Messages which the debouncer handles.

You will need to integrate this into your own `Msg` type, and then handle it
in your `update` function (see code example above).

The type parameter represents the type of your own `Msg` type. Thus, it should
match the `msg` in `Debouncer msg`.

The only message you will typically need to send to the debouncer explicitly
is the message that provides input. You can construct such a message with the
`provideInput` function. Other messages are used internally by the debouncer.

-}
type alias Msg msg =
    Debouncer.Basic.Msg msg


{-| Construct a message that provides input to a debouncer.

The type parameter represents your own `Msg` type.

-}
provideInput : msg -> Msg msg
provideInput =
    Debouncer.Basic.provideInput


{-| Cancel any input collected so far (and not yet emitted). This throws away
whatever input has been provided in the past, and forces the debouncer back to
a "settled" state (without emitting anything).
-}
cancel : Debouncer msg -> Debouncer msg
cancel =
    Debouncer.Basic.cancel


{-| Configuration that simplifies how your `update` function calls our `update`
function.

This module handles the case where the input to the debouncer is your `Msg`
type, and what you want to happen when output is emitted is that the `Msg` gets
applied. So, the debouncer is basically delaying some messages, according to
your configuration.

Our `update` function is simplified (compared to `Debouncer.Basic.update`), so
that you can call it (from your update function) like this:

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            MsgQuietForOneSecond subMsg ->
                Debouncer.Messages.update update updateDebouncer subMsg model

This way, you don't have to "massage" what our `update` function returns. You
provide the debouncer and your model, and we return the standard tuple that
your update function also needs to return.

To make this work, we need some configuration:

    - `mapMsg` is the tag, in your `Msg` type, that wraps our `Msg` type
    - `getDebouncer` is a function we can use to get the debouncer from your `model` type
    - `setDebouncer` is a function we can use to update your model with an updated debouncer

In practice, it is pretty straight-forward to specify these things -- see the
`updateDebouncer` example in the module docs for one example:

    updateDebouncer : Debouncer.UpdateConfig Msg Model
    updateDebouncer =
        { mapMsg = MsgQuietForOneSecond
        , getDebouncer = .quietForOneSecond
        , setDebouncer = \debouncer model -> { model | quietForOneSecond = debouncer }
        }

-}
type alias UpdateConfig msg model =
    { mapMsg : Msg msg -> msg
    , getDebouncer : model -> Debouncer msg
    , setDebouncer : Debouncer msg -> model -> model
    }


{-| Handle a message for the debouncer.

You will need to integrate this into your `update` function, so that the debouncer
can act on its messages. (There is a code example at the top of the module docs).

Note that we actually return your own `(model, Cmd msg)` type. This simplifies
your `update` function, since you don't need to massage what we return in order
to make things work -- we return exactly what your `update` function will need
to return.

To make that work, you need to provide a couple of additional parameters.

    - The first parameter is essentially your own `update` function. If your
      `update` function takes additional parameters, you will need to partially
      apply them. This allows us to immediately execute an emitted message at
      the appropriate time, without you needing to handle it specially.

    - The second parameter allows us to update your model with the new debouncer
      state, without you needing to handle it specially.

-}
update : (msg -> model -> ( model, Cmd msg )) -> UpdateConfig msg model -> Msg msg -> model -> ( model, Cmd msg )
update parentUpdate config msg model =
    let
        ( updatedDebouncer, cmd, output ) =
            Debouncer.Basic.update msg (config.getDebouncer model)

        mappedCmd =
            Cmd.map config.mapMsg cmd

        newModel =
            config.setDebouncer updatedDebouncer model
    in
        output
            |> Maybe.map
                (\emittedMsg ->
                    parentUpdate emittedMsg newModel
                        |> Tuple.mapSecond (\recursiveCmd -> Cmd.batch [ mappedCmd, recursiveCmd ])
                )
            |> Maybe.withDefault
                ( newModel, mappedCmd )
