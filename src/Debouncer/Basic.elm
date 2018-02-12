module Debouncer.Basic
    exposing
        ( Accumulator
        , Config
        , Debouncer
        , Msg
        , accumulateWith
        , addInputs
        , allInputs
        , appendInputToOutput
        , appendOutputToInput
        , cancel
        , cancelNow
        , debounce
        , emitFirstInput
        , emitNow
        , emitWhenUnsettled
        , emitWhileUnsettled
        , firstInput
        , lastInput
        , manual
        , provideInput
        , settleNow
        , settleWhenQuietFor
        , throttle
        , toDebouncer
        , update
        )

{-| Ths module allows you to managing inputs that occur over time, so that they
get handed back to you, at future moments, grouped in a way that you have
configured. Depending on the configuration you provide, you can use this to
implemented debouncing, throttling, or other ways of managing inputs that occur
over time.

This module provides the most general, comprehensive interface. You can choose:

  - the type of the inputs you provide
  - the type of the outputs the debouncer will emit
  - how the debouncer should accumulate inputs to form the output
  - the times at which outputs will be emitted
  - what you do with the outputs when you receive them

For a simpler module that is focused on debouncing or throttling a `Msg`
type, see the `Debouncer.Messages` module.

A quick note on terminology: the debouncer is said to be "unsettled" while it
is collecting inputs and considering when to emit output. It becomes "settled"
again once a specified period has passed without any inputs (see
`settleWhenQuietFor`).

To use this module, you will need to integrate it into your `Model` and `Msg`
type, and handle it in your `update` function. Here's one example, where the
"input" you're providing is your own `Msg` type, and the output is also your
`Msg` type. (To avoid some of the verbosity below, you can use the
`Debouncer.Messages` module).

    import Debouncer.Basic as Debouncer exposing (Debouncer, provideInput, settleWhenQuietFor, toDebouncer)
    import Html exposing (..)
    import Html.Attributes exposing (..)
    import Html.Events exposing (..)
    import Time exposing (Time)

    type alias Model =
        { quietForOneSecond : Debouncer Msg Msg
        , messages : List String
        }

    init : ( Model, Cmd Msg )
    init =
        ( { quietForOneSecond =
                Debouncer.debounce (1 * Time.second)
                    |> toDebouncer
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

But, remember that you get to choose the types of the inputs and outputs. So,
you can do something quite different from this example if you like.


## Creating a debouncer

@docs Debouncer, Config, toDebouncer


## Creating a configuration

@docs manual, debounce, throttle
@docs settleWhenQuietFor, emitWhenUnsettled, emitFirstInput, emitWhileUnsettled
@docs Accumulator, accumulateWith


## Pre-built accumulators

@docs lastInput, firstInput, allInputs, addInputs, appendInputToOutput, appendOutputToInput


## Running a debouncer

@docs Msg, provideInput, emitNow, settleNow, cancel, cancelNow, update

-}

import Debouncer.Internal
import List.Extra
import Maybe
import Process
import Task
import Time exposing (Time, second)
import Tuple


{-| An opaque type which holds the state of a debouncer. You will need to
integrate this into your `Model` type to keep the state.

The `i` type parameter represents the type of the inputs that you will provide.
One common case will be that your inputs will be your `Msg` type -- that is,
you'll be providing messages to "smooth out" over time. However, you're not
limited to that -- you can provide any kind of input you like.

The `o` type parameter represents the type of the output which will be emitted
by the debouncer. In many cases, this will be the same thing as your input type
-- for instance, you might provide your `Msg` type and get your `Msg` type
back. However, you're not limited to that -- you can get a different type back
if you like. (You just have to provide an `Accmulator` function to accumulate
your inputs into an output).

To create a `Debouncer`:

  - start with `manual`, `debounce` or `throttle`
  - modify the config as needed,
  - use `toDebouncer`

For instance:

    manual
        |> settleWhenQuietFor (Just (0.5 * Time.second))
        |> toDebouncer

... or, the equivalent:

    debounce (0.5 * Time.second)
        |> toDebouncer

-}
type alias Debouncer i o =
    Debouncer.Internal.Debouncer i o


{-| A function with the signature `i -> Maybe o -> Maybe o`.

This function will be used to accumulate each provided input with the output so
far. (If this is the first input since we've emitted something, the output so
far will be `Nothing`).

For some standard accumulators, see `lastInput`, `firstInput`, `allInputs`,
`addInputs`, `appendInputToOutput` and `appendOutputToInput`. But you're
welcome to write your own, if none of those suit you.

-}
type alias Accumulator i o =
    Debouncer.Internal.Accumulator i o


{-| An accumulator which just keeps the last provided input. This is
probably the one you'll want most often (and is the default), but you do have
other choices.
-}
lastInput : Accumulator i i
lastInput =
    Debouncer.Internal.lastInput


{-| An accumulator which just keeps the first provided input. Thus, the
remaining inputs are only used for timing purposes -- their values aren't
actually remembered.
-}
firstInput : Accumulator i i
firstInput =
    Debouncer.Internal.firstInput


{-| An accmulator which keeps all the inputs in a `List`. You can use this
if you want the whole list once the deouncer emits its output. Note that the
output list will have the most recent input first.
-}
allInputs : Accumulator i (List i)
allInputs =
    Debouncer.Internal.allInputs


{-| An accumulator which adds numeric inputs.
-}
addInputs : Accumulator number number
addInputs =
    Debouncer.Internal.addInputs


{-| An accmulator which appends the input to the output so far.
-}
appendInputToOutput : Accumulator appendable appendable
appendInputToOutput =
    Debouncer.Internal.appendInputToOutput


{-| An accumulator which appends the output to far to the input.
-}
appendOutputToInput : Accumulator appendable appendable
appendOutputToInput =
    Debouncer.Internal.appendOutputToInput


{-| An opaque type representing the configuration needed for a debouncer.

To create a debouncer:

  - start with `manual`, `debounce` or `throttle`
  - modify it as needed
  - `toDebouncer`.

For instance:

    manual
        |> settleWhenQuietFor (Just (2.0 * Time.second))
        |> emitWhileUnsettled (Just (0.5 * Time.second))
        |> toDebouncer

-}
type alias Config i o =
    Debouncer.Internal.Config i o


{-| A starting point for configuring a debouncer that only emits when you tell
it to, via `emitNow`.

By default, it:

  - never settles
  - does not emit when it becomes unsettled
  - does not emit while unsettled
  - accumulates only the last input

So, without more, you would need to tell this debouncer when to emit something
(via `emitNow`) -- it would never happen automatically.

To change any of those parameters, use the various functions that alter a
`Config` (i.e. `settleWhenQuietFor`, `emitWhenUnsettled`, `emitWhileUnsettled`).

By default, the output type is the same as the input type. However, you can
change that by using the `accumulateWith` function to provide a different
accumulator.

-}
manual : Config i i
manual =
    Debouncer.Internal.manual


{-| A starting point for a configuring a debouncer which **debounces**--
that is, which will emit once quiet for the time you specify.

So, `debounce (2 * Time.second)` is equivalent to

    manual
        |> settleWhenQuietFor (Just (2 * Time.second))

If you also want to emit using the first input, then you can use
`emitWhenSettled`. For instance, the following configuration would emit the
first input immediately when becoming unsettled, and then emit any
subsequent input once the debouncer was quiet for 2 seconds.

    debounce (2 * Time.second)
        |> emitWhenUnsettled (Just 0)

-}
debounce : Time -> Config i i
debounce interval =
    settleWhenQuietFor (Just interval) manual


{-| A starting point for configuring a debouncer which throttles -- that is,
which will emit the first input immediately, and then accmulate and
emit no more often than the specified interval.

So, `throttle (2 * Time.second)` is equivalent to

    manual
        |> emitWhileUnsettled (Just (2 * Time.second))
        |> emitWhenUnsettled (Just 0)

-}
throttle : Time -> Config i i
throttle interval =
    manual
        |> emitWhileUnsettled (Just interval)
        |> emitWhenUnsettled (Just 0)


{-| Should the debouncer do something special when it becomes unsettled?

If `Nothing` (the default), the debouncer emits only on the "trailing edge."
That is, it won't do anything special with the first input -- it will just
accumulate it and eventually emit according to the other configuration
parameters.

If `Just 0`, the debouncer will immediately emit the first input when becoming
unsettled. It will then remain unsettled, accumulating further input and
eventually emitting it.

If `Just interval`, the debouncer will use the provided interval to emit after
becoming unsettled. It will then remain unsettled, collecting further input and
eventually emitting it.

-}
emitWhenUnsettled : Maybe Time -> Config i o -> Config i o
emitWhenUnsettled =
    Debouncer.Internal.emitWhenUnsettled


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
emitFirstInput =
    Debouncer.Internal.emitFirstInput


{-| Should the debouncer emit while it is unsettled?

If `Nothing` (the default), the debouncer will wait until it becomes settled
again before emitting. This is what you might refer to as "debouncing".

If `Just interval`, the debouncer will emit at the provided interval while it
is unsettled. This is what you might refer to as "throttling".

-}
emitWhileUnsettled : Maybe Time -> Config i o -> Config i o
emitWhileUnsettled =
    Debouncer.Internal.emitWhileUnsettled


{-| How long should the debouncer wait without input before becoming "settled"
again?

If you are "debouncing" (i.e. `emitWhileUnsettled` is `Nothing`), then this is
the key parameter controlling when you will receive output -- you'll receive
the output after no inputs have been provided for the specified time.

If you are "throttling" (i.e. `emitWhileUnsettled is not`Nothing`), then this
parameter won't make much difference, unless you are also specifying
emitWhenUnsettled` in order to do something with the initial input.

-}
settleWhenQuietFor : Maybe Time -> Config i o -> Config i o
settleWhenQuietFor =
    Debouncer.Internal.settleWhenQuietFor


{-| How should the debouncer combine new input with the output collected
so far?

You can use several pre-built accumulators:

    - `lastInput` (the default)
    - `firstInput`
    - `allInputs`
    - `addInputs`
    - `appendInputToOutput`
    - `appendOutputToInput`

Or, if none of those suit, you can provide your own function of the form

    i -> Maybe o -> Maybe o

The `o` will be `Nothing` if this is the first input since the debouncer
has become unsettled.

-}
accumulateWith : Accumulator i o -> Config a b -> Config i o
accumulateWith =
    Debouncer.Internal.accumulateWith


{-| Initialize a `Debouncer` using the supplied `Config`.

For now, a debouncer's configuration cannot be changed once the debouncer is
created. (This is a feature that could be provided in future, if desirable).

-}
toDebouncer : Config i o -> Debouncer i o
toDebouncer =
    Debouncer.Internal.toDebouncer


{-| Messages which the debouncer handles.

You will need to integrate this into your own `Msg` type, and then handle it
in your `update` function (see code example above).

The type parameter represents the type of the input to be provided to
the debouncer. Thus, it should match the `i` in `Debouncer i o`.

You can construct a message with `provideInput`, `emitNow`, `cancelNow`, or
`settleNow`.

The only message you will typically need to send to the debouncer explicitly
is the message that provides input. You can construct such a message with the
`provideInput` function. Other messages are used internally by the debouncer.

-}
type Msg i
    = ProvideInput i
    | EmitNow
    | MsgInternal (Debouncer.Internal.Msg i)


{-| Construct a message that provides input to a debouncer.

The type parameter represents the type of the input to be provided. One typical
example would be your own `Msg` type, but it can be anything you like. It will
need to match the `i` in your `Debouncer i o` type.

-}
provideInput : i -> Msg i
provideInput =
    ProvideInput


{-| Construct a message which settles the debouncer now, even if it wouldn't
otherwise settle at this time.

Any accumulated output will be emitted. If you want to settle without emitting
any output, use `cancel` or `cancelNow` instead.

-}
settleNow : Msg i
settleNow =
    MsgInternal Debouncer.Internal.ManualSettle


{-| Cancel any input collected so far (and not yet emitted). This throws away
whatever input has been provided in the past, and forces the debouncer back to
a "settled" state (without emitting anything).
-}
cancel : Debouncer i o -> Debouncer i o
cancel =
    Debouncer.Internal.cancel


{-| Like `cancel`, but operates via a message instead of acting directly on
the debouncer. This is a convenience for cases where you'd like to cancel
via a message.
-}
cancelNow : Msg i
cancelNow =
    MsgInternal Debouncer.Internal.ManualCancel


{-| Construct a message which will emit any accumulated output. This doesn't
affect whether the debouncer is "settled" or not. If you'd like to emit output
and force the debouncer to be settled, then use `settleNow` instead.
-}
emitNow : Msg i
emitNow =
    EmitNow


{-| Handle a message for the debouncer.

You will need to integrate this into your `update` function, so that the debouncer
can act on its messages. (There is a code example at the top of the module docs).

If we have arrived at a time for output to be emitted, then the extra return
parameter will be the output. So, you should do whatever it is you want to do
with that output. For instance, if the output is your `Msg` type, you should
recursively feed it into your `update` function. (Again, see the example above
for that case). But that is just one example -- the output type can be whatever
you wish, and you can decide what to do with it.

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

        EmitNow ->
            -- Just gets the current time, and then hands the message to our
            -- internal update function.
            ( debouncer
            , Task.perform (MsgInternal << Debouncer.Internal.ManualEmitAt) Time.now
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
