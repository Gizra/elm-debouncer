module Messages exposing (Model, Msg(..), init, main, update, updateDebouncer, view)

{-| This does exactly the same thing as the `Basic` example, but it
uses `Debouncer.Messages` instead of `Debouncer.Basic`. This simplifies
your code in the (common) case where what you're debouncing is your
own `Msg` type. (You would want `Debouncer.Basic` in other cases, since
it is more general).
-}

import Browser
import Debouncer.Messages as Debouncer exposing (Debouncer, provideInput, settleWhenQuietFor, toDebouncer)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { quietForOneSecond : Debouncer Msg
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { quietForOneSecond =
            Debouncer.manual
                |> settleWhenQuietFor (Just <| 1 * 1000)
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
    div [ style "margin" "1em" ]
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


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
