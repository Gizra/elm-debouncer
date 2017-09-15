module Basic exposing (..)

{-| This shows an implementation of debouncing your own `Msg` type using
`Debouncer.Basic`. It's actually better to use `Debouncer.Messages` for this --
see the `Messages` example for that simplified approach. But you could use
`Debouncer.Basics` in other cases -- it is more general.
-}

import Debouncer.Basic as Debouncer exposing (Debouncer, provideInput)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)


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
