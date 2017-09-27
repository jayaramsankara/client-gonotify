module Notify exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src, class)
import WebSocket exposing (..)
import Time exposing (..)
import Date exposing (..)


--model


type Msg
    = Notify String
    | Tick Time


type alias Notification =
    { message : String
    , time : Time
    }


type alias Model =
    { messages : List Notification
    , curTime : Time
    }


initState : Model
initState =
    Model [] 0



-- update


notifyUpdate : Msg -> Model -> ( Model, Cmd Msg )
notifyUpdate msg model =
    case msg of
        Notify msg ->
            ( { model | messages = (Notification msg model.curTime) :: model.messages }, Cmd.none )

        Tick val ->
            ( { model | curTime = val }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ listen "wss://gonotify.herokuapp.com/ws/mlittle" Notify
        , every Time.second Tick
        ]



-- View


formatTime : Time -> String
formatTime time =
    if time == 0 then
        ""
    else
        let
            date =
                Date.fromTime time

            leadingZero x =
                (if x < 10 then
                    "0"
                 else
                    ""
                )
                    ++ toString x
        in
            leadingZero (Date.hour date % 12)
                ++ ":"
                ++ leadingZero (Date.minute date)
                ++ ":"
                ++ leadingZero (Date.second date)
                ++ if Date.hour date <= 12 then
                    "AM"
                   else
                    "PM"


notifyMsg : String -> Maybe Notification -> Html Msg
notifyMsg style maybemsg =
    Maybe.withDefault (div [] []) (Maybe.map (\msg -> div [] [ div [ class "time-display" ] [ text (formatTime msg.time) ], div [ class style ] [ text msg.message ] ]) maybemsg)


notifyView : Model -> Html Msg
notifyView model =
    let
        notifyActiveMsg : Maybe Notification -> Html Msg
        notifyActiveMsg msg =
            notifyMsg "triangle-right-active" msg

        notifyInactiveMsg : Notification -> Html Msg
        notifyInactiveMsg msg =
            notifyMsg "triangle-right" (Just msg)
    in
        body [ src "clhero.png" ]
            [ div [] <| (notifyActiveMsg (List.head model.messages)) :: List.map notifyInactiveMsg ((Maybe.withDefault [] (List.tail model.messages))) ]


main : Program Never Model Msg
main =
    Html.program { init = ( initState, Cmd.none ), view = notifyView, update = notifyUpdate, subscriptions = subscriptions }
