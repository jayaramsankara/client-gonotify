module Notify exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src, class)
import WebSocket exposing (..)
import Time exposing (..)
import Date exposing (..)
import User exposing (..)
import Json.Decode as Json exposing (..)


--model


type Msg
    = Notify String
    | Tick Time
    | UserLogin User.Msg


type alias Notification =
    { message : String
    , time : Time
    }


type alias Model =
    { messages : List Notification
    , curTime : Time
    , userInfo : User.Model
    }


initState : Model
initState =
    Model []
        0
        (User.Model Nothing False False)


userPresent : Model -> Bool
userPresent model =
    model.userInfo.readyToConnect



-- update


notifyUpdate : Msg -> Model -> ( Model, Cmd Msg )
notifyUpdate msg model =
    case msg of
        Notify msg ->
            ( { model | messages = (Notification msg model.curTime) :: model.messages }, Cmd.none )

        Tick val ->
            ( { model | curTime = val }, Cmd.none )

        UserLogin userMsg ->
            let
                userModel =
                    model.userInfo
            in
                case userMsg of
                    User.UserIdReady uid ->
                        ( { model | userInfo = { userModel | userId = Just uid } }, Cmd.none )

                    User.ConnectReady ->
                        ( { model | userInfo = { userModel | readyToConnect = True } }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        userId =
            Maybe.withDefault "" model.userInfo.userId
    in
        if userPresent model then
            Sub.batch
                [ listen ("wss://gonotify.herokuapp.com/ws/" ++ (userId)) Notify
                , every Time.second Tick
                ]
        else
            Sub.batch
                [ every Time.second Tick
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


type alias RawMessage =
    { message : String
    , sender : String
    }


messageDecoder : Decoder RawMessage
messageDecoder =
    map2 RawMessage
        (field "message" string)
        (field "sender" string)


notifyMsg : String -> Maybe Notification -> Html Msg
notifyMsg style maybemsg =
    let
        extract : String -> ( String, String )
        extract rawMsg =
            Maybe.withDefault ( "", "" ) <| Maybe.map (\rm -> ( rm.message, rm.sender )) <| Result.toMaybe <| decodeString messageDecoder rawMsg
    in
        Maybe.withDefault (div [] [])
            (Maybe.map
                (\msg ->
                    let
                        ( message, sender ) =
                            extract msg.message
                    in
                        (div [] [ div [ class "time-display" ] [ text <| (formatTime msg.time) ++ " , " ++ sender ++ " says : " ], div [ class style ] [ text message ] ])
                )
                maybemsg
            )


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
        div [ class "notifysection" ] <| (notifyActiveMsg (List.head model.messages)) :: List.map notifyInactiveMsg ((Maybe.withDefault [] (List.tail model.messages)))


userConnectedView : Model -> Html Msg
userConnectedView model =
    body []
        [ div []
            [ div [ class "heading1" ] [ text ("Welcome " ++ (Maybe.withDefault "" model.userInfo.userId) ++ "!") ]
            , Html.br [] []
            , Html.br [] []
            , div [ class "heading2" ]
                [ text "Your Messages : " ]
            , Html.br [] []
            , notifyView model
            ]
        ]


userView : Model -> Html Msg
userView model =
    Html.map (\m -> UserLogin m) <| User.view model.userInfo


appView : Model -> Html Msg
appView model =
    if (userPresent model) then
        userConnectedView model
    else
        userView model


main : Program Never Model Msg
main =
    Html.program { init = ( initState, Cmd.none ), view = appView, update = notifyUpdate, subscriptions = subscriptions }
