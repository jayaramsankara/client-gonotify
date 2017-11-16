module User exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type Msg
    = UserIdReady String
    | ConnectReady


type alias Model =
    { userId : Maybe String
    , readyToConnect : Bool
    , connected : Bool
    }


view : Model -> Html Msg
view model =
    div [ class "loginsection" ] [ table [] [ tr [] [ td [] [ input [ placeholder "Enter User Name", onInput UserIdReady, class "recipient-field" ] [] ], td [] [ button [ onClick ConnectReady, class "connect" ] [ text "Connect" ] ] ] ] ]
