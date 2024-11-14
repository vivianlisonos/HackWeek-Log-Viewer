module View.TimeLine exposing (..)
import Model.LogModel exposing (..)
import List exposing (minimum)
import Update.UserAction exposing (..)
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Attributes as Attribute

timeLine : List TimeStamp -> Html Msg
timeLine timeStamps =
    div [] <| List.map timeItem timeStamps

timeItem : TimeStamp -> Html Msg 
timeItem timeStamp = 
    let { year, month, day, hour, minute, second, miliSecond} = timeStamp
        timeStr = String.join ":" <| List.map String.fromInt [hour, minute, second, miliSecond]
    in div [Attribute.css []][button [onClick (PickTime timeStamp)] [text  timeStr]]
