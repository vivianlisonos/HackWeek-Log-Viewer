module View.LogItem exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as A
import Model.LogModel exposing (..)
import Update.UserAction exposing (..)
import Css
import Html.Styled.Events exposing (onClick)
import EverySet exposing (EverySet)


type LogSectionModel = 
      Closed TimeStamp 
    | Expanded (List PlayerLog) (List ControllerLog) TimeStamp




logSectionShrinked: TimeStamp -> Html Msg 
logSectionShrinked timeStamp = 
    let timeString = displayedTimeString { timeStamp | miliSecond = 0}
    in div [A.css [Css.width <| Css.vw 20, Css.margin2 (Css.vw 0) (Css.vw 40)]] [button [onClick <| PickTime timeStamp] [text timeString]]


logItem: String -> TimeStamp -> Html Msg 
logItem content timestamp = 
    div [A.css [Css.border3 (Css.px 2) Css.solid (Css.rgb 250 250 250), 
                        Css.hover [Css.border3 (Css.px 2) Css.solid (Css.rgb 10 10 10 )]]] 
        [div [A.css [Css.fontSize <| Css.px 10, Css.color <| Css.rgb 150 150 150]]
                [text <| displayedTimeString timestamp],
         pre [A.css [Css.overflow Css.scroll]] [text content]]
    
playerLogView : PlayerLog  -> Html Msg 
playerLogView log = 
    let text =  "[" ++ log.playerAPI ++ "] " ++ log.content 
    in logItem text log.timeStamp


playerLogsView : List PlayerLog  -> Html Msg 
playerLogsView logs = 
    div [] <| List.map playerLogView logs 



controllerLogView : ControllerLog  -> Html Msg 
controllerLogView log = 
    let text = "[" ++ log.package ++ "]" ++ " [" ++ log.class ++ "] " ++ log.content
    in logItem text log.timeStamp

controllerLogsView : List ControllerLog  -> Html Msg 
controllerLogsView logs = 
    div [] <| List.map controllerLogView logs 

displayedTimeString: TimeStamp -> String 
displayedTimeString time = 
    let yy = String.fromInt time.year 
        mm = String.fromInt time.month 
        dd = String.fromInt time.day 
        hour = String.fromInt time.hour 
        minute = String.fromInt time.minute 
        second = String.fromInt time.second 
        milliSecond = String.fromInt time.miliSecond
    in yy ++ "-" ++ mm ++ "-" ++ dd ++ "   T" ++ hour ++ ":" ++ minute ++ ":" ++ second ++ ":" ++ milliSecond 
