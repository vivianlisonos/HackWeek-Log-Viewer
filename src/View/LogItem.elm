module View.LogItem exposing (..)
import Model.LogModel exposing (..)
import Model.TimeStampModel exposing (..)
import Update.UserAction exposing (..)
-- 
-- HTML import 
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Attributes exposing (..)
import Css


defaultPlayerLogItem = logItem "player_log_item"
defaultControllerLogItem = logItem "controller_log_item"
defaultLogItem log = case log.origin of Player _ ->  defaultPlayerLogItem log 
                                        Controller -> defaultControllerLogItem log 

splitPlayerLogItem = logItem "split_player_item"
splitControllerLogItem = logItem "split_controller_item"
splitLogItem log = case log.origin of Player _ -> splitPlayerLogItem log
                                      Controller -> splitControllerLogItem log 

logItem className { content, timeStamp, origin }  = 
    div [css [Css.border3 (Css.px 2) Css.solid (Css.rgb 250 250 250), 
                        Css.hover [Css.border3 (Css.px 2) Css.solid (Css.rgb 10 10 10 )]], 
        class className] 
        [div [css [Css.fontSize <| Css.px 10, Css.color <| Css.rgb 150 150 150]]
             [text <| displayedTimeString timeStamp,
              case origin of 
                    Player player -> text player
                    Controller -> div [][]
             ],
         pre [css [Css.overflow Css.scroll]] [text content]]

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

------------------- log view for controller ------------------- 
controllerLogArea logs = div [][header [] [text "controller log view"], controllerLogsListView logs]

controllerLogsListView logs = 
    div [] <| List.map (logItem "controller_log_item") logs 


------------------- log view for player ------------------- 
playerLogArea logs = 
    div [] [header [] [text "player log"], playerLogsListView logs]


playerLogsListView logs = 
    div [] <| List.map (logItem "player_log_item") logs 

------------------- log view for mix ------------------------
comobineLogArea controllerLogs playerLogs = 
    let totalLog = List.sortWith (\a b -> compareTimeStamp a.timeStamp b.timeStamp) (List.append controllerLogs playerLogs) 
    in div [] [header [][text "combined logs"], div [] <| List.map defaultLogItem totalLog]

splitLogArea  controllerLogs playerLogs = 
    let totalLog = List.sortWith (\a b -> compareTimeStamp a.timeStamp b.timeStamp) (List.append controllerLogs playerLogs) 
    in div [class "split_container"] [header [][text "split logs"], div [] <| List.map splitLogItem totalLog] 
-------- time selection view ---------
