module View.LogItem exposing (..)

--
-- HTML import

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.LogModel exposing (..)
import Model.TimeStampModel exposing (..)
import Update.UserAction exposing (..)
import Set


defaultPlayerLogItem =
    logItem "player_log_item"


defaultControllerLogItem =
    logItem "controller_log_item"


defaultLogItem log =
    case log.origin of
        Player _ ->
            defaultPlayerLogItem log

        Controller ->
            defaultControllerLogItem log


splitPlayerLogItem =
    logItem "split_player_item"


splitControllerLogItem =
    logItem "split_controller_item"


splitLogItem log =
    case log.origin of
        Player _ ->
            splitPlayerLogItem log

        Controller ->
            splitControllerLogItem log


logItem className { content, timeStamp, origin } =
    div [ classList [ ( "log_item_container", True ), ( className, True ) ] ]
        [ div [ class "log_item_label", css [ Css.fontSize <| Css.px 10, Css.color <| Css.rgb 150 150 150 ] ]
            [ div [css [Css.displayFlex]]
                [ text <| displayedTimeString timeStamp
                , case origin of
                    Player player ->
                        div [css [Css.marginLeft <| Css.vw 1]] [text player]

                    Controller ->
                        div [] []
                ]
            , div [ class "pick_time_container" ]
                [ 
                  div [] [button [class "time_picker_button"][text "hide"]]
                , div [ class "pick_time" ] [ button [ onClick <| PickStartTime timeStamp, class "time_picker_button" ] [ text "before " ] ]
                , div [] [button [class "time_picker_button"][text " / "]]
                , div [ class "pick_time" ] [ button [ onClick <| PickEndTime timeStamp, class "time_picker_button" ] [ text "after" ] ]
                ]
            ]
        , pre [ css [ Css.overflow Css.scroll ] ] [ text content ]
        ]


emphasizeSearched string keys =
    Set.foldl (\key acc -> String.replace key ("<strong>" ++ key ++ "</strong>") acc) string keys


displayedTimeString : TimeStamp -> String
displayedTimeString time =
    let
        yy =
            String.fromInt time.year

        mm =
            String.fromInt time.month

        dd =
            String.fromInt time.day

        hour =
            String.fromInt time.hour

        minute =
            String.fromInt time.minute

        second =
            String.fromInt time.second

        milliSecond =
            String.fromInt time.miliSecond
    in
    yy ++ "-" ++ mm ++ "-" ++ dd ++ "   T" ++ hour ++ ":" ++ minute ++ ":" ++ second ++ ":" ++ milliSecond



------------------- log view for controller -------------------

controllerLogArea logs =
    div [] <| List.map (logItem "controller_log_item") logs



------------------- log view for player -------------------

playerLogArea logs =
    div [] <| List.map (logItem "player_log_item") logs



------------------- log view for mix ------------------------


comobineLogArea (controllerLogs, playerLogs) =
    let
        totalLog =
            List.sortWith (\a b -> compareTimeStamp a.timeStamp b.timeStamp) (List.append controllerLogs playerLogs)
    in
     div [] <| List.map defaultLogItem totalLog 


splitLogArea (controllerLogs, playerLogs) =
    let
        totalLog =
            List.sortWith (\a b -> compareTimeStamp a.timeStamp b.timeStamp) (List.append controllerLogs playerLogs)
    in
   div [] <| List.map splitLogItem totalLog 
