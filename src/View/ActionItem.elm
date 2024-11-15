module View.ActionItem exposing (..)


import Update.UserAction exposing (..)
import Model.TimeStampModel exposing (..)
import Utils exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Attributes exposing (..)


actionPanel: String -> String -> (TimeStamp, TimeStamp) -> Html Msg 
actionPanel controllerKey playerKey (start, end) = 
        div [] 
        -- TODO: have a better way to d
            [button [onClick SelectIOSFile] [text "select IOS controller file"],
             button [onClick SelectAndriodFile] [text "select Android controller file"],
             button [onClick SelectPlayerFile] [text "select Player log file"],
             div [] [input [placeholder "enter search keyword", value controllerKey, onInput InputControllerSearch] [],
                     button [onClick <| ConfirmSearchController controllerKey] [text "search controller log"]],
             div [] [input [placeholder "enter search keyword", value playerKey, onInput InputPlayerSearch] [],
                     button [onClick <| ConfirmSearchPlayer playerKey] [text "search player log"]],
            timeFilter start end] 

----------------------
---- Time Action -----
----------------------
-- TODO: add better validation 
timeFilter startTime endTime= 
    div [class "time_filter_container"] [pickStartTime "start time" startTime, pickEndTime "end time" endTime, searchButton startTime endTime]

searchButton startTime endTime = button [class "search_button", onClick <| SearchTimeStamp startTime endTime] [text "filter by time"]
pickStartTime = pickTime PickStartTime
pickEndTime = pickTime PickEndTime 

--- TODO: add time label
pickTime : (TimeStamp -> Msg) -> String -> TimeStamp -> Html Msg 
pickTime action label time  = 
    div [class "time_input_container"] 
        [text label, 
        text "year",
        input [class "time_input_component", placeholder "yyyy", onInput (\year -> action { time | year =  getOpt 0 <| String.toInt year }), value <| String.fromInt time.year] [],
        text "month",
        input [class "time_input_component", placeholder "mm", onInput (\month -> action { time | month =  getOpt 0 <| String.toInt month }), value <| String.fromInt time.month] [],
        text "day",
        input [class "time_input_component", placeholder "dd", onInput (\day -> action { time | day =  getOpt 0 <| String.toInt day }), value <| String.fromInt time.day] [],
        text "T",
        input [class "time_input_component", placeholder "hh", onInput (\hour -> action { time | hour =  getOpt 0 <| String.toInt hour }), value <| String.fromInt time.hour] [], 
        text ":",
        input [class "time_input_component", placeholder "mm", onInput (\minute -> action { time | minute =  getOpt 0 <| String.toInt minute }), value <| String.fromInt time.minute] [],
        text ":",
        input [class "time_input_component", placeholder "ss", onInput (\second -> action { time | second =  getOpt 0 <| String.toInt second }), value <| String.fromInt time.second] []]