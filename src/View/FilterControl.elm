module View.FilterControl exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import LogParser.Timestamp exposing (parseHtmlDate, unparseHtmlDate)
import Model.LogModel exposing (CaseConfig(..))
import Model.TimeStampModel exposing (..)
import Set
import Update.UserAction exposing (..)
import Utils exposing (..)
import View.FileControl exposing (uploadedPlayers)


----------------------
---- Time Action -----
----------------------


timeFilter ( startTime, endTime ) =
    div [ class "time_filter_container" ] [ div [ class "start_end_time_filter" ] [ pickStartTime "Display from " startTime, pickEndTime "to " endTime ], div [ class "time_filter_button_container" ] [allControllerBtn, allPlayerBtn] ]

allControllerBtn =
    button [ class "basic_button", onClick ShowAllController ] [ text "reset to controller" ]

allPlayerBtn = 
    button [ class "basic_button", onClick ShowAllPlayer ] [ text "reset to player" ]
pickStartTime =
    pickTime PickStartTime


pickEndTime =
    pickTime PickEndTime


pickTime : (TimeStamp -> Msg) -> String -> TimeStamp -> Html Msg
pickTime action label time =
    div [ class "time_input_container" ]
        [ h3 [ class "time_input_leading_label" ] [ text label ]
        , div [ class "time_input_body" ]
            [ div []
                [ h4 [] [ text "date" ]
                , input
                    [ class "time_day_inpute"
                    , type_ "date"
                    , onInput
                        (\date ->
                            let
                                { year, month, day } =
                                    parseHtmlDate date
                            in
                            action { time | year = year, month = month, day = day }
                        )
                    , value (unparseHtmlDate time.year time.month time.day)
                    ]
                    []
                ]
            , div []
                [ h4 [] [ text "hour" ]
                , input [ class "time_input_component", placeholder "hh", onInput (\hh -> action { time | hour = getOpt 0 <| String.toInt hh }), value <| String.fromInt time.hour ] []
                , strong [] [ text ":" ]
                ]
            , div []
                [ h4 [] [ text "minute" ]
                , input [ class "time_input_component", placeholder "mm", onInput (\min -> action { time | minute = getOpt 0 <| String.toInt min }), value <| String.fromInt time.minute ] []
                , strong [] [ text ":" ]
                ]
            , div []
                [ h4 [] [ text "second" ]
                , input [ class "time_input_component", placeholder "ss", onInput (\ss -> action { time | second = getOpt 0 <| String.toInt ss }), value <| String.fromInt time.second ] []
                ]
            ]
        ]



--------------------------
--- Search Section -------
--------------------------


configControl ( caseConfig, matchConfig ) changeCase =
    button [ class "case_control_button", classList [("case_sensitive", caseConfig == CaseSensitive)], onClick changeCase ] [ text "Aa" ]


controllerFilterSection searchKeys inputValue searchConfig =
    div [ class "filter_keys_container" ]
        [ h3 [ class "filter_keys_subtitle" ] [ text "search controller logs" ]
        , div [ class "filter_keys_body" ]
            [ input [ placeholder "enter search keyword", value inputValue, onInput InputControllerSearch, class "search_key_item_input" ] []
            , button [ onClick <| AddControllerSearchKey inputValue, class "add_search_key_button" ] [ text "+" ]
            , configControl searchConfig ToggleControllyerCaseSensitivity
            , button [ onClick SelectControllerFile, class "upload_file_button" ] [ text "Upload Controller Log" ]
            ]
        , keysList searchKeys RemoveControllerSearchKey
        ]


playerFilterSection searchKeys inputValue searchConfig playerFiles selectedPlayers =
    div [ class "filter_keys_container" ]
        [ h3 [ class "filter_keys_subtitle" ] [ text "search player logs" ]
        , div [ class "filter_keys_body" ]
            [ input [ placeholder "enter search keyword", value inputValue, onInput InputPlayerSearch, class "search_key_item_input" ] []
            , button [ onClick <| AddPlayerSearchKey inputValue, class "add_search_key_button" ] [ text "+" ]
            , configControl searchConfig TogglePlayerCaseSensitivity
            , button [ onClick SelectPlayerFile, class "upload_file_button" ] [ text "Upload Player Log" ]
            ]
        , keysList searchKeys RemovePlayerSearchKey
        , uploadedPlayers playerFiles selectedPlayers
        ]


keywordItem removeKeyAction key =
    strong [ class "deletable_button_container" ] [ text key, button [ class "deletable_button", onClick <| removeKeyAction key ] [ text "˟" ] ]


keysList keys removeKeyAction =
    div [ class "search_keywordItem_container" ] <| List.map (keywordItem removeKeyAction) <| Set.toList keys
