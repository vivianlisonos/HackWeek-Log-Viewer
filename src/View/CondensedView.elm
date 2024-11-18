module View.CondensedView exposing (..)

-- Basics imports
-- HTML imports
-- Model imports
-- View imports
-- Parser imports
-- Util imports
-- Update function imports

import Browser
import Dict
import File exposing (..)
import File.Select as Select
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Lazy exposing (..)
import LogParser.LogParser exposing (parseControllerFile, parsePlayerRawFile)
import Model.LogModel exposing (..)
import Model.TimeStampModel exposing (..)
import Set
import Svg.Styled.Attributes exposing (mode)
import Task
import Update.UserAction exposing (..)
import Utils exposing (..)
import View.FileControl exposing (..)
import View.FilterControl exposing (..)
import View.LogItem exposing (..)


main : Program Int Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



------------------ MODEL SECTION ------------------------


init : Int -> ( Model, Cmd Msg )
init _ =
    ( defaultView, Cmd.none )


type alias Model =
    { --- viewData for controller logs
      displayedControllerLogs : List LogData
    , controllerLogRepo : List LogData
    , --- viewData for player logs
      playerLogRepo : Dict.Dict String (List LogData)
    , selectedPlayers : Set.Set String
    , --- viewData for showing relevant player log
      relevantPlayerLogs : List LogData
    , logViewMode : LogViewMode
    , --- viewData for key search
      playerSearchInput : String
    , searchPlayerKeys : Set.Set String
    , searchPlayerMode : SearchConfig
    , controllerSearchInput : String
    , searchControllerKeys : Set.Set String
    , searchControllerMode : SearchConfig
    , -- viewData for time search
      searchTimeStampRange : ( TimeStamp, TimeStamp )
    }


defaultTimeStamp =
    TimeStamp 0 0 0 0 0 0 0


defaultView =
    Model [] [] Dict.empty Set.empty [] JoinView "" Set.empty ( CaseSensitive, ContainsWord ) "" Set.empty ( CaseSensitive, ContainsWord ) ( defaultTimeStamp, defaultTimeStamp )



------------------------ ACTION SECTION -------------------------
---------------- MAIN UPDATE FUNCTION ---------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --- key search
        -- search for controller log
        AddControllerSearchKey key ->
            let
                newKeySet =
                    if String.all (\c -> c == ' ') key then
                        model.searchControllerKeys

                    else
                        Set.insert key model.searchControllerKeys

                logs =
                    filterControllerLogByKeyAndTime model.searchControllerMode newKeySet model.searchTimeStampRange model.controllerLogRepo
            in
            ( { model | searchControllerKeys = newKeySet, displayedControllerLogs = logs, controllerSearchInput = "" }, Cmd.none )

        RemoveControllerSearchKey key ->
            let
                newKeySet =
                    Set.remove key model.searchControllerKeys

                logs =
                    filterControllerLogByKeyAndTime model.searchControllerMode newKeySet model.searchTimeStampRange model.controllerLogRepo
            in
            ( { model | searchControllerKeys = newKeySet, displayedControllerLogs = logs }, Cmd.none )

        InputControllerSearch key ->
            ( { model | controllerSearchInput = key }, Cmd.none )

        -- search for player log
        InputPlayerSearch key ->
            ( { model | playerSearchInput = key }, Cmd.none )

        AddPlayerSearchKey key ->
            let
                newKeySet =
                    if String.all (\c -> c == ' ') key then
                        model.searchControllerKeys

                    else
                        Set.insert key model.searchPlayerKeys

                logs =
                    filterPlayerLogByKeyAndTime model.searchPlayerMode newKeySet model.searchTimeStampRange model.selectedPlayers model.playerLogRepo
            in
            ( { model | searchPlayerKeys = newKeySet, relevantPlayerLogs = logs, playerSearchInput = "" }, Cmd.none )

        RemovePlayerSearchKey key ->
            let
                newKeySet =
                    Set.remove key model.searchPlayerKeys

                logs =
                    filterPlayerLogByKeyAndTime model.searchPlayerMode newKeySet model.searchTimeStampRange model.selectedPlayers model.playerLogRepo
            in
            ( { model | searchPlayerKeys = newKeySet, relevantPlayerLogs = logs }, Cmd.none )

        --- time search
        SearchTimeStamp start end ->
            let
                controllerLogs =
                    filterControllerLogByKeyAndTime model.searchControllerMode model.searchControllerKeys ( start, end ) model.controllerLogRepo

                playerLogs =
                    filterPlayerLogByKeyAndTime model.searchPlayerMode model.searchPlayerKeys ( start, end ) model.selectedPlayers model.playerLogRepo
            in
            ( { model | displayedControllerLogs = controllerLogs, relevantPlayerLogs = playerLogs, searchTimeStampRange = ( start, end ) }, Cmd.none )

        PickStartTime start ->
            let
                ( _, end ) =
                    model.searchTimeStampRange

                controllerLogs =
                    filterControllerLogByKeyAndTime model.searchControllerMode model.searchControllerKeys ( start, end ) model.controllerLogRepo

                playerLogs =
                    filterPlayerLogByKeyAndTime model.searchPlayerMode model.searchPlayerKeys ( start, end ) model.selectedPlayers model.playerLogRepo
            in
            ( { model | displayedControllerLogs = controllerLogs, relevantPlayerLogs = playerLogs, searchTimeStampRange = ( start, Tuple.second model.searchTimeStampRange ) }, Cmd.none )

        PickEndTime end ->
            let
                ( start, _ ) =
                    model.searchTimeStampRange

                controllerLogs =
                    filterControllerLogByKeyAndTime model.searchControllerMode model.searchControllerKeys ( start, end ) model.controllerLogRepo

                playerLogs =
                    filterPlayerLogByKeyAndTime model.searchPlayerMode model.searchPlayerKeys ( start, end ) model.selectedPlayers model.playerLogRepo
            in
            ( { model | displayedControllerLogs = controllerLogs, relevantPlayerLogs = playerLogs, searchTimeStampRange = ( start, end ) }, Cmd.none )

        ShowAllController ->
            let
                startTime =
                    mapOpt defaultTimeStamp .timeStamp <| List.head model.controllerLogRepo

                endTime =
                    mapOpt defaultTimeStamp .timeStamp <| List.reverse >> List.head <| model.controllerLogRepo

                newControllerLogs =
                    filterLogDataByKeys model.searchControllerMode model.searchControllerKeys model.controllerLogRepo

                newPlayerLogs =
                    filterPlayerLogByKeyAndTime model.searchPlayerMode model.searchPlayerKeys ( startTime, endTime ) model.selectedPlayers model.playerLogRepo
            in
            ( { model | searchTimeStampRange = ( startTime, endTime ), displayedControllerLogs = newControllerLogs, relevantPlayerLogs = newPlayerLogs }, Cmd.none )

        ShowAllPlayer ->
            let
                allPlayerLogs =
                    List.sortWith (\a b -> compareTimeStamp a.timeStamp b.timeStamp) <| List.concat <| Dict.values model.playerLogRepo

                startTime =
                    mapOpt defaultTimeStamp .timeStamp <| List.head allPlayerLogs

                endTime =
                    mapOpt defaultTimeStamp .timeStamp <| List.reverse >> List.head <| allPlayerLogs

                newControllerLogs =
                    filterLogDataByKeys model.searchControllerMode model.searchControllerKeys model.controllerLogRepo

                newPlayerLogs =
                    filterPlayerLogByKeyAndTime model.searchPlayerMode model.searchPlayerKeys ( startTime, endTime ) model.selectedPlayers model.playerLogRepo
            in
            ( { model | searchTimeStampRange = ( startTime, endTime ), displayedControllerLogs = newControllerLogs, relevantPlayerLogs = newPlayerLogs }, Cmd.none )

        ---- file upload
        SelectControllerFile ->
            ( model, Select.file [ "*" ] UploadControllerFile )

        RemovePlayerLog playerName ->
            let
                newDisplayedPlayers =
                    Set.remove playerName model.selectedPlayers

                newPlayerRepo =
                    Dict.remove playerName model.playerLogRepo

                releventLogs =
                    filterPlayerLogByKeyAndTime model.searchControllerMode model.searchControllerKeys model.searchTimeStampRange model.selectedPlayers newPlayerRepo
            in
            ( { model | selectedPlayers = newDisplayedPlayers, relevantPlayerLogs = releventLogs, playerLogRepo = newPlayerRepo }, Cmd.none )

        UploadControllerFile file ->
            ( model, Task.perform (\content -> parseControllerFile content |> UpdateControllerLogRepo) (File.toString file) )

        UpdateControllerLogRepo logs ->
            let
                displayedLogs =
                    filterLogDataByKeys model.searchControllerMode model.searchControllerKeys logs

                startTime =
                    mapOpt defaultTimeStamp .timeStamp <| List.head logs

                endTime =
                    mapOpt defaultTimeStamp .timeStamp <| List.reverse >> List.head <| logs
            in
            ( { model | controllerLogRepo = logs, displayedControllerLogs = displayedLogs, searchTimeStampRange = ( startTime, endTime ) }, Cmd.none )

        SelectPlayerFile ->
            ( model, Select.file [ "*" ] UploadPlayerFile )

        UploadPlayerFile file ->
            let
                fileName =
                    File.name file

                playerName =
                    getOpt fileName <| List.head <| String.split "." fileName
            in
            ( { model | selectedPlayers = Set.insert playerName model.selectedPlayers }, Task.perform (\content -> ( playerName, parsePlayerRawFile playerName content ) |> UpdatePlayerLogRepo) (File.toString file) )

        UpdatePlayerLogRepo ( playerName, logs ) ->
            let
                updatedRepo =
                    Dict.insert playerName logs model.playerLogRepo

                relevantLog =
                    filterPlayerLogByKeyAndTime model.searchPlayerMode model.searchPlayerKeys model.searchTimeStampRange model.selectedPlayers updatedRepo
            in
            ( { model | playerLogRepo = updatedRepo, relevantPlayerLogs = relevantLog }, Cmd.none )

        ChangeViewMode mode ->
            ( { model | logViewMode = mode }, Cmd.none )

        ToggleControllyerCaseSensitivity ->
            let
                searchConfig =
                    case model.searchControllerMode of
                        ( CaseInsensitive, m ) ->
                            ( CaseSensitive, m )

                        ( CaseSensitive, m ) ->
                            ( CaseInsensitive, m )

                displayedLogs =
                    filterControllerLogByKeyAndTime searchConfig model.searchControllerKeys model.searchTimeStampRange model.controllerLogRepo
            in
            ( { model | searchControllerMode = searchConfig, displayedControllerLogs = displayedLogs }, Cmd.none )

        TogglePlayerCaseSensitivity ->
            let
                searchConfig =
                    case model.searchPlayerMode of
                        ( CaseInsensitive, m ) ->
                            ( CaseSensitive, m )

                        ( CaseSensitive, m ) ->
                            ( CaseInsensitive, m )

                displayedLogs =
                    filterPlayerLogByKeyAndTime searchConfig model.searchPlayerKeys model.searchTimeStampRange model.selectedPlayers model.playerLogRepo
            in
            ( { model | searchPlayerMode = searchConfig, relevantPlayerLogs = displayedLogs }, Cmd.none )

        ChooseDisplayedPlayer playerId displayed ->
            let
                newDisplayedPlayer =
                    (if displayed then
                        Set.insert

                     else
                        Set.remove
                    )
                        playerId
                        model.selectedPlayers

                displayedPlayerLogs =
                    filterPlayerLogByKeyAndTime model.searchPlayerMode model.searchPlayerKeys model.searchTimeStampRange newDisplayedPlayer model.playerLogRepo
            in
            ( { model | relevantPlayerLogs = displayedPlayerLogs, selectedPlayers = newDisplayedPlayer }, Cmd.none )



----------------------------------------------- VIEW Section ------------------------------------------------


view : Model -> Html.Html Msg
view model =
    Html.div [] [ toUnstyled <| mainCard model ]


mainCard : Model -> Html Msg
mainCard model =
    let
        { displayedControllerLogs, playerLogRepo, selectedPlayers, relevantPlayerLogs, logViewMode, playerSearchInput, searchPlayerKeys, searchPlayerMode, controllerSearchInput, searchControllerKeys, searchControllerMode, searchTimeStampRange } =
            model
    in
    div []
        [ div [ class "filter_control" ]
            [ div [ class "default_flex" ] [ lazy timeFilter searchTimeStampRange, lazy controlViewMode logViewMode ]
            , div [ class "player_controller_search" ]
                [ lazy3 controllerFilterSection searchControllerKeys controllerSearchInput searchControllerMode
                , lazy5 playerFilterSection searchPlayerKeys playerSearchInput searchPlayerMode (Dict.keys playerLogRepo) selectedPlayers
                ]
            ]

        -- , -- log view
        , case logViewMode of
            JoinView ->
                lazy2 comobineLogArea displayedControllerLogs relevantPlayerLogs

            SplitView ->
                lazy2 splitLogArea displayedControllerLogs relevantPlayerLogs

            ControllerOnly ->
                lazy controllerLogArea displayedControllerLogs

            PlayerOnly ->
                lazy playerLogArea relevantPlayerLogs
        ]
