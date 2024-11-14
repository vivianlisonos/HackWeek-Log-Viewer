module View.CondensedView exposing (..)
import Model.LogModel exposing (..)
import File exposing (..)
import File.Select as Select 
import Task
import Html.Styled exposing (..)
import Html 
import Html.Styled.Events exposing (..)
import Html.Styled.Attributes exposing (..)
import Css
import Browser
import View.MainView exposing (displayedLogs)
import Utils exposing (..)
import Update.UserAction exposing (Msg(..))
import Update.UserAction exposing (hitSearchTarget)
import LogParser.LogParser exposing (parseIOSFile, parseAndroidFile, parsePlayerRawFile)
import Set
import Dict

main : Program Int Model Msg
main = Browser.element {init = init , view = view, update = update, subscriptions = subscriptions}

subscriptions : Model -> Sub Msg 
subscriptions model = Sub.none

------------------ MODEL SECTION ------------------------
init: Int -> (Model, Cmd Msg)
init flags = (defaultView, Cmd.none)

type LogViewMode = CombineView | SpliteView | PlayerOnly | ControllerOnly

type alias Model = {
        --- viewData for controller logs
        displayedControllerLogs: List LogData, 
        controllerLogRepo: List LogData, 

        --- viewData for player logs 
        playerLogRepo: Dict.Dict String (List LogData),
        playerLogFiles: Set.Set String, 
        
        --- viewData for showing relevant player log 
        relevantPlayerLogs: List LogData, 
        relevantPlayerLogNotification: Maybe String, 
        logViewMode: LogViewMode, 

        --- viewData for key search 
        searchPlayerKey: String, 
        searchControllerKey: String,
        searchMode: SearchConfig, 

        -- viewData for time search
        searchTimeStampRange: (TimeStamp, TimeStamp)
    }

defaultTimeStamp = TimeStamp 0 0 0 0 0 0 0 
defaultView = Model [] [] Dict.empty Set.empty [] Nothing ControllerOnly "" "" (SearchConfig CaseSensitive ContainsWord) (defaultTimeStamp, defaultTimeStamp) 


------------------------ ACTION SECTION -------------------------
type Msg = 
    --- action regarding search 
        InputPlayerSearch String 
    |   InputControllerSearch String 
    |   ConfirmSearchPlayer String 
    |   ConfirmSearchController String 

    --- action regarding filter timestamp 
    |  SearchTimeStamp TimeStamp TimeStamp 
    |  PickStartTime TimeStamp 
    |  PickEndTime TimeStamp
    
    --- action regarding uploading file 
    |  SelectIOSFile 
    |  SelectAndriodFile
    |  SelectPlayerFile
    |  UploadIOSFile File 
    |  UploadAndroidFile File 
    |  UploadPlayerFile File 
    |  UpdateControllerLogRepo (List LogData)
    |  UpdatePlayerLogRepo (String, List LogData)

    --- action regarding player log 
    | ChangeViewMode LogViewMode

---------------- MAIN UPDATE FUNCTION ---------------------
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        --- key search
        InputPlayerSearch key -> ({ model | searchPlayerKey = key}, Cmd.none )
        InputControllerSearch key -> ( { model | searchControllerKey = key}, Cmd.none)
        ConfirmSearchController key -> 
            -- TODO: decide if we need to search on player logs as well 
            let logs = filterControllerLogByKeyAndTime model.searchMode key model.searchTimeStampRange model.controllerLogRepo
            in ( { model | displayedControllerLogs = logs }, Cmd.none ) 
       
        ConfirmSearchPlayer key -> 
            let logs = filterPlayerLogByKeyAndTime model.searchMode key model.searchTimeStampRange model.playerLogRepo
            in  ( { model | relevantPlayerLogs = logs }, Cmd.none)

        --- time search
        SearchTimeStamp start end -> 
            let controllerLogs = filterControllerLogByKeyAndTime model.searchMode model.searchControllerKey (start, end) model.controllerLogRepo
                -- TODO: consider ability to allow user to select player that they want to be displayed 
                playerLogs = filterPlayerLogByKeyAndTime model.searchMode model.searchPlayerKey (start, end) model.playerLogRepo 
                showNotification = if List.isEmpty playerLogs then Nothing else Just "player logs found during selected timestamp"
            in ({model | displayedControllerLogs = controllerLogs, relevantPlayerLogs = playerLogs, searchTimeStampRange = (start, end), relevantPlayerLogNotification = showNotification}, Cmd.none)
        PickStartTime start -> ({model | searchTimeStampRange = (start, Tuple.second model.searchTimeStampRange)}, Cmd.none)
        PickEndTime end -> ({model | searchTimeStampRange = (Tuple.first model.searchTimeStampRange,  end)}, Cmd.none)

        ---- file upload 
        SelectIOSFile -> (model, Select.file ["*"] UploadIOSFile)
        SelectAndriodFile -> (model, Select.file ["*"] UploadAndroidFile)
        -- TODO: update the parser to have better way to decide the terminating character 
        UploadAndroidFile file -> (model, Task.perform (\content -> parseAndroidFile "2024-" content |> UpdateControllerLogRepo) (File.toString file))
        UploadIOSFile file -> (model, Task.perform (\content -> parseIOSFile "2024/" content |> UpdateControllerLogRepo) (File.toString file))
        UpdateControllerLogRepo logs ->
            let displayedLogs = searchLogData model.searchMode model.searchControllerKey logs 
                startTime = mapOpt (TimeStamp 0 0 0 0 0 0 0) .timeStamp <| List.head logs
                endTime = mapOpt (TimeStamp 0 0 0 0 0 0 0) .timeStamp <| List.reverse >> List.head  <| logs 
            in 
            ( {model | controllerLogRepo = logs , displayedControllerLogs = displayedLogs, searchTimeStampRange = (startTime, endTime)}, Cmd.none)
        SelectPlayerFile -> (model, Select.file ["*"] UploadPlayerFile)
        UploadPlayerFile file -> 
            let fileName = File.name file 
                playerName = getOpt fileName <| List.head <| String.split "." fileName in 
            ( { model | playerLogFiles = Set.insert playerName model.playerLogFiles } , Task.perform (\content -> (playerName, parsePlayerRawFile playerName content)|> UpdatePlayerLogRepo) (File.toString file))
        UpdatePlayerLogRepo (playerName, logs) -> 
          let   (start, end) = model.searchTimeStampRange
                playerLog = getLogDataDuringRange start end logs in 
            ( { model | playerLogRepo = Dict.insert playerName logs model.playerLogRepo, relevantPlayerLogs = playerLog }, Cmd.none)
        ChangeViewMode mode -> ( { model | logViewMode = mode }, Cmd.none)

-- function to filter logs by timestamp
getLogDataDuringRange start end logs = List.filter (\log -> isInrange start end log.timeStamp) logs 


-- functions to filter logs by search key word
searchLogData searchConfig key logs = List.filter (\log -> hitSearchTarget searchConfig key log.content) logs 

--- functions to filter logs by search key and then timeStamp 
filterControllerLogByKeyAndTime searchLogConfig key (start, end) logs = 
    getLogDataDuringRange start end <| searchLogData searchLogConfig key logs 

filterPlayerLogByKeyAndTime searchLogConfig key (start, end) logs = 
    let allPlayerLogs = List.concat <| Dict.values logs 
        filteredLogs = getLogDataDuringRange start end <|  searchLogData searchLogConfig key allPlayerLogs
    in List.sortWith (\a b -> compareTimeStamp a.timeStamp b.timeStamp) filteredLogs


----------------------------------------------- VIEW Section ------------------------------------------------
view: Model -> Html.Html Msg 
view model = 
    Html.div [] [toUnstyled <| mainCard model]

mainCard: Model -> Html Msg 
mainCard model = div [] 
    [actionPanel model.searchControllerKey model.searchPlayerKey model.searchTimeStampRange,
     uploadedPlayerFileSection model.playerLogFiles,
     hasPlayerLogNotification model.relevantPlayerLogNotification,
     case model.logViewMode  of
        SpliteView -> splitLogArea model.displayedControllerLogs model.relevantPlayerLogs
        ControllerOnly -> controllerLogArea model.displayedControllerLogs
        PlayerOnly -> playerLogArea model.relevantPlayerLogs
        CombineView -> comobineLogArea model.displayedControllerLogs model.relevantPlayerLogs
    ]

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

uploadedPlayerFileSection: Set.Set String -> Html Msg 
uploadedPlayerFileSection players = 
    div [class "player_file_list"] <| List.map text <| Set.toList players

-------------------------
hasPlayerLogNotification notification = 
    case notification of 
    Just message -> div [] [
        text message, 
        button [onClick <| ChangeViewMode SpliteView] [text "split"], 
        button [onClick <| ChangeViewMode CombineView] [text "interwined"], 
        button [onClick <| ChangeViewMode ControllerOnly] [text "controller only"], 
        button [onClick <| ChangeViewMode PlayerOnly][text "player only"]]
    Nothing -> div [] []


----------------------------------------------- log view ------------------------------------------------
defaultPlayerLogItem = logItem "player_log_item"
defaultControllerLogItem = logItem "controller_log_item"
defaultLogItem log = case log.origin of Player _ ->  defaultPlayerLogItem log 
                                        Controller -> defaultControllerLogItem log 

splitPlayerLogItem = logItem "split_player_item"
splitControllerLogItem = logItem "split_controller_item"
splitLogItem log = case log.origin of Player _ -> splitPlayerLogItem log
                                      Controller -> splitControllerLogItem log 

logItem: String -> LogData -> Html Msg 
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
controllerLogArea: List LogData -> Html Msg 
controllerLogArea logs = div [][header [] [text "controller log view"], controllerLogsListView logs]

controllerLogsListView : List LogData  -> Html Msg 
controllerLogsListView logs = 
    div [] <| List.map (logItem "controller_log_item") logs 


------------------- log view for player ------------------- 
playerLogArea : List LogData -> Html Msg 
playerLogArea logs = 
    div [] [header [] [text "player log"], playerLogsListView logs]


playerLogsListView : List LogData  -> Html Msg 
playerLogsListView logs = 
    div [] <| List.map (logItem "player_log_item") logs 

------------------- log view for mix ------------------------
comobineLogArea : List LogData -> List LogData -> Html Msg 
comobineLogArea controllerLogs playerLogs = 
    let totalLog = List.sortWith (\a b -> compareTimeStamp a.timeStamp b.timeStamp) (List.append controllerLogs playerLogs) 
    in div [] [header [][text "combined logs"], div [] <| List.map defaultLogItem totalLog]

splitLogArea : List LogData -> List LogData -> Html Msg 
splitLogArea  controllerLogs playerLogs = 
    let totalLog = List.sortWith (\a b -> compareTimeStamp a.timeStamp b.timeStamp) (List.append controllerLogs playerLogs) 
    in div [class "split_container"] [header [][text "split logs"], div [] <| List.map splitLogItem totalLog] 
-------- time selection view ---------

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

