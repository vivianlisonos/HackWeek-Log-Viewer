module View.CondensedView exposing (..)

-- Basics imports 
import Task
import Set
import Dict

-- HTML imports
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Attributes exposing (..)
import Html 
import Browser

-- Model imports 
import Model.LogModel exposing (..)
import Model.TimeStampModel exposing (..)

-- View imports 
import View.LogItem exposing (..)
import View.ActionItem exposing (..)
import View.FileControl exposing (..)

-- Parser imports
import File exposing (..)
import File.Select as Select 
import LogParser.LogParser exposing (parseIOSFile, parseAndroidFile, parsePlayerRawFile)

-- Util imports
import Utils exposing (..)

-- Update function imports 
import Update.UserAction exposing (..)


main : Program Int Model Msg
main = Browser.element {init = init , view = view, update = update, subscriptions = subscriptions}

subscriptions : Model -> Sub Msg 
subscriptions _ = Sub.none

------------------ MODEL SECTION ------------------------
init: Int -> (Model, Cmd Msg)
init _ = (defaultView, Cmd.none)


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
                startTime = mapOpt defaultTimeStamp .timeStamp <| List.head logs
                endTime = mapOpt defaultTimeStamp .timeStamp <| List.reverse >> List.head  <| logs 
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





