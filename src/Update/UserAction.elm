module Update.UserAction exposing (..)
import Model.LogModel exposing (..)
import LogParser.Logs exposing (..)
import File.Select as Select 
import File exposing (..)
import Task
import EverySet exposing (..)
import Html exposing (time)
type OS = Android | IOS 
type Msg = 
          SearchPlayerKey String  
        | SearchControllerKey String 
        | SearchFromPlayer String 
        | SearchFromController String 
        | PickTime  TimeStamp 
        | UnpickTime TimeStamp
        | UpdatePlayerFile File  
        | UpdateAndroidControllerFile File  -- TODO: 
        | UpdateIOSControllerFile File -- TODO: add a way to update decide controller file dynamically 
        | PickPlayer String 
        | ChooseControllerOS OS 
        | SearchMode SearchConfig
        | SelectPlayerFile 
        | SelectAndroidControllerFile 
        | SelectIOSControllerFile 
        | UpdatePlayerLogDB (List PlayerLog)
        | UpdateControlloerLogDB (List ControllerLog)
        | SearchByTime TimeStamp TimeStamp 



update : Msg -> Model -> (Model , Cmd Msg)
update msg model = 
    case msg of 
        SelectPlayerFile -> (model, Select.file ["*"] UpdatePlayerFile)
        SelectIOSControllerFile -> (model, Select.file ["*"] UpdateIOSControllerFile)
        SelectAndroidControllerFile -> (model, Select.file ["*"] UpdateAndroidControllerFile)
        -- TODO: swap "\n" to dynamically determine the terminal 
        UpdatePlayerFile file -> (model, Task.perform   (\content -> parsePlayerFile "\n" content |> UpdatePlayerLogDB) (File.toString file))
        -- TODO: swap 2024 to dynamically determine the terminal
        UpdateIOSControllerFile file -> (model, Task.perform (\content -> parseIOSControllerFile "2024/" content |> UpdateControlloerLogDB) (File.toString file))
        UpdateAndroidControllerFile file -> (model, Task.perform (\content -> parseAndroidControllerFile "2024-" content |> UpdateControlloerLogDB) (File.toString file))

        UpdatePlayerLogDB playerLogs -> ({model | allPlayerLogs = playerLogs}, Cmd.none)
        UpdateControlloerLogDB controllerLogs -> ({ model | allControllerLogs = controllerLogs}, Cmd.none)

        -- search action 
        SearchControllerKey key -> ({model | searchControlerTarget = key} , Cmd.none)
        SearchPlayerKey key -> ({ model | searchPlayerTarget = key}, Cmd.none)
        SearchFromPlayer key -> 
            let logs = searchFromPlayerLogs model.searchMode key model.allPlayerLogs
                timeStamp = uniqueTimeLine model.displayedControllerLogs logs 
            in ({ model | displayedPlayerLogs = logs, displayedTimeStamps = timeStamp }, Cmd.none)
        SearchFromController key -> 
            let logs = searchFromControllerLogs model.searchMode key model.allControllerLogs 
                timeStamp = uniqueTimeLine logs model.displayedPlayerLogs
            in  ({ model | displayedControllerLogs = logs, displayedTimeStamps = timeStamp}, Cmd.none)


        -- time stamp action 
        PickTime timeStamp -> ({ model | focusedTimeStamp = EverySet.insert timeStamp model.focusedTimeStamp }, Cmd.none)
        UnpickTime timeStamp -> ({ model | focusedTimeStamp = EverySet.remove timeStamp model.focusedTimeStamp}, Cmd.none)

        _ -> (model, Cmd.none)
        


-- TODO: may need to change the timeStamp 
changeSearchMode : SearchConfig -> Model -> Model
changeSearchMode config model = 
        { model | searchMode = config, 
                  displayedControllerLogs = searchFromControllerLogs config model.searchControlerTarget model.allControllerLogs, 
                  displayedPlayerLogs = searchFromPlayerLogs config model.searchControlerTarget model.allPlayerLogs }


-------------------------
---- File Function ----
-------------------------
swapPlayerFile : String -> String -> List PlayerLog 
swapPlayerFile player file  = []
-- read file 
-- send LoadNewPlayerLog message 


-------------------------
---- Search Function ----
-------------------------
hitSearchTarget : SearchConfig -> String -> String -> Bool 
hitSearchTarget {caseConfig, matchConfig} target text = 
    case (caseConfig, matchConfig) of
        (CaseSensitive, MatchWord) -> String.contains (target ++ " ") text 
        (CaseInsensitive, MatchWord) -> String.contains (String.toLower  target ++ " " ) (String.toLower text)
        (CaseSensitive, ContainsWord) -> String.contains target text 
        (CaseInsensitive, ContainsWord) -> String.contains (String.toLower target) (String.toLower target)


-- TODO: decides if we need to search by class or class name 
searchFromPlayerLogs: SearchConfig -> String -> List PlayerLog -> List PlayerLog 
searchFromPlayerLogs config keyword logs = 
    List.filter (\log -> hitSearchTarget config keyword log.content) logs


searchFromControllerLogs: SearchConfig -> String -> List ControllerLog -> List ControllerLog 
searchFromControllerLogs config keyword logs = 
    List.filter (\log -> hitSearchTarget config keyword log.content) logs


------------------------------
---- Filter Time Function ----
------------------------------
getPlayerLogsDuringSecond : TimeStamp -> List PlayerLog -> List PlayerLog
getPlayerLogsDuringSecond timeStamp logs = 
    List.filter ( \log -> isInRangedBySec timeStamp log.timeStamp) logs 

getControllerLogsDuringRange: TimeStamp -> TimeStamp -> List ControllerLog -> List ControllerLog 
getControllerLogsDuringRange start end logs =
    List.filter (\log -> isInrange start end log.timeStamp) logs 

getPlayerLogsDuringRange: TimeStamp -> TimeStamp -> List PlayerLog -> List PlayerLog
getPlayerLogsDuringRange start end logs =
    List.filter (\log -> isInrange start end log.timeStamp) logs 
getControllerLogsDuringSecond : TimeStamp -> List ControllerLog -> List ControllerLog
getControllerLogsDuringSecond timeStamp logs = 
    List.filter (\log -> isInRangedBySec timeStamp log.timeStamp) logs 

uniqueTimeLine : List ControllerLog -> List PlayerLog -> List TimeStamp 
uniqueTimeLine controloerLogs playerLogs = 
    let  controllerTimestamps = List.map extractControllerTimeStamp controloerLogs
         playerTimeStamps = List.map extractPlayerTimeStamp playerLogs
         timeSet = EverySet.fromList <| List.append controllerTimestamps playerTimeStamps
    in  List.sortWith compareTimeStamp (EverySet.toList timeSet)

extractControllerTimeStamp : ControllerLog -> TimeStamp 
extractControllerTimeStamp log = let timeStamp = log.timeStamp in  { timeStamp | miliSecond = 0 }


extractPlayerTimeStamp : PlayerLog -> TimeStamp 
extractPlayerTimeStamp log = let timeStamp = log.timeStamp in  { timeStamp | miliSecond = 0 }


-- TODO: consider the case when second can be greater than 60 fater added by 1 

