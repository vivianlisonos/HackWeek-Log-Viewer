module Update.UserAction exposing (..)
import Model.LogModel exposing (..)
import Model.TimeStampModel exposing (..)
import EverySet exposing (..)
import Dict
import Set 
import File exposing (File)
import Html.Attributes exposing (target)

type LogViewMode = JoinView | SplitView | PlayerOnly | ControllerOnly

type Msg = 
    --- action regarding search 
        -- controller log search
        InputControllerSearch String 
    |   AddControllerSearchKey String  
    |   RemoveControllerSearchKey String 
    |   ToggleControllyerCaseSensitivity

        -- player log search 
    |   InputPlayerSearch String 
    |   AddPlayerSearchKey String 
    |   RemovePlayerSearchKey String 
    |   TogglePlayerCaseSensitivity  

    --- action regarding filter timestamp 
    |  SearchTimeStamp TimeStamp TimeStamp 
    |  PickStartTime TimeStamp 
    |  PickEndTime TimeStamp
    |  ShowAllController 
    |  ShowAllPlayer 

    --- action regarding uploading file 
    |  SelectControllerFile
    |  SelectPlayerFile
    |  UploadControllerFile File 
    |  UploadPlayerFile File 
    |  RemovePlayerLog String 
    |  ChooseDisplayedPlayer String Bool 
    |  UpdateControllerLogRepo (List LogData)
    |  UpdatePlayerLogRepo (String, List LogData)

    --- action regarding player log 
    | ChangeViewMode LogViewMode



--------------------------------
---- Search by Key Function ----
--------------------------------
hitSearchTarget : SearchConfig -> String -> String -> Bool 
hitSearchTarget (caseConfig, matchConfig) target text = 
    case (caseConfig, matchConfig) of
         (CaseSensitive, _) -> String.contains target text 
         (CaseInsensitive, _) -> String.contains (String.toLower target) (String.toLower text)

hitSearchTargets : SearchConfig -> Set.Set String -> String -> Bool 
hitSearchTargets (caseConfig, matchConfig) targets text = 
    case (caseConfig, matchConfig) of
         (CaseSensitive, _) -> List.all (\tgt -> String.contains tgt text) <| Set.toList targets
         (CaseInsensitive, _) -> 
            let toLowTarget = Set.toList <| Set.map String.toLower targets
                toLowText = String.toLower text  in 
            List.all (\tgt -> String.contains tgt toLowText) toLowTarget


searchLogData searchConfig key logs = List.filter (\log -> hitSearchTarget searchConfig key log.content) logs 

filterLogDataByKeys searchConfig keys logs =  List.filter (\log -> hitSearchTargets searchConfig keys log.content) logs 


-------------------------------------
---- Search by Timeline Function ----
-------------------------------------
uniqueTimeLineTillSec : List LogData -> List LogData -> List TimeStamp 
uniqueTimeLineTillSec controloerLogs playerLogs = 
    let  controllerTimestamps = List.map extractTimeStampTillSec controloerLogs
         playerTimeStamps = List.map extractTimeStampTillSec playerLogs
         timeSet = EverySet.fromList <| List.append controllerTimestamps playerTimeStamps
    in  List.sortWith compareTimeStamp (EverySet.toList timeSet)

extractTimeStampTillSec : LogData -> TimeStamp 
extractTimeStampTillSec log = let timeStamp = log.timeStamp in  { timeStamp | miliSecond = 0 }
getLogDataDuringRange start end logs = List.filter (\log -> isInrange start end log.timeStamp) logs 



---------------------------------------------
---- Search by Key and Timeline Function ----
---------------------------------------------
filterControllerLogByKeyAndTime searchLogConfig keys (start, end) logs = 
    getLogDataDuringRange start end <| filterLogDataByKeys searchLogConfig keys logs 

filterPlayerLogByKeyAndTime searchLogConfig keys (start, end) selectedPlayers playerLogRepo = 
    let allPlayerLogs = List.concat <| Dict.values <| Dict.filter (\key _ -> Set.member key selectedPlayers) playerLogRepo
        filteredLogs = getLogDataDuringRange start end <|  filterLogDataByKeys searchLogConfig keys allPlayerLogs
    in List.sortWith (\a b -> compareTimeStamp a.timeStamp b.timeStamp) filteredLogs
