module Update.UserAction exposing (..)
import Model.LogModel exposing (..)
import Model.TimeStampModel exposing (..)
import EverySet exposing (..)
import Dict
import File exposing (File)

type LogViewMode = CombineView | SpliteView | PlayerOnly | ControllerOnly

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



--------------------------------
---- Search by Key Function ----
--------------------------------
hitSearchTarget : SearchConfig -> String -> String -> Bool 
hitSearchTarget {caseConfig, matchConfig} target text = 
    case (caseConfig, matchConfig) of
        (CaseSensitive, MatchWord) -> String.contains (target ++ " ") text 
        (CaseInsensitive, MatchWord) -> String.contains (String.toLower  target ++ " " ) (String.toLower text)
        (CaseSensitive, ContainsWord) -> String.contains target text 
        (CaseInsensitive, ContainsWord) -> String.contains (String.toLower target) (String.toLower target)

searchLogData searchConfig key logs = List.filter (\log -> hitSearchTarget searchConfig key log.content) logs 


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
filterControllerLogByKeyAndTime searchLogConfig key (start, end) logs = 
    getLogDataDuringRange start end <| searchLogData searchLogConfig key logs 

filterPlayerLogByKeyAndTime searchLogConfig key (start, end) logs = 
    let allPlayerLogs = List.concat <| Dict.values logs 
        filteredLogs = getLogDataDuringRange start end <|  searchLogData searchLogConfig key allPlayerLogs
    in List.sortWith (\a b -> compareTimeStamp a.timeStamp b.timeStamp) filteredLogs
