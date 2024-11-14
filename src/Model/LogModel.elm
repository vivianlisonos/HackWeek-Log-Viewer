module Model.LogModel exposing(..)
import EverySet exposing (..)



type alias Model =
    { 
    ---viewData for timeStamp 
      focusedTimeStamp: EverySet TimeStamp
    , displayedTimeStamps: List TimeStamp

    --viewData for logs
    , displayedPlayerLogs: List PlayerLog
    , displayedControllerLogs: List ControllerLog
    , allPlayerLogs: List PlayerLog 
    , allControllerLogs: List ControllerLog 

    -- viewData for files
    , selectedPlayerFile: Maybe String 
    , selectedPlayer: Maybe String
    , players : List String 
    , playerFiles : List String

    -- viewData about searching 
    , searchMode: SearchConfig
    , searchPlayerTarget: String 
    , searchControlerTarget: String 
    }

defaultView = {
    focusedTimeStamp = EverySet.empty,
    displayedTimeStamps = [],
    displayedPlayerLogs = [],
    displayedControllerLogs = [] ,
    allPlayerLogs = [],
    allControllerLogs = [],

    -- viewData for files
    selectedPlayerFile = Nothing ,
    selectedPlayer = Nothing ,
    players = [],
    playerFiles = [],

    -- viewData about searching 
    searchMode = SearchConfig CaseSensitive ContainsWord,
    searchPlayerTarget = "",
    searchControlerTarget = "" }

type alias PlayerLog = {   
        timeStamp: TimeStamp,
        playerAPI: String, 
        content: String
        }


type alias ControllerLog = {
    timeStamp: TimeStamp, 
    package: String, 
    class: String, 
    content: String
    }

------ Time data type 
defaultTime: TimeStamp 
defaultTime =  TimeStamp 0 0 0 0 0 0 0

type alias  TimeStamp =
    { year : Int 
    , month: Int 
    , day : Int 
    , hour: Int
    , minute: Int
    , second: Int 
    , miliSecond: Int 
    }

isInRangedBySec : TimeStamp -> TimeStamp -> Bool 
isInRangedBySec range time = 
    isInrange range  {range | second = range.second + 1} time 

isInrange : TimeStamp -> TimeStamp -> TimeStamp -> Bool 
isInrange earlyLimit lateLimit time= 
    sametime earlyLimit time || 
    sametime lateLimit time || 
    (aEarliearThanB earlyLimit time && aEarliearThanB time lateLimit)

-- TODO: this is super ugly. clean this up !!!!!!!1
aEarliearThanB : TimeStamp -> TimeStamp -> Bool 
aEarliearThanB timeA timeB = 
    timeA.year < timeB.year || 
    (timeA.year == timeA.year && 
        (timeA.month < timeB.month || 
        (timeA.month ==  timeB.month &&
            (timeA.day < timeB.day || 
            (timeA.day == timeB.day &&  
                (timeA.hour < timeB.hour ||
                (timeA.hour == timeB.hour &&
                    (timeA.minute < timeB.minute || 
                    (timeA.minute == timeB.minute &&   
                        (timeA.second < timeB.second || 
                        (timeA.second == timeB.second &&  timeA.miliSecond < timeB.miliSecond)))))))))))

sametime : TimeStamp -> TimeStamp -> Bool 
sametime timeA timeB = 
    timeA.year == timeB.year &&  
    timeA.month == timeB.month &&  
    timeA.day == timeB.day &&  
    timeA.hour == timeB.hour &&  
    timeA.minute == timeB.minute &&  
    timeA.second == timeB.second &&  
    timeA.miliSecond == timeB.miliSecond

compareTimeStamp: TimeStamp -> TimeStamp -> Order 
compareTimeStamp timea timeb = 
    if sametime timea timeb then EQ else if aEarliearThanB timea timeb then LT else GT


--------- Log data type 
type LogSource = Player String | Controller
type alias LogData = { timeStamp: TimeStamp, content: String, origin: LogSource }

makePlayerLog playerID time content = 
    let simplified = String.replace "<![CDATA[>]]>" "]" <| String.replace "<![CDATA[<]]>" "[" content 
    in LogData time simplified <| Player playerID
makeControllerLog time content =  LogData time content Controller


---- TODO: this probably could be moded to main 

type CaseConfig = CaseSensitive | CaseInsensitive 
type MatchConfig = MatchWord | ContainsWord 
type alias SearchConfig = { caseConfig: CaseConfig,  matchConfig: MatchConfig }

