module Model.LogModel exposing(..)
import Model.TimeStampModel exposing (..)
import EverySet exposing (..)

------ Time data type 

--------- Log data type 
type LogSource = Player String | Controller
type alias LogData = { timeStamp: TimeStamp, content: String, origin: LogSource }

makePlayerLog playerID time content = 
    let simplified = String.replace "<![CDATA[>]]>" "]" <| String.replace "<![CDATA[<]]>" "[" content 
    in LogData time simplified <| Player playerID
makeControllerLog time content =  LogData time content Controller

type CaseConfig = CaseSensitive | CaseInsensitive 
type MatchConfig = MatchWord | ContainsWord 
type alias SearchConfig = (CaseConfig, MatchConfig)

