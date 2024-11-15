module LogParser.LogParser exposing (..)
import Parser exposing (..)
import Model.LogModel as LM
import Model.TimeStampModel as TM
import LogParser.Timestamp exposing (androidTimeStampParser)
import LogParser.ParserUtil exposing (receiveUntil)
import LogParser.Timestamp exposing (iosTimeStampParser)
import LogParser.Timestamp exposing (playerTimeStampParser)
import LogParser.ParserUtil exposing (receiveRest)

androidParser : String -> Parser LM.LogData
androidParser terminal = 
    succeed LM.makeControllerLog
    |= androidTimeStampParser
    |= receiveUntil terminal

iosParser : String -> Parser LM.LogData 
iosParser terminal = 
    succeed LM.makeControllerLog 
    |= iosTimeStampParser 
    |= receiveUntil terminal 

---- TODO: player parser will need to be improved to cover differen types of log 
playerParser : String -> Parser LM.LogData
playerParser playerName  = 
    succeed (LM.makePlayerLog playerName)
    |= playerTimeStampParser
    |= receiveRest 


logFileParser logparser terminal revLogs =  
    oneOf [
        succeed (\log -> Loop (log :: revLogs)) |= (logparser terminal),
        succeed () |> map (\_ -> Done (List.reverse revLogs))
    ]


parseFile itemParser  terminal  fileContent = 
    let fileParser = loop []  (logFileParser itemParser terminal) in 
    case run fileParser fileContent of 
    Result.Ok res -> res 
    Result.Err _ -> []


parseIOSFile : String -> String -> List LM.LogData
parseIOSFile = parseFile iosParser

parseAndroidFile : String -> String -> List LM.LogData 
parseAndroidFile = parseFile androidParser

-- TODO: fix this to not using "\n"
parseOneLineLog playerName log = 
    case run (playerParser playerName) <| log ++ "\n" of 
    Result.Ok res -> Just res
    Result.Err _ -> Nothing 


--- parse a raw file 
parsePlayerRawFile : String -> String -> List LM.LogData 
parsePlayerRawFile plyaerName content  = 
    let logList =  String.split "\n" content 
        parsedLogs = List.map (parseOneLineLog plyaerName) logList 
    in  List.sortWith (\a b -> TM.compareTimeStamp a.timeStamp b.timeStamp) (List.filterMap identity parsedLogs)
