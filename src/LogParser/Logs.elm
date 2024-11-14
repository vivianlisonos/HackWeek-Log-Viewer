module LogParser.Logs exposing (..)
import Model.LogModel exposing (ControllerLog, PlayerLog)
import Parser exposing (..)
import LogParser.Timestamp exposing (..)
import LogParser.ParserUtil exposing (..)
import Set
import Model.LogModel exposing (TimeStamp)
import LogParser.Timestamp exposing (unparseTimeStamp)
import List exposing (concat)
import LogParser.LogParser exposing(..)

makeAndroidControllerLog : Model.LogModel.TimeStamp -> String -> String -> String -> ControllerLog
makeAndroidControllerLog timeStamp claz package = ControllerLog timeStamp package claz

parseAndroidControllerLog : String -> Parser ControllerLog 
parseAndroidControllerLog terminal = 
    succeed makeAndroidControllerLog
       |= androidTimeStampParser 
       |. spaces
       |= getStringInBracket
       |. chompUntilAndDrop "-"
       |. spaces 
       |= receiveUntilAndDrop ":"
       |= receiveUntil terminal


parseIOSControllerLog : String -> Parser ControllerLog 
parseIOSControllerLog  terminal = 
    succeed ControllerLog 
     |= iosTimeStampParser
     |. spaces 
     |= getStringInBracket
     |= getStringInBracket
     |. spaces
     |= receiveUntil terminal

--- TODO: needd a better way to decide the terminal symbol for player logs
parsePlayerLog : String -> Parser PlayerLog 
parsePlayerLog terminal = 
    succeed PlayerLog 
    |= playerTimeStampParser 
    |. spaces
    |= getStringInArrow
    |. spaces 
    |= receiveUntil terminal
    |. symbol terminal
    
unparseConstrollerLog : ControllerLog -> String 
unparseConstrollerLog {timeStamp, package, class, content} =
   String.join " "  [unparseTimeStamp timeStamp, package, class, content]


unparsePlayerLog : PlayerLog -> String 
unparsePlayerLog {timeStamp, playerAPI, content} = 
    String.join " " [unparseTimeStamp timeStamp, playerAPI, content]



androidFileParser: String -> Parser (List ControllerLog)
androidFileParser terminal = 
    loop [] (logFileParser parseAndroidControllerLog terminal)


iosFileParser: String -> Parser (List ControllerLog)
iosFileParser terminal = 
    loop [] (logFileParser parseIOSControllerLog terminal)


playerFileParser terminal = loop [] (logFileParser parsePlayerLog terminal)


parsePlayerFile: String -> String -> List PlayerLog
parsePlayerFile terminal content = 
    case run (playerFileParser terminal) content of 
    Result.Ok res -> res 
    Result.Err _ -> []


parseAndroidControllerFile : String -> String -> List ControllerLog
parseAndroidControllerFile terminal content = 
    case run (androidFileParser terminal) content of 
    Result.Ok res -> res 
    Result.Err _ -> [] 


parseIOSControllerFile = parseFile parseIOSControllerLog

logFileParser logparser terminal revLogs =  
    oneOf [
        succeed (\log -> Loop (log :: revLogs)) |= (logparser terminal),
        succeed () |> map (\_ -> Done (List.reverse revLogs))
    ]