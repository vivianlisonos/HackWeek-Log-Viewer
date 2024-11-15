module LogParser.Timestamp exposing (androidTimeStampParser, iosTimeStampParser, playerTimeStampParser, unparseTimeStamp)

import Parser exposing(..)
import Model.TimeStampModel exposing (TimeStamp)
import LogParser.ParserUtil exposing(..)

-- TimeStamp Parser for android timestamp 
androidTimeStampParser: Parser TimeStamp
androidTimeStampParser = 
    succeed TimeStamp 
        |= yy
        |. hyphen
        |= mon 
        |. hyphen 
        |= dd
        |. spaces
        |= hh
        |. colon 
        |= min 
        |. colon 
        |= sec 
        |. comma 
        |= micro 
        
-- TimeStamp Parser for ios timestamp 
iosTimeStampParser: Parser TimeStamp
iosTimeStampParser = 
    succeed  TimeStamp 
        |= yy
        |. slash
        |= mon
        |. slash
        |= dd
        |. spaces 
        |= hh
        |. colon 
        |= min
        |. colon 
        |= sec
        |. colon 
        |= micro

playerTimeStampBuilder : Int -> Int -> Int -> Int -> Int -> Float -> TimeStamp
playerTimeStampBuilder y mo d h mi se = 
    let receiveFloat f total = let  secPart = floor total 
                                    microPart = ceiling ((total - (toFloat secPart)) * 1000)
                                in f secPart microPart
    in receiveFloat (TimeStamp y mo d h mi) se 

-- TimeStamp Parser for player timestamp 
playerTimeStampParser: Parser TimeStamp 
playerTimeStampParser = 
    succeed playerTimeStampBuilder 
        |. symbol "["
        |= yy 
        |. hyphen
        |= mon 
        |. hyphen 
        |= dd 
        |. symbol "T"
        |= hh
        |. colon 
        |= min 
        |. colon 
        |= float 
        |.chompUntilAndDrop "]"


        
-- Stringfy function 
unparseTimeStamp : TimeStamp -> String
unparseTimeStamp { year, month, day, hour, minute, second, miliSecond} = 
    String.join "/" (List.map String.fromInt  [year, month, day, hour, minute, second, miliSecond])


-- Helper function for smaller parser 
-- parser for separator 
-- parser for time symbol 
yy= int 
mon = flexibleInt
dd = flexibleInt
hh = flexibleInt
min = flexibleInt
sec  = flexibleInt
micro= flexibleInt 