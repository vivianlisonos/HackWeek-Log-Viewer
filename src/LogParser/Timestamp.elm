module LogParser.Timestamp exposing (androidTimeStampParser, iosTimeStampParser, playerTimeStampParser, unparseTimeStamp, flexibleInt)

import Parser exposing(..)
import Model.LogModel exposing (TimeStamp)
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
playerTimeStampBuilder y mo d h mi se = receiveFloat (TimeStamp y mo d h mi) se 

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

receiveFloat : (Int -> Int -> TimeStamp) -> Float -> TimeStamp 
receiveFloat f total = 
    let secPart = floor total 
        microPart = ceiling ((total - (toFloat secPart)) * 1000)
    in f secPart microPart

        
-- Stringfy function 
unparseTimeStamp : TimeStamp -> String
unparseTimeStamp { year, month, day, hour, minute, second, miliSecond} = 
    String.join "/" (List.map String.fromInt  [year, month, day, hour, minute, second, miliSecond])



-- Helper function for smaller parser 
-- parser for separator 
flexibleInt = succeed identity |. chompWhile (\x -> x == '0') |= (oneOf [int, succeed 0])
slash = symbol "/"
colon = symbol ":"
hyphen = symbol "-"
period = symbol "."
comma = symbol ","

-- parser for time symbol 
yy= int 
mon = flexibleInt
dd = flexibleInt
hh = flexibleInt
min = flexibleInt
sec  = flexibleInt
micro= flexibleInt 