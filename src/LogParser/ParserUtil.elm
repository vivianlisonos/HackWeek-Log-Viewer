module LogParser.ParserUtil exposing (..)
import Parser exposing (..)


receiveRest: Parser String 
receiveRest = 
    getChompedString <| 
        succeed () |. chompWhile (\ _ -> True)

receiveUntil : String -> Parser String 
receiveUntil terminal = 
    getChompedString <| 
        succeed () 
            |. chompUntilEndOr terminal 

receiveUntilAndDrop : String -> Parser String 
receiveUntilAndDrop terminal = 
    receiveUntil terminal |. symbol terminal

chompUntilAndDrop : String -> Parser ()
chompUntilAndDrop terminal = 
    chompUntil terminal |. symbol terminal

wrappedArround : String -> String -> Parser String 
wrappedArround start terminal = 
    succeed identity  
     |. symbol start 
     |= receiveUntilAndDrop terminal

getStringInBracket = wrappedArround "[" "]"
getStringInParenthesis = wrappedArround "[" "]"
getStringInCurly = wrappedArround "{" "}"
getStringInArrow  = wrappedArround "<" ">"

--- helper function for smaller parser 

flexibleInt = succeed identity |. chompWhile (\x -> x == '0') |= (oneOf [int, succeed 0])
slash = symbol "/"
colon = symbol ":"
hyphen = symbol "-"
period = symbol "."
comma = symbol ","
