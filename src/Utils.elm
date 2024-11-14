module Utils exposing (..)


getOpt : b -> Maybe b -> b 
getOpt default tgt  = 
    case tgt of 
        Just value -> value 
        Nothing -> default


mapOpt default f tgt = 
    case tgt of 
        Just value -> f value 
        Nothing -> default