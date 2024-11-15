module Model.TimeStampModel exposing (..)

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
