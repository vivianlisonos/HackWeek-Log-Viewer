module TimeParserTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parser exposing (..)
import LogParser.Timestamp exposing (..)
import LogParser.Logs exposing (..)
import LogParser.ParserUtil exposing (..)
import Model.LogModel exposing (TimeStamp)
import LogParser.LogParser exposing (..)
import Model.LogModel exposing (..)

suite : Test
suite = describe "The String module"
        [ describe "Int parser"
          [ test "test flexible int parser 000" <|
            \_ -> testParser String.fromInt flexibleInt "000" "0",
            test "test flexible int parser 0" <|
            \_ -> testParser String.fromInt flexibleInt "0" "0",
            test "test flexible int parser 001" <|
            \_ -> testParser String.fromInt flexibleInt "001" "1",
            test "test flexible int parser 1" <|
            \_ -> testParser String.fromInt flexibleInt "1" "1"
            ] ,
          
          describe "IOS time parser"  
            [ test "test ios parser succeeds" <|
                \_ -> testTimeStampParser iosTimeStampParser "2024/11/05 01:02:27:708" "2024/11/5/1/2/27/708", 
              test "test ios parser fails" <|
                \_ -> testTimeStampParser  iosTimeStampParser "2024/11 01:02:27:708" "err"
            ], 

            describe "Player time parser" 
           [ 
              test "test player parser succeeds" <|
                \_ -> testTimeStampParser playerTimeStampParser "[1970-01-01T00:00:08.772Z | 0000008772]"  "1970/1/1/0/0/0/0",
              test "test player parser fails" <|
                \_ -> testTimeStampParser playerTimeStampParser "[2024-11-0123:08:41.598Z  | 0000010240]"  "err"
            ],
            
            describe "Android time parser" 
            [ test "test android parser succeeds" <|
                \_ -> testTimeStampParser androidTimeStampParser "2024-11-06 12:35:50,304"  "2024/11/6/12/35/50/304", 
              test "test android parser fail" <|
                \_ -> testTimeStampParser androidTimeStampParser "2024-11-06 12:35:50304"  "err"
            ],

          describe "Test Parser Util" 
            [ test "test receiveUntil" <|
              \_ -> testStringParser (wrappedArround ";" ";")  ";hello;" "hello",
              test "test string in bracket" <|
              \_ -> testStringParser getStringInBracket "[hello]" "hello",
              test "drop string" <|
              \_ -> Expect.equal (trimStartnAndEnd "[dada]") "dada"],

          describe "Test Parser Individual Log"
            [ test "test parser for ios controller log" <| 
               \_ -> 
               testControllerLogParser (parseIOSControllerLog  "\n")
               "2024/10/27 14:55:16:120 [com.sonos.setup.sdk][SetupSDK] set systemID = \"Sonos_hITqGYpGOiojLqeRrJSG9xnFxP.cpDyVKVHTZN60oTVCO8F\"\n" 
               "2024/10/27/14/55/16/120 com.sonos.setup.sdk SetupSDK set systemID = \"Sonos_hITqGYpGOiojLqeRrJSG9xnFxP.cpDyVKVHTZN60oTVCO8F\"",

               test "test parser for android controller log" <|
               \_ -> 
               testControllerLogParser (parseAndroidControllerLog "\n")
               "2024-11-06 12:35:59,038 [Timer-3] ERROR SLog - com.sonos.sdk.utils: Got unexpected channel: LTR\n"
               "2024/11/6/12/35/59/38 com.sonos.sdk.utils Timer-3  Got unexpected channel: LTR",

               test "test player log" <|
               \_ -> 
               testPlayerLogParser (parsePlayerLog "\n")
               "[1970-01-01T00:00:08.772Z | 0000008772] <chsnk,4>  SNTP waiting for valid\n"
               "1970/1/1/0/0/0/0 chsnk,4 SNTP waiting for valid"
            ],
          describe "Test Parse log files" 
            [ test "test parser for android logs" <|
              \_ -> testControllerLogFileParser (androidFileParser "2024")
                 (String.join "\n" 
                 ["2024-11-06 12:35:58,961 [DefaultDispatcher-worker-31] ERROR SLog - com.sonos.sdk.musetransport: Error subscribing to household(Sonos_sudm0DGiogo2SGYniWIXxkQpOa.ngWZHzpQORs3TImddG6U).upnpAlarmClock: com.sonos.sdk.musetransport.MuseTransportError$NoConnection",
                  "2024-11-06 12:35:59,033 [Timer-3] ERROR SLog - com.sonos.sdk.client: Group settle timer fired without settling group change.",
                  "2024-11-06 12:35:59,038 [Timer-3] ERROR SLog - com.sonos.sdk.utils: Got unexpected channel: LTR"])
                (String.join "\n"
                  ["2024/11/6/12/35/58/961 com.sonos.sdk.musetransport DefaultDispatcher-worker-31  Error subscribing to household(Sonos_sudm0DGiogo2SGYniWIXxkQpOa.ngWZHzpQORs3TImddG6U).upnpAlarmClock: com.sonos.sdk.musetransport.MuseTransportError$NoConnection",
                  "2024/11/6/12/35/59/33 com.sonos.sdk.client Timer-3  Group settle timer fired without settling group change.",
                  "2024/11/6/12/35/59/38 com.sonos.sdk.utils Timer-3  Got unexpected channel: LTR"]),
              test "test parser for ios logs" <|
              \_ -> testControllerLogFileParser (iosFileParser "2024")
                 (String.join "\n" 
                 ["2024/10/27 14:55:16:121  [com.sonos.setup.sdk][SetupSDK] [MuseClientInterface] start subscription for zones event",
                  "2024/10/27 14:55:16:121  [com.sonos.setup.sdk][SetupSDK] [MuseClientInterface] start subscription for zones event",
                  "2024/10/27 14:55:16:148  [com.sonos.setup.sdk][SetupSDK] [NetworkPermissionsHelper] Has LAN permission"])
                (String.join "\n"
                  ["2024/10/27/14/55/16/121 com.sonos.setup.sdk SetupSDK [MuseClientInterface] start subscription for zones event",
                  "2024/10/27/14/55/16/121 com.sonos.setup.sdk SetupSDK [MuseClientInterface] start subscription for zones event",
                  "2024/10/27/14/55/16/148 com.sonos.setup.sdk SetupSDK [NetworkPermissionsHelper] Has LAN permission"]),
                test "test parser for player logs" <|
                \_ -> testPlayerLogFileParser (playerFileParser "\n")
                (String.join "\n"
                [
                  "[1970-01-01T00:00:08.285Z | 0000008285] <powCrdLib,4> CONNECTED anacapa|2802|1|/opt/bin/anacapad) C [8285] [1970-01-01 00:00:08.285]",
                  "[1970-01-01T00:00:08.302Z | 0000008302] <mod_zp,3> Version: 82.2-58290",
                  "[1970-01-01T00:00:08.424Z | 0000008424] <alsa_out,4> sample rate: 44100 channel count: 4 format: 10\n"
                ])
                 (String.join "\n"
                [
                 "1970/1/1/0/0/0/0 powCrdLib,4 CONNECTED anacapa|2802|1|/opt/bin/anacapad) C [8285] [1970-01-01 00:00:08.285]",
                 "1970/1/1/0/0/0/0 mod_zp,3 Version: 82.2-58290",
                 "1970/1/1/0/0/0/0 alsa_out,4 sample rate: 44100 channel count: 4 format: 10"
                ])
            ],
            
            describe "Test time comparison function"
            [ test "test timestamp comparison" <| 
              \_-> Expect.equal True <| isInRangedBySec (TimeStamp 1 1 1 1 1 1 0) (TimeStamp 1 1 1 1 1 1 100)],
            
            describe "Test time parser"
              [test "test " <| 
              \_ -> testParser .content (iosParser "\n" ) "2024/11/13 14:07:09:763  [com.sonos.AccessoryCore][ACSDK/AccessorySystemCache] App entering background: flushing accessory cache"
                                                                               "  [com.sonos.AccessoryCore][ACSDK/AccessorySystemCache] App entering background: flushing accessory cache"],
            
            describe "Test parse ios file"
              [test "test parse ios file" <|
                \_ -> 
                Expect.equal  (unparseListLogData <| (parseIOSFile "2024") 
                    (String.join "\n" 
                 ["2024/10/27 14:55:16:121  [com.sonos.setup.sdk][SetupSDK] [MuseClientInterface] start subscription for zones event",
                  "2024/10/27 14:55:16:121  [com.sonos.setup.sdk][SetupSDK] [MuseClientInterface] start subscription for zones event",
                  "2024/10/27 14:55:16:148  [com.sonos.setup.sdk][SetupSDK] [NetworkPermissionsHelper] Has LAN permission\n"]))
                  " "
                ]
        ]



unparseLogData log = log.content 
unparseListLogData : List LogData -> String
unparseListLogData logs = String.join "\n"  <| List.map unparseLogData logs 

testStringParser  = testParser identity
testTimeStampParser = testParser unparseTimeStamp
testControllerLogParser = testParser unparseConstrollerLog 
testControllerLogFileParser = testParser ((List.map unparseConstrollerLog) >> (String.join ""))
testPlayerLogFileParser = testParser ((List.map unparsePlayerLog) >> (String.join "\n"))
testPlayerLogParser = testParser unparsePlayerLog 

testParser unparser parser  orig expected  
  = case (run parser orig) of 
    Result.Ok res -> Expect.equal (unparser res) expected
    Result.Err _ -> Expect.equal "err" expected