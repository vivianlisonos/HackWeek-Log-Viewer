module View.FileControl exposing (..)

import Update.UserAction exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Attributes exposing (..)
import Set 

uploadedPlayerFileSection players = 
    div [class "player_file_list"] <| List.map text <| Set.toList players




hasPlayerLogNotification notification = 
    case notification of 
    Just message -> div [] [
        text message, 
        button [onClick <| ChangeViewMode SpliteView] [text "split"], 
        button [onClick <| ChangeViewMode CombineView] [text "interwined"], 
        button [onClick <| ChangeViewMode ControllerOnly] [text "controller only"], 
        button [onClick <| ChangeViewMode PlayerOnly][text "player only"]]
    Nothing -> div [] []