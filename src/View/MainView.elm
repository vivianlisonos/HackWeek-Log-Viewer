module View.MainView exposing (..)
import View.TimeLine exposing (..)
import View.ListItem exposing (..)
import Model.LogModel exposing (..)
import Update.UserAction exposing (..)
import Css exposing (..)
import Html 
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing(..)

import Browser
import View.ReadFile exposing (subscriptions)
import View.LogItem exposing (playerLogsView, controllerLogsView)
import Html.Styled.Attributes as Att
import Html.Styled.Events exposing (onInput)
import View.LogItem exposing (displayedTimeString)
import EverySet exposing (EverySet)
main : Program Int Model Msg
main = Browser.element {init = init , view = view, update = update, subscriptions = subscriptions}


init : Int -> (Model, Cmd Msg)  
init flags = (defaultView, Cmd.none)



view: Model -> Html.Html Msg
view model = 
    Html.div [] [toUnstyled <| mainCard model] 



subscriptions : Model -> Sub Msg 
subscriptions model =
    Sub.none

mainCard: Model -> Html Msg 
mainCard model = 
    div [] [actionPanel model.searchPlayerTarget model.searchControlerTarget, 
            displayedLogsByTime model.allPlayerLogs model.allControllerLogs model.displayedTimeStamps model.focusedTimeStamp,
            displayedLogs model.displayedPlayerLogs model.displayedControllerLogs model.displayedTimeStamps,  
            logRepo model.allPlayerLogs model.allControllerLogs]

displayedLogsByTime: List PlayerLog -> List ControllerLog -> List TimeStamp -> EverySet TimeStamp -> Html Msg 
displayedLogsByTime playerLogs controllerLogs timeStamps selectedTimeStamp = 
    div [] []

displayedLogs: List PlayerLog -> List ControllerLog -> List TimeStamp ->  Html Msg
displayedLogs playerLogs controllerLogs timeStamps= 
    div []
    [ div [Att.height 50, Att.css [Css.width <| Css.vw 100, Css.textAlign Css.center, Css.padding2 (Css.px 50) (Css.px 10), Css.border (px 3) ]] [text "-------------------------- DISPLAYED LOG --------------------------"],
      mainLayout (playerSection playerLogs) (controllerSection controllerLogs) (timeDisplay timeStamps)]

logRepo: List PlayerLog -> List ControllerLog -> Html Msg 
logRepo playerLogs controllerLogs = 
    div [] 
    [ div [Att.height 50, Att.css [Css.width <| Css.vw 100, Css.textAlign Css.center, Css.padding2 (Css.px 50) (Css.px 10), Css.border (px 3) ]] [text "-------------------------- LOG REPOSITORY --------------------------"],
      mainLayout (playerSection playerLogs) (controllerSection controllerLogs) (div [] [text "---"])]


playerSection : List PlayerLog -> Html Msg 
playerSection playerSavedLogs = 
    div [] [
            text "all player logs from file", 
            playerLogsView playerSavedLogs
    ]

controllerSection : List ControllerLog -> Html Msg 
controllerSection controllerSavedLogs = 
    div [] [
            text "all controller logs from file", 
            controllerLogsView controllerSavedLogs
    ]


actionPanel:String -> String -> Html Msg 
actionPanel playerSearchKey controllerSearchKey = 
    mainLayout 
    (playerAction playerSearchKey)
    (controllerAction controllerSearchKey)
    (text "--")


playerAction: String -> Html Msg 
playerAction key = 
    div []
        [button [onClick SelectPlayerFile] [text "select player file"],
         div [] [input [Att.placeholder "enter search keyword", Att.value key, onInput SearchPlayerKey] [], 
                 button [onClick <| SearchFromPlayer key] [text "search"]]]


controllerAction: String -> Html Msg 
controllerAction key = 
    div [] 
        [button [onClick SelectIOSControllerFile] [text "select IOS controller file"],
        button [onClick SelectAndroidControllerFile] [text "select Android controller file"],
         div [] [input [Att.placeholder "enter search keyword", Att.value key, onInput SearchControllerKey] [],
                 button [onClick <| SearchFromController key] [text "search"]]]

timeDisplay : List TimeStamp -> Html Msg 
timeDisplay timeStampList = 
    div [] <| List.map (\time -> div [Att.css [Css.fontSize <| Css.px 8]] [text <| displayedTimeString time]) timeStampList