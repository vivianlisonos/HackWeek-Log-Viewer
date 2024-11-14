module View.ListItem exposing (..)
import Model.LogModel exposing (..)
import Html.Styled as Html exposing (..)
import Update.UserAction exposing (..)
import Css 
import Html.Styled.Attributes as Att 
import Html.Styled.Events
import Html.Styled.Keyed as KHtml 


item : TimeStamp -> List ControllerLog -> List PlayerLog -> Html Msg 
item timeline controllerLog playerLog =  
    Html.div 
    [Att.css 
        [Css.displayFlex,
         Css.alignItems Css.center,
         Css.width <| Css.vw 80,
         Css.margin2 (Css.vw 0) (Css.vw 10)]]
    [Html.div 
            [Att.css 
            [Css.width <| Css.vw 38]]
            [Html.text "hello"], -- TODO: should be a list of player logs
       
       Html.div 
        [Att.css 
            [Css.width <| Css.vw 4]]
            [Html.text "--"],    -- TODO: should be timestamp 

       Html.div 
        [Att.css 
            [Css.width <| Css.vw 38]]
            [Html.text "hello"]  -- TODO: should be a list of controller logs 
    ]


mainLayout : Html Msg -> Html Msg -> Html Msg -> Html Msg
mainLayout left right middle  = 
 Html.div 
    [Att.css 
        [Css.displayFlex ,
         Css.alignItems Css.stretch,
         Css.width <| Css.vw 98,
         Css.padding <| Css.px 30,
         Css.margin2 (Css.vw 0) (Css.vw 2)]]
    
    [Html.div 
        [Att.css 
        [Css.width <| Css.vw 42]]
        [left],
       
    Html.div 
        [Att.css 
        [Css.width <| Css.vw 10]]
        [middle],

    Html.div 
        [Att.css 
        [Css.width <| Css.vw 42]]
        [right]]