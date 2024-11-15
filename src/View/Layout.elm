module View.Layout exposing (..)
import Model.LogModel exposing (..)
import Html.Styled as Html exposing (..)
import Update.UserAction exposing (..)
import Css 
import Html.Styled.Attributes as Att 



OIOLayout left right middle  = 
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


