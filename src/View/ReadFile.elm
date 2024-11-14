module View.ReadFile exposing (..)
import Html exposing (..)
import Browser
import File exposing (File)
import File.Select as Select 
import Task 
import Html.Events exposing (onClick)

main = Browser.element { init = init,  view = view, update = update, subscriptions = subscriptions }

main2 = Browser.element 
type alias Model = String 

init: Int -> (Model, Cmd Msg)
init flag = ("initial string", Cmd.none) 

type Msg = LoadString String 
         | LoadFile File
         | FileSelected 

update : Msg -> Model -> (Model , Cmd Msg) 
update msg model =
    case msg of
        FileSelected -> 
            ("loading file", Select.file ["playerLog.txt"] LoadFile)
        LoadFile filename -> 
            (model, Task.perform LoadString (File.toString filename))
        LoadString str ->
            (str, Cmd.none)


view : Model -> Html Msg 
view model = 
    div [] [text model, button [onClick FileSelected][text "viewFile"]]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none 





