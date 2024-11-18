module View.FileControl exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes as A
import Html.Styled.Events exposing (..)
import Set
import Update.UserAction exposing (..)


uploadedPlayers allPlayers selectedPlayers =
    div [ A.class "player_file_list" ] <|
        case allPlayers of
            [] ->
                []

            ps ->
                h3 [] [ text "players:" ] :: List.map (playerItem selectedPlayers) ps


playerItem selectedPlayers playerId =
    strong [ A.class "uploaded_player_container" ]
        [ text playerId
        , input [ A.type_ "checkbox", onCheck <| ChooseDisplayedPlayer playerId, A.checked <| Set.member playerId selectedPlayers, A.class "uploaded_player_checkbox" ] []
        , button [ A.class "uploaded_player_button", onClick <| RemovePlayerLog playerId ] [ text "˟" ]
        ]


controlViewMode currentViewModel =
    let
        splitView =
            button [ onClick <| ChangeViewMode SplitView, A.classList [ ( "selected_view_mode", currentViewModel == SplitView ) ] ] [ text "☷" ]

        joinedView =
            button [ onClick <| ChangeViewMode JoinView, A.classList [ ( "selected_view_mode", currentViewModel == JoinView ) ] ] [ text "☰" ]

        setControllerOnly =
            button [ onClick <| ChangeViewMode ControllerOnly, A.classList [ ( "selected_view_mode", currentViewModel == ControllerOnly ) ] ] [ text "controller" ]

        setPlayerOnly =
            button [ onClick <| ChangeViewMode PlayerOnly, A.classList [ ( "selected_view_mode", currentViewModel == PlayerOnly ) ] ] [ text "player" ]
    in
    div [ A.class "view_mode_control" ]
        [ h3 [ A.class "view_mode_label" ]
             [ text "view option"],
          div [] [
             joinedView
            , splitView
            , setControllerOnly
            , setPlayerOnly
          ]
        ]
