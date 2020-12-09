module Main exposing (main)

import Browser
import Html exposing (Html, a, div, input, li, span, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type EditModes
    = Edit
    | Tele


type alias Model =
    { editMode : EditModes
    , text : String
    , fontSize : String
    , speed : Int
    , offset : Int
    }


initialModel : Model
initialModel =
    { editMode = Tele
    , text = ""
    , fontSize = "24"
    , speed = 0
    , offset = 0 }


type Msg
    = ChangeMode EditModes
    | UpdateText String
    | ChangeFontSize String
    | ChangeSpeed String

toTopAttribute : Int -> String
toTopAttribute off =
    (String.fromInt off) ++ "px"

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeMode mode ->
            { model | editMode = mode }
        UpdateText txt ->
            { model | text = txt }
        ChangeFontSize s  ->
            { model | fontSize = s }
        ChangeSpeed s ->
            { model | speed = (String.toInt s |> Maybe.withDefault 0) }


view : Model -> Html Msg
view model =
      div []
          [ renderNavs model
          , renderTextContent model
          ]


renderNavs : Model -> Html Msg
renderNavs model =
    ul [ class "nav nav-pills" ]
        [ li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , onClick (ChangeMode Tele)
                , classList [ ( "active", model.editMode == Tele ) ]
                ]
                [ text "Tele" ]
            ]
        , li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , onClick (ChangeMode Edit)
                , classList [ ( "active", model.editMode == Edit ) ]
                ]
                [ text "Edit" ]
            ]
        , li []
             [ div [ class "input-group" ]
                   [ div [ class "input-group-prepend" ]
                     [ span [ class "input-group-text" ]
                            [ text " Font Size: "]
                     ]
                   , input [ type_ "number"
                           , Html.Attributes.min "1"
                           , Html.Attributes.max "120"
                           , value model.fontSize
                           , onInput ChangeFontSize
                           ]
                           []
                   ]
             ]
         , li []
              [ div [ class "input-group" ]
                    [ div [ class "input-group-prepend" ]
                          [ span [ class "input-group-text" ]
                                 [ text " Speed: "]
                          ]
                    , input [ type_ "number"
                            , Html.Attributes.min "1"
                            , Html.Attributes.max "100"
                            , value (String.fromInt model.speed)
                            , onInput ChangeSpeed
                            ]
                            []
                    ]
              ]
        ]


renderTextContent : Model -> Html Msg
renderTextContent model =
    case model.editMode of
        Tele ->
            div [ class "container-fluid" ]
                [ div [ class "row flex-grow-1 h-100"
                      , style "min-width" "100%"
                      , style "background-color" "black"
                      ]
                      [ textarea [ class "form-control"
                                 , style "font-size" model.fontSize
                                 , style "background-color" "black"
                                 , style "height" "90%"
                                 , style "color" "white"
                                 , style "border" "none"
                                 , style "position" "relative"
                                 , style "top" (toTopAttribute model.offset)
                                 , readonly True
                                 ]
                                 [ text model.text ]
                      ]
                ]

        Edit ->
            div []
                [ textarea
                           [ class "form-control"
                           , style "min-width" "100%"
                           , style "height" "90%"
                           , onInput UpdateText
                           ]
                           [ text model.text ]
                ]

main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
