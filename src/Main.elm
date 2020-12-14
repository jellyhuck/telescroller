module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, a, button, div, input, li, span, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type EditModes
    = Edit
    | Tele


type PauseModes
    = Pause
    | Play


type alias Model =
    { editMode : EditModes
    , text : String
    , fontSize : String
    , speed : Int
    , offset : Float
    , pauseMode : PauseModes
    }


initialModel : Model
initialModel =
    { editMode = Tele
    , text = ""
    , fontSize = "24"
    , speed = 0
    , offset = 0.0
    , pauseMode = Pause
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( initialModel, Cmd.none )


type Msg
    = ChangeMode EditModes
    | UpdateText String
    | ChangeFontSize String
    | ChangeSpeed String
    | UpdateFrame Float
    | ChangePauseMode PauseModes
    | ResetTele

toTopAttribute : Float -> String
toTopAttribute off =
    (String.fromInt (floor off)) ++ "px"
calculateOffset : Model -> Float -> Float
calculateOffset model deltaFrames =
    model.offset + deltaFrames * (toFloat model.speed) / 1000
buttonClass t isOn =
    "btn-" ++ (if isOn == False then "outline-" else "") ++ t

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( case msg of
        ChangeMode mode ->
            { model | editMode = mode }
        UpdateText txt ->
            { model | text = txt }
        ChangeFontSize s  ->
            { model | fontSize = s }
        ChangeSpeed s ->
            { model | speed = (String.toInt s |> Maybe.withDefault 0) }
        UpdateFrame d ->
            case model.pauseMode of
                Pause -> model
                Play -> { model | offset = calculateOffset model d }
        ChangePauseMode m ->
            { model | pauseMode = m }
        ResetTele ->
            { model | offset = 0, pauseMode = Pause }
  , Cmd.none )


view : Model -> Html Msg
view model =
      div []
          [ renderNavs model
          , renderTextContent model
          ]


renderNavs : Model -> Html Msg
renderNavs model =
    ul [ class "nav nav-pills p-1" ]
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
             [ div [ class "input-group mx-1" ]
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
              [ div [ class "input-group mx-1" ]
                    [ div [ class "input-group-prepend" ]
                          [ span [ class "input-group-text" ]
                                 [ text " Speed: "]
                          ]
                    , input [ type_ "number"
                            , Html.Attributes.min "0"
                            , Html.Attributes.max "100"
                            , value (String.fromInt model.speed)
                            , onInput ChangeSpeed
                            ]
                            []
                    ]
              ]
          , button
              [ class ("btn mx-1 " ++
                       (buttonClass "primary" (model.pauseMode == Play)))
              , onClick (ChangePauseMode Play)
              ]
              [ text "Play" ]
          , button
              [ class ("btn mx-1 " ++
                       (buttonClass "secondary" (model.pauseMode == Pause)))
              , onClick (ChangePauseMode Pause)
              ]
              [ text "Pause" ]
          , button
              [ class ("btn mx-5 btn-sm" )
              , onClick (ResetTele)
              ]
              [ text "Reset" ]
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



subscriptions : Model -> Sub Msg
subscriptions model =
  Browser.Events.onAnimationFrameDelta UpdateFrame

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
