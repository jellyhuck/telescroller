port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, a, button, div, input, li, span, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, on)
import Json.Decode as Decode
import Json.Encode as Encode


type EditModes
    = Edit
    | Tele


type PauseModes
    = Pause
    | Play


type alias PersistentModel ext =
    { ext
    | text : String
    , fontSize : String
    , speed : Int
    , padding : String }
constructPersistentModel :
    (PersistentModel a) -> String -> String -> Int -> String
    -> (PersistentModel a)
constructPersistentModel a txt fs sp padd =
    { a | text = txt, fontSize = fs, speed = sp, padding = padd }

type alias Model = PersistentModel
    { editMode : EditModes
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
    , padding = "1"
    }

init : (Maybe String) -> (Model, Cmd msg)
init storedStateJson =
    ( decodePersistentModel
        initialModel (storedStateJson |> Maybe.withDefault "")
    , Cmd.none
    )


onWheel : (Maybe Float -> msg) -> Html.Attribute msg
onWheel a =
  on "wheel" (Decode.map a (Decode.maybe (Decode.at ["deltaY"] Decode.float)))

type Msg
    = ChangeEditMode EditModes
    | UpdateText String
    | ChangeFontSize String
    | ChangeSpeed String
    | UpdateFrame Float
    | ChangePauseMode PauseModes
    | ResetTele
    | ChangePadding String
    | MouseClick
    | MouseWheel (Maybe Float)

toTopAttribute : Float -> String
toTopAttribute off =
    (String.fromInt (floor off)) ++ "px"
calculateOffset : Model -> Float -> Float
calculateOffset model deltaFrames =
    model.offset - deltaFrames * (toFloat model.speed) / 1000
buttonClass t isOn =
    "btn-" ++ (if isOn == False then "outline-" else "") ++ t

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      ChangeEditMode mode ->
          ( { model | editMode = mode }, encodePersistentModel True model )
      UpdateText txt ->
          ( { model | text = txt }, Cmd.none )
      ChangeFontSize s ->
          ( { model | fontSize = s }, Cmd.none )
      ChangeSpeed s ->
          ( { model | speed = (String.toInt s |> Maybe.withDefault 0) }
          , Cmd.none
          )
      UpdateFrame d ->
          ( case model.pauseMode of
                Pause -> model
                Play -> { model | offset = calculateOffset model d }
          , Cmd.none
          )
      ChangePauseMode m ->
          ( { model | pauseMode = m }, encodePersistentModel False model )
      ResetTele ->
          ( { model | offset = 0, pauseMode = Pause }, Cmd.none )
      ChangePadding p ->
          ( { model | padding = p }, Cmd.none)
      MouseClick ->
          ( { model | pauseMode =
                (if model.pauseMode == Pause then Play else Pause) }
          , Cmd.none
          )
      MouseWheel mayBeDeltaY ->
          ( { model | offset =
                model.offset - (mayBeDeltaY |> Maybe.withDefault 0)}
            , Cmd.none
          )


view : Model -> Html Msg
view model =
      div []
          [ renderNavs model
          , renderTextContent model
          ]


renderNavs : Model -> Html Msg
renderNavs model =
    ul [ class "nav nav-pills p-1 ml-1" ]
        [ li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , onClick (ChangeEditMode Tele)
                , classList [ ( "active", model.editMode == Tele ) ]
                ]
                [ text "Tele" ]
            ]
        , li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , onClick (ChangeEditMode Edit)
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
          , li []
               [ div [ class "input-group mx-1" ]
                     [ div [ class "input-group-prepend" ]
                           [ span [ class "input-group-text" ]
                                  [ text " Margin: "]
                           ]
                     , input [ type_ "number"
                             , Html.Attributes.min "0"
                             , Html.Attributes.max "50"
                             , value model.padding
                             , onInput ChangePadding
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
            div [ class "container-fluid"
                , onClick MouseClick
                , onWheel MouseWheel
                ]
                [ div [ class "row flex-grow-1 h-100"
                      , style "min-width" "100%"
                      , style "overflow" "hidden"
                      , style "background-color" "black"
                      ]
                      [ textarea [ class "form-control"
                                 , style "font-size" model.fontSize
                                 , style "background-color" "black"
                                 , style "height" "90%"
                                 , style "color" "white"
                                 , style "border" "none"
                                 , style "overflow" "auto"
                                 , style "outline" "none"
                                 , style "resize" "none"
                                 , style "box-shadow" "none"
                                 , style "position" "relative"
                                 , style "margin-left" (model.padding ++ "px")
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


port storePersistentModel : String -> Cmd msg
encodePersistentModel : Bool -> PersistentModel model -> Cmd msg
encodePersistentModel includingText model =
    Encode.object [ ("fontSize", Encode.string model.fontSize)
                  , ("speed", Encode.int model.speed)
                  , ("padding", Encode.string model.padding)
                  , ("text", Encode.string
                                (if includingText then model.text else ""))
                  ]
    |> Encode.encode 0
    |> storePersistentModel
decodePersistentModel :
    (PersistentModel model) -> String -> (PersistentModel model)
decodePersistentModel model jsn =
    case (Decode.decodeString
             (Decode.map4
                 (constructPersistentModel model)
                 (Decode.at ["text"] Decode.string)
                 (Decode.at ["parameters", "fontSize"] Decode.string)
                 (Decode.at ["parameters", "speed"] Decode.int)
                 (Decode.at ["parameters", "padding"] Decode.string)
             )
             jsn
         ) of
        Ok modelResult -> modelResult
        _ -> (constructPersistentModel
                model "Press Edit to put your text here." "24" 0 "1")
