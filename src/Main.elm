module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html
import Html.Attributes
import Html.Events
import Task
import Time



---- MODEL ----


type alias Model =
    { entries : List Entry
    , zone : Time.Zone
    , editProjectName : String
    , editDescription : String
    , editTimeSpent : String
    }


type alias Entry =
    { name : String
    , description : String
    , timeSpent : Float
    , date : Time.Posix
    }


init : ( Model, Cmd Msg )
init =
    ( { entries =
            [ { name = "track projects time"
              , description = "Start the project"
              , timeSpent = 0.5
              , date = Time.millisToPosix 0
              }
            ]
      , zone = Time.utc
      , editProjectName = ""
      , editDescription = ""
      , editTimeSpent = "1"
      }
    , Task.perform AdjustTimeZone Time.here
    )



---- UPDATE ----


type Msg
    = AdjustTimeZone Time.Zone
    | UpdateProjectName String
    | UpdateDescription String
    | UpdateTimeSpent String
    | AddEntry


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        UpdateProjectName name ->
            ( { model | editProjectName = name }, Cmd.none )

        UpdateDescription description ->
            ( { model | editDescription = description }, Cmd.none )

        UpdateTimeSpent timeSpent ->
            ( { model | editTimeSpent = timeSpent }, Cmd.none )

        AddEntry ->
            let
                timeSpent =
                    model.editTimeSpent
                        |> String.toFloat
                        |> Maybe.withDefault 0

                entry =
                    { name = model.editProjectName
                    , description = model.editDescription
                    , timeSpent =
                        model.editTimeSpent
                            |> String.toFloat
                            |> Maybe.withDefault 0
                    , date = Time.millisToPosix 0
                    }
            in
            ( { model | entries = model.entries ++ [ entry ] }, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Time spent on projects" ]
        , Html.h3 [] [ Html.text "Add a new entry" ]
        , Html.form
            [ Html.Events.onSubmit AddEntry
            ]
            [ Html.label []
                [ Html.text "Project name"
                , Html.input
                    [ Html.Attributes.type_ "text"
                    , Html.Events.onInput UpdateProjectName
                    , Html.Attributes.value model.editProjectName
                    ]
                    []
                ]
            , Html.label
                []
                [ Html.text "What was done"
                , Html.input
                    [ Html.Attributes.type_ "text"
                    , Html.Events.onInput UpdateDescription
                    , Html.Attributes.value model.editDescription
                    ]
                    []
                ]
            , Html.label
                []
                [ Html.text "How many days?"
                , Html.input
                    [ Html.Attributes.type_ "number"
                    , Html.Attributes.step "0.25"
                    , Html.Attributes.min "0"
                    , Html.Events.onInput UpdateTimeSpent
                    , Html.Attributes.value model.editTimeSpent
                    ]
                    []
                ]
            , Html.input [ Html.Attributes.type_ "submit", Html.Attributes.value "Add this entry" ] []
            ]
        , Html.hr [] []
        , Html.table [ Html.Attributes.style "width" "100%" ]
            [ Html.thead []
                [ Html.th [] [ Html.text "When was it" ]
                , Html.th [] [ Html.text "Project name" ]
                , Html.th [] [ Html.text "What was done" ]
                , Html.th [] [ Html.text "How long did it take" ]
                ]
            , model.entries
                |> List.map
                    (\entry ->
                        Html.tr []
                            [ Html.td [] [ Html.text <| String.fromInt <| Time.posixToMillis entry.date ]
                            , Html.td [] [ Html.text entry.name ]
                            , Html.td [] [ Html.text entry.description ]
                            , Html.td [] [ Html.text <| String.fromFloat entry.timeSpent ]
                            ]
                    )
                |> Html.tbody []
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
