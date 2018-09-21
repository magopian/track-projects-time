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
    , editDate : String
    , editProjectName : String
    , editDescription : String
    , editTimeSpent : String
    }


type alias Entry =
    { name : String
    , description : String
    , timeSpent : Float
    , date : String
    }


init : ( Model, Cmd Msg )
init =
    ( { entries =
            [ { name = "track projects time"
              , description = "Start the project"
              , timeSpent = 0.5
              , date = "2018-09-21"
              }
            ]
      , zone = Time.utc
      , editDate = ""
      , editProjectName = ""
      , editDescription = ""
      , editTimeSpent = "1"
      }
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Task.perform NewTime Time.now
        ]
    )



---- UPDATE ----


type Msg
    = AdjustTimeZone Time.Zone
    | NewTime Time.Posix
    | UpdateDate String
    | UpdateProjectName String
    | UpdateDescription String
    | UpdateTimeSpent String
    | AddEntry
    | DeleteEntry Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        NewTime time ->
            ( { model | editDate = toDate time model.zone }, Cmd.none )

        UpdateDate date ->
            ( { model | editDate = date }, Cmd.none )

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
                    , timeSpent = timeSpent
                    , date = model.editDate
                    }
            in
            ( { model | entries = [ entry ] ++ model.entries }, Cmd.none )

        DeleteEntry index ->
            let
                newEntries =
                    List.take index model.entries ++ List.drop (index + 1) model.entries
            in
            ( { model | entries = newEntries }, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Time spent on projects" ]
        , Html.form
            [ Html.Events.onSubmit AddEntry ]
            [ Html.table [ Html.Attributes.style "width" "100%" ]
                [ Html.thead []
                    [ Html.th [] [ Html.text "Date" ]
                    , Html.th [] [ Html.text "Project name" ]
                    , Html.th [] [ Html.text "What was done" ]
                    , Html.th [] [ Html.text "How long did it take" ]
                    , Html.th [] [ Html.text "Actions" ]
                    ]
                , Html.tbody []
                    ([ Html.tr []
                        [ Html.td []
                            [ Html.input
                                [ Html.Attributes.type_ "date"
                                , Html.Events.onInput UpdateDate
                                , Html.Attributes.value model.editDate
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.input
                                [ Html.Attributes.type_ "text"
                                , Html.Events.onInput UpdateProjectName
                                , Html.Attributes.value model.editProjectName
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.textarea
                                [ Html.Events.onInput UpdateDescription
                                , Html.Attributes.value model.editDescription
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.input
                                [ Html.Attributes.type_ "number"
                                , Html.Attributes.step "0.25"
                                , Html.Attributes.min "0"
                                , Html.Events.onInput UpdateTimeSpent
                                , Html.Attributes.value model.editTimeSpent
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.input [ Html.Attributes.type_ "submit", Html.Attributes.value "Add this entry" ] []
                            ]
                        ]
                     ]
                        ++ (model.entries
                                |> List.indexedMap
                                    (\index entry ->
                                        Html.tr []
                                            [ Html.td [] [ Html.text entry.date ]
                                            , Html.td [] [ Html.text entry.name ]
                                            , Html.td [] [ Html.text entry.description ]
                                            , Html.td [] [ Html.text <| String.fromFloat entry.timeSpent ]
                                            , Html.td []
                                                [ Html.a
                                                    [ Html.Attributes.href "#"
                                                    , Html.Attributes.style "text-decoration" "none"
                                                    , Html.Attributes.style "font-size" "1.5em"
                                                    , Html.Attributes.style "color" "#F00"
                                                    , Html.Events.onClick <| DeleteEntry index
                                                    ]
                                                    [ Html.text "✗" ]
                                                ]
                                            ]
                                    )
                           )
                    )
                ]
            ]
        ]



---- UTILS ----


toDate : Time.Posix -> Time.Zone -> String
toDate time zone =
    let
        year =
            String.fromInt (Time.toYear zone time)

        month =
            stringFromMonth (Time.toMonth zone time)

        day =
            String.fromInt (Time.toDay zone time)
    in
    year ++ "-" ++ month ++ "-" ++ day


stringFromMonth : Time.Month -> String
stringFromMonth month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
