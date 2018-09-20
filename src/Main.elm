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
    , time : Time.Posix
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
      , time = Time.millisToPosix 0
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
    | UpdateProjectName String
    | UpdateDescription String
    | UpdateTimeSpent String
    | AddEntry
    | AddingEntry Entry Time.Posix
    | DeleteEntry Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        NewTime time ->
            ( { model | time = time }, Cmd.none )

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
                    , date = Time.millisToPosix 0
                    }
            in
            -- Don't add the entry yet, ask for a time to timestamp it
            ( model, Task.perform (AddingEntry entry) Time.now )

        AddingEntry entry time ->
            let
                datedEntry =
                    { entry | date = time }
            in
            -- Add the entry now that it's timestamped
            ( { model | entries = [ datedEntry ] ++ model.entries }, Cmd.none )

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
                    [ Html.th [] [ Html.text "Timestamp" ]
                    , Html.th [] [ Html.text "Project name" ]
                    , Html.th [] [ Html.text "What was done" ]
                    , Html.th [] [ Html.text "How long did it take" ]
                    , Html.th [] [ Html.text "Actions" ]
                    ]
                , Html.tbody []
                    ([ Html.tr []
                        [ Html.td [] [] -- The date is a timestamp from the time the entry is added
                        , Html.td []
                            [ Html.input
                                [ Html.Attributes.type_ "text"
                                , Html.Events.onInput UpdateProjectName
                                , Html.Attributes.value model.editProjectName
                                , Html.Attributes.style "width" "90%"
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.textarea
                                [ Html.Events.onInput UpdateDescription
                                , Html.Attributes.value model.editDescription
                                , Html.Attributes.style "width" "100%"
                                , Html.Attributes.rows 2
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
                        , Html.input [ Html.Attributes.type_ "submit", Html.Attributes.value "Add this entry" ] []
                        ]
                     ]
                        ++ (model.entries
                                |> List.indexedMap
                                    (\index entry ->
                                        Html.tr []
                                            [ Html.td [] [ Html.text <| toDate entry.date model.zone ]
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

        hour =
            String.fromInt (Time.toHour zone time)

        minute =
            String.fromInt (Time.toMinute zone time)

        second =
            String.fromInt (Time.toSecond zone time)
    in
    year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second


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
