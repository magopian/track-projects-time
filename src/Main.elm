module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Kinto
import Task



---- MODEL ----


type alias Model =
    { entries : List Entry
    , editDate : String
    , editProjectName : String
    , editDescription : String
    , editTimeSpent : String
    }


type alias Entry =
    { id : String
    , last_modified : Int
    , name : String
    , description : String
    , timeSpent : Float
    , date : String
    }


init : ( Model, Cmd Msg )
init =
    ( { entries =
            [ { id = "foobar"
              , last_modified = 0
              , name = "track projects time"
              , description = "Start the project"
              , timeSpent = 0.5
              , date = "2018-09-21"
              }
            ]
      , editDate = ""
      , editProjectName = ""
      , editDescription = ""
      , editTimeSpent = "1"
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateDate String
    | UpdateProjectName String
    | UpdateDescription String
    | UpdateTimeSpent String
    | AddEntry
    | EntryAdded (Result Kinto.Error Entry)
    | DeleteEntry Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

                data =
                    encodeData
                        model.editProjectName
                        model.editDescription
                        timeSpent
                        model.editDate
            in
            ( model
            , client
                |> Kinto.create recordResource data
                |> Kinto.send EntryAdded
            )

        EntryAdded (Ok entry) ->
            ( { model | entries = [ entry ] ++ model.entries }, Cmd.none )

        EntryAdded (Err err) ->
            let
                _ =
                    Debug.log "Error while adding the entry" err
            in
            ( model, Cmd.none )

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



---- DECODERS ----


decodeEntry : Decode.Decoder Entry
decodeEntry =
    Decode.map6 Entry
        (Decode.field "id" Decode.string)
        (Decode.field "last_modified" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "timeSpent" Decode.float)
        (Decode.field "date" Decode.string)


encodeData : String -> String -> Float -> String -> Encode.Value
encodeData name description timeSpent date =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "description", Encode.string description )
        , ( "timeSpent", Encode.float timeSpent )
        , ( "date", Encode.string date )
        ]


client : Kinto.Client
client =
    Kinto.client
        "https://kinto.agopian.info/v1/"
        (Kinto.Basic "test" "test")


recordResource : Kinto.Resource Entry
recordResource =
    Kinto.recordResource "default" "track-projects-time" decodeEntry



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
