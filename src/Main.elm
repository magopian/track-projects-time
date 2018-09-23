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
    , page : Page
    , serverURL : String
    , username : String
    , password : String
    }


type Page
    = LoginForm
    | LoggedIn Kinto.Client


init : ( Model, Cmd Msg )
init =
    ( { entries = []
      , editDate = ""
      , editProjectName = ""
      , editDescription = ""
      , editTimeSpent = "1"
      , serverURL = ""
      , username = ""
      , password = ""
      , page = LoginForm
      }
      -- , getEntryList client
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
    | DeleteEntry String
    | EntryDeleted (Result Kinto.Error DeletedEntry)
    | EntriesFetched (Result Kinto.Error (Kinto.Pager Entry))
    | UpdateServerURL String
    | UpdateUsername String
    | UpdatePassword String
    | UseLogin


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
            , case model.page of
                LoggedIn client ->
                    client
                        |> Kinto.create recordResource data
                        |> Kinto.send EntryAdded

                LoginForm ->
                    let
                        _ =
                            Debug.todo "No client!"
                    in
                    Cmd.none
            )

        EntryAdded (Ok entry) ->
            let
                entries =
                    [ entry ]
                        ++ model.entries
                        |> List.sortBy .date
                        |> List.reverse
            in
            ( { model | entries = entries }, Cmd.none )

        EntryAdded (Err err) ->
            let
                _ =
                    Debug.log "Error while adding the entry" err
            in
            ( model, Cmd.none )

        DeleteEntry entryID ->
            ( model
            , case model.page of
                LoggedIn client ->
                    deleteEntry client entryID

                LoginForm ->
                    let
                        _ =
                            Debug.todo "No client!"
                    in
                    Cmd.none
            )

        EntryDeleted (Ok deletedEntry) ->
            ( { model
                | entries =
                    model.entries
                        |> List.filter (\e -> e.id /= deletedEntry.id)
              }
            , Cmd.none
            )

        EntryDeleted (Err err) ->
            let
                _ =
                    Debug.log "Error while deleting the entry" err
            in
            ( model, Cmd.none )

        EntriesFetched (Ok entriesPager) ->
            ( { model | entries = entriesPager.objects }, Cmd.none )

        EntriesFetched (Err err) ->
            let
                _ =
                    Debug.log "Error while fetching the entries" err
            in
            ( model, Cmd.none )

        UpdateServerURL serverURL ->
            ( { model | serverURL = serverURL }, Cmd.none )

        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        UseLogin ->
            let
                client =
                    Kinto.client model.serverURL (Kinto.Basic model.username model.password)
            in
            ( { model | page = LoggedIn client }, getEntryList client )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    case model.page of
        LoginForm ->
            viewLoginForm model

        LoggedIn client ->
            viewLoggedIn model


viewLoginForm : Model -> Html.Html Msg
viewLoginForm model =
    Html.form
        [ Html.Events.onSubmit UseLogin
        ]
        [ Html.fieldset []
            [ Html.legend [] [ Html.text "Kinto credentials" ]
            , Html.div [ Html.Attributes.class "input-field" ]
                [ Html.label []
                    [ Html.text "Server URL"
                    , Html.input
                        [ Html.Attributes.type_ "text"
                        , Html.Attributes.name "serverURL"
                        , Html.Events.onInput UpdateServerURL
                        ]
                        []
                    ]
                ]
            , Html.div [ Html.Attributes.class "input-field" ]
                [ Html.label []
                    [ Html.text "Username"
                    , Html.input
                        [ Html.Attributes.type_ "text"
                        , Html.Attributes.name "username"
                        , Html.Events.onInput UpdateUsername
                        ]
                        []
                    ]
                ]
            , Html.div [ Html.Attributes.class "input-field" ]
                [ Html.label []
                    [ Html.text "Password"
                    , Html.input
                        [ Html.Attributes.type_ "password"
                        , Html.Events.onInput UpdatePassword
                        ]
                        []
                    ]
                ]
            , Html.div [ Html.Attributes.class "input-field" ]
                [ Html.input
                    [ Html.Attributes.type_ "submit"
                    , Html.Attributes.class "button"
                    , Html.Attributes.value "Use this"
                    ]
                    []
                ]
            ]
        ]


viewLoggedIn : Model -> Html.Html Msg
viewLoggedIn model =
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
                                , Html.Attributes.name "date"
                                , Html.Events.onInput UpdateDate
                                , Html.Attributes.value model.editDate
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.input
                                [ Html.Attributes.type_ "text"
                                , Html.Attributes.name "name"
                                , Html.Events.onInput UpdateProjectName
                                , Html.Attributes.value model.editProjectName
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.textarea
                                [ Html.Attributes.name "description"
                                , Html.Events.onInput UpdateDescription
                                , Html.Attributes.value model.editDescription
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.input
                                [ Html.Attributes.type_ "number"
                                , Html.Attributes.name "timeSpent"
                                , Html.Attributes.step "0.25"
                                , Html.Attributes.min "0"
                                , Html.Events.onInput UpdateTimeSpent
                                , Html.Attributes.value model.editTimeSpent
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.input
                                [ Html.Attributes.type_ "submit"
                                , Html.Attributes.class "button"
                                , Html.Attributes.value "Add this entry"
                                ]
                                []
                            ]
                        ]
                     ]
                        ++ (model.entries
                                |> List.map
                                    (\entry ->
                                        Html.tr []
                                            [ Html.td [] [ Html.text entry.date ]
                                            , Html.td [] [ Html.text entry.name ]
                                            , Html.td [] [ Html.text entry.description ]
                                            , Html.td [] [ Html.text <| String.fromFloat entry.timeSpent ]
                                            , Html.td []
                                                [ Html.button
                                                    [ Html.Attributes.class "button-danger"
                                                    , Html.Events.onClick <| DeleteEntry entry.id
                                                    ]
                                                    [ Html.text "Remove this entry" ]
                                                ]
                                            ]
                                    )
                           )
                    )
                ]
            ]
        ]



---- DECODERS ----
-- Entry --


type alias Entry =
    { id : String
    , last_modified : Int
    , name : String
    , description : String
    , timeSpent : Float
    , date : String
    }


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


recordResource : Kinto.Resource Entry
recordResource =
    Kinto.recordResource "default" "track-projects-time" decodeEntry


getEntryList : Kinto.Client -> Cmd Msg
getEntryList client =
    client
        |> Kinto.getList recordResource
        |> Kinto.sort [ "-date", "name" ]
        |> Kinto.send EntriesFetched



-- Deleted Entry --


type alias DeletedEntry =
    { id : String
    , last_modified : Int
    , deleted : Bool
    }


deletedRecordResource : Kinto.Resource DeletedEntry
deletedRecordResource =
    Kinto.recordResource "default" "track-projects-time" decodeDeletedEntry


decodeDeletedEntry : Decode.Decoder DeletedEntry
decodeDeletedEntry =
    Decode.map3 DeletedEntry
        (Decode.field "id" Decode.string)
        (Decode.field "last_modified" Decode.int)
        (Decode.field "deleted" Decode.bool)


deleteEntry : Kinto.Client -> String -> Cmd Msg
deleteEntry client entryID =
    client
        |> Kinto.delete deletedRecordResource entryID
        |> Kinto.send EntryDeleted



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
