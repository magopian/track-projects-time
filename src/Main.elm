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
    | LoggingIn Kinto.Client
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
    | Logout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        -- LOGINFORM --
        ( UpdateServerURL serverURL, LoginForm ) ->
            ( { model | serverURL = serverURL }, Cmd.none )

        ( UpdateUsername username, LoginForm ) ->
            ( { model | username = username }, Cmd.none )

        ( UpdatePassword password, LoginForm ) ->
            ( { model | password = password }, Cmd.none )

        ( UseLogin, LoginForm ) ->
            let
                client =
                    Kinto.client model.serverURL (Kinto.Basic model.username model.password)
            in
            ( { model | page = LoggingIn client }
            , getEntryList client
            )

        -- LOGGINGIN --
        ( EntriesFetched (Ok entriesPager), LoggingIn client ) ->
            ( { model | page = LoggedIn client, entries = entriesPager.objects }, Cmd.none )

        ( EntriesFetched (Err err), LoggingIn client ) ->
            let
                _ =
                    Debug.log "Error while fetching the entries" err
            in
            ( model, Cmd.none )

        -- LOGGEDIN --
        ( UpdateDate date, LoggedIn client ) ->
            ( { model | editDate = date }, Cmd.none )

        ( UpdateProjectName name, LoggedIn client ) ->
            ( { model | editProjectName = name }, Cmd.none )

        ( UpdateDescription description, LoggedIn client ) ->
            ( { model | editDescription = description }, Cmd.none )

        ( UpdateTimeSpent timeSpent, LoggedIn client ) ->
            ( { model | editTimeSpent = timeSpent }, Cmd.none )

        ( AddEntry, LoggedIn client ) ->
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

        ( EntryAdded (Ok entry), LoggedIn client ) ->
            let
                entries =
                    [ entry ]
                        ++ model.entries
                        |> List.sortBy .date
                        |> List.reverse
            in
            ( { model | entries = entries }, Cmd.none )

        ( EntryAdded (Err err), LoggedIn client ) ->
            let
                _ =
                    Debug.log "Error while adding the entry" err
            in
            ( model, Cmd.none )

        ( DeleteEntry entryID, LoggedIn client ) ->
            ( model
            , deleteEntry client entryID
            )

        ( EntryDeleted (Ok deletedEntry), LoggedIn client ) ->
            ( { model
                | entries =
                    model.entries
                        |> List.filter (\e -> e.id /= deletedEntry.id)
              }
            , Cmd.none
            )

        ( EntryDeleted (Err err), LoggedIn client ) ->
            let
                _ =
                    Debug.log "Error while deleting the entry" err
            in
            ( model, Cmd.none )

        ( Logout, LoggedIn client ) ->
            ( { model | page = LoginForm, entries = [] }, Cmd.none )

        -- NOT FOUND --
        ( _, _ ) ->
            let
                _ =
                    Debug.todo "bad message for page"
            in
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    case model.page of
        LoginForm ->
            viewLoginForm model Nothing

        LoggingIn client ->
            viewLoginForm model <| Just client

        LoggedIn client ->
            viewLoggedIn client model


viewLoginForm : Model -> Maybe Kinto.Client -> Html.Html Msg
viewLoginForm model maybeClient =
    let
        button =
            case maybeClient of
                Just _ ->
                    loadingButton "Use these credentials" Loading

                Nothing ->
                    loadingButton "Use these credentials" NotLoading
    in
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
                        , Html.Attributes.value model.serverURL
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
                        , Html.Attributes.value model.username
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
                        , Html.Attributes.value model.password
                        , Html.Events.onInput UpdatePassword
                        ]
                        []
                    ]
                ]
            , Html.div [ Html.Attributes.class "input-field" ]
                [ button
                ]
            ]
        ]


viewLoggedIn : Kinto.Client -> Model -> Html.Html Msg
viewLoggedIn client model =
    Html.div []
        [ viewUserInfo client model.username
        , Html.h1 [ Html.Attributes.style "padding-top" "1em" ] [ Html.text "Time spent on projects" ]
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
                                , Html.Attributes.name "project name"
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
                            [ loadingButton "Add this entry" NotLoading ]
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
                                                [ loadingDangerButtonLink "Remove this entry" NotLoading <| DeleteEntry entry.id ]
                                            ]
                                    )
                           )
                    )
                ]
            ]
        ]


viewUserInfo : Kinto.Client -> String -> Html.Html Msg
viewUserInfo { baseUrl, headers } username =
    Html.div
        [ Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "right" "0"
        , Html.Attributes.style "padding" ".2em 1.15em"
        , Html.Attributes.style "z-index" "999"
        , Html.Attributes.style "text-align" "right"
        , Html.Attributes.class "bg-secondary"
        ]
        [ Html.text "Connected as "
        , Html.strong [] [ Html.text username ]
        , Html.text " on "
        , Html.strong [] [ Html.text baseUrl ]
        , Html.text " "
        , Html.button
            [ Html.Attributes.class "button-sm"
            , Html.Events.onClick Logout
            ]
            [ Html.text "logout" ]
        ]


type LoadingState
    = Loading
    | NotLoading


loadingButton : String -> LoadingState -> Html.Html Msg
loadingButton label loadingState =
    let
        loadingAttrs =
            case loadingState of
                Loading ->
                    [ Html.Attributes.type_ "submit"
                    , Html.Attributes.class "button button-loader"
                    , Html.Attributes.disabled True
                    ]

                NotLoading ->
                    [ Html.Attributes.class "button" ]
    in
    Html.button
        loadingAttrs
        [ Html.text label ]


loadingDangerButtonLink : String -> LoadingState -> Msg -> Html.Html Msg
loadingDangerButtonLink label loadingState msg =
    let
        loadingAttrs =
            case loadingState of
                Loading ->
                    [ Html.Attributes.style "opacity" "0.5"
                    , Html.Attributes.class "button button-danger button-loader"
                    ]

                NotLoading ->
                    [ Html.Events.onClick msg
                    , Html.Attributes.class "button button-danger"
                    ]
    in
    Html.a
        loadingAttrs
        [ Html.text label ]



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
