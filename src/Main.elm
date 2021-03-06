port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation
import Dict
import Filters
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Kinto
import Url
import Url.Parser exposing ((<?>))



---- MODEL ----


type alias Model =
    { navKey : Browser.Navigation.Key
    , entries : KintoData (List Entry)
    , newEntry : NewEntry
    , newEntryKintoData : KintoData Entry
    , loginForm : LoginForm
    , deleteEntryList : List String -- List of entry IDs being deleted
    , errorList : List String
    , filters : Filters.Filters Entry
    }


type alias NewEntry =
    { date : String
    , name : String
    , description : String
    , timeSpent : String
    }


type alias LoginForm =
    { serverURL : String
    , username : String
    , password : String
    }


emptyLoginForm : LoginForm
emptyLoginForm =
    { serverURL = ""
    , username = ""
    , password = ""
    }


type alias Flags =
    Encode.Value


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        loginForm =
            -- Decode a string from the value (the stringified session data)
            Decode.decodeValue Decode.string flags
                -- Decode a loginForm from the value
                |> Result.andThen (Decode.decodeString decodeSessionData)
                |> Result.withDefault emptyLoginForm

        model =
            { navKey = key
            , entries = NotRequested
            , newEntry =
                { date = ""
                , name = ""
                , description = ""
                , timeSpent = "1"
                }
            , newEntryKintoData = NotRequested
            , loginForm = loginForm
            , deleteEntryList = []
            , errorList = []
            , filters = Filters.urlToFilters url
            }
    in
    useLogin model



---- UPDATE ----


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | UpdateEntry NewEntry
    | AddEntry
    | EntryAdded (Result Kinto.Error Entry)
    | DeleteEntry String
    | EntryDeleted String (Result Kinto.Error DeletedEntry)
    | EntriesFetched (Result Kinto.Error (Kinto.Pager Entry))
    | UpdateLoginForm LoginForm
    | UseLogin
    | Logout
    | DiscardError Int


isLoginFormComplete : LoginForm -> Bool
isLoginFormComplete loginForm =
    loginForm.serverURL /= "" && loginForm.username /= "" && loginForm.password /= ""


useLogin : Model -> ( Model, Cmd Msg )
useLogin model =
    if isLoginFormComplete model.loginForm then
        let
            client =
                Kinto.client model.loginForm.serverURL (Kinto.Basic model.loginForm.username model.loginForm.password)
        in
        ( { model | entries = Requested }
        , Cmd.batch
            [ getEntryList client
            , saveSession <| encodeSessionData model.loginForm
            ]
        )

    else
        ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            ( { model | filters = Filters.urlToFilters url }, Cmd.none )

        -- LOGINFORM --
        UpdateLoginForm loginForm ->
            ( { model | loginForm = loginForm }, Cmd.none )

        UseLogin ->
            useLogin model

        EntriesFetched (Ok entriesPager) ->
            ( { model | entries = Received entriesPager.objects }, Cmd.none )

        EntriesFetched (Err err) ->
            ( { model
                | entries = Failed err
                , errorList = Kinto.errorToString err :: model.errorList
              }
            , Cmd.none
            )

        -- LOGGEDIN --
        UpdateEntry newEntry ->
            ( { model | newEntry = newEntry }, Cmd.none )

        AddEntry ->
            let
                timeSpent =
                    model.newEntry.timeSpent
                        |> String.toFloat
                        |> Maybe.withDefault 0

                data =
                    encodeData
                        model.newEntry.name
                        model.newEntry.description
                        timeSpent
                        model.newEntry.date

                client =
                    Kinto.client model.loginForm.serverURL (Kinto.Basic model.loginForm.username model.loginForm.password)
            in
            ( { model | newEntryKintoData = Requested }
            , client
                |> Kinto.create recordResource data
                |> Kinto.send EntryAdded
            )

        EntryAdded (Ok entry) ->
            let
                entries =
                    case model.entries of
                        Received entryList ->
                            entry
                                :: entryList
                                |> List.sortBy .date
                                |> List.reverse
                                |> Received

                        _ ->
                            model.entries

                newEntry =
                    model.newEntry
            in
            ( { model
                | entries = entries

                -- We're going straight back to "NotRequested" as we added the entry to the list
                , newEntryKintoData = NotRequested

                -- reset the description, keep the rest as we might need to add other items for the same day, same project, same duration
                , newEntry = { newEntry | description = "" }
              }
            , Cmd.none
            )

        EntryAdded (Err err) ->
            ( { model
                | newEntryKintoData = Failed err
                , errorList = Kinto.errorToString err :: model.errorList
              }
            , Cmd.none
            )

        DeleteEntry entryID ->
            let
                client =
                    Kinto.client model.loginForm.serverURL (Kinto.Basic model.loginForm.username model.loginForm.password)
            in
            ( { model | deleteEntryList = entryID :: model.deleteEntryList }
            , deleteEntry client entryID
            )

        EntryDeleted entryID (Ok _) ->
            let
                entries =
                    case model.entries of
                        Received entryList ->
                            entryList
                                |> List.filter (\e -> e.id /= entryID)
                                |> Received

                        _ ->
                            model.entries

                deleteEntryList =
                    model.deleteEntryList
                        |> List.filter (\id -> id /= entryID)
            in
            ( { model
                | entries = entries
                , deleteEntryList = deleteEntryList
              }
            , Cmd.none
            )

        EntryDeleted entryID (Err err) ->
            let
                deleteEntryList =
                    model.deleteEntryList
                        |> List.filter (\id -> id /= entryID)
            in
            ( { model
                | deleteEntryList = deleteEntryList
                , errorList = Kinto.errorToString err :: model.errorList
              }
            , Cmd.none
            )

        Logout ->
            ( { model | entries = NotRequested, loginForm = emptyLoginForm }, logoutSession () )

        DiscardError index ->
            ( { model | errorList = List.take index model.errorList ++ List.drop (index + 1) model.errorList }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Track projects time"
    , body =
        [ Html.div []
            [ viewGithubLink
            , viewErrorList model.errorList
            , case model.entries of
                Received entries ->
                    viewEntryList entries model

                _ ->
                    viewLoginForm model
            ]
        ]
    }


viewLoginForm : Model -> Html.Html Msg
viewLoginForm ({ loginForm } as model) =
    let
        formComplete =
            isLoginFormComplete loginForm

        buttonState =
            if formComplete then
                case model.entries of
                    Requested ->
                        Loading

                    _ ->
                        NotLoading

            else
                Disabled

        button =
            loadingButton "Use these credentials" buttonState
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
                        , Html.Attributes.value loginForm.serverURL
                        , Html.Events.onInput <| \serverURL -> UpdateLoginForm { loginForm | serverURL = serverURL }
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
                        , Html.Attributes.value loginForm.username
                        , Html.Events.onInput <| \username -> UpdateLoginForm { loginForm | username = username }
                        ]
                        []
                    ]
                ]
            , Html.div [ Html.Attributes.class "input-field" ]
                [ Html.label []
                    [ Html.text "Password"
                    , Html.input
                        [ Html.Attributes.type_ "password"
                        , Html.Attributes.value loginForm.password
                        , Html.Events.onInput <| \password -> UpdateLoginForm { loginForm | password = password }
                        ]
                        []
                    ]
                ]
            , Html.div [ Html.Attributes.class "input-field" ]
                [ button
                ]
            ]
        ]


viewEntryList : List Entry -> Model -> Html.Html Msg
viewEntryList entries ({ newEntry, filters } as model) =
    let
        filterBadge : String -> String -> Html.Html Msg
        filterBadge label value =
            Html.a
                [ Html.Attributes.href <| Filters.removeFilterFromFragment filters label
                , Html.Attributes.class "badge"
                , Html.Attributes.style "cursor" "pointer"
                ]
                [ Html.text <| label ++ " = " ++ value ++ " | x" ]

        filteredEntries =
            filters
                |> Dict.toList
                -- Apply all filters in turn on the accumulator (the entries list)
                |> List.foldl
                    (\( _, { value, entryFieldGetter, compareFunc } ) entryList ->
                        entryList
                            |> List.filter (\entry -> compareFunc (entryFieldGetter entry) value)
                    )
                    entries

        badges =
            filters
                |> Dict.toList
                |> List.map
                    (\( label, { value } ) ->
                        filterBadge label value
                    )

        totalDays =
            filteredEntries
                |> List.map .timeSpent
                |> List.foldl (+) 0
    in
    Html.div []
        [ viewUserInfo model.loginForm.serverURL model.loginForm.username
        , Html.h1 []
            [ Html.text "Days spent on projects: "
            , Html.span [ Html.Attributes.class "badge" ] [ Html.text <| String.fromFloat totalDays ++ " days" ]
            ]
        , Html.div [] badges
        , Html.form
            [ Html.Events.onSubmit AddEntry ]
            [ Html.table [ Html.Attributes.style "width" "100%" ]
                [ Html.thead []
                    [ Html.th [] [ Html.text "Date" ]
                    , Html.th [] [ Html.text "Project name" ]
                    , Html.th [] [ Html.text "What was done" ]
                    , Html.th [] [ Html.text "How many days" ]
                    , Html.th [] [ Html.text "Actions" ]
                    ]
                , Html.tbody []
                    (Html.tr []
                        [ Html.td []
                            [ Html.input
                                [ Html.Attributes.type_ "date"
                                , Html.Attributes.name "date"
                                , Html.Events.onInput <| \date -> UpdateEntry { newEntry | date = date }
                                , Html.Attributes.value newEntry.date
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.input
                                [ Html.Attributes.type_ "text"
                                , Html.Attributes.name "project name"
                                , Html.Events.onInput <| \name -> UpdateEntry { newEntry | name = name }
                                , Html.Attributes.value newEntry.name
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.textarea
                                [ Html.Attributes.name "description"
                                , Html.Events.onInput <| \description -> UpdateEntry { newEntry | description = description }
                                , Html.Attributes.value newEntry.description
                                , Html.Attributes.style "white-space" "pre-wrap"
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.input
                                [ Html.Attributes.type_ "number"
                                , Html.Attributes.name "timeSpent"
                                , Html.Attributes.step "0.25"
                                , Html.Attributes.min "0"
                                , Html.Events.onInput <| \timeSpent -> UpdateEntry { newEntry | timeSpent = timeSpent }
                                , Html.Attributes.value newEntry.timeSpent
                                ]
                                []
                            ]
                        , Html.td []
                            [ loadingButton "Add this entry" <|
                                case model.newEntryKintoData of
                                    Requested ->
                                        Loading

                                    _ ->
                                        NotLoading
                            ]
                        ]
                        :: (filteredEntries
                                |> List.map
                                    (\entry ->
                                        Html.tr []
                                            [ Html.td []
                                                [ let
                                                    ( filterLabel, compareFunc ) =
                                                        -- First date we add is a "from", the second an "until"
                                                        if Dict.member "from" filters then
                                                            ( "until", (<=) )

                                                        else
                                                            ( "from", (>=) )
                                                  in
                                                  Html.a
                                                    [ Html.Attributes.href <| Filters.addFilterToFragment filters filterLabel (Filters.Filter entry.date .date compareFunc)
                                                    , Html.Attributes.title <| "Filter on " ++ filterLabel ++ " " ++ entry.date
                                                    ]
                                                    [ Html.text entry.date ]
                                                ]
                                            , Html.td []
                                                [ Html.a
                                                    [ Html.Attributes.href <| Filters.addFilterToFragment filters "name" (Filters.Filter entry.name .name (==))
                                                    , Html.Attributes.title <| "Filter on project name = " ++ entry.name
                                                    ]
                                                    [ Html.text entry.name ]
                                                ]
                                            , Html.td [ Html.Attributes.style "white-space" "pre-wrap" ] [ Html.text entry.description ]
                                            , Html.td [] [ Html.text <| String.fromFloat entry.timeSpent ]
                                            , Html.td []
                                                [ removeEntryButton "Remove this entry" entry.id model.deleteEntryList <| Filters.filtersToFragment filters ]
                                            ]
                                    )
                           )
                    )
                ]
            ]
        ]


viewUserInfo : String -> String -> Html.Html Msg
viewUserInfo serverURL username =
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
        , Html.strong [] [ Html.text serverURL ]
        , Html.text " "
        , Html.button
            [ Html.Attributes.class "button-sm"
            , Html.Events.onClick Logout
            ]
            [ Html.text "logout" ]
        ]


viewGithubLink : Html.Html Msg
viewGithubLink =
    Html.a
        [ Html.Attributes.target "_blank"
        , Html.Attributes.href "https://github.com/magopian/track-projects-time"
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "top" "30px"
        , Html.Attributes.style "right" "0"
        ]
        [ Html.img
            [ Html.Attributes.src "https://upload.wikimedia.org/wikipedia/commons/thumb/e/eb/Ei-sc-github.svg/768px-Ei-sc-github.svg.png"
            , Html.Attributes.style "height" "96px"
            , Html.Attributes.style "width" "96px"
            ]
            []
        , Html.span
            [ Html.Attributes.style "display" "block"
            , Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "color" "#000"
            ]
            [ Html.text "Github" ]
        ]


viewErrorList : List String -> Html.Html Msg
viewErrorList errorList =
    Html.ul
        [ Html.Attributes.style "list-style-type" "none"
        , Html.Attributes.style "padding-left" "0"
        , Html.Attributes.style "padding-top" "3em"
        ]
        (errorList
            |> List.indexedMap
                (\index error ->
                    Html.li
                        [ Html.Attributes.class "alert alert-danger"
                        ]
                        [ Html.a
                            [ Html.Attributes.class "float-right"
                            , Html.Attributes.style "font-weight" "normal"
                            , Html.Attributes.style "text-decoration" "none"
                            , Html.Attributes.style "cursor" "pointer"
                            , Html.Events.onClick <| DiscardError index
                            ]
                            [ Html.text "x" ]
                        , Html.text error
                        ]
                )
        )


type ButtonState
    = Disabled
    | Loading
    | NotLoading


loadingButton : String -> ButtonState -> Html.Html Msg
loadingButton label buttonState =
    let
        loadingAttrs =
            case buttonState of
                Disabled ->
                    [ Html.Attributes.type_ "submit"
                    , Html.Attributes.class "button"
                    , Html.Attributes.disabled True
                    ]

                Loading ->
                    [ Html.Attributes.type_ "submit"
                    , Html.Attributes.class "button button-loader"
                    , Html.Attributes.disabled True
                    ]

                NotLoading ->
                    [ Html.Attributes.type_ "submit"
                    , Html.Attributes.class "button"
                    ]
    in
    Html.button
        loadingAttrs
        [ Html.text label ]


removeEntryButton : String -> String -> List String -> String -> Html.Html Msg
removeEntryButton label entryID deleteEntryList urlFragment =
    let
        loadingAttrs =
            if List.member entryID deleteEntryList then
                [ Html.Attributes.style "opacity" "0.5"
                , Html.Attributes.class "button button-danger button-loader"
                ]

            else
                [ Html.Events.onClick <| DeleteEntry entryID
                , Html.Attributes.class "button button-danger"
                ]
    in
    Html.a
        (Html.Attributes.href urlFragment :: loadingAttrs)
        [ Html.text label ]



---- DECODERS ----


type KintoData a
    = NotRequested
    | Requested
    | Received a
    | Failed Kinto.Error



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
        |> Kinto.sort [ "-date" ]
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
        |> Kinto.send (EntryDeleted entryID)



-- Session Data --


encodeSessionData : LoginForm -> Encode.Value
encodeSessionData loginForm =
    Encode.object
        [ ( "serverURL", Encode.string loginForm.serverURL )
        , ( "username", Encode.string loginForm.username )
        , ( "password", Encode.string loginForm.password )
        ]


decodeSessionData : Decode.Decoder LoginForm
decodeSessionData =
    Decode.map3
        LoginForm
        (Decode.field "serverURL" Decode.string)
        (Decode.field "username" Decode.string)
        (Decode.field "password" Decode.string)



---- PORTS ----


port saveSession : Encode.Value -> Cmd msg


port logoutSession : () -> Cmd msg



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
