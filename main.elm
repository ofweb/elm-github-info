module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (alt, height, href, placeholder, src, width)
import Html.Events exposing (onInput, onBlur)
import Http
import Json.Decode as Json exposing (Decoder, field, string)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Maybe exposing (..)
import Task exposing (..)


-- MODEL


type alias GithubUser =
    { name : String, avatar : String, link : String, login : String }


type alias GithubRepo =
    { description : String, language : String, link : String, name : String }


type alias Model =
    { ownerInput : String, owner : Maybe GithubUser, repoInput : String, repo : Maybe GithubRepo }


initialModel : Model
initialModel =
    { owner = Nothing, repo = Nothing, ownerInput = "", repoInput = "" }


userDecoder : Decoder GithubUser
userDecoder =
    Json.map4 GithubUser
        (field "name" string)
        (field "avatar_url" string)
        (field "html_url" string)
        (field "login" string)


repoDecoder : Decoder GithubRepo
repoDecoder =
    decode GithubRepo
        |> required "description" string
        |> required "language" string
        |> required "html_url" string
        |> required "name" string



-- UPDATE


type Msg
    = OwnerChange String
    | OwnerCheck
    | OwnerResp (Result Http.Error GithubUser)
    | RepoChange String
    | RepoCheck
    | RepoResp (Result Http.Error GithubRepo)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OwnerChange str ->
            ( { model | ownerInput = str }, Cmd.none )

        OwnerCheck ->
            ( model, Http.send OwnerResp (lookUpUser model.ownerInput) )

        OwnerResp (Ok user) ->
            ( { model | owner = Just user }, Cmd.none )

        OwnerResp (Err _) ->
            ( { model | owner = Nothing }, Cmd.none )

        RepoChange str ->
            ( { model | repoInput = str }, Cmd.none )

        RepoCheck ->
            case model.owner of
                Just githubUser ->
                    ( model, Http.send RepoResp (lookUpRepo githubUser.login model.repoInput) )

                Nothing ->
                    ( { model | repo = Nothing }, Cmd.none )

        RepoResp (Ok repo) ->
            ( { model | repo = Just repo }, Cmd.none )

        RepoResp (Err _) ->
            ( { model | repo = Nothing }, Cmd.none )


lookUpUser : String -> Http.Request GithubUser
lookUpUser query =
    Http.get ("https://api.github.com/users/" ++ query) userDecoder


lookUpRepo : String -> String -> Http.Request GithubRepo
lookUpRepo user query =
    Http.get ("https://api.github.com/repos/" ++ user ++ "/" ++ query) repoDecoder



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Owner", onInput OwnerChange, onBlur OwnerCheck ] []
        , withDefault (text "user not found") (Maybe.map viewUser model.owner)
        , br [] []
        , input [ placeholder "Repo", onInput RepoChange, onBlur RepoCheck ] []
        , withDefault (text "repo not found") (Maybe.map viewRepo model.repo)
        ]


viewUser : GithubUser -> Html Msg
viewUser githubUser =
    div [] [ a [ href githubUser.link ] [ img [ src githubUser.avatar, width 100, height 100, alt githubUser.name ] [] ] ]


viewRepo : GithubRepo -> Html Msg
viewRepo githubRepo =
    div [] [ a [ href githubRepo.link ] [ text githubRepo.name ], text (" " ++ githubRepo.description), text (" made in " ++ githubRepo.language) ]


main =
    Html.program { init = ( initialModel, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }
