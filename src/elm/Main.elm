module Main exposing (..)

-- import Parser exposing ((|.), (|=), Parser, andThen, drop, float, keyword, parse, spaces, succeed, symbol, take)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


main : Program Decode.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias AccInfo =
    { username : String
    , acc_info : String
    , is_used : Int
    }


type alias AccsInfo =
    { accs : List AccInfo
    }


type State
    = Used
    | Cancel


type alias UpdateState =
    { username : String
    , state : State
    }


type Msg
    = AccountRes (Result Http.Error (List AccInfo))
    | AccountChangeRes (Result Http.Error AccInfo)
    | Change UpdateState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AccountRes v ->
            case v of
                Ok accs ->
                    let
                        accDict =
                            Dict.fromList (List.map (\s -> ( s.username, s )) accs)
                    in
                    ( { model | accsDict = accDict }, Cmd.none )

                Err err ->
                    ( { model | error = Just (buildErrorMessage err) }, Cmd.none )

        Change v ->
            let
                accs =
                    Dict.get v.username model.accsDict
                        |> Maybe.withDefault (AccInfo "" "" 0)

                accInfo =
                    AccInfo accs.username accs.acc_info (stateToInt v.state)

                accsDict =
                    Dict.insert v.username accInfo model.accsDict
            in
            ( { model | accsDict = accsDict }, savePost accInfo v.username model.host )

        AccountChangeRes v ->
            ( model, Cmd.none )


view model =
    div [ class "flex content-center justify-center my-4" ]
        [ viewBody model, viewErr model ]


viewBody model =
    div [ class "w-1/2" ]
        [ table [ class "table table-compact table-zebra w-full" ]
            [ tbody []
                (List.map accList (List.sortBy (\s -> s.is_used) (Dict.values model.accsDict)))
            ]
        ]


viewErr model =
    case model.error of
        Just err ->
            div [ class "text-red-700" ]
                [ text err ]

        Nothing ->
            text ""


accList acc =
    let
        used =
            if acc.is_used == 0 then
                False

            else
                True
    in
    tr [ class "", classList [ ( "text-red-500", used ), ( "hover", not used ), ( "select-none", used ) ] ]
        [ td []
            [ text acc.acc_info ]
        , if used then
            td [ class "btn btn-error btn-sm" ]
                [ button [ onClick (Change (UpdateState acc.username Cancel)) ]
                    [ text "取消" ]
                ]

          else
            td [ class "btn btn-primary btn-sm" ]
                [ button [ onClick (Change (UpdateState acc.username Used)) ]
                    [ text "使用" ]
                ]
        ]


type alias Model =
    { accsDict : Dict.Dict String AccInfo
    , host : String
    , error : Maybe String
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        host =
            case Decode.decodeValue (Decode.field "host" Decode.string) flags of
                Ok v ->
                    v

                Err _ ->
                    ""
    in
    ( { accsDict = Dict.empty
      , host = host
      , error = Nothing
      }
    , getAccsCmd host
    )


subscriptions model =
    Sub.none


getAccsCmd : String -> Cmd Msg
getAccsCmd host =
    Http.get
        { url = "http://" ++ host ++ ":5019/accs/"
        , expect =
            decoderAccs |> Http.expectJson AccountRes
        }


savePost : AccInfo -> String -> String -> Cmd Msg
savePost accs id host=
    let
        postUrl =
            "http://" ++ host ++ ":5019/accs/" ++ id
    in
    Http.request
        { method = "PUT"
        , headers = []
        , url = postUrl
        , body = Http.jsonBody (accEncoder accs)
        , expect = Http.expectJson AccountChangeRes decoderAcc
        , timeout = Nothing
        , tracker = Nothing
        }


decoderAccs : Decode.Decoder (List AccInfo)
decoderAccs =
    Decode.list decoderAcc


decoderAcc : Decode.Decoder AccInfo
decoderAcc =
    Decode.map3 AccInfo
        (Decode.field "username" Decode.string)
        (Decode.field "acc_info" Decode.string)
        (Decode.field "is_used" Decode.int)


accEncoder : AccInfo -> Encode.Value
accEncoder post =
    Encode.object
        [ ( "username", Encode.string post.username )
        , ( "acc_info", Encode.string post.acc_info )
        , ( "is_used", Encode.int post.is_used )
        ]


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            "URL格式不正确:" ++ message

        Http.Timeout ->
            "请求超时，请稍后重试！"

        Http.NetworkError ->
            "请求超时，请稍后重试！"

        Http.BadStatus statusCode ->
            "请求失败！: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


stateToInt state =
    case state of
        Used ->
            1

        Cancel ->
            0
