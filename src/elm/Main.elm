module Main exposing (..)

-- import Parser exposing ((|.), (|=), Parser, andThen, drop, float, keyword, parse, spaces, succeed, symbol, take)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import InfiniteList as IL
import Json.Decode as Decode
import Json.Encode as Encode
import Time


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


type ListItem
    = Header
    | Row AccInfo


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
    | ChangeMark Bool String
    | Tick Time.Posix
    | InfListMsg IL.Model
    | Search String


type alias Model =
    { accsDict : Dict.Dict String AccInfo
    , host : String
    , mark : Dict.Dict String String
    , error : Maybe String
    , infList : IL.Model
    , search : String
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
      , mark = Dict.empty
      , host = host
      , error = Nothing
      , infList = IL.init
      , search = ""
      }
    , getAccsCmd host
    )


subscriptions model =
    Sub.batch
        [ Time.every 5000 Tick
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AccountRes v ->
            case v of
                Ok accs ->
                    let
                        accDict =
                            Dict.fromList (List.map (\s -> ( s.username, s )) accs)

                        filtered =
                            filterAccs model.search accDict
                    in
                    ( { model | accsDict = filtered }, Cmd.none )

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

        Tick v ->
            ( model, getAccsCmd model.host )

        ChangeMark isMark user ->
            let
                mark =
                    if isMark then
                        Dict.insert user user model.mark

                    else
                        Dict.remove user model.mark
            in
            ( { model | mark = mark }, Cmd.none )

        InfListMsg infList ->
            ( { model | infList = infList }, Cmd.none )

        Search v ->
            let
                filtered =
                    filterAccs v model.accsDict
            in
            ( { model | search = v, accsDict = filtered }, Cmd.none )


filterAccs : String -> Dict.Dict String b -> Dict.Dict String b
filterAccs filter accs =
    let
        filtered k v =
            String.contains filter k
    in
    Dict.filter filtered accs


itemHeight : Int -> ListItem -> Int
itemHeight _ item =
    case item of
        Header ->
            50

        Row _ ->
            48


containerHeight : Int
containerHeight =
    760



-- viewCount model =


view model =
    div [ class " overflow-hidden min-w-5xl" ]
        [ viewBody model
        , viewErr model
        ]


viewBody model =
    let
        sortAccsDict =
            List.sortBy (\s -> s.is_used) (Dict.values model.accsDict)
                |> List.map Row
    in
    div [ class "w-full lg:w-1/2 m-auto" ]
        [ div
            [ class "text-gray-900 overflow-scroll"
            , style "height" (String.fromInt containerHeight ++ "px")
            , IL.onScroll InfListMsg
            ]
            [ IL.view (config model) model.infList (Header :: sortAccsDict)
            ]
        ]


searchView =
    div [ class "absolute top-4 left-4" ]
        [ input
            [ class "input input-primary input-sm"
            , placeholder "搜索"
            , onInput Search
            ]
            []
        ]


itemView : Model -> Int -> Int -> ListItem -> Html Msg
itemView model idx listIdx item =
    case item of
        Header ->
            thead
                [ style "height" (String.fromInt (itemHeight idx item) ++ "px")
                , class "sticky top-0 bg-base-100"
                ]
                [ tr []
                    [ td [ class "px-4" ]
                        [ searchView ]
                    ]
                ]

        Row acc ->
            tbody
                [ class "whitespace-nowrap overflow-scroll"
                , style "height" (String.fromInt (itemHeight idx item) ++ "px")
                ]
                [ let
                    used =
                        if acc.is_used == 0 then
                            False

                        else
                            True

                    mark =
                        List.any (\s -> s == acc.username) (Dict.values model.mark)
                  in
                  tr
                    [ classList
                        [ ( "text-red-500", used )
                        , ( "hover:bg-base-300", not used )
                        , ( "select-none", used )
                        , ( "text-yellow-600", mark )
                        ]
                    , class ""
                    ]
                    (List.append
                        (case String.split "----" acc.acc_info of
                            username :: passward :: email :: _ ->
                                [ td [ class "px-4" ]
                                    [ text username ]
                                , td [ class "px-4" ] [ text passward ]
                                , td [ class "px-4" ] [ text email ]
                                ]

                            _ ->
                                []
                        )
                        [ if used then
                            td [ class "pr-4" ]
                                [ button [ class "btn btn-error btn-outline btn-sm", onClick (Change (UpdateState acc.username Cancel)) ]
                                    [ text "取消" ]
                                ]

                          else
                            td [ class "pr-4" ]
                                [ button [ class "btn btn-primary btn-outline btn-sm", onClick (Change (UpdateState acc.username Used)) ]
                                    [ text "使用" ]
                                ]
                        , td
                            [ class "pr-4 " ]
                            [ button
                                [ class "btn btn-warning btn-outline btn-sm"
                                , classList [ ( "btn-error", mark ) ]
                                , onClick (ChangeMark (not mark) acc.username)
                                ]
                                [ text <|
                                    if mark then
                                        "取消"

                                    else
                                        "标记"
                                ]
                            ]
                        ]
                    )
                ]


config : Model -> IL.Config ListItem Msg
config model =
    IL.config
        { itemView = itemView model
        , itemHeight = IL.withVariableHeight itemHeight
        , containerHeight = containerHeight
        }
        |> IL.withOffset 320
        |> IL.withKeepFirst 1



-- searcher model  =


viewErr model =
    case model.error of
        Just err ->
            div [ class "text-red-700" ]
                [ text err ]

        Nothing ->
            text ""


getAccsCmd : String -> Cmd Msg
getAccsCmd host =
    Http.get
        { url = "http://" ++ host ++ ":5019/accs/"
        , expect =
            decoderAccs |> Http.expectJson AccountRes
        }


savePost : AccInfo -> String -> String -> Cmd Msg
savePost accs id host =
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
