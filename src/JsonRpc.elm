module JsonRpc exposing
    ( WebData
    , Response(..), RpcError, RpcData
    , TaskData, RpcTaskData
    , Data(..)
    , Param
    , call
    , callTask
    , flatResponse, flat, toResult, handleJsonResponse
    , httpErrToString, errorToString
    )

{-| 一个通用的JsonRpc-V2调用接口，帮助实现JsonRpc客户端应用。


# 数据结构


## Http 数据结构

@docs WebData


## JsonRpc 数据结构

@docs Response, RpcError, RpcData


## 用于 Task 调用的 JsonRpc 数据结构

@docs TaskData, RpcTaskData


## 便于用户使用的 JsonRpc 数据结构

@docs Data


## JsonRpc 调用参数结构

@docs Param


# Cmd 方式调用

@docs call


# Task 方式调用

@docs callTask


# 辅助函数

@docs flatResponse, flat, toResult, handleJsonResponse
@docs httpErrToString, errorToString

-}

import Http exposing (Header)
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E
import Task exposing (Task)



-- 数据结构


{-| JsonRpc 返回错误格式定义
-}
type alias RpcError =
    { code : Int
    , message : String
    , data : Maybe String
    }


{-| JsonRpc 响应结果
-}
type Response a
    = InnerResult a
    | InnerError RpcError


{-| Http 返回数据定义
-}
type alias WebData a =
    Result Http.Error a


{-| 基于 http 的 JsonRpc 响应结果
-}
type alias RpcData a =
    WebData (Response a)


{-| Http Task 调用返回数据定义
-}
type alias TaskData a =
    Task Http.Error a


{-| 基于 http Task 调用 JsonRpc 返回结果
-}
type alias RpcTaskData a =
    TaskData (Response a)


{-| 便于处理的返回结果
-}
type Data a
    = RpcResult a
    | RpcErr RpcError
    | HttpErr Http.Error



-- 调用函数


{-| 调用参数
-}
type alias Param =
    { url : String
    , token : Maybe String
    , method : String
    , params : List ( String, E.Value )
    }


{-| 构建JsonRpc请求体
-}
jsonrpc_request : Param -> E.Value
jsonrpc_request opts =
    E.object
        [ ( "id", E.int 0 )
        , ( "jsonrpc", E.string "2.0" )
        , ( "method", E.string opts.method )
        , ( "params", E.object opts.params )
        ]


{-| Header for Authorization.
-}
jsonrpc_header : Maybe String -> List Header
jsonrpc_header token =
    let
        json_header =
            Http.header "Accept" "application/json"
    in
    case token of
        Just t ->
            [ json_header
            , Http.header "Authorization" ("bearer " ++ t)
            ]

        Nothing ->
            [ json_header ]


{-| HTTP 调用方法
-}
call :
    Param
    -> D.Decoder a
    -> (RpcData a -> msg)
    -> Cmd msg
call opts dc toMsg =
    let
        dcX =
            D.oneOf
                [ D.map InnerResult (D.at [ "result" ] dc)
                , D.map InnerError (D.at [ "error" ] errorDecoder)
                ]
    in
    Http.request
        { method = "POST"
        , headers = jsonrpc_header opts.token
        , url = opts.url
        , body = Http.jsonBody (jsonrpc_request opts)
        , expect = Http.expectJson toMsg dcX
        , timeout = Nothing
        , tracker = Nothing
        }


{-| -}
handleJsonResponse : D.Decoder a -> Http.Response String -> WebData a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case D.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


{-| HTTP Task调用方法
-}
callTask :
    Param
    -> D.Decoder a
    -> RpcTaskData a
callTask opts dc =
    let
        dcX =
            D.oneOf
                [ D.map InnerResult (D.at [ "result" ] dc)
                , D.map InnerError (D.at [ "error" ] errorDecoder)
                ]
    in
    Http.task
        { method = "POST"
        , headers = jsonrpc_header opts.token
        , url = opts.url
        , body = Http.jsonBody (jsonrpc_request opts)
        , resolver = Http.stringResolver <| handleJsonResponse <| dcX
        , timeout = Nothing
        }



-- 辅助函数


{-| 将rpc调用的返回结果平面化处理
-}
flat : RpcData a -> Data a
flat rpc =
    case rpc of
        Ok d ->
            flatResponse d

        Err e ->
            HttpErr e


{-| 返回结果平面化处理
-}
flatResponse : Response a -> Data a
flatResponse rpc =
    case rpc of
        InnerResult v ->
            RpcResult v

        InnerError e ->
            RpcErr e


{-| 将平面化处理结果转换为Result
-}
toResult : Data a -> Result String a
toResult data =
    case data of
        RpcResult d ->
            Ok d

        RpcErr e ->
            Err <| errorToString e

        HttpErr he ->
            Err <| httpErrToString he


{-| Http.Error转换为String
-}
httpErrToString : Http.Error -> String
httpErrToString he =
    case he of
        Http.BadUrl u ->
            "无效URL => " ++ u

        Http.Timeout ->
            "超时"

        Http.NetworkError ->
            "网络出错"

        Http.BadStatus status ->
            "返回状态码" ++ String.fromInt status

        Http.BadBody reason ->
            reason


{-| 错误格式解码器
-}
errorDecoder : D.Decoder RpcError
errorDecoder =
    D.succeed RpcError
        |> required "code" D.int
        |> required "message" D.string
        |> optional "data" (D.nullable D.string) Nothing


{-| -}
errorToString : RpcError -> String
errorToString e =
    "Code: "
        ++ String.fromInt e.code
        ++ " Message: "
        ++ e.message
        ++ (case e.data of
                Just d ->
                    " Data: " ++ d

                _ ->
                    ""
           )
