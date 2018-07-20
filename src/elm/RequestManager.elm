module RequestManager exposing
    ( RequestError
    , imageUrl
    , puzzleListRequest
    , readmeRequest
    , nextRequest
    )

import Http
import HttpBuilder exposing (..)
import List
import Json.Decode as Decode
import Json.Encode as Encode

import Piece exposing (Frame)

baseUrl : String
baseUrl = "http://localhost:8080/api/"

type alias RequestError = Http.Error

type alias PuzzleId =
    String

type alias PieceId =
    (String, String)

puzzleListUrl : String
puzzleListUrl = baseUrl ++ "puzzles"

puzzleUrl : PuzzleId -> String
puzzleUrl = (++) (baseUrl ++ "puzzles/")

pieceUrl : PieceId -> String
pieceUrl (puzzleId, pieceId) =
    puzzleUrl puzzleId ++ "/" ++ pieceId

imageUrl : PieceId -> String
imageUrl = pieceUrl >> flip (++) "/image"

readmeUrl : PieceId -> String
readmeUrl = pieceUrl >> flip (++) "/readme"

nextUrl : PieceId -> String
nextUrl = pieceUrl >> flip (++) "/next"

-- requests
decoderStringList : Decode.Decoder (List String)
decoderStringList = Decode.list Decode.string

puzzleListRequest : (Result RequestError (List String) -> msg)  -> Cmd msg
puzzleListRequest lift =
    get puzzleListUrl |> withExpectJson decoderStringList |> send lift

readmeRequest : (Result RequestError String -> msg) -> PieceId -> Cmd msg
readmeRequest lift =
    readmeUrl >> get
        >> withExpectString
        >> send lift

encodeFrameList : List Frame -> Encode.Value
encodeFrameList =
    List.map
        (\frame ->
             let {x1, y1, x2, y2} = frame
             in Encode.object
             [ ("x1", Encode.int x1)
             , ("y1", Encode.int y1)
             , ("x2", Encode.int x2)
             , ("y2", Encode.int y2)
             ]
        )
    >> Encode.list

nextRequest : (Result RequestError String -> msg) -> List Frame -> PieceId -> Cmd msg
nextRequest lift frames =
    nextUrl >> post
        >> withJsonBody (encodeFrameList frames)
        >> withExpectString
        >> send lift


