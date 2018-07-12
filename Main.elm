port module Main exposing (main)

import Css exposing (..)
import Css.Colors exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css, href, src)

import Canvas exposing (Size, Point, Canvas, Style(Color), DrawOp(..))
import Color exposing (Color)

import Http
import Debug
import Markdown

import Material
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Options as Options
import Material.Scheme as Scheme
import Material.Grid exposing (grid, cell, size, Device(..))

import Pointer

baseUrl : String
baseUrl = "http://localhost:8000"

main : Program Never Model Msg
main =
    Html.program
        { init = init "2343" -- default hash
        , view = view >> toUnstyled >> Scheme.top
        , update = update
        , subscriptions = subscriptions
        }

type CoordinateID =
    X1
    | Y1
    | X2
    | Y2

type alias Frame =
    { x1 : Int
    , y1 : Int
    , x2 : Int
    , y2 : Int
    }

type alias Model =
    { pieceID : String
    , pieceSize : { width: Int, height: Int }
    , readme : String
    , frame : Frame
    , mdl : Material.Model
    }


init : String -> (Model, Cmd Msg)
init firstPiece =
    let
        initModel = Model
                    firstPiece
                    { width = 0, height = 0 }
                    ""
                    (Frame 0 0 0 0)
                    Material.model
    in (initModel, getReadme initModel)

type Msg =
    SubmitSolution
    | SubmissionStatus (Result Http.Error String)
    | GetReadmeStatus (Result Http.Error String)
    | DownMsg (Float, Float)
    | MoveMsg (Float, Float)
    | UpMsg (Float, Float)
    | DimensionsResponse (Int, Int)
    -- internal
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SubmitSolution ->
            (model, submitSolution model)
        SubmissionStatus (Ok nextPieceID) ->
            ({ model | pieceID = nextPieceID }, getReadme model)
        SubmissionStatus (error) ->
            Debug.log (toString error) (model, Cmd.none)
        GetReadmeStatus (Ok readmeString) ->
            ({ model | readme = readmeString }, dimensionsRequest <| imageLink model)
        GetReadmeStatus (error) ->
            (model, Cmd.none)
        DownMsg (x, y) ->
                ({model | frame =
                      let
                          {x2, y2} = model.frame
                      in Frame (x |> floor) (y |> floor) x2 y2
                 }, Cmd.none)
        MoveMsg (x, y) ->
                ({model | frame =
                      let
                          {x1, y1} = model.frame
                      in Frame x1 y1 (x |> floor) (y |> floor)
                 }, Cmd.none)
        UpMsg (x, y) ->
                ({model | frame =
                      let
                          {x1, y1} = model.frame
                      in Frame x1 y1 (x |> floor) (y |> floor)
                 }, Cmd.none)
        DimensionsResponse (w, h) ->
            Debug.log (toString (w, h))
            ({ model | pieceSize = { width = w, height = h } },
                 Cmd.none)
        Mdl (message_) ->
            Material.update Mdl message_ model

imageLink : Model -> String
imageLink model = baseUrl ++ "/puzzles/gallaxy/"
                  ++ model.pieceID ++ "/image.jpg"

submitSolution : Model -> Cmd Msg
submitSolution model =
    let
        {x1, y1, x2, y2} = model.frame
        url = baseUrl ++ "/link/gallaxy/"
              ++ model.pieceID
              ++ "?x1=" ++ toString x1
              ++ "&y1=" ++ toString y1
              ++ "&x2=" ++ toString x2
              ++ "&y2=" ++ toString y2
    in Http.send SubmissionStatus
        <| Http.getString url

getReadme : Model -> Cmd Msg
getReadme model =
    let
        url = baseUrl ++ "/puzzles/gallaxy/"
              ++ model.pieceID ++ "/readme.md"
    in Http.send GetReadmeStatus
        <| Http.getString url

-- View

relativePos : Pointer.Event -> ( Float, Float )
relativePos event =
    event.pointer.offsetPos


regions : Model -> DrawOp
regions model =
    let
        {x1, y1, x2, y2} = model.frame
        frameWidth = x2 - x1
        frameHeight = y2 - y1
    in
        [ rectangle (Point (toFloat x1) (toFloat y1)) frameWidth frameHeight (Color.rgba 0 0 255 0.5)
        , FillStyle <| Color Color.white
        ] |> Canvas.batch

rectangle : Point -> Int -> Int -> Color.Color -> DrawOp
rectangle point width height color =
    [ BeginPath
    , Rect point (Size width height)
    , FillStyle <| Color color
    , Fill
    ] |> Canvas.batch

submissionContainer : Model -> Html Msg
submissionContainer model =
    div [ css [ display inlineBlock
              , position relative
              , float left
              , width (px <| toFloat model.pieceSize.width)
              , height (px <| toFloat model.pieceSize.height)
              , maxWidth (px <| toFloat model.pieceSize.width)
              , maxHeight (px <| toFloat model.pieceSize.height)
              , backgroundColor blue
              ]
        , Attributes.fromUnstyled <| Pointer.onDown (relativePos >> DownMsg)
        , Attributes.fromUnstyled <| Pointer.onMove (relativePos >> MoveMsg)
        , Attributes.fromUnstyled <| Pointer.onUp (relativePos >> UpMsg)
        ]
    [
     img [ css
           [ position absolute
           , zIndex (int 1)
           , backgroundSize contain
           , maxWidth inherit
           , maxHeight inherit
           ]
         , src (baseUrl ++ "/puzzles/gallaxy/" ++ model.pieceID ++ "/image.jpg")
         ][]
    , div
         [ css
           [ position absolute
           , zIndex (int 2)
           ]
         ]
         (if model.pieceSize.width /= 0 then
             [
              Canvas.initialize (Size model.pieceSize.width model.pieceSize.height)
             |> Canvas.draw (regions model)
             |> Canvas.toHtml []
             |> fromUnstyled
             ]
         else [])
    ]

view : Model -> Html Msg
view model =
    div
    []
    [ h1 [] [ text model.pieceID ]
    , submissionContainer model
    , Markdown.toHtml [] model.readme |> fromUnstyled
    , Button.render Mdl [0] model.mdl
        [ Options.onClick SubmitSolution ]
        [ Html.text "submit solution" ] |> fromUnstyled
    ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    dimensionsResponse DimensionsResponse

-- we will send request for image dimensions
-- through this port
port dimensionsRequest : String -> Cmd msg

port dimensionsResponse : ((Int, Int) -> msg) -> Sub msg
