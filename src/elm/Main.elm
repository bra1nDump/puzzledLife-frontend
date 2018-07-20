module Main exposing (main)

import Css exposing (..)
import Css.Colors exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css, href, src)

import Http exposing (jsonBody)
import HttpBuilder
import Debug
import List exposing (map)
import Json.Decode as Decode
import Json.Encode as Encode

import Markdown

import Material
import Material.Button as Button
import Material.Options as Options
import Material.Layout as Layout
import Material.Tabs as Tabs
import Material.Scheme as Scheme

import Piece exposing (..)

type alias Model =
    { mdl : Material.Model
    , selectedTab : Int
    , pieceID : String
    , piece : Piece.Piece
    , readme : String
    }

type Msg =
    Mdl (Material.Msg Msg)
    | SwitchedTab (Int)
    | PieceMsg (Piece.Msg)
    | SubmitSolution
    | ClearSelection
    | SubmissionStatus (Result Http.Error String)
    | GetReadmeStatus (Result Http.Error String)


main : Program Never Model Msg
main =
    Html.program
        { init = init "2343" -- default hash
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- helpers
baseUrl : String
baseUrl = "http://localhost:8080"

init : String -> (Model, Cmd Msg)
init firstPiece =
    let
        (piece, pieceCmd) = Piece.model PieceMsg <| pieceUrl firstPiece
        initModel = Model
                    Material.model
                    0
                    firstPiece
                    piece
                    ""
    in ({ initModel | mdl = Layout.setTabsWidth 200 initModel.mdl }
       , Cmd.batch
           [ getReadme initModel.pieceID
           , pieceCmd
           , Layout.sub0 Mdl
           ]
       )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Mdl (message_) ->
            Material.update Mdl message_ model
        PieceMsg (message_) ->
            let (piece, pieceCmd) = Piece.update message_ model.piece
            in ({ model | piece = piece}, pieceCmd)
        SwitchedTab (index) ->
            ({ model | selectedTab = index }, Cmd.none)
        SubmitSolution ->
            (model, submitSolution model)
        ClearSelection ->
            let (piece, pieceCmd) = Piece.model PieceMsg <| pieceUrl model.pieceID
            in ({ model | piece = piece }, pieceCmd)
        SubmissionStatus (Ok nextPieceID) ->
            let (piece, pieceCmd) = Piece.model PieceMsg <| pieceUrl nextPieceID
                newModel =
                    { model
                        | pieceID = nextPieceID
                        , piece = piece
                    }
            in (newModel,
                    Cmd.batch
                    [ getReadme nextPieceID
                    , pieceCmd
                    ]
               )
        SubmissionStatus (error) ->
            Debug.log (toString error) (model, Cmd.none)
        GetReadmeStatus (Ok readmeString) ->
            ({ model | readme = readmeString }, Cmd.none)
        GetReadmeStatus (error) ->
            (model, Cmd.none)

pieceUrl : String -> String
pieceUrl pieceID = baseUrl ++ "/api/puzzles/gallaxy/"
                 ++ pieceID ++ "/image.jpg"

submitSolution : Model -> Cmd Msg
submitSolution model =
    let
        json = model.piece.frames
             |> List.filter
                (\frame ->
                     let {x1, y1, x2, y2} = frame
                     in (x1 /= x2) && (y1 /= y2)
                )
             |> List.map
                (\frame ->
                     let {x1, y1, x2, y2} = frame
                     in Encode.object
                         [ ("x1", Encode.int x1)
                         , ("y1", Encode.int y1)
                         , ("x2", Encode.int x2)
                         , ("y2", Encode.int y2)
                         ]
                )
             |> Encode.list
        url = baseUrl ++ "/api/link/gallaxy/"
              ++ model.pieceID
        request = HttpBuilder.post url
                  |> HttpBuilder.withJsonBody json
                  |> HttpBuilder.withExpectString
    in HttpBuilder.send SubmissionStatus request

getReadme : String -> Cmd Msg
getReadme pieceID =
    let
        url = baseUrl ++ "/api/puzzles/gallaxy/"
              ++ pieceID ++ "/readme.md"
    in Http.send GetReadmeStatus
        <| Http.getString url

-- view
buttons : Model -> Html Msg
buttons model =
    div
    [ css
      [ width (px <| toFloat <| Tuple.first model.piece.size)
      , displayFlex
      , flexDirection row
      , justifyContent center
      ]
    ]
    [ Button.render Mdl [0] model.mdl
          [ Options.onClick ClearSelection ]
          [ Html.text "Clear" ] |> fromUnstyled
    , Button.render Mdl [0] model.mdl
          [ Options.onClick SubmitSolution ]
          [ Html.text "Submit" ] |> fromUnstyled
    ]

header : Model -> Html Msg
header model =
    div
    [ css
      [ height (px 30)
      , color olive
      ]
    ]
    []

tabs : List (Html Msg)
tabs =
    [ text "explore"
    , text "create"
    , text "about"
    ]

visitorView : Model -> Html Msg
visitorView model =
    div
    [ css
      [ margin (px 10) ]
    ]
    [ div
      [ css
        [ display inlineBlock
        , float left
        , marginRight (px 30)
        ]
      ]
      [ Piece.render PieceMsg model.piece
      , buttons model
      ]
    , Markdown.toHtml
        []
        model.readme |> fromUnstyled
    ]

creatorView : Model -> Html Msg
creatorView model =
    div
    []
    []

aboutView : Html Msg
aboutView =
    div
    [ css
      [ backgroundColor blue
      , height (pct 100)
      , width (pct 100)
      ]
    ]
    [text "lol"]

view : Model -> Html.Html Msg
view model =
    Layout.render Mdl model.mdl
    [ Layout.selectedTab model.selectedTab
    , Layout.onSelectTab SwitchedTab
    , Layout.fixedHeader
    ]
    { header = [ header model |> toUnstyled ]
    , drawer = []
    , tabs = (List.map toUnstyled tabs, [])
    , main =
        List.map toUnstyled
        [ case model.selectedTab of
              0 -> visitorView model
              1 -> creatorView model
              _ -> aboutView
        ]
    } |> Scheme.top


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Piece.subscriptions PieceMsg
    , Layout.subs Mdl model.mdl
    ]
