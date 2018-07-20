module Main exposing (main)

import Css exposing (..)
import Css.Colors exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css, href, src)

import List exposing (map)
import Json.Decode as Decode
import Json.Encode as Encode

import Markdown

import Material
import Material.Layout as Layout
import Material.Tabs as Tabs
import Material.List as MList
import Material.Button as Button
import Material.Options as Options
import Material.Scheme as Scheme

import Piece
import RequestManager exposing (..)

type alias Model =
    { mdl : Material.Model
    , selectedTab : Int
    , pieceId : (String, String)
    , piece : Piece.Piece
    , readme : String
    }

type Msg =
    Mdl (Material.Msg Msg)
    | SwitchedTab (Int)
    | PieceMsg (Piece.Msg)
    | SubmitSolution
    | ClearSelection
    | SubmissionStatus (Result RequestError String)
    | GetReadmeStatus (Result RequestError String)


main : Program Never Model Msg
main =
    Html.program
        { init = init ("gallaxy", "2343") -- default hash
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : (String, String) -> (Model, Cmd Msg)
init pieceId =
    let
        (piece, pieceCmd) = Piece.model PieceMsg <| imageUrl pieceId
        initModel = Model
                    Material.model
                    0
                    pieceId
                    piece
                    ""
    in ({ initModel | mdl = Layout.setTabsWidth 200 initModel.mdl }
       , Cmd.batch
           [ readmeRequest GetReadmeStatus initModel.pieceId
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
            (model, nextRequest SubmissionStatus model.piece.frames model.pieceId)
        ClearSelection ->
            let (piece, pieceCmd) = Piece.model PieceMsg <| imageUrl model.pieceId
            in ({ model | piece = piece }, pieceCmd)
        SubmissionStatus (Ok nextPieceId) ->
            let (puzzleId, _) = model.pieceId
                (piece, pieceCmd) = Piece.model PieceMsg <| imageUrl (puzzleId, nextPieceId)
                newModel =
                    { model
                        | pieceId = (puzzleId, nextPieceId)
                        , piece = piece
                    }
            in Debug.log (nextPieceId) (newModel,
                    Cmd.batch
                    [ readmeRequest GetReadmeStatus model.pieceId
                    , pieceCmd
                    ]
               )
        SubmissionStatus (error) ->
            Debug.log (toString error) (model, Cmd.none)
        GetReadmeStatus (Ok readmeString) ->
            ({ model | readme = readmeString }, Cmd.none)
        GetReadmeStatus (error) ->
            (model, Cmd.none)

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

pieceList : Model -> Html Msg
pieceList model =
    div
    [][]

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
