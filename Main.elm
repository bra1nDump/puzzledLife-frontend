import Css exposing (..)
import Css.Colors exposing(..)
--import Css.Foreign exposing(canvas)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css, href, src)
import Svg.Styled.Attributes exposing (z)
-- import Html.Styled.Events exposing (onClick)

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
    , readme : String
    , frame : Frame
    , mdl : Material.Model
    }


init : String -> (Model, Cmd Msg)
init firstPiece =
    let
        initModel = Model
                    (firstPiece)
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
            ({ model | readme = readmeString }, Cmd.none)
        GetReadmeStatus (error) ->
            Debug.log (toString error) (model, Cmd.none)
        DownMsg (x, y) ->
            Debug.log (toString (x,y))
                ({model | frame =
                      let
                          {x2, y2} = model.frame
                      in Frame (x |> floor) (y |> floor) x2 y2
                 }, Cmd.none)
        MoveMsg (x, y) ->
            Debug.log (toString (x,y))
                ({model | frame =
                      let
                          {x1, y1} = model.frame
                      in Frame x1 y1 (x |> floor) (y |> floor)
                 }, Cmd.none)
        UpMsg (x, y) ->
            Debug.log (toString (x,y))
                ({model | frame =
                      let
                          {x1, y1} = model.frame
                      in Frame x1 y1 (x |> floor) (y |> floor)
                 }, Cmd.none)
        Mdl (message_) ->
            Material.update Mdl message_ model

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

-- viewSubmissionCard : Model -> Html Msg
-- viewSubmissionCard model = div [] []

relativePos : Pointer.Event -> ( Float, Float )
relativePos event =
    event.pointer.offsetPos

view : Model -> Html Msg
view model =
    let
        {x1, y1, x2, y2} = model.frame
        frameWidth = abs (x2 - x1)
        frameHeight = abs (y2 - y1)
    in
        div
        [ css
          [ position relative
          , display inlineBlock
          ]
        ]
        [ h1 [] [ text model.pieceID ]
          , div
          [ css
            [
             position absolute
            , float left
            ]
          ]
          [ img [ src (baseUrl ++ "/puzzles/gallaxy/" ++ model.pieceID ++ "/image.jpg")
                , css
                  [ --display inlineBlock
                  position absolute
                  
                  -- , width (px 400)
                  -- , height (px 400)
                  ]
                , Attributes.fromUnstyled <| Pointer.onDown (relativePos >> DownMsg)
                , Attributes.fromUnstyled <| Pointer.onMove (relativePos >> MoveMsg)
                , Attributes.fromUnstyled <| Pointer.onUp (relativePos >> UpMsg)
                , z "1"
                ]
                []
          , canvas
                [ css
                  [ position relative
                  , top (px <| toFloat x1)
                  , left (px <| toFloat y1)
                  , width (px <| toFloat frameWidth)
                  , height (px <| toFloat frameHeight)
                  , color blue
                  , backgroundColor blue
                  ]
                , z "20"
                ]
                []
          ]
        , Markdown.toHtml [] model.readme |> fromUnstyled
        -- , Button.render Mdl [0] model.mdl
        --     [ Options.onClick SubmitSolution ]
        --     [ text "submit solution" ] >> fromUnstyled
        ]


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

