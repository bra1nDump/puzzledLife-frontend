import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Http
import Debug
import Markdown

baseUrl : String
baseUrl = "http://localhost:8000"

main : Program Never Model Msg
main =
    Html.program
        { init = init "4760" -- default hash
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type CoordinateID =
    X1
    | Y1
    | X2
    | Y2

type alias Frame =
    { x1 : String
    , y1 : String
    , x2 : String
    , y2 : String
    }

type alias Model =
    { pieceID : String
    , readme : String
    , solution : Frame
    }


init : String -> (Model, Cmd Msg)
init firstPiece =
    let
        initModel = Model (firstPiece) "" (Frame "" "" "" "")
    in (initModel, getReadme initModel)

type Msg =
    CoordinateChanged CoordinateID String
    | SubmitSolution
    | SubmissionStatus (Result Http.Error String)
    | GetReadmeStatus (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CoordinateChanged id value ->
            let solution = model.solution
                newSolution =
                    case id of
                        X1 -> { solution | x1 = value }
                        Y1 -> { solution | y1 = value }
                        X2 -> { solution | x2 = value }
                        Y2 -> { solution | y2 = value }
            in ({ model | solution = newSolution }, Cmd.none)
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

submitSolution : Model -> Cmd Msg
submitSolution model =
    let
        solution = model.solution
        url = baseUrl ++ "/link/gallaxy/"
              ++ model.pieceID
              ++ "?x1=" ++ solution.x1
              ++ "&y1=" ++ solution.y1
              ++ "&x2=" ++ solution.x2
              ++ "&y2=" ++ solution.y2
    in Http.send SubmissionStatus
        <| Http.getString url

getReadme : Model -> Cmd Msg
getReadme model =
    let
        url = baseUrl ++ "/puzzles/gallaxy/"
              ++ model.pieceID ++ "/readme.md"
    in Http.send GetReadmeStatus
        <| Http.getString url

view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.pieceID ]
        , img [ src (baseUrl ++ "/puzzles/gallaxy/" ++ model.pieceID ++ "/image.jpg") ] []
        , Markdown.toHtml [class "content"] model.readme
        , input [ type_ "text", placeholder "x1", onInput (CoordinateChanged X1) ] []
        , input [ type_ "text", placeholder "y1", onInput (CoordinateChanged Y1) ] []
        , input [ type_ "text", placeholder "x2", onInput (CoordinateChanged X2) ] []
        , input [ type_ "text", placeholder "y2", onInput (CoordinateChanged Y2) ] []
        , button [ onClick SubmitSolution ] [ text "submit solution" ]
        ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

