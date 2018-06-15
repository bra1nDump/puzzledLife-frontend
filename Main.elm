import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Http

baseUrl : String
baseUrl = "http://localhost:8080/puzzles/braindump/"

main : Program Never Model Msg
main =
    Html.program
        { init = init "100" -- default hash
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
    , solution : Frame
    }


init : String -> (Model, Cmd Msg)
init firstPiece =
    (Model (firstPiece ++ ".jpg") (Frame "" "" "" ""), Cmd.none)

type Msg =
    CoordinateChanged CoordinateID String
    | SubmitSolution
    | SubmissionStatus (Result Http.Error String)

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
            ({ model | pieceID = nextPieceID }, Cmd.none)
        SubmissionStatus (Err _) ->
            (model, Cmd.none)

submitSolution : Model -> Cmd Msg
submitSolution model =
    let
        solution = model.solution
        url = baseUrl
              ++ model.pieceID ++ "/passage?"
              ++ "x1=" ++ solution.x1
              ++ "&y1=" ++ solution.y1
              ++ "&x2=" ++ solution.x2
              ++ "&y2=" ++ solution.y2
    in Http.send SubmissionStatus
        <| Http.getString url

view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.pieceID ]
        , img [ src (baseUrl ++ model.pieceID) ] []
        , input [ type_ "text", placeholder "x1", onInput (CoordinateChanged X1) ] []
        , input [ type_ "text", placeholder "y1", onInput (CoordinateChanged Y1) ] []
        , input [ type_ "text", placeholder "x2", onInput (CoordinateChanged X2) ] []
        , input [ type_ "text", placeholder "y2", onInput (CoordinateChanged Y2) ] []
        , button [ onClick SubmitSolution ] [ text "submit solution" ]
        ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

