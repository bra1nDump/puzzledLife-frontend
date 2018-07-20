port module Piece exposing
    ( Piece
    , Frame
    , Msg
    , model
    , update
    , render
    , subscriptions
    )

import Css exposing (..)
import Css.Colors exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css, href, src)

import Canvas exposing (Size, Point, Canvas, Style(Color), DrawOp(..))
import Color exposing (Color)

import Pointer

type alias Frame =
    { x1 : Int
    , y1 : Int
    , x2 : Int
    , y2 : Int
    }

type alias Piece =
    { url : String
    , size : (Int, Int)
    , isSelectingFrame : Bool
    , frames : List Frame
    }

type Msg =
    DimensionsResponse (Int, Int)
    | DownMsg (Float, Float)
    | MoveMsg (Float, Float)
    | UpMsg (Float, Float)

-- init

model : (Msg -> msg) -> String -> (Piece, Cmd msg)
model lift url =
    (
         { url = url
         , size = (0,0)
         , isSelectingFrame = False
         , frames = []
         }
    , dimensionsRequest url
    )

update : Msg -> Piece -> (Piece, Cmd msg)
update msg piece =
    case msg of
        DownMsg (x, y) ->
            ({ piece | frames =
                   Frame (x |> floor) (y |> floor) (x |> floor) (y |> floor)
                   :: piece.frames
             , isSelectingFrame = True
             }
            , Cmd.none)
        MoveMsg (x, y) ->
            (if piece.isSelectingFrame then
                 { piece | frames =
                       case piece.frames of
                           {x1, y1} :: tail ->
                               Frame x1 y1 (x |> floor) (y |> floor)
                               :: tail
                           _ -> piece.frames
                 }
             else piece
            , Cmd.none
            )
        UpMsg (x, y) ->
            let frames =
                    case piece.frames of
                        { x1, y1 } :: tail ->
                           Frame x1 y1 (x |> floor) (y |> floor)
                               :: tail
                        _ ->
                            piece.frames
                nonZeroFrames =
                    frames |> List.filter
                              (\frame ->
                                   let {x1, y1, x2, y2} = frame
                                   in (x1 /= x2) && (y1 /= y2)
                              )
            in
                ({ piece | frames = nonZeroFrames
                 , isSelectingFrame = False
                 }
                , Cmd.none
                )
        DimensionsResponse dimensions ->
            ({ piece | size = dimensions }
            , Cmd.none
            )

-- view

-- canvas
relativePos : Pointer.Event -> ( Float, Float )
relativePos event =
    event.pointer.offsetPos

drawRectangles : List Frame -> DrawOp
drawRectangles frames =
    let drawFrame frame =
            let
                {x1, y1, x2, y2} = frame
                frameWidth = x2 - x1
                frameHeight = y2 - y1
            in rectangle (Point (toFloat x1) (toFloat y1)) frameWidth frameHeight (Color.rgba 0 0 255 0.5)
        operationList = List.map drawFrame frames
    in Canvas.batch operationList

regions : Piece -> DrawOp
regions piece =
        [ piece.frames |> drawRectangles
        , FillStyle <| Color Color.white
        ] |> Canvas.batch

rectangle : Point -> Int -> Int -> Color.Color -> DrawOp
rectangle point width height color =
    [ BeginPath
    , Rect point (Size width height)
    , FillStyle <| Color color
    , Fill
    ] |> Canvas.batch

render : (Msg -> msg) -> Piece -> Html msg
render toParentMsg piece =
    let
        (width, height) = piece.size
    in
        map toParentMsg <|
        div [ css [ display inlineBlock
                  , position relative
                  , Css.width (px <| toFloat width)
                  , Css.height (px <| toFloat height)
                  , margin (pct 1)
                  , borderStyle solid
                  , borderRadius (px 5)
                  ]
            , Attributes.fromUnstyled <| Pointer.onDown (relativePos >> DownMsg)
            , Attributes.fromUnstyled <| Pointer.onMove (relativePos >> MoveMsg)
            , Attributes.fromUnstyled <| Pointer.onUp (relativePos >> UpMsg)
            ]
        [ img [ css
                [ position absolute
                , backgroundSize contain
                , zIndex (int 1)
                ]
              , src piece.url
              ][]
        , div
              [ css
                [ position absolute
                , zIndex (int 20)
                ]
              ]
              (if (width,height) == (0,0)
               then []
               else
                   [ Canvas.initialize (Size width height)
                   |> Canvas.draw (regions piece)
                   |> Canvas.toHtml []
                   |> fromUnstyled
                   ]
              )
        ]

-- subscriptions
subscriptions : (Msg -> msg) -> Sub msg
subscriptions toParentMsg =
    dimensionsResponse <| DimensionsResponse >> toParentMsg

-- we will send request for image dimensions
-- through this port
port dimensionsRequest : String -> Cmd msg
port dimensionsResponse : ((Int, Int) -> msg) -> Sub msg
