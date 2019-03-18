import Browser
import Svg
import Svg.Attributes exposing (cx, cy, r, fill, width, height)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg.Events exposing (onClick, onMouseOver, onMouseOut)
import Random

main = 
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- data model --

type alias Model = 
  { board: Board
  , highlight: Maybe (Int, Int)
  }
type alias Board = List Int

type Msg = 
  Select (Int, Int) 
  | Highlight (Int, Int) 
  | UnHighlight
  | NewBoard Board

-- init --

init: () -> (Model, Cmd Msg)
init _ = 
  ( { board = []
    , highlight = Maybe.Nothing
    }
  , Random.generate NewBoard (Random.list numberOfColumns (Random.int 0 maxPerColumn))
  )

numberOfColumns = 5
maxPerColumn = 8

-- update --

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( case msg of
      Select move -> {model | board = boardAmend move model.board}
      Highlight highlight -> {model | highlight = Maybe.Just highlight}
      UnHighlight -> {model | highlight = Maybe.Nothing}
      NewBoard newBoard -> {model | board = newBoard}
  , Cmd.none
  )

boardAmend move board = 
  board
    |> List.indexedMap (\n -> \x -> if (n == Tuple.first move) then (Tuple.second move) else x)    

-- subscriptions --

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none

-- view --

view: Model -> Html Msg
view = modelViewFromModel >> display

  -- modelViewFromModel --

modelViewFromModel: Model -> ModelView
modelViewFromModel model = 
  coordinatesFromBoard model.board
    |> withHighlights model.highlight

coordinatesFromBoard board = 
  board
    |> List.indexedMap (\x -> \n -> (List.map (Tuple.pair x) (List.range 0 (n-1))))
    |> List.concat

withHighlights highlight coordinates = 
  coordinates
    |> List.map (\coordinate -> Tuple.pair coordinate (shouldHighlightCoordinate highlight coordinate))

shouldHighlightCoordinate highlight (x, y) = 
  case highlight of
    Maybe.Just (xh, yh) -> x == xh && y >= yh
    Maybe.Nothing -> False 
      
  -- display --

display: ModelView -> Html Msg
display modelView =
  Svg.svg 
    [ width (String.fromInt(boardWidth modelView)), height (String.fromInt(boardHeight modelView))]
    (List.map circleFromCoordinateAndHighlight modelView)

circleFromCoordinateAndHighlight ((x, y), highlight) = 
  Svg.circle 
    [ onClick (Select (x, y)) 
    , onMouseOver (Highlight (x, y)) 
    , onMouseOut UnHighlight
    , cx (svgCoordinate x)
    , cy (svgCoordinate y) 
    , r (String.fromInt(circleRadius)) 
    , fill (if (highlight) then blueHighlight else blueColour) 
    ] []

circleDimension = 2 * (circleRadius + circlePadding)
boardWidth modelView = (Maybe.withDefault 0 (List.maximum (List.map (\x -> Tuple.first (Tuple.first x)) modelView)) + 1) * circleDimension
boardHeight modelView = (Maybe.withDefault 0 (List.maximum (List.map (\x -> Tuple.second (Tuple.first x)) modelView)) + 1) * circleDimension
svgCoordinate x = String.fromInt((circleRadius + circlePadding) * (1 + 2 * x))

circleRadius = 20
circlePadding = 5
blueColour = "#0B79CE"
blueHighlight = "#84c1ff"

  -- 

type alias ModelView = List ((Int, Int), Bool)

-- notes --

-- would be really nice to have a haskell where, that would read more naturally
-- also annoying that you can't define types within lets
-- need to be able to use lambda functions without brackets
-- in haskell is it ok to use parameter names that are already bound?
-- would be good if case classes didn't require commas
  






    








