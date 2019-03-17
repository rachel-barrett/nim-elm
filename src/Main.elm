import Browser
import Svg
import Svg.Attributes exposing (cx, cy, r, fill, width, height)
import Html
import Html.Attributes exposing (style)
import Svg.Events exposing (onClick, onMouseOver)

main = 
  Browser.sandbox {
    init = init, 
    update = update, 
    view = view
  }

type alias Model = {
    board: Board,
    highlight: Maybe (Int, Int)
  }
type alias Board = List Int

type Msg = Select (Int, Int) | Highlight (Int, Int)

init = {
  board = [5,2,0,4],
  highlight = Maybe.Nothing
  }

update msg model = 
  case msg of
    Select move -> {model | board = boardAmend move model.board}
    Highlight highlight -> {model | highlight = Just highlight}


view model =  circleBoard model


boardAmend move board = 
  List.indexedMap (\n -> \x -> if (n == Tuple.first move) then (Tuple.second move) else x) board


circleBoard model = Svg.svg [svgWidth model.board, svgHeight model.board] (List.map (circleFromCoordinate model.highlight) (coordinatesFromBoard model.board))

circleFromCoordinate highlight (x, y) = Svg.circle [onClick (Select (x, y)), onMouseOver (Highlight (x, y)), cx (svgCoordinate x), cy (svgCoordinate y), r (String.fromInt(circleRadius)), fill (if (shouldHighlightCoordinate highlight (x,y)) then blueHighlight else blueColour) ] []

coordinatesFromBoard board = List.concat (List.indexedMap (\x -> \n -> (List.map (Tuple.pair x) (List.range 0 (n-1)))) board)

shouldHighlightCoordinate highlight (x, y) = Maybe.withDefault False (Maybe.map (\(xh, yh) -> x == xh && y >= yh) highlight)

circleRadius = 20
circlePadding = 5
blueColour = "#0B79CE"
blueHighlight = "#84c1ff"
circleDimension = 2 * (circleRadius + circlePadding)
boardWidth board = (List.length board) * circleDimension
boardHeight board = (Maybe.withDefault 0 (List.maximum board)) * circleDimension
svgWidth board = width (String.fromInt(boardWidth board))
svgHeight board = height (String.fromInt(boardHeight board))
svgCoordinate x = String.fromInt((circleRadius + circlePadding) * (1 + 2 * x))

