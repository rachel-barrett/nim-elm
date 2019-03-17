import Browser
import Svg
import Svg.Attributes exposing (cx, cy, r, fill, width, height)
import Html
import Html.Attributes exposing (style)
import Svg.Events exposing (onClick)

main = 
  Browser.sandbox {
    init = init, 
    update = update, 
    view = view
  }

type alias Model = Board
type alias Board = List Int

type alias Msg = Move
type alias Move = (Int, Int)

init = [5,2,0,4]

update move board = boardAmend move board

view board =  circleBoard board


boardAmend move board = 
  List.indexedMap (\n -> \x -> if (n == Tuple.first move) then Tuple.second move else x) board


circleBoard board = Svg.svg [svgWidth board, svgHeight board] (List.map circleFromCoordinate (coordinatesFromBoard board))

circleFromCoordinate (x, y) = Svg.circle [onClick (x, y), cx (svgCoordinate x), cy (svgCoordinate y), r (String.fromInt(circleRadius)), fill blueColour ] []

coordinatesFromBoard board = List.concat (List.indexedMap (\x -> \n -> (List.map (Tuple.pair x) (List.range 0 (n-1)))) board)


circleRadius = 20
circlePadding = 5
blueColour = "#0B79CE"
circleDimension = 2 * (circleRadius + circlePadding)
boardWidth board = (List.length board) * circleDimension
boardHeight board = (Maybe.withDefault 0 (List.maximum board)) * circleDimension
svgWidth board = width (String.fromInt(boardWidth board))
svgHeight board = height (String.fromInt(boardHeight board))
svgCoordinate x = String.fromInt((circleRadius + circlePadding) * (1 + 2 * x))

