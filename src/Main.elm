import Browser
import Svg
import Svg.Attributes exposing (..)
import Html
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
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


circleBoard model = Html.div [] (List.indexedMap circleColumn model)

circleColumn x n = Html.div [style "float" "left", style "min-height" "10px", style "width" "50px"] (List.map (circleElement x) (List.range 0 (n-1)))

circleElement x y = Svg.svg [viewBox "0 0 50 50", width "50px", style "display" "block"] [Svg.circle [onClick (x,y), cx "25", cy "25", r "20", fill "#0B79CE" ] []]


boardAmend move board = 
  List.indexedMap (\n -> \x -> if (n == Tuple.first move) then Tuple.second move else x) board
