import Browser
import Svg
import Svg.Attributes exposing (..)
import Html
import Html.Attributes exposing (style)

main = 
  Browser.sandbox {
    init = init, 
    update = update, 
    view = view
  }

type alias Model = List Int
type alias Msg = (Int, Int)

init = [1,2,0,4]

update msg model = model

view model =  circleBoard model

circleBoard model = Html.div [] (List.map circleColumn model)

circleColumn n = Html.div [style "float" "left", style "min-height" "10px", style "min-width" "50px"] (List.repeat n circleElement)

circleElement = Svg.svg [viewBox "0 0 50 50", width "50px", style "display" "block"] [Svg.circle [cx "25", cy "25", r "20", fill "#0B79CE" ] []]