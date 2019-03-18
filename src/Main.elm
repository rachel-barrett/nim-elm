module Main exposing (..)

import Browser
import Svg
import Svg.Attributes exposing (cx, cy, r, fill, width, height)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg.Events exposing (onClick, onMouseOver, onMouseOut)
import Random exposing (Generator)
import Process
import Time
import Task exposing (Task)
import Bitwise
import Maybe.Extra
import Flip

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
  , currentPlayer: Player
  }
type alias Board = List Int
type Player = Human | Computer

type Msg = 
  HumanMove (Int, Int) 
  | Highlight (Int, Int) 
  | UnHighlight
  | NewBoard Board
  | ComputerMove (Int, Int)

-- init --

init: () -> (Model, Cmd Msg)
init _ = 
  ( { board = []
    , highlight = Maybe.Nothing
    , currentPlayer = Human
    }
  , Random.generate NewBoard (Random.list numberOfColumns (Random.int 0 maxPerColumn))
  )

numberOfColumns = 5
maxPerColumn = 8

-- update --

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    HumanMove move -> 
      if model.currentPlayer /= Human
        then (model, Cmd.none)
        else let newBoard = boardAmend move model.board in ({model | board = newBoard, currentPlayer = Computer }, computerMove newBoard)
    Highlight highlight -> ({model | highlight = Maybe.Just highlight}, Cmd.none)
    UnHighlight -> ({model | highlight = Maybe.Nothing}, Cmd.none)
    NewBoard newBoard -> ({model | board = newBoard}, Cmd.none)
    ComputerMove move -> ({model | board = boardAmend move model.board, currentPlayer = Human}, Cmd.none)

boardAmend move board = 
  board
    |> List.indexedMap (\n -> \x -> if (n == Tuple.first move) then (Tuple.second move) else x)

computerMove : Board -> Cmd Msg
computerMove board = 
  Process.sleep (1000)
    |> Task.andThen (\_ -> Task.map ComputerMove (optimalMove board))
    |> Task.perform identity

optimalMove: Board -> Task Never (Int, Int)
optimalMove board = 
  let bitsum = List.foldr Bitwise.xor 0 board in 
    board
      |> List.indexedMap (\x -> \y -> 
          let newy = Bitwise.xor y bitsum in 
            if (newy < y) then Maybe.Just (x, newy) else Maybe.Nothing
          )
      |> List.map (Maybe.Extra.toList) >> List.concat
      |> randomPick
      |> Flip.flip Maybe.Extra.or (randomPick <| coordinatesFromBoard board)
      |> Maybe.withDefault (Random.constant (0,0))
      |> taskFromRandom

randomPick: List a -> Maybe (Generator a)
randomPick list = 
  case list of
    head :: tail -> Maybe.Just (Random.uniform head tail)
    _ -> Maybe.Nothing

taskFromRandom: Generator a -> Task Never a
taskFromRandom generator = 
  Time.now |> Task.map (Time.posixToMillis >> Random.initialSeed >> Random.step generator >> Tuple.first)


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
    |> withHighlights model.currentPlayer model.highlight

coordinatesFromBoard board = 
  board
    |> List.indexedMap (\x -> \n -> (List.map (Tuple.pair x) (List.range 0 (n-1))))
    |> List.concat

withHighlights currentPlayer highlight coordinates = 
  coordinates
    |> List.map (\coordinate -> Tuple.pair coordinate (shouldHighlightCoordinate currentPlayer highlight coordinate))

shouldHighlightCoordinate currentPlayer highlight (x, y) =
  case currentPlayer of
    Human -> case highlight of
      Maybe.Just (xh, yh) -> x == xh && y >= yh
      Maybe.Nothing -> False
    Computer -> False
      
  -- display --

display: ModelView -> Html Msg
display modelView =
  Svg.svg 
    [ width (String.fromInt(boardWidth modelView)), height (String.fromInt(boardHeight modelView))]
    (List.map circleFromCoordinateAndHighlight modelView)

circleFromCoordinateAndHighlight ((x, y), highlight) = 
  Svg.circle 
    ( [ cx (svgCoordinate x)
      , cy (svgCoordinate y) 
      , r (String.fromInt(circleRadius)) 
      , fill (if (highlight) then blueHighlight else blueColour)
      , onClick (HumanMove (x, y)) 
      , onMouseOver (Highlight (x, y)) 
      , onMouseOut UnHighlight     
      ]      
    ) []

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

-- one issue we have is that the event is only triggered on entring the circle
  






    








