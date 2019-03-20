module Main exposing (main)

import Browser
import Svg
import Svg.Attributes exposing (cx, cy, r, fill, width, height)
import Html exposing (Html)
import Html.Attributes exposing (style, hidden, href, rel)
import Svg.Events exposing (onClick, onMouseOver, onMouseOut)
import Random exposing (Generator)
import Process
import Time
import Task exposing (Task)
import Bitwise
import Maybe.Extra
import Flip
import Html.Events exposing (onClick)
import Bootstrap.Modal as Modal

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
  , gameState: GameState
  , showInstructions: Bool
  }
type alias Board = List Int
type GameState = Choose | Play Player | Winner Player
type Player = Human | Computer

type Msg = 
  HumanMove (Int, Int) 
  | Highlight (Int, Int) 
  | UnHighlight
  | NewBoard Board
  | ComputerMove (Int, Int)
  | GoFirst Player
  | ShowInstructions
  | HideInstructions
  | NewGame

-- init --

init: () -> (Model, Cmd Msg)
init _ = 
  ( { board = []
    , highlight = Maybe.Nothing
    , gameState = Choose
    , showInstructions = False
    }
  , getRandomInitialBoard
  )

getRandomInitialBoard: Cmd Msg
getRandomInitialBoard = 
  Random.generate NewBoard (Random.list numberOfColumns (Random.int 0 maxPerColumn))

numberOfColumns = 5
maxPerColumn = 8

-- update --

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    HumanMove move -> 
      if model.gameState == Play Human then 
        let 
          newBoard = boardAmend move model.board 
          newGameState = if (isBoardEmpty newBoard) then Winner Human else Play Computer
        in 
          ( { model
              | board = newBoard
              , gameState = newGameState 
            }
          , if newGameState == Play Computer then computerPlay newBoard else Cmd.none
          )
      else 
        ( model, Cmd.none)
    Highlight highlight -> ({model | highlight = Maybe.Just highlight}, Cmd.none)
    UnHighlight -> ({model | highlight = Maybe.Nothing}, Cmd.none)
    NewBoard newBoard -> ({model | board = newBoard}, Cmd.none)
    ComputerMove move -> 
      let 
        newBoard = boardAmend move model.board
        newGameState = if (isBoardEmpty newBoard) then Winner Computer else Play Human
      in 
        ( {  model
            | board = newBoard
            , gameState = newGameState
          }
        , Cmd.none
        )
    GoFirst player -> 
      let 
        newGameState = Play player
      in
        ({model | gameState = newGameState}
        , if newGameState == Play Computer then computerPlay model.board else Cmd.none
        )
    ShowInstructions -> ({model | showInstructions = True }, Cmd.none)
    HideInstructions -> ({model | showInstructions = False}, Cmd.none)
    NewGame -> init ()

boardAmend move board = 
  board
    |> List.indexedMap (\x -> \y -> 
          if (x == Tuple.first move) then Tuple.second move
          else y
        )

isBoardEmpty: Board -> Bool
isBoardEmpty board = 
  List.foldr (+) 0 board == 0

  -- computerPlay --

computerPlay : Board -> Cmd Msg
computerPlay board = 
  Process.sleep (2000)
    |> Task.andThen (\_ -> Task.map ComputerMove (computerMove board))
    |> Task.perform identity

computerMove: Board -> Task Never (Int, Int)
computerMove board = 
  board
    |> optimalMoves >> randomPick
    |> Flip.flip Maybe.Extra.or (randomPick <| coordinatesFromBoard board)
    |> Maybe.withDefault (Random.constant (0,0))
    |> taskFromRandom

optimalMoves: Board -> List (Int, Int)
optimalMoves board = 
  let 
    bitsum = List.foldr Bitwise.xor 0 board 
  in 
    board
      |> List.indexedMap (\x -> \y -> 
          let newy = Bitwise.xor y bitsum in 
            if (newy < y) then 
              Maybe.Just (x, newy) 
            else 
              Maybe.Nothing
          )
      |> List.map (Maybe.Extra.toList) >> List.concat

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
view model = 
  Html.div [style "padding-left" "10px"]
    [ viewMenu
    , [instructionsModal] |> Html.div [hidden (model.showInstructions == False)]
    , showGameState model.gameState
    , viewBoard model
    , [viewQuestion] |> Html.div [hidden (model.gameState /= Choose)]
    ]

  -- viewMenu --

viewMenu : Html Msg
viewMenu = 
  Html.div []
    [ Html.h3 [style "padding-top" "10px"] [Html.text "Nim"]
    , Html.button [onClick ShowInstructions] [Html.text "How to Play"]
    , Html.button [onClick NewGame] [Html.text "New Game"]
    ]

  -- instructionsModal --

instructionsModal: Html Msg
instructionsModal = 
  Html.div []
    [ Html.node "link" [ href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css", rel "stylesheet" ] []
    , Modal.config HideInstructions
        |> Modal.h3 [] [Html.text "How to Play"]
        |> Modal.body [] [instructions]
        |> Modal.view Modal.shown
    ]

instructions: Html Msg 
instructions = 
  [ """The game board consists of counters arranged into columns. 
    You take it in turns with the computer to remove as many counters as you like from a single column. 
    The aim of the game is to remove the last counter."""
  , "You may choose if you would like to go first or second."
  ] 
    |> List.map (\paragraph -> Html.p [] [ Html.text paragraph])
    |> Html.div []

  -- showGameState --

showGameState : GameState -> Html Msg
showGameState gameState = 
  let 
    message = case gameState of
      Choose -> "Choose who goes first"
      Play Human -> "Your turn"
      Play Computer -> "Computer's turn" 
      Winner Human -> "You win!"
      Winner Computer -> "You lose :("
  in 
    Html.h5 
      [style "padding-top" "20px", style "padding-bottom" "10px"]
      [Html.text message]

  -- viewBoard --

viewBoard : Model -> Html Msg
viewBoard = boardViewFromModel >> displayBoard

    -- boardViewFromModel --

boardViewFromModel: Model -> BoardView
boardViewFromModel model = 
  coordinatesFromBoard model.board
    |> withHighlights model.gameState model.highlight

coordinatesFromBoard board = 
  board
    |> List.indexedMap (\x -> \y -> 
          List.range 0 (y-1) |> List.map (Tuple.pair x)
        )
    |> List.concat

withHighlights gameState highlight coordinates = 
  coordinates
    |> List.map (\coordinate -> Tuple.pair coordinate (shouldHighlightCoordinate gameState highlight coordinate))

shouldHighlightCoordinate gameState highlight (x, y) =
  case gameState of
    Play Human -> case highlight of
      Maybe.Just (xh, yh) -> x == xh && y >= yh
      Maybe.Nothing -> False
    _ -> False
      
    -- displayBoard --

displayBoard: BoardView -> Html Msg
displayBoard boardView =
  Svg.svg 
    [ width (String.fromInt(boardWidth boardView)), height (String.fromInt(boardHeight boardView))]
    (List.map circleFromCoordinateAndHighlight boardView)

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
boardWidth boardView = (Maybe.withDefault 0 (List.maximum (List.map (\x -> Tuple.first (Tuple.first x)) boardView)) + 1) * circleDimension
boardHeight boardView = (Maybe.withDefault 0 (List.maximum (List.map (\x -> Tuple.second (Tuple.first x)) boardView)) + 1) * circleDimension
svgCoordinate x = String.fromInt((circleRadius + circlePadding) * (1 + 2 * x))

circleRadius = 20
circlePadding = 5
blueColour = "#0B79CE"
blueHighlight = "#84c1ff"

    -- 

type alias BoardView = List ((Int, Int), Bool)

  -- viewQuestion --

viewQuestion: Html Msg
viewQuestion =
  Html.div [style "padding-top" "10px", style "padding-bottom" "10px"]
    [ Html.text "Would you like to go first or second?"
    , Html.div [style "padding-top" "5px"]
      [ Html.button [onClick (GoFirst Human)] [Html.text "First"]
      , Html.button [onClick (GoFirst Computer)] [Html.text "Second"]
      ]
    ]

-- notes --

-- would be really nice to have a haskell `where`, that would read more naturally
-- also annoying that you can't define types within lets
-- need to be able to use lambda functions without brackets
-- in haskell is it ok to use parameter names that are already bound?
-- would be good if case classes didn't require commas
-- it should be more like a state machine in that certain messages can only be triggered in certain states
-- why do the two exposed onClick methods not clash?
-- have experienced the first time the dom rendering affects the logic - when the dom re-renders a circle it doesn't retrigger an onMouseOver event
-- could you use a subscription to subscribe to changes in a model e.g. create a ComputersTurn msg every time the model gameState changes to Play Computer, so we can centralise the computer play logic
-- I prefer Scala's Some and None to Just and Nothing
  






    








