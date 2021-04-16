module Main exposing (..)

import Html exposing (Html, text, div, h1, h2, img, button, option)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick, onMouseOver)
import Time exposing (Time, millisecond)
import Array exposing (..)
import Maybe

---- MODEL ----

type Cell = Dead | Alive
type alias Matrix a = Array (Array a)

type alias Model = { paused : Bool, board : Matrix Cell, hoverMode : Bool }

isAlive cell = case cell of
    Dead -> False
    Alive -> True

count : (a -> Bool) -> List a -> Int
count condition = List.length << List.filter condition

next cell neighbors = let aliveNeighbors = count isAlive neighbors in
    case cell of
        Alive -> if aliveNeighbors < 2 then Dead
            else if aliveNeighbors < 4 then Alive
            else                            Dead

        Dead -> if aliveNeighbors == 3 then Alive
            else                            Dead


matrixGet : Int -> Int -> Matrix a -> Maybe a
matrixGet i j = Array.get i >> Maybe.andThen (Array.get j)

matrixSet : Int -> Int -> a -> Matrix a -> Matrix a
matrixSet i j value matrix = case Maybe.map (Array.set j value) (Array.get i matrix) of
    Just newRow -> Array.set i newRow matrix
    Nothing -> matrix

minusOne : Int -> Int
minusOne x = x - 1

plusOne : Int -> Int
plusOne x = x + 1

same : Int -> Int
same = identity

type PositionOperation = MinusOne | PlusOne | Same

runPositionOperation op x = case op of
    MinusOne -> x - 1
    PlusOne -> x + 1
    Same -> x

positionOperations : List PositionOperation
positionOperations = [MinusOne, PlusOne, Same]

directions : List (Int->Int, Int->Int)
directions = List.map (\(a,b) -> (runPositionOperation a, runPositionOperation b)) <|
                List.filter (\(a,b) -> (a,b) /= (Same, Same)) <|
                List.concatMap (\x -> List.map (\y -> (x, y)) positionOperations) positionOperations

surroundingCellsForPosition : Int -> Int -> Matrix a -> List a
surroundingCellsForPosition i j matrix = 
    List.map (\(f,g) -> (f i, g j)) directions |>
        List.filterMap (\(newI, newJ) -> matrixGet newI newJ matrix)

surroundingCellsForMatrix : Matrix a -> Matrix (a, List a)
surroundingCellsForMatrix matrix = Array.indexedMap (\i fila ->
    Array.indexedMap (\j valor -> (valor, surroundingCellsForPosition i j matrix)) fila) matrix

nextBoard : Matrix Cell -> Matrix Cell
nextBoard = matrixMap (\(cell, neighbors) -> next cell neighbors) << surroundingCellsForMatrix

squareMatrixFilledWith : Int -> a -> Matrix a
squareMatrixFilledWith n value = Array.repeat n (Array.repeat n value)

flipCellAtPosition : (Int, Int) -> Matrix Cell -> Matrix Cell
flipCellAtPosition (i, j) matrix = case matrixGet i j matrix of
    Just cell -> matrixSet i j (flipCell cell) matrix
    Nothing -> matrix

flipCell : Cell -> Cell
flipCell cell = case cell of
    Alive -> Dead
    Dead -> Alive

matrixMap : (a -> b) -> Matrix a -> Matrix b
matrixMap f = Array.map (Array.map f)

init : ( Model, Cmd Msg )
init = ( { board = squareMatrixFilledWith 20 Dead, paused = True, hoverMode = False }, Cmd.none )



---- UPDATE ----

type Msg = Tick | Change (Int, Int) | Pause | ToggleHoverMode | Hover (Int, Int)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let newModel = case msg of
            Tick -> { model | board = if model.paused then model.board else nextBoard model.board }
            Change position -> { model | board = flipCellAtPosition position model.board }
            Pause -> { model | paused = not model.paused }
            ToggleHoverMode -> { model | hoverMode = not model.hoverMode }
            Hover position -> { model | board = if model.hoverMode then flipCellAtPosition position model.board else model.board }
    in (newModel, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Time.every (millisecond * 500) (\_ -> Tick)


---- VIEW ----

viewCell : Bool -> Cell -> Html Msg
viewCell paused cell = case (cell, paused) of
    (Alive, True) -> square "Green"
    (Alive, False) -> square "LightGreen"
    (Dead, True) -> square "Gray"
    (Dead, False) -> square "AliceBlue"

square : String -> Html Msg
square color = div [style [("height", "40px"), ("width", "40px"), ("background", color)]] []

table : Matrix (Html Msg) -> Html Msg
table = div [verticalFlexStyle] <<
            Array.toList << Array.indexedMap (\i row -> div [horizontalFlexStyle]
                (Array.toList <| addListenerToChangeCell i <| row))

verticalFlexStyle = flexStyle "column"
horizontalFlexStyle = flexStyle "row"
flexStyle direction = style [("display", "flex"), ("flex-direction", direction)]

addListenerToChangeCell : Int -> Array (Html Msg) -> Array (Html Msg)
addListenerToChangeCell i = Array.indexedMap (\j value -> div [onClick <| Change (i,j), onMouseOver <| Hover (i,j)] [value])

pauseButtonText paused = if paused then "RESUME" else "PAUSE"

rules = div [verticalFlexStyle] <| List.map (\line -> div [style [("fontSize", "x-large")]] [text line]) [
                                        "Every 0.5 seconds, the following transitions occur:",
                                        "Any live cell with fewer than two live neighbors dies, as if by under population.",
                                        "Any live cell with two or three live neighbors lives on to the next generation.",
                                        "Any live cell with more than three live neighbors dies, as if by overpopulation.",
                                        "Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction."]

view : Model -> Html Msg
view model = div [horizontalFlexStyle] [
    div [verticalFlexStyle, style [("font-family", "monospace"), ("alignItems", "center")]] [
                     h1 [] [text "Conway's Game of Life!"],
                     rules,
                     h2 [style [("height", "10px")]] [text <| "You can flip cells with the mouse, either by clicking or hovering"],
                     h2 [style [("height", "10px")]] [text <| "Start by flipping some cells and then resuming the game!"],
                     div [verticalFlexStyle, style [("width", "100%")]] [
                         button [onClick ToggleHoverMode] [text <| if model.hoverMode then "Flip cells on Hover ON" else "Flip cells on Hover OFF"],
                         button [style [("height", "10vh")], onClick Pause] <| [text <| pauseButtonText model.paused]
                     ]
    ],
                     table <| matrixMap (viewCell model.paused) model.board]

---- PROGRAM ----

main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
