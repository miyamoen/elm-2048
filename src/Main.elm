module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { board : Board
    , score : Int
    , state : GameState
    }


type GameState
    = Playing
    | Won
    | Over


type alias Board =
    Array Row


type alias Row =
    Array Cell


type Cell
    = Tile Int
    | Empty


type alias Position =
    ( Int, Int )


type Direction
    = Left
    | Right
    | Up
    | Down


init : () -> ( Model, Cmd Msg )
init _ =
    let
        emptyBoard =
            Array.repeat 4 <| Array.repeat 4 <| Empty
    in
    ( { board = emptyBoard
      , score = 0
      , state = Playing
      }
    , randomPositionedTiles 2 emptyBoard
        |> Random.generate PutMany
    )



-- UPDATE


type Msg
    = Slide Direction
    | Put ( Position, Cell )
    | PutMany (List ( Position, Cell ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Slide direction ->
            let
                ( slidedBoard, increase ) =
                    slideBoard direction model.board

                cmd =
                    if model.board == slidedBoard then
                        Cmd.none

                    else
                        randomPosition slidedBoard
                            |> Maybe.map (\position -> Random.pair position randomTile)
                            |> Maybe.map (Random.generate Put)
                            |> Maybe.withDefault Cmd.none
            in
            ( { model
                | board = slidedBoard
                , score = model.score + increase
              }
            , cmd
            )

        Put ( position, cell ) ->
            let
                newBoard =
                    setBoard position cell model.board
            in
            ( { model
                | board = newBoard
                , state =
                    if stuck newBoard then
                        Over

                    else if won newBoard then
                        Won

                    else
                        Playing
              }
            , Cmd.none
            )

        PutMany list ->
            ( { model
                | board = List.foldl (\( position, cell ) -> setBoard position cell) model.board list
              }
            , Cmd.none
            )


setBoard : Position -> Cell -> Board -> Board
setBoard ( i, j ) cell board =
    Array.get i board
        |> Maybe.map (\oldRow -> Array.set j cell oldRow)
        |> Maybe.map (\newRow -> Array.set i newRow board)
        |> Maybe.withDefault board


type Accumulator
    = Waiting Int (List Int) Int
    | Done (List Int) Int


accumulate : Int -> Accumulator -> Accumulator
accumulate cell acc =
    case acc of
        Waiting waiting done score ->
            if waiting == cell then
                Done (cell * 2 :: done) (score + cell * 2)

            else
                Waiting cell (waiting :: done) score

        Done done score ->
            Waiting cell done score


cellToInt : Cell -> Maybe Int
cellToInt cell =
    case cell of
        Tile num ->
            Just num

        Empty ->
            Nothing


accToCells : Accumulator -> ( List Cell, Int )
accToCells acc =
    Tuple.mapFirst (List.map Tile) <|
        case acc of
            Waiting waiting done s ->
                ( waiting :: done, s )

            Done done s ->
                ( done, s )


fillEmpty : Int -> List Cell -> List Cell
fillEmpty length row =
    List.repeat (length - List.length row) Empty ++ row


slideRow : List Cell -> ( List Cell, Int )
slideRow row =
    List.filterMap cellToInt row
        |> List.foldr accumulate (Done [] 0)
        |> accToCells
        |> Tuple.mapFirst (fillEmpty (List.length row))


toListBoard : Board -> List (List Cell)
toListBoard board =
    Array.toList <| Array.map Array.toList board


fromListBoard : List (List Cell) -> Board
fromListBoard board =
    Array.fromList <| List.map Array.fromList board


transpose : List (List Cell) -> List (List Cell)
transpose matrix =
    List.foldr (List.map2 (::)) (List.repeat (List.length matrix) []) matrix


slideBoard : Direction -> Board -> ( Board, Int )
slideBoard direction board =
    board
        |> toListBoard
        |> slideListBoard direction
        |> Tuple.mapFirst fromListBoard


slideListBoard : Direction -> List (List Cell) -> ( List (List Cell), Int )
slideListBoard direction board =
    case direction of
        Left ->
            board
                |> List.map List.reverse
                |> slideListBoard Right
                |> Tuple.mapFirst (List.map List.reverse)

        Right ->
            board
                |> List.map slideRow
                |> List.unzip
                |> Tuple.mapSecond List.sum

        Up ->
            board
                |> transpose
                |> slideListBoard Left
                |> Tuple.mapFirst transpose

        Down ->
            board
                |> transpose
                |> slideListBoard Right
                |> Tuple.mapFirst transpose


emptyPositionList : Board -> List Position
emptyPositionList board =
    board
        |> Array.map Array.toList
        |> Array.toList
        |> List.indexedMap (\i -> List.indexedMap (\j -> Tuple.pair ( i, j )))
        |> List.concat
        |> List.filterMap
            (\( position, cell ) ->
                case cell of
                    Tile n ->
                        Nothing

                    Empty ->
                        Just position
            )


randomPosition : Board -> Maybe (Random.Generator Position)
randomPosition board =
    let
        positionList =
            emptyPositionList board
    in
    case positionList of
        [] ->
            Nothing

        head :: tail ->
            Just (Random.uniform head tail)


randomTile : Random.Generator Cell
randomTile =
    Random.uniform (Tile 2) [ Tile 4 ]


randomPositionedTiles : Int -> Board -> Random.Generator (List ( Position, Cell ))
randomPositionedTiles n board =
    let
        positionList =
            sample n (emptyPositionList board)

        tileList =
            Random.list n randomTile
    in
    Random.map2 (List.map2 Tuple.pair) positionList tileList


sample : Int -> List a -> Random.Generator (List a)
sample n list =
    if n <= 0 then
        Random.constant []

    else
        let
            indexedList =
                List.indexedMap Tuple.pair list
        in
        case indexedList of
            [] ->
                Random.constant []

            head :: tail ->
                Random.uniform head tail
                    |> Random.andThen
                        (\( index, element ) ->
                            let
                                omitted =
                                    List.take index list ++ List.drop (index + 1) list
                            in
                            Random.map ((::) element) (sample (n - 1) omitted)
                        )


stuck : Board -> Bool
stuck board =
    [ Left, Right, Up, Down ]
        |> List.all
            (\direction ->
                Tuple.first (slideBoard direction board)
                    == board
            )


won : Board -> Bool
won board =
    toListBoard board
        |> List.any (List.any ((==) (Tile 2048)))



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "Consolas"
        ]
        [ h1 [] [ text "Hello" ]
        , div [] [ viewScore model.score ]
        , div [] [ viewBoard model.board ]
        , div []
            [ text <|
                case model.state of
                    Over ->
                        "Game over!"

                    Won ->
                        "You win!"

                    Playing ->
                        ""
            ]
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    div
        [ style "display" "inline-block"
        , style "margin" "10px"
        , style "padding" "5px"
        , style "border-radius" "10px"
        , style "background-color" "#bbb"
        ]
    <|
        Array.toList (Array.map viewRow board)


viewRow : Row -> Html Msg
viewRow row =
    div [] <|
        Array.toList (Array.map viewCell row)


viewCell : Cell -> Html Msg
viewCell cell =
    let
        cellStyle =
            [ style "display" "inline-block"
            , style "margin" "5px"
            , style "width" "50px"
            , style "height" "50px"
            , style "line-height" "50px"
            , style "border-radius" "5px"
            , style "font-size" "24px"
            , style "text-align" "center"
            , style "vertical-align" "middle"
            ]
    in
    case cell of
        Tile number ->
            div
                (cellStyle
                    ++ [ style "background-color" "#eee"
                       ]
                )
                [ text (String.fromInt number)
                ]

        Empty ->
            div
                (cellStyle
                    ++ [ style "background-color" "#ccc"
                       ]
                )
                []


viewScore : Int -> Html Msg
viewScore score =
    div
        [ style "display" "inline-block"
        , style "margin" "10px"
        , style "padding" "10px"
        , style "border-radius" "10px"
        , style "font-size" "20px"
        , style "background-color" "#bbb"
        ]
        [ text <| "Score: " ++ String.fromInt score ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map Slide keyDecoder)
        ]


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.andThen directionDecoder (Decode.field "key" Decode.string)


directionDecoder : String -> Decode.Decoder Direction
directionDecoder string =
    case string of
        "ArrowLeft" ->
            Decode.succeed Left

        "ArrowRight" ->
            Decode.succeed Right

        "ArrowUp" ->
            Decode.succeed Up

        "ArrowDown" ->
            Decode.succeed Down

        _ ->
            Decode.fail "NoControlKey"
