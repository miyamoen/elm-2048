module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy, lazy2, lazy4, lazy5)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Random
import Random.List
import Random.Set
import Set exposing (Set)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm 2048", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { board : Board
    , size : ( Int, Int )
    , score : Int
    , state : GameState
    , nextId : Int
    }


type GameState
    = Playing
    | Won
    | Over


type alias Board =
    List AnimationCell


type alias Cell =
    { position : Position, num : Int, id : String }


type AnimationCell
    = ShowUpCell Cell
    | MoveCell Cell Position
    | MergeCell Cell ( Position, Position )


type alias Position =
    ( Int, Int )


type Direction
    = Left
    | Right
    | Up
    | Down


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = []
      , size = ( 4, 4 )
      , score = 0
      , state = Playing
      , nextId = 0
      }
    , randomCells 2 ( 4, 4 ) []
        |> Random.generate PutMany
    )



-- UPDATE


type Msg
    = Slide Direction
    | Put (String -> Cell)
    | PutMany (List (String -> Cell))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Slide direction ->
            let
                slidedBoard =
                    slideBoard model.size direction model.board

                ( increase, cmd ) =
                    if boardEqual model.board slidedBoard then
                        ( 0, Cmd.none )

                    else
                        ( countScore model.board
                        , randomCell model.size slidedBoard
                            |> Random.generate Put
                        )
            in
            ( { model
                | board = slidedBoard
                , score = model.score + increase
              }
            , cmd
            )

        Put toCell ->
            let
                newBoard =
                    ShowUpCell (toCell <| String.fromInt model.nextId)
                        :: model.board
            in
            ( { model
                | board = newBoard
                , nextId = model.nextId + 1
                , state =
                    if stuck model.size newBoard then
                        Over

                    else if won newBoard then
                        Won

                    else
                        Playing
              }
            , Cmd.none
            )

        PutMany toCells ->
            ( { model
                | board =
                    List.indexedMap
                        (\i toCell ->
                            (model.nextId + i)
                                |> String.fromInt
                                |> toCell
                                |> ShowUpCell
                        )
                        toCells
                        ++ model.board
                , nextId = model.nextId + List.length toCells
              }
            , Cmd.none
            )


accumulate : Cell -> List AnimationCell -> List AnimationCell
accumulate cell cells =
    case cells of
        (MergeCell _ _) :: _ ->
            MoveCell cell cell.position :: cells

        (MoveCell nearest _) :: rest ->
            if nearest.num == cell.num then
                MergeCell { cell | num = cell.num * 2 }
                    ( cell.position, nearest.position )
                    :: rest

            else
                MoveCell cell cell.position :: cells

        (ShowUpCell _) :: _ ->
            MoveCell cell cell.position :: cells

        [] ->
            [ MoveCell cell cell.position ]


slideBoard : ( Int, Int ) -> Direction -> Board -> Board
slideBoard ( width, height ) direction board =
    case direction of
        Left ->
            List.concatMap
                (\j ->
                    getRow j board
                        |> List.foldl accumulate []
                        |> List.reverse
                        |> List.indexedMap (\i cell -> replacePosition ( i, j ) cell)
                )
                (List.range 0 (height - 1))

        Right ->
            List.concatMap
                (\j ->
                    getRow j board
                        |> List.foldr accumulate []
                        |> List.reverse
                        |> List.indexedMap (\i cell -> replacePosition ( width - i - 1, j ) cell)
                )
                (List.range 0 (height - 1))

        Up ->
            List.concatMap
                (\i ->
                    getColumn i board
                        |> List.foldl accumulate []
                        |> List.reverse
                        |> List.indexedMap (\j cell -> replacePosition ( i, j ) cell)
                )
                (List.range 0 (width - 1))

        Down ->
            List.concatMap
                (\i ->
                    getColumn i board
                        |> List.foldr accumulate []
                        |> List.reverse
                        |> List.indexedMap (\j cell -> replacePosition ( i, height - j - 1 ) cell)
                )
                (List.range 0 (width - 1))


getRow : Int -> Board -> List Cell
getRow index board =
    List.map getCell board
        |> List.filter (.position >> Tuple.second >> (==) index)
        |> List.sortBy .position


getColumn : Int -> Board -> List Cell
getColumn index board =
    List.map getCell board
        |> List.filter (.position >> Tuple.first >> (==) index)
        |> List.sortBy .position


getCell : AnimationCell -> Cell
getCell animationCell =
    case animationCell of
        ShowUpCell cell ->
            cell

        MoveCell cell _ ->
            cell

        MergeCell cell _ ->
            cell


replacePosition : Position -> AnimationCell -> AnimationCell
replacePosition position animationCell =
    case animationCell of
        ShowUpCell cell ->
            ShowUpCell { cell | position = position }

        MoveCell cell old ->
            MoveCell { cell | position = position } old

        MergeCell cell olds ->
            MergeCell { cell | position = position } olds


emptyPositionList : ( Int, Int ) -> Board -> List Position
emptyPositionList ( width, height ) board =
    List.map (getCell >> .position) board
        |> Set.fromList
        |> Set.diff
            (List.range 0 (height - 1)
                |> List.concatMap
                    (\j ->
                        List.range 0 (width - 1)
                            |> List.map (\i -> ( i, j ))
                    )
                |> Set.fromList
            )
        |> Set.toList


boardEqual : Board -> Board -> Bool
boardEqual a b =
    (List.length a == List.length b)
        && (toDict a == toDict b)


toDict : Board -> Dict Position Int
toDict board =
    List.map (getCell >> (\{ num, position } -> ( position, num ))) board
        |> Dict.fromList



-- Random


randomEmptyPositions : Int -> ( Int, Int ) -> Board -> Random.Generator (List Position)
randomEmptyPositions length size board =
    emptyPositionList size board
        |> Random.List.shuffle
        |> Random.map (List.take length)


randomEmptyPosition : ( Int, Int ) -> Board -> Random.Generator Position
randomEmptyPosition size board =
    Random.Set.notInSet (List.map (getCell >> .position) board |> Set.fromList) <|
        randomPosition size


randomPosition : ( Int, Int ) -> Random.Generator Position
randomPosition ( width, height ) =
    Random.pair (Random.int 0 <| width - 1) (Random.int 0 <| height - 1)


randomNum : Random.Generator Int
randomNum =
    Random.uniform 2 [ 4 ]


randomCells : Int -> ( Int, Int ) -> Board -> Random.Generator (List (String -> Cell))
randomCells n size board =
    let
        positionList =
            randomEmptyPositions n size board

        numList =
            Random.list n randomNum
    in
    Random.map2 (List.map2 Cell) positionList numList


randomCell : ( Int, Int ) -> Board -> Random.Generator (String -> Cell)
randomCell size board =
    Random.map2 Cell (randomEmptyPosition size board) randomNum



-- judge


countScore : Board -> Int
countScore board =
    List.map scoreHelp board
        |> List.sum


scoreHelp : AnimationCell -> Int
scoreHelp animationCell =
    case animationCell of
        MergeCell { num } _ ->
            num

        _ ->
            0


stuck : ( Int, Int ) -> Board -> Bool
stuck (( w, h ) as size) board =
    (List.length board == w * h)
        && List.all
            (\direction -> boardEqual board <| slideBoard size direction board)
            [ Left, Right, Up, Down ]


won : Board -> Bool
won board =
    List.any (getCell >> .num >> (<=) 64) board



-- VIEW


view : Model -> Html Msg
view model =
    layout [ Font.family [ Font.typeface "Consolas" ] ] <|
        column [ spacing 20, padding 20 ]
            [ viewScore model.score
            , viewBoard model
            , viewResult model.state
            ]


viewBoard : Model -> Element msg
viewBoard { size, board } =
    let
        ( w, h ) =
            size
    in
    Keyed.column
        [ Border.rounded 10
        , Background.color <| rgb255 187 187 187
        , width <| px <| w * 50 + (w - 1) * 5 + 20
        , height <| px <| h * 50 + (h - 1) * 5 + 20
        , padding 10
        ]
    <|
        List.map (Tuple.mapSecond <| el [ width <| px 0, height <| px 0 ])
            (viewEmptyCells size ++ viewAnimationCells board)


viewAnimationCells : List AnimationCell -> List ( String, Element msg )
viewAnimationCells animationCells =
    List.map (\cell -> ( getCell cell |> .id, viewAnimationCell cell ))
        animationCells


viewAnimationCell : AnimationCell -> Element msg
viewAnimationCell animationCell =
    let
        cell =
            getCell animationCell
    in
    case animationCell of
        ShowUpCell _ ->
            lazy4 positionHelp "" "showup" cell.position cell.num

        MoveCell _ position ->
            lazy4 positionHelp "move" "" cell.position cell.num

        MergeCell _ ( position, position2 ) ->
            lazy4 positionHelp "merge" "" cell.position cell.num


positionHelp : String -> String -> Position -> Int -> Element msg
positionHelp cls2 cls3 ( i, j ) num =
    el
        [ moveRight <| toFloat <| i * 55
        , moveDown <| toFloat <| j * 55
        , class "move"
        ]
    <|
        lazy2 viewCell cls3 num


viewCell : String -> Int -> Element msg
viewCell cls num =
    column
        [ class "showup"
        , width <| px 50
        , height <| px 50
        , Border.rounded 5
        , Font.size 24
        , Background.color <|
            case num of
                2 ->
                    rgb255 238 238 238

                4 ->
                    rgb255 248 252 223

                8 ->
                    rgb255 243 190 152

                16 ->
                    rgb255 234 162 109

                32 ->
                    rgb255 245 156 135

                64 ->
                    rgb255 255 97 61

                128 ->
                    rgb255 238 229 161

                256 ->
                    rgb255 245 231 132

                512 ->
                    rgb255 228 214 111

                1024 ->
                    rgb255 224 195 88

                2048 ->
                    rgb255 196 224 88

                _ ->
                    rgb255 100 100 100
        , Font.color <|
            if num == 2 || num == 4 then
                rgb255 34 34 34

            else
                rgb255 250 250 250
        ]
        [ el [ centerX, centerY ] <| text (String.fromInt num) ]


viewEmptyCells : ( Int, Int ) -> List ( String, Element msg )
viewEmptyCells ( w, h ) =
    List.range 0 (h - 1)
        |> List.concatMap
            (\j ->
                List.range 0 (w - 1)
                    |> List.map
                        (\i ->
                            ( String.fromInt i ++ "_" ++ String.fromInt j
                            , el
                                [ width <| px 50
                                , height <| px 50
                                , Border.rounded 5
                                , Background.color <| rgb255 204 204 204
                                , moveRight <| toFloat <| i * 55
                                , moveDown <| toFloat <| j * 55
                                ]
                                none
                            )
                        )
            )


viewResult : GameState -> Element msg
viewResult state =
    case state of
        Won ->
            text "You win!"

        Over ->
            text "Game over!"

        Playing ->
            none


viewScore : Int -> Element msg
viewScore score =
    el
        [ Border.rounded 10
        , padding 10
        , Font.size 20
        , Background.color <| rgb255 187 187 187
        ]
    <|
        text <|
            "Score: "
                ++ String.fromInt score


class : String -> Attribute msg
class =
    htmlAttribute << Html.Attributes.class



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
