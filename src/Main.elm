module Main exposing (..)

import Html exposing (Html, text, div, img, input, h1, h2, a, button)
import Html.Attributes exposing (src, class, href)
import Html.Events exposing (onClick)
import Http
import Dict exposing (Dict)


visibleCols : List String
visibleCols =
    [ "competitorname", "sugarpercent", "pricepercent", "winpercent" ]


filterableCols : List String
filterableCols =
    [ "fruity"
    , "caramel"
    , "peanutyalmondy"
    , "nougat"
    , "crispedricewafer"
    , "hard"
    , "bar"
    , "pluribus"
    ]



---- MODEL ----


type SortBy
    = Asc (Maybe String)
    | Desc (Maybe String)


type Filter
    = Include String
    | Exclude String
    | All String


type alias Model =
    { headers : List String
    , rows : List (Dict String String)
    , sortBy : SortBy
    , curFilters : List Filter
    , isLegendVisible : Bool
    }


initFilters : List Filter
initFilters =
    List.map All filterableCols


init : ( Model, Cmd Msg )
init =
    ( { headers = []
      , rows = []
      , sortBy = Asc Nothing
      , curFilters = initFilters
      , isLegendVisible = False
      }
    , getRawCsv
    )



---- COMMANDS ----


getRawCsv : Cmd Msg
getRawCsv =
    Http.getString "https://raw.githubusercontent.com/fivethirtyeight/data/master/candy-power-ranking/candy-data.csv"
        |> Http.send ReceiveCsv



---- UPDATE ----


type Msg
    = NoOp
    | ReceiveCsv (Result Http.Error String)
    | ToggleSortBy String
    | ToggleFilter Filter
    | ToggleLegend


rowToDict : List String -> List String -> Dict String String
rowToDict headers row =
    List.map2 (,) headers row
        |> Dict.fromList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveCsv (Ok data) ->
            let
                stringsList =
                    String.split "\n" data

                headers =
                    List.head stringsList
                        |> Maybe.withDefault ""
                        |> String.split ","

                rows =
                    List.tail stringsList
                        |> Maybe.withDefault []
                        |> List.map (String.split ",")

                dictRows =
                    List.map (rowToDict headers) rows

                sortBy =
                    Asc (List.head headers)
            in
                ( { model
                    | headers = headers
                    , rows = dictRows
                    , sortBy = sortBy
                  }
                , Cmd.none
                )

        ToggleSortBy key ->
            case model.sortBy of
                Asc _ ->
                    ( { model | sortBy = Desc (Just key) }, Cmd.none )

                Desc _ ->
                    ( { model | sortBy = Asc (Just key) }, Cmd.none )

        ToggleFilter filter ->
            let
                updatedFilters =
                    List.map (updateFilter filter) model.curFilters
            in
                ( { model | curFilters = updatedFilters }, Cmd.none )

        ToggleLegend ->
            ( { model | isLegendVisible = not model.isLegendVisible }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateFilter : Filter -> Filter -> Filter
updateFilter updatingFilter curFilter =
    case updatingFilter of
        Include name ->
            case matchFilter name curFilter of
                True ->
                    updatingFilter

                False ->
                    curFilter

        Exclude name ->
            case matchFilter name curFilter of
                True ->
                    updatingFilter

                False ->
                    curFilter

        All name ->
            case matchFilter name curFilter of
                True ->
                    updatingFilter

                False ->
                    curFilter


matchFilter : String -> Filter -> Bool
matchFilter name filter =
    case filter of
        Include filterName ->
            name == filterName

        Exclude filterName ->
            name == filterName

        All filterName ->
            name == filterName



---- VIEW ----


viewHeader : SortBy -> String -> Html Msg
viewHeader sortBy header =
    let
        sortClass =
            case sortBy of
                Asc (Just key) ->
                    case key == header of
                        True ->
                            "asc cell"

                        False ->
                            "cell"

                Desc (Just key) ->
                    case key == header of
                        True ->
                            "desc cell"

                        False ->
                            "cell"

                _ ->
                    "cell"

        classes =
            case List.member header visibleCols of
                True ->
                    sortClass

                False ->
                    sortClass ++ " hidden"
    in
        div [ class classes, onClick (ToggleSortBy header) ] [ text header ]


viewHeaders : List String -> SortBy -> Html Msg
viewHeaders headers sortBy =
    div [ class "row headers" ] (List.map (viewHeader sortBy) headers)


viewCell : String -> Html Msg
viewCell cell =
    div [ class "cell" ] [ text cell ]


viewRow : Model -> Dict String String -> Html Msg
viewRow model row =
    let
        visibleKeys =
            List.filter ((flip List.member) visibleCols) model.headers

        rowText =
            List.map (\x -> Maybe.withDefault "" (Dict.get x row)) visibleKeys
    in
        div [ class "row" ] (List.map viewCell rowText)


sortRows : SortBy -> Dict String String -> Dict String String -> Order
sortRows sortBy a b =
    case sortBy of
        Asc (Just key) ->
            let
                compA =
                    Dict.get key a |> Maybe.withDefault ""

                compB =
                    Dict.get key b |> Maybe.withDefault ""
            in
                compare compA compB

        Desc (Just key) ->
            let
                compA =
                    Dict.get key a |> Maybe.withDefault ""

                compB =
                    Dict.get key b |> Maybe.withDefault ""
            in
                case compare compA compB of
                    LT ->
                        GT

                    GT ->
                        LT

                    EQ ->
                        EQ

        _ ->
            EQ


matchRow : Dict String String -> Filter -> Bool
matchRow row filter =
    case filter of
        Include key ->
            (Dict.get key row) == Just "1"

        Exclude key ->
            (Dict.get key row) == Just "0"

        All key ->
            case Dict.get key row of
                Nothing ->
                    False

                _ ->
                    True


applyFilters : List Filter -> Dict String String -> Bool
applyFilters filters row =
    List.all (matchRow row) filters


viewRows : Model -> Html Msg
viewRows ({ rows, sortBy, curFilters } as model) =
    let
        filteredRows =
            List.filter (applyFilters curFilters) rows

        sortedRows =
            List.sortWith (sortRows sortBy) filteredRows
    in
        div [] (List.map (viewRow model) sortedRows)


viewFilterOption : Filter -> List Filter -> Html Msg
viewFilterOption filter curFilters =
    let
        label =
            case filter of
                Include name ->
                    name

                Exclude name ->
                    "not " ++ name

                All name ->
                    "all"

        display =
            case List.member filter curFilters of
                True ->
                    "active-filter filter-option"

                False ->
                    "filter-option"
    in
        div [ onClick (ToggleFilter filter), class display ] [ text label ]


viewFilter : List Filter -> String -> Html Msg
viewFilter curFilters filterName =
    div [ class "cell" ]
        [ viewFilterOption (Include filterName) curFilters
        , viewFilterOption (Exclude filterName) curFilters
        , viewFilterOption (All filterName) curFilters
        ]


viewFilters : List Filter -> Html Msg
viewFilters curFilters =
    div []
        [ h2 [] [ text "Filters" ]
        , div [ class "row filters" ]
            (List.map (viewFilter curFilters) filterableCols)
        ]


viewRankings : Model -> Html Msg
viewRankings model =
    div [ class "rankings" ]
        [ viewHeaders model.headers model.sortBy
        , viewRows model
        ]


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ div [] [ h1 [] [ text "Halloween Candy Power Rankings" ] ]
        , viewAbout model.isLegendVisible
        , viewFilters model.curFilters
        , viewRankings model
        ]


viewAbout : Bool -> Html Msg
viewAbout isExpanded =
    let
        showExpansionOption =
            case isExpanded of
                True ->
                    "Hide legend for columns and filters"

                False ->
                    "Show legend for columns and filters"
    in
        div []
            [ div []
                [ text "Candy rankings based on data from FiveThirtyEight: "
                , a [ href "https://github.com/fivethirtyeight/data/tree/master/candy-power-ranking" ] [ text "Link to source" ]
                ]
            , text "Use the filters below to explore which candy was the top in its category and click on the column names to sort by a specific column"
            , div [ onClick ToggleLegend, class "legend-button" ] [ text showExpansionOption ]
            , viewLegend isExpanded
            ]


viewLegend : Bool -> Html Msg
viewLegend isExpanded =
    case isExpanded of
        False ->
            text ""

        True ->
            div [ class "legend" ]
                [ div [ class "row headers" ]
                    [ div [ class "cell" ] [ text "Header" ]
                    , div [ class "cell" ] [ text "Description" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "cell" ] [ text "chocolate" ]
                    , div [ class "cell" ] [ text "Does it contain chocolate?" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "cell" ] [ text "fruity" ]
                    , div [ class "cell" ] [ text "Is it fruit flavored?" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "cell" ] [ text "caramel" ]
                    , div [ class "cell" ] [ text "Is there caramel in the candy?" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "cell" ] [ text "peanutalmondy" ]
                    , div [ class "cell" ] [ text "Does it contain peanuts, peanut butter or almonds?" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "cell" ] [ text "nougat" ]
                    , div [ class "cell" ] [ text "Does it contain nougat?" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "cell" ] [ text "crispedricewafer" ]
                    , div [ class "cell" ] [ text "Does it contain crisped rice, wafers, or a cookie component?" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "cell" ] [ text "hard" ]
                    , div [ class "cell" ] [ text "Is it a hard candy?" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "cell" ] [ text "bar" ]
                    , div [ class "cell" ] [ text "Is it a candy bar?" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "cell" ] [ text "pluribus" ]
                    , div [ class "cell" ] [ text "Is it one of many candies in a bag or box?" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "cell" ] [ text "sugarpercent" ]
                    , div [ class "cell" ] [ text "The percentile of sugar it falls under within the data set." ]
                    ]
                , div [ class "row" ]
                    [ div [ class "cell" ] [ text "pricepercent" ]
                    , div [ class "cell" ] [ text "The unit price percentile compared to the rest of the set." ]
                    ]
                , div [ class "row" ]
                    [ div [ class "cell" ] [ text "winpercent" ]
                    , div [ class "cell" ] [ text "The overall win percentage according to 269,000 matchups." ]
                    ]
                ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
