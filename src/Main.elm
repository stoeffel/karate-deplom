port module Main exposing (..)

import Browser
import Date exposing (Date)
import DatePicker as D
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as I
import Html
import Html.Attributes as HA
import Json.Encode as Encode
import Task
import Time


port dataUpdated : Encode.Value -> Cmd msg


port print : {} -> Cmd msg


encode : { model | name : String, grad : Grade, date : Date.Date } -> Encode.Value
encode model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "grad", Encode.int (gradeToInt model.grad) )
        , ( "day", Encode.int (Date.day model.date) )
        , ( "month", Encode.int (monthToInt <| Date.month model.date) )
        , ( "year", Encode.int (Date.year model.date) )
        ]


monthToInt : Date.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


gradeToInt : Grade -> Int
gradeToInt grade =
    case grade of
        Kyu1 ->
            1

        Kyu2 ->
            2

        Kyu3 ->
            3

        Kyu4 ->
            4

        Kyu5 ->
            5

        Kyu6 ->
            6

        Kyu7 ->
            7

        Kyu8 ->
            8


type alias Model =
    { name : String
    , grad : Grade
    , date : Date.Date
    , datePicker : D.Model
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update =
            \msg model ->
                update msg model
                    |> executeEffect
        , subscriptions = \_ -> Sub.none
        , view =
            \model ->
                { title = "Karate Diplom"
                , body =
                    [ layout
                        [ padding 20
                        , behindContent
                            (html
                                (Html.canvas
                                    [ HA.id "canvas"
                                    , HA.width 957
                                    , HA.height 780
                                    , HA.style "position" "absolute"
                                    , HA.style "top" "0"
                                    , HA.style "left" "0"
                                    ]
                                    []
                                )
                            )
                        ]
                        (view model)
                    ]
                }
        }


init : a -> ( Model, Cmd Msg )
init _ =
    ( { name = ""
      , grad = Kyu8
      , datePicker = D.init
      , date = initialDate
      }
    , Task.perform TodayFetched Date.today
    )


formatDate : Date.Date -> String
formatDate =
    Date.format "dd.MM.yyyy"


initialDate : Date.Date
initialDate =
    Date.fromCalendarDate 2018 Time.Jan 1


type Msg
    = NameChange String
    | GradeChange Grade
    | DatePickerChange D.ChangeEvent
    | TodayFetched Date.Date
    | Print


type Grade
    = Kyu1
    | Kyu2
    | Kyu3
    | Kyu4
    | Kyu5
    | Kyu6
    | Kyu7
    | Kyu8


type Effect
    = DataUpdated
    | PrintEffect
    | NoEffect


executeEffect : ( Model, Effect ) -> ( Model, Cmd Msg )
executeEffect ( model, effect ) =
    case effect of
        DataUpdated ->
            ( model, dataUpdated (encode model) )

        PrintEffect ->
            ( model, print {} )

        NoEffect ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        NameChange name ->
            ( { model | name = name }, DataUpdated )

        GradeChange grad ->
            ( { model | grad = grad }, DataUpdated )

        DatePickerChange (D.DateChanged date) ->
            -- update both date and text
            ( { model | date = date }, DataUpdated )

        DatePickerChange (D.TextChanged text) ->
            ( { model
                | date =
                    case Date.fromIsoString text of
                        Ok date ->
                            date

                        Err _ ->
                            model.date
              }
            , DataUpdated
            )

        DatePickerChange (D.PickerChanged subMsg) ->
            ( { model | datePicker = D.update subMsg model.datePicker }
            , NoEffect
            )

        TodayFetched today ->
            ( { model | date = today, datePicker = D.initWithToday today }
            , DataUpdated
            )

        Print ->
            ( model, PrintEffect )


view : Model -> Element Msg
view model =
    column
        [ Background.color (rgba255 255 255 255 0.8)
        , HA.style "backdrop-filter" "blur(10px)"
            |> htmlAttribute
        , HA.style "backdrop-filter" "blur(10px)"
            |> htmlAttribute
        , Border.color (rgba255 20 20 20 0.95)
        , Border.solid
        , Border.rounded 8
        , Border.width 4
        , padding 20
        , spacing 20
        ]
        [ row []
            [ I.text []
                { onChange = NameChange
                , text = model.name
                , placeholder = Nothing
                , label = I.labelAbove [] (text "Name")
                }
            ]
        , row []
            [ D.input
                []
                { onChange = DatePickerChange
                , selected = Just model.date
                , text = formatDate model.date
                , label =
                    I.labelAbove [] <|
                        Element.text "Pick A Date"
                , placeholder = Nothing
                , settings = D.defaultSettings
                , model = model.datePicker
                }
            ]
        , wrappedRow []
            [ I.radio
                []
                { onChange = GradeChange
                , selected = Just model.grad
                , label = I.labelAbove [] (text "Kyu")
                , options =
                    [ I.option Kyu8 (text "Kyu 8")
                    , I.option Kyu7 (text "Kyu 7")
                    , I.option Kyu6 (text "Kyu 6")
                    , I.option Kyu5 (text "Kyu 5")
                    , I.option Kyu4 (text "Kyu 4")
                    , I.option Kyu3 (text "Kyu 3")
                    , I.option Kyu2 (text "Kyu 2")
                    , I.option Kyu1 (text "Kyu 1")
                    ]
                }
            ]
        , row []
            [ I.button [ Background.color (colorFromGrade model.grad |> .background), padding 10, Border.rounded 8, Font.color (colorFromGrade model.grad |> .foreground) ]
                { onPress = Just Print
                , label = text "Print"
                }
            ]
        ]


{-| Function uses the grade color to color the background and uses a contrasting color for the text.
Kyu 8: Yellow
Kyu 7: Orange
Kyu 6: Green
Kyu 5: Blue
Kyu 4: Purple
Kyu 3: Brown
Kyu 2: Brown
Kyu 1: Brown
-}
colorFromGrade : Grade -> { background : Color, foreground : Color }
colorFromGrade grade =
    case grade of
        Kyu1 ->
            { background = rgb255 139 69 19, foreground = rgb255 255 255 255 }

        Kyu2 ->
            { background = rgb255 139 69 19, foreground = rgb255 255 255 255 }

        Kyu3 ->
            { background = rgb255 139 69 19, foreground = rgb255 255 255 255 }

        Kyu4 ->
            { background = rgb255 128 0 128, foreground = rgb255 255 255 255 }

        Kyu5 ->
            { background = rgb255 0 0 255, foreground = rgb255 255 255 255 }

        Kyu6 ->
            { background = rgb255 0 128 0, foreground = rgb255 255 255 255 }

        Kyu7 ->
            { background = rgb255 255 165 0, foreground = rgb255 0 0 0 }

        Kyu8 ->
            { background = rgb255 255 255 0, foreground = rgb255 0 0 0 }
