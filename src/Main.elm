port module Main exposing (..)

import Array exposing (Array)
import Array.Extra
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


port print : Encode.Value -> Cmd msg


encodeStudents : Array Student -> Encode.Value
encodeStudents students =
    Encode.list encodeStudent (Array.toList students)


encodeStudent : Student -> Encode.Value
encodeStudent student =
    Encode.object
        [ ( "name", Encode.string student.name )
        , ( "grad", Encode.int (gradeToInt student.grad) )
        , ( "day", Encode.int (Date.day student.date) )
        , ( "month", Encode.int (monthToInt <| Date.month student.date) )
        , ( "year", Encode.int (Date.year student.date) )
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
    { student : Student
    , students : Array Student
    , datePicker : D.Model
    }


type alias Student =
    { name : String
    , grad : Grade
    , date : Date.Date
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
                                    , HA.height 800
                                    , HA.style "height" "790px"
                                    , HA.style "width" "947px"
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
    ( { student =
            { name = ""
            , grad = Kyu8
            , date = initialDate
            }
      , students = Array.empty
      , datePicker = D.init
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
    | Add
    | Remove Int
    | Edit Int
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
            ( model, dataUpdated (encodeStudent model.student) )

        PrintEffect ->
            ( model, print (encodeStudents model.students) )

        NoEffect ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Effect )
update msg ({ student } as model) =
    case msg of
        NameChange name ->
            ( { model | student = { student | name = name } }, DataUpdated )

        GradeChange grad ->
            ( { model | student = { student | grad = grad } }, DataUpdated )

        DatePickerChange (D.DateChanged date) ->
            -- update both date and text
            ( { model | student = { student | date = date } }, DataUpdated )

        DatePickerChange (D.TextChanged text) ->
            ( { model
                | student =
                    { student
                        | date =
                            case Date.fromIsoString text of
                                Ok date ->
                                    date

                                Err _ ->
                                    model.student.date
                    }
              }
            , DataUpdated
            )

        DatePickerChange (D.PickerChanged subMsg) ->
            ( { model | datePicker = D.update subMsg model.datePicker }
            , NoEffect
            )

        TodayFetched today ->
            ( { model | student = { student | date = today }, datePicker = D.initWithToday today }
            , DataUpdated
            )

        Add ->
            ( { model
                | students = Array.push student model.students
                , student = { student | name = "" }
              }
            , NoEffect
            )

        Remove index ->
            ( { model | students = Array.Extra.removeAt index model.students }
            , NoEffect
            )

        Edit index ->
            ( { model
                | student =
                    model.students
                        |> Array.get index
                        |> Maybe.withDefault model.student
                , students = Array.Extra.removeAt index model.students
              }
            , NoEffect
            )

        Print ->
            ( model, PrintEffect )


view : Model -> Element Msg
view model =
    row
        [ Background.color (rgba255 255 255 255 0.8)
        , HA.style "backdrop-filter" "blur(10px)"
            |> htmlAttribute
        , HA.style "backdrop-filter" "blur(10px)"
            |> htmlAttribute
        , Border.color (rgba255 20 20 20 0.95)
        , Border.solid
        , Border.rounded 8
        , Border.width 4
        , fill
            |> minimum 800
            |> width
        ]
        [ column
            [ padding 20
            , spacing 20
            , alignTop
            , fillPortion 1
                |> width
            ]
            [ row []
                [ I.text []
                    { onChange = NameChange
                    , text = model.student.name
                    , placeholder = Nothing
                    , label = I.labelAbove [] (text "Name")
                    }
                ]
            , row []
                [ D.input
                    []
                    { onChange = DatePickerChange
                    , selected = Just model.student.date
                    , text = formatDate model.student.date
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
                    , selected = Just model.student.grad
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
                [ I.button [ Background.color (colorFromGrade model.student.grad |> .background), padding 10, Border.rounded 8, Font.color (colorFromGrade model.student.grad |> .foreground) ]
                    { onPress = Just Add
                    , label = text "Add"
                    }
                ]
            ]
        , column
            [ padding 20
            , spacing 20
            , alignTop
            , fillPortion 1
                |> width
            ]
            [ row [ spacing 20 ]
                [ if Array.isEmpty model.students then
                    Element.text "keine Studenten erfasst"

                  else
                    Element.indexedTable [ spacing 8 ]
                        { data = Array.toList model.students
                        , columns =
                            [ { header = Element.none
                              , width = fill
                              , view = \index _ -> Element.text (String.fromInt (index + 1) ++ ".")
                              }
                            , { header = tableHeader [] "Name"
                              , width = fill
                              , view = \_ student -> Element.text student.name
                              }
                            , { header = tableHeader [ Font.center ] "Grad"
                              , width = fill
                              , view =
                                    \_ student ->
                                        el
                                            [ Font.color (colorFromGrade student.grad |> .foreground)
                                            , Background.color (colorFromGrade student.grad |> .background)
                                            , padding 2
                                            , Border.solid
                                            , Border.rounded 8
                                            , Border.width 0
                                            , Font.center
                                            ]
                                            (Element.text ("Kyu " ++ String.fromInt (gradeToInt student.grad)))
                              }
                            , { header = tableHeader [] "Datum"
                              , width = fill
                              , view = \_ student -> Element.text (formatDate student.date)
                              }
                            , { header = Element.none
                              , width = fill
                              , view =
                                    \index _ ->
                                        row []
                                            [ I.button [ padding 2, centerX, centerY ]
                                                { onPress = Just (Remove index)
                                                , label = text "🗑️"
                                                }
                                            , I.button [ padding 2, centerX, centerY ]
                                                { onPress = Just (Edit index)
                                                , label = text "✏️"
                                                }
                                            ]
                              }
                            ]
                        }
                ]
            , if Array.isEmpty model.students then
                Element.none

              else
                row []
                    [ I.button [ Background.color (colorFromGrade model.student.grad |> .background), padding 10, Border.rounded 8, Font.color (colorFromGrade model.student.grad |> .foreground) ]
                        { onPress = Just Print
                        , label = text "Print"
                        }
                    ]
            ]
        ]


tableHeader : List (Attribute msg) -> String -> Element msg
tableHeader attrs text =
    el (attrs ++ [ Font.bold ]) (Element.text text)


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
