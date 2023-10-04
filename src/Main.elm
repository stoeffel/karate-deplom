port module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import Browser
import Date exposing (Date)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as I
import Html
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Time


port dataUpdated : Encode.Value -> Cmd msg


port print : Encode.Value -> Cmd msg


port focus : {} -> Cmd msg


encodeStudents : Array Student -> Encode.Value
encodeStudents students =
    Encode.list encodeStudent (Array.toList students)


encodeStudent : Student -> Encode.Value
encodeStudent student =
    case List.filterMap String.toInt <| String.split "." student.date of
        [ d, m, y ] ->
            Encode.object
                [ ( "name", Encode.string student.name )
                , ( "grad", Encode.int (gradeToInt student.grad) )
                , ( "day", Encode.int d )
                , ( "month", Encode.int m )
                , ( "year", Encode.int y )
                ]

        _ ->
            Encode.object []


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


monthFromInt : Int -> Date.Month
monthFromInt month =
    case month of
        1 ->
            Time.Jan

        2 ->
            Time.Feb

        3 ->
            Time.Mar

        4 ->
            Time.Apr

        5 ->
            Time.May

        6 ->
            Time.Jun

        7 ->
            Time.Jul

        8 ->
            Time.Aug

        9 ->
            Time.Sep

        10 ->
            Time.Oct

        11 ->
            Time.Nov

        12 ->
            Time.Dec

        _ ->
            Time.Jan


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
    , dateError : Bool
    , mode : Mode
    , students : Array Student
    }


type Mode
    = Adding
    | Editing Int


type alias Student =
    { name : String
    , grad : Grade
    , date : String
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
            , date = formatDate initialDate
            }
      , students = Array.empty
      , dateError = False
      , mode = Adding
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
    | DateChange String
    | TodayFetched Date.Date
    | Add
    | Remove Int
    | Edit Int
    | Save Int
    | Cancel
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
    | Focus
    | NoEffect


executeEffect : ( Model, Effect ) -> ( Model, Cmd Msg )
executeEffect ( model, effect ) =
    case effect of
        DataUpdated ->
            ( model, dataUpdated (encodeStudent model.student) )

        PrintEffect ->
            ( model, print (encodeStudents model.students) )

        Focus ->
            ( model, focus {} )

        NoEffect ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Effect )
update msg ({ student } as model) =
    case msg of
        NameChange name ->
            ( { model | student = { student | name = name } }, DataUpdated )

        GradeChange grad ->
            ( { model | student = { student | grad = grad } }, DataUpdated )

        DateChange editDate ->
            { model
                | student =
                    { student
                        | date =
                            editDate
                    }
                , dateError =
                    case String.split "." editDate of
                        [ dStr, mStr, yStr ] ->
                            (String.length dStr /= 2)
                                || (String.length mStr /= 2)
                                || (String.length yStr /= 4)
                                || (case List.filterMap String.toInt [ dStr, mStr, yStr ] of
                                        [ d, m, y ] ->
                                            (d < 1)
                                                || (d > 31)
                                                || (m < 1)
                                                || (m > 12)
                                                || (y < 1900)
                                                || (y > 2100)

                                        _ ->
                                            True
                                   )

                        _ ->
                            True
            }
                |> (\m ->
                        ( m
                        , if m.dateError then
                            NoEffect

                          else
                            DataUpdated
                        )
                   )

        TodayFetched today ->
            ( { model | student = { student | date = formatDate today } }
            , DataUpdated
            )

        Add ->
            ( { model
                | students = Array.push student model.students
                , student = { student | name = "" }
              }
            , Focus
            )

        Remove index ->
            ( { model | students = Array.Extra.removeAt index model.students }
            , NoEffect
            )

        Edit index ->
            ( { model
                | mode = Editing index
                , student =
                    model.students
                        |> Array.get index
                        |> Maybe.withDefault model.student
              }
            , Focus
            )

        Save index ->
            ( { model
                | mode = Adding
                , students = Array.set index student model.students
                , student = { student | name = "" }
              }
            , Focus
            )

        Cancel ->
            ( { model
                | mode = Adding
                , student = { student | name = "" }
              }
            , Focus
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
            , onEnter Add
            ]
            [ row []
                [ I.text
                    [ HA.autofocus True
                        |> htmlAttribute
                    ]
                    { onChange = NameChange
                    , text = model.student.name
                    , placeholder = Nothing
                    , label = I.labelAbove [] (text "Name")
                    }
                ]
            , row []
                [ column [ width fill ]
                    [ I.text
                        (if model.dateError then
                            [ Border.color (rgb 255 0 0)
                            ]

                         else
                            []
                        )
                        { onChange = DateChange
                        , text = model.student.date
                        , label =
                            I.labelAbove [] (text "Datum")
                        , placeholder = Nothing
                        }
                    , if model.dateError then
                        text "Datum muss im Format dd.mm.yyyy sein"

                      else
                        text ""
                    ]
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
            , row [ spacing 20 ] <|
                case model.mode of
                    Adding ->
                        [ I.button
                            [ padding 10
                            , Border.rounded 8
                            , Font.color (colorFromGrade model.student.grad |> .foreground)
                            , if model.dateError then
                                Background.color (rgb255 180 180 180)

                              else
                                Background.color (colorFromGrade model.student.grad |> .background)
                            ]
                            { onPress =
                                if model.dateError then
                                    Nothing

                                else
                                    Just Add
                            , label = text "Hinzufügen"
                            }
                        ]

                    Editing index ->
                        [ I.button [ Background.color (colorFromGrade model.student.grad |> .background), padding 10, Border.rounded 8, Font.color (colorFromGrade model.student.grad |> .foreground) ]
                            { onPress = Just (Save index)
                            , label = text "ändern"
                            }
                        , -- cancel
                          I.button [ Background.color (rgb255 180 180 180), padding 10, Border.rounded 8, Font.color (rgb255 10 10 10) ]
                            { onPress = Just Cancel
                            , label = text "abbrechen"
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
                              , view =
                                    \index _ ->
                                        -- when editing show little right pointing arrow before as emoji
                                        case model.mode of
                                            Adding ->
                                                Element.text (" " ++ String.fromInt (index + 1) ++ ".")

                                            Editing editingIndex ->
                                                if index == editingIndex then
                                                    Element.text "➡️"

                                                else
                                                    Element.text (" " ++ String.fromInt (index + 1) ++ ".")
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
                              , view = \_ student -> Element.text student.date
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
                    [ I.button [ Background.color (rgb 0.5 0.8 0.5), padding 10, Border.rounded 8, Font.color (rgb255 10 10 10) ]
                        { onPress = Just Print
                        , label = text "Drucken"
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


onEnter : msg -> Attribute msg
onEnter msg =
    HE.on "keydown" (Decode.andThen (onEnterDecoder msg) keyCodeDecoder)
        |> htmlAttribute


onEnterDecoder : msg -> Int -> Decode.Decoder msg
onEnterDecoder msg keyCode =
    if keyCode == 13 then
        Decode.succeed msg

    else
        Decode.fail "Not the enter key"


keyCodeDecoder : Decode.Decoder Int
keyCodeDecoder =
    Decode.field "keyCode" Decode.int
