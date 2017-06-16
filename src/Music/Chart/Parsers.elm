module Music.Chart.Parsers exposing (..)

import Parser exposing (..)
import Music.Chart exposing (..)
import Music.Chord as Chord exposing (..)
import Music.Note as Note exposing (..)


newLine : Parser ()
newLine =
    symbol "\n"


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


spacesAndNewlines : Parser ()
spacesAndNewlines =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')


keepUntilEndOfLine : Parser String
keepUntilEndOfLine =
    keep oneOrMore (\c -> c /= '\n')


chart : Parser Chart
chart =
    let
        dashes =
            "---"
    in
        inContext "chart" <|
            succeed Chart
                |. spacesAndNewlines
                |. symbol dashes
                |. newLine
                |. symbol "title:"
                |. spaces
                |= keepUntilEndOfLine
                |. newLine
                |. symbol "key:"
                |. spaces
                |= note
                |. newLine
                |. symbol dashes
                |. spacesAndNewlines
                |= repeat oneOrMore (part |. spacesAndNewlines)
                |. end


part : Parser Part
part =
    inContext "part" <|
        succeed asPart
            |. symbol "="
            |. spaces
            |= keepUntilEndOfLine
            |. spacesAndNewlines
            |= oneOf
                [ succeed Just
                    |= repeat oneOrMore (bar |. spacesAndNewlines)
                , succeed Nothing
                ]


asPart : String -> Maybe (List Bar) -> Part
asPart name bars =
    case bars of
        Nothing ->
            PartRepeat name

        Just b ->
            Part name b


bar : Parser Bar
bar =
    inContext "bar" <|
        oneOf
            [ succeed BarRepeat
                |. symbol "-"
            , succeed Bar
                |= chords
            ]


chords : Parser (List Chord)
chords =
    inContext "chords" <|
        succeed (::)
            |= chord
            |= repeat zeroOrMore
                (succeed identity
                    |. symbol "/"
                    |= chord
                )


chord : Parser Chord
chord =
    inContext "chord" <|
        succeed (,)
            |= note
            |= quality


quality : Parser Quality
quality =
    inContext "quality" <|
        oneOfTuples Chord.qualitiesAndStringsHidingMajor


note : Parser Note
note =
    inContext "note" <|
        oneOfTuples Note.notesAndStrings


oneOfTuples : List ( a, String ) -> Parser a
oneOfTuples tuples =
    let
        -- Sort tuples to avoid having empty strings first, which would match in every cases.
        sortTuples =
            List.sortBy (Tuple.second >> String.length)
                >> List.reverse
    in
        oneOf
            (tuples
                |> sortTuples
                |> List.map
                    (\( x, name ) ->
                        succeed x
                            |. symbol name
                    )
            )
