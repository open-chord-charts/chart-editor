module Music.Types exposing (..)


type alias Chart =
    { title : String
    , key : ChartKey
    , parts : List Part
    }


type Part
    = Part PartName (List Bar)
    | PartRepeat PartName


type alias PartName =
    String


type Bar
    = Bar ( Chord, Maybe Chord, Maybe Chord, Maybe Chord )
    | BarRepeat


type Chord
    = Chord ChromaticNote Quality


type ChromaticNote
    = Ab
    | A
    | Bb
    | B
    | C
    | Db
    | D
    | Eb
    | E
    | F
    | Gb
    | G


type Quality
    = Major
    | Minor


type ChartKey
    = ChartKey ChromaticNote
