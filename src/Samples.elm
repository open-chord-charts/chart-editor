module Samples exposing (..)


samples : List String
samples =
    [ test
    , allOfMe
    ]


test : String
test =
    """
---
title: Chords chart grammar
key: C
---

= A
C - A7/D7 - Gm/Eb7/D7 Ab/C7/Fm/Eb6
"""


allOfMe : String
allOfMe =
    """
---
title: All of me
key: C
---

= A
C - E7 - A7 - Dm -

= B
E7 - Am - D7 - G7 -

= A

= C
F Fm C A7 Dø G7 C -
"""
