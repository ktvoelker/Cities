
module Test where

import Layout
import Network
import Space

testNetwork = Network
  { nNodes =
    [ Node { nPos = Pos 1 1, nName = "Foo" }
    , Node { nPos = Pos 3 3, nName = "Bar" }
    , Node { nPos = Pos 5 5, nName = "Baz" }
    ]
  , nLines =
    [ Line
      { lEdges  = [Edge (Pos 1 1) (Pos 5 5)]
      , lName   = "Foobaz"
      , lSymbol = Symbol (Color 0 0 0) "FZ" Circle
      , lColor  = Color 0 0 0
      }
    , Line
      { lEdges  = [Edge (Pos 3 3) (Pos 5 5), Edge (Pos 1 1) (Pos 3 3)]
      , lName   = "Foobarbaz"
      , lSymbol = Symbol (Color 0 0 0) "FB" Circle
      , lColor  = Color 0 0 0
      }
    ]
  , nLineCount = 2
  }

testLayout = initialLayout testNetwork

