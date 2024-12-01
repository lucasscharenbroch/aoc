#!/usr/bin/dyalogscript

input ← ⊃⎕NGET '1.input' 1

solve ← {+/⊃|-/{⍵[⍋⍵]}¨↓⍉↑⍎¨⍵}

⎕ ← solve input

solve2 ← {
    left right ← ↓⍉↑⍎¨⍵
    ((∪right)∘⍳¨left)(+/left×⌷¨)⊂({≢⍵}⌸right),0
}

⎕ ← solve2 input
