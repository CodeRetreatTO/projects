[0]   r←life board
[1]   shape ← ⍴ board
[2]   stack ← (8, shape)⍴board
[3]   hrotate ← ⍉ (shape[0], 8) ⍴ 1 1 1 0 0 ¯1 ¯1 ¯1
[4]   vrotate ← ⍉ (shape[1], 8) ⍴ 1 0 ¯1 1 ¯1 1 0 ¯1
[5]   h ← hrotate⌽stack
[6]   v ← vrotate ⌽ [1] h
[7]   count ← +⌿ v
[8]   (count=3) ∨ board ∧ count = 2

