(block foo
  (data (
    (set r0 (int 0x3))
    (set r1 (reg r0))))
  (control
    (goto bar)))

(block bar
  (data (
    (set r2 (add (int 0x7) (reg r3)))
    (set r0 (reg r2))))
  (control
    (goto foo)))
