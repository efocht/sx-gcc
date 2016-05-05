# Questions and Answers #

This section should serve as an entry point for questions and possible answers until we have a better system.

## Questions regarding SX ##

---

**Does the SX have instructions to move memory without registers? (matthias)**

No. You allways need to involve a register. But this is the same on all other CPUs
I know (EF).


---

**Does the SX have instructions to move a register into another? (matthias)**

Yes, you can use
```
or $s1,0,$s2
```
to load register s2 into s1.

---

## Questions regarding GNU Compiler Collection ##

---

**And the next question...**
No answer, yet!

---

## Questions regarding GNU Binutils ##

---

**I try to link fortran object files using the GNU binutils ld (for whatever arcane reason), and my executable crashes with SIGILL the moment I execute it.**

This is due to improper initialization of the centry symbol in case of the fortran entrypoint (MAIN). You need to link against libi90sxe.a, and explicitly pull the main symbol from there by adding `-u main` switch to the linker command line.


---

**And the next question...**
No answer, yet!

---
