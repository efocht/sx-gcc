SX floating point instruction sometimes (mostly when dealing with special values or errors) have semantics that does not quite match the standards (IEEE etc.), and not what the gcc testsuite expects.

Strict adherence to standards is achieved using -mstrict-float (use software-assisted fp operations when hw semantics differs), or -msoft-float (use software fp exclusively).

Here we list the instructions that behave differently and how:
  * fmp, fmp@: after performing the operation, this function doesn't do the rounding on denormal results. On the other hand, the gcc tests expect the denormal results to be rounded.
  * cvs: after performing the operation, this function doesn't do the rounding on denormal results. On the other hand, the gcc tests expect the denormal results to be rounded.
  * flt, flt@: after performing the operation, this function doesn't do the rounding on denormal results. On the other hand, the gcc tests expect the denormal results to be rounded. See the [issue #50](https://code.google.com/p/sx-gcc/issues/detail?id=#50) in the sx-gcc bug tracker.
  * fltx: after performing the operation, this function doesn't do the rounding on denormal results. On the other hand, the gcc tests expect the denormal results to be rounded. See the [issue #50](https://code.google.com/p/sx-gcc/issues/detail?id=#50) in the sx-gcc bug tracker.