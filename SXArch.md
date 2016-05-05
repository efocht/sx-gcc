# SX Architecture #

This page is an entry point for other wiki or external pages describing
the NEC SX CPU architecture. The details of the SX architecture were collected
from documents freely available on the WEB or distributed without restrictions to
SX users.


## History ##

The first SX CPU (SX-1) was probably designed some time at the end of the 1980's.
It was a vector CPU built of several LSIs in bipolar technology.

The first SX Vector CPU in CMOS technology was launched in 1996, it was the NEC SX-4.
Since then the SX CPU instruction set was kept compatible, with only small modifications, while the functional units and technology have evolved dramatically.

The line of SX Vectorcomputers is documented at http://en.wikipedia.org/wiki/SX_architecture .


## CPU Details ##
  * [SX CPU Registers](SXRegisters.md)
  * [CPU functional units](SX_CPU_units.md)
  * [Instruction formats and addressing modes](SX_instr_fmts.md)
  * [Assembler mnemonics and opcodes](SXopcodes.md)
  * [SuperUX calling conventions:](SX_calling_conv.md) registers and return values
  * [Differences between SX floating point instruction semantics and standardized semantics](SX_float_std_diff.md)

## Memory ##

  * [Memory access](SX_memory.md)


## Other machine details ##

  * RCU, IXS, global memory
  * ...