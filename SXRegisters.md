# SX Registers #


Normal registers:
  * 128 Scalar registers (64 bit wide; S0, S1,..., S127)
  * 8 Vector Arithmetic registers (64 bit wide, max. 256 Elements)
  * 64 Vector Data registers
  * 1 Vector Control register
  * 16 VM Vector Mask registers (256 bit wide; VM0, ..., VM15)
  * 1 VL Vector Length register (10 bit wide)
  * 1 VC Vector Control register (256 bit wide)

  * 1 PSW (process status word) register (64 bit wide)
  * 1 IC (instruction counter) (64 bit wide)
  * 1 CPSB (current process status block) (64 bit wide)
  * 1 NPSB (next process status block) (64 bit wide)
  * TM Timer register (40 bits)
  * EX Execution counter
  * VX Vector exection counter
  * VE Vector element counter

Miscellaneous registers:
  * Vector execution clock counter
  * Floating-point data execution element counter
  * Cache miss clock counter
  * Bank conflict clock counter
  * Debugging identifier register
  * Store address register
  * Instruction cache miss clock counter
  * Security identifier register
  * Branch prediction failure counter
  * User clock counter
  * Instruction pipeline hold clock counter
  * Vector operation execution clock counter
  * Vector load execution clock counter