# Function calls #

## Function call preparation ##
  * Prepare argument list
  * Load entry address into register $s33
  * Set $s34 to the start of the argument list
  * Call function with `bsic $s32,($s33)`. That will store the current IC + 8 in register $s32 as the return address for the callee.

## Called function ##
  * Save registers $s1 to $s8 to stack ` stm $s1,8,-272(,$s2) `
  * Set current stack frame: ` lea $s1,-272(,$s2) `
  * Copy parameter argument pointer: ` or $s3,0,$s34 `
  * Jump to monitoring call, eventually ` bh $s8,-20(,$s33) `
  * Save return address on stack ` sts $s32,248(0,$s1) `
  * Not completed, yet

# Calling conventions #

This will describe the calling convention for C programs. Fortran or other languages might differ from this scheme.


# Return values #

Return values are sored in registers $s123 - $s127.

# Stack Layout #

This stack layout is the convention used in our implementation of SX-GCC. SuperUX and the native SX compilers have the order of ARGS and LOCALS reversed! That doesn't actually matter for ineroperability of code from SX-GCC and native compilers.

Stack grows from low towards high addresses. It is organised in the following manner (for a function "current" with N args, called by function "caller", and calling a set of functions "callees"):

```

$s2 ------>  +--------
             | 
             | RSA (current)
             |
             | 272
             |
             +-------- <----------- $s1 (callees)
             |
             | ARGS   (callees)
             |
             | LOCALS (current)
             |
             +-------- <----------- $s2 (caller)
             |
             |  RSA (caller)
             |  
             |  272
             |
$s1 -------> +--------
             |  ARGS (current)
             |
             |  ARG N - 1
             |  ...
             |  ARG 0
             |  N
$s34 ------> +--------

```

# Register Usage #
This section descibes the registers that are used in functions and are therfore not usable in the function itself because they serve a certain purpose that must be maintained during function execution.

| **Register** | **Purpose** |
|:-------------|:------------|
| $s0          | Max. address that the stack can expand to |
| $s1          | Hard frame pointer (bottom of stack for a function) |
| $s2          | Top of the current stack frame |
| $s3          | Function copy of $s34. When calling another function from within a function it is important to remeber the parameter entry because for the other function call $s34 must be overwritten |
| $s4          | Pointer to linkage section |
| $s32         | Return address. The callee stores it on its stack. |
| $s33         | Entry address of the current function. It seems this is not put onto the stack |
| $s34         | Pointer to the functions argument list. Set by the caller (and copied into $s3 by the function itself) |
| $s123 - $s127 | Function return value(s) |