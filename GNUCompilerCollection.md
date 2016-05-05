# Introduction #

# Back-end #

## Back-end files ##
Each machine dependent part goes into its own subdirectory of 'gcc/gcc/config'. In the case of SX port this will be the sub-directory 'sx'.

## Register definitions ##
Register in the GCC are numbered zero to an arbitrary number. There are two big classes of registers, hard registers that correspond to teh hardwares register set and pseudo registers that the compiler allocates for itself to setup the internal representation of code. The pseudo registers are mapped to hard registers in the code creation step.

The hardware registers for the SX will be numbered in the following way:

| Number | Registers | Names |
|:-------|:----------|:------|
| 0-127  | Scalar registers | $s0 - $s127 |
| 128 - 135 | Vector algorithmic registers | $v0 - $v7 |
| 136 - 199 | Vector data registers | $vd0 - $vd63 |
| 200 - 215 | Vector mask registers | $vm0 - $vm15 |
| 216    | Vector control register | $vc   |

The pseudo registers start then from 217.

(More to be added, like instruction counter (maybe), status register etc...)

## [RTL](RTL.md) ##

Internally the GNU compiler uses a so called [Register Transfer Language (RTL)](RTL.md) that describes the operations that the compiler made out of the language source text file. Optimizations are (partially) done in this language.