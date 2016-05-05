# SX Assembler Mnemonics #

Huge list should be added here...
Overall 336 instruction mnemonics, including instructions available only on certain versions of the processor.
Include them all as a table?

### Instruction Add-ons (mnemonic suffixes) ###

| `*` | enable masked operation |
|:----|:------------------------|
| `%` | specify an arithmetic unit C<sub>f</sub> = 1 |
| `?` | specify selection mode C<sub>m</sub> = 1 |
| `=` | clear high order 32 bits, C<sub>m</sub> = 1 for VLDL, VLDLX, VGTL, C<sub>x</sub>=1 for LDL, DLDL |
| `<` | static prediction for no branching |
| `>` | static prediction for branching |
| `!` | enable extended operation C<sub>x</sub>=1 |
| `@` | single prectision for floating point data and arithmetic |