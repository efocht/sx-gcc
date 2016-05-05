;;- Machine description for NEC SX architecture for GCC compiler
;;   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
;;   2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
;;   Original DLX version contributed by Aaron Sawdey and Carsten Meyer

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;;
;; Base code donated (2008).
;; Some parts: Copyright (c) 1999- Takayoshi Kochi (gcc-2.95 version)
;; 
;; Copyright (c) 2008 Erich Focht <efocht@hpce.nec.com>
;; Copyright (c) 2008 Jaka Mocnik <jaka@xlab.si>
;; Copyright (c) 2008 Marko Novak <marko.novak@xlab.si>
;; Copyright (c) 2008 Fredrik Unger <funger@hpce.nec.com>
;; 
;;
;; $Id: sx.md 248 2009-02-18 16:17:49Z jaka $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;* Immediate integer operand constraints: (sx.h: CONST_OK_FOR_LETTER_P())
;;
;;  I : -64 .. 63
;;  J : 0 .. 31
;;  K : 0 .. 63
;;  L : 0 .. 128
;;  M : (m)0 or (m)1 form (m=0..63)
;;  N : -2147483648 .. -65 || 64 .. 2147483647
;;
;;
;;* Assembler specs: (cf. print_operand())
;;
;;  "%C"    conditional
;;  "%N"    reverse conditional
;;  "%M"    (m)0 or (m)1 immediate
;;  "%H"    upper 32 bits
;;  "%L"    lower 32 bits
;;  "%S"    symbol reference
;;  "%U"    unsigned int/long
;;  "%G"    signed int/long
;;  "%F"    target format representation of floating point



(define_constants
  [
  (UNSPEC_ADDRESS_FIRST        100)
  (UNSPEC_LOAD_DI_HIGH         101)
  (UNSPEC_LOAD_PROCMASK        102)
  (UNSPEC_STORE_PROCMASK       103)
  (UNSPEC_MONC                 104)
  ]
)

;; UNSPEC_VOLATILE
(define_constants
  [
  (UNSPECV_BLOCKAGE               1)
  (UNSPECV_ICACHEFLUSH            2)
  (UNSPECV_DCACHEFLUSH            3)
  ]
)

;; Include predicate definitions
(include "predicates.md")

;; instruction type
(define_attr "type" 
  "unknown,load,store,move,alu,branch,jump,fp,multi" 
  (const_string "unknown"))

;; instruction mode
(define_attr "mode" "unknown,none,QI,HI,SI,DI,SF,DF" 
             (const_string "unknown"))

;; # instructions 
(define_attr "length" "" (const_int 1))


(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (plus:SF (match_operand:SF 1 "register_operand" "%r")
                 (match_operand:SF 2 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "fad@\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "SF")])

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (plus:DF (match_operand:DF 1 "register_operand" "%r")
                 (match_operand:DF 2 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "fad\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "DF")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (minus:SF (match_operand:SF 1 "register_operand" "r")
                 (match_operand:SF 2 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "fsb@\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "SF")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (minus:DF (match_operand:DF 1 "register_operand" "r")
                 (match_operand:DF 2 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "fsb\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "DF")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (mult:SF (match_operand:SF 1 "register_operand" "%r")
                 (match_operand:SF 2 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT && ! TARGET_STRICT_FLOAT"
  "fmp@\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "SF")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (mult:DF (match_operand:DF 1 "register_operand" "%r")
                 (match_operand:DF 2 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT && ! TARGET_STRICT_FLOAT"
  "fmp\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "DF")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (div:SF (match_operand:SF 1 "register_operand" "r")
                (match_operand:SF 2 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "fdv@\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "SF")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (div:DF (match_operand:DF 1 "register_operand" "r")
                (match_operand:DF 2 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "fdv\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "DF")])

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r,r,r")
        (plus:SI (match_operand:SI 1 "reg_or_I_operand" "r,I,r")
                 (match_operand:SI 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   ads\\t%0,%1,%2
   ads\\t%0,%1,%2
   ads\\t%0,%1,%M2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")
   (set_attr "length" "1,1,1")])


;; (define_insn ""
;;   [(set (match_operand:DI 0 "general_operand" "=r")
;;         (plus:DI (match_operand:DI 1 "general_operand" "r")
;;                  (match_operand:SI 2 "general_operand" "i")))]
;;   ""
;;   "lea\\t%0,%2(,%1)"
;;   [(set_attr "type"     "alu")
;;    (set_attr "mode"     "DI")
;;    (set_attr "length" "1")])

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand"         "=r,r,r,r")
        (plus:DI (match_operand:DI 1 "register_operand" "r,r,r,r")
                 (match_operand:DI 2 "general_operand"  "r,M,I,N")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == REG)
    return \"add\\t%0,%1,%2\";
  else if (GET_CODE (operands[2]) == CONST_INT &&
           CONST_OK_FOR_LETTER_P ((INTVAL (operands[2])), 'M'))
    return \"adx\\t%0,%1,%M2\";
  else if (GET_CODE (operands[2]) == CONST_INT &&
           CONST_OK_FOR_LETTER_P ((INTVAL (operands[2])), 'I'))
    return \"adx\\t%0,%2,%1\";
  else if (GET_CODE (operands[2]) == LABEL_REF ||
           GET_CODE (operands[2]) == SYMBOL_REF)
    return \"lea\\t%0,%2(%1)\";
  else
    return \"lea\\t%0,%2(,%1)\";
}"
   [(set_attr "type"     "alu")
    (set_attr "mode"     "DI")
    (set_attr "length" "1,1,1,1")])


(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (minus:SI (match_operand:SI 1 "reg_or_I_operand" "r,I,r")
                  (match_operand:SI 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   sbs\\t%0,%1,%2
   sbs\\t%0,%1,%2
   sbs\\t%0,%1,%M2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")
   (set_attr "length" "1,1,1")])


(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (minus:DI (match_operand:DI 1 "reg_or_I_operand" "r,I,r")
                  (match_operand:DI 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   sbx\\t%0,%1,%2
   sbx\\t%0,%1,%2
   sbx\\t%0,%1,%M2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "DI")
   (set_attr "length" "1,1,1")])


(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r,r,r")
        (and:SI (match_operand:SI 1 "reg_or_I_operand"  "r,I,r")
                (match_operand:SI 2 "reg_or_M_operand"  "r,r,M")))]
  ""
  "@
   and\\t%0,%1,%2
   and\\t%0,%1,%2
   and\\t%0,%1,%M2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "1,1,1")])


(define_insn "anddi3"
  [(set (match_operand:DI 0 "register_operand"        "=r,r,r")
        (and:DI (match_operand:DI 1 "reg_or_I_operand" "r,I,r")
                (match_operand:DI 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   and\\t%0,%1,%2
   and\\t%0,%1,%2
   and\\t%0,%1,%M2"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1,1,1")])


(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand"        "=r,r,r")
        (ior:SI (match_operand:SI 1 "reg_or_I_operand" "r,I,r")
                (match_operand:SI 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   or\\t%0,%1,%2
   or\\t%0,%1,%2
   or\\t%0,%1,%M2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "1,1,1")])


(define_insn "iordi3"
  [(set (match_operand:DI 0 "register_operand"        "=r,r,r")
        (ior:DI (match_operand:DI 1 "reg_or_I_operand" "r,I,r")
                (match_operand:DI 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   or\\t%0,%1,%2
   or\\t%0,%1,%2
   or\\t%0,%1,%M2"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1,1,1")])


(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r,r,r")
        (xor:SI (match_operand:SI 1 "register_operand"  "r,I,r")
                (match_operand:SI 2 "nonmemory_operand" "r,r,M")))]
  ""
  "@
   xor\\t%0,%1,%2
   xor\\t%0,%1,%2
   xor\\t%0,%1,%M2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "1,1,1")])


(define_insn "xordi3"
  [(set (match_operand:DI 0 "register_operand"         "=r,r,r")
        (xor:DI (match_operand:DI 1 "register_operand"  "r,I,r")
                (match_operand:DI 2 "nonmemory_operand" "r,r,M")))]
  ""
  "@
   xor\\t%0,%1,%2
   xor\\t%0,%1,%2
   xor\\t%0,%1,%M2"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1,1,1")])

;;
;; -x = 0-x
;; This is ok for integers.
;;

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sbs\\t%0,0,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")])

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (neg:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "sbx\\t%0,0,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")])

;;
;; When negating floating point numbers it is enough to negate the sign bit 
;; using the XOR operation. This solves the issue that subtraction operations
;; are having with negative zeros.
;;

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (neg:SF (match_operand:SF 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "xor\\t%0,%1,(1)1"
  [(set_attr "type" "alu")
   (set_attr "mode" "SF")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (neg:DF (match_operand:DF 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "xor\\t%0,%1,(1)1"
  [(set_attr "type" "alu")
   (set_attr "mode" "DF")])


(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (not:QI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "xor\\t%0,%1,(56)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "1")])

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (not:HI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "xor\\t%0,%1,(48)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "1")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "xor\\t%0,%1,(32)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "1")])

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (not:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "xor\\t%0,%1,(0)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (ashift:SI (match_operand:SI 1 "reg_or_M_operand" "r,r,M")
                   (match_operand:SI 2 "reg_or_J_operand" "r,J,r")))]
  ""
  "@
   sla\\t%0,%1,%2
   sla\\t%0,%1,%2
   sla\\t%0,%M1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")])

(define_insn "ashldi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (ashift:DI (match_operand:DI 1 "reg_or_M_operand" "r,r,M")
                   (match_operand:DI 2 "reg_or_K_operand" "r,K,r")))]
  ""
  "@
   slax\\t%0,%1,%2
   slax\\t%0,%1,%2
   slax\\t%0,%M1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")])

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (ashiftrt:SI (match_operand:SI 1 "reg_or_M_operand" "r,r,M")
                     (match_operand:SI 2 "reg_or_J_operand" "r,J,r")))]
  ""
  "@
   sra\\t%0,%1,%2
   sra\\t%0,%1,%2
   sra\\t%0,%M1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")])

(define_insn "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (ashiftrt:DI (match_operand:DI 1 "reg_or_M_operand" "r,r,M")
                     (match_operand:DI 2 "reg_or_K_operand" "r,K,r")))]
  ""
  "@
   srax\\t%0,%1,%2
   srax\\t%0,%1,%2
   srax\\t%0,%M1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (lshiftrt:SI (match_operand:SI 1 "reg_or_M_operand" "r,r,M")
                     (match_operand:SI 2 "reg_or_K_operand" "r,K,r")))]
  ""
  "@
   srl\\t%0,%1,%2
   srl\\t%0,%1,%2
   srl\\t%0,%M1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")])

(define_insn "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (lshiftrt:DI (match_operand:DI 1 "reg_or_M_operand" "r,r,M")
                     (match_operand:DI 2 "reg_or_K_operand" "r,K,r")))]
  ""
  "@
   srl\\t%0,%1,%2
   srl\\t%0,%1,%2
   srl\\t%0,%M1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")])

;
; count leading zeros
;
(define_insn "clzdi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(clz:DI (match_operand:DI 1 "reg_or_M_operand" "r,M")))]
  ""
  "@
   ldz\\t%0,%1
   ldz\\t%0,%M1"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")])

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (mult:SI (match_operand:SI 1 "reg_or_I_operand" "r,I,r")
                 (match_operand:SI 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   mps\\t%0,%1,%2
   mps\\t%0,%1,%2
   mps\\t%0,%1,%M2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")])

(define_insn "muldi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (mult:DI (match_operand:DI 1 "reg_or_I_operand" "r,I,r")
                 (match_operand:DI 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   mpx\\t%0,%1,%2
   mpx\\t%0,%1,%2
   mpx\\t%0,%1,%M2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "DI")])


;;
;; Replacing explicit insn using floating point division by expansion,
;; this way the compiler can optimize around and we don't need to care
;; about the clobbered registers.
;; Written by Fred, inspired by ia64 code.
;; TODO: divdi3 : for 56 bit numbers use similar code, and call library
;;                function when number is larger than 56 bits.
;;
(define_expand "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (div:SI (match_operand:SI 1 "register_operand" "r")
                (match_operand:SI 2 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
{
  rtx op1_df, op2_df, op0_df;

  op0_df = gen_reg_rtx (DFmode);

  op1_df = gen_reg_rtx (DFmode);
  expand_float (op1_df, operands[1], 0);

  op2_df = gen_reg_rtx (DFmode);
  expand_float (op2_df, operands[2], 0);

  emit_insn (gen_divdf3 (op0_df, op1_df, op2_df));

  expand_fix(operands[0], op0_df, 0);

  DONE;
})

(define_expand "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (udiv:SI (match_operand:SI 1 "register_operand" "r")
                 (match_operand:SI 2 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
{
  rtx op1_df, op2_df, op0_df;

  op0_df = gen_reg_rtx (DFmode);

  op1_df = gen_reg_rtx (DFmode);
  expand_float (op1_df, operands[1], 1);

  op2_df = gen_reg_rtx (DFmode);
  expand_float (op2_df, operands[2], 1);

  emit_insn (gen_divdf3 (op0_df, op1_df, op2_df));

  expand_fix (operands[0], op0_df, 1);

  DONE;
})

(define_expand "modsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (mod:SI (match_operand:SI 1 "register_operand" "r")
                (match_operand:SI 2 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
{
  rtx div_floor, prod;

  div_floor = gen_reg_rtx (SImode);
  prod = gen_reg_rtx (SImode);

  emit_insn (gen_divsi3 (div_floor, operands[1], operands[2]));
  emit_insn (gen_mulsi3 (prod, operands[2], div_floor));
  emit_insn (gen_subsi3 (operands[0], operands[1], prod));

  DONE;
})

(define_expand "umodsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (umod:SI (match_operand:SI 1 "register_operand" "r")
                 (match_operand:SI 2 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
{
  rtx div_floor, prod;

  div_floor = gen_reg_rtx (SImode);
  prod = gen_reg_rtx (SImode);

  emit_insn (gen_udivsi3 (div_floor, operands[1], operands[2]));
  emit_insn (gen_mulsi3 (prod, operands[2], div_floor));
  emit_insn (gen_subsi3 (operands[0], operands[1], prod));

  DONE;
})

;;
;; Conversion patterns
;;

;; signed int -> float

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (float:SF (match_operand:SI 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT && ! TARGET_STRICT_FLOAT"
  "flt@\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "1") ])

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (float:SF (match_operand:DI 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT && ! TARGET_STRICT_FLOAT"
  "fltx\\t%0,%1\\n\\tcvs\\t%0,%0"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "2") ])

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (float:DF (match_operand:SI 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "flt\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "1")])

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (float:DF (match_operand:DI 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT && ! TARGET_STRICT_FLOAT"
  "fltx\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "1")])

;; unsigned int -> float

(define_insn "floatunssisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (unsigned_float:SF (match_operand:SI 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT && ! TARGET_STRICT_FLOAT"
  "and\\t%1,%1,(32)0\\n\\tfltx\\t%0,%1\\n\\tcvs\\t%0,%0"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "1") ])

;; NOTE: default expands is OK, no insn needed.
;;
;;(define_insn "floatunsdisf2"
;;  [(set (match_operand:SF 0 "register_operand" "=r")
;;        (unsigned_float:SF (match_operand:DI 1 "register_operand" "r")))]
;;  ""
;;  "fltx\\t%0,%1\\n\\tcvs\\t%0,%0"
;;  [(set_attr "type" "fp")
;;   (set_attr "mode" "SF")
;;   (set_attr "length" "2") ])

(define_insn "floatunssidf2"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (unsigned_float:DF (match_operand:SI 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "and\\t%1,%1,(32)0\\n\\tfltx\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "1")])

;; NOTE: default expand is NOT OK for very high ULL numbers (issue 67), need
;; to fix that; then, insn will not be needed.
;;
;;(define_insn "floatunsdidf2"
;;  [(set (match_operand:DF 0 "register_operand" "=r")
;;        (unsigned_float:DF (match_operand:DI 1 "register_operand" "r")))]
;;  ""
;;  "fltx\\t%0,%1"
;;  [(set_attr "type" "fp")
;;   (set_attr "mode" "DF")
;;   (set_attr "length" "1")])

;; float -> int

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (fix:SI (match_operand:SF 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "fix@\\t%0,%1,1"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "1")])

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (fix:DI (match_operand:SF 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT && ! TARGET_STRICT_FLOAT"
  "fix@\\t%0,%1,1\\n\\tmpy\\t%0,1,%0"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "2")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (fix:SI (match_operand:DF 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "fix\\t%0,%1,1"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "1")])

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (fix:DI (match_operand:DF 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "fixx\\t%0,%1,1"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "1")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (float_truncate:SF (match_operand:DF 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT && ! TARGET_STRICT_FLOAT"
  "cvs\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "1")])


;; Assume QI,HI, and SI hold lower 32bit of S register (upper 32bit is 0)
;; In case of sign extension of QI and HI, lower 32bit is filled.
;;

(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,m")
        (truncate:SI (match_operand:DI 1 "register_operand" "r,r")))]
  ""
  "@
   and\\t%0,%1,(32)0
   stl\\t%1,%0"
  [(set_attr "type" "alu,store")
   (set_attr "mode" "SI")
   (set_attr "length" "1,20")])

(define_insn "truncdihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,m")
        (truncate:HI (match_operand:DI 1 "register_operand" "r,r")))]
  ""
  "@
   and\\t%0,%1,(48)0
   st2b\\t%1,%0"
  [(set_attr "type" "alu,store")
   (set_attr "mode" "HI")
   (set_attr "length" "1,20")])

(define_insn "truncdiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,m")
        (truncate:QI (match_operand:DI 1 "register_operand" "r,r")))]
  ""
  "@
   and\\t%0,%1,(56)0
   st1b\\t%1,%0"
  [(set_attr "type" "alu,store")
   (set_attr "mode" "QI")
   (set_attr "length" "1,20")])

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,m")
        (truncate:HI (match_operand:SI 1 "register_operand" "r,r")))]
  ""
  "@
   and\\t%0,%1,(48)0
   st2b\\t%1,%0"
  [(set_attr "type" "alu,store")
   (set_attr "mode" "HI")
   (set_attr "length" "1,20")])

(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,m")
        (truncate:QI (match_operand:SI 1 "register_operand" "r,r")))]
  ""
  "@
   and\\t%0,%1,(56)0
   st1b\\t%1,%0"
  [(set_attr "type" "alu,store")
   (set_attr "mode" "QI")
   (set_attr "length" "1,1")])

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,m")
        (truncate:QI (match_operand:HI 1 "register_operand" "r,r")))]
  ""
  "@
   and\\t%0,%1,(56)0
   st1b\\t%1,%0"
  [(set_attr "type" "alu,store")
   (set_attr "mode" "QI")
   (set_attr "length" "1,20")])

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (zero_extend:HI (match_operand:QI 1 "general_operand" "r,m")))]
  ""
  "@
   and\\t%0,%1,(56)0
   ld1b\\t%0,%1"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "HI")
   (set_attr "length" "1,20")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (zero_extend:SI (match_operand:QI 1 "general_operand" "r,m")))]
  ""
  "@
   and\\t%0,%1,(56)0
   ld1b\\t%0,%1"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "SI")
   (set_attr "length" "1,20")])

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (zero_extend:SI (match_operand:HI 1 "general_operand" "r,m")))]
  ""
  "@
   and\\t%0,%1,(48)0
   ld2b\\t%0,%1\\n\\tand\\t%0,%0,(48)0"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "SI")
   (set_attr "length" "1,21")])

(define_insn "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (zero_extend:DI (match_operand:QI 1 "general_operand" "r,m")))]
  ""
  "@
   and\\t%0,%1,(56)0
   ld1b\\t%0,%1"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "DI")
   (set_attr "length" "1,20")])

(define_insn "zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (zero_extend:DI (match_operand:HI 1 "general_operand" "r,m")))]
  ""
  "@
   and\\t%0,%1,(48)0
   ld2b\\t%0,%1\\n\\tand\\t%0,%0,(48)0"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "DI")
   (set_attr "length" "1,21")])

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (zero_extend:DI (match_operand:SI 1 "general_operand" "r,m")))]
  ""
  "@
   and\\t%0,%1,(32)0
   ldl=\\t%0,%1"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "DI")
   (set_attr "length" "1,20")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (sign_extend:HI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "sll\\t%0,%1,24\\n\\tsra\\t%0,%0,24"
  [(set_attr "type" "alu")
   (set_attr "mode" "HI")
   (set_attr "length" "2")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (sign_extend:SI (match_operand:QI 1 "general_operand" "r,m")))]
  ""
  "@
   sll\\t%0,%1,24\\n\\tsra\\t%0,%0,24
   ld1b\\t%0,%1\\n\tsll\\t%0,%0,24\\n\\tsra\\t%0,%0,24"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "SI")
   (set_attr "length" "2,22")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (sign_extend:SI (match_operand:HI 1 "general_operand" "r,m")))]
  ""
  "@
   sll\\t%0,%1,16\\n\\tsra\\t%0,%0,16
   ld2b\\t%0,%1\\n\\tand\\t%0,%0,(32)0"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "SI")
   (set_attr "length" "2,21")])

(define_insn "extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (sign_extend:DI (match_operand:QI 1 "general_operand" "r,m")))]
  ""
  "@
   sll\\t%0,%1,56\\n\\tsrax\\t%0,%0,56
   ld1b\\t%0,%1\\n\tsll\\t%0,%0,56\\n\\tsrax\\t%0,%0,56"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "DI")
   (set_attr "length" "2,22")])

(define_insn "extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (sign_extend:DI (match_operand:HI 1 "general_operand" "r,m")))]
  ""
  "@
   sll\\t%0,%1,48\\n\\tsrax\\t%0,%0,48
   ld2b\\t%0,%1"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "DI")
   (set_attr "length" "2,20")])

;EF;; This was replaced by the expand below. Keeping the commented code
;EF;; in case we need it again.
;EF;(define_insn "extendsidi2"
;EF;  [(set (match_operand:DI 0 "register_operand" "=r,r")
;EF;        (sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "r,m")))]
;EF;  ""
;EF;  "@
;EF;   sll\\t%0,%1,32\\n\\tsrax\\t%0,%0,32
;EF;   ldl\\t%0,%1"
;EF;  [(set_attr "type" "alu,load")
;EF;   (set_attr "mode" "DI")
;EF;   (set_attr "length" "1,20")])

; There might be better ways to do this, actually...
;EF;see below;(define_expand "extendsidi2"
;EF;see below;  [(set (match_operand:DI 0 "register_operand" "=r")
;EF;see below;        (sign_extend:DI (match_operand:SI 1 "register_operand" "r")))]
;EF;see below;  ""
;EF;see below;{
;EF;see below;  rtx tmp = gen_rtx_SUBREG(DImode, operands[1], 0);
;EF;see below;
;EF;see below;  emit_insn(gen_ashldi3(operands[0], tmp, GEN_INT(32)));
;EF;see below;  emit_insn(gen_ashrdi3(operands[0], operands[0], GEN_INT(32)));
;EF;see below;  DONE;
;EF;see below;})

(define_insn_and_split "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "")
        (sign_extend:DI (match_operand:SI 1 "register_operand" "")))]
  ""
  "#"
  ""
  [(set (match_dup 0)
        (ashift:DI (match_dup 1)
                   (const_int 32)))
   (set (match_dup 0)
        (ashiftrt:DI (match_dup 0)
                     (const_int 32)))]
"
{ operands[1] = gen_lowpart (DImode, operands[1]); }"
)


(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (float_extend:DF (match_operand:SF 1 "register_operand" "r")))]
  "! TARGET_SOFT_FLOAT"
  "cvd\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "1")])

;;
;; move instructions:
;; mem-reg, reg-mem, imm-reg
;;
;; The standard "movdi" pattern for RTL generation. It
;; makes sure one of the operands is in a register, but 
;; avoids trying to do this later during compilation when
;; the register allocation is complete.
;;

;;
;; Trying an alternative which takes care of 64bit
;; immediate loads
;;
(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
        (match_operand:DI 1 "movdi_target_operand" ""))]
  ""
{
  if (mem_mem_operand(operands[1], DImode))
    {
      /* if we encounter such an expansion after register allocator has
         finished, god help compiler ... because we can't, not without
         introducing another reg */
      gcc_assert(!no_new_pseudos);
      sx_emit_mem_mem_mov(operands[0], operands[1]);
      DONE;
    }
  if (sx_expand_mov(DImode, operands))
    {
      DONE;
    }
  if (CONSTANT_POOL_ADDRESS_P(operands[1]) &&
      sx_mov_source_needs_s4(operands[1]))
    {
      sx_emit_mov_via_s4(operands[0], operands[1]);
      DONE;
    }
})

;;
;; this movdi expansion is used for loads via implicit base register s4
;;

(define_insn "*movdi_base_s4"
  [(parallel [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r")
                   (match_operand:DI 1 "memory_operand" "m,s"))
              (use (reg:DI 4))])]
  ""
{
  switch(which_alternative)
    {
    case 0:
      return "lds\t%0,%1";
    case 1:
      return "lea\t%0,%1";
    default:
      gcc_unreachable();
    }
}
  [(set_attr "type" "load,move")
   (set_attr "mode" "DI")
   (set_attr "length" "20,1")])

;;
;; This is used to match against RTL during assembly code
;; generation. It knows how to move floats in and out of 
;; integer registers.
;;

(define_insn "*movdi_general"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r,r,m,r")
        (match_operand:DI 1 "movdi_target_operand" "I,M,N,m,r,r"))]
  ""
{ 
  switch(which_alternative)
    {
    case 0:
      return "or\t%0,%G1,(0)1";
    case 1:
      return "or\t%0,0,%M1";
    case 2:
      gcc_assert(GET_CODE(operands[1]) == CONST_INT);
      return "lea\t%0,%G1(0)";
    case 3:
      return "lds\t%0,%1";
    case 4:
      return "sts\t%1,%0";
    case 5:
      return "or\t%0,0,%1";
    default:
      gcc_unreachable();
   }
}
  [(set_attr "type" "move,move,move,load,store,move")
   (set_attr "mode" "DI")
   (set_attr "length" "1,1,1,20,20,1")])

;; this one is required by RTL prologue for setting pointer to linkage
;; section ($s4)
(define_insn "*movdi_dispdiff"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (plus:DI (match_operand:DI 1 "register_operand" "r")
                 (minus:DI (match_operand:DI 2 "symref_operand" "")
                           (match_operand:DI 3 "symref_operand" ""))))]
  ""
  "lea\t%0,%2-%3(,%1)"
  [(set_attr "type" "move")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])


(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
        (match_operand:SI 1 "general_operand" ""))]
  ""
{
  if(GET_CODE(operands[1]) == SYMBOL_REF ||
     GET_CODE(operands[1]) == LABEL_REF)
    gcc_unreachable();

  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], SImode)
      && !register_operand (operands[1], SImode))
    {
      rtx temp = force_reg (SImode, operands[1]);
      temp = emit_move_insn (operands[0], temp);
      DONE;
    }
})

;;
;; This is used to match against RTL during assembly code
;; generation. It knows how to move floats in and out of 
;; integer registers.
;;

(define_insn "movsi_general"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,m,r")
        (match_operand:SI 1 "general_operand"  "I,M,i,m,r,r"))]
  ""
  "@
  or\\t%0,%G1,(0)1
  or\\t%0,0,%M1
  lea\\t%0,%1(0)
  ldl=\\t%0,%1
  stl\\t%1,%0
  or\\t%0,0,%1"
  [(set_attr "type" "move,move,move,load,store,move")
   (set_attr "mode" "SI")
   (set_attr "length" "1,1,1,20,20,1")])

;;
;; Move half words.
;;
(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
        (match_operand:HI 1 "general_operand" ""))]
  ""
{
  if(GET_CODE(operands[1]) == SYMBOL_REF ||
     GET_CODE(operands[1]) == LABEL_REF)
    gcc_unreachable();

  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], HImode)
      && !register_operand (operands[1], HImode))
    {
      rtx temp = force_reg (HImode, operands[1]);
      temp = emit_move_insn (operands[0], temp);
      DONE;
    }
})

(define_insn "movhi_general"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,m,r")
        (match_operand:HI 1 "general_operand"  "I,M,i,m,r,r"))]
  ""
  "@
  or\\t%0,%G1,(0)1
  or\\t%0,0,%M1
  lea\\t%0,%1
  ld2b\\t%0,%1\\n\\tand\\t%0,%0,(32)0
  st2b\\t%1,%0
  or\\t%0,0,%1"
  [(set_attr "type" "move,move,move,load,store,move")
   (set_attr "mode" "HI")
   (set_attr "length" "1,1,1,21,20,1")])

;;
;; move bytes.
;;
(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
        (match_operand:QI 1 "general_operand" ""))]
  ""
{
  if(GET_CODE(operands[1]) == SYMBOL_REF ||
     GET_CODE(operands[1]) == LABEL_REF)
    gcc_unreachable();

  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], QImode)
      && !register_operand (operands[1], QImode))
    {
      rtx temp = force_reg (QImode, operands[1]);
      temp = emit_move_insn (operands[0], temp);
      DONE;
    }
})

(define_insn "movqi_general"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,r,m,r")
        (match_operand:QI 1 "general_operand"  "I,M,i,m,r,r"))]
  ""
  "@
  or\\t%0,%G1,(0)1
  or\\t%0,0,%M1
  lea\\t%0,%1
  ld1b\\t%0,%1\\n\\tsll\\t%0,%0,24\\n\\tsra\\t%0,%0,24
  st1b\\t%1,%0
  or\\t%0,0,%1"
  [(set_attr "type" "move,move,move,load,store,move")
   (set_attr "mode" "QI")
   (set_attr "length" "1,1,1,22,20,1")])

;;
;; Move floats.
;;

;------ SF move

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
        (match_operand:SF 1 "general_operand" ""))]
  ""
{
  if(GET_CODE(operands[1]) == SYMBOL_REF ||
     GET_CODE(operands[1]) == LABEL_REF)
    gcc_unreachable();

  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], SFmode)
      && !register_operand (operands[1], SFmode))
    {
      rtx temp = force_reg (SFmode, operands[1]);
      temp = emit_move_insn (operands[0], temp);
      DONE;
    }
})

;; case 2: 
;; This pattern has to handle the strange special case of
;; moving a float value into r123 to return it. This is required
;; to be compatible with Peter Dahl's C-Regs C Compiler.
;;

(define_insn "movsf_immediate"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (match_operand:SF 1 "fp_I_operand"     "F"))]
  ""
  "flt@\\t%0,%G1"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "1")])

(define_insn "movsf_general"
  [(set (match_operand:SF 0 "reg_or_mem_operand" "=r,m,r")
        (match_operand:SF 1 "reg_or_mem_operand"  "r,r,m"))]
  ""
  "@
  and\\t%0,%1,(32)1 # EF: sure?
  stu\\t%1,%0
  ldu\\t%0,%1"
  [(set_attr "type" "load,store,move")
   (set_attr "mode" "SF")
   (set_attr "length" "1,20,20")])

;; case 3:
;; this pattern loads a 32-bit float immediate via lea by putting target
;; format of the float into displacement field of ASX param to lea
(define_insn "movsf_lea"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (match_operand:SF 1 "fp_lea_I_operand" "F"))]
  ""
  "lea\\t%0,%F1(0)\\n\\tslax\\t%0,%0,32"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "2")])

;;----------- DF move

(define_expand "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "")
        (match_operand:DF 1 "general_operand" ""))]
  ""
{
  if(GET_CODE(operands[1]) == SYMBOL_REF ||
     GET_CODE(operands[1]) == LABEL_REF)
    gcc_unreachable();

  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], DFmode)
      && !register_operand (operands[1], DFmode))
    {
      rtx temp = force_reg (DFmode, operands[1]);
      temp = emit_move_insn (operands[0], temp);
      DONE;
    }
})


(define_insn "movdf_immediate"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (match_operand:DF 1 "fp_I_operand"     "F"))]
  ""
  "fltx\\t%0,%G1"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "1")])

(define_insn "movdf_general"
  [(set (match_operand:DF 0 "reg_or_mem_operand" "=r,m,r")
        (match_operand:DF 1 "reg_or_mem_operand"  "r,r,m"))]
  ""
  "@
  or\\t%0,0,%1
  sts\\t%1,%0
  lds\\t%0,%1"
  [(set_attr "type" "load,store,move")
   (set_attr "mode" "DF")
   (set_attr "length" "1,20,20")])

;;
;; from alpha
;;
(define_expand "movtf"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
        (match_operand:TF 1 "general_operand" ""))]
  ""
{
  /* alpha style */
  if (GET_CODE (operands[0]) == MEM
      && ! reg_or_0_operand (operands[1], TFmode))
    operands[1] = force_reg (TFmode, operands[1]);
})

(define_insn_and_split "*movtf_internal"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=r,r,o")
        (match_operand:TF 1 "input_operand"        "ri,m,r"))]
  "register_operand (operands[0], TFmode)
   || reg_or_0_operand (operands[1], TFmode)"
  "#"
  "reload_completed"
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 1) (match_dup 3))]
{
  sx_split_tmode_pair (operands, TFmode, true);
})

;; We need to prevent reload from splitting TImode moves, because it
;; might decide to overwrite a pointer with the value it points to.
;; In that case we have to do the loads in the appropriate order so
;; that the pointer is not destroyed too early.

(define_insn_and_split "*movti_internal"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=r,o")
        (match_operand:TI 1 "input_operand"        "roi,r"))]
  "(register_operand (operands[0], TImode)
    /* Prevent rematerialization of constants.  */
    && ! CONSTANT_P (operands[1]))
   || reg_or_0_operand (operands[1], TImode)"
  "#"
  "reload_completed"
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 1) (match_dup 3))]
{
  sx_split_tmode_pair (operands, TImode, true);
})

(define_expand "movti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "")
        (match_operand:TI 1 "general_operand" ""))]
  ""
{
  if (GET_CODE (operands[0]) == MEM
      && ! reg_or_0_operand (operands[1], TImode))
    operands[1] = force_reg (TImode, operands[1]);

  if (operands[1] == const0_rtx)
    ;
  /* We must put 64-bit constants in memory.  We could keep the
     32-bit constants in TImode and rely on the splitter, but
     this doesn't seem to be worth the pain.  */
  else if (GET_CODE (operands[1]) == CONST_INT
           || GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      rtx in[2], out[2], target;

      gcc_assert (!no_new_pseudos);

      split_double (operands[1], &in[0], &in[1]);

      if (in[0] == const0_rtx)
        out[0] = const0_rtx;
      else
        {
          out[0] = gen_reg_rtx (DImode);
          emit_insn (gen_movdi (out[0], in[0]));
        }

      if (in[1] == const0_rtx)
        out[1] = const0_rtx;
      else
        {
          out[1] = gen_reg_rtx (DImode);
          emit_insn (gen_movdi (out[1], in[1]));
        }

      if (GET_CODE (operands[0]) != REG)
        target = gen_reg_rtx (TImode);
      else
        target = operands[0];

      emit_insn (gen_movdi (gen_rtx_SUBREG (DImode, target, 0), out[0]));
      emit_insn (gen_movdi (gen_rtx_SUBREG (DImode, target, 8), out[1]));

      if (target != operands[0])
        emit_insn (gen_rtx_SET (VOIDmode, operands[0], target));

      DONE;
    }
})

;;
;; Load/Store multiple
;;

(define_expand "load_multiple"
  [(match_par_dup 3 [(set (match_operand:SI 0 "" "")
                          (match_operand:SI 1 "" ""))
                     (use (match_operand:SI 2 "" ""))])]
  ""
{
  int regno, count, i;

  /* Support only loading a constant number of registers from memory and
     only if at least two registers. */
  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) < 2
      || GET_CODE (operands[1]) != MEM
      || GET_CODE (operands[0]) != REG
      || REGNO (operands[0]) + INTVAL (operands[2]) > 128)
    FAIL;

  count = INTVAL (operands[2]);
  regno = REGNO (operands[0]);

  operands[3] = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));

  for (i = 0; i < count; i++)
    XVECEXP (operands[3], 0, i)
      = gen_rtx_SET (VOIDmode,
		 gen_rtx_REG (DImode, regno + i),
		 gen_rtx_MEM (DImode, plus_constant (XEXP(operands[1], 0), i * 8)));
})

(define_insn "*load_multiple"
  [(match_parallel 0 "load_multiple_operation"
		   [(set (match_operand:DI 1 "register_operand" "=r")
                 (mem:DI (match_operand:DI 2 "memory_operand" "m")))])]
  ""
{
  int nregs = XVECLEN (operands[0], 0);
  operands[0] = gen_rtx_raw_CONST_INT (DImode, nregs);
  return "ldm\t%1,%0,%2";
}
  [(set_attr "type" "load")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

(define_expand "store_multiple"
  [(match_par_dup 3 [(set (match_operand:SI 0 "" "")
                          (match_operand:SI 1 "" ""))
                     (use (match_operand:SI 2 "" ""))])]
  ""
{
  int regno, count, i;

  /* Support only storing a constant number of registers to memory and
     only if at least two registers. */
  if (GET_CODE(operands[2]) != CONST_INT
      || INTVAL(operands[2]) < 2
      || GET_CODE(operands[0]) != MEM
      || GET_CODE(operands[1]) != REG
      || REGNO(operands[1]) + INTVAL(operands[2]) > 128)
    FAIL;

  count = INTVAL(operands[2]);
  regno = REGNO(operands[1]);

  operands[3] = gen_rtx_PARALLEL(VOIDmode, rtvec_alloc (count));

  for (i = 0; i < count; i++)
    XVECEXP(operands[3], 0, i)
      = gen_rtx_SET(VOIDmode,
		 gen_rtx_MEM(DImode, plus_constant(XEXP(operands[0], 0), i * 8)),
		 gen_rtx_REG(DImode, regno + i));
})

(define_insn "*store_multiple"
  [(match_parallel 0 "store_multiple_operation"
                   [(set (match_operand:DI 2 "memory_operand" "=m")
                         (match_operand:DI 1 "register_operand" "r"))])]
  ""
{
  int nregs = XVECLEN (operands[0], 0);
  operands[0] = gen_rtx_raw_CONST_INT (DImode, nregs);
  return "stm\t%1,%0,%2";
}
  [(set_attr "type" "store")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

;;
;; loadhigh
;;
(define_expand "loadhigh"
  [(set (match_operand:DI 0 "register_operand" "")
        (unspec:DI [(match_operand:DI 1 "IN_operand" "")]
		   UNSPEC_LOAD_DI_HIGH))]
  ""
  "")

(define_insn "*loadhigh"
  [(set (match_operand:DI 0 "reg_or_mem_operand"     "=r")
        (unspec:DI [(match_operand:DI 1 "IN_operand" "i")]
		   UNSPEC_LOAD_DI_HIGH))]
  ""
  "ldu\\t%0,%1(0,0)"
  [(set_attr "type" "load")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

;;
;; monc
;;
(define_insn "monc"
  [(unspec [(match_operand:DI 0 "IN_operand" "i")
            (match_operand:DI 1 "IN_operand" "i")
            (match_operand:DI 2 "IN_operand" "i")]
           UNSPEC_MONC)]
  ""
  "monc\\t%0,%1,%2"
  [(set_attr "type" "unknown")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

;;
;; No-Op
;;

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "none")
   (set_attr "length"   "1")])

;;
;; unconditional branches and such.
;;

;; ;; may be removed
;; (define_insn "indirect_jump_si"
;;   [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
;;   ""
;;   "be>\\t0,(,%0)"
;;   [(set_attr "type" "jump")
;;    (set_attr "mode" "none")])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:DI 0 "register_operand" "r"))]
  ""
  "be>\\t0,(,%0)"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")
   (set_attr "length"    "1")])

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
{
  if (GET_CODE (operands[0]) == REG)
    gcc_unreachable(); /* return "be>\t0,(,%0)"; */
  else
    return "be>\t0,%0";
}
  [(set_attr "type"     "jump")
   (set_attr "mode"     "none")
   (set_attr "length"    "1")])

(define_insn "function_return"
  [(parallel [(return)
              (use (reg:DI 32))])]
  ""
  "be>\t0,(,$s32)"
  [(set_attr "type"     "jump")
   (set_attr "mode"     "none")
   (set_attr "length"    "1")])


;;
;; tablejump is used for building switch-case jump tables
;;
(define_insn "tablejump"
  [(set (pc) (match_operand:DI 0 "address_operand" "p"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "be>\t0,(,%0)"
  [(set_attr "type"   "jump")
   (set_attr "mode"   "none")
   (set_attr "length" "1")])

;;
;; calls
;;

; operand 0 : register in which the call result is returned
; operand 1 : function to call
; operand 2 : amount of stack taken for arguments (?more precise?)
(define_expand "call_value"
    [(parallel [(set (match_operand 0 "register_operand" "=r")
                     (call (match_operand 1 "sym_ref_mem_operand" "")
                           (match_operand 2 "const_int_operand" "i")))
                (clobber (reg:DI 32))
                (clobber (reg:DI 33))
                (clobber (reg:DI 34))
                (clobber (match_scratch:DI 3 "=&r")) ])]
  "" 
{
  operands[1] = gen_rtx_MEM(GET_MODE(operands[1]),
                            force_reg (Pmode, XEXP (operands[1], 0)));
})

(define_expand "call"
  [(parallel [(call (match_operand 0 "sym_ref_mem_operand" "")
                    (match_operand 1 "const_int_operand" "i"))
              (clobber (reg:DI 32))
              (clobber (reg:DI 33))
              (clobber (reg:DI 34))
              (clobber (match_scratch:DI 2 "=&r")) ])]
  ""
{
  operands[0] = gen_rtx_MEM(GET_MODE (operands[0]),
                            force_reg (Pmode, XEXP (operands[0], 0)));
})

(define_insn "call_value_indirect"
  [(parallel [(set (match_operand 0 "register_operand" "=r")
                   (call (mem:QI (match_operand 1 "register_operand" "r"))
                         (match_operand 2 "const_int_operand" "i")))
              (clobber (reg:DI 32))
              (clobber (reg:DI 33))
              (clobber (reg:DI 34))
              (clobber (match_scratch:DI 3 "=&r")) ])]
  ""
{
  return sx_asm_output_call(&operands[1], 0);
}
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

(define_insn "call_tfvalue_indirect"
  [(parallel [(set (match_parallel 0 ""
                                   [(match_operand:DI 1 "" "")
                                    (match_operand:DI 2 "" "")])
                   (call (mem:QI (match_operand 3 "register_operand" "r"))
                         (match_operand 4 "const_int_operand" "i")))
              (clobber (reg:DI 32))
              (clobber (reg:DI 33))
              (clobber (reg:DI 34))
              (clobber (match_scratch:DI 5 "=&r")) ])]
  ""
{
  return sx_asm_output_call(&operands[3], 0);
}
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

(define_insn "call_indirect"
  [(parallel [(call (mem:QI (match_operand 0 "register_operand" "r,r"))
                    (match_operand 1 "IN_operand" "I,N"))
              (clobber (reg:DI 32))
              (clobber (reg:DI 33))
              (clobber (reg:DI 34))
              (clobber (match_scratch:DI 2 "=&r,&r")) ])]
  ""
{
    return sx_asm_output_call(&operands[0], 0);
}
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

;; Call subroutine returning any type.
(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "")
                    (const_int 0))
              (match_operand 1 "")
              (match_operand 2 "")])]
  ""
{
  int i;

  emit_call_insn (GEN_CALL (operands[0], const0_rtx, NULL, const0_rtx));

  for (i = 0;
       i < XVECLEN(operands[2], 0);
       i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  emit_insn (gen_blockage ());
  DONE;
})

;; FIXME: tail call optimization does not work right now

;; sibcalls don't clobber 32, they just use it, as it already contains
;; the return address of the caller of the function making a sibcall; apart
;; from that, at RTL level a sibcall is just like any other call.
(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "register_operand" "=r")
                   (call (match_operand 1 "sym_ref_mem_operand" "")
                         (match_operand 2 "const_int_operand" "i")))
              (use (reg:DI 32))
              (clobber (reg:DI 33))
              (clobber (reg:DI 34))
              (clobber (match_scratch:DI 3 "=&r")) ])]
  ""
{
  operands[1] = gen_rtx_MEM(GET_MODE(operands[1]),
                            force_reg (Pmode, XEXP (operands[1], 0)));
})

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "sym_ref_mem_operand" "")
                    (match_operand 1 "const_int_operand" "i"))
              (use (reg:DI 32))
              (clobber (reg:DI 33))
              (clobber (reg:DI 34))
              (clobber (match_scratch:DI 2 "=&r")) ])]
  ""
{
  operands[0] = gen_rtx_MEM(GET_MODE (operands[0]),
                            force_reg (Pmode, XEXP (operands[0], 0)));
})

(define_insn "sibcall_value_indirect"
  [(parallel [(set (match_operand 0 "register_operand" "=r")
                   (call (mem:QI (match_operand 1 "register_operand" "r"))
                         (match_operand 2 "const_int_operand" "i")))
              (use (reg:DI 32))
              (clobber (reg:DI 33))
              (clobber (reg:DI 34))
              (clobber (match_scratch:DI 3 "=&r")) ])]
  ""
{
  return sx_asm_output_call(&operands[1], 1);
}
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

(define_insn "sibcall_tfvalue_indirect"
  [(parallel [(set (match_parallel 0 ""
                                   [(match_operand:DI 1 "" "")
                                    (match_operand:DI 2 "" "")])
                   (call (mem:QI (match_operand 3 "register_operand" "r"))
                         (match_operand 4 "const_int_operand" "i")))
              (use (reg:DI 32))
              (clobber (reg:DI 33))
              (clobber (reg:DI 34))
              (clobber (match_scratch:DI 5 "=&r")) ])]
  ""
{
  return sx_asm_output_call(&operands[3], 1);
}
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

(define_insn "sibcall_indirect"
  [(parallel [(call (mem:QI (match_operand 0 "register_operand" "r,r"))
                    (match_operand 1 "IN_operand" "I,N"))
              (use (reg:DI 32))
              (clobber (reg:DI 33))
              (clobber (reg:DI 34))
              (clobber (match_scratch:DI 2 "=&r,&r")) ])]
  ""
{
    return sx_asm_output_call(&operands[0], 1);
}
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

;;;
;;; RTL for this is generated by the define_expand for call_value.
;;;
;
;;;
;;; calls that return int in r123
;;;
;
;(define_insn "call_val_internal_return_s123_doubleword"
;[(parallel [(set (reg:DI 123)
;                  (call (match_operand 0 "register_operand" "r")
;                        (match_operand 1 "" "i")))
;	    (clobber (reg:DI 32))])]
;  ""
;  "bsic\\t$s32,%0"
;  [(set_attr "type" "jump")
;   (set_attr "mode" "none")])
;
;(define_insn "call_val_internal_return_s123"
;[(parallel [(set (reg:SI 123)
;                  (call (match_operand 0 "register_operand" "r")
;                        (match_operand 1 "" "i")))
;	    (clobber (reg:DI 32))])]
;  ""
;  "bsic\\t$s32,%0"
;  [(set_attr "type" "jump")
;   (set_attr "mode" "none")])
;
;(define_insn "call_val_internal_return_s123_halfword"
;[(parallel [(set (reg:HI 123)
;                  (call (match_operand 0 "register_operand" "r")
;                        (match_operand 1 "" "i")))
;	    (clobber (reg:DI 32))])]
;  ""
;  "bsic\\t$s32,%0"
;  [(set_attr "type" "jump")
;   (set_attr "mode" "none")])
;
;(define_insn "call_val_internal_return_s123_quarterword"
;[(parallel [(set (reg:QI 123)
;                  (call (match_operand 0 "register_operand" "r")
;                        (match_operand 1 "" "i")))
;	    (clobber (reg:DI 32))])]
;  ""
;  "bsic\\t$s32,%0"
;  [(set_attr "type" "jump")
;   (set_attr "mode" "none")])
;;;
;;; calls that return float in s124
;;;
;
;(define_insn "call_val_internal_return_s124_float"
;[(parallel [(set (reg:SF 124)
;                  (call (match_operand 0 "register_operand" "r")
;                        (match_operand 1 "" "i")))
;	    (clobber (reg:DI 32))])]
;  ""
;  "bsic\\t$s32,%0"
;  [(set_attr "type" "jump")
;   (set_attr "mode" "none")])
;
;;;
;;; calls that return double in s124
;;;
;
;(define_insn "call_val_internal_return_s124_double"
;[(parallel [(set (reg:DF 124)
;                   (call (match_operand 0 "register_operand" "r")
;                         (match_operand 1 "" "i")))
;	    (clobber (reg:DI 32))])]
;  ""
;  "bsic\\t$s32,%0"
;  [(set_attr "type" "jump")
;   (set_attr "mode" "none")])
;
;;;
;;; calls that do not return a value.
;;;
;;; clobber reg:SI 32
;;(define_insn "call_val_internal_no_return"
;;  [(call (match_operand 0 "register_operand" "r")
;;         (match_operand 1 "" "i"))]
;;  ""
;;  "bsic\\t$s32,%S0"
;;  [(set_attr "type" "jump")
;;   (set_attr "mode" "none")])

(define_expand "movccfp"
  [(set (reg:SF 0)
        (match_operand 0 "const_int_operand" ""))]
  ""
{
  if ((unsigned HOST_WIDE_INT) INTVAL(operands[0]) > 1)
    FAIL;
})

(define_insn "*movccfp_imm"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (match_operand     1 "I_operand"        "I" ))]
  ""
  "flt@\\t%0,%G1"
  [(set_attr "length" "1")
   (set_attr "mode" "SF")])

(define_expand "movccdfp"
  [(set (reg:DF 0)
        (match_operand 0 "const_int_operand" ""))]
  ""
{
  if ((unsigned HOST_WIDE_INT) INTVAL(operands[0]) > 1)
    FAIL;
})

(define_insn "*movccdfp_imm"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (match_operand       1 "I_operand"        "I" ))]
  ""
  "fltx\\t%0,%G1"
  [(set_attr "length" "1")
   (set_attr "mode" "DF")])

;
; matches any comparison operator
;
(define_code_macro any_cond [eq ne gt ge lt le gtu geu ltu leu])

;;
;; Comparisons and branches.
;;

; EF: Would be nice to generate here the signed vs. unsigned compares
(define_expand "cmpsi"
  [(set (reg:SI 0)
        (compare:SI (match_operand:SI 0 "reg_or_I_operand" "")
                    (match_operand:SI 1 "reg_or_M_operand" "")))]
  ""
  "
{
  if (operands[0]) {
      sx_compare_op0 = operands[0];
      sx_compare_op1 = operands[1];
      sx_compare_mode = SImode;
      DONE;
  }
}")



(define_expand "cmpdi"
  [(set (reg:DI 0)
        (compare:DI (match_operand:DI 0 "reg_or_I_operand" "")
                    (match_operand:DI 1 "reg_or_M_operand" "")))]
  ""
  "
{
  if (operands[0]) {
      sx_compare_op0 = operands[0];
      sx_compare_op1 = operands[1];
      sx_compare_mode = DImode;
      DONE;
  }
}")

(define_expand "cmpdf"
  [(set (reg:DF 0)
        (compare:DF (match_operand:DF 0 "register_operand" "r")
		    (match_operand:DF 1 "register_operand" "r")))]
  ""
  "
{
  if (operands[0]) {
      sx_compare_op0 = operands[0];
      sx_compare_op1 = operands[1];
      sx_compare_mode = DFmode;
      DONE;
  }
}")

(define_expand "cmpsf"
  [(set (reg:SF 0)
        (compare:SF (match_operand:SF 0 "register_operand" "r")
		    (match_operand:SF 1 "register_operand" "r")))]
  ""
  "
{
  if (operands[0]) {
      sx_compare_op0 = operands[0];
      sx_compare_op1 = operands[1];
      sx_compare_mode = SFmode;
      DONE;
  }
}")

;;
;; RTL for compare integer instructions.
;; The RTL for these is generated in sx_conditional_branch().
;; unsigned int compare uses 64bit compare and 64bit branch(tricky)
;;
;; EF: Integer compares are machine specific, such that we can differentiate
;; between signed and unsigned compares.
;;

;;
;; signed SI compare
;;
(define_insn "comp_si"
  [(set (match_operand:CCSSI 0 "register_operand" "=r,r,r")
        (compare:CCSSI (match_operand:SI 1 "reg_or_I_operand" "r,I,r")
                       (match_operand:SI 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   cps\\t%0,%1,%2\\t# comp_SI, =r,r,r
   cps\\t%0,%G1,%2\\t# comp_SI, =r,I,r
   cps\\t%0,%1,%M2\\t# comp_SI, =r,r,M"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "1,1,1")])

;;
;; signed DI compare
;;
(define_insn "comp_di"
  [(set (match_operand:CCSDI 0 "register_operand" "=r,r,r")
        (compare:CCSDI (match_operand:DI 1 "reg_or_I_operand" "r,I,r")
                       (match_operand:DI 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   cpx\\t%0,%1,%2\\t# comp_DI, =r,r,r
   cpx\\t%0,%G1,%2\\t# comp_DI, =r,I,r
   cpx\\t%0,%1,%M2\\t# comp_DI, =r,r,M"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1,1,1")])


;;
;; unsigned DI compare
;;
(define_insn "comp_udi"
  [(set (match_operand:CCUDI 0 "register_operand" "=r,r,r")
        (compare:CCUDI (match_operand:DI 1 "reg_or_I_operand" "r,I,r")
                       (match_operand:DI 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   cmp\\t%0,%1,%2\\t# comp_UDI, =r,r,r
   cmp\\t%0,%G1,%2\\t# comp_UDI, =r,I,r
   cmp\\t%0,%1,%M2\\t# comp_UDI, =r,r,M"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1,1,1")])


;;
;; Float compares.
;; The RTL for these is also generated in
;; sx_conditional_branch().
;;

(define_insn "comp_sf"
  [(set (match_operand:CCFSF 0 "register_operand" "=r,r,r")
        (compare:CCFSF (match_operand:SF 1 "reg_or_I_operand" "r,I,r")
                       (match_operand:SF 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   fcp@\\t%0,%1,%2
   fcp@\\t%0,%G1,%2
   fcp@\\t%0,%1,%M2"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "1,1,1")])

(define_insn "comp_df"
  [(set (match_operand:CCFDF 0 "register_operand" "=r,r,r")
        (compare:CCFDF (match_operand:DF 1 "reg_or_I_operand" "r,I,r")
                       (match_operand:DF 2 "reg_or_M_operand" "r,r,M")))]
  ""
  "@
   fcp\\t%0,%1,%2
   fcp\\t%0,%G1,%2
   fcp\\t%0,%1,%M2"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "1,1,1")])


;;
;; Branches.
;;

(define_expand "b<code>"
  [(set (pc)
        (if_then_else (any_cond (match_dup 1) (match_operand 2))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  sx_conditional_branch (operands, <CODE>);
  DONE;
})

;; Used to implement built-in functions.
(define_expand "condjump"
  [(parallel [(set (pc)
                   (if_then_else (match_operand 0)
                                 (label_ref (match_operand 1))
                                 (pc)))
              (use (reg:DI 4))])])

;; Used to implement built-in functions.
(define_expand "inv_condjump"
  [(parallel [(set (pc)
                   (if_then_else (match_operand 0)
                                 (pc)
                                 (label_ref (match_operand 1))))
              (use (reg:DI 4))])])

;;
;; Integer conditional branches; the RTL for these is 
;; generated in sx_conditional_branch().
;;

(define_insn "cond_branch_si"
  [(parallel
    [(set (pc)
          (if_then_else
           (match_operator 0 "comparison_operator"
                           [ (match_operand:CCSSI 1 "register_operand" "r")
                             (const_int 0)])
           (label_ref (match_operand 2 "" ""))
           (pc)))
     (use (reg:DI 4))])]
  ""
  "b%C0s\\t%1,%2"
  [(set_attr "type" "branch")
   (set_attr "mode" "SI")
   (set_attr "length" "1")])

(define_insn "cond_branch_di"
  [(parallel
    [(set (pc)
          (if_then_else
           (match_operator 0 "comparison_operator"
                           [ (match_operand:CCSDI 1 "register_operand" "r")
                             (const_int 0)])
           (label_ref (match_operand 2 "" ""))
           (pc)))
     (use (reg:DI 4))])]
  ""
  "b%C0\\t%1,%2"
  [(set_attr "type" "branch")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

(define_insn "cond_branch_udi"
  [(parallel
    [(set (pc)
          (if_then_else
           (match_operator 0 "comparison_operator"
                           [ (match_operand:CCUDI 1 "register_operand" "r")
                             (const_int 0)])
           (label_ref (match_operand 2 "" ""))
           (pc)))
     (use (reg:DI 4))])]
  ""
  "b%C0\\t%1,%2"
  [(set_attr "type" "branch")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

(define_insn "inv_cond_branch_si"
  [(parallel
    [(set (pc)
          (if_then_else
           (match_operator 0 "comparison_operator"
                           [ (match_operand:CCSSI 1 "register_operand" "r")
                             (const_int 0)])
           (pc)
           (label_ref (match_operand 2 "" ""))))
     (use (reg:DI 4))])]
  ""
  "b%N0s\\t%1,%2"
  [(set_attr "type" "branch")
   (set_attr "mode" "SI")
   (set_attr "length" "1")])

(define_insn "inv_cond_branch_di"
  [(parallel 
    [(set (pc)
          (if_then_else
           (match_operator 0 "comparison_operator"
                           [ (match_operand:CCSDI 1 "register_operand" "r")
                             (const_int 0)])
           (pc)
           (label_ref (match_operand 2 "" ""))))
     (use (reg:DI 4))])]
  ""
  "b%C0\\t%1,%2"
  [(set_attr "type" "branch")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

(define_insn "inv_cond_branch_udi"
  [(parallel 
    [(set (pc)
          (if_then_else
           (match_operator 0 "comparison_operator"
                           [ (match_operand:CCUDI 1 "register_operand" "r")
                             (const_int 0)])
           (pc)
           (label_ref (match_operand 2 "" ""))))
     (use (reg:DI 4))])]
  ""
  "b%C0\\t%1,%2"
  [(set_attr "type" "branch")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

;;
;; Floating point conditional branches,
;; also generated in sx_conditional_branch().

(define_insn "fp_cond_branch_sf"
  [(parallel
    [(set (pc)
          (if_then_else
           (match_operator 0 "comparison_operator"
                           [ (match_operand:CCFSF 1 "register_operand" "r")
                             (match_operand:SF 3)])
           (label_ref (match_operand 2 "" ""))
           (pc)))
     (use (reg:DI 4))])]
  ""
  {
    /* the NE condition should always succeed if one of the operands is NaN. */
    if (GET_CODE(operands[0]) == NE) 
      return "b%C0fn@\t%1,%2";
    else
      return "b%C0f@\t%1,%2";
  }
  [(set_attr "type" "branch")
   (set_attr "mode" "SF")
   (set_attr "length" "1")])

(define_insn "fp_cond_branch_df"
  [(parallel
    [(set (pc)
          (if_then_else
           (match_operator 0 "comparison_operator"
                           [ (match_operand:CCFDF 1 "register_operand" "r")
                             (match_operand:DF 3)])
           (label_ref (match_operand 2 "" ""))
           (pc)))
     (use (reg:DI 4))])]
  ""
  {
    /* the NE condition should always succeed if one of the operands is NaN. */
    if (GET_CODE(operands[0]) == NE) 
      return "b%C0fn\t%1,%2";
    else
      return "b%C0f\t%1,%2";
  }
  [(set_attr "type" "branch")
   (set_attr "mode" "DF")
   (set_attr "length" "1")])

;;
;; We don't use (cc0), therefore we should not define tst<mode> insns!
;; 

;;
;; The default behavior is to only save FP and SP. Adding the base register
;; $s4 to the saved stuff. Maybe we'll need to restore $s3 (arg pointer), too.
;; We could actually restore this also in nonlocal_goto, but this place is more
;; comfortable.
;;
(define_expand "save_stack_nonlocal"
  [(match_operand 0 "memory_operand" "")
   (match_operand 1 "register_operand" "")]
  ""
{
  enum machine_mode mode = BLKmode;
  rtx base_reg_rtx = gen_rtx_REG (Pmode, 4);

  emit_move_insn(operand_subword(operands[0], 1, 0, mode), base_reg_rtx);
  emit_move_insn(operand_subword(operands[0], 0, 0, mode), operands[1]);
  DONE;
})

;;
;; restore the base register $s4 in addition to the stack pointer from the
;; static chain.
;;
(define_expand "restore_stack_nonlocal"
  [(match_operand 0 "register_operand" "")
   (match_operand 1 "memory_operand" "")]
  ""
{
  enum machine_mode mode = BLKmode;
  rtx base_reg_rtx = gen_rtx_REG (Pmode, 4);

  emit_move_insn(base_reg_rtx, operand_subword(operands[1], 1, 0, mode));
  emit_move_insn(operands[0], operand_subword(operands[1], 0, 0, mode));

  emit_insn(gen_rtx_USE (VOIDmode, base_reg_rtx));
  DONE;
})



;;; ::::::::::::::::::::
;;; ::
;;; :: Misc insns
;;; ::
;;; ::::::::::::::::::::

;; Block any insns from being moved before this point, since the
;; profiling call to mcount can use various registers that aren't
;; saved or used to pass arguments.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])

(define_insn "flush_icache"
  [(unspec_volatile [(const_int 0)] UNSPECV_ICACHEFLUSH)]
  ""
  "rcr\t2\t# flush i-cache"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])

(define_insn "flush_dcache"
  [(unspec_volatile [(const_int 0)] UNSPECV_DCACHEFLUSH)]
  ""
  "\trcr\t1\t# flush d-cache"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])

;;
;; store process mask
;; argument is the register where to store the mask part of the PSW
;;
(define_insn "store_process_mask"
  [(unspec [(match_operand 0 "register_operand" "r")] UNSPEC_STORE_PROCMASK)]
  ""
  "spm\t%0\t# store process mask"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])

;;
;; load process mask
;; load mask part of PSW from the passed register
;;
(define_insn "load_process_mask"
  [(unspec [(match_operand 0 "register_operand" "r")] UNSPEC_LOAD_PROCMASK)]
  ""
  "lpm\t%0\t# load process mask"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])


;;; ::::::::::::::::::::
;;; ::
;;; :: Prologue and Epilogue instructions
;;; ::
;;; ::::::::::::::::::::

(define_expand "prologue"
  [(const_int 1)]
  ""
  {
    sx_expand_prologue ();
    DONE;
  })

(define_expand "epilogue"
  [(return)]
  ""
  {
    sx_expand_epilogue (0);
    DONE;
  })

(define_expand "sibcall_epilogue"
  [(return)]
  ""
  {
    sx_expand_epilogue (1);
    DONE;
  })


;;Emacs setting
;;- Local variables:
;;- mode:emacs-lisp
;;- comment-start: "; "
;;- End:
