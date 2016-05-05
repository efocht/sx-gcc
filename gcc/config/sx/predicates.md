;; Predicate definitions for SX
;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; $Id: predicates.md 215 2009-01-30 12:20:17Z jaka $
;;


;; Return 1 if OP is an I constant or any register.
(define_predicate "reg_or_I_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) < 64 && INTVAL (op) >= -64")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an I constant.
(define_predicate "I_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) < 64 && INTVAL (op) >= -64")))

;; Return 1 if OP is a floating point I constant.
(define_predicate "fp_I_operand"
  (match_code "const_double")
{
  REAL_VALUE_TYPE rv, ri;
  HOST_WIDE_INT i;
  HOST_WIDE_INT hi;
  unsigned HOST_WIDE_INT lo;

  REAL_VALUE_FROM_CONST_DOUBLE(rv, op);
  i = real_to_integer(&rv);
  real_to_integer2((HOST_WIDE_INT *)&lo, &hi, &rv);
  real_from_integer(&ri, mode, lo, hi, 0);

  return ((i <= 63) && 
	  (i >= -64) &&
	  real_identical(&rv, &ri));
}
)

;; Return 1 if OP is a floating point constant suitable
;; for loading via lea: any 32-bit float satisfies this.
(define_predicate "fp_lea_I_operand"
  (match_code "const_double")
{
  return (mode == SFmode);
}
)

;; Return 1 if OP is an J constant or any register.
(define_predicate "reg_or_J_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) < 32 && INTVAL (op) >= 0")
    (match_operand 0 "register_operand")))


;; Return 1 if OP is an K constant or any register.
(define_predicate "reg_or_K_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) < 64 && INTVAL (op) >= 0")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an L constant or any register.
(define_predicate "reg_or_L_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) < 128 && INTVAL (op) >= 0")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an M constant or any register.
(define_predicate "reg_or_M_operand"
  (if_then_else (match_code "const_int")
    (match_test " ((INTVAL(op) & (INTVAL(op)+1))==0)||((~INTVAL(op) & (~INTVAL(op)+1))==0) ")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an M constant.
(define_predicate "M_operand"
  (and (match_code "const_int")
       (match_test " ((INTVAL(op) & (INTVAL(op)+1))==0)||((~INTVAL(op) & (~INTVAL(op)+1))==0) ")))

;; Return 1 if OP is an N constant.
(define_predicate "IN_operand"
  (and (match_code "const_int")
    (match_test "INTVAL (op) <= 2147483647LL && INTVAL(op) >= -2147483648LL")))

;; Return 1 if OP is an I or M constant.
(define_predicate "IM_operand"
  (ior (match_operand 0 "I_operand")
       (match_operand 0 "M_operand")))

;; Return 1 if OP is the zero constant for MODE.
(define_predicate "const0_operand"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (mode)")))

;; Return 1 if OP is an N constant or any register.
(define_predicate "reg_or_IN_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) <= 2147483647LL && INTVAL(op) >= -2147483648LL")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an I, M constant or any register.
(define_predicate "reg_or_IM_operand"
  (ior (match_operand 0 "IM_operand")
       (match_operand 0 "register_operand")))

;; Returns true if OP is either the constant zero or a register.
(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const0_operand")))

(define_predicate "reg_or_mem_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "memory_operand")))

(define_predicate "const_double_zero_operand"
  (match_code "const_double")
{
  if (op == CONST0_RTX(mode))
    return 1;
  return 0;
})

;; Returns 1 if OP is a symbolic operand, i.e. a symbol_ref or a label_ref,
;; possibly with an offset.
(define_predicate "symbolic_operand"
  (ior (match_code "symbol_ref,label_ref")
       (and (match_code "const")
	    (match_test "GET_CODE (XEXP (op,0)) == PLUS
			 && GET_CODE (XEXP (XEXP (op,0), 0)) == SYMBOL_REF
			 && GET_CODE (XEXP (XEXP (op,0), 1)) == CONST_INT"))))

(define_predicate "sym_ref_mem_operand"
 	(match_code "mem")
{
  rtx t1 = XEXP(op, 0);
  if(GET_CODE(t1) == SYMBOL_REF)
    return 1;
  return 0;
})

;; returns 1 if op is a symbol ref operand
(define_predicate "symref_operand"
  (match_code "symbol_ref"))

;; Register or symbolic memory reference operand
(define_predicate "reg_or_sym_ref_mem_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "sym_ref_mem_operand")))

;; Bad mem-mem operand on which movdi should fail
(define_predicate "mem_mem_operand"
  (match_code "mem")
{
  rtx t1 = XEXP(op, 0);
  if(GET_CODE(t1) == MEM)
    return 1;
  return 0;
})

;; Movedi target operand
(define_predicate "movdi_target_operand"
  (ior (match_operand 0 "register_operand")
       (ior (match_operand 0 "IN_operand")
	    (ior (match_operand 0 "M_operand")
		 (ior (match_operand 0 "sym_ref_mem_operand")
		      (match_operand 0 "memory_operand")
		      )
		 )
	    )
       )
)


;; Return 1 if OP is a valid operand for the source of a move insn.
(define_predicate "input_operand"
  (match_code "label_ref,symbol_ref,const,high,reg,subreg,mem,
	       const_double,const_vector,const_int")
{
  switch (GET_CODE (op))
    {
    case LABEL_REF:
    case CONST:
      return mode == ptr_mode || mode == DImode;

    case SYMBOL_REF:
    {
      bool res;
      res = (CONSTANT_POOL_ADDRESS_P(op) && 
	      (mode == ptr_mode || mode == DImode));
      /*
      fprintf(stderr, "input_operand for symbol queried: ");
      print_inline_rtx(stderr, op, 1); fprintf(stderr, " (%d)\n", res);
      */
      return (CONSTANT_POOL_ADDRESS_P(op) && 
	      (mode == ptr_mode || mode == DImode)) ;
    }


    case REG:
      return true;

    case SUBREG:
      if (register_operand (op, mode))
	return true;
      /* ... fall through ...  */

    case MEM:
      return ((mode != HImode && mode != QImode && mode != SImode)
	      && GET_CODE(XEXP(op, 0)) != MEM
	      && general_operand (op, mode));

    case CONST_DOUBLE:
      return op == CONST0_RTX (mode);

    case CONST_VECTOR:
      if (reload_in_progress || reload_completed)
	return sx_legitimate_constant_p (op);
      return op == CONST0_RTX (mode);

    case CONST_INT:
      if (mode == QImode || mode == HImode || mode == SImode)
	return true;
      if (mode == DImode)
        return IN_operand(op, mode) || M_operand(op, mode);
      if (reload_in_progress || reload_completed)
	return sx_legitimate_constant_p (op);
      return (reg_or_IN_operand (op, mode) || M_operand(op, mode));

    default:
      gcc_unreachable ();
    }
  return false;
})


;; Return 1 if OP is a valid SX comparison operator for "cmp" style
;; instructions.
(define_predicate "signed_comparison_operator"
  (match_code "eq,ne,lt,le,ge,gt"))

(define_predicate "unsigned_comparison_operator"
  (match_code "ltu,leu,geu,gtu"))

(define_predicate "const_arith_operand"
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (INTVAL (op))")))

(define_predicate "load_multiple_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  int dest_regno;
  rtx src_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return false;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      HOST_WIDE_INT off = -(SX_RSA_SIZE - i*8);

      if (GET_CODE (elt) != SET
	      || GET_CODE (SET_DEST (elt)) != REG
	      || GET_MODE (SET_DEST (elt)) != DImode
          || REGNO (SET_DEST (elt))    != (unsigned) (dest_regno + i)
          || GET_CODE (SET_SRC (elt))  != MEM
          || GET_MODE (SET_SRC (elt))  != DImode
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
          || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
          || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
          || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != off)
        return false;
    }

  return true;
})

;; Similar, but tests for store multiple.

(define_predicate "store_multiple_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  int src_regno;
  rtx dest_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return false;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      HOST_WIDE_INT off = -(SX_RSA_SIZE - i*8);

      if (GET_CODE (elt) != SET
          || GET_CODE (SET_SRC (elt)) != REG
          || GET_MODE (SET_SRC (elt)) != DImode
          || REGNO (SET_SRC (elt)) != (unsigned) (src_regno + i)
          || GET_CODE (SET_DEST (elt)) != MEM
          || GET_MODE (SET_DEST (elt)) != DImode
          || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
          || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
          || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
          || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != off)
        return false;
    }

  return true;
})


;;Emacs setting
;;- Local variables:
;;- mode:emacs-lisp
;;- comment-start: "; "
;;- End:
