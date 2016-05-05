/* 
   Subroutines used for SX code generation.
   Copyright (C) 1989, 1990, 1991, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2007 Free Software Foundation, Inc.
   Contributed by A. Lichnewsky, lich@inria.inria.fr.
   Changes by Michael Meissner, meissner@osf.org.
   64 bit r4000 support by Ian Lance Taylor, ian@cygnus.com, and
   Brendan Eich, brendan@microunity.com.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  


   Base code donated (2008).
   Some parts: Copyright (c) 1999- Takayoshi Kochi (gcc-2.95 version)

   Copyright (c) 2008 Erich Focht <efocht@hpce.nec.com>
   Copyright (c) 2008 Jaka Mocnik <jaka@xlab.si>
   Copyright (c) 2008 Marko Novak <marko.novak@xlab.si>
   Copyright (c) 2008 Fredrik Unger <funger@hpce.nec.com>
   
   $Id: sx.c 250 2009-02-26 13:44:23Z jaka $
*/


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "insn-attr.h"
#include "recog.h"
#include "toplev.h"
#include "output.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "flags.h"
#include "reload.h"
#include "tm_p.h"
#include "ggc.h"
#include "gstab.h"
#include "hashtab.h"
#include "debug.h"
#include "target.h"
#include "target-def.h"
#include "integrate.h"
#include "langhooks.h"
#include "cfglayout.h"
#include "sched-int.h"
#include "tree-gimple.h"
#include "bitmap.h"
#include <ctype.h>

#include "sx-protos.h"

/* #define SXDEBUG 1 */
#define SX_DEBUG 1

/* in most cases we need to print 2 expressions */
#ifdef SXDEBUG
#define DBG_RTX2(mark, label1, rtx1, label2, rtx2)		    \
  fprintf(stderr, "%s: [%s]\n  %s = ", __FUNCTION__, mark, label1); \
  print_inline_rtx(stderr, rtx1, 0);				    \
  fprintf(stderr, "\n  %s = ", label2);				    \
  print_inline_rtx(stderr, rtx2, 0);				    \
  fprintf(stderr, "\n");
#else
#define DBG_RTX2(mark, label1, rtx1, label2, rtx2) {}
#endif

/* True if X is an unspec wrapper around a SYMBOL_REF or LABEL_REF.  */
#define UNSPEC_ADDRESS_P(X)                                     \
  (GET_CODE (X) == UNSPEC                                       \
   && XINT (X, 1) >= UNSPEC_ADDRESS_FIRST                       \
   && XINT (X, 1) < UNSPEC_ADDRESS_FIRST + NUM_SYMBOL_TYPES)

/* Extract the symbol or label from UNSPEC wrapper X.  */
#define UNSPEC_ADDRESS(X) \
  XVECEXP (X, 0, 0)

/* Extract the symbol type from UNSPEC wrapper X.  */
#define UNSPEC_ADDRESS_TYPE(X) \
  ((enum sx_symbol_type) (XINT (X, 1) - UNSPEC_ADDRESS_FIRST))

#define REG_NEEDS_SAVE(i) (regs_ever_live[i] && !(call_used_regs[i]) && i != 1)
#define REG_ALWAYS_SAVE 8

enum sx_address_type {
  ADDRESS_REG,
  ADDRESS_LO_SUM,
  ADDRESS_CONST_INT,
  ADDRESS_SYMBOLIC
};

/* One stage in a constant building sequence.  These sequences have
   the form:

   A = VALUE[0]
   A = A CODE[1] VALUE[1]
   A = A CODE[2] VALUE[2]
   ...

   where A is an accumulator, each CODE[i] is a binary rtl operation
  and each VALUE[i] is a constant integer.  */

struct sx_integer_op {
  enum rtx_code code;
  unsigned HOST_WIDE_INT value;
};

/*
  The largest number of operations needed to load an integer constant.
*/

#define SX_MAX_INTEGER_OPS 7


/* Information about an address described by sx_address_type.

   ADDRESS_CONST_INT
       No fields are used.

   ADDRESS_REG
       REG is the base register and OFFSET is the constant offset.

   ADDRESS_LO_SUM
       REG is the register that contains the high part of the address,
       OFFSET is the symbolic address being referenced and SYMBOL_TYPE
       is the type of OFFSET's symbol.

   ADDRESS_SYMBOLIC
       SYMBOL_TYPE is the type of symbol being referenced.  */



/* Classifies a SYMBOL_REF, LABEL_REF or UNSPEC address.
   These are probably more appropriate for PIC and ELF than for COFF, but we
   keep them for now.

   SYMBOL_GENERAL
       Used when none of the below apply.

   SYMBOL_SMALL_DATA
       The symbol refers to something in a small data section.

   SYMBOL_CONSTANT_POOL
       The symbol refers to something in the mips16 constant pool.

   SYMBOL_GOT_LOCAL
       The symbol refers to local data that will be found using
       the global offset table.

   SYMBOL_GOT_GLOBAL
       Likewise non-local data.

   SYMBOL_GOTOFF_PAGE
       An UNSPEC wrapper around a SYMBOL_GOT_LOCAL.  It represents the
       offset from _gp of a GOT page entry.

   SYMBOL_GOTOFF_GLOBAL
       An UNSPEC wrapper around a SYMBOL_GOT_GLOBAL.  It represents the
       the offset from _gp of the symbol's GOT entry.

   SYMBOL_GOTOFF_CALL
       Like SYMBOL_GOTOFF_GLOBAL, but used when calling a global function.
       The GOT entry is allowed to point to a stub rather than to the
       function itself.

   SYMBOL_GOTOFF_LOADGP
       An UNSPEC wrapper around a function's address.  It represents the
       offset of _gp from the start of the function.

   SYMBOL_TLS
       A thread-local symbol.

   SYMBOL_TLSGD
   SYMBOL_TLSLDM
   SYMBOL_DTPREL
   SYMBOL_GOTTPREL
   SYMBOL_TPREL
       UNSPEC wrappers around SYMBOL_TLS, corresponding to the
       thread-local storage relocation operators.

   SYMBOL_64_HIGH
       For a 64-bit symbolic address X, this is the value of
       X & 0xffffffff00000000 >> 32

   SYMBOL_64_LOW
       For a 64-bit symbolic address X, this is the value of
       X & 0xffffffff
*/

enum sx_symbol_type {
  SYMBOL_GENERAL,
  SYMBOL_SMALL_DATA,
  SYMBOL_CONSTANT_POOL,
  SYMBOL_GOT_LOCAL,
  SYMBOL_GOT_GLOBAL,
  SYMBOL_GOTOFF_PAGE,
  SYMBOL_GOTOFF_GLOBAL,
  SYMBOL_GOTOFF_CALL,
  SYMBOL_GOTOFF_LOADGP,
  SYMBOL_TLS,
  SYMBOL_TLSGD,
  SYMBOL_TLSLDM,
  SYMBOL_DTPREL,
  SYMBOL_GOTTPREL,
  SYMBOL_TPREL,
  SYMBOL_64_HIGH,
  SYMBOL_64_LOW
};

static const char *sx_symbol_name[] = {
  "SYMBOL_GENERAL",
  "SYMBOL_SMALL_DATA",
  "SYMBOL_CONSTANT_POOL",
  "SYMBOL_GOT_LOCAL",
  "SYMBOL_GOT_GLOBAL",
  "SYMBOL_GOTOFF_PAGE",
  "SYMBOL_GOTOFF_GLOBAL",
  "SYMBOL_GOTOFF_CALL",
  "SYMBOL_GOTOFF_LOADGP",
  "SYMBOL_TLS",
  "SYMBOL_TLSGD",
  "SYMBOL_TLSLDM",
  "SYMBOL_DTPREL",
  "SYMBOL_GOTTPREL",	
  "SYMBOL_TPREL",
  "SYMBOL_64_HIGH",
  "SYMBOL_64_LOW"
};

#define NUM_SYMBOL_TYPES (SYMBOL_64_LOW + 1)

struct sx_address_info
{
  enum sx_address_type type;
  rtx reg;
  rtx offset;
  enum sx_symbol_type symbol_type;
};

static enum sx_symbol_type sx_classify_symbolic_expression (rtx x);
static bool sx_valid_base_register_p (rtx x, enum machine_mode mode,
				      int strict);
static bool sx_classify_address (struct sx_address_info *info, rtx x,
				 enum machine_mode mode, int strict);
static bool sx_symbolic_address_p (enum sx_symbol_type symbol_type,
				   enum machine_mode mode);
static bool sx_offset_within_object_p (rtx symbol, HOST_WIDE_INT offset);
static void sx_split_const (rtx x, rtx *base, HOST_WIDE_INT *offset);
static enum sx_symbol_type sx_classify_symbol (rtx x);
static int sx_symbol_insns (enum sx_symbol_type type);
static rtx sx_add_offset (rtx temp, rtx reg, HOST_WIDE_INT offset);
static rtx sx_force_temporary (rtx dest, rtx value);
static unsigned int sx_build_shift (struct sx_integer_op *codes,
				    HOST_WIDE_INT value);
static unsigned int sx_build_lower (struct sx_integer_op *codes,
				    unsigned HOST_WIDE_INT value);
static unsigned int sx_build_integer (struct sx_integer_op *codes,
                    unsigned HOST_WIDE_INT value);
static bool sx_symbolic_constant_p (rtx x, enum sx_symbol_type *symbol_type);
static rtx sx_split_symbol (rtx temp, rtx addr);

#ifndef SX_RTL_PRO_EPI_LOGUE
static void asm_function_prologue (FILE *file, int size);
static void asm_function_epilogue (FILE *file, int size);
#endif /* !SX_RTL_PRO_EPI_LOGUE */

/* holding start address of each section for address conversion
   FIXME: currently not used anywhere. if this does not change,
   remove code and these declarations as well */
static void sx_remember_data_section(char *name);
static void sx_remember_linkage_section(char *name);
static void sx_remember_bss_section(FILE *stream);

/* address conversion */
static const char *sx_address_conversion(char *opcode, rtx op1 , rtx op2);

#if 0
static void show_stackframe(void);
#endif

static void abort_with_insn (rtx insn, const char *reason);
static void oops_message(char *str);


/* from mips.c */
/* Abort after printing out a specific insn.  */

static void
abort_with_insn (rtx insn, const char *reason)
{
  error (reason);
  debug_rtx (insn);
  abort ();
}

static void
oops_message(char *str)
{
  fputs(str, stderr);
}

/*
 * for holding comparison operands.
 */
rtx sx_compare_op0, sx_compare_op1;
enum machine_mode sx_compare_mode;

int
hard_regno_mode_ok_func(int regno, int mode)
{
  /* modes that require multiple registers we choose to 
     always put in an even/odd pair. This is required on
     the FP side but not on the integer. We do it there anyway
     just to make things easier.
   */
  if (GET_MODE_SIZE(mode) > UNITS_PER_WORD &&
      ((regno&1) != 0))
    return 0;

  /* int mode, int register */
  if (INTEGRAL_MODE_P(mode) && 
      regno >= FIRST_S_REG && regno <= LAST_S_REG)
    return 1;

  /* float mode and float register */
  if(FLOAT_MODE_P(mode) &&
     regno >= FIRST_S_REG && regno <= LAST_S_REG)
    return 1;

  /* comparison mode: any scalar register */
  if ((mode == CCSSImode || mode == CCSDImode || mode == CCUDImode ||
       mode == CCFSFmode || mode == CCFDFmode) &&
      regno >= FIRST_S_REG && regno <= LAST_S_REG)
      return 1;

  return 0;
}

int
reg_class_from_letter(int chr)
{
  switch(chr)
    {
    case 'r':
      return S_REGS;
    case 'd':
      return S_REGS;
    case 'f':
      return S_REGS;
    default:
      return NO_REGS;
    }
}


/* Return a composite value in a pair of scalar registers.
   MODE1 and OFFSET1 are the mode and byte offset for the first value,
   likewise MODE2 and OFFSET2 for the second.  MODE is the mode of the
   complete value.
*/
static rtx
sx_return_sreg_pair (enum machine_mode mode,
		     enum machine_mode mode1, HOST_WIDE_INT offset1,
		     enum machine_mode mode2, HOST_WIDE_INT offset2,
		     int start)
{
  return gen_rtx_PARALLEL
    (mode,
     gen_rtvec (2,
                gen_rtx_EXPR_LIST (VOIDmode,
                                   gen_rtx_REG (mode1, start),
                                   GEN_INT (offset1)),
                gen_rtx_EXPR_LIST (VOIDmode,
                                   gen_rtx_REG (mode2, start + 1),
                                   GEN_INT (offset2))));
}

int
sx_regno_mode_ok_for_base_p (int regno,
			     enum machine_mode mode ATTRIBUTE_UNUSED,
			     int strict)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (!strict)
        return true;
      regno = reg_renumber[regno];
    }

  if (regno == ARG_POINTER_REGNUM || regno == STACK_POINTER_REGNUM)
    return true;

  return GP_REG_P(regno);
}


/* This function is used to implement GO_IF_LEGITIMATE_ADDRESS.  It
   returns a nonzero value if X is a legitimate address for a memory
   operand of the indicated MODE.  STRICT is nonzero if this function
   is called during reload.  */

bool
sx_legitimate_address_p (enum machine_mode mode, rtx x, int strict)
{
  struct sx_address_info addr;

  return sx_classify_address (&addr, x, mode, strict);
}


/* Return true if X is a valid address for machine mode MODE.  If it is,
   fill in INFO appropriately.  STRICT is true if we should only accept
   hard base registers.  */

static bool
sx_classify_address (struct sx_address_info *info, rtx x,
                       enum machine_mode mode, int strict)
{
  switch (GET_CODE (x))
    {
    case REG:
    case SUBREG:
      info->type = ADDRESS_REG;
      info->reg = x;
      info->offset = const0_rtx;
      return sx_valid_base_register_p (info->reg, mode, strict);

    case PLUS:
      info->type = ADDRESS_REG;
      info->reg = XEXP (x, 0);
      info->offset = XEXP (x, 1);
      return (sx_valid_base_register_p (info->reg, mode, strict)
              && const_arith_operand (info->offset, VOIDmode));

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      info->type = ADDRESS_SYMBOLIC;
      return (sx_symbolic_constant_p (x, &info->symbol_type)
              && sx_symbolic_address_p (info->symbol_type, mode));

    default:
      return false;
    }
}


/* Classify symbolic expression X, given that it appears in context
   CONTEXT.  */
static enum sx_symbol_type
sx_classify_symbolic_expression (rtx x)
{
  HOST_WIDE_INT offset;

#ifdef SXDEBUG
  fprintf(stderr, "%s: x = ", __FUNCTION__);
  print_inline_rtx(stderr, x, 1);
  fprintf(stderr, "   \n");
#endif

  sx_split_const(x, &x, &offset);
  if (UNSPEC_ADDRESS_P(x))
    return UNSPEC_ADDRESS_TYPE(x);

  return sx_classify_symbol(x);
}

/* Classify symbol X, which must be a SYMBOL_REF or a LABEL_REF.  */

static enum sx_symbol_type
sx_classify_symbol (rtx x)
{
  tree decl;
  enum sx_symbol_type type;

  if (GET_CODE (x) == LABEL_REF)
    {
      type = SYMBOL_GOT_LOCAL;
      goto out;
    }

  gcc_assert (GET_CODE (x) == SYMBOL_REF);

  if (SYMBOL_REF_TLS_MODEL (x))
    {
      type = SYMBOL_TLS;
      goto out;
    }

#ifdef SX_BROKEN
  /* GET_MODE_SIZE() returns ushort, so the below makes no sense;
     we need to determine which mode sizes should be flagged as
     small data before turning this back on, if at all. */
  if (CONSTANT_POOL_ADDRESS_P (x))
    {
      if (GET_MODE_SIZE (get_pool_mode (x)) <= -1)
        {
          type = SYMBOL_SMALL_DATA;
          goto out;
        }
    }
#endif /* SX_BROKEN */

  /* Do not use small-data accesses for weak symbols; they may end up
     being zero.  */
  if (SYMBOL_REF_SMALL_P (x) && !SYMBOL_REF_WEAK (x))
    {
      type = SYMBOL_SMALL_DATA;
      goto out;
    }

  {
    decl = SYMBOL_REF_DECL (x);
    if (decl == 0)
      {
        if (!SYMBOL_REF_LOCAL_P (x))
          {
            type = SYMBOL_GOT_GLOBAL;
            goto out;
          }
      }
    else
      {
        /* Don't use GOT accesses for locally-binding symbols if
           TARGET_ABSOLUTE_ABICALLS.  Otherwise, there are three
           cases to consider:
	   
           - o32 PIC (either with or without explicit relocs)
           - n32/n64 PIC without explicit relocs
           - n32/n64 PIC with explicit relocs
	   
           In the first case, both local and global accesses will use an
           R_MIPS_GOT16 relocation.  We must correctly predict which of
           the two semantics (local or global) the assembler and linker
           will apply.  The choice doesn't depend on the symbol's
           visibility, so we deliberately ignore decl_visibility and
           binds_local_p here.

           In the second case, the assembler will not use R_MIPS_GOT16
           relocations, but it chooses between local and global accesses
           in the same way as for o32 PIC.

           In the third case we have more freedom since both forms of
           access will work for any kind of symbol.  However, there seems
           little point in doing things differently.

           Note that weakref symbols are not TREE_PUBLIC, but their
           targets are global or weak symbols.  Relocations in the
           object file will be against the target symbol, so it's
           that symbol's binding that matters here.  */
        if (DECL_P (decl)
            && (TREE_PUBLIC (decl) || DECL_WEAK (decl))
            && !(targetm.binds_local_p (decl)))
          {
            type = SYMBOL_GOT_GLOBAL;
            goto out;
          }
      }
    if (SYMBOL_REF_FAR_ADDR_P(x) ||
        (SYMBOL_REF_FUNCTION_P(x) && SYMBOL_REF_EXTERNAL_P(x)))
      {
        type = SYMBOL_GOT_GLOBAL;
        goto out;
      }
    type = SYMBOL_GOT_LOCAL;
    goto out;
  }
  type = SYMBOL_GENERAL;

 out:
#ifdef SXDEBUG
  fprintf(stderr, "%s: x= ", __FUNCTION__);
  print_inline_rtx(stderr, x, 1);
  fprintf(stderr, "   >>> type=%s\n", sx_symbol_name[type]);
#endif

  return type;
}


/* Return true if X is a valid base register for the given mode.
   Allow only hard registers if STRICT.  */

static bool
sx_valid_base_register_p (rtx x, enum machine_mode mode, int strict)
{
  if (!strict && GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  return (REG_P (x)
          && sx_regno_mode_ok_for_base_p (REGNO (x), mode, strict));
}


/* Return true if symbols of type SYMBOL_TYPE can directly address a value
 *    with mode MODE.  This is used for both symbolic and LO_SUM addresses.  */

static bool
sx_symbolic_address_p (enum sx_symbol_type symbol_type,
		       enum machine_mode mode)
{
  switch (symbol_type)
    {
    case SYMBOL_GENERAL:
      return true;

    case SYMBOL_SMALL_DATA:
      return true;

    case SYMBOL_CONSTANT_POOL:
      /* PC-relative addressing is only available for lw and ld.  */
      return GET_MODE_SIZE (mode) == 4 || GET_MODE_SIZE (mode) == 8;

    case SYMBOL_GOT_LOCAL:
      return true;

    case SYMBOL_GOT_GLOBAL:
      /* The address will have to be loaded from the GOT first.  */
      return false;

    case SYMBOL_GOTOFF_PAGE:
    case SYMBOL_GOTOFF_GLOBAL:
    case SYMBOL_GOTOFF_CALL:
    case SYMBOL_GOTOFF_LOADGP:
    case SYMBOL_TLS:
    case SYMBOL_TLSGD:
    case SYMBOL_TLSLDM:
    case SYMBOL_DTPREL:
    case SYMBOL_GOTTPREL:
    case SYMBOL_TPREL:
    case SYMBOL_64_HIGH:
    case SYMBOL_64_LOW:
      return true;
    }
  gcc_unreachable ();
}

/* Return true if SYMBOL is a SYMBOL_REF and OFFSET + SYMBOL points
   to the same object as SYMBOL, or to the same object_block.  */

static bool
sx_offset_within_object_p (rtx symbol, HOST_WIDE_INT offset)
{
  if (GET_CODE (symbol) != SYMBOL_REF)
    return false;

  if (CONSTANT_POOL_ADDRESS_P (symbol)
      && offset >= 0
      && offset < (int) GET_MODE_SIZE (get_pool_mode (symbol)))
    return true;

  if (SYMBOL_REF_DECL (symbol) != 0
      && offset >= 0
      && offset < int_size_in_bytes (TREE_TYPE (SYMBOL_REF_DECL (symbol))))
    return true;

  if (SYMBOL_REF_HAS_BLOCK_INFO_P (symbol)
      && SYMBOL_REF_BLOCK (symbol)
      && SYMBOL_REF_BLOCK_OFFSET (symbol) >= 0
      && ((unsigned HOST_WIDE_INT) offset + SYMBOL_REF_BLOCK_OFFSET (symbol)
          < (unsigned HOST_WIDE_INT) SYMBOL_REF_BLOCK (symbol)->size))
    return true;

  return false;
}


/* Return true if X is a symbolic constant that can be calculated in
   the same way as a bare symbol.  If it is, store the type of the
   symbol in *SYMBOL_TYPE.  */

static bool
sx_symbolic_constant_p (rtx x, enum sx_symbol_type *symbol_type)
{
  HOST_WIDE_INT offset;

#ifdef SXDEBUG
  fprintf(stderr, "\n%s: x= ", __FUNCTION__);
  print_inline_rtx(stderr, x, 1);
#endif

  sx_split_const (x, &x, &offset);
#ifdef SXDEBUG
  fprintf(stderr, " splitted:  base= ");
  print_inline_rtx(stderr, x, 0);
  fprintf(stderr, " offs=%ld", offset);
#endif
  if (UNSPEC_ADDRESS_P (x))
    *symbol_type = UNSPEC_ADDRESS_TYPE (x);
  else if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF)
    {
      *symbol_type = sx_classify_symbol (x);
      if (*symbol_type == SYMBOL_TLS) {
#ifdef SXDEBUG
        fprintf(stderr, " SYMBOL_TLS (false)\n");
#endif
        return false;
      }
    }
  else
    {
#ifdef SXDEBUG
      fprintf(stderr, " else... (false)\n");
#endif
      return false;
    }

  if (offset == 0)
    {
#ifdef SXDEBUG
      fprintf(stderr, " offset=0 (true)\n");
#endif
      return true;
    }
#ifdef SXDEBUG
  fprintf(stderr, " offset nonzero (%s) (?)\n", sx_symbol_name[*symbol_type]);
#endif

  /* Check whether a nonzero offset is valid for the underlying
     relocations.  */
  switch (*symbol_type)
    {
    case SYMBOL_GENERAL:
    case SYMBOL_64_HIGH:
    case SYMBOL_64_LOW:
      /* If the target has 64-bit pointers and the object file only
         supports 32-bit symbols, the values of those symbols will be
         sign-extended.  In this case we can't allow an arbitrary offset
         in case the 32-bit value X + OFFSET has a different sign from X.  */
      if (Pmode == DImode && !1)
        return sx_offset_within_object_p (x, offset);

      /* In other cases the relocations can handle any offset.  */
      return true;

    case SYMBOL_CONSTANT_POOL:
      /* Allow constant pool references to be converted to LABEL+CONSTANT.
         In this case, we no longer have access to the underlying constant,
         but the original symbol-based access was known to be valid.  */
      if (GET_CODE (x) == LABEL_REF)
        return true;

      /* Fall through.  */

    case SYMBOL_SMALL_DATA:
      /* Make sure that the offset refers to something within the
         underlying object.  This should guarantee that the final
         PC- or GP-relative offset is within the 16-bit limit.  */
      return sx_offset_within_object_p (x, offset);

    case SYMBOL_GOT_LOCAL:
    case SYMBOL_GOTOFF_PAGE:
      /* The linker should provide enough local GOT entries for a
         16-bit offset.  Larger offsets may lead to GOT overflow.  */
      return SMALL_OPERAND (offset);

    case SYMBOL_GOT_GLOBAL:
    case SYMBOL_GOTOFF_GLOBAL:
    case SYMBOL_GOTOFF_CALL:
    case SYMBOL_GOTOFF_LOADGP:
    case SYMBOL_TLSGD:
    case SYMBOL_TLSLDM:
    case SYMBOL_DTPREL:
    case SYMBOL_TPREL:
    case SYMBOL_GOTTPREL:
    case SYMBOL_TLS:
      return false;
    }
  gcc_unreachable ();
}

/* Split X into a base and a constant offset, storing them in *BASE
   and *OFFSET respectively.  */

static void
sx_split_const (rtx x, rtx *base, HOST_WIDE_INT *offset)
{
#ifdef SXDEBUG
  fprintf(stderr, "%s: x = ", __FUNCTION__);
  print_inline_rtx(stderr, x, 1);
  fprintf(stderr, "   \n  base = ");
  print_inline_rtx(stderr, *base, 1);
  fprintf(stderr, "   \n");
#endif

  *offset = 0;

  if (GET_CODE (x) == CONST)
    {
      x = XEXP (x, 0);
      if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == CONST_INT)
        {
          *offset += INTVAL (XEXP (x, 1));
          x = XEXP (x, 0);
        }
    }
  *base = x;
}

/* Return a LO_SUM expression for ADDR.  TEMP is as for sx_force_temporary
   and is used to load the high part into a register.  (NEW)*/

static rtx
sx_split_symbol (rtx temp, rtx addr)
{
  rtx high;

  high = sx_force_temporary (temp, gen_rtx_HIGH(Pmode, copy_rtx(addr)));
  return gen_rtx_LO_SUM (Pmode, high, addr);
}


/* Split a TImode or TFmode move from OP[1] to OP[0] into a pair of
   DImode moves from OP[2,3] to OP[0,1].  If FIXUP_OVERLAP is true,
   guarantee that the sequence
     set (OP[0] OP[2])
     set (OP[1] OP[3])
   is valid.  Naturally, output operand ordering is little-endian.
   This is used by *movtf_internal and *movti_internal.
   (from alpha)
*/
  
void
sx_split_tmode_pair (rtx operands[4], enum machine_mode mode,
		     bool fixup_overlap)
{

  DBG_RTX2("START", "op0", operands[0], "op1", operands[1]);

  switch (GET_CODE (operands[1]))
    {
    case REG:
      operands[3] = gen_rtx_REG (DImode, REGNO (operands[1]) + 1);
      operands[2] = gen_rtx_REG (DImode, REGNO (operands[1]));
      break;

    case MEM:
      operands[3] = adjust_address (operands[1], DImode, 8);
      operands[2] = adjust_address (operands[1], DImode, 0);
      break;

    case CONST_INT:
    case CONST_DOUBLE:
      gcc_assert (operands[1] == CONST0_RTX (mode));
      operands[2] = operands[3] = const0_rtx;
      break;

    default:
      gcc_unreachable ();
    }

  switch (GET_CODE (operands[0]))
    {
    case REG:
      operands[1] = gen_rtx_REG (DImode, REGNO (operands[0]) + 1);
      operands[0] = gen_rtx_REG (DImode, REGNO (operands[0]));
      break;

    case MEM:
      operands[1] = adjust_address (operands[0], DImode, 8);
      operands[0] = adjust_address (operands[0], DImode, 0);
      break;

    default:
      gcc_unreachable ();
    }

  if (fixup_overlap && reg_overlap_mentioned_p (operands[0], operands[3]))
    {
      rtx tmp;
      tmp = operands[0], operands[0] = operands[1], operands[1] = tmp;
      tmp = operands[2], operands[2] = operands[3], operands[3] = tmp;
    }

  DBG_RTX2("END", "op0", operands[0], "op1", operands[1]);
  DBG_RTX2("END", "op2", operands[2], "op3", operands[3]);

}


/* This function is used to implement LEGITIMIZE_ADDRESS.  If *XLOC can
   be legitimized in a way that the generic machinery might not expect,
   put the new address in *XLOC and return true.  MODE is the mode of
   the memory being accessed.  */

bool
sx_legitimize_address (rtx *xloc, enum machine_mode mode)
{
#ifdef SX_DEBUG_L
  fprintf(stderr, "%s: rtx = ", __FUNCTION__);
  print_inline_rtx(stderr, *xloc, 1);
  fprintf(stderr, "   mode = %s\n", GET_MODE_NAME(mode));
#endif

  if (GET_CODE (*xloc) == PLUS
      && GET_CODE (XEXP (*xloc, 0)) == REG
      && GET_CODE (XEXP (*xloc, 1)) == CONST_INT)
    {
      /* Handle REG + CONSTANT using sx_add_offset.  */
      rtx reg;

      reg = XEXP (*xloc, 0);
      if (!sx_valid_base_register_p (reg, mode, 0))
        reg = copy_to_mode_reg (Pmode, reg);
      *xloc = sx_add_offset (0, reg, INTVAL (XEXP (*xloc, 1)));
      return true;
    }

  /* this should evolve to something more smart ... */
  if (GET_CODE(*xloc) == SYMBOL_REF)
    {
      /* loading a symbol ref via ptr to linkage section */
      *xloc = force_const_mem (Pmode, *xloc);
#ifdef SX_DEBUG_L
      fprintf(stderr, "\t after_force_const_mem rtx = ");
      print_inline_rtx(stderr, *xloc, 1);
      fprintf(stderr, "   mode = %s\n", GET_MODE_NAME(mode));
#endif
      return true;
    }

  return false;
}

/* Return a legitimate address for REG + OFFSET.  TEMP is as for
   mips_force_temporary; it is only needed when OFFSET is not a
   SMALL_OPERAND.  */

static rtx
sx_add_offset (rtx temp, rtx reg, HOST_WIDE_INT offset)
{
  if (!SMALL_OPERAND (offset))
    {
      rtx high;
          /* Leave OFFSET as a 32-bit offset and put the excess in HIGH.  */
          high = GEN_INT (CONST_HIGH_PART (offset));
          offset = CONST_LOW_PART (offset);
       
      high = sx_force_temporary (temp, high);
      reg = sx_force_temporary (temp, gen_rtx_PLUS (Pmode, high, reg));
    }
  return plus_constant (reg, offset);
}

/* Copy VALUE to a register and return that register.  If new pseudos
 * are allowed, copy it into a new register, otherwise use DEST.  */

static rtx
sx_force_temporary (rtx dest, rtx value)
{
  if (!no_new_pseudos)
    return force_reg (Pmode, value);
  else
    {
      emit_move_insn (copy_rtx (dest), value);
      return dest;
    }
}

/*
 * Subroutine of sx_build_integer (with the same interface).
 * Assume that the final action in the sequence should be a left shift.
 */

static unsigned int
sx_build_shift (struct sx_integer_op *codes, HOST_WIDE_INT value)
{
  unsigned int i, shift;

  /* Shift VALUE right until its lowest bit is set.  Shift arithmetically
     since signed numbers are easier to load than unsigned ones.  */
  shift = 0;
  while ((value & 1) == 0)
    value >>= 1, shift++;

  i = sx_build_integer (codes, value);
  codes[i].code = ASHIFT;
  codes[i].value = shift;
  return i + 1;
}


/* As for sx_build_shift, but assume that the final action will be
   an IOR or PLUS operation.  */
static unsigned int
sx_build_lower (struct sx_integer_op *codes, unsigned HOST_WIDE_INT value)
{
  unsigned int i;

  i = sx_build_integer (codes, CONST_HIGH_PART (value));
  codes[i].code = PLUS;
  codes[i].value = CONST_LOW_PART (value);
  return i + 1;
}


/* Fill CODES with a sequence of rtl operations to load VALUE.
   Return the number of operations needed.  */

static unsigned int
sx_build_integer (struct sx_integer_op *codes,
                    unsigned HOST_WIDE_INT value)
{
#ifdef SXDEBUG
  fprintf(stderr, "%s: value = %ld\n", __FUNCTION__, value);
#endif
  if (SMALL_OPERAND (value) || SMALL_OPERAND_UNSIGNED (value))
    {
      /* The value can be loaded with a single instruction.  */
      codes[0].code = UNKNOWN;
      codes[0].value = value;
      return 1;
    }
  else if (CONST_LOW_PART (value))
    {
      return sx_build_lower (codes, value);
    }
  else if (CONST_LOW_PART(value) == 0)
    {
      /* The constant will need at least two actions.  The lowest
         32 bits are clear, so the final action will be a shift.  */
      return sx_build_shift (codes, value);
    }
  else
    {
      /* The final action could be a shift, add or inclusive OR.
         Rather than use a complex condition to select the best
         approach, try both sx_build_shift and sx_build_lower
         and pick the one that gives the shortest sequence.
         Note that this case is only used once per constant.  */
      struct sx_integer_op alt_codes[SX_MAX_INTEGER_OPS];
      unsigned int cost, alt_cost;

      cost = sx_build_shift (codes, value);
      alt_cost = sx_build_lower (alt_codes, value);
      if (alt_cost < cost)
        {
          memcpy (codes, alt_codes, alt_cost * sizeof (codes[0]));
          cost = alt_cost;
        }
      return cost;
    }
}


/* Likewise for constant X.  */
int
sx_const_insns (rtx x)
{
  struct sx_integer_op codes[SX_MAX_INTEGER_OPS];
  enum sx_symbol_type symbol_type;
  HOST_WIDE_INT offset;

#ifdef SXDEBUG
  fprintf(stderr, "%s: rtx= ", __FUNCTION__);
  print_inline_rtx(stderr, x, 1);
  fprintf(stderr, " mode=%s  ", GET_MODE_NAME(GET_MODE(x)));
#endif

  switch (GET_CODE (x))
    {
    case HIGH:
#ifdef SXDEBUG
      fprintf(stderr, " HIGH (1)\n", __FUNCTION__);
#endif
      return 1;

    case CONST_INT:
#ifdef SXDEBUG
      fprintf(stderr, " CONST_INT (%d)\n",
	      sx_build_integer (codes, INTVAL (x)));
#endif
      return sx_build_integer (codes, INTVAL (x));

    case CONST_DOUBLE:
    case CONST_VECTOR:
#ifdef SXDEBUG
      fprintf(stderr, " CONST_DOUBLE/VEC (%d)\n",
              (x == CONST0_RTX (GET_MODE (x)) ? 1 : 0));
#endif
      return (x == CONST0_RTX (GET_MODE (x)) ? 1 : 0);

    case CONST:
#ifdef SXDEBUG
      fprintf(stderr, " CONST symb_const(%d) type=%d ",
              sx_symbolic_constant_p(x, &symbol_type), symbol_type);
#endif
      /* See if we can refer to X directly.  */
      if (sx_symbolic_constant_p (x, &symbol_type))
        {
#ifdef SXDEBUG
          fprintf(stderr, " ret=%d\n",
                  sx_symbol_insns(symbol_type));
#endif
          return sx_symbol_insns (symbol_type);
        }
      /* Otherwise try splitting the constant into a base and offset.
         16-bit offsets can be added using an extra addiu.  Larger offsets
         must be calculated separately and then added to the base.  */
      sx_split_const (x, &x, &offset);
#ifdef SXDEBUG
      fprintf(stderr, " split_const offs=%ld x=",offset);
      print_inline_rtx(stderr, x, 0);
#endif
      if (offset != 0)
        {
          int n = sx_const_insns (x);
          if (n != 0)
            {
              if (SMALL_OPERAND (offset))
                {
#ifdef SXDEBUG
                  fprintf(stderr, " SMALL_OPERAND %d\n", n+1);
#endif
                  return n + 1;
                }
              else 
                {
#ifdef SXDEBUG
                  fprintf(stderr, " !SMALL_OPERAND %d\n",
                          n+1+sx_build_integer (codes, offset));
#endif
                  return n + 1 + sx_build_integer (codes, offset);
                }
            }
        }
#ifdef SXDEBUG
      fprintf(stderr, " ret=0\n");
#endif
      return 0;

    case SYMBOL_REF:
    case LABEL_REF:
#ifdef SXDEBUG
      fprintf(stderr, " SYMBOL_REF or LABEL_REF %d\n",
              sx_symbol_insns(sx_classify_symbol (x)));
#endif
      return sx_symbol_insns (sx_classify_symbol (x));

    default:
#ifdef SXDEBUG
      fprintf(stderr, " default ret=0\n");
#endif
      return 0;
    }
}

extern int size_directive_output;

static void __attribute__((format(printf, 4, 5)))
sx_declare_object (FILE *stream, const char *name, const char *init_string,
                   const char *final_string, ...)
{
  va_list ap;
  tree name_tree;

  fputs (init_string, stream);
  assemble_name (stream, name);
  va_start (ap, final_string);
  vfprintf (stream, final_string, ap);
  va_end (ap);

  name_tree = get_identifier (name);
  TREE_ASM_WRITTEN (name_tree) = 1;
}

void
sx_declare_object_name (FILE *stream, const char *name,
			tree decl ATTRIBUTE_UNUSED)
{
#if 0
#ifdef ASM_OUTPUT_TYPE_DIRECTIVE
  ASM_OUTPUT_TYPE_DIRECTIVE (stream, name, "object");
#endif

  size_directive_output = 0;
  if (!flag_inhibit_size_directive && DECL_SIZE (decl))
    {
      HOST_WIDE_INT size;

      size_directive_output = 1;
      size = int_size_in_bytes (TREE_TYPE (decl));
      ASM_OUTPUT_SIZE_DIRECTIVE (stream, name, size);
    }
#endif
  sx_declare_object (stream, name, "", ":\n");
}
/*#endif*/

/* from contrib/b */
/* check for patterns like 00000000000, 000001111111 or 11111000
   i.e. either : all bits are the same, or there is only one
   change of bits in the bit stream.
 */
int
sx_g_ok(unsigned HOST_WIDE_INT vh, unsigned HOST_WIDE_INT vl)
{
  int i;
  int prev, now, change;
  unsigned HOST_WIDE_INT mask;

  mask = 1 << (HOST_BITS_PER_WIDE_INT - 1);
  prev = (vh & mask) ? 1 : 0;
  mask >>= 1;
  change = 0;

  for (i=1; i < HOST_BITS_PER_WIDE_INT; i++, mask >>= 1)
    {
      now = (vh & mask) ? 1 : 0;
      if (prev != now)
        change ++;
      prev = now;
    }

  mask = 1 << (HOST_BITS_PER_WIDE_INT - 1);

  for (i = 0; i < HOST_BITS_PER_WIDE_INT; i++, mask >>= 1)
    {
      now = (vl & mask) ? 1 : 0;
      if (prev != now)
        change ++;
      prev = now;
    }

#ifdef SXDEBUG
  if ((change == 0) || (change == 1))
    fprintf (stderr, "g_ok: high %08x low %08x -> %d\n", vh, vl, change);
#endif

  return (change == 0) || (change == 1);
}


/* srl $sXX,(m)0 or (m)1 style
   0000ffff00000000   -> srl $sXX,(16)1,16
*/
int
sx_h_ok(unsigned HOST_WIDE_INT vh, unsigned HOST_WIDE_INT vl)
{
  int i;
  int prev, now, change;
  unsigned HOST_WIDE_INT mask;

  mask = 1 << (HOST_BITS_PER_WIDE_INT - 1);
  prev = (vh & mask) ? 1 : 0;
  if (prev)
    return 0;

  mask >>= 1;
  change = 0;

  for (i=1; i < HOST_BITS_PER_WIDE_INT; i++, mask >>= 1)
    {
      now = (vh & mask) ? 1 : 0;
      if (prev != now)
        change ++;
      prev = now;
    }

  mask = 1 << (HOST_BITS_PER_WIDE_INT - 1);

  for (i=0; i < HOST_BITS_PER_WIDE_INT; i++, mask >>= 1)
    {
      now = (vl & mask) ? 1 : 0;
      if (prev != now)
        change ++;
      prev = now;
    }

#ifdef SXDEBUG
  if (change == 2)
    fprintf (stderr, "h_ok: high %08x low %08x -> %d\n", vh, vl, change);
#endif
  return change == 2;
}

/*
  A C expression that is nonzero if X is a legitimate constant for
  an immediate operand on the target machine.  You can assume that X
  satisfies `CONSTANT_P', so you need not check this.  In fact, `1'
  is a suitable definition for this macro on machines where anything
  `CONSTANT_P' is valid.
*/
int
sx_legitimate_constant_p(rtx x)
{
#ifdef SXDEBUG
  fprintf(stderr, "%s: rtx = ", __FUNCTION__);
  print_inline_rtx(stderr, x, 1);
  fprintf(stderr, "\n");
#endif
  switch (GET_CODE (x))
    {
    case CONST_DOUBLE:
      /* DFmode (64bit double) or VOIDmode (64bit int on 32bit-int host) */
#ifdef SXDEBUG
      fprintf(stderr, "x is mode %s (const_double)\n",
              GET_MODE_NAME(GET_MODE(x)));
#endif
      if (GET_MODE (x) == VOIDmode)
        return sx_g_ok (CONST_DOUBLE_HIGH (x), CONST_DOUBLE_LOW (x)) ||
          sx_h_ok (CONST_DOUBLE_HIGH (x), CONST_DOUBLE_LOW (x));
      return 0;
      break;
    case CONST_INT:
#ifdef SXDEBUG
      fprintf(stderr, "%s:    const_int mode=%s\n", __FUNCTION__,
              GET_MODE_NAME(GET_MODE(x)));
#endif
      if (GET_MODE (x) == SFmode)
        {
          fprintf(stderr, "const_int used for SFmode!\n");
          print_inline_rtx(stderr, x, 0);
          fprintf(stderr, "\n");
          return 0;
        }
      if (GET_MODE (x) == DImode)
        {
#ifdef SXDEBUG
          fprintf(stderr, "const_int(64bit):0 (%s)\n", XSTR (x,0));
#endif
          return 0;
        }
      else
        return 1;
      break;
    case SYMBOL_REF:
#ifdef SXDEBUG
      fprintf(stderr, "%s: symbol_ref:%d (%s)   ", __FUNCTION__,
              CONSTANT_POOL_ADDRESS_P (x) ? 1 : 0, XSTR (x,0));
      print_inline_rtx(stderr, x, 0);
      fprintf(stderr, "\n");
#endif
      return CONSTANT_POOL_ADDRESS_P (x);
      break;
    case CONST:
      if (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF)
        {
#ifdef SXDEBUG
          fprintf(stderr, "%s: const symbol_ref:%d (%s)\n", __FUNCTION__,
                  CONSTANT_POOL_ADDRESS_P (XEXP (XEXP (x, 0), 0)),
                  XSTR (XEXP (XEXP (x, 0), 0),0));
#endif
          return CONSTANT_POOL_ADDRESS_P (XEXP (XEXP (x, 0), 0));
        }
#ifdef SXDEBUG
      fprintf(stderr, "%s: const NOT symbol_ref: x=", __FUNCTION__);
      print_inline_rtx(stderr, x, 0);
      fprintf(stderr, "\n");
#endif
      return 1;
      break;
    default:
      return 0;
    }

#ifdef SXDEBUG
  fprintf(stderr, "%s: default exit:  x=", __FUNCTION__);
  print_inline_rtx(stderr, x, 0);
  fprintf(stderr, "\n");
#endif
  return 1;
}

/*
 * A C expression that is 1 if the RTX X is a constant which is a valid
 *  address.
 */
int
constant_address_p(rtx x)
{
#ifdef SXDEBUG
  fprintf(stderr, "%s: rtx = ", __FUNCTION__);
  print_inline_rtx(stderr, x, 1);
  fprintf(stderr, "\n");
#endif
  if (GET_CODE (x) == SYMBOL_REF &&
      !CONSTANT_POOL_ADDRESS_P (x))
    {
      return 0;
    }
  if (GET_CODE (x) == CONST)
    {
      if (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF &&
          !CONSTANT_POOL_ADDRESS_P (XEXP (XEXP (x, 0), 0)))
        {
#ifdef SXDEBUG
          fprintf(stderr, "%s: const not in pool x = ", __FUNCTION__);
          print_inline_rtx(stderr, x, 1);
          fprintf(stderr, "\n");
#endif
          /* const is not in pool */
          return 0;
        }
    }
  return CONSTANT_P (x);
}



/* Return the number of instructions needed to load a symbol of the
   given type into a register.  If valid in an address, the same number
   of instructions are needed for loads and stores.  Treat extended
   mips16 instructions as two instructions.  */

static int
sx_symbol_insns (enum sx_symbol_type type)
{
  switch (type)
    {
    case SYMBOL_GENERAL:
      return 3;

    case SYMBOL_SMALL_DATA:
      return 1;

    case SYMBOL_CONSTANT_POOL:
      /* This case is for mips16 only.  Assume we'll need an
         extended instruction.  */
      return 2;

    case SYMBOL_GOT_LOCAL:
      /* Unless -funit-at-a-time is in effect, we can't be sure whether
         the local/global classification is accurate.  See override_options
         for details.

         The worst cases are:

         (1) For local symbols when generating o32 or o64 code.  The assembler
             will use:

                 lw           $at,%got(symbol)
                 nop

             ...and the final address will be $at + %lo(symbol).

         (2) For global symbols when -mxgot.  The assembler will use:

                 lui     $at,%got_hi(symbol)
                 (d)addu $at,$at,$gp

             ...and the final address will be $at + %got_lo(symbol).  */
      return 3;

    case SYMBOL_GOT_GLOBAL:
      return 0;

    case SYMBOL_GOTOFF_PAGE:
    case SYMBOL_GOTOFF_GLOBAL:
    case SYMBOL_GOTOFF_CALL:
    case SYMBOL_GOTOFF_LOADGP:
    case SYMBOL_64_HIGH:
    case SYMBOL_64_LOW:
    case SYMBOL_TLSGD:
    case SYMBOL_TLSLDM:
    case SYMBOL_DTPREL:
    case SYMBOL_GOTTPREL:
    case SYMBOL_TPREL:
      /* Check whether the offset is a 16- or 32-bit value.  */
      return 2 ;

    case SYMBOL_TLS:
      /* We don't treat a bare TLS symbol as a constant.  */
      return 0;
    }
  gcc_unreachable ();
}

/* Implement FUNCTION_ARG_BOUNDARY.  Every parameter gets at least
   PARM_BOUNDARY bits of alignment, but will be given anything up
   to STACK_BOUNDARY bits if the type requires it.  */

int
function_arg_boundary (enum machine_mode mode, tree type)
{
  unsigned int alignment;

  alignment = type ? TYPE_ALIGN (type) : GET_MODE_ALIGNMENT (mode);
  if (alignment < PARM_BOUNDARY)
    alignment = PARM_BOUNDARY;
  if (alignment > STACK_BOUNDARY)
    alignment = STACK_BOUNDARY;
  return alignment;
}

/* Return true if FUNCTION_ARG_PADDING (MODE, TYPE) should return
   upward rather than downward.  In other words, return true if the
   first byte of the stack slot has useful data, false if the last
   byte does.  */
bool
sx_pad_arg_upward (enum machine_mode mode, tree type)
{
  /* Integral types are padded downward: the last byte of a
     stack argument is passed in the last byte of the stack slot.  */
  if (type != 0
      ? INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type)
      : GET_MODE_CLASS (mode) == MODE_INT)
    return false;

  /* floating point is padded upwards */
  if (type != 0 ? FLOAT_TYPE_P (type) : GET_MODE_CLASS (mode) == MODE_FLOAT)
    return true;

  /* Arguments smaller than a stack slot are padded downward.  */
  if (mode != BLKmode)
    return (GET_MODE_BITSIZE (mode) >= PARM_BOUNDARY);
  else
    return (int_size_in_bytes (type) >= (PARM_BOUNDARY / BITS_PER_UNIT));
}

/* Define how to find the value returned by a function.  VALTYPE is the
   data type of the value (as a tree).  If the precise function being
   called is known, FUNC is its FUNCTION_DECL; otherwise, FUNC is 0.
   MODE is set instead of VALTYPE for libcalls.

   On SX the value is found in $123 for integer functions and
   in $124/$125 for floating-point functions.  */

rtx
sx_function_value (tree valtype, tree func ATTRIBUTE_UNUSED,
		   enum machine_mode mode)
{

  if (valtype)
    {
      int unsignedp;

      mode = TYPE_MODE (valtype);
      unsignedp = TYPE_UNSIGNED (valtype);
      mode = promote_mode (valtype, mode, &unsignedp, 1);
    }


  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      /* || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT) */
      && GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
    return gen_rtx_REG (mode, 124);

  if (mode == HImode || mode == QImode || mode == DImode)
    return gen_rtx_REG(mode, 123);

  if (mode == TFmode)
    return sx_return_sreg_pair (mode,
                                DFmode, 0,
                                DFmode, GET_MODE_SIZE (mode) / 2, 124);
  
  if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
    return sx_return_sreg_pair (mode,
                                GET_MODE_INNER (mode), 0,
                                GET_MODE_INNER (mode),
                                GET_MODE_SIZE (mode) / 2, 124);

  return gen_rtx_REG(mode,123);
}

static void
sx_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);

#ifdef SXDEBUG
  fprintf(stderr, "%s: rtl= ", __FUNCTION__);
  print_inline_rtx(stderr, rtl, 0);
  fprintf(stderr, "\n");
#endif
  if (TREE_CODE (decl) == VAR_DECL
      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl))
      /* && ix86_in_large_data_p (decl)*/)
    SYMBOL_REF_FLAGS (XEXP (rtl, 0)) |= SYMBOL_FLAG_FAR_ADDR;
}

/* Operand 1 is known to be a constant, and should require more than one
   instruction to load.  Emit that multi-part load.  */

static void
sx_split_const_movdi(rtx target, HOST_WIDE_INT value)
{
  HOST_WIDE_INT lo, hi, c;

  lo = ((value & 0xffffffff) ^ 0x80000000) - 0x80000000;
  c = (value - lo) >> 32;
  hi = ((c & 0xffffffff) ^ 0x80000000) - 0x80000000;

  if (hi)
    {
      emit_move_insn(target, GEN_INT(hi));
      emit_move_insn(target, gen_rtx_ASHIFT(DImode, target, GEN_INT(32)));
    }
  if (lo)
    {
      emit_move_insn(target, gen_rtx_PLUS(DImode, target, GEN_INT(lo)));
    }
}

bool
sx_mov_source_needs_s4(rtx x)
{
  /* we need to denote implicit base addressing via s4 if x
       is a MEM and address is either a symbol ref or an
       (possibly constant) addition/subtraction to/from a
       symbol ref */
  if(GET_CODE(x) == MEM)
    {
      x = XEXP(x, 0);
      if(GET_CODE(x) == SYMBOL_REF)
        return true;
      if(GET_CODE(x) == CONST)
        x = XEXP(x, 0);
      if(GET_CODE(x) == PLUS || GET_CODE(x) == MINUS)
        {
          if(GET_CODE(XEXP(x, 0)) == SYMBOL_REF ||
             GET_CODE(XEXP(x, 1)) == SYMBOL_REF)
            return true;
        }
    }
  return false;
}

void
sx_emit_mem_mem_mov(rtx dst, rtx src)
{
  rtx tmpreg, submem;

  submem = XEXP(src, 0);
  tmpreg = gen_reg_rtx(GET_MODE(submem));

  emit_insn(gen_movdi(tmpreg, submem));

  XEXP(src, 0) = tmpreg;
  emit_insn(gen_movdi(dst, src));
}

void
sx_emit_mov_via_s4(rtx dst, rtx src)
{
  /* this is loaded via implicit base reg s4, so we need
     to denote its use in rtl to prevent optimizer from
     deleting the s4 load from prologue */
  rtx par = gen_rtx_PARALLEL(VOIDmode, rtvec_alloc(2));
  XVECEXP(par, 0, 0) = 
    gen_rtx_SET(VOIDmode, dst, src);
  XVECEXP(par, 0, 1) =
    gen_rtx_USE(VOIDmode, gen_rtx_REG(DImode, 4));
  emit_insn(par);
}

/* move expansion, right now only used for movdi */
bool
sx_expand_mov(enum machine_mode mode, rtx *operands)
{
#ifdef SX_DEBUG_E
  DBG_RTX2("\t","op[0]", operands[0], "op[1]", operands[1]);
#endif

  /* If the output is not a register, the input must be.  */
  if ((reload_in_progress | reload_completed) == 0
      && GET_CODE(operands[0]) == MEM
      && !register_operand(operands[1], mode))
    {
      operands[1] = force_reg(mode, operands[1]);
    }

  if (mode == Pmode && symbolic_operand(operands[1], mode))
    {
      rtx tmp = operands[1];

      if (sx_legitimize_address(&operands[1], mode))
        {
          if (tmp == operands[1])
            {
              /* why this return, anyway? */
              return true;
            }

          return false;
        }
    }

  /* Early out for non-constants and valid constants.  */
  if (!CONSTANT_P(operands[1])
      || reg_or_IN_operand(operands[1], mode)
      || memory_operand(operands[1], mode)
      || sym_ref_mem_operand(operands[1], mode)
      )
    {
      return false;
    }

  /* Split large integers. */
  if (((GET_CODE(operands[1]) == CONST_INT) && mode == DImode)
      /* || GET_CODE(operands[1]) == CONST_DOUBLE
         || GET_CODE(operands[1]) == CONST_VECTOR */
      )
    {
      sx_split_const_movdi(operands[0], INTVAL(operands[1]));
      return true;
    }

  /* Otherwise we've nothing left but to drop the thing to memory.  */

  operands[1] = force_const_mem(mode, operands[1]);

  if (reload_in_progress)
    {
      emit_move_insn(operands[0], XEXP(operands[1], 0));
      operands[1] = copy_rtx(operands[1]);
      XEXP(operands[1], 0) = operands[0];
    }
  else
    {
      operands[1] = validize_mem(operands[1]);
    }

  return false;
}


/* Emit an instruction of the form (set TARGET (CODE OP0 OP1)).  */
static void
sx_emit_binary(enum rtx_code code, rtx target, rtx op0, rtx op1)
{
  emit_insn(gen_rtx_SET(VOIDmode, target,
			gen_rtx_fmt_ee(code, GET_MODE(target), op0, op1)));
}

/* NOTE: sx_select_cc_mode() is based on the assumption that we never
   need to select mode for operands larger than DI or DF (that is:
   no TI or TF operands). not really sure whether this is true, but
   hopefully, the testsuite will show it. */
enum machine_mode
sx_select_cc_mode (enum rtx_code code, rtx op0, rtx op1 ATTRIBUTE_UNUSED)
{
  if(GET_MODE_CLASS(GET_MODE(op0)) == MODE_INT)
    {
      switch(code)
        {
        case GTU:
        case GEU:
        case LTU:
        case LEU:
          return CCUDImode;
        default:
          if(GET_MODE(op0) == DImode)
            return CCSDImode;
          else
            return CCSSImode;
        }
    }
  else
    {
      if(GET_MODE(op0) == DFmode)
        return CCFDFmode;
      else
        return CCFSFmode;
    }
}

/* Convert a comparison into something that can be used in a branch or
   conditional move.  sx_compare_op0 and sx_compare_op1 are the values
   being compared and *CODE is the code used to compare them.

   Update *CODE, *OP0 and *OP1 so that they describe the final comparison.
   The standard branch condition is:
      - any comparison between a register and zero.  */

static void
sx_emit_compare (enum rtx_code *code,
                 rtx *op0, rtx *op1,
                 bool *invert ATTRIBUTE_UNUSED)
{
  bool uns = false;
  rtx compare, czero;
  enum machine_mode cmp_res_mode;

  /* determine whether it's an unsigned comparison, and zero-extend input
     operands if necessary (unsigned SI compares require DI operands) */
  if (GET_MODE_CLASS(GET_MODE(sx_compare_op0)) == MODE_INT)
    {
      if (sx_compare_mode == SImode)
        {
          switch (*code)
            {
            case GTU:
            case GEU:
            case LTU:
            case LEU:
              uns = true;
              /* we need to zero-extend SI operands to DI here in two cases:

                 1. for register/subregister operands. kind of obvious.

                 2. negative const_int operands.
                 why? otherwise, higher optimization levels will attempt to
                 pass some negative integers (those that fit M or I operand
                 type) to cmp as immediates. however, such immediates will
                 be sign extended to DI (due to machine semantics), which
                 is plain *wrong*.

                 such behaviour can be observed with gcc.dg/range-test-1.c,
                 in routine test2(). converting negative const_ints
                 remedies this!

                 now, there's more obscurity. gcc won't really let us
                 zero-extend a const_int (combiner will complain), so
                 we need to force the value into a register and zero
                 extend it there. thus, we will first force_reg the
                 const_int and then fall back to the first case. still
                 faster than memory loads, but anyway ... sigh ...
              */

              if(GET_CODE(sx_compare_op0) == CONST_INT &&
                 INTVAL(sx_compare_op0) < 0)
                {
                  sx_compare_op0 = force_reg(SImode, sx_compare_op0);
                }
              if(GET_CODE(sx_compare_op1) == CONST_INT &&
                 INTVAL(sx_compare_op1) < 0)
                {
                  sx_compare_op1 = force_reg(SImode, sx_compare_op1);
                }

              if (GET_CODE(sx_compare_op0) == REG ||
                  GET_CODE(sx_compare_op0) == SUBREG)
                {
                  rtx tmp = sx_compare_op0;
                  sx_compare_op0 = gen_reg_rtx(DImode);
                  emit_insn(gen_extend_insn(sx_compare_op0, tmp,
                                            DImode, SImode, 1));
                }
              if (GET_CODE(sx_compare_op1) == REG ||
                  GET_CODE(sx_compare_op1) == SUBREG)
                {
                  rtx tmp = sx_compare_op1;
                  sx_compare_op1 = gen_reg_rtx(DImode);
                  emit_insn(gen_extend_insn(sx_compare_op1, tmp,
                                            DImode, SImode, 1));
                }
              sx_compare_mode = DImode;
              break;
            default:
              break;
            }
        }
      else if (sx_compare_mode == DImode)
        {
          switch (*code)
            {
            case GTU:
            case GEU:
            case LTU:
            case LEU:
              uns = true;
              break;
            default:
              break;
            }
        }
    }

  cmp_res_mode = sx_select_cc_mode(*code, sx_compare_op0, sx_compare_op1);

  czero = CONST0_RTX(sx_compare_mode);

#if 0
  /* TODO: eventually, restore this optimization ... */
  if (sx_compare_op1 == czero)
    {
      /* no need for explicit compare */
      *op0 = force_reg(cmp_res_mode, sx_compare_op0);
      *op1 = czero;
    }
  else if (sx_compare_op0 == czero)
    {
      /* no need for explicit compare, but need to swap condition */
      *op0 = force_reg(cmp_res_mode, sx_compare_op1);
      *op1 = czero;
      *code = swap_condition(*code);
    }
  else
#endif
    {
      /* The comparison needs a separate cmp instruction. Store the
         result in *OP0 and compare it against zero.  */
      *op0 = gen_reg_rtx(cmp_res_mode);
      *op1 = czero;

      if(GET_MODE_CLASS(GET_MODE(sx_compare_op0)) == MODE_INT)
        {
          /* int compare and branch */
          if (sx_compare_mode == SImode)
            compare = gen_comp_si(*op0, sx_compare_op0, sx_compare_op1);
          else if (sx_compare_mode == DImode)
            {
              if (uns)
                compare = gen_comp_udi(*op0, sx_compare_op0, sx_compare_op1);
              else
                compare = gen_comp_di(*op0, sx_compare_op0, sx_compare_op1);
            }
          else
            gcc_assert(FALSE);
        }
      else
        {
          /* fp compare and branch */
          if(sx_compare_mode == SFmode)
            compare = gen_comp_sf(*op0, sx_compare_op0, sx_compare_op1);
          else if(sx_compare_mode == DFmode)
            compare = gen_comp_df(*op0, sx_compare_op0, sx_compare_op1);
          else
            gcc_assert(FALSE);
        }
      
      emit_insn(compare);

#if 0
      /* EF: branching condition *MUST* be signed ! */
      /* JM: no, it *MUST* not be signed, it *MUST* be
         the same condition as the one we used to
         select the CC mode ... see explanation at
         http://code.google.com/p/sx-gcc/issues/detail?id=109 */
      if (uns)
        *code = signed_condition(*code);
#endif
    }
}

/*
 * operands[0] will be the branch target.
 * sx_compare_op0 and sx_compare_op1 are the things to be 
 * compared.
 *
 * This routine has to generate a compare and a branch rtx.
 */
void
sx_conditional_branch (rtx *operands, enum rtx_code code)
{
  rtx op0, op1, condition;
  bool invert = false;

#if 0
  fprintf(stderr,"cbranch : %s\n", GET_RTX_NAME(code));
  print_inline_rtx(stderr, sx_compare_op0, 0);
  print_inline_rtx(stderr, sx_compare_op1, 0);
  fprintf(stderr,"\n");
#endif

  sx_emit_compare(&code, &op0, &op1, &invert);
  condition = gen_rtx_fmt_ee(code, VOIDmode, op0, op1);

  /* EF: keeping the inversion logic for the time we will want to add static
     branch prediction */
  if (!invert)
    emit_jump_insn(gen_condjump(condition, operands[0]));
  else
    emit_jump_insn(gen_inv_condjump(condition, operands[0]));

  sx_compare_op0 = sx_compare_op1 = NULL;
  sx_compare_mode = VOIDmode;
}

/* Emit the common code for doing conditional branches.
   operand[0] is the label to jump to.
   The comparison operands are saved away by cmp{si,di,sf,df}.  */

void
print_operand(FILE *stream, rtx x, int letter)
{
  enum rtx_code code;

  if(letter == '%') 
    {
      putc('%',stream);
      return;
    }
  if(!x)
    {
      error("PRINT_OPERAND x==NULL");
      return;
    }
  code = GET_CODE(x);

  if(letter == 'C')
    { /* conditional */
      switch(code)
        {
        case EQ:        fputs("e",stream); break;
        case NE:        fputs("ne",stream); break;
        case GT:        fputs("h",stream); break;
        case GE:        fputs("he",stream); break;
        case LT:        fputs("l",stream); break;
        case LE:        fputs("le",stream); break;
        case GTU:       fputs("h",stream); break;
        case GEU:       fputs("he",stream); break;
        case LTU:       fputs("l",stream); break;
        case LEU:       fputs("le",stream); break;
          
        default:
          abort_with_insn (x, "PRINT_OPERAND, illegal insn for %%C");
        }
    }
  else if(letter == 'N')
    { /* reverse conditional */
      switch(code)
        {
        case EQ:        fputs("ne",stream); break;
        case NE:        fputs("e",stream); break;
        case GT:        fputs("le",stream); break;
        case GE:        fputs("l",stream); break;
        case LT:        fputs("he",stream); break;
        case LE:        fputs("h",stream); break;
        case GTU:       fputs("le",stream); break;
        case GEU:       fputs("l",stream); break;
        case LTU:       fputs("he",stream); break;
        case LEU:       fputs("h",stream); break;
          
        default:
          abort_with_insn (x, "PRINT_OPERAND, illegal insn for %%N");
        }
    }
  /* for sx */
  else if(letter == 'M') /* special (n)1 or (n)0 notation */
    {
      unsigned long long iv = INTVAL(x);
      int m, n;

      if (iv && (iv & (iv + 1)) == 0)
        n = 0;
      else
        n = 1;

      for (m = 64;
           (iv & 1) == (unsigned long long)(1 - n) && m > 0;
           iv  >>= 1)
        m--;  
      fprintf(stream,"(%d)%d", m, n);
    }
  /* end for sx */

  else if(letter == 'H') /* print upper 32 bits. */
    { 
      HOST_WIDE_INT iv = (INTVAL(x) >> 32) & 0xffffffff;

      /* if (iv > 0x7fffffff) iv |= ((-1) ^ 0xffffffff); */
      fprintf(stream, "%d", (int)iv);
    }
  else if(letter == 'L') /* print lower 32 bits. */
    { 
      HOST_WIDE_INT iv = INTVAL(x) & 0xffffffff;

      /* if (iv > 0x7fffffff) iv |= ((-1) ^ 0xffffffff); */
      fprintf(stream, "%d", (int)iv);
    }
  else if(letter == 'S') /* print SYMBOL_REF */
    {
      assemble_name(stream,XSTR(XEXP(x,0),0));
    }
  else if(code == REG)
    {
      int regnum = REGNO(x);
      fputs("$",stream);
      fputs(reg_names[regnum],stream);
    }
  else if(letter == 'U') /* Unsigned int */
    {
      unsigned long long iv = (unsigned long long)(INTVAL(x));
      fprintf(stream, "%lld", iv);
    }
  else if(letter == 'G') /* siGned int */
    {
      long long iv;

      if (code == CONST_INT)
        {
          iv = INTVAL(x);
        }
      else if (code == CONST_DOUBLE && fp_I_operand(x, GET_MODE(x)))
        {
          REAL_VALUE_TYPE rv;

          REAL_VALUE_FROM_CONST_DOUBLE(rv, x);
          iv = real_to_integer(&rv);
        }
      else
        {
          gcc_unreachable();
          iv = 0;
        }
      fprintf(stream, "%lld", iv);
    }
  else if(code == CONST_INT)
    {
      fprintf(stream, HOST_WIDE_INT_PRINT_DEC, INTVAL(x));
    }
  else if(letter == 'F')
    {
      REAL_VALUE_TYPE rv;

      gcc_assert(code == CONST_DOUBLE);
      
      REAL_VALUE_FROM_CONST_DOUBLE(rv, x);

      if(GET_MODE (x) == SFmode)
        {
          long raw = 0;

          REAL_VALUE_TO_TARGET_SINGLE(rv, raw);

          fprintf(stream, "0x%lx", raw);
        }
      else
        {
          /* we don't want to convert DF and TFmode stuff;
             fp_lea_I_operand predicate should prevent us
             reaching this anyway, but just in case ... */
          gcc_unreachable();
        }
    }
  else if (GET_CODE (x) == MEM && GET_CODE (XEXP (x,0)) == MEM)
    {
      output_address (XEXP (XEXP (x,0), 0));
    }
  else if(code == MEM)
    {
       output_address(XEXP(x, 0));
    }
  else
    {
      output_addr_const(stream,x);
    }
}

void
print_operand_address (FILE *file, rtx addr)
{
  if (!addr)
    error ("PRINT_OPERAND_ADDRESS, null pointer");
  else
    {
    switch (GET_CODE (addr))
      {
      default:
        abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, invalid insn #1");
        break;

      case REG:
        if (REGNO (addr) == ARG_POINTER_REGNUM)
          abort_with_insn (addr, "Arg pointer not eliminated.");

        fprintf (file, "0(,$%s)", reg_names [REGNO (addr)]);
        break;

      case PLUS:
        {
          register rtx reg    = (rtx)0;
          register rtx offset = (rtx)0;
          register rtx arg0   = XEXP (addr, 0);
          register rtx arg1   = XEXP (addr, 1);

          if (GET_CODE (arg0) == REG)
            {
              reg = arg0;
              offset = arg1;
              if (GET_CODE (offset) == REG)
                abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, 2 regs");
            }
          else if (GET_CODE (arg1) == REG)
            {
              reg = arg1;
              offset = arg0;
            }
          else if (CONSTANT_P (arg0) && CONSTANT_P (arg1))
            {
              output_addr_const (file, addr);
              break;
            }
          else
            abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, no regs");

          if (!CONSTANT_P (offset))
            abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, invalid insn #2");

	  
          output_addr_const (file, offset);
          fprintf (file, "(,$%s)", reg_names [REGNO (reg)]);
        }
        break;

      case LABEL_REF:
      case SYMBOL_REF:
      case CONST_INT:
      case CONST:
        output_addr_const (file, addr);
        break;
      }
   }
}


const char *
sx_asm_output_call(rtx *operands, int sibcall)
{
  char buf[100];
  int arg_offset = STACK_POINTER_OFFSET - 8;

  /* put arg count in scratch reg */
  sprintf(buf, "lea\t%%2," HOST_WIDE_INT_PRINT_DEC,
          INTVAL(operands[1])/UNITS_PER_WORD);
  output_asm_insn(buf, operands);
  if(!sibcall)
    {
      /* normal call, use our own argument space */
      sprintf(buf,"sts\t%%2,%d(,$s2)", arg_offset);
      output_asm_insn(buf, operands);
      sprintf(buf,"lea\t$s34,%d(,$s2)", arg_offset);
      output_asm_insn(buf, operands);
    }
  else
    {
      /* tail call: use our caller's argument space.! */
      output_asm_insn("sts\t%2,0(,$s3)", operands);
      output_asm_insn("or\t$s34,0,$s3", operands);
    }
  if (REGNO(operands[0]) != 33)
    output_asm_insn("or\t$s33,0,%0", operands);
  if(!sibcall)
    {
      /* regular call, store return address to s32 using bsic */
      output_asm_insn("bsic\t$s32,($s33)", operands);
    }
  else
    {
      /* tail call; assume return address is already in s32 as we will
         return to our caller's caller, so just do an unconditional jump.
         first, we need to load arg and frame ptrs from RSA, as the
         epilogue of a tail-calling function did not load them,
         since we needed them here in order to access our caller's
         argument space. */
      output_asm_insn("lds\t$s3,16(,$s1)", operands);
      output_asm_insn("lds\t$s1,0(,$s1)", operands);
      output_asm_insn("b\t(,$s33)", operands);
    }

  return "";
}

static char* sx_data_section_top = NULL;
static char* sx_linkage_section_top = NULL;
static char* sx_bss_section_top = NULL;

static void
sx_clear_sections(void)
{
   sx_data_section_top = NULL;
   sx_linkage_section_top = NULL;
   if (sx_bss_section_top)
     {
       free(sx_bss_section_top);
       sx_bss_section_top = NULL;
     }
}

static void
sx_remember_data_section(char *name)
{
   if (!sx_data_section_top)
     sx_data_section_top = name;
}

static void
sx_remember_linkage_section(char *name)
{
   if (!sx_linkage_section_top)
     sx_linkage_section_top = name;
}   

static int sx_bss_label = 0;

static void
sx_remember_bss_section(FILE *stream)
{
  if (!sx_bss_section_top)
    {
      char *name;

      name = (char *)xmalloc(32);
      sprintf(name, ".L.bss.%d", sx_bss_label++);
      sx_bss_section_top = name;

      fprintf(stream, "%s:\n", sx_bss_section_top);
    }
}

/*
void
sx_asm_init_sections(void)
{
  fprintf(stderr, "sx_asm_init_sections\n");
}
#undef TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS sx_asm_init_sections
*/

static const char *
sx_address_conversion(char *opcode, rtx op1, rtx op2)
{
   rtx reg = NULL;
   rtx sym = NULL;
   rtx offset = NULL;
   rtx opp1, opp2;
   rtx operands[2];

   operands[0] = op1;
   operands[1] = op2;

#ifdef SXDEBUG
   fprintf(stderr, "%s: opcode=%s op1 = ", __FUNCTION__, opcode);
   print_inline_rtx(stderr, op1, 1);
   fprintf(stderr, "\n op2 = ");
   print_inline_rtx(stderr, op2, 1);
   fprintf(stderr, "\n");
#endif
   if (GET_CODE(op1) == MEM)
      op1 = XEXP(op1,0);

   if (GET_CODE(op1) == CONST)
      op1 = XEXP(op1,0);

   switch GET_CODE(op2)
     {
     case REG:
     case CONST_INT:
       break;
     case SYMBOL_REF:
       sym = op2;
       break;

     case PLUS:
       opp1 = XEXP(op2,0);
       opp2 = XEXP(op2,1);
       if (GET_CODE (opp1) != SYMBOL_REF) 
         {
           opp1 = XEXP(op1,1);
           opp2 = XEXP(op1,0);
         }
       if (GET_CODE (opp1) != SYMBOL_REF)
         break;

       sym = opp1;
       switch GET_CODE(opp2)
         {
         case REG:
           reg = opp2;
           break;
         case CONST_INT:
           offset = opp2;
           break;
         default:
           abort_with_insn (opp2,
                            "SX_ADDRESS_CONVERSION, invalid PLUS operand");
         }
       break;
     default:
       abort_with_insn (op2, "SX_ADDRESS_CONVERSION, invalid operand");
     }   
   
   if (sym == NULL)
     {
       output_asm_insn (opcode, operands);
       return "";
     }
   return "";
}

#define TARGET_BUGGY_QP_LIB 0
 
static void
emit_soft_tfmode_libcall (const char *func_name, int nargs, rtx *operands)
{
  rtx ret_slot = NULL, arg[3], func_sym;
  int i;

#ifdef SXDEBUG
  fprintf(stderr, "%s: func=%s\n", __FUNCTION__, func_name);
  for (i = 0; i < nargs; i++)
    {
      fprintf(stderr, "   op%d = ", i);
      print_inline_rtx(stderr, operands[i], 1);
      fprintf(stderr, "\n");
    }
#endif
  /* We only expect to be called for conversions, unary, and binary ops.  */
  gcc_assert (nargs == 2 || nargs == 3);

  for (i = 0; i < nargs; ++i)
    {
      rtx this_arg = operands[i];
      rtx this_slot;

      /* TFmode arguments and return values are passed by reference.  */
      if (GET_MODE (this_arg) == TFmode)
        {
          int force_stack_temp;

          force_stack_temp = 0;
          if (TARGET_BUGGY_QP_LIB && i == 0)
            force_stack_temp = 1;

          if (GET_CODE (this_arg) == MEM
              && ! force_stack_temp) {
            this_arg = XEXP (this_arg, 0);

            DBG_RTX2("1", "this_slot", this_slot, "this_arg", this_arg);
          }
          else if (CONSTANT_P (this_arg)
                   && ! force_stack_temp)
            {
              this_slot = force_const_mem (TFmode, this_arg);
              this_arg = XEXP (this_slot, 0);
              
              DBG_RTX2("2", "this_slot", this_slot, "this_arg", this_arg);
            }
          else
            {
              this_slot = assign_stack_temp (TFmode, GET_MODE_SIZE (TFmode), 0);
              
              /* Operand 0 is the return value.  We'll copy it out later.  */
              if (i > 0)
                emit_move_insn (this_slot, this_arg);
              else
                ret_slot = this_slot;
              
              this_arg = XEXP (this_slot, 0);
              
              DBG_RTX2("3", "this_slot", this_slot, "this_arg", this_arg);
            }
        }

      arg[i] = this_arg;
    }

  func_sym = gen_rtx_SYMBOL_REF (Pmode, func_name);

  if (GET_MODE (operands[0]) == TFmode)
    {
      if (nargs == 2)
        emit_library_call (func_sym, LCT_NORMAL, VOIDmode, 2,
                           arg[0], GET_MODE (arg[0]),
                           arg[1], GET_MODE (arg[1]));
      else
        emit_library_call (func_sym, LCT_NORMAL, VOIDmode, 3,
                           arg[0], GET_MODE (arg[0]),
                           arg[1], GET_MODE (arg[1]),
                           arg[2], GET_MODE (arg[2]));
      
      if (ret_slot)
        emit_move_insn (operands[0], ret_slot);

      DBG_RTX2("4", "op0", operands[0], "ret_slot", ret_slot);
    }
  else
    {
      rtx ret;

      gcc_assert (nargs == 2);

      ret = emit_library_call_value (func_sym, operands[0], LCT_NORMAL,
                                     GET_MODE (operands[0]), 1,
                                     arg[1], GET_MODE (arg[1]));

      if (ret != operands[0])
        emit_move_insn (operands[0], ret);

      DBG_RTX2("5", "op0", operands[0], "ret", ret);
    }
}

/* two variants of ascii output: _as_bytes() breaks some dejagnu tests
   that scan assembler for string constants (gcc.dg/const-elim-2.c)
   _as_str() is the preferred way to go, therefore */

void
asm_output_ascii_as_str(FILE *stream, const char *ptr, int len)
{
  int i,p;
  unsigned int c;
  fprintf(stream, "%s\t\"", ASM_STRING_OP);
  i = 0;
  p = 0;
  do 
    { 
      c = ptr[i++];
      if(c < 0x80 && isprint(c))
        {
          switch(c)
            {
            case '\"': fputs("\\\"", stream); p += 2; break;
            case '\'': fputs("\\\'", stream); p += 2; break;
            case '\\': fputs("\\\\", stream); p += 2; break;
            default:
              fputc(c, stream);
              p++;
            }
        }
      else
        {
          switch(c)
            {
            case '\b': fputs("\\b", stream); p += 2; break;
            case '\f': fputs("\\f", stream); p += 2; break;
            case '\n': fputs("\\n", stream); p += 2; break;
            case '\r': fputs("\\r", stream); p += 2; break;
            case '\t': fputs("\\t", stream); p += 2; break;
            case '\v': fputs("\\v", stream); p += 2; break;
            default:
              /* only output lower 8-bits; higher ones will
                 be set to 1 in case of wchar_t */
              fprintf(stream, "\\%03o", (c & 0xFF)); 
              p += 4; 
              break;
            }
        }
      if(i < len) 
        {
          if(p >= 50)
            {
              fprintf(stream, "\"\n%s\t\"", ASM_STRING_OP);
              p = 0;
            }
        }
    } while(i < len); 
  fputs("\"\n", stream);
}

void
asm_output_ascii_as_bytes(FILE *stream, const char *ptr, int len)
{ 
  int i,p;
  unsigned int c;
  fprintf(stream, "%s", ASM_BYTE_OP);
  i = 0;
  p = 0;
  do 
    { 
      c = ptr[i++];
      if(c < 0x80 && isprint(c))
        {
          switch(c) {
          case '\'': fputs("\'\\\'",stream); p+=3; break;
          case '\\': fputs("\'\\\\",stream); p+=3; break;
          default:
            fputs("\'",stream); 
            fputc(c,stream);
            p += 2;
          }
        }
      else
        {
          switch(c)
            {
            case '\b': fputs("\'\\b",stream); p+=3; break;
            case '\f': fputs("\'\\f",stream); p+=3; break;
            case '\n': fputs("\'\\n",stream); p+=3; break;
            case '\r': fputs("\'\\r",stream); p+=3; break;
            case '\t': fputs("\'\\t",stream); p+=3; break;
            case '\v': fputs("\'\\v",stream); p+=3; break;
            default:
              /* only output lower 8-bits; higher ones will
                 be set to 1 in case of wchar_t */
              fprintf(stream,"0x%02x", (c & 0xFF)); 
              p+=4; 
              break;
            }
        }
      if(i < len)
        {
        if(p >= 50)
          {
            fprintf(stream, "\n%s", ASM_BYTE_OP);
            p = 0;
          }
        else
          {
            fputs(",", stream);
            p++;
          }
        }
    } while(i < len); 
  fputs("\n",stream);
}

#define ARGS_COUNT_SLOT_SIZE 8

/*
 * Distance between frame pointer and stack pointer after the function
 * prologue. Input argument is size needed for local (automatic) variables.
 */
int
sx_compute_frame_size(int localvars_size)
{
  int local_space;

  local_space = localvars_size + current_function_outgoing_args_size +
    ARGS_COUNT_SLOT_SIZE;
  /* make local space a multiple of 16 bytes (why, exactly?) */
  local_space = (local_space + 15)&(~15);
  return local_space + 2 * SX_RSA_SIZE;
}

void 
asm_output_function_prefix(FILE *file, const char *efnname) 
{
  int fnlen;
  const char *fnname = targetm.strip_name_encoding(efnname);

  /* making of linkage section */
  fprintf(file, ".LK.%s:\n", fnname);
  fprintf(file, "\tusing\t.LK.%s,$s4\n", fnname);
  fprintf(file, "%s0xffffffffffffffff\n", ASM_LONGLONG);
  fprintf(file, "%s.CD.%s-.LK.%s\n", ASM_LONG, fnname, fnname);
  fprintf(file, "%s27\n", ASM_LONG);
  fprintf(file, "%s0\n", ASM_LONGLONG);
  fprintf(file, "%s0\n", ASM_LONGLONG);
  fprintf(file, "%s7,0\n", ASM_RLONG);

  fnlen = strlen(fnname);
  fprintf(file, "%s%d\n", ASM_LONG, (fnlen>32)?32:fnlen);
  fprintf(file, "%s\"%.32s\"\n", ASM_STRING_OP, fnname);
  if (fnlen < 32)
    {
      fprintf(file, "%s%d,0x20\n", ASM_RBYTE, 32-fnlen);
    }

  if (sx_data_section_top)
    {
      fprintf(file, ".IL.%s:\n", fnname);
      fprintf(file, "%s%s\n", ASM_LONGLONG, sx_data_section_top);
    }
  if (sx_bss_section_top)
    {
      fprintf(file, ".UL.%s:\n", fnname);
      fprintf(file,"%s%s\n", ASM_LONGLONG, sx_data_section_top);
    }

#ifdef SX_ENABLE_PROFILER
  /* if profiling is required, output address of _mcount here */
  /* TODO: conditionally output this, what flag tells us if
     profiling is required? */
    {
      fprintf(file, ".PF.MC.%s:\n", fnname);
      fprintf(file, "\tglobal\t_mcount\n");
      fprintf(file, "\tadcn\t_mcount\n");
    }
#endif /* SX_ENABLE_PROFILER */

  ASM_OUTPUT_ALIGN(file, 7); /* 128 */
  fprintf(file, ".CD.%s:\n", fnname);

  /* Code for stack extension */
  /* NOTE: with RTLized prologue, we do not use this anymore, as we need to
     put this part in the prologue itself; it's kept in the prefix just to
     keep it compliant with what the SuperUX compiler produces */
  if(sx_stack_check)
    {
      fputs("\tnop\n", file);
      fputs("\tmonc\t7,0,1\n", file);
      fputs("\tbe>\t0,48(,$s33)\n", file);
    }

  fprintf(file, "%s.LK.%s\n", ASM_LONGLONG, fnname);
}

int
log_of_two(int x)
{
  int y;

  x >>= 1;
  for(y = 0; x; x >>= 1, y++) ;

  return y;
}

#ifndef SX_RTL_PRO_EPI_LOGUE

/* assembler pro- and epi-logue. currently used. */

static void
asm_function_prologue (FILE *file, int size)
{
  int local_space;
  int i;
  int n_regs_to_save;
  const char *fnname = XSTR(XEXP(DECL_RTL(current_function_decl), 0), 0);
  int is_main = 0;
  char *name = targetm.strip_name_encoding(fnname);

  if(!strcmp("main",name))
    is_main = 1;

  /* always save first 8 registers */
  n_regs_to_save = REG_ALWAYS_SAVE ;

  for(i = REG_ALWAYS_SAVE+1 ; i <= LAST_S_REG ; i++)
    {
      if(REG_NEEDS_SAVE(i)) {
        n_regs_to_save = i;
      }
    }
  if (n_regs_to_save > 32)
    error("saving more than 32 registers in function prologue!");
  /* align 16 */

  /* add max function paramlist size*/
  fprintf(file,
          "# Function '%s; local:%d bytes, param:%d bytes, save regs:%d, "
          "pretend_args_size:%d bytes.\n",
          name,size,current_function_outgoing_args_size+8,n_regs_to_save,
          current_function_pretend_args_size);

  local_space = size + current_function_outgoing_args_size+8;
  local_space = (local_space + 15)&(~15);

  fprintf(file,"\tstm\t$s1,%d,-%d(,$s2)\n",n_regs_to_save,SX_RSA_SIZE);
  fprintf(file,"\tlea\t$s1,-%d(,$s2)\n",SX_RSA_SIZE);
  fprintf(file,"\tlea\t$s2,%d(,$s2)\n",local_space+SX_RSA_SIZE);
  fprintf(file,"\tlea\t$s4,.LK.%s-%s(,$s33)\n", name, name);
  fputs("\tor\t$s3,0,$s34\n",file);
  if(sx_stack_check)
    {
      fputs("\tsub\t$s8,$s2,$s0\n",file);
      fputs("\tbh\t$s8,-20(,$s33)\n",file);
    }
  if (n_regs_to_save < 32)
    {
      fputs("\tsts\t$s32,248(,$s1)\n",file);
    }
  if (sx_data_section_top != NULL)
    { 
      fprintf(file, "\tlds\t$s5,.IL.%s-.LK.%s(,$s4)\n", name, name);
    }
  if (sx_bss_section_top != NULL)
    {
      fprintf(file, "\tlds\t$s6,.UL.%s-.LK.%s(,$s4)\n", name, name);
    }
  fprintf(file, "# -- function_prologue end (%s)\n", name);
}

static void
asm_function_epilogue(FILE *file, int size ATTRIBUTE_UNUSED)
{
  int i;
  const char *name = XSTR(XEXP(DECL_RTL(current_function_decl), 0), 0);
  int is_main = 0;

  fprintf(file, "# -- function_epilogue: %s\n", name);

  if (!strcmp("main",name))
    is_main = 1;

  fputs("\tlds\t$s32,248(,$s1)\n", file);
  for (i = 31 ; i > 0 ; i --)
    {
      if (REG_NEEDS_SAVE(i)|| i<= REG_ALWAYS_SAVE ) 
        {
          fprintf(file,"\tlds\t$s%d,%d(,$s1)\n",i,(i-1)*8);
        }
    }
  fputs("\tbe>\t0,(,$s32)\n", file);
  ASM_OUTPUT_ALIGN(file, 7); /* 128 */
  fprintf(file,"# end Function '%s'\n", name);

  sx_clear_sections();
}

#else

/* RTLized pro- and epi-logue: not finished yet and currently not used;
   need to uncomment prologue and epilogue expands in md file to use
   these and #define SX_RTL_PRO_EPI_LOGUE in t-sx. */

void
sx_expand_prologue(void)
{
  int size = get_frame_size();
  int local_space;
  int i;
  int n_regs_to_save;
  const char *fnname = XSTR(XEXP(DECL_RTL(current_function_decl), 0), 0);
  int is_main = 0;
  rtx cond, sym1, sym2, insn;
  char buf[1024 + 1];
  const char *name = targetm.strip_name_encoding(fnname), *lname;

  if(!strcmp("main",name))
    is_main = 1;

  /* always save first 8 registers */
  n_regs_to_save = REG_ALWAYS_SAVE ;

  for(i = REG_ALWAYS_SAVE + 1 ; i <= LAST_S_REG ; i++)
    {
      if(REG_NEEDS_SAVE(i))
        {
          n_regs_to_save = i;
        }
    }
  if (n_regs_to_save > 32)
    error("saving more than 32 registers in function prologue!");

  /* add max function paramlist size
  fprintf(file,
          "# Function '%s; local:%d bytes, param:%d bytes, save regs:%d, "
          "pretend_args_size:%d bytes.\n",
          name, size, current_function_outgoing_args_size + 8, n_regs_to_save,
          current_function_pretend_args_size);
  */

  local_space = size + current_function_outgoing_args_size + 8;
  local_space = (local_space + 15)&(~15);

  /* fprintf(file,"\tstm\t$s1,%d,-%d(,$s2)\n",n_regs_to_save,SX_RSA_SIZE); */
  emit_insn(gen_store_multiple
            (gen_rtx_MEM(BLKmode, plus_constant(stack_pointer_rtx, -SX_RSA_SIZE)),
             frame_pointer_rtx,
             GEN_INT(n_regs_to_save)));

  /* fprintf(file,"\tlea\t$s1,-%d(,$s2)\n",SX_RSA_SIZE); */
  emit_insn(gen_rtx_SET(VOIDmode,
                        frame_pointer_rtx,
                        plus_constant(stack_pointer_rtx, -SX_RSA_SIZE)));

  /* fprintf(file,"\tlea\t$s2,%d(,$s2)\n",local_space+SX_RSA_SIZE); */
  emit_insn(gen_rtx_SET(VOIDmode,
                        stack_pointer_rtx,
                        plus_constant(stack_pointer_rtx,
                                      local_space + SX_RSA_SIZE)));

  /* fprintf(file,"\tlea\t$s4,.LK.%s-%s(,$s33)\n",name,name); */
  /* FIXME: we will die a miserable death if a function name is over
     1020 chars long; can probably happen with some C++ ;) */
  gcc_assert(strlen(name) < 1020);
  snprintf(buf, 1024, ".LK.%s", name);
  lname = ggc_strdup(buf);
  sym1 = gen_rtx_SYMBOL_REF(Pmode, lname);
  sym2 = gen_rtx_SYMBOL_REF(Pmode, name);
  insn =
    emit_insn(gen_rtx_SET(VOIDmode,
                          gen_rtx_REG(DImode, 4),
                          gen_rtx_PLUS(DImode,
                                       gen_rtx_REG(DImode, 33), 
                                       gen_rtx_MINUS(DImode, sym1, sym2))));
  /* if the function won't load anything via implicit base register, it's
     safe for the optimizer to remove it, thus flag it with MAYBE_DEAD */
  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD,
                                        NULL_RTX,
                                        REG_NOTES (insn));

  /* fputs("\tor\t$s3,0,$s34\n",file); */
  emit_insn(gen_rtx_SET(VOIDmode, arg_pointer_rtx, gen_rtx_REG(DImode, 34)));

  /* fputs("\tsub\t$s8,$s2,$s0\n",file); */
  if(sx_stack_check)
    {
      rtx reg = gen_rtx_REG(CCUDImode, 8);
      rtx lbl = gen_label_rtx();

      insn = emit_insn(gen_comp_udi(reg,
				    stack_pointer_rtx,
				    gen_rtx_REG(DImode, 0)));
      /* fputs("\tbh\t$s8,-20(,$s33)\n",file); */
      cond = gen_rtx_fmt_ee(LE, DImode,
			    reg, const0_rtx);
      insn = emit_jump_insn(gen_condjump(cond, lbl));
      
      emit_insn(gen_monc(gen_rtx_raw_CONST_INT (DImode, 7),
			 const0_rtx,
			 const1_rtx));
      
      emit_label(lbl);

      JUMP_LABEL(insn) = lbl;
    }
  
  if (n_regs_to_save < 32) {
    /* fputs("\tsts\t$s32,248(,$s1)\n",file); */
    insn = emit_insn(gen_move_insn
                     (gen_rtx_MEM(DImode,
                                  plus_constant(frame_pointer_rtx, 248)),
                      gen_rtx_REG(DImode, 32)));
    REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD,
                                          NULL_RTX,
                                          REG_NOTES (insn));
  }
  if (sx_data_section_top != NULL) { 
    /* fprintf(file,"\tlds\t$s5,.IL.%s-.LK.%s(,$s4)\n",name,name); */
    /* TODO */
  }
  if (sx_bss_section_top != NULL) {
    /* fprintf(file,"\tlds\t$s6,.UL.%s-.LK.%s(,$s4)\n",name,name); */
    /* TODO */
  }
  /* fprintf(file, "# -- function_prologue end (%s)\n", name); */
}

void
sx_expand_epilogue(int sibcall)
{
  int i;
  const char *name = XSTR(XEXP(DECL_RTL(current_function_decl), 0), 0);
  int is_main = 0;

  if(!strcmp("main",name))
    is_main = 1;

  /* load return address from RSA to s32 */
  emit_insn(gen_move_insn(gen_rtx_REG(DImode, 32),
                          gen_rtx_MEM(DImode,
                                      plus_constant(frame_pointer_rtx, 248))));
  /* restore register from RSA */
  for(i = 31 ; i > 0 ; i --)
    {
      if(sibcall && ((i == 3) || (i == 1)))
        {
          /* if this is an epilogue for a function that will make a tail
             call, we need to prevent restoring arg and frame pointers at
             this time as the tail call will need them (arg ptr to put arg
             count for call on stack, and frame ptr to load first the arg
             ptr and finally itself from RSA. in this case, we will restore s1
             and s3 as part of the tail call code itself. */
          continue;
        }
      if(REG_NEEDS_SAVE(i) || i <= REG_ALWAYS_SAVE ) 
        {
          /* restore register i from RSA */
          emit_insn(gen_move_insn(gen_rtx_REG(DImode, i),
                                  gen_rtx_MEM(DImode,
                                              plus_constant(frame_pointer_rtx,
                                                            (i - 1)*8))));
        }
    }

  if(!sibcall)
    {
      /* we only want to emit final jump back to caller if the function
         does not end with a tail call ... */
      emit_jump_insn(gen_function_return());
    }

  sx_clear_sections();
}

#endif /* !SX_RTL_PRO_EPI_LOGUE */

int
sx_handle_option(size_t code, const char *arg, int value)
{
  /* just check if option vals are correct */
  switch(code)
    {
    case OPT_mfloat:
      return (value >= 0 && value <= 2);
    case OPT_mmemlayout_:
      return (0 == strcmp(arg, "32G") || 0 == strcmp(arg, "512G"));
    default:
      return 1;
    }
}

static section *
sx_select_rtx_section (enum machine_mode mode ATTRIBUTE_UNUSED,
                       rtx x ATTRIBUTE_UNUSED,
                       unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED)
{
  return function_section (current_function_decl);
}

void
sx_named_section (const char *name, unsigned int flags,
                  unsigned int align ATTRIBUTE_UNUSED)
{
  char sflagsbuf[16], *sflags = sflagsbuf;

  if(flags & SECTION_CODE)
    *sflags++ = 'x';
  else if(flags & SECTION_BSS)
    *sflags++ = 'b';
  if(flags & SECTION_WRITE)
    *sflags++ = 'w';
  else
    *sflags++ = 'r';    
  *sflags = '\0';

  fprintf (asm_out_file, "\tsection\t%s, \"%s\"\n",
	   name, sflags);  
}

static rtx
sx_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED,
                     int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG(Pmode, 123);
}

static bool
sx_function_ok_for_sibcall (tree decl, tree exp)
{
  /* currently, we don't really need to enforce much here: any function
     is ok for a sibcall. might need to revise this when we allow reg
     params, etc. */
  return true;
}

void
sx_dump_rtx(const char *pfx, rtx x)
{
  fprintf(stderr, "%s: ", pfx);
  print_inline_rtx(stderr, x, 0);
  fprintf(stderr, "\n\n");
}

/* Initialize the GCC target structure.  */
#undef  TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP ASM_BYTE_OP
#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP ASM_SHORT
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP ASM_LONG
#undef  TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP ASM_LONGLONG

#ifndef SX_RTL_PRO_EPI_LOGUE 
#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE asm_function_prologue
#undef  TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE asm_function_epilogue
#endif /* !SX_RTL_PRO_EPI_LOGUE */

#undef  TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE 1

#undef  TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO sx_encode_section_info

#undef  TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION  sx_handle_option

#undef  TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX sx_struct_value_rtx

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL sx_function_ok_for_sibcall

/* we want function constant pools in the *same* section
   as the function even when flag_pic is true (which is not
   the case with default rtx section selection) */
#undef  TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION sx_select_rtx_section

struct gcc_target targetm = TARGET_INITIALIZER;

#if 0
#include <execinfo.h>

/* shows stack frame. Needs to be linked with -rdynamic */
static void
show_stackframe(void)
{
  void *trace[16];
  char **messages = (char **)NULL;
  int i, trace_size = 0;

  trace_size = backtrace(trace, 16);
  messages = backtrace_symbols(trace, trace_size);
  printf("[bt] Execution path:\n");
  for (i=0; i<trace_size; ++i)
    printf("[bt] %s\n", messages[i]);
}
#endif

void 
sx_function_profiler(FILE *file, int labelno)
{
#ifdef SX_ENABLE_PROFILER
  fprintf(file, "# profiling start: call _mcount()\n");

  /* load counter */
  fprintf(file, "\tlds\t$s34,LP%d\n", labelno);
  fprintf(file, "\tlea\t$s34,0($s30,$s34)\n");
  fprintf(file, "\tor\t$s34,$s34,(1)1\n");
  /* jump to _mcount */
  fprintf(file, "\tlds\t$s33,%s\n", NULL /* TODO: symbol name for _mcount address */);
  fprintf(file, "\tbsic\t$s32,0($s33)\n");

  fprintf(file, "# profiling end\n");
#endif
}

/*
;;- Local variables:
;;- mode:c
;;- eval: (c-set-style "gnu")
;;- End:
*/
