/*
   Definitions of target machine for GNU compiler.  SX version.

   Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2007 Free Software Foundation, Inc.
   Contributed by A. Lichnewsky (lich@inria.inria.fr).
   Changed by Michael Meissner  (meissner@osf.org).
   64 bit r4000 support by Ian Lance Taylor (ian@cygnus.com) and
   Brendan Eich (brendan@microunity.com).

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

   $Id: sx.h 250 2009-02-26 13:44:23Z jaka $
*/


#include "hwint.h"

/*
 * Building for SuperUX or Linux?
 */
#define TARGET_SUPERUX 1

#define TARGET_VERSION fprintf(stderr," (NEC SX)");

/* This fixes an issue with builtin-convert-2.c
   but is actually only valid with true glibc support.
*/
#define TARGET_C99_FUNCTIONS 1

/*
 *
 * Run-time Target
 *
 */

#define TARGET_CPU_CPP_BUILTINS()            \
  do                                         \
    {                                        \
      builtin_define ("__sx__");             \
      builtin_define ("__SX__");             \
      builtin_define ("_SX");                \
      if (TARGET_PROCESSOR_SX6)              \
        {                                    \
          builtin_define ("__sx6__");        \
          builtin_define ("__SX6__");        \
        }                                    \
      else if (TARGET_PROCESSOR_SX8)         \
        {                                    \
          builtin_define ("__sx8__");        \
          builtin_define ("__SX8__");        \
        }                                    \
      builtin_assert ("cpu=sx");             \
      builtin_assert ("machine=sx");         \
    }                                        \
  while (0)

/* __SUPERUX__ needs to be target dependent later ;-) */

/* The "_SIZE_T64" define solves the issue with SuperUX "malloc" receiving 
   operand of incorrect size (the operand is 32-bit, whereas SuperUX "malloc" 
   expects 64-bit operand). */ 
#define TARGET_OS_CPP_BUILTINS()                \
  do                                            \
    {                                           \
      builtin_define ("unix");                  \
      builtin_define ("_unix");                 \
      builtin_define ("__SUPERUX__");           \
      builtin_define ("_SIZE_T64");             \
      builtin_define ("_LONG64");               \
    }                                           \
  while (0)

/* SX GNU assembler command line spec */
#define ASM_SPEC \
  "%{msx4:-h sx4} %{msx5:-h sx5} %{msx6:-h sx6} %{msx8:-h sx8} \
   %{mfloat0:-h float0} %{mfloat1:-h float1} %{mfloat2:-h float2} \
   %{Wa,*:%*}"

#ifdef TARGET_SUPERUX

/* === building for SuperUX === */

/* name of initializer __main function invoked in a function called main()
   right after the prologue. The default is __main (glibc alike),
   for SuperUX compatibility we change this to _main */
#define NAME__MAIN "_main"

#define CPP_SPEC \
  "%{.c: -D_FLOAT0 -D_LONG64}"

/* link spec for GNU ld */
#define LINK_SPEC \
  "%{msmall:--sx-paging-mode=small} %{mlarge:--sx-paging-mode=large} \
   %{msx4:--sx-option=sx4} %{msx6:--sx-option=sx6} %{msx8:--sx-option=sx8} \
   %{mssize=*:--sx-stack-size=%*}					\
   %{mmemlayout=32G:--sx-layout=32G} %{mmemlayout=512G:--sx-layout=512G} \
   %{mfloat0:--sx-option=float0} %{mfloat1:--sx-option=float1}		\
   %{mfloat2:--sx-option=float2}					\
   %{Wl,*:%*}"

/* need to add -lcpp and -lc when it works */
#define LIB_SPEC \
  ""

/* NOTE: no need to add libg.a. */
/*
#define LIB_SPEC |
  "%{!g: }" "%{g:-lg}"
*/

#define STARTFILE_SPEC \
  "%{!p:crt3.o%s values-Xa.o%s} \
   %{p:mcrt3.o%s values-Xa.o%s}"

/* -lc doesn't work with sxld as it tries /usr/lib/libc.a  */
#define ENDFILE_SPEC  "libcpp.a%s libc.a%s crtn.o%s"

/* SuperUX provides POSIX compliant access/mkdir/fcntl(F_SETLKW); this
   improves profiling code */
#define TARGET_POSIX_IO 1

/* NOTE: need to link against libgcc for TF soft float support
   at the very least ... */
#define LIBGCC_SPEC "-lgcc"

/* we're using COFF for SuperUX */
#define OBJECT_FORMAT_COFF

/* SuperUX wants STDC defined to 0 in system headers, much like Solaris */
#define STDC_0_IN_SYSTEM_HEADERS 1

#endif /* TARGET_SUPERUX */


/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields. */
#define BITS_BIG_ENDIAN 1
	
/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is numbered.  */
#define WORDS_BIG_ENDIAN 1

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 8

#define MIN_UNITS_PER_WORD 4

#define REG_PARM_STACK_SPACE(FNDECL) 0

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 64

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 64

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
/* Stack pointer points to the top of the RSA of the called function,
 * the frame pointer is computed by decrementing it by 272. The frame
 * must be aligned to a 16 bytes boundary, i.e. the stack pointer must be,
 * too.
 */
/* EF: TODO: temporarilly setting stack alignment to 64 bits. When
   stack alignment is at 128 bits, the passed args for a function
   are increased by the value of the fillup, and number of arguments
   seems broken.*/
#define STACK_BOUNDARY 64

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 1024

#define BIGGEST_ALIGNMENT 128    /* long double can be 8 byte alignment */
#define BIGGEST_FIELD_ALIGNMENT 128

/* biggest alignment that can be used with __attribute__((aligned(NN)))
   on non-ELF systems. TODO: find right value for SX-COFF */
#define MAX_OFILE_ALIGNMENT (32768 * 8)

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* probably there's no need to align struct sizes to 64 bits or to anything
   but BITS_PER_UNIT (i.e. 8, which is the default).  */
/*
#define STRUCTURE_SIZE_BOUNDARY 64
*/

/* Set this non-zero if move instructions will actually fail to work
   when given unaligned data.*/
#define STRICT_ALIGNMENT 1

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 64
#define LONG_LONG_TYPE_SIZE 64
#define CHAR_TYPE_SIZE 8
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 128

#define WINT_TYPE "int"

/* This way we can do char operations with regular 
   register-register ops and not worry about negative
   chars, so it should be faster. */
#define DEFAULT_SIGNED_CHAR 0

/* size in bits of the largest integer machine mode that should
   actually be used */
#define MAX_FIXED_MODE_SIZE 64

/* For compatibility with sxcc, all structure and union return values
   must be in memory. So, we define 1. This results in slower code. */ 
#define DEFAULT_PCC_STRUCT_RETURN 1
	
/* A function address in a call instruction
 *    is a byte address (for indexing purposes)
 *       so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

#define Pmode DImode

/* LIBGCC2 modes and types */
/* ... not yet ... */


/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK 1

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE DImode

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/*
 * double constants loadable as immediates (for floats and DI)
 */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)			\
  ((C) == 'G' ? (GET_MODE (VALUE) == VOIDmode &&		\
                 sx_g_ok(CONST_DOUBLE_HIGH (VALUE),		\
                         CONST_DOUBLE_LOW (VALUE))) :   \
   (C) == 'H' ? (GET_MODE (VALUE) == VOIDmode &&		\
                 sx_h_ok(CONST_DOUBLE_HIGH (VALUE),		\
                         CONST_DOUBLE_LOW (VALUE))) : 0)

/*
 * Since all the registers are equivalent, 
 * a value can be reloaded in any register of the same
 * class.
 */

#define PREFERRED_RELOAD_CLASS(x,class) class

/*
#define PREDICATE_CODES                                         \
{ "signed_comparison_operator", {EQ, NE, LT, LE, GE, GT} },     \
{ "sym_ref_mem_operand", {MEM} },                               \
{ "reg_or_i_operand", {SUBREG, REG, CONST_INT} },               \
{ "reg_or_j_operand", {SUBREG, REG, CONST_INT} },               \
{ "reg_or_k_operand", {SUBREG, REG, CONST_INT} },               \
{ "reg_or_m_operand", {SUBREG, REG, CONST_INT} },               \
{ "reg_or_in_operand", {SUBREG, REG, CONST_INT} },
*/


/*
 *
 * Debugger Info
 *
 */
#define SDB_DEBUGGING_INFO 1
#define SX_SDB_BASIC_TYPES 1

/* ****************** later  */
/*
#define ASM_OUTPUT_REG_PUSH(stream,regno)       \
{ if(regno <= LAST_S_REG) fprintf(stream,"\tsw\t-4(r29),r%d\n",regno);  \
  else fprintf(stream,"\tstores\t-4(r29),f%d\n",regno);                       \
  fputs("\taddi\tr29,r29,#-4\n",stream); }
*/

/* here was a 'stores' which i would call wrong!!! */
/* for testing only -4(r29) I replaced by (r29) */

/* ****************** later  */
/*
#define ASM_OUTPUT_REG_POP(stream,regno)        \
{ if(regno <= LAST_S_REG) fprintf(stream,"\tlw\tr%d,(r29)\n",regno);  \
  else fprintf(stream,"\tloads\tr%d,(r29)\n",regno);                       \
  fputs("\taddi\tr29,r29,#4\n",stream); }
*/

/* memory copy with 64 bit words. */
#define MOVE_MAX 8

#define MEMORY_MOVE_COST(mode, class, in)                   \
  ((GET_MODE_SIZE(mode) > UNITS_PER_WORD) ? 8 : 4)

#if 0

/* Given a decl node or constant node, choose the section to output it in
   and select that section.  */
/* These are from mips.h, simplified somewhat. */

#define SELECT_RTX_SECTION(MODE,RTX)            \
  text_section()

#define SELECT_SECTION(DECL, RELOC)                                     \
  do {                                                                  \
    if(RELOC)                                                           \
      text_section();                                                   \
    else if(TREE_CODE(DECL) == STRING_CST)                              \
      data_section();                                                   \
    else if(TREE_CODE(DECL) != VAR_DECL)                                \
      text_section();                                                   \
    else if(DECL_INITIAL(DECL) &&                                       \
            (DECL_INITIAL (DECL) == error_mark_node                     \
             || TREE_CONSTANT (DECL_INITIAL (DECL))))                   \
      data_section();                                                   \
    else                                                                \
      text_section();                                                   \
  } while(0)

#endif /* 0/1 */

#define SET_ASM_OP           "\tset\t"
#define GLOBAL_ASM_OP        "\tglobal\t"
#define TEXT_SECTION_ASM_OP  "\ttext"
#define BSS_SECTION_ASM_OP   "\tbss"
#define DATA_SECTION_ASM_OP  "\tdata"

#define TARGET_HAVE_NAMED_SECTION 1
#define TARGET_ASM_NAMED_SECTION  sx_named_section

/* JM: erich, is there _OP added to only a few of const pseudoops on
   purpose? if not, I'd like to *remove* it, for uniformity's sake ... */
#define ASM_LONG             "\tlong\t"
#define ASM_RLONG            "\trlong\t"
#define ASM_LONGLONG         "\tllong\t"
#define ASM_SHORT            "\tshort\t"
#define ASM_BYTE_OP          "\tbyte\t"
#define ASM_RBYTE            "\trbyte\t"
#define ASM_ALIGN            "\talign\t"
#define ASM_SPACE            "\tspace\t"
#define ASM_STRING_OP        "\tstr\t"
#define ASM_COMMON_OP        "\tcomm\t"

#define ASM_APP_ON "#APP\n"
#define ASM_APP_OFF "#NO_APP\n"
#define ASM_COMMENT_START "#"

/* we want jump tables close to our code ... */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* If defined, a C expression to compute the alignment given to a
   constant that is being placed in memory.  CONSTANT is the constant
   and ALIGN is the alignment that the object would ordinarily have.
   The value of this macro is used instead of that alignment to align
   the object.

   If this macro is not defined, then ALIGN is used.

   The typical use of this macro is to increase alignment for string
   constants to be word aligned so that `strcpy' calls that copy
   constants can be done inline.  */
/* Stolen from mips.h */
/* string constant changed to be 16 byte aligned */

#define CONSTANT_ALIGNMENT(EXP, ALIGN)                                  \
  ((TREE_CODE (EXP) == STRING_CST  || TREE_CODE (EXP) == CONSTRUCTOR)   \
   && (ALIGN) < 128                                                     \
   ? 128                                                                \
   : (ALIGN) ) 
/*: BITS_PER_WORD) */

/* If defined, a C expression to compute the alignment for a static
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that `strcpy' calls
   that copy constants to character arrays can be done inline.  */
/* Stolen from mips.h */

#undef DATA_ALIGNMENT
#define DATA_ALIGNMENT(TYPE, ALIGN)                                     \
  ((((ALIGN) < BITS_PER_WORD)                                           \
    && (TREE_CODE (TYPE) == ARRAY_TYPE                                  \
        || TREE_CODE (TYPE) == UNION_TYPE                               \
        || TREE_CODE (TYPE) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

#define ASM_OUTPUT_ALIGN(stream,val)                \
  fprintf(stream,"%s%d\n", ASM_ALIGN, 1 << (val))

#define ASM_OUTPUT_SKIP(stream,val)                 \
  fprintf(stream,"%s%d\n", ASM_SPACE, (int)(val))

#define ASM_OUTPUT_FLOAT(stream,value)                                  \
  do {                                                                  \
    long l;                                                             \
    REAL_VALUE_TO_TARGET_SINGLE(value,l);                               \
    fprintf(stream,"%s0x%08x\t\n#  %26.7e\n", ASM_LONG, l, value);      \
  } while(0)

#define ASM_OUTPUT_DOUBLE(stream,value)                 \
  do {                                                  \
    long l[2];                                          \
    REAL_VALUE_TO_TARGET_DOUBLE(value,&l[0]);			\
    fprintf(stream,"%s0x%08x%08x\t\n#  %26.16le\n",		\
            ASM_LONGLONG, l[0], l[1], value);           \
  } while(0)

#define ASM_OUTPUT_LONG_DOUBLE(stream,value)                        \
  do {                                                              \
    long l[4];                                                      \
    REAL_VALUE_TO_TARGET_DOUBLE(value,&l[0]);                       \
    fprintf(stream,"%s0x%08x%08x,0x%08x%08x\t\n#  %26.18lle\n",     \
            ASM_LONGLONG, l[0], l[1], l[2], l[3], value);           \
  } while(0)

#define ASM_OUTPUT_INT(stream,exp)                                  \
  do {                                                              \
    fprintf(stream,"%s", ASM_LONG);                                 \
    output_addr_const(stream,exp);                                  \
    putc('\n',stream);                                              \
  } while(0)

#define ASM_OUTPUT_SHORT(stream,exp)                            \
  do {                                                          \
    fprintf(stream,"%s", ASM_SHORT);                            \
    output_addr_const(stream,exp);                              \
    putc('\n',stream);                                          \
  } while(0)

#define ASM_OUTPUT_CHAR(stream,exp)                                 \
  do {                                                              \
    fprintf(stream,"%s", ASM_BYTE_OP);                              \
    output_addr_const(stream,exp);                                  \
    putc('\n',stream);                                              \
  } while(0)

#define ASM_OUTPUT_BYTE(stream,val)                 \
  fprintf(stream,"%s0x%02x\n", ASM_BYTE_OP, val)

/*
 * Need to split up .ascii directives to avoid breaking 
 * the linker.
 */

#define ASM_OUTPUT_ASCII(stream, ptr, len)      \
    asm_output_ascii_as_str(stream,ptr,len)


#define ASM_OUTPUT_FUNCTION_PREFIX(stream, fnname)  \
    asm_output_function_prefix(stream,fnname)

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_DECLARE_OBJECT_NAME sx_declare_object_name

/* no need for this because we support aligned .bss
#define ASM_OUTPUT_BSS(FILE, DECL, NAME, SIZE, ROUNDED)		\
  asm_output_bss ((FILE), (DECL), (NAME), (SIZE), (ROUNDED))
*/

#define ASM_OUTPUT_COMMON(stream, name, size, rounded)       \
  do {                                                       \
    unsigned HOST_WIDE_INT real_size = rounded;              \
    fprintf(stream, "%s", ASM_COMMON_OP);                    \
    assemble_name(stream, name);                             \
    fprintf(stream, ", " HOST_WIDE_INT_PRINT_UNSIGNED "\n",  \
            real_size);                                      \
  } while(0)
  
/* the following definition is *wrong*: it does not actually
   align the symbol in the common section - it merely aligns
   whatever section is currently being used (as the comm
   directive switches to comm and back to the previous section).
#define ASM_OUTPUT_ALIGNED_COMMON(stream, name, size, align)            \
  do {                                                                  \
    int a = align >= 8 ? (1 << log_of_two(align>>3)) : (align >> 3);	\
    fprintf(stream, "%s%d\n", ASM_ALIGN, a);                            \
    fprintf(stream, "%s", ASM_COMMON_OP);                               \
    assemble_name(stream,name);                                         \
    fprintf(stream,", " HOST_WIDE_INT_PRINT_DEC                         \
            " # ASM_OUTPUT_ALIGNED_COMMON\n",                           \
            size > 0 ? ((size - 1) / a + 1) * a : a);                   \
  } while(0)
*/

#define ASM_OUTPUT_ALIGNED_LOCAL(stream, name, size, align)             \
  do { /* switch_to_section(data_section);	*/                          \
    int a = align >= 8 ? (1 << log_of_two(align>>3)) : (align >> 3);	\
    fprintf(stream, "%s\t", BSS_SECTION_ASM_OP);                        \
    assemble_name(stream,name);                                         \
    fprintf(stream,", " HOST_WIDE_INT_PRINT_DEC ", %d "                 \
            "# ASM_OUTPUT_ALIGNED_LOCAL\n", size, a);                   \
  } while(0)

#define ASM_OUTPUT_SOURCE_FILENAME(stream, name)    \
  fprintf(stream, "\tfile\t\"%s\"\n", name)

#define ASM_OUTPUT_IDENT(stream, idents)        \
  fprintf(stream, "\tident\t\"%s\"\n", idents)

#define ASM_OUTPUT_LABEL(stream, label)                 \
  do {                                                  \
    assemble_name(stream,label);                        \
    fputs(":\n",stream);                                \
  } while(0)

#define ASM_OUTPUT_LABELREF(stream,name)        \
  fputs(name,stream)

#define ASM_OUTPUT_INTERNAL_LABEL(stream,name)  \
  fprintf(stream, "%s:\n", name)
   
/* ******************** later*/
#define ASM_OUTPUT_CASE_LABEL(stream,prefix,num,table)          \
  do {                                                          \
    ASM_OUTPUT_ALIGN (stream, 8);                               \
    (*targetm.asm_out.internal_label) (stream,prefix,num);      \
  } while(0)

#define ASM_OUTPUT_CASE_END(stream,num,table)           

/* This is how to output an element of a case-vector that is absolute. */
/* EF: should we switch to relative addresses? */
#define ASM_OUTPUT_ADDR_VEC_ELT(stream,value)       \
  fprintf(stream,"%s.L%d\n", ASM_LONGLONG, value)

/* EF: probably this will be needed with -fPIC */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)        \
  do {                                                          \
    char label[30];                                             \
    ASM_GENERATE_INTERNAL_LABEL (label, "L", (VALUE));			\
    if (CASE_VECTOR_MODE == SImode)                             \
      fprintf (FILE, "%s", ASM_LONG);                           \
    else                                                        \
      fprintf (FILE, "%s", ASM_LONGLONG);                       \
    assemble_name (FILE, label);                                \
    ASM_GENERATE_INTERNAL_LABEL (label, "L", (REL));			\
    fputc ('-', FILE);                                          \
    assemble_name (FILE, label);                                \
    fputc ('\n', FILE);                                         \
  } while(0)

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)			\
  ( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 11),			\
    sprintf ((OUTPUT), "%s%d", (NAME), (LABELNO)))

#define ASM_GENERATE_INTERNAL_LABEL(string,prefix,num)  \
  sprintf(string, ".%s%d", prefix, num)

#define PRINT_OPERAND(stream,x,code) print_operand(stream,x,code)
#define PRINT_OPERAND_ADDRESS(stream,x) print_operand_address(stream,x)

/* if we want weak symbol support, we need to build GNU assembler
   with weak symbol support as well (#define SX_SUPPORT_WEAK_SYMBOLS) */
#ifdef SX_SUPPORT_WEAK_SYMBOLS
#define SUPPORTS_WEAK 1

/* this takes care of weak declarations and weak aliases: strong aliases
   are taken care of by #defining SET_ASM_OP */
#define ASM_WEAKEN_DECL(stream, decl, name, val)    \
  do {                                              \
    fputs("\tweak\t", stream);                      \
    assemble_name(stream, name);                    \
    fputs("\n", stream);                            \
    if(NULL != val)                                 \
      {                                             \
        fputs("\n\tset\t", stream);                 \
        assemble_name(stream, name);				\
        fputs(", ", stream);                        \
        assemble_name(stream, val);                 \
        fputs("\n", stream);                        \
      }                                             \
  } while(0)
#endif /* SX_SUPPORT_WEAK_SYMBOLS */


/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   We define all 128 integer/floating registers, there are no other
   registers.  */

#define FIRST_PSEUDO_REGISTER 128


#define FIRST_S_REG 0
#define LAST_S_REG 127

/* == SX function calling conventions (under SuperUX) ==
  S0  : stack limit pointer
  S1  : frame pointer
  S2  : stack pointer
  S3  : return address pointer
  S4  : base pointer
 (S5  : top of data section)
 (S6  : top of bss section)
 (S7  : top of constant block)
  S32 : return address
  S33 : subroutine address pointer
  S34 : argument pointer
  S120: compiler internal use(XXX)
  S121: compiler internal use(XXX)
  S122: reserved for compiler internal use(XXX)
  S123: return value (integer/long/pointer)
  S124: return value (float/double/long double)
  S125: return value (long double)
*/


/* FIXED_REGISTERS 
   An initializer that says which registers are used for fixed purposes all
   throughout the compiled code and are therefore not available for general
   allocation. These would include the stack pointer, the frame pointer
   (except on machines where that can be used as a general register when no
   frame pointer is needed), the program counter on machines where that is
   considered one of the addressable registers, and any other numbered
   register with a standard use.

   Value = 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   Fixed registers are s0(stack limit), s1(frame), s2(next frame),
   s3(argument/parameter list), s4(linkage/text) */


#define FIXED_REGISTERS                                \
  {                                                    \
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0     \
  }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.

   [gccint definition:]

   Like FIXED_REGISTERS but has 1 for each register that is clobbered (in
   general) by function calls as well as for fixed registers. This macro
   therefore identifies the registers that are not available for general
   allocation of values that must live across function calls. If a register
   has 0 in CALL_USED_REGISTERS, the compiler automatically saves it on
   function entry and restores it on function exit, if the register is used
   within the function.

   s0 (stack limit), s1(stack/frame pointer), s2(next stack/frame-272),
   s3(arg/paramlist), s4(top of linkage block),
   s5(top of data section), s6(top of bss section), s7(top of constant),
   s32 (return PC), s33(entry), s34(argument/parameter list)
   s123 (integer return value), s124(float return value) */

#define CALL_USED_REGISTERS                             \
  {                                                     \
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,     \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1      \
  }

#define REGISTER_NAMES                                       \
  {                                                          \
    "s0" , "s1" , "s2" , "s3" , "s4" , "s5" , "s6" , "s7",   \
    "s8" , "s9" , "s10", "s11", "s12", "s13", "s14", "s15",  \
    "s16", "s17", "s18", "s19", "s20", "s21", "s22", "s23",  \
    "s24", "s25", "s26", "s27", "s28", "s29", "s30", "s31",  \
    "s32", "s33", "s34", "s35", "s36", "s37", "s38", "s39",  \
    "s40", "s41", "s42", "s43", "s44", "s45", "s46", "s47",  \
    "s48", "s49", "s50", "s51", "s52", "s53", "s54", "s55",  \
    "s56", "s57", "s58", "s59", "s60", "s61", "s62", "s63",  \
    "s64", "s65", "s66", "s67", "s68", "s69", "s70", "s71",  \
    "s72", "s73", "s74", "s75", "s76", "s77", "s78", "s79",  \
    "s80", "s81", "s82", "s83", "s84", "s85", "s86", "s87",  \
    "s88", "s89", "s90", "s91", "s92", "s93", "s94", "s95",  \
    "s96", "s97", "s98", "s99", "s100","s101","s102","s103", \
    "s104","s105","s106","s107","s108","s109","s110","s111", \
    "s112","s113","s114","s115","s116","s117","s118","s119", \
    "s120","s121","s122","s123","s124","s125","s126","s127"  \
  }

/* List the order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.*/
#define REG_ALLOC_ORDER                       \
  {                                           \
    35, 36, 37, 38, 39,                       \
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49,   \
    50, 51, 52, 53, 54, 55, 56, 57, 58, 59,   \
    60, 61, 62, 63, 64, 65, 66, 67, 68, 69,   \
    70, 71, 72, 73, 74, 75, 76, 77, 78, 79,   \
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89,   \
    90, 91, 92, 93, 94, 95, 96, 97, 98, 99,   \
    100,101,102,103,104,105,106,107,108,109,  \
    110,111,112,113,114,115,116,117,118,119,  \
    120,121,122,                              \
    127,126,125,124,123,                      \
    34, 33, 32,                               \
    8, 9,                                     \
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,   \
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,   \
    30, 31,                                   \
    7,  6,  5,  4,  3,  2,  1,  0             \
  }


/********************************************* later*/
/* values that can go in particular registers. */
/* q-doubles will be the only thing that will take 2 registers. */

#define HARD_REGNO_NREGS(regno,mode)                            \
  (COMPLEX_MODE_P (mode) ? 2 :                                  \
   (GET_MODE_SIZE(mode) + UNITS_PER_WORD - 1)/UNITS_PER_WORD)

/* SX has 32 bit immediates. */
#define SMALL_INT(X)                                                    \
  (((HOST_WIDE_INT)INTVAL(X) >= -2147483647-1 &&                        \
    (HOST_WIDE_INT)INTVAL(X) <= 2147483647))

#define SMALL_OPERAND(VALUE)                                        \
  ((unsigned HOST_WIDE_INT) (VALUE) + 0x80000000 < 0x100000000LL)

/* True if VALUE is an unsigned 32-bit number.  */

#define SMALL_OPERAND_UNSIGNED(VALUE)                       \
  (((VALUE) & ~(unsigned HOST_WIDE_INT)0xffffffff) == 0)

/* Return a value X with the low 32 bits clear, and such that
 *    VALUE - X is a signed 32-bit value.  */

#define CONST_HIGH_PART(VALUE)                                      \
  (((VALUE) + 0x80000000) & ~(unsigned HOST_WIDE_INT)0xffffffff)

#define CONST_LOW_PART(VALUE)                   \
  ((VALUE) - CONST_HIGH_PART(VALUE))

#define SMALL_INT_UNSIGNED(X) SMALL_OPERAND_UNSIGNED(INTVAL(X))


/* Allocate registers appropriate to data types. doubles 
   require even/odd pairs of long double registers. */

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.*/

#define HARD_REGNO_MODE_OK(regno,mode) hard_regno_mode_ok_func(regno,mode)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define MODES_TIEABLE_P(mode1, mode2) 1 

/* Prevent direct changes of pseudo reg modes from/to SF: on SX, SF data
   are put in the upper half of the the register, while SI data are in the
   lower half, thus data loaded as SI can't be interpreted as SF without
   an appropriate reg shift and vice-versa. */

#define CANNOT_CHANGE_MODE_CLASS(from, to, class)                       \
  ((from == SFmode && to != SFmode) || (from != SFmode && to == SFmode))

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

enum reg_class
{
  NO_REGS,                      /* no registers in set */
  S_REGS,                       /* scalar registers */
  ALL_REGS,                     /* all registers */
  LIM_REG_CLASSES               /* max value + 1 */
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define GENERAL_REGS S_REGS
#define GP_REG_P(REGNO)                                         \
  (FIRST_S_REG <= (int)(REGNO) && (int)(REGNO) <= LAST_S_REG)

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES                         \
  {                                             \
    "NO_REGS",                                  \
    "S_REGS",                                   \
    "ALL_REGS"                                  \
  }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS                                                 \
  {                                                                        \
    { 0x00000000,0x00000000,0x00000000,0x00000000 },   /* no registers */  \
    { 0xffffffff,0xffffffff,0xffffffff,0xffffffff },   /* s registers */   \
    { 0xffffffff,0xffffffff,0xffffffff,0xffffffff }    /* all registers */ \
  }

#define REGNO_REG_CLASS(regno) (S_REGS)   

/* The class value for index registers, and the one for base regs.  */

#define BASE_REG_CLASS S_REGS
#define INDEX_REG_CLASS S_REGS

#define REG_CLASS_FROM_LETTER(char) reg_class_from_letter(char)

/* If we use the normal load/store ops ,
   it will always sign-extend sub-word types. */
#define LOAD_EXTEND_OP(mode) SIGN_EXTEND

/*
 * Memory address stuff.
 */

/* BEGIN #if-0-ed-out-code */
#if 0

#error "This should be 0-ed out."

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

/* 1 if X is an rtx for a constant that is a valid address.  */

#define CONSTANT_ADDRESS_P(X) CONSTANT_P(X)

#define CONSTANT_POOL_BEFORE_FUNCTION 1
/* #define CONSTANT_AFTER_FUNCTION_P(EXP) 1 */

#define ASM_OUTPUT_POOL_PROLOGUE(FILE, FUNNAME, FUNDECL, SIZE) \
  fprintf(FILE, "#.%s.pool:\n", FUNNAME);


#define REGNO_OK_FOR_BASE_P(REGNO)                                          \
((REGNO) < FIRST_PSEUDO_REGISTER ? ((REGNO) > 0 && (REGNO) <= LAST_S_REG) \
 : (reg_renumber[REGNO] > 0 && (reg_renumber[REGNO] <= LAST_S_REG )))

#ifdef REG_OK_STRICT
/* Strict version, used in reload pass. This should not
 * accept pseudo registers.
 */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P(REGNO(X))
#else
/* Accept an int register or a pseudo reg. */
#define REG_OK_FOR_BASE_P(X) (REGNO(X) <= LAST_S_REG || \
                              REGNO(X) >= FIRST_PSEUDO_REGISTER)
#endif

/*
 * SX doesn't have any indexed addressing
 * modes, so nothing is ok as an index register.
 */
#define REG_OK_FOR_INDEX_P(X) 0
#define REGNO_OK_FOR_INDEX_P(X) 0

#define LEGITIMATE_ADDRESS_INTEGER_P(X,OFFSET)          \
 (GET_CODE (X) == CONST_INT && SMALL_INT(X))

#define LEGITIMATE_OFFSET_ADDRESS_P(MODE,X)             \
 (GET_CODE (X) == PLUS                                  \
  && GET_CODE (XEXP (X, 0)) == REG                      \
  && REG_OK_FOR_BASE_P (XEXP (X, 0))                    \
  && LEGITIMATE_ADDRESS_INTEGER_P (XEXP (X, 1), 0) )
/*
  && (((MODE) != DFmode && (MODE) != DImode)            \
      || LEGITIMATE_ADDRESS_INTEGER_P (XEXP (X, 1), 4)))
*/


#define LEGITIMATE_NONOFFSET_ADDRESS_P(MODE,X)          \
             (GET_CODE(X) == REG && REG_OK_FOR_BASE_P(X))
/* 
 * This is simple because SX only has one addressing mode:
   register + 16 bit signed offset.
 */
#define GO_IF_LEGITIMATE_ADDRESS(MODE,X,ADDR)           \
  if(LEGITIMATE_OFFSET_ADDRESS_P(MODE,X)) goto ADDR;    \
  if(LEGITIMATE_NONOFFSET_ADDRESS_P(MODE,X)) goto ADDR; 

/* 
 * We have to force symbol_ref's into registers here
 * because nobody else seems to want to do that!
 */
#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)     

/*
 * SX addresses do not depend on the machine mode they are
 * being used in.
 */
#define GO_IF_MODE_DEPENDENT_ADDRESS(addr,label)

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   The macro definition, if any, is executed immediately after the
   rtl for DECL or other node is created.
   The value of the rtl will be a `mem' whose address is a
   `symbol_ref'.

   The usual thing for this macro to do is to a flag in the
   `symbol_ref' (such as `SYMBOL_REF_FLAG') or to store a modified
   name string in the `symbol_ref' (if one bit is not enough
   information). */ 

#define TEXT_SPACE_P(DECL)\
  (TREE_CODE (DECL) == FUNCTION_DECL                                    \
   || (TREE_CODE (DECL) == VAR_DECL                                     \
       && TREE_READONLY (DECL) && ! TREE_SIDE_EFFECTS (DECL)            \
       && !flag_pic)                                                    \
   || (*tree_code_type[(int) TREE_CODE (DECL)] == 'c'                   \
       && !(TREE_CODE (DECL) == STRING_CST && flag_writable_strings)))

#define FUNCTION_NAME_P(NAME) \
(*(NAME) == '@' || (*(NAME) == '*' && *((NAME) + 1) == '@'))
#endif
/* END #if-0-ed-out-code */


/* Addressing modes, and classification of registers for them.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) 0
#define REGNO_MODE_OK_FOR_BASE_P(REGNO, MODE)   \
  sx_regno_mode_ok_for_base_p (REGNO, MODE, 1)

#define SELECT_CC_MODE(OP,X,Y) sx_select_cc_mode(OP, X, Y)

/* #define REVERSIBLE_CC_MODE(mode) 0 */

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects them all.
   The symbol REG_OK_STRICT causes the latter definition to be used.

  Most source files want to accept pseudo regs in the hope that
  they will get allocated to the class that the insn wants them to be in.
  Some source files that are used after register allocation
  need to be strict.  */

#ifndef REG_OK_STRICT
#define REG_MODE_OK_FOR_BASE_P(X, MODE)             \
  sx_regno_mode_ok_for_base_p (REGNO (X), MODE, 0)
#else
#define REG_MODE_OK_FOR_BASE_P(X, MODE)             \
  sx_regno_mode_ok_for_base_p (REGNO (X), MODE, 1)
#endif

#define REG_OK_FOR_INDEX_P(X) 0

/* Maximum number of registers that can appear in a valid memory address.  */

/* #define MAX_REGS_PER_ADDRESS 1 */

#ifdef REG_OK_STRICT
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)    \
  do {                                             \
    if (sx_legitimate_address_p (MODE, X, 1))      \
      goto ADDR;                                   \
  } while(0)
#else
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR) \
  do {                                          \
    if (sx_legitimate_address_p (MODE, X, 0))   \
      goto ADDR;                                \
  } while(0)
#endif

/* disabled, took thing from contrib/b
#define CONSTANT_ADDRESS_P(X) \
  (CONSTANT_P (X) && sx_legitimate_address_p (SImode, X, 0))
*/

extern int constant_address_p(rtx x);
#define CONSTANT_ADDRESS_P(X) constant_address_p(X)
#define MAX_REGS_PER_ADDRESS 2

#if 1
/* the following is from contrib/a */
#define LEGITIMATE_CONSTANT_P(X) (sx_const_insns (X) > 0)
#else
/* took the following from contrib/b */
/* It forces certain loads and references to be done from the
   constant pool.  */
#define LEGITIMATE_CONSTANT_P(X) sx_legitimate_constant_p(X)
#endif

#if 1
/*EF# # stuff below is from contrib/a */
#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)                     \
  do {                                                          \
    if (sx_legitimize_address (&(X), MODE))                     \
      goto WIN;                                                 \
  } while (0)
#else
/* following is from contrib/b */
#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)                         \
  do {                                                              \
    if (GET_CODE (X) == CONST)                                      \
      {                                                             \
        rtx ptr_reg = gen_reg_rtx (Pmode);                          \
        rtx constant = XEXP (XEXP (X,0), 1);                        \
        if (GET_CODE (XEXP (X, 0)) != PLUS)                         \
          abort(); /* XXX */                                        \
        if (GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF &&       \
            !CONSTANT_POOL_ADDRESS_P (XEXP (XEXP (X, 0), 0)))       \
          {                                                         \
            rtx tmp;                                                \
            tmp = force_const_mem (Pmode, XEXP (XEXP (X,0), 0));	\
            emit_move_insn (ptr_reg, tmp);                          \
            fprintf(stderr, "legitimize_address: ");                \
            print_inline_rtx(stderr,x,0);                           \
            fprintf(stderr, "\n");                                  \
          }                                                         \
        else                                                        \
          {                                                         \
            emit_move_insn (ptr_reg, XEXP (XEXP (X, 0), 0));        \
          }                                                         \
                                                                    \
        X = gen_rtx_PLUS(Pmode, ptr_reg, constant);                 \
        goto WIN;                                                   \
      }                                                             \
    else {                                                          \
      fprintf(stderr, "legitimize_address not CONST: ");            \
      print_inline_rtx(stderr,x,0);                                 \
      fprintf(stderr, "\n");                                        \
    }                                                               \
  } while(0)
#endif

/* A C statement or compound statement with a conditional `goto
   LABEL;' executed if memory address X (an RTX) can have different
   meanings depending on the machine mode of the memory reference it
   is used for.

   Autoincrement and autodecrement addresses typically have
   mode-dependent effects because the amount of the increment or
   decrement is the size of the operand being addressed.  Some
   machines have other mode-dependent addresses.  Many RISC machines
   have no mode-dependent addresses.

   You may assume that ADDR is a valid address for the machine.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL) do {} while(0)

#define CONST_OK_FOR_LETTER_P(VALUE, C)                         \
  ((C) == 'I' ? ((VALUE) >= -64 && (VALUE) < 64)                \
   : (C) == 'J' ? ((VALUE) >= 0 && (VALUE) < 32)                \
   : (C) == 'K' ? ((VALUE) >= 0 && (VALUE) < 64)                \
   : (C) == 'L' ? ((VALUE) >= 0 && (VALUE) < 128)               \
   : (C) == 'M' ? ((((VALUE) & ((VALUE) + 1)) == 0)             \
                   || (((~(VALUE)) & ((~(VALUE)) + 1)) == 0))   \
   : (C) == 'N' ? (((VALUE) >= -2147483647-1 && (VALUE) < -64)	\
                   || ((VALUE) >=64 && (VALUE) <= 2147483647))	\
   : 0)

#define CLASS_UNITS(mode, size)                                         \
  ((GET_MODE_SIZE (mode) + (size) - 1) / (size))

#define CLASS_MAX_NREGS(CLASS, MODE)            \
  (CLASS_UNITS (MODE, UNITS_PER_WORD))
/*
  ((CLASS) == FP_REGS                                               \
   ? (CLASS_UNITS (MODE, 4))                                        \
   : CLASS_UNITS (MODE, UNITS_PER_WORD))
*/

#undef STACK_GROWS_DOWNWARD
#undef FRAME_GROWS_DOWNWARD
#undef ARGS_GROWS_DOWNWARD

#define SX_RSA_SIZE 272
#define FRAME_GROWS_DOWNWARD 0

/* offset from frame pointer to first local variable slot */
#if SX_NORMAL_STACK_LAYOUT
#define STARTING_FRAME_OFFSET \
  (SX_RSA_SIZE + 8 + current_function_outgoing_args_size)
#else
#define STARTING_FRAME_OFFSET SX_RSA_SIZE
#endif

/* Define this if the maximum size of all the outgoing args is to be
   accumulated and pushed during the prologue.  The amount can be
   found in the variable current_function_outgoing_args_size.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* offset from stack pointer to outgoing arguments */
#if SX_NORMAL_STACK_LAYOUT
#define STACK_POINTER_OFFSET                                    \
  (-sx_compute_frame_size(get_frame_size()) + SX_RSA_SIZE + 8)
#else
#define STACK_POINTER_OFFSET                            \
  (-SX_RSA_SIZE - current_function_outgoing_args_size)
#endif

#define STACK_CHECK_BUILTIN 1
/* #define STACK_CHECK_PROBE_INTERVAL */
/* #define STACK_CHECK_PROBE_LOAD */
/* #define STACK_CHECK_PROTECT */
/* #define STACK_CHECK_MAX_FRAME_SIZE */
/* #define STACK_CHECK_FIXED_FRAME_SIZE */
/* #define STACK_CHECK_MAX_VAR_SIZE */

/* incomming args are accessed from this offset with $s3 */
#define FIRST_PARM_OFFSET(FNDECL) 8

/* since SX doesn't have push instructions for storing function arguments on 
   stack, we have to set PUSH_ARGS to 0. */
#define PUSH_ARGS 0

/**
 * dynamic space on stack is placed above static local space, and before
 * outgoing vars space. I hope ... ;)
 */
#define STACK_DYNAMIC_OFFSET(FUNDECL)                               \
  (-SX_RSA_SIZE - current_function_outgoing_args_size - 8)

/* Register to use for pushing function arguments. */
#define STACK_POINTER_REGNUM 2

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 1

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 1

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 3

/* Definitions for register elimination.  */
#define ELIMINABLE_REGS                             \
  { { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM} }

/* we will only be able to eliminate the frame pointer in favour of stack
   pointer once the prologues are not hardcoded anymore. until that point,
   CAN_ELIMINATE should be always false. */
#define CAN_ELIMINATE(FROM, TO) 0

/* We always pretend that this is a leaf function because if it's not,
   there's no point in trying to eliminate the frame pointer.  If it
   is a leaf function, we guessed right!  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)                \
  do                                                                \
    {                                                               \
      if ((FROM) == FRAME_POINTER_REGNUM &&                         \
          (TO) == STACK_POINTER_REGNUM)                             \
        (OFFSET) = sx_compute_frame_size(get_frame_size());         \
      else                                                          \
        gcc_assert(FALSE);                                          \
    }                                                               \
  while (0)

/* EF: let's try without static chain. Do we need only a static chain regno? */
#if 0
/* Place to put static chain when calling a function that requires it.  */
#define STATIC_CHAIN                                                 \
  gen_rtx_MEM (Pmode, gen_rtx_PLUS(Pmode, frame_pointer_rtx,         \
                                gen_rtx_CONST_INT(VOIDmode, -24)))

/* Place where static chain is found upon entry to routine.  */
#define STATIC_CHAIN_INCOMING                                          \
  gen_rtx_MEM (Pmode, gen_rtx_PLUS(Pmode, stack_pointer_rtx,           \
                                gen_rtx_CONST_INT(VOIDmode, 248)))
#else
/* EF test: define static chain regnum */
#define STATIC_CHAIN_REGNUM 7
#endif

/* The following macro doesn't actually work as expected, the compiler still
   uses the builtin constructs.  */
/* #define DONT_USE_BUILTIN_SETJMP 1 */

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).*/

#define FUNCTION_ARG(cum,mode,type,named) 0

/*
  Determines whether, and in which direction, to pad out an argument with
  extra space. The value should be of type enum direction: either upward
  to pad above the argument, downward to pad below, or none to inhibit
  padding.
*/
#define FUNCTION_ARG_PADDING(MODE, TYPE)                \
  (sx_pad_arg_upward (MODE, TYPE) ? upward : downward)

/*
 * If defined, a C expression that gives the alignment boundary, in
 * bits, of an argument with the specified mode and type.  If it is
 * not defined,  `PARM_BOUNDARY' is used for all arguments.  
 */

#define FUNCTION_ARG_BOUNDARY function_arg_boundary

/* 
 * Return char/short/int in s123
 * return float in s124
 * return double in s124
 * struct/union in memory referenced by s123
 * return anything else in s123
 */

/* extern rtx sx_function_value (tree, tree , enum machine_mode); */
#define FUNCTION_VALUE(VALTYPE, FUNC)               \
  sx_function_value ((VALTYPE), (FUNC), VOIDmode)

/*
 * Which return types are returned in memory.
 * EF: fixed problems with complex testcases,
 * still not sure whether complex are returned in memory or in
 * s124,s125.
 */
#define RETURN_IN_MEMORY(TYPE)                                  \
  ((TYPE_MODE (TYPE) == BLKmode) ||                             \
   (GET_MODE_CLASS(TYPE_MODE(TYPE)) == MODE_COMPLEX_FLOAT) ||   \
   (GET_MODE_CLASS(TYPE_MODE(TYPE)) == MODE_COMPLEX_INT)) 

/* return in s123 or s124. */
#define LIBCALL_VALUE(MODE)                     \
	sx_function_value(NULL_TREE, NULL, (MODE))
/* was: (FLOAT_MODE_P(MODE)?gen_rtx_REG(DFmode,124):gen_rtx_REG(MODE,123)) */

/*typedef int CUMULATIVE_ARGS;*/
/********************** later*/
#define CUMULATIVE_ARGS int
#define INIT_CUMULATIVE_ARGS(cum,fntype,libname,indirect,n_named_args)  \
	(cum) = 0

/*
  Update the summarizer variable cum to advance past an argument in the
  argument list. The values mode, type and named describe that argument.
  Once this is done, the variable cum is suitable for analyzing the
  following argument with FUNCTION_ARG, etc. This macro need not do
  anything if the argument in question was passed on the stack. The
  compiler knows how to track the amount of stack space used for arguments
  without any special help.
*/
#define FUNCTION_ARG_ADVANCE(cum,mode,type,named)                       \
  do {                                                                  \
    cum += UNITS_PER_WORD*((GET_MODE_SIZE(mode)+UNITS_PER_WORD-1)       \
                           /UNITS_PER_WORD);                            \
  } while(0)

/*
 * As is usual in C, the caller pops all the arguments.
 */

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACKSIZE) 0

/*
 * All registers are passed on the stack.
 */
#define FUNCTION_ARG_REGNO_P(regno) 0

/*
 * Return is always in s123 or s124 or s124/s125.
 */
#define FUNCTION_VALUE_REGNO_P(regno) \
  ((regno) == 123 || (regno) == 124 || (regno) == 125)


/* A C statement to output, on the stream FILE, assembler code for a
   block of data that contains the constant parts of a trampoline. 
   This code should not include a label--the label is taken care of
   automatically.  */

#define TRAMPOLINE_TEMPLATE(STREAM)                                     \
  do {                                                                  \
    fprintf(STREAM, "\tsic\t$s33\t# trampoline start\n");               \
    fprintf(STREAM, "\tnop\n");                                         \
    fprintf(STREAM, "\tlds\t$%s,28(,$s33)\n",                           \
            reg_names[STATIC_CHAIN_REGNUM]);                            \
    fprintf(STREAM, "\tlds\t$s33,36(,$s33)\n");                         \
    fprintf(STREAM, "\tb\t0(,$s33)\n");                                 \
    fprintf(STREAM, "%s0xabbadada\t# static_chain address\n",           \
            ASM_LONGLONG);                                              \
    fprintf(STREAM, "%s0xfeedbeef\t# function address\n",               \
            ASM_LONGLONG);                                              \
  } while(0)

/* A C expression for the size in bytes of the trampoline, as an
   integer. This is the number of bytes that gets copied to the stack
   and executed. */

#define TRAMPOLINE_SIZE (48)

/* Alignment required for trampolines, in bits.

   If you don't define this macro, the value of `BIGGEST_ALIGNMENT'
   is used for aligning trampolines.  */

#define TRAMPOLINE_ALIGNMENT 64

/* A C statement to initialize the variable parts of a trampoline. 
   ADDR is an RTX for the address of the trampoline; FNADDR is an
   RTX for the address of the nested function; STATIC_CHAIN is an
   RTX for the static chain value that should be passed to the
   function when it is called.  */

#define INITIALIZE_TRAMPOLINE(ADDR, FUNC, CHAIN)                        \
  do {                                                                  \
    rtx addr = ADDR;                                                    \
    emit_move_insn (gen_rtx_MEM(DImode, plus_constant (addr, 32)), CHAIN); \
    emit_move_insn (gen_rtx_MEM(DImode, plus_constant (addr, 40)), FUNC); \
                                                                        \
    /* Flush the instruction cache.  */                                 \
    emit_insn(gen_flush_icache());                                      \
  } while(0)

/* Flush the instruction cache.  */

/* On SX we can't specify an interval to clear instruction cache. We
   simply clear the whole i-cache */
#define CLEAR_INSN_CACHE(BEG, END)  { __asm __volatile ("rcr 2"); }


/* Flag to mark data that is in the large address area.  */
#define SYMBOL_FLAG_FAR_ADDR (SYMBOL_FLAG_MACH_DEP << 0)
#define SYMBOL_REF_FAR_ADDR_P(X)                                \
  ((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_FAR_ADDR) != 0)

/* hard regs s1 - s8 are always saved by prologue, so they will always be
   restored by epilogue and need to be marked so ... also, hard reg s32
   is used to load the return address to, and needs to be marked as well */
#define EPILOGUE_USES(regno)  (((regno >= 1) && (regno <= 8)) || (regno == 32))

/* #pragma handling */
#define HANDLE_SYSV_PRAGMA            1
#define HANDLE_PRAGMA_PACK_PUSH_POP   1

/* profiling support */
#define FUNCTION_PROFILER(file, labelno) sx_function_profiler(file, labelno)

/*
;;- Local variables:
;;- mode:c
;;- eval: (c-set-style "gnu")
;;- End:
*/

