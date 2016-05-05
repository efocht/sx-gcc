#ifndef __SX_PROTOS_H
#define __SX_PROTOS_H

#include "tm.h"
#include "rtl.h"

extern enum machine_mode sx_compare_mode;

extern struct rtx_def *sx_compare_op0, *sx_compare_op1;

extern rtx   sx_function_value(tree, tree , enum machine_mode);
extern int   sx_regno_mode_ok_for_base_p(int, enum machine_mode, int);
extern bool  sx_legitimate_address_p(enum machine_mode, rtx, int);
extern int   sx_legitimate_constant_p(rtx);
extern void  sx_split_tmode_pair(rtx *, enum machine_mode, bool);
extern bool  sx_expand_mov(enum machine_mode, rtx *);
extern void  sx_emit_mov_via_s4(rtx dst, rtx src);
extern void  sx_emit_mem_mem_mov(rtx dst, rtx src);
extern bool  sx_legitimize_address (rtx *, enum machine_mode);
extern int   sx_compute_frame_size(int size);
extern int   sx_const_insns (rtx x);
extern bool  sx_pad_arg_upward(enum machine_mode mode, tree type);
extern void  sx_declare_object_name(FILE *, const char *, tree);
extern bool  sx_mov_source_needs_s4(rtx x);
extern void  sx_named_section (const char *name, unsigned int flags,
                               unsigned int align);
extern void  sx_function_profiler(FILE *, int);

extern void  sx_dump_rtx(const char *pfx, rtx x);

/* asm output */
extern void asm_output_ascii_as_bytes(FILE *stream, const char *ptr, int len);
extern void asm_output_ascii_as_str(FILE *stream, const char *ptr, int len);
extern void asm_output_function_prefix(FILE *file, const char *fnname);
extern const char *sx_asm_output_call(rtx *operands, int sibcall);

/* double constants loadable as immediates (for floats and DI) */
extern int sx_g_ok(unsigned HOST_WIDE_INT vh, unsigned HOST_WIDE_INT vl);
extern int sx_h_ok(unsigned HOST_WIDE_INT vh, unsigned HOST_WIDE_INT vl);

extern int   log_of_two(int x);
extern void  print_operand(FILE *stream, rtx x, int letter);
extern void  print_operand_address(FILE *file, rtx addr);
extern int   hard_regno_mode_ok_func(int regno, int mode);
extern int   reg_class_from_letter(int chr);
extern void  emit_soft_tfmode_cvt(enum rtx_code, rtx *);
extern void  sx_conditional_branch(rtx *operands, enum rtx_code test);
extern enum machine_mode sx_select_cc_mode (enum rtx_code code, rtx op0, rtx op1);
extern int   function_arg_boundary(enum machine_mode mode, tree type);

/* option handling */
extern int sx_handle_option(size_t, const char *, int);

#ifdef SX_RTL_PRO_EPI_LOGUE
extern void sx_expand_prologue(void);
extern void sx_expand_epilogue(int sibcall);
#endif /* SX_RTL_PRO_EPI_LOGUE */

#endif
