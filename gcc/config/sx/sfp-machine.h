#define _FP_W_TYPE_SIZE		64
#define _FP_W_TYPE		unsigned long
#define _FP_WS_TYPE		signed long
#define _FP_I_TYPE		long

#define CMPtype			long

typedef int TItype __attribute__ ((mode (TI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));

#define TI_BITS (__CHAR_BIT__ * (int)sizeof(TItype))

 #define _FP_MUL_MEAT_S(R,X,Y)                           \
  _FP_MUL_MEAT_1_wide(_FP_WFRACBITS_S,R,X,Y,umul_ppmm)
#define _FP_MUL_MEAT_D(R,X,Y)                           \
  _FP_MUL_MEAT_1_wide(_FP_WFRACBITS_D,R,X,Y,umul_ppmm) 
#define _FP_MUL_MEAT_Q(R,X,Y)                           \
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_S(R,X,Y)   _FP_DIV_MEAT_1_udiv(S,R,X,Y)
#define _FP_DIV_MEAT_D(R,X,Y)   _FP_DIV_MEAT_1_udiv(D,R,X,Y)
#define _FP_DIV_MEAT_Q(R,X,Y)   _FP_DIV_MEAT_2_udiv(Q,R,X,Y)


#define _FP_NANFRAC_S		_FP_QNANBIT_S
#define _FP_NANFRAC_D		_FP_QNANBIT_D
#define _FP_NANFRAC_E		_FP_QNANBIT_E, 0
#define _FP_NANFRAC_Q		_FP_QNANBIT_Q, 0
#define _FP_NANSIGN_S		1
#define _FP_NANSIGN_D		1
#define _FP_NANSIGN_E		1
#define _FP_NANSIGN_Q		1

/* Someone please check this.  */
#define _FP_CHOOSENAN(fs, wc, R, X, Y, OP)                      \
  do {                                                          \
    if ((_FP_FRAC_HIGH_RAW_##fs(X) & _FP_QNANBIT_##fs)          \
        && !(_FP_FRAC_HIGH_RAW_##fs(Y) & _FP_QNANBIT_##fs))     \
      {                                                         \
        R##_s = Y##_s;                                          \
        _FP_FRAC_COPY_##wc(R,Y);                                \
      }                                                         \
    else                                                        \
      {                                                         \
        R##_s = X##_s;                                          \
        _FP_FRAC_COPY_##wc(R,X);                                \
      }                                                         \
    R##_c = FP_CLS_NAN;                                         \
  } while (0)


#define _FP_KEEPNANFRACP 1

#define	__LITTLE_ENDIAN	1234
#define	__BIG_ENDIAN	4321

#define __BYTE_ORDER __BIG_ENDIAN

/* Define ALIASNAME as a strong alias for NAME.  */
# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));
