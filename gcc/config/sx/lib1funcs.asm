	text
__lib1funcs_start:
	using	__lib1funcs_start,$s4

###############################################################################
# __ucmpti2: This function performs an unsigned comparison of a and b. If a is 
# less than b, it returns 0; if a is greater than b, it returns 2; and if 
# a and b are equal it returns 1. 
###############################################################################	
	global __ucmpti2
	align 8
__ucmpti2:
	# function prologue begin:
	# since the "__ucmpi2" is a very simple function, we decided to 
	# use a register store area (RSA) that a caller function has reserved for 
	# it and we don't create new stack entry for it. in it, we store registers 
	# $s3 (procedure's callee-saved copy of $s34) and $s4 (pointer to linkage
	# section).
	stm $s3, 2, -272(,$s2)
	# we also store all the registers that are overwritten by the "__ucmpi2"
	# routine.
	stm $s35, 3, -256(,$s2)

	lea	$s4, __lib1funcs_start-__ucmpti2(,$s33)
	or	$s3,0,$s34
	# function prologue end 
	
	#	first compare the bits 0..63 of both operands
	lds	$s35, 8(,$s3) # read bits 0..63 of operand A
	lds $s36, 24(,$s3) # read bits 0..63 of operand B
	cmp	$s37, $s35, $s36
	bh $s37, __ucmpti2_higher
	bl $s37, __ucmpti2_lower
	
	#	in case the bits 0..63 are equal, compare the bits 64..127
	lds	$s35, 16(,$s3) # read bits 64..127 of operand A
	lds $s36, 32(,$s3) # read bits 64..127 of operand B
	cmp	$s37, $s35, $s36
	bh $s37, __ucmpti2_higher
	bl $s37, __ucmpti2_lower
	#	in case we have come to this point, the two numbers are equal (A = B)
	lea $s123, 1
	b __ucmpti2_end
__ucmpti2_higher:
	# A > B
	lea $s123, 2
	b __ucmpti2_end
__ucmpti2_lower:
	# A < B
	lea $s123, 0
__ucmpti2_end:
	# function epilogue begin
	
	# restore the old value of the $s3 and $s4 registers
	ldm	$s3, 2, -272(,$s2)
	# restore the values of the registers that were overwritten by the 
	# "__ucmpi2" routine.
	ldm $s35, 3, -256(,$s2)
	
	b	0(,$s32)
	# function epilogue end 	

###############################################################################
# __multi3: This functions returns the product of a and b.  
###############################################################################	
	global __multi3
	align 8
__multi3:
	# function prologue begin:
	# since the "__multi3" is a very simple function, we decided to 
	# use a register store area (RSA) that a caller function has reserved for 
	# it and we don't create new stack entry for it. in it, we store registers 
	# $s3 (procedure's callee-saved copy of $s34) and $s4 (pointer to linkage
	# section).
	stm $s3, 2, -272(,$s2)
	# we also store all the registers that are overwritten by the "__ucmpi2"
	# routine.
	stm $s35, 17, -256(,$s2)
	stm $s113, 9, -120(,$s2)

	lea	$s4, __lib1funcs_start-__multi3(,$s33)
	or	$s3,0,$s34
	# function prologue end 


	# When multiplying two 128-bit numbers, we divide them in 4 blocks each.
	# A = (A1 << 96) + (A2 << 64) + (A3 << 32) + A4  
	# B = (B1 << 96) + (B2 << 64) + (B3 << 32) + B4
	#
	# This gives us the 32-bit blocks which we multiply by using the 
	# NEC SX's "MPY" instructions.
	
	# load block A1
	ldl=	$s35,8(,$s3)
	# load block A2
	ldl=	$s36,12(,$s3)
	# load block A3
	ldl=	$s37,16(,$s3)
	# load block A4
	ldl=	$s38,20(,$s3)
	
	# load block B1
	ldl=	$s39,24(,$s3)
	# load block B2
	ldl=	$s40,28(,$s3)
	# load block B3
	ldl=	$s41,32(,$s3)
	# load block B4
	ldl=	$s42,36(,$s3)

	# the multiplication is based on the following formula:
	# AB = A * B = (A1 * B1) << 192 + (A1 * B2 + A2 * B1) << 160 +
	#		+ (A1 * B3 + A2 * B2 + A3 * B1) << 128 +
	#		+ (A1 * B4 + A2 * B3 + A3 * B2 + A4 * B1) << 96 +
	#		+ (A2 * B4 + A3 * B3 + A4 * B2) << 64 +
	#		+ (A3 * B4 + A4 * B3) << 32 + (A4 * B4)
	
	# 1) calculate block AB4 = (A4 * B4) + (A3 * B4 + A4 * B3)[32..63] << 32
	
	# (A4 * B4)
	mpx	$s43, $s38, $s42
	# (A3 * B4 + A4 * B3) and store it to register $s45. Store the carry bit
	# to register $s44. 
	mpx	$s44, $s37, $s42
	mpx	$s45, $s38, $s41
	
	or	$s118, 0, $s44
	or	$s119, 0, $s45
	bsic	$s117, __multi3_add
	or $s44, 0, $s121
	or $s45, 0, $s120
	
	# (A4 * B4) + (A3 * B4 + A4 * B3)[32..63] << 32
	or	$s118, 0, $s43
	and	$s119, $s45, (32)0
	sll $s119, $s119, 32
	bsic	$s117, __multi3_add
	or	$s124, 0, $s120
	
	
	# 2) calculate block AB3 = (A3 * B4 + A4 * B3)[0..31] >> 32 + 
	#		+ "carry bit from (A3 * B4 + A4 * B3)" << 32
	#		+ "carry bit from (A4 * B4) + (A3 * B4 + A4 * B3)[32..63] << 32"
	#		+ (A2 * B4 + A3 * B3 + A4 * B2) +
	#		+ (A1 * B4 + A2 * B3 + A3 * B2 + A4 * B1)[32..63] << 32
	#
	# Note: for the second block (i.e. the one that contains the most 
	# significant bytes of the 128-bit result) we don't need to use the
	# "__multi3_add" anymore because the carry bit is not important to
	# us anymore. Thus, we can use the "add" SX instruction for performing
	# the addition. 
	
	# (A3 * B4 + A4 * B3)[0..31] >> 32
	# (A3 * B4 + A4 * B3) is already stored in $s45
	srl	$s46, $s45, 32
	# + "carry bit from (A3 * B4 + A4 * B3)" << 32
	sll $s47, $s44, 32
	or	$s46, $s46, $s47
	# + "carry bit from (A4 * B4) + (A3 * B4 + A4 * B3)[32..63] << 32"
	add	$s46, $s46, $s121

	# (A2 * B4 + A3 * B3 + A4 * B2)
	mpx $s47, $s36, $s42
	mpx $s48, $s37, $s41
	mpx $s49, $s38, $s40
	
	add	$s47, $s47, $s48
	add $s47, $s47, $s49
	

	# (A1 * B4 + A2 * B3 + A3 * B2 + A4 * B1)[32..63] << 32
	mpx	$s48, $s35, $s42
	mpx	$s49, $s36, $s41
	mpx $s50, $s37, $s40
	mpx $s51, $s38, $s39
	
	add	$s48, $s48, $s49
	add	$s48, $s48, $s50
	add	$s48, $s48, $s51
	
	
	# (A3 * B4 + A4 * B3)[0..31] >> 32 + (A2 * B4 + A3 * B3 + A4 * B2) +
	#		+ (A1 * B4 + A2 * B3 + A3 * B2 + A4 * B1)[32..63] << 32
	add	$s123, $s46, $s47
	add	$s123, $s123, $s48	
	
	# the AB1 and AB2 don't have to be calculated because they are out of 
	# the scope for 128-bit integers. 

	# function epilogue begin
	
	# restore the old value of the $s3 and $s4 registers
	ldm	$s3, 2, -272(,$s2)
	# restore the values of the registers that were overwritten by the 
	# "__multi3" routine.
	ldm $s35, 17, -256(,$s2)
	ldm $s113, 9, -120(,$s2)

	b	0(,$s32)
	# function epilogue end 	
	
	
# "__multi3_add" is a utility function that makes an exact sum of two 64-bit
# numbers. I decided to implement it as a separate routine because the SX's 
# "add" instruction for adding unsigned 64-bit integers is unable to detect if
# an overflow has occured. Because of this, it doesn't properly calculate a sum
# of very large numbers (for example, 
# "add 0xFFFFFFFE00000001, 0xFFFFFFFE00000001" gives us FFFFFFFC00000002 
# instead of 1FFFFFFFC00000002).
#
# Since we didn't want for the functions in "lib1funcs.asm" to have their own
# stack (this makes thier implementation a little simpler and their execution
# a little faster), the "__multi3_add" uses a different calling convention from
# the general SX convention:
#	- the return address is stored in the $s117 register.
#	- the 64-bit operand A is stored in the $s118 register.
#	- the 64-bit operand B is stored in the $s119 register.
#	- the result (A+B) is stored in the $s120 register. 
#	- the cary bit is stored in the $s121 register. 
#
# WARNING: the "__multi3_add" routine doesn't do copies of values for the
# registers it uses (i.e. registers $s113-$s121). The caller that uses 
# "__multi3_add" has to save those registers by itself.

__multi3_add:
	and	$s116, $s118, (32)0
	and	$s115, $s119, (32)0
	add	$s116, $s116, $s115
	srl	$s115, $s116, 32
	srl $s114, $s118, 32
	srl $s113, $s119, 32
	add	$s115, $s115, $s114
	add $s115, $s115, $s113
	srl	$s121, $s115, 32
	sll	$s115, $s115, 32
	and $s120, $s116, (32)0
	or	$s120, $s120, $s115
	b	0(,$s117)
