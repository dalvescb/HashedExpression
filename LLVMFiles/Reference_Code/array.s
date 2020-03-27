	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15
	.globl	_add                    ## -- Begin function add
	.p2align	4, 0x90
_add:                                   ## @add
## %bb.0:                               ## %entry
                                        ## kill: def $esi killed $esi def $rsi
                                        ## kill: def $edi killed $edi def $rdi
	leal	(%rdi,%rsi), %eax
	retq
                                        ## -- End function

.subsections_via_symbols
