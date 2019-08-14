	.text
	.file	"hello.pcl"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %main.entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	%edi, 4(%rsp)
	movl	$1, i(%rip)
	movl	$.Lstr.main.entry.2, %edi
	movl	$.Lstr.main.entry.3, %esi
	movl	$.Lstr.main.entry.4, %edx
	xorl	%eax, %eax
	callq	printf
	movl	4(%rsp), %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lstr.main.entry.2,@object # @str.main.entry.2
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lstr.main.entry.2:
	.asciz	"%s%s"
	.size	.Lstr.main.entry.2, 5

	.type	.Lstr.main.entry.3,@object # @str.main.entry.3
.Lstr.main.entry.3:
	.asciz	"hello"
	.size	.Lstr.main.entry.3, 6

	.type	.Lstr.main.entry.4,@object # @str.main.entry.4
.Lstr.main.entry.4:
	.asciz	"\n"
	.size	.Lstr.main.entry.4, 2


	.section	".note.GNU-stack","",@progbits
