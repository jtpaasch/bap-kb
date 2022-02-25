    global main:function (main.end - main)

; -------------------------------------------
    section .text
; -------------------------------------------

main:
    mov rdi, 3
    mov rax, 60
    syscall
.end:
