@ ----------------------------------------------
    .text

@ ----------------------------------------------
@ The MAIN function
@ ----------------------------------------------
    .global main
    .type main, %function
main:
    mov r0, #5
    add r0, r0, #7
    bx lr
    .size main, .-main
