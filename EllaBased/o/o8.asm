section .bss
        x       resq    1
        y       resq    1
        z       resq    1
        c1      resq    1
        c2      resq    1
        c3      resq    1
        c4      resq    1
        c5      resq    1

section .text
        global _start

_start:        
        mov     qword [x],5

;;; Tested and works
;;; cat <( echo 'break o8.asm:43' ; echo 'r' ; echo 'print y' ) | gdb o8
    mov  rax,[x]
    mov  [z],rax
    mov qword  [y],1
lc0:
    mov  rax,[z]
    mov  [c2],rax
    mov  rax,0
    cmp  [c2],rax
    setne  bl
    dec  bl
    not  bl
    test  bl,bl
    jz  lc1
    mov  rax,[z]
    mov  [c3],rax
    mov  rax,[y]
    mul qword  [c3]
    mov  [y],rax
    mov qword  [c4],1
    mov  rax,[z]
    sub  rax,[c4]
    mov  [z],rax
    jmp  lc0
lc1:

        
        ;; exit
        xor rdi,rdi
        mov rax,60
        syscall
