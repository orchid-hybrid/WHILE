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
        mov     qword [y],3

;;; Tested and works
;;; cat <( echo 'break o3.asm:40' ; echo 'r' ; echo 'print z' ) | gdb o3
    mov qword  [z],1
lc0:
    mov  rax,[x]
    mov  [c2],rax
    mov  rax,0
    cmp  [c2],rax
    setle  bl
    dec  bl
    jz  lc1
    mov  rax,[y]
    mov  [c3],rax
    mov  rax,[z]
    mul qword  [c3]
    mov  [z],rax
    mov qword  [c4],1
    mov  rax,[x]
    sub  rax,[c4]
    mov  [x],rax
    jmp  lc0
lc1:

        
        ;; exit
        xor rdi,rdi
        mov rax,60
        syscall
