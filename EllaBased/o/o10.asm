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
        mov     qword [x],77

;;; Tested and works
;;; cat <( echo 'break o10.asm:38' ; echo 'r' ; echo 'print y' ) | gdb o10
    mov qword  [y],0
lc0:
    mov qword  [c2],1
    mov  rax,[x]
    cmp  [c2],rax
    setg  bl
    dec  bl
    jz  lc1
    mov  rax,[x]
    mov  [c3],rax
    mov  rax,[y]
    add  rax,[c3]
    mov  [y],rax
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
