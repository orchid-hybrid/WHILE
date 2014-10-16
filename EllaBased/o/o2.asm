section .bss
        x       resq    1
        c1      resq    1
        c2      resq    1
        c3      resq    1
        c4      resq    1
        c5      resq    1

section .text
        global _start

_start:        
        mov     qword [x],4

;;; Tested and works
;;; cat <( echo 'break o2.asm:40' ; echo 'r' ; echo 'print x' ) | gdb o2 
    mov  rax,[x]
    mov  [c2],rax
    mov  rax,0
    cmp  [c2],rax
    setle  bl
    dec  bl
    jz  lc0
    mov qword  [c3],1
    mov  rax,[x]
    add  rax,[c3]
    mov  [x],rax
    jmp  lc1
lc0:
    mov qword  [c4],2
    mov  rax,[x]
    add  rax,[c4]
    mov  [x],rax
    mov qword  [c5],3
    mov  rax,[x]
    add  rax,[c5]
    mov  [x],rax
lc1:

        
        ;; exit
        xor rdi,rdi
        mov rax,60
        syscall
