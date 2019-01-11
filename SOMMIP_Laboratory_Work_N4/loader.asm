name "loader"

#make_boot# 

org 7c00h


mov     ax, 07c0h
mov     ss, ax
mov     sp, 03feh


xor     ax, ax
mov     ds, ax


mov     ah, 00h
mov     al, 03h
int     10h

;priting
lea     si, msg
call    print_string   

;when key 1 is pressed
key1:
    mov     ah, 0
    int     16h       

    cmp al, 31h ; compare if the pressed key is '1'
    je loadkernel

    lea si, err2
    call print_string 

    jmp key1
    

loadkernel:
    mov     ah, 02h
    mov     al, 10  
    mov     ch, 2   
    mov     cl, 3   
    mov     dh, 0   
    mov     bx, 0800h   
    mov     es, bx
    mov     bx, 0C700h
    int     13h

; integrity check:   
integritycheck:
cmp     es:[0C700h],0E9h
je     integrity_check_ok

; integrity check error
lea     si, err
call    print_string

; wait for any key...
mov     ah, 0
int     16h

;reboot
mov     ax, 0040h
mov     ds, ax
mov     w.[0072h], 0000h
jmp	0ffffh:0000h	     

integrity_check_ok:
    ;print message 2
    lea si, msg2
    call print_string

; wait for key 2 to be pressed
waitforkey2:
    mov     ah, 0
    int     16h       

    cmp al, 32h ; compare if the pressed key is '2'
    je passcontrol

    lea si, err2
    call print_string 

    jmp waitforkey2
    
passcontrol:
jmp 0800h:0C700h    


print_string proc near
push    ax      
push    si      
next_char:      
        mov     al, [si]
        cmp     al, 0
        jz      printed
        inc     si
        mov     ah, 0eh
        int     10h
        jmp     next_char
printed:
pop     si      
pop     ax      
ret
print_string endp


msg  db "BootLoader developed by student Zaharia Gabriel", 0Dh, 0Ah
     db "Press 1 to start loading the kernel to RAM", 0Dh, 0Ah, 0   
     
msg2 db "Kernel was loaded into RAM address 0800h:0C700h", 0Dh, 0Ah
     db "Press 2 to start Kernel", 0Dh, 0Ah, 0     
     
err2 db "Try again", 0Dh, 0Ah, 0    
     
err  db "Invalid data. Integrity Check Failed.", 0Dh,0Ah
     db "System will reboot now. Press any key...", 0