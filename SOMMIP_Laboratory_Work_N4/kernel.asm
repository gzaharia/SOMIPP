name "kernel"

;directives             
       
        

                            
; org 7c00h
org 0000h      
push cs
pop ds       
#make_bin# 
jmp start

        macro putch c
            mov ah,0eh
            mov al,c
            int 10h
        endm putch
        
        PUT_NEWLINE MACRO
            PUTC 0ah
            PUTC 0dh
        ENDM
             
        macro get_pos
            mov ah, 03h
            int 10h
            
        endm get_pos 
        
        macro set_pos y,x
            mov dh,y
            mov dl,x
            mov ah, 02h
            int 10h
        endm set_pos

        CLEAR_SCREEN PROC NEAR
            PUSH    AX      ; store registers...
            PUSH    DS      ;
            PUSH    BX      ;
            PUSH    CX      ;
            PUSH    DI      ;

            MOV     AX, 40h
            MOV     DS, AX  ; for getting screen parameters.
            MOV     AH, 06h ; scroll up function id.
            MOV     AL, 0   ; scroll all lines!
            MOV     BH, 07  ; attribute for new lines.
            MOV     CH, 0   ; upper row.
            MOV     CL, 0   ; upper col.
            MOV     DI, 84h ; rows on screen -1,
            MOV     DH, [DI] ; lower row (byte).
            MOV     DI, 4Ah ; columns on screen,
            MOV     DL, [DI]
            DEC     DL      ; lower col.
            INT     10h

            ; set cursor position to top
            ; of the screen:
            MOV     BH, 0   ; current page.
            MOV     DL, 0   ; col.
            MOV     DH, 0   ; row.
            MOV     AH, 02
            INT     10h

            POP     DI      ; re-store registers...
            POP     CX      ;
            POP     BX      ;
            POP     DS      ;
            POP     AX      ;

            RET
        CLEAR_SCREEN ENDP

;***************************************************************
; Compare 2 null terminated strings from DI and SI. The result is written to AL:
;   0 - not equal
;   1 - equal
DEFINE_STREQU     MACRO
LOCAL skip_proc_strequ, str_loop, strs_are_not_equal, strs_are_equal, end_of_proc

; protect from wrong definition location:
JMP     skip_proc_strequ

strequ PROC NEAR
    push si
    push di

    str_loop:
        mov al, b. [di]
        cmp b. [si], al
        jne strs_are_not_equal

        cmp b. [si], 0
        je strs_are_equal

        inc si
        inc di
        jmp str_loop

    strs_are_not_equal:
        mov al, 0
        jmp end_of_proc

    strs_are_equal:
        mov al, 1

    end_of_proc:
    pop di
    pop si

    RET
strequ ENDP

skip_proc_strequ:
DEFINE_CLEAR_SCREEN     ENDM
;***************************************************************

get_cursor_pos MACRO
    mov ah, 03h
    mov bh, 0
    int 10h
ENDM

; this macro prints a char in AL and advances
; the current cursor position:
PUTC    MACRO   char
    PUSH    AX
    MOV     AL, char
    MOV     AH, 0Eh
    INT     10h     
    POP     AX
ENDM

; Extract the words from [DI] to [SI]
define_extract_words MACRO
    LOCAL last_char_was_sep
    LOCAL cmd_loop, its_separator, its_not_separator, end_of_sep_control
    LOCAL cmd_loop_end

    last_char_was_sep db 0

extract_words PROC
    pusha

    mov last_char_was_sep, 1
    mov w. [si], 0
    cmd_loop:
        cmp b. [di], 0
        je cmd_loop_end

        ; Check if it's a separator
        ; - Check for space
        cmp b. [di], ' '
        je its_separator

        ; - Check for tab
        cmp b. [di], 9
        jne its_not_separator

        its_separator:
            ; Put 0 instead of separators
            mov b. [di], 0 
            
            mov last_char_was_sep, 1
            jmp end_of_sep_control

        its_not_separator:
            cmp last_char_was_sep, 1
            jne end_of_sep_control

            mov last_char_was_sep, 0
            
            mov ax, di
            mov [si], ax
            
            add si, 2
            mov w. [si], 0

        end_of_sep_control:

        inc di
        jmp cmd_loop

    cmd_loop_end:

    popa
    ret
extract_words ENDP
define_extract_words ENDM

;***************************************************************
; this macro defines a procedure to print a null terminated
; string at current cursor position, receives address of string in DS:SI
DEFINE_PRINT_STRING     MACRO
LOCAL   next_char, printed, skip_proc_print_string

; protect from wrong definition location:
JMP     skip_proc_print_string

PRINT_STRING PROC NEAR
PUSH    AX      ; store registers...
PUSH    SI      ;

next_char:      
    MOV     AL, [SI]
    CMP     AL, 0
    JZ      printed
    INC     SI
    MOV     AH, 0Eh ; teletype function.
    INT     10h
    JMP     next_char
printed:

POP     SI      ; re-store registers...
POP     AX      ;

RET
PRINT_STRING ENDP

skip_proc_print_string:

DEFINE_PRINT_STRING     ENDM
;***************************************************************

DEFINE_GET_STRING       MACRO
LOCAL   empty_buffer, wait_for_key, skip_proc_get_string
LOCAL   exit, add_to_buffer, screen_width
LOCAL   its_not_on_the_edge_of_the_screen, edge_of_the_screen_endif
LOCAL   _process_pos_at_x_0_exit, do_the_beep

; protect from wrong definition location:
JMP     skip_proc_get_string
    screen_width db 0

_process_pos_at_x_0 PROC NEAR
    pusha
        get_cursor_pos
        cmp dl, 0

        jne _process_pos_at_x_0_exit

        ; Goto row - 1, screen_width
        dec     DH ; decrease the row
        MOV     DL, screen_width
        MOV     AH, 02h
        INT     10h

    _process_pos_at_x_0_exit:
    popa
    ret
_process_pos_at_x_0 ENDP

GET_STRING      PROC    NEAR
pusha

; Get the screen_width from parameters
mov     screen_width, bl
MOV     CX, 0      ; char counter.

CMP     DX, 1      ; buffer too small?
JBE     empty_buffer        ;

DEC     DX        ; reserve space for last zero.


;============================
; loop to get and processes key presses:

wait_for_key:

MOV     AH, 0      ; get pressed key.
INT     16h

CMP     AL, 13    ; 'RETURN' pressed?
JZ      exit


CMP     AL, 8               ; 'BACKSPACE' pressed?
JNE     add_to_buffer
JCXZ    do_the_beep         ; nothing to remove!
DEC     CX
DEC     DI

; If it's at the edge of the screen (right)
    pusha
    
    ; Get cursor pos
    get_cursor_pos

    ; if x == screen_width
    cmp dl, screen_width

    popa
    jne its_not_on_the_edge_of_the_screen

    pusha
    get_cursor_pos

    ; Decrease x
    dec dl

    ; Set cursor pos to screen_width - 1
    mov ah, 02h
    int 10h

    ; clear position.
    PUTC    ' '
    PUTC    8

    ; Set cursor pos to screen_width - 1
    mov ah, 02h
    int 10h


    popa

    jmp edge_of_the_screen_endif

    its_not_on_the_edge_of_the_screen:
    PUTC 8         ; backspace.
    PUTC ' '         ; clear position.
    PUTC 8         ; backspace again.
    
    edge_of_the_screen_endif:

    call _process_pos_at_x_0

JMP     wait_for_key

add_to_buffer:

    CMP     CX, DX    ; buffer is full?

    JAE     do_the_beep    ; if so wait for 'BACKSPACE' or 'RETURN'...

    MOV     [DI], AL
    INC     DI
    INC     CX
    
    ; print the key:
    MOV     AH, 0Eh
    INT     10h

    call _process_pos_at_x_0
JMP     wait_for_key

do_the_beep:
    ; beep
    jmp wait_for_key

;============================

exit:

; terminate by null:
MOV     [DI], 0

empty_buffer:

popa
RET
GET_STRING      ENDP


skip_proc_get_string:

DEFINE_GET_STRING       ENDM
;***************************************************************

define_extract_words
DEFINE_GET_STRING
DEFINE_PRINT_STRING
DEFINE_STREQU

   
start:  

            
        push cs
        pop ds               
           
        start_printing:
            ; Print prompt
            get_cursor_pos
            mov al, 0011b
            mov bh, 0
            mov bl, 0
            mov cx, (prompt_end - offset prompt) >> 1
            lea bp, prompt
            mov ah, 13h
            int 10h

            xor di, di 

        printing:
                mov ah,00h
                int 16h 
                
                cmp al,08h
                je Backspace 
                
                cmp al,0Dh
                je Enter 
                
                cmp di,256
                ; jge call beep
                je printing 
                
                mov ah, 0eh
                int 10h     ;output the current char
                
                mov Buff[di], al
                inc di
                mov Buff[di], 0

                
                jmp printing
                
        
        Backspace:  
                test di, di
                ;jz call beep    
                jz start_printing
                mov ah, 03h
                mov bh, 0h ; set page number
                int 10h ; get cursor position and size
            
                
                test dl, dl ; dh - number of columns,  test sets ZF if dl is zero
                mov ah, 02h
                jnz not_at_the_edge
                    mov dl, 80 ; move cursor to the right edge
                    dec dh ; and one row above
                not_at_the_edge: 
                dec dl
                int 10h
                mov ah, 0ah 
                mov al, ' '
                mov cx, 1
                int 10h ; writes space at the cursor position without advancing the cursor
                dec di    
                
                jmp printing 
             
        
        Enter:
                cmp di, 0
                je start_printing
                get_pos
                
                putch 0Ah
                get_pos
                set_pos dh, 0

                pusha 
                lea di,Buff
                lea si,argv_buff
                call extract_words

                ; help .........................................................
                mov di, argv_buff[0]
                lea si, cmd_str_help
                call strequ

                cmp al, 0
                je not_help_cmd
                    lea si, help_stuff
                    call PRINT_STRING
                    jmp end_of_cmd_processing

                not_help_cmd:


                ; about ........................................................
                mov di, argv_buff[0]
                lea si, cmd_str_about
                call strequ

                cmp al, 0
                je not_about_cmd
                    lea si, about_stuff
                    call PRINT_STRING
                    jmp end_of_cmd_processing

                not_about_cmd:


                ; ascii ........................................................
                mov di, argv_buff[0]
                lea si, cmd_str_ascii
                call strequ

                cmp al, 0
                je not_ascii_cmd
                    pusha
                    xor si, si

                    ascii_print_loop:
                        mov ax, si
                        mov ah, 0Eh
                        int 10h

                        inc si

                        cmp si, 256
                        jne ascii_print_loop
                    popa
                    jmp end_of_cmd_processing

                not_ascii_cmd:


                ; clear ........................................................
                mov di, argv_buff[0]
                lea si, cmd_str_clear
                call strequ

                cmp al, 0
                je not_clear_cmd
                    call clear_screen
                    jmp end_of_cmd_processing

                not_clear_cmd:


                ; beep .........................................................
                mov di, argv_buff[0]
                lea si, cmd_str_beep
                call strequ

                cmp al, 0
                je not_beep_cmd
                    putc 7
                    jmp end_of_cmd_processing

                not_beep_cmd:


                ; reboot .......................................................
                mov di, argv_buff[0]
                lea si, cmd_str_reboot
                call strequ

                cmp al, 0
                je not_reboot_cmd
                    int 19h
                    jmp end_of_cmd_processing

                not_reboot_cmd:


                ; echo .........................................................
                mov di, argv_buff[0]
                lea si, cmd_str_echo
                call strequ

                cmp al, 0
                je not_echo_cmd
                    pusha
                    lea di, argv_buff
                    ; Ignore argv[0]
                    add di, 2

                    echo_argv_loop:
                        cmp w. [di], 0
                        je echo_argv_loop_end

                        mov si, w. [di]
                        call print_string
                        putc ' '

                        add di, 2
                        jmp echo_argv_loop

                    echo_argv_loop_end:

                    put_newline
                    popa
                    jmp end_of_cmd_processing

                not_echo_cmd:


                ; unknown cmd ..................................................
                unknown_cmd:
                    mov si, argv_buff[0]
                    call print_string

                    putch ':'
                    putch ' '
                    lea si, unknown_cmd_str
                    call print_string
                end_of_cmd_processing:
                popa
                
                ; new line
                mov ah, 0eh
                mov al, 0Dh
                int 10h
                mov al, 0Ah
                int 10h
            
                jmp start_printing

; data            
endl equ 0ah, 0dh

Buff db 512 dup(0)
argv_buff dw 128 dup(0)

cmd_str_help    db "help", 0
cmd_str_about   db "about", 0
cmd_str_ascii   db "ascii", 0
cmd_str_clear   db "clear", 0
cmd_str_beep    db "beep", 0
cmd_str_reboot  db "reboot", 0
cmd_str_echo    db "echo", 0

help_stuff      db "Available commands: ", endl
                db "about", endl
                db "ascii - print the ascii table", endl
                db "clear - clear the screen", endl
                db "beep - should produce a sound", endl
                db "reboot - restart the system", endl
                db "echo - print the arguments", endl
                db 0

about_stuff     db "This OS was made by Zaharia Gabi FAF161", 0

prompt  db '$', 04h,
        db '>', 02h,
        db ' ', 07h
prompt_end:

unknown_cmd_str db "unknown cmd", 0 