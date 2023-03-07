.286
.model tiny
.code 
org 100h

Start:              ;mov data_s, ds 

                    cli    ; prohibit ints 
                    xor bx, bx     ; int table at the beginning of mem 
                    mov es, bx    
                    mov bx, 4*9    ; 09h, 4 bytes for each (adrr, segment)
                    

                    mov ax, es:[bx]                ; relative adress of default handler 
                    mov Old09Ofs, ax               ; memorizing relative addres default handler 
                    mov es:[bx], offset New09      ; rewriting adress of our new handler 
                    
                    
                    mov ax, es:[bx+2]              ; segment of default handler 
                    mov Old09Seg, ax               ; memorizing default segment 

                    mov ax, cs 
                    mov es:[bx+2], ax 
                    sti    ; allow ints


Next:               in al, 60h
                    cmp al, 1
                    jne Next

                    mov ax, 3100h ; terminate and stay resident
                    mov dx, offset EOP
                    shr dx, 4     ; size of prog in 16 bytes paragraphs, therefore div by 16 
                    inc dx 
                    int 21h      

;------------------------------------------------------------
;New09h
;New interapt handler
;------------------------------------------------------------
New09               proc
                    push ax bx cx dx si di es ds
                    
                    push bp
                    mov bp, sp

                    in al, 60h         ; 
                    cmp al, 2 ; key '1'
                    jne check            ; if '1' - show regs

                    mov ax, cs      ; setting cs 
                    mov ds, ax  

                    mov al, base_y
                    mov y_cord, al

                    mov bx, 0b800h
                    mov es, bx

                    cmp flag, 0h
                    jne NotSave

                    call SetSaveBuff
NotSave:            mov flag, 1h 
                    call DoFrame

                    mov cx, 8 ; register counter
                    mov al, base_y
                    mov y_cord, al
                    inc y_cord

                    mov al, x_cord
                    mov reg_x, al   ;reg_x = x_cord 
                    mov al, width
                    sub al, 2d
                    add x_cord, al

                    lea si, regs
                    push si
                    
;       ---------Printing Regs----------
ShowReg:            call SetCoord       ; DI set 
                    mov ax, [bp + 16]   ; starting from ax 
                    call HexVid

                    pop si
                    call writeReg
                    push si

                    sub bp, 2
                    inc y_cord
                    Loop ShowReg
;       --------------------------------
                    pop si
                    mov al, reg_x       ; x_cord = reg_x
                    mov x_cord, al

                    jmp default 


;       -----------Talking to PPI-----------
;                    in al, 61h
;                    or al, 80h    ;1000 0000
;                    out 61h, al                      ;blinking with most significant bit 
;                    and al, not 80h; 0111 1111
;                    out 61h, al
;       ----------Talking to the INTC-------
;                    mov al, 20h       
;                    out 20h, al 
;       ------------------------------------
check:         cmp al, 3 ; key '2's
               jne default ; if '2' - clear videoseg
               mov ax, cs      ; setting cs 
               mov ds, ax  

               mov al, base_y
               mov y_cord, al

               mov bx, 0b800h
               mov es, bx

               call DrawSaveBuff
               mov flag, 0h

default:       pop bp 
               pop ds es di si dx cx bx ax  
                    
                    db 0eah   ; far jmp === jump Old09Ofs:[Old09Seg]
Old09Ofs            dw 0      ; jumping directly to the default handler  
Old09Seg            dw 0 

                    iret                    
                    endp
;------------------------------------------------------------
;------------------------------------------------------------
;SetSaveBuff
;Expects:
;------------------------------------------------------------
SetSaveBuff     proc
                lea si, savebuff 
                xor cx, cx 
                mov cl, height
Save:           call SetCoord        ; di is set
                push cx
                mov cl, width

SaveLine:       mov ax, es:[di]      ; from videoSeg to saveBuff
                mov [si], ax
                add si, 2
                add di, 2
                loop SaveLine

                pop cx
                inc y_cord
                loop Save


                mov al, base_y
                mov y_cord, al
                
                ret 
                endp
;------------------------------------------------------------
;------------------------------------------------------------
;DrawSaveBuff
;Expects: height, width, y_cord, x_cord, base_y
;Calls: SetCoord()
;------------------------------------------------------------
DrawSaveBuff    proc
                lea si, savebuff
                xor cx, cx  
                mov cl, height
                
DrawBuff:       call SetCoord        ; di is set
                push cx
                mov cl, width

DrawLine:       mov ax, [si]      ; from saveBuff to Videoseg
                mov es:[di], ax
                add si, 2
                add di, 2
                loop DrawLine

                pop cx
                inc y_cord
                loop DrawBuff

                mov al, base_y
                mov y_cord, al
                
                ret 
                endp
;------------------------------------------------------------
;------------------------------------------------------------
;writeReg
;Expects: si = adrr of cur reg 
;------------------------------------------------------------
writeReg        proc
                push cx
                mov cx, 3
                mov ah, color

Write:          mov byte ptr al, [si]
                mov byte ptr es:[di], al
                mov byte ptr es:[di+1], ah
                sub di, 2
                inc si
                loop Write

                pop cx 
                ret 
                endp
;------------------------------------------------------------
;------------------------------------------------------------
;HexVid
;Destroys: DX
;Expects: AX - num to display, DI - relative video adress  
;------------------------------------------------------------
HexVid			proc
                push cx 
                xor dx, dx    ; dx = 0
				mov dl, 1111b ; (dl = 0000 1111)
                mov cx, 4
				
Lp1:	        and dl, al		;analising last byte (dl = 0000 XXXX)
                cmp dl, 9
                jbe Digit

                add dl, 7d      ; it's letter

Digit:          add dl, 30h 

                mov byte ptr es:[di], dl
                mov dl, color
				mov byte ptr es:[di+1], dl ; 

				shr ax, 4		;next 4 bits of ax 
				sub di, 2
                mov dl, 1111b

				loop Lp1
                pop cx 
				ret
				endp
;------------------------------------------------------------
;-------------------------------------------------------
;DoFrame
; Expects: y_cord, height, es = 0b800h
; Destroys: BX, Cl, SI
; Calls: 
;        SetCoord()
;        Draw()
;-------------------------------------------------------
DoFrame         proc

                xor cx, cx 
                
                lea si, frame0    ; SI is set for top left symb 

                call SetCoord     ; DI is set for the 1st line 
                
                call Draw         ; Drawing first line 
                
                mov cl, height
                sub cl, 2d

                inc y_cord
                call SetCoord     ; DI is set for the 2nd line

Side:           call Draw         ; Drawing middle lines 
                sub si, 3
                inc y_cord
                call SetCoord     ; Di is set for [ 3rd  - height(th)] line              
                loop Side
                
                add si, 3
                call Draw         ; Drawing last line 

                ret
                endp
;-------------------------------------------------------
;-------------------------------------------------------
;Draw
;Expects:  DI = relative adress of video seg ES:[DI]        (SetCoord) for stosw
;          SI = adress of top left symbol    DS:[SI]        (SetType)  for lodsb
;          color, width
;Returns:  Draw() draws line with 3 symbols
;          left symb, middle symb * (width - 2), right symb  
;-------------------------------------------------------
Draw            proc
                push cx
                xor cx, cx 

                mov ah, color
                lodsb            ; ax is set   (left symb)
                stosw            ; ax -> es:di 

                mov cl, width    ; setting counter for rep 
                sub cx, 2
                
                lodsb            ; ax is set   (middle symb)
                rep stosw        ; ax -> es:di 
                
                lodsb            ; ax is set   (right symb)  
                stosw            ; ax -> es:di           
                
                pop cx
                ret
                endp
;-------------------------------------------------------
;-------------------------------------------------------
; SetCoord
; Expects: x_cord, y_cord
; Destroys: ax, bx
; Returns: DI = relative adress of video seg ES:[DI]   
;-------------------------------------------------------
SetCoord        proc

                xor ax, ax
                mov bx, 160d 
                mov al, y_cord
                mul bx        ; ax = y * 160d 
                mov di, ax    ; di = y * 160d
                xor ax, ax
                mov bx, 2d
                mov al, x_cord
                mul bx        ; ax = x * 2d
                add di, ax    ; di = y * 160d + x * 2d

                ret
                endp
;-------------------------------------------------------
.data

flag db 0h 
reg_x db ? 
x_cord db 65   
y_cord db ?
base_y db 0 
width db 10 
height db 15 
color db 02h
frame0 db 0c9h, 0cdh, 0bbh, 0bah, 0h, 0bah, 0c8h, 0cdh, 0bch   ; two lines, empty
regs db ':xa:xb:xc:xd:is:id:se:sd'

saveBuff db 600 dup(?)   ; width * height


EOP:
end                 Start 