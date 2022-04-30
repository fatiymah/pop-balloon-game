[org 0x100]

start:
  call clrscr
  xor ax, ax 
 mov es, ax ; point es to IVT base 
 mov ax, [es:9*4] 
 mov [oldisr], ax ; save offset of old routine 
 mov ax, [es:9*4+2] 
 mov [oldisr+2], ax ; save segment of old routine 
 cli ; disable interrupts 
 mov word [es:9*4], kbisr ; store offset at n*4 
 mov [es:9*4+2], cs ; store segment at n*4+2 
 sti ; enable interrupts 
  xor ax, ax 
 mov es, ax ; point es to IVT base 
 cli ; disable interrupts 
 mov word [es:8*4], timer; store offset at n*4 
 mov [es:8*4+2], cs ; store segment at n*4+2 
 sti ; enable interrupts 

call up_date1
call up_date2
call up_date3
call up_date4


l2:
 call sleep
 call sleep
 call clrscr
 mov si,[char1]
 push si
 mov si,[char2]
 push si
 mov si,[char3]
 push si
 mov si,[char4]
 push si

 push ax
 push bx
 push cx
 push dx

 call floatingBalloons
 sub ax,160;
 cmp ax,158
 jnz u1
 call up_date1
u1:
  sub bx,160
  cmp bx,158
  jnz u2
  call up_date2
u2:
  sub cx,160
  cmp cx,158
  jnz u3
  call up_date3
u3:
  sub dx,160
  cmp dx,158
  jnz u4
  call up_date4
u4:
 push 0 ; push x position 
 push 0 ; push y position 
 push 0x71 ; push attribute 
 mov di, msg1
 push di ; push address of message 
 call printstr ; call the printstr subroutine
 push 65 ; push x position 
 push 0 ; push y position 

 push 0x71 ; push attribute 
 mov di, msg2
 push di ; push address of message 
 call printstr ; call the printstr subroutine
 push 0
 push 75
 mov di,[score]
 push di
 call printnum
 push 0
 ;push 9
 ;//mov di,[time]
 ;push di
;// call printnum
 call sleep
 jmp l2


 mov ax, [oldisr] ; read old offset in ax 
 mov bx, [oldisr+2] ; read old segment in bx 
 cli ; disable interrupts 
 mov [es:9*4], ax ; restore old offset from ax 
 mov [es:9*4+2], bx ; restore old segment from bx 
 sti ; enable interrupts 

mov ax,0x4C00
int 0x21
floatingBalloons:
 push bp
 mov bp,sp
 push ax
 push bx
 push cx
 mov cx,[attribute2]
 push cx
 mov bx,[bp+12]   ;alphabet
 push bx
 mov ax,[bp+10]
 sub ax,22
 push ax
;location
 call drawBalloon
 mov cx,[attribute1]
 push cx
 mov bx,[bp+14]   ;alphabet
 push bx
  mov ax,[bp+8]
  sub ax,62
  ;push 0x41
  push ax
  call drawBalloon
  mov cx,[attribute3]
 push cx
  mov bx,[bp+16]   ;alphabet
  push bx
  mov ax,[bp+6]
  sub ax,102
  ;push 0x41
  push ax
  call drawBalloon
  mov cx,[attribute4]
 push cx
  mov bx,[bp+18]   ;alphabet
  push bx
  mov ax,[bp+4]
  sub ax,142
  ;push 0x41
  push ax
  call drawBalloon
  pop cx
  pop bx
  pop ax
  pop bp
  ret 16
end:
            mov ax, 0x4c00
			int 21h

drawBalloon:
  push bp
  mov bp,sp
  push dx
  push bx
  push ax
  push es
  push di
  push cx
  mov ax,0xB800
  mov es,ax
  mov di,[bp+4]
  mov bx,3
l1:
  mov cx,3
  mov ah,[bp+8]
  mov al,0x20
  rep stosw
  add di,154
  dec bx
  jne l1
  mov al,[bp+6]
  mov di,[bp+4]
  add di,162
  stosw
  pop cx
  pop di
  pop es
  pop ax
  pop bx
  pop dx
  pop bp
  ret 6

clrscr:
    push ax
    push es
    push di
    push cx
    mov ax, 0xb800
	mov es, ax				;Loading the video memory

	xor di,di

	mov ax,0x0720
	mov cx,2000

	cld
                              
	rep stosw
    pop cx
	pop di
    pop es
    pop ax
	ret
up_date1:
 push si
 mov ax,3998
 mov si,[char4]
 mov byte[attribute2],0x20
 call updatechar
 
 mov [char4],si

 
 pop si
 
 ret
up_date2:
 push si
 mov bx,3998
 add bx,640
 add bx,320
 mov si,[char3]
 mov byte[attribute1],0x20
 call updatechar
 
 mov [char3],si
 pop si
 ret
up_date3:
 push si
 mov cx,3998
 add cx,640
 mov si,[char2]
 mov byte[attribute3],0x20
 call updatechar

 mov [char2],si
 pop si
 ret
up_date4:
 push si
 mov dx,3998
 add dx,640
 add dx,640
 mov si,[char1]
 ;............
 mov byte[attribute4],0x20
 call updatechar

 mov [char1],si
 pop si
 ret
updatechar:
   push dx
   push si
   pop dx
   add dl,2
   cmp dl,122
   jng nextt
   mov dl,97
nextt:
   push dx
   pop si
   pop dx
   ret

printnum: 
push bp 
 mov bp, sp 
 push es 
 push ax 
 push bx 
 push cx
 push dx 
 push di 
 mov di, 80 ; load di with columns per row 
 mov ax, [bp+8] ; load ax with row number 
 mul di ; multiply with columns per row 
 mov di, ax ; save result in di 
 add di, [bp+6] ; add column number 
 shl di, 1 ; turn into byte count 
 add di, 8 ; to end of number location 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 mov ax, [bp+4] ; load number in ax 
 mov bx, 10 ; use base 16 for division 
 mov cx, 4 ; initialize count of digits 
nextdigit: mov dx, 0 ; zero upper half of dividend 
 div bx ; divide by 10 
 add dl, 0x30 ; convert digit into ascii value 
 cmp dl, 0x39 ; is the digit an alphabet 
 jbe skipalpha ; no, skip addition 
 add dl, 7 ; yes, make in alphabet code 
skipalpha: mov dh, 0x71 ; attach normal attribute 
 mov [es:di], dx ; print char on screen 
 sub di, 2 ; to previous screen location 
 loop nextdigit ; if no divide it again 
 pop di 
 pop dx 
 pop cx 
 pop bx 
 pop ax 
 pop es 
 pop bp 
 ret 6

printstr: push bp 
 mov bp, sp 
 push es 
 push ax 
 push cx 
 push si 
 push di 
 push ds ; push segment of string 
 mov ax, [bp+4] 
 push ax ; push offset of string 
 call strlen ; calculate string length
 cmp ax, 0 ; is the string empty 
 jz exit ; no printing if string is empty
 mov cx, ax ; save length in cx 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 mov al, 80 ; load al with columns per row 
 mul byte [bp+8] ; multiply with y position 
 add ax, [bp+10] ; add x position 
 shl ax, 1 ; turn into byte offset 
 mov di,ax ; point di to required location 
 mov si, [bp+4] ; point si to string 
 mov ah, [bp+6] ; load attribute in ah 
 cld ; auto increment mode 
nextchar: lodsb ; load next char in al 
 stosw ; print char/attribute pair 
 loop nextchar ; repeat for the whole string 
exit: pop di 
 pop si 
 pop cx 
 pop ax 
 pop es 
 pop bp 
 ret 8

strlen: push bp 
 mov bp,sp 
 push es 
 push cx 
 push di 
 les di, [bp+4] ; point es:di to string 
 mov cx, 0xffff ; load maximum number in cx 
 xor al, al ; load a zero in al 
 repne scasb ; find zero in the string 
 mov ax, 0xffff ; load maximum number in ax 
 sub ax, cx ; find change in cx 
 dec ax ; exclude null from length 
 pop di 
 pop cx 
 pop es 
 pop bp 
 ret 4

 kbisr:
 jmp isr
 popballon1:
   add word[score],1;
   mov byte[cs:attribute4],0xA0;
   jmp exitISR
 popballon2:
   add word[score],1;
   mov byte[cs:attribute3],0xA0;
   jmp exitISR
 popballon3:
   add word[score],1;
   mov byte[cs:attribute1],0xA0;
   jmp exitISR
 popballon4:
   add word[score],1;
   mov byte[cs:attribute2],0xA0;
   jmp exitISR

isr:
 push ax 
 push es 
 push bx
 push dx

 mov ax, 0xb800 
 mov es, ax ; point es to video memory 
 in al, 0x60 ; read a char from keyboard port 
 mov bx,-1
findKey:
 add bx,2
 cmp bx,53
 je exitISR
 
 cmp al,[key_arr+bx]
 jne findKey
 sub bx,1
 ;[key_arr+bx] contain ascii of pressed button
 mov dl,[cs:char1]
 cmp dl,[cs:key_arr+bx]
 je popballon1
 mov dl,[cs:char2]
 cmp dl,[cs:key_arr+bx]
 je popballon2
  mov dl,[cs:char3]
 cmp dl,[cs:key_arr+bx]
 je popballon3
  mov dl,[cs:char4]
 cmp dl,[cs:key_arr+bx]
 je popballon4
 ; mov al, 0x20 
 ; out 0x20, al
exitISR:
 pop dx
 pop bx
 pop es 
 pop ax 
 jmp far [cs:oldisr] ; call the original ISR 
 ; iret 
sleep:
                        push cx
                        mov cx, 0xFFFF
               
    delay: 
                         loop delay
                         mov cx,0xFFFF
                    l7:
                        loop l7
                        mov cx,0xFFFF

                    l8:
                       loop l8
                         ;sub word[ddd],2
                         ;jnz delay
                         pop cx
                         ret



timer: push ax 
 inc word [cs:tickcount]; increment tick count 
 mov ax,[cs:tickcount];
 cmp ax,18
 jne prnt
 inc word [cs:seconds];
 cmp word[seconds],121;
 jz end
 mov word[cs:tickcount],0;
prnt:
 push word [cs:seconds] 
 call _printnum ; print tick count 
 mov al, 0x20 
 out 0x20, al ; end of interrupt 
 pop ax 
 iret 

 ; subroutine to print a number at top left of screen 
; takes the number to be printed as its parameter 
_printnum: push bp 
 mov bp, sp 
 push es 
 push ax 
 push bx 
 push cx 
 push dx 
 push di 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 mov ax, [bp+4] ; load number in ax 
 mov bx, 10 ; use base 10 for division 
 mov cx, 0 ; initialize count of digits 
_nextdigit: mov dx, 0 ; zero upper half of dividend 
 div bx ; divide by 10 
 add dl, 0x30 ; convert digit into ascii value 
 push dx ; save ascii value on stack 
 inc cx ; increment count of values 
 cmp ax, 0 ; is the quotient zero 
 jnz _nextdigit ; if no divide it again 
 mov di, 24; point di to 70th column 
nextpos: pop dx ; remove a digit from the stack 
 mov dh, 0x07 ; use normal attribute 
 mov [es:di], dx ; print char on screen 
 add di, 2 ; move to next screen location 
 loop nextpos ; repeat for all digits on stack 
 pop di 
 pop dx 
 pop cx 
 pop bx 
 pop ax
 pop es 
 pop bp 
 ret 2 
 seconds: dw 0
 tickcount: dw 0 
oldisr: dd 0 ; space for saving old isr 
key_arr: db 'q',0x10,'w',0x11,'e',0x12,'r',0x13,'t',0x14,'y',0x14,'u',0x16,'i',0x17,'o',0x18,'p',0x19,'a',0x1E,'s',0x1F,'d',0x20,'f',0x21,'g',0x22,'h',0x23,'j',0x24,'k',0x25,'l',0x26,'z',0x2C,'x',0x2D,'c',0x2E,'v',0x2F,'b',0x30,'n',0x31,'m',0x32
char1: db 'c'
char2: db 'g'
char3: db 'q'
char4: db 't'
score: dw 0
time: dw 120
msg1: db 'Time Left:',0
msg2: db 'Live Score:',0 ; string to be printed 
attribute1: db 0x20
attribute2: db 0x20
attribute3: db 0x20
attribute4: db 0x20