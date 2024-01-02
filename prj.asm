  [org 0x0100]
jmp start
message: db 'WELCOME TO RABBIT JUMP' ; string to be printed
message2: db 'Developed By: 22L-6570 And 22L-6555'
message3: db 'Press Enter to continue and Escape to Exit'
length: dw 22,35,42 ; length of string

instruction1: db 'Press UP key for the rabbit to jump'   
instruction2: db 'If at the point of jump there is no brick above the rabbit the game will end' 
instruction3: db 'If you stay on the blue brick for over 05 seconds the game will end'
length_instruction: dw 35,76,67


ending3: db '!!!!Thanks For Playing!!!!'
ending2: db '!!!!Come Back Soon!!!!'
ending1: db 'GAME OVER '

length_end: dw 9,22,26

score_string:dw 'SCORE: '
score_string_length: dw 7

escape: dw 0
esc_msg1: db "Do you want to Quit?"
esc_msg2: db "Press Y to Quit"
esc_msg3: db "Press N to Continue" 
esc_length: dw 20, 15,19
yes: dw 0
no: dw 0

coinPosition: dw 8838;;;;;initial index of row containing coin
topcoinRow: dw 8712 ;;;;;starting index of row containing coin
coinRow: dw 8712
score: dw 0

randomCoinPositions: dw 0,8848 ,8848
coinPositionNumber: dw 0


tilePositionNum: dw 0
randNumTilePos: dw 9074,9082,9090
 
randomBrickColor: dw  0x3020,0x4020, 0x7020
currentBrickColor: dw 0 ;;;;ups wli tile  color
backupCurrentColor: dw 0 ;;;;niche wli tile check krne k  lie
counter: dw 0

rabbitPosition: dw 10682

mountain_start: dw 0x0030,0x01d0,0x06A6
mountain_height: dw 0x000D ,0x000C,0x0007
 
birds:dw 0x0066,0x008A,0x094,0x00BE,0x0BEE
 
exitLost: dw 0


currentRightMoved: dw 0,0,0 ;;;;;;;;;for recording individual tile movement
limitMovement: dw 20,14,14 ;;;;right mov   ;;;;;;;;;checking if rotation completed
currentLeftMoved: dw 0,0,0    ;;;;;left mov
tilePosition: dw 9082,10930   ;;;;;initial Tile position
 
movingBrickCurrSpeed: dw 0,1,2
movingBrickCurrLine: dw 34,41,40

timerBrick: dw 0
check: dw 0
numTicks: dw 0
jumpOrNot: dw 0
escapeCheck1: dw 0
escapeCheck2: dw 0
buffer: times 5676 dw 0
bufferSize: dw 5676
stopAnimation: dw 0
 

RandomNumberGenerator:		
                            push bp
							mov bp , sp
							push dx
							push ax
							push cx

                            mov dx,word [ numTicks]
							mov  ax, dx
							xor  dx, dx
							mov  cx, [bp + 4] ;;;;;bp+4 coontains num to divide by 
							div  cx           ;;;;;

							mov [bp + 6] , dx ;;;;;bp+6 coontains space to store result
							pop cx
							pop ax
							pop dx
							pop bp
							ret 2
 

Delay:               
    push cx
    mov cx, 0xFFFF
    del:
    loop del
    pop cx
 
    ret

clrscr:
push es
push ax
push di
mov ax, 0xb800
mov es, ax  
mov di, 0  
nextloc:
mov word [es:di], 0x0720  
add di, 2  
cmp di, 11928 
jne nextloc  
pop di
pop ax
pop es
ret

  kbisr:		
                push ax
			    push es
                push cx
				push dx
				
				 
				
			in al, 0x60  ;;;;demanding keystroke

nextcmp:	
             
             cmp al, 0x48 ;;;;al stores scan code
			 jne nomatch  
			
            mov cx,[cs:rabbitPosition]
			sub cx,1584
            mov dx,[cs:tilePosition]			 
             cmp cx,dx
			 jb setCheckFlag
			 

            add dx,38
			cmp cx,dx
			jae  setCheckFlag
			
			mov word[jumpOrNot],1
jmp dontSet		
		
setCheckFlag: 
mov word[check],1

dontSet:

			 		 
nomatch:	

            cmp al,0x01
            jne  secondEscape
		    mov word[escapeCheck1],1
			
			jmp exit2

setYes:
mov word[yes],1
jmp exit2


setNope:
mov word[no],1
jmp exit2


 checkYesNo:
 cmp al , 0x15
 je setYes
 cmp al,0x31
 je setNope
 jmp normalNoMatch
 

secondEscape:			
		   cmp word[escapeCheck1],1		      
		    je checkYesNo
            jne normalNoMatch
		    mov word[escapeCheck1],1
		
normalNoMatch:		
            
            pop dx
			pop cx
            pop es
			pop ax
			jmp far [cs:oldisr] ; call the original ISR
			


exit2:		mov al, 0x20
			out 0x20, al ; send EOI to PIC
			pop dx
			pop cx
			pop es
			pop ax
			
			iret  
			

 
timer:		 	 
             push ax
			 
			 cmp word[escapeCheck1],1
			 je stopTimer
			 inc word [ cs:numTicks] 
			 cmp word[cs: backupCurrentColor], 0x3020
			 jne incTick 
			 cmp word[cs:numTicks], 18 ;;;;55 milisecond times 18 = 9.9 seconds
			 jne ignoreNum
			
			 inc word [ cs:timerBrick]
			 cmp word[ cs:timerBrick], 10
			 jne  incTick
		     mov word[check],1
			 
  incTick:
			  mov word[ cs:numTicks], 0
	
stopTimer:	
ignoreNum:
     push word[cs:timerBrick]
	 push 262
	 call PrintNumber
			mov al, 0x20
			out 0x20, al 
            pop ax
return5:			
			 iret ;;;;pops flags,code segment,IP
 
PrintNumber:		push bp
					mov bp, sp
					push es
				

				push ax
					push bx
					push cx
					push dx
					push di

					mov ax, 0xb800
					mov es, ax 
					mov ax , 25 ;;;;fixed row number
					mov cx , 132
					mul cx
					add ax , [bp + 4]
					shl ax , 1
					mov di , ax
					
					mov ax, [bp + 6] 
					mov bx, 10 
					mov cx, 0 

nextdigit: 			mov dx, 0 
					div bx 
					add dl, 0x30
					push dx 
					inc cx 
					cmp ax, 0 
					jnz nextdigit 

nextpos: 			pop dx 
					mov dh, 0x07
					mov [es:di], dx 
					add di, 2 
					loop nextpos 

					pop di
					pop dx
					pop cx
					pop bx
					pop ax
					pop es
					pop bp
					ret 4
	
 
 
 
Jump:
cmp word[jumpOrNot],1
je workingJump
jne dontJump
workingJump:

 
			call coinCheck
			call moveUp2
			call Delay
			call removeTile
		    call Delay
			 call Delay
			call scroll_down2		 
			sub word[cs:tilePosition],1848
			call addLine
dontJump:


ret
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SCREEN MOVEMENT;;;;;;;;;;;;;;;;;;;;;;;;;;;;
move_right:              
    push bp             
    mov bp, sp

    push es
    push ax
    push ds
    push cx
    push di
    push si
    
    mov ax, 0xb800
    mov es, ax
    mov ds, ax
    mov ax, 132 
    mul byte [bp + 4]    
    SHL ax, 1               
    add ax, 262
	 mov di, ax
	 sub ax,2
    mov si, ax
    mov cx, 132        

    push word [es:di]    
    std
    rep movsw
    add di, 2           
    pop word [es:di]     

    pop si
    pop di
    pop cx
    pop ds
    pop ax
    pop es
    pop bp

    ret 2


move_upper:          
    push ax             
    mov ax, 0
    right_upper_loop:
        push ax
        call move_right
        inc ax
        cmp ax, 14
    jne right_upper_loop 
    pop ax
    ret


move_left:              
    push bp             
    mov bp, sp

    push es
    push ax
    push ds
    push cx
    push di
    push si
    
    mov ax, 0xb800
    mov es, ax
    mov ds, ax
    mov ax, 132
    mul byte [bp + 4]    
    SHL ax, 1            
    mov di, ax
    add ax, 2
    mov si, ax
    mov cx, 132           

    push word [es:di]    
    cld
    rep movsw
    sub di, 2           
    pop word [es:di]     

    pop si
    pop di
    pop cx
    pop ds
    pop ax
    pop es
    pop bp

    ret 2

move_mid:          
    push ax             
    mov ax, 15
    left_upper_loop:
        push ax
        call move_left
        inc ax
        cmp ax, 25
    jne left_upper_loop 
    pop ax
    ret
	
 printCoin:
push ax
push bx
push es
push si
push cx
mov ax,0xb800
mov es,ax
push word 0xCCCC
push word 3
call RandomNumberGenerator 
pop dx
mov word [coinPositionNumber],dx
mov si,dx
shl si,1
mov cx,[randomCoinPositions+si]
cmp cx, 0
je skip10
mov si,cx
mov word[coinPosition],cx
mov ah,0x36
mov al,'C'
mov [es:si],ax
skip10:
 pop cx
pop si
pop es
pop bx
pop ax

ret

removeOldCoin:
push ax
push bx
push es
push si
push cx
mov ax,0xb800
mov es,ax
 
 mov si,[coinRow]
 mov cx,132
 mov bx,0x1020
 
 colorLine:
 mov word[es:si],bx
 add si,2
 loop colorLine
 
 pop cx
pop si
pop es
pop bx
pop ax

ret
	
 	
coinCheck:
push ax
push bx
push cx
push dx
call removeOldCoin
call printCoin

mov ax,[rabbitPosition]
mov bx,[coinPosition]
sub ax,1848

cmp ax,bx
je updateScore

add ax,2
cmp ax,bx
jne dontUpdateScore

updateScore:
add word[score],10


dontUpdateScore:
pop dx
pop cx
pop bx
pop ax

ret	
	
	
movingBrick:
push bp
mov bp, sp 

push ax
	push bx 
	push cx
	push dx 
	push si 
	push di 
	push es 
	push ds
 
cmp word[bp+4], 0 
je return
mov si,[bp+8]
cmp word[bp+4], 2  
jz fastMovement

        mov cx, word [currentRightMoved+si]
		mov dx,cx
		mov ax,word [limitMovement+si]   
        cmp cx,ax
        jz loopBack
		
		mov ax,[bp+6] ;;;;;;;;;;;;;starting index of line with brick		 
		push ax
		call move_right
		
		cmp word[bp+8],0		 
		jz moveTileRight
		
		cmp word[bp+8],2		 
		jz moveTileRight2
		
		cmp word[bp+8],4
		jz moveRabbitRight
		
moveTileRight:
              add word[tilePosition],2
	         jmp goRabbitMovement
		
moveTileRight2:
              add word[tilePosition+2],2
		
goRabbitMovement:	
	    cmp word[bp+8],4
		jnz dontMoveRabbit
		

moveRabbitRight:
add word[rabbitPosition],2

		
dontMoveRabbit:
		add dx,1
		mov word[currentRightMoved+si],dx
		 
		mov bx,0
		mov word[currentLeftMoved+si],bx
		jmp return
		
		
fastMovement:
        mov cx, word [currentRightMoved+si]
		mov dx,cx
		mov ax,word [limitMovement+si]   
        cmp cx,ax
        jz moveFastBack
		
	    mov ax,[bp+6]  
		 
		push ax
		call move_right
		 mov ax,[bp+6]  
		 
		push ax
		call move_right
		
		cmp word[bp+8],0		 
		jz moveTileRightFast1
		
		cmp word[bp+8],2		 
		jz moveTileRightFast2
		
		cmp word[bp+8],4
		jz moveRabbitRightFast
		
moveTileRightFast1:
              add word[tilePosition],4
	         jmp goRabbitMovementFast
		
moveTileRightFast2:
              add word[tilePosition+2],4
		
goRabbitMovementFast:	
	    cmp word[bp+8],4
		jnz dontMoveRabbitFast
		

moveRabbitRightFast:
add word[rabbitPosition],4

dontMoveRabbitFast:
		add dx,1
		mov word[currentRightMoved+si],dx
		 
		mov bx,0
		mov word[currentLeftMoved+si],bx
		jmp return
		
           
rett: 
        cmp word [bp+4],0
		jz return
		
loopBack:
        mov bx, word [currentLeftMoved+si]
		 
		mov cx,word [limitMovement+si]   
        cmp bx,cx
        jz shiftLeft1
		push word[bp+6]
		call  move_left
						
		cmp word[bp+8],0		 
		jz moveTileLeft
		
		cmp word[bp+8],2		 
		jz moveTileLeft2
		
		cmp word[bp+8],4
		jz moveRabbitLeft
		
moveTileLeft:
              sub word[tilePosition],2
	          jmp goRabbitMovement2
		
moveTileLeft2:
              sub word[tilePosition+2],2
		
		
goRabbitMovement2:		
	    cmp word[bp+8],4
		jnz dontMoveRabbit2
		 
		
moveRabbitLeft:
sub word[rabbitPosition],2

		
dontMoveRabbit2:
		
		add bx,1
		mov word[currentLeftMoved+si],bx
		 
		jmp return
		
shiftLeft1:
    mov cx,0
	mov word[currentRightMoved+si],cx
	jmp return 
		
moveFastBack:
        mov bx, word [currentLeftMoved+si]
		 
		mov cx,word [limitMovement+si]   
        cmp bx,cx
        jz shiftLeft2
		push word[bp+6]
		call  move_left
		push word[bp+6]
		call  move_left
		
		cmp word[bp+8],0		 
		jz moveTileLeftFast1
		
		cmp word[bp+8],2		 
		jz moveTileLeftFast2
		
		cmp word[bp+8],4
		jz moveRabbitLeftFast
		
				
moveTileLeftFast1:
              sub word[tilePosition],4
	          jmp goRabbitMovementFast2
		
moveTileLeftFast2:
              sub word[tilePosition+2],4
		
		
goRabbitMovementFast2:		
	    cmp word[bp+8],4
		jnz dontMoveRabbitFast2
		
 		
moveRabbitLeftFast:
sub word[rabbitPosition],4

		
dontMoveRabbitFast2:
		
		add bx,1
		mov word[currentLeftMoved+si],bx
		 
		jmp return
		
shiftLeft2:
    mov cx,0
	mov word[currentRightMoved+si],cx
	jmp return 
	 

return:
    pop ds 
	pop es 
	pop di 	
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
pop bp
ret 6

 
;;;;;;;;;--------MOUNTAIN PRINTING START------------- 
printMountain:
push bp
mov bp, sp

sub sp, 4
mov dx, 0
push es
push ax
push cx
push di

mov word [bp-2],2
mov ax, 0xb800
mov es, ax 
mov di, [bp + 6]   

mov ax, 1
mov cx, [bp+4] 
mov bh, 0x66     
mov bl, 0x2F

leftSideMountain:
mov bl, 0x2F
mov [es:di], bx
jmp rightSideMountain
l1: add di, 260
inc dx
cmp dx,cx
jz return2
jmp leftSideMountain

rightSideMountain:
mov ax,dx        ;;;;
mul byte[bp-2]    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mul byte[bp-2]    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mul byte[bp-2]    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
add ax,2
mov bl, 0x5C
mov si, di
mov word[bp - 4], ax
add word[bp - 4], si

push bx

fillMountain:
mov bx, 0x6720
mov [es:si], bx  ;;;;;;;ADDING COLOR TO MOUNTAIN
add si, 2
cmp si, [bp-4]
jne fillMountain
je popStack

popStack:
pop bx
mov [es:si], bx   ;;;;;;;;;;;;PRINTING RIGHT SIDE Of MOUNTAIN
jmp l1            ;;;;;;;;;;;JUMP TO L1 WHEN WORK ON ONE LINE IS FINISHED

return2:        
pop di
pop cx
pop ax
pop es
mov sp, bp 
pop bp
ret 4



Mountains:
mov ax,[mountain_start]
push ax
mov ax,[mountain_height]
push ax
call printMountain
mov ax, [mountain_start+2]
push ax
mov ax, [mountain_height+2]
push ax
call printMountain
mov ax, [mountain_start+4]
push ax
mov ax, [mountain_height+4]
push ax
call printMountain
ret	

;;;;;------------MOUNTAIN CALL END -----------------------

back_color:
nextblk:
mov word [es:di], bx
add di, 2 
cmp di,si  
jne nextblk ;
ret


Background:
push es
push ax
push di

mov ax, 0xb800
mov bx,0x3720 
mov es, ax 
mov di, 0 
mov si,3432;;;;;;first 13 rows 
call back_color
mov di,3432
mov si,3696 ;;;;;;single row green patch
mov bx,0x2000
call back_color
mov di,3696
mov si,6336;;;;;;;10 rows mid section for boat
mov bx,0x09db
call back_color
mov di,6336
mov si, 11352 ;;;;;;;last 18 rows
mov bx,0x1720
call back_color

pop di
pop ax
pop es
ret


printPlants:
push bp
mov bp, sp
push es
push ax
push cx
push di

mov ax, 0xb800
mov es, ax 
mov di, [bp + 4]
mov bh, 0x12
mov bl, 0x7C
mov [es:di], bx
mov bl,0x5F
sub di, 262
mov [es:di], bx
sub di,4
mov [es:di], bx
add di,2
mov bx, 0x01240
mov [es:di], bx

pop di
pop cx
pop ax
pop es
pop bp
ret 2

Plants:
mov ax, 11350
push ax
call printPlants
mov ax, 11250
push ax
call printPlants
mov ax, 11150
push ax
call printPlants
mov ax, 11100
push ax
 
call printPlants
ret


printBoat:
push bp
mov bp, sp
sub sp, 2 
push es
push ax
push cx
push di

mov ax, 0xb800
mov es, ax 
mov di, [bp + 8] ;;;;;;starting position of boat
mov bh, 0x00     ;;;;;;color of boat boundary

mov cx, [bp+4]   ;;;;;height of boat
mov dx, [bp+6]   ;;;;;length of boat

ship:
mov si, di
mov bl, 0x5C 
mov [es:di], bx
mov [bp-2], di ;;;;;start of each line of boat

push di   ;;;;;;;;;;to save value of di each time so it is not lost

mov di, [bp-2]
add si, dx

filloopBackoat:
add di, 2
mov ax, 0x5020
mov [es:di], ax

cmp di, si
jne filloopBackoat

pop di

mov bl, 0x2F
mov [es:si], bx
add di, 270
sub dx, 12
loop ship

pop di
pop cx
pop ax
pop es
mov sp, bp 
pop bp
ret 6

Boats:
mov ax,4252 
push ax
mov ax, 88
push ax
mov ax, 5
push ax
call printBoat
ret



printstr:	
        push bp
		mov bp, sp
		push es
		push ax
		push cx
		push si
		push di

		mov ax, 0xb800
		mov es, ax 			; point es to video base

		mov al, 132 			; load al with columns per row
		mul byte [bp+12] 	; multiply with row number
		add ax, [bp+10] 	; add col
		shl ax, 1 			; turn into byte offset
        add ax,2
		mov di, ax 			; point di to required location
		mov si, [bp+6] 		; point si to string
		mov cx, [bp+4] 		; load length of string in cx
		mov ah, [bp+8] 		; load attribute in ah

		cld ; auto increment mode

nextchar:
    lodsb 	; load next char in al -> mov al,[ds:si] -> add si,1
	stosw	; print char/attribute pair -> mov [es:di], ax -> add di,2
	loop nextchar 		; repeat111 for the whole string
		
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 10


strings:       
        mov ax, dx
		push ax;;;row 		 
		mov ax, 0
		push ax ;;;column		 		
		mov ax, 0x1F 		 
		push ax ;;;text attribute		 
		mov ax, message
		push ax ;;;text array	 
		push word [length]  ;;;text length
		call printstr 			
        ret 
		
string2:       
        mov ax, dx
		push ax 		 
		mov ax, 0
		push ax 		 		
		mov ax, 0x1F 		 
		push ax 		 
		mov ax, message2
		push ax 		 
		push word [length+2]  
		call printstr 			
        ret
		
			
string3:       
        mov ax, dx
		push ax 		 
		mov ax, 44
		push ax 		 		
		mov ax, 0x9F 		 
		push ax 		 
		mov ax, message3
		push ax 		 
		push word [length+4]  
		call printstr 			
        ret
		
				
string4:       
        mov ax, dx
		push ax 		 
		mov ax, 0
		push ax 		 		
		mov ax, 0x1F 		 
		push ax 		 
		mov ax, instruction1
		push ax 		 
		push word [length_instruction]  
		call printstr 			
        ret
		
string5:       
        mov ax, dx
		push ax 		 
		mov ax, 0
		push ax 		 		
		mov ax, 0x1F 		 
		push ax 		 
		mov ax, instruction2
		push ax 		 
		push word [length_instruction+2]  
		call printstr 			
        ret
		
		
string6:       
        mov ax, dx
		push ax 		 
		mov ax, 0
		push ax 		 		
		mov ax, 0x1F 		 
		push ax 		 
		mov ax, instruction3
		push ax 		 
		push word [length_instruction+4]  
		call printstr 			
        ret
				
				
string7:       
        mov ax, dx
		push ax 		 
		mov ax, 190
		push ax 		 		
		mov ax, 0x07 		 
		push ax 		 
		mov ax, ending1
		push ax 		 
		push word [length_end]  
		call printstr 			
        ret
		
string8:       
        mov ax, dx
		push ax 		 
		mov ax, 0
		push ax 		 		
		mov ax, 0x07 		 
		push ax 		 
		mov ax, ending2
		push ax 		 
		push word [length_end+2]  
		call printstr 			
        ret
		
		
string9:       
        mov ax, dx
		push ax 		 
		mov ax, 0
		push ax 		 		
		mov ax, 0x07 		 
		push ax 		 
		mov ax, ending3
		push ax 		 
		push word [length_end+4]  
		call printstr 			
        ret
		
string10:       
        mov ax, dx
		push ax 		 
		mov ax, 0
		push ax 		 		
		mov ax, 0x07 		 
		push ax 		 
		mov ax, score_string
		push ax 		 
		push word [score_string_length]  
		call printstr	
		
        ret

stringEsc1:       
        mov ax, dx
		push ax 		 
		mov ax, 40
		push ax 		 		
		mov ax, 0x0F 		 
		push ax 		 
		mov ax, esc_msg1
		push ax 		 
		push word [esc_length]  
		call printstr	
		
        ret
		
stringEsc2:       
        mov ax, dx
		push ax 		 
		mov ax, 20
		push ax 		 		
		mov ax, 0x8F 		 
		push ax 		 
		mov ax, esc_msg2
		push ax 		 
		push word [esc_length+2]  
		call printstr	
		
        ret
		
stringEsc3:       
        mov ax, dx
		push ax 		 
		mov ax, 66
		push ax 		 		
		mov ax, 0x8F 		 
		push ax 		 
		mov ax, esc_msg3
		push ax 		 
		push word [esc_length+4]  
		call printstr	
		
        ret		

stringScore:       
        mov ax, dx
		push ax 		 
		mov ax, 0
		push ax 		 		
		mov ax, 0x8F 		 
		push ax 		 
		mov ax, score_string
		push ax 		 
		push word [score_string_length]  
		call printstr	
		
        ret		
		
 Print_Border:
    push es         
    push ax
    push cx
    push di

    mov ax, 0xb800
    mov es, ax
    mov di, 1848
    mov ah, 0x1f
    mov al, 0xcd;
    mov cx, 132
    cld 
    rep stosw

    mov cx, 27  ;;;; size of intro screen    
    mov al, 0xBA     
    mov di,2374
right:
   
    mov word[es:di], ax
    add di, 264
    sub cx, 1
    jnz right

    mov cx, 132      ; bottom boundary distance
    mov al, 0xcd
    std
    rep stosw

    mov cx, 27       
    mov al, 0xBA    ;||
    mov di, 8976
    left:
    mov word[es:di], ax
    sub di, 264
    sub cx, 1
    jnz left


    pop di
    pop cx
    pop ax
    pop es
    ret

back_clr:
nextblock:
mov word [es:di], bx
add di, 2 
cmp di,si  
jne nextblock 
ret

border:
push es
push ax
push di
push si

mov ax, 0xb800
mov es,ax
mov di,1848
mov si,9504
mov bx,0x1000 
call back_clr
call Print_Border

pop si
pop di
pop ax
pop es     
ret
		
printBird:
push bp
mov bp, sp
push es
push ax
push cx
push di

mov ax, 0xb800
mov es, ax 
mov di, [bp + 4]
mov bh, 0x30

mov bl, 0x5C
mov [es:di], bx
add di, 2
mov bl, 0x2F
mov [es:di], bx

pop di
pop cx
pop ax
pop es
pop bp
ret 2

 
Birds:
mov ax, [birds]
push ax
call printBird
mov ax, [birds+2]
push ax
call printBird
mov ax, [birds+4]
push ax
call printBird
mov ax, [birds+6]
push ax
call printBird
mov ax, [birds+8]
push ax
call printBird
ret

intro:
call border
mov dx,8
call strings
mov dx,10
call string2
mov dx,12
call string4
mov dx,14
call string5
mov dx,16
call string6

ret


printingLine:
push bp
mov bp, sp
push es
push ax 
push cx
push di
push si
mov ax, 0xb800
mov es, ax 

mov cx, 20
mov di, [bp+4] ;;;;;; 
mov word ax, [currentBrickColor]
printing:

mov [es:di], ax
add di, 2
loop printing


pop si
pop di
pop cx
pop ax
pop es
pop bp
ret 2

printrabbit:
push bp
mov bp, sp
push es
push ax
push cx
push di

mov ax, 0xb800
mov es, ax 
mov di,  [bp+4]
mov ah,0x30
mov al,'R'
mov word [es:di],ax

add di,2
mov ah,0x30
mov al,'R'
mov word [es:di],ax
 

pop di
pop cx
pop ax
pop es
pop bp
ret 2

rabbit:
push word [rabbitPosition]
call printrabbit
ret

 

clear_last_tile:
push bp
mov bp,sp
push si
push ax
push cx
mov cx,20
mov ax,0xb800
mov es,ax
mov si, [bp+4]
  
 
 loopingTile4:
  mov word[es:si], 0x1020
  add si,2
  sub cx,1
  jnz loopingTile4
   
  
 pop cx  
 pop ax
pop si
pop bp
ret 2
 
removeTile:
 
 push word[tilePosition+2]
call clear_last_tile
 ret

Bricks:
push si
push ax
push 0xCCCC
push 3
call  RandomNumberGenerator
pop dx
mov si, dx
shl si,1
mov ax,[randomBrickColor+si]
mov [currentBrickColor],ax
push word[tilePosition]
call printingLine
mov word[backupCurrentColor],ax

mov word[currentBrickColor],0x7020
push word[tilePosition+2]
call printingLine
mov word[currentBrickColor],ax
mov word[backupCurrentColor],0

pop ax
pop si
ret

 
repeat111:
mov dx,34
call string3
mov ah,0
int 0x16

;cmp ah,1
;je outro

cmp ah,0x1C
jne repeat111

ret

StoreBuffer:	push es
				push ds
				push ax
				push cx
				push di
				push si
				
				mov di , buffer
				push cs
				pop es
				mov ax , 0xb800
				mov ds , ax
				mov si , 0
				mov cx ,    5676
				cld
				rep movsw
				
				pop si
				pop di
				pop cx
				pop ax
				pop ds
				pop es
				ret

RestoreBuffer:	push es
				push ds
				push ax
				push cx
				push di
				push si
				
				mov si , buffer
				push cs
				pop ds
				mov ax , 0xb800
				mov es , ax
				mov di , 0
				mov cx ,  5676
				cld
				rep movsw
				
				pop si
				pop di
				pop cx
				pop ax
				pop ds
				pop es
				ret	

esc_screen:
push ax
push cx
push dx
push bx
push si

call StoreBuffer
call clrscr
mov word[stopAnimation],1

mov dx,4
call stringEsc1
mov dx,6
call stringEsc2
mov dx,6
call stringEsc3

 
loopingEsc:
cmp word[no],1
jne checkAgain
 
call RestoreBuffer
 mov word[stopAnimation],0
 mov word[no],0
 mov word[ escapeCheck1],0
jmp goBack



checkAgain:
 cmp word[yes],1
jne loopingEsc
jmp outro
 
 
  
goBack:
pop si
pop bx
pop dx
pop cx
pop ax
 
ret

outro:
call clrscr
mov dx,21
call string7
mov dx,40
call string8
mov dx,41
call string9

mov dx,25
call  stringScore

 push word[score]
 push 10
 call PrintNumber

cli
xor ax, ax
mov es, ax ; point es to IVT base
mov ax, [cs:oldisr]
mov [es:9*4], ax ; save offset of old routine
mov ax, [cs:oldisr+2]
mov [es:9*4+2], ax ; save segment of old routine

xor ax, ax
mov es, ax ; point es to IVT base
mov ax, [cs:oldTisr]
mov [es:8*4], ax ; save offset of old routine
mov ax, [cs:oldTisr+2]
mov [es:8*4+2], ax ; save segment of old routine

;mov dx, start
;add dx, 15
;mov cl, 4
;shr dx, cl

sti
call clear_kb_bufferds
mov ah,0
int 0x16

mov ax, 0x0003
int 10h

call RestoreBuffer2

mov ax,0x4c00
int 21h 

resetScr:
 
push bp
mov bp,sp
push si
push ax
mov ax,0xb800
mov es,ax
mov si, [rabbitPosition]
mov word[es:si],0x1020
add  si,2
mov word[es:si],0x1020
pop ax
pop si
pop bp
ret 
 
 moveUp2:
 push cx
 push bx
 push ax
 mov cx,5
 mov ax,[rabbitPosition]
 
 looper:
 call Delay
  call Delay


 call resetScr
 
 sub ax,264
 push ax
 call printrabbit
 mov [rabbitPosition],ax
 
 sub cx,1
  jnz looper
  call resetScr
  sub ax,528
  mov [rabbitPosition],ax
  push ax
  call printrabbit
  pop ax
   pop bx
 pop cx
 ret 
 
 clear_kb_bufferds:
push ax
    push ds
    mov ax, 0040h
    mov ds, ax
    mov ax, [001Ah]
    mov [001Ch], ax
  pop ds 
  pop ax
  ret
 
 scroll_down2:
 push cx
 push es
 push si
 push ax
 push dx
 mov cx,7
 
      mov word[cs:timerBrick], 0

 looper2:
 call Delay
 call Delay
 call scroll_down 
 sub cx,1
 
 call Delay
 call Delay
 jnz looper2
   
  mov cx,20 
  mov si,[tilePosition]
 
  loopingTile5:
  mov word[es:si],0x1020
  add si,2
  sub cx,1
  jnz loopingTile5
  
  pop dx
   pop ax
   pop si
   pop es
 pop cx
 ret 
 
 randTilePosition:
 push si
 push ax
 push word 0xCCCC
push word 3
 call RandomNumberGenerator
 pop dx
mov word [tilePositionNum  ],dx
mov si,dx
 mov si,[ tilePositionNum ]
 shl si,1
 mov ax,[ randNumTilePos+si]
 mov word[tilePosition],ax
 pop ax
 pop si
 ret
 
 addLine:
 push ax
 push bx
 push si
 mov word ax,[tilePosition]
  add ax,1848
  push ax
 call printingLine
 mov word[tilePosition+2],ax
 
 mov ax,[currentBrickColor]
 mov word[backupCurrentColor],ax

 
 
 push 0xCCCC
push 3
call  RandomNumberGenerator
pop dx
mov si, dx
shl si,1
 
mov bx,[randomBrickColor+si]
mov [currentBrickColor],bx
 
  push  word[tilePosition]
 call printingLine
 pop si
 pop bx
 pop ax
 ret
 
clear_rabbit:
push bp
mov bp,sp
push si
mov si, [rabbitPosition]
sub si,264
mov word[es:si],0x1020
add  si,2
mov word[es:si],0x1020
pop si
pop bp
ret 
 
move_rabbit_down:
 mov bh,0x30
 mov bl,'R'
 add word[rabbitPosition],264
 mov ax,0xb800
 mov es,ax
 mov si,[rabbitPosition]
 mov word[es:si],bx
 add si,2
 mov word[es:si],bx
ret


clear_tile:
push si
push cx
mov cx,20
mov ax,0xb800
mov es,ax
mov si, [tilePosition]
sub si, 264
mov ax,0xb800
 mov es,ax
 loopingTile2:
  mov word[es:si], 0x1020
  add si,2
  sub cx,1
  jnz loopingTile2
  pop cx
pop si
ret 
 
move_tile_down:
push si
push cx
push dx
mov cx,20
  mov bx,[currentBrickColor]
 add word[tilePosition],264
 mov ax,0xb800
 mov es,ax
 mov si,[tilePosition]
 
 loopingTile:
  mov word[es:si],bx
  add si,2
  sub cx,1
  jnz loopingTile
 pop dx
 pop cx
 pop si
ret
 
scroll_down:
 push ax
 push di
 push si
 push bx
 push cx
 
 call move_tile_down  
 call Delay
 call Delay
 call clear_tile
 call move_rabbit_down
 call clear_rabbit
 
 
 
pop cx
pop bx
pop si	 
pop di
pop ax
ret



 
 
playAnimation:
push si
push cx

call Delay
call Delay
call move_upper
call move_mid
call Delay
call Delay
call Delay



mov cx,4
push word 0xCCCC
push word 3

call RandomNumberGenerator
pop dx 
mov word[tilePositionNum], dx
mov si,[tilePositionNum]
shl si,1
 
cmp word[currentBrickColor],0x4020
je skipRedColor
push 0
push word[movingBrickCurrLine]
push word[movingBrickCurrSpeed+4]
call movingBrick
skipRedColor:
push word 0xCCCC
push word 4

call RandomNumberGenerator
pop dx 
mov word[tilePositionNum], dx
mov si,[tilePositionNum]
shl si,1


cmp word[ backupCurrentColor],0x4020
je skipRedColor2


push 2
push word[movingBrickCurrLine+2]
push word[movingBrickCurrSpeed+2]
call movingBrick

push 4
push word[movingBrickCurrLine+4]
push word[movingBrickCurrSpeed+2]
call movingBrick;;;;;;;;;basically moving rabbit using movingBrick
skipRedColor2:
pop cx
pop si
 
ret

scoreString:
mov dx,25
call string10

ret


callerFunction:
call intro  
call repeat111
call Background
call Mountains
call Boats
call Birds
call Bricks
call rabbit
call scoreString
 

ret





  
start:

call StoreBuffer2
        mov ah,0x00
		mov al, 0x54
		int 0x10		 
 		call clrscr
 	
		call callerFunction
		
        xor ax, ax
		mov es, ax ; point es to IVT base
		mov ax, [es:9*4]
		mov [oldisr], ax ; save offset of old routine
		mov ax, [es:9*4+2]
		mov [oldisr+2], ax ; save segment of old routine
		
		 xor ax, ax
		mov es, ax ; point es to IVT base
		mov ax, [es:8*4]
		mov [oldTisr], ax ; save offset of old routine
		mov ax, [es:8*4+2]
		mov [oldTisr+2], ax ; save segment of old routine
		
 		cli ; disable interrupts
		mov word [es:8*4], timer
		mov [es:8*4+2], cs
		mov word [es:9*4], kbisr ; store offset at n*4
		mov [es:9*4+2], cs ; store segment at n*4+2
		
		sti ; enable interrupts
		 
		
      loop1:
	   cmp word[escapeCheck1],1
	   jne  skipEscape
	   call esc_screen 
	  cmp word[escapeCheck2],1
	   jne  skipEscape
	  call outro
skipEscape:
    mov word[escapeCheck1],0	 
  mov word[escapeCheck2],0
	 cmp word[check],1
	 je outro
	 
	 
	 push word[score]
	 push 10
	 call PrintNumber
	 cmp word[stopAnimation],1
	 jne keepPlaying
	 je skipAnimation
keepPlaying:
	  call playAnimation
       
skipAnimation:	  
	 cmp word[jumpOrNot],1 
	 jne  loop1
     call Jump
	 mov word[jumpOrNot],0	
	  jmp loop1
	  
	  
gameOver:
call clrscr
mov ah,0x1
int 0x21

oldisr: dd 0
oldTisr: dd 0

StoreBuffer2:	push es
				push ds
				push ax
				push cx
				push di
				push si
				
				mov di , buffer2
				push cs
				pop es
				mov ax , 0xb800
				mov ds , ax
				mov si , 0
				mov cx ,    2000
				cld
				rep movsw
				
				pop si
				pop di
				pop cx
				pop ax
				pop ds
				pop es
				ret

RestoreBuffer2:	push es
				push ds
				push ax
				push cx
				push di
				push si
				
				mov si , buffer2
				push cs
				pop ds
				mov ax , 0xb800
				mov es , ax
				mov di , 0
				mov cx ,  5676
				cld
				rep movsw
				
				pop si
				pop di
				pop cx
				pop ax
				pop ds
				pop es
				ret	
				
buffer2: times 2000 dw 0
bufferSize2: dw 2000
