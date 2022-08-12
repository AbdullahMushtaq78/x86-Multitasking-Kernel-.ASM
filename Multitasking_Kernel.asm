org 100h
jmp Start_the_Kernel
;______________________Memory Allocation
currThr: dw 0
mStack: times (16*256) dw 0
PCB: times (16*16) dw 0
AXSAVE: EQU 0x04
BXSAVE: EQU 0x06
CXSAVE: EQU 0x08
DXSAVE: EQU 0x0A
SISAVE: EQU 0x0C
DISAVE: EQU 0x0E
BPSAVE: EQU 0x10
SPSAVE: EQU 0x12
IPSAVE: EQU 0x14
CSSAVE: EQU 0x16 
DSSAVE: EQU 0x18
SSSAVE: EQU 0x1A
ESSAVE: EQU 0x1C
FLAGSAVE: EQU 0x1E
TempRet: dw 0
ScreenElements: db 'Multitaksing'
Esize: dw 12
oldTimerISR: dw 0,0
;_________________________________________________________
Start_the_Kernel:
; initialize 0th pcb
mov word[cs:PCB+2],1
xor ax,ax
mov es,ax
;saving oldTimerISR
mov ax,[es:0x08*4]	; saving ip
mov [oldTimerISR],ax
mov ax,[es:0x08*4+2]	; cs
mov [oldTimerISR+2],ax
cli
;hooking isr08
mov word[es:8*4],isr08
mov word[es:8*4+2],cs 
sti
;hooking isr80
mov word [es:80h*4], isr80
mov word [es:80h*4+2], cs
mov ax,0xb800
mov es,ax

xor ax,ax; Clearing the AX for next process
;>>>>>>>>>>>>>>>>>>>>>>>>>>Read This FIRST<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
; First of all, push 0x55 in AX for new thread,
; 0x56 for suspending thread,
; 0x57 for resuming thread,
; 0x58 for deleting the thread.
;__________________________________________________________________
; For Creating the New thread, first push CS, then push the offset of the Task to be Multitasked and Call 80H interrupt 
; For Suspending any task, just push the number of PCB you want to Suspend in BX.
; For Resuming a task, push the number of PCB you want to suspend in BX.
; For Deleting a task, push the number of PCB you want to Delete in BX.
;__________________________________________________________________

;__MULTITASKING TASK1
mov al,0x55	; create it's thread
push cs
push MultTask1
int 0x80

;__MULTITASKING TASK2
mov al,0x55	; create it's thread
push cs
push MultTask2
int 0x80

; >>>>>>>>>>>>>>Dummy Cases<<<<<<<<<<<
;__Suspending the Task 2
; mov  al, 0x56
; mov bx, 2
; int 80h

;__Resuming the Task 2 
; mov  al, 0x57
; mov bx, 2
; int 80h

;__Deleting the Task 2
; mov al, 0x58
; mov bx, 2
; int 80h


jmp MultTask3 ;This task is for kernel to keep working in background.  
int 0x80

MultTask1: 

		mov ah,0xC7
		mov al, [cs:ScreenElements+bx]
		mov [es:240], ax 
		inc bx
        cmp bx, word[Esize]
        jne MultTask1
        xor bx, bx
        jmp MultTask1

MultTask2:
		mov ah,0xEF
		mov al, [cs:ScreenElements+bx] 
		mov [es:720], ax 
		inc bx 
		cmp bx, word[Esize]
        jne MultTask2
        xor bx, bx
        jmp MultTask2
MultTask3:
		mov ah,0x2F
		mov al, [cs:ScreenElements+bx] 
		mov [es:1200], ax 
		inc bx 
		cmp bx, word[Esize]
        jne MultTask3
        xor bx, bx
        jmp MultTask3

DeleteTheThread1:
    push ax
    push bx
    push cx
    xor ax, ax
    xor cx, cx 
    shl bx, 5   
    mov al,[CS:PCB+BX+1] ; saving the Previous 
    mov cl,[CS:PCB+BX+0] ; saving the Next pointer
    mov bx, cx

    shl bx, 5
    mov [CS:PCB+BX+1], al ; setting the previous
    xor ah, ah
    mov bx, ax 
    shl bx, 5
    mov [CS:PCB+bx+0], cx ;setting the next 
    pop cx
    pop bx
    pop ax
iret 
;____________________________________________

SaveState:

    push bx ;storing bx for later use
    mov bx,[CS:currThr] ;getting the value of Current thread
    shl bx, 5 ;multiplying bx by 32 to get the offset for memory location for that thread

    mov [CS:PCB+bx+AXSAVE], ax
    mov [CS:PCB+bx+CXSAVE], cx
    mov [CS:PCB+bx+DXSAVE], dx
    mov [CS:PCB+bx+SISAVE], si
    mov [CS:PCB+bx+DISAVE], di
    mov [CS:PCB+bx+BPSAVE], bp
    mov [CS:PCB+bx+ESSAVE], es
    mov [CS:PCB+bx+DSSAVE], ds
    pop ax ;restoring the value of bx to store in BXSAVE
    mov [CS:PCB+bx+BXSAVE], ax
    pop cx ;saving the return address of call made by isr08
    pop ax
    mov [CS:PCB+bx+IPSAVE], ax ;getting IP from stack
    pop ax 
    mov [CS:PCB+bx+CSSAVE], ax ;getting CS from stack
    pop ax
    mov [CS:PCB+bx+FLAGSAVE], ax ;getting FLAGS from 
    mov [CS:PCB+bx+SSSAVE], ss 
    mov [CS:PCB+bx+SPSAVE], sp 
    push cx ; pushing the return address back in stack to go from where it was called
    ret
;____________________________________________

isr08: 
        call SaveState
        call Getnext			
        call RestoreState 		;Thread to be resumed is in al

	jmp far[cs:oldTimerISR]

isr80:
	cmp 	al,0x55
	je 	    create_Thread
    cmp     al, 0x58
    je      DeleteTheThread1
    cmp 	al,0x56
	je		suspendTheThread
	cmp 	al,0x57
	je 		ResumeTheThread
	;cmp 	al,0x58
	;je 		DeleteTheThread
	iret 

create_Thread:
; start with finding Free PCB
; Intializa the PCB if possible
; Insert the Thread if possible
    push bp
    mov bp, sp
    push bx
    push ax
    call FindTheFreePCB
;After calling that ax will have the index of free PCB
    cmp ax, 0
    je NoFreePCBFound
    call InitializeThePCB
    mov bx, ax
    shl bx, 5
    push ax ;saving the index of current free pcb 
    mov ax,[bp+10]
    mov [CS:PCB+BX+CSSAVE], ax ; saving the CS for this free PCB
    mov ax, [bp+8]
    mov [CS:PCB+BX+IPSAVE], ax
    pop ax  ;restoring the index of free pcb in ax
    call InsertTheThread
NoFreePCBFound:
    pop ax
    pop bx
    pop bp
iret
;____________________________________________

suspendTheThread:
;   0 for Free, 1 for Occupied, 2 for suspended
    push bx
    shl bx, 5
    mov word[CS:PCB+BX+2], 2 ; suspending Thread
    pop bx
iret
;_____________________________________________

ResumeTheThread:
;   0 for Free, 1 for Occupied, 2 for suspended
    push bx
    shl bx, 5
    mov word[CS:PCB+BX+2], 1 ; Resuming Thread
    pop bx
iret
;_____________________________________________

Getnext:
    ; push bx
    ; mov bx,[CS:currThr]
    ; shl bx, 5
    ; mov ax, [PCB+bx]
    ; and ax, 00FFh ;saving al for next pointer of next thread
    ; pop bx
    ; ret
    push bx
    push cx
    mov bx, [CS:currThr]
    shl bx, 5
    GetNextLoop:
        mov ax, [CS:PCB+BX+0]
        and ax, 0x00FF
        mov bx, ax
        shl bx, 5
        mov cx, [CS:PCB+BX+2]
        cmp cx, 1 ;Checking if occupied, we will need this only if this is occupied to do some task
        jne GetNextLoop
        pop cx
        pop bx
        ;Saving the Next PCB in AX so that we can use it for further processes
ret

;____________________________________________

RestoreState:
    mov bx, ax ;the pointer to nex thread is basically saved in ax so storing it in bx
    mov [CS:currThr], bx
    shl bx, 5 ; setting the offset
    
    mov cx, [CS:PCB+bx+CXSAVE]
    mov dx, [CS:PCB+bx+DXSAVE]
    mov si, [CS:PCB+bx+SISAVE]
    mov di, [CS:PCB+bx+DISAVE]
    mov bp, [CS:PCB+bx+BPSAVE]
    mov ax, [CS:PCB+bx+ESSAVE]
    mov es, ax
    mov ax, [CS:PCB+bx+DSSAVE]
    mov ds, ax
    pop ax ; Saving the return address in ax because the segment will be changed after few lines
    mov [CS:TempRet], ax ; sving that return address in TempRet to use it later in code

    cli ; clearing the interrupt flag for segments
    mov ax,[cs:PCB+bx+SSSAVE]
	mov ss,ax
	mov sp,[cs:PCB+bx+SPSAVE]
    sti ; setting the interrupt flag
    
    mov ax, [CS:PCB+bx+FLAGSAVE]
    push ax ; pushing the flags for interrupts
    mov ax, [CS:PCB+BX+CSSAVE]
    push ax ; pushing the CS for interrupts
    mov ax, [CS:PCB+BX+IPSAVE]
    push ax ; pushing the IP for interrupts
    mov ax, [TempRet]
    push ax ;pushing the return address from where it was called
    mov ax, [CS:PCB+BX+AXSAVE] ; at last restoring the ax and bx
    mov bx, [CS:PCB+BX+BXSAVE]
    ret
;____________________________________________

FindTheFreePCB:
    push cx
    push bx
    mov cx, 16; no. of PCBs to iterate
    xor bx, bx
    NextPointerChecker:
        mov al, [CS:PCB+bx+2] ; Saving the next pointer in al
        cmp al, 0 ;comparing the al whether it is free or not if free then value in al would be 0 
        je FreeExit ; if free then jump to Free exit
        add bx, 32  ; we will add only if the current next of pcb in al is not 0, adding 32 because there are 16 words in each pcb 
        loop NextPointerChecker
        xor bx,bx ; this will happen only when there is not free pcb and 0th pcb is not free by default.
FreeExit:
    shr bx, 5 ;As we were jumping by 32 and dividing by 32 will give us exactly the number of pcb which is free.
    mov ax, bx ; storing the index of free pcb in ax
    pop bx 
    pop cx
    ret
;____________________________________________

InitializeThePCB:
    push bx
    push ax
    mov bx, ax
    shl bx, 5
    push si
    mov si, bx
    mov word[CS:PCB+bx+AXSAVE], 0
    mov word[CS:PCB+bx+BXSAVE], 0
    mov word[CS:PCB+bx+CXSAVE], 0
    mov word[CS:PCB+bx+DXSAVE], 0
    mov word[CS:PCB+bx+BPSAVE], 0
    mov word[CS:PCB+bx+SISAVE], 0
    mov word[CS:PCB+bx+DISAVE], 0
    mov word[CS:PCB+bx+DSSAVE], DS
    mov word[CS:PCB+bx+ESSAVE], ES
    mov word[CS:PCB+bx+SSSAVE], CS
    mov word[CS:PCB+bx+FLAGSAVE], 0x0200; setting the I Flag as 1
    mov byte[CS:PCB+bx+2], 1 ; setting that this pcb is active now(Not Free)
    inc ax
    shl ax, 9 ; mutliply by 256
    dec ax
    add ax, mStack ;Setting the offset
    mov bx, ax
    mov [CS:bx], cs; storing the CS at the end of Stack
    sub bx, 2   ; moving the myStack up
    mov word[CS:bx], LegalTerm ; setting the IP
    sub bx, 2 ;moving the stack up
    mov [CS:PCB+si+SPSAVE], bx ;saving the sp
    pop si
    pop ax
    pop bx
    ret

LegalTerm:
;; _______________Good____
;____________________________________________

; 0byte, 1byte, 2-3byte(whole word)
; Next , Prev , Status of that PCB
; Maintaining the Linked list of PCBs
InsertTheThread:
    push bx
    push cx
    push dx
    push ax
    ; current free pcb is in AX
    ; next of 0 is in CX now
    cli
    mov cx, [CS:PCB+0] ;saving the next of 0th in CX because we are always adding the new thread after the 0th pcb
    and cx,0x00FF
    mov bx, ax
    shl bx, 5
    mov byte [CS:PCB+BX+0], cl; saving the next of new thread which was basically the next of 0th
    mov byte[CS:PCB+BX+1], 0 ; saving the previous of 
    shl cx, 5
    push di
    xor di,di
    mov di,cx
    mov [CS:PCB+di+1], al ;setting the previous of next of new thread
    pop di
    mov [CS:PCB+0], al; setting the next of 0th ocb which is the index of new thread
    pop ax
    pop dx
    pop cx
    pop bx
    sti
    ret 
;____________________________________________