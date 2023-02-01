assume cs: Code, ds: Data

_Stack SEGMENT stack
	db 128 dup (?)
_Stack ENDS

Data SEGMENT
	max    equ 64
	base   db  16 
	num1   db  max, max+1 dup (?)
	num2   db  max, max+1 dup (?)
	res    db  max, max+2 dup (?)
	errMsg db  "The string contains invalid symbol!$"
Data ENDS

Code SEGMENT
; все массивы по умолчанию считаются буферизированными 

; макрос печати символа новой линии
PrintNewLine MACRO
	push ax
	push dx
	
	mov dl, 0ah
	mov ah, 02h
	int 21h

	pop dx
	pop ax
ENDM

; макрос печати небуферизированного массива str с '$' в конце
Print MACRO str
	push ax
	push dx
	
	lea dx, str
	mov ah, 09h
	int 21h
	
	pop dx
	pop ax
ENDM

; процедура печати массива без '$' в конце
; вход:  dx - адрес массива 
PrintBuff PROC
PB_Start:
	push ax
	push bx

PB_Main:
	mov bx, dx
	xor ah, ah
	mov al, [bx + 1]
	add bx, 2
	add bx, ax
	mov al, [bx]
	mov byte ptr [bx], '$'
	
	add dx, 2
	mov ah, 09h
	int 21h
	xor ah, ah
	mov [bx], ax

PB_Exit:
	pop bx
	pop ax
	ret
PrintBuff ENDP

; процедура преобразования массива ascii-кодов в массив цифр
; вход:  dx     - адрес массива ascii-кодов
; выход: dx     - адрес массива цифр
;        cf = 1 - ошибка
ASCIIToDigits PROC
ATD_Start:
	push ax
	push bx
	push cx

ATD_Main:
	mov bx, dx
	xor ch, ch
	mov cl, [bx + 1]
	jcxz ATD_Error
	add bx, 2
	
	ATD_Loop:
		mov al, [bx]
		sub al, '0'
		cmp al, 0
		jl ATD_Error
		cmp al, 9
		jng ATD_Move

		sub al, 11h
		cmp al, 0
		jl ATD_Error
		cmp al, 5
		jg ATD_Error
		add al, 10
	
	ATD_Move:
		mov [bx], al
		inc bx
		loop ATD_Loop
		
	clc
	jmp ATD_Exit
		
ATD_Error:
	stc

ATD_Exit:
	pop cx
	pop bx
	pop ax
	ret
ASCIIToDigits ENDP

; процедура пребразования массива цифр в массив ascii-кодов
; предполагается, что вход корректен
; вход:  dx - адрес массива цифр
; выход: dx - адрес массива ascii-кодов
DigitsToASCII PROC
DTA_Start:
	push ax
	push bx
	push cx
	
DTA_Main:
	mov bx, dx
	xor ch, ch
	mov cl, [bx + 1]
	add bx, 2
	
	DTA_Loop:
		mov al, [bx]
		add al, '0'
		cmp al, '9'
		jng DTA_Move
		add al, 7 ; 10 + '0' + 7 = 'A'
	
	DTA_Move:
		mov [bx], al
		inc bx
		loop DTA_Loop
			
DTA_Exit:
	pop cx
	pop bx
	pop ax
	ret
DigitsToASCII ENDP

; процедура дополнения меньшего из двух массивов цифр
; слева нулями до равенства длин массивов
; вход:  si - адрес первого массива
;        di - адрес второго массива
; выход: si - адрес первого массива
;        di - адрес второго массива
ShiftDigits PROC
SD_Start:
	irp reg, <ax, bx, cx, dx, si, di>
		push reg
	endm
SD_Main:
	irp reg, <al, bh, ch, dh>
		xor reg, reg
	endm
	std
	
	mov bl, [si+1]
	mov dl, [di+1]
	cmp bx, dx
	je SD_Exit
	jl SD_ShiftFirst
	jg SD_ShiftSecond

SD_ShiftFirst:
	mov di, si
	inc si
	add si, bx
	inc di
	add di, dx
	mov cl, bl
	
	rep movsb

	mov cl, dl
	sub cl, bl

	rep stosb
	
	mov [di], dx
	jmp SD_Exit

SD_ShiftSecond:
	mov si, di
	inc si
	add si, dx
	inc di
	add di, bx
	mov cl, dl
	
	rep movsb
	
	mov cl, bl
	sub cl, dl

	rep stosb

	mov [di], bx
	jmp SD_Exit

SD_Exit:
	irp reg, <di, si, dx, cx, bx, ax>
		pop reg
	endm
	ret
ShiftDigits ENDP

; процедура беззнакового сложения массивов цифр
; предполагается, что уже вызвана процедура ShiftDigits
; вход:  si - адрес первого массива
;        di - адрес второго массива
;        dx - адрес результата
; выход: dx - адрес результата
UnsignedSum PROC
US_Start:
	push ax
	push bx
	push cx
	
US_Main:
	xor al, al
	mov bx, dx
	xor ch, ch
	mov cl, [si + 1]
	mov [bx + 1], cl
	add bx, 2
	
	inc si
	add si, cx
	inc di
	add di, cx
	
	US_Loop:
		xor ah, ah
		add al, [si]
		add al, [di]
		div base
		mov [bx], ah
		
		dec si
		dec di
		inc bx
		loop US_Loop
		
	cmp al, 0
	je US_Exit
	mov [bx], al
	mov bx, dx
	inc byte ptr [bx + 1]

US_Exit:
	pop cx
	pop bx
	pop ax
	ret
UnsignedSum ENDP

; процедура "переворачивания" массива
; вход:  dx - адрес массива
; выход: dx - адрес "перевёрнутого" массива
Reverse PROC
R_Start:
	irp reg, <ax, cx, si, di>
		push reg
	endm
R_Main:
	mov si, dx
	mov di, dx
	xor ch, ch
	mov cl, [si + 1]
	add si, 2
	inc di
	add di, cx
	
	shr cl, 1
	jcxz R_Exit

	R_Loop:
		mov al, [si]
		xchg al, [di]
		xchg al, [si]
		inc si
		dec di
		loop R_Loop

R_Exit:
	irp reg, <di, si, cx, ax>
		pop reg
	endm
	ret
Reverse ENDP

Start:
	mov ax, data
	mov ds, ax
	mov es, ax

	irp num, <num1, num2>
		lea dx, num
		mov ah, 0ah
		int 21h
		
		call ASCIIToDigits
		PrintNewLine
		jc Error
	endm

	lea si, num1
	lea di, num2
	call ShiftDigits
	
	lea dx, res
	call UnsignedSum
	call Reverse
	call DigitsToASCII
	call PrintBuff
	PrintNewLine
	
	jmp Exit

	Error:
		Print errMsg 
		PrintNewLine

	Exit:
		mov ah, 4ch
		int 21h
Code ENDS

END Start
