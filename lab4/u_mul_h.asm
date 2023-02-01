assume cs: Code, ds: Data

_Stack SEGMENT stack
	db 128 dup (?)
_Stack ENDS

Data SEGMENT
	max     equ 64
	base    db  16 
	num1    db  max, max+1 dup (?)
	num2    db  max, max+1 dup (?)
	res     db  max, 2*max+1 dup (0)
	errMsg  db  "The string contains invalid symbol!$"
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

; процедура удаления лишних нулей слева
; вход:  dx - массив цифр
; выход: dx - массив цифр
TrimZeros PROC
TZ_Start:
	push cx
	push si
	push di
	
TZ_Main:
	mov si, dx
	xor ch, ch
	mov cl, [si + 1]
	add si, 2

	TZ_Loop:
		cmp byte ptr [si], 0
		jne TZ_Break
		
		inc si
		loop TZ_Loop

TZ_Break:
	mov di, dx
	cmp cl, [di + 1]
	je TZ_Exit

	cmp cl, 0
	je TZ_Trivial

	mov [di + 1], cl
	add di, 2
	cld
	rep movsb

TZ_Trivial:
	mov byte ptr [di + 1], 1

TZ_Exit:
	pop di
	pop si
	pop cx
	ret
TrimZeros ENDP

; процедура беззнакового умножения
; вход:  si - адрес первого массива
;        di - адрес второго массива
;        dx - адрес результата
; выход: dx - адрес результата
UnsignedMultiplication PROC
UM_Start:
	push ax
	push bx
	push cx
	
UM_Main:
	mov bx, dx
	inc si
	xor ch, ch
	mov cl, [si] 
	add [bx + 1], cx
	add si, cx
	
	inc di
	xor ah, ah
	mov al, [di]
	add [bx + 1], ax
	add bx, 2
	
	UM_Loop1:
		add di, ax
		push ax
		push cx
		mov cl, al
		
		UM_Loop2:
			mov al, [di]
			xor ah, ah
			mul byte ptr [si]
			add al, [bx]
			div base
			add [bx + 1], al
			mov [bx], ah
			dec di
			inc bx
			loop UM_Loop2

		pop cx
		pop ax
		sub bx, ax
		inc bx
		dec si
		loop UM_Loop1

UM_Exit:
	pop cx
	pop bx
	pop ax
	ret
UnsignedMultiplication ENDP

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
		jc Error
		PrintNewLine
	endm

	lea si, num1
	lea di, num2
	lea dx, res
	call UnsignedMultiplication
	call Reverse
	call TrimZeros
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
