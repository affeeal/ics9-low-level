include macro.inc

assume cs: Code, ds: Data

_Stack SEGMENT STACK
	db 128 dup (?)
_Stack ENDS

Data SEGMENT
	max equ 128
	string db max, max+1 dup (?)
	base db 10
Data ENDS

Code SEGMENT
; процедура подсчёта слов в буферизированной строке
; вход:  dx - адрес строки
; выход: al - число слов
CountWords PROC
CW_Start:
	push bx
	push cx
	
CW_Main:
	xor ax, ax
	mov bx, dx
	xor ch, ch
	mov cl, [bx + 1]
	add bx, 2
	jcxz CW_Exit

	CW_Loop:
		cmp byte ptr [bx], ' '
		je CW_Space

		mov ah, 1
		jmp CW_Continue

	CW_Space:
		cmp ah, 0
		je CW_Continue

		inc al
		xor ah, ah
		
	CW_Continue:
		inc bx
		loop CW_Loop

	cmp ah, 0
	je CW_Exit
	
	xor ah, ah
	inc al

CW_Exit:
	pop cx
	pop bx
	ret
CountWords ENDP

Start:
	mov ax, Data
	mov ds, ax
	
	scanString string
	lea dx, string
	call CountWords
	
	printNewLine
	printNumber al
	
	mov ah, 4ch
	int 21h
Code ENDS

END Start
