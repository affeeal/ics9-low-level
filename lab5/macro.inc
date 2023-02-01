scanString MACRO string
	push ax
	push dx
	
	lea dx, string
	mov ah, 0ah
	int 21h

	pop dx
	pop ax
ENDM

printSymbol MACRO sym
	push ax
	push dx

	mov dl, sym
	mov ah, 02h
	int 21h

	pop dx
	pop ax
ENDM

printNewLine MACRO
	printSymbol 0ah
ENDM

printNumber MACRO num
	LOCAL Loop1, Loop2
	
	push ax
	push bx
	push cx
	
	xor bh, bh
	xor cx, cx
	mov al, num	
	
	Loop1:
		xor ah, ah
		div base
	
		mov bl, ah
		add bl, '0'
		push bx
		
		inc cl
		cmp al, 0
		jne Loop1
	
	Loop2:
		pop ax
		printSymbol al
		loop Loop2
	
	PrintNewLine
	
	pop cx
	pop bx
	pop ax
ENDM
