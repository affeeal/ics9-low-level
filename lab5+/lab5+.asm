use16
org 100h
jmp Start

inputFileName  db "input.txt", 0
outputFileName db "output.txt", 0
errMsg         db "error", 13, 10, '$'
base           db 10

buffer   rb 81
result   rb 41
handle   rw 1

Start:
    mov ah, 3dh
    xor al, al
    mov dx, inputFileName
    xor cx,cx
    int 21h
    jnc @f
	jmp Error
	
@@: mov [handle], ax
    mov bx, ax
    mov ah, 3fh
    mov dx, buffer
    mov cx, 80
    int 21h
    jnc @f
	call CloseFile
	jmp Error

@@: call CloseFile
	call CountWords
	mov dx, result
	call NumberToString
	push ax
	
    mov ah, 3ch
    mov dx, outputFileName
    xor cx, cx
    int 21h
    jnc @F
	jmp Error
	
@@: mov [handle], ax
    mov bx, ax
    mov ah, 40h
    mov dx, result
	pop cx
    int 21h
	call CloseFile
	jmp Exit

CloseFile:
	push ax
	push bx
	
    mov ah, 3eh
    mov bx, [handle]
    int 21h
	jnc @f
	jmp Error
	
@@: pop bx
	pop ax
	ret

; (ax, dx) => ax
CountWords:
	push bx
	push cx
	
	mov bx, dx
	mov cx, ax
	xor ax, ax
	jcxz CountWords_Exit

	CountWords_Loop:
		cmp byte[bx], ' '
		je @f
		cmp byte[bx], 10
		je @f
		mov ah, 1
		jmp CountWords_Continue

	@@: cmp ah, 0
		je CountWords_Continue
		inc al
		xor ah, ah
		
	CountWords_Continue:
		inc bx
		loop CountWords_Loop

	cmp ah, 0
	je CountWords_Exit
	xor ah, ah
	inc al

CountWords_Exit:
	pop cx
	pop bx
	ret

; (ax, dx) => ax
NumberToString:
	push bx
	push cx
	xor cx, cx
	xor bh, bh
	
	@@: xor ah, ah
		div [base]
		mov bl, ah
		add bl, '0'
		push bx
		inc cl
		cmp al, 0
		jne @b
	
	mov bx, dx
	mov dx, cx
	
	@@: pop ax
		mov [bx], al
		inc bx
		loop @b
	
	mov byte[bx], 10
	mov ax, dx
	inc ax
	pop cx
	pop bx
	ret

Error:
    mov ah, 09h
    mov dx, errMsg
    int 21h

Exit:
	mov ah, 4ch
    int 21h
