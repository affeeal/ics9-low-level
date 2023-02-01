assume cs: code, ds: data

_stack segment stack
	db 128 dup (?)
_stack ends

data segment
	max    equ 64
	base   db  10 
	sgn1   dw  ?
	sgn2   dw  ?
	sgn    dw  ?
	num1   db  max, max+1 dup (?)
	num2   db  max, max+1 dup (?)
	res    db  max, 2*max+1 dup (0)
	errMsg db  "The string contains invalid symbol!$"
data ends

code segment
pushEA macro var
	lea dx, var
	push dx
endm

ScanBuff proc
	push bp
	mov bp, sp
	
	push ax
	push dx
	
	mov dx, [bp + 4]
	mov ah, 0ah
	int 21h

	pop dx
	pop ax
	pop bp
	ret 2
ScanBuff endp

NewLine proc
	push ax
	push dx
	
	mov dl, 0ah
	mov ah, 02h
	int 21h

	pop dx
	pop ax
	ret
NewLine endp

SetSign proc
	push bp
	mov bp, sp

	push bx
	push si
	
	mov bx, [bp + 6]
	mov byte ptr [bx], 0
	
	mov si, [bp + 4]
	cmp byte ptr [si + 2], '-'
	jne SetSign_Exit
	
	inc byte ptr [bx]
	
SetSign_Exit:
	pop si
	pop bx
	pop bp
	ret 4
SetSign endp

ASCIIToDigits proc
	push bp
	mov bp, sp
	
	push ax
	push bx
	push cx

	mov bx, [bp + 4]
	xor ch, ch
	mov cl, [bx + 1]
	sub cx, [bp + 6]
	jcxz ASCIIToDigits_Error
	add bx, 2
	add bx, [bp + 6]
	
	ASCIIToDigits_Loop:
		mov al, [bx]
		sub al, '0'
		cmp al, 0
		jl ASCIIToDigits_Error
		cmp al, 9
		jg ASCIIToDigits_Error
		mov [bx], al
		
		inc bx
		loop ASCIIToDigits_Loop
		
	clc
	jmp ASCIIToDigits_Exit
		
ASCIIToDigits_Error:
	stc

ASCIIToDigits_Exit:
	irp reg, <cx, bx, ax, bp>
		pop reg
	endm
	ret 4
ASCIIToDigits endp

PrintStr proc
	push bp
	mov bp, sp

	push ax
	push dx

	mov dx, [bp + 4]
	mov ah, 09h
	int 21h

	irp reg, <dx, ax, bp>
		pop reg
	endm
	ret 2
PrintStr endp

SignedMul proc
	push bp
	mov bp, sp
	
	irp reg, <ax, bx, cx, si, di>
		push reg
	endm
	
	mov bx, [bp + 4]
	mov si, [bp + 6]
	mov di, [bp + 8]

	inc bx
	inc si
	inc di
	
	xor ch, ch
	mov cl, [si] 
	add si, cx
	sub cl, [bp + 12]
	mov [bx], cx
	
	xor ah, ah
	mov al, [di]
	sub al, [bp + 14] 
	add [bx], ax
	
	add di, [bp + 14]
	add bx, ax
	
	SignedMul_Loop1:
		sub bx, ax
		inc bx
		add di, ax
		
		push ax
		push cx
		
		mov cl, al
		
		SignedMul_Loop2:
			mov al, [di]
			xor ah, ah
			mul byte ptr [si]
			add al, [bx]
			div base
			add [bx + 1], al
			mov [bx], ah
			
			dec di
			inc bx
			loop SignedMul_Loop2

		pop cx
		pop ax
		
		dec si
		loop SignedMul_Loop1

	mov si, [bp + 10]
	mov byte ptr [si], 0
	
	mov ax, [bp + 12]
	xor ax, [bp + 14]
	jz SignedMul_Exit

	mov byte ptr [bx + 1], '-'
	mov bx, [bp + 4]
	inc byte ptr [bx + 1]
	inc byte ptr [si]

SignedMul_Exit:
	irp reg, <di, si, cx, bx, ax, bp>
		pop reg
	endm
	ret 12
SignedMul endp

Reverse proc
	push bp
	mov bp, sp
	
	irp reg, <ax, cx, si, di>
		push reg
	endm
	
	mov si, [bp + 4]
	mov di, [bp + 4]
	xor ch, ch
	mov cl, [si + 1]
	add si, 2
	inc di
	add di, cx
	
	shr cl, 1
	jcxz Reverse_Exit

	Reverse_Loop:
		mov al, [si]
		xchg al, [di]
		xchg al, [si]
		
		inc si
		dec di
		loop Reverse_Loop

Reverse_Exit:
	irp reg, <di, si, cx, ax, bp>
		pop reg
	endm
	ret 2
Reverse endp

TrimZeros proc
	push bp
	mov bp, sp

	irp reg, <ax, bx, cx, si, di>
		push reg
	endm
	
	mov di, [bp + 4]
	xor ch, ch
	mov cl, [di + 1]
	
	mov bx, [bp + 6]
	sub cl, [bx]
	
	add di, 2
	add di, [bx]

	cld
	xor al, al
	repe scasb

	dec di
	inc cl

	xchg si, di
	mov di, [bp + 4]
	mov al, [di + 1]
	sub al, [bx]
	cmp cl, al
	je TrimZeros_Exit

	cmp cl, 0
	je TrimZeros_Trivial

	sub al, cl
	sub [di + 1], al
	add di, 2
	add di, [bx]
	rep movsb
	jmp TrimZeros_Exit

TrimZeros_Trivial:
	mov byte ptr [di + 1], 1
	mov byte ptr [di + 2], 0
	mov byte ptr [bx], 0

TrimZeros_Exit:
	irp reg, <di, si, cx, bx, ax, bp>
		pop reg
	endm
	ret 4
TrimZeros endp

DigitsToASCII proc
	push bp
	mov bp, sp
	
	push ax
	push bx
	push cx
	
	mov bx, [bp + 4]
	xor ch, ch
	mov cl, [bx + 1]
	sub cl, [bp + 6]
	add bx, 2
	add bx, [bp + 6]
	
	DigitsToASCII_Loop:
		mov al, [bx]
		add al, '0'
		mov [bx], al
		
		inc bx
		loop DigitsToASCII_Loop
			
	irp reg, <cx, bx, ax, bp>
		pop reg
	endm
	ret 4
DigitsToASCII endp

PrintBuff proc
	push bp
	mov bp, sp
	
	push ax
	push bx
	push dx

	mov bx, [bp + 4]
	xor ah, ah
	mov al, [bx + 1]
	add bx, 2
	add bx, ax
	mov al, [bx]
	mov byte ptr [bx], '$'
	
	mov dx, [bp + 4]
	add dx, 2
	mov ah, 09h
	int 21h
	mov [bx], al

PB_Exit:
	irp reg, <dx, bx, ax, bp>
		pop reg
	endm
	ret 2
PrintBuff endp

Start:
	mov ax, data
	mov ds, ax
	mov es, ax

	irpc n, 12
	local Continue 
		pushEA num&n
		call ScanBuff
		call NewLine
		
		pushEA sgn&n
		pushEA num&n
		call SetSign
		
		push sgn&n
		pushEA num&n
		call ASCIIToDigits
		jnc Continue
		
		pushEA errMsg
		call PrintStr
		jmp Exit
		
	Continue:
	endm
	
	push sgn2
	push sgn1
	irp var, <sgn, num2, num1, res>
		pushEA var
	endm
	call SignedMul
	
	pushEA res
	call Reverse
	
	pushEA sgn
	pushEA res
	call TrimZeros
	
	push sgn
	pushEA res
	call DigitsToASCII
	
	pushEA res
	call PrintBuff
	
Exit:
	call NewLine
	mov ah, 4ch
	int 21h
code ends

end Start
