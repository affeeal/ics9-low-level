assume CS:code, DS:data

; a/b + d/c - 1

data segment
a       db 118
b       db 2
c       db 3
d       db 126
divider db 10
res     db ?
data ends

; 118/2 + 126/3 - 1 = 100

code segment
start:
mov AX, data
mov DS, AX

mov AH, 0
mov AL, a
div b
mov res, AL
xor AH, AH
mov AL, d
div c
add res, AL
dec res

mov AL, res
mov CX, 0

PushDigits:
	xor AH, AH
	div divider
	push AX
	inc CX
	cmp AL, 0
	jne PushDigits

PrintDigits:
	pop DX
	mov DL, DH
	add DL, '0'
	mov AH, 02h
	int 21h
	dec CX
	cmp CX, 0
	jne PrintDigits

mov AX, 4C00h
int 21h

code ends
end start
