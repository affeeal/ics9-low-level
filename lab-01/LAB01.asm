assume CS:code,DS:data

data segment
a db 6
b db 2
c db 4
d db 8
res db ?
data ends

code segment
start:
mov AX, data
mov DS, AX
; a/b + d/c -1
mov AH, 0
mov AL, a
div b
mov res, AL
mov AH, 0
mov AL, d
div c
add res, AL
dec res
mov AL, res
; AL = 4
mov AX, 4C00h
int 21h
code ends
end start