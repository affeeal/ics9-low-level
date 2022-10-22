ASSUME CS: CODE, DS: DATA

DATA SEGMENT
string1 DB 250, 249 DUP (0)
string2 DB 250, 249 DUP (0)
num     DB 6,   5   DUP (0)
DATA ENDS

CODE SEGMENT
NewLine PROC
	MOV AH, 02h
	MOV DL, 0Ah
	INT 21H
	RET
NewLine ENDP

ParseInt PROC
	PUSH BP
	MOV BP, SP

	XOR AX, AX
	XOR BL, BL
	XOR CX, CX
	MOV DI, 10
	MOV SI, [BP + 4]
	MOV CL, [SI + 1]
	ADD SI, 2

	Parse:
		MOV BL, [SI]
		INC SI
		MUL DI
		SUB BL, '0'
		ADD AL, BL
		LOOP Parse

	POP BP
	POP BX
	PUSH AX
	PUSH BX
	RET 
ParseInt ENDP

strncmp PROC
	PUSH BP
	MOV BP, SP
	
	XOR DX, DX
	MOV SI, [BP + 4]
	ADD SI, 2
	MOV DI, [BP + 6]
	ADD DI, 2
	MOV CL, [BP + 8]
	
	REPE CMPSB
	JZ Return
	JS SecondLonger
	JMP FirstLonger

	FirstLonger:
		INC DX
		JMP Return

	SecondLonger:
		DEC DX
		JMP Return

	Return:
		POP BP
		POP AX
		PUSH DX
		PUSH AX
		RET
strncmp ENDP

Start:
	MOV AX, DATA
	MOV DS, AX
	MOV ES, AX

	MOV DX, OFFSET string1
	MOV AH, 0Ah
	INT 21h
	CALL NewLine

	MOV DX, OFFSET string2
	MOV AH, 0Ah
	INT 21h
	CALL NewLine

	MOV DX, OFFSET num
	MOV AH, 0Ah
	INT 21h

	PUSH DX
	CALL ParseInt		

	MOV DX, OFFSET string2
	PUSH DX
	MOV DX, OFFSET string1
	PUSH DX
	CALL strncmp	

	CALL NewLine
	MOV AH, 02h
	POP BX
	CMP BX, 0
	JE EqualZero
	JG GreaterZero
	JL LessZero

	EqualZero:
		MOV DL, '0'
		JMP PrintResult

	GreaterZero:
		MOV DL, '1'
		JMP PrintResult

	LessZero:
		MOV DL, '-'
		INT 21h
		MOV DL, '1'
		JMP PrintResult

	PrintResult:
		INT 21h

	MOV AX, 4C00h
	INT 21h
CODE ENDS
END Start
