	clearPreviousWalls:
		push ax
		push bx
		push cx
		push dx
; Clears walls in middle of room
		mov square_start_x,20		; Move start x into square_start_x
		mov square_start_y,15		; Move start y into square_start_y
		mov square_final_x,300		; Move final x into square_final_x
		mov square_final_y,185		; Move final y into square_final_y
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		MOV BH, 00h					; Draw on page 0
		call drawBlack
; Clears any extra square or wall at top door
		mov square_start_x,132		; CX to set start col
		mov square_start_y,0		; DX to set start row
		mov square_final_x,188		; AX to set length of column size
		mov square_final_y,15		; BX to set length of row size
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		call drawBlack
; Clears any extra square or wall at bottom door
		mov square_start_x,132		; CX to set start col
		mov square_start_y,185		; DX to set start row
		mov square_final_x,188		; AX to set length of column size
		mov square_final_y,200		; BX to set length of row size
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		;MOV BH, 00h					; Draw on page 0
		call drawBlack	
; Clears any extra square or wall at left door
		mov square_start_x,0		; CX to set start col
		mov square_start_y,82		; DX to set start row
		mov square_final_x,20		; AX to set length of column size
		mov square_final_y,120		; BX to set length of row size
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		call drawBlack	
; Clears any extra square or wall at right door
		MOV BH, 00h					; Draw on page 0
		rightDoorLoop:	
		mov square_start_x,300		; CX to set start col
		mov square_start_y,82		; DX to set start row
		mov square_final_x,320		; AX to set length of column size
		mov square_final_y,120		; BX to set length of row size
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		call drawBlack			
		cmp BH,00h
		MOV BH,01h
		je rightDoorLoop
; Draw Missing Tile on Page 1 after drawing black above
		MOV BH, 00h					; Draw on page 0
		leftDoorLoop:
		MOV CX,304		; Draw Tile missing in center row
		MOV DX,96		; DX to set start row at 0
		MOV AX,16		; AX to set length of column size 
		MOV BX,10		; BX to set length of row size
		call wallIt
		call drawMiddleTile
		pop dx
		pop cx
		pop bx
		pop ax
	RET
	
	drawBlack:					; Draws black 
        MOV AH, 0Ch				; Draw character pixel
        MOV AL, 00h				; Color to Black
        INT 10h
		INC CX
        CMP CX, [square_final_x]
        jne drawBlack
        MOV CX, [square_start_x]
        INC DX
        CMP DX, [square_final_y]
        jne drawBlack
	ret
	
	checkRoomChange:	;Checks if moving to another room
		push ax
		push bx
		push cx
		push dx
		MOV AX,square_final_x
		MOV BX,square_final_y
		MOV CX,square_start_x
		MOV DX,square_start_y
		mapAbove:
			cmp DX,0
			JGE mapBelow
			MOV AX,currentRoom
			SUB AX, 5
			MOV currentRoom, AX
			call setLoadingScreen
			call clearPreviousWalls
			call setBackground
			call changeRooms1Thru12
			MOV CX,155		; Sets the starting x to the same value as initial start_x
			MOV DX,189		; Sets the starting y to the bottom door (max y - squareSize)
			MOV BX,199 		; Set final y for top starting square
			MOV AX,165		; Set final x for top starting square
			MOV square_start_x,CX
			MOV square_start_y,DX
			MOV square_final_y,BX
			MOV square_final_x,AX
			call drawNewSquare
			call setGameScreen
			jmp roomChange
		mapBelow:
			cmp BX,200
			JBE mapLeft
			MOV AX,currentRoom
			ADD AX, 5
			cmp AX, 26
			je setWinRoom
			MOV currentRoom, AX
			call setLoadingScreen
			call clearPreviousWalls
			call setBackground
			call changeRooms1Thru12
			MOV CX,155		; Sets the starting x to the same value as initial start_x
			MOV DX,0		; Sets the starting y to the top door 0
			MOV BX,10 		; Set final y for top starting square
			MOV AX,165		; Set final x for top starting square
			MOV square_start_x,CX
			MOV square_start_y,DX
			MOV square_final_y,BX
			MOV square_final_x,AX
			call drawNewSquare
			call setGameScreen
			jmp roomChange
			setWinRoom:
			mov currentScreen,4
			jmp roomChange
		mapLeft:
			cmp CX,0	;Basically Negative 
			JGE mapRight
			MOV AX,currentRoom
			SUB AX,1
			MOV currentRoom, AX
			call setLoadingScreen
			call clearPreviousWalls
			call setBackground
			call changeRooms1Thru12
			MOV CX,309		; Sets the starting x to the right door (max x - squareSize)
			MOV DX,95		; Sets the starting y to the same value as inital start_y
			MOV BX,105 		; Set final y for right starting square
			MOV AX,319		; Set final x for right starting square
			MOV square_start_x,CX
			MOV square_start_y,DX
			MOV square_final_y,BX
			MOV square_final_x,AX
			call drawNewSquare
			call setGameScreen
			jmp roomChange
		mapRight:
			cmp AX,319
			JBE roomChange
			MOV AX,currentRoom
			ADD AX,1
			MOV currentRoom, AX
			call setLoadingScreen
			call clearPreviousWalls
			call setBackground
			call changeRooms1Thru12
			MOV CX,0		; Sets the starting x to the left door (min x)
			MOV DX,94		; Sets the starting y to the same value as inital start_y
			MOV BX,104 		; Set final y for left starting square
			MOV AX,10		; Set final x for left starting square
			MOV square_start_x,CX
			MOV square_start_y,DX
			MOV square_final_y,BX
			MOV square_final_x,AX
			call drawNewSquare
			call setGameScreen
		roomChange:
		MOV AH,0Ch
        MOV AL,00h	; Return nothing
        INT 21h		; INT 21h, AH = 0Ch, AL = 0Ah | Reset input buffer and input
		noRoomChange:
		pop dx
		pop cx
		pop bx
		pop ax
	RET
	
	changeRooms1Thru12:
		push AX
		push BX
		MOV AX,currentRoom
		MOV BX,1
		cmp AX,BX
		je toRoom1
		INC BX
		cmp AX,BX
		je toRoom2
		INC BX
		cmp AX,BX
		je toRoom3
		INC BX
		cmp AX,BX
		je toRoom4
		INC BX
		cmp AX,BX
		je toRoom5
		INC BX
		cmp AX,BX
		je toRoom6
		INC BX
		cmp AX,BX
		je toRoom7
		INC BX
		cmp AX,BX
		je toRoom8
		INC BX
		cmp AX,BX
		je toRoom9
		INC BX
		cmp AX,BX
		je toRoom10
		INC BX
		cmp AX,BX
		je toRoom11
		INC BX
		cmp AX,BX
		je toRoom12
		call changeRooms13Thru25
		jmp roomChanged1
		
		toRoom1:
		call room1
		jmp roomChanged1
		toRoom2:
		call room2
		jmp roomChanged1
		toRoom3:
		call room3
		jmp roomChanged1
		toRoom4:
		call room4
		jmp roomChanged1
		toRoom5:
		call room5
		jmp roomChanged1
		toRoom6:
		call room6
		jmp roomChanged1
		toRoom7:
		call room7
		jmp roomChanged1
		toRoom8:
		call room8
		jmp roomChanged1
		toRoom9:
		call room9
		jmp roomChanged1
		toRoom10:
		call room10
		jmp roomChanged1
		toRoom11:
		call room11
		jmp roomChanged1
		toRoom12:
		call room12
		jmp roomChanged1
		roomChanged1:
		pop BX
		pop AX
		RET
		
		changeRooms13Thru25:
		INC BX
		cmp AX,BX
		je toRoom13
		INC BX
		cmp AX,BX
		je toRoom14
		INC BX
		cmp AX,BX
		je toRoom15
		INC BX
		cmp AX,BX
		je toRoom16
		INC BX
		cmp AX,BX
		je toRoom17
		INC BX
		cmp AX,BX
		je toRoom18
		INC BX
		cmp AX,BX
		je toRoom19
		INC BX
		cmp AX,BX
		je toRoom20
		INC BX
		cmp AX,BX
		je toRoom21
		INC BX
		cmp AX,BX
		je toRoom22
		INC BX
		cmp AX,BX
		je toRoom23
		INC BX
		cmp AX,BX
		je toRoom24
		INC BX
		cmp AX,BX
		je toRoom25
		INC BX
		cmp AX,BX
		je toRoom26 	; This is Win Room
		
		toRoom13:
		call room13
		jmp roomChanged2
		toRoom14:
		call room14
		jmp roomChanged2
		toRoom15:
		call room15
		jmp roomChanged2
		toRoom16:
		call room16
		jmp roomChanged2
		toRoom17:
		call room17
		jmp roomChanged2
		toRoom18:
		call room18
		jmp roomChanged2
		toRoom19:
		call room19
		jmp roomChanged2
		toRoom20:
		call room20
		jmp roomChanged2
		toRoom21:
		call room21
		jmp roomChanged2
		toRoom22:
		call room22
		jmp roomChanged2
		toRoom23:
		call room23
		jmp roomChanged2
		toRoom24:
		call room24
		jmp roomChanged2
		toRoom25:
		call room25
		jmp roomChanged2
		toRoom26:
		mov currentScreen, 4
		roomChanged2:
	RET

	wallIt:
	;Updates wall size:
		push si
		push di
		MOV si, offset sizeWallCol	; Move location of column length to SI
		MOV [si],AX					; Move value from AX to column size
		MOV di,offset sizeWallRow	; Move location of row length to DI
		MOV [di],BX					; Move value from BX to row size
		pop di
		pop si
			
	;Updates wall Start location:
		MOV si, offset col	; Move location of starting column to SI
		MOV [si],CX					; Move value from CX to starting column
		MOV di,offset row	; Move location of starting row to DI
		MOV [di],DX					; Move value from DX to starting row
		MOV si,0
		MOV di,0
	ret
	
	drawOuterWalls:
	;Left Side Walls
		MOV CX,0		; Create 1st Wall from row = 0, col = 0 with x = 40, y = 40
		MOV DX,0		; DX to set start row at 0
		MOV AX,20		; AX to set length of column size 
		MOV BX,82		; BX to set length of row size
		call wallIt
		call drawWalls
		
		MOV CX,0		; CX to set start col
		MOV DX,120		; DX to set start row
		MOV AX,20		; AX to set length of column size
		MOV BX,82		; BX to set length of row size
		call wallIt
		call drawWalls
;Right Side Walls
		MOV CX,300		; CX to set start col
		MOV DX,0		; DX to set start row
		MOV AX,20		; AX to set length of column size 
		MOV BX,82		; BX to set length of row size
		call wallIt
		call drawWalls
		
		MOV CX,300		; CX to set start col
		MOV DX,120		; DX to set start row
		MOV AX,20		; AX to set length of column size
		MOV BX,82		; BX to set length of row size
		call wallIt
		call drawWalls
;Top Wall
		MOV CX,0		; CX to set start col
		MOV DX,0		; DX to set start row
		MOV AX,132		; AX to set length of column size
		MOV BX,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		MOV CX,188		; CX to set start col
		MOV DX,0		; DX to set start row
		MOV AX,132		; AX to set length of column size
		MOV BX,15		; BX to set length of row size
		call wallIt
		call drawWalls
;Bottom Wall
		MOV CX,0		; CX to set start col
		MOV DX,185		; DX to set start row
		MOV AX,132		; AX to set length of column size
		MOV BX,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		MOV CX,188		; CX to set start col
		MOV DX,185		; DX to set start row
		MOV AX,132		; AX to set length of column size
		MOV BX,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
; Check if keys should be drawn at top right inventory

		checkHaveCyan:
		cmp cyanKey,0
		jne drawCyanKey
		mov keyColor, 00h
		mov keyStartY,2
		mov keyStartX,280
		call drawKey
		jmp checkHaveRed
		drawCyanKey:
		mov keyColor, 0Bh
		mov keyStartY,2
		mov keyStartX,280
		call drawKey
		
		checkHaveRed:
		cmp redKey,0
		jne drawRedKey
		mov keyColor, 00h
		mov keyStartY,2
		mov keyStartX,295
		call drawKey
		jmp checkHaveYellow
		drawRedKey:
		mov keyColor, 0Ch
		mov keyStartY,2
		mov keyStartX,295
		call drawKey
		
		checkHaveYellow:
		cmp yellowKey,0
		jne drawYellowKey
		mov keyColor, 00h
		mov keyStartY,2
		mov keyStartX,310
		call drawKey
		jmp checkHaveKeyFin
		drawYellowKey:
		mov keyColor, 0Eh
		mov keyStartY,2
		mov keyStartX,310
		call drawKey
		checkHaveKeyFin:
		
		; Check amount of coins drawn at top left
		
		
		
		
		
	Ret

	drawKey:
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	PUSH SI
	MOV DI,0	; Counter for rows drawn
	MOV SI,0	; Counter for columns drawn
	MOV CX,keyStartX	; Move the start X to CX
	MOV DX,keyStartY	; Move the start Y to DX
	MOV AL,keyColor		; Move the key color into AL to draw correct color pixel
	MOV BH,00			; Write on page 0 (Game Screen)
;These Steps draw all parts of key that have a width of 3 and 2
		keyStep1:		
		MOV AH,0Ch
		int 10h
		INC SI
		INC CX
		cmp SI,3
		jne keyStep1
		ADD DX,4
		MOV SI,0
		keyStep2:
		SUB CX,2
		step2Loop:
		MOV AH,0Ch
		int 10h
		INC SI
		INC CX
		cmp SI,2
		jne step2Loop
		MOV SI,0
		INC DX
		INC DI
		CMP DI,3
		jne keyStep2
		keyStep3:
		INC DX
		MOV DI,0
		keyStep4:
		SUB CX,2
		step4Loop:
		MOV AH,0Ch
		int 10h
		INC SI
		INC CX
		cmp SI,2
		jne step4Loop
		MOV SI,0
		INC DX
		INC DI
		CMP DI,2
		jne keyStep4
;These steps draw key with width of 5 and 7
		SUB CX,4
		MOV SI,0
		MOV DI,0
		keyStep5:
		MOV AH,0Ch
		int 10h
		INC SI
		INC CX
		cmp SI,4
		jne keyStep5
		MOV SI,0
		SUB CX,4
		SUB DX,3
		keyStep6:
		MOV AH,0Ch
		int 10h
		INC SI
		INC CX
		cmp SI,4
		jne keyStep6
		MOV SI,0
		SUB DX,4
		SUB CX,5
		keyStep7:
		MOV AH,0Ch
		int 10h
		INC SI
		INC CX
		cmp SI,7
		jne keyStep7
		MOV SI,0
		INC DI
		DEC DX
		SUB CX,7
		CMP DI,3
		jne keyStep7
		MOV SI,0
		ADD DX,2
		ADD CX,2
		keyCenter:
		cmp AL,00h
		MOV AL,00h
		jne drawCenter
		MOV AL,02h
		drawCenter:
		MOV AH,0Ch
		int 10h
		INC CX
		INC SI
		CMP SI,3
		jne drawCenter
	POP SI
	POP DX
	POP CX
	POP BX
	POP AX
	RET
	
	eraseKey:
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	PUSH SI
	PUSH DI
		MOV CX,keyStartX
		MOV DX,keyStartY
		SUB CX, 2
		MOV SI,0
		MOV DI,0
		eraseKeyLoop:
		MOV AH,0Dh		; Read graphics Pixel 
		MOV BH,01h		; Read from page 1 since this only has tiles
		INT 10h
        MOV AH, 0Ch		; Draw pixel read from page 1
        MOV BH, 00h		; Draw pixels on page 0 (has character)
        INT 10h
        INC CX			; Inc the column (x value) of the square
		INC SI
        CMP SI,7	; Compare if column is at final x 
		jne eraseKeyLoop
		MOV SI,0
		SUB CX,7
		INC DX
		INC DI
		cmp DI,11
		jne eraseKeyLoop
	POP DI
	POP SI
	POP DX
	POP CX
	POP BX
	POP AX
	RET
	
	resetKeys&Doors:
		mov redKey,0
		mov cyanKey,0
		mov yellowKey,0
		mov redDoor,0
		mov cyanDoor,0
		mov yellowDoor,0
	RET
	
	drawTopOrBottomDoor:
		MOV AH,0Ch
		MOV AL, doorColor
		INT 10h
		INC CX
		INC SI
		CMP SI,56
		JNE drawTopOrBottomDoor
		MOV SI,0
		SUB CX,56
		INC DX
		INC DI 
		CMP DI,10
		JNE drawTopOrBottomDoor
		
		MOV SI,0
		MOV DI,0
		CMP DX,20
		JG drawBottomLock
		
		drawTopLock:
		MOV CX,158
		MOV DX,06
		call drawLock
		jmp drawDoorFin
		
		drawBottomLock:
		MOV CX,158
		MOV DX,186
		call drawLock
		
		drawDoorFin:
	RET
	
	
	removeTopDoor:
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	PUSH SI
	PUSH DI
		MOV CX,132
		MOV DX,5
		MOV SI,0
		MOV DI,0
		removeTopDoorLoop:
		MOV AH,0Dh		; Read graphics Pixel 
		MOV BH,01h		; Read from page 1 since this only has tiles
		INT 10h
        MOV AH, 0Ch		; Draw pixel read from page 1 (tiles)
        MOV BH, 00h		; Draw pixels on page 0 (has character)
        INT 10h
        INC CX			; Inc the column
		INC SI			; Inc column counter
		CMP SI,56		
		jne removeTopDoorLoop
		MOV SI,0
		SUB CX,56
		INC DI
		INC DX
		cmp DI,10
		jne removeTopDoorLoop
	POP DI
	POP SI
	POP DX
	POP CX
	POP BX
	POP AX
	RET
	
	removeBottomDoor:
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	PUSH SI
	PUSH DI
		MOV CX,132
		MOV DX,185
		MOV SI,0
		MOV DI,0
		removeBottomDoorLoop:
		MOV AH,0Dh		; Read graphics Pixel 
		MOV BH,01h		; Read from page 1 since this only has tiles
		INT 10h
        MOV AH, 0Ch		; Draw pixel read from page 1 (tiles)
        MOV BH, 00h		; Draw pixels on page 0 (has character)
        INT 10h
        INC CX			; Inc the column
		INC SI			; Inc column counter
		CMP SI,56		
		jne removeBottomDoorLoop
		MOV SI,0
		SUB CX,56
		INC DI
		INC DX
		cmp DI,10
		jne removeBottomDoorLoop
	POP DI
	POP SI
	POP DX
	POP CX
	POP BX
	POP AX
	RET
	
	drawLock:
	MOV SI,0
	MOV DI,0
		topLoop:
		MOV AL,00h	;Make color black for all doors
		MOV AH,0Ch
		MOV BH,00
		int 10h
		INC CX
		INC SI
		CMP SI,4
		JNE topLoop
		SUB CX,4
		MOV SI,0
		INC DI
		INC DX
		CMP DI,4
		JNE topLoop
		
		INC CX
		MOV SI,0
		MOV DI,0
		BottomLoop:
		MOV AH,0Ch
		int 10h
		INC CX
		INC SI
		CMP SI,2
		JNE BottomLoop
		SUB CX,2
		MOV SI,0
		INC DX
		MOV SI,0
		INC DI
		CMP DI,4
		JNE BottomLoop
	RET
	
	drawCoin:	; Coin is Light Green 
	PUSH SI
	PUSH DI
	PUSH CX
	PUSH DX
	MOV DI,0
	MOV SI,0
	MOV AL,0Ah	;Light Green
	topCoin:
	MOV AH,0Ch
	INT 10h
	INC CX
	INC SI
	CMP SI,5
	JNE topCoin
	
	ADD CX,2
	ADD DX,3
	MOV SI,0
	middleCoin:
	MOV AH,0Ch
	INT 10h
	INC DX
	INC SI
	CMP SI,5
	JNE middleCoin
	MOV SI,0
	INC DI
	DEC CX
	SUB DX,5
	CMP DI,11
	JNE middleCoin
	
	POP DX
	POP CX
	MOV SI,0
	INC DX
	DEC CX
	top1Loop:
	MOV AH,0Ch
	INT 10h
	INC CX
	INC SI
	CMP SI,7
	JNE top1Loop
	MOV SI,0
	INC DX
	top2Loop:
	MOV AH,0Ch
	INT 10h
	DEC CX
	INC SI
	CMP SI,9
	JNE top2Loop
	INC CX
	ADD DX,6
	MOV SI,0
	bottom1Loop:
	MOV AH,0Ch
	INT 10h
	INC CX
	INC SI
	CMP SI,9
	JNE bottom1Loop
	MOV SI,0
	INC DX
	SUB CX,2
	bottom2Loop:
	MOV AH,0Ch
	INT 10h
	DEC CX
	INC SI
	CMP SI,7
	JNE bottom2Loop
	INC DX
	ADD CX,2
	MOV SI,0
	bottomCoin:
	MOV AH,0Ch
	INT 10h
	INC CX
	INC SI
	CMP SI,5
	JNE bottomCoin
	SUB CX,5
	SUB DX,8
	MOV AL,02h
	MOV SI,0
	MOV DI,0
	coinLogo:
	MOV AH,0Ch
	INT 10h
	INC DX
	INC SI
	CMP SI,7
	JNE coinLogo
	MOV SI,0
	SUB DX,7
	ADD CX,4
	INC DI
	CMP DI,2
	JNE coinLogo
	SUB CX,4
	ADD DX,7
	SUB DX,6
	DEC CX
	MOV AH,0Ch
	INT 10h
	INC DX
	DEC CX
	MOV AH,0Ch
	INT 10h
	DEC CX
	DEC DX
	MOV AH,0Ch
	INT 10h
	
	POP DI
	POP SI
	RET
	
	eraseCoin:
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	PUSH SI
	PUSH DI
		MOV CX,coinStartX
		MOV DX,coinStartY
		SUB CX,3
		MOV SI,0
		MOV DI,0
		eraseCoinLoop:
		MOV AH,0Dh		; Read graphics Pixel 
		MOV BH,01h		; Read from page 1 since this only has tiles
		INT 10h
        MOV AH, 0Ch		; Draw pixel read from page 1
        MOV BH, 00h		; Draw pixels on page 0 (has character)
        INT 10h
        INC CX			; Inc the column (x value) of the square
		INC SI
        CMP SI,11	; Compare if column is at final x 
		jne eraseCoinLoop
		MOV SI,0
		SUB CX,11
		INC DX
		INC DI
		cmp DI,11
		jne eraseCoinLoop
	POP DI
	POP SI
	POP DX
	POP CX
	POP BX
	POP AX
	
	Ret
	
	drawWalls:
		MOV AH,0ch		; Writes Graphics Pixel
		MOV AL,02h		; Set Color to: Green (02h), Light Green is (0Ah)
		MOV CX,col  	; Sets the Column (X)
		MOV DX,row  	; Sets the Row (Y)
		MOV BH,00		; Set Page to 0
		INT 10h			; Draws the pixel at location row,col
		inc si			; Increment the number of columns drawn
		inc col			; Increment the next column to be drawn
		
		CMP si,sizeWallCol
		je rowWalls
		
		CMP di,sizeWallRow
		jne drawWalls	
	ret
	
	rowWalls:
		push AX
		push BX
		push CX
		push si
		
		MOV si,offset col	
		MOV AX,sizeWallCol	; Move sizeWallCol to AX 
		MOV CX,[si]			; Move current column to CX
		SUB CX,AX			; Subtract current column by wall size
		MOV [si],CX			; Move updated column location to col
		
		pop si
		pop CX
		pop BX
		pop AX
		
		MOV si,0			; Move column counter back to 0
		inc row				; Increment the current row being drawn
		inc di				; Increment row counter 
		CMP di,sizeWallRow	; Compare amount of rows drawn to sizeWallRow
		jne drawWalls		; If rows drawn doesn't equal sizeWallRow, keep drawing
	ret
		
	rowMiddleTile:
		push AX
		push BX
		push CX
		push si
		
		MOV si,offset col	
		MOV AX,sizeWallCol	; Move sizeWallCol to AX 
		MOV CX,[si]			; Move current column to CX
		SUB CX,AX			; Subtract current column by wall size
		MOV [si],CX			; Move updated column location to col
		
		pop si
		pop CX
		pop BX
		pop AX
		
		MOV si,0			; Move column counter back to 0
		inc row				; Increment the current row being drawn
		inc di				; Increment row counter 
		CMP di,sizeWallRow	; Compare amount of rows drawn to sizeWallRow
		jne drawMiddleTile		; If rows drawn doesn't equal sizeWallRow, keep drawing
	ret
	
	drawMiddleTile:
		MOV AH,0ch		; Writes Graphics Pixel
		MOV AL,08h		; Set Color to: Dark Gray
		MOV CX,col  	; Sets the Column (X)
		MOV DX,row  	; Sets the Row (Y)
		MOV BH,01		; Set Page to 1
		INT 10h			; Draws the pixel at location row,col
		MOV AH,0ch		; Writes Graphics Pixel
		MOV AL,08h		; Set Color to: Dark Gray
		MOV CX,col  	; Sets the Column (X)
		MOV DX,row  	; Sets the Row (Y)
		MOV BH,00		; Set Page to 1
		INT 10h			; Draws the pixel at location row,col
		inc si			; Increment the number of columns drawn
		inc col			; Increment the next column to be drawn
		
		CMP si,sizeWallCol
		je rowMiddleTile
		
		CMP di,sizeWallRow
		jne drawMiddleTile
	ret
	
	setBackground:
	push CX
		MOV row,0000h
		MOV col,0000h
		MOV CX,7
		tileLoop:
		push CX
		call firstFloorTiles
		call nextFloorRow
		pop CX
		loop tileLoop
		call firstFloorTiles
		
		MOV CX,0		; Draw Tile missing in center row
		MOV DX,96		; DX to set start row at 0
		MOV AX,12		; AX to set length of column size 
		MOV BX,10		; BX to set length of row size
		call wallIt
		call drawMiddleTile
		
		MOV CX,304		; Draw Tile missing in center row
		MOV DX,96		; DX to set start row at 0
		MOV AX,16		; AX to set length of column size 
		MOV BX,10		; BX to set length of row size
		call wallIt
		call drawMiddleTile
		pop CX
	ret
	
	firstFloorTiles:
		push AX
		push BX
		push CX
		
		MOV di,offset row		; Move location of row to DI
		MOV AX,[di]				; Move current row to AX
		CMP AX,0				; Compare row to 0
		je firstRow				; If current row is 0, skip adding space at start
		MOV si,offset col		; Move location of col to SI
		MOV word ptr [si],0		; Set col to 0
		MOV BX,tileSpacing		; Move spacing variable to BX, (4)
		MOV AX,[di]				; Move current row to AX
		ADD AX,BX				; Add spacing to current row
		MOV [di],AX				; Move sum INTo row variable
		MOV si,0				; Set SI to 0 for draw counter
		MOV di,0				; Set DI to 0 for draw counter
		firstRow:				; Jump Here if at first row (0)
		pop CX
		pop BX
		pop AX
		call drawFloorTile
		
		
	ret
		
	drawFloorTile:
		MOV AH,0ch		; Writes Graphics Pixel
		MOV AL,08h		; Set Color to Dark Gray, Light Gray (07h)
		MOV CX,col  	; Sets the Column (X)
		MOV DX,row  	; Sets the Row (Y)
		MOV BH,01		; Set page to 1
		INT 10h			; Draws the pixel at location row,col
		MOV AH,0ch		; Writes Graphics Pixel
		MOV AL,08h		; Set Color to Dark Gray, Light Gray (07h)
		MOV CX,col  	; Sets the Column (X)
		MOV DX,row  	; Sets the Row (Y)
		MOV BH,00		; Set page to 0
		INT 10h			; Draws the pixel at location row,col
		inc si			; Increment the number of columns drawn
		inc col			; Increment the next column to be drawn
		
		CMP si,tileXLength
		je tileRows
		
		CMP di,tileYLength
		jne drawFloorTile
		
	ret	
		
	tileRows:
		push AX
		push BX
		push CX
		MOV BX, offset col
		MOV AX,tileXLength
		MOV CX,[BX]
		SUB CX,AX		; col = col - xlength
		MOV [BX],CX
		pop CX
		pop BX
		pop AX
		
		MOV si,0		; Column counter back to zero
		inc row			; Increment Row variable
		inc di			; Increment Row counter
		CMP di,tileYLength		; Continue until di is 10
		jne drawFloorTile
		
		CMP CX,284
		jae finish
		call nextTileStart
		finish:
	ret
		
	nextTileStart:
		push AX
		push BX
		push CX
		push si
		MOV AX,tileSpacing
		MOV CX,tileXLength
		ADD AX,CX
		MOV si,offset col
		MOV BX,[si]
		ADD BX,AX
		MOV [si],BX
		
		MOV si,offset row	; Move row location to SI
		MOV BX,[si]			; Move current row to BX
		MOV AX,tileYLength	; Move ylength to AX (8)
		SUB BX,AX			; Subtract row by ylength 
		MOV [si],BX
		
		pop si
		pop CX
		pop BX
		pop AX
		MOV si,0
		MOV di,0
		call drawFloorTile
	ret
		
	nextFloorRow:
		push si
		push AX
		push BX
		push DX
		MOV si,offset row	; Move Row location to SI
		MOV AX,[si]			; Move current row to AX (Should be 10)
		MOV BX,tileSpacing	; Move spacing variable to BX (5)
		ADD AX,BX			; Add current row to spacing variable
		MOV [si],AX			; Move sum INTo Row
		
		MOV si,offset col		; Move Col location to SI
		MOV word ptr [si],16	; Set col variable to this new starting column (Half tile x)
		pop DX
		pop BX
		pop AX
		pop si
		MOV si,0
		MOV di,0
		call drawFloorTile
	ret
	