	bits 16
	%use masm
	
	%include "timer.inc"
	%include "sound.inc"
;NASM test, combining all files into one. Will mark files with comments.		

segment code ;Code Segment
; Constants Lookup
; Page 0 = Game Screen | Walls, Tiles, and Character
; Page 1 = Tile Screen | Tiles
	..start:
	
	MOV AX, data	;\ Initialize the data segment
	MOV ds,AX		;/
	
	mov ax, stack
	mov ss, ax
	mov sp, stacktop
	

	call graphics

	; Set the square size before program begins
	MOV AX, square_final_x
	SUB AX, square_start_x
	MOV [squareSize], AX
	
	call titleScreen
	call pauseScreen
	;call winScreen		   
	call creditScreen
	call deathScreen
	
	call far install_timer_handler
	call far start_fast_clock
;Start of pro	
	gameLoop:
		;Used for Pages with Inputs
		cmp word [currentScreen], 0000h
		jne page0
			call setGameScreen
			call gameInput
		page0:
			cmp word [currentScreen], 0003h
		jne page3
			call setTitleScreen
			call main_screen_input
		page3:
			cmp word [currentScreen], 0004h
		jne page4
			call setWinScreen
			call secDelay
			call resetInput
			call winInput
		page4:
			cmp word [currentScreen], 0005h
		jne page5
			call setPauseScreen
			call pausedInput
		page5:
			cmp word [currentScreen], 0006h
		jne page6
			call setCreditScreen
			call creditInput
		page6:
	jmp gameLoop
	
	terminate:
		call far stop_fast_clock
		call far uninstall_timer_handler
		;Terminates the program and returns to text mode
        MOV AH,00h ;
        MOV AL,03h ; Graphics mode -> text mode 40x25
        INT 10h    ; INT 10h, AH = 00h | Set Video Mode
        MOV AH,4Ch
		MOV AL,00h
        INT 21h    ; INT 21h, AH = 4Ch, AL = exit code | Terminate program with exit code
    ret
	
	resetInput:
		MOV AH,0Ch
        MOV AL,00h	; Return nothing
        INT 21h		; INT 21h, AH = 0Ch, AL = 0Ah | Reset input buffer and input
	RET
	
	black_screen: 
						; Sets All Pixels in the Screen to Black
		mov ah,0fh
		int 10h
		MOV CX, 0       ; Column starts at square_start ends at square_end
		MOV DX, 0       ; Row Starts at 20 and ends at 40 for all squares
		blackrow:       ;
		MOV AH, 0Ch     ; Setup for INT 10h change color for a single pixel
		MOV AL, 00h     ; Access current color set color for pixel
		int 10h         ; Change pixel color
		INC CX          ; Increment the Column (CX)
		CMP CX, 320d    ; Compare CX to square_end
		jng blackrow    ; IF CX is not greater than square_end start next column ( greater than because increment is done first )
		MOV CX, 0       ; Starting new row at column square_start
		INC DX          ; Increment the Row (DX)
		CMP DX,200d     ; Compare DX to 40 the final row
		jng blackrow    ; If DX is not greater than square_end start next row ( greater than because increment is done first )
	ret
    delay:
		; Delays for 1 second
		MOV CX,000Fh
		MOV DX,4240h    
		MOV AH,86h      
		INT 15h         ; INT 15h, AH = 86h, CX:DX number of microseconds to elapse | Wait
						; 000F 0000 + 0000 4240 = 1,000,000 microseconds = 1 second
    ret

    graphics:
		;Changes video mode to 320x200 16 color graphics with 8 pages
        MOV AH,00h
        MOV AL,0Dh	
        INT 10h		; INT 10h, AH = 00h, AL = mode | Set Video Mode
    ret

	secDelay:			;Roughly delays for a second when called
		push AX
		MOV AX,0FFFFh
		call restLoop
		call restLoop
		call restLoop
		call restLoop
		call restLoop
		call restLoop
		cmp word [currentScreen],4
		jne smallerSec
		call restLoop
		call restLoop
		call restLoop
		call restLoop
		call restLoop
		smallerSec:
		pop AX
	ret
		
	restLoop:			; Used for secDelay Loop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		dec AX
		loopnz restLoop
	ret

;end of proto
;start of room

	clearPreviousWalls:
		push ax
		push bx
		push cx
		push dx
; Clears walls in middle of room
		mov word [square_start_x],20		; Move start x into square_start_x
		mov word [square_start_y],15		; Move start y into square_start_y
		mov word [square_final_x],300		; Move final x into square_final_x
		mov word [square_final_y],185		; Move final y into square_final_y
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		MOV BH, 00h					; Draw on page 0
		call drawBlack
; Clears any extra square or wall at top door
		mov word [square_start_x],132		; CX to set start col
		mov word [square_start_y],0		; DX to set start row
		mov word [square_final_x],188		; AX to set length of column size
		mov word [square_final_y],15		; BX to set length of row size
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		call drawBlack
; Clears any extra square or wall at bottom door
		mov word [square_start_x],132		; CX to set start col
		mov word [square_start_y],185		; DX to set start row
		mov word [square_final_x],188		; AX to set length of column size
		mov word [square_final_y],200		; BX to set length of row size
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		;MOV BH, 00h					; Draw on page 0
		call drawBlack	
; Clears any extra square or wall at left door
		mov word [square_start_x],0		; CX to set start col
		mov word [square_start_y],82		; DX to set start row
		mov word [square_final_x],20		; AX to set length of column size
		mov word [square_final_y],120		; BX to set length of row size
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		call drawBlack	
; Clears any extra square or wall at right door
		MOV BH, 00h					; Draw on page 0
		rightDoorLoop:	
		mov word [square_start_x],300		; CX to set start col
		mov word [square_start_y],82		; DX to set start row
		mov word [square_final_x],320		; AX to set length of column size
		mov word [square_final_y],120		; BX to set length of row size
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		call drawBlack			
		cmp BH,00h
		MOV BH,01h
		je rightDoorLoop
; Draw Missing Tile on Page 1 after drawing black above
		MOV CX,307		; Draw Tile missing in center row
		MOV DX,96		; DX to set start row at 0
		MOV AX,13		; AX to set length of column size 
		MOV BX,10		; BX to set length of row size
		call wallIt
		call drawMiddleTiles
		MOV CX,300		; Draw Tile missing in center row
		MOV DX,96		; DX to set start row at 0
		MOV AX,2		; AX to set length of column size 
		MOV BX,10		; BX to set length of row size
		call wallIt
		call drawMiddleTiles
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
		MOV AX,[square_final_x]
		MOV BX,[square_final_y]
		MOV CX,[square_start_x]
		MOV DX,[square_start_y]
		mapAbove:
			cmp DX,0
			JGE mapBelow
			MOV AX,[currentRoom]
			SUB AX, 5
			MOV word [currentRoom], AX
			call setLoadingScreen
			call clearPreviousWalls
			MOV BH,00
			call setBackground
			call changeRooms1Thru12
			MOV CX,155		; Sets the starting x to the same value as initial start_x
			MOV DX,190		; Sets the starting y to the bottom door (max y - squareSize)
			MOV BX,200 		; Set final y for top starting square
			MOV AX,165		; Set final x for top starting square
			MOV [square_start_x],CX
			MOV [square_start_y],DX
			MOV [square_final_y],BX
			MOV [square_final_x],AX
			call drawNewSquare
			call setGameScreen
			jmp roomChange
		mapBelow:
			cmp BX,200
			JBE mapLeft
			MOV AX,[currentRoom]
			ADD AX, 5
			cmp AX, 26
			je setWinRoom
			MOV [currentRoom], AX
			call setLoadingScreen
			call clearPreviousWalls
			MOV BH,00
			call setBackground
			call changeRooms1Thru12
			MOV CX,155		; Sets the starting x to the same value as initial start_x
			MOV DX,0		; Sets the starting y to the top door 0
			MOV BX,10 		; Set final y for top starting square
			MOV AX,165		; Set final x for top starting square
			MOV [square_start_x],CX
			MOV [square_start_y],DX
			MOV [square_final_y],BX
			MOV [square_final_x],AX
			call drawNewSquare
			call setGameScreen
			jmp roomChange
			setWinRoom:
			mov word [currentScreen], 0004h
			jmp roomChange
		mapLeft:
			cmp CX,0	;Basically Negative 
			JGE mapRight
			MOV AX,[currentRoom]
			SUB AX,1
			MOV [currentRoom], AX
			call setLoadingScreen
			call clearPreviousWalls
			MOV BH,00
			call setBackground
			call changeRooms1Thru12
			MOV CX,309		; Sets the starting x to the right door (max x - squareSize)
			MOV DX,95		; Sets the starting y to the same value as inital start_y
			MOV BX,105 		; Set final y for right starting square
			MOV AX,319		; Set final x for right starting square
			MOV [square_start_x],CX
			MOV [square_start_y],DX
			MOV [square_final_y],BX
			MOV [square_final_x],AX
			call drawNewSquare
			call setGameScreen
			jmp roomChange
		mapRight:
			cmp AX,319
			JBE roomChange
			MOV AX,[currentRoom]
			ADD AX,1
			MOV [currentRoom], AX
			call setLoadingScreen
			call clearPreviousWalls
			MOV BH,00
			call setBackground
			call changeRooms1Thru12
			MOV CX,0		; Sets the starting x to the left door (min x)
			MOV DX,94		; Sets the starting y to the same value as inital start_y
			MOV BX,104 		; Set final y for left starting square
			MOV AX,10		; Set final x for left starting square
			MOV [square_start_x],CX
			MOV [square_start_y],DX
			MOV [square_final_y],BX
			MOV [square_final_x],AX
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
		MOV AX, word [currentRoom]
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
		mov word [currentScreen], 0004h
		roomChanged2:
	RET
	
		drawWalls:
		MOV AH,0ch		; Writes Graphics Pixel
		MOV AL,02h		; Set Color to: Green (02h), Light Green is (0Ah)
		MOV CX,word [col]  	; Sets the Column (X)
		MOV DX,word [row] 	; Sets the Row (Y)
		MOV BH,00		; Set Page to 0
		INT 10h			; Draws the pixel at location row,col
		inc si			; Increment the number of columns drawn
		inc word [col]	; Increment the next column to be drawn
		
		CMP si,word [sizeWallCol]
		je rowWalls
		
		CMP di,word [sizeWallRow]
		jne drawWalls	
	ret
	
	rowWalls:
		push AX
		push BX
		push CX
		push si
		
;MOV si,offset col	
		MOV AX,word [sizeWallCol]	; Move sizeWallCol to AX 
		MOV CX,word [col]			; Move current column to CX
		SUB CX,AX			; Subtract current column by wall size
		MOV word [col],CX			; Move updated column location to col
		
		pop si
		pop CX
		pop BX
		pop AX
		
		MOV si,0			; Move column counter back to 0
		inc word [row]		; Increment the current row being drawn
		inc di				; Increment row counter 
		CMP di,word [sizeWallRow]	; Compare amount of rows drawn to sizeWallRow
		jne drawWalls		; If rows drawn doesn't equal sizeWallRow, keep drawing
	ret
		
		rowMiddleTile:
		push AX
		push BX
		push CX
		push si
		
;MOV si,offset col	
		MOV AX,word [sizeWallCol]	; Move sizeWallCol to AX 
		MOV CX,[col]			; Move current column to CX
		SUB CX,AX			; Subtract current column by wall size
		MOV word [col],CX			; Move updated column location to col
		
		pop si
		pop CX
		pop BX
		pop AX
		
		MOV si,0			; Move column counter back to 0
		inc word [row]		; Increment the current row being drawn
		inc di				; Increment row counter 
		CMP di,word [sizeWallRow]	; Compare amount of rows drawn to sizeWallRow
		jne drawMiddleTiles	; If rows drawn doesn't equal sizeWallRow, keep drawing
	ret
	
		drawMiddleTiles:
		MOV AH,0ch		; Writes Graphics Pixel
		MOV AL,08h		; Set Color to: Dark Gray
		MOV CX,word [col]  	; Sets the Column (X)
		MOV DX,word [row]  	; Sets the Row (Y)
		MOV BH,01		; Set Page to 1
		INT 10h			; Draws the pixel at location row,col
		MOV AH,0ch		; Writes Graphics Pixel
		MOV AL,08h		; Set Color to: Dark Gray
		MOV CX,word [col]  	; Sets the Column (X)
		MOV DX,word [row]  	; Sets the Row (Y)
		MOV BH,00		; Set Page to 1
		INT 10h			; Draws the pixel at location row,col
		inc si			; Increment the number of columns drawn
		inc word [col]	; Increment the next column to be drawn
		
		CMP si,word [sizeWallCol]
		je rowMiddleTile
		
		CMP di,word [sizeWallRow]
		jne drawMiddleTiles
	ret
	
	setBackground:
	PUSH SI
	PUSH DI
	PUSH AX
	PUSH BX
	push CX
	PUSH DX
		MOV word [row],0000h
		MOV word [col],0000h
		MOV SI,0
		MOV DI,0
		CMP BH,0
		JE backgroundPage0
		MOV CX,7
		tileLoop1:
		push CX
		MOV BH,1
		call firstFloorTiles
		call nextFloorRow
		pop CX
		loop tileLoop1			;Will loop the first and second tile rows 7 times 
		call firstFloorTiles	; Need to add additional first row to finish the bottom of the tiles
		
		MOV word [row],0000h
		MOV word [col],0000h
		MOV SI,0
		MOV DI,0
		backgroundPage0:
		MOV CX,7
		tileLoop0:
		MOV BH,0
		push CX
		call firstFloorTiles
		call nextFloorRow
		pop CX
		loop tileLoop0			;Will loop the first and second tile rows 7 times 
		call firstFloorTiles	; Need to add additional first row to finish the bottom of the tiles
		
		
		MOV CX,0		; Draw Tile missing in center row
		MOV DX,96		; DX to set start row at 0
		MOV AX,14		; AX to set length of column size 
		MOV BX,10		; BX to set length of row size
		call wallIt
		call drawMiddleTiles
		
		MOV CX,306		; Draw Tile missing in center row
		MOV DX,96		; DX to set start row at 0
		MOV AX,14		; AX to set length of column size 
		MOV BX,10		; BX to set length of row size
		call wallIt
		call drawMiddleTiles
		
		MOV CX,300
		MOV DX,82
		MOV AX,20
		MOV BX,10
		call wallIt
		call drawMiddleTiles
		
		MOV CX,300
		MOV DX,110
		MOV AX,20
		MOV BX,10
		call wallIt
		call drawMiddleTiles
		
		POP DX
		POP CX
		POP BX
		POP AX
		POP DI
		POP SI
	ret
	
	firstFloorTiles:
		push AX
		push BX
		MOV AX,word [row]		; Move current row to AX
		CMP AX,0				; Compare row to 0
		je firstRow				; If current row is 0, skip adding space at start
		MOV word [col],0
		MOV BX,4		; Move spacing variable to BX, (4)
		MOV AX,word [row]				; Move current row to AX
		ADD AX,BX				; Add spacing to current row
		MOV word [row],AX		; Move sum INTo row variable
		MOV si,0				; Set SI to 0 for draw counter
		MOV di,0				; Set DI to 0 for draw counter
		jmp doneRows
		firstRow:				; Jump Here if at first row (0)
		DEC word [row]		;For some reason first rows off by 2
		DEC word [row]		;So decrement rows by 2 which makes it same as current implementation
		doneRows:
		pop BX
		pop AX
		call drawFloorTile
	ret
	
	drawFloorTile:
		MOV AH,0ch		; Writes Graphics Pixel
		MOV AL,08h		; Set Color to Dark Gray, Light Gray (07h)
		MOV CX,word [col]  	; Sets the Column (X)
		MOV DX,word [row]  	; Sets the Row (Y)
		INT 10h			; Draws the pixel at location row,col
		inc si			; Increment the number of columns drawn
		inc word [col]	; Increment the next column to be drawn
		CMP si,32	;TileXLength always 32
		je tileRows
		CMP di,10	;TileYLength always 32
		jne drawFloorTile
	ret	
	
	tileRows:
		push AX
		push CX
;MOV BX, offset col
		MOV AX,32	;TileXLength always 32
;MOV CX,[BX]
		MOV CX,word [col]
		SUB CX,AX		; col = col - xlength
		MOV word [col],CX
		pop CX
		pop AX
		MOV si,0				; Column counter back to zero
		inc word [row]			; Increment Row variable
		inc di					; Increment Row counter
		CMP di, 10	;TileYLength always 10	; Continue until di is 10
		jne drawFloorTile
		CMP CX, 284
		JAE tileRowsFin
		call nextTileStart
		tileRowsFin:
	ret
		
	nextTileStart:
		push AX
		push BX
		push CX
		push si
		MOV AX,4	;Tile spacing always 4
		MOV CX,32	;TileXLength always 32
		ADD AX,CX
		MOV BX,word [col]
		ADD BX,AX
		MOV word [col],BX
		
		MOV BX,word [row]			; Move current row to BX
		MOV AX,10		;TileYLength 10
		SUB BX,AX		;Subtract row by ylength 
		MOV word [row],BX
		pop si
		pop CX
		pop BX
		pop AX
		MOV si,0
		MOV di,0
		call drawFloorTile
	ret
		
	nextFloorRow:
		push AX
		push BX
		MOV AX,word [row]			; Move current row to AX (Should be 10)
		MOV BX,4	; Move spacing variable to BX (4)
		ADD AX,BX			; Add current row to spacing variable
		MOV word [row],AX			; Move sum INTo Row
		MOV word [col],18	; Set col variable to this new starting column (Half tile x)
		pop BX
		pop AX
		MOV si,0
		MOV di,0
		call drawFloorTile
	ret
	
	drawKey:
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	PUSH SI
	MOV DI,0	; Counter for rows drawn
	MOV SI,0	; Counter for columns drawn
	MOV CX, word [keyStartX]	; Move the start X to CX
	MOV DX, word [keyStartY]	; Move the start Y to DX
	MOV AL, byte [keyColor]		; Move the key color into AL to draw correct color pixel
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
		MOV CX,word [keyStartX]
		MOV DX,word [keyStartY]
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
	
	resetKeysDoorsCoins:
		mov byte [redKey],0
		mov byte [cyanKey],0
		mov byte [magentaKey],0
		mov byte [redDoor],0
		mov byte [cyanDoor],0
		mov byte [magentaDoor],0
		mov byte [coin1],0
		mov byte [coin2],0
		mov byte [coin3],0
	RET
	
	drawTopOrBottomDoor:
		MOV AH,0Ch
		MOV AL, byte [doorColor]
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
	MOV CX,word [coinStartX]
	MOV DX,word [coinStartY]
	PUSH SI
	PUSH DI
	PUSH CX
	PUSH DX
	MOV DI,0
	MOV SI,0
	MOV AL,byte [coinColor]	;Light Green
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
	CMP AL,00h
	JE blackCoin
	MOV AL,02h
	blackCoin:
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
		
		MOV CX,word [coinStartX]
		MOV DX,word [coinStartY]
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
	RET
	
	drawLavaPit:
	PUSH SI
	PUSH DI
	MOV SI,0
	MOV DI,0
	lavaPitLoop:
	call drawLavaTexture
	INC SI		;SI will count the columns drawn
	CMP SI,BX	;BX stores the width of the lava pits (amount of textures to draw
	JNE lavaPitLoop
	MOV SI,0
	PUSH AX
	PUSH DX
	MOV AX,BX
	MOV DX, 6
	MUL DX
	SUB CX,AX
	POP DX
	POP AX
	INC DI		;DI will count the rows drawn
	ADD DX,6
	CMP DI,BP	;BP stores the length of the lava pits
	JNE lavaPitLoop
	DEC DX
	PUSH DX
	MOV DX,6
	MOV AX,BX
	MUL DX
	ADD CX,AX
	MOV AX,BP
	MOV DX,6
	MUL DX
	MOV BP,AX
	POP DX
	MOV DI,0
	MOV AL,0Eh
	MOV BH,00
	rightYellow:
	MOV AH,0Ch
	INT 10h
	DEC DX
	INC DI
	CMP DI,BP
	JNE rightYellow
	POP DI
	POP SI
	RET
	
	drawLavaTexture:
	PUSH BX
	PUSH SI
	PUSH DI
	MOV SI,0
	MOV DI,0
	MOV AL,04h
	MOV BH,0
	INC CX
	INC DX
	brownMiddle:
	MOV AH,0Ch
	INT 10h
	INC CX
	INC SI
	CMP SI,5	;Draw 5 colums of brown, some will be erased but more memory efficient this way
	JNE brownMiddle
	MOV SI,0
	SUB CX,5
	INC DX
	INC DI
	CMP DI,4
	JNE brownMiddle
	MOV SI,0
	MOV DI,0
	MOV AL,0Eh
	DEC CX			;After brown middle, decrement CX to be at bottom left corner
	yellowOutsideBottom:
	MOV AH,0Ch
	INT 10h
	INC CX
	INC SI
	CMP SI,6		; Draw all yellow at bottom of lava texture
	JNE yellowOutsideBottom
	MOV SI,0
;Yellows located on the brown square previously drawn
	DEC CX
	DEC DX
	MOV AH,0Ch
	INT 10h		; Draws at bottom right inside yellow
	SUB DX,3
	MOV AH,0Ch
	INT 10h		; Draws at top right inside yellow
	SUB CX,4
	MOV AH,0Ch
	INT 10h		; Draws at top left inside yellow
	ADD DX,2
	MOV AH,0Ch
	INT 10h		; Draws at left bottom inside yellow 
	INC CX
	INC DX
	MOV AH,0Ch	; Draws at bottom left inside yellow
	INT 10h
	SUB CX,2
	yellowOutsideLeft:
	MOV AH,0Ch
	Int 10h
	DEC DX
	INC SI
	CMP SI,4
	JNE yellowOutsideLeft
	MOV SI,0
	yellowOutsideTop:
	MOV AH,0Ch
	INT 10h
	INC CX
	INC SI
	CMP SI,6
	JNE yellowOutsideTop
	POP DI
	POP SI
	POP BX
	Ret
	
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
		MOV word [col],CX					; Move value from CX to starting column
		MOV word [row],DX					; Move value from DX to starting row
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
		cmp byte [cyanKey],0
		jne drawCyanKey
		mov byte [keyColor], 00h	;00h = Black 
		mov word [keyStartY],2		;Start Row of Key 
		mov word [keyStartX],280
		call drawKey
		jmp checkHaveRed
		drawCyanKey:
		mov byte [keyColor], 0Bh	;0Bh = Light Cyan
		mov word [keyStartY],2
		mov word [keyStartX],280
		call drawKey
	checkHaveRed:
		cmp byte [redKey],0
		jne drawRedKey
		mov byte [keyColor], 00h
		mov word [keyStartY],2
		mov word [keyStartX],295
		call drawKey
		jmp checkHaveMagenta
		drawRedKey:
		mov byte [keyColor], 0Ch	;0Ch = Light Red
		mov word [keyStartY],2
		mov word [keyStartX],295
		call drawKey
	checkHaveMagenta:
		cmp byte [magentaKey],0
		jne drawMagentaKey
		mov byte [keyColor], 00h
		mov word [keyStartY],2
		mov word [keyStartX],310
		call drawKey
		jmp checkHaveKeyFin
		drawMagentaKey:
		mov byte [keyColor], 05h	;05h = Magenta 
		mov word [keyStartY],2
		mov word [keyStartX],310
		call drawKey
		checkHaveKeyFin:
	; Check amount of coins drawn at top left
	checkCoin1:
		MOV word [coinStartX],75	; Initialize starting col for coin 1
		MOV word [coinStartY],2		; Initialize starting row for coin 1
		cmp byte [coin1],0		; Check if coin1 was picked up, 0 if not, 1 if obtained
		jne drawCoin1			; If coin 1 obtained, jump to drawCoin1
		MOV byte [coinColor],00h	; Draw black coin if coin 1 not obtained
		call drawCoin				; Draw the missing coin in inventory
		jmp checkCoin2				; Check for coin 2 now
		drawCoin1:		
		MOV byte [coinColor],0Ah	; Move Light Green to coin color to draw obtained coin
		call drawCoin			; Call subroutine to draw coin in inventory
	checkCoin2:	
		MOV word [coinStartX],90
		MOV word [coinStartY],2
		cmp byte [coin2],0
		jne drawCoin2
		MOV byte [coinColor],00h	;Draw black coin if no coin 1
		call drawCoin
		jmp checkCoin3
		drawCoin2:
		MOV byte [coinColor],0Ah
		call drawCoin
	checkCoin3:
		MOV word [coinStartX],105
		MOV word [coinStartY],2
		cmp byte [coin3],0
		jne drawCoin3
		MOV byte [coinColor],00h	;Draw black coin if no coin 1
		call drawCoin
		jmp checkCoinFin
		drawCoin3:
		MOV byte [coinColor],0Ah
		call drawCoin
		checkCoinFin:
		call updateHealth
	ret
;end of room

;start of rooms
room1:
	;Walter2
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		call drawOuterWalls
	;Top Middle Walls
		mov cx,132		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,56		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		mov cx,112		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,160		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		mov cx,188		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,160		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Top Middle	
		mov cx,132		; CX to set start col
		mov dx,0		; DX to set start row
		mov ax,56		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Left Middle
		mov cx,0		; CX to set start col
		mov dx,82		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,38		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls 
		
		cmp byte [magentaKey],1
		je magentaKeyObtained
		mov word [keyStartY],95
		mov word [keyStartX],158
		mov byte [keyColor], 05h	; Draw Light Magenta key 
		call drawKey
		magentaKeyObtained:
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET

	room2:
		;Walter 1
		push AX
		push BX
		push CX
		push DX
		push si
		push di
	MOV CX,67	; Starting Column
		MOV DX,85	; Starting Row
		MOV BX,10	;Number of (6x6) textures drawn to the right of start position
		MOV BP,7	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,115	; Starting Column
		MOV DX,67	; Starting Row
		MOV BX,10	;Number of (6x6) textures drawn to the right of start position
		MOV BP,19	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit						 
		call drawOuterWalls
		;Top Middle Walls
		mov cx,50		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,220		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,50		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,30		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,188		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,160		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	
    ;Bottom Middle Walls
		mov cx,50		; CX to set start col
		mov dx,145		; DX to set start row
		mov ax,82		; AX to set length of column size
		mov bx,17		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,50		; CX to set start col
		mov dx,130		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,30		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,100		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,50		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		;Top Middle	
		mov cx,132		; CX to set start col
		mov dx,0		; DX to set start row
		mov ax,56		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
	RET
	
	room3:
		;Template
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
		MOV CX,18	; Starting Column
		MOV DX,15	; Starting Row
		MOV BX,47	;Number of (6x6) textures drawn to the right of start position
		MOV BP,2	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,84	; Starting Column
		MOV DX,74	; Starting Row
		MOV BX,3	;Number of (6x6) textures drawn to the right of start position
		MOV BP,9	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,218	; Starting Column
		MOV DX,74	; Starting Row
		MOV BX,3	;Number of (6x6) textures drawn to the right of start position
		MOV BP,9	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,103	; Starting Column
		MOV DX,92	; Starting Row
		MOV BX,19	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,18	; Starting Column
		MOV DX,120	; Starting Row
		MOV BX,2	;Number of (6x6) textures drawn to the right of start position
		MOV BP,11	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,289	; Starting Column
		MOV DX,120	; Starting Row
		MOV BX,2	;Number of (6x6) textures drawn to the right of start position
		MOV BP,11	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		call drawOuterWalls
    ;Top Middle Walls
		MOV CX,50		; CX to set start col
		MOV DX,40		; DX to set start row
		MOV AX,220		; AX to set length of column size
		MOV BX,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		MOV CX,50		; CX to set start col
		MOV DX,40		; DX to set start row
		MOV AX,20		; AX to set length of column size
		MOV BX,30		; BX to set length of row size
		call wallIt
		call drawWalls
		
		MOV CX,250		; CX to set start col
		MOV DX,40		; DX to set start row
		MOV AX,20		; AX to set length of column size
		MOV BX,30		; BX to set length of row size
		call wallIt
		call drawWalls
	
    ;Bottom Middle Walls
		MOV CX,50		; CX to set start col
		MOV DX,145		; DX to set start row
		MOV AX,220		; AX to set length of column size
		MOV BX,17		; BX to set length of row size
		call wallIt
		call drawWalls
		
		MOV CX,50		; CX to set start col
		MOV DX,130		; DX to set start row
		MOV AX,20		; AX to set length of column size
		MOV BX,30		; BX to set length of row size
		call wallIt
		call drawWalls
		
		MOV CX,250		; CX to set start col
		MOV DX,130		; DX to set start row
		MOV AX,20		; AX to set length of column size
		MOV BX,30		; BX to set length of row size
		call wallIt
		call drawWalls
		
		;Top Middle	
		mov cx,132		; CX to set start col
		mov dx,0		; DX to set start row
		mov ax,56		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET
	
	room4:
	;Jimmy 3
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		call drawOuterWalls
	;Top Middle
		mov cx,86		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,148		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
	;Bottom Middle	
		mov cx,86		; CX to set start col
		mov dx,150		; DX to set start row
		mov ax,148		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
	;Left Middle
		mov cx,50		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,95		; BX to set length of row size
		call wallIt
		call drawWalls
	;Right Middle
		mov cx,255		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,95		; BX to set length of row size
		call wallIt
		call drawWalls
	;Middle
		mov cx,150		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,95		; BX to set length of row size
		call wallIt
		call drawWalls
		
		;Top Middle	
		mov cx,132		; CX to set start col
		mov dx,0		; DX to set start row
		mov ax,56		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
	RET
	
	room5:
	;Drey 2
		push AX
		push BX
		push CX
		push DX
		push si
		push di
	;Top room lava
		MOV CX,124	; Starting Column
		MOV DX,15	; Starting Row
		MOV BX,24	;Number of (6x6) textures drawn to the right of start position
		MOV BP,5	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,20	; Starting Column
		MOV DX,15	; Starting Row
		MOV BX,3	;Number of (6x6) textures drawn to the right of start position
		MOV BP,11	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,39	; Starting Column
		MOV DX,15	; Starting Row
		MOV BX,14	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,250	; Starting Column
		MOV DX,62	; Starting Row
		MOV BX,3	;Number of (6x6) textures drawn to the right of start position
		MOV BP,14	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,281	; Starting Column
		MOV DX,59	; Starting Row
		MOV BX,3	;Number of (6x6) textures drawn to the right of start position
		MOV BP,21	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,190	; Starting Column
		MOV DX,173	; Starting Row
		MOV BX,15	;Number of (6x6) textures drawn to the right of start position
		MOV BP,2	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,57	; Starting Column
		MOV DX,48	; Starting Row
		MOV BX,2	;Number of (6x6) textures drawn to the right of start position
		MOV BP,23	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,124	; Starting Column
		MOV DX,45	; Starting Row
		MOV BX,13	;Number of (6x6) textures drawn to the right of start position
		MOV BP,6	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,218	; Starting Column
		MOV DX,60	; Starting Row
		MOV BX,3	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,124	; Starting Column
		MOV DX,131	; Starting Row
		MOV BX,18	;Number of (6x6) textures drawn to the right of start position
		MOV BP,7	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		call drawOuterWalls
		; middle long
		mov cx,70		; CX to set start col 70
		mov dx,100		; DX to set start row
		mov ax,180		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		; short left
		mov cx,70		; CX to set start col
		mov dx,60		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,130   	; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
	;Top Middle	
		mov cx,132		; CX to set start col
		mov dx,0		; DX to set start row
		mov ax,56		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Right Middle
		mov cx,300		; CX to set start col
		mov dx,82		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,38		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		;Check if coin needs to be drawn in room
		cmp byte [coin3], 1
		je coin3Obtained
		mov byte [coinColor], 0Ah
		mov word [coinStartX],280
		mov word [coinStartY],25
		call drawCoin
		coin3Obtained:
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET
	
	room6:
		;Start Jack's Smile Room Code		
	;Eyes
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
		MOV CX,80	; Starting Column
		MOV DX,15	; Starting Row
		MOV BX,10	;Number of (6x6) textures drawn to the right of start position
		MOV BP,24	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,14	; Starting Column
		MOV DX,82	; Starting Row
		MOV BX,12	;Number of (6x6) textures drawn to the right of start position
		MOV BP,11	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,122	; Starting Column
		MOV DX,105	; Starting Row
		MOV BX,8	;Number of (6x6) textures drawn to the right of start position
		MOV BP,5	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,160	; Starting Column
		MOV DX,15	; Starting Row
		MOV BX,5	;Number of (6x6) textures drawn to the right of start position
		MOV BP,13	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,184	; Starting Column
		MOV DX,76	; Starting Row
		MOV BX,11	;Number of (6x6) textures drawn to the right of start position
		MOV BP,5	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit						 
		call drawOuterWalls
	
		mov cx,50		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,40		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,40		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Mouth
		mov cx,50		; CX to set start col
		mov dx,145		; DX to set start row
		mov ax,220		; AX to set length of column size
		mov bx,17		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,50		; CX to set start col
		mov dx,130		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,30		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,130		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,30		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Left Middle
		mov cx,0		; CX to set start col
		mov dx,82		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,38		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls 
		
		cmp byte [redDoor],1
		je redDoorUnlocked
		MOV byte [doorColor], 0Ch
		MOV CX,132
		MOV DX,5
		MOV SI,0
		MOV DI,0
		call drawTopOrBottomDoor
		redDoorUnlocked:
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET
	;End Jack's Smile Room Code.
	room7:
		;Drey 2	
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
		call drawOuterWalls
		; middle long
		mov cx,70		; CX to set start col 70
		mov dx,100		; DX to set start row
		mov ax,180		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		; short left
		mov cx,70		; CX to set start col
		mov dx,60		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,130   	; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		; short right
		mov cx,230		; CX to set start col
		mov dx,10		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,100		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET
	
	room8:
		;Pillar Room
		push AX
		push BX
		push CX
		push DX
		push si
		push di
	
		MOV CX,173	; Starting Column
		MOV DX,10	; Starting Row
		MOV BX,21	;Number of (6x6) textures drawn to the right of start position
		MOV BP,15	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		call drawOuterWalls
	;Start Jack's Pillar Room Code		
	; First Pillar Row (From the top).
		mov cx,50		; CX to set start col
		mov dx,45		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,80		; CX to set start col
		mov dx,45		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	
		
		mov cx,150		; CX to set start col
		mov dx,45		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
			
		mov cx,220		; CX to set start col
		mov dx,45		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,45		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	
	;Second Pillar Row
		mov cx,50		; CX to set start col
		mov dx,75		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,100		; CX to set start col
		mov dx,75		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,200		; CX to set start col
		mov dx,75		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,75		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	
	;Third Pillar Row
		mov cx,50		; CX to set start col
		mov dx,105		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,100		; CX to set start col
		mov dx,105		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,200		; CX to set start col
		mov dx,105		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,105		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	
	;Fourth Pillar Row	
				
		mov cx,50		; CX to set start col
		mov dx,135		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,80		; CX to set start col
		mov dx,135		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,150		; CX to set start col
		mov dx,135		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
			
		mov cx,220		; CX to set start col
		mov dx,135		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,135		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	ret

	room9:
		;Straight Corridor
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		
		MOV CX,259	; Starting Column
		MOV DX,70	; Starting Row
		MOV BX,7	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,259	; Starting Column
		MOV DX,114	; Starting Row
		MOV BX,7	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,18	; Starting Column
		MOV DX,114	; Starting Row
		MOV BX,7	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,18	; Starting Column
		MOV DX,70	; Starting Row
		MOV BX,7	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		call drawOuterWalls
		;Top Middle Walls
		mov cx,60		; CX to set start col
		mov dx,70		; DX to set start row
		mov ax,200		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,60		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,240		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,20		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,40		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,260		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,40		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		;Bottom Middle Wall
		mov cx,60		; CX to set start col
		mov dx,117		; DX to set start row
		mov ax,200		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,20		; CX to set start col
		mov dx,132		; DX to set start row
		mov ax,40		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,240		; CX to set start col
		mov dx,132		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,60		; CX to set start col
		mov dx,132		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,260		; CX to set start col
		mov dx,132		; DX to set start row
		mov ax,40		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
	RET
	
	room10:
		;Drey 1
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
		call drawOuterWalls
		; middle long
		mov cx,70		; CX to set start col
		mov dx,60		; DX to set start row
		mov ax,180		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		; short left
		mov cx,70		; CX to set start col
		mov dx,60		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,75		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		; short right
		mov cx,230		; CX to set start col
		mov dx,60		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,75		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Bottom Middle Walls
		; bottom left
		mov cx,10		; CX to set start col
		mov dx,120		; DX to set start row
		mov ax,80		; AX to set length of column size
		mov bx,17		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		; bottom right
		mov cx,230		; CX to set start col
		mov dx,120		; DX to set start row
		mov ax,80		; AX to set length of column size
		mov bx,17		; BX to set length of row size
		call wallIt
		call drawWalls
	;Right Middle
		mov cx,300		; CX to set start col
		mov dx,82		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,38		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		cmp byte [redKey],1
		je redKeyObtained
		mov word [keyStartY],95
		mov word [keyStartX],158
		mov byte [keyColor], 0Ch	; Draw Red key 
		call drawKey
		redKeyObtained:
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET
	
	room11:
		; Corridor Up
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
		call drawOuterWalls
	; top left
		mov cx,100		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,30		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	; top right
		mov cx,185		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,30		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
	;short top left
		mov cx,100		; CX to set start col
		mov dx,15		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,25		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;short top right
		mov cx,200		; CX to set start col
		mov dx,15		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,25		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	; long left
		mov cx,115		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,120		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	; long right
		mov cx,185		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,120		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Bottom Middle Walls
		; bottom left
		mov cx,100		; CX to set start col
		mov dx,150		; DX to set start row
		mov ax,30		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	; bottom right
		mov cx,185		; CX to set start col
		mov dx,150		; DX to set start row
		mov ax,30		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
	;short bottom left
		mov cx,100		; CX to set start col
		mov dx,150		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,40		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;short bottom right
		mov cx,200		; CX to set start col
		mov dx,150		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,40		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Left Middle
		mov cx,0		; CX to set start col
		mov dx,82		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,38		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls 
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET
	
	room12:
	;Tilted Pillars
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
  MOV CX,35	; Starting Column
		MOV DX,90	; Starting Row
		MOV BX,10	;Number of (6x6) textures drawn to the right of start position
		MOV BP,5	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,47	; Starting Column
		MOV DX,102	; Starting Row
		MOV BX,10	;Number of (6x6) textures drawn to the right of start position
		MOV BP,5	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,59	; Starting Column
		MOV DX,114	; Starting Row
		MOV BX,12	;Number of (6x6) textures drawn to the right of start position
		MOV BP,5	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,71	; Starting Column
		MOV DX,126	; Starting Row
		MOV BX,14	;Number of (6x6) textures drawn to the right of start position
		MOV BP,5	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,83	; Starting Column
		MOV DX,138	; Starting Row
		MOV BX,16	;Number of (6x6) textures drawn to the right of start position
		MOV BP,5	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		call drawOuterWalls
		;Start Jack's  Tilted Pillar Room Code		
	;Top left pillars.
		mov cx,30		; CX to set start col
		mov dx,62		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls

		mov cx,60		; CX to set start col
		mov dx,67		; DX to set start row
		mov ax,18		; AX to set length of column size
		mov bx,18		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,90		; CX to set start col
		mov dx,72		; DX to set start row
		mov ax,16		; AX to set length of column size
		mov bx,16		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,100		; CX to set start col
		mov dx,47		; DX to set start row
		mov ax,17		; AX to set length of column size
		mov bx,17		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,105		; CX to set start col
		mov dx,20		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Bottom right pillars
		mov cx,270		; CX to set start col
		mov dx,118		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls

		mov cx,240		; CX to set start col
		mov dx,113		; DX to set start row
		mov ax,18		; AX to set length of column size
		mov bx,18		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,210		; CX to set start col
		mov dx,108		; DX to set start row
		mov ax,16		; AX to set length of column size
		mov bx,16		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,200		; CX to set start col
		mov dx,133		; DX to set start row
		mov ax,18		; AX to set length of column size
		mov bx,18		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,195		; CX to set start col
		mov dx,160		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls		
;middle pillars
		mov cx,115		; CX to set start col
		mov dx,82		; DX to set start row   
		mov ax,15		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls

		mov cx,140		; CX to set start col
		mov dx,92		; DX to set start row 
		mov ax,13		; AX to set length of column size
		mov bx,13		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,162		; CX to set start col ;Changed to 162 from 160 to fix gap
		mov dx,97		; DX to set start row
		mov ax,13		; AX to set length of column size
		mov bx,13		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,185		; CX to set start col
		mov dx,102		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET

	room13:
		;Template
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
		call drawOuterWalls
    ;Top Middle Walls
		MOV CX,50		; CX to set start col
		MOV DX,40		; DX to set start row
		MOV AX,220		; AX to set length of column size
		MOV BX,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		MOV CX,50		; CX to set start col
		MOV DX,40		; DX to set start row
		MOV AX,20		; AX to set length of column size
		MOV BX,30		; BX to set length of row size
		call wallIt
		call drawWalls
		
		MOV CX,250		; CX to set start col
		MOV DX,40		; DX to set start row
		MOV AX,20		; AX to set length of column size
		MOV BX,30		; BX to set length of row size
		call wallIt
		call drawWalls
;Bottom Middle Walls
		MOV CX,50		; CX to set start col
		MOV DX,145		; DX to set start row
		MOV AX,220		; AX to set length of column size
		MOV BX,17		; BX to set length of row size
		call wallIt
		call drawWalls
		
		MOV CX,50		; CX to set start col
		MOV DX,130		; DX to set start row
		MOV AX,20		; AX to set length of column size
		MOV BX,30		; BX to set length of row size
		call wallIt
		call drawWalls
		
		MOV CX,250		; CX to set start col
		MOV DX,130		; DX to set start row
		MOV AX,20		; AX to set length of column size
		MOV BX,30		; BX to set length of row size
		call wallIt
		call drawWalls
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET
	
	room14:
		;Pillar Room
		push AX
		push BX
		push CX
		push DX
		push si
		push di
	
		MOV CX,150	; Starting Column
		MOV DX,45	; Starting Row
		MOV BX,20	;Number of (6x6) textures drawn to the right of start position
		MOV BP,19	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit		  
		call drawOuterWalls
	;Start Jack's Pillar Room Code		
	; First Pillar Row (From the top).
		mov cx,50		; CX to set start col
		mov dx,45		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,80		; CX to set start col
		mov dx,45		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,150		; CX to set start col
		mov dx,45		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls

		mov cx,220		; CX to set start col
		mov dx,45		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,45		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Second Pillar Row
		mov cx,50		; CX to set start col
		mov dx,75		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,100		; CX to set start col
		mov dx,75		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,200		; CX to set start col
		mov dx,75		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,75		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Third Pillar Row
		mov cx,50		; CX to set start col
		mov dx,105		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,100		; CX to set start col
		mov dx,105		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,200		; CX to set start col
		mov dx,105		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,105		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Fourth Pillar Row		
		mov cx,50		; CX to set start col
		mov dx,135		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,80		; CX to set start col
		mov dx,135		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,150		; CX to set start col
		mov dx,135		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,220		; CX to set start col
		mov dx,135		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,135		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	ret
	
	room15:
		;Start Jack's Smile Room Code		
	;Eyes
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
		MOV CX,70	; Starting Column
		MOV DX,7	; Starting Row
		MOV BX,6	;Number of (6x6) textures drawn to the right of start position
		MOV BP,23	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,106	; Starting Column
		MOV DX,115	; Starting Row
		MOV BX,15	;Number of (6x6) textures drawn to the right of start position
		MOV BP,5	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,130	; Starting Column
		MOV DX,85	; Starting Row
		MOV BX,11	;Number of (6x6) textures drawn to the right of start position
		MOV BP,8	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,130	; Starting Column
		MOV DX,85	; Starting Row
		MOV BX,20	;Number of (6x6) textures drawn to the right of start position
		MOV BP,4	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit 
		call drawOuterWalls
		
		mov cx,50		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,40		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,40		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Mouth
		mov cx,50		; CX to set start col
		mov dx,145		; DX to set start row
		mov ax,220		; AX to set length of column size
		mov bx,17		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,50		; CX to set start col
		mov dx,130		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,30		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,130		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,30		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Right Middle
		mov cx,300		; CX to set start col
		mov dx,82		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,38		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		cmp byte [cyanDoor],1
		je cyanDoorUnlocked
		MOV byte [doorColor], 0Bh	; 0Bh for cyan door
		MOV CX,132
		MOV DX,05
		MOV SI,0
		MOV DI,0
		call drawTopOrBottomDoor
		cyanDoorUnlocked:
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET
	
	room16:
		;Walter 1
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
		call drawOuterWalls
		;Top Middle Walls
		mov cx,50		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,220		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,50		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,30		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,188		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,160		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Bottom Middle Walls
		mov cx,50		; CX to set start col
		mov dx,145		; DX to set start row
		mov ax,82		; AX to set length of column size
		mov bx,17		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,50		; CX to set start col
		mov dx,130		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,30		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,100		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,50		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Left Middle
		mov cx,0		; CX to set start col
		mov dx,82		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,38		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
	RET
	
	room17:
		;Drey 1
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
		call drawOuterWalls
	; middle long
		mov cx,70		; CX to set start col
		mov dx,60		; DX to set start row
		mov ax,180		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	; short left
		mov cx,70		; CX to set start col
		mov dx,60		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,75		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	; short right
		mov cx,230		; CX to set start col
		mov dx,60		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,75		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Bottom Middle Walls
		; bottom left
		mov cx,10		; CX to set start col
		mov dx,120		; DX to set start row
		mov ax,80		; AX to set length of column size
		mov bx,17		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	; bottom right
		mov cx,230		; CX to set start col
		mov dx,120		; DX to set start row
		mov ax,80		; AX to set length of column size
		mov bx,17		; BX to set length of row size
		call wallIt
		call drawWalls
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET
	
	room18:
	;Tilted Pillars
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
		MOV CX,20	; Starting Column
		MOV DX,120	; Starting Row
		MOV BX,18	;Number of (6x6) textures drawn to the right of start position
		MOV BP,11	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,191	; Starting Column
		MOV DX,15	; Starting Row
		MOV BX,18	;Number of (6x6) textures drawn to the right of start position
		MOV BP,11	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		
		call drawOuterWalls
		;Start Jack's  Tilted Pillar Room Code		
	;Top left pillars.
		mov cx,30		; CX to set start col
		mov dx,62		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls

		mov cx,60		; CX to set start col
		mov dx,67		; DX to set start row
		mov ax,18		; AX to set length of column size
		mov bx,18		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,90		; CX to set start col
		mov dx,72		; DX to set start row
		mov ax,16		; AX to set length of column size
		mov bx,16		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,100		; CX to set start col
		mov dx,47		; DX to set start row
		mov ax,17		; AX to set length of column size
		mov bx,17		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,105		; CX to set start col
		mov dx,20		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Bottom right pillars
		mov cx,270		; CX to set start col
		mov dx,118		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls

		mov cx,240		; CX to set start col
		mov dx,113		; DX to set start row
		mov ax,18		; AX to set length of column size
		mov bx,18		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,210		; CX to set start col
		mov dx,108		; DX to set start row
		mov ax,16		; AX to set length of column size
		mov bx,16		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,200		; CX to set start col
		mov dx,133		; DX to set start row
		mov ax,18		; AX to set length of column size
		mov bx,18		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,195		; CX to set start col
		mov dx,160		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,20		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;middle pillars
		mov cx,115		; CX to set start col
		mov dx,82		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls

		mov cx,140		; CX to set start col
		mov dx,92		; DX to set start row
		mov ax,13		; AX to set length of column size
		mov bx,13		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,162		; CX to set start col  ;Changed to 162 from 160 to fix gap
		mov dx,97		; DX to set start row
		mov ax,13		; AX to set length of column size
		mov bx,13		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,185		; CX to set start col
		mov dx,102		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET

	room19:
	;Walter 3
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		
		call drawOuterWalls
	;Top Middle Walls
		mov cx,50		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,160		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,50		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,30		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,188		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,120		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,112		; CX to set start col
		mov dx,15		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,75		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Bottom Middle Walls
		mov cx,50		; CX to set start col
		mov dx,145		; DX to set start row
		mov ax,82		; AX to set length of column size
		mov bx,17		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,50		; CX to set start col
		mov dx,130		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,30		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,100		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,50		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
	RET	
	
	room20:
	;Walter2
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		call drawOuterWalls
	;Top Middle Walls
		mov cx,132		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,56		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,112		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,160		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,188		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,160		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Right Middle
		mov cx,300		; CX to set start col
		mov dx,82		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,38		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		cmp byte [cyanKey],1
		je cyanKeyObtained
		mov word [keyStartY],95
		mov word [keyStartX],158
		mov byte [keyColor], 0Bh	; Draw Cyan key 
		call drawKey
		cyanKeyObtained:
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET
	
	room21:
	;Exit Room 
	; Corridor Up
		push AX
		push BX
		push CX
		push DX
		push si
		push di
	
;Top Right corner Lava
		MOV CX,168	; Starting Column
		MOV DX,15	; Starting Row
		MOV BX,22	;Number of (6x6) textures drawn to the right of start position
		MOV BP,10	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
;Left Side Lava 
	;Larger Left Corner
		MOV CX,19	; Starting Column
		MOV DX,15	; Starting Row
		MOV BX,22	;Number of (6x6) textures drawn to the right of start position
		MOV BP,5	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
	;Left Side Addition
		MOV CX,19	; Starting Column
		MOV DX,45	; Starting Row
		MOV BX,6	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
;First Middle Turns
		MOV CX,138	; Starting Column
		MOV DX,57	; Starting Row
		MOV BX,6	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,108	; Starting Column
		MOV DX,45	; Starting Row
		MOV BX,3	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,72	; Starting Column
		MOV DX,57	; Starting Row
		MOV BX,4	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
	;Left Near Wall
		MOV CX,42	; Starting Column
		MOV DX,75	; Starting Row
		MOV BX,23	;Number of (6x6) textures drawn to the right of start position
		MOV BP,4	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,42	; Starting Column
		MOV DX,99	; Starting Row
		MOV BX,8	;Number of (6x6) textures drawn to the right of start position
		MOV BP,8	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
	;Second Middle Turns
		MOV CX,103	; Starting Column
		MOV DX,113	; Starting Row
		MOV BX,22	;Number of (6x6) textures drawn to the right of start position
		MOV BP,8	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,193	; Starting Column
		MOV DX,89	; Starting Row
		MOV BX,7	;Number of (6x6) textures drawn to the right of start position
		MOV BP,4	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit

	;Bottom Left Corner
		MOV CX,19	; Starting Column
		MOV DX,161	; Starting Row
		MOV BX,21	;Number of (6x6) textures drawn to the right of start position
		MOV BP,4	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
;Right Door Lava
		MOV CX,248	; Starting Column
		MOV DX,75	; Starting Row
		MOV BX,2	;Number of (6x6) textures drawn to the right of start position
		MOV BP,9	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
;Bottom Right Corner & Right side of Bottom Door
		MOV CX,248	; Starting Column
		MOV DX,129	; Starting Row
		MOV BX,9	;Number of (6x6) textures drawn to the right of start position
		MOV BP,11	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,176	; Starting Column
		MOV DX,173	; Starting Row
		MOV BX,12	;Number of (6x6) textures drawn to the right of start position
		MOV BP,2	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		call drawOuterWalls
		
	;Left Middle
		mov cx,0		; CX to set start col
		mov dx,82		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,38		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls 
		
		cmp byte [magentaDoor],1
		je magentaDoorUnlocked
		MOV byte [doorColor], 05h	;Light Magenta
		MOV CX,132
		MOV DX,185
		MOV SI,0
		MOV DI,0
		call drawTopOrBottomDoor
		magentaDoorUnlocked:
		
		;Check if coin needs to be drawn in room
		cmp byte [coin2], 1
		je coin2Obtained
		mov byte [coinColor], 0Ah
		mov word [coinStartX],275
		mov word [coinStartY],95
		call drawCoin
		coin2Obtained:
		pop di
		pop si
		pop DX
		pop CX
		pop BX
		pop AX
	RET

	room22:
		;Jimmy 3
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		
		MOV CX,84	; Starting Column
		MOV DX,74	; Starting Row
		MOV BX,7	;Number of (6x6) textures drawn to the right of start position
		MOV BP,9	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,194	; Starting Column
		MOV DX,74	; Starting Row
		MOV BX,7	;Number of (6x6) textures drawn to the right of start position
		MOV BP,9	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,18	; Starting Column
		MOV DX,176	; Starting Row
		MOV BX,47	;Number of (6x6) textures drawn to the right of start position
		MOV BP,2	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,16	; Starting Column
		MOV DX,40	; Starting Row
		MOV BX,8	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,255	; Starting Column
		MOV DX,40	; Starting Row
		MOV BX,8	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		call drawOuterWalls
	;Top Middle
		mov cx,86		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,148		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
	;Bottom Middle	
		mov cx,86		; CX to set start col
		mov dx,150		; DX to set start row
		mov ax,148		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
	;Left Middle
		mov cx,50		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,95		; BX to set length of row size
		call wallIt
		call drawWalls
	;Right Middle
		mov cx,255		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,95		; BX to set length of row size
		call wallIt
		call drawWalls
	;Middle
		mov cx,150		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,95		; BX to set length of row size
		call wallIt
		call drawWalls
	;Bottom Middle	
		mov cx,132		; CX to set start col
		mov dx,185		; DX to set start row
		mov ax,56		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
	RET
	
	room23:
		;Empty room
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
		MOV CX,17	; Starting Column
		MOV DX,15	; Starting Row
		MOV BX,19	;Number of (6x6) textures drawn to the right of start position
		MOV BP,2	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,113	; Starting Column
		MOV DX,26	; Starting Row
		MOV BX,6	;Number of (6x6) textures drawn to the right of start position
		MOV BP,6	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,113	; Starting Column
		MOV DX,62	; Starting Row
		MOV BX,14	;Number of (6x6) textures drawn to the right of start position
		MOV BP,2	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,173	; Starting Column
		MOV DX,74	; Starting Row
		MOV BX,4	;Number of (6x6) textures drawn to the right of start position
		MOV BP,2	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,173	; Starting Column
		MOV DX,85	; Starting Row
		MOV BX,12	;Number of (6x6) textures drawn to the right of start position
		MOV BP,4	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,221	; Starting Column
		MOV DX,109	; Starting Row
		MOV BX,4	;Number of (6x6) textures drawn to the right of start position
		MOV BP,8	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		MOV CX,173	; Starting Column
		MOV DX,26	; Starting Row
		MOV BX,17	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,263	; Starting Column
		MOV DX,43	; Starting Row
		MOV BX,2	;Number of (6x6) textures drawn to the right of start position
		MOV BP,13	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,263	; Starting Column
		MOV DX,120	; Starting Row
		MOV BX,6	;Number of (6x6) textures drawn to the right of start position
		MOV BP,12	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,215	; Starting Column
		MOV DX,44	; Starting Row
		MOV BX,5	;Number of (6x6) textures drawn to the right of start position
		MOV BP,4	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,17	; Starting Column
		MOV DX,173	; Starting Row
		MOV BX,41	;Number of (6x6) textures drawn to the right of start position
		MOV BP,2	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,17	; Starting Column
		MOV DX,15	; Starting Row
		MOV BX,2	;Number of (6x6) textures drawn to the right of start position
		MOV BP,11	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,42	; Starting Column
		MOV DX,41	; Starting Row
		MOV BX,6	;Number of (6x6) textures drawn to the right of start position
		MOV BP,9	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,78	; Starting Column
		MOV DX,53	; Starting Row
		MOV BX,3	;Number of (6x6) textures drawn to the right of start position
		MOV BP,7	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,95	; Starting Column
		MOV DX,26	; Starting Row
		MOV BX,3	;Number of (6x6) textures drawn to the right of start position
		MOV BP,2	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,42	; Starting Column
		MOV DX,95	; Starting Row
		MOV BX,19	;Number of (6x6) textures drawn to the right of start position
		MOV BP,7	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,17	; Starting Column
		MOV DX,137	; Starting Row
		MOV BX,26	;Number of (6x6) textures drawn to the right of start position
		MOV BP,6	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		call drawOuterWalls
		;Bottom Middle	
		mov cx,86		; CX to set start col
		mov dx,185		; DX to set start row
		mov ax,148		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
	RET
	
	room24:
		;Horizontal Corridor
		push ax
		push bx
		push cx
		push dx
		push si
		push di
	;Lava Pits
		MOV CX,259	; Starting Column
		MOV DX,70	; Starting Row
		MOV BX,7	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,259	; Starting Column
		MOV DX,114	; Starting Row
		MOV BX,7	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,18	; Starting Column
		MOV DX,114	; Starting Row
		MOV BX,7	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		MOV CX,18	; Starting Column
		MOV DX,70	; Starting Row
		MOV BX,7	;Number of (6x6) textures drawn to the right of start position
		MOV BP,3	;Number of (6x6) textures drawn down from start position	
		call drawLavaPit
		
		call drawOuterWalls
	;Top Middle Walls
		mov cx,60		; CX to set start col
		mov dx,70		; DX to set start row
		mov ax,200		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,60		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,240		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,20		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,40		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,260		; CX to set start col
		mov dx,55		; DX to set start row
		mov ax,40		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
	;Bottom Middle Wall
		mov cx,60		; CX to set start col
		mov dx,117		; DX to set start row
		mov ax,200		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,20		; CX to set start col
		mov dx,132		; DX to set start row
		mov ax,40		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,240		; CX to set start col
		mov dx,132		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,60		; CX to set start col
		mov dx,132		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,260		; CX to set start col
		mov dx,132		; DX to set start row
		mov ax,40		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
	;Bottom Middle	
		mov cx,132		; CX to set start col
		mov dx,185		; DX to set start row
		mov ax,56		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
	;Check if coin needs to be drawn in room
		cmp byte [coin1], 1
		je coin1Obtained
		mov byte [coinColor], 0Ah
		mov word [coinStartX], 158
		mov word [coinStartY], 40
		call drawCoin
		coin1Obtained:
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
	RET
	
	room25:
		;Walter 3
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		call drawOuterWalls
	;Top Middle Walls
		mov cx,50		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,160		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,50		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,30		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,188		; CX to set start col
		mov dx,40		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,120		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,112		; CX to set start col
		mov dx,15		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,75		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Bottom Middle Walls
		mov cx,50		; CX to set start col
		mov dx,145		; DX to set start row
		mov ax,82		; AX to set length of column size
		mov bx,17		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,50		; CX to set start col
		mov dx,130		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,30		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,250		; CX to set start col
		mov dx,100		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,50		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
	;Bottom Middle	
		mov cx,132		; CX to set start col
		mov dx,185		; DX to set start row
		mov ax,56		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
	;Right Middle
		mov cx,300		; CX to set start col
		mov dx,82		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,38		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
	RET
;end of rooms

;start of char
    draw_character:
        CMP word [square_movement], 0
        je no_movement 	; 0 indicates no movement so skip drawing the character
		
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		call checkNewSquare
		MOV CX, word [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, word [square_start_y]	; Need this for any movement to start drawing square at start_y
		call drawNewSquare
		
		CMP word [square_movement],1
		je movingUp				;If character needs to be drawn moving upwards
		CMP word [square_movement],2
		je movingDown			; If character needs to be drawn moving downwards
		CMP word [square_movement], 3
		je movingRight			; If character needs to be drawn moving right
		CMP word [square_movement], 4
		je movingLeft
		jmp drawFin				; Jumps to finish on initial launch as square_movement is 5
		movingLeft:
	;Draw Left
		push si
		mov si,0
		call drawLeft			; If none of the above, character is drawn moving left of current position
		pop si
		jmp drawFin				; After drawing, jmp to set movement to 0		
	;Draw Up
		movingUp:
		push si
		mov si,0
		call drawUp				; Call subroutine to draw character above original position
		pop si
		jmp drawFin
	;Draw Down
		movingDown:
		push si
		mov si,0
		call drawDown			; Call subroutine to draw character below original position
		pop si
		jmp drawFin
	;Draw Right		
		movingRight:
		push si
		mov si,0
		call drawRight			; Call subroutine to draw character to the right of original position
		pop si
		jmp drawFin
	; Finish Drawing
		drawFin:
        MOV word [square_movement],0 ; set back to zero because character has now moved
		call checkRoomChange
        no_movement:
    ret
	; Checks the pixels underneath the players new position
	checkNewSquare:
		MOV AH,0Dh			; Read graphics Pixel 
		MOV BH,00h			; Read from page 0 as this has keys
		INT 10h
		call checkLavaOrCoin	; Check if color returned to AL is lava or a coin 
		cmp AL,0Dh				; If subroutine returns 0Dh, the player has entered lava
		je checkSquareFin		; No need to continue checking pixels if character has died
		call checkKeyColors		; Check if the color returned is a key  			
		cmp AL,07h				; If subroutine returns 07h, the player has picked up a key
		je checkSquareFin		; No need checking pixels if key obtained 
		INC CX					; Increase to the next pixel in the column to be read 
        CMP CX, [square_final_x]	
        JNA checkNewSquare		; Check all pixels in a column before incrementing row 
        MOV CX, [square_start_x]
        INC DX					; Increase to the next row to be read
        CMP DX, [square_final_y]
        JNA checkNewSquare		; Check all rows of the new position
		checkSquareFin:
	RET
	
	drawNewSquare:				; Draws the character in its new position no matter the direction moved
        MOV AH, 0Ch				; Draw character pixel
        MOV AL, 09h				; Color to Blue
        MOV BH, 00h				; Draw on page 0
        INT 10h
		INC CX
        CMP CX, word [square_final_x]
        JNA drawNewSquare
        MOV CX, word [square_start_x]
        INC DX
        CMP DX, word [square_final_y]
        JNA drawNewSquare
	RET
	
	drawUp:		
		; Removes trail when drawing up
		MOV AH,0Dh		; Read graphics Pixel 
		MOV BH,01h		; Read from page 1 since this only has tiles
		INT 10h
        MOV AH, 0Ch		; Draw pixel read from page 1
        MOV BH, 00h		; Draw pixels on page 0 (has character)
        INT 10h
        INC CX			; Inc the column (x value) of the square
        CMP CX, [square_final_x]	; Compare if column is at final x 
        JNG drawUp					; Continue looping until it is greater than
        MOV CX, [square_start_x]	; Set start x back to CX
		INC DX			; Inc row being drawn
        INC SI			; Inc row counter
        CMP SI,[actualMoveDistance]	; Compare the amount of rows removed by the move distance
        JNE drawUp					; Continue removing rows until move distance reached
	ret
	
	drawDown:	
		; Removes trail when drawing down
		mov cx, word [square_start_x]
		mov dx, word [square_start_y]
		SUB dX,word [actualMoveDistance]
		loopDown:
		MOV AH,0Dh		; Read graphics Pixel 
		MOV BH,01h		; Read from page 1 since this only has tiles
		INT 10h
        MOV AH, 0Ch		;  pixel read from page 1 (tiles)
        MOV BH, 00h		; Draw pixels on page 0 (has character)
        INT 10h
        INC CX			; Inc the column (x value) of square
        CMP CX, word [square_final_x]	; Compare column to the end of the square
        JNG loopDown				; Jump if not greater to continue looping
        MOV CX, word [square_start_x]	; Move CX with start x position
        INC DX						; Inc the row (y value) of the square
		INC SI						; Inc row counter (SI) 
        CMP SI,word [actualMoveDistance]	;Compare amount of rows draw to the actual move distance
        JNE loopDown				;If number of rows drawn is not equal to move distance, keep looping
	ret
	
	drawRight:	
		; Removes trail when drawing right
		MOV DX, [square_start_y]		; Move start y to DX
		SUB CX, [actualMoveDistance]	; Subtract start x with move distance
		loopRight:
		MOV AH,0Dh		; Read graphics Pixel 
		MOV BH,01h		; Read from page 1 since this only has tiles
		INT 10h
        MOV AH, 0Ch		; Draw pixel read from page 1
        MOV BH, 00h		; Draw pixels on page 0 (has character)
        INT 10h
        INC CX			; Inc the col (x value) of square
		INC SI			; Increment column counter
        CMP SI, [actualMoveDistance]	; Compare  counter with the move distance
        JNE loopRight				; Continue looping until all columns have been drawn 
		MOV si,0		; Reset column counterColumn
        MOV CX, [square_start_x]	; Move start x back to CX
		SUB CX, [actualMoveDistance]		; Subtract CX by move distance
        INC DX							; Inc row (y value) of square
        CMP DX, [square_final_y]		; Compare row with the final y
        JNG loopRight
	ret
	
	drawLeft:	
		; Removes trail when drawing left
		mov cx, word [square_final_x]
		MOV DX, word [square_start_y]
		add cx,word [actualMoveDistance]
		loopLeft:
		MOV AH,0Dh					; Read graphics Pixel 
		MOV BH,01h					; Read from page 1 since this only has tiles
		INT 10h
        MOV AH, 0Ch					; Draw pixel
        MOV BH, 00h					; Draw pixels on page 0 (has character)
        INT 10h
        dec CX
		INC SI
        CMP cx, word [square_final_x]
        JNE loopLeft				
		MOV si,0
        MOV CX, word [square_final_x]
		add CX, word [actualMoveDistance]
		;ADD CX, 1
        INC DX
        CMP DX, word [square_final_y]
        JNG loopLeft
	ret
	
	checkRight:
		MOV CX,0000h
		ADD CX,word [moveDistance]
		MOV word [actualMoveDistance],CX
		minimizeRight:
        MOV CX,word [square_final_x]		; Moving right so final or rightmost column
        ADD CX,word [actualMoveDistance]	; Add movement to get new position of rightmost column
		CMP CX,319					; Compare the final_x + move distance to CX
		JG noRightCollision			; If its greater, assume there is no collision as it must be at the right door
		MOV DX,word [square_start_y]		; Sets y to the top row
		MOV AH,0Dh					; Read Pixel at this position
		MOV BH,00h					; Read from page 0
		INT 10h						; INT 10h, AH = 0Dh, BH = Page Number, CX = x, DX = y | Read Graphics Pixel
		CMP AL,02h					; Checks to see if the pixel was green
		je rightCollision			; If Green, then collision 			
		call checkKeyColors		; Compare the color read to keys
		call minimizeLava
		cmp AL, 02h
		je rightCollision
		
		MOV DX,word [square_final_y]		; Sets y to the bottom row
		MOV AH,0Dh					
		MOV BH,00h					
		INT 10h						; INT 10h, AH = 0Dh, BH = Page Number, CX = x, DX = y | Read Graphics Pixel
		CMP AL,02h					; Checks to see if the pixel was green
		je rightCollision
		call checkKeyColors		; Compare the color read to keys
		call minimizeLava
		cmp AL, 02h
		je rightCollision
		jmp noRightCollision
		rightCollision:
			dec word [actualMoveDistance]
			CMP word [actualMoveDistance],0
			je collisionRight
			jmp minimizeRight
		noRightCollision:
			MOV BX,word [actualMoveDistance]
			collisionRight:
	ret
	
	checkLeft:
		MOV CX,0
		ADD CX,word [moveDistance]
		MOV [actualMoveDistance],CX
		minimizeLeft:
        MOV CX,word [square_start_x]		; Moving left so start or leftmost column
        SUB CX,word [actualMoveDistance]	; Add movement to get new position of leftmost column
		MOV DX,word [square_start_y]		; Sets y to the top row
		MOV AH,0Dh					
		MOV BH,00h					
		INT 10h						; INT 10h, AH = 0Dh, BH = Page Number, CX = x, DX = y | Read Graphics Pixel
		CMP AL,02h					; Checks to see if the pixel was green
		je leftCollision
		call checkKeyColors
		call minimizeLava
		cmp AL, 02h
		je leftCollision
		
		MOV DX,word [square_final_y]		; Sets y to the bottom row
		MOV AH,0Dh
		MOV BH,00h
		INT 10h						; INT 10h, AH = 0Dh, BH = Page Number, CX = x, DX = y | Read Graphics Pixel
		CMP AL,02h					; Checks to see if the pixel was green
		je leftCollision
		call checkKeyColors
		call minimizeLava
		cmp AL, 02h
		je leftCollision
		jmp noLeftCollision
		leftCollision:
			dec word [actualMoveDistance]
			CMP word [actualMoveDistance], 0
			je collisionLeft
			jmp minimizeLeft
		noLeftCollision:
			MOV BX,word [actualMoveDistance]
			NEG BX
		collisionLeft:
	ret
	
	checkUp:
		MOV DX, 0
		ADD DX,word [moveDistance]
		MOV [actualMoveDistance],DX
		minimizeUp:
        MOV CX,word [square_start_x]		; Checks pixel to the right of top corner
		MOV DX,word [square_start_y]		; Sets y to the top corner
		SUB DX,word [actualMoveDistance]	; Checks the pixel above to make sure not Green
		MOV AH,0Dh					; Read graphics Pixel 
		MOV BH,00h					; Read from page 0 as this has walls drawn
		INT 10h
		call checkKeyColors
		call minimizeLava
		call checkInventory
		cmp AL, 02h
		je upCollision
		
		MOV CX,word [square_final_x]		; Checks pixel to the right of top corner
		MOV AH,0Dh					; Read graphics Pixel 
		MOV BH,00h					; Read from page 0 as this has walls drawn
		INT 10h
		call checkKeyColors
		call minimizeLava
		call checkInventory
		cmp AL, 02h
		je upCollision
		jmp noUpCollision
		upCollision:
			dec word [actualMoveDistance]
			CMP word [actualMoveDistance], 0
			je collisionUp
			jmp minimizeUp
		noUpCollision:
			MOV BX,word [actualMoveDistance]
			NEG BX
		collisionUp:
	ret
	
	checkDown:
		MOV DX,0
		ADD DX,word [moveDistance]
		MOV word [actualMoveDistance],DX
		minimizeDown:
        MOV CX,word [square_start_x]		; Checks pixel below bottom right
		MOV DX,word [square_final_y]		; Sets y to the right corner
		ADD DX,word [actualMoveDistance]	; Add actualMoveDistance to final y value
		MOV AH,0Dh					; Read graphics Pixel 
		MOV BH,00h					; Read from page 0 as this has walls drawn
		INT 10h
		call checkKeyColors
		call minimizeLava
		cmp AL, 02h
		je downCollision
		
		MOV CX,word [square_final_x]		; Checks pixel to the right of top corner
		MOV AH,0Dh					; Read graphics Pixel 
		MOV BH,00h					; Read from page 0 as this has walls drawn
		INT 10h
		call checkKeyColors
		call minimizeLava
		cmp AL, 02h
		je downCollision
		jmp noDownCollision
		downCollision:
			dec word [actualMoveDistance]
			CMP word [actualMoveDistance], 0000h
			je collisionDown
			jmp minimizeDown
		noDownCollision:
			MOV BX,word [actualMoveDistance]
		collisionDown:
	ret
	
	minimizeLava:
		CMP AL,0Eh
		jne checkRedLava
		CMP byte [closeLava],1
		JE minimizeLavaFin
		cmp word [actualMoveDistance],1
		JNE lavaWall
		INC byte [closeLava]
		jmp moveToLava
	checkRedLava:
		CMP AL,04h
		jne minimizeLavaFin
		CMP byte [closeLava],1
		JE moveToLava
		cmp word [actualMoveDistance],1
		JNE lavaWall
		INC byte [closeLava]
	lavaWall:
		MOV AL,02h
		MOV byte [closeLava],0
		jmp minimizeLavaFin
	moveToLava:
		MOV word [actualMoveDistance],2
	minimizeLavaFin:
	RET
	
	checkKeyColors:
	PUSH SI
	PUSH DI
	MOV SI,0
	MOV DI,0
		checkCyan:
			CMP AL,0Bh				; See if cyan pixel
			jne checkRed			; If key isn't cyan, check if red
			CMP word [square_start_y], 30	; See if the cyan pixel is at the top door
			JG getCyanKey
			CMP word [square_final_x], 190
			JL checkCyanDoor		; If it is at the top, jmp to checkCyanDoor to see if player has the cyan key
			jmp notKey
			getCyanKey:
			INC byte [cyanKey]				; If cyanKey = 1, cyan key obtained
			call eraseKey
			mov byte [keyColor], 0Bh
			mov word [keyStartY],2
			mov word [keyStartX],280
			jmp isKey					
		checkCyanDoor:			; Check if cyan key obtained to remove cyan door
			CMP byte [cyanKey],1			
			je removeCyan		; If cyanKey is 1, jmp to removeCyan to remove the cyan door
			MOV AL,02h			; If cyanKey 0, pretend that door is a wall by moving 02h to AL
			jmp notKey			; Jump to notKey since there are no keys at this point
			removeCyan:
			call removeTopDoor	; The top door can be removed since the correct key was acquired
			inc byte [cyanDoor]
			jmp notKey			; return from keyCheck by jmp to notKey
		checkRed:
			CMP AL,0Ch				; See if red pixel 
			jne checkMagenta
			CMP word [square_start_y], 30
			JL checkRedDoor
			INC byte [redKey] 	; IF redKey = 1, red key obtained
			call eraseKey
			mov byte [keyColor], 0Ch
			mov word [keyStartY],2
			mov word [keyStartX],295
			jmp isKey
		checkRedDoor:			; Check if red key obtained to remove red door
			CMP byte [redKey],1			
			je removeRed			; If redKey is 1, jmp to removeRed to remove the red door
			MOV AL,02h
			jmp notKey
			removeRed:
			call removeTopDoor
			inc byte [redDoor]
			jmp notKey
		checkMagenta:
			CMP AL,05h	;Compare to Light Magenta
			jne notKey
			CMP word [square_final_y], 170
			JG checkMagentaDoor
			INC byte [magentaKey]
			call eraseKey
			mov byte [keyColor], 05h	;0Dh is light Magenta
			mov word [keyStartY],2
			mov word [keyStartX],310
			jmp isKey
		checkMagentaDoor:
			CMP byte [magentaKey],1			
			je removeMagenta			; If yellowKey is 1, jmp to removeYellow to remove the yellow door
			MOV AL,02h
			jmp notKey
			removeMagenta:
			call removeBottomDoor
			inc byte [magentaDoor]
			jmp notKey
		isKey:
			call drawKey
			MOV AL,07h	; This is used by the drawSquare routine to determine if there was a key
		notKey:
		POP DI
		POP SI 
	RET
	
	checkInventory:
		CMP word [square_start_y],15
		JG skipCoinCheck
		CMP word [square_start_x],132
		JGE skipCoinCheck
		MOV AL,02h
	skipCoinCheck:
	RET
	
	checkLavaOrCoin:
		CMP AL,0Ah	; Checks if Coin 
		JNE checkLava
		call eraseCoin
		cmp word [currentRoom],24
		JNE checkCoinRoom21
		MOV byte [coin1],1	;Coin 1 is in room 24
		MOV byte [coinColor],0Ah
		MOV word [coinStartY],2			;Position to draw at first coin slot
		MOV word [coinStartX],75
		call drawCoin
		MOV AL,07h		;07h used to signify routine can skip checking values
		jmp noLava
		
	checkCoinRoom21:
		cmp word [currentRoom],21
		JNE checkCoinRoom5
		MOV byte [coin2],1	;Coin 2 is in room 
		MOV byte [coinColor],0Ah
		MOV word [coinStartY],2			;Position to draw at first coin slot
		MOV word [coinStartX],90
		call drawCoin
		MOV AL,07h		;07h used to signify routine can skip checking values
		jmp noLava
		
	checkCoinRoom5:
		MOV byte [coin3],1
		MOV byte [coinColor],0Ah
		MOV word [coinStartY],2			;Position to draw at first coin slot
		MOV word [coinStartX],105
		call drawCoin
		MOV AL,07h		;07h used to signify routine can skip checking values
		jmp noLava
	checkLava:
		CMP AL,0Eh	; Checks if lava
		je lavaDeath
		CMP AL,04h	; Checks if lava
		je lavaDeath
		jmp noLava
	lavaDeath:
		MOV AL,0Dh	;If light magenta, then call deathscreen
		MOV byte [died],1
	noLava:
	RET
	
	updateHealth: ;draws characters health on the screen
		 push ax
		 push bx
		 push dx
		 push cx
		 
		healthPosition:
		;sets starting cursor position.
		 MOV AH, 02H
		 MOV BH, 0 ; Draws on Game screen page
		 MOV DH, 01h ; row/y (01)
		 MOV DL, 03h ; column/x (01)
		 INT 10H
		
		redHearts:
		;prints a heart on the screen
		 MOV AH, 09H
		 MOV AL, 03H	; ASCII for a heart
		 MOV BH, 00h	 ; Draws on game screen page
		 MOV BL, 0D6h	; 0DEh light red on green  	;0D6h dark red on green
		 MOV CL, byte [health]; Hearts based off of player health.
		 INT 10h
		 
		 cmp byte [health],3
		 JE healthFin
		 cmp byte [health],1
		 JE twoBlackHearts
		 MOV CL, 1
		 MOV AH, 02H
		 MOV BH, 0 ; Draws on Game screen page
		 MOV DH, 01h ; row/y (01)
		 MOV DL, 05h ; column/x (01)
		 INT 10H
		 jmp blackHearts
		 
		 twoBlackHearts:
		 MOV AH, 02H
		 MOV BH, 0 ; Draws on Game screen page
		 MOV DH, 01h ; row/y (01)
		 MOV DL, 04h ; column/x (01)
		 INT 10H
		 MOV CL,2

		 blackHearts:
		 MOV AH, 09H
		 MOV AL, 03H	; ASCII for a heart
		 MOV BH, 00h	 ; Draws on game screen page
		 MOV BL, 0D2h	; 0D2h Black on green
		 INT 10h
		 
		 healthFin:
		 pop cx
		 pop dx
		 pop bx
		 pop ax
	ret
	updateScore:
	  push ax
		 push bx
		 push dx
		 push cx
		
		;sets starting cursor position.
		 MOV AH, 02H
		 MOV BH, 0 ; Draws on Game screen page
		 MOV DH, 00h ; row/y (01)
		 MOV DL, 03h ; column/x (01)
		 INT 10H
		
		;prints a heart on the screen
		 MOV AH, 0EH
		 MOV AL, byte [score]
		 MOV BH, 00h	 ; Draws on game screen page
		 MOV BL, 0D6h	; 0DEh light red on green  	;0D6h dark red on green
		 MOV CL, 1; Hearts based off of player health.
		 INT 10h
		 pop cx
		 pop dx
		 pop bx
		 pop ax
	 Ret											
;end of char

;start of pages
;This File details the functions for setting the page, drawing, and inputs
;Page 0 => Game Screen
;Page 1 => Tiles only
;Page 2 => Death Screen 
;Page 3 => Title Screen
;Page 4 => Win Screen
;Page 5 => Pause screen
;Page 6 => Credit Screen
;Page 7 => Loading Screen
	setDeathScreen:
		push ax
		Mov AH,05h
		MOV AL,02	; Set screen page
		INT 10h
		pop ax
	ret
	deathScreen:
		push bp
        push es
        push AX
        push BX
        push cx
        push DX
		
        mov ax, ds
        mov es, ax
        mov ah, 13h
        mov al, 1   ; update cursor position
        mov bh, 2   ; page #

        ;Setup for ASCII Text 
        mov bl, 3ah ; text attribute (color)
        mov cx, 40  ; string length
        mov dh, 0   ; row 0 - 24    \40x25
        mov dl, 0   ; column 0 - 39 /

        cmp byte [health], 0  ; Determine which version of the screen to draw
        je game_over

        ;Writing You Died
        mov bp, offset you_died_1
        int 10h
        inc dh
        mov bp, offset you_died_2
        int 10h
        inc dh
        mov bp, offset you_died_3
        int 10h
        inc dh
        mov bp, offset you_died_4
        int 10h
        inc dh
        mov bp, offset you_died_5
        int 10h
        inc dh
        mov bp, offset you_died_6
        int 10h

        ;Drawing of heart and minus
        mov bl, 0ch ; Change color to light red
        mov cx, 27  ; Change string lenght
        mov dh, 13  ; Change row
        mov dl, 7   ; Change column
        mov bp, offset heart_1
        int 10h
        inc dh
        mov bp, offset heart_2
        int 10h
        inc dh
        mov bp, offset heart_3
        int 10h
        inc dh
        mov bp, offset heart_4
        int 10h
        inc dh
        mov bp, offset heart_5
        int 10h
        inc dh
        mov bp, offset heart_6
        int 10h
        inc dh
        mov bp, offset heart_7
        int 10h

        ;Drawing of minus symbol with new color
        mov bl, 0fh ; Change color to white
        mov cx, 7   ; Change string length
        mov dh, 15  ; Change row
        mov dl, 7   ; Change column
        mov bp, offset minus_1
        int 10h
        inc dh
        mov bp, offset minus_2
        int 10h
        jmp death_screen_end

        game_over:
        ;Writing of You Lose
        mov bp, offset you_lose_1
        int 10h
        inc dh
        mov bp, offset you_lose_2
        int 10h
        inc dh
        mov bp, offset you_lose_3
        int 10h
        inc dh
        mov bp, offset you_lose_4
        int 10h
        inc dh
        mov bp, offset you_lose_5
        int 10h
        inc dh
        mov bp, offset you_lose_6
        int 10h

        ;Drawing the skull
        mov bl,0fh  ; Change color to white
        mov cx,37   ; Change string length
        mov dh,7    ; Change row
        mov dl,2    ; Change column
        mov bp, offset skull_01
        int 10h
        inc dh
        mov bp, offset skull_02
        int 10h
        inc dh
        mov bp, offset skull_03
        int 10h
        inc dh
        mov bp, offset skull_04
        int 10h
        inc dh
        mov bp, offset skull_05
        int 10h
        inc dh
        mov bp, offset skull_06
        int 10h
        inc dh
        mov bp, offset skull_07
        int 10h
        inc dh
        mov bp, offset skull_08
        int 10h
        inc dh
        mov bp, offset skull_09
        int 10h
        inc dh
        mov bp, offset skull_10
        int 10h
        inc dh
        mov bp, offset skull_11
        int 10h
        inc dh
        mov bp, offset skull_12
        int 10h
        inc dh
        mov bp, offset skull_13
        int 10h
        inc dh
        mov bp, offset skull_14
        int 10h
        inc dh
        mov bp, offset skull_15
        int 10h
        inc dh
        mov bp, offset skull_16
        int 10h
        inc dh
        mov bp, offset skull_17
        int 10h
        inc dh
        mov bp, offset skull_18
        int 10h

        death_screen_end:
        pop DX
        pop CX
        pop BX
        pop AX
        pop es
        pop bp
    ret
    setWinScreen:
		call winScreen
		push ax
		Mov AH,05h
		MOV AL,04	; Set screen page
		INT 10h
		pop ax
	ret
    winScreen:
        push bp
        push es
        push AX
        push BX
        push cx
        push DX
        
        mov ax,ds
        mov es,ax
        mov ah,13h
        mov al, 1   ; update cursor position
        mov bh, 4   ; page #

        ;Writing text to screen
        mov bl, 0ah ; text attribute (color)
        mov cx, 15  ; string length
        mov dh, 2   ; row 0 - 24    \40x25
        mov dl, 12  ; column 0 - 39 /
        mov bp, offset congrats
        int 10h

        mov cx, 28  ; Change string length
        mov dh, 5   ; Change row
        mov dl, 6   ; Change column
        mov bp, offset win_message
        int 10h

        mov cx, 34  ; Change string length
        mov dh, 7   ; Change row
        mov dl, 3   ; Change column
        mov bp, offset complete
        int 10h

        ;Drawing firework 1
		cmp byte [coin1],0
		JE no_firework_1
        mov bl, 0bh ; Change color to light cyan
        mov cx, 5   ; Change string length
        mov dh, 11  ; Change row
        mov dl, 3   ; Change column
        mov bp, offset firework_1_1
        int 10h
        inc dh
        mov bp, offset firework_1_2
        int 10h
		no_firework_1:

        ;Drawing firework 2
		cmp byte [coin3],0
		JE no_firework_2
        mov bl, 0ch ; Change color to light red
        mov cx, 9   ; Change string length
        mov dh, 12  ; Change row
        mov dl, 15  ; Change column
        mov bp, offset firework_2_1
        int 10h
        inc dh
        mov bp, offset firework_2_2
        int 10h
        inc dh
        mov bp, offset firework_2_3
        int 10h
        inc dh
        mov bp, offset firework_2_4
        int 10h
		no_firework_2:
		
        ;Drawing firework 3
		cmp byte [coin2],0
		JE no_firework_3
        mov bl, 09h ; Change color to light blue
        mov cx, 6   ; Change string length
        mov dh, 9   ; Change row
        mov dl, 30  ; Change column
        mov bp, offset firework_3_1
        int 10h
        inc dh
        mov bp, offset firework_3_2
        int 10h
        inc dh
        mov bp, offset firework_3_3
        int 10h
        inc dh
        mov bp, offset firework_3_4
        int 10h
		no_firework_3:

        ;Drawing the Village
        mov bl, 06h ; Change color to brown
        mov cx, 31  ; Change string length
        mov dh, 16  ; Change row
        mov dl, 4   ; Change column
        mov bp, offset village_1
        int 10h
        inc dh
        mov bp, offset village_2
        int 10h
        inc dh
        mov bp, offset village_3
        int 10h
        inc dh
        mov bp, offset village_4
        int 10h
        inc dh
        mov bp, offset village_5
        int 10h
        inc dh
        mov bp, offset village_6
        int 10h
        inc dh
        mov bp, offset village_7
        int 10h
        inc dh
        mov bp, offset village_8
        int 10h
        inc dh
        mov bp, offset village_9
        int 10h
        
        ;Adding color to the village

        ;Coloring the cross
        mov bl, 0eh ; Change color to yellow
        mov cx, 1   ; Change string length
        mov dh, 16  ; Change row
        mov dl, 11   ; Change column
        mov bp, offset cross
        int 10h

        ;Coloring the house
        mov bl, 0fh ; Change color to white
        mov cx, 1   ; Change string length
        mov dh, 17  ; Change row
        mov dl, 33  ; Change column
        mov bp, offset smoke_1
        int 10h
        mov dh, 18  ; Change row
        mov dl, 29  ; Change column
        int 10h
        mov bl, 07h ; Change color to light gray
        mov cx, 3   ; Change string length
        mov dh, 18  ; Change row
        mov dl, 25  ; Change column
        mov bp, offset smoke_2
        int 10h
        mov cx, 2   ; Change string length
        inc dh      ; Change row
        dec dl      ; Change col
        mov bp, offset chimney
        int 10h
        
        ;Coloring the tree
        mov bl, 02h ; Change color to green
        mov cx, 1   ; Change string length
        mov dh, 20  ; Change row
        mov dl, 32  ; Change column
        mov bp, offset tree_1
        int 10h
        mov cx, 3   ; Change string length
        inc dh      ; Change row
        mov dl, 31  ; Change column
        mov bp, offset tree_2
        int 10h
        mov cx, 5   ; Change string length
        inc dh      ; Change row
        mov dl, 30  ; Change column
        mov bp, offset tree_3
        int 10h
        mov cx, 3   ; Change string length
        inc dh      ; Change row
        mov dl, 31  ; Change column
        mov bp, offset tree_4
        int 10h

        pop DX
        pop CX
        pop BX
        pop AX
        pop es
        pop bp
    ret
    winInput:
        MOV AH,00h
        INT 16h    		; INT 16h, AH = 00h | Keyboard Status
        mov word [currentScreen], 3
    ret
    setCreditScreen:
		push ax
		Mov AH,05h
		MOV AL,06	; Set screen page
		INT 10h
		pop ax
	ret
	
	creditScreen:
		push bp
        push es
        push AX
        push BX
        push cx
        push DX
		
		mov ax, ds
        mov es,ax
        mov ah,13h
        mov al, 1; update cursor position
        mov bh, 6 ; page #
        mov bl, 0fh; text attribute (color)
		
        mov cx, 7;string length
        mov dh, 1;row 0 - 24     \40x25
        mov dl, 16;column 0 - 39 /
        mov bp, offset credit_title
        int 10h

        mov cx, 16
        mov dh, 4
        mov dl, 12
        mov bp, offset dev_label
        int 10h

        mov cx, 38
        mov dh, 8
        mov dl, 1
        mov bp, offset jimmy_name
        int 10h
        
        mov cx, 38
        mov dh, 10
        mov dl, 1
        mov bp, offset walter_name
        int 10h
		
		mov cx, 38
        mov dh, 12
        mov dl, 1
        mov bp, offset jack_name
        int 10h
		
		mov cx, 38
        mov dh, 14
        mov dl, 1
        mov bp, offset drey_name
        int 10h
		
		mov cx, 38
        mov dh, 18
        mov dl, 1
        mov bp, offset audio_credit
        int 10h
		
		pop DX
        pop CX
        pop BX
        pop AX
        pop es
        pop bp
    ret
	
    creditInput:
        MOV AH,00h
        INT 16h    		; INT 16h, AH = 00h | Keyboard Status
        mov word [currentScreen], 3
    ret
	
	setTitleScreen:
		push ax
		Mov AH,05h
		MOV AL,03	; Set waiting screen page to last page
		INT 10h
		pop ax
	ret
	
	titleScreen:
		push bp
        push es
        push AX
        push BX
        push cx
        push DX
		
        mov ax, ds
        mov es,ax
        mov ah,13h
        mov al, 1   ; update cursor position
        mov bh, 3   ; page #

        ;Drawing of Castle
        mov bl, 07h ; text attribute (color)
		mov cx,40   ; string length
        mov dh,3    ; row 0 - 24    \40x25
        mov dl,0    ; column 0 - 39 /
        mov bp, offset castle_01
        int 10h
        inc dh
        mov bp, offset castle_02
        int 10h
        inc dh
        mov bp, offset castle_03
        int 10h
        inc dh
        mov bp, offset castle_04
        int 10h
        inc dh
        mov bp, offset castle_05
        int 10h
        inc dh
        mov bp, offset castle_06
        int 10h
        inc dh
        mov bp, offset castle_07
        int 10h
        inc dh
        mov bp, offset castle_08
        int 10h
        inc dh
        mov bp, offset castle_09
        int 10h
        inc dh
        mov bp, offset castle_10
        int 10h
        inc dh
        mov bp, offset castle_11
        int 10h
        inc dh
        mov bp, offset castle_12
        int 10h
        inc dh
        mov bp, offset castle_13
        int 10h
    
        ;Drawing Flags
        mov bl,0ch  ; Change color to light red
        mov cx,3    ; Change string length
        mov dh,3    ; Change row
        mov dl,20   ; Change column
        mov bp, offset castle_flag
        int 10h
        mov dh,4    ; Change row
        mov dl,6    ; Change column
        int 10h
        mov dl,35   ; Change column
        int 10h

        ;Drawing of castle gate
        mov bl, 06h ; Change color to brown
        mov cx,8    ; Change string length
        mov dh, 13  ; Change row 
        mov dl, 16  ; Change column
        mov bp, offset castle_gate
        int 10h
        inc dh
        int 10h
        inc dh 
        int 10h

        ;Writing text to screen
        mov bl,0ah  ; Change color to light green
        mov cx, 14  ; Change string length
        mov dh, 1   ; Change row
        mov dl, 14  ; Change column
        mov bp, offset game_name
        int 10h

        mov cx, 13  ; Change string length
        mov dh, 17  ; Change row
        mov bp, offset play_game
        int 10h

        mov dh, 19  ; Change row
        mov bp, offset load_game
        int 10h
        
        mov dh, 21  ; Change row
        mov bp, offset quit_game
        int 10h

        mov dh, 23  ; Change row
        mov bp, offset credit_screen
        int 10h
		
        pop DX
        pop CX
        pop BX
        pop AX
        pop es
        pop bp
    ret
	
	main_screen_input:
		;bgm playback
		
		push word 0
		call far enable_loop
		add sp, 2
		
		.menuLoop:
			MOV AH,01h
			INT 16h    		; INT 16h, AH = 01h | Keyboard Status
			jnz .menuLoop	; IF ZF is zero there was a keypress
			
			MOV AH,00h
			INT 16h 
			CMP AH, 31h
			je new_game
			CMP AH, 26h
			je load_save
			CMP AH,10h
			je quitTitle
			CMP AH,2Eh
			je credits
			jmp .menuLoop ; If no jmp has been executed then given input is invalid so skip the movement section
			new_game:
				push word 0
				call far stop_loop
				add sp, 2
				
				call loadingScreen
				call setLoadingScreen
				call clearPreviousWalls
				MOV BH,01
				call setBackground
				mov word [square_start_x], 155
				mov word [square_final_x], 165
				mov word [square_start_y], 95
				mov word [square_final_y], 105
				mov word [square_movement],5
				mov word [currentRoom], 13
				mov byte [health], 3
				mov byte [closeLava],0
				call resetKeysDoorsCoins
				call room13
				mov word [currentScreen], 0
				ret
			load_save:
				push word 0
				call far stop_loop
				add sp, 2
				
				call loadingScreen
				call setLoadingScreen
				call clearPreviousWalls
				MOV BH,01
				call setBackground
				call loadGame
				call changeRooms1Thru12
				; After loading room and saved character position, draw the character at that position
				MOV CX, [square_start_x]	
				MOV DX, [square_start_y]
				call drawNewSquare
				call resetInput
				mov word [currentScreen], 0
				ret
			jmp inputFinTitle
			quitTitle:
				push word 0
				call far stop_loop
				add sp, 2
				call terminate
			credits:
				mov word [currentScreen], 6
				push word 0
				call far stop_loop
				add sp, 2
				ret
			inputFinTitle:
				jmp main_screen_input
    ret

; Game Screen
    setGameScreen:
		; Game Screen for character movement is set to page 0
		push ax
		MOV AH,05h
		MOV AL,00
		INT 10h
		pop ax
	ret
	
	setTileScreen:
	push AX
		MOV AH,05h
		MOV AL,01
		INT 10h
	pop AX
	RET
	
    ;Drawing the game Screen is to complicated to include in this file
    gameInput:
		; Evaluates user input during game sequence
        ;	CURRENT SUPPORTED INPUTS
		;	Q=>quit | W=>up | S=>down | A=>left | D=>right
        call draw_character
		cmp byte [died],1	;If 1, the player has died 
		je playerDied
        MOV AH,01h
        INT 16h    		; INT 16h, AH = 01h | Keyboard Status
        jnz gameInput	; IF ZF is zero there was a keypress
        keypress:
        MOV AH,00h
        INT 16h
        CMP AH,11h 
        je character_up
        CMP AH,1fh
        je character_down
        CMP AH,20h
        je character_right
        CMP AH,1Eh
        je character_left
        CMP AH,19h
        je pause_screen
		CMP AH,39h		; If Spacebar pressed, toggle walk mode (move Distance = 1)
		jne inputFin
		cmp word [moveDistance],5
		JE toggleWalkSpeed
		MOV word [moveDistance],5
		jmp speedToggleFin
		toggleWalkSpeed:
		MOV word [moveDistance],1
		
		speedToggleFin:
		jmp inputFin ; If no jmp has been executed then given input is invalid so skip the movement section

        character_up:
			call checkUp				; Checks if Wall is a single pixel above it
            je inputFin					; if the character will be in a wall moving up
            ADD [square_start_y],BX		; Adds the negative move distance from checkUp to the start y
            ADD [square_final_y],BX		; Adds negative move distance to final y
			MOV word [square_movement],1		; 1 will move square up
        jmp inputFin
        character_down:
			call checkDown				; Checks if Wall is a single pixel below it
            je inputFin					; if the character will be in a wall moving down
            ADD [square_start_y],BX		; Adds move distance from checkDown to start y 
            ADD [square_final_y],BX		; Adds move distance to final y
			MOV word [square_movement],2		; 2 will move square down
        jmp inputFin
        character_right:	
			call checkRight				; Checks if Wall is a single pixel to the right
            je inputFin					; if the character will be in a wall moving right
            ADD [square_start_x],BX		; Adds move distance from checkRight to start x
            ADD [square_final_x],BX		; Adds move distance to final x
			MOV word [square_movement],3		; 3 will move square right
        jmp inputFin
        character_left:
			call checkLeft				; Checks if Wall is a single pixel to the left
            je inputFin					; if the character will be in a wall moving left
            ADD [square_start_x],BX		; Adds negative move distance from checkLeft to start x 
            ADD [square_final_x],BX		; Adds negative move distance to final x
			MOV word [square_movement],4		; 4 will move square left
        jmp inputFin
        pause_screen:
            mov word [currentScreen], 5
            ret
        jmp inputFin
		
		playerDied:
			call secDelay
			call black_screen
			dec byte [health]
			call updateHealth
			call setDeathScreen
			call deathScreen
			call clearPreviousWalls
			MOV BH,00
			call setBackground
			mov word [square_start_x], 155
			mov word [square_final_x], 165
			mov word [square_start_y], 95
			mov word [square_final_y], 105
			MOV CX, [square_start_x]	
			MOV DX, [square_start_y]
			call drawNewSquare
			mov word [currentRoom], 13
			call room13
			mov byte [died],0
			mov byte [closeLava],0
			mov word [currentScreen],0
			call resetInput
			cmp byte [health],0
			je gameOver
			jmp inputFin
		gameOver:
			call black_screen
			mov word [currentScreen],0003h
        inputFin:
    ret

; Wait Screen that is set between rooms being drawn so the process is not seen by the player
	setLoadingScreen:
        ;Set display page to last page
		push ax
		Mov AH,05h
		MOV AL,07	; Set waiting screen page to last page
		INT 10h
		pop ax
	RET
	loadingScreen:
	 	push bp
        push es
        push AX
        push BX
        push cx
        push DX
        
        mov ax, ds
        mov es,ax
        mov ah,13h
        mov al, 1; update cursor position
        mov bh, 7 ; page #

        ;Writing ASCII art text on screen
        mov bl, 0ah ; text attribute (color)
		mov cx, 34  ; string length
        mov dh, 3   ; row 0 - 24    \40x25
        mov dl, 3   ; column 0 - 39 /
        mov bp, offset loading_room_01
        int 10h 
        inc dh
        mov bp, offset loading_room_02
        int 10h
        inc dh
        mov bp, offset loading_room_03
        int 10h 
        inc dh
        mov bp, offset loading_room_04
        int 10h 
        inc dh
        mov bp, offset loading_room_05
        int 10h  
        inc dh
        mov bp, offset loading_room_06
        int 10h 
        inc dh
        mov bp, offset loading_room_07
        int 10h 
        inc dh
        mov bp, offset loading_room_08
        int 10h 
        mov dh, 14
        mov bp, offset loading_room_09
        int 10h 
        inc dh
        mov bp, offset loading_room_10
        int 10h
        
        ;Writing text onto the screen
        mov cx, 35  ; Change string length
        mov dh, 20  ; Change row
        mov dl, 3   ; Chane column
        mov bp, offset tip_message
        int 10h  

        pop DX
        pop CX
        pop BX
        pop AX
        pop es
        pop bp
	ret
; Pause Screen
    setPauseScreen:
        ;Set display page to last page
		push ax
		Mov AH,05h
		MOV AL,05	; Set waiting screen page to last page
		INT 10h
		pop ax
	RET
    pauseScreen:
        push bp
        push es
        push AX
        push BX
        push cx
        push DX
        
        mov ax, ds
        mov es,ax
        mov ah,13h
        mov al, 1; update cursor position
        mov bh, 5 ; page #

        ;;Writing ASCII art text on screen
        mov bl, 0ah ; text attribute (color)
        mov cx, 38  ; string length
        mov dh, 1   ; row 0 - 24    \40x25
        mov dl, 1   ; column 0 - 39 /
        mov bp, offset game_paused_01
        int 10h
        inc dh
        mov bp, offset game_paused_02
        int 10h
        inc dh
        mov bp, offset game_paused_03
        int 10h
        inc dh
        mov bp, offset game_paused_04
        int 10h
        inc dh
        mov bp, offset game_paused_05
        int 10h
        inc dh
        mov bp, offset game_paused_06
        int 10h
        inc dh
        mov bp, offset game_paused_07
        int 10h
        inc dh
        mov bp, offset game_paused_08
        int 10h
        inc dh
        mov bp, offset game_paused_09
        int 10h
        inc dh
        mov bp, offset game_paused_10
        int 10h
        inc dh
        mov bp, offset game_paused_11
        int 10h
        inc dh
        mov bp, offset game_paused_12
        int 10h

        ;Writing text onto the screen
        mov cx, 17  ; Change string length
        mov dh, 15  ; Change row
        mov dl, 12  ; Change column
        mov bp, offset resume
        int 10h

        mov dh, 17 ; Change row
        mov bp, offset save
        int 10h

        mov dh, 19  ; Change row
        mov bp, offset save_quit
        int 10h

        mov dh, 21  ; Change row
        mov bp, offset to_title
        int 10h

        pop DX
        pop CX
        pop BX
        pop AX
        pop es
        pop bp
    ret
     pausedInput:
        MOV AH,01h
        INT 16h    		; INT 16h, AH = 01h | Keyboard Status
        jz pausedInput	; IF ZF is zero there was a keypress
        MOV AH,00h
        INT 16h   
        CMP AH, 13h
        je resumeGame
        CMP AH, 1fh
        je saveGame
        cmp AH, 10h
        je quitYesSave
		CMP AH, 32h
		je quitNoSave
		jmp pausedInput; If no jmp has been executed then given input is invalid so skip the movement section
        
		resumeGame:
            mov word [currentScreen],0
        ret
        
		saveGame:
			call saveFile
		ret
        
		quitYesSave:
			call saveFile
			call terminate
      
		quitNoSave:
            mov word [currentScreen], 3
            ret
        inputFinPause:
        jmp pausedInput
    ret
;end of pages

;start of saveLoad
;Variables to save/load:
;currentRoom dw
;square_start_x dw 		
;square_final_x dw 
;square_start_y dw 


saveFile:	
    ;Creates save file
	mov ah, 3ch
	mov cx, 0
	mov dx, offset fileName
	int 21h
	
	mov word [fileHandler], ax ;Preserve the file handler.
		
	;saves current room to file.
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 2 ;Number of bytes to write
	mov dx, currentRoom
	int 21h

	;saves current x start location of the character
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 2 ;Number of bytes to write
	mov dx, square_start_x
	int 21h

	
	;saves current x final location of the character
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 2 ;Number of bytes to write
	mov dx, square_final_x
	int 21h
	
	
	;saves current y start location of the character.
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 2 ;Number of bytes to write
	mov dx, square_start_y
	int 21h

	
	;saves current y final location of the character.
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 2 ;Number of bytes to write
	mov dx, square_final_y
	int 21h
	
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 1 ;Number of bytes to write
	mov dx, redKey
	int 21h
	
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 1 ;Number of bytes to write
	mov dx, cyanKey
	int 21h
	
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 1 ;Number of bytes to write
	mov dx, magentaKey
	int 21h
	
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 1 ;Number of bytes to write
	mov dx, redDoor
	int 21h
	
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 1 ;Number of bytes to write
	mov dx, cyanDoor
	int 21h
	
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 1 ;Number of bytes to write
	mov dx, magentaDoor
	int 21h
	
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 1 ;Number of bytes to write
	mov dx, health
	int 21h
	
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 1 ;Number of bytes to write
	mov dx, coin1
	int 21h
	
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 1 ;Number of bytes to write
	mov dx, coin2
	int 21h
	
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 1 ;Number of bytes to write
	mov dx, coin3
	int 21h
	
	mov ah, 40h
	mov bx, word [fileHandler]
	mov cx, 2 ;Number of bytes to write
	mov dx, '\n'
	int 21h
	
	call closeFile
RET


loadGame:
	;Opens the file
	mov dx, offset fileName ;Loads address of file to DX
	mov al, 0 ;open file as read only.
	mov ah, 3dh
	int 21h
	JNC successful_open
	ret
	successful_open:
	mov word [fileHandler], ax ;Preserves file handler
	
	;Reads the file two bytes at a time and stores data in current room.
	mov ah,3fh
	mov bx, word [fileHandler]
	mov cx, 2
	mov dx, offset currentRoom
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_start_x.
	mov ah,3fh
	mov bx, word [fileHandler]
	mov cx, 2
	mov dx, offset square_start_x
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_final_x.
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 2
	mov dx, offset square_final_x
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_start_y.
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 2
	mov dx, offset square_start_y
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_final_y.
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 2
	mov dx, offset square_final_y
	int 21h
	
	;Reads the file 1 bytes at a time and stores data in redKey
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 1		; Keys and rooms only 1 byte
	lea dx, offset redKey
	int 21h
	
	;Reads the file 1 bytes at a time and stores data in cyanKey
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 1	; Keys and rooms only 1 byte
	lea dx, offset cyanKey
	int 21h
	
	;Reads the file 1 bytes at a time and stores data in magentaKey
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 1	; Keys and rooms only 1 byte
	lea dx, offset magentaKey
	int 21h
	
	;Reads the file 1 bytes at a time and stores data in redDoor
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 1		; Keys and rooms only 1 byte
	lea dx, offset redDoor
	int 21h
	
	;Reads the file 1 bytes at a time and stores data in cyanDoor
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 1		; Keys and rooms only 1 byte
	lea dx, offset cyanDoor
	int 21h
	
	;Reads the file 1 bytes at a time and stores data in magentaDoor
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 1		; Keys and rooms only 1 byte
	lea dx, offset magentaDoor
	int 21h
	
	;Reads the file 1 bytes at a time and stores data in health
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 1		; Health only 1 byte
	lea dx, offset health
	int 21h
	
	;Reads the file 1 bytes at a time and stores data in coin1
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 1		; Coins only 1 byte
	lea dx, offset coin1
	int 21h
	
	;Reads the file 1 bytes at a time and stores data in coin2
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 1		; Coins only 1 byte
	lea dx, offset coin2
	int 21h
	
	;Reads the file 1 bytes at a time and stores data in coin3
	mov ah,3fh
	mov bx,word [fileHandler]
	mov cx, 1		; Coins only 1 byte
	lea dx, offset coin3
	int 21h
	
	call closeFile		
RET

closeFile:;closes the file
	mov ah, 3eh
	mov bx, word [fileHandler]
	int 21h
RET

;end of saveLoad

segment data ;Data Segment for Variables Used
	
	;initial square starting values
	INITIAL_SQUARE_START_X dw 155
	INITIAL_SQUARE_FINAL_X dw 165
	INITIAL_SQUARE_START_Y dw 95
	INITIAL_SQUARE_FINAL_Y dw 105

	; Used in Background and Room Creation
	col dw 0000h
	row dw 0000h
	tileYLength dw 10
	tileXLength dw 32
	tileSpacing dw 4
	sizeWallCol dw 0000h
	sizeWallRow dw 0000h
	

	; Initial starting position for character
	square_start_x dw 155 	; First Column		; If these changed then must change checkRoomChange
	square_final_x dw 165	; Final Column
	square_start_y dw 95	; First Row
	square_final_y dw 105	; Final Row
	squareSize dw 0000h			; Size of the square will be determined in program from the start_x and final_x

	; Used to aid in square movement
	moveDistance dw 5		; MAX distance character can move
	actualMoveDistance dw 0	; Actual calculated distance character will move
	square_movement dw 5	; 0 = no movement | 1 move up | 2 = move down | 3 = move right | 4 = move left 
	
	;Changing Rooms
	currentRoom dw 13
	
	; Key Variables
	keyColor db 0		;0 for BlackKey ; 0Ch for RedKey (0Ch) ; 0Bh for CyanKey ; 0Eh for Yellowkey ; 
	keyStartY dw 0000h
	keyStartX dw 0000h
	redKey db 0
	cyanKey db 0
	magentaKey db 0
	
	; Door Variables
	doorColor db 0
	redDoor db 0
	cyanDoor db 0
	magentaDoor db 0
	doorStartY dw 0000h
	doorStartX dw 0000h
	
	; Coin Variables
	coin1 db 0
	coin2 db 0
	coin3 db 0
	coinColor db 00h
	coinStartY dw 0000
	coinStartX dw 0000
	score dw 5000	   
	
	;Health
	health db 3		;Start with three hearts
	died db 0	;If recently died, make 1, this is for calling death screen during movements
	closeLava db 0
	
	;Displaying Screens
	currentScreen dw 3; Set First Screen to Title Screen

	;Screen Text
	;Loading Screen
		tip_message db "Tip: Press P at any point to pause.";35

        ;ASCII TEXT based on webiste below and Small font default settings
        ;https://patorjk.com/software/taag/#p=display&f=3D%20Diagonal&t=Type%20Something%20
        loading_room_01 db " _                  _ _           ";34x10
        loading_room_02 db "| |    ___  __ _ __| (_)_ _  __ _ "
        loading_room_03 db "| |__ / _ \/ _` / _` | | ' \/ _` |"
        loading_room_04 db "|____|\___/\__,_\__,_|_|_||_\__, |"
        loading_room_05 db "         ___                |___/ "
        loading_room_06 db "        | _ \___  ___ _ ___       "
        loading_room_07 db "        |   / _ \/ _ \ '   \      "
        loading_room_08 db "        |_|_\___/\___/_|_|_|      "
        loading_room_09 db "         _   _   _   _   _        "
        loading_room_10 db "        (_) (_) (_) (_) (_)       "
		
	;Credit Screen
		credit_title db "CREDITS",0
		dev_label db    "Development Team",0
		jimmy_name db   "Traversal Code:           Jimmy Rogers",0
		walter_name db  "Character Design:     Walter Sandmeier",0
		jack_name db    "Audio:                      Jack Byers",0
		drey_name db    "Map Design:               Drey Newland",0
		audio_credit db "Audio:            Leonardo Ono, Konami",0
	;Title Screen
		game_name db "Dungeon Escape";14
		play_game db "New Game  - N";13
		load_game db "Load Game - L";13
		quit_game db "Quit Game - Q";13
		credit_screen db   "Credits   - C";13 
        
        ;ASCII art for castle found here http://www.asciiworld.com/-Castels-.html and then edited 
        castle_01 db "                   |>>>                 "
        ;castle_02 db"     |>>>     ___ _|__ ___        |>>>  "
        castle_03 db "___ _|_ ___   | |_|  |_| |   ___ _|_ ___"
        castle_04 db "| |_| |_| |   |__________|   | |_| |_| |"
        castle_05 db "|_________|    |        |    |_________|"
        castle_06 db "  |     |      |        |      |   . |  "
        castle_07 db "  |  ,  |__ ___|___  ___|___ __| ,   |  "
        ;castle_08 db"  |     | |_| ||| |__| ||| |_| |  :  |  "
        castle_09 db "  |  .  | .    .    . .      . |   . |  "
        castle_10 db "  |  ,  | :  ,  ________   .   |     |  "
        castle_11 db "  |   . |  .   /++++++++\    . | : . |  "
        castle_12 db "  |  .  | .    |++++++++| .    |  ,  |  "
        castle_13 db "  |_____|______|++++++++|______|_____|  "
        castle_02 db '     |>>>     ___ _',193,'__ ___        |>>>   '
        castle_08 db '  |     | |_| |',193,'| |__| |',193,'| |_| |  :  |  '
        castle_flag db ">>>";line 1 20, line 2 6 35
        castle_gate db "++++++++"
		
	;Pause Screen
		resume db       "Resume Play   - R";17
		save db         "Save Game     - S";17
		save_quit db    "Save and Quit - Q";17
		to_title db     "Main Screen   - M";17
        
        ;ASCII TEXT based on webiste below and Big font change width to fitted
        ;https://patorjk.com/software/taag/#p=display&f=3D%20Diagonal&t=Type%20Something%20
        game_paused_01 db "    ______                            ";38x12
        game_paused_02 db "   /  ____|                           "
        game_paused_03 db "   | |  __  ___ _  _ __ ____  ____    "
        game_paused_04 db "   | | |_ |/  _` || '_ ` _  \/  _ \   "
        game_paused_05 db "   | |__| || (_| || | | | | ||  __/   "
        game_paused_06 db "   \______|\___,_||_| |_| |_|\____|   "
        game_paused_07 db " _____                              _ "
        game_paused_08 db "|  __ \                            | |"
        game_paused_09 db "| |__)| __ _  _   _  ___  ____  ___| |"
        game_paused_10 db "|  ___// _` || | | |/ __|/  _ \/  _` |"
        game_paused_11 db "| |    |(_| || |_| |\__ \|  __/| (_| |"
        game_paused_12 db "|_|    \__,_| \__,_||___/\____|\___,_|"
		
	;Win Screen
		congrats db "Congratulations";15
		win_message db "You Have Escaped the Dungeon";28
		complete db "Hit any key to return Title Screen";34

        ;All ASCII art fireworks obtained from here https://www.asciiart.eu/holiday-and-events/fireworks
        firework_1_1 db "_\(/_";5x2
        firework_1_2 db " /)\ "

        firework_2_1 db "  ^,^,^  ";9x4
        firework_2_2 db "^,^,^,^,^"
        firework_2_3 db "^,^,^,^,^"
        firework_2_4 db "  ^,^,^  "

        firework_3_1 db " *''* ";6x4
        firework_3_2 db "*_\/_*"
        firework_3_3 db "* /\ *"
        firework_3_4 db " *..* "
        
       ;ASCII art village obtained from here https://www.asciiart.eu/buildings-and-places/cities with minor editing
        village_1 db "      _+                       ";31x9
        village_2 db "     // \                    ~ "
        village_3 db "     ][O]            ,-~ ~     "
        village_4 db " /'''-I_I         __II____     "
        village_5 db "/__ _/   \       /  ''   /\ ,  "
        village_6 db "|   |/''''\     :--..___/  { } "
        village_7 db "| _ | /__\|     |[] .-.| O{ _ }"
        village_8 db "|[_]| |  ||     ''--:_:|_- \,/ "
        village_9 db "|   | |[]||                 |  "
        tree_1 db ",";1x1
        tree_2 db "{ }";3x1
        tree_3 db "{ _ }";5x1
        tree_4 db "\,/";3x1
        smoke_1 db "~";1x1
        smoke_2 db ",-~";3x1
        chimney db "II";2x1
        cross db "+";1x1
		
	;Death Screen
		;ASCII TEXT based on webiste below and Big font default settings
		;https://patorjk.com/software/taag/#p=display&f=3D%20Diagonal&t=Type%20Something%20
		you_died_1 db "__     __           _____  _          _ ";40x6
		you_died_2 db "\ \   / /          |  __ \(_)        | |"
		you_died_3 db " \ \_/ /__  _   _  | |  | |_  ___  __| |"
		you_died_4 db "  \   / _ \| | | | | |  | | |/ _ \/ _` |"
		you_died_5 db "   | | (_) | |_| | | |__| | |  __/ (_| |"
		you_died_6 db "   |_|\___/ \__,_| |_____/|_|\___|\__,_|"

		;ASCII art heart with additional minus symbol
		;Heart is found here http://www.asciiworld.com/-Hearts-.html
		heart_1 db "             .:::.   .:::. ";27x7
		heart_2 db "            :::::::.:::::::"
		heart_3 db " _____      :::::::::::::::"
		heart_4 db "|_____|      ::::::::::::: "
		heart_5 db "               :::::::::   "
		heart_6 db "                 :::::     "
		heart_7 db "                   :       "
		minus_1 db " _____ ";7x2
		minus_2 db "|_____|"

		;ASCII TEXT based on webiste below and Big font default settings
		;https://patorjk.com/software/taag/#p=display&f=3D%20Diagonal&t=Type%20Something%20
		you_lose_1 db "__     _            _                   ";40x6
		you_lose_2 db "\ \   / /          | |                  "
		you_lose_3 db " \ \_/ /__  _   _  | |    ___  ___  ___ "
		you_lose_4 db "  \   / _ \| | | | | |   / _ \/ __|/ _ \"
		you_lose_5 db "   | | (_) | |_| | | |__| (_) \__ \  __/"
		you_lose_6 db "   |_|\___/ \__,_| |_____\___/|___/\___|"

		;ASCII art skull found here http://www.asciiworld.com/-Death-Co-.html
		skull_01 db "               _____                 ";37x18
		skull_02 db "        __,---'     `--.__           "
		skull_03 db "     ,-'                ; `.         "
		skull_04 db "    ,'                  `--.`--.     "
		skull_05 db "   ,'                       `._ `-.  "
		skull_06 db "   ;                     ;     `-- ; "
		skull_07 db " ,-'-_       _,-~~-.      ,--      `."
		skull_08 db " ;;   `-,;    ,'~`.__    ,;;;    ;  ;"
		skull_09 db " ;;    ;,'  ,;;      `,  ;;;     `. ;"
		skull_10 db " `:   ,'    `:;     __/  `.;      ; ;"
		skull_11 db "  ;~~^.   `.   `---'~~    ;;      ; ;"
		skull_12 db "  `,' `.   `.            .;;;     ;' "
		skull_13 db "  ,',^. `.  `._    __    `:;     ,'  "
		skull_14 db "  `-' `--'    ~`--'~~`--.  ~    ,'   "
		skull_15 db " /;`-;_ ; ;. /. /   ; ~~`-.     ;    "
		skull_16 db "; ;  ; `,;`-;__;---;      `----'     "
		skull_17 db "``-`-;__;:  ;  ;__;                  "
		skull_18 db "         `-- `-'                     "
		
	;used to save/load games
	fileName db "saveGame.bin",0
	fileHandler dw ?


segment stack stack
	resb 256
stacktop:
