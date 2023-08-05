.model small
;.386
.stack 100h
INITIAL_SQUARE_START_X equ 155
INITIAL_SQUARE_FINAL_X equ 165
INITIAL_SQUARE_START_Y equ 95
INITIAL_SQUARE_FINAL_Y equ 105
.data; Data Segment for Variables Used

	; Used in Background and Room Creation
	col dw 0000h
	row dw 0000h
	tileYLength dw 10
	tileXLength dw 32
	tilespacing dw 4
	sizeWallCol dw ?
	sizeWallRow dw ?

	; Initial starting position for character
	square_start_x dw 155 	; First Column		; If these changed then must change checkRoomChange
	square_final_x dw 165	; Final Column
	square_start_y dw 95	; First Row
	square_final_y dw 105	; Final Row
	squareSize dw ?			; Size of the square will be determined in program from the start_x and final_x

	; Used to aid in square movement
	moveDistance dw 5		; MAX distance character can move
	actualMoveDistance dw 0	; Actual calculated distance character will move
	square_movement dw 5	; 0 = no movement | 1 move up | 2 = move down | 3 = move right | 4 = move left 
	
	; Variables for Arrow obstacles
	arrowsPresent dw 0		; 0 = no arrows, 1 = 1 arrow, 2 = 2 arrows
	arrowDirection db 0		; 0 = Vertical Arrows, 1 = Horizontal Arrows
	arrow1RowV dw ?
	arrow1ColV dw ?
	arrow2RowV dw ?
	arrow2ColV dw ?
	arrow1RowH dw ?
	arrow1ColH dw ?
	arrow2RowH dw ?
	arrow2ColH dw ?
	arrowLength dw 12
	arrowSpeed dw 1
	arrowTravelDistance dw 0
	erasingArrow dw ?
	
	;Changing Rooms
	currentRoom dw 13

	;Displaying Screens
	currentScreen dw 3; Set First Screen to Title Screen

	;Screen Text
	;Loading Screen
		tip_message db "Tip: Press P at any point to pause."
		waiting_message db "Loading Room"
		waiting db "...."
	;Credit Screen
		credit_title db "CREDITS"
		dev_label db    "Development Team"
		jimmy_name db   "Traversal Code:           Jimmy Rogers"
		walter_name db  "Character Design:     Walter Sandmeier"
		jack_name db    "Audio:                      Jack Byers"
		drey_name db    "Map Design:               Drey Newland"
		audio_credit db "Audio:             Insert music credit"
	;Title Screen
		game_name db "The Maze"
		play_game db "New Game - N"
		load_game db "Resume Game - R"
		quit_game db "Quit Game - Q" 
		credit_screen db   "Credits - C"  
	;Pause Screen
		paused db       "Game Paused";11
		resume db       "Resume Play - R";15
		save db         "Save Game - S";13
		save_quit db    "Save and Quit - Q";17
		to_title db     "Main Screen - M";15
	;Win Screen
		congrats db "Congratulations";15
		win_message db "You Have Escaped the Maze";25
		complete db "Hit any key to return Title Screen";34

	;used to save/load games
	fileName db "saveGame.txt",0
	fileHandler dw ?
	 
.code; Code Segement
; Constants Lookup
; Page 0 = Game Screen | Walls, Tiles, and Character
; Page 1 = Tile Screen | Tiles
main proc
	start:
	ORG 100h

	MOV AX, @data	;\ Initialize the data segment
	MOV ds,AX		;/

	call graphics

	; Set the square size before program begins
	MOV AX, square_final_x
	SUB AX, square_start_x
	MOV squareSize, AX
	
	call titleScreen
	call pauseScreen
	call winScreen
	call creditScreen
	
	gameLoop:
		;Used for Pages with Inputs
		cmp currentScreen, 0
		jne page0
			call setGameScreen
			call gameInput
		page0:
		cmp currentScreen, 3
		jne page3
			call setTitleScreen
			call main_screen_input
		page3:
		cmp currentScreen, 4
		jne page4
			call setWinScreen
			call secDelay
			call resetInput
			call winInput
		page4:
		cmp currentScreen, 5
		jne page5
			call setPauseScreen
			call pausedInput
		page5:
		cmp currentScreen, 6
		jne page6
			call setCreditScreen
			call creditInput
		page6:
	jmp gameLoop
	
	terminate:
		;Terminates the program and returns to text mode
        MOV AH,00h ;
        MOV AL,03h ; Graphics mode -> text mode 40x25
        INT 10h    ; INT 10h, AH = 00h | Set Video Mode
        MOV AH,4Ch
		MOV AL,00h
        INT 21h    ; INT 21h, AH = 4Ch, AL = exit code | Terminate program with exit code
    ret
	
	black_screen: 
						; Sets All Pixels in the Screen to Black
		MOV CX, 0       ; Column starts at square_start ends at square_end
		MOV DX, 0       ; Row Starts at 20 and ends at 40 for all squares
		MOV BH, 0
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

	INCLUDE tchar.inc
	INCLUDE tpages.inc
	INCLUDE troom.inc
	INCLUDE trooms.inc
	INCLUDE tSL.inc
	
	arrowRoutine:
		push AX
		push BX
		push CX
		push DX
		push SI
		push DI
		mov si,0
		mov di,0
		cmp arrowDirection,0
		jne horizontalArrows
		mov CX, arrow1ColV		; The x cord of the arrow
		mov DX, arrow1RowV		; The y cord of the arrow
		arrow1VLoop:
		MOV AH, 0Ch		; Write Graphics pixel
		MOV AL, 0Fh		; Color (White)
		MOV BH, 00h		; Page (0)
		INT 10h
		INC DX
		INC SI			; Increment the pixels drawn until it is the length of an arrow
		cmp SI, 12
		jne arrow1VLoop
		SUB DX, 12 	;Subtract by arrow length
		MOV SI,0
		inc CX
		inc DI
		cmp DI,3		; Arrow Width?
		jne arrow1VLoop
		MOV erasingArrow, 1
		SUB CX,3
		call removeDownTrail
		INC arrowTravelDistance
		
		INC arrow1RowV	; Add the move distance of the arrows, default 1
		
		cmp arrowsPresent,1
		je arrowFin
		
		
		
		horizontalArrows:
		
		
		
		cmp arrowsPresent,1
		je arrowFin
		
		
		
		
		arrowFin:
		pop DI
		pop SI
		pop DX
		pop CX
		pop BX
		pop AX
	RET
	
	removeDownTrail:
		mov si,0
		mov di,0
		loopArrowDown:
		MOV AH,0Dh		; Read graphics Pixel 
		MOV BH,01h		; Read from page 1 since this only has tiles
		INT 10h
        MOV AH, 0Ch		; Draw pixel read from page 1 (tiles)
        MOV BH, 00h		; Draw pixels on page 0 (has character)
        INT 10h
		INC DX			; Inc the row of the arrow
        INC SI			; Inc row counter (SI)
		cmp SI,erasingArrow	; Arrow Speed
        JNE loopArrowDown				; Jump if not greater to continue looping
		MOV SI,0
        SUB DX,erasingArrow				; Subtract the row by the Arrow speed/or length of arrow
        INC CX					
		INC DI
        CMP DI,3					
        JNE loopArrowDown	
	RET
	
	arrowCheckDown:
		MOV DX,0000h
        MOV CX,arrow1ColV			; Sets CX to x cord
		MOV DX,arrow1RowV			; Sets DX to y cord
		ADD DX,11					; Check pixel in front of arrow (add 12 since length of arrow is 12
		MOV AH,0Dh					; Read graphics Pixel 
		MOV BH,00h					; Read from page 0 as this has walls drawn
		INT 10h
		CMP AL,02h					; Check if pixel is green
		jne noDownCollisionArrow
		MOV erasingArrow, 11
		SUB DX,11
		call removeDownTrail
		MOV AX,arrowTravelDistance
		SUB arrow1RowV, AX
		MOV arrowTravelDistance, 0
		noDownCollisionArrow:
	ret
	
	resetInput:
		MOV AH,0Ch
        MOV AL,00h	; Return nothing
        INT 21h		; INT 21h, AH = 0Ch, AL = 0Ah | Reset input buffer and input
	RET
	
	secDelay:			;Roughly delays for a second when called
		push AX
		MOV AX,0FFFFh
		call restLoop
		call restLoop
		call restLoop
		call restLoop
		call restLoop
		call restLoop
		call restLoop
		call restLoop
		call restLoop
		call restLoop
		call restLoop
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
main endp
end main