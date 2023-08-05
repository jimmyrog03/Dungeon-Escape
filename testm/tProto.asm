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
	
	;Changing Rooms
	currentRoom dw 13
	
	keyColor db 0		;0 for BlackKey ; 0Ch for RedKey (0Ch) ; 0Bh for CyanKey ; 0Eh for Yellowkey ; 
	keyStartY dw ?
	keyStartX dw ?
	redKey db 0
	cyanKey db 0
	yellowKey db 0
	
	doorColor db ?
	redDoor db 0
	cyanDoor db 0
	yellowDoor db 0
	doorStartY dw ?
	doorStartX dw ?
	
	coin1 db 0
	coin2 db 0
	coin3 db 0
	coinStartY dw ?
	coinStartX dw ?
	
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
		load_game db "Load Game - R"
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
	fileName db "saveGame.bin",0
	fileHandler dw ?
	newline db '\r\n'
	 
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
	
	resetInput:
		MOV AH,0Ch
        MOV AL,00h	; Return nothing
        INT 21h		; INT 21h, AH = 0Ch, AL = 0Ah | Reset input buffer and input
	RET
	
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