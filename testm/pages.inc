;This File details the functions for setting the page, drawing, and inputs
;Page 0 => Game Screen
;Page 1 => Tiles only
;Page 2 => 
;Page 3 => Title Screen
;Page 4 => Win Screen
;Page 5 => Pause screen
;Page 6 => Credit Screen
;Page 7 => Loading Screen

    setWinScreen:
		push ax
		Mov AH,05h
		MOV AL,04	; Set screen page
		INT 10h
		pop ax
	ret
    winScreen:
        mov ax,ds
        mov es,ax
        mov ah,13h
        mov al, 1; update cursor position
        mov bh, 4; page #
        mov bl, 0ah; text attribute (color)
        mov cx, 15;string length
        mov dh, 8;row 0 - 24     \40x25
        mov dl, 11;column 0 - 39 /
        mov bp, offset [congrats]
        int 10h

        mov cx, 25
        mov dh, 10
        mov dl, 8
        mov bp, offset [win_message]
        int 10h

        mov cx, 34
        mov dh, 16
        mov dl, 3
        mov bp, offset [complete]
        int 10h
    ret
    winInput:
        MOV AH,00h
        INT 16h    		; INT 16h, AH = 00h | Keyboard Status
        mov currentScreen, 3
    ret
    setCreditScreen:
		push ax
		Mov AH,05h
		MOV AL,06	; Set screen page
		INT 10h
		pop ax
	ret
	
	creditScreen:
		mov ax, ds
        mov es,ax
        mov ah,13h
        mov al, 1; update cursor position
        mov bh, 6 ; page #
        mov bl, 0fh; text attribute (color)
		
        mov cx, 7;string length
        mov dh, 1;row 0 - 24     \40x25
        mov dl, 16;column 0 - 39 /
        mov bp, offset [credit_title]
        int 10h

        mov cx, 16
        mov dh, 4
        mov dl, 12
        mov bp, offset [dev_label]
        int 10h

        mov cx, 38
        mov dh, 8
        mov dl, 1
        mov bp, offset [jimmy_name]
        int 10h
        
        mov cx, 38
        mov dh, 10
        mov dl, 1
        mov bp, offset [walter_name]
        int 10h
		
		mov cx, 38
        mov dh, 12
        mov dl, 1
        mov bp, offset [jack_name]
        int 10h
		
		mov cx, 38
        mov dh, 14
        mov dl, 1
        mov bp, offset [drey_name]
        int 10h
		
		mov cx, 38
        mov dh, 18
        mov dl, 1
        mov bp, offset [audio_credit]
        int 10h
		
    ret
    creditInput:
        MOV AH,00h
        INT 16h    		; INT 16h, AH = 00h | Keyboard Status
        mov currentScreen, 3
    ret
		
	setTitleScreen:
		push ax
		Mov AH,05h
		MOV AL,03	; Set waiting screen page to last page
		INT 10h
		pop ax
	ret
	
	titleScreen:
		mov ax, ds
        mov es,ax
        mov ah,13h
        mov al, 1; update cursor position
        mov bh, 3 ; page #
        mov bl, 0ah; text attribute (color)
		
        mov cx, 8;string length
        mov dh, 6;row 0 - 24     \40x25
        mov dl, 16;column 0 - 39 /
        mov bp, offset [game_name]
        int 10h

        mov cx, 12
        mov dh, 10
        mov dl, 14
        mov bp, offset [play_game]
        int 10h

        mov cx, 13
        mov dh, 12
        mov dl, 14
        mov bp, offset [load_game]
        int 10h
        
        mov cx, 13
        mov dh, 14
        mov dl, 14
        mov bp, offset [quit_game]
        int 10h

        mov cx, 11
        mov dh, 18
        mov dl, 14
        mov bp, offset [credit_screen]
        int 10h
    ret
	
	main_screen_input:
        MOV AH,01h
        INT 16h    		; INT 16h, AH = 01h | Keyboard Status
        jnz main_screen_input	; IF ZF is zero there was a keypress
        MOV AH,00h
        INT 16h   
        
		CMP AH, 31h
        je new_game
        
		CMP AH, 13h
        je load_save
		
		CMP AH,10h
		je quitTitle
		
        CMP AH,2Eh
        je credits
    	jmp inputFinTitle ; If no jmp has been executed then given input is invalid so skip the movement section

		new_game:
            call loadingScreen
            call setLoadingScreen
            mov square_start_x, 155
            mov square_final_x, 165
            mov square_start_y, 95
            mov square_final_y, 105
            mov square_movement,5
            mov currentRoom, 13
			call resetKeys&Doors
            call black_screen
            call setBackground
            call room13
            mov currentScreen, 0
            ret
		;jmp inputFinTitle
		load_save:
			call loadingScreen
			call setLoadingScreen
			call clearPreviousWalls
			call setBackground
			call loadGame
            call changeRooms1Thru12
			; After loading room and saved character position, draw the character at that position
			MOV CX, square_start_x	
			MOV DX, square_start_y	
			call drawNewSquare
            mov currentScreen, 0
            ret
		jmp inputFinTitle
        
		quitTitle:
            call terminate
			;jmp inputFinTitle

        credits:
            mov currentScreen, 6
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
    ;Drawing the game Screen is to complicated to include in this file
    gameInput:
		; Evaluates user input during game sequence
        ;	CURRENT SUPPORTED INPUTS
		;	Q=>quit | W=>up | S=>down | A=>left | D=>right
        call draw_character
        MOV AH,01h
        INT 16h    		; INT 16h, AH = 01h | Keyboard Status
        jnz gameInput	; IF ZF is zero there was a keypress
        ;ret        		; IF there is no key press return
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
		jmp inputFin ; If no jmp has been executed then given input is invalid so skip the movement section

        character_up:
			call checkUp				; Checks if Wall is above it 
            CMP AL,02h					; Checks if Color Returned is Green
            je inputFin					; if the character will be in a wall moving up
            ADD [square_start_y],BX		; Adds the negative move distance from checkUp to the start y
            ADD [square_final_y],BX		; Adds negative move distance to final y
			MOV [square_movement],1		; 1 will move square up
        jmp inputFin
        character_down:
			call checkDown				; Checks if Wall is a single pixel below it
            CMP AL,02h					; Checks if Color Returned is Green
            je inputFin					; if the character will be in a wall moving down
            ADD [square_start_y],BX		; Adds move distance from checkDown to start y 
            ADD [square_final_y],BX		; Adds move distance to final y
			MOV [square_movement],2		; 2 will move square down
        jmp inputFin
        character_right:	
			call checkRight				; Checks if Wall is a single pixel to the right
			CMP AL,02h					; Checks if Color Returned is Green
            je inputFin					; if the character will be in a wall moving right
            ADD [square_start_x],BX		; Adds move distance from checkRight to start x
            ADD [square_final_x],BX		; Adds move distance to final x
			MOV [square_movement],3		; 3 will move square right
        jmp inputFin
        character_left:
			call checkLeft				; Checks if Wall is a single pixel to the left
            CMP AL,02h					; Checks if Color Returned is Green, Otherwise it will be the distance moved
            je inputFin					; if the character will be in a wall moving left
            ADD [square_start_x],BX		; Adds negative move distance from checkLeft to start x 
            ADD [square_final_x],BX		; Adds negative move distance to final x
			MOV [square_movement],4		; 4 will move square left
        jmp inputFin
        pause_screen:
            mov currentScreen, 5
            ret
        jmp inputFin
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
	 	mov ax, @data
        mov es,ax
        mov ah,13h
        mov al, 1; update cursor position
        mov bh, 7 ; page #
        mov bl, 0ah; text attribute (color)
		
		mov cx, 35
        mov dh, 8
        mov dl, 3
        mov bp, offset[tip_message]
        int 10h 

        mov cx, 12
        mov dh, 16
        mov dl, 14
        mov bp, offset[waiting_message]
        int 10h  
		
		mov cx, 4;string length
        mov dh, 18;row 0 - 24     \40x25
        mov dl, 18;column 0 - 39 /
        mov bp, offset [waiting]
        int 10h
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
        mov ax, ds
        mov es,ax
        mov ah,13h
        mov al, 1; update cursor position
        mov bh, 5 ; page #
        mov bl, 0ah; text attribute (color)
        mov cx, 11;string length
        mov dh, 6;row 0 - 24     \40x25
        mov dl, 15;column 0 - 39 /
        mov bp, offset [paused]
        int 10h

        mov cx, 15
        mov dh, 10
        mov dl, 12
        mov bp, offset [resume]
        int 10h

        mov cx, 13
        mov dh, 12
        mov dl, 12
        mov bp, offset [save]
        int 10h

        mov cx, 17
        mov dh, 14
        mov dl, 12
        mov bp, offset [save_quit]
        int 10h

        mov cx, 15
        mov dh, 16
        mov dl, 12
        mov bp, offset [to_title]
        int 10h
    ret
    pausedInput:
        MOV AH,01h
        INT 16h    		; INT 16h, AH = 01h | Keyboard Status
        jz pausedInput	; IF ZF is zero there was a keypress
                 		; IF there is no key press return
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
            mov currentScreen,0
            ;call setGameScreen
        ret
        ;jmp inputFinPause
        
		saveGame:
			call saveFile
		ret
        ;jmp inputFinPause
        
		quitYesSave:
			call saveFile
			call terminate
		;jmp inputFinPause
        
		quitNoSave:
            mov currentScreen, 3
            ret
            ;jmp programStart
            ;call terminate
	        ;call setTitleScreen
	        ;call main_screen_input 
        ;jmp inputFinPause

        inputFinPause:
        jmp pausedInput
    ret