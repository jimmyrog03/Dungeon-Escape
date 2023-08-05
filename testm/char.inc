    draw_character:
        CMP [square_movement],0
        je no_movement 	; 0 indicates no movement so skip drawing the character
		
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		call checkNewSquare
		MOV CX, [square_start_x]	; Need this for any movement to start drawing square at start_x
        MOV DX, [square_start_y]	; Need this for any movement to start drawing square at start_y
		call drawNewSquare
		CMP [square_movement],1
		je movingUp				;If character needs to be drawn moving upwards
		CMP [square_movement],2
		je movingDown			; If character needs to be drawn moving downwards
		CMP [square_movement],3
		je movingRight			; If character needs to be drawn moving right
		CMP [square_movement],4
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
        MOV [square_movement],0 ; set back to zero because character has now moved
		call checkRoomChange
        no_movement:
    ret
	
	checkNewSquare:
		MOV AH,0Dh					; Read graphics Pixel 
		MOV BH,00h					; Read from page 0 as this has keys
		INT 10h
		call checkKeyColors
		cmp AL,0Fh
		je keyErased
		INC CX
        CMP CX, [square_final_x]
        JNA checkNewSquare
        MOV CX, [square_start_x]
        INC DX
        CMP DX, [square_final_y]
        JNA checkNewSquare
		keyErased:
	RET
	
	drawNewSquare:					; Draws the character in its new position no matter the direction moved
		MOV AH, 0Ch				; Draw character pixel
        MOV AL, 09h				; Color to Blue
        INT 10h
		INC CX
        CMP CX, [square_final_x]
        JNA drawNewSquare
        MOV CX, [square_start_x]
        INC DX
        CMP DX, [square_final_y]
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
        CMP SI,actualMoveDistance	; Compare the amount of rows removed by the move distance
        JNE drawUp					; Continue removing rows until move distance reached
	ret
	
	drawDown:	
		; Removes trail when drawing down
		SUB DX,squareSize
		SUB DX,actualMoveDistance
		SUB DX,1
		loopDown:
		MOV AH,0Dh		; Read graphics Pixel 
		MOV BH,01h		; Read from page 1 since this only has tiles
		INT 10h
        MOV AH, 0Ch		; Draw pixel read from page 1 (tiles)
        MOV BH, 00h		; Draw pixels on page 0 (has character)
        INT 10h
        INC CX			; Inc the column (x value) of square
        CMP CX, [square_final_x]	; Compare column to the end of the square
        JNG loopDown				; Jump if not greater to continue looping
        MOV CX, [square_start_x]	; Move CX with start x position
        INC DX						; Inc the row (y value) of the square
		INC SI						; Inc row counter (SI) 
        CMP SI,actualMoveDistance	;Compare amount of rows draw to the actual move distance
        JNE loopDown				;If number of rows drawn is not equal to move distance, keep looping
	ret
	
	drawRight:	
		; Removes trail when drawing right
		MOV DX,square_start_y		; Move start y to DX
		SUB CX, actualMoveDistance	; Subtract start x with move distance
		loopRight:
		MOV AH,0Dh		; Read graphics Pixel 
		MOV BH,01h		; Read from page 1 since this only has tiles
		INT 10h
        MOV AH, 0Ch		; Draw pixel read from page 1
        MOV BH, 00h		; Draw pixels on page 0 (has character)
        INT 10h
        INC CX			; Inc the col (x value) of square
		INC SI			; Increment column counter
        CMP SI, actualMoveDistance	; Compare Column counter with the move distance
        JNE loopRight				; Continue looping until all columns have been drawn 
		MOV si,0		; Reset column counter
        MOV CX, [square_start_x]	; Move start x back to CX
		SUB CX, actualMoveDistance		; Subtract CX by move distance
        INC DX							; Inc row (y value) of square
        CMP DX, [square_final_y]		; Compare row with the final y
        JNG loopRight
		
	ret
	
	drawLeft:	
		; Removes trail when drawing left
		MOV DX,[square_start_y]
		ADD CX,squareSize
		ADD CX,1
		loopLeft:
		MOV AH,0Dh					; Read graphics Pixel 
		MOV BH,01h					; Read from page 1 since this only has tiles
		INT 10h
        MOV AH, 0Ch					; Draw pixel
        MOV BH, 00h					; Draw pixels on page 0 (has character)
        INT 10h
        INC CX
		INC SI
        CMP SI, actualMoveDistance
        JNE loopLeft				
		MOV si,0
        MOV CX, [square_start_x]
		ADD CX, squareSize
		ADD CX,1
        INC DX
        CMP DX, [square_final_y]
        JNG loopLeft
	ret
	
	checkRight:
		MOV CX,0000h
		ADD CX,[moveDistance]
		MOV [actualMoveDistance],CX
		minimizeRight:
        MOV CX,[square_final_x]		; Moving right so final or rightmost column
        ADD CX,[actualMoveDistance]	; Add movement to get new position of rightmost column
		CMP CX,319					; Compare the final_x + move distance to CX
		JG noRightCollision			; If its greater, assume there is no collision as it must be at the right door
		MOV DX,[square_start_y]		; Sets y to the top row
		MOV AH,0Dh					; Read Pixel at this position
		MOV BH,00h					; Read from page 0
		INT 10h						; INT 10h, AH = 0Dh, BH = Page Number, CX = x, DX = y | Read Graphics Pixel
		CMP AL,02h					; Checks to see if the pixel was green
		je rightCollision			; If Green, then collision 			
		call checkKeyColors		; Compare the color read to keys
		cmp AL, 02h
		je rightCollision
		
		MOV DX,[square_final_y]		; Sets y to the bottom row
		MOV AH,0Dh					
		MOV BH,00h					
		INT 10h						; INT 10h, AH = 0Dh, BH = Page Number, CX = x, DX = y | Read Graphics Pixel
		CMP AL,02h					; Checks to see if the pixel was green
		je rightCollision
		call checkKeyColors		; Compare the color read to keys
		cmp AL, 02h
		je rightCollision
		jmp noRightCollision

		rightCollision:
			dec actualMoveDistance
			CMP actualMoveDistance,0
			je collisionRight
			jmp minimizeRight

		noRightCollision:
		MOV BX,actualMoveDistance

		collisionRight:
	ret
	
	checkLeft:
		MOV CX,0000h
		ADD CX,[moveDistance]
		MOV [actualMoveDistance],CX
		minimizeLeft:
        MOV CX,[square_start_x]		; Moving left so start or leftmost column
        SUB CX,[actualMoveDistance]	; Add movement to get new position of leftmost column
		MOV DX,[square_start_y]		; Sets y to the top row
		MOV AH,0Dh					
		MOV BH,00h					
		INT 10h						; INT 10h, AH = 0Dh, BH = Page Number, CX = x, DX = y | Read Graphics Pixel
		CMP AL,02h					; Checks to see if the pixel was green
		je leftCollision
		call checkKeyColors
		cmp AL, 02h
		je leftCollision
		
		MOV DX,[square_final_y]		; Sets y to the bottom row
		MOV AH,0Dh
		MOV BH,00h
		INT 10h						; INT 10h, AH = 0Dh, BH = Page Number, CX = x, DX = y | Read Graphics Pixel
		CMP AL,02h					; Checks to see if the pixel was green
		je leftCollision
		call checkKeyColors
		cmp AL, 02h
		je leftCollision
		
		jmp noLeftCollision
		leftCollision:
			dec actualMoveDistance
			CMP actualMoveDistance,0
			je collisionLeft
			jmp minimizeLeft
		noLeftCollision:
		MOV BX,actualMoveDistance
		NEG BX
		collisionLeft:
	ret
	
	checkUp:
		MOV DX,0000h
		ADD DX,[moveDistance]
		MOV [actualMoveDistance],DX
		minimizeUp:
        MOV CX,[square_start_x]		; Checks pixel to the right of top corner
		MOV DX,[square_start_y]		; Sets y to the top corner
		SUB DX,[actualMoveDistance]	; Checks the pixel above to make sure not Green
		MOV AH,0Dh					; Read graphics Pixel 
		MOV BH,00h					; Read from page 0 as this has walls drawn
		INT 10h
		call checkKeyColors
		cmp AL, 02h
		je upCollision
		
		MOV CX,[square_final_x]		; Checks pixel to the right of top corner
		MOV AH,0Dh					; Read graphics Pixel 
		MOV BH,00h					; Read from page 0 as this has walls drawn
		INT 10h
		call checkKeyColors
		cmp AL, 02h
		je upCollision
		
		
		jmp noUpCollision
		upCollision:
			dec actualMoveDistance
			CMP actualMoveDistance,0
			je collisionUp
			jmp minimizeUp
		noUpCollision:
		MOV BX,actualMoveDistance
		NEG BX
		collisionUp:
	ret
	
	checkDown:
		MOV DX,0000h
		ADD DX,[moveDistance]
		MOV [actualMoveDistance],DX
		minimizeDown:
        MOV CX,[square_start_x]		; Checks pixel below bottom right
		MOV DX,[square_final_y]		; Sets y to the right corner
		ADD DX,[actualMoveDistance]	; Add actualMoveDistance to final y value
		MOV AH,0Dh					; Read graphics Pixel 
		MOV BH,00h					; Read from page 0 as this has walls drawn
		INT 10h
		call checkKeyColors
		cmp AL, 02h
		je downCollision
		
		MOV CX,[square_final_x]		; Checks pixel to the right of top corner
		MOV AH,0Dh					; Read graphics Pixel 
		MOV BH,00h					; Read from page 0 as this has walls drawn
		INT 10h
		call checkKeyColors
		cmp AL, 02h
		je downCollision
		
		jmp noDownCollision
		downCollision:
			dec actualMoveDistance
			CMP actualMoveDistance,0
			je collisionDown
			jmp minimizeDown
		noDownCollision:
		MOV BX,actualMoveDistance
		collisionDown:
	ret
	
	checkKeyColors:
	PUSH SI
	PUSH DI
	MOV SI,0
	MOV DI,0
		checkCyan:
			CMP AL,0Bh				; See if cyan pixel
			jne checkRed			; If key isn't cyan, check if red
			CMP square_start_y, 30	; See if the cyan pixel is at the top door
			JG getCyanKey
			CMP square_final_x, 190
			JL checkCyanDoor		; If it is at the top, jmp to checkCyanDoor to see if player has the cyan key
			jmp notKey
			getCyanKey:
			INC cyanKey				; If cyanKey = 1, cyan key obtained
			call eraseKey
			mov keyColor, 0Bh
			mov keyStartY,2
			mov keyStartX,280
			jmp isKey					
		checkCyanDoor:			; Check if cyan key obtained to remove cyan door
			CMP cyanKey,1			
			je removeCyan		; If cyanKey is 1, jmp to removeCyan to remove the cyan door
			MOV AL,02h			; If cyanKey 0, pretend that door is a wall by moving 02h to AL
			jmp notKey			; Jump to notKey since there are no keys at this point
			removeCyan:
			call removeTopDoor	; The top door can be removed since the correct key was acquired
			inc cyanDoor
			jmp notKey			; return from keyCheck by jmp to notKey
		checkRed:
			CMP AL,0Ch				; See if red pixel 
			jne checkYellow
			CMP square_start_y, 30
			JL checkRedDoor
			INC redKey 	; IF redKey = 1, red key obtained
			call eraseKey
			mov keyColor, 0Ch
			mov keyStartY,2
			mov keyStartX,295
			jmp isKey
		checkRedDoor:			; Check if red key obtained to remove red door
			CMP redKey,1			
			je removeRed			; If redKey is 1, jmp to removeRed to remove the red door
			MOV AL,02h
			jmp notKey
			removeRed:
			call removeTopDoor
			inc redDoor
			jmp notKey
		checkYellow:
			CMP AL,0Eh
			jne notKey
			CMP square_final_y, 170
			JG checkYellowDoor
			INC yellowKey
			call eraseKey
			mov keyColor, 0Eh
			mov keyStartY,2
			mov keyStartX,310
			jmp isKey
		checkYellowDoor:
			CMP yellowKey,1			
			je removeYellow			; If yellowKey is 1, jmp to removeYellow to remove the yellow door
			MOV AL,02h
			jmp notKey
			removeYellow:
			call removeBottomDoor
			inc yellowDoor
			jmp notKey
			
		isKey:
		call drawKey
		MOV AL,0Fh	; This is used by the drawSquare routine to determine if there was a key
		
		notKey:
		POP DI
		POP SI 
	RET
	
;Checks for lava and coins 
	checkOtherColors:
	
	
	RET