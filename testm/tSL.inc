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
	
	mov fileHandler, ax ;Preserve the file handler.
		
	;saves current room to file.
	mov ah, 40h
	mov bx, fileHandler
	mov cx, 2 ;Number of bytes to write
	mov dx, offset currentRoom
	int 21h

	;saves current x start location of the character
	mov ah, 40h
	mov bx, fileHandler
	mov cx, 2 ;Number of bytes to write
	mov dx, offset square_start_x
	int 21h

	
	;saves current x final location of the character
	mov ah, 40h
	mov bx, fileHandler
	mov cx, 2 ;Number of bytes to write
	mov dx, offset square_final_x
	int 21h
	
	
	;saves current y start location of the character.
	mov ah, 40h
	mov bx, fileHandler
	mov cx, 2 ;Number of bytes to write
	mov dx, offset square_start_y
	int 21h

	
	;saves current y final location of the character.
	mov ah, 40h
	mov bx, fileHandler
	mov cx, 2 ;Number of bytes to write
	mov dx, offset square_final_y
	int 21h
	
	mov ah, 40h
	mov bx, fileHandler
	mov cx, 1 ;Number of bytes to write
	mov dx, offset redKey
	int 21h
	
	mov ah, 40h
	mov bx, fileHandler
	mov cx, 1 ;Number of bytes to write
	mov dx, offset cyanKey
	int 21h
	
	mov ah, 40h
	mov bx, fileHandler
	mov cx, 1 ;Number of bytes to write
	mov dx, offset yellowKey
	int 21h
	
	mov ah, 40h
	mov bx, fileHandler
	mov cx, 1 ;Number of bytes to write
	mov dx, offset redDoor
	int 21h
	
	mov ah, 40h
	mov bx, fileHandler
	mov cx, 1 ;Number of bytes to write
	mov dx, offset cyanDoor
	int 21h
	
	mov ah, 40h
	mov bx, fileHandler
	mov cx, 1 ;Number of bytes to write
	mov dx, offset yellowDoor
	int 21h

	mov ah, 40h
	mov bx, fileHandler
	mov cx, 2 ;Number of bytes to write
	mov dx, offset newline
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
	mov fileHandler, ax ;Preserves file handler
	
	;Reads the file two bytes at a time and stores data in current room.
	mov ah,3fh
	mov bx,[fileHandler]
	mov cx, 2
	lea dx, currentRoom
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_start_x.
	mov ah,3fh
	mov bx,[fileHandler]
	mov cx, 2
	lea dx, square_start_x
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_final_x.
	mov ah,3fh
	mov bx,[fileHandler]
	mov cx, 2
	lea dx, square_final_x
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_start_y.
	mov ah,3fh
	mov bx,[fileHandler]
	mov cx, 2
	lea dx, square_start_y
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_final_y.
	mov ah,3fh
	mov bx,[fileHandler]
	mov cx, 2
	lea dx, square_final_y
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_final_y.
	mov ah,3fh
	mov bx,[fileHandler]
	mov cx, 1		; Keys and rooms only 1 byte
	lea dx, redKey
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_final_y.
	mov ah,3fh
	mov bx,[fileHandler]
	mov cx, 1	; Keys and rooms only 1 byte
	lea dx, cyanKey
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_final_y.
	mov ah,3fh
	mov bx,[fileHandler]
	mov cx, 1	; Keys and rooms only 1 byte
	lea dx, yellowKey
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_final_y.
	mov ah,3fh
	mov bx,[fileHandler]
	mov cx, 1		; Keys and rooms only 1 byte
	lea dx, redDoor
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_final_y.
	mov ah,3fh
	mov bx,[fileHandler]
	mov cx, 1		; Keys and rooms only 1 byte
	lea dx, cyanDoor
	int 21h
	
	;Reads the file two bytes at a time and stores data in square_final_y.
	mov ah,3fh
	mov bx,[fileHandler]
	mov cx, 1		; Keys and rooms only 1 byte
	lea dx, yellowDoor
	int 21h

	call closeFile		
RET

closeFile:;closes the file
	mov ah, 3eh
	mov bx, [fileHandler]
	int 21h
RET
