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
		mov cx,86		; CX to set start col
		mov dx,0		; DX to set start row
		mov ax,148		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,0		; CX to set start col
		mov dx,50		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,100		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls 
		
		cmp yellowKey,1
		je yellowKeyObtained
		mov keyStartY,95
		mov keyStartX,157
		mov keyColor, 0Eh	; Draw Yellow key 
		call drawKey
		yellowKeyObtained:
		
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
		mov cx,86		; CX to set start col
		mov dx,0		; DX to set start row
		mov ax,148		; AX to set length of column size
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
		mov cx,86		; CX to set start col
		mov dx,0		; DX to set start row
		mov ax,148		; AX to set length of column size
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
		mov cx,86		; CX to set start col
		mov dx,0		; DX to set start row
		mov ax,148		; AX to set length of column size
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
		
		;Top Middle	
		mov cx,86		; CX to set start col
		mov dx,0		; DX to set start row
		mov ax,148		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		mov cx,300		; CX to set start col
		mov dx,50		; DX to set start row
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
	
	room6:
		;Start Jack's Smile Room Code		
	;Eyes
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
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
		
		mov cx,0		; CX to set start col
		mov dx,50		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,100		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		cmp redDoor,1
		je redDoorUnlocked
		MOV doorColor, 0Ch
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
		
		mov cx,300		; CX to set start col
		mov dx,50		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,100		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		cmp redKey,1
		je redKeyObtained
		mov keyStartY,95
		mov keyStartX,157
		mov keyColor, 0Ch	; Draw Red key 
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
		mov dx,0		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,40		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		;short top right
		mov cx,200		; CX to set start col
		mov dx,0		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,40		; BX to set length of row size
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
		
		mov cx,0		; CX to set start col
		mov dx,50		; DX to set start row
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
		
	
	room12:
	;Tilted Pillars
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
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
		
		mov cx,300		; CX to set start col
		mov dx,50		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,100		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		cmp cyanDoor,1
		je cyanDoorUnlocked
		MOV doorColor, 0Bh	; 0Bh for cyan door
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
		
		mov cx,0		; CX to set start col
		mov dx,50		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,100		; BX to set length of row size
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
		
		mov cx,300		; CX to set start col
		mov dx,50		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,100		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		cmp cyanKey,1
		je cyanKeyObtained
		mov keyStartY,95
		mov keyStartX,157
		mov keyColor, 0Bh	; Draw Cyan key 
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
		mov dx,0		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,40		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		;short top right
		mov cx,200		; CX to set start col
		mov dx,0		; DX to set start row
		mov ax,15		; AX to set length of column size
		mov bx,40		; BX to set length of row size
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
		
		mov cx,0		; CX to set start col
		mov dx,50		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,100		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls
		
		cmp yellowDoor,1
		je yellowDoorUnlocked
		MOV doorColor, 0Eh
		MOV CX,132
		MOV DX,185
		MOV SI,0
		MOV DI,0
		call drawTopOrBottomDoor
		yellowDoorUnlocked:
		
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
	
	
	room23:
		;Empty room
		push AX
		push BX
		push CX
		push DX
		push si
		push di
		
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
		mov cx,86		; CX to set start col
		mov dx,185		; DX to set start row
		mov ax,148		; AX to set length of column size
		mov bx,15		; BX to set length of row size
		call wallIt
		call drawWalls
		
		mov cx,300		; CX to set start col
		mov dx,50		; DX to set start row
		mov ax,20		; AX to set length of column size
		mov bx,100		; BX to set length of row size
		call wallIt	; Must put updateWallStart before drawWalls
		call drawWalls

		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
	RET