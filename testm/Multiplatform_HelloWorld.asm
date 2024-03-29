include \SrcALL\V1_Header.asm

	call ScreenInit
	
	call domonitor
	
	
	ifdef BuildDOS
		push ds
		push es
			mov bh,6	;X
			mov bl,11	;Y
			call getscreenpos
			ifdef DosCGA
				mov ch,12	;Width
			endif
			ifdef DosEGA
				mov ch,6	;Width
			endif
			ifdef DosVGA
				mov ch,48	;Width
			endif
			mov cl,48		;Height
			
			mov ax, @code
			mov ds, ax
			mov si,offset BitmapTest ;DS:SI = Source Bitmap
DrawBitmap_Yagain:
			push di
			push cx
DrawBitmap_Xagain:				
				ifdef DosEGA
					mov ax,0102h	;plane 0 (0102h)
					mov dx,03c4h		
					out dx,ax		;Apply plane mask
					movsb			;DS:SI -> ES:DI
					dec di			;Reset Dest Ram
					
					mov ah,2h		;plane 1 (0202h)
					out dx,ax		;Apply plane mask
					movsb			;DS:SI -> ES:DI
					dec di			;Reset Dest Ram
					
					mov ah,4h		;plane 2 (0402h)
					out dx,ax		;Apply plane mask
					movsb			;DS:SI -> ES:DI
					dec di			;Reset Dest Ram
					
					mov ah,8h		;plane 3 (0802h)
					out dx,ax		;Apply plane mask
					movsb			;DS:SI -> ES:DI
				endif
				ifdef DosCGA
					movsb			;DS:SI -> ES:DI
				endif
				ifdef DosVGA
					movsb				;DS:SI -> ES:DI
				endif
				dec ch
				jnz DrawBitmap_Xagain
			pop cx
			pop di
			call GetScreenNextLine
			inc bl
			dec cl
			jnz DrawBitmap_Yagain
		pop es
		pop ds
	endif
		  
	ifdef BuildWSW
		mov ax,@code			;Segment of ROM (for source data)
		mov ds,ax
		
		mov cx,BitmapTestEnd-BitmapTest	;Data Length
		mov ax,offset BitmapTest		;Address of source data
		mov SI,ax
		
		mov ax,128						;First DEST tile
		;call DefineTiles				;Transfer the patterns to VRAM
		
		mov bh,6		;Xpos
		mov bl,6		;Ypos
		
		mov ch,6		;Width
		mov cl,6		;Height
		
		mov ax,0		;First Tile
		call FillAreaWithTiles		;Draw Tiles to screen
	endif
	
	mov ax,@code
	mov ds,ax
	mov si,offset palette
	xor ax,ax
	
Paletteagain:
	mov dx,[ds:si]
	call SetPalette ;Set Color AL to DX (-GRB)
	inc si			;Move down two bytes
	inc si
	inc ax
	cmp ax,16		;Are we done?
	jnz Paletteagain
	
	jmp $
	
	
include \SrcAll\V1_BitmapMemory.asm
include \SrcAll\V1_Monitor.asm	

BitmapFont:
	incbin "\resALL\Font96.FNT"
BitmapFontEnd:
	
BitmapTest:	
	
	ifdef BuildDOS
		ifdef DosCGA
			incbin "\resALL\SpriteTestCGA.RAW"
		endif
		ifdef DosEGA
			incbin "\resALL\SpriteTestEGA.RAW"
		endif
		ifdef DosVGA
			incbin "\resALL\SpriteTestVGA.RAW"
		endif
	endif
	ifdef BuildWSC
		incbin "\resALL\SpriteTestWSW4BppPlanar.RAW"
		;incbin "\resALL\SpriteTestWSW4BppLinear.RAW"
		
	endif
	ifdef BuildWSW
		incbin "\resALL\SpriteTestWSW2BppPlanar.RAW"
		;incbin "\resALL\SpriteTestWSW2BppLinear.RAW"		;Doesn't seem like Packed on BW wonderswan works!
	endif
BitmapTestEnd:


Palette:
	dw 0250h;0  -GRB
	dw 0000h;1  -GRB
	dw 0555h;2  -GRB
	dw 0AAAh;3  -GRB
	dw 0FFFh;4  -GRB
	dw 0826h;5  -GRB
	dw 0D33h;6  -GRB
	dw 03E3h;7  -GRB
	dw 07E6h;8  -GRB
	dw 0AE5h;9  -GRB
	dw 0FF4h;10  -GRB
	dw 02AAh;11  -GRB
	dw 00FFh;12  -GRB
	dw 030Dh;13  -GRB
	dw 063Bh;14  -GRB
	dw 0D0Fh;15  -GRB
	
include \SrcALL\V1_Footer.asm


