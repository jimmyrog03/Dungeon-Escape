; Original, code written by Leonardo Ono (ono.leo@gmail.com) on 22/10/2018
; Source: https://github.com/leonardo-ono/Assembly8086PlayMultipleSoundsSimultaneouslyTest	
;
; Additional annotations and code modifications by Jack Byers.
; Additional code by Walt Sandmeier.

	bits 16 ;denotes 16bit compilation. 
	
	;Global calls used by other files.
	global play_mix
	global enable_sound
	global enable_loop
	global stop_loop
	global active_sounds
	
segment sound
			
	play_mix:
		;pushes data segment, extra segment, source index, and destination index.
			push ds
			push es
			push si
			push di
			
			mov ax, active_sounds_segment ;moves the active_sounds segment to ax.
			mov es, ax 					  ;moves ax to the extra segment.
			
			mov dx, 127 ; --> result mixed sample; Used to calculate mixed sample.
			
			mov si, 0	;clears source index.
			mov cl, 0	;clears cl.
			
		.next_sound:

			mov al, [es:active_sounds + si] 	;sound s (status)
			cmp al, 0							;checks if al == 0
			jz .continue						;if true, jumps to continue. else continues on to mix the sound files.
			
			; mixes sound samples
			mov bx, [es:active_sounds + si + 7] ;sound index
			mov ax, [es:active_sounds + si + 3] ;sound segment
			mov ds, ax							;moves ax to the data segment. 
			mov di, [es:active_sounds + si + 1] ;sound offset
			mov ah, 0							;sets ah to 0.
			mov al, [ds:di + bx] 				;get sound byte sample
			add dx, ax							;adds ax to dx. 
			sub dx, 127 						;mixed_sample = sample_a + sample_b - 127
						
			inc word [es:active_sounds + si + 7];increment sound index
			mov bx, [es:active_sounds + si + 7] ;sound index; Current index is moved to bx
			cmp bx, [es:active_sounds + si + 5] ;end of sound; Checks to see if there is more sound to play. 
			jb .continue						;jumps to continue if there is more sound to play. 	

			mov word [es:active_sounds + si + 7], 300 ; sound index; updates the sound index

			; is loop ? 						;checks to see if sound is looped.
			cmp byte [es:active_sounds + si], 2	;If sound playback is set to 2, then the sound will loop for continuous playback (see active_sounds_segment below)
			je .continue  						;jumps to continue to if the sound is looped.
			
			; end of sound
			mov byte [es:active_sounds + si], 0 ; sound s (status); otherwise playback ends. 
			
		.continue: 			;continues play back.
			add si, 9		;adds 9 to the source index
			inc cl			;increments lower counter (cl).
			cmp cl, 15		;compares cl to 15.
			jbe .next_sound	;if cl <= 15, then loops to the .next_sound call.
			
			; limit sample 0~255	;makes sure that the samples to write into memory they aren't supposed to. 
			cmp dx, 255				;compares dx to 255
			jbe .play_mixed_sample	;if dx <= 255, everything is OK and jumps to .play_mixed_sample
			mov dx, 255				;Else dx > 255 and needs to be corrected. DX is set to 255.
			
		.play_mixed_sample:
			mov bl, dl 		;moves sound byte sample from dl to bl.
			
			; send DSP Command 10h
			mov dx, 22ch	;DSP Command Port
			mov al, 10h		;8-bit direct mode digitized sound command. 
			out dx, al		;sends command to DSP.	

			; send byte audio sample
			mov al, bl		;move sound byte sample from bl to al
			out dx, al		;sends sound byte sample to DSP.
			
		.end: ;pops di, si, es, ds, and returns from call to far segment.
			pop di
			pop si
			pop es
			pop ds
			retf			

	; void enable_sound(int sound_index);
	enable_sound:
			push bp 						;pushes base pointer to the stack.
			mov bp, sp						;moves stack point to base pointer.
			push es							;pushes extra segment to the stack.
			
			mov ax, active_sounds_segment	;moves active_sounds_segment to ax.
			mov es, ax						;moves ax to extra segment.

			mov dx, 0						;clears dx
			mov bx, [bp + 6]				;moves effective address of base pointer + 6 to bx
			mov ax, 9						;moves 9 to ax
			mul bx							;multiplies bx by 9.
			mov bx, ax						;moves results to ax.
			mov word [es:active_sounds + bx + 7], 300 	; sound index
			mov byte [es:active_sounds + bx], 1 		; sound status 
			
			pop es							;pops es
			pop bp							;pops bp
			retf							;returns far call to other segment.
			
	enable_loop:; Created by Team
			;Performs exactly the same as enable sound except it sets the sound status to loop
			;Based on sound status definition in active_sounds_segment
			push bp
			mov bp, sp
			push es
			mov cl, 0
		
			mov ax, active_sounds_segment
			mov es, ax

			mov dx, 0
			mov bx, [bp + 6]
			mov ax, 9
			mul bx
			mov bx, ax
			mov word [es:active_sounds + bx + 7], 300 ; sound index
			mov byte [es:active_sounds + bx], 2 ; sound status 
			
			pop es
			pop bp
	retf
	
	stop_loop:; Created by Team
			;Performs exactly the same as enable sound except it sets the sound status to free stopping playback
			;Based on sound status definition in active_sounds_segment
			push bp
			mov bp, sp
			push es
			mov cl, 0
			
			mov ax, active_sounds_segment
			mov es, ax

			mov dx, 0
			mov bx, [bp + 6]
			mov ax, 9
			mul bx
			mov bx, ax
			mov word [es:active_sounds + bx + 7], 300 ; sound index
			mov byte [es:active_sounds + bx], 0 ; sound status 
			
			pop es
			pop bp
			retf
	

segment active_sounds_segment

	active_sounds:
		;  s -> 0=free, 1=playing, 2=loop
		;  s  low   high  size  index
		;  -  ----  ----  ----  ----
		db 0			; Necessary for smooth playback. Removal will cause interesting things to happen with sound. 
		dw sound_data	; Name of sound.
		dw music_sound	; Segment name that sound is stored in.
		dw 51029		; Size of sound in bytes. Must be =< 64KB or program will not compile.
		dw 0 			; 0 is pushed in the main program to call this sound.
		
		;db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 0
		;can be edited to set up additional sounds.
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 1
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 2
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 3
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 4
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 5
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 6
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 7
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 8
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 9
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 10
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 11
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 12
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 13
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 14
		db 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 15

		
segment music_sound ;music segment

	sound_data:
			incbin "kingsv.wav" ; 51.529 bytes, music file 
		

segment sound_effects ;not used due to playback issues.

	; sound_index dw 0


