; Original, code written by Leonardo Ono (ono.leo@gmail.com) on 22/10/2018
; Source: https://github.com/leonardo-ono/Assembly8086PlayMultipleSoundsSimultaneouslyTest	
; Additional annotations and minor code modifications by Jack Byers.
	bits 16 ;denotes 16-bit compilation
	
	;file include
	%include "sound.inc"
	
	;sets up global calls
	global start_fast_clock
	global stop_fast_clock
	global install_timer_handler
	global uninstall_timer_handler
	
segment timer

	; count = 1193180 / sampling_rate
	; sampling_rate = 4000 cycles per second
	; count = 1193180 / 4000 = 298 (in decimal) = 12a (in hex) 
	; 12ah = 4000 Hz (wav pcm sampling rate)
	start_fast_clock:
			cli				;Clears interrupt
					
			mov al, 2ah 	; -
			out 40h, al		; | Sets transfer rate to 12ah
			mov al, 1h		; | using the 40h command (set digitized sound transfer time constant).
			out 40h, al		; -

			sti				;sets interrupt
			retf			;returns far call to other segment. 

	stop_fast_clock:
			cli				;Clears interrupt
				
			mov al, 0h		; -
			out 40h, al		; | Sets transfer rate to 0 
			mov al, 0h		; |	using the 40h command (set digitized sound transfer time constant)
			out 40h, al		; _
			
			sti				;sets interrupt
			retf			;returns far call to other segment.
			
	install_timer_handler:
			cli    								;Clears interrupt
			mov ax, data						;Moves data to ax
			mov ds, ax							;Moves ax to data segment 
			mov ax, 0							;Clears ax
			mov es, ax							;Moves ax to extra segment.
			mov ax, [es:4 * 8 + 2]				;Moves an effective address in the extra segment to ax.
			mov [ds:int8_original_segment], ax  ;Moves ax to an effective address in the data segment. 
			mov ax, [es:4 * 8]					;Moves an effective address in the extra segment to ax.
			mov [ds:int8_original_offset], ax	;Moves ax to an effective address in the data segment. 
			mov word [es:4 * 8 + 2], timer		;moves the timer segment into an effective address in the extra segment.
			mov word [es:4 * 8], timer_handler  ;moves the timer_handler into an effective address in the extra segment.  
			sti									;sets interrupt flag	
			retf								;returns far call to another segment.
			
	uninstall_timer_handler:
			cli									;Clears interrupt
			mov ax, data						;Move data to ax
			mov ds, ax							;Moves ax to data segment 
			mov ax, 0							;Clears ax
			mov es, ax							;Moves ax to extra segment.
			mov ax, [ds:int8_original_offset]	;Moves effective address of the original offset in the data segment to ax.	
			mov [es:4 * 8], ax					;Moves AX into an effective address in the extra segment.
			mov ax, [ds:int8_original_segment]	;Move the effective address of the original segment in the data segment to ax.
			mov [es:4 * 8 + 2], ax				;Moves ax into an effective address of the extra segment. 
			sti									;sets interrupt
			retf								;returns far call to other segment.
			
	timer_handler:
			push ds				;pushes ds on to the stack.
			pusha				;NASM command used in order to handle 32-bit in 16-bit compilations.
				
			call far play_mix	;Call play_mix to play the sound mix.
			
			mov al, 20h			; 20h = 8-bit stereo  unsigned PCM playback
			out 20h, al			; sends command to DSP port for 8Bit direct mode digitized sound
			
			popa				;NASM command used in order to handle 32-bit in 16-bit compilations.
			pop ds				;pops ds off of the stack.
			iret  				;Interrupt return
			

section data						;data section
	int8_original_offset	dw 0	;variable that stores the original offset.
	int8_original_segment	dw 0	;variable that stores the original segment. 
