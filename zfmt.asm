; zfmt: Format a disk on a system with no OS.

        ; 8080 assembler code
        .hexfile zfmt.hex
        .binfile zfmt.bin
        ; try "hex" for downloading in hex format
        .download bin  
	.org 0000

stack_ptr		equ 0ff00h

zfdc_data_port		equ 010h
zfdc_stat_port		equ 011h
zfdc_reset_port		equ 013h

cmd_reset_zfdc		equ 03h
cmd_set_format		equ 04h
cmd_set_home		equ 0ah
cmd_handshake		equ 021h
cmd_format_disk		equ 016h
cmd_confirm_fmt		equ 032h

no_errors_flag		equ 00h

direction_mask		equ 080h
data_out_rdy_mask	equ 002h
data_in_rdy_mask 	equ 001h

drive_num		equ 00h		; drive we want to format
disk_fmt		equ 01h		; format to use for the disk
start_track		equ 0		; start formatting here
end_track		equ 76		; finish here

start:
	lxi sp, stack_ptr	; init the stack
	
zfdc_boot:
	out zfdc_reset_port	; reset the zfdc board

	lxi bc, 0
wait_d:				; delay for ~0.5 sec
	dcr b
	jnz wait_d
	dcr b
	dcr c
	jnz wait_d

	in zfdc_data_port	; make sure board exists
	cpi cmd_handshake	; make sure we get the handshake byte
	jnz err_nr

	mvi a,cmd_handshake	; clear any zfdc ints
	out zfdc_data_port
	call wait_for_ack
	ora a			; a should be zero if no error
	jnz err_nr

	mvi c,cmd_set_format	; send set format command
	call s100out
	mvi c,drive_num	; tell the board which drive we want
	call s100out
	mvi c,disk_fmt
	call s100out
	call wait_for_ack
	jnz err_nr		; bail if the board has an error

	mvi b,start_track
next_track:
	mvi c,cmd_format_disk	; time to actually format this thing
	call s100out
	mov c,b
	call s100out
	mvi c,cmd_confirm_fmt
	call s100out

wait_f:
	call s100stat		; check to see if track format finished
	jnz track_done		; yay, it did
	jmp wait_f		; nope, gotta keep waiting

track_done:
	call s100in
	cpi no_errors_flag
	jnz err_nr		; if error, abort

	mov a,b
	cpi end_track		; are we at the last track?
	jz format_done
	inr b			; increment track number
	jmp next_track
	
format_done:
	mvi c,cmd_set_home
	call s100out
	call wait_for_ack
	out zfdc_reset_port
	mvi a,0ffh
	out 0ffh		; show no error
	hlt			; nowhere to return to, so we'll halt

err_nr:
	out 0ffh		; show the error to the user
	hlt			; bail

s100out:
	in zfdc_stat_port	; check to see if fdc is ready for a byte
	mov d,a
	ani direction_mask	; wait for direction to be IN
	jnz s100out

	mov a,d
	ani data_out_rdy_mask	; is there already data waiting?
	jz s100out		; loop if so

	mov a,c
	out zfdc_data_port	; send the byte
	ret

s100stat:
	in zfdc_stat_port
	ani data_in_rdy_mask	; any bytes waiting for us?
	rz			; return if not
	xra a
	dcr a
	ret			; return with a=ff if so

s100in:
	in zfdc_stat_port
	mov d,a
	ani direction_mask	; wait if direction is IN
	jz s100in

	mov a,d
	ani data_in_rdy_mask	; wait for board to have a byte for us
	jz s100in
	in zfdc_data_port	; read the byte
	ret			; return with the byte in a

wait_for_ack:
	push bc
	lxi b,0
wait_1:
	in zfdc_stat_port
	ani direction_mask	; is zfdc in input mode?
	jz wait_2		; yep, it is
	call s100stat		; wait for zfdc to send something
	jz wait_2
	call s100in		; get the byte
	cpi no_errors_flag	; is it an error?
	pop bc
	ret			; will return z if no problem
wait_2:
	dcr b
	jnz wait_1		; inner loop
	dcr b
	dcr c
	jnz wait_1		; outer loop

	xra a			; timed out, return ff
	dcr a
	pop bc
	ret
