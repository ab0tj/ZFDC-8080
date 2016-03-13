tor for ZFDC and IMSAI SIO

.hexfile zmon.hex
.binfile zmon.bin
.download bin

; MODIFY THESE VALUES BEFORE BURNING TO ROM
.org 0h ; 0 for testing, 0C000h for rom
startmem		equ 0
endmem			equ 0C000h

stack   equ 0BFF0h ; stack pointer, just below rom and some temp space

; i/o ports
uart_data               equ 02h         ; uart data port, emu=1 sio=2
uart_ctl                equ 03h         ; uart control port, emu=0 sio=3
uart_reset              equ 040h        ; reset byte
uart_mode               equ 04Eh        ; mode byte
uart_cmd_byte           equ 027h        ; control byte
zfdc_data_port          equ 010h
zfdc_stat_port          equ 011h
zfdc_reset_port         equ 013h

;zfdc commands
zcmd_reset              equ 03h
zcmd_set_format         equ 04h
zcmd_set_drive          equ 05h
zcmd_set_track		equ 07h
zcmd_set_side		equ 08h
zcmd_set_sector		equ 09h
zcmd_set_home           equ 0Ah
zcmd_seek_nv		equ 0Dh
zcmd_read_sector	equ 10h
zcmd_write_sector	equ 11h
zcmd_format_disk        equ 016h
zcmd_handshake          equ 021h
zcmd_get_sec_size	equ 025h
zcmd_confirm_fmt        equ 032h

;flags
no_errors_flag          equ 00h

; Bit Masks
uart_rx_rdy     equ 02h
uart_tx_rdy     equ 01h
zfdc_direction  equ 080h
zfdc_out_rdy    equ 02h
zfdc_in_rdy     equ 01h

; ascii
BS equ 08h
CR equ 0Dh
LF equ 0Ah
SPC equ 020h
CtrlZ equ 01Ah

; CODE STARTS HERE
;main program
        lxi sp, stack   ; initialize the stack pointer

; initialize hardware, show signon message
; init uart
        mvi a, 0              ; send dummy mode and command to uart
        out uart_ctl
        out uart_ctl
        out uart_ctl
        mvi a, uart_reset       ; reset the uart
        out uart_ctl
        mvi a, uart_mode        ; initialize the uart
        out uart_ctl
        mvi a, uart_cmd_byte
        out uart_ctl

        call uart_out_crlf      ; start on a fresh line

; init memory	MODIFY THIS FOR ROM VERSION
	lxi h, initmem_str
	call uart_out_str	; show message

	lxi h, end		; beginning of testable memory
	lxi d, endmem		; end of testable memory

initmem_loop_1:
	mvi a, 0ffh
	mov m, a	
	cmp m			; is this what we stored?
	jnz initmem_error	; that's not right
	inx h			; next memory address
	mov a, h
	cmp d
	jc initmem_loop_1
	mov a, l
	cmp e
	jc initmem_loop_1	; not at end of memory?
	lxi h, end		; we are; time for next pass

initmem_loop_2:
	xra a			; zero
	mov m, a
	cmp m			; is this what we stored?
	jnz initmem_error	; no...
	inx h			; next memory address
	mov a, h
	cmp d
	jc initmem_loop_2
	mov a, l
	cmp e
	jc initmem_loop_2	; not at end of memory?
	jmp initzfdc		; we are. move on.

initmem_error:
	lxi h, initmem_error_str
	call uart_out_str	; tell the user
	mov d, h		; high byte
	call uart_out_hex
	mov d, l		; low byte
	call uart_out_hex
	call uart_out_crlf

; init zfdc
initzfdc:
        out zfdc_reset_port     ; reset the zfdc board

        lxi b, 0
wait_d:                         ; delay for ~0.5 sec
        dcr b
        jnz wait_d
        dcr b
        dcr c
        jnz wait_d

        in zfdc_data_port       ; make sure board exists
        cpi zcmd_handshake    ; make sure we get the handshake byte
        jnz zfdc_init_error

        mvi a, zcmd_handshake   ; clear any zfdc ints
        out zfdc_data_port
        call zfdc_waitack
        ora a                   ; a should be zero if no error
        jnz zfdc_init_error

        mvi e, 0
        lxi h, drive_formats
init_set_fmt:                   ; set format for each drive
        mvi c, zcmd_set_format  ; send set format command
        call zfdc_out
        mov c, e               ; tell the board which drive we want
        call zfdc_out
        mov c, m                ; get format number
        call zfdc_out
        call zfdc_waitack
        jnz zfdc_init_error     ; bail if the board has an error
        inr e                   ; next drive
        mov a, e
        cpi 4
        jz zfdc_init_done
        inx h
        jmp init_set_fmt

zfdc_init_error:
        call zfdc_error
	call uart_out_crlf

zfdc_init_done:
        lxi h, signon_str       ; show signon
        call uart_out_str
        in uart_data            ; clear rx buffer

prompt:
	lxi sp, stack	; just in case there should have been a ret
        lxi h, prompt_str
        call uart_out_str
        call uart_in
        mov c, a
        push a
        call uart_out           ; echo
        call uart_out_crlf

        pop a
        cpi 'm'                 ; modify memory
        jz memin
        cpi 'd'                 ; dump memory
        jz memout
        cpi 'j'                 ; jump to memory address
        jz jump
        cpi 'b'                 ; boot from floppy
        jz boot
        cpi 'r'                 ; read sector(s) from floppy
        jz diskread
        cpi 'w'                 ; write sector(s) to floppy
        jz diskwrite
        cpi 'f'                 ; format floppy track(s)
        jz diskfmt
	cpi 'i'			; input from i/o
	jz portin
	cpi 'o'			; output to i/o
	jz portout

        mvi c, '?'
        call uart_out
        jmp prompt              ; probably didn't understand cmd

memin:
        lxi h, hextype_str
        call uart_out_str       ; raw or intel?
        call uart_in
        mov c, a                ; save the input
        call uart_out           ; echo
        mov a, c
        cpi 'r'                 ; raw
        jz memin_raw
        cpi 'i'                 ; intel
        jz memin_intel
        jmp prompt

memin_raw:
        lxi h, start_str
        call uart_out_str       ; "start:"
        call uart_in_hexaddr
        call uart_out_crlf

        mov a, l
        ani 0fh                 ; calculate offset
        mov b, a
        mov a, l
        ani 0f0h                ; round down
        mov l, a

        call uart_out_mempos
memin_raw_1:
        mov a, l
        ani 0fh
        cmp b                   ; end of offset?
        jz memin_raw_2
        mvi c, ' '
        call uart_out
        mov d, m
        call uart_out_hex
        inx h
        jmp memin_raw_1

memin_raw_2:
        mvi c, ' '
        call uart_out
        call uart_in_hex
        mov m, a
        inx h
        inr b

        mov a, b
        cpi 010h                ; end of line?
        jnz memin_raw_2         ; nope

        call uart_out_crlf
        call uart_out_mempos
        mvi b, 0
        jmp memin_raw_2

memin_intel:
        call uart_out_crlf
memin_intel_start:
        call uart_in
        cpi ':'                 ; start code
        jz memin_intel_1
        cpi CtrlZ               ; bail
        jz prompt
        jmp memin_intel_start
memin_intel_1:
        call uart_in_hex        ; byte count
        mov b, a
        call uart_in_hex        ; address high
        mov h, a
        call uart_in_hex        ; address low
        mov l, a

        mov a, b
        add h
        add l                   ; start of checksum
        mov e, a

        push d
        call uart_in_hex        ; record type
        pop d
        cpi 0                   ; data
        jz memin_intel_2
        cpi 1                   ; eof
        jz memin_intel_eof
        jmp memin_intel ; not supported, wait for another record
memin_intel_2:
        mov a, b
        ana a
        jz memin_intel_cksum    ; b=0, checksum comes next
        push d                  ; e will get clobbered by input
        call uart_in_hex
        pop d
        mov m, a                ; save byte
        add e                   ; add to checksum
        mov e, a
        inx h                   ; increment pointer
        dcr b                   ; decrement counter
        jmp memin_intel_2       ; next byte
memin_intel_cksum:
        push d                  ; input clobbers e
        call uart_in_hex        ; get checksum
        pop d
        add e
        jz memin_intel          ; checksum is good, get next record
        lxi h, ckerr_str
        call uart_out_str       ; show error on console
        jmp memin_intel         ; get next record
memin_intel_eof:
        call uart_in_hex        ; checksum
        jmp prompt              ; done

memout:
        lxi h, hextype_str      ; raw or intel?
        call uart_out_str
        call uart_in
        mov c, a                ; save the input
        call uart_out
        mov a, c
        cpi 'r'
        jz memout_raw
        cpi 'i'
        jz memout_intel
        jmp prompt

memout_prompt:
        lxi h, start_str        ; "start"
        call uart_out_str
        call uart_in_hexaddr
        push h
        lxi h, end_str          ; "end:"
        call uart_out_str
        call uart_in_hexaddr
        call uart_out_crlf
        xchg                    ; end addr in de
        pop h                   ; start addr in hl
        ret

memout_raw:
        call memout_prompt
        mov a, l
        ani 0f0h                ; round down
        mov l, a
        mov a, e
        ani 0f0h
        mov e, a
        dcx h

memout_raw_1:
        inx h
        mvi b, 0
        push d
        call uart_out_mempos
        mvi c, ' '
        call uart_out
memout_raw_2:
        mov d, m                ; spit out a byte
        call uart_out_hex
        mvi c, ' '
        call uart_out

        inx h                   ; increment the pointer
        inr b                   ; increment the byte counter
        mov a, b
        cpi 010h                ; time for a new line?
        jnz memout_raw_2        ; not yet

        call uart_out_crlf
        in uart_ctl
        ani uart_rx_rdy
        jz memout_raw_3
        in uart_data
        cpi CtrlZ
        jz prompt
memout_raw_3:
        pop d
        dcx h
        mov a, h                ; see if we're at the end addr
        cmp d
        jc memout_raw_1
        mov a, l
        cmp e
        jc memout_raw_1
        jmp prompt              ; done

memout_intel:
        call memout_prompt
        mov a, e
        ana a                   ; clear carry
        sub l
        mov e, a
        mov a, d
        sbb h
        mov d, a                ; de=de-hl
memout_intel_1:
        mov a, d
        ora a
        jnz memout_intel_2      ; d>0
        mov a, e
        cpi 020h
        jnc memout_intel_2      ; e>20h
        mov b, e                ; b=e
        jmp memout_intel_3
memout_intel_2:
        mvi b, 020h             ; b=20h
memout_intel_3:
        ana a                   ; clear carry
        mov a, e
        sub b                   ; e=e-b
        mov e, a
        mov a, d
        sbi 0                   ; d=d-(borrow)
        mov d, a
        push d

        mvi c, ':'
        call uart_out           ; start code
        mov d, b
        call uart_out_hex       ; byte count
        mov d, h
        call uart_out_hex       ; address high
        mov d, l
        call uart_out_hex       ; address low
        mvi d, 0
        call uart_out_hex       ; record type

        mov a, b                ; checksum=b
        add h
        add l
        mov e, a                ; store checksum
memout_intel_4:
        mov a, b
        ana a
        jz memout_intel_5       ; end of line
        mov d, m                ; grab hex byte
        mov a, e
        add d
        mov e, a                ; checksum+=d
        call uart_out_hex       ; send to console
        dcr b                   ; decrement byte counter
        inx hl                  ; increment memory pointer
        jmp memout_intel_4
memout_intel_5:
        mov a, e
        cma                     ; compliment checksum
        inr a                   ; a++
        mov d, a
        call uart_out_hex       ; checksum
        call uart_out_crlf      ; newline

        in uart_ctl
        ani uart_rx_rdy
        jz memout_intel_6
        in uart_data
        cpi CtrlZ
        jz prompt
memout_intel_6:
        pop d
        mov a, d
        ana a                   ; d=0
        jnz memout_intel_1      ; next line
        mov a, e
        ana a                   ; e=0
        jz memout_intel_7       ; done
        jmp memout_intel_1      ; next line
memout_intel_7:
        lxi h, hexeof_str
        call uart_out_str
        jmp prompt

jump: ; jump to user-specified memory address
        lxi h, addr_str
        call uart_out_str       ; "addr:"
        call uart_in_hexaddr
        push h
        lxi h, crlf_str
        call uart_out_str
        pop h
        pchl                    ; jump!

boot:
	jmp prompt

disk_setdrvfmt: ; select drive, set format
	mvi c, zcmd_set_format
        call zfdc_out
        lxi h, drive_str        ; "drive:"
        call uart_out_str
        call uart_in_hex
        mov c, a
        call zfdc_out
        lxi h, format_str       ; "format:"
        call uart_out_str
        call uart_in_hex
        mov c, a
        call zfdc_out
        call zfdc_chkerr
	ret

disk_setsec:
	mvi c, zcmd_set_side
	call zfdc_out
	lxi h, side_str
	call uart_out_str	; "side:"
	call uart_in_hex
	mov c, a
	call zfdc_out
	call zfdc_chkerr

	mvi c, zcmd_set_track
	call zfdc_out
	lxi h, track_str	; "track:"
	call uart_out_str
	call uart_in_hex
	mov c, a
	call zfdc_out
	call zfdc_chkerr

	mvi c, zcmd_set_sector
	call zfdc_out
	lxi h, sector_str	; "sector:"
	call uart_out_str
	call uart_in_hex
	call zfdc_out
	call zfdc_chkerr
	ret

diskread:
	call disk_setdrvfmt
	call disk_setsec
	mvi c, zcmd_get_sec_size
	call zfdc_out
	call zfdc_in		; number of 128 byte blocks
	rlc			; *2
	rlc			; *4 lines per block
	mov d, a
	lxi h, 0		; "address" counter
diskread_1:
	push d			; save line counter
	mvi b, 20h		; output 20h bytes
	mov a, b		; beginning of checksum
	add h
	add l
	mov e, a		; save checksum
	mvi c, ':'		; start code
	call uart_out
	mov d, b		; byte count
	call uart_out_hex
	mov d, h		; high address
	call uart_out_hex
	mov d, l		; low address
	call uart_out_hex
	mvi d, 0		; record type
	call uart_out_hex
diskread_2:
	call zfdc_in		; get byte
	mov d, a
	call uart_out_hex	; send to uart
	mov a, e
	add d			; add to checksum
	mov e, a
	inx h			; increment pointer
	dcr b			; decrement byte counter
	jnz diskread_2		; next byte
	cma			; complement checksum
	inr a			; checksum++
	mov d, a
	call uart_out_hex	; send checksum
	call uart_out_crlf	; new line
	pop d
	dcr d
	jnz diskread_1		; more lines to follow
	lxi h, hexeof_str	; EOF
	call uart_out_str
        jmp prompt		; done

diskwrite: ; write a sector to disk from memory
        call disk_setdrvfmt
        call disk_setsec
        lxi h, start_str
        call uart_out_str
        call uart_in_hexaddr
        call disk_getsecsz
diskwrite_0:
        mvi c, zcmd_write_sector ; tell zfdc data is coming
        call zfdc_out
        call zfdc_waitack       ; wait for it to be ready
diskwrite_1:
        mov c, m                ; get byte from memory
        call zfdc_out           ; send to zfdc
        inx h
        dcx d
        mov a, e
        ora d                   ; last byte?
        jnz diskwrite_1         ; loop if not yet done

        call zfdc_chkerr
        jmp prompt              ; done!

diskfmt: 
	call disk_setdrvfmt
        lxi h, start_str        ; "start:"
        call uart_out_str
        call uart_in_hex
        mov h, a
        push h                  ; save start track
        lxi h, end_str
        call uart_out_str       ; "end:"
        call uart_in_hex
        pop h
        mov l, a                ; h=start trk, l=end trk
        call uart_out_crlf

	mvi c, zcmd_set_track	; set start track
	call zfdc_out
	mov c, h
	call zfdc_out
	call zfdc_chkerr
	mvi c, zcmd_seek_nv	; seek to start track
	call zfdc_out
	call zfdc_chkerr
diskfmt_nexttrk: ; format a track
        mvi c, zcmd_format_disk ; command
        call zfdc_out
        mov d, h                ; track number
        call uart_out_hex
        mov c, h
        call zfdc_out
        mvi c, zcmd_confirm_fmt ; go!
        call zfdc_out
        call uart_out_crlf
diskfmt_wait: ; wait for track to be formatted
        call zfdc_in            ; will wait for data
        cpi no_errors_flag
        jz diskfmt_trkdone      ; no error
        call zfdc_error
        jmp prompt              ; bail
diskfmt_trkdone:
	mvi c, BS
	call uart_out
	call uart_out		; overwrite previous track number 
        mov a, h
        cmp l                   ; last track?
        jz diskfmt_done
        inr h
        jmp diskfmt_nexttrk     ; no, move to next track
diskfmt_done:
        mvi c, zcmd_set_home
        call zfdc_out
        call zfdc_waitack
        mvi c, zcmd_set_drive
        call zfdc_out
        mvi c, 3
        call zfdc_out
        call zfdc_waitack
        jmp prompt

portin: ; input from i/o port, display result
;there is no "in r" instruction, so we'll have to put some code in memory
	lxi h, port_str		; "port:"
	call uart_out_str
	lxi h, stack+1		; use the temp space just above the stack
	mvi m, 0DBh		; "in"
	inx h			; increment pointer
	call uart_in_hex
	mov m, a		; port number
	call uart_out_crlf
	inx h
	mvi m, 0C9h		; "ret"
	call stack+1		; call input instruction
portin_1:
	mov d, a
	call uart_out_hex
	jmp prompt

portout: ; output specified value to i/o port
	lxi h, port_str		; "port:"
	call uart_out_str
	call uart_in_hex
	mov b, a
	lxi h, byte_str		; "byte:"
	call uart_out_str
	call uart_in_hex
	lxi h, stack+1
	mvi m, 0D3h		; "out"
	inx h			; increment pointer
	mov m, b		; port number
	inx h
	mvi m, 0C9h		; "ret"
	call stack+1
	jmp prompt	

; hardware support routines
uart_out_hex: ; send byte in d as ascii hex digits
        mov a, d
        rar
        rar
        rar
        rar                     ; get the high nibble
        call bin2hex
        mov c, a
        call uart_out           ; send first digit
        mov a, d
        call bin2hex
        mov c, a
        call uart_out           ; send second digit
        ret

uart_in_hexaddr: ; get an address from the console in hex, return in hl
        call uart_in_hex        ; high byte
        mov h, a
        call uart_in_hex        ; low byte
        mov l, a
        ret

uart_in_hex: ; get a byte from the console in ascii hex
        call uart_in_hexnib     ; get high nibble
        ral
        ral
        ral
        ral                     ; shift to the high nibble
        ani 0f0h                ; mask off low nibble
        mov e, a                ; store the first nibble
        call uart_in_hexnib     ; get low nibble
        ora e                   ; combine high and low nibble
        ret                     ; return with byte in a

uart_in_hexnib: ; get a single hex digit from the console, return in a
        call uart_in            ; get digit from console
        cpi CtrlZ               ; ctrl-z?
        jz prompt               ; bail if so
        cpi 061h                ; lowercase?
        jc hexnib1              ; nope
        sui 20h                 ; lower -> upper
hexnib1:
        mov c, a                ; save ascii version
        call hex2bin            ; convert to bin
        out 0ffh                ; DEBUG
        cpi 010h                ; carry if 0-F
        jnc uart_in_hexnib      ; try again
        mov d, a                ; a will get clobbered by uart_out
        call uart_out           ; echo
        mov a, d
        ret

hex2bin: ; convert ascii hex digit to binary nibble in a
        cpi 041h                ; >=A?
        jc hex2bin1             ; <A.
        sui 7                   ; correct for A-F
hex2bin1:
        sui 030h                ; get binary value
        ret

bin2hex: ; convert low nibble of a to ascii hex digit
        ani 0fh                 ; mask off high nibble
        cpi 0ah                 ; >=A?
        jc bin2hex1             ; <A.
        adi 7                   ; correct for A-F
bin2hex1:
        adi 030h                ; get ascii code
        ret

uart_out_crlf: ; newline without clobbering de
        mvi c, CR
        call uart_out
        mvi c, LF
        call uart_out
        ret

uart_out_str: ; send string at hl to the console
        mov a, m                ; get a byte to send
        ora a                   ; is it null?
        rz                      ; we're done if so
        mov c, a
        call uart_out           ; send this byte
        inx h                   ; increment the pointer
        jmp uart_out_str        ; loop until we find a null

uart_out_mempos: ; address in hl, plus a colon
        mov d, h
        call uart_out_hex
        mov d, l
        call uart_out_hex
        mvi c, ':'
        call uart_out
        ret

uart_out: ; send byte in c to the console
        in uart_ctl             ; check uart status
        ani uart_tx_rdy
        jz uart_out             ; loop until we can send this byte
        mov a, c
        out uart_data           ; send the byte
        ret

uart_in: ; get a byte from the console, store in a
        in uart_ctl             ; check uart status
        ani uart_rx_rdy
        jz uart_in              ; loop until a byte comes in
        in uart_data            ; get the byte
        ret

zfdc_out: ; send byte in c to the zfdc
        in zfdc_stat_port       ; check to see if fdc is ready for a byte
        mov d, a
        ani zfdc_direction      ; wait for direction to be IN
        jnz zfdc_out

        mov a, d
        ani zfdc_out_rdy        ; is there already data waiting?
        jz zfdc_out             ; loop if so

        mov a,c
        out zfdc_data_port      ; send the byte
        ret

zfdc_in: ; get byte from the zfdc, store it in a
        in zfdc_stat_port
        mov d, a
        ani zfdc_direction      ; wait if direction is IN
        jz zfdc_in

        mov a, d
        ani zfdc_in_rdy         ; wait for board to have a byte for us
        jz zfdc_in
        in zfdc_data_port       ; read the byte
        ret                     ; return with the byte in a

zfdc_stat: ; is the zfdc trying to tell us something?
        in zfdc_stat_port
        ani zfdc_in_rdy         ; any bytes waiting for us?
        rz                      ; return if not
        xra a
        dcr a
        ret                     ; return with a=ff if so

zfdc_chkerr: ; wait for ack, check for error
	call zfdc_waitack
	ora a
	rz			; just return if no error
	call zfdc_error		; show zfdc error
	jmp prompt		; return to prompt

zfdc_waitack: ; block until the zfdc sends something (with timeout)
        push b
        lxi b, 0
zwait_1:
        in zfdc_stat_port
        ani zfdc_direction      ; is zfdc in input mode?
        jz zwait_2              ; yep, it is
        call zfdc_stat          ; wait for zfdc to send something
        jz zwait_2
        call zfdc_in            ; get the byte
        cpi no_errors_flag      ; is it an error?
        pop b
        ret                     ; will return z if no problem
zwait_2:
        dcr b
        jnz zwait_1             ; inner loop
        dcr b
        dcr c
        jnz zwait_1             ; outer loop

        xra a                   ; timed out, return ff
        dcr a
        pop bc
        ret

zfdc_error:
        mov d, a
        lxi h, zfdc_error_str
        call uart_out_str
        call uart_out_hex
        ret

; DATA STARTS HERE
; parameters
drive_formats: db 01,01,01,01   ; default format number for drives 0-3

; strings (null terminated)
crlf_str: db CR,LF,0
initmem_str: db "Initializing memory...",CR,LF,0
initmem_error_str: db "Memory error at ",0
prompt_str: db CR,LF,'>',0
zfdc_error_str: db CR,LF,"ZFDC error ",0
signon_str: db "8080 ROM Monitor",CR,LF,0
hextype_str: db "(r)aw or (i)ntel:",0
start_str: db CR,LF,"start:",0
end_str: db CR,LF,SPC," end:",0
addr_str: db "addr:",0
hexeof_str: db ":00000001FF",CR,LF,0
ckerr_str: db " cksum err",0
drive_str: db "drive:",0
side_str: db CR,LF,"side:",0
track_str: db CR,LF,"track:",0
sector_str: db CR,LF,"sector:",0
format_str: db CR,LF,"format:",0
port_str: db "port:",0
byte_str: db CR,LF,"byte:",0

end:
