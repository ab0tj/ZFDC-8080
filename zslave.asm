.org 0100h

; I/O ports
uart_data       equ 02h         ; sio data port
uart_ctl        equ 03h         ; sio control port
uart_reset      equ 040h        ; reset byte
uart_mode       equ 04Eh        ; mode byte
uart_cmd_byte   equ 027h        ; control byte
zfdc_data       equ 010h        ; zfdc data port
zfdc_status     equ 011h        ; zfdc status port
zfdc_reset      equ 013h        ; zfdc reset port

; Bit Masks
uart_rx_rdy     equ 02h
uart_tx_rdy     equ 01h
zfdc_direction  equ 080h
zfdc_out_rdy    equ 02h
zfdc_dir_in     equ 081h
zfdc_dir_out	equ 082h

 out zfdc_reset         ; reset the zfdc board

 mvi a, 0               ; send dummy mode and command to uart
 out uart_ctl
 out uart_ctl
 out uart_ctl
 mvi a, uart_reset      ; reset the uart
 out uart_ctl
 mvi a, uart_mode       ; initialize the uart
 out uart_ctl
 mvi a, uart_cmd_byte
 out uart_ctl

loop:
 in zfdc_status         ; does the zfdc have a byte for us?
 ani zfdc_dir_in        ; mask out the bits we want
 xri zfdc_dir_in        ; make sure both are set
 jnz uart               ; move on if not
 in uart_ctl            ; is the uart ready for us to send a byte?
 ani uart_tx_rdy	
 jz uart		; move on if not
 in zfdc_data           ; grab byte from zfdc
 out uart_data		; send it to uart

uart:
 in uart_ctl            ; is there a byte in the uart?
 ani uart_rx_rdy
 jz loop                ; move on if not
 in zfdc_status		; is the zfdc ready to receive it?
 ani zfdc_dir_out	; mask off the bits we want
 xri zfdc_out_rdy	; dir in, out ready
 jnz loop		; move on if not
 in uart_data           ; grab the byte from uart
 out zfdc_data		; send it to the zfdc
 
 jmp loop
