; CP/M 2.2 BIOS
; For IMSAI SIO Board and ZFDC Floppy Controller

.hexfile cbios.hex
.binfile cbios.bin
.download hex

; cp/m params
msize			equ 20
bias			equ (msize-20)*1024
ccp			equ 03400h+bias
bdos			equ ccp+0806h
bios			equ ccp+01600h
cdisk			equ 0h
iobyte			equ 0h

; i/o ports
uart_0_data		equ 02h
uart_0_ctl		equ 03h
zfdc_data		equ 010h
zfdc_stat		equ 011h
zfdc_reset		equ 013h

; mask values
uart_tx_rdy		equ 02h
uart_rx_rdy		equ 01h
zfdc_direction		equ 080h
zfdc_data_out_rdy	equ 02h
zfdc_data_in_rdy	equ 01h

; zfdc parameters
zfdc_disk_fmt_0		equ 01h	;IBM 8inch	
zfdc_disk_fmt_1		equ 01h	;IBM 8inch
zfdc_disk_fmt_2		equ 01h	;IBM 8inch
zfdc_disk_fmt_3		equ 01h	;IBM 8inch

; zfdc commands
zfdc_no_errors		equ 0h
zcmd_set_format		equ 04h
zcmd_set_drive		equ 05h
zcmd_set_track		equ 07h
zcmd_set_sector		equ 09h
zcmd_read_sector	equ 010h
zcmd_write_sector	equ 011h
zcmd_set_home		equ 0Ah
zcmd_handshake		equ 021h
zcmd_check_drive	equ 028h

.org bios

nsects	equ (.-ccp)/128

; BIOS jump table
 jmp boot
wboote:
 jmp wboot
 jmp const
 jmp conin
 jmp conout
 jmp list
 jmp punch
 jmp reader
 jmp home
 jmp seldsk
 jmp settrk
 jmp setsec
 jmp setdma
 jmp read
 jmp write
 jmp listst
 jmp sectran

dpbase: ;disk parameter headers for 8" IBM disk
; disk1
 dw trans, 0h
 dw 0h, 0h
 dw dirbf, dpblk
 dw chk00, all00
;disk2
 dw trans, 0h
 dw 0h, 0h
 dw dirbf, dpblk
 dw chk01, all01
;disk3
 dw trans, 0h
 dw 0h, 0h
 dw dirbf, dpblk
 dw chk02, all02
;disk4
 dw trans, 0h
 dw 0h, 0h
 dw dirbf, dpblk
 dw chk03, all03

trans: ;sector translation vector
 db 1, 7, 13, 19
 db 25, 5, 11, 17
 db 23, 3, 9, 15
 db 21, 2, 8, 14
 db 20, 26, 6, 12
 db 18, 24, 4, 10
 db 16, 22

dpblk: ; disk parameter block (for IBM 8" disk)
 dw 26		;sectors per track
 db 3		;block shift factor
 db 7		;block mask
 db 0		;null mask
 dw 242		;disk size-1
 dw 63		;directory max
 db 192		;alloc 0
 db 0		;alloc 1
 dw 16		;check size
 dw 2		;track offset

boot: ;cold boot
 xra a		;zero the accum
 sta iobyte	;clear the iobyte
 sta cdisk	;select disk 0
 jmp gocpm	;initialize and go to cpm

wboot: ;warm boot, read system from disk
 lxi sp, 80h	;use space below buffer for stack
 mvi c, 0	;select disk 0
 call seldsk
 call home	;go to track 0

 mvi b, nsects	;number of sectors to load
 mvi c, 0	;current track number
 mvi d, 2	;next sector to read (0 and 1 are cold start loader)
 lxi h, ccp	;start at base of ccp

load1:	;load a sector
 push b		;save sector count
 push d		;save next sector
 push h		;save dma address
 mov c, d	;get sector address to c
 call setsec	;set sector address from c
 pop b		;recall dma address to b, C
 push b		;replace on stack for later recall
 call setdma

 call read
 cpi 0h		;any errors?
 jnz wboot	;try again if so

;no error, move on
 pop h		;recall dma address
 lxi d, 128	;dma=dma+128
 dad d		;new dma address is in h, l
 pop d		;recall sector address
 pop b		;recall # of sectors remaining and current trk
 dcr b		;sectors--
 jz gocpm	;jump to cp/m if we're all done

 inr d		;we're not, check for track change
 mov a, d
 cpi 27
 jc load1	;sector<27

;end of current track, move to next
 mvi d, 1	;start on 1st sector
 inr c		;track++

;save register state, change track
 push b
 push d
 push h
 call settrk
 pop h
 pop d
 pop b
 jmp load1	;read next sector

gocpm: ;done loading, set params and go to cp/m
 mvi a, 0c3h	;c3=jmp
 sta 0		;for jump to wboot
 lxi h, wboote	;wboot entry point
 shld 1		;set address field for jmp at 0

 sta 5		;for jmp to bdos
 lxi h, bdos	;bdos entry point
 shld 6		;address field of jump at 5 to bdos

 lxi b, 80h	;default dma address is 80h
 call setdma

 ei		;enable interrupts 
 lda cdisk	;get current disk number
 mov c, a	;send to the ccp
 jmp ccp	;start cp/m

const: ;console status: ff if char ready, 00 if not
 in uart_0_ctl	;get uart status
 ani uart_rx_rdy ;mask out the ready bit
 rz		;0=not ready
 xra a
 dcr a		;ff=ready
 ret

conin: ;get a byte from the console
 in uart_0_ctl	;get uart status
 ani uart_rx_rdy ; mask out ready bit
 jz conin	;loop until ready
 in uart_0_data	;get a byte
 ani 07Fh	;strip msb
 ret

conout: ;send a byte to the console
 in uart_0_ctl	;get uart status
 ani uart_tx_rdy ;mask out ready bit
 jz conout	;loop until ready
 mov a, c
 out uart_0_data ;send the byte
 ret

list: ;send a char to the list device
 ret	;we have no printer

listst: ;list device status
 xra a		;we have no printer
 ret

punch: ;send a byte to the punch device
 ret	;we have no punch

reader: ;get a byte from the tape reader
 mvi a, 01Ah	;EOF
 ret

home: ;move current drive to track 0
 mvi c, zcmd_set_home	;zfdc has a command for this
 call zfdcout
 ret

seldsk:	;select specified disk drive
 lxi h, 0		;clear hc
 mov a, c
 sta diskno		;store current drive
 
 cpi 4			;must be 0-3
 rnc

 mvi c, zcmd_set_drive	;tell the zfdc we want to select a drive
 call zfdcout
 mov c, a		;send the drive number
 call zfdcout
 call zfdcack
 rnz			;return if no ack

 mvi a, 01h		;should we check the disk?
 ana c
 jnz seldsk_dph
 mvi c, zcmd_check_drive
 call zfdcout		;send check drive command to zfdc
 lda diskno
 mov c, a
 call zfdcout		;send drive number
 call zfdcack
 rnz			;return if error
seldsk_dph:
 lda diskno		;now we'll get the dph address
 mov l, a		;drive number
 mvi h, 0
 dad h			;*2
 dad h			;*4
 dad h			;*8
 dad h			;*16 (size of dph)
 lxi d, dpbase
 dad d			;hl=dpbase(diskno*16)
 ret

settrk: ;select track specified in c
 mov a, c		;save track number
 sta track
 mvi c, zcmd_set_track	;tell the zfdc we want to change tracks
 call zfdcout
 mov c, a		;send track number
 call zfdcout
 call zfdcack
 ret

setsec: ;select sector specified in c
 mov a, c		;save sector number
 sta sector
 mvi c, zcmd_set_sector
 call zfdcout
 mov c, a
 call zfdcout		;sector number
 call zfdcack
 ret

sectran: ;logical sector -> physical sector
 xchg			;hl=trans
 dad b			;hl=trans(sector)
 mov l, m		;l=trans(sector)
 mvi h, 0		;hl=trans(sector)
 ret			;with value in hl

setdma: ;set dma address given by registers b and c 
 mov l, c		;low addr
 mov h, b		;high addr
 shld dmaad		;store address
 ret

read: ;read a sector from the disk
;note: assumes drive, track, sector already set
 mvi c, zcmd_read_sector
 call zfdcout
 call zfdcack
 rnz			;error

 lxi d, 128		;TODO: different sector sizes
 
rd_data:
 call zfdcin		;grab byte
 mov m, a		;hl = dma address
 inx h			;increment dma address
 dcx d			;decrement byte count
 mov a, e
 ora d			;last byte?
 jnz rd_data

 call zfdcack		;any errors?
 ret

write: ;write sector to sisk
;note: assumes drive, track, sector already set
 mvi c, zcmd_write_sector
 call zfdcout
 call zfdcack
 rnz

 lxi d, 128		;TODO: different sector sizes

wr_data: ;a lot like rd_data...
 mov c, m
 call zfdcout
 inx h
 dcx d
 mov a, e
 ora d
 jnz wr_data

 call zfdcack
 ret

zfdcstat:
 in zfdc_stat
 ani zfdc_data_in_rdy	;any bytes waiting for us?
 rz			;return if not
 xra a
 dcr a
 ret			;return with a=ff if so

zfdcin:
 in zfdc_stat
 mov d, a
 ani zfdc_direction	;wait if direction is IN
 jz zfdcin

 mov a, d
 ani zfdc_data_in_rdy	;wait for board to have a byte for us
 jz zfdcin
 in zfdc_data		;read the byte
 ret			;return with the byte in a


zfdcout:
 in zfdc_stat		;check to see if fdc is ready for a byte
 mov d, a
 ani zfdc_direction	;wait for direction to be IN
 jnz zfdcout

 mov a, d
 ani zfdc_data_out_rdy	;is there already data waiting?
 jz zfdcout		;loop if so

 mov a, c
 out zfdc_data		;send the byte
 ret

zfdcack:
 push bc
 lxi b,0
ack_wait_1:
 in zfdc_stat
 ani zfdc_direction	;is zfdc in input mode?
 jz ack_wait_2		;yep, it is
 call zfdcstat		;wait for zfdc to send something
 jz ack_wait_2
 call zfdcin		;get the byte
 cpi zfdc_no_errors	;is it an error?
 pop bc
 ret			;will return z if no problem
ack_wait_2:
 dcr b
 jnz ack_wait_1		;inner loop
 dcr b
 dcr c
 jnz ack_wait_1		;outer loop

 xra a			;timed out, return ff
 dcr a
 pop bc
 ret

;variables
 ;bios
track: ds 2
sector: ds 2
dmaad: ds 2
diskno: ds 1

 ;bdos
begdat equ .
dirbf: ds 128
all00: ds 31
all01: ds 31
all02: ds 31
all03: ds 31
chk00: ds 16
chk01: ds 16
chk02: ds 16
chk03: ds 16
enddat equ .
datsiz equ .-begdat
end
