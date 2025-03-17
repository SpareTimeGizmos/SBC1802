; *******************************************************************
; *** This software is copyright 2006 by Michael H Riley          ***
; *** You have permission to use, modify, copy, and distribute    ***
; *** this software so long as this copyright notice is retained. ***
; *** This software may not be used in commercial applications    ***
; *** without express written permission from the author.         ***
; *******************************************************************

#ifdef TASM
#define equ .equ
#endif

; BIOS ENTRY VECTOR TABLES ...
;   Note that for TASM these must be .EQUs, not #defines ...
BIOS:	.equ	0FF00h	; standard BIOS entry vector table
EBIOS:	.equ	0F800h	; extended BIOS entry vector table
SBIOS:	.equ	0F000h	; SBC1802  BIOS entry vector table

#ifndef TASM
scall:      equ  r4                    ; register for SCALL
sret:       equ  r5                    ; register for SRET
#endif

; Standard BIOS entry points ...
f_call      equ  0ffc6h                ; SCRT CALL routine
f_return    equ  0ffd8h                ; SCRT RETURN routine
f_boot:     equ  (BIOS+00h)            ; boot from ide device
f_type:     equ  (BIOS+03h)            ; type 1 character to console
f_read:     equ  (BIOS+06h)            ; read 1 character from console
f_msg:      equ  (BIOS+09h)            ; type asciiz string to console
f_typex:    equ  (BIOS+0ch)            ; depricated, just returns now
f_input:    equ  (BIOS+0fh)            ; read asciiz from console
f_strcmp:   equ  (BIOS+12h)            ; compare 2 strings
f_ltrim:    equ  (BIOS+15h)            ; trim leading spaces
f_strcpy:   equ  (BIOS+18h)            ; copy an asciiz string
f_memcpy:   equ  (BIOS+1bh)            ; copy memory
f_wrtsec:   equ  (BIOS+1eh)            ; write floppy sector (depricated)
f_rdsec:    equ  (BIOS+21h)            ; read floppy sector (depricated)
f_seek0:    equ  (BIOS+24h)            ; floppy seek to track 0 (depricated)
f_seek:     equ  (BIOS+27h)            ; floopy track seek (depricated)
f_drive:    equ  (BIOS+2ah)            ; select floppy drive (depricated)
f_setbd:    equ  (BIOS+2dh)            ; set console baud rate
f_mul16:    equ  (BIOS+30h)            ; 16-bit multiply
f_div16:    equ  (BIOS+33h)            ; 16-bit division
f_idereset: equ  (BIOS+36h)            ; reset ide device
f_idewrite: equ  (BIOS+39h)            ; write ide sector
f_ideread:  equ  (BIOS+3ch)            ; read ide sector
f_initcall: equ  (BIOS+3fh)            ; initialize R4 and R5
f_bootide:  equ  (BIOS+42h)            ; boot from ide device
f_hexin:    equ  (BIOS+45h)            ; convert ascii number to hex
f_hexout2:  equ  (BIOS+48h)            ; convert hex to 2-digit ascii
f_hexout4:  equ  (BIOS+4bh)            ; convert hex to 4-digit ascii
f_tty:      equ  (BIOS+4eh)            ; type character to console
f_mover:    equ  (BIOS+51h)            ; program relocator
f_minimon:  equ  (BIOS+54h)            ; mini monitor
f_freemem:  equ  (BIOS+57h)            ; determine memory size
F_isnum:    equ  (BIOS+5ah)            ; determine if D is numeric
f_atoi:     equ  (BIOS+5dh)            ; convert ascii to integer
f_uintout:  equ  (BIOS+60h)            ; convert unsigned integer to ascii
f_intout:   equ  (BIOS+63h)            ; convert signed integer to ascii
f_inmsg:    equ  (BIOS+66h)            ; type in-line message
f_inputl:   equ  (BIOS+69h)            ; read limited line from console
f_brktest:  equ  (BIOS+6ch)            ; check for serial break
f_findtkn:  equ  (BIOS+6fh)            ; find token in a token table
f_isalpha:  equ  (BIOS+72h)            ; determine if D is alphabetic
f_ishex:    equ  (BIOS+75h)            ; determine if D is hexadecimal
f_isalnum:  equ  (BIOS+78h)            ; determine if D is alpha or numeric
f_idnum:    equ  (BIOS+7bh)            ; determine type of ascii number
f_isterm:   equ  (BIOS+7eh)            ; determine if D is a termination char
f_getdev:   equ  (BIOS+81h)            ; get supported devices
f_nbread:   equ  (BIOS+84h)            ; non-blocking read from console
f_sdread:   equ  (BIOS+87h)            ; read storage device sector
f_sdwrite:  equ  (BIOS+8Ah)            ; write storage device sector
f_sdreset:  equ  (BIOS+8Dh)            ; reset storage device subsystem
f_sdboot:   equ  (BIOS+90h)	       ; boot from a specific storage device
f_sdsize:   equ  (BIOS+93h)	       ; deprecated
f_version:  equ  (BIOS+0f9h)           ; 3 bytes holding bios version number
f_extbver:  equ  (f_version-2)	       ; 2 byte pointer to extended bios version
f_checksum: equ  $fffc		       ; 4 bytes with the STG EPROM checksum


; "Extended" BIOS vectors
f_bread     equ  (EBIOS+00h)        ; read from bit bang serial port
f_btype     equ  (EBIOS+03h)        ; write to bit bang serial port
f_btest     equ  (EBIOS+06h)        ; test bit bang serial port
f_utype     equ  (EBIOS+09h)        ; write to console UART
f_uread     equ  (EBIOS+0ch)        ; read from console UART
f_utest     equ  (EBIOS+0fh)        ; test console UART
f_usetbd    equ  (EBIOS+12h)        ; set console UART baud rate and format
f_gettod    equ  (EBIOS+15h)        ; read time of day clock
f_settod    equ  (EBIOS+18h)        ; set time of day clock
f_rdnvr     equ  (EBIOS+1bh)        ; read non volatile RAM
f_wrnvr     equ  (EBIOS+1eh)        ; write non volatile RAM
f_idesize   equ  (EBIOS+21h)        ; deprecated
f_ideid     equ  (EBIOS+24h)        ; deprecated
f_tmtoas    equ  (EBIOS+2ah)        ; deprecated
f_dttoas    equ  (EBIOS+27h)        ; deprecated
f_rtctest   equ  (EBIOS+2dh)        ; test size and presence of RTC/NVR
f_astodt    equ  (EBIOS+30h)        ; deprecated
f_astotm    equ  (EBIOS+33h)        ; deprecated
f_nvrcchk   equ  (EBIOS+36h)	    ; compute NVR checksum

; BIOS capability bits returned by f_getdev ...
fgd_ide     equ  BIT0		    ; IDE support
fgd_floppy  equ  BIT1               ; floppy disk support
fgd_bbser   equ  BIT2               ; software (i.e. bit-banged) serial support
fgd_uart    equ  BIT3               ; UART support
fgd_rtc     equ  BIT4               ; real time clock support
fgd_nvr     equ  BIT5               ; non-volatile RAM support
fgd_f8vec   equ  BIT6               ; extended BIOS vectors at $F800 implemented
fgd_f0vec   equ  BIT7               ; extended BIOS vectors at $F000 implemented
fgd_nbread  equ  BIT8               ; f_nbread implemented
fgd_sdfun   equ  BIT9		    ; f_sdread/f_sdwrite/f_sdreset implemented

; SBC1802 specific vectors and functions
f_trap	    equ	 (SBIOS+00h)	    ; breakpoint trap entry
f_continue  equ  (SBIOS+03h)        ; continue after a trap
f_run       equ  (SBIOS+06h)        ; run a program with P=0
f_tuinit    equ  (SBIOS+09h)        ; initialize TU58
f_turead    equ  (SBIOS+0ch)        ; read TU58 sector
f_tuwrite   equ  (SBIOS+0fh)        ; write TU58 sector
f_sl1out    equ  (SBIOS+12h)        ; blocking output to SLU1
f_sl1hit    equ  (SBIOS+15h)        ; test SLU1 for input
f_sl1in     equ  (SBIOS+18h)        ; blocking read w/o echo
f_sl1get    equ  (SBIOS+1bh)        ; blocking read with echo
f_sl1nbr    equ  (SBIOS+1eh)        ; non-blocking read w/o echo
f_testhwf   equ  (SBIOS+21h)	    ; test SBC1802 hardware flags

; SBC1802 debugger breakpoint (cause a trap to f_trap)...
#define	BPT	MARK\ SEP 1

; Spare Time Gizmos BIOS identifier ...
STGBIOS     equ  3
