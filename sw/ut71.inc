;	.SBTTL	 UT71 Definitions for Spare Time Gizmos SBC1802
;	 Bob Armstrong [09-SEP-2021]


;            Copyright (C) 2021 By Spare Time Gizmos, Milpitas CA.

;++
;   This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
; for more details.
;
;   You should have received a copy of the GNU General Public License along
; with this program; if not, write to the Free Software Foundation, Inc.,
; 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;--

;++
;   This include file defines the standard entry points, known constants, and
; any other magic numbers for RCA's UT71 ROM.  The SBC1802 firmware emulates
; many of the standard UT71 entries and functions for MicroDOS compatability.
;
;   Note that a few of these names have been changed slightly from the RCA 
; documentation by prepending "UT" or "U".  This is to avoid conflicts with
; other names used on the SBC1802.
;--
;0000000001111111111222222222233333333334444444444555555555566666666667777777777
;1234567890123456789012345678901234567890123456789012345678901234567890123456789

	.MSFIRST \ .PAGE \ .CODES

	.EJECT
;++
; REVISION HISTORY
;
; 001	-- Start from scratch!
;
;--

	.EJECT
;	.SBTTL	UT71 Registers and Entry Points

; UT71 memory layout ...
UTBASE	.EQU	$8000	; base address for UT71 ROM
UTSIZE	.EQU	$0800	; size of the UT71 ROM (2K)
UTCKSM	.EQU	$87FF	; location of UT71 checksum byte
UTREGS	.EQU	$8C00	; UT71 stores user registers here
UTSTK	.EQU	$8CFF	; and the UT71 stack grows down from here

; Standard UT71 register usage...
;   Note that the SCRT registers - SP, PC, CALL, RETN and LINK, are exactly the
; same as are used everywhere by ElfOS and UT71 alike, so we don't need those.
;SP	 .EQU	2	; stack pointer
;PC	 .EQU	3	; standard program counter
;CALL	 .EQU	4	; SCRT CALL routine pointer
;RETN	 .EQU	5	; SCRT RETURN  "       "
;LINK	 .EQU	6	; SCRT caller's argument list pointer
;UTASL	 .EQU	$D	; used by READAH to assemble hex values
;UTAUX	 .EQU	$E	; local echo flag and temporary for console I/O
;UTCHAR	 .EQU	$F	; ASCII character for input/output

; UT71 SCRT and console I/O entry points ...
READAH	 .EQU	$813B	; read one hexadecimal character
UTREAD	 .EQU	$813E	; read one character into RF.1
UTYPE5	 .EQU	$81A0	; type character at M(R5)
UTYPE6	 .EQU	$81A2	; type character at M(R6)
UTYPE	 .EQU	$81A4	; type character from RF.1
UTYPE2	 .EQU	$81AE	; type hexadecimal pair from RF.1
OSTRNG	 .EQU	$83F0	; type ASCIZ string at M(R6)
UTINIT1	 .EQU	$83F3	; initialize R2, R3, R4, R5, X and P
UTINIT2	 .EQU	$83F6	; initialize R2, R4, R5, X and P (NOT R3!)
GOUT71	 .EQU	$83F9	; return to ROM command prompt
UTCKHEX	 .EQU	$83FC	; convert ASCII character in RF.1 to binary in RE.0
UTCALLR	 .EQU	$8364	; SCRT CALL routine
UTRETR	 .EQU	$8374	; SCRT RETURN routine
LINEPR	 .EQU	$850E	; send RF.1 to line printer port

; UT71 floppy disk I/O entry points ...
CFRETS	 .EQU	$87D8	; command file return point
READTRS	 .EQU	$87DB	; read multiple sectors by parameter block
WRITTRS	 .EQU	$87DE	; write  "   "     "    "    "   "     "
READAS	 .EQU	$87E1	; read sector using existing DMA pointer
WRITAS	 .EQU	$87E4	; write  "      "     "   "   "   "   "
RECALS	 .EQU	$87E7	; recalibrate, use R0.0 as unit number
SEEKAS	 .EQU	$87EA	; seek by track and sector in ASL
UTCMDS	 .EQU	$87ED	; output command bytes to FDC
RENTERS	 .EQU	$87F0	; re-enter UT71 (same as GOUT71?)
UTWAITS	 .EQU	$87F3	; service FDC after command
UTSEEKS	 .EQU	$87F6	; seek by parameter block
UTREADS	 .EQU	$87F9	; read sector by parameter block
UTWRITS  .EQU	$87FC	; write  "    "    "   "     "
UTCHECK	 .EQU	$87FF	; UT71 checksum

; Other magic addresses ... 
CFREAD	 .EQU	$8FFD	; MicroDOS command file interpeter entry point
PGMSRT	 .EQU	$0005	; start address for INIT1
UTSTACK	 .EQU	$8CFF	; UT71 default stack area
UTIOCB	 .EQU	$8F00	; MicroDOS loader IOCB

; MicroDOS constants ...
MDSTACK	 .EQU	$BFFF	; TOP OF STACK
MDCALL	 .EQU	$91BC	; SCRT CALL ADDRESS
MDRETN	 .EQU	$919C	; SCRT RETURN ADDRESS
MDECHO	 .EQU	$910A	; ECHO STATUS
MDENTRY	 .EQU	$9040	; ENTRY ADDRESS INTO MICRODOS
MDNAME	 .EQU	$A843	; 'MICRODOS' NAME IN DP SYS

; Floppy disk I/O error codes (returned in P2.0) ...
FDFDDM	 .EQU	$80	; deleted data mark found
FDDINA	 .EQU	$40	; drive inactive (means what??)
FDFAIL	 .EQU	$20	; drive failure
FDPROT	 .EQU	$10	; write protect error
FDCRCE	 .EQU	$08	; CRC error
FDTERM	 .EQU	$02	; FDC command abnormal termination
