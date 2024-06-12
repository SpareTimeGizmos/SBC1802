	.SBTTL	"UT71 Definitions for Spare Time Gizmos SBC1802"
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
;   For compatibility with BASIC3 and the RCA MCDS ("Microboard COSMAC
; Development System") UT62 ROM, several UT62 entry points are defined as well.
; Most of the UT62 entries are identical to UT71, and there are only a few
; extras needed.
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
; 002	-- Add some UT62 entry points for BASIC3 ...
;
;--

	.SBTTL	"UT71 Registers and Entry Points"

; UT71 memory layout ...
UTBASE	.EQU	$8000	; base address for UT71 ROM
UTSIZE	.EQU	$0800	; size of the UT71 ROM (2K)
UTCKSM	.EQU	$87FF	; location of UT71 checksum byte
UTDATA	.EQU	$8C00	; UT71 RAM data page
UTSTACK	.EQU	$8CFF	; UT71 private stack
UTIOCB	.EQU	$8F00	; bootstrap I/O control block


; Standard UT71 register usage...
;   Note that the SCRT registers - SP, PC, CALL, RETN and LINK, are exactly the
; same as are used everywhere by ElfOS and UT71 alike, so we don't need those.
;SP	 .EQU	2	; stack pointer
;PC	 .EQU	3	; standard program counter
;CALL	 .EQU	4	; SCRT CALL routine pointer
;RETN	 .EQU	5	; SCRT RETURN  "       "
;LINK	 .EQU	6	; SCRT caller's argument list pointer
ASL	 .EQU	$D	; used by READAH to assemble hex values
AUX	 .EQU	$E	; local echo flag and temporary for console I/O
CHAR	 .EQU	$F	; ASCII character for input/output
PARA	 .EQU   $F	; parameter list pointer for UT71 calls

; UT71 SCRT and console I/O entry points ...
UTDELAY	 .EQU	$80EF	; delay routine
UTREADAH .EQU	$813B	; read one hexadecimal character
UTREAD	 .EQU	$813E	; read one character into RF.1
UTYPE5	 .EQU	$81A0	; type character at M(R5)
UTYPE6	 .EQU	$81A2	; type character at M(R6)
UTYPE	 .EQU	$81A4	; type character from RF.1
UTYPE2	 .EQU	$81AE	; type hexadecimal pair from RF.1
UTOSTRNG .EQU	$83F0	; type ASCIZ string at M(R6)
UTINIT1	 .EQU	$83F3	; initialize R2, R3, R4, R5, X and P
UTINIT2	 .EQU	$83F6	; initialize R2, R4, R5, X and P (NOT R3!)
UTGOUT71 .EQU	$83F9	; return to ROM command prompt
UTCKHEX	 .EQU	$83FC	; convert ASCII character in RF.1 to binary in RE.0
UTCALL	 .EQU	$8364	; SCRT CALL routine
UTRET	 .EQU	$8374	; SCRT RETURN routine
UTLINEPR .EQU	$850E	; send RF.1 to line printer port

; UT62 entry points (which aren't also UT71 entries!) ...
UTTYPED	 .EQU	$8198	; delay, then call TYPE
UTTYPE5D .EQU	$819C	; delay, then call TYPE5
UTTIMALC .EQU	$80FE	; autobaud
UTMOVE	 .EQU	$8429	; move memory
UTFILL	 .EQU	$824B	; fill memory

; UT71 floppy disk I/O entry points ...
UTBOOT	  .EQU	$8400	; SBC1802 specific - bootstrap MicroDOS
UTCFRETS  .EQU	$87D8	; command file return point
UTREADTRS .EQU	$87DB	; read multiple sectors by parameter block
UTWRITTRS .EQU	$87DE	; write  "   "     "    "    "   "     "
UTREADAS  .EQU	$87E1	; read sector using existing DMA pointer
UTWRITAS  .EQU	$87E4	; write  "      "     "   "   "   "   "
UTRECALS  .EQU	$87E7	; recalibrate, use R0.0 as unit number
UTSEEKAS  .EQU	$87EA	; seek by track and sector in ASL
UTCMDS	  .EQU	$87ED	; output command bytes to FDC
UTRENTERS .EQU	$87F0	; re-enter UT71 (same as GOUT71?)
UTWAITS	  .EQU	$87F3	; service FDC after command
UTSEEKS	  .EQU	$87F6	; seek by parameter block
UTREADS   .EQU	$87F9	; read sector by parameter block
UTWRITS   .EQU	$87FC	; write  "    "    "   "     "
UTCHECK	  .EQU	$87FF	; UT71 checksum
UTCFREAD  .EQU	$8FFD	; MicroDOS command file interpeter entry point

; Floppy disk I/O error codes (returned in P2.0) ...
FD.DDM	 .EQU	$80	; deleted data mark found
FD.DNR	 .EQU	$40	; drive not ready
FD.DER	 .EQU	$20	; drive failure
FD.WPT	 .EQU	$10	; write protect error
FD.CRC	 .EQU	$08	; CRC error
FD.ABE	 .EQU	$02	; FDC command abnormal termination

; UT62 Cassette tape I/O entry points ...
UTREADT	 .EQU	$8502	; read tape
UTWRITET .EQU	$8500	; write tape
UTREWT   .EQU	$8504	; rewind tape
