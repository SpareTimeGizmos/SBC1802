	.TITLE	 UT71 Emulation for Spare Time Gizmos SBC1802
;	 Bob Armstrong [06-OCT-2021]
; NEED LOADER (AKA BOOTSTRAP) AT $8400...

;               db    db d888888b  db .d888b.  .d88b.  .d888b. 
;               88    88 `~~88~~' o88 88   8D .8P  88. VP  `8D 
;               88    88    88     88 `VoooY' 88  d'88    odD' 
;               88    88    88     88 .d~~~b. 88 d' 88  .88'   
;               88b  d88    88     88 88   8D `88  d8' j88.    
;               ~Y8888P'    YP     VP `Y888P'  `Y88P'  888888D 
;
;            Copyright (C) 2021 By Spare Time Gizmos,Milpitas CA.

;++
;   This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License,or
; (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,but
; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
; for more details.
;
;   You should have received a copy of the GNU General Public License along
; with this program; if not,write to the Free Software Foundation,Inc.,
; 59 Temple Place,Suite 330,Boston,MA  02111-1307  USA
;--

;++
;   This file contains an emulation of the RCA UT71 ROM for the SBC1802.  The
; goal here is to be able to run RCA MicroDOS but NOT to emulate the UT71 user
; interface.  The SBC1802 already has its own perfectly good CLI after all.
; The primary things we're concerned with here are -
;
;   * Emulating the UT71 console terminal interface functions (READ, READAH,
; TYPE, TYPE5/6, OSTRNG, etc).
;
;   * Emulating the UT71 floppy diskette I/O routines.  These are redirected
; to a partition on the IDE drive that contains floppy diskette images.
;
;   * And some miscellaneous functions (INIT1/2, SCRT, CKHEX, etc).
;--
;0000000001111111111222222222233333333334444444444555555555566666666667777777777
;1234567890123456789012345678901234567890123456789012345678901234567890123456789

	.MSFIRST \ .PAGE \ .CODES

	.NOLIST
	.INCLUDE "sbc1802.inc"
	.INCLUDE "bios.inc"
	.INCLUDE "ut71.inc"
	.LIST
	
	.EJECT
;++
; REVISION HISTORY
;
; 001	-- Start from scratch!
;
; --
VERMAJ	.EQU	0	; major version number
VERMIN	.EQU	0	; minor   "  "    "
VEREDT	.EQU	0	; and the edit level

	.SBTTL	"Memory Layout, SCRT and Register Usage"

;++
; MEMORY LAYOUT
;--------------
;   The original MS2000 UT71 ROM was addressed from $8000 to $87FF and there
; also was 256 bytes of RAM from $8C00 to $8CFF.  The latter was used to store
; user registers and for stack space, and it's not clear to me that any of the
; remaining space from $8800 thru $8FFF was actually used.  The MS2000 also had
; RAM from $9000 thru $BFFF to hold the MicroDOS kernel and, optionally, RAM
; from $C000 thru $FFFF could be used as additional program space for MicroDOS
; programs.
;
;   The SBC1802, using the MicroDOS memory map, maps the EPROM from $8000 to
; $87FF and also from $F000 thru $FFFF (excepting the space at $FExx set aside
; for firmware RAM, PIC, RTC and memory control).  The space from $8800 to
; $EFFF is all RAM and we really don't care how it's used.  The tricky bit then
; is to fit our UT71 emulation into the SBC1802 EPROM from $8000 thru $87FF.
; The space from $F000 up contains the same BIOS that's used by ElfOS and,
; although MicroDOS doesn't care about this, our UT71 emulation can call that
; code if we need to.
;
;   The UT71 ROM has a couple of vector tables, one at $83F0 and another at
; $87D8, but unfortunately it also has several entry points scattered randomly
; around the address space.  We have no choice but to work around these.  The
; regular SBC1802 firmware also uses the space from $8000 thru $80FF for the
; cold start vector, warm start vector, copyright notice, and version. Luckily
; the first UT71 entry point that we have to match up with is at $8134, so
; we're OK there.
;
; SCRT
; ----
;   The UT71 ROM has its standard call and return routines and, although they
; are pretty close to the ElfOS BIOS versions, they're not identical.  The
; stack, PC and CALL/RETURN register usage is exactly the same, however the
; ElfOS BIOS versions preserve D across CALL and RETURN by using RE.0 as
; temporary storage.  The UT71 versions don't save D and, worse, there are
; several UT71 routines that expect to pass or return a value in RE.0.  These
; are all READ and TYPE routines, OSTRNG, and CKHEX.  The good news is that
; they all expect to return a character in RE.0, and we can achieve the same
; result with the ElfOS SCRT simply by returning the character in D.
;
;   *****   Currently we provide exact copies of the UT71 CALL and RETURN
; routines at the exact same addresses.  At some point we should try replacing
; these with LBRs to the ElfOS BIOS versions, just to see what happens. *****
;
; CONSOLE I/O
; -----------
;   The UT71 console I/O functions - READ, READAH, TYPE5, TYPE6, TYPE, TYPE2,
; and OSTRNG - all access the CDP1854 console serial port SLU0 directly. I'm
; tempted to go thru the BIOS functions, but most of those pass the character
; in D and thus work only with the ElfOS versions of SCRT.  There's also an
; incompatibility in the handling of the local echo bit - both ElfOS and
; MicroDOS use the LSB of RE.1, however the sense is reversed.  For ElfOS if
; the RE.1 LSB is set then echo is enabled, and for MicroDOS if the LSB is
; cleared then echo is enabled.
;
;   All the UT71 routines can do is to send or receive characters and access
; the receiver/transmitter buffer and the DR/THRE bits.  Initializing the UART
; and setting the baud rate is still handled by the BIOS alone.  This works
; well enough for now, but if we ever add any fancy features to the BIOS
; console handler (XON/XOFF support, auto XOFF, CRT or hardcopy mode, etc)
; then it might be nice to share.
;
; FLOPPY DISKETTE I/O
; -------------------
;
; REGISTER USAGE
; --------------
;   This code has been rewritten to use the Elf2K/SBC1802 register names.
; The following table of equivalences might be helpful -
;
;	Register	UT71 Name	Our Name
;	--------	---------	--------
;	   0		DMAPTR		DMAPTR
;	   1		INTPC		INTPC
;	   2		SP		SP
;	   3		PC		PC
;	   4		CALL		CALLPC
;	   5		RETN		RETPC
;	   6		LINK		A
;	   7		IOCBPTR		T1
;	   8		CMDCNT		T2
;	   9		TRKCNT		T3
;	   A				DP
;	   B				P4
;	   C				P3
;	   D		ASL		P2
;	   E		AUX		BAUD
;	   F		CHAR/PARA	P1

;   Fortunately most of the register usage is the same between UT71 and ElfOS.
; R2 is the standard stack pointer; R3 is the standard PC; R4 is the PC for
; SCRT CALL; R5 is the PC for SCRT RETURN, and R6 is the subroutine argument
; list pointer.  
;
;   The UT71 READAH routine uses RD (ASL in the UT71 code; P2 in our world) to
; assemble 16 bit hex input values.  All of the console I/O 

;--

;   This macro is used to define UT71 entry points.   The labels and addresses
; are already defined in UT71.INC, and this macro simple verifies that the
; defined entry address matches the actual address.  If it doesn't, then you
; have a problem!
#define UENTRY(vec)	\#if (vec-$)
#defcont		\	.echo "VECTOR ERROR FOR vec\n"
#defcont		\#endif

	.SBTTL	"Console Input Functions"

;++
;--
	.ORG	UTBASE+$012F

CKDEC:	ADI	7		; CHECK FOR ASCII DECIMAL
	BDF	NFND		; OUT OF RANGE
	ADI	$0A		; SUBTRACT NET 30
	BDF	FND

NFND:	CDF			; SET DF = 0

REXIT:	GHI	P1		; PUT INPUT INTO D
	RETURN			;  & RETURN

	UENTRY(READAH)
	LDI 0\ SKP		; SKIP TO READ1 HITH D=0

	UENTRY(UTREAD)
	GHI	PC		;  CONSTANT > 0
	PLO	P1		; SAVE ENTRY POINT

	GHI BAUD\ SHL		; IF COMMAND FILE IS IN CONTROL;
	LBDF	CFREAD		; GO TO IT, SKIP KEYBOARD READ
       
READ1:	INP SL0STS\ SHR
	BNF	READ1
	INP	SL0BUF		; READ CHARACTER
	ANI $7F\ PHI P1
	BZ	READ1		; IGNORE IF ITS'S A NULL
CFRET:	GHI BAUD\ SHR		; COMMAND FILE RETURN POINT
	BDF	NEXT
RDWAIT:	INP SL0STS\ SHL		; ECHO IF ECHO BIT SET
	BNF	RDWAIT
	GHI P1\ STR SP
	OUT	SL0BUF
	DEC	SP

NEXT:	GLO	P1		; CHECK ENTRY
	BNZ	REXIT		; ENTERED VIA READ

CKHXE:	GHI P1\ SMI $41		; CHECK FOR ASCII HEX
	BNF	CKDEC		; CHECK FOR ASCII DECIHAL
	SMI	6		; A THRU F
	BDF	NFND		; NO
	ADI	$10		; SUBTRACT NET 37

FND:	ANI $0F\ STXD		; SAVE TEMPORARILY
	GHI	P2		; SHIFT DATA INTO ASL
	SHL\ SHL\ SHL\ SHL\ STR SP	  ;SHL 4X
	GLO	P2
	SHR\ SHR\ SHR\ SHR
	OR\ PHI P2
	GLO	P2
	SHL\ SHL\ SHL\ SHL
	INC	SP
	OR\ PLO P2
	SDF			; SET DF = 1
	BR	REXIT

	.SBTTL	"Console Output Functions"

;++
;--
	.ORG	UTBASE+$0198
TYPED:	BR	TYPE

	.ORG	UTBASE+$019C
TYPE5D:	BR	TYPE5

	.ORG	UTBASE+$019F
TEXIT:	RETURN

	UENTRY(UTYPE5)
TYPE5:	LDA	RETPC		; PICK UP DATA
	SKP

	UENTRY(UTYPE6)
TYPE6:	LDA	A		; PICK UP DATA
	SKP

	UENTRY(UTYPE)
TYPE:	GHI P1\ STXD		; KEEP A COPY
	XRI	CH.LFD		; IS IT A LINE-FEED ?
	BNZ	TY2
	LDI	$80		; # BITS ADI # NULLS
	BR	TY3

	UENTRY(UTYPE2)
TYPE2:	GHI P1\ SHR\ SHR\ SHR\ SHR
	ADI	$F6		; CONVERT TO HEX
	BNF	TY1		; IF A OR >, ADD 37
	ADI	7

TY1:	SMI $C6\ STXD		; ELSE ADD 30
	LDI	$10		; 10 ADI NO. OF BITS
	BR	TY3

TY2:	LDI	0		; NO OF BITS

TY3:	PLO	P1

BEGIN:	INP SL0STS\ SHL
	BNF	BEGIN
	INC	SP		; PT BACK TO CHARACTER
	OUT	SL0BUF
	DEC	SP

NXCHAR:	GLO P1\ ADI $F0\ PLO P1
	BNF	TEXIT		; SEP RETPC IF NO MORE
	SMI	$10		; TEST FOR ALTERNATIVES
	BZ	TEXIT		; TYPED LAST NULL
	BNF	HEX1		; TYPED FIRST HEX
	LDI	0		; TYPED LF OR NULL
	BR	HEX3

HEX1:	GHI P1\ ANI $0F		; GET 2ND HEX DIGIT
	ADI	$F6		; CONVERT TO HEX
	BNF	HEX2		; IF A OR MORE.
	ADI	7		; ADD NET 37

HEX2:	SMI	$C6		; ELSE ADD NET 30

HEX3:	STXD			; AND SAVE
	BR	BEGIN

	.SBTTL	"Miscellaneous TBA"

	.ORG	UTBASE+$02AD

	.ORG	UTBASE+$031C
; OSTRNG
MSGE:
;;	LDI $EF\ PLO DELAY
;;	LDI $80\ PHI DELAY
MSGE1:	LDA A\ PHI P1
	BZ	EXITM
	CALL(TYPED)
	BR	MSGE1
EXITM:	RETURN

	.SBTTL	"SCRT CALL and RETURN, INIT1, and INIT2"

;++
;--
	.ORG UTBASE+$0363

; STANDARD CALL
EXITC:	SEP	PC		; GO TO IT

	UENTRY(UTCALLR)
	SEX	SP		; SET R(X)
	GHI A\ STXD		; SAVE THE CURRENT LINK ON
	GLO A\ STXD		; THE STACK
	GHI PC\ PHI A
	GLO PC\ PLO A
	LDA A\ PHI PC		; PICK UP THE SUBROUTINE
	LDA A\ PLO PC		; ADDRESS
	BR	EXITC

; STANDARD RETURN
EXITR:	SEP	PC		; RETURN TO MAIN PGM

	UENTRY(UTRETR)
	GHI A\ PHI PC
	GLO A\ PLO PC
	SEX SP\ INC SP		; SET THE STACK POINTER
	LDXA\ PLO A		; RESTORE THE CONTENTS OF
	LDX\ PHI A		; LINK
	GHI	P1		; PUT THE CONTENTS OF P1.1 INTO D
				; BEFORE RETURNING
	BR	EXITR

; REGISTER INITIALIZATION ROUTINES
ENTER1:	LDI LOW(PGMSRT)\ PLO PC
	LDI HIGH(PGMSRT)\ PHI PC

ENTER2:
;;	LDI LOW(DELAY1)\ PLO DELAY	; DELAY ROUTINE
;;	LDI HIGH(DELAY1)\ PHI DELAY
	LDI HIGH(UTCALLR)\ PHI CALLPC\ PHI RETPC
	LDI LOW(UTCALLR)\ PLO CALLPC
	LDI LOW(UTRETR)\ PLO RETPC
	LDI LOW(UTSTACK)\ PLO SP
	LDI HIGH(UTSTACK)\ PHI SP
	SEX SP\ SEP PC

	.SBTTL	"Floppy Diskette Emulation"

;++
; DISKETTE EMULATION
; ------------------
;   MicroDOS, as far as I know, supports only a single type of diskette drive
; and a single diskette format - 512 bytes/sector, 9 sectors/track, 70 tracks
; per diskette and only a single head.  That's a total of 630 sectors per
; diskette.  The standard RCA MS2000 FDC supports a maximum of four drives,
; however MicroDOS itself can be tricked into allowing as many as sixteen.
; That's all we have - there's no hard disk support, and no other diskette
; capacities or geometries are allowed.
;
;   The SBC1802 doesn't support any kind of floppy drive or controller, however
; it does support an IDE interface and up to two drives.  And even better, the
; SBC1802 BIOS supports partitioning either hard disk into arbitrary sized
; logical units.  This code emulates up to 16 diskettes using a SINGLE hard
; disk partition.  You might be tempted to emulate each diskette using a
; separate partition, however the BIOS supports a maximum of four active
; partitions at any time, and a maximum of 8 partitions per disk, and so with
; only 630 sectors per diskette image that'd be pretty limiting.
;
;  However, since diskette images are both really small AND a fixed size, it's
; easy to pack many diskette images into a single partition.  The sector number
; within the partition is simply
;
;	<partition sector> = (<diskette unit> * 630)
;			   + (<diskette track> * 9)
;			   + (<diskette sector> - 1)
;
; Sixteen diskettes require 10,080 sectors so the whole result fits in an 1802
; register and isn't too hard to calculate using double precision arithmetic.
; It's also just under 5Mb, which by hard disk standards is peanuts.
;
;
; PARAMETER BLOCK
; ---------------
;   Many of the floppy diskette I/O routines expect a pointer to a parameter
; block to be passed in P1 ("PARA" in the UT71 listing).  This block is five
; bytes and is formatted as follows -
;
;	+---------------+
; P1 ->	| COUNT |  UNIT |
;	|   PSN (HIGH)  |
;	|   PSN (LOW)	|
;	| DMAPTR (HIGH) |
;	| DMAPTR (LOW)	|
;	+---------------+
;
;   The high nibble of the first byte contains a count of sectors to transfer
; and the low nibble contains the floppy drive unit number.  The sector count
; is less one, so a count of zero transfers 1 sector and $F0 transfers sixteen.
; The MS2000 FDC only supports four drives and UT71 limits the unit number to
; 0..3, however we have no such limitations and any unit 0..F will be allowed.
;
;   PSN is the "physical sector number" and is a 16 bit binary value from 0 to
; 629 (remember that diskettes hold 70 tracks of 9 sectors each, or 630 total).
; The upper byte is barely needed (it only contains one bit, after all!) but
; a full sixteen bits are allocated.
;
;   DMAPTR is the address of the buffer which contains the data (for WRITE)
; or which receives the data (for READ) transferred.  If COUNT transfers more
; than one sector, then data is transferred consectutively in increasing PSNs.
;
;
; I/O CONTROL BLOCK
; -----------------
;   UT71 allocates an "I/O control block" in RAM at $8F00.  It's not entirely
; clear to me all the things that this space is used for, however the first
; five bytes are used by the MicroDOS bootstrap routine (see LOAD) as a 
; temporary parameter block (see above).  The bytes after that are used by the
; SEEKA (and SEEKST) routines to store FDC command bytes for the CMD routine.
; I believe a total of 16 bytes are available for the IOCB in UT71, but don't
; quote me on that one.
;
; ROUTINES
; --------

	.SBTTL	"SEEKST and SEEKA"

;++
;   The SEEKST routine takes a pointer to a parameter block in P1, and it
; extracts the unit number and PSN from the parameter block and uses them to
; build an FDC SEEK command that's stored at the end IOCB.  Using the PSN
; requires that it be converted to a track and sector number, which requires a
; division by 9.  The quotient is the track number and the remainder (plus 1
; since sectors are 1 based) is the sector number.  After computing the track
; and sector numbers, SEEKST simply falls into SEEKA to complete the setup.
;
;   The SEEKA routine is similar to SEEKST, however in this case the physical
; track and sector are passed in the P2 register (the track is in P2.1 and the
; sector in P2.0).  P1 still points to a parameter block and the unit number
; is still extracted from that parameter block.  The remainder of the parameter
; block, including the PSN bytes, are ignored, however.
;
;   The SEEKST and SEEKA routines then send a seek command to the FDC and wait
; for it to complete.  The seek status is returned in P2.0.  With our hard
; disk emulation we have no need or use for a seek command, and in our case
; all these routines do is to compute a partition relative sector number from
; the diskette unit, track and sector, and then store that partition sector
; number at the end of the IOCB.  The hard disk is not actually accessed and,
; unless the PSN and/or track/sector is illegal we always return success.
;
;   You might observe that when SEEKST converts the PSN to a track and sector
; it has to divide by nine, and when we convert the track and sector back to
; a partition sector we have to multiply by nine.  You might thus be tempted
; to do away with SEEKA and skip that step, but there are MicroDOS routines
; that call SEEKA directly to address the diskette by track/sector, and we
; need to keep it.
;
;   Lastly, note that the buffer address and the sector count in the parameter
; block are never used by either of these routines.  They are used later in 
; the read and write routines, but not here...
;--
	.ORG	UTBASE+$0400

;       PARAMETER BLOCK POINTER: @ PARA = UNIT NO.
;         	                  @ +1 = PSN HIGH BYTE, @ +2 = LOW BYTE
; 				  @ +3 = BUFFER ADDRESS H.B; @ +4 = L.B
;       SEEKST USES IOCBPTR.0 FOR COUNTER,.1 FOR TEMP STORE.
; 	 AUX.0 AS DIVIDEND H.B., ASL.1 FOR TRACK #, ASL.0 FOR SECTOR # (-1)
; 	 ASL.0 HOLDS TERMINATION RESULT AT END

; Convert the PSN in the parameter block to a track and sector in P2 ...
SEEKST:	LDI 0\ PHI P2		; CLEAR FUTURE RESULT
	INC P1\ LDA P1\ PLO BAUD; save the PSN high byte temporarily
	LDN P1\ PLO P2		; and put the low byte in P2
	DEC P1\ DEC P1		; back up the parameter block pointer
	LDI HIGH(9*64)\ STXD	; DIVISOR = 9 SHIFTED LEFT 6 TIMES
	LDI LOW(9*64)\ STR SP
	LDI 7\ PLO T1		; SUBTRACT AND SHIFT 7 TIMES
SUBLP:	GLO P2\ SM\ PHI T1
	INC SP\ GLO BAUD\ SMB	; DIVIDEND - DIVISOR
	BM	SHRES		; IF NOT -
	PLO BAUD\ GHI T1\ PLO P2; STORE NEH DIVIDEND
SHRES:	GHI P2\ SHLC\ PHI P2	; SHIFT NO BORROW INTO RESULT
	LDX\ SHR\ STXD
	LDX\ SHRC\ STR SP	; SHIFT DIVISOR RIGHT
	DEC T1\	GLO T1
	BNZ	SUBLP		; LOOP 7 TIMES
	INC	SP			    ;FIX STACK POINTER

; Convert unit, track and sector to partition relative sector number ...
SEEKA:
#ifdef UNUSED
	LDI	LOW(IOCB+10)
	PLO	T1	    ;POINT AT IOCB DTL VALUE
	LDI	HIGH(IOCB+10)
	PHI	T1
	SEX	T1
	LDI	DTL
	STXD			 ;DTL
	LDI	GPL3
	STXD		    ;GPLB
	LDI	EOT
	STXD		    ;EOT
	LDI	N
	STXD			;N
	GLO	P2
	ADI	1
	STXD		    ;SECTOR + 1
	LDI	00H
	STXD		    ;HEAD 0
	GHI	P2
	STXD		    ;TRACK
	LDN	P1
	STXD		    ;HEAD AND UNIT
	LDI	3
	PLO	CMDCNT		    ;3 COMMAND BYTES IN SEEK
	GHI	P2
	BNZ	SEEK5	    ;IF TRACK IS 0.
	DEC	CMDCNT		    ;ONLY 2 COMMAND BYTES
	LDI	RCCMD		    ;AND RECAL COMMAND INSTEAD
	LSKP
SEEK5	LDI	SKCMD
	STXD		    ;OF SEEK COMMAND
	LDI	BC
	STXD			    ;BYTE COUNT
	LDI	DMANOP
	STR	T1		    ;DMANOP
	SEP	CALL
	.DW	CMD
	SEP	CALL
	.DW	WAIT
	LDI	HIGH(STA0)
	PHI	T1	       ;POINT AT RESULT STATUS
	LDI	LOW(STA0)
	PLO	T1
	LDN	T1
	ANI	0C0H	       ;NORMAL TERMINATION ?
	BZ	SEEK40		    ;EXIT IF YES
	LDN	T1
	ANI	10H	    ;GET DRIVE FAIL BIT
	SHL
	STR	SP		    ;LINE UP FOR STATUS
	LDN	T1
	ANI	08H	    ;GET DRIVE INACTIVE BIT
	SHL
	SHL
	SHL
	OR			    ;COMBINE WITH ABOVE
	LSNZ
	LDI	02H		    ;IF NEITHER: GET BAD TERM BIT
SEEK40	PLO	P2
	SEP	RETN		    ;LOAD RESULT STATUS: AND EXIT
#endif

UNIOFF:	.WORD	 0*MO.SPD
	.WORD	 1*MO.SPD
	.WORD	 2*MO.SPD
	.WORD	 3*MO.SPD
	.WORD	 4*MO.SPD
	.WORD	 5*MO.SPD
	.WORD	 6*MO.SPD
	.WORD	 7*MO.SPD
	.WORD	 8*MO.SPD
	.WORD	 9*MO.SPD
	.WORD	10*MO.SPD
	.WORD	11*MO.SPD
	.WORD	12*MO.SPD
	.WORD	13*MO.SPD
	.WORD	14*MO.SPD
	.WORD	15*MO.SPD

	.SBTTL	"READTR/WRITTR, READST/WRITST, and READA/WRITA"

;++
;   There are six routines, three sets of read/write pairs, that do diskette
; I/O.  ALL OF THEM assume a previous call to either SEEKST or SEEKA to setup
; the correct diskette unit, track and sector (or in our case, the partition
; relative sector number).  READTR/WRITTR and READST/WRITST require that a
; parameter block pointer be passed in P1 (presumably the same parameter
; block you passed to SEEKST/SEEKA), however READA/WRITA does not.
;
;   READTR/WRITTR extracts a sector count from the most significant nibble
; of the first byte in the parameter block, and this determines the number of
; bytes to be transferred.  It then falls into the READST/WRITST code to set
; up the DMA pointer, below.
;
;   READST/WRITST always transfer only a single sector, however they will
; extract the buffer address from the fourth and fifth bytes of the parameter
; block and load it into the DMAPTR register (register 0).  This is the memory
; address used to transfer data.
;
;   READA/WRITA always transfer only a single sector, AND they also use the
; buffer address already contained in register DMAPTR.  The parameter block
; is not used and P1 is ignored.
;
;   All six routines return the completion status in P2.0.  The BIOS hard disk
; routines return the drive status byte in D in the event of an error, but we
; don't go to much trouble trying to convert this to a floppy error code.  If
; there's any hard disk error, we just return the $20 bit - "DRIVE FAIL".

READST:
WRITST:
READTR:
WRITTR:
READA:
WRITA:
	.SBTTL	"RECAL, CMD, WAIT and other floppy routines"


; RECALIBRATE (a NOP for the CF card) ...
RECAL:	RETURN

; COMMAND ROUTINE OUTPUTS COMMAND WORDS FROM @ IOCB FOR NUMBER
; SPECIFIED IN CMDCNT.0. IT CLEARS SERVICE REOUEST FROM 765 FIRST.
; AND CLEARS OUT ANY RESULTS THAT ARE PENDING
;
; Not clear what, if anything, this needs to do for the CF card ...
CMD:	RETURN

; FOR COMPATABILITY WITH UT21 LINE PRINTER ROUTINE
	.ORG	UTBASE+$050E
	UENTRY(LINEPR)\ RETURN	; TBA!!

; WAIT ROUTINE WAITS FOR SERVICE REOUEST FROM 765. TIMES OUT IF NONE
; IF RESULT OF READ/WRITE. INPUTS STATUS BYTES. IF RESULT OF SEEK OR
; RECAL. DOES SENSE INTERRUPT STATUS FIRST. IF WRONG DRIVE. REPEATS
; USES CRC DMA CYCLE TO CLEAR DMA REOUEST IN CASE OF SERIOUS OVER-RUN.
WAIT:	RETURN

	.SBTTL	"Entry Vectors"

	.ORG	UTBASE+$03F0
	UENTRY(OSTRNG) \ LBR MSGE
	UENTRY(UTINIT1)\ LBR ENTER1
	UENTRY(UTINIT2)\ LBR ENTER2
	UENTRY(GOUT71) \ LBR F_MINIMON
	UENTRY(UTCKHEX)\ LBR CKHXE

	.ORG	UTBASE+$07D8
	UENTRY(CFRETS)\	 LBR CFRET	; COMMAND FILE RETURN POINT
	UENTRY(READTRS)\ LBR READTR	; READ MULTIPLE SECTOR
	UENTRY(WRITTRS)\ LBR WRITTR	; WRITE AS ABOVE
	UENTRY(READAS)\  LBR READA	; READ SECTOR USING ALREADY SET DMA
	UENTRY(WRITAS)\	 LBR WRITA	; WRITE SECTOR AS ABOVE
	UENTRY(RECALS)\	 LBR RECAL	; RECALIBRATE (NOT NEEDED!)
	UENTRY(SEEKAS)\	 LBR SEEKA	; SEEK BY TRACK & SECTOR IN ASL
	UENTRY(UTCMDS)\	 LBR CMD	; OUTPUT COMMAND BYTES (NOT NEEDED?)
	UENTRY(RENTERS)\ LBR GOUT71	; UT71 RENTRY (DUPLICATES GOUT71?)
	UENTRY(UTWAITS)\ LBR WAIT	; SERVICE FDC AFTER COMMAND
	UENTRY(UTSEEKS)\ LBR SEEKST	; SEEK BY PSN IN PARA. BLOCK
	UENTRY(UTREADS)\ LBR READST	; READ SECTOR. SET DMA FROM PARA. BLOCK
	UENTRY(UTWRITS)\ LBR WRITST	; WRITE SECTOR AS ABOVE
	UENTRY(UTCHECK)\ .BYTE $FF	; CHECKSUM BYTE (UNUSED)

	.EJECT
	.END
