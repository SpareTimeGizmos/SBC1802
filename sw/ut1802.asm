	.TITLE	 UT71 Emulation for Spare Time Gizmos SBC1802
;	 Bob Armstrong [06-OCT-2021]

; the regular OS only seems to use SEEKST, READST and WRITST ...
; Even DIAG uses only these.
; FORMAT uses SEEKA, CMD and WAIT.
; SYSGEN;E uses CMDS and WAITS! (even to seek and read sectors)

;++
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
; to container files on an ElfOS formatted mass storage device.
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
	
	.SBTTL	"Revision History"

;++
; REVISION HISTORY
;
; 001	-- Start from scratch!
;
; 002   -- Make the RECAL function execute F_SDRESET ...
;
; 003	-- Fix error in LBA calculation for SEEKST ...
;
; --
VERMAJ	.EQU	1	; major version number
VEREDT	.EQU	2	; and the edit level

	.SBTTL	"Implementation Notes"

;++
; MEMORY LAYOUT
;--------------
;   The original MS2000 UT71 ROM was addressed from $8000 to $87FF and the
; system also had 256 bytes of RAM from $8C00 to $8CFF.  The latter was used to
; store user registers and for stack space, and it's not clear to me that any
; of the remaining space from $8800 thru $8FFF was actually used.  The MS2000
; had RAM from $9000 thru $BFFF to hold the MicroDOS kernel and optionally,
; RAM from $C000 thru $FFFF could be used as additional program space for
; MicroDOS programs.
;
;   The SBC1802, using the MicroDOS memory map, maps the EPROM from $8000 to
; $87FF and also from $F000 thru $FFFF (excepting the space at $FExx set aside
; for firmware RAM, PIC, RTC and memory control).  The space from $8800 to
; $EFFF is all RAM and we really don't care how it's used.  The tricky bit then
; is to fit our UT71 emulation into the SBC1802 EPROM from $8000 thru $87FF.
; The space from $F000 up contains the same BIOS that's used by ElfOS and,
; although MicroDOS doesn't care about this, our UT71 emulation can call that
; code for access to mass storage devices.
;
;   The UT71 ROM has a couple of vector tables, one at $83F0 and another at
; $87D8, but unfortunately it also has several entry points scattered randomly
; around the address space.  We have no choice but to work around these.  The
; regular SBC1802 firmware also uses the space from $8000 thru $80FF for the
; cold start vector, warm start vector, copyright notice, and version. The
; first UT71 entry point that we have to match up with is at $80EE for DELAY,
; so it's a close call but we're OK for now.
;
; SCRT
; ----
;   The UT71 ROM has its standard call and return routines and, although they
; are pretty close to the ElfOS BIOS versions, they're not identical.  The
; stack, PC and CALL/RETURN register usage is exactly the same, however the
; ElfOS BIOS versions preserve D across CALL and RETURN by using RE.0 as
; temporary storage.  The UT71 versions don't save D and, worse, there are
; several UT71 routines that expect to pass or return a value in RE.0.  These
; include all READ and TYPE routines, OSTRNG, and CKHEX.
;
;   Currently we provide exact copies of the UT71 CALL and RETURN routines at
; the exact same addresses, and these are set up by the INIT1/2 functions we
; provide.  This is probably pointless because MicroDOS contains yet another
; set of SCRT CALL/RETURN routines (which also do not save D!) and I don't
; believe our UT71 versions will ever be used, but we have plenty of room.
;
;   All the console I/O functions, and random miscellaneous functions like
; CKHEX, et al, expect to use the MicroDOS/UT71 SCRT routines.  The diskette
; I/O functions are a different matter, but more on that later.
;
; CONSOLE I/O
; -----------
;   The UT71 console I/O functions - READ, READAH, TYPE5, TYPE6, TYPE, TYPE2,
; and OSTRNG - all access the CDP1854 console serial port SLU0 directly. I'm
; tempted to rewrite these to use the BIOS functions, but most of those pass
; the character in D and thus work only with the ElfOS versions of SCRT. There
; is also an incompatibility in the handling of the local echo bit - both ElfOS
; and UT71 use the LSB of RE.1, however the sense is reversed.  For ElfOS if
; the RE.1 LSB is set then echo is enabled, and for MicroDOS if the LSB is
; cleared then echo is enabled.  And finally, MicroDOS uses the MSB of RE.1
; for a command file flag, which the ElfOS BIOS would trash.
;
;   So there's really no point in changing the UT71 console I/O routines.
; The SBC1802 has a CDP1854 UART at the same port addresses and I/O group as
; the RCA system, and we can keep all the UT71 console I/O functions pretty
; much verbatim with one minor change.  Our version selects the base board
; I/O group before attempting to access SLU0; the RCA version doesn't.  RCA
; seems to assume that anything which changes the I/O group will always change
; it back, but we don't make that assumption!
;
; FLOPPY DISKETTE I/O
; -------------------
;   There are no floppy diskettes here, of course, but we emulate up to eight
; virtual diskette drives using container files on either the IDE/ATA drive
; or a TU58 drive.  And yes, TU58 drives work for this purpose albeit a bit
; slowly.  For more discussion on this topic, see the diskette I/O routines
; at the end of this file.
;
;   MicroDOS, as far as I know, supports only a single type of diskette drive
; and a single diskette format - 512 bytes/sector, 9 sectors/track, 70 tracks
; per diskette and only a single head.  That gives a total of 630 sectors per
; diskette.  The standard RCA MS2000 FDC supports a maximum of four drives,
; however MicroDOS itself can be tricked into allowing as many as nine drives,
; 0 thru 8.  Don't ask my why it's nine and not eight!  In any case, that's
; all we have - there's no hard disk support, and no other diskette capacities
; or geometries are allowed.
;
;   The SBC1802 can map up to 8 virtual diskette units onto any contiguous
; 630 sector block on any ElfOS mass storage device.  There is a table stored
; in RAM at UTDKMAP which contains 4 bytes for each virtual diskette drive.
; These four bytes are -
;
;	+--------------------------+
; 	| SDUNIT | LBA (bits 27-24 |
;	|     LBA (bits 23-16)     |
;	|     LBA (bits 15- 8)     |
;	|     LBA (bits  7- 0)     |
;	+--------------------------+
;
;  "SDUNIT" is the four bit storage device number passed in calls to the BIOS
; F_SDREAD, F_SDWRITE, etc routines, and the remaining part of the four bytes
; is a 28 bit LBA offset for the container file.  Conntainer files must be
; contiguous, so the code in this module just needs to add the MicroDOS PSN
; to the starting LBA from the table to get the correct storage device address.
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
;	   B		CMDPTR		P4
;	   C		DELAY		P3
;	   D		ASL		P2
;	   E		AUX		BAUD
;	   F		CHAR/PARA	P1

;   Fortunately most of the register usage is the same between UT71 and ElfOS.
; R2 is the standard stack pointer; R3 is the standard PC; R4 is the PC for
; SCRT CALL; R5 is the PC for SCRT RETURN, and R6 is the subroutine argument
; list pointer.  
;--

;   This macro is used to define UT71 entry points.   The labels and addresses
; are already defined in UT71.INC, and this macro simply verifies that the
; defined entry address matches the actual address.  If it doesn't, then you
; have a problem!
#define UENTRY(vec)	\#if (vec-$)
#defcont		\	.echo "VECTOR ERROR FOR vec\n"
#defcont		\#endif

	.SBTTL	"Delay Routine"

;++
;   UT71 contains a DELAY routine that generates (what else?) programmed delays.
; The delay constant is passed inline after the call -
;
;CALL:
;	SEP	RC		; RC == P3 == DELAY
;	 .BYTE	 constant	; delay constant
;
; I'm tempted not to implement this at all, but the MicroDOS kernel actually
; calls it whenever a line feed is output to the terminal.  It's doing this
; to add filler time for slow printers, like an ASR-33, and we couldn't care
; less, but since MicroDOS calls it we have to have it.
;--
	.ORG	$UTDELAY-1
DLYXIT:	SEP	PC		; called via SEP RD, return via SEP R3
	UENTRY(UTDELAY)
DLY:	LDA	PC		; fetch the delay constant
DLY2:	SMI	1		; count it down
	BZ	DLYXIT		; and loop until it's zero
	BR	DLY2		; then return

	.SBTTL	"Console Input Functions"

;++
;   UT71 provides three console input functions (at least there are three that
; are documented!) -
;
; READ   $813E -> read ASCII character to RF.1
; READAH $813B -> same as READ, but hex value to RD
; CKHEX  $83FC -> RF.1 (ASCII) -> RE.0 (hex) DF=0 if not hex
;
;   Note that CKHEX actually has a vector in the $83F0 table, and that LBRs
; to CKHXE here.  No idea why the others don't get vectors too.
;
;   I would say that this is some of the ugliest and most convoluted code I've
; seen, but the output stuff on the next page is worse.  I'd like to rewrite
; it, but there's no telling what might depend on certain entry points being
; at certain addresses (note, for example, that READ and READAH don't have
; vectors!).  So, this is the RCA code reproduced as-is.
;--
	.ORG	UTBASE+$012F

CKDEC:	ADI	7		; CHECK FOR ASCII DECIMAL
	BDF	NFND		; OUT OF RANGE
	ADI	$0A		; SUBTRACT NET 30
	BDF	FND

NFND:	CDF			; SET DF = 0

REXIT:	GHI	P1		; PUT INPUT INTO D
	RETURN			;  & RETURN

	UENTRY(UTREADAH)
	LDI 0\ SKP		; SKIP TO READ1 HITH D=0

	UENTRY(UTREAD)
	GHI	PC		;  CONSTANT > 0
	PLO	P1		; SAVE ENTRY POINT

	GHI BAUD\ SHL		; IF COMMAND FILE IS IN CONTROL;
	LBDF	UTCFREAD	; GO TO IT, SKIP KEYBOARD READ
       
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
;   UT71 implements half a dozen or so console terminal output functions -
;
; TYPE5  $81A0 -> output ASCII from M(R5)
; TYPE6  $81A2 -> output ASCII from M(R6), increment R6
; TYPE   $81A4 -> output ASCII from RF.1
; TYPE2  $81AE -> output two hex digits from RF.1
; OSTRNG $83F0 -> output inline ASCIZ string
;
;   Note that OSTRNG is the only one that actually as a vector in the table
; at $83F0.  The rest all have fixed addresses where they MUST be.
;
;   This is really ugly code, and good luck trying to unravel it.  Not going
; to change it though, because of all the hardwired addresses that we must
; line up with.
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

	.SBTTL	"Miscellaneous Routines"

;++
;   The RENTER routine is supposed to restart UT71, but in our case we'll
; restart the SBC1802 monitor instead ...
;--
	.ORG	UTBASE+$02AD
RENTER:	LBR	F_MINIMON


;++
; The OSTRNG routine types an ASCIZ string passed inline after the call...
;--
	.ORG	UTBASE+$031C
MSGE:	RLDI(RC, UTDELAY)
MSGE1:	LDA A\ PHI P1
	BZ	EXITM
	CALL(TYPED)
	BR	MSGE1
EXITM:	RETURN

	.SBTTL	"SCRT CALL and RETURN, INIT1, and INIT2"

;++
;   These are the UT71 versions of the RCA SCRT routines.  They're very much
; similar to the ElfOS BIOS version, with the notably exception that the UT71
; versions DON'T preserve D where as the ElfOS BIOS versions do!
;--
	.ORG UTBASE+$0363

; Standard call routine ...
EXITC:	SEP	PC		; return (to the new subroutine that is!)
	UENTRY(UTCALL)		; UT71 standard CALL routine
	SEX	SP		; ...
	GHI A\ STXD		; save the current return address ...
	GLO A\ STXD		;  ... on the stack
	GHI PC\ PHI A		; the caller's PC is ...
	GLO PC\ PLO A		;  ... the new return address
	LDA A\ PHI PC		; get the subroutine address
	LDA A\ PLO PC		;  ...
	BR	EXITC		; jump there and we're done

; Standard return routine ...
EXITR:	SEP	PC		; return to the original caller
	UENTRY(UTRET)		; UT71 standard RETURN routine
	GHI A\ PHI PC		; the argument pointer is
	GLO A\ PLO PC		;  ... the address we return to
	SEX SP\ INC SP		;  ...
	LDXA\ PLO A		; pop the old argument pointer ...
	LDX\ PHI A		;  ... off the stack
;   This is a UT71/MicroDOS specific hack - rather than saving and restoring
; D, it always returns the contents of RF.1 in D.  This is handy for some of
; the console I/O routines, but I don't think it helps anybody else.
	GHI	P1		; return RF.1 in D
	BR	EXITR		; and back to the caller


;++
;   These three routines, INIT, ENTER1 (aka INIT1) and ENTER2 (aka INIT2),
; are used to set up the UT71 SCRT registers.  I'm not sure these are ever
; used, but we'll duplicate them just in case.
;
;   INIT in particular loads the PC with the address of the UT71 startup
; routine, but that won't work here so we use the SBC1802 monitor instead.
; Once again, I don't think this is ever used.
;--
INIT:	RLDI(PC, F_MINIMON)
	BR	ENTER2

; Register initialization routines ...
ENTER1:	RLDI(PC,$0005)
ENTER2:	RLDI(RC,UTDELAY)	; DELAY ROUTINE
	LDI HIGH(UTCALL)\ PHI CALLPC\ PHI RETPC
	LDI LOW(UTCALL)\ PLO CALLPC
	LDI LOW(UTRET)\ PLO RETPC
	RLDI(SP,UTSTACK)
	SEX SP\ SEP PC

	.SBTTL	"Primary Entry Point Table"

;++
;   This is the first of two UT71 vector table, the second one is at $87D8.
; The vector tables allow external code to have fixed entry points for this
; ROM, but still allow us to move code around as necessary.  Needless to say,
; this is a great idea!  Sadly, a lot of UT71 doesn't follow it...
;--

	.ORG	UTBASE+$03F0
	UENTRY(UTOSTRNG)\   LBR MSGE
	UENTRY(UTINIT1)\    LBR ENTER1
	UENTRY(UTINIT2)\    LBR ENTER2
	UENTRY(UTGOUT71)\   LBR RENTER
	UENTRY(UTCKHEX)\    LBR CKHXE

	.SBTTL	"MicroDOS Bootstrap"

;++
;   This routine is called from the SBC1802 monitor UBOOT command to finish
; starting MicroDOS.  When we get here, PC=R0, the MC.MDOS MicroDOS memory map
; has been selected,  and the MicroDOS kernel has already been loaded into RAM
; from $9000 to $BFFF.  We need to set up the rest of the MicroDOS context,
; including the SCRT CALLPC and RETPC registers, load R3 with the starting
; address for the kernel, and then jump there.
;
;   Note that the original UT71 ROM contained a bootstrap too, but it was
; nothing like this version.  This is unique to the SBC1802.
;--
	.ORG	UTBASE+$0400

	UENTRY(UTBOOT)
	SEX R0\ OUT GROUP	; be sure I/O group 1 is selected
	 .BYTE BASEGRP		; ...
	RLDI(T1,UDECHO)		; set the TTY echo flag in MicroDOS
	LDI 0\ STR T1		;  (0 -> echo terminal input)
	RLDI(BAUD,0)		;  ... here too
	RLDI(SP,UDSTACK)	; set the MicroDOS stack pointer
	RLDI(CALLPC,UDCALL)	; and set up the MicroDOS ...
	RLDI(RETPC,UDRETURN)	;  ... call and return SCRT routines
	RLDI(P3,UTDELAY)	; point RC at the delay routine
	RLDI(PC,UDENTRY)	; load the kernel entry point
	SEP	PC		; and start it up

	.SBTTL	"SEEKST/SEEKA"

;++
;   On the real MS2000, the SEEKST/SEEKA routines seek the selected diskette
; drive to the correct track.  SEEKST requires a pointer to a parameter block
; (see below) containing the PSN ("physical sector number" - the MicroDOS name
; for what we would call an LBA), which SEEKST uses to compute a track and
; sector.  SEEKA takes an already computed track and sector argument in P2
; (aka ASL); the track is in P2.1 and the sector (-1) is in P2.0
;
;   The parameter block passed to SEEKST looks like this
;
;	+---------------+
; P1 ->	| COUNT |  UNIT |
;	|   PSN (HIGH)  |
;	|   PSN (LOW)	|
;	| DMAPTR (HIGH) |
;	| DMAPTR (LOW)	|
;	+---------------+
;
; The parts we're interested in here are the unit, and the PSN (high and low).
; In the real UT71 SEEKST decomposes the PSN into a track and sector and then
; joins the SEEKA code.  But in our case the PSN (aka LBA) is already what we
; want and we do the reverse - SEEKA converts a track and sector into a PSN
; and then joins the SEEKST code.
;
;   Since diskettes have 9 sectors/track, SEEKA needs to multiply the track
; number by 9 and then add the sector-1.  Computing 9x is of course equal to
; 8x+x, so this is a matter of three bit shifts and an add.  And FWIW, SEEKA
; is passed a pointer to the same parameter block in P1, however only the unit
; is used; the PSN is ignored.
;
;   SEEKST needs to use the unit to lookup the correct virtual diskette drive
; mapping; this gives it the ElfOS storage device number and also the offset
; for this diskette image on that storage device.  SEEKST then adds the PSN
; to the offset to get the actual LBA for accessing the ElfOS storage device.
; This isn't hard but requires some 32 bit math for the computation.
;
;   After all is done, we leave the results in RAM at UTIOCB for the actual
; read or write routines to use later.
;
;   The success (zero) or failure (non-zero) code is returned in P2.0.  The
; original parameter pointer, P1, MUST BE PRESERVED.  I'm not clear on what
; other registers must be preserved, but the original UT71 code uses and
; trashes T1 (IOCBPTR), P2 (ASL), BAUD.0 (AUX.0) and P4 (CMDPTR) so those
; should be safe.
;--

; SEEKA - convert the track and sector in P2 (AUX) to a PSN also in P2.
;   Note that we can be clever and save a few bytes here, because we know
; that the highes track number is 69 ($45) so the first SHL can never
; generate a carry and we don't need to worry about that.  For the second
; and third shifts, however, we do need to worry about a carry.
SEEKA:	GHI P2\ SHL\ SHL\ PLO T1; get the track number x4
	LDI 0\ SHLC\ PHI T1	; handle the carry
	GLO T1\ SHL\  PLO T1	; and x2 one more time
	GHI T1\ SHLC\ PHI T1	;  ...
	GHI P2\ STR SP\ SEX SP	; get the original track number again
	GLO P2\ ADD\ STR SP	; and add in the sector number
	GLO T1\ ADD\ PLO P2	; add that to 8*track
	GHI T1\ ADCI 0\ PHI P2	; handle the carry
	BR	SEEKS1		; PSN is in P2 now

; SEEKST - load the PSN into P2 ...
SEEKST:	INC	P1		; skip the unit number for now
	LDA P1\ PHI P2		; store the PSN high
	LDN P1\ PLO P2		; ... and low
	DEC P1\ DEC P1		; restore P1

; Test the PSN for validity - it must be .LT. UD.SPD (630) ...
SEEKS1:	GLO P2\ SMI  LOW(UD.SPD)  ; subtract the low part
	GHI P2\ SMBI HIGH(UD.SPD) ;  ... and the high part
	BDF	SKDER		; "drive error" if no borrow here

;   Fetch the unit from the parameter block and index into the virtual diskette
; table.  If this unit isn't mapped, then return an error now.  If it is, then
; store the storage device number and quad add the PSN to the LBA offset.
	LDN P1\ ANI $08		; only allow units 0-7
	BNZ	SKDNR		; return "drive not ready" for any otherss
	LDN P1\ ANI $07		; get the unit again
	SHL\ SHL		; multiply by four
	ADI LOW(UTDKMAP+3)\ PLO T1; index into the diskette unit table
	LDI HIGH(UTDKMAP)\  PHI T1; ...
	RLDI(P4,UTIOCB+4)	; store the results here

;   T1 now points to the LAST byte in the mapping entry for this diskette unit.
; The last byte is the least significant of the storage device offset LBA
; and we need to add that to the 16 bit PSN currently stored in P2 and then
; store the result in the IOCB pointed to by P4.  Whew!  Remember that the
; 1802 stores the most significant byte first, so we have to work backwards
; here ...
	GLO P2\ SEX T1\ ADD	; add the low byte
	STR P4\ DEC P4\ DEC T1	; store that result
	GHI P2\ ADC   \ STR P4	; add and store the next byte
	DEC P4\ DEC T1		; ...
	LDI 0\ ADCI 0\ STR P4	; third byte
	DEC P4\ DEC T1		; ...
	LDI 0\ ADCI 0		; ...
	ANI $0F\ STR P4		; fourth byte only has 4 bits!

;   Now we need to handle the storage device unit number.  T1 still points to
; the first byte of the mapping entry (that's the one with the unit number) and
; if this byte is $FF then the diskette is not mapped and we return "drive not
; ready".  Otherwise we extract the storage unit from the LEFT four bits of
; this byte, shift it right, and store it in the IOCB (which is still pointed
; to by P4)...
	LDN T1\ XRI $FF		; is the unit byte $FF?
	BZ	SKDNR		; yes - "drive not ready" error
	LDN	T1		; get the unit back again
	SHR\ SHR\ SHR\ SHR	; right justify it
	ANI	$0F		; only allow four bits
	DEC P4\ STR P4		; and store in the IOCB

; Success!
	LDI 0\ PLO P2\ RETURN	; return zero for success

; Various error returns ...
SKDER:	LDI FD.DER\ LSKP	; return "drive error"
SKDNR	LDI FD.DNR		; return "drive not ready"
	PLO P2\ RETURN		; error code in P2.0 and we're outta here

	.SBTTL	"Recalibrate Command"

;++
;   With a real floppy, the recalibrate command seeks to track zero.  That's
; meaningless here, but the point is that MicroDOS calls RECALibrate after
; any I/O error is reported.  What does make sense, given the circumstances,
; is to do F_SDRESET on the drive.  This is particularly helpful for TU58s,
; since it will clear any protocol errors.
;
;   MicroDOS passes the diskette drive number in R0.0.  Yes, that's an odd
; place, that's where it is.
;--
RECAL:	GLO R0\ ANI $08		; make sure the unit is 0..7
	BNZ	SKDNR		; otherwise return "drive not ready"
	GLO R0\ ANI $07		; get the unit number again
	SHL\ SHL		; multiply by four
	ADI LOW(UTDKMAP)\  PLO T1; and index into the UTDKMAP table
	LDI HIGH(UTDKMAP)\ PHI T1; ...
	LDN T1\ XRI $FF		; is this unit even mapped?
	BZ	SKDNR		; "drive not ready" error if not

;   We need to go thru the same gymnastics to call SDRESET as we would fo
; SDREAD/SDWRITE, so save the MicroDOS SCRT and switch to our own.
	SEX SP\ PUSHR(CALLPC)	; save pointers to the ...
	PUSHR(RETPC)		;  ... MicroDOS SCRT
	RLDI(CALLPC,F_CALL)	; and switch to our own versions instead
	RLDI(RETPC, F_RETURN)	; ...

; Get the storage device unit and call F_SDRESET ...
	LDN T1\ ANI $F0		; get the storage device unit
	SHR\ SHR\ SHR\ SHR	; right justify it
	CALL(F_SDRESET)		; and away we go!

;  We don't bother worrying about an error return here; either it works or it
; doesn't, but there's not much we can do about it.  Restore the original
; SCRT pointers and return ...
	SEX SP\ IRX		; restore the registers
	POPR(RETPC)		; MicroDOS SCRT CALL routine
	POPRL(CALLPC)		; ... and return
	OUTI(GROUP, BASEGRP)	; restore the I/O group
	RETURN			; and we're done!

	.SBTTL	"Line Printer Output"

;++
;   This doesn't really belong here, with the diskette code, but the UT71
; ROM wants the entry point for the line printer driver at $850E, and here
; we are.  Fortunately we have plenty of room, so we can allocate a whole
; page to the printer driver if we want.
;
;   The MS2000 had a parallel printer interface and in the original UT71 code
; this routine would output one ASCII character from RF.1 to the printer.
; The SBC1802 has a Centronics printer port (of sorts) if the expansion card
; is installed and this could conceivably do the same, but we haven't bothered
; to implement it yet.
;
;   Right now, printing is just a NOP that does nothing...
;--
	.ORG	UTBASE+$050E

	UENTRY(UTLINEPR)
	RETURN				; TBA!!  someday...

	.SBTTL	"READTR/WRITTR, READST/WRITST, and READA/WRITA"

;++
;   There are six routines, three sets of read/write pairs, that do diskette
; I/O.  ALL OF THEM assume a previous call to either SEEKST or SEEKA to setup
; the correct diskette unit, track and sector (or in our case, the correct
; storage device and LBA).
;
;   READST/WRITST are the "standard" verssions and assume a pointer to a
; parameter block in P1 ("PARA" in the UT71 listing).  This block is five
; bytes and is identical to the one described above, under SEEKST/SEEKA.
; The only thing we use that block for here is to extract the DMA address
; for data transfer.  READST/WRITST always trasnfer exactly one sector.
;
;   READTR/WRITTR are identical, excep that they transfer multiple sectors.
; The sector count is stored in the upper 4 bits of the parameter block
; (same byte as the drive number).  Like READST/WRITST, the DMA transfer
; address is also extracted from the parameter block.
;
;   Finally, READA/WRITA expect that the transfer address is already contained
; in register R0.  These routines always transfer exactly one sector, and
; the parameter block is effectively ignored.  In this case there's nothing in
; there that we need.
;
;   Note that the ultimate goal here is just to call the ElfOS BIOS F_SDREAD
; or F_SDWRITE routine, but it's bit tricky.  First, we have to switch SCRT
; routines because ElfOS preserves D and the MicroDOS versions don't.  The
; BIOS calls want the unit number passed in D, and more over, the BIOS routines
; themselves may call other routines that expect D to be preserved.  Simplest
; thing is to switch SCRT functions and restore them on return.  We can and do
; use the caller's stack, however.
;
;   All six routines return the completion status in P2.0.  The BIOS hard disk
; routines return the drive status byte in D in the event of an error, but we
; don't go to much trouble trying to convert this to a floppy error code.  If
; there's any hard disk error, we just return the $20 bit - "DRIVE FAIL".
;
;   P1 must be unchanged and left pointing to the first byte of the parameter
; block on return.  R0 is used to temporarily hold the transfer address, but
; the real UT71 does that too.  T1 (IOCBPTR) and T2 (CMDCNT) are used and not
; preserved.  And lastly, P2.0 is used as sector counter, but that's destroyed
; by the return code anyway.
;
;   To save code, everything is shared between read and write operations until
; the last possible moment.  DF is used as a flag here - DF=0 --> read, and
; DF=1 --> write.  We have the careful while saving and restoring registers
; that nothing changes DF.
;--

; READTR/WRITETR - load the sector count into D ...
READTR:	LDN	P1		; get the unit/count byte
	SHR\ SHR\ SHR\ SHR	; right justify the sector count
	CDF\ BR READS1		; DF=0 for read and join the common code
WRITTR:	LDN	P1		; same deal to get the count
	SHR\ SHR\ SHR\ SHR	; ...
	SDF\ BR READS1		; DF=1 for write and set up the DMA

; READST/WRITST - read or write exactly one sector ...
READST:	LDI 1\ CDF		; sector count = 1 and DF=0 for read
	BR	READS1		; set up the DMA address
WRITST:	LDI 1\ SDF		; sector count = 1 and DF=1 for write
				; and fall into READS1 ...

; Extract the DMA address from the parameter block and store in R0 ...
READS1:	ANI	$0F		; trim the sector count to 4 bits
	PLO	P2		; and save the sector count in P2.0
	INC P1\ INC P1\ INC P1	; point to the DMA addres high byte
	LDA P1\ PHI R0		; save that
	LDN P1\ PLO R0		; and the low byte
	DEC P1\ DEC P1		; restore P1
	DEC P1\ DEC P1		; ...
	BR	DOIOT		; proceed ...

; READA/WRITA - transfer 1 sector using the DMA address already in R0 ...
READA:	CDF\ LSKP		; DF=0 for a read
WRITA:	SDF			; DF=1 for a write
	LDI 1\ PLO P2		; transfer one sector
				; and join the common setup code 

;   Common read/write code - save P1, CALLPC, and RETPC.  Setup our SCRT
; pointers and DON'T change DF!
DOIOT:	SEX SP\ PUSHR(P1)	; P1 has to be preserved
	PUSHR(DP)		; need a temporary register
	PUSHR(CALLPC)		; save pointers to the ...
	PUSHR(RETPC)		;  ... MicroDOS SCRT routines
	RLDI(CALLPC,F_CALL)	; and switch to our SCRT
	RLDI(RETPC,F_RETURN)	;  ... routines instead

; Load the I/O parameters left by SEEKST/SEEKA ...
	RLDI(DP,UTIOCB)		; point to the unit left by SEEKST/SEEKA
	LDA DP\ STR SP		; save that temporarily
	RLDA(T2,DP)		; load the high 16 bits of the LBA
	RLDA(T1,DP)		; then the low 16 bits
	RCOPY(P1,R0)		; put the DMA address in P1	
	LDN	SP		; get the storage device unit back

; Do either a READ or a WRITE ...
DOIOT0:	BDF	DOIOT1		; branch if write
	CALL(F_SDREAD)		; read a sector
	 BDF	 DOIOT5		; branch if I/O error
	BR	DOIOT2		; and otherwise finish the setup
DOIOT1:	CALL(F_SDWRITE)		; write a sector
	 BDF	 DOIOT5		; branch if I/O error
	SDF			; otherwise ensure that DF stays 1!

;   Decrement the sector count in P2.0 and, if it's zero, then we're done.
; If not, increment the LBA in T2:T1 and go do it all again!
DOIOT2:	DEC P2\ GLO P2		; decrement the sector count
	BZ	DOIOT4		; and quit if it's zero
	SHLC\ STR SP		; save DF while we compute
	INC	T1		; increment the low order LBA
	GHI T1\ BNZ DOIOT3	; branch if the result isn't zero
	GLO T1\ BNZ DOIOT3	; ...
	INC	T2		; T1 rolled over, so carry to T2
DOIOT3:	LDN SP\ SHR		; restore DF
	BR	DOIOT0		; and transfer another sector

; Here when the transfer is finished successfully!
DOIOT4:	LDI 0			; return zero for the error code
	LSKP			; restore the registers and return

; Here when the transfer fails because of an I/O error ...
DOIOT5:	LDI	FD.ABE		; set the abnormal termination bit
	PLO	P2		; save the return code in P2.0
	RCOPY(R0,P1)		; transfer the final DMA address back to R0
	SEX SP\ IRX		; ...
	POPR(RETPC)		; restore the MicroDOS SCRT routines
	POPR(CALLPC)		; ...
	POPR(DP)		; restore our temporary register
	POPRL(P1)		; restore the original parameter block pointer
	OUTI(GROUP, BASEGRP)	; restore the I/O group
	RETURN			; and we're done!

	.SBTTL	"CMD and WAIT floppy routines"

;++
;   These floppy diskette routines -
;
; CMD	-> issue uPD765 FDC command
; WAIT	-> wait for uPD765 FDC to finish
;
; are not implemented.  They really don't have any meanting for our diskette
; emulation, and we simply make them NOPs that always return dailure.
;
; The MicroDOS FORMAT program will actually "work" (although it does absolutely
; nothing, of course) if you change these to always return success instead.  The
; DIAG program, FWIW, doesn't even use any of these - it reads one sector at a
; time using SEEKST/READST.
;
; SYSGEN, however, does use both of these and, even more annoyingly, all it
; uses them for are things like SEEK and READ SECTOR, which it could do by
; calling the SEEKxx/READxx routines anyway.
;--
CMD:
WAIT:	LDI $FF\ PLO P2		; return failure code in AUX (RD) .0!!
	RETURN			; and just quit

	.SBTTL	"Disk Entry Vectors"

	.ORG	UTBASE+$07D8
	UENTRY(UTCFRETS)\   LBR CFRET	; command file return point
	UENTRY(UTREADTRS)\  LBR READTR	; read multiple sector
	UENTRY(UTWRITTRS)\  LBR WRITTR	; write as above
	UENTRY(UTREADAS)\   LBR READA	; read sector using already set DMA
	UENTRY(UTWRITAS)\   LBR WRITA	; write sector as above
	UENTRY(UTRECALS)\   LBR RECAL	; recalibrate (not needed!)
	UENTRY(UTSEEKAS)\   LBR SEEKA	; seek by track & sector in ASL
	UENTRY(UTCMDS)\	    LBR CMD	; output command bytes (not needed?)
	UENTRY(UTRENTERS)\  LBR UTGOUT71; ut71 rentry (duplicates GOUT71?)
	UENTRY(UTWAITS)\    LBR WAIT	; service fdc after command
	UENTRY(UTSEEKS)\    LBR SEEKST	; seek by PSN in parameter block
	UENTRY(UTREADS)\    LBR READST	; read sector set DMA from parameter
	UENTRY(UTWRITS)\    LBR WRITST	; write sector as above
	UENTRY(UTCHECK)\    .BYTE $FF	; checksum byte (unused)

	.EJECT
	.END
