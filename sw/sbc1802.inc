	.SBTTL	Spare Time Gizmos SBC1802 Definitions

;++
;       Copyright (C) 2006-2020 By Spare Time Gizmos, Milpitas CA.
;
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
;
; DESCRIPTION:
;   This file contains definitions used by both the monitor and BIOS for the
; Spare Time Gizmos SBC1802 main board and expansion card.
;
; REVISION HISTORY:
; dd-mmm-yy	who     description
; 09-SEP-21	RLA	Stolen from Elf2K
; 15-SEP-21	RLA	Add RSHLC and RSHRC; make PUSHD/POPD SEX first
; 23-JUN-22	RLA 	Fix ID_MDL (it was way off!)
; 15-FEB-24	RLA	Add Tu58 definitions
;--

;0000000001111111111222222222233333333334444444444555555555566666666667777777777
;1234567890123456789012345678901234567890123456789012345678901234567890123456789

	.SBTTL	Register Definitions and Subroutine Linkages

; Hardware defined registers...
PC0	.EQU	0	; PC register for initialization
DMAPTR	.EQU	0	; Register used for DMA
INTPC	.EQU	1	; PC register for interrupt service
SP	.EQU	2	; Stack pointer

; Standard call and return (aka SCRT) registers...
;   Note that these are pretty much universal to all 1802 software, including
; both ElfOS and MicroDOS (although MicroDOS calls the argument pointer "LINK"
; instead of A!).
PC	.EQU	3	; Normal PC register (both for BOOTS and user programs)
CALLPC	.EQU	4	; The PC register dedicated to the CALL routine
RETPC	.EQU	5	; " "	"	"	"	" RETURN "
A	.EQU	6	; Call linkage register (i.e. the argument list pointer)

; Temporary (sometimes extremely temporary!) registers...
T1	.EQU	7	; The first of 3 temporary registers ($9 on Elf2K)
T2	.EQU	8	;  "  second " "   "   "     "   "   ($A on Elf2K)
T3	.EQU	9	;  "  third  " "   "   "     "   "   ($8 on Elf2K)

;  DP always contains in its upper byte the monitor's data page whenever this
; monitor is running.  This allows code to address variables in the data page
; simply by switching values in the low byte...
DP	.EQU	$A	; monitor's data page ($7 on Elf2K)

; Parameter passing registers for subroutines...
;   Register RF is used by the UT71 console I/O routines for character I/O, and
; register RD is used by UT71 for console hexadecimal I/O.  Additionally many
; of the UT71 diskette I/O routines used RD as a parameter/pointer.  On UT71
; these two registers are called "CHAR" (P1) and "ASL" (P2).
P1	.EQU	$F	; The first of 3 parameter/permanant registers
P2	.EQU	$D	;  "  second " "	"		"
P3	.EQU	$C	;  "  third  " "	"		"
P4	.EQU	$B	;  and fourth...

;   On the Elf2K and PicoElf with Mike's BIOS then the BAUD register holds the
; magic baud rate constants for the console serial port. With UT71 and MicroDOS
; the same register is called AUX and holds the local echo flag in RE.1 as well
; as being a temporary for the console I/O routines.
;
;   WARNING - Mike's BIOS SCRT call and return routines use BAUD.0 as a 
; temporary for saving the D register.  Not only that, but there's apparently
; some misguided code out there that depends on a copy of the original value
; of D, from the time of the CALL, being left in BAUD.0!
BAUD	.EQU	$E	; unused on the SBC1802

	.SBTTL	Generic Definitions

; Bit equates ...
BIT0	.EQU	%0000000000000001
BIT1	.EQU	%0000000000000010
BIT2	.EQU	%0000000000000100
BIT3	.EQU	%0000000000001000
BIT4	.EQU	%0000000000010000
BIT5	.EQU	%0000000000100000
BIT6	.EQU	%0000000001000000
BIT7	.EQU	%0000000010000000
BIT8	.EQU	%0000000100000000
BIT9	.EQU	%0000001000000000
BIT10	.EQU	%0000010000000000
BIT11	.EQU	%0000100000000000
BIT12	.EQU	%0001000000000000
BIT13	.EQU	%0010000000000000
BIT14	.EQU	%0100000000000000
BIT15	.EQU	%1000000000000000

; Special ASCII control characters that get used here and there...
CH.NUL	.EQU	$00	; control-shift-@
CH.CTC	.EQU	$03	; control-C (abort command)
CH.BEL	.EQU	$07	; control-G (ring bell)
CH.BSP	.EQU	$08	; control-H (backspace)
CH.TAB	.EQU	$09	; control-I (horizontal tab)
CH.LFD	.EQU	$0A	; control-J (line feed)
CH.FFD	.EQU	$0C	; control-L (form feed)
CH.CRT	.EQU	$0D	; control-M (carriage return)
CH.CTO	.EQU	$0F	; control-O (suppress output)
CH.XON	.EQU	$11	; control-Q (resume output)
CH.CTR	.EQU	$12	; control-R (retype input)
CH.XOF	.EQU	$13	; control-S (pause output
CH.CTU	.EQU	$15	; control-U (erase input)
CH.ESC	.EQU	$1B	; control-[ (escape)
CH.DEL	.EQU	$7F	; rubout (delete)

	.SBTTL	Useful Macro Definitions

; Advance the current origin to the next page...
#define PAGE	.ORG (* + $FF) & $FF00

; Return the high and/or low bytes of a 16 bit value...
#define HIGH(X)		(((X)>>8) & $FF)
#define LOW(X)		( (X)     & $FF)

; Make a 16 bit word out of two 8 bit bytes ...
#define WORD(x)		((x) & $FFFF)
#define MKWORD(H,L)	((H)<<8) | (L))

;   These delay macros are all calculated based on a 2.5MHz crystal frequency.
; Each major cycle (8 clocks) requires 3.2us, and a two (yes, two not one!)
; millisecond delay needs exactly 625 major cycles.  The following loop uses 4
; major cycles per iteration, so a count of 155 gives 620 clocks.  Add in two
; more for the LDI and the final NOP adds three more (yes, NOP takes 3!) for a
; grand total of 625...
DLYCONS	.EQU	155		; magic constant for 2mS at 2.5MHz
#define DLY2MS	LDI DLYCONS\ SMI 1\ BNZ $-2\ NOP

;  This macro will delay for one millisecond, and you can use it with DLY2MS
; to generate delays for odd numbers of milliseconds.  It's not quite accurate
; since a 1mS delay with a 2.5MHz CPU clock needs 312.5 major cycles, and
; there's no trick that I can think of to get half a cycle.
#define DLY1MS	LDI DLYCONS/2+1\ SMI 1\ BNZ $-2

;   And this macro uses T1.0 and the DLY2MS macro to delay up to half a second.
; It's not very accurate for small intervals (just call DLY2MS directly!), but
; it is good enough for most purposes...
#define DELAY(x)	LDI x/2\ PLO T1\ DLY2MS\ DEC T1\ GLO T1\ BNZ $-9

; Macros for some common BIOS functions...
#define OUTSTR(pstr)	RLDI(P1,pstr)\ CALL(F_MSG)
#define OUTCHR(c)	LDI c\ CALL(F_TTY)

;   INLMES() simply prints out an ASCII string "in line", which saves us the
; trouble of defining a label and a .TEXT constant for OUTSTR().  The catch
; is that TASM HAS A 16 CHARACTER LIMIT ON THE LENGTH OF A MACRO ARGUMENT!
; That's not many when it comes to messages, so use INLMES with caution...
#define	INLMES(str)	CALL(F_INMSG)\ .TEXT str \ .BYTE 0

	.SBTTL	Instructions I Wish the 1802 Had

;   These macros define some additional mnemonics and a few simple instructions
; that the 1802 should have had all along!

; Standard subroutine call and return macros...
#define CALL(ADDR)	SEP CALLPC\ .WORD ADDR
#define RETURN		SEP RETPC

;   Push or pop the D register from the stack.  Note that POPD assumes the
; SP is in the usual position (pointing to a free byte on the TOS) and it
; does the IRX for you!
#define PUSHD	STXD
#define POPD	IRX\ LDX

;   Set and clear the DF flag.  Note that these do not actually affect the
; current contents of D!
#define CDF	ADI 0
#define SDF	SMI 0

;   The standard assembler defines the mnemonics BL (branch if less), BGE
; (branch if greater or equal), etc.  These all simply test the DF flag and
; are normally used after a subtract operation.  These are all short branches
; however, and while there are equivalent long branch instructions that also
; test DF, those don't have these same mnemonics.  Let's fix that...
#define LBPZ	LBDF
#define LBGE	LBDF
#define LBM	LBNF
#define LBL	LBNF

; Enable and disable interrupts (P = PC) ...
#define INTON	SEX PC\ RET\ .BYTE (SP<<4) | PC
#define INTOFF	SEX PC\ DIS\ .BYTE (SP<<4) | PC

;   This macro does the equivalent of an "OUT immediate" instruction with the
; specified port and data.  It assumes the P register is set to the PC.
#define	OUTI(p,n)	SEX PC\ OUT p\ .BYTE n

;   This macro outputs a POST code to the LED display during startup.  It can
; ONLY be used during the actual POST testing because it assumes P=0!
#define POST(n)		SEX PC0\ OUT LEDS\ .BYTE n

	.SBTTL	Sixteen Bit Register Operation Macros

;   Note that many of these (though not all of them!)  are all compatible with
; the 1804/5/6 CPU and, if you had one of those CPUs and were so inclined, you
; could replace these macros with the appropriate extended instructions and save
; some code space.

; Register load immedate...
#define RLDI(R,V)	LDI HIGH(V)\ PHI R\ LDI LOW(V)\ PLO R

; Clear a register...
#define RCLR(R)		LDI $00\ PHI R\ PLO R

; Register load via pointer...
#define RLDA(R,N)	LDA N\ PHI R\ LDA N\ PLO R
#define RLXA(R)		LDXA \ PHI R\ LDXA \ PLO R

;   Copy the 16 bit source register to the destination. Note that the closest
; equivalent 1804 operation would be RNX - copy register N to register X.
; Unfortunately the 1802 has no "PLO X" or "PHI X" operation, so there's no way
; to emulate that.
#define RCOPY(D,S)	GHI S\ PHI D\ GLO S\ PLO D

;   Push a 16 bit register on the stack.  Note that RSXD and PUSHR are exactly
; the same - the PUSHR mnemonic is just for convenience.
#define RSXD(R)		GLO R\ STXD\ GHI R\ STXD
#define PUSHR(R)	RSXD(R)

;   Pop a 16 bit register from the stack.  Note that POPR is exactly the same
; as RLXA - the POPR mnemonic is just for convenience.  Unfortunately the 1802
; stack operations are BOTH pre-increment and pre-decrement, which doesn't work
; out so well.  POPR/RLXA always needs to be preceeded by an IRX, and also the
; LAST pop operation needs to do an LDX rather than LDXA (or else you need to
; stick a DEC SP in there somewhere).  The latter is why POPRL exists.
#define POPR(R)		RLXA(R)
#define POPRL(R)	LDXA\ PHI R\ LDX\ PLO R

; Decrement register and branch if not zero ...
#define DBNZ(R,A)	DEC R\ GLO R\ LBNZ A\ GHI R\ LBNZ A

; Shift a sixteen bit register left 1 bit (with or w/o carry in) ...
#define RSHL(R)		GLO R\ SHL\  PLO R\ GHI R\ SHLC\ PHI R
#define RSHLC(R)	GLO R\ SHLC\ PLO R\ GHI R\ SHLC\ PHI R

; Shift a sixteen bit register right 1 bit (with or w/o carry in) ...
#define RSHR(R)		GHI R\ SHR\  PHI R\ GLO R\ SHRC\ PLO R
#define RSHRC(R)	GHI R\ SHRC\ PHI R\ GLO R\ SHRC\ PLO R

	.SBTTL	Thirty Two Bit Register Pair Macros

; Clear a register pair ...
#define QCLR(R1,R2)		LDI 0\ PHI R1\ PLO R1\ PHI R2\ PLO R2

; Copy one 32 bit register pair to another 32 bit register pair ...
#define QCOPY(D1,D2,S1,S2)	  GHI S1\ PHI D1\ GLO S1\ PLO D1
#defcont			\ GHI S2\ PHI D2\ GLO S2\ PLO D2

;   Load or store a 32 bit register pair.  Remember that the order of the
; registers is reversed for QSXD vs QLDA/QLXA - that's not a mistake!
#define QSXD(R2,R1)		RSXD(R1)\ RSXD(R2)
#define QLDA(R2,R1)		RLDA(R2)\ RLDA(R1)
#define QLXA(R2,R1)		RLXA(R2)\ RLXA(R1)
#define PUSHQ(R2,R1)		PUSHR(R1)\ PUSHR(R2)
#define POPQ(R2,R1)		POPR(R2)\ POPRL(R1)

; Quad shift left with carry -- DF <- (R2,R1) <- DF ...
#define QSHLC(R2,R1)		  GLO R1\ SHLC\ PLO R1
#defcont			\ GHI R1\ SHLC\ PHI R1
#defcont			\ GLO R2\ SHLC\ PLO R2
#defcont			\ GHI R2\ SHLC\ PHI R2

; Quad shift left no carry -- DF <- (R2,R1) <- 0 ...
#define QSHL(R2,R1)		  GLO R1\ SHL \ PLO R1
#defcont			\ GHI R1\ SHLC\ PHI R1
#defcont			\ GLO R2\ SHLC\ PLO R2
#defcont			\ GHI R2\ SHLC\ PHI R2

; Quad shift right with carry -- DF -> (R2,R1) -> DF ...
#define QSHRC(R2,R1)		  GHI R2\ SHRC \ PHI R2
#defcont			\ GLO R2\ SHRC \ PLO R2
#defcont			\ GHI R1\ SHRC \ PHI R1
#defcont			\ GLO R1\ SHRC \ PLO R1

; Quad shift right no carry -- 0 -> (R2,R1) -> DF ...
#define QSHR(R2,R1)		  GHI R2\ SHR  \ PHI R2
#defcont			\ GLO R2\ SHRC \ PLO R2
#Defcont			\ GHI R1\ SHRC \ PHI R1
#defcont			\ GLO R1\ SHRC \ PLO R1

; Quad subtract (D2,D1) <- (D2,D1)-(S2,S1)
#define QSUB(D2,D1,S2,S1) 	  GLO S1\ STR SP\ GLO D1\ SM \ PLO D1
#defcont			\ GHI S1\ STR SP\ GHI D1\ SMB\ PHI D1
#defcont			\ GLO S2\ STR SP\ GLO D2\ SMB\ PLO D2
#defcont			\ GHI S2\ STR SP\ GHI D2\ SMB\ PHI D2

; Quad add (D2,D1) <- (D2,D1)+(S2,S1)
#define QADD(D2,D1,S2,S1)	  GLO S1\ STR SP\ GLO D1\ ADD\ PLO D1
#defcont			\ GHI S1\ STR SP\ GHI D1\ ADC\ PHI D1
#defcont			\ GLO S2\ STR SP\ GLO D2\ ADC\ PLO D2
#defcont			\ GHI S2\ STR SP\ GHI D2\ ADC\ PHI D2

; Branch to DST if the register pair (R2,R1) is not zero ...
#define QBNZ(R2,R1,DST)		  GHI R2\ LBNZ DST\ GLO R2\ LBNZ DST
#defcont			\ GHI R1\ LBNZ DST\ GLO R1\ LBNZ DST

	.SBTTL	SBC1802 Base Board Definitions

; Memory layout and memory mapped peripherals for the SBC1802 ...
DPBASE	 .EQU	$FE00	; RAM page set aside for the firmware
DPEND    .EQU	$FEDF	; last byte of firmware data page
MCR	 .EQU	$FEE7	; memory mapping control register
RTCBASE	 .EQU	$FEE8	; CDP1879 real time clock address
PICBASE	 .EQU	$FEF0	; CDP1877 programmable interrupt controller
CHKSUM	 .EQU	$FFFE	; EPROM checksum is stored in the last two bytes

;   Most of the time, while the firmware is running, the EPROM is mapped from
; $8000 thru $FFFF (with holes for the data page and memory mapped peripherals
; defined above).  The "cold start" entry point at $8000 contains a DIS, $00,
; and then a long branch to the POST code.  The "warm start" entry at $8005
; skips the POST, prints the saved user registers and then starts prompting
; for commands.
EPROM	 .EQU	$8000	; address of EPROM in ROM0 and ROM1 maps
COLD	 .EQU	EPROM+0	; cold start entry vector
WARM	 .EQU	EPROM+5	; warm start/breakpoint trap entry vector
BASIC	 .EQU	$B000	; BASIC interpreter entry point
HELP	 .EQU	$E000	; address of help text (from the ROMTEXT program)
XVECTORS .EQU	$EFE0	; XMODEM (and others) vector table used by BASIC
USRRAM	 .EQU	$EFFF	; top of RAM space available to the user

; Standard I/O ports implemented on the base board ...
SL0BUF	 .EQU	2	; console CDP1854 UART data register
SL0CTL	 .EQU	3	;  "   "     "     "   control
SL0STS	 .EQU	SL0CTL	;  "   "     "     "   status
LEDS	 .EQU	4	; 7 segment LED POST display
SWITCHES .EQU	4	; DIP configuration/startup switches
IDESEL	 .EQU	5	; IDE/ATA register select
IDEBUF	 .EQU	6	;  "   "  data buffer
SLUBRG	 .EQU	7	; SLU0/SLU1 baud rate generator

; Standard EF flag assignments for the base board ...
#define B_SL0SID	B1	; branch on SLU0 RXD
#define B_RTCIRQ	B2	;  "  "  "  CDP1879 IRQ
#define BN_RTCIRQ	BN2	;  "  "  "  no  "    "
#define B_SL0IRQ	B3	;  "  "  "  SLU0 IRQ
#define BN_SL0IRQ	BN3	;  "  "  "  no "  "
#define B_INPUT		B4	; branch on INPUT/ATTENTION pressed

	.SBTTL	Memory Control Register Definitions

;++
;    The SBC1802 supports five memory maps which are selectable by the memory
; control register (aka MCR).   The memory map is dynamic and can be changed
; at any time just by writing a new value to the MCR, with some caution. Map
; changes take place instantly as soon as the MCR is updated, and there is no
; buffering, delay or grace period.  If you unmap the part of memory that holds
; the currently executing code then something bad is guaranteed to happen.
;
;   There are parts of the memory map that never change (e.g. the BIOS are from
; $F000 to $FFFF) and the safe thing would be to execute any map changes from
; there.
;
; FFFF  +-------+    +-------+    +-------+    +-------+    +-------+
;       | EPROM |    | EPROM |    |       |    |       |    |       |
; F000  +-------+    +-------+    |       |    |       |    |       |
;       |       |    |       |    |       |    |       |    |       |
;       | RAM 1 |    |       |    | EPROM |    | EPROM |    | EPROM |
;       |       |    | RAM 1 |    |       |    |       |    |       |
; 8400  +-------+    |       |    |       |    |       |    |       |
;       | EPROM |    |       |    |       |    |       |    |       |
; 8000  +-------+    +-------+    +-------+    +-------+    +-------+
;       |       |    |       |    |       |    |       |    |       |
;       |       |    |       |    |       |    |       |    |       |
;       |       |    |       |    |       |    |       |    |       |
;       | RAM 0 |    | RAM 0 |    | RAM 0 |    | RAM 1 |    | EPROM |
;       |       |    |       |    |       |    |       |    |       |
;       |       |    |       |    |       |    |       |    |       |
;       |       |    |       |    |       |    |       |    |       |
; 0000  +-------+    +-------+    +-------+    +-------+    +-------+
;       MICRODOS       ELFOS        ROM0         ROM1         BOOT
;
;   The MicroDOS and ElfOS maps are for running those respective operating
; systems.  The ROM0 and ROM1 maps are normally used while this firmware
; is active, and two maps exist so that the firmware has access to all of RAM
; even though only half of it is mapped at any time.  The BOOT map is selected
; after a hardware reset and ensures that the 1802 (which starts execution at
; address zero) begins running the EPROM code.
;--

; Memory control register bits ...
MC.BOOT	 .EQU	%000	; boot time memory map
MC.ROM0	 .EQU	%100	; EPROM with RAM chip 0 mapped
MC.ROM1	 .EQU	%101	;   "     "   "    "  1    "
MC.MDOS	 .EQU	%110	; MicroDOS compatible memory map
MC.ELOS	 .EQU	%111	; ElfOS compatible memory map
MC.MASK	 .EQU	%111	; mask for all mapping bits

;   The MCR contains a fourth writable bit which service as a master interrupt
; enable.  This is unfortunately necessary because the CDP1877 PIC doesn't have
; a clear/reset input and could be in any state after the SBC1802 is reset,
; including with an active interrupt request.  Since the 1802 CPU also resets to
; an interrupt enabled state, this is potentially a Very Bad Thing.  The MCR
; master interrupt enable is cleared by a reset and blocks CDP1877 interrupts
; until the software sets it.  And no, this has nothing to do with the memory
; mapping - I just happened to have a spare bit in the MCR...
MC.MIEN	 .EQU	BIT7	; set to enable CDP1877 PIC interrupts
MC.PIRQ	 .EQU	BIT3	; CLEARED when the PIC is requesting an interrupt

;   The MCR can be read as well as written (this allows an ISR to save and later
; restore the map in use) and, since it has a few spare bits, it also contains
; the status of the memory backup battery.  Needless to say, this bit is read
; only.
MC.BBAT	 .EQU	BIT6	; if set the backup battery is still good

	.SBTTL	"POST Display and Codes"

;   The SBC1802 POST display is a single seven segment LED display that's driven
; directly by whatever 8 bits are output to port 4 in I/O group 0. This replaces
; the 8 discrete LEDs found on the traditional Elf, but it's the same port.

; Bits to display segment mapping ...
;;PD.SEGA	 .EQU	BIT0
;;PD.SEGB	 .EQU	BIT1
;;PD.SEGC	 .EQU	BIT2
;;PD.SEGD	 .EQU	BIT3
;;PD.SEGE	 .EQU	BIT4
;;PD.SEGF	 .EQU	BIT5
;;PD.SEGG	 .EQU	BIT6
;;PD.SEGDP .EQU	BIT7

;   And these are the post codes, F..0, displayed using the traditional seven
; segment hexadecimal fonT.  Note that the decimal point is not used - not sure
; what to do with that at this time...
POSTF	.EQU	$0F	;; PD.SEGA+PD.SEGE+PD.SEGF+PD.SEGG
POSTE	.EQU	$0E	;; PD.SEGA+PD.SEGD+PD.SEGE+PD.SEGF+PD.SEGG
POSTD	.EQU	$0D	;; PD.SEGB+PD.SEGC+PD.SEGD+PD.SEGE+PD.SEGG
POSTC	.EQU	$0C	;; PD.SEGA+PD.SEGD+PD.SEGE+PD.SEGF
POSTB	.EQU	$0B	;; PD.SEGC+PD.SEGD+PD.SEGE+PD.SEGF+PD.SEGG
POSTA	.EQU	$0A	;; PD.SEGA+PD.SEGB+PD.SEGC+PD.SEGE+PD.SEGF+PD.SEGG
POST9	.EQU	$09	;; PD.SEGA+PD.SEGB+PD.SEGC+PD.SEGF+PD.SEGG
POST8	.EQU	$08	;; PD.SEGA+PD.SEGB+PD.SEGC+PD.SEGD+PD.SEGE+PD.SEGF+PD.SEGG
POST7	.EQU	$07	;; PD.SEGA+PD.SEGB+PD.SEGC
POST6	.EQU	$06	;; PD.SEGA+PD.SEGC+PD.SEGD+PD.SEGE+PD.SEGF+PD.SEGG
POST5	.EQU	$05	;; PD.SEGA+PD.SEGC+PD.SEGD+PD.SEGF+PD.SEGG
POST4	.EQU	$04	;; PD.SEGB+PD.SEGC+PD.SEGF+PD.SEGG
POST3	.EQU	$03	;; PD.SEGA+PD.SEGB+PD.SEGC+PD.SEGD+PD.SEGG
POST2	.EQU	$02	;; PD.SEGA+PD.SEGB+PD.SEGD+PD.SEGE+PD.SEGG
POST1	.EQU	$01	;; PD.SEGB+PD.SEGC
POST0	.EQU	$00	;; PD.SEGA+PD.SEGB+PD.SEGC+PD.SEGD+PD.SEGE+PD.SEGF
POSTDP	.EQU	$80	;; PD.SEGDP

	.SBTTL	CDP1854 UART Definitions

; CDP1854 status register bits ...
SL.THRE	.EQU	BIT7	; transmitter holding register empty
SL.TSRE	.EQU	BIT6	; transmitter buffer register empty
SL.PSI	.EQU	BIT5	; peripheral status interrupt
SL.ES	.EQU	BIT4	; extra status (RTS on the SBC1802)
SL.FE	.EQU	BIT3	; framing error
SL.PE	.EQU	BIT2	; parity error
SL.OE	.EQU	BIT1	; overrun error
SL.DA	.EQU	BIT0	; received data available

; CDP1854 control register bits ...
SL.TR	.EQU	BIT7	; transmit request (unused on SBC1802)
SL.BRK	.EQU	BIT6	; set to force transmit break
SL.IE	.EQU	BIT5	; interrupt enable
SL.WS2	.EQU	BIT4	; select number of data bits (5..8)
SL.WS1	.EQU	BIT3	;   "      "     "   "    "
SL.SBS	.EQU	BIT2	; set to select 1.5/2 stop bits
SL.EPE	.EQU	BIT1	; set for even parity
SL.PI	.EQU	BIT0	; set to inhibit parity
SL.8N1	.EQU	SL.WS2+SL.WS1+SL.PI

; Baud rate selections for the COM8116/COM8136 ...
;   A couple of things are worth noting here - first, the COM8116/8136 actually
; has TWO independent baud rate generators.  In the SBC1802, one is used for
; SLU0 (the console) and the other for SLU1.  The lower four bits of the SLBAUD
; port control the SLU1 baud, and the upper four bits control SLU0.  To get the
; appropriate constants for SLU0, just shift these values left 4 bits.
;
;   It also worth noting, should you read the COM8116/8136 data sheet, that the
; order of the bits is reversed.  The "DCBA" bits shown on the datasheet map to
; D0..D3 (for SLU1) and D4..D7 (for SLU0).  You don't really have to worry about
; that though, since these constants have been reversed to allow for that.
;
;   Lastly, note that the COM8116/8136 can generate other baud rates besides the
; ones shown here.  Some of the less useful ones (e.g. 134.5 baud, 50 baud, 3600
; baud, etc) have been omitted from this table..
BD.110	 .EQU	%0100	;    110 bps
BD.150	 .EQU	%0010	;    150 bps
BD.300	 .EQU	%1010	;    300 bps
BD.600	 .EQU	%0110	;    600 bps
BD.1200	 .EQU	%1110	;  1,200 bps
BD.2400	 .EQU	%0101	;  2,400 bps
BD.4800	 .EQU	%0011	;  4,800 bps
BD.9600	 .EQU	%0111	;  9,600 bps
BD.19.2	 .EQU	%1111	; 19,200 bps
BD.SLU0	 .EQU	$0F	; COM8116 mask for SLU0 baud setting
BD.SLU1	 .EQU	$F0	;  "   "   "    "  SLU1  "    "   "
BD.DFLT	 .EQU	(BD.9600*16)+BD.9600

	.SBTTL	CDP1879 Real Time Clock Definitions

; Register offsets for the real time clock ...
;   In the SBC1802 the CDP1879 is memory mapped and these offsets are alll
; relative to RTCBASE.  Also note that there are no offsets zero or one - that's
; not a mistake.  The RCA guys did that on purpose just in case you want to I/O
; map your RTC.  Also notice that the CDP1879 doesn't keep track of the year -
; you're on your own for that...
RTCSEC	.EQU	2	; seconds
RTCMIN	.EQU	3	; minutes
RTCHRS	.EQU	4	; hours
RTCDAY	.EQU	5	; days
RTCMON	.EQU	6	; month
RTCCSR	.EQU	7	; control and status register

; Magic bits in the counter registers ...
RT.PM   .EQU	BIT7	; bit 7 of hours is AM/PM flag (1=PM)
RT.12H  .EQU	BIT6	; bit 6 of hours is 12/24 hour mode (1=12hr)
RT.LPY  .EQU	BIT7	; bit 7 of month is leap year flag (1=YES)

; Control register bit assignments for the CDP1879 ...
RT.OSC	.EQU	$C0	; oscillator frequency selection (see below)
RT.STRT	.EQU	BIT2	; start the clock running
RT.WEN	.EQU	BIT3	; write/update counter registers
RT.CKO	.EQU	$0F	; clock output and interrupt (see below)

; Oscillator selection bits ...
;   Note that the SBC1802 has a 32.768kHz oscillator and that never changes.
; The other values are for curiosity only...
RT.O32	.EQU	$00	; 32.768kHz clock input
RT.O1M	.EQU	$01	; 1.048576MHz
RT.O2M	.EQU	$02	; 2.097152MHz
RT.O4M	.EQU	$03	; 4.194304MHz

; Clock output selection ...
;   These bits control the frequency of the CLK OUT pin on the CDP1879.  This is
; unused on the SBC1802, however this output can also cause an interrupt, which
; IS usable on the SBC1802.
RT.CDY   .EQU	%11110000	; once a day
RT.CHR   .EQU	%11100000	; once a hour
RT.CMN   .EQU	%11010000	; once a minute
RT.CSC   .EQU	%11000000	; once a second
RT.CK2   .EQU	%10110000	;    2 Hz
RT.CK4   .EQU	%10100000	;    4 Hz
RT.CK8   .EQU	%10010000	;    8 Hz
RT.C16   .EQU	%10000000	;   16 Hz
RT.C32   .EQU	%01110000	;   32 Hz
RT.C64   .EQU	%01100000	;   64 Hz
RT.C128  .EQU	%01010000	;  128 Hz
RT.C256  .EQU	%01000000	;  256 Hz
RT.C512  .EQU	%00110000	;  512 Hz
RT.C1024 .EQU	%00100000	; 1024 Hz
RT.C2048 .EQU	%00010000	; 2048 Hz
RT.CDIS  .EQU	%00000000	; disabled

; RTC status register bits (there are only two!) ...
RT.AIRQ	 .EQU	BIT7	; alarm generated interrupt request
RT.CIRQ	 .EQU	BIT6	; clock   "    "    "    "     "

	.SBTTL	CDP1877 Programmable Interrupt Controller Definitions

;   The CDP1877 PIC is memory mapped and all of the following register addresses
; are relative to the PICBASE constant.  The RCA documentation would have you
; believe that the PIC requires a whopping 4K (!!!) of address space, but that's
; not really true.  The SBC1802 memory GAL does some trickery with the CDP1877
; select inputs and manages to get the footprint down to just 16 bytes.
PICSTS	.EQU	0	; status register  (R/O)
PICMSK	.EQU	0	; interrupt mask   (W/O)
PICPOL	.EQU	4	; polling register (R/O)
PICCTL	.EQU	4	; control register (W/O)
PICVEC	.EQU	8	; interrupt vector (R/O)
PICPAG	.EQU	8	; ISR page address (W/O)

; Interrupt mask bits ...
;   These correspond to the way the SBC1802 interrupts are wired to the CDP1877.
; Note that these same bits work in the MASK register, the POLL register, and
; the STATus register.
IM.SLU1	.EQU	BIT7	; CDP1854 UART #1 (expansion)
IM.PPI	.EQU	BIT6	; CDP1851 PPI     (expansion)
IM.TIMR	.EQU	BIT5	; CDP1878 TIMER   (expansion)
IM.SLU0	.EQU	BIT4	; CDP1854 UART #1 (console)
IM.DISK	.EQU	BIT3	; IDE/ATA disk interface
IM.IRQ2	.EQU	BIT2	; spare interrupt
IM.RTC	.EQU	BIT1	; CDP1879 RTC
IM.ATTN	.EQU	BIT0	; input switch/button

; CDP1877 control register bits ...
PI.VADN	.EQU	$F0	; upper nibble of the low vector address byte
PI.NRPI	.EQU	BIT3	; reset pending interrupts WHEN ZERO!
PI.NRMR	.EQU	BIT2	; reset all interrupt mask bits WHEN ZERO!
PI.VS2B	.EQU	0	; vector spacing 2 bytes
PI.VS4B	.EQU	1	;    "      "    4   "
PI.VS8B	.EQU	2	;    "      "    8   "
PI.VS16	.EQU	3	;    "      "   16   "

	.SBTTL	IDE/ATA Interface Definitions

; Classic IDE register definitions...
;   Notice that the status register and the command register have the same
; address!  The status register is read only and the command register is write
; only.  The same holds for the error register and the features register.
IDECS1	.EQU	0		; select the CS1FX address space
IDECS3	.EQU	BIT3		;   "     "  CS3FX   "  "    "
IDEDATA	.EQU	IDECS1+0	; data register (R/W)
IDEFEA	.EQU	IDECS1+1	; features register (W/O)
IDEERR	.EQU	IDECS1+1	; error register (R/O)
IDESCT	.EQU	IDECS1+2	; sector count (R/W)
IDELBA0	.EQU	IDECS1+3	; LBA register (low byte)
IDELBA1	.EQU	IDECS1+4	;  "     "
IDELBA2	.EQU	IDECS1+5	;  "     "
IDELBA3	.EQU	IDECS1+6	;  "     "     (high byte)
IDECMD	.EQU	IDECS1+7	; command register (W/O)
IDESTS	.EQU	IDECS1+7	; status register (R/O)
; "Alternate" registers in the CS3FX space ...
IDEAST	.EQU	IDECS3+6	; alternate status register (R/O)
IDECTL	.EQU	IDECS3+6	; device control register
IDEADR	.EQU	IDECS3+7	; drive address register

; IDE status register (IDESTS) bits...
ID.BSY	.EQU	BIT7	; busy
ID.RDY	.EQU	BIT6	; device ready
ID.DF	.EQU	BIT5	; device fault
ID.DSC	.EQU	BIT4	; device seek complete
ID.DRQ	.EQU	BIT3	; data request
ID.COR	.EQU	BIT2	; corrected data flag
ID.IDX	.EQU	BIT1	; index mark
ID.ERR	.EQU	BIT0	; error detected

; IDE error register (IDEERR) bits ...
ID.BBK	.EQU	BIT7	; bad block
ID.UNC	.EQU	BIT6	; uncorrectable data error
ID.MC	.EQU	BIT5	; media changed
ID.INF	.EQU	BIT4	; ID mark not found
ID.MCR	.EQU	BIT3	; media change requested
ID.ABT	.EQU	BIT2	; command aborted
ID.0NF	.EQU	BIT1	; track 0 not found
ID.ANF	.EQU	BIT0	; address mark not found

; IDE feature codes for the IDEFEA register ...
ID_8BT	.EQU	$01	; enable 8 bit mode

; IDE command codes (or at least the ones we use!)...
ID_EDD	.EQU	$90	; execute device diagnostic
ID_IDD	.EQU	$EC	; identify device
ID_RDB	.EQU	$E4	; read buffer
ID_RDS	.EQU	$20	; read sectors with retry
ID_WRB	.EQU	$E8	; write buffer
ID_WRS	.EQU	$30	; write sectors with retry
ID_FEA	.EQU	$EF	; set features
ID_SUP	.EQU	$E1	; spin up
ID_SDN	.EQU	$E0	; spin down

; Device Control register (IDECTL) bits ...
ID.RST	.EQU	BIT2	; software reset
ID.NIE	.EQU	BIT1	; interrupt enable (WHEN 0!!)

; Drive/Head register (IDELB3) bits ...
;   The documentation shows bits 7 and 5 as always being 1.  I'm not clear if
; that means we have to set them when writing, or just that they always read
; back as one.  Better safe than sorry...
ID.LBA	.EQU BIT7+BIT5+BIT6	; logical block addressing mode
ID.MST	.EQU BIT7+BIT5		; select the master drive
ID.SLV	.EQU BIT7+BIT5+BIT4	; select the slave drive

; Magic offsets in the IDENTIFY DEVICE response ...
ID_MOD	.EQU  27*2	; model name/number in ASCII
ID_MDL	.EQU  40	; model name/number length in bytes
ID_LBA	.EQU  60*2	; double word giving total number of sectors

; Other IDE/ATA constants ...
DSKBSZ	.EQU	512	; IDE block size (always 512 bytes!)
DSKBUF	.EQU	$0000	; temporary disk buffer in low RAM

; These macros will read and write IDE registers ...
#define WRIDE(r,v)	SEX PC\ OUT IDESEL\ .BYTE r\ OUT IDEBUF\ .BYTE v
#define RDIDE(r)	SEX PC\ OUT IDESEL\ .BYTE r\ SEX SP\ INP IDEBUF

	.SBTTL	XMODEM Definitions

; XMODEM protocol constants ..
XDATSZ	.EQU	128		; data (payload) size in XMODEM record
XPKTSZ	.EQU	XDATSZ+5	; overall XMODEM packet size (header+data)
XTIMEO	.EQU	5000		; protocol timeout, in 2ms units (10s)

;   These are the XMODEM buffer locations that overlap DSKBUF.  Needless to
; say, this part of RAM CANNOT BE UPLOADED OR DOWNLOADED!  DSKBUF normally
; lives from $0000 to $01FF in RAM0.
XHDR1	.EQU	DSKBUF		; received SOH or EOT
XHDR2	.EQU	XHDR1+1		; received block number
XHDR3	.EQU	XHDR2+1		; received "inverse" block number
XBUFFER	.EQU	XHDR3+1		; 128 bytes of received data
XHDR4	.EQU	XBUFFER+XDATSZ	; received checksum

; XMODEM protocol special characters ...
CH.SOH	.EQU	$01	; start of a data block
CH.EOT	.EQU	$04	; end of file
CH.ACK	.EQU	$06	; packet received OK
CH.NAK	.EQU	$15	; packet not OK - retransmit
CH.SUB	.EQU	$1A	; filler for partial data blocks

	.SBTTL	TU58 RSP Definitions

;   The SBC1802 supports a serial connection from SLU1 to a PC to emulate
; a disk drive.  The protocol used by this connection is the DEC TU58 RSP,
; or "Radial Serial Protocol".
;--

; RSP packet types
;   in the TU58 documentation, these are referred to as "flags"!
TUPDAT	.EQU	BIT0		; data packets for read/write
TUPCTL	.EQU	BIT1		; control (command) packet
TUPINI	.EQU	BIT2		; forces controller initialization
TUPBOO	.EQU	BIT3		; sends unit 0/block 0 w/o RSP
TUPCON	.EQU	BIT4		; continue with the next data packet

; RSP command opcodes ...
TUCNOP	.EQU	 0		; no operation (returns an END packet)
TUCRDD	.EQU	 2		; reads data from the tape
TUCWRT	.EQU	 3		; writes data to the tape
TUCPOS	.EQU	 5		; positions the tape at a given block
TUCDIA	.EQU	 7		; executes built in self test
TUCEND	.EQU	64		; acknowledgement (end) packet type

; RSP command modifiers ...
TUMSAM	.EQU	BIT7		; selects special addressing mode
TUMRCK	.EQU	BIT0		; decreased sensitivity (read command)
TUMMRS	.EQU	BIT3		; switches MRSP mode ON

; RSP end packet error codes ...
;   Note that the error codes are all negative and must be sign extended!
TUESUC	.EQU	      0		; operation completed without errors
TUEWRT	.EQU	      1		; success, but retries were needed
TUEPST	.EQU	WORD(-1)	; self test failed
TUEEOT	.EQU	WORD(-2)	; partial operation - end of tape
TUEUNI	.EQU	WORD(-8)	; unit number not 0 or 1
TUENOT	.EQU	WORD(-9)	; no cartridge in drive
TUEWLK	.EQU	WORD(-11)	; tape is write protected
TUECHK	.EQU	WORD(-17)	; bad data (checksum failure)
TUESER	.EQU	WORD(-32)	; block not found (bad tape format?)
TUEJAM	.EQU	WORD(-33)	; motor stopped (jammed cartridge?)
TUEOPC	.EQU	WORD(-48)	; bad opcode in command packet
TUEBLK	.EQU	WORD(-55)	; block number > 511

; Other TU58 related constants
TUMAXB	.EQU	512		; maximum TU58 block number
TUPKSZ	.EQU	128		; TU58 data packet size
TUBAUD	.EQU	0;;;DL.PBE!DL3840	; default TU58 baud rate
; See the code at TUGET: for the calculation of this timeout ...
TUTIMO	.EQU	1873		; about 100ms with a 2.5MHz clock

	.SBTTL	Hardware Flags

;   The POST leaves a bit vector of the working peripherals stored in RAM at
; HWFLAGS.  This is a 16 bit value (there are too many gizmos for just one
; byte!) which is stored in the standard 1802 big endian (i.e. most significant
; byte first) order.  This is the same order used by the 1805 RLDI, RLXA and
; RSXD instructions.
;
;   BUT, this means that the H1 flags are stored in the first byte at HWFLAGS
; (because they correspond to the Reg.1 notation) and the H0 flags are stored
; in the second byte of HWFLAGS.

; First byte flags at HWFLAGS+0 ...
H1.TLIO	 .EQU	BIT7	; I/O group select hardware present
H1.SLU0	 .EQU	BIT6	; CDP1854 console serial line unit present
H1.PIC	 .EQU	BIT5	; CDP1877 priority interrupt controller present
H1.RTC	 .EQU	BIT4	; CDP1879 real time clock present
H1.IDE1	 .EQU	BIT1	; IDE/ATA slave drive present
H1.IDE0	 .EQU	BIT0	; IDE/ATA master drive present

; Second byte flags at HWFLAGS+1 ...
H0.SLU1	 .EQU	BIT7	; CDP1854 communication serial line unit present
H0.PPI	 .EQU	BIT6	; CDP1851 programmable parallel interface present
H0.MDU	 .EQU	BIT5	; CDP1855 multiply/divide unit present
H0.PSG	 .EQU	BIT4	; AY-3-8912 programmable sound generator prsent
H0.TMR	 .EQU	BIT3	; CDP1878 programmable timer/counter present

	.EJECT
;	SBTTL	SBC1802 Expansion Board Definitions 

;   The EXP1802 expansion board implements a number of additional options, most
; notably an RCA style I/O group selection register.  If the expansion board is
; installed then ALL I/O instructions must be preceeded by the correct group
; selection!  If the expansion board is not present then I/O group selection is
; a harmless NOP.  All base board options are in group 1.
GROUP	 .EQU	1	; port 1 is always the I/O group selection
BASEGRP	 .EQU	$1	; I/O group for all base board devices
MDUGRP	 .EQU	$4	;  "    "    "  CDP1855 multiply/divide unit
SL1GRP	 .EQU	$4	;  "    "    "  CDP1854 SLU1 (same as MDUGRP!)
TMRGRP	 .EQU	$6	;  "    "    "  CDP1878 counter/timer
PPIGRP	 .EQU	$5	;  "    "    "  CDP1851 parallel interface
PSGGRP	 .EQU	$7	;  "    "    "  AY-3-8912 sound generator
;   Note that groups 8 and up are reserved for future options and the group
; selection bits are available on the expansion bus connector.


;   The base board I/O ports and EF assignments are defined way, way back
; on the "Base Board Definitions" page ...


; CDP1855 MDU/SLU1 I/O Group definitions...
SL1BUF	 .EQU	2	; secondary CDP1854 UART data register
SL1CTL	 .EQU	3	;   "   "      "     "   control
SL1STS	 .EQU	SL1CTL	;   "   "      "     "   status
MDUX	 .EQU	4	; multiply/divide X register
MDUY	 .EQU	5	;   "   "   "  "  Y   "  "
MDUZ	 .EQU	6	;   "   "   "  "  Z   "  "
MDUCTL	 .EQU	7	;   "   "   "  "  control register (W/O)
MDUSTS	 .EQU	7	;   "   "   "  "  status register (R/O)
#define B_MDUOVF	B2	; branch on MDU overflow
#define B_SL1IRQ	B3	;  "  "  "  SLU1 IRQ
#define BN_SL1IRQ	BN3	;  "  "  "  no SLU1 IRQ
#define B_SL1SID	B4	;  "  "  "  SLU1 RXD


; CDP1878 Timer I/O group definitions ...
TMRAHI	 .EQU	6	; CDP1878 counter A MSB
TMRALO	 .EQU	2	; CDP1878 counter A LSB
TMRCRA	 .EQU	4	; CDP1878 control register A
TMRBHI	 .EQU	7	; CDP1878 counter B MSB
TMRBLO	 .EQU	3	; CDP1878 counter B LSB
TMRCRB	 .EQU	5	; CDP1878 control register B
TMRSTS	 .EQU	4	; CDP1878 interrupt status
#define B_TMRIRQ	B3	; branch on timer IRQ
#define BN_TMRIRQ	BN3	;  "  "   " no "   "


; CDP1851 PPI I/O group definitions ...
PPISTS	 .EQU	2	; CDP1851 status register
PPICTL	 .EQU	2	; CDP1851 control register
PPIA	 .EQU	4	; CDP1851 port A
PPIB	 .EQU	6	; CDP1851 port B
#define B_PPIARDY	B1	; branch on PPI port A ready
#define BN_PPIARDY	BN1	;  "  "   " no   "   "   "
#define B_PPIBRDY	B2	; branch on PPI port A ready
#define BN_PPIBRDY	BN2	;  "  "   " no   "   "   "
#define B_PPIIRQ	B3	; branch on PPI IRQ
#define BN_PPIIRQ	BN3	;  "  "   " no   "


; AY-3-8912 PSG I/O group definitions ...
PSGADDR	.EQU	2	; register address (write only)
PSGDATA	.EQU	3	; data port (read/write)

	.SBTTL	CDP1855 Multiply and Divide Unit Definitions

;   The CDP1855 is an 8 bit cascadable, well, multiply and divide unit.
; The SBC1802 has two of these chips and they're capable of doing a 16x16
; bit multiply, or  16/16 bit divide, in 16 clock cycles (or about the
; same time it takes the 1802 to execute a single instruction!).  Note
; that the two CDP1855 chips are daisy chained and cooperate with each other
; to share a single set of I/O ports.

; CDP1855 Control Register bits ...
MD.CPRE	 .EQU	BIT7	   	; enable clock prescaler
MD.RSTS	 .EQU	BIT6		; reset MDU state machine
MD.NMDU  .EQU	BIT5+BIT4	; number of MDUs present
MD.1MDU	 .EQU	BIT5+BIT4	; one lonely MDU only
MD.2MDU	 .EQU	BIT5		; two MDUs
MD.3MDU	 .EQU	BIT4		; three MDUs
MD.4MDU	 .EQU	0		; four MDUs
MD.RSTY	 .EQU	BIT3		; reset Y register
MD.RSTZ	 .EQU	BIT2		; reset Z register
MD.OPER	 .EQU	BIT1+BIT0	; MDU operation
MD.NOP	 .EQU	0		; do nothing
MD.MUL	 .EQU	BIT0		; multiply
MD.DIV	 .EQU	BIT1		; divide

; CDP1855 Status Register bits ...
MD.OVF	 .EQU	BIT0		; overflow detected

	.SBTTL	CDP1878 Timer Counter Definitions

;   The CDP1878 contains two independent 16 bit counter/timer units.  In the
; SBC1802, counter/timer A is clocked by the CPU clock (usually 2.5MHz) and
; counter/timer B is clocked by the baud clock/4 (1.2288MHz).  The CPU clock
; may change, but of course the baud clock won't.  Both units can be clocked
; externally, and both can generate a variety of outputs - one shot, square
; wave, delay, etc.  Both units can also generate interrupts via the PIC.

; CDP1878 Control Register bits (control A and B are both the same) ...
TM.JAM	 .EQU	BIT7	; jam enable
TM.HRC	 .EQU	BIT6	; freeze holding register
TM.GO	 .EQU	BIT5	; start counter
TM.IE	 .EQU	BIT4	; interrupt enable
TM.GPOS	 .EQU	BIT3	; gate level positive
TM.TMO	 .EQU	$01	; mode 1 - timeout
TM.TSTB	 .EQU	$02	; mode 2 - timeout w/strobe
TM.1SHT	 .EQU	$03	; mode 3 - gate controlled one shot
TM.RATE	 .EQU	$04	; mode 4 - square wave rate generator
TM.PWM	 .EQU	$05	; mode 5 - vstisblr duty cycle
TM.NOP	 .EQU	$00	; no mode (counter operstion unaffected

; CDP1878 Status Register bits ...
;   Note that there is only one status register, but it can be accessed as
; either port 4 or 5.  There are two separate control registers, however.
TM.IRQA	 .EQU	BIT7	; timer/counter A interrupt request
TM.IRQB	 .EQU	BIT6	;   "    "   "  B   "    "    "  "

	.SBTTL	CDP1851 Programmable Parallel Interface Definitions

;   The CDP1851 contains two independent 8 bit parallel I/O interfaces.  Either
; interface may be programmed for input or output on a bit by bit or byte wide
; basis.  Both ports have an optional hardware handshake mode, and port A may
; be programmed for fully bidrectional handshaking.

; CDP1851 Control Register mode select bits ...
;   You should OR together either PP.SETA or PP.SETB (depending on which port
; you want to modify) with one of the mode settings below, and then output
; the resulting byte to the PPI control register.
PP.SETA	 .EQU	BIT3		; set port A mode
PP.SETB	 .EQU	BIT4		; set port B mode
PP.MDIN	 .EQU	$03		; byte input mode
PP.MDOU	 .EQU	$43		; byte output mode
PP.MDBP	 .EQU	$C3		; bit programmable mode
PP.MDBI	 .EQU	$83		; bidirectional mode

; CDP1851 Control Register bit programmed mode bits ...
;   After sending PP.MDBP to the control register, the software should send two
; additional bytes.  The first is a bitmap of input or outputs (0 -> input, 
; 1 -> output), and the second is a control byte as follows...
PP.BPPB	 .EQU	BIT1		; select port B for modification (else port A)
PP.RDYE	 .EQU	BIT2		; set READY mode as per BIT6
PP.STBE	 .EQU	BIT3		; set STROBE mode as per BIT7
PP.RDYO	 .EQU	BIT4		; READY bit output data
PP.STBO	 .EQU	BIT5		; STROBE "   "  "    "
PP.OUTR	 .EQU	BIT6		; set READY mode if BIT2 is set
PP.OUTS	 .EQU	BIT7		;  "  STROBE  "  "  BIT3 "   "

; CDP1851 Control Register interrupt mask bits ...
;   After writing this byte to the control register, the software should
; immediately follow it with a mask byte.  This second byte selects the bits
; that are used to generate interrupts.
PP.SIMC	 .EQU	$05		; set interrupt mask and conditions
PP.INTB	 .EQU	BIT3		; set conditions for port B (A otherwise)
PP.MASK	 .EQU	BIT4		; interrupt mask byte follows
PP.NAND	 .EQU	0		; NAND of masked bits
PP.AND	 .EQU	BIT5+BIT6	;  AND "   "  "   "
PP.NOR	 .EQU	BIT6		;  NOR "   "  "   "
PP.OR	 .EQU	BIT5		;   OR "   "  "   "

; CDP1851 Control Register interrupt enable/disable bits ...
PP.SIEN	 .EQU	$01		; set interrupt enable
PP.INTE	 .EQU	BIT7		; enable interrupts (else disable)
;PP.INTB .EQU	BIT3		; for port B (else port A)

; CDP1851 Status register bits (all modes) ...
PP.BINT	 .EQU	BIT0		; port B interrupt request
PP.AINT	 .EQU	BIT1		;  "   A  "    "     "  "
PP.ASIR	 .EQU	BIT2		; A strobe caused port A interrupt request
PP.BSIS	 .EQU	BIT3		; B  "  "   "  "   "   B  "    "    "   "
PP.ARDY	 .EQU	BIT4		; A READY  input data
PP.ASTB	 .EQU	BIT5		; A STROBE   "    "
PP.BDRY	 .EQU	BIT6		; B READY    "    "
PP.BSTB	 .EQU	BIT7		; B STROBE   "    "

	.SBTTL	AY-3-8912 Programmable Sound Generator Definitions

;   Note that the AY-3-8910, 8912 and 8913 are all identical as far as the
; software is concerned.  They differ only in package, pin out and hardware
; interface.

;   Another useful factoid is that the 8910 actually decodes all eight bits of
; the register select address.  Yes, there are only 16 registers so only the
; lower four bits select the register, but the upper four bits MUST be zero or
; the chip will ignore us.  Most of the time, that is - it turns out that GI
; would actually manufacture custom version of the 8910 that expected other,
; non-zero, nibbles on the upper four register select bits.  The idea was to
; allow multiple PSG chips on the same bus to be separately addressed by these
; extra bits in the register address.  Normally any 8910 chip a hobbyist bought
; would want to see zeros on these bits, but these days IC pulls and surplus
; parts have found their way onto the market and you will sometimes find chips
; that want non-zero select codes.  There's no real way to find out, except by
; trial and error...
PSGSEL	.EQU	$00		; PSG select code*16 (e.g. $10, $20, etc)

;   Define mnemonics for the PSG registers just to make it easier to use the
; GI documentation.  BTW, note that GI numbered the registers in OCTAL!
PSGR0	.EQU	PSGSEL+$0	; tone generator low byte, channel A
PSGR1	.EQU	PSGSEL+$1	;  "    "    "   high  " ,    "    "
PSGR2	.EQU	PSGSEL+$2	;  "    "    "   low byte, channel B
PSGR3	.EQU	PSGSEL+$3	;  "    "    "   high  " ,    "    "
PSGR4	.EQU	PSGSEL+$4	;  "    "    "   low byte, channel C
PSGR5	.EQU	PSGSEL+$5	;  "    "    "   high  " ,    "    "
PSGR6	.EQU	PSGSEL+$6	; noise generator control
PSGR7	.EQU	PSGSEL+$7	; mixer and I/O control
PSGR10	.EQU	PSGSEL+$8	; amplitude control, channel A
PSGR11	.EQU	PSGSEL+$9	;  "     "     "        "    B
PSGR12	.EQU	PSGSEL+$A	;  "     "     "        "    C
PSGR13	.EQU	PSGSEL+$B	; envelope period, low byte
PSGR14	.EQU	PSGSEL+$C	;  "    "     "    high  "
PSGR15	.EQU	PSGSEL+$D	; envelope shape and cycle control
PSGR16	.EQU	PSGSEL+$E	; I/O port A data
PSGR17	.EQU	PSGSEL+$F	;  "   "   B   "

;
PSGVOL	.EQU	7	; default volume (0..15)

;   This macro outputs a constant, inline, value to a PSG register ..
; Note that it assumes the correct I/O group has already been selected!!!
#define OUTPSG(r,d)	SEX PC\ OUT PSGADDR\ .DB r\ OUT PSGDATA\ .DB d

	.SBTTL	ElfOS and MicroDOS Definitions

; ElfOS magic numbers and constants ...
;   To boot an ElfOS volume, we read LBN 0 into memory at EO.BBUF.  To verify
; that this is indeed a bootable ElfOS system, we checksum the first EO.BCBC
; bytes of that data and, if the result matches EO.BCCK, we're Golden.  Start
; running the bootstrap at address EO.BBUF with P=R0.
EO.BBUF	.EQU	$0100	; RAM area for loading bootstrap
EO.BTSA	.EQU	$0106	; bootstrap starting address
EO.BCBC	.EQU	$41	; ElfOS boot check byte count
EO.BCCK	.EQU	$60	; ElfOS boot check checksum

; Other magic locations in the ElfOS boot sector ...
;   Note that these addresses assume that the boot has been read into
; memory at $100 (EO.BBUF).  They're NOT the offsets within the sector
; itself!
EB.SIZE	.EQU	$200	; 4 bytes that contain the total number of sectors
EB.FSTY	.EQU	$204	; file system type - 1->LAT-16 and 2->LAT32
EB.MDR1	.EQU	$205	; 2 bytes sector of master directory (LAT-16)
EB.MDR2	.EQU	$210	; 4 bytes sector of master directory (LAT-32)


; MicroDOS diskette geometry ...
;   Single sided, 3-1/2" diskettes, are as far as I know the only mass storage
; media supported by MicroDOS.  The real hardware supports only two drives,
; units 0 and 1, however it's fairly easy to trick MicroDOS into supporting
; more units.
UD.SPT	.EQU	 9	; sectors per track
UD.TPD	.EQU	70	; tracks per diskette
UD.SPD	.EQU	630	; total sectors per diskette

; Disk Identification Block ...
;   Every MicroDOS diskette has one of these blocks in the first LBN (track 0,
; sector 1).  You can call it a volume ID if you want; the SBC1802 firmware
; uses the name and date stored here to identify diskette image partitions.
DI.CDTO	.EQU	12	; creation date, in ASCII format "mm/dd/yy"
DI.CDTL	.EQU	 8	; length of the date field
DI.UIDO	.EQU	20	; volume label ("User ID") as 44 ASCII characters
DI.UIDL	.EQU	44	; length of the volume label field

; MicroDOS constants ...
UDVDKS	 .EQU	8	; number of virtual diskette drives
UDLOAD	 .EQU	$9000	; kernel load address
UDBTPSN	 .EQU	11	; first kernel load sector
UDBTLEN	 .EQU	24	; number of kernel sectors to load
UDSTACK	 .EQU	$BFFF	; top of MicroDOS initial stack
UDCALL   .EQU	$918C	; SCRT CALL address
UDRETURN .EQU	$919C	; SCRT RETURN address
UDECHO   .EQU	$910A	; echo status
UDENTRY	 .EQU	$9040	; entry address into MicroDOS
UDNAME   .EQU	$A843	; 'MICRODOS' name in kernel


	.SBTTL	RAM Page Storage Map

;   The SBC1802 squirrels away one page (224 bytes to be exact) of RAM for the
; exclusive use of this firmware.  This is mapped to memory at address $FExx
; and, although it does take a chunk out of the BIOS space, it's in a place
; where neither ElfOS nor MicroDOS can find it.  And, better yet, it's in a
; part of memory that's always mapped (well, except for in BOOT mode) so it's
; always accessible regardless of the map in use.  Like all of RAM this memory
; is battery backed up and setting store there are non-volatile.

;   By convention, whenever this firmware is active the DP register always
; contains DPBASE in its upper byte.  This allows the DP register to be used
; for direct addressing of any data on this page simply by reloading the lower
; byte rather than the entire register...
	.ORG	DPBASE

;   This "key" is used to determine whether the SRAM contents are valid or junk.
; If the bytes in this "key" matches with the EPROM signature then this data
; is good.
KEY:	.BLOCK	5	; 3 "key" bytes plus the EPROM checksum...

;   On a break point, as much of the user state as we can recover is saved
; here.  Note that the order of these bytes is critical - you can't change
; 'em without also changing the code at TRAP:...
SAVEXP:	.BLOCK	1	; saved state of the user's X register
SAVED:	.BLOCK	1	;   "    "    "   "    "    D    "
SAVEDF:	.BLOCK	1	;   "    "    "   "    "    DF   "
REGS:	.BLOCK	16*2	; All user registers after a breakpoint

; Hardware configuration/POST flags (see SBC1802.INC) ...
HWFLAGS: .BLOCK	2	; two bytes of bitmapped H/W test results

;   These three bytes control the firmware's action on a warm start (i.e. a
; startup where the RAM contents are valid).  If the boot flag (BOOTF) is zero,
; then nothing special happens and the firmware enters the normal command loop
; after system initialization finishes.  If BOOTF is 1, then the firmware
; executes the equivalent of a RUN command using the address stored in RESTA.
; And lastly, if BOOTF is 0xFF then the monitor attempts to boot from the
; primary IDE drive. 
AB.NONE	.EQU	$00	; no special action on startup
AB.BOOT	.EQU	$80	; boot storage device specified by low bits
AB.ADDR	.EQU	$01	; run program at address in RESTA
; DON'T CHANGE THE ORDER OF THE FOLLOWING TWO LOCATIONS!
BOOTF:	.BLOCK	1	; attempt automatic disk boot if non-zero
RESTA:	.BLOCK	2	; restart address when BOOTF == 1

; Memory map to use for user programs ...
OSTYPE:	.BLOCK	1	; should be MC.ELOS or MC.MDOS

; Serial port parameters ...
;   SLBAUD contains the baud rate for both ports; the upper 4 bits are the
; encoded rate for SLU0 (console) and the lower 4 bits are for SLU1.  Baud rates
; are encoded for the COM8116/8136 - see the BD.xxx constants in SBC1802.INC.
; SL1FMT contains the character format for SLU1 in the lower 5 bits, exactly as
; used by the CDP1854 UART.  The format for SLU0 is fixed as 8N1.
SLBAUD:	.BLOCK	1	; baud rates for both serial ports
SL1FMT:	.BLOCK	1	; character format for SLU1

;    Believe it or not, the CDP1879 doesn't store the current year - all it
; knows is the month and the day.  We keep the year here, in RAM which is 
; (hopefully) non-volatile, and you'll have to update it every 12 months.
YEAR:	.BLOCK	1	; year (0..99 only)

; Buffers for time and INPUT/OUTPUT commands ...
IOTBUF:			; buffer for the INP[UT] and OUT[PUT} commands
TIMBUF:	.BLOCK	8	; buffer for the DA[TIME] command

;   These are temporary locations used by the XMODEM transmit and receive
; routines, and in order to save RAM space we share them with the TIMBUF
; above.  Note that the order of these four locations is critical, so don't
; change that!
XINIT	.EQU	TIMBUF+0; send initial ACK or NAK for XRDBLK
XBLOCK	.EQU	TIMBUF+1; current XMODEM block number
XCOUNT	.EQU	TIMBUF+2; current XMODEM buffer byte count
XDONE	.EQU	TIMBUF+3; non-zero when we receive EOT from the host

; Temporary storage for the TU58 handler code ...
TUCKSM:	.BLOCK	3	; even/odd byte flag and two byte checksum accumulator

;   These two words (16 bits each) are used by the exhaustive RAM test to keep
; count of the total number of passes, and the total number of errors.  KEEP
; THEM IN THIS ORDER!
PASSK:	.BLOCK	2	; pass count for MEMTEST and other diagnostics
ERRORK:	.BLOCK	2	; error  "    "     "     "    "        "

; MicroDOS virtual diskette mapping ...
UTDKMAP:.BLOCK	4*UDVDKS; four bytes for every virtual diskette drive
UTDKEND:		; end of virtual diskette map

; Command line buffer...
CMDMAX	.EQU	64	; maximum command line length
CMDBUF:	.BLOCK	CMDMAX+1; buffer for a line of text read from the terminal

;   Since the CDP1802 stack grows downward, we place our firmware monitor and
; BIOS stack at the end of our data page.  We then figure out how many bytes are
; left between that and the last variable, and double check that it's enough.
; What's enough?  That's pretty arbitrary but we make an educated guess...
STACK	.EQU	DPEND	; address (TOP) of this firmware's stack
STKSIZ	.EQU	STACK-$	; and the space allocated for that stack
	.ECHO	"STACK SIZE "\ .ECHO STKSIZ\ .ECHO " BYTES\n"
#if (STKSIZ < 48)
	.ECHO	"**** NOT ENOUGH ROOM FOR STACK! *****\n"
#endif

	.ORG	$0000
	.EJECT
