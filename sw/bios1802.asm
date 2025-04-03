	.TITLE	 BIOS for Spare Time Gizmos SBC1802
;	 Bob Armstrong [09-SEP-2021]


;       d8888b. d888888b  .d88b.  .d8888.  db .d888b.  .d88b.  .d888b. 
;       88  `8D   `88'   .8P  Y8. 88'  YP o88 88   8D .8P  88. VP  `8D 
;       88oooY'    88    88    88 `8bo.    88 `VoooY' 88  d'88    odD' 
;       88~~~b.    88    88    88   `Y8b.  88 .d~~~b. 88 d' 88  .88'   
;       88   8D   .88.   `8b  d8' db   8D  88 88   8D `88  d8' j88.    
;       Y8888P' Y888888P  `Y88P'  `8888Y'  VP `Y888P'  `Y88P'  888888D 
;
;         Copyright (C) 2021-2024 By Spare Time Gizmos,Milpitas CA.

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
;0000000001111111111222222222233333333334444444444555555555566666666667777777777
;1234567890123456789012345678901234567890123456789012345678901234567890123456789

	.MSFIRST \ .PAGE \ .CODES

	.NOLIST
	.INCLUDE "sbc1802.inc"	; SBC1802 hardware and environment definitions
	.INCLUDE "bios.inc"	; Mike's BIOS entry points and declarations
	.LIST
	
	.SBTTL	ElfOS Compatible BIOS

;++
;   This file is an ElfOS compatible BIOS for the SBC1802.  The original
; BIOS was written by Mike Riley for his various 1802 systems and then was
; later modified by this author to support the Elf 2000 and now the SBC1802.
;
;   Mike's original BIOS really contained two different kinds of code - there
; was a group of functions that were hardware dependent and which served to
; abstract the hardware implementation and thus make it possble to run ElfOS on
; different machines.  After being ported several times to different 1802
; implementations this part of the original BIOS was getting pretty cluttered
; with #ifdef's and was difficult to maintain.  
;
;   But Mike also used the BIOS as a kind of subroutine library and there was
; a fair amount of code in there which had no hardware dependencies at all.
; This includes things like string functions (strcpy, memcpy, strcmp, ltrim,
; isalnum, etc), hexadecimal and decimal conversions (hexin, hexout, atoi,
; uintout, ...), parser functions (findtkn, idnum, ...) and arithmetic functions
; (mul16, div16).  There really is no reason to change these when porting to a
; new hardware platform.
;
;   One of the ideas here is to split the hardware dependent and independent
; portions of the BIOS into separate files to make them easier to maintain.
; Unfortunately we don't have a relocating linking loader available for building
; the BIOS image, but we do have a fair amount of EPROM available.  That makes
; it easy enough to arbitrarily partition the code into separate memory chunks.
;--

	.SBTTL	Revision History

;++
; REVISION HISTORY
;
; 001	-- Start from scratch!
;
; 002	-- CONOUT needs a SEX SP after the OUTI!!!
;
; 003	-- GTOD and STOD should return DF=0 for success.  Yes, that's the
;	   OPPOSITE of how RTCTEST works!
;
; 004	-- Make sure to do a DEC SP after every OUT in DISKWR and SETLBA!
;
; 005	-- WREADY needs to preserve P3
;
; 006	-- DISKID needs to swap the byte order
;
; 007	-- F_IDERESET needs to set both master and slave to 8 bit mode
;
; 008	-- More fixes to IDDCMD to correctly detect whether a slave exists
;
; 009	-- Add the SBIOS tertiary entry vectors and the TRAP/TRAPx routines
;
; 010	-- Add a separate copyright notice to this BIOS
;
; 011	-- Add F_RUN to start a program with P=0
;
; 012	-- Add F_FREEMEM and F_GETDEV
;
; 013	-- A misplaced SEX SP in SETLBA screwed up the LBA3 register
;
; 014	-- Implement F_USETBD and F_SETBD (autobaud!)
;
; 015	-- Add WARMBOOT and COLDBOOT (F_BOOTIDE and F_BOOT)
;
; 016	-- Add disk partition support - F_GETPART, F_SETPART, F_PARTREAD and
;	   F_PARTWRITE...
;
; 017	-- Make sure the ElfOS bootstrap selects the ElfOS memory map first!!
;
; 018	-- Remove disk partition code and support.
;
; 019	-- Fix CONGET so that it correctly implements the ECHO bit.
;
; 020	-- Re-arrange some of the code to better fit around the vector tables.
;	   Add #ifs to generate assembly error if a table is overwritten.
;
; 021	-- Add TU58 support!
;
; 022	-- Add F_SDREAD, F_SDWRITE and F_SDRESET functions.
;
; 023	-- ADD F_SDBOOT function, rewrite IDEBOOT and WARMBOOT.
;
; 024	-- Clean up the TUREAD code to reduce the number of register required.
;	   Save and restore the registers TUREAD does use.
;	   ElfOS finally boots from a TU58!!!
;
; 025	-- TUINIT needs to save and restore T2 (used by TUGETC!)
;
; 026	-- Implement the "extended" BIOS version pointer.
;
; 027	-- Implement the SBC1802 specific F_HWFLAGS function.
;
; 028	-- Make TUINIT give error return if SLU1 is not installed.
;
; 029	-- Implement F_SDSIZE function, and F_TUSIZE.
;
; 030	-- Change calling sequence for DISKID/DISKSZ to be compatible
;	   with F_IDEREAD/F_IDEWRITE/F_SDSIZE.
;
; 031	-- Add F_SDIDENT (SBC1802 specific) and TUIDENT functions ...
;
; 032	-- Add F_BTCHECK (SBC1802 specific) as an entry for btcheck ...
;
; 033	-- Fix bug in SDTYPE for TU58 units.
;
; 034	-- update btcheck for new boot sector checksum
;
; 035	-- Jam the SCRT code right up against the BIOS version to free up
;	   a few bytes.  Change the definitions of F_CALL and F_RETURN.
;
; 036	-- Move F_GETDEV and F_FREEMEM code to just before SCRT to free
;	   up a few bytes before $F800.  Rewrite the F_HWFLAGS code to
;	   be more clever (and save a few bytes!).
;
; 037	-- Change TRAP not to push R2 twice (and save a few bytes).
;
; 038	-- Fix TUINIT to send breaks the way the CDP1854 really works!
;
; 039	-- The CDP1854 trashes the first character transmitted after a BREAK.
;	   Always transmit a null after a BREAK!
;
; 040	-- Make CONOUT check for CTS before sending
;
; 041	-- Remove DTTOAS, TMTOAS, ASTODT and ASTOTM from the BIOS.  All these
;	   functions are now unimplemented.
;
; 042	-- Remove F_IDSIZE, F_IDIDENT, F_SDSIZE, F_SDIDENT, TUSIZE, TUIDENT,
;	   etc from the BIOS.  All these functions are now unimplemented!
;
; 043	-- COLDBOOT doesn't work - you can't call F_INITCALL with PC == R3!
;
; 044	-- Change to use DLY1MS instead of DLY2MS ...
;
; 045	-- Remove DRVRST and SDRESET.  Drives are reset only once, at startup.
;
; 046	-- Make TUREAD and TUWRITE handle drive reported errors better.
;
; 047	-- Change the BIOS version to MANUFACTURER and EDIT LEVEL, like MiniDOS.
;
; 048	-- Do away with FGD_EVER in F_GETDEV.
;
; 049	-- Move btcheck and F_BTCHECK to the mapped part of the code.
;	   Remove the "bootable" check from SDBOOT.
;
; 050	-- Fix INISCRT (F_INITCALL) so that it works even if P=R3.
;
; 051	-- Change F_GETHWF to F_TESTHWF to directly test configuration.
;
; 052	-- Rewrite DRVSEL to a) check HWFLAGS to verify the selected drive is
;	   installed, and b) to wait for BUSY=0 before changing the selection.
;
; 053	-- Rewrite DISKRD/DISKWR to remove timeouts and consistently return
;	   an error code in the event of failure.
;
; 054	-- Implement the MiniDOS style multiple device addressing in the
;	   MDREAD/MDWRITE routines.  By defaultl, F_IDEREAD and F_IDEWRITE call
;	   these now.  F_SDREAD and F_SDWRITE however, work as before.
;
; 055	-- The character count for F_INPUTL should not include the terminating
;	   NULL character.
;
; 056	-- MDREAD and MDWRITE must preserve T2.1, otherwise it breaks MiniDOS.
;
; 057	-- TUREAD and TUWRUTE also need to actually preserve T2 (and not just
; 	   zero it!).  More over, they need to take the error return if T2 != 0
;	   (i.e. block number .GT. 65535).
;
; 058	-- Implement SL1OUT, SL1HIT, SL1IN, SL1GET and SL1NBR routines.  These
;	   parallel the SLU0 console routines, but for SLU1.  Use SL1OUT for
;	   the TU58 code.
;
; 059	-- Add the BD.RTS bit in BAUD.1 to enable/disable flow control.
;
; 060	-- Add the BD.ALT bit in BAUD.1 to redirect all the CONxxx functions
;	   to use SLU1 instead.
;
; 061	-- Make SL1GET obey the echo flag in BAUD.1 just as CONGET does...
;
; 062	-- Remove F_RTCTEST (it's never used) and just have F_GETTOD and
;	   F_SETTOD test the hardware flags directly.
;
; 063	-- Add the hardware test for SLU1 back to TUINIT.  And at the same time
;	   also check whether SLU1 is being used as an alternate console.
;
; 064	-- TUINIT accidentally corrupts the control register of SLU1!
;
; 065	-- Add parallel printer functions F_PRTINIT, F_PRTCHAR, F_PRTTEXT, and
;	   F_PRTSTAT.
;
; 066	-- Remove F_FINDTKN and F_IDNUM and make them unimplemented.  Change
;	   F_RDNVR, F_WRNVR, and F_NVRCCHK to always return failure (DF=1)
;	   but they are not actually unimplemented.
;
; 067	-- Change F_NBREAD and F_SL1NBR to implement a timeout so as to agree
;	   with David's MBIOS.
;
; 068	-- TUINIT corrupts EEPROM!  Missing a SEX SP ...
;
; 069	-- For some printers the BUSY pulse is shorter than the time it took us
;	   to detect it!  Rewrite the PRTCHAR code to work with these printers.
;--
VEREDT	.EQU	68	; and the edit level

;TODO

	.SBTTL	BIOS Memory Layout

;++
;   The memory image for the BIOS is unfortunately constrained by several
; factors that we don't have much control over, and it's important to understand
; these so you can work around the limitations.  This BIOS fits in the 4K of
; memory from $F000 up to the end of memory at $FFFF.  Within this space we have
; these "magic" locations that we can't mess with  -
;
; $F000	- SBC1802 entry vectors, copyright, context save and restore
;	- hardware independent BIOS routines
;	- console UART primitives, console I/O routines
;	- console line input w/editing (F_INPUTL)
;	... approximately 255 bytes free
;
; $F800	- extended BIOS entry vectors
;	- CDP1879 real time clock code
;	- parallel (Centronics) printer support
;	- IDE/ATA disk code
;	- TU58 serial tape or disk code
;	- disk/tape bootstrap
;	... approximately 98 bytes free
;
; $FE00	- RAMPAGE, MCR, CDP1877 PIC, and CDP1879 RTC
;
; $FF00	- primary BIOS entry vectors
; 	- SCRT routines, F_FREEMEM and F_GETDEV
;	... approximately 35 bytes free
; $FFE0	- LBR to the SCRT "CALL" routine
; $FFF1	- LBR to the SCRT "RETURN" routine
; $FFF7	- pointer to BIOS copyright notice (2 bytes)
; $FFF9	- BIOS version number (three bytes)
; $FFFC	- EPROM checksum (four bytes)
;
;   The three BIOS entry vector tables at $F000, $F800 and $FF00 consist of a
; series of LBR instructions to various routines within the BIOS.  These entry
; points are defined in the BIOS.INC file and are hardwired into ElfOS and
; pretty much all ElfOS programs.  The page at $FF00 is full and no new vectors
; will be added there, however if it becomes necessary to add BIOS functions in
; the future then new vectors will be added to the end of the table at $F800.
; The table at $F000 is for "manufacturer" specific functions only!
;
;   The page at $FExx is actually RAM and memory mapped peripherals, and
; references to this space don't access EPROM at all.  Our scratch pad RAM
; extends from $FE00 to $FEDF (224 bytes) and is shared with the other SBC1802
; firmware.  Definitions for the things stored in this RAM space are found in
; the SBC1802.INC header.  The remainder of this page consists of memory mapped
; peripherals, notably -
;
; $FEE7		- memory control register (MCR)
; $FEE8..$FEEF	- CDP1879 real time clock
; $FEF0..FEFF	- CDP1877 priority interrupt controller
;
; These are also defined in SBC1802.INC.  Note that this RAM space and these
; peripherals are mapped in ALL memory maps.
;--

	.SBTTL	SBC1802 Specific Entry Vectors

;++
;   The BIOS uses a table (actually, three separate tables!) of LBR instructions
; to dispatch to the correct function.  Each entry vector is located at a
; permanently fixed address defined by a name in BIOS.INC and, needless to say,
; it's critical that the code we generate match up with the definitions there.
; This macro generates an entry vector AND verifies that it is at the address
; specified by BIOS.INC.
;--
#define BENTRY(vec,rtn)	\#if (vec-$)
#defcont		\	.echo "VECTOR ERROR FOR vec\n"
#defcont		\#endif
#defcont		\	LBR	rtn

	.ORG	SBIOS
;   These vectors all transfer control to a user program in RAM.  The tricky
; part of that is dealing with the memory mapping, and that's why they have to
; be part of the BIOS ('cause it's always mapped in any mode!).
	BENTRY(F_TRAP,      TRAP)	; SBC1802 debugger trap vector
	BENTRY(F_CONTINUE,  CONTINUE)	; restore user context and continue
	BENTRY(F_RUN,       RUNPGM)	; run user program (with P=0)
; SBC1802 specific functions for accessing the TU58 ...
	BENTRY(F_TUINIT,    TUINIT)	; initialize TU58
	BENTRY(F_TUREAD,    TUREAD)	; read TU58 sector
	BENTRY(F_TUWRITE,   TUWRITE)	; write TU58 sector
; SBC1802 SLU1 specific functions ...
	BENTRY(F_SL1OUT,    SL1OUT)	; blocking output to SLU1
	BENTRY(F_SL1HIT,    SL1HIT)	; test SLU1 for input
	BENTRY(F_SL1IN,	    SL1IN)	; blocking read w/o echo
	BENTRY(F_SL1GET,    SL1GET)	; blocking read with echo
	BENTRY(F_SL1NBR,    SL1NBR)	; non-blocking read w/o echo
; Other SBC1802 specific functions ...
	BENTRY(F_TESTHWF,   TESTHW)	; test SBC1802 hardware flags
; Parallel (Centronics) printer functions ...
	BENTRY(F_PRTINIT,   PRTINIT)	; initialize parallel port and printer
	BENTRY(F_PRTCHAR,   PRTCHAR)	; print one ASCII character
	BENTRY(F_PRTTEXT,   PRTTEXT)	; print null terminated string
	BENTRY(F_PRTSTAT,   PRTSTAT)	; return printer status bits

;   The copyright notice always follows the last SBC1802 vector.  There's also a
; BIOS version number, but that's located at the end of this file (right before
; the checksum!).
RIGHTS:	.TEXT	"Copyright (C) 2021-2025 by Spare Time Gizmos.\r\n"
	.TEXT	"Portions Copyright (C) 2004-2020 by Mike Riley.\r\n"
	.TEXT	"All rights reserved.\r\n"
	.BYTE	0

	.SBTTL	Save User Context

;++
; BREAKPOINT TRAPS
;   Whenever the firmware starts a user program running it will initialize R1
; with the address of this routine.  Provided that the user program does not 
; use R1 or R0 for anything, a break point can be placed in the program with
; the two byte sequence:
;
;	MARK	; $79
;	SEP  R1	; $D1
;
;   R1, as you may remember, normally points to an interrupt service routine and
; R0 is the DMA pointer.  This code will save all of the user's registers, with
; the exception of R0 and R1, but including X, P, D and DF.
;
;   This code has to be here, in the BIOS, so that it will be mapped into memory
; at all times.  The rest of the firmware EPROM is unmapped whenever we're
; running an ElfOS or MicroDOS program.
;--

;++
; MINIMON
;   Mike's original BIOS contained a "minimon" routine, which was a super simple
; debug monitor that allowed you to examine and modify memory and start program
; execution.  We have a more sophisticated monitor in our EPROM, but we retain
; the BIOS F_MINIMON function as another way to get there.  We treat this much
; like another breakpoint and try to save as much of the user context as we can.
; Whether this is useful is debatable, but it no harm is done.
;
;   Saving X and P is pretty much a waste since we know P=3 and X is most likely
; 2, but the TRAP code expects this to be on the stack.  Also, notice that the
; PC which gets saved will be the R3 value right before the "SEP R1", and it's
; worth thinking about what would happen if the user tells us to CONTINUE in
; that case.  That's why there's an LBR back to MINIMON after the SEP R1...
;--
MINIMON:STR	SP		; save D temporarily
	RLDI(R1,TRAP)		; make sure R1 is valid
	LDN	SP		; restore D
	MARK			; push (X,P) on the stack
	SEP	R1		; branch to TRAP to save all the registers
	LBR	MINIMON		; in case of CONTINUE!


;   The CONTINUE routine (see the next page) will branch to TRAPX as the final
; step in restoring the user's context.  This restores the original X and P
; registers while leaving R1 pointing at TRAP: once again.
TRAPX:	SEX R2\ DIS		; restore (X,P) and turn interrupts off

;   Save the current D and DF, assuming that there's a valid stack pointer in R2
; (and that it points to a valid stack!).  The MARK instruction executed by the
; breakpoint has already pushed (X,P)...
TRAP:	SEX R2\ STXD		; save D on the user's stack
	LDI 0\ SHLC\ STR R2	; and save DF on the stack too

; Save registers RF thru R2 in memory at REGS:...
	RLDI(R0,REGS+32-1)\ SEX R0
	PUSHR(RF)
	PUSHR(RE)
	PUSHR(RD)
	PUSHR(RC)
	PUSHR(RB)
	PUSHR(RA)
	PUSHR(R9)
	PUSHR(R8)
	PUSHR(R7)
	PUSHR(R6)
	PUSHR(R5)
	PUSHR(R4)
	PUSHR(R3)
	DEC R0\ DEC R0		; skip over R2 (we'll fix that later)

;   The next two registers, R1 and R0, don't really contain useful data but
; we need to skip over those before we can save D, DF and X...
	LDI	00
	STXD\ STXD		; save R1 as all zeros
	STXD\ STXD		; and save R0 as all zeros

; Recover DF, D and X from the user's stack and save them in our memory...
	LDN	R2		; get the user's DF
	STXD			; store that at SAVEDF:
	INC R2\ LDN R2		; then get the user's D
	STXD			; and store that at SAVED:
	INC R2\ LDN R2		; and lastly get the user's (X,P)
	STXD			; and store it at SAVEXP:

;   Finally, update the value of register 2 that we saved so that it shows the
; current (and correct) value, before we pushed three bytes onto the stack...
	RLDI(R0,REGS+5)
	PUSHR(R2)

;   We're all done saving stuff - change the memory map to ROM0 so that the rest
; of the EPROM is mapped from $8000 and up.  Note that we can't do this until
; AFTER we've saved the context, just in case the user's stack is located in 
; RAM above $8000!
	RLDI(DP,MCR)		; point to the memory control register
	LDI MC.ROM0\ STR DP	; select the ROM0 memory map

;   Lastly, (re)initialize our own firmware stack and the SCRT, just in case
; the user's program has messed them up.  After that, we're done here.  The
; F_INITCALL routine will return to $8005 (RESTA), which is the warm start
; and breakpoint trap entry vector in the main EPROM firmware.
	RLDI(SP,STACK)		; initialize the stack
	RLDI(A,WARM)		; continue processing from $8005
	LBR	F_INITCALL	; and initialize the SCRT routines

	.SBTTL	Restore User Contaxt

;++
;   This BIOS routine is called by the main SBC1802 EPROM firmware when we're
; ready to start or resume execution of a user program.  It will select the
; correct memory map, either ELFOS or MICRODOS, restore the user's registers
; from our RAMPAGE locations REGS, SAVED, SAVEDF and SAVEXP, and then start
; the user program running.
;
;   Note that the contents of R0 and R1 are not actually restored.  R0 is
; unused, and R1 will be loaded with the address of our TRAP routine (above).
; The latter is required for handling any breakpoint trap in the user program.
;
;   The main reason this code is here, in the BIOS area, is because of the
; SBC1802 memory mapping.  The majority of the EPROM code is only accessible in
; the ROM0/ROM1 maps, and as soon as we switch to th ELFOS or MICRODOS map then
; that code is gone.  This BIOS area, from $F000 and up, is always mapped and so
; this code has to live here.
;
;   You might be tempted to defer changing the memory map until the very last
; second, but remember that this code needs the user's stack to restore the
; last few registers.  The user stack might be somewhere in the RAM 1 space,
; and that's not accessible in ROM1/ROM0 either.
;
;CALL:
;	D - desired memory map (either MC.ELOS or MC.MDOS)
;	<the REGS area, et al, in RAMPAGE contains the user context>
;	CALL(F_CONTINUE)
;	<never return - jumps to the user's program instead!>
;--
CONTINUE:
	STR	SP		; save the desired memory map
	RLDI(DP,MCR)		; point to the memory control register
	LDN SP\ STR DP		; and select the desired memory map
	RLDI(R1,CONT1)\ SEP R1	; use R1 as the PC from now on

; Now restore the user's registers, R2 thru RF...
CONT1:	RLDI(R0,REGS+4)		; point R0 at the saved registers
	SEX	R0		; use that as a stack pointer
	POPR(R2)		; ...
	POPR(R3)		; ... 
	POPR(R4)		; ...
	POPR(R5)		; ...
	POPR(R6)		; ...
	POPR(R7)		; ...
	POPR(R8)		; ...
	POPR(R9)		; ...
	POPR(RA)		; ...
	POPR(RB)		; ...
	POPR(RC)		; ...
	POPR(RD)		; ...
	POPR(RE)		; ...
	POPR(RF)		; ...

;   Now recover (X,P), D and DF.  Unfortunately the only way to do this is to
; temporarily push them on the user's stack first, which works fine as long as
; R2 points to a valid stack!
	RLDI(R0,SAVEXP)		; point R0 at (X,P), D and DF...
	LDXA			; get (X,P)
;   Note that the final instruction we execute, DIS (or RET - it doesn't matter)
; is a little backwards in that it does M(R(X))->(X,P) FIRST, and then R(X)+1
; second.  So we have to decrement the user's stack pointer first, which has the
; effect of wasting a byte, but in the end it comes out right.
	DEC R2\ STR R2		; save (X,P) on the user's stack
	LDXA			; now load D from SAVED
	DEC R2\ STR R2		; save that on the user's stack
	LDXA			; finally load SAVEDF
	SHRC			; just restore restore DF directly
	SEX R2\ LDXA		; restore D from the user stack

; Change the POST display to just a decimal point ...
	SEX R1\ OUT LEDS\ .BYTE POSTDP

;   The last step is a standard 1802 trick - we have to restore (X,P) from the
; user's stack while at the same time leaving R1 (our current PC) pointing to
; the TRAP routine.  The contents of R0 are never restored...
	LBR	TRAPX	; go restore (X,P)

	.SBTTL	Run Program

;++
;   The BIOS F_RUN entry will run a user program using P=R0 and with pretty much
; the same conditions that you would get after a 1802 hardware reset.  The
; caller should pass the desired memory map, usually either MC.ELOS or MC.MDOS,
; in D and the desired starting address in R0.  Unlike F_CONTINUE, this routine
; DOES NOT use the previous user context saved at REGS et al.
;
;CALL:
;	D  - desired memory map (either MC.ELOS or MC.MDOS)
;	R0 - user program starting address
;	CALL(F_RUN)
;	<never return - jumps to the user's program instead!>
;
;   NOTE: On the 1802 CLEAR/RESET only guarantees that X=P=0, R0=0 and IE=1.
; None of the other registers are cleared.  We try a little harder and most
; of the registers will be cleared, with the exception of R0, R1, R2 and R3.
; R0 is the new PC, of course, and R1 is loaded the address of the breakpoint
; trap routine.  As a convenience R2 will be loaded with the top of RAM.
; R3 is our current PC right now, and we can't mess with that.
;
;   Unlike the real 1802, we don't set IE before starting.  That might cause
; a problem for some programs, but remember that a real 1802 inhibits interrupts
; for one instruction after RESET.  That gives the programmer a chance to do a
; DIS as the firsts instruction, but that won't work here.
;--
RUNPGM:	STR	SP		; save the desired memory map
	RLDI(DP,MCR)		; point to the memory control register
	LDN SP\ STR DP		; and select the desired memory map
	
; Clear all the registers we can ..
	RLDI(R1, F_TRAP)	; setup R1 for a possible breakpoint/trap
	RLDI(R2, USRRAM)	; and point R2 at the top of RAM
	LDI	0		; leave R3 alone
	PLO R4\ PHI R4		; and zero all the rest
	PLO R5\ PHI R5		; ...
	PLO R6\ PHI R6		; ...
	PLO R7\ PHI R7		; ...
	PLO R8\ PHI R8		; ...
	PLO R9\ PHI R9		; ...
	PLO RA\ PHI RA		; ...
	PLO RB\ PHI RB		; ...
	PLO RC\ PHI RC		; ...
	PLO RD\ PHI RD		; ...
	PLO RE\ PHI RE		; ...
	PLO RF\ PHI RF		; ...
	PLO R4\ PHI R4		; ...

; Clear D and DF, disable interrupts and set P=X=0 ...
	OUTI(LEDS, POSTDP)	; change the LEDs to just a decimal point
	CDF			; D is already zero, so clear DF too
	DIS			; disable interrupts
	 .BYTE	 $00		; set X=P=0 and we're outta here!

	.SBTTL	Unimplemented BIOS Function Trap
;++
;   We have, over time, accumulated a number of BIOS entry points which, for
; better or worse, are no longer implemented.  Rather than having these entry
; points branch to a simple NOP RETURN, instead we fill their vector with a
; CALL to this routine.  Conveniently a CALL opcode (SEP CALLPC) and an address
; word take exactly the same number of bytes as the usual "LBR xyz" so we can
; do this without messing up the vector table.
;
;   The effect is to push the caller's return address (currently in A) onto the
; stack and then save the address of the vector (or actually the vector address
; +3) in A.  This allows us to print an error message that says "UNIMPLEMENTED
; BIOS CALL" or some such and give both the vector that was called and the
; address it was called from.  Neat, no?
;
;   What we need to do here is 1) get the caller's return address off the stack,
; and 2) switch the memory map back to ROM0 so all the EPROM is mapped, and
; 3) switch back to the monitor stack at $FExx, and lastly 4) jump to the
; UNIMPL routine in the main monitor code.  That will take care of printing
; the message and then starting the firmware command scanner.
;
;   Why get the return address from the stack and then switch stacks?  Remember
; that when we get here we're running on the application program's stack, and
; that can be anything and anywhere.  It might even be in the upper part of
; RAM, which will be unmapped as soon we switch to the ROM0 map.  Rather than
; deal with that complexity later, it's easier to do it now.
;
;   BTRAP wants the vector address in P3 and the user address in P4.
;--
UNIMPL:	RCOPY(P3,A)		; put the vector address in P3
	SEX SP\ IRX\ POPRL(P4)	; and the caller's return address in P4
	RLDI(DP,MCR)		; point to the memory control register
	LDI MC.ROM0\ STR DP	; select the ROM0 memory map
	RLDI(SP,STACK)		; initialize the stack
	RLDI(A,WARM+3)		; continue processing from BTRAP
	LBR	F_INITCALL	; and initialize the SCRT routines

#define BUNIMP(vec)	\#if (vec-$)
#defcont		\	.echo "VECTOR ERROR FOR vec\n"
#defcont		\#endif
#defcont		\	CALL(UNIMPL)

	.SBTTL	Run Time Library Functions

	.include "rtl1802.asm"

	.SBTTL	Console UART I/O Primitives

;++
;   This routine writes one byte from D to the console UART SLU0.  If the
; UART transmitter is currently busy, then we'll wait until it's finished.
; Note that this returns with the original character still in D, unchanged.
; That's important for CONGET!
;--
CONOUT:	PUSHD			; save the byte to print
	GHI BAUD\ ANI BD.ALT	; alternate console selected?
	LBNZ	SL1OU0		; yes - use that
	OUTI(GROUP,BASEGRP)	; make sure the base board I/O group is selected
CONOU1:	SEX SP\ INP SL0STS	; read the UART status register
	ANI	SL.THRE		; is the transmitter busy?
	LBZ	CONOU1		; wait if it is
	GHI BAUD\ ANI BD.RTS	; is flow control enabled?
	LBNZ	CONOU2		; no - just output now
	LDN SP\ ANI SL.ES	; yes - check for CTS too
	LBZ	CONOU1		; wait if CTS is clear
CONOU2:	IRX\ OUT SL0BUF		; type the saved character
	DEC SP\ LDN SP		; restore the original character
	RETURN			; and we're done


;   Test to see if the console UART has a character ready to be read, and return
; with DF=1 if it does.  Don't wait, and don't actually read the character.
CONHIT:	GHI BAUD\ ANI BD.ALT	; alternate console selected?
	LBNZ	SL1HIT		; yes - use SLU1 instead
	OUTI(GROUP,BASEGRP)	; select the base board I/O group
	SEX SP\ INP SL0STS	; read the UART status
	SHR			; put the DA bit into DF
	RETURN			; and we're done


;   Read one character from the console UART and return it in D, waiting for
; one to become available if necessary.  The character read is NEVER echoed,
; regardless of the echo flag in the BAUD register.
CONIN:	GHI BAUD\ ANI BD.ALT	; alternate console selected?
	LBNZ	SL1IN		; yes - use SLU1 instead
	CALL(CONHIT)		; see if a character is ready
	LBNF	CONIN		; and wait until one is
	INP	SL0BUF		; read the receiver buffer
	RETURN			; and return the byte in D

;   This routine is the same as CONIN, however this one DOES echo the input
; but only if the echo flag in BAUD.1 is set...
CONGET:	GHI BAUD\ ANI BD.ALT	; is the alternate console selected?
	LBNZ	SL1GET		; yes - use that instead
	CALL(CONIN)		; read a character
	PLO	BAUD		; and save it for a moment
	GHI	BAUD		; see if echo is required
	SHR			; put the echo bit into DF
	GLO	BAUD		; and get the character back
	LBDF	CONOUT		; echo it if required
	RETURN			; or return without echo

	.SBTTL	Set Console UART Baud Rate

;++
;   This routine will set the baud rate and character format for the console
; SLU0 serial port.  Bits 0..3 of the D register select the baud rate
; according to this table -
;
;  D bits 0..2 select the baud 
;	000 -   300 baud
;	001 -  1200 baud
;	010 -  2400 baud
;	011 -  4800 baud
;	100 -  9600 baud
;	101 - 19200 baud
;	110 - ignored (current baud rate does not change)
;	111 -  "   "    "   "    "   "    "    "   "  "
;
;   On other systems (Elf2K, PicoElf) the remaining bits in D select the
; character format - number of bits, parity, stop bits, etc - however on
; the SBC1802 the console port is fixed at 8N1 so these bits are ignored.
;
;   Also note that baud rates 6 and 7 select 38.4kBPS and 76.8kBPS on other
; systems, but the SBC1802 maxes out at 19.2kBPS.
;--
CONSET:	ANI	7		; ignore everything except the baud rate
	ADI LOW(BDTAB1)\ PLO DP	; index into the baud rate table
	LDI HIGH(BDTAB1)	; ...
	ADCI 0\ PHI DP		; ...
	LDN	DP		; get the corresponding COM8116/8136 code
	LBZ	CONSE1		; do nothing if it's 38.4 or 76.8kBPS
	STR	SP		; save it for a moment
	RLDI(DP,SLBAUD)		; point to our saved baud rate
	LDN DP\ ANI BD.SLU1	; clear out the SLU0 bits
	OR\ STR DP		; set the new baud rate
	OUTI(GROUP,BASEGRP)	; select the base board I/O group
	SEX DP\ OUT SLUBRG	; update the COM8116/8136
CONSE1:	CDF\ RETURN		; always return DF=0 for success

; This is a table of the COM8116/8136 baud rate settings ...
BDTAB1:	.BYTE	BD.300,  BD.1200, BD.2400, BD.4800
	.BYTE	BD.9600, BD.19.2, 0,       0

	.SBTTL	Console UART Autobaud

;++
;   The BIOS F_SETBD call will try to automatically determine the correct baud
; rate for the console (don't confuse this call with F_USETBD, which calls 
; the CONSET routine above).  The basic idea is for the user to press RETURN
; on his terminal and we guesstimate his baud rate from that.
;
;   We start by initializing the UART for 9600 baud and wait for something to
; be received.  Obviously, if his terminal is set to 9600bps we will actually
; receive a 0Dh character in the UART buffer.  But even if his terminal is set
; to a faster or slower baud rate, assuming that he is still typing carriage
; return, the bit pattern we receive is totally unique.  For example, if he
; transmits CR at 4800 bps and we receive at 9600bps, well actually see 0E6h
; in the buffer.  So by looking up whatever value we receive in a table, we
; can determine the correct baud rate.
;
;   Assuming the UART is set for 9600 baud this will work for terminal baud 
; rates from 2400 to 19,200.  Baud rates below 2400 aren't reliable because the
; UART receiver will finish before the terminal UART gets past the start bit.
; The solution is to program our UART for 1200 baud and try again.  This second
; attempt allows us to recognize baud rates from 300 thru 2400 and should cover
; everything we need.
;
;   Note that baud rates below 300 are not supported (does anybody actually
; have an ASR33 out there???) nor are baud rates above 19200 (the COM8116
; doesn't go any faster than that).
;--
AUTOBAUD:LDI 4\ CALL(CONSET)	; set the UART for 9600 baud
	CALL(AUTOBD)		; try to recognize a <CR>
	 .WORD	ABDTB1		;  ... autobaud table for 9600 baud
	LBNZ	AUTOB9		; branch if we were successful
	LDI 1\ CALL(CONSET)	; no match - try at 1200 baud
	CALL(AUTOBD)		; try again to find a <CR>
	 .WORD	ABDTB2		;  ... autobaud table for 1200 baud
	LBZ	AUTOBAUD	; loop forever until we find a match

; Here if we successfully find a match!
AUTOB9:	RLDI(BAUD,$0100)	; ALWAYS set the echo flag
	RETURN			; and we're done


; Table of autobaud results w/UART set to 9600 baud ...
ABDTB1:	.BYTE	$0D, 4		;  9,600bps
	.BYTE	$FE, 5		; 19,200bps
	.BYTE	$E6, 3		;  4,800bps
	.BYTE	$78, 2		;  2,400bps
	.BYTE	0		; end of table

; Table of autobaud results w/UART set to 1200 baud ...
ABDTB2:	.BYTE	$0D, 1		; 1,200bps
	.BYTE	$FE, 2		; 2,400bps
	.BYTE	$78, 0		;   300bps
	.BYTE	0		; end of table


;   This routine will do the real work for AUTOBAUD.  It clears the UART, waits
; for something to be received, and then tries to look that up in the table
; passed by the caller.  If it finds a match then it sets the UART to the
; correct baud rate according to the table and returns with DF=1.  If it doesn't
; find a match then it returns DF=0.
AUTOBD:	INP SL0STS\ SHR		; read the DA bit from the UART status register
	LBNF	AUTOBD		; wait for DA to be set
	INP	SL0BUF		; get the character and put it on the stack
	SEX A\ POPR(P1)		; point at the baud rate table
AUTOB2:	LDA	P1		; get a byte from the baud table
	LBZ	AUTOB4		; branch if end of table
	SEX SP\ XOR		; compare it to the byte on the stack
	LBZ	AUTOB3		; branch if we found a match
	INC	P1		; no match - skip the baud byte
	LBR	AUTOB2		; and keep looking

; Here if we found a match - get the baud rate and set it ...
AUTOB3:	LDN P1\ CALL(CONSET)	; set the correct baud rate
	LDI $FF\ LSKP		; and return DF=1

; Here if don't find a match ...
AUTOB4:	LDI 0\ PUSHD		; save the return code on the stack

;   Remember that were receiving at a fairly fast rate (9600bps) and if the
; user was sending at a slow rate (e.g. 2400bps) then his UART is PROBABLY
; STILL SENDING right now!  To avoid having the last half of the CR show up as
; a garbage character, we have to delay long enough for the transmitter to
; finish.  Worst case at 300 baud a character takes 33ms to send, so we'll
; delay for 100ms just to be safe.
	CALL(DLY100)		; delay for 100ms

; Read the UART buffer and status to be sure all errors are cleared ...
	INP SL0BUF\ INP SL0STS	; ...
	POPD\ SHL\ RETURN	; return the result in DF and we're done

	.SBTTL	Auxiliary SLU1 I/O Primitives

;++
;   This routine writes one byte from D to the auxiliary UART SLU1.  If the
; UART transmitter is currently busy, then we'll wait until it's finished.
; Note that this returns with the original character still in D, unchanged.
;--
SL1OUT:	PUSHD			; save the byte to print
SL1OU0:	OUTI(GROUP,SL1GRP)	; be sure the SLU1 I/O group is selected
SL1OU1:	SEX SP\ INP SL1STS	; read the UART status register
	ANI SL.THRE\ LBZ SL1OU1	; wait for THRE to set
	IRX\ OUT SL1BUF		; output the character we saved
	DEC SP\ LDN SP		; restore the original character
	OUTI(GROUP,BASEGRP)	; just to be safe
	RETURN			; and we're done


;   Test to see if the auxiliary UART has a character ready to be read, and 
; return with DF=1 if it does.  Don't wait, and don't actually read the
; character.
SL1HIT:	OUTI(GROUP,SL1GRP)	; select the SLU1 I/O group
	SEX SP\ INP SL1STS	; read the UART status
	SHR			; put the DA bit into DF
	RETURN			; and that's it


;   Read one character from the auxiliary UART and return it in D, waiting for
; one to become available if necessary.  The character read is NEVER echoed.
SL1IN:	CALL(SL1HIT)		; see if a character is ready
	LBNF	SL1IN		; and wait until one is
	INP	SL1BUF		; read the receiver buffer
	RETURN			; and return the byte in D

;   This routine is the same as SL1IN, however this one DOES echo the input,
; but only if the echo flag in BAUD.1 is set...
SL1GET:	CALL(SL1IN)		; read a character
	PLO	BAUD		; and save it for a moment
	GHI	BAUD		; see if echo is required
	SHR			; put the echo bit into DF
	GLO	BAUD		; and get the character back
	LBDF	SL1OUT		; echo it if required
	RETURN			; or return without echo

	.SBTTL	Non-Blocking Serial Port Read

;++
;   The F_NBREAD call (and the F_SL1NBR call for SLU1) tries to read one
; character from the serial port, but with a timeout.  If no character is
; received within the specified period then this call returns immediately with
; DF=0.  If there is a character, then we read it, echo it according to BAUD.1,
; and return it in with DF=1.
;
;CALL:
;	<P3 contains the timeout, in milliseconds>
;	CALL(F_NBREAD)
;	<return with DF=1 and character in D, otherwise timeout and DF=0>
;
;   Yes, DF=1 for success and DF=0 for timeout ("failure").  That's the opposite
; of the usual BIOS convention, but it's consistent with the way F_BTEST, UTEST,
; and BRKTEST work.  
;
;   And note that the timeout passed in P3 is in milliseconds, which gives a
; maximum wait of just over one minute.  Zero implies no wait, and F_NBREAD
; returns immediately if no character is available.
;--
NBREAD:	GHI BAUD\ ANI BD.ALT	; alternate console selected?
	LBNZ	SL1NBR		; yes - use SLU1
NBREA1:	CALL(CONHIT)		; see if a character is waiting
	LBDF	NBREA3		; branch if there's something there
	GLO P3\ LBNZ NBREA2	; has the timeout expired?
	GHI P3\ LBZ NBREA9	; yes - return DF=0 and quit now
NBREA2:	DLY1MS			; wait for 1ms
	DEC P3\ LBR NBREA1	; decrement the timeout and try again
NBREA3:	CALL(CONGET)		; character waiting - read it and echo
	SDF			; make sure DF=1
NBREA9:	RETURN			; and we're done	


; This function is identical to NBREAD, but for SLU1 ...
SL1NBR:	CALL(SL1HIT)		; see if a character is waiting
	LBDF	SL1NB3		; branch if there's something there
	GLO P3\ LBNZ SL1NB2	; has the timeout expired?
	GHI P3\ LBZ NBREA9	; yes - return DF=0 and quit now
SL1NB2:	DLY1MS			; delay for 1 millisecond
	DEC P3\ LBR SL1NBR	; decrement the timeout and keep trying
SL1NB3:	CALL(SL1GET)		; actually read the character and echo
	SDF\ RETURN		; return DF=1 and we're done

	.SBTTL	Console Output Primitives

;   These routines are either used by INPUT (F_INPUTL), or are direct
; BIOS console output primitives themselves ...

; Type a CARRIAGE RETURN, LINE FEED pair ...
TCRLF:	LDI	CH.CRT		; CARRIAGE RETURN
	CALL(CONOUT)		; ...
	LDI	CH.LFD		; LINE FEED
	LBR	CONOUT		; ...


;   Type a "funny" character - ASCII control characters are printed as "^x",
; where "x" is the printing equivalent.  All other characters print normally.
; the exception of ESCAPE ($1B) which prints as "$".
TFCHAR:	SMI	' '		; is it any control character ?
	LBDF	TFCHA1		; no - just print it
	PUSHD			; yes - save the character for a moment
	LDI	'^'		; and print a "^" first
	CALL(CONOUT)		; ...
	POPD			; then get the character back
	ADI	'@'		; convert to its printing equivalent
TFCHA1:	ADI	' '		; restore the original character
	LBR	CONOUT		; and print it


; Type the sequence <BACKSPACE> <SPACE> <BACKSPACE> to erase the last character.
TBACKSP:LDI	CH.BSP		; type <backspace>
	CALL(CONOUT)		; ...
	LDI	' '		; <space>
	CALL(CONOUT)		; ...
	LDI	CH.BSP		; <backspace>
	LBR	CONOUT		; ...


;   Type an ASCIZ (null terminated ASCII) string on the console.  P1 points to
; the string, and we return when we find a zero byte.  Notice that, on return,
; P1 points to the next byte AFTER the end of string.  That's critical!
TTEXT:	LDA	P1		; get the next character to type
	LBZ	TTEXT1		; branch if we're done
	CALL(CONOUT)		; nope - type this on the console
	LBR	TTEXT		; and keep typing
TTEXT1:	RETURN			; here when we find a null


;   Type an ASCIZ string "in line" after the CALL.  This is basically the same
; as TTEXT, but this time A points to the string instead of P1 ...
INLMSG:	LDA	A		; get the next character
	LBZ	TTEXT1		; return when we find a null
	CALL(CONOUT)		; bot yet - print this one
	LBR	INLMSG		; and keep going

	.SBTTL	Console Line Input w/Editing

;++
;   The INPUT and INPUTL routines read an entire line from the console and
; store the result in a buffer provided by the caller.  Text is echoed as
; it is read and several intraline editing characters are recognized -
;
;   BACKSPACE ($08) - erase the last character input
;   DELETE    ($7F) - same as BACKSPACE
;   CONTROL-U ($15) - erase the entire line and start over
;   RETURN    ($0D) - echos <CRLF>, terminates input and returns
;   LINE FEED ($0A) - same as RETURN
;   ESCAPE    ($1B) - echos "$" and <CRLF>, then terminates input and returns
;   CONTROL-C ($03) - aborts all input and returns with DF=1
;
;   The BACKSPACE, DELETE and CONTROL-U functions all work by echoing the
; <backspace> <space> <backspace> sequence and assume that you're using a CRT
; terminal (does anybody even use hardcopy these days?).
;
;   The line terminators, RETURN, LINE FEED, ESCAPE and CONTROL-C all insert a
; NULL byte in the buffer to end the string, however the terminator itself is
; not added to the buffer. CONTROL-C returns with DF=1 to signal that input was
; aborted, but the other terminators all return with DF=0.
;
;   Other control characters echo as "^x" where "x" is the printing equivalent,
; however the original ASCII control character will be added to the buffer.
; If the buffer overflows during input then a BELL ($07) will be echoed for
; everything instead of the original character.
;
;   Lastly, INPUT and INPUTL are identical with the exeption that INPUTL allows
; the caller to specify the buffer size, but with INPUT it is assumed to be 256
; bytes.   And note that the buffer size DOES NOT include the terminating NULL
; character!
;
; CALL:
;	P1/RF <- buffer address
;	P3/RC <- buffer size (for INPUTL only)
;	CALL(F_INPUTL)
;	DF=1 (CONTROL-C typed) or DF=0 otherwise
;--
INPUT:	RLDI(P3,256)		; set the buffer size
				; and then fall into INPUTL
INPUTL:	PUSHR(T1)		; save a work register
	LDI 0\ PLO T1		; and keep a byte count there

; Read the next character and figure out what it is ...
INPUT1:	CALL(CONIN)		; read WITHOUT echo
	ANI	$7F		; always trim this input to 7 bits
	LBZ	INPUT1		; ignore nulls
	PLO	BAUD		; and save the character temporarily
	SMI	CH.CTC		; CONTROL-C?
	LBZ	INPCTC		; ...
	SMI	CH.BSP-CH.CTC	; BACKSPACE?
	LBZ	INPBSP		; ...
	SMI	CH.LFD-CH.BSP	; LINE FEED?
	LBZ	INPEOL		; ...
	SMI	CH.CRT-CH.LFD	; CARRIAGE RETURN?
	LBZ	INPEOL		; ...
	SMI	CH.CTU-CH.CRT	; CONTROL-U?
	LBZ	INPCTU		; ...
	SMI	CH.ESC-CH.CTU	; ESCAPE?
	LBZ	INPESC		; ...
	SMI	CH.DEL-CH.ESC	; DELETE?
	LBZ	INPBSP		; ...

;   Here for any normal, average, boring, printing ASCII character.  If the
; buffer isn't full, then store this one and echo it.  If the buffer IS full,
; then echo a bell instead and don't store anything.  Remember that we always
; need at least one empty byte left over to store the terminating null!
INPUT2:	GHI P3\ LBNZ INPU2A	; if the buffer size .GE. 256 then no worries
	GLO P3\ SMI 1		; be sure there is at least 1 byte free
	LBNF	INPBEL		; nope - the buffer is full
INPU2A:	GLO BAUD\ STR P1	; store the original character in the buffer
	CALL(TFCHAR)		; and echo it
	DEC	P3		; decrement the buffer size
	INC	T1		; increment the character count
	INC	P1		; and and increment the buffer pointer
	LBR	INPUT1		; then go do it all over again

; Here if the buffer is full - echo a bell and don't store anything ...
INPBEL:	LDI	CH.BEL		; ...
	CALL(CONOUT)		; ...
	LBR	INPUT1		; and wait for a line terminator

; Here for a BACKSPACE or a DELETE - try to erase the last character ...
INPBSP:	CALL(DELCHR)		; do all the real work
	LBDF	INPBEL		; ring the bell if there's nothing there
	LBR	INPUT1		; and then keep going

; Here for CONTROL-U - erase ALL the characters back to the prompt ...
INPCTU:	CALL(DELCHR)		; try to delete the last character
	LBNF	INPCTU		; and keep going until the buffer is empty
	LBR	INPUT1		; then start over again

; Here for a CONTROL-C - echo ^C and then terminate input ...
INPCTC:	GLO	BAUD		; get the CONTROL-C back
	CALL(TFCHAR)		; echo ^C
	SDF			; return DF=1
	LBR	INPUT3		; and fall into the rest of the EOL code

; Here for ESCAPE - echo "$" and terminate the input ...
INPESC:	LDI	'$'		; echo a '$'
	CALL(CONOUT)		; ...
				; and fall into the rest of the EOL code

; Here for RETURN or LINE FEED ...
INPEOL:	CDF			; return DF=0
INPUT3:	CALL(TCRLF)		; echo <CRLF> regardless 
	LDI	0		; then terminate the input string
	STR	P1		; ...
	IRX\ POPRL(T1)		; restore T1
	RETURN			; and we're finally done!


;   This little subroutine erases the last character from the buffer, and from
; the screen as well.  If the buffer is currently empty, then it does nothing
; and returns with DF=1...
DELCHR:	SDF			; assume DF=1
	GLO	T1		; get the character count
	LBZ	DELCH2		; if it's zero then quit now
	CALL(TBACKSP)		; erase the character from the screen
	INC	P3		; increment the buffer size
	DEC	T1		; decrement the character count
	DEC	P1		; and back up the buffer pointer
	LDN	P1		; get the character we just erased
	ANI	~$1F		; is it a control character??
	LBNZ	DELCH1		; no - quit now
	CALL(TBACKSP)		; yes - erase the "^" too
DELCH1:	CDF			; and return with DF=0
DELCH2:	RETURN			; ...

	.SBTTL	Test SBC1802 Hardware Configuration

;++
;   This routine will check whether a particular hardware option is installed
; on this system.  The argument, which is passed inline, is a mask of the
; HWFLAGS bits to test.  Remember that all the H1.xxx bits are in the most
; significant byte of HWFLAGS, and the H0.xxx bits are in the LSB.  And
; remember that the 1802 is a big endian machine, so the inline argument is
; passed MSB first!
;
;   And finally, if more than one bit is set in the mask, then the result
; is an AND of all bits - in other words, ALL options specified must be
; present for a successful return.
;
;CALL:
;	CALL(TESTHW)
;	 .BYTE	<H1 flags>,<H0 flags>
;	<DF=0 if all option(s) installed, DF=1 if not>
;--
TESTHW:	PUSHR(T1)		; save a temporary register
	RLDI(T1,HWFLAGS)	; and point to the hardware flags
	LDA A\ STR SP		; put the first mask byte on the stack
	LDA T1\ AND\ XOR	; and test the masked bits
	LBNZ	TESTH1		; branch if they don't match
	LDA A\ STR SP		; then test the second byte
	LDA T1\ AND\ XOR	; ...
	LBNZ	TESTH2		; again, branch if they don't match
	CDF\ LBR TESTH3		; success!!
TESTH1:	INC	A		; skip the second argument byte
TESTH2:	SDF			; and return failure
TESTH3:	IRX\ POPRL(T1)\ RETURN	; restore T1 and we're done

	.SBTTL	Binary to BCD and BCD to Binary Conversions

;++
; These are used by the CDP1879 real time clock ...
;--

;   This routine converts a 2 digit BCD value, in D, into the equivalent binary
; form and returns it in D...
BCD2BIN:PLO	BAUD		; save D in a very temporary place
	SHR\ SHR\ SHR\ SHR	; get the most sigificant digit
	STR	SP		; save D for a second
	SHL\ SHL		; multiply by four
	ADD			; and then compute 5x
	SHL			; multiply by 2 again to get 10x
	STR	SP		; and save that
	GLO	BAUD		; get the original digit pair
	ANI	$F		; isolate the least significant digit
	ADD			; and add 10x the most significant
	RETURN			; that's all we need!


;   This routine converts a binary value, 0..99, in D into the two digit BCD
; equivalent and returns it in D...  This is kind of inefficient (it works by
; repeated subtraction!) but for small numbers it'll be fine...
BIN2BCD:PLO	BAUD		; save the original value
	LDI	0		; and set up the BCD result
	STR	SP		; ...
BIN2B1:	GLO	BAUD		; ...
	SMI	10		; subtract 10
	LBNF	BIN2B2		; branch if the result is negative
	PLO	BAUD		; save this
	LDI	$10		; and increment the BCD most significant digit
	ADD			; ...
	STR	SP		; ...
	LBR	BIN2B1		; keep going until we get underflow
BIN2B2:	ADI	10		; restore the remainder
	ADD			; and add in the most significant BCD digit
	RETURN			; we're done

	.SBTTL	Extended BIOS Entry Vectors

#if ($ > EBIOS)
	.ECHO	"***** EBIOS VECTOR TABLE OVERWRITTEN *****\n"
#endif
	.ORG	EBIOS

;++
;   On the implementations with a bit banged serial port, the F_BREAD/TYPE/TEST
; functions force access to the bit banged ports.  Likewise on implementations
; with a UART, the F_UREAD/TYPE/TEST functions force access to the UART.  We
; don't do bit banged serial at all, so we don't bother with the distinction
; here and all are the same ...
;--
	BENTRY(F_BREAD,   CONGET)	; ...
	BENTRY(F_BTYPE,   CONOUT)	; ...
	BENTRY(F_BTEST,   CONHIT)	; ...
	BENTRY(F_UTYPE,   CONOUT)	; ...
	BENTRY(F_UREAD,   CONGET)	; ...
	BENTRY(F_UTEST,   CONHIT)	; ...
	BENTRY(F_USETBD,  CONSET)	; set console baud rate and format
	BENTRY(F_GETTOD,  GTOD)		; get current time of day
	BENTRY(F_SETTOD,  STOD)		; set  "   "    "   "  "
	BENTRY(F_RDNVR,   ERRRET)	; read bytes from NVR
	BENTRY(F_WRNVR,   ERRRET)	; write bytes to NVR
	BUNIMP(F_IDESIZE)		; deprecated
	BUNIMP(F_IDEID)			; deprecated
	BUNIMP(F_DTTOAS)		; convert date buffer to ASCII
	BUNIMP(F_TMTOAS)		;  "   "  time   "    "    "
	BUNIMP(F_RTCTEST)		; test for existence of RTC
	BUNIMP(F_ASTODT)		; convert ASCII date to binary
	BUNIMP(F_ASTOTM)		;  "   "    "   time  "   "
	BENTRY(F_NVRCCHK, ERRRET)	; compute checksum for NVR

;   Here to return failure (DF=1) - this is used for the functions that are
; not in this BIOS but which are not officially unimplemented.  That's mostly
; the NVR functions in our case...
ERRRET:	SDF\ RETURN

	.SBTTL	Get CDP1879 RTC Time and Date

;++
;   The time and date routines, GTOD and STOD, operate with a time buffer
; pointed to by P1.  This buffer is six bytes long and contains -
;
;TIMBUF:.BYTE	<month>		; 1..12
;	.BYTE	<day>		; 1..31
;	.BYTE	<year>		; two digits only, 0..99
;	.BYTE	<hour>		; 0..23 (24 hour clock)
;	.BYTE	<minute>	; 0..59
;	.BYTE	<second>	; 0..59
;
;   All values are stored in binary, which is a bit inconvenient for us as the
; CDP1879 only supports BCD.  Fortunately it's not that difficult to convert.
;
;   Also note that the CDP1879 doesn't handle the year at all; only month and
; day.  The year is actually stored in our own RAMPAGE at location YEAR and
; never updates.  Once every year you'll have to use the firmware's "SET TIME"
; command to update the clock and year.
;--


;++
;   GTOD returns the current date and time, in binary as described above.
; P1 points to the time buffer, and on return DF=0 if all is well.
;
;CALL:	RLDI(P1,timbuf)
;	CALL(F_GTOD)
;	<return DF=1 if no RTC present>
;--
GTOD:	CALL(F_TESTHWF)		; test the hardware configuration flags
	 .BYTE	 H1.RTC, 0	;  ... for the RTC
	LBDF	RTCRET		; branch if no RTC installed
	PUSHR(DP)		; save a register to work with
	RLDI(DP,RTCBASE+RTCMON)	; point to the month register
	LDN DP\ ANI $7F		; strip off the leap year bit
	CALL(BCD2BIN)\ STR P1	; convert BCD to binary and store
	DEC DP\ LDN DP		; grab the day next
	CALL(BCD2BIN)		; convert to binary
	INC P1\ STR P1		; and put that in the caller's buffer
	LDI LOW(YEAR)		; fetch the year from our RAMPAGE
	PLO DP\ LDN DP		; ...
	INC P1\ STR P1		; and store (this is already in binary!)
	LDI LOW(RTCBASE+RTCHRS)	; back to the CDP1879 hours register
	PLO DP\ LDN DP\ ANI $3F	; remove the 24HR and AM/PM flags
	CALL(BCD2BIN)		; convert to binary
	INC P1\ STR P1		; and store it in the caller's buffer
	DEC DP\ LDN DP		; minutes next
	CALL(BCD2BIN)		; ...
	INC P1\ STR P1		; ...
	DEC DP\ LDN DP		; and lastly the seconds
	CALL(BCD2BIN)		; ...
	INC P1\ STR P1		; ...
	DEC	DP		; do a dummy write to RTC location 1
	STR	DP		;  ... to reset the "freeze" circuit
	IRX\ POPRL(DP)\ CDF	; restore DP and return DF=0 for success
RTCRET:	RETURN			; and we're finally done!

	.SBTTL	Set CDP1879 RTC Time and Date

;++
;   STOD sets the CDP1879 clock based on the information in the time buffer
; passed by the caller.  Again we have the problem that the data in the caller's
; buffer is in binary where as the CDP1879 wants BCD, but we can adjust.  Also
; remember that the CDP1879 doesn't keep track of the year at all, and this
; value is stored in our own RAMPAGE instead.
;
;CALL:	RLDI(P1,timbuf)
;	CALL(F_STOD)
;	<return DF=1 if no RTC present>
;
;   A couple of final points - the clock is always set to 24 hour mode, and if
; the year given is a leap year, then the leap year flag will be set in the
; clock's month register.  We can figure out a leap year just be checking for
; a multiple of 4 - even though only two digits of the year are used, since
; 2000 was a leap year everything will work out.
;--
STOD:	CALL(F_TESTHWF)		; test the hardware configuration flags
	 .BYTE	 H1.RTC, 0	;  ... for the RTC
	LBDF	RTCRET		; branch if no RTC installed
	PUSHR(DP)		; save a register to work with
	RLDI(DP,RTCBASE+RTCCSR)	; first set the CSR just to be safe
	LDI	RT.O32+RT.STRT	; be sure the clock is running
	STR DP\ DEC DP		; point to the month register
	LDA P1\ CALL(BIN2BCD)	; get the month from the caller
	STR	DP		; and store that in the clock
	LDA P1\ CALL(BIN2BCD)	; get the day next
	DEC DP\ STR DP		; ...
	LDI LOW(YEAR)		; point to the year in RAMPAGE
	PLO DP\ LDA P1\ STR DP	; get that from the caller and save it
	ANI 3\ LBNZ STOD1	; branch if not a leap year
	LDI LOW(RTCBASE+RTCMON)	; yes
	PLO DP\ LDI RT.LPY	; set the leap year bit
	SEX DP\ OR\ STR DP	; ... in the month register
STOD1:	LDI LOW(RTCBASE+RTCHRS)	; the hours register is next
	PLO DP\ LDA P1		; ...
	CALL(BIN2BCD)		; ...
;;	ORI	RT.12H		; select 12 hour mode
	STR	DP		; ...
	LDA P1\ CALL(BIN2BCD)	; minutes
	DEC DP\ STR DP		; ...
	LDN P1\ CALL(BIN2BCD)	; and the seconds
	DEC DP\ STR DP		; ...
	DEC	DP		; write to RTC address 1
	STR	DP		;  ... to release the "freeze" circuit
	IRX\ POPRL(DP)\ CDF	; restore DP and return DF=0 for success
	RETURN			; and we're done!

	.SBTTL	Initialize Parallel Printer

;++
;   This routine will initialize the parallel port printer interface.  It
; configures the CDP1851 PPI properly to talk to the printer, asserts the
; SELECT OUT and INIT lines, waits a moment for the printer to reset, and
; then checks for a SELECT IN response from the printer.  It returns DF=0
; if all is well, and DF=1 if there's a problem.
;
;CALL:
;	CALL(F_PRTINIT)
;	<return DF=1 if error>
;
; FYI - here are the PPI pins and the corresponding Centronics signals ...
;
;	PPI PIN	PPI DIR		CENTRONICS SIGNAL
;	------	-------		-------------------
;	PA0..7	output		data 0..7
;	ARDY	output		STROBE H
;	ASTB	input		ACK H
;	BRDY	output		AUTO LF L
;	BSTB	input		BUSY H
;	PB0	output		INIT L
;	PB1	input		SELECT IN H
;	PB2	output		SELECT OUT L
;	PB3	input		ERROR L
;	PB4	input		PAPER OUT L
;	PB5..7	input		unused
;
;   Note that STROBE and ACK are both active low at the connector, however the
; SBC1802 inverts both of these signals.  All the rest appear at the PPI pins
; exactly as they do at the connector.
;--
PRTINIT:CALL(F_TESTHWF)		; make sure the CDP1851 is installed
	 .BYTE	H0.PPI,0	; ...
	LBDF	PRTIN9		; no PPI - just give up now!
	PUSHR(T1)		; save a temporary register
	OUTI(GROUP,PPIGRP)	; select the PPI I/O group
;   Set port A to bit programmable mode and make all bits output.  Note that
; we must use the bit programmable mode instead of simple output mode so we
; can gain control of the ARDY and ASTROBE pins!
	OUT PPICTL\ .BYTE PP.SETA|PP.MDBP	; set port A mode
	OUT PPICTL\ .BYTE $FF			; all pins are outputs
	OUT PPICTL\ .BYTE PP.CARDY		; and clear STROBE
; Set port B to bit programmable mode and make bits 0 & 2 outputs.
	OUT PPICTL\ .BYTE PP.SETB|PP.MDBP	; set port B mode
	OUT PPICTL\ .BYTE $05			; pins 0 & 2 are outputs
	OUT PPICTL\ .BYTE PP.SBRDY		; disable AUTO LF
;   Set SELECT OUT and INIT, wait, and then clear INIT ...  Remember that
; SELECT OUT and INIT are both active low!
	OUT PPIB\ .BYTE $00	; assert SELECT OUT and INIT
	DLY1MS			; short delay
	OUT PPIB\ .BYTE $01	; clear INIT but leave SELECT OUT set
; Now wait for the printer to respond with SELECT IN ...
	RLDI(T1,1000)		; wait for 1 second maximum
PRTIN1:	DLY1MS			; ...
	SEX SP\ INP PPIB	; read the printer status
	ANI $02\ LBNZ PRTIN2	; did we find SELECT IN??
	DBNZ(T1,PRTIN1)		; no - wait a little longer
	SDF\ LSKP		; the printer must be turned off
; Success!
PRTIN2:	CDF			; return DF=0
	IRX\ POPRL(T1)		; restore T1
PRTIN9:;;OUTI(GROUP,BASEGRP)	; select the base group again
	RETURN			; and we're done

	.SBTTL	Print Characters and Strings

;++
;   This routine will print a single character to the parallel port printer.
; It waits for the printer not to be busy, outputs the data, pulses the STROBE
; signal, and then waits for the printer to assert BUSY.  Note that this
; assumes that you've called F_PRTINIT first.  This routine doesn't actually
; check that the PPI is installed, but it does have a timeout and will return
; with DF=1 if the printer doesn't respond.
;
;CALL:
;	D -> character to print
;	CALL(F_PRTCHAR)
;	<return DF=1 if error, D is preserved>
;--
PRTCHAR:PLO	BAUD			; save the character to print
	PUSHR(T1)\ RCLR(T1)		; keep the timeout in T1
	OUTI(GROUP,PPIGRP)		; select the PPI/printer group

; Wait for BUSY to be clear ...
PRTCH1:	SEX SP\ INP PPISTS		; read the status register
	ANI PP.BSTB\ LBZ PRTCH2		; wait for BUSY clear
	DLY1MS				; give the printer some time
	DBNZ(T1,PRTCH1)			; decrement the timeout and keep waiting
	LBR	PRTCH4			; timeout!

;   Output the data and assert STROBE ...   Note that the minimum STROBE width
; is 750ns.  Even at 5MHz, one CDP1805 instruction (2 major cycles) is 3.2us -
; that's plenty!!
PRTCH2:	GLO BAUD\ STR SP		; get the original character back
	OUT PPIA\ DEC SP		; write the data to port A
	OUTI(PPICTL,PP.SARDY)		; set ARDY/STROBE
	OUT PPICTL\ .BYTE PP.CARDY	; and then clear ARDY/STROBE

; Now wait for BUSY to be set ...
PRTCH3:	SEX SP\ INP PPISTS		; read the printer status again
	ANI PP.BSTB\ LBNZ PRTCH5	; wait for BUSY to set this time
	DBNZ(T1,PRTCH3)			; count down and wait
PRTCH4:	SDF\ LSKP			; timeout!
PRTCH5:	CDF				; success!
	IRX\ POPRL(T1)			; restore T1
	GLO BAUD\ RETURN		; and we're done


;++
;   This routine sends a null terminated string to the printer, similar to
; F_MSG.  It just calls PRTCHAR in a loop ...
;
;CALL:
;	P1 -> address of null terminated string
;	CALL(F_PRTTEXT)
;	<return DF=1 if error>
;--
PRTTEXT:LDA	P1		; get the next character to print
	LBZ	PRTTX1		; branch if we're done
	CALL(PRTCHAR)		; nope - print this one
	LBNF	PRTTEXT		; and keep printing
PRTTX1:	RETURN			; here when we find a null

	.SBTTL	Return Printer Status

;++
;   This routine will return the current printer status bits, especially the
; PAPER OUT, SELECT IN and ERROR bits.  Note that it does not return the BUSY
; nor the ACK bits!
;
;CALL:
;	CALL(F_PRTSTAT)
;	<return status bits in D>
;
; The bits in D correspond to -
;
;	D0..D1 -> unused
;	D2 -> selected (i.e. printer present and turned on)
;	D3 -> printer error
;	D4 -> out of paper
;	D5..D7 -> unused
;--
PRTSTAT:OUTI(GROUP,PPIGRP)	; select the PPI I/O group
	SEX SP\ INP PPIB	; and return the status bits
	RETURN			; return those and we're done!

	.SBTTL	Read/Write IDE/ATA Disk Drive Sectors

;++
;   This routine writes one sector (always exactly 512 bytes) to either the
; master or slave drive.  It needs a number of registers to be set up before
; calling, like this -
;
;CALL:
;	P1   - address of sector data buffer
;	T1   - LBA (low  16 bits)
;	T2   - LBA (high 12 bits + master/slave select)
;	CALL(DISKWR)
;	<if error return with DF=1 and error register in D>
;
;   As with all the IDE functions, if the operation fails for any reason it will
; return DF=1 and the drive error register in D.
;--
DISKWR:	GHI	T2		; select the correct drive
	CALL(DRVSEL)		; and wait for it to be ready
	LBDF	DSKRET		; quit now if error
	CALL(SETLBA)		; set up the LBA registers
	WRIDE(IDECMD,ID_WRS)	; issue write sectors command
	CALL(WDRQ)		; wait for the drive to ask for data
	LBDF	DSKRET		; give up if error
	PUSHR(P3)		; save a temporary register
	RLDI(P3,DSKBSZ)		; transfer 512 bytes of data
	OUTI(IDESEL,IDEDATA)	; select the drive data register
	SEX	SP		; be sure the stack is addressed
DISKW1:	LDA P1\ STR SP		; get buffer byte and put it on the TOS
	OUT IDEBUF\ DEC SP	; and send it to the drive
	DBNZ(P3,DISKW1)		; loop until we've done 512 bytes
	IRX\ POPRL(P3)		; restore P3
	LBR	WREADY		; wait for the drive to write the sector


;++
;   This routine reads one sector (always exactly 512 bytes) from either the
; master or slave drive.  It needs a number of registers to be set up before
; calling, all of which are the same as DISKWR -
;
;CALL:
;	P1   - address of sector data buffer
;	T1   - LBA (low  16 bits)
;	T2   - LBA (high 12 bits + master/slave select)
;	CALL(DISKRD)
;	<if error return with DF=1 and error register in D>
;
;   As with all the IDE functions, if the operation fails for any reason it will
; return DF=1 and the drive error register in D.
;--
DISKRD:	GHI	T2		; select the correct drive
	CALL(DRVSEL)		; and wait for it to be ready
	LBDF	DSKRET		; quit now if error
	CALL(SETLBA)		; set up the LBA registers
	WRIDE(IDECMD,ID_RDS)	; issue read sectors command
	CALL(WDRQ)		; wait for the drive to ask for data
	LBDF	DSKRET		; give up if error
	PUSHR(P3)		; save a temporary register
	RLDI(P3,DSKBSZ)		; transfer 512 bytes of data
	OUTI(IDESEL,IDEDATA)	; select the drive data register
	SEX	P1		; address the caller's buffer
DISKR1:	INP IDEBUF\ INC P1	; read from the drive and put it in the buffer
	DBNZ(P3,DISKR1)		; loop for all 512 bytes
	SEX SP\ IRX\ POPRL(P3)	; restore P3
	LBR	WREADY		; wait for ready or just return???

	.SBTTL	Set IDE/ATA Disk LBA

;++
;   This routine will load the four drive LBA registers using the values in
; the T1 and T2 registers.  It can't really fail and has no error return.
;
;CALL:
;	T1   - LBA (low  16 bits)
;	T2   - LBA (high 12 bits)
;	CALL(DISKRD)
;
;   Note that this routine DOES NOT select the IDE drive, and the drive select
; bits in the upper part of T2 are ignored.
;--
SETLBA:	OUTI(IDESEL,IDELBA0)	; select the LBA0 register
	GLO	T1		; and T1.0 contains the LSB LBA 
	STR SP\ SEX SP		; put it where OUT can find it
	OUT IDEBUF\ DEC SP	; and load the LBA0 register
	OUTI(IDESEL,IDELBA1)	; just keep doing this for the next
	GHI	T1		; three bytes
	STR SP\ SEX SP		; ...
	OUT IDEBUF\ DEC SP	; ...
	OUTI(IDESEL,IDELBA2)	; once more
	GLO	T2		; ...
	STR SP\ SEX SP		; ...
	OUT IDEBUF\ DEC SP	; ...
	OUTI(IDESEL,IDELBA3)	; for the LBA3 register read back ...
	SEX SP\ INP IDEBUF	;  ... the current drive select bits
	ANI $F0\ STR SP		;  save just those
	GHI T2\ ANI $0F		; get the upper 4 bits of the LBA
	OR\ STR SP		; combine with the drive select
	OUT IDEBUF\ DEC SP	; and update LBA3
	WRIDE(IDESCT,1)		; always set the sector count to 1
	RETURN			; and all done

	.SBTTL	Wait for IDE/ATA Disk Ready

;++
;   Wait for the disk drive to become ready after some operation.  This routine
; continuously polls the drive status register waiting for the BUSY bit to be
; clear and the READY bit to be set.  If we detect the ERROR bit while we're
; waiting, then we take the error return.
;
;CALL:
;	CALL(WREADY)
;	<if error return with DF=1 and error register in D>
;	<if no error return with DF=0>
;
;  FWIW, if the BUSY set it means that the drive has control over the registers
; and the host should not read or write them. The registers are not dual-ported,
; so either the host has access to them or the drive does but never both, and
; BUSY indicates which.  The exception is is the status register, which the
; host is allowed to read even when BUSY is set.
;
;   BUSY does not necessarily imply the drive is active, but the READY bit 
; indicates the drive is idle and ready to receive a command.  The caveat is
; that you can't check the READY bit until BUSY is clear.
;
;   Note that there is no timeout here.  We assume that if the drive was
; online during the POST then it's online now!
;--
WREADY:	OUTI(IDESEL,$00)	; don't know why we need this, but we do!
	OUTI(IDESEL,IDESTS)	; select the drive status register
WRDY1:	SEX SP\ INP IDEBUF	; read the drive's status register
	ANI	ID.ERR		; check the error bit first
	LBNZ	DRVERR		; take the error return if set
	LDN	SP		; get the status back
	ANI	ID.BSY+ID.RDY	; check the BUSY and READY bits
	XRI	ID.RDY		; want READY set and BUSY clear
	LBNZ	WRDY1		; keep waiting if not
	CDF\ RETURN		; return DF=0 for success


;   This routine is branched to in the event of an IDE error.  It will read and
; return the IDE error register in D, and also set DF=1 ...
DRVERR:	RDIDE(IDEERR)		; read the IDE error register
	SDF			; return DF=1
DSKRET:	RETURN			; and back to the original caller

	.SBTTL	Wait for IDE/ATA Data Request

;++
;   This routine will wait for the DRQ bit to set in the drive status register.
; This bit is set when the drive is ready to load or unload its sector buffer,
; and the next thing to do would be to transfer 512 bytes of data either to or
; from our caller's buffer.  This routine returns with DF=1 if the drive sets
; its error bit.
;
;CALL:
;	CALL(WDRQ)
;	<if error return with DF=1 and error register in D>
;	<if no error return with DF=0>
;	
;   WARNING - unlike WREADY, this routine does not have a timeout!
;--
WDRQ:	RDIDE(IDESTS)		; read the drive status register
	ANI	ID.BSY		; is the drive still busy?
	LBNZ	WDRQ		; yes - wait for it to finish
	LDN	SP		; get the status bits back again
	ANI	ID.ERR		; is the error bit set?
	LBNZ	DRVERR		; yes - failure
	LDN	SP		; check the status one more time
	ANI	ID.DRQ		; data transfer request?
	LBZ	WDRQ		; not yet - keep waiting
	CDF\ RETURN		; success - return DF=0

	.SBTTL	Select IDE/ATA Master or Slave

;++
;   This routine selects either the master or the slave drive depending the
; value in D and then waits for it to become ready.  The BIOS calls for
; F_IDEREAD and F_IDEWRITE expect that the caller will set up the master/slave
; bit in the most significant LBA byte (that would be T2.1).  Actually most of
; Mike's code sets T2.1 to $E0 for the master (and presumably $F0 for the slave)
; so it sets the LBA and the two unused bits as well, but but we don't take
; that for granted and always OR the value with $E0 regardless.
;
;CALL:
;	LDI <$00 to select the MASTER; $10 to select the SLAVE>
;	CALL(DRVSEL)
;	<if error return with DF=1>
;	<if no error return with DF=0>
;
;   Note that the first thing we do is to check the HWFLAGS to see whether the
; selected drive was actually detected by the POST.  If it wasn't then we take
; the error return immediately.  Also note that there are no timeouts here.
; IDE drives and CF cards are not hot swappable and we just assume that if the
; drive was present and working for the POST then it's still there now.
;--
DRVSEL:	ANI $10\ PUSHD		; save the master/slave bit
	LBNZ	DRVSE1		; was the IDE slave selected?
	CALL(TESTHW)		; is the IDE master installed?
	 .BYTE	 H1.IDE0, 0	; ...
	LBDF	DRVSE9		; no - just give up now
	LBR	DRVSE2		; yes - go select it
DRVSE1:	CALL(TESTHW)		; is the IDE slave installedd?
	 .BYTE	H1.IDE1, 0	; ...
	LBDF	DRVSE9		; no - give up now

; Wait for the IDE to be not busy and then select the correct drive .
DRVSE2:	OUTI(GROUP,BASEGRP)	; be sure the base board I/O group is selected
	OUTI(IDESEL,IDESTS)	; select the drive status register
DRVS20:	SEX SP\ INP IDEBUF	; read the drive status register
	ANI ID.BSY\ LBNZ DRVS20	; and loop until BUSY is clear	

; Ok, finally we can write the drive select bit!
	OUTI(IDESEL,IDELBA3)	; LBA3 has the drive select bit
	SEX SP\ POPD		; get the drive select back
	ORI ID.LBA\ STR SP	; always select LBA mode
	OUT IDEBUF\ DEC SP	; and write the drive's LBA3 register

; Now wait again for BUSY clear and READY set ...
	OUTI(IDESEL,IDESTS)	; select the drive status register
DRVSE3:	SEX SP\ INP IDEBUF	; read the status register
	ANI	ID.BSY|ID.RDY	; test the BUSY and READY bits
	XRI	ID.RDY		; and wait for BUSY=0 and READY=1
	LBNZ	DRVSE3		; ...
	CDF\ RETURN		; all done - return DF=0 for success

; Here if the drive isn't online ...
DRVSE9:	SEX SP\ IRX		; remove the unit number from the stack
DRVS99:	LDI $FF\ SDF		; return D=$FF and DF=1 for failure
	RETURN			; and we're done

	.SBTTL	Read TU58 Tape Records

;++
;   This routine will read exactly one sector, 512 bytes, from the TU58 drive.
; On call the registers (see below) specify the unit and block number, and the
; address of a buffer in RAM.  A real TU58 drive only has units 0 or 1 and
; tapes only contain 512 blocks, but everybody is using TU58 emulators these
; days.  Those allow for unit numbers up to 255 and up to 65536 blocks, so we
; don't range check either argument.  If any error occurs we return with DF
; flag set and an error code from the TU58 in D.  If the read is successful
; then DF will be clear on return.
;
;	<D contains the unit number>
;	<T2:T1 contains the block number>
;	<P1 points to the caller's buffer>
;	CALL(TUREAD)
;	<return with DF=0 if successful>
;
;   Note that the ElfOS BIOS API allows for 32 bit mass storage block numbers,
; but RSP only allows for 16 bits, so T2 is almost ignored.  "Almost" because
; MiniDOS' format program attempts to discover the size of a drive by trying 
; to read non-existent blocks.  Because of that we have to check T2 for all
; zeros and take the error return if it isn't.  Also note that, in keeping
; with the BIOS API, we always read exactly one block, and never more or less.
;
; BTW, you should be sure to call TUINIT before calling this routine!
;
; NOTES
;   This routine is hardwired to always read 512 bytes, the standard disk
; sector size.  However you can easily change that by removing the
; "RLDI(P2,...)" below and passing the byte count in P2.  HOWEVER, having
; said that, be aware that the code DOES assume all transfers are multiples
; of 128 bytes!  That's because it uses P2 to count both the bytes in this
; record AND the total bytes.
;
;   ElfOS pretty much depends on us not destroying any registers on return,
; and that includes the parameters we're called with.  The only exception
; seems to be P1, which points to the buffer and gets incremented on
; return by the number of bytes we read.
;
; REGISTERS USED
; T1 contains the block/sector number and is preserved on return
; T2 is used by TUGET for the timeout and is preserved on return
; P1 is used as a pointer to the data buffer and on return P1 points to
;      the next byte AFTER the buffer end!
; P2 is used as a transfer counter, and also by TUREND, and is preserved
; DP is used by all the checksum code and is preserved on return
;--
TUREAD:	PLO	BAUD		; save the unit number for a moment
	PUSHR(P2)\ PUSHR(DP)	; save temporary registers we use
	PUSHR(T2)		; ...
	GHI T2\ LBNZ TURD52	; T2 must be zero
	GLO T2\ LBNZ TURD52	; ...
	RLDI(P2,DSKBSZ)		; alway transfers 512 bytes
	GLO	BAUD		; restore the unit number
	CALL(TUXCMD)		; transmit a control/command packet
	 .BYTE	 TUCRDD		;  ... read data from tape
	
; Try to read a data packet header from the TU58 ...
TURD10:	CALL(TUCLSM)		; clear the checksum accumulator
	CALL(TUGETC)		; read the next byte
	LBDF	TURD50		; branch if timeout
	SMI	TUPDAT		; is this a DATA packet?
	LBZ	TURD11		; yes - read the packet length next 
	ADI	TUPDAT		; no - restore what we read
	CALL(TUREN1)		; and check for an END packet with an error
	LBR	TURD51		; return the error from the END packet

; Read the packet length next ...
TURD11:	CALL(TUGETC)		; get the length byte
	LBDF	TURD50		; timeout
	SMI	TUPKSZ		; the packet size must be 128 bytes
	LBNZ	TURD50		;  ... error if it isn't

; Read the bytes in the data packet ...
TURD20:	CALL(TUGETC)		; get another data byte
	STR P1\ INC P1		; and store in the caller's buffer
	DEC	P2		; decrement the byte count
	GLO P2\ ANI TUPKSZ-1	; end of packet ?
	LBNZ	TURD20		; nope - keep reading data

; End of packet - verify the data record checksum ...
	CALL(TURXSM)		; receive and verify the checksum
	LBDF	TURD50		; checksum bad - give up now
	GHI P2\ LBNZ TURD10	; read another packet 
	GLO P2\ LBNZ TURD10	;   ... if there are more bytes

; End of transfer - check for an END packet ...
TURD40:	CALL(TUREND)		; try to read an END packet
	LBR	TURD51		; and fall into TURD51

; Here for any error - return DF=1
TURD52:	LDI	LOW(TUEBLK)	; return "invalid block number"
TURD50:	SDF			; checksum bad (or some other) error
TURD51:	PLO BAUD\ IRX		; save the error code for later
	POPR(T2)\ POPR(DP)	; restore registers
	POPRL(P2)		; ...
	GLO BAUD\ RETURN	; and we're done!

	.SBTTL	Write TU58 Tape Data Records

;++
;   This routine will write exactly one 512 byte data record to the TU58 drive.
; Except for the direction of data transfer the parameter setup is identical to
; TUREAD.  If any error occurs we return with DF set and an error code from the
; TU58 in D.  If the data is read successfully then DF will be clear on return.
;
;	<D contains the unit number>
;	<T2:T1 contains the block number>
;	<P1 points to the caller's buffer>
;	CALL	TUWRIT
;	<return with DF=0 if all is well>
;
; BTW, you should be sure to call TUINIT before calling this routine!
;
; See the registers used and notes in TUREAD!
;--
TUWRITE:PLO	BAUD		; save the unit number for a moment
	PUSHR(P2)\ PUSHR(DP)	; save temporary registers we use
	PUSHR(T2)		; ...
	GHI T2\ LBNZ TURD52	; T2 must be zero
	GLO T2\ LBNZ TURD52	; ...
	RLDI(P2,DSKBSZ)		; write always transfers 512 bytes
	GLO	BAUD		; restore the unit
	CALL(TUXCMD)		; transmit a control/command packet
	 .BYTE	 TUCWRT		;  ... write data to tape

; Try to read a data packet request from the TU58 ...
TUWR10:	CALL(TUGETC)		; read the next byte
	LBDF	TURD50		; branch if timeout
	SMI	TUPCON		; expect the TU58 to send a CONTINUE
	LBZ	TUWR11		; yes - go send a data packet
	ADI	TUPCON		; otherwise restore what we read
	CALL(TUREN1)		; and look for an END packet with an error
	LBR	TURD51		; return the error code from the END and quit

; Send a data packet to the TU58 ...
TUWR11:	CALL(TUCLSM)		; clear the checksum accumulator
	LDI	TUPDAT		; send the data packet flag
	CALL(TUPUTC)		; ...
	LDI	TUPKSZ		; we always send 128 bytes
	CALL(TUPUTC)		; send the packet byte count

; Send the bytes in the data packet ...
TUWR20:	DEC P2\	LDA P1		; decrement the buffer count and 
	CALL(TUPUTC)		;  ... transmit the next data byte
	GLO P2\ ANI TUPKSZ-1	; end of packet?
	LBNZ	TUWR20		; no - keep sending data

; Here when we've sent a whole packet - transmit the checksum ...
TUWR30:	CALL(TUTXSM)		; transmit the checksum
	GHI P2\ LBNZ TUWR10	; if there are more bytes
	GLO P2\ LBNZ TUWR10	;  ... then send another packet

; No more bytes to send - read an END packet from the TU58 ...
	LBR	TURD40		; after that we're done!

	.SBTTL	Send TU58 Command Packet

;++
;   This routine will send a command packet (i.e. one where the flag byte is
; TUPCTL, 002) to the drive.  The opcode is passed inline, the unit number
; comes from D and the block from T1.  The latter two are exactly the same as
; the calling sequence for TUREAD and TUWRIT routines. The modifier, switches
; and sequence number fields of the command packet will always be zero.  Since
; this operation only involves sending data, it can never fail nor timeout
; so there is no returned status.
;
;	<D contains the unit number>
;	<T1 contains the block number>
;	<P2 contains the transfer byte count>
;	CALL	TUXCMD
;	.BYTE	<TU58 opcode (e.g.TUCRDD, TUWRT, etc)>
;--
TUXCMD:	PUSHD			; save the unit number for later
	OUTI(GROUP,SL1GRP)	; select the SLU1 I/O group
	CALL(TUCLSM)		; clear the checksum accumulator
	LDI TUPCTL\ CALL(TUPUTC); this is a "control" packet
	LDI 10    \ CALL(TUPUTC); message byte count (always 10)
	LDA  A    \ CALL(TUPUTC); send the opcode next
	LDI  0    \ CALL(TUPUTC); modifier byte (not used)
	POPD      \ CALL(TUPUTC); send the unit number
	LDI  0    \ CALL(TUPUTC); send the switch byte (always zero)
	CALL(TUPUTC)		; and the sequence number
	CALL(TUPUTC)		;  ... always zero too
	GLO P2    \ CALL(TUPUTC); low order byte of the transfer count
	GHI P2    \ CALL(TUPUTC); and then the high byte
	GLO T1    \ CALL(TUPUTC); finally send the block number
	GHI T1    \ CALL(TUPUTC); ...
	LBR	TUTXSM		; and lastly send the checksum

	.SBTTL	Receive TU58 END Packet

;++
;   This routine will attempt to receive a control/command packet with the
; opcode END (otherwise known simply as an END packet!).  The TU58 sends one
; of these so signal the end of a data transfer.  Note that the only useful
; information in the END packet is the success code stored in byte 3 - the
; rest of the packet is pro forma and mostly useless.
;
;   If this routine successfully reads an END packet then it will return with
; DF cleared and the success code in D.  Note that success codes are typically
; negative numbers and are truncated to 8 bits here.  If the next packet isn't
; an END packet, or if there's some error in the end packet, or if there's a
; timeout, then this routine returns with carry cleared. 
;
;	CALL(TUREND)
;	<if success return with DF=0 and success code in D>
;
;   Note that there's an alternate entry point at TUREN1 - this assumes that
; the first byte of the packet, whatever it might be, has already been read
; and is in D.
;
; Uses P2 as a temporary ...
;--
TUREND:	CALL(TUCLSM)		; clear the checksum
	CALL(TUGETC)		; and read the next character
	LBDF	TUREN2		; branch if timeout
TUREN1:	SMI	TUPCTL		; is this a command/control packet?
	LBNZ	TUREN2		; nope - bad
	CALL(TUGETC)		; read the length byte
	LBDF	TUREN2		; timeout
	SMI	10		; the length is always 10
	LBNZ	TUREN2		; bad packet
	CALL(TUGETC)		; read the opcode
	LBDF	TUREN2		; ...
	SMI	TUCEND		; is this an END packet?
	LBNZ	TUREN2		; nothing else is allowed here
	CALL(TUGETC)		; get the success code
	LBDF	TUREN2		; ...
	PHI	P2		; save that for later

; Read 4 words (8 bytes) of unused data ...
	LDI 8 \ PLO P2		; set up a counter
TUREN3:	CALL(TUGETC)		; read a byte
	LBDF	TUREN2		;  quit if timeout
	DEC P2\ GLO P2		; have we done enough?
	LBNZ	TUREN3		; branch if not

; Verify the checksum and we're done!
	CALL(TURXSM)		; read and verify the checksum
	LBDF	TUREN2		; branch if bad checksum
	GHI	P2		; get the success code back
	SHL			; DF=1 if the code is negative (an error!)
	GHI	P2		; return the code in D
	RETURN			; and we're done

; Here if anything goes wrong ...
TUREN2:	SDF \ LDI $FF		; return DF=1 and error code FF
	RETURN			; ...

	.SBTTL	Initialize TU58 Tape

;++
;   This routine will initialize any TU58 drive attached to the secondary serial
; port SLU1.  If it's successful and the drive responds then it returns with DF
; cleared, and if there are any errors or if there's no response from the
; drive then DF will be set.
;
;	CALL(TUINIT)
;	<return with DF=0 if the drive is ready>
;
; Uses P1 as a retry counter ...
;
;   Note that we do not set, nor do we change, the SLU1 baud rate or character
; format.  By default these are set to 9600bps and 8N1, however you can change
; that with the SET SLU1 ... command.  Whatever is established before TUINIT
; is called, we'll stick with.
;--
TUINIT:	PUSHR(T2)		; save T2 (used by TUGET!)
	CALL(F_TESTHWF)		; see if SLU1 is even present?
	 .BYTE	 H0.SLU1, 0	; ...
	LBDF	TUINI5		; no - just quit now
	GHI BAUD\ ANI BD.ALT	; is the alternate console in use?
	LBNZ	TUINI5		; yes - that uses SLU1 so no TU58!
	OUTI(GROUP,SL1GRP)	; be sure the SLU1 I/O group is selected
	LDI  8\ PHI P1		; retry 8 times before giving up

; Send a BREAK to the TU58 ...
TUINI1:	RLDI(T2,SL1FMT)		; saved character format for SLU1
	LDN T2\ ORI SL.BRK	; set the force break bit
	STR SP\ SEX SP		; ...
	OUT SL1CTL\ DEC SP	; write the SLU1 control register
	CALL(DLY100)		; delay for 100ms
	LDN T2\ STR SP		; get SLU1 character format again
	OUT SL1CTL\ DEC SP	; write the SLU1 control register
;   The CDP1854 needs us to transmit a byte, which will be turned into garbage
; by the other end since there will be no start bit, to finally and completely
; clear the break condition.  Note that we don't bother to check THRE here -
; we've just delayed for 100ms; I'm pretty sure it's done now!
	LDI 0\ STR SP		; transmit a null byte
	OUT SL1BUF\ DEC SP	;  ...
	CALL(DLY100)		; delay for 100ms again
	INP	SL1BUF		; and discard any junk we received

;   Send not one but two INIT packets.  Note that the INIT packets are just a
; single byte.  There is no checksum, nothing, else.
	LDI	TUPINI		; send two <INIT> flag bytes
	CALL(SL1OUT)		; ...
	CALL(SL1OUT)		; ...

;   If our friend the TU58 is alive and well, it should send a CONTINUE packet
; as the response.  If we get that now, then great.  If we timeout, then there's
; a problem...
	CALL(TUGET)		; read a response
	LBDF	TUINI4		; branch if there was a timeout
	SMI	TUPCON		; is this a CONTINUE?
	LBZ	TUINI6		; yes - success!
TUINI4:	GHI	P1		; no - get the retry counter
	SMI	1		; and decrement it
	PHI	P1		; ...
	LBNZ	TUINI1		; retry 8 times
				; failure - fall into the error return

; One way or another we're done ...
TUINI5:	SDF\ LSKP		; return DF=1 for failure
TUINI6:	CDF			; return DF=0 for success
	OUTI(GROUP,BASEGRP)	; restore the base I/O group
	SEX SP\ IRX\ POPRL(T2)	; restore T2
	RETURN			; and we're done for now

;++
;   This little routine delays for 100ms, assuming the 2.5MHz clock speed.
; It's used by the TU58 and IDE code, and got stuck here because of space
; limitations in the BIOS sections where those functions reside.
;--
DLY100:	LDI 100 \ PLO P1	; call DLY1MS 100 times
DLY101:	DLY1MS			; spin for 1ms 
	DEC P1 \ GLO P1		; have we waited for 100ms?
	LBNZ	DLY101		; no - keep going
	RETURN			; all done

	.SBTTL	TU58 I/O Primitives

;++
;   This routine reads (or at least it tries to) one character from the TU58
; serial port.  If it's successful then it returns the character in D and
; with DF cleared.  If it times out and nothing is read, then it returns with
; DF set.
;
;   IMPORTANT - this version does NOT alter the current checksum.  Use TUGETC
; to get a character AND update the checksum calculation.
;
;	CALL(TUGET)
;	<if no timeout return DF=0 and byte in D>
;
;   Note that the magic constant for the timeout loop is calculated such
; that it takes exactly 100ms to expire.  The TUTIMO constant specifies the
; number of iterations for that loop.  Most loop iterations, where T2.1 is
; not zero, take 16 machine cycles.  The last 256 iterations, where T2.1 is
; zero, take 21 cycles.  At 2.5Mhz, a 100ms delay needs a count of 1873.
;
; This assumes that the SLU1 I/O group is already selected!!!
;
; Uses T2 for the timeout counter!
;
;   This probably could be rewritten to use SL1HIT and SL1IN, but we'd still
; need to add the time out and it's not clear that's worth the effort.
;--
TUGET:	RLDI(T2,TUTIMO)		; this is the timeout loop counter
	OUTI(GROUP,SL1GRP)	; select the SLU1 I/O group
TUGET1:	SEX SP\ INP SL1STS	; [4] have we received anything?
	SHR			; [2] put the DA bit in the DF
	LBNF	TUGET2		; [3] nope - check for timeout
	INP	SL1BUF		; get the byte received
	CDF			; success!
	RETURN			; and we're done
; Here to check for a timeout ...
TUGET2:	DEC	T2		; [2] count down the 100ms timeout
	GHI T2\ LBNZ TUGET1	; [5] keep looping until it's zero
	GLO T2\ LBNZ TUGET1	; [5] ...
	SDF\ RETURN		; timeout!


;++
;   This routine calls TUGET and, assuming that's successful, adds the character
; to the current TU58 packet checksum.  
;
;	CALL(TUGETC)
;	<if no timeout return DF=0 and byte in D>
;
;   Note that the calling sequence for this is exactly the same as TUGET, with
; the side effect of updating the checksum.
;--
TUGETC:	CALL(TUGET)		; first get something
	LBDF	TUGET3		; and quit now if we failed
	CALL(TUADSM)		; update the checksum
	CDF			; return DF=0 and data in D
TUGET3:	RETURN			; and we're done


;++
; This routine calls SL1OUT and also updates the running TU58 checksum.
;
;	<character to transmit in D>
;	CALL(TUPUTC)
;
;   Note that the calling sequence for this is exactly the same as TUPUT, with
; the side effect of updating the checksum.
;--
TUPUTC:	CALL(TUADSM)		; update the checksum
	LBR	SL1OUT		; and fall into SL1OUT ...

	.SBTTL	TU58 Checksum Utilities

;++
;   Add the byte in D to the running TU58 checksum in TUCKSM.  This is trickier
; than it should be because the TU58 keeps a 16 bit checksum and actually
; checksums PAIRs of bytes rather than individual ones.  Worse, it also uses
; end around carry so carries out of 16 bits are added to the checksum LSB.
;
;	<byte to checksum in D>
;	CALL	TUADSM
;	<return with D unchanged and checksum updated>
;
; Uses DP!!
;--
TUADSM:	STR	SP		; save the data byte for a moment
	RLDI(DP,TUCKSM)		; point at the checksum accumulator
	LDN DP\ XRI $FF\ STR DP	; toggle the even/odd byte flag
	INC	DP		; and point to the low checksum byte
	LBZ	TUADS1		; branch if this was the odd byte

; Here for an even byte - add to the checksum LSB ...
	LDN DP\ ADD\ STR DP	; update the low byte of the checksum
	INC	DP		; then point to the high byte
	LBNF	TUADS3		; we're done if no carry is needed
	LDN DP\ ADI 1\ STR DP	; yes - carry to the high byte
	LBR	TUADS2		; and do the end around carry

; Here for an odd byte - add to the checksum MSB ...
TUADS1:	INC	DP		; point to the high byte
	LDN DP\ ADD\ STR DP	; update the high byte of the checksum

; Here to do an end around carry after the addition ...
TUADS2:	LBNF	TUADS3		; branch if no carry needed
	DEC	DP		; yes - point to the low byte again
	LDN DP\ ADI 1\ STR DP	; increment that
	LBNF	TUADS3		; return if no carry
	INC	DP		; otherwise increment the high byte too
	LDN DP\ ADI 1\ STR DP	; ...
TUADS3:	LDX \ RETURN		; restore D and we're done


;++
; Clear the TU58 checksum accumulator ...
;--
TUCLSM:	RLDI(DP,TUCKSM+2)	; point to the cheksum accumulator
	LDI 0\ SEX DP		; and clear three bytes
	STXD\ STXD\ STXD	; ...
	RETURN			; that's all we need


;++
; Transmit the curreent TU58 checksum (presumably as part of a packet) ...
;--
TUTXSM:	RLDI(DP,TUCKSM+1)	; send the low byte first
	LDN	DP		; ...
	CALL(SL1OUT)		; DON'T call TUPUTC!
	INC DP\ LDN DP		; now sent the high byte
	LBR	SL1OUT		; ...


;++
;   This routine will receive two bytes from the TU58 and compare them to the
; current checksum.  If they match then it returns with DF cleared, and if
; they don't match then it returns with DF set.  Note that we can't use TUGETC
; here because that would alter the current checksum!
;
;	CALL(TURXSM)
;	<return DF=0 if checksum matches>
;
; Uses DP!
;--
TURXSM:	RLDI(DP,TUCKSM+1)	; point to the checksum accumulator
	CALL(TUGET)		; read the low byte first
	LBDF	TURXS1		; branch if timeout
	SEX DP\ SM		; compare to the checksum accumulator
	LBNZ	TURXS1		; doesn't match!
	CALL(TUGET)		; now get the high byte
	LBDF	TURXS1		; branch if timeout
	INC DP\ SEX DP\ SM	; and compare the high byte
	LBNZ	TURXS1		; again, branch if they don't match
	CDF \ RETURN		; return success
; Here if the checksum doesn't match ...
TURXS1:	SDF \ RETURN		; signal failure

	.SBTTL	Storage Device Calls

;++
;   The latest BIOS defines a set of generic "storage device" calls for mass
; storage I/O -
;
;	F_SDREAD - read one sector from a generic "storage device"
;	F_SDWRITE- write one sector to a generic "storage device"
;	F_SDBOOT - boot ElfOS from a generic "storage device"
;
;   All the F_SDXXX calls pass a unit number in D and it's up to the BIOS 
; (that's us!) to decide which physical device and unit should be addressed.
; This BIOS currently supports two types of mass storage devices - IDE/ATA,
; which as two units, master and slave.  And TU58 serial disk/tape, which
; nominally has two units too but in theory can support any number of units.
;
;   In this implementation it was somewhat arbitrarily decided that units
; 0 and 1 reference the IDE master and slave, and units 2 and up reference
; TU58 units 0 and up.  So our job here is pretty simple - we just have to
; look at the unit number in D and decide whether we need to call the
; IDE/ATA or the TU58 code.
;
;   Note that F_SDBOOT is a special problem.  It's handled on the next
; page!
;--

;++
;   This routine will examine the storage unit number in D and figure out
; whether IDE, TU58, or neither of those, should be addressed.  If IDE is
; selected it returns to the instruction immediately after the call to
; SDTYPE.  If TU58 is selected, then it skips 3 bytes on return, and if it's
; neither then it skips 6 bytes.  This allows the caller to put three LBRs
; right after the call to SDTYPE, which makes life super easy!
;
;   As a side effect it will also fix up the drive number to be appropriate
; for a call to DISKRD/DISKWR or TUREAD/TUWRITE.
;--
SDTYPE:	SMI	2		; is the unit number 0 or 1?
	LBGE	SDTYP1		; no - go check for TU58

; Here if an IDE drive is selected ...
;   After the "SMI 2", D will contain $FE if the original unit was 0, and
; $FF if the original unit was 1.  We want T2.1 to be $E0 for unit 0 and
; $F0 for unit 1, so we just need to shift left and then mask.
SDTYP0:	SHL\ SHL\ SHL\ SHL	; put the IDE unit in the upper 4 bits of D
	ANI $F0\ STR SP		; get rid of the other garbage
	GHI T2\ ANI $0F		; isolate the upper four bits of the LBA
	OR\ PHI T2		; and combine with the unit in T2
	RETURN			; and take the IDE return

; Here if a TU58 drive is selected ...
;   Note that if the original storage unit was 2 then D contains 0 and if
; it was 3 then D contains 1.
SDTYP1:	INC A\ INC A\ INC A	; skip 3 bytes on return
	SMI	2		; compare to two again
	LBGE	SDTYP2		; branch if the unit was .GT. 3
	ADI	2		; restore the TU58 unit 0 or 1
	RETURN			; return to the TU58 case

; Here if the original unit was .GT. 3 ...
SDTYP2:	INC A\ INC A\ INC A	; skip six bytes on return
	RETURN			; and we're done


; Read from a storage device ...
SDREAD:	CALL(SDTYPE)		; see if IDE or TU58 is referenced
	 LBR	 DISKRD		;  ... IDE disk selected
	 LBR	 TUREAD		;  ... TU58 selected
	 LBR	 BADDEV		;  ... or none of the above!

; Write to a storage device ...
SDWRITE:CALL(SDTYPE)		; same as SDREAD!
	 LBR	 DISKWR		;  ...
	 LBR	 TUWRITE	;  ...
;;	 LBR	 BADDEV		;  ...

; Here for a bad device number ...
BADDEV:	LDI $FF\ SDF		; return DF=1 and error $FF in D
	RETURN			; ...

	.SBTTL	Storage Device Bootstrap

;++
;   The BIOS has three different calls that will attempt to bootstrap ElfOS.
;
;   F_SDBOOT calls F_SDREAD to read sector zero of that unit into memory at
; $100.  F_SDBOOT stores the unit number in memory for ElfOS to use later.
; After that F_SDBOOT jumps to $106 to start the ElfOS bootstrap.  F_SDBOOT
; requires that the caller have SCRT and a valid stack set up so that it can
; call other subroutines like F_SDREAD, and also so that F_SDBOOT can return
; to the caller in the event of an any error.
;
;   If F_SDBOOT succeeds then it simply never returns.  If F_SDBOOT does
; encounter a hardware error, then it returns with DF=1 and the drive error
; code in D.  If F_SDBOOT reads sector zero successfully, but that sector
; doesn't contain a valid ElfOS bootstrap, then it returns with DF=0.  D is
; undefined in this case.
;
;   F_BOOTIDE always boots from the master IDE drive, which in the case of this
; BIOS means unit 0.  So F_BOOTIDE simply sets D=0 and then falls into the same
; F_SDBOOT code.  Like F_SDBOOT, it requires that the caller set up SCRT and a
; stack, and it will return in the event of an error.
;
;   F_BOOT sets up a temporary stack at $F0 and calls F_INITCALL to initialize
; SCRT.  After that it falls into F_BOOTIDE to boot the IDE master drive.
; F_BOOT is intended to be entered via an LBR rather than a CALL, and can never
; return.  In the event of any error F_BOOT simply hangs forever.
;
;   You could say that F_SDBOOT and F_BOOTIDE are "warm" boots, since they
; require that a stack and SCRT be set up.  F_BOOT is a "cold" boot in the
; sense that it requires that nothing be set up.  Note, however, that F_SDBOOT
; is the only one that can boot from anything other than the IDE master drive.
;
;CALL:
;	<unit number in D>
;	CALL(F_SDBOOT)
;	<return DF=1 if any hardware error>
;	<NEVER return if the bootstrap is successful!>
;
;	CALL(F_BOOTIDE)
;	<return as with F_SDBOOT>
;
;	LBR	F_BOOT
;	<NEVER return, ever!>
;--
COLDBOOT:
	RLDI(SP,$00F0)		; set up a temporary stack
	RLDI(A,BOOTIDE)		; proceed with the IDE bootstrap
	LBR	F_INITCALL	;  ... after initializing SCRT

; Here to boot from the IDE master drive ...
BOOTIDE:LDI	0		; just select storage device 0
				; and fall into sdboot ...

; Here to boot any storage device ...
SDBOOT:	PUSHD			; save the unit number for later
	QCLR(T2,T1)		; read LBN zero
	RLDI(P1,EO.BBUF)	;  ... into memory at $0100
	POPD \ PUSHD		; get unit number, but leave it on the stack
	CALL(F_SDREAD)		; and read sector zero
        LBNF	BTELFOS		; branch if no disk error
	IRX\ RETURN		; return DF=1 for any hardware error

; Select the ElfOS memory map and then jump to the ElfOS boot code ...
;   Note that at this point there's no way we can ever return - either
; the ElfOS boot sector will succeed or it'll blow up - but it's never
; coming back here.  So there's no reason to worry about saving the
; current memory mapping mode!
;
;   Another thing is that we need to store the unit number we used for
; booting in the memory image of the boot sector.  ElfOS uses this to
; remember where it came from for all future disk access.
BTELFOS:POPD			; get the unit one last time
	;TODO TBA NYI!!		; and store it in the boot block for ElfOS
	RLDI(T1,MCR)		; access the memory control register
	LDI MC.ELOS\ STR T1	; and select the ElfOS map
	LBR	EO.BTSA		; jump to the ElfOS sector 0 code

	.SBTTL	MiniDOS Storage Calls

;++
;   David Madole's Mini/DOS uses a different scheme to access multiple mass
; storage devices, which is fortunately not completely incompatible with the
; way we do things.  In Mini/DOS the standard IDEREAD and IDEWRITE calls take
; a 24 bit LBA (NOT the full 28 bits!) passed in T2:T1 and T2.1 will contain
; $En, where 'n' is the device number.  Thus $E0 would access device 0, $E1
; accesses device 1, etc.  The upper nibble, which would have selected the
; master or slave drive in the original IDEREAD/IDEWRITE, is always E and is
; ignored.
;
;   We basically implement this literally with our four mass storage devices,
; and $E0 selects the IDE master, $E1 the IDE slave, and $E2/$E3 the two TU58
; units. For backward compatibility we also allow $F0 to select the IDE slave
; as a special case, however $Fn where n != 0, will return an error.  Likewise,
; $En where n > 3 is also an error.
;--

;++
;   This routine will get the MiniDOS unit select from T2.1, convert it to
; something DISKRD/DISKWR or TUREAD/TUWRITE will like, and then take the
; appropriate return.  Except for the input, it's identical to the SDTYPE
; routine used by the SDREAD/SDWRITE functions and we can share most of the
; same code.
;
;   One tricky bit though - after we extract the unit from T2.1 we must then
; zero T2.1.  If we don't then DISKWR/DISKRD will interpret the unit number as
; part of the LBA and that'll never work.  Note that MiniDOS disks are limited
; to 24 bit LBA addresses (T1, and T2.0) where as SDREAD/SDWRITE support the
; full 28 bits.
;--
MDTYPE:	GHI T2\ ANI $1F		; get the selected unit number
	PLO	BAUD		; save that for a minute
	LDI 0\ PHI T2		; zero T2.1 so it's not taken as an address
	GLO	BAUD		; restore the unit selected
	LBR	SDTYPE		; and the rest is the same as SDTYPE

;++
;   There's a problem here (isn't that always the case?!) - since the MDREAD and
; MDWRITE routines pass the unit number in T2.1 they are limited to 24 bit disk
; addressing, but the SDREAD/SDWRITE routines still support 28 bits.  Because
; of that, MDTYPE zeros out T2.1, BUT some ElfOS and MiniDOS code depends on
; T2 being unchanged on return.  That means we need Yet Another Wrapper that
; will save T2, do the actual read or write, and then restore T2.
;
;   Note that we don't need to save D on call, but we do need to preserve D
; (which holds an error code) on return.
;--

; MDREAD wrapper ...
MDREAD:	GHI T2\ PUSHD		; save T2.1
	CALL(MDRD1)		; do the actual read
	POPD\ PHI T2\ RETURN	; and return

; MDWRITE wrapper ...
MDWRITE:GHI T2\ PUSHD		; save T2.1
	CALL(MDWR1)		; do the actual write
	POPD\ PHI T2\ RETURN	; and return


; Here when MiniDOS wants to read from a mass storage device ...
MDRD1:	CALL(MDTYPE)		; figure out whether IDE or TU58 is addressed
	 LBR	 DISKRD		; IDE master or slave
	 LBR	 TUREAD		; TU58 unit 0 or 1
	 LBR	 BADDEV		; none of the above!

; Here when MiniDOS wants to write to a mass storage device ...
MDWR1:	CALL(MDTYPE)		; same as MDREAD ...
	 LBR	 DISKWR		; write IDE
	 LBR	 TUWRITE	; write TU58
	 LBR	 BADDEV		; bad device

	.SBTTL	Primary BIOS Entry Vectors

#if ($ > DPBASE)
	.ECHO	"***** MONITOR RAM PAGE OVERWRITTEN *****\n"
#endif

#if ($ > BIOS)
	.ECHO	"***** BIOS VECTOR TABLE OVERWRITTEN *****\n"
#endif
	.ORG	BIOS

;   This is the standard BIOS entry vector table at $FF00.  Note that lower
; case names (e.g. ltrim, strcpy, mul16, etc) are hardware independent and
; are found in the rtl1802.asm file.  Upper case names are hardware dependent
; and are in this file.
	BENTRY(F_BOOT,     COLDBOOT)	; cold boot from disk drive
	BENTRY(F_TYPE,     CONOUT)	; type character in D on console
	BENTRY(F_READ,     CONGET)	; read character from console to D
	BENTRY(F_MSG,      TTEXT)	; type ASCIZ string pointed to by P1
	BUNIMP(F_TYPEX)			; deprecated
	BENTRY(F_INPUT,    INPUT)	; read string into 256 byte buffer at P1
	BENTRY(F_STRCMP,   strcmp)	; compare two strings
	BENTRY(F_LTRIM,    ltrim)	; trim spaces from beginning of a string
	BENTRY(F_STRCPY,   strcpy)	; copy a string
	BENTRY(F_MEMCPY,   memcpy)	; copy arbitrary data
	BUNIMP(F_WRTSEC)		; deprecated
	BUNIMP(F_RDSEC)			; deprecated
	BUNIMP(F_SEEK0)			; deprecated
	BUNIMP(F_SEEK)			; deprecated
	BUNIMP(F_DRIVE)			; deprecated
	BENTRY(F_SETBD,    AUTOBAUD)	; figure out the console baud rate
	BENTRY(F_MUL16,    mul16)	; 16x16 multiply -> 32 bit result
	BENTRY(F_DIV16,    div16)	; 16/16 divide -> result and remainder
	BENTRY(F_IDERESET, BADDEV)	; deprecated
	BENTRY(F_IDEWRITE, MDWRITE)	; write IDE sector (Mini/DOS style)
	BENTRY(F_IDEREAD,  MDREAD)	; read   "     "      "   "    "
	BENTRY(F_INITCALL, INISCRT)	; initialize the SCRT routines
	BENTRY(F_BOOTIDE,  BOOTIDE)	; same as F_BOOT
	BENTRY(F_HEXIN,    hexin)	; hexadecimal ASCII string to binary
	BENTRY(F_HEXOUT2,  hexout2)	; binary to hexadecimal ASCII string
	BENTRY(F_HEXOUT4,  hexout4)	;   "     "   "    "      "      "
	BENTRY(F_TTY,      CONOUT)	; same as F_TYPE
	BUNIMP(F_MOVER)			; deprecated??
	BENTRY(F_MINIMON,  MINIMON)	; trap to the EPROM firmware
	BENTRY(F_FREEMEM,  FREEMEM)	; return memory size
	BENTRY(F_ISNUM,    isnum)	; DF=1 if D contains numeric char
	BENTRY(F_ATOI,     atoi)	; decimal ASCII string to binary
	BENTRY(F_UINTOUT,  uintout)	; unsigned binary to decimal ASCII
	BENTRY(F_INTOUT,   intout)	; signed     "    "     "      "
	BENTRY(F_INMSG,    INLMSG)	; type an ASCIZ string "in line"
	BENTRY(F_INPUTL,   INPUTL)	; read a line from the console
	BENTRY(F_BRKTEST,  CONHIT)	; return DF=1 if console has input
	BUNIMP(F_FINDTKN)		; search table for token
	BENTRY(F_ISALPHA,  isalpha)	; DF=1 if D contains alphabetic char
	BENTRY(F_ISHEX,    ishex)	; DF=1 if D contains hexadecimal char
	BENTRY(F_ISALNUM,  isalnum)	; DF=1 if D contains alphanumeric char
	BUNIMP(F_IDNUM)			; identify symbol as dec, hex or other
	BENTRY(F_ISTERM,   isterm)	; DF=1 if D contains non-alphanumeric
	BENTRY(F_GETDEV,   GETDEV)	; return supported device map
	BENTRY(F_NBREAD,   NBREAD)	; non-blocking read from console
	BENTRY(F_SDREAD,   SDREAD)	; read storage device sector
	BENTRY(F_SDWRITE,  SDWRITE)	; write storage device sector
	BUNIMP(F_SDRESET)		; deprecated
	BENTRY(F_SDBOOT,   SDBOOT)	; boot from specific storage device
	BUNIMP(F_SDSIZE)		; deprecated already!

	.SBTTL	F_FREEMEM and F_GETDEV functions

;   This is the F_FREEMEM function - it returns the address of the last usable
; RAM location in P1.  Since our memory size is fixed (and indeed, the POST
; will bomb if all RAM is not present) this is trivial.
FREEMEM:RLDI(P1,USRRAM)		; this is all we need
	RETURN			; ...


;   The F_GETDEV function returns a bit map of devices supported by this BIOS.
; In this case "supported" means that code exists in the BIOS to support that
; device, NOT that the device actually exists on this particular system. This
; is totally different from the HWFLAGS bitmap - that one tells us which
; devices are present and passed the POST.  Firmware and BIOS support for
; these devices is implicit.
BIOSBITS .EQU	FGD_SDFUN+FGD_NBREAD+FGD_F0VEC+FGD_F8VEC+FGD_RTC+FGD_UART+FGD_IDE
GETDEV:	RLDI(P1,BIOSBITS)
	RETURN

	.SBTTL	Standard Call and Return Technique

;++
;   These two routines implement the "standard call and return tecnhique", more
; or less right out of the RCA manual.  All assume the following register usage
;
;	R2 (SP)     - stack pointer (1802 stacks grow DOWNWARD!)
;	R3 (PC)     - program counter
;	R4 (CALLPC) - always points to the SCALL routine
;	R5 (RETPC)  - always points to the SRETURN routine
;	R6 (A)	    - subroutine argument list pointer
;
;   A subroutine call goes like this -
;
;	SEP	CALLPC
;	.WORD	<address of subroutine>
;	<any arguments, if desired>
;
; The SCALL routine first pushes the current argument pointer, register R6/A,
; onto the stack.  It then copies the R3/PC, which is the caller's PC to the
; A register.  Next it uses A to fetch the two bytes following the SEP CALLPC,
; which are the address of the subroutine, and load them into the PC register.
; Finally it switches the P register back to R3/PC and the called subroutine
; is running.  The subroutine may use the A register to fetch additional inline
; arguments if desired, being sure to increment A as it does.  
;
;  When the subroutine wants to return it executes a
;
;	SEP	RETPC
;
; This starts the SRETURN routine running, which copies the current A register
; back to the PC, and then pops the previous A value from the stack.  Lastly
; it switches the P register back to R3 and we're back at the caller's location.
;
;   It's obviously handy to have SCALL and SRETURN preserve D, but the problem
; is "where to save it"?  Can't use the stack because it gets in the way of
; saving A, and we can't use other memory because we don't have a register to
; address it.  Mike's BIOS uses BAUD.0, the low order byte of the console
; timing constant, to save it.  
;
;   The SCRT routines in UT71 also store D in RE.0 (BAUD.0), which is
; conveniently the same as ElfOS and Mike's BIOS.  Moreover, UT71 and some of
; the MicroDOS code actually DEPENDS on the fact that a copy of D is contained
; in RE.0 as a side effect of SCALL.  That's obviously a problem if you want to
; use the native 1804/5/6 SCAL/SRET instructions, below.
; 
;   Lastly, the call action is EXACTLY (well, almost) equivalent to the
; CDP1804/5/6 instruction
;
;	SCAL	A, <subroutine>
;
; And likewise, return is almost equivalent to 
;
;	SRET	A
;
; The reason for the "almost" qualifier is the side effect of storing D in RE.0,
; as mentioned above.  Also, SCAL/SRET depend on X being 2 (the stack pointer),
; where as the SCALL and SRETURN subroutines both set X to 2 before pushing.
; And of course, with the hardware implementation R4 and R5 are not used and
; are potentially free for other purposes.
;
;   One last point - if you're interested in calculating exact timing for
; something, then a CALL requires 36 machine cycles and that's including the
; "SEP R4" used by the initial CALL macro.  A return takes 30 cycles, including
; the "SEP R5" for the RETURN macro. That makes the total call/return overhead
; 66 machine cycles.
;--

;++
;   One more complication, as if all that wasn't enough, is that SCALL and
; SRETURN have to be at locations that agree with the F_CALL and F_RETURN
; definitions in the bios.inc file.  And we want to jam all three of these
; routines, SCALL, SRETURN, and INISCRT, as close to the end of memory as
; we can to maximize the free space available elsewhere.  The BIOS is
; getting pretty tight in its 4K footprint these days.
;
;   Note that SCALL takes 18 bytes; SRETURN takes 15, and INISCRT needs
; 17 bytes.  Those are unlikely to change that this point, since the code
; is pretty well known.  This little calculation attempts to make all
; that work.
;--
SCRTBASE .EQU	F_EXTBVER-18-15-17
	.ORG	SCRTBASE

; Standard subroutine call ... 18 bytes, 34 cycles ...
	SEP	PC		; [2] start the subroutine running
SCALL:	PLO	BAUD		; [2] save the D register
;   Mike's BIOS code stored the LOW byte of R6 first, and then the HIGH byte.
; That's wrong, at least compared to the way the 1804 hardware works.  I'm
; going to switch it here and hopefully I won't regret it!
	SEX	SP		; [2] make sure the stack is selected
	PUSHR(A)		; [8] and save the A register
	RCOPY(A,PC)		; [8] then copy the caller's PC to A
	RLDA(PC,A)		; [8] fetch the subroutine address
	GLO	BAUD		; [2] restore D
	BR	SCALL-1		; [2] restore CALLPC and start the subroutine

#if (SCALL != F_CALL)
	.ECHO	"***** SCALL ADDRESS MISMATCH *****\n"
#endif


; Standard subroutine return ... 15 bytes, 28 cycles ...
	SEP	PC		; [2] return to the original caller
SRETURN:PLO	BAUD		; [2] save D temporarily
	RCOPY(PC,A)		; [8] A contains the return address
	SEX	SP		; [2] make sure the stack is selected
	IRX			; [2] point to the saved A
	POPRL(A)		; [8] and pop it off the stack
	GLO	BAUD		; [2] restore D
	BR	SRETURN-1	; [2] restore RETPC and return to the caller

#if (SRETURN != F_RETURN)
	.ECHO	"***** SRETURN ADDRESS MISMATCH *****\n"
#endif


;   This routine will initialize the SCRT routines.  The caller should load
; the A register with the address where he wants to continue, and then LBR
; to the BIOS F_INITCALL entry point.  This routine is usually called with
; R0 as the PC and, when we return, R3 will be the current PC register.
;
;  For example,
;	RLDI(A,MYADDR)
;	LBR	F_INITCALL	; P=R0
;MYADDR: ... continue here ...	; P=R3
;
;   Note that it's up to the caller (that's you!) to load SP with a valid
; stack pointer!
;
;   And note that it's written in this rather awkward way, using a RETURN,
; so that it will work whether the PC is R0 or R3 when we're called!
;
; 17 bytes ...
INISCRT:RLDI(CALLPC,SCALL)	; initialize the CALLPC and ...
	RLDI(RETPC,SRETURN)	;  ... RETPC registers ...
	DEC SP\ DEC SP		; fixup SP (because RETURN will increment it!)
	NOP\ NOP		; keep the length at 17 bytes!
	RETURN			; and then branch to the address in A

	.SBTTL	Last BIOS Page

;   The very last few bytes of the BIOS contain some additional fixed, magic,
; locations that we need to play along with.


;   A pointer to the "extended" BIOS version, which is simply an ASCIZ string,
; is stored just before the regular BIOS version ...
#if ($ != F_EXTBVER)
	.ECHO	"***** F_EXTBVER ADDRESS MISMATCH *****\n"
#endif
	.WORD	RIGHTS

;  The BIOS version is stored here in three bytes.  The current format, as used
; by MiniDOS, puts the BIOS "manufacturer" in the first byte and then a BIOS
; dependent version in the second two bytes.
#if ($ != F_VERSION)
	.ECHO	"***** F_VERSION ADDRESS MISMATCH *****\n"
#endif
	.BYTE	STGBIOS
	.WORD	VEREDT


;   The ROMCKSUM program computes a sixteen bit checksum for the entire EPROM
; image and stores it in the last two bytes of EPROM.  This checksum is computed
; such that the sum of ALL bytes in the EPROM (including the last two!) is equal
; to the sixteen bit value in the last two bytes.  This rather arcane system is
; used because it gives the same value for the EPROM checksum that the Data I/O
; and other EPROM programmers report.
;
;   Since the checksum is included in its own calculation, we have to go to
; some lengths to prevent the checksum value from affecting its own calculation.
; The secret is to actually use the last FOUR bytes of the ROM - the last two
; contain the checksum and the two before that contain the complement of each
; byte in the checksum.  The sum of a byte and its complement is always 0x0100,
; and since there are two such bytes, adding a checksum to the ROM in this way 
; always adds 0x0200 to the original checksum REGARDLESS of what the actual
; checksum may be.  The ROMCKSUM program takes this into account, and  we can
; simply ignore the whole issue here.
#if ($ != F_CHECKSUM)
	.ECHO	"***** F_CHECKSUM ADDRESS MISMATCH *****\n"
#endif
	.WORD	$0000
	.WORD	$0000

	.END
