;*****************
;*RCA BASIC3 V1.1*
;*  Source Code  *
;*****************
;
;..EXTENDED BASIC
;..WRITTEN BY:  RON CENKER
;..		(215)  263_8240
;..
;..REGISTER DEFINITION
;..EBEGIN.D
;CL	EQU	01H
;SP	EQU	02H
;PC	EQU	03H
;CALL	EQU	04H
;RET	EQU	05H
;LINK	EQU	06H
;LBPT	EQU	07H
;FLAG	EQU	08H
;CSTPT	EQU	09H
;GP1	EQU	0AH
;LP	EQU	0BH
;GP2	EQU	0CH
;DATR	EQU	0DH
;WK	EQU	0FH

;	RCA's Basic Level 3 v1.1 converted and corrected into A18 Assembler code
;				By Charles Yakym, January 9,2017
;
;	The path to make this possible was via a "cut and paste" then re-entering it
;			then correcting all the errors that exist.
;
;	Many Thanks to Lee Hart who did the "Track and Sector" cut and paste stuff and to
;	 Ed Keefe and his wife Mary who re-entered it into an improved source code file
;
;	To reconstuct this listing has been a lot of fun and quite the learning experience.
;
;	If you find any errors or improvements that are needed, please contact me at
;			The-Eagle@att.net or The--Eagle@sbcglobal.net
;					Thank you 
;
;	Currently there are no errors usings the A18 assembler with this listing
;				to assemble this source code
;
;NOTE	THE FUNCTIONAL USE OF THIS SOURCE CODE IS UNKNOWN AS IS, HOWEVER IT IS STILL A
; WORK IN PROGRESS
;
;	Enjoy......
;

; SBC1802 MODIFICATIONS:
; 17-MAY-24	RLA	Restore all equates to their original CDS/UT62 settings.
;			Apparently the sense of the console break (B4/BN4) is reversed
;			from the MS2000.  Fix the code at BREAKC to account for that.
;
; 31-MAY-24	RLA	Modify START1 so the initial PC is R3 and to explicitly set the
;			 RAM top.  There's no more autosizing of RAM.
;
; 31-MAY-24	RLA	Remove DIN, DOUT, RFLN, WFLN, and CLOSE statements, and all
; 			 associated code and references to UT62 tape functions.
;			Rewrite the COMLIB table to be more user friendly.
;
;  3-JUN-24	RLA	Remove all the CDS specific code, except for the console I/O,
;			 which works with the SBC1802 UT71 emulation.  Modify MSCRT
;			 to point to the SBC1802 BIOS SCRT routines instead of the
;			 LCDS NCALL/NRET.  Remove NCALL/NRET routines.
;
; 10-Jun-24	RLA	Implment PSAVE/DSAVE and PLOAD/DLOAD using the XMODEM routines
;			 in the SBC1802 firmware.
;  

; REGISTER EQUIVALENTS FOR A18
R0	EQU	0
R1	EQU	1
R2	EQU	2
R3	EQU	3
R4	EQU	4
R5	EQU	5
R6	EQU	6
R7	EQU	7
R8	EQU	8
R9	EQU	9
RA	EQU	10
RB	EQU	11
RC	EQU	12
RD	EQU	13
RE	EQU	14
RF	EQU	15

;[RLA]   On the SBC1802, BASIC fits into ROM just after the SBC1802 monitor
;[RLA] and just before the built in help text.  The SBC1802 runs BASIC using
;[RLA] the ROM0 memory map, so RAM0 is mapped from $0000 to $7FFF, and EPROM
;[RLA] (including this BASIC!) is mapped from $8000 to $FFFF.  We have to
;[RLA] set aside the first 512 bytes of RAM because the SBC1802 firmware
;[RLA] uses this space for XMODEM buffers (used for PLOAD/PSAVE).
FIRST	EQU	0B000H		;[RLA] address (in ROM) of this code
RAM	EQU	0200H		;[RLA] "bottom" of usable RAM
RAMTOP	EQU	7FFFH		;[RLA] "top" of usable RAM
WP1	EQU	RAM		;WORKPAGE #1
WP2	EQU	RAM+0100H	;WORKPAGE #2
LOWUS	EQU	RAM+0200H	;USER SPACE FOR BASIC

;note all I/O is routed thru RF.1
OUTPT1	EQU	WP1+008EH	;output jump
INPD	EQU	WP1+008AH	;input jump

;[RLA] Restore all these to the original values (for UT62!) ...
DELAY1	EQU	80EFH		;[RLA] UT62 delay subroutine
UREAD	EQU	813EH		;[RLA] UT62 read character from console
TYPE	EQU	81A4H		;[RLA] UT62 write character to console
;[RLA]   PSAVE/PLOAD now uses XMODEM from the SBC1802 EPROM.
;[RLA] The old UT62 cassette tape routines aren't used anymore.
;DREAD	EQU	8502H		;[RLA] UT62 tape read (PLOAD/DLOAD CMD)
;DWRITE	EQU	8500H		;[RLA] UT62 tape write (PSAVE/DSAVE CMD)
;UTIL	EQU	87F0H		;[RLA] UT62 tape utility (rewind)

;[RLA] Special vectors for the SBC1802 firmware EPROM ...
F_CALL   EQU	0FFC6H		;[RLA] SBC1802 standard SCRT CALL routine
F_RETURN EQU	0FFD8H		;[RLA] SBC1802 standard SCRT RETURN routine
XENTRY	 EQU	0EFE0H		;[RLA] base of SBC1802 vector table
BEXIT	 EQU	XENTRY+(3*0)	;[RLA] exit from BASIC
BSAVE	 EQU	XENTRY+(3*1)	;[RLA] exit from BASIC
BLOAD	 EQU	XENTRY+(3*2)	;[RLA] exit from BASIC


;START OF BASIC
	ORG FIRST
CSTART	DIS
	DB	00H
	LBR	START	;...GO INITIALIZE
START1	GHI	R2
	SEP	RD
	DB	3BH	;ON RET RF=WP1+3Bh - D=M(RF)
	SEP	R4
	DW	STKEY
	SEP	R4
	DW	RSTIO
	SEP	R4
	DW	CLEAR1
	SEP	R4
	DW	MESOUT
	DB	0AH
	DB	"C RCA 1981"
	DB 	0DH,0AH
	DB	"BASIC3 V1.1"
	DW	0D0AH
	DB	"C/W?"
	DB	0DH,0AH,00H
START2	SEP	R4
	DW	INPD
	XRI	57H
	BZ	A3A	;warm start jump to A3A
	XRI	14H	;ASCII DC4 what is this about? should be 43h for "C"
	BNZ	START2	;wait until either warm or cold start is selected

;cold start entry point
	PLO	RC	;..INIT DEFUS BYTE
	LDI	0CH
	SEP	RD
	DB	02H
	LDI	HIGH LOWUS
	STR	RF
	PHI	RC	;..POINT AS DEFUS BYTE
	GLO	RC	;..CONTAINS ZERO
	STR	RC	;..DEFUS BYTE INITIATED
	SEP	R4
	DW	CLEAR

;warm start entry point
A3A	SEP	R4
	DW	RSTFLG
;[RLA]	SEP	RD
;[RLA]	DB	9DH
;[RLA]	ANI	03H
;[RLA]	BNZ	A8
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	SEP	R4
	DW	MESOUT
	DB	"READY"
	DB	0DH,0AH,00H
A8	SEP	R4
	DW	LLB
A4	EQU	A8
	SEP	R4
	DW	LILB
	GHI	R8
	ANI	01H	;..CHK RUN/COM FLG
	BZ	A5
	SEP	R5
A5	LDI	HIGH WP1
	PHI	RB	;..SET LINE PTR FOR
	LDI	0D0H
	PLO	RB	;..FIRST EXEC STATE.
	GHI	R8
	ANI	02H	;..CHK EXEC. OR LOAD
	BZ	A6
	SEP	R4
	DW	EXEC
A1	SEP	R4
	DW	CRLF
	BR	A3A
A6	SEP	R4
	DW	LUS
	BR	A4
;....................................................
;	..FLAG AND Q STATUS FUNCTION..
;	..EQSTAT.B..
;	............
QSTAT	GHI	R8
	STXD
	LDI	39H
	BR	STGO
ESTAT	GHI	R8
	STXD
	SEP	R4
	DW	GVAL
	GLO	RA
	SMI	1
STATE	LBNF	GVALE
	GLO	RA
	SDI	4
	BM	STATE
	GLO	RA
	ADI	3BH

STGO	SEP	RD
	DB	7BH
	LDI	0D5H
	SEP	RD
	DB	7EH
	LDI	1AH
	STR	RF
	DEC	RF
	LDI	0FEH
	STR	RF
	LDI	0
	PLO	RA
	SEP	R4
	DB	HIGH WP1
	DB	0FBH
	INC	R9
	INC	R9
	INC	R9
	INC	R9
	SEP	R4
	DW	ZRST
	INC	R9
;	CLO RA
	GLO	RA
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	LBR	FCONV
;.................................................
;	..MODULO FUNCTION..
;	..MOD.B
;	...................	
;HAD TO RENAME FROM MOD TO MODU FOR THE A18

MODU	GHI	R8
	STXD
	GLO	R8
	STXD
	SEP	R4
	DW	EVIARG
	XRI	02H
	BNZ	MODE
	GHI	R9
	PHI	RF
	GLO	R9
	SMI	04H
	PLO	RF
	ADI	08H
	PLO	R9
	LDI	08H
	PLO	R8
MOD1	LDA	RF
	STR	R9
	INC	R9
	DEC	R8
	GLO	R8
	BNZ	MOD1
	GLO	R9
	SMI	04H
	PLO	R9
MOD2	SEP	R4
	DW	IDIV
	SEP	R4
	DW	IMULT
	INC	RD
	SEP	RD
	DB	LOW IMINUS
MODO	INC	R2
	LDX
	PLO	R8
	LBR	ICONV
MODE	SEP	R4
	DW	ERROR
	DB	31H

;[RLA] COMLIB is at $B100
	DS	09H

;  ..............................................
;	..COMMAND LIBRARY..
;	..COMLIB.BX..
;	...................
;[RLA]   This table contains one entry for every BASIC keyword, statement,
;[RLA] function, operator, etc.  Each entry is formatted like this
;[RLA]
;[RLA]		<flag byte>
;[RLA]		<ASCII keyword name; last char has MSB set>
;[RLA]		<corresponding token (and pointer into EXTAB)>
;[RLA]
;[RLA]   The bits in the flag byte are -
;[RLA]
;[RLA]		B7     -> 1-> don't parse the rest of this line (used by REM)
;[RLA]		B6     -> ??? unknown
;[RLA]		B5     -> ??? unknown
;[RLA]		B4..B0 -> length of this entry, in bytes (used to skip to the next)
;[RLA]
;[RLA]    The table is generally arranged in order by the token code, but there are
;[RLA] some obvious exceptions, such as FORMAT right below.  Also, "statements" are
;[RLA] in the first half, and "functions" are in the second half.  No idea if there
;[RLA] is any significance to the order or not.
COMLIB	DB	0A4H, 'R', 'E', 80H+'M', 080H				;..REM
	DB	067H, 'F', 'O', 'R', 'M', 'A', 80H+'T', 0B3H		;..FORMAT
	DB	024H, 'C', 'L', 80H+'S', 081H				;..CLS
	DB	024H, 'N', 'E', 80H+'W', 082H				;..NEW
	DB	004H, 'R', 'U', 80H+'N', 083H				;..RUN
	DB	024H, 'E', 'N', 80H+'D', 084H				;..END
	DB	064H, 'L', 'E', 80H+'T', 085H				;..LET
	DB	066H, 'P', 'R', 'I', 'N', 80H+'T', 086H			;..PRINT
	DB	063H, 'P', 80H+'R', 086H				;..PR
	DB	065H, 'G', 'O', 'T', 80H+'O', 087H			;..GOTO
	DB	063H, 'I', 80H+'F', 088H				;..IF
	DB	066H, 'I', 'N', 'P', 'U', 80H+'T', 089H			;..INPUT
	DB	065H, 'L', 'I', 'S', 80H+'T', 08AH			;..LIST
	DB	066H, 'G', 'O', 'S', 'U', 80H+'B', 08BH			;..GOSUB
	DB	067H, 'R', 'E', 'T', 'U', 'R', 80H+'N', 08CH		;..RETURN
	DB	025H, 'W', 'A', 'I', 80H+'T', 08DH			;..WAIT
	DB	064H, 'D', 'I', 80H+'M', 08EH				;..DIM
	DB	064H, 'F', 'O', 80H+'R', 08FH				;..FOR
	DB	065H, 'N', 'E', 'X', 80H+'T', 090H			;..NEXT
	DB	066H, 'F', 'I', 'X', 'E', 80H+'D', 091H			;..FIXED
	DB	025H, 'P', 'O', 'K', 80H+'E', 092H			;..POKE
	DB	064H, 'D', 'E', 80H+'G', 093H				;..DEG
	DB	064H, 'R', 'A', 80H+'D', 094H				;..RAD
	DB	064H, 'S', 'T', 80H+'Q', 095H				;..STQ
	DB	067H, 'D', 'E', 'F', 'I', 'N', 80H+'T', 096H		;..DEFINT
	DB	066H, 'P', 'S', 'A', 'V', 80H+'E', 097H			;..PSAVE
	DB	066H, 'P', 'L', 'O', 'A', 80H+'D', 098H			;..PLOAD
	DB	026H, 'D', 'E', 'F', 'U', 80H+'S', 099H			;..DEFUS
	DB	024H, 'E', 'O', 80H+'P', 09AH				;..EOP
	DB	065H, 'D', 'A', 'T', 80H+'A', 09BH			;..DATA
	DB	065H, 'R', 'E', 'A', 80H+'D', 09CH			;..READ
	DB	028H, 'R', 'E', 'S', 'T', 'O', 'R', 80H+'E', 09DH	;..RESTORE
	DB	024H, 'E', 'O', 80H+'D', 09EH				;..EOD
	DB	024H, 'C', 'L', 80H+'D', 09FH				;..CLD
	DB	066H, 'D', 'S', 'A', 'V', 80H+'E', 0A0H			;..DSAVE
	DB	066H, 'D', 'L', 'O', 'A', 80H+'D', 0A1H			;..DLOAD
	DB	066H, 'E', 'N', 'I', 'N', 80H+'T', 0A2H			;..ENINT
	DB	067H, 'D', 'I', 'S', 'I', 'N', 80H+'T', 0A3H		;..DISINT
	DB	066H, 'D', 'M', 'A', 'P', 80H+'T', 0A4H			;..DMAPT
	DB	064H, 'T', 'I', 80H+'N', 0A5H				;..TIN
	DB	065H, 'T', 'O', 'U', 80H+'T', 0A6H			;..TOUT
	DB	066H, 'T', 'R', 'A', 'C', 80H+'E', 0A7H			;..TRACE
	DB	065H, 'C', 'A', 'L', 80H+'L', 0A8H			;..CALL
;	DB	065H, 'D', 'I', 80H+'N', 0A9H				;[RLA] was ..DIN
;	DB	065H, 'D', 'O', 'U',  80H+'T', 0AAH			;[RLA] was ..DOUT
	DB	064H, 'O', 'U', 80H+'T', 0ABH				;..OUT
;	DB	066H, 'C', 'L', 'O', 'S', 80H+'E', 0ACH			;[RLA] was ..CLOSE
	DB	064H, 'B', 'Y', 80H+'E', 0ADH				;..BYE
	DB	065H, 'E', 'X', 'I', 80H+'T', 0AEH			;..EXIT
;	DB	065H, 'W', 'F', 'L', 80H+'N', 0AFH			;[RLA] was ..WFLN
;	DB	065H, 'R', 'F', 'L', 80H+'N', 0B0H			;[RLA] was ..RFLN
	DB	069H, 'R', 'E', 'N', 'U', 'M', 'B', 'E', 80H+'R', 0B1H	;..RENUMBER
	DB	065H, 'E', 'D', 'I', 80H+'T', 0B2H			;..EDIT
;	 ..B3 IS USED FOR 'FORMAT' ABOVE
;	.. B4 is used by RUN+ for a line address
;	.. D0 is used for a 16 bit hex constant
;	.. D1 is used for a variable
;	.. D2 is used for an integer constant
;	.. D3 is used for a floating point constant
	DB	004H, 'S', 'I', 80H+'N', 0D4H				;..SIN
	DB	004H, 'C', 'O', 80H+'S', 0D5H				;..COS
	DB	002H, 80H+'(', 0D6H					;..(
;	.. D7 is used for a string variable
	DB	004H, 'A', 'T', 80H+'N', 0D8H				;..ATN
	DB	004H, 'E', 'X', 80H+'P', 0D9H				;..EXP
	DB	004H, 'L', 'O', 80H+'G', 0DAH				;..LOG
	DB	004H, 'S', 'Q', 80H+'R', 0DBH				;..SQR
	DB	004H, 'I', 'N', 80H+'T', 0DCH				;..INT
	DB	005H, 'P', 'E', 'E', 80H+'K', 0DDH			;..PEEK
	DB	004H, 'A', 'B', 80H+'S', 0DEH				;..ABS
	DB	004H, 'R', 'N', 80H+'D', 0DFH				;..RND
	DB	004H, 'U', 'S', 80H+'R', 0E0H				;..USR
	DB	005H, 'I', 'N', 'U', 80H+'M', 0E1H			;..INUM
;	.. E2 is used for an 8 bit hex constant
	DB	005H, 'F', 'N', 'U', 80H+'M', 0E3H			;..FNUM
	DB	004H, 'A', 'S', 80H+'C', 0E4H				;..ASC
	DB	004H, 'L', 'E', 80H+'N', 0E5H				;..LEN
	DB	002H, 0DCH, 0E6H					;..\(BINARY DATA)
	DB	004H, 'S', 'G', 80H+'N', 0E7H				;..SGN
	DB	004H, 'M', 'O', 80H+'D', 0E8H				;..MOD
	DB	004H, 'I', 'N', 80H+'P', 0E9H				;..INP
	DB	064H, 'N', 'O', 80H+'T', 0EAH				;..NOT
	DB	003H, 'P', 80H+'I', 0EBH				;..PI
	DB	005H, 'F', 'V', 'A', 80H+'L', 0ECH			;..FVAL
;	 ..ED IS A SPARE
	DB	004H, 'M', 'E', 80H+'M', 0EEH				;..MEM
	DB	004H, 'Q', 'S', 80H+'T', 0EFH				;..QST
	DB	003H, 'E', 80H+'F',0F0H					;..EF
	DB	064H, 'A', 'N', 80H+'D', 0B5H				;..AND
	DB	064H, 'X', 'O', 80H+'R', 0B6H				;..XOR
	DB	063H, 'O', 80H+'R', 0B7H				;..OR
;	 ..B8 IS USED FOR \ TO CLOSE BINARY DATA
	DB	005H, 'C', 'H', 'R', 80H+'$', 0B9H			;..CHR$
	DB	005H, 'M', 'I', 'D', 80H+'$', 0BAH			;..MID$
	DB	002H, 0DEH, 0BBH					;..^
	DB	004H, 'T', 'A', 80H+'B', 0BCH				;..TAB
	DB	003H, '>', 80H+'=', 0BDH				;..>=
	DB	003H, '<', 80H+'=', 0BEH				;..<=
	DB	003H, '<', 80H+'>', 0BFH				;..<>
	DB	065H, 'S', 'T', 'E', 80H+'P', 0C0H			;..STEP
	DB	063H, 'T', 80H+'O', 0C1H				;..TO
	DB	002H, 80H+',', 0C2H					;..,
	DB	002H, 80H+';', 0C3H					;..;
	DB	002H, 80H+')', 0C4H					;..)
	DB	065H, 'T', 'H', 'E', 80H+'N', 0C5H			;..THEN
	DB	002H, 80H+'<', 0C6H					;..<
	DB	002H, 80H+'>', 	0C7H					;..>
	DB	002H, 80H+'+', 	0C8H					;..+
	DB	002H, 80H+'-', 	0C9H					;..-
	DB	002H, 80H+'*', 	0CAH					;..*
	DB	002H, 80H+'/', 	0CBH					;../
	DB	002H, 80H+'=', 	0CCH					;..=
	DB	002H, 80H+':', 	0CDH					;..:
	DB	0FFH							;..END OF LIBRARY
	DS	5+6+7+6+6	; [RLA] adjust for the keywords removed

;......................................................
;	..LOAD PROGRAM FROM DISC..
;	..PLOAD.B
;	..........................
PLOAD	LDI	LOW PLOAD9	;[RLA]
	LSKP
DLOAD	LDI	LOW DLOAD9	;[RLA]
	PHI	RE
	LDN	RB		;[RLA]
	XRI	0DH		;[RLA]
	BZ	PLCONT		;[RLA]
	SEP	R4		;[RLA]
	DW	ERROR		;[RLA]
	DB	37H		;[RLA]
PLCONT	GHI	RE		;[RLA]
	PLO	R3

PLOAD9	SEP	RD
	DB	9EH		;[RLA] start of RAM user space!
	PHI	RC		;[RLA] into RC.1
	STR	R2
	LDI	LOW PLOADR
	PLO	R8
	LDI	00H
	BR	DLOAD2

DLOAD9	SEP	RD
	DB	83H
	PHI	RC
	STR	R2
	LDI LOW DLOADR
	PLO	R8
	LDN	RF
DLOAD2	PLO	RC		;[RLA] for PLOAD, set RC.0 to 0
	SDI	00H
	PLO	RA		;[RLA] RA.0 = $FF
	SEP	RD
	DB	0BBH		;[RLA] stack page
	SMB
	PHI	RA		;[RLA] RA.1 = stack page - start of RAM

	LDI	1		;[RLA]
	PHI	RE		;[RLA]
	LDI	0		;[RLA]
	PLO	RE		;[RLA]

	GLO	RA
	STR	R2
	GLO	RE
	SD
	GHI	RA
	STR	R2
	GHI	RE
	SDB
	BPZ	DLOAD3
	SEP	R4
	DW	ERROR
	DB	32H

;[RLA]  Here to load either program or data using XMODEM.  We switch to the
;[RLA] SBC1802 BIOS SCRT routines and then call the SBC1802 BLOAD entry point.
;[RLA] The SBC1802 firmware will take care of the rest for us!  Note that the
;[RLA] SBC1802 SCRT will trash RE, but BASIC doesn't care.
DLOAD3	SEP	R4		;[RLA] change to the SBC1802 SCRT routines
	DW	MSCRT		;[RLA] ...
	SEP	R4		;[RLA] ask the SBC1802 firmware to load
	DW	BLOAD		;[RLA]  ... our program or data
	SEP	R4		;[RLA] and restore the BASIC SCRT routines
	DW	RSCRT		;[RLA] ...

	GLO	R8
	PLO	R3
PLOADR	LDI	(HIGH WP2)+1
	PHI	RC
	STR	R2
	LDI	00H
	PLO	RC
	LDA	RC
	ADD
	SEP	RD
	DB	01H
	PHI	RC
	STR	R2
	LDA	RC
	ADD
	SEP	RD
	DB	03H
	LDN	RC
	SEP	RD
	DB	04H
	LBR	CLARS
DLOADR	SEP	R4
	DW	CLARS
	SEP	RD
	DB	 83H
	PHI	RC
	LDN	RF
	PLO	RC
	STR	R2
	LDA	RC
	ADD
	PLO	RA
	LDA	RC
	STR	R2
	SEP	RD
	DB	83H
	ADC
	SEP	RD
	DB	19H
	GLO	RA
	SEP	RD
	DB	1AH
	SEP	RD
	DB	84H
	STR	R2
	LDA	RC
	ADD
	PLO	RA
	SEP	RD
	DB	83H
	STR	R2
	LDA	RC
	ADC
	SEP	RD
	DB	12H
	GLO	RA
	SEP	RD
	DB	13H
	SEP	RD
	DB	84H
	STR	R2
	LDA	RC
	ADD
	PLO	RA
	SEP	RD
	DB	83H
	STR	R2
	LDN	RC
	ADC
	SEP	RD
	DB	14H
	GLO	RA
	SEP	RD
	DB	15H
	SEP	R5

;[RLA] SPBS is at $B3F6
	DS	39H

;......................................................
;	..SPACE/BACKSPACE..
;	..SPBS.B
;	........
SPBS	SEP	RD
	DB	0ACH
	SEP	R4
	DW	OUTPUT
	SEP	RD
	DB	80H
	LBR	OUTPUT

;'''''''''''''''''''''''''''''''''''''''''''''''''''''
;	..LINE EDIT FUNCTION..
;	..EDOT/BX
;	.........
EDIT	SEP	RD
	DB	8FH
	STXD
	LDN	RF
	STXD
	LDI	LOW TEMPO
	SEP	RD
	DB	10H
	GHI	R3
	STR	RF
	GHI	RF
	PHI	RE
	LDI	00H
	PLO	RE
	SEP	R4
	DW	LIST
	INC	R2
	LDXA
	SEP	RD
	DB	10H
	LDX
	STR	RF
	GLO	RE
	XRI	80H
	LBZ	LUS10-4
EDITB	SEP	R4
	DW	CRLF
	PLO	R7
	PLO	RE
	GHI	RE
	PHI	R7
	LDI	20H
	SEP	R4
	DW	OUTPUT
EDIT1	LDN	R7
	SEP	R4
	DW  OUTPUT
	LDA	R7
	XRI	0DH
	INC	RE
	BNZ	EDIT1
	DEC	RE
EDIT2	PLO	R7
	LDI	0AH
EDIT10	SEP	R4
	DW	OUTPUT
EDIT3	SEP	R4
	DW	INPD
	STR	R2
	SEP	RD
	DB	0CFH
	XOR		;..CHK ABORT
	BZ	TEMPOD
	SEP	RD
	DB	80H
	XOR		;..CHK BACKSPACE
	BNZ	EDIT8
	GLO	R7
	BNZ	EDIT9
	LDI	20H
	BR	EDIT10
EDIT9	DEC	R7
	SEP	R4
	DW	SPBS
	BR	EDIT3
EDIT8	SEP	RD
	DB	0AEH
	XOR		;..CHK STOP
	BZ	EDITD
	SEP	RD
	DB	0CEH
	XOR		;..CHK CHANGE
	BZ	EDITCH
	SEP	RD
	DB	0B5H
	XOR		;..CHK DELETE
	BZ	EDITDE
	LDN	RF
	XOR		;..CHK INSERT
	BZ	EDITIN
	INC	R7
	GLO	R7
	STR	R2
	GLO	RE
	SM
	BPZ	EDIT3
	DEC	R7
	SEP	RD
	DB	80H
	BR	EDIT10
EDITCH	LDI LOW EDIT11
	LSKP
EDITDE	LDI LOW EDITB
	LSKP
EDITIN	LDI LOW EDIT13
	PLO	R8
	SEP	R4
	DW	EDITO
	BZ	EDITB
	STR	R2
	PLO	RA
	GLO	R7
	PLO	RC
	GLO	R8
	XRI	LOW EDIT13
	BZ	EDIT14
	GLO	R7
	ADD
	PLO	RE
EDIT16	LDA	RE
	STR	R7
	INC	R7
	XRI	0DH
	BNZ	EDIT16
	DEC	RE
	GLO	RC
	PLO	R7
EDIT14	GLO	R8
	PLO	R3
EDIT11	NOP
EDIT13	GLO	RE
	ADD
	PLO	R7
EDIT18	LDN	RE
	STR	R7
	GLO	RE
	STR	R2
	GLO	RC
	XOR
	BZ	EDIT17
	DEC	RE
	DEC	R7
	BR	EDIT18
EDIT17	LDI	0D0H
	PLO	R7
EDIT19	LDA	R7
	STR	RE
	INC	RE
	DEC	RA
	GLO	RA
	BNZ	EDIT19
	BR	EDITB
EDITD	SEP	R4
	DW	LILB
	LDI	0D0H
	PLO	RB
	GHI	RE
	PHI	RB
	SEP	R4
	DW	LUS
	LBR	AN2+3
TEMPO	XRI	0AH
	BZ	TEMPOD
	XRI	07H
	BNZ	TEMPO1
	GLO	RE
	BZ	TEMPOD
TEMPO1	GLO	RE
	XRI	80H
	BZ	TEMPOD
	GHI	RF
	STR	RE
	INC	RE
TEMPOD	SEP	R5
;......................................................
;	..THIS ROUTINE SUPPORTS LUS
;	..LUS1.B
;	...........................
LUS1	SEP	RD
	DB	81H
	PHI	RB	;..GET LOW PT.  IN
	LDN	RF
	PLO	RB	;..USER SPACE
E5	INC	RB
	LDN	RB
	STR	R2	;..GET FIRST CHAR. IN US.
	SEP	RD
	DB	86H
	SD		;..CHECK FOR TOP OF USER SP
	DEC	RB
	LDA	RB
	STR	R2	;..GET NEXT HALF OF NUM
			;..IN USER SPACE
	SEP	RD
	DB	85H
	SDB
	BDF	E8
	DEC	RB
	SEP	R4
	DW	INCNL
	BR	E5	;..CHECK LINE NUM AGAIN
E8	DEC	RB
	SEP	R5

;[RLA] IOROUT is at $B500
	DS	0BH

;......................................................
;	..I/O ROUTINES FOR LCDS BASIC
;	..IOROUT.D
;	..........
IOROUT	NOP
	NOP
	NOP
BREAKT	LBR	BREAKC
OUTC	LDI	LOW OUTPT2
	LSKP
INPDC	LDI	LOW INPD1
	PLO	RF
	GLO	RE
	STXD
	GHI	RE
	STXD
	GLO	RC
	STXD
	GHI	RC
	STXD
	LDI	HIGH WP1 
	PHI	RC
	LDI	0A9H
	PLO	RC
	LDN	RC
	PHI	RE
	LDI	HIGH DELAY1 
	PHI	RC
	LDI	LOW DELAY1 
	PLO	RC
	GLO	RF
	PLO	R3
INPD1	SEP R4
	DW	UREAD
	BR	INPDE
OUTPT2	GHI	RF
	SEP	R4
	DW	TYPE
INPDE	INC	R2
	LDXA
	PHI	RC
	LDXA
	PLO	RC
	LDXA
	PHI	RE
	LDX
	PLO	RE
	GHI	RF
	SEP	R5

BREAKC	B4	BREAK1	;[RLA] BN4	BREAK1
BREAK2	BN4	BREAK2	;[RLA] B4	BREAK2
	SMI	00H
BREAK1	SEP	R5


;......................................................
;	..SAVE PROGRAM ON DISC..
;	..PSAVE.B
;	........................
PSAVE	LDI	LOW PSAVE1
	LSKP
DSAVE	LDI	LOW DSAVE1
	PHI	RE
	LDN	RB		;[RLA]
	XRI	0DH		;[RLA]
	BZ	PSCONT		;[RLA]
	SEP	R4		;[RLA]
	DW	ERROR		;[RLA]
	DB	38H		;[RLA]
PSCONT	GHI	RE		;[RLA]
	PLO	R3
PSAVE1	LDI 	(HIGH WP2)+1
	PHI	RA
	LDI	00H
	PLO	RA
	PLO	RE
	SEP	RD
	DB	83H
	SMI	HIGH WP2
	PHI	RE
	BR	SAVE1
DSAVE1	SEP	RD
	DB	83H
	PHI	RC
	PHI	RA
	LDN	RF
	PLO	RC
	STR	R2
	PLO	RA
	SEP	RD
	DB	9AH
	SM
	PLO	RE
	STR	RC
	INC	RC
	GHI	RA
	STR	R2
	SEP	RD
	DB	99H
	SMB
	PHI	RE
	STR	RC
	INC	RC
	GLO	RA
	STR	R2
	SEP	RD
	DB	93H
	SM
	STR	RC
	INC	RC
	GHI	RA
	STR	R2
	SEP	RD
	DB	92H
	SMB
	STR	RC
	INC	RC
	GLO	RA
	STR	R2
	SEP	RD
	DB	95H
	SM
	STR	RC
	INC	RC
	GHI	RA
	STR	R2
	SEP	RD
	DB	94H
	SMB
	STR	RC

;[RLA]  Here to save either program or data using XMODEM.  At this point
;[RLA] we have -
;[RLA]
;[RLA]		RA = start address
;[RLA]		RE = byte count
;[RLA]
;[RLA] What we do here is to switch to the SBC1802 BIOS SCRT routines and then
;[RLA} call the SBC1802 BSAVE entry point.  The SBC1802 firmware will take care
;[RLA] of the rest for us!   Note that the SBC1802 SCRT will trash RE, so we
;[RLA] have to copy the byte count to RC (aka P3) first.
SAVE1	GHI	RE		;[RLA] copy RE -> RC
	PHI	RC		;[RLA] ...
	GLO	RE		;[RLA] ...
	PLO	RC		;[RLA] ...
	SEP	R4		;[RLA] change to the SBC1802 SCRT routines
	DW	MSCRT		;[RLA] ...
	SEP	R4		;[RLA] ask the SBC1802 firmware to save
	DW	BSAVE		;[RLA]  ... our program (or data)
	SEP	R4		;[RLA] and restore our SCRT routines
	DW	RSCRT		;[RLA] ...
	LBR	TOUT

;[RLA] SETQ is at $B5F4
	DS	55H

;......................................................
;	..SET Q STATEMENT..
;	..SETQ.B..
;	..........
SETQ	SEP	R4
	DW	GVAL
	GLO	RA
	ANI	1
	BZ	SETQ1
	SEQ
	SEP	R5
SETQ1	REQ
	SEP	R5
;......................................................
;	..OUTPUT TO A PORT..
;	..PORTO.B
;	....................
PORTO	SEP	R4
	DW	EVIARG
	XRI	03H
	BNZ	PORTOE
	INC	R9
	INC	R9
	INC	R9
	LDN	R9
	PLO	RE
	GLO	R9
	SMI	07H
	PLO	R9
	SEP	R4
	DW	PORTH
	ORI	60H
	SEP	RD
	DB	40H
	LDI	LOW PORTOR
	SEP	RD
	DB	44H
	GHI	R3
	STR	RF
	DEC	RF
	LDI	0C0H
	STR	RF
	DEC	RF
	LDI	22H
	STR	RF
	GLO	RE
	STR	R2
	LBR	WP1+0C0H
PORTOR	GLO	R9
	SMI	04H
	PLO	R9
	SEP	R5
PORTOE	SEP	R4
	DW	ERROR
	DB	35H
;......................................................
;	..ROUTINE TO CLEAR
;	..CLEAR.D
;	..................
CLEAR	SEP	RD
	DB	81H
	SEP	RD
	DB	03H
	PHI	RC
	SEP	RD
	DB	82H
	PLO	RC
	ADI	05H
	SEP	RD
	DB	04H
	LDI	0FFH
	STR	RC
	SEP	RD
	DB	3DH
	INC	RC
	STR	RC
	INC	RC
	LDI	03H
	STR	RC
	INC	RC
	LDI	84H
	STR	RC
	INC	RC
	LDI	0DH
	STR	RC
	LDI	00H
	PHI	R8
CLEAR1	SEP	R4
	DW	INIT
	SEP	RD
	DB	1CH
	SEP	RD
	DB	1DH
	SEP	RD
	DB	33H
	LDI	41H
	SEP	RD
	DB	3CH
	LBR	CLARS
;......................................................
;	..INITIAL STARTUP..
;	..START.B
;	...................
;
;THIS ROUTINE SETS R4 R5 R8.1 RD R9
;STARTS LOOKING FOR RAM MEMORY FROM DFFFh AND DOWNWARD UNTIL FOUND
;THEN JUMP TO USTART1 WITH R3 AS PC
; 
START	LDI	HIGH STCALL
	PHI	R4
	LDI	HIGH STRET 
	PHI	R5
	LDI	HIGH DDROUT
	PHI	RD
	LDI	LOW DDROUT
	PLO	RD		;RD = DDROUT
	LDI	HIGH WP2
	PHI	R9
	LDI	70H
	PLO	R9		;R9 = WP2+70H
	LDI	LOW STCALL
	PLO	R4		;R4 = STD CALL ROUTINE ADDRESS
	LDI	LOW STRET
	PLO	R5		;R5 = STD RETURN ROUTINE ADDRESS
	LDI	0FFH
	PLO	R2		;R2 = XXFFH
	PHI	R8		;R8 = XXFFH
	SEX	R2		;X = R2
;[RLA]   All the code which used to size RAM has been removed.  It's pointless
;[RLA] because the RAM size in the SBC1802 is fixed, and more importantly, the
;[RLA] RAM is at the bottom of the address space and EPROM at the top!
	LDI	HIGH RAMTOP	;[RLA] RAM ends at 0xEDFF
	PHI	R2		;[RLA] ...
START5	LDI	HIGH START1
	PHI	R3
	LDI	LOW START1
	PLO	R3		;R3 = ADDRESS OF USTART1 ROUTINE
	SEP	R3		;JUMP TO USTART1

;[RLA] STKEY needs to be at $B696
	DS	10		;[RLA] ...

;......................................................
;	..INITIALIZE KEY FUNCTIONS..
;	..STKEY.B
;	.........
STKEY	LDI	08H	; [RLA] ^H backspace
	SEP	RD
	DB	00H	;..BCKSPC

	LDI	7FH	; [RLA] DELete
	SEP	RD
	DB	2AH	;..DELETE

	LDI	03H	; [RLA] ^C
	SEP	RD
	DB	2BH	;..CANCEL

	LDI	20H
	SEP	RD	
	DB	2CH	;..SPACE

	LDI	0CH	; [RLA] form feed!
	SEP	RD
	DB	2DH	;..CLEAR SCREEN

	LDI	13H
	SEP	RD
	DB	2EH	;..EDIT STOP
	LDI	44H
	SEP	RD
	DB	35H	;..EDIT ERASE
	LDI	49H
	SEP	RD
	DB	36H	;..EDIT INSERT
	LDI	43H
	SEP	RD
	DB	4EH	;..EDIT REPLACE
	LDI	01H
	SEP	RD
	DB	4FH	;..EDIT ABORT

	LDI	01H	; [RLA] ^A
	SEP	RD
	DB	4DH	;..CLS TIME DEL
	SEP	R5
;......................................................
;	..GET INPUT FROM PORT..
;	..PORT1.B
;	.......................
PORT1	GHI	R8
	STXD
	SEP	R4
	DW	EVIARG
	XRI	02H
	BNZ	PORTIE
	SEP	R4
	DW	PORTH
	ORI	68H
	SEP	RD
	DB	25H
	SEP	R4
	DW	ZRST
	INC	R9
	SEP	R4
	DB	HIGH WP1
	DB	0A5H
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	LBR	FCONV
PORTIE	SEP	R4
	DW	ERROR
	DB	34H

;[RLA] RSTIO is at $B74A
	DS	65H

;......................................................
;	..RESET I/O ON WORK PAGE..
;	..RSTIO.B
;	..........................
RSTIO	GHI	RE
	SEP	RD
	DB	29H
	LDI	LOW RSTIOT
	PLO	RA
	LDI	08H
	PLO	R8
	LDI	HIGH RSTIOT
	PHI	RA
	LDI	HIGH WP1
	PHI	RF
	LDI	8AH
	PLO	RF
RSTIO1	LDA	RA
	STR	RF
	INC	RF
	DEC	R8
	GLO	R8
	BNZ	RSTIO1
	SEP	R5
RSTIOT	DB	0D4H
	DB	HIGH IOROUT
	DW	09D5H
	DB	0D4H
	DB	HIGH IOROUT
	DW	06D5H

;[RLA] LIST2 is at $B78D
	DS	14H

;......................................................
;	..RETURN to standard (for BASIC) SCRT POINTERS..
;	..RSCRT.B
;	........................
RSCRT	LDI	HIGH STCALL
	PHI	R4
	LDI	LOW STCALL
	PLO	R4
	LDI	HIGH STRET
	PHI	R5
	LDI	LOW STRET
	PLO	R5
	SEP	R5
;......................................................
;	..APPENDAGE TO LIST..
;	..LIST2.B
;	.....................
LIST2	INC	RA
	LDA	RA
	SEP	R4
	DW	OUTPUT
	LDI	24H	;..LOAD A "$"
	LBR	OUTPUT
;......................................................
;	..BYE TO UTILITY..
;	..BYE.D
;	.......

BYE	SEP	RD
	DB	0A9H
	PHI	RE
	LBR	BEXIT	;[RLA] return to SBC1802 monitor
;......................................................
;	..APPENDAGE TO LIS..
;	..AH5.B
;	....................
AH5	GLO	RB
	STXD
	GHI	RB
	STXD
	GHI	RC
	PHI	RB
	GLO	RC
	PLO	RB	;..GET LINE NUM. FROM GP2
	SEP	R4
	DW	INCNL
	DEC	RB
AH6	GHI	RB
	PHI	RC	;..LOAD GP2 WITH FOUND LINE
	GLO	RB
	PLO	RC
	INC	R2
	LDXA
	PHI	RB
	LDX
	PLO	RB
	SEP	R5
AH7	GLO	RB
	STXD
	GHI	RB
	STXD
	GHI	RC
	SEP	RD
	DB	05H	;..SAVE AS CURRENT
	GLO	RC
	SEP	RD
	DB	06H	;..LINE NUMBER
	SEP	R4
	DW	LUS1
	BR	AH6
AH8	LDA	RA
	STR	R9
	INC	R9
	LDA	RA
	STR	R9
	INC	R9
	LDA	RA
AH9	STR	R9
	INC	R9
	LDA	RA
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	SEP	R5
AH10	GLO	RA
	STXD
	GHI	RA
	STXD
	SEP	R4
	DW	GVAL
	GHI	RA
	PHI	RC
	GLO	RA
	PLO	RC
	INC	R2
	LDXA
	PHI	RA
	LDX
	PLO	RA
	SEP	R5
;......................................................
;	..NEG FOR EXPRESSION..
;	..ENEG.B
;	......................
ENEG	GHI	R8
	ANI	04H
	BZ	ENEG1
	SEP	R4
	DW	FCOMP
	BR	ENEG2
ENEG1	INC	RD
	SEP	RD
	DB	LOW INEG
ENEG2	LBR	EXPR24

;......................................................
;	..SET TERMINAL AS OUTPUT DEVICE..
;	..TOUT.B
;	.................................
TOUT	LDI	06H
	SEP	RD
	DB	10H
	LDI	HIGH IOROUT
	STR	RF
;[RLA]	SEP	RD
;[RLA]	DB	9DH
;[RLA]	ANI	0FDH
;[RLA]	DEC	RF
;[RLA]	STR	RF
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	SEP	R5
;......................................................
;	..EXECUTION TABLE..
;	..EXTABS.B
;	...................

EXTAB	DW	REM		; 080H
	DW	INIT		; 081H
	DW	CLEAR		; 082H
	DW	RUN		; 083H
	DW	END		; 084H
	DW	LET		; 085H
	DW	PRINT		; 086H
	DW	GOTO		; 087H
	DW	IF		; 088H
	DW	INPUT		; 089H
	DW	LIST		; 08AH
	DW	GOSUB		; 08BH
	DW	RETURN		; 08CH
	DW	PAUSE		; 08DH
	DW	DIM		; 08EH
	DW	FOR		; 08FH
	DW	NEXT		; 090H
	DW	FIXED		; 091H
	DW	POKE		; 092H
	DW	RD11		; 093H
	DW	DR		; 094H
	DW	SETQ		; 095H
	DW	DLINTV		; 096H
	DW	PSAVE		; 097H
	DW	PLOAD		; 098H
	DW	DEFUS		; 099H
	DW	EOM		; 09AH
	DW	DATA		; 09BH
	DW	READ		; 09CH
	DW	RESTOR		; 09DH
	DW	EOD		; 09EH
	DW	CLARS		; 09FH
	DW	DSAVE		; 0A0H
	DW	DLOAD		; 0A1H
	DW	ENINT		; 0A2H
	DW	DISINT		; 0A3H
	DW	DMAPT		; 0A4H
	DW	TIN		; 0A5H
	DW	TOUT		; 0A6H
	DW	TRACE		; 0A7H
	DW	USCALL		; 0A8H
	DW	NOOP		; 0A9H [RLA] was BINS
	DW	NOOP		; 0AAH [RLA] was DOUT
	DW	PORTO		; 0ABH
	DW	NOOP		; 0ACH [RLA] was CLOSE
	DW	BYE		; 0ADH
	DW	EXIT		; 0AEH
	DW	NOOP		; 0AFH [RLA] was SUWBF
	DW	NOOP		; 0B0H [RLA] was SURBF
	DW	RENUM		; 0B1H
	DW	EDIT		; 0B2H
	DW	FORMAT		; 0B3H

;......................................................
;	..ROUTINE TO LIST..
;	..LIST.B
;	...................
LIST	SEP	RD
	DB	81H	;..GET LOW ADDRESS IN US.
	PHI	RA
	LDA	RF
	PLO	RA
	LDA	RF
	PHI	RC	;..STR HIGH ADDR. IN US.
	LDN	RF
	PLO	RC
	DEC	RC
	LDN	RB
	XRI	0DH	;..CHECK FOR CR
	BZ	AH2
	XRI	0C0H
	BZ	AH2
	SEP	R4
	DW	AH10
	SEP	R4
	DW	AH7
	GLO	RC
	PLO	RA
	GHI	RC
	PHI	RA
	SEP	R4
	DW	AH5
	LDN	RB	;..GET CHAR FROM COM. STRING
	XRI	0C2H	;..CHECK FOR COMMA (#C2)
	BNZ	AH2
AH11	INC	RB
	SEP	R4
	DW	AH10
	SEP	R4
	DW	AH7
	SEP	R4
	DW	AH5
AH2	SEP	R4
	DW	CRLF
	SEP	R4
	DW	LIST1
	LBR	CRLF
	
;......................................................
;	..EXECUTION TABLE FOR FUNCTIONS..
;	..EXTABF.BX
;	.................................

	ORG	EXTAB+0A0H
	DW	HEXN		; 0D0H
	DW	VAR		; 0D1H
	DW	FPN		; 0D2H
	DW	FIXN		; 0D3H
	DW	SIN		; 0D4H
	DW	COS		; 0D5H
	DW	OPPAR		; 0D6H
	DW	SEXPR		; 0D7H
	DW	ATAN		; 0D8H
	DW	EXP		; 0D9H
	DW	LN		; 0DAH
	DW	SQRT		; 0DBH
	DW	FINT		; 0DCH
	DW	PEEK		; 0DDH
	DW	ABS		; 0DEH
	DW	RND		; 0DFH
	DW	USR		; 0E0H
	DW	INT		; 0E1H
	DW	HEX2N		; 0E2H
	DW	FNUM		; 0E3H
	DW	ASC		; 0E4H
	DW	LEN		; 0E5H
	DW	BIN		; 0E6H
	DW	SGN		; 0E7H
	DW	MODU		; 0E8H
	DW	PORT1		; 0E9H
	DW	NOTB		; 0EAH
	DW	PI		; 0EBH
	DW	FVAL		; 0ECH
	DW	ERROR		; 0EDH
	DW	MEM		; 0EEH
	DW	QSTAT		; 0EFH
	DW	ESTAT		; 0F0H

;......................................................
;	..BREAK ROUTINE FOR CDS
;	..BREAK.B
;	.........
BREAK	ADI	00H
	SEP	R4
	DW	BREAKT
	BDF	BRK1
	SEP	R5
BRK1	SEP	R4
	DW	ERROR
	DB	00H
;......................................................
;	..SPACE OR NO SPACE..
;	..SPORN.B
;	.....................
SPORN	SEP	RD
	DB	0AFH
	BZ	SPORN1
	LDI	20H
	SEP	R4
	DW	OUTPUT
SPORN1	SEP	R5
;......................................................
;	..LIST1 SUPPORTS LIST ROUTINE
;	..LIST1.B
;	.............................
LIST1	SEP	R4
	DW	ZRST
	LDI	01H
	SEP	RD
	DB	2FH
	LDA	RA
	STR	R2	;..GET CHAR AND SAVE
	XRI	0FFH	;..ARE YOU AT END OF US???
	BNZ	AJ3
	LDN	RA
	XRI	0FFH
	BNZ	AJ3
	DEC	R9
	DEC	R9	;..REST. STACK
AJ4	SEP	R5
AJ3	LDN	R2	;..GET SAVED CHAR. FROM ABOVE
	SEP	R4
	DW	AH9
	SEP	R4
	DW	IBCDD
	SEP	R4
	DW	PINTN
	INC	RA
	BR	AJ15A
AJ7	INC	RA
	SEP	R4
	DW	OUTPUT
AJ5	LDN	RA
	STR	R2	;..SAVE NEXT CHAR
	XRI	0DH		;..CHECK FOR CR
	BZ	AJ6	;..GO CHK IF AT END OF LIST
	LDN	R2
	BR	AJ7	;..GO PRINT ANOTHER CHAR
AJ11	LDI	20H
	SEP	R4
	DW	OUTPUT
AJ15	LDI	00H
	SEP	RD
	DB	2FH
AJ15A	LDN	RA
	XRI	0CFH	;..CHECK FOR OPEN QUOTES
	BNZ	AJ30
	LDI	22H		;..LOAD SET OF QUOTES
AJ10	INC	RA
	SEP	R4
	DW	OUTPUT
	LDN	RA
	STR	R2	;..SAVE CHAR. TO BE PR.
	XRI	0CEH	;..IS IT END OF QUOTE??
	BZ	AJ9
	LDN	R2	;..GET NEXT CHAR TO BE PRINTED
	BR	AJ10
AJ9	LDI	22H
	INC	RA	;..LOAD CLOSING QUOTES
	SEP	R4
	DW	OUTPUT
	BR	AJ15
AJ30	LDN	RA
	XRI	0E6H
	BNZ	AJ8
	LDI	5CH
AJ31	INC	RA
	SEP	R4
	DW	OUTPUT
	LDN	RA
	STR	R2
	XRI	0B8H
	BZ	AJ32
	LDN	R2
	BR	AJ31
AJ32	LDI	5CH
	INC	RA
	SEP	R4
	DW	OUTPUT
	BR	AJ11
AJ8	LDN	RA
	XRI	0D0H	;..CHECK FOR HEX NUMBER
	BNZ	AJ12A
	LDI	40H
	INC	RA	;..LOAD AT SIGN (#)
	SEP	R4
	DW	OUTPUT
	LDI	04H
	PLO	R8	;..SET COUNTER
AJ13	LDA	RA
	DEC	R8
	SEP	R4
	DW	OUTPUT
	GLO	R8
	BNZ	AJ13	;..CHECK COUNT AND BRANCH
	BR	AJ15
AJ12A	LDN	RA
	XRI	0E2H
	BNZ	AJ12
	LDI	23H
	INC	RA
	SEP	R4
	DW	OUTPUT
	LDI	02H
	BR	AJ13-1
AJ12	LDN	RA
	XRI	0D1H	;..IS IT A VAARIABLE TO COME
	BNZ	AJ23
	SEP	R4
	DW	SPORN
	INC	RA
	LDA	RA
	SEP	R4
	DW	OUTPUT	;..PRINT VARIABLE
	BR	AJ15
AJ23	LDN	RA
	XRI	0D7H	;..IS IT A STRING VARIABLE?
	BNZ	AJ14
	SEP	R4
	DW	SPORN
	SEP	R4
	DW	LIST2
	BR	AJ15
AJ14	LDN	RA
	XRI	0D2H	;..IS IT A FIXED POINT NUMBER??
	BNZ	AJ40
	INC	RA
	SEP	R4
	DW	AH8
	SEP	R4
	DW	IBCDD
	SEP	R4
	DW	PINTN
	BR	AJ15
AJ40	LDN	RA
	XRI	0B4H
	BNZ	AJ16
	STR	R9
	INC	R9
	STR	R9
	INC	RA
	INC	RA
	INC	RA
	SEP	R4
	DW	AH9-02H
	BR	AJ40-8
AJ16	LDN	RA
	XRI	0D3H	;..IS IT FIXED POINT NUMBER??
	BNZ	AJ17
	INC	RA
	SEP	R4
	DW	AH8
	SEP	R4
	DW	FPACA
	DB	LOW U21
	SEP	R4
	DW	PFPN
	BR	AJ15
AJ17	LDN	RA
	XRI	0DH	;..IS IT A CR????
	BZ	AJ6
	SEP	R4
	DW	LIST3
	BZ	AJ15
	BR	AJ5
AJ6	GHI	RA
	STR	R2
	GHI	RC
	XOR		;..CHECK IF AT END OF LIST
	BNZ	AJ21
	GLO	RA
	STR	R2
	GLO	RC
	XOR
	BZ	AJ4
AJ21	SEP	R4
	DW	CRLF
	INC	RA
AJ22	SEP	R4
	DW	BREAK
	LBR	LIST1
	
;......................................................
;	..EVALUATE EXPRESSION..
;	..EEXPR.B
;	.......................
EXPR	GHI	R8
	ANI	0F3H
	PHI	R8
EXPRP	LDN	RB
	STR	R2
	XRI	0C9H
	BNZ	EXPR2
	INC	RB
	GLO	R3
	STXD
	BR	EXPR10
	LBR	ENEG
EXPR2	LDI	0C8H
	XOR
	BNZ	EXPR5
	INC	RB
EXPR5	GLO	R3
	STXD
	BR	EXPR10
EXPR24	LDN	RB
	STR R2
	XRI	0B5H
	BNZ	EXPR26
	INC	RB
	GLO	R3
	STXD
	BR	EXPR10
	SEP	R4
	DW	AND2
EXPR26	LDI	0B7H
	XOR
	BNZ	EXPR27
	INC	RB
	GLO	R3
	STXD
	BR	EXPR10
	SEP	R4
	DW	OR2
EXPR27	LDI	0B6H
	XOR
	BNZ	EXPR4
	INC	RB
	GLO	R3
	STXD
	BR	EXPR10
	SEP	R4
	DW	XOR2
EXPR4	LDI	0C8H
	XOR
	BNZ	EXPR6
	INC	RB
	GLO	R3
	STXD
	BR	EXPR10
	LBR	EADD
EXPR6	LDI	0C9H
	XOR
	BNZ	EXPR19
	INC	RB
	GLO	R3
	STXD
	BR	EXPR10
	LBR	ESUB
EXPR10	GLO	R3
	STXD
	BR	EXP16A
EXPR13	LDN	RB
	STR	R2
	XRI	0CAH
	BNZ	EXPR11
	INC	RB
	GLO	R3
	STXD
	BR	EXP16A
	LBR	EMULT
EXPR11	LDI	0CBH
	XOR
	BNZ	EXPR14
	INC	RB
	GLO	R3
	STXD
	BR	EXP16A
	LBR	EDIV
EXPR14	INC	R2
	LDX
	ADI	03H
	PLO	R3
EXP16A	GLO	R3
	STXD
	BR	EXPR16
EXP16C	LDN	RB
	STR	R2
	XRI	0BBH
	BNZ	EXP16B
	INC	RB
	GLO	R3
	STXD
	BR	EXPR16
	SEP	R4
	DW	EXPON
	BR	EXP16C
EXP16B	INC	R2
	LDX
	ADI	03H
	PLO	R3
EXPR16	LDI	HIGH EXTAB 
	PHI	RA
EXP16D	LDN	RB
	SMI	0D0H
	BDF	EXPR17
	SEP	R4
	DW	ERROR
	DB	09H
EXPR17	LDI	HIGH WP1
	PHI	RF
	LDI	0A4H
	PLO	RF
	LDN	RB
	SHL
	PLO	RA
	LDA	RA
	STR	RF
	INC	RF
	LDN	RA
	STR	RF
	INC	RB
	SEP	R4
	DB	HIGH WP1
	DB	0A3H
	GHI	R8
	ANI	0CH
	XRI	0CH
	BNZ	EXPR8
	SEP	R4
	DW	ERROR
	DB	07H
EXPR8	INC	R2
	LDX
	ADI	03H
	PLO	R3
EXPR19	SEP	R5
;......................................................
;	..BINARY BYTE ROUTINE..
;	..BIN.B
;	.......................
BIN	GLO	R9
	ADI	04H
	PLO	R9
	SEP	R4
	DW	ZRST
	LDI	00H
	PHI	RF
	LDI	08H
	PLO	RF
BIN1	LDA	RB
	XRI	30H
	BZ	BIN2
	XRI	01H
	BZ	BIN3
BINE	SEP	R4
	DW	ERROR
	DB	26H
BIN2	ADI	00H
	BR	BIN4
BIN3	SMI	00H
BIN4	GHI	RF
	SHLC
	PHI	RF
	DEC	RF
	GLO	RF
	BNZ	BIN1
	LDA	RB
	XRI	0B8H
	BNZ	BINE
	INC	R9
	GHI	RF
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	GHI	R8
	LBR	ICONV+3
	
;......................................................
;	..ROUT. TO LOAD INT. LINE BUFFER
;	..ELILB.B
;	................................
LILB	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	R9
	GLO	RC
	DB	00H
	LDI	0D0H
	PLO	R7	;..PT. AT LIB.
	LDI	HIGH WP1
	PHI	R7
	PHI	RC	;..PT. AT LINE
	LDI	00H
	PLO	RC	;..BUFFER.
	GHI	R8
	ANI	01H	;..CHK RUN/COM FLG
	BNZ	C2
	SEP	R4
	DW	NORLET
	BZ	C2
	GHI	R8
	ORI	02H
	PHI	R8	;..SET IMM. FLG
C2	LDI	HIGH COMLIB
	PHI	R9	;..PT. AT LIBRARY
	LDI	00H
	PLO	R9	;..PAGE
C3	LDA	RC
	STR	R7	;..GET LB CHAR.
	XRI	20H	;..CHK FOR SPACE
	BZ	C3	;..IGNORE SPACES
	DEC	RC
	GHI	RC
	PHI	RA
	GLO	RC
	PLO	RA	;..GP2 HAS START
			;..OF LINE BUFFER
			;..IGNOR. SPEACES
	LDN	R7
	XRI	0DH	;..CHK FOR CR (#0D)
	BZ	C5
C4	XRI	23H	;..CHK FOR DECIMAL PT (#2E)
	LBZ	FIORFP
C6	XRI	0FH	;..CHK FOR REM (#21)
	BNZ	C7
	LDI	80H
	STR	R7	;..LOAD REM. COM.
	INC	R7	;..INCR. POINTER
C9	INC	RC
	LDN	RC
	STR	R7
	INC	R7
	XRI	0DH
	BNZ	C9
C5	INC	R2
	SEP	R2
	SEP	R5
C11	SEP	R4
	DW	ERROR
	DB	16H
C7	XRI	03H	;..CHK FOR QUOTES (#22)
	LBNZ	HLILB
	LDI	0CFH
	STR	R7
	INC	R7
	INC	RC
C13	LDA	RC
	STR	R7
	INC	R7
	XRI	0DH
	BZ	C11
	XRI	2FH
	BNZ	C13
	LDI	0CEH
C25	DEC	R7
	STR	R7
	INC	R7
	BR	C2
C10	XRI	1CH	;..CHK FOR @ (#40)
	BNZ	C14A
	LDI	0D0H	;..LOAD HEX COM.
	STR	R7
	INC	R7
	INC	RC
	LDI	04H
	PLO	R8	;..LOAD COUNTER
C21	LDA	RC
	STR	R7
	INC	R7
	DEC	R8
	GLO	R8
	BNZ	C21	;..LOAD ILB WITH HEX
	BR	C3	;..GET ANOTHER CHAR
C14A	XRI	63H	;..CHK FOR # (#23)
	BNZ	C14
	LDI	0E2H
	STR	R7
	INC	R7
	INC	RC
	LDI	02H
	BR	C21-1
C14	SEP	R4
	DW	C34
C15	SEP	R4
	DW	NORLET
	LBZ	FIORFP
	LDA	RC
	STR	R2
	LDA	RC
	XRI	24H	;..CHK FOR $ STRING
	BNZ	C19
	LDI	0D7H	;..LOAD STRING COM.
	STR	R7
	INC	R7	;..IN ILB
	BR	C20
C19	DEC	RC
	LDI	0D1H	;..LOAD VAR. COM.
	STR	R7
	INC	R7	;..IN ILB
C20	LDN	R2
	BR	C25+1
;......................................................
;	..BOOLEAN FUNCTIONS..
;	..BOOL.B
;	.....................
OR2	LDI	LOW OR1
	LSKP
XOR2	LDI	LOW XOR1
	LSKP
AND2	LDI	LOW AND1
	STXD
	GHI	R8
	ANI	08H
	BNZ	BOOL1
	GLO	R9
	SMI	04H
	PLO	R9
	SEP	R4
	DW	INT12
	GLO	R9
	ADI	04H
	PLO	R9
	SEP	R4
	DW	INT12
BOOL1	INC	R9
	INC	R9
	INC	R9
	LDN	R9
	PLO	RF
	GLO	R9
	SMI	04H
	PLO	R9
	INC	R2
	LDX
	SEX	R9
	PLO	R3
OR1	GLO	RF
	OR
	LSKP
XOR1	GLO	RF
	XOR
	LSKP
AND1	GLO	RF
	AND
;STXED?
	STXD
	LDI	00H
	STXD
	STXD
	STR	R9
	GHI	R8
	ANI	08H
	BNZ	BOOL2
	SEP	R4
	DW	FNUMA
BOOL2	LDI	LOW EXPR24
	PLO	R6
	SEP	R5
	
;......................................................
;	..SUBTRACT FOR EXPRESSION..
;	..ESUB.B
;	...........................
ESUB	GHI	R8
	ANI	04H
	BZ	ESUB1
	SEP	R4
	DW	FPAC
	DB	LOW U7
	BR	ESUB2
ESUB1	INC	RD
	SEP	RD
	DB	LOW IMINUS
ESUB2	LBR	EXPR24
;......................................................
;	..EXECUTE ROUTINE....
;	..EEXEC.B
;	.....................
EXEC	LDI	HIGH WP2
	PHI	R9	;..SET UP COMP.
	LDI	70H
	PLO	R9	;..STACK PTR.
	BR	K2
K7	LDA	RB	;..SAVE CURRENT LINE NUM IN WORK
	SEP	RD
	DB	08H	;..SPACE
	LDA	RB
	SEP	RD
	DB	09H
	INC	RB
	GHI	R8
	ANI	40H
	BZ	K2
	GLO	R9
	ADI	04H
	PLO	R9
	SEP	R4
	DW	ZRST
	SEP	RD
	DB	88H
	STR	R9
	INC	R9
	LDN	RF
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	SEP	R4
	DW	MESOUT
	DB	"TR ["
	DB	00H
	SEP	R4
	DW	IBCDD
	SEP	R4
	DW	PINTN
	SEP	R4
	DW	MESOUT
	DB	"]",0DH,0AH,00H
	GLO	R9
	SMI	04H
	PLO	R9
K2	GHI	R8
	ANI	0F3H
	PHI	R8		;..RST. NUM FLGS
	LDI	HIGH WP1
	PHI	RA		;..PT. AT WORK PAGE
	LDI	0A0H
	PLO	RA		;..EXECUTER
	LDI	HIGH EXTAB	;..PT. AT EXECUTE TABLE
	PHI	RC
K20	LDN	RB
	XRI	0D1H
	BZ	K1
	LDN	RB
	XRI	0D7H
	BZ	K1
	LDN	RB
	SDI	0B4H
	BNF	K5-4
K1	LDA	RB	;..GET COMMAND TO BE EXEC.
	SHL
	PLO	RC	;..DIVIDE BY 2
	LDA	RC	;..GET ADDR. OF COMMAND
	STR	RA
	INC	RA	;..PUT IN WORK
	LDN	RC
	STR	RA	;..SPACE.
	SEP	R4
	DB	HIGH WP1
	DB	9FH	;..GO EXECUTE
K3	SEP	R4
	DW	BREAK
	SEP	RD
	DB	98H
	BZ	K3H
	SEP	R4
	DW	DISINT
	SEP	RD
	DB	96H
	PHI	RA
	LDA	RF
	PLO	RA
	LDI	00H
	STR	RF
	GLO	RB
	STXD
	GHI	RB
	STXD
	SEP	R4
	DW	MEM
	BZ	CHKIN1
	SEP	R4
	DW	ERROR
	DB	2BH
CHKIN1	SEP	R4
	DW	GOTO1+3
	SEP	R4
	DW	K7
	GHI	R8
	ORI	01H
	PHI	R8
	SEP	R4
	DW	ENINT1
	INC	R2
	LDXA
	PHI	RB
	LDX
	PLO	RB
K3H	LDA	RB	;..GET NEXT COMMAND AFTER EXEC
	XRI	0CDH	;..CHK FOR COLON (NEW COM.)
	BZ	K2
	DEC	RB
	LDA	RB
	XRI	0DH	;..CHECK FOR CR
	BZ	K5
	SEP	R4
	DW	ERROR
	DB	2AH
K5	GHI	R8
	ANI	01H	;..CHK RUN/COM FLG
	BNZ	K7
	SEP	R5
;......................................................
;	..HELP FOR ELIST3A
;	..HELIST.B
;	..........
LIST3A	PLO	RB
DA2	LDN	RB
	XRI	0FFH
	BNZ	LIST3E
	SEP	R5
LIST3E	LDN	RB
	ANI	1FH
	STR	R2
	GLO	RB
	PLO	RC
	ADD
	PLO	RB
	GHI	RB
	PHI	RC
	ADCI	00H
	PHI	RB
	DEC	R2
	LDA	RB
	STR	R2
	LDN	RA
	XOR
	BZ	DA3
	INC	R2
	BR	DA2
DA3	INC	R2
	INC	RA
	LDA	RC
	PLO	R8
	ANI	20H
	BZ	DA6
	SEP	R4
	DW	OUTPUT
DA6	GLO	R8
	ANI	1FH
	PLO	RB
	DEC	RB
DA4	LDA	RC
	ANI	7FH
	SEP	R4
	DW	OUTPUT
	DEC	RB
	GLO	RB
	BNZ	DA4
	GLO	R8
	ANI	40H
	BZ	DA5
	LDI	20H
	SEP	R4
	DW	OUTPUT
DA5	LDI	01H
	SEP	R5
	
;......................................................
;	..MULTIPLY FOR EXPRESSION..
;	..EMULT.B
;	...........................
EMULT	GHI	R8
	ANI	04H
	BZ	EMULT1
	SEP	R4
	DW	FPAC
	DB	LOW U8
	BR	EMULT2
EMULT1	SEP	R4
	DW	IMULT
EMULT2	LBR	EXPR13
;......................................................
;	..ROUTINE TO LOAD USER SPACE
;	..LUS.B
;	............................
LUS	LDA	RB	;..GET CURRENT LINE
			;..NUMBER FROM ILB
	SEP	RD
	DB	05H	;..LOAD WORK SPACE
	LDN	RB	;..GET REST OF LINE NUMBER
	SEP	RD
	DB	06H	;..LOAD WORK SPACE
	SEP	R4
	DW	LNUMS
	BZ	E2	;..SKIP DELETE LINE IF LINE IS NOT FOUND
	SEP	R4
	DW	DELLIN
E2	LDI	0D2H
	PLO	RB	;..USE LP TO SEARCH
			;..ILB TEMPORARILY
	LDI	HIGH WP1
	PHI	RB
	LDN	RB
	XRI	0DH	;..CHECK FOR CR
	BZ	E3	;..MUST BE A LINE DEL.
	SEP	R4
	DW	H5
	PLO	RC	;..STR LENGTH OF ILB
	SMI	9FH
	BM	LUS10
	SEP	R4
	DW	ERROR
	DB	27H
LUS10	SEP	R4
	DW	LUS1
	GHI	R8
	STXD		;..SAVE FLAG REG
	SEP	RD
	DB	83H	;..GET HIGHEST POINT IN
	PHI	RA
	PHI	R8	;..USER SPACE
	LDN	RF
	PLO	RA
	PLO	R8
	GLO	RC
	STR	R2	;..STORE ILB LENGTH
	GLO	R8
	ADD		;..PT AT TOP OF
	PLO	R8
	PLO	RF	;..USER SPACE
	GHI	R8
	ADCI	00H
	PHI	R8
	PHI	RF
E6	DEC	RA
	DEC	RF
	LDN	RA
	STR	RF	;..CLOSE GAP IN USER SPACE
	GHI	RA
	STR	R2	;..WATCH WAS CAUSED BY LINE
	GHI	RB
	XOR		;..DELETE.
	BNZ	E6
	GLO	RA
	STR	R2
	GLO	RB
	XOR
	BNZ	E6
	LDI	0D0H
	PLO	R7	;..USE LBPT AS PTR.
	LDI	HIGH WP1
	PHI	R7
	LDA	R7
	STR	RB
	INC	RB
	LDA	R7
	STR	RB
	INC	RB
	DEC	RC
	DEC	RC
	GLO	RC
	STR	RB
	INC	RB
	DEC	RC
E7	LDA	R7
	STR	RB
	INC	RB
	DEC	RC
	GLO	RC	;..CHECK COUNT
	BNZ	E7
	GLO	R8
	SEP	RD
	DB	04H
	GHI	R8
	STR	RF
	INC	R2
	LDX
	PHI	R8
E3	SEP	RD
	DB	81H
	PHI	RC
	STR	R2
	LDI	01H
	PLO	RC
	SEP	RD
	DB	83H
	SM
	STR	RC
	INC	RC
	LDN	RF
	STR	RC
	SEP	R4
	DW	CLARS
	SEP	R4
	DW	MEM
	BZ	E10
	SEP	R4
	DW	MESOUT
	DB	"<MEM WARN>"
	DB	0DH,0AH,00H
E10	LBR	REPBR
	
;......................................................
;	..HELP FOR PORT ROUTINES..
;	..PORTH.B
;	..........................
PORTH	GLO	R9
	SMI	04H
	PLO	R9
	LDI	00H
PORTH2	PLO	RF
	LDA	R9
	BNZ	PORTHE
	LDA	R9
	BNZ	PORTHE
	LDA	R9
	BNZ	PORTHE
	LDA	R9
	STR	R2
	GLO	RF
	BNZ	PORTH1
	LDN	R2
	BZ	PORTH5
	OUT 1
	DEC	R2
PORTH5	LDI	01H
	BR	PORTH2
PORTH1	LDI	07H
	OR
	XRI	07H
	BNZ	PORTHE
	GLO	R9
	SMI	08H
	PLO	R9
	LDX
	SEP	R5
PORTHE	SEP	R4
	DW	ERROR
	DB	33H
;......................................................
;	..REPAIR BRANCHES..
;	..REPBR.B
;	.........
REPBR	SEP	R4
	DW	RENUMH
	BZ	X1
	SEP	R5
X1	DEC	RA
	DEC	RA
	DEC	RA
	LDI	0D2H
	STR	RA
	INC	RA
	LDI	00H
	STR	RA
	INC	RA
	STR	RA
	INC	RA
	INC	RA
	INC	RA
	SEP	R4
	DW	RENUM5
	BR	REPBR+3

;[RLA] TIN is at $BD9E

;......................................................
;	..MOVE STANDARD CALL AND RETURN POINTERS to SBC1802 BIOS standard ...
;	..MSCRT.B
;	..........................................
MSCRT	LDI	HIGH F_CALL	;[RLA]
	PHI	R4		;[RLA]
	PHI	R5		;[RLA]
	LDI	LOW F_CALL	;[RLA]
	PLO	R4		;[RLA]
	LDI	LOW F_RETURN	;[RLA]
	PLO	R5		;[RLA]
	SEP	R5		;[RLA]
;......................................................
;	..SET TERMINAL AS INPUT DEVICE..
;	..TIN.B
;	................................
TIN	LDI	09H
	SEP	RD
	DB	0CH
	LDI	HIGH IOROUT
	STR	RF
;[RLA]	SEP	RD
;[RLA]	DB	9DH
;[RLA]	ANI	0FEH
;[RLA]	DEC	RF
;[RLA]	STR	RF
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	SEP	R5
;......................................................
;	..SET DMA POINTERS..
;	..DMAPT.B
;	....................
DMAPT	SEP	R4
	DW	GVAL
	GLO	RA
	PLO	R0
	GHI	RA
	PHI	R0
	SEP	R5
;......................................................
;	..LOAD LINE BUFFER..
;	..LLB.B
;	.......
LLB12	SEP	R4
	DW	CRLF
LLB	GHI	R8
	ANI	01H
	PLO	RC
	BZ	LLBP
	LDI	3FH
	LSKP
LLBP	LDI	3AH
	STR	R2
;[RLA]	SEP	RD
;[RLA]	DB	9DH
;[RLA]	ANI	03H
;[RLA]	PHI	RC
;[RLA]	BNZ	LLB2
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	LDN	R2
	SEP	R4
	DW	OUTPUT
LLB2	LDI	HIGH WP1
	PHI	R7
	LDI	00H
	PLO	R7
LLB3	SEP	R4
	DW	INPD
	STR	R2
	GLO	RC
	BNZ	LLB1
	GLO	R7
	BNZ	LLB1
	LDI	0DH
	XOR
	BZ	LLB2
	XRI	2DH	;..CHK FOR SPACE (#20)
	BZ	LLB2
LLB1	SEP	RD
	DB	0ABH
	XOR		;..CHK FOR CANCEL
	BZ	LLB12
LLB4	SEP	RD
	DB	0AAH
	XOR		;..CHK FOR DEL
	BNZ	LLB6A
	GLO	R7
	BZ	LLB3
LLB10	DEC	R7
	LDN	R7
	SEP	R4
	DW	OUTPT1
	BR	LLB3
LLB6A	SEP	RD
	DB	80H
	XOR
	BNZ	LLB6
	GLO	R7
	BZ	LLB
LLB6B	DEC	R7
	SEP	R4
	DW	SPBS
	BR	LLB3
LLB6	LDN	R2
	STR	R7
	INC	R7
	XRI	0DH
	BNZ	LLB7
	SEP	RD
	DB	1BH
;[RLA]	GHI	RC
;[RLA]	BNZ	LLB11
	NOP
	NOP
	NOP
	LDI	0AH
	LBR	OUTPUT
LLB7	GLO	R7
	XRI	60H
	BNZ	LLB3
	SEP	RD
	DB	80H
	SEP	R4
	DW	OUTPT1
	BR	LLB6B
LLB11	SEP	R5
	
;......................................................
;	..ERROR ROUTINE..
;	..ERROR.B
;	.................
ERROR	LDI	LOW DDROUT
	PLO	RD
	LDI	HIGH DDROUT
	PHI	RD
	LDI	HIGH WP2
	PHI	R9
	LDI	74H
	PLO	R9
	NOP		;[RLA]
	NOP		;[RLA]
	NOP		;[RLA]
AN1	SEP	R4
	DW	CRLF
	SEP	R4
	DW	ZRST
	INC	R9
	LDN	R6
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	SEP	R4
	DW	IBCDD
	SEP	R4
	DW	MESOUT
	DB	"ERR CODE ",00H
	SEP	R4
	DW	PINTN
AN3	GHI	R8
	ANI	01H
			;..IMMEDIATE MODE OR RUN??
	BZ	AN6
AN4	SEP	R4
	DW	MESOUT
	DB	0DH,0AH
	DB	"AT LINE ",00H
	SEP	R4
	DW	ZRST
	SEP	RD
	DB	88H	;..GET CURRENT LINE NUM.
	STR	R9
	INC	R9	;..PUT ON STK
	LDN	RF
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	SEP	R4
	DW	IBCDD
	SEP	R4
	DW	PINTN
AN6	SEP	R4
	DW	TIN
AN2	SEP	R4
	DW	CRLF
	SEP	RD
	DB	0BBH
	PHI	R2	;..RESET STACK
	LDI	0FFH
	PLO	R2
	SEP	R4
	DW	DISINT
	LBR	A3A
END	EQU	AN2

;......................................................
;	..NOT FUNCTION..
;	..NOT.B
;	................
NOTB	GHI	R8
	STXD
	SEP	R4
	DW	IEVARG
	LDI	00H
	STR	R9
	INC	R9
	STR	R9
	INC	R9
	STR	R9
	INC	R9
	LDN	R9
	XRI	0FFH
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	LBR	FCONV
	
;......................................................
;	..DIVIDE FOR EXPRESSION
;	..EDIV.B
;	........
EDIV	GHI	R8
	ANI	04H
	BZ	EDIV1
EDIV2	SEP	R4
	DW	FPAC
	DB	LOW U9	; WAS HIGH U9
	BR	EDIV3
EDIV1	SEP	R4
	DW	IDIV
EDIV3	LBR	EXPR13
;......................................................
;	..EDIT SUBROUTINE
;	..EDITO.B
;	.........
EDITO	GHI	RE
	PHI	RC
	LDI	0D0H
	PLO	RC
	GLO	R8
	XRI	LOW EDIT13
	BNZ	EDITO5
	GLO	RE
	SMI	5FH
	BR	EDITO4
EDITO5	GLO	RE
	STR	R2
	GLO	R7
	SM
EDITO4	XRI	0FFH
	PLO	RA
	LDI	00H
	INC	RA
	INC	RA
	BPZ	EDITOR
EDITO1	SEP	R4
	DW	INPD
	STR	R2
	SEP	RD
	DB	0AEH
	XOR
	BZ	EDITOD	;..CHK STOP
	SEP	RD
	DB	0CFH	
	XOR
	BZ	EDITOR	;..CHK ABORT
	SEP	RD
	DB	80H
	XOR
	BZ	EDITOB	;..CHK BKSPC
	SEP	RD
	DB	0AAH
	XOR
	BZ	EDITOE	;..CHK DELETE
	LDN	R2
	STR	RC
	INC	RC
	DEC	RA
	GLO	RA
	BNZ	EDITO1
	DEC	RC
	INC	RA
	SEP	RD
	DB	80H
EDITO3	SEP	R4
	DW	OUTPUT
	BR	EDITO1
EDITOB	GLO	RC
	XRI	0D0H
	BNZ	EDITO2
	SEP	RD
	DB	0ACH
	BR	EDITO3	;..OUT ->
EDITO2	DEC	RC
	SEP	R4
	DW	SPBS
	INC	RA
	BR	EDITO1
EDITOE	GLO	RC
	XRI	0D0H
	BZ	EDITO1
	DEC	RC
	LDN	RC
	SEP	R4
	DW	OUTPUT
	BR	EDITO2+1
EDITOD	GLO	RC
	SMI	0D0H
EDITOR	SEP	R5
;......................................................
;	..HELP FOR LILB..
;	..HLILB.B
;	.................
HLILB	XRI	7EH	;..CHK FOR \ (#5C)
	LBNZ	C10
	LDI	0E6H
	STR	R7
	INC	R7
	INC	RC
HLILB2	LDA	RC
	STR	R7
	INC	R7
	XRI	0DH
	LBZ	C11
	XRI	51H
	BNZ	HLILB2
	LDI	0B8H
	LBR	C25
;......................................................
;	..LIST COMMANDS FROM EXPANDED TABLES
;	..ELIST3.B
;	..........
LIST3	GLO	RC
	STXD
	GHI	RC
	STXD
	GLO	RB
	STXD
	GHI	RB
	STXD
	LDI	HIGH COMLIB
	PHI	RB
	LDI	00H
	SEP	R4
	DW	LIST3A
	BNZ	LIST3D
	SEP	R4
	DW	ERROR
	DB	18H
LIST3D	INC	R2
	LDXA
	PHI	RB
	LDXA
	PLO	RB
	LDXA
	PHI	RC
	LDX
	PLO	RC
	GLO	R8
	ANI	80H
	SEP	R5

;[RLA] CLARS is at $BFD0
NOOP	SEP	R5		;[RLA] unimplemented functions
	DS	2DH

;......................................................
;	..CLEAR ARRAY AND STRINGS..
;	..CLARS.B
;	...........................
CLARS	SEP	RD
	DB	84H
	ADI	09H
	PLO	RA
	SEP	RD
	DB	83H
	ADCI	00H
	SEP	RD
	DB	12H
	SEP	RD
	DB	14H
	SEP	RD
	DB	19H
	GLO	RA
	SEP	RD
	DB	13H
	SEP	RD
	DB	15H
	SEP	RD
	DB	1AH
	SEP	R5
;......................................................
;	..ADD FOR EXPRESSION..
;	..EADD.B
;	......................
EADD	GHI	R8
	ANI	04H
	BZ	EADD1
	SEP	R4
	DW	FPAC
	DB	LOW U6
	BR	EADD2
EADD1	INC	RD
	SEP	RD
	DB	LOW IADD
EADD2	LBR	EXPR24

;UNKNOWN ROUTINE ADDED
;REPLACES THE PAGE COMMAND BELOW
	STR	RF
	SEP	RD
	GHI	RD
	ANI	0FDH
	DEC	RF
	STR	RF
	SEP	R5


;	PAGE
;......................................................
;	..INTEGER FUNCTION..
;	..FINT.A
;	........
FINT	GHI	R8
	STXD
	SEP	R4
	DW	EVARG
	GHI	R8
	ANI	04H
	BZ	FINTD
FINT1	LDN	R9
	BZ	FINTD
	PLO	RF
	ANI	80H
	BZ	FINTC
	INC	R9
	LDN	R9
	ANI	80H
	PHI	RA
	DEC	R9
	GLO	RF
	SDI	98H
	PLO	RF
	PLO	RA
	BM	FINTD
	SMI	18H
	BPZ	FINTZ
	LDI	00H
	PHI	RF
	INC	R9
FINT4	GLO	RF
	BZ	FINT2
	DEC	RF
	LDN	R9
	SHR
	STR	R9
	INC	R9
	LDN	R9
	SHRC
	STR	R9
	INC	R9
	LDN	R9
	SHRC
	STR	R9
	BNF	FINT3
	LDI	01H
	PHI	RF
FINT3	DEC	R9
	DEC	R9
	BR	FINT4
FINT2	GLO	RA
	BZ	FINT5
	DEC	RA
	INC	R9
	INC	R9
	LDN	R9
	SHL
	STR	R9
	DEC	R9
	LDN	R9
	SHLC
	STR	R9
	DEC	R9
	LDN	R9
	SHLC
	STR	R9
	BR	FINT2
FINT5	DEC	R9
	GHI	RA
	BZ	FINTD
	GHI	RF
	BZ	FINTD
FINT6	SEP	R4
	DW	DFONE+1
	SEP	R4
	DW	FPAC
	DB	LOW U7
FINTD	LBR	CONV
FINTZ	BZ	FINTC
	LBR	SGNZ
FINTC	INC	R9
	LDN	R9
	ANI	80H
	DEC	R9
	PLO	RF
	INC	RD
	SEP	RD
	DB	LOW AZRST
	GLO	RF
	BNZ	FINT6
	BR	FINTD
	
;......................................................
;	..ROUTINE FOR RUN ..
;	..RUN.A
;	........................
RUN	GHI	R8
	ORI	01H	;..SET RUN/COM FLAG
	PHI	R8
	LDI	0D0H
	SEP	RD
;NOTE DZ
;	DZ	07H
	DB	07H	;REPLACE DZ WITH DC/DB
	LDI	0DH
	SEP	RD
	DB	50H
	SEP	R4
	DW	RESTOR
	LDN	RB
	XRI	0C8H
	BZ	RUN1
	XRI	0C5H
	BZ	RUN2
	LBR	GOTO
RUN1	SEP	R4
	DW	RENUMH
RUN3	BNZ	RUN2
	LDA	RA
	SEP	RD
	DB	05H
	INC	RF
	INC	RF
	LDN	RA
	STR	RF
	DEC	RA
	DEC	RA
	DEC	RA
	DEC	RA
	SEP	R4
	DW	LNUMS
	BZ	RUNE
	LDI	0B4H
	STR	RA
	INC	RA
	GHI	RB
	STR	RA
	INC	RA
	GLO	RB
	STR	RA
	DEC	RA
	DEC	RA
	SEP	R4
	DW	RENUM5
	BR	RUN3
RUN2	SEP	R4
	DW	CLARS
	SEP	RD
	DB	81H
	PHI	RB
	LDN	RF
	PLO	RB
	LBR	GOTO3
RUNE	SEP	R4
	DW	ERROR
	DB	2EH
	
;......................................................
;	..PRINT FLOATING POINT NUMBER..
;	..PFPN.A
;	........
PFPN	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	R7
	GLO	R8
	GLO	RA
	GLO	RC
	GLO	RB
	GLO	RE
	DB	00H
	PLO	RB
	LDI	0E0H
	PLO	RE
	SEP	RD
	DB	0BDH
	PLO	R7
	PHI	RB
	SMI	06H
	BM	PFP9
	LDI	06H
	PLO	R7
PFP9	GHI	RF
	PHI	RA
	PHI	RC
	GHI	R9
	PHI	RE
	LDI	0C0H
	PLO	RA
	LDA	RA
	XRI	0BH
	BZ	PFP15
	LDI	2DH
	SEP	R4
	DW	TNUM
PFP15	LDA	RA
	BNZ	PFP2
	PLO	R8
	SEP	R4
	DW	ATNUM
	LBR	PFP3
PFP2	LDI	0CAH
	PLO	RA
	LDA	RA
	PLO	R8
	LDI	00H
	STR	R2
PFP5	GLO	R8
	BZ	PFP4
	LDN	R2
	ADI	0AH
	STR	R2
	DEC	R8
	BR	PFP5
PFP4	LDN	RA
	ADD
	PLO	R8
	SEP	RD
	DB	0C9H
	XRI	0BH
	BNZ	PFP30
	GLO	R8
	SMI	07H
	BPZ	PFPSN
	BR	PFP6
PFP30	GLO	R8
	STR	R2
	SMI	04H
	BM	PFP23
	GHI	RB
	SMI	07H
	BPZ	PFPSN
PFP23	LDI	0C8H
	PLO	RC
	SM
	PLO	RA
	SMI	0C1H
	BM	PFP8
PFP7	LDN	RA
	STR	RC
	DEC	RA
	DEC	RC
	GLO	RA
	XRI	0C0H
	BNZ	PFP7
PFP8	LDI	00H
	STR	RC
	DEC	RC
	GLO	RC
	XRI	0C0H
	BNZ	PFP8
	PLO	R8
PFP6	GLO	R7
	STR	R2
	GLO	R8
	ADD
	PHI	R7
	SMI	07H
	BM	PFP10
	LDI	06H
	PHI	R7
PFP10	GHI	R7
	ADI	0C1H
	PLO	RA
	PLO	RC
	LDI	05H
PFP13	STR	R2
	LDN	RA
	ADD
	SMI	0AH
	BM	PFP11
	LDI	00H
	STR	RA
	DEC	RA
	GLO	RA
	XRI	0C0H
	BZ	PFP12
	LDI	01H
	BR	PFP13
PFP12	INC	RA
	LDI	01H
	STR	RA
	INC	R8
	BR	PFP14
PFP11	LDN	RA
	ADI	01H
	STR	RA
PFP14	LDI	00H
	STR	RC
	INC	RC
	GLO	RC
	XRI	0C9H
	BNZ	PFP14
PFP3	LDI	0C1H
	PLO	RA
PFP24	GLO	R8
	BZ	PFP16
	DEC	R8
	LDA	RA
	SEP	R4
	DW	ATNUM
	BR	PFP24
PFP16	GLO	RA
	SDI	0C9H
	PLO	R8
	LDI	0C8H
	PLO	RC
PFP18	LDN	RC
	BNZ	PFP17
	DEC	R8
	DEC	RC
	GLO	RC
	STR	R2
	GLO	RA
	SMI	01H
	XOR
	BNZ	PFP18
PFP17	GLO	R8
	BZ	PFP26
PFP21	SEP	RD
	DB	0BDH
	PLO	R7
	BZ	PFP26
PFP27	LDI	2EH
	SEP	R4
	DW	TNUM
PFP22	GLO	R8
	BZ	PFPAD
	GLO	R7
	BZ	PFPAD
	DEC	R7
	DEC	R8
	LDA	RA
	SEP	R4
	DW	ATNUM
	BR	PFP22
PFPAD	GHI	RB
	XRI	0FFH
	BZ	PFPAD1
PFP25	GLO	R7
	BZ	PFPAD1
	LDI	30H
	SEP	R4
	DW	TNUM
	DEC	R7
	BR	PFP25
PFP26	GHI	RB
	XRI	0FFH
	BNZ	PFP27
PFPAD1	GLO	RB
	BZ	PFPAD2
	LDI	45H
	SEP	R4
	DW	TNUM
	LDI	0C9H
	PLO	RA
	LDA	RA
	ADI	20H
	SEP	R4
	DW	TNUM
	LDA	RA
	SEP	R4
	DW	ATNUM
	LDN	RA
	SEP	R4
	DW	ATNUM
PFPAD2	INC	R2
	SEP	R2
	LBR	ONUM
PFPSN	LDI	0C6H
	PLO	RA
	LDI	06H
	PLO	R8
PFP20	LDN	RA
	BNZ	PFP19
	DEC	R8
	DEC	RA
	BR	PFP20
PFP19	LDI	0C1H
	PLO	RA
	PLO	RB
	BR	PFP21
	
;.................................................
;	..FOR ROUTINE..
;	..FOR.A
;	...............
FOR	GHI	R8
	ANI	01H
	BZ	FORE
	SEP	R4
	DW	MEM
	BZ	FOR1
FORE	SEP	R4
	DW	ERROR
	DB	0BH
FOR1	PLO	RE
	LDA	RB
	PLO	RF
	INC	RB
	LDN	RB
	DEC	RB
	XRI	0D6H
	BZ	FORE1
	SEP	R4
	DW	AT15
	DEC	RC
	DEC	RC
	DEC	RC
	GLO	RC
	STXD
	GHI	RC
	STXD
	LDA	RB
	XRI 0C1H
	BZ	FORE1+4
FORE1	SEP	R4
	DW	ERROR
	DB	0CH
	SEP	R4
	DW	EXPRP
	LDA	RB
	XRI	0C0H
	BNZ	FOR2
	SEP	R4
	DW	EXPRP
	GHI	R8
	ANI	08H
	BZ	FOR14
	LDN	R9
	BR	FOR16
FOR14	INC	R9
	LDN	R9
	DEC	R9
FOR16	ANI	80H
	PLO	RE
	BR	FOR3
FOR2	SEP	R4
	DW	DFONE
FOR3	GLO	RB
	STXD
	GHI	RB
	STXD
	GLO	RE
	STXD
	GHI	R8
	ANI	0CH
	STR	R2
	INC	RD
	SEP	RD
	DB	LOW PIS
	DEC	R9
	DEC	R9
	DEC	R9
	DEC	R9
	INC	R2
	INC	RD
	SEP	RD
	DB	LOW PIS
	GLO	R9
	SMI	04H
	PLO	R9
FOR4	SEP	R4
	DW	K3
	GLO	R2
	PLO	RA
	GHI	R2
	PHI	RA
	INC	RA
	GLO	R9
	ADI	04H
	PLO	R9
	LDI	08H
	PLO	RF
FOR5	LDA	RA
	STR	R9
	INC	R9
	DEC	RF
	GLO	RF
	BNZ	FOR5
	LDA	RA
	STR	R2
	GHI	R8
	OR
	ORI	01H
	PHI	R8
	LDA	RA
	PLO	RE
	INC	RA
	INC	RA
	LDA	RA
	PHI	RC
	LDN	RA
	PLO	RC
	DEC	RA
	DEC	RA
	DEC	RA
	LDI	04H
	PLO	RF
FOR6	LDA	RC
	STR	R9
	INC	R9
	DEC	RF
	GLO	RF
	BNZ	FOR6
	GLO	R9
	SMI	04H
	PLO	R9
	GHI	R8
	ANI	08H
	BZ	FOR7
	INC	RD
	SEP	RD
	DB	LOW IADD 
	BR	FOR8
FOR7	SEP	R4
	DW	FPAC
	DB	LOW U6
FOR8	INC	R9
	INC	R9
	INC	R9
	DEC	RC
	LDI	04H
	PLO	RF
FOR9	LDN	R9
	STR	RC
	DEC	R9
	DEC	RC
	DEC	RF
	GLO	RF
	BNZ	FOR9
	INC	R9
	GHI	R8
	ANI	08H
	BZ	FOR10
	INC	RD
	SEP	RD
	DB	LOW IMINUS
	INC	RD
	SEP	RD
	DB	LOW ZRCH
	PLO	RF
	LDN	R9
	BR	FOR11
FOR10	SEP	R4
	DW	FPAC
	DB	LOW U7
	LDA	R9
	PLO	RF
	LDN	R9
	DEC	R9
FOR11	ANI	80H
	STR	R2
	GLO	R9
	SMI	04H
	PLO	R9
	GLO	RF
	BZ	FOR15
	GLO	RE
	XOR
	BNZ	FOR12
FOR15	LDA	RA
	PHI	RB
	LDN	RA
	PLO	RB
	BR	FOR4
FOR12	LDI	10H
	PLO	RF
FOR13	INC	R2
	DEC	RF
	GLO	RF
	BNZ	FOR13
	LDX
	PHI	R6
	LDI	LOW K3
	PLO	R6
	SEP	R5
	
;......................................................
;	..RESTORE STATEMENT..
;	..RESTOR.A
;	.....................
RESTOR	SEP	RD
	DB	82H
	SEP	RD
	DB	32H
	SEP	RD
	DB	81H
	SEP	RD
	DB	31H
	LDI	00H
	STR	RF
	SEP	R5
;......................................................
;	..DATA STATEMENT..
;	..DATA.A
;	..................
DATA	SEP	R4
	DW	INCNL+3
	DEC	RB
	SEP	R5
;......................................................
;	..ROUTINE FOR "LET" WITH ARRAYS..
;	..LET.A
;	.................................
LET	LDA	RB
	PLO	RF
	XRI	0D7H	;..IS IT A STR. VAR?
	BNZ	AT15
	SEP	R4
	DW	SVLET
	SEP	R5
AT15	GLO	RF	;..GET COMMAND SAVED ABOVE
	XRI	0D1H	;..BETTER BE A VARIABLE!!!
	BZ	AT2
AT4	SEP	R4
	DW	ERROR
	DB	15H
AT2	LDA	RB
	PLO	R8	;..GET VAR. NAME
	SMI	41H
	SHL
	SHL
	PLO	RC
	LDI	HIGH WP2
	PHI	RC
	LDN	RB
	XRI	0D6H	;..LOOK FOR PARENS
	BNZ	AT25
	SEP	R4
	DW	AT13
	BR	AT10
AT25	XRI	04H		;..CHK FOR 'D2'
	BNZ	AT10
SUBLT	GHI	R7
	STXD
	INC	RB
	LDA	RB
	BNZ	SUBLTE
	LDA	RB
	BNZ	SUBLTE
	LDA	RB
	BZ	SUBLT6
SUBLTE	SEP	R4
	DW	ERROR
	DB	3BH
SUBLT6	LDA	RB
	PHI	R7
	SEP	R4
	DW	SUBSCH
	BNZ	SUBLTD
	GLO	R7
	STXD
	GHI	R7
	STXD
	GLO	RA
	STXD
	GHI	RA
	STXD
	SEP	RD
	DB	99H
	PHI	RA
	LDN	RF
	PLO	RA
	ADI	06H
	PLO	RF
	GHI	RA
	ADCI	00H
	PHI	RF
	STR	R2
	GHI	R2
	XOR
	BNZ	SUBLT1
	SEP	R4
	DW	ERROR
	DB	30H
SUBLT1	GLO	RA
	STR	R2
	GLO	RC
	SD
	PLO	R7
	GHI	RA
	STR	R2
	GHI	RC
	SDB
	ADI	01H
	PHI	R7
SUBLT3	LDN	RA
	STR	RF
	DEC	RA
	DEC	RF
	DEC	R7
	GHI	R7
	BNZ	SUBLT3
SUBLT2	SEP	RD
	DB	95H
	DEC	RF
	ADI	06H
	STR	RF
	DEC	RF
	LDN	RF
	ADCI	00H
	STR	RF
	DEC	RF
	LDN	RF
	ADI	06H
	STR	RF
	DEC	RF
	LDN	RF
	ADCI	00H
	STR	RF
	GLO	RF
	ADI	08H
	PLO	RF
	LDN	RF
	ADI	06H
	STR	RF
	DEC	RF
	LDN	RF
	ADCI	00H
	STR	RF
	INC	R2
	LDXA
	PHI	RA
	LDXA
	PLO	RA
	LDXA
	PHI	R7
	LDX
	PLO	R7
	GLO	R8
	STR	RC
	INC	RC
	GHI	R7
	STR	RC
	INC	RC
SUBLTD	INC	R2
	LDX
	PHI	R7
AT10	GLO	R8
	STR	R2	;..INT OR FP???
	SEP	RD
	DB	0BCH
	SD
	BNF	AT11
	GHI	R8
	ORI	04H
	BR	AT12
AT11	GHI	R8
	ORI	08H
AT12	PHI	R8
	GLO	R6
	XRI	(LOW INRD)+3
	BNZ	AT19
	GHI	R6
	XRI	HIGH INRD+3
	BNZ	AT19
	SEP	R5
AT19	LDA	RB
	XRI	0CCH	;..SHOULD BE = SIGN
	BNZ	AT4
	SEP	R4
	DW	EXPRP
AT17	LDA	R9
	STR	RC
	INC	RC
	LDA	R9
	STR	RC
	INC	RC
	LDA	R9
	STR	RC
	INC	RC
	LDN	R9
	STR	RC
	GLO	R9
	SMI	07H
	PLO	R9
	SEP	R5

;......................................................
;	..PAUSE ROUTINE..
;	..PAUSE.A
;	.................
PAUSE	SEP	R4
	DW	IEVARG
	SEP	R4
	DW	GVAL+3
AQ5	SEP	R4
	DW	BREAK
PAUSE1	LDI	0F0H
	PLO	RF	;..LOAD LOOP COUNTER
AQ7	DEC	RF	;..DEC COUNTER
	GLO	RF	;..CHECK COUNT
	BNZ	AQ7
	GHI	RA
	LSNZ
	GLO	RA
	NOP
	DEC	RA
	BNZ	AQ5	;..COMPLETE LOOP
AQ9	SEP	R5
;......................................................
;	..PRINT CHARACTER STRING..
;	..PCHARS.A
;	..........................
PCHARS	INC	RB
AB3	LDA	RB
	STR	R2	;..STORE CHAR
	XRI	0CEH	;..CHK FOR END OF QUOTE
	BNZ	AB2
	SEP	R5
AB2	LDN	R2	;..GET BACK CHAR
	SEP	R4
	DW	OUTPUT
	BR	AB3

;......................................................
;	..RENUMBERING ROUTINE..
;	..RENUM.A
;	.........
RENUM	GHI	R8
	STXD
	GLO	R7
	STXD
	LDN	RB
	XRI	0D2H
	BNZ	RENM12
	SEP	R4
	DW	GVAL
	GLO	RA
	LSKP
RENM12	LDI	0AH
RENM13	PLO	R7
	SEP	RD
	DB	99H
	PHI	RA	;..SET TABLE PTR
	LDN	RF
	PLO	RA
	SEP	RD
	DB	81H
	PHI	RC
	LDN	RF
	PLO	RC
	LDI	00H
	PHI	RE
	PLO	RE
RENUM1	LDA	RC
	STR	RA
	INC	RA
	XRI	0FFH
	BZ	RENM14
	LDA	RC
	STR	RA
	INC	RA
	LDN	RC
	STR	R2
	GLO	RC
	ADD
	PLO	RC
	GHI	RC
	ADCI	00H
	PHI	RC
	GHI	RA
	STR	R2
	GHI	R2
	XOR
	BNZ	RENUM1
	SEP	R4
	DW	ERROR
	DB	3FH
RENM14	SEP	RD
	DB	81H
	PHI	RA
	LDN	RF
	PLO	RA
RENM15	LDN	RA
	XRI	0FFH
	BZ	RENUM2
	GLO	RE
	STR	R2
	GLO	R7
	ADD
	PLO	RE
	GHI	RE
	ADCI	00H
	PHI	RE
	STR	RA
	INC	RA
	GLO	RE
	STR	RA
	INC	RA
	LDN	RA
	STR	R2
	GLO	RA
	ADD
	PLO	RA
	GHI	RA
	ADCI	00H
	PHI	RA
	BR	RENM15
RENUM2	SEP	R4
	DW	RENUMH
	BNZ	RENUMD
	PLO	R8
	PHI	R8
	SEP	RD
	DB	99H
	PHI	RC	;..GET READY TO
	LDN	RF
	PLO	RC	;..SCAN TABLE
RENUM9	GLO	R8
	STR	R2
	GLO	R7
	ADD
	PLO	R8
	GHI	R8
	ADCI	00H
	PHI	R8
	LDA	RC
	STR	R2
	XRI	0FFH
	BZ	RENUME	;..NO LINE NUMBER FOUND
	LDN	RA
	XOR
	BZ	RENUM8
	INC	RC
	BR	RENUM9
RENUM8	INC	RA
	LDN	RA
	STR	R2
	DEC	RA
	LDA	RC
	XOR
	BNZ	RENUM9
	GHI	R8
	STR	RA
	INC	RA
	GLO	R8
	STR	RA
	INC	RA
	SEP	R4
	DW	RENUM5
	BR	RENUM2+3
RENUMD	INC	R2
	LDXA
	PLO	R7
	LDX
	PHI	R8
	SEP	R4
	DW	ZRST
	GHI	RE
	STR	R9
	INC	R9
	GLO	RE
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	SEP	R4
	DW	IBCDD
	SEP	R4
	DW	PINTN
	SEP	R4
	DW	MESOUT
	DB	" COMP BR"
	DB	0DH,0AH,00H
	SEP	R5
RENUME	SEP	R4
	DW	ERROR
	DB	40H
;......................................................
;	..ROUTINE TO INCR TO NEXT LINE
;	..INCNL.A
;	...................................
INCNL	INC	RB
	INC	RB
	INC	RB	;..MOVE LINE PTR PAST LINE
			;..NUMBER.
H5	LDI	03H
	PLO	R8
H4	LDA	RB	;..SET COUNTER
	INC	R8
	XRI	0D2H
	BZ	H2
	XRI	01H
	BZ	H2
	XRI	67H
	BNZ	H3
H2	INC	RB
	INC	RB	;..MOVE LINE POINTER PAST
	INC	RB
	INC	RB	;..4 BYTE NUMBER
	INC	R8
	INC	R8
	INC	R8
	INC	R8
	BR	H4	;ASSUME IT IS H5
H3	XRI	0B9H	;..CHK CR
	BNZ	H4	;..GO GET ANOTHER CHARACTER
	GLO	R8	;..FLAG.0 HAS LENGTH OF LINE
	SEP	R5
;......................................................
;	..STRING VARIABLE LET..
;	..SVLET.A
;	.......................
SVLET	GLO	RE
	STXD
	LDA	RB
	STXD
	SEP	R4
	DW	SVWK
	STXD
	LDA	RB
	XRI	0CCH
	BNZ	SVLETE
	SEP	R4
	DW	SEXPR1
	INC	R2
	LDXA
	PLO	RE
	LDX
	PLO	R8
	SEP	R4
	DW	LDSV
	INC	R2
	PLO	RE
	SEP	R5
SVLETE	SEP	R4
	DW	ERROR
	DB	22H

;......................................................
;	..ROUTINE TO FIND ARRAY STORAGE..
;	..AT13.AX
;	.................................
AT13	GLO	RA
	STXD
	GHI	RA
	STXD
	GHI	R8
	STXD
	GLO	R8
	STXD
	SEP	R4
	DW	EVIARG
	PLO	RF
	PLO	RC
	LDI	00H
	PLO	R8
AT13B	LDA	R9
	BNZ	AT13E
	LDA	R9
	BNZ	AT13E
	LDA	R9
	BNZ	AT13E
	GLO	R8
	BNZ	AT13N
	LDN	R9
	PHI	RC
	BR	AT13G
AT13N	LDN	R9
	PLO	RC
AT13G	BZ	AT13E
	GLO	R9
	SMI	07H
	PLO	R9
	DEC	RF
	INC	R8
	GLO	RF
	BNZ	AT13B
AT13C	GLO	R9
	ADI	04H
	PLO	R9
	INC	R2
	LDX
	PLO	R8
	SEP	RD
	DB	92H
	PHI	RA
	LDA	RF
	PLO	RA
	LDA	RF
	PHI	R7
	LDN	RF
	PLO	R7
AT13F	GHI	R7
	STR	R2
	GHI	RA
	XOR
	BNZ	AT13D
	GLO	R7
	STR	R2
	GLO	RA
	XOR
	BZ	AT13E
AT13D	LDA	R7
	STR	R2
	GLO	R8
	XOR
	BZ	AT13A
	LDA	R7
	STR	R2
	LDA	R7
	PHI	RF
	GLO	R7
	ADD
	PLO	R7
	GHI	RF
	STR	R2
	GHI	R7
	ADC
	PHI	R7
	SEX	R2
	BR	AT13F
AT13A	INC	R7
	INC	R7
	SEP	R4
	DW	ZRST
	INC	R9
	GHI	RC
	STR	R9
	INC	R9
	STR	R2
	INC	R7
	LDN	R7
	SM
	BNF	AT13E
	DEC	R7
	SEP	R4
	DW	ZRST
	INC	R9
	GLO	RC
	STR	R2
	LDA	R7
	SM
	BNF	AT13E
	DEC	RC
	GLO	RC
	STR	R9
	INC	R9
	SEP	R4
	DW	ZRST
	INC	R9
	LDN	R7
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	DEC	R7
	DEC	R7
	DEC	R7
	SEP	R4
	DW	IMULT
	INC	RD
	SEP	RD
	DB	LOW IADD
	INC	R9
	INC	R9
	LDA	R9
	PHI	RC
	LDN	R9
	PLO	RC
	GLO	R9
	SMI	07H
	PLO	R9
	GLO	RC
	SHL
	PLO	RC
	GHI	RC
	SHLC
	PHI	RC
	GLO	RC
	SHL
	PLO	RC
	GHI	RC
	SHLC
	PHI	RC
	GLO	R7
	STR	R2
	GLO	RC
	ADD
	PLO	RC
	GHI	R7
	STR	R2
	GHI	RC
	ADC
	PHI	RC
	INC	R2
	LDXA
	PHI	R8
	LDXA
	PHI	RA
	LDX
	PLO	RA
	SEP	R5
AT13E	SEP	R4
	DW	ERROR
	DB	02H

;......................................................
;	..INPUT AND READ SUPPORT..
;	..INRD.A
;	..........................
INRD	SEP	R4
	DW	LET
INRD4	GLO	RB
	STXD
	GHI	RB
	STXD
	GLO	RC
	STXD
	GHI	RC
	STXD
	GLO	RE
	PLO	RB
	GHI	RE
	PHI	RB
	SEP	R4
	DW	EXPRP
	LDN	RB
	XRI	0C2H
	BNZ	INRD5
	INC	RB
	BR	INRD6
INRD5	LDN	RB
	XRI	0DH
	BZ	INRD6
	SEP	R4
	DW	ERROR
	DB	13H
INRD6	GLO	RB
	PLO	RE
	GHI	RB
	PHI	RE
	INC	R2
	LDXA
	PHI	RC
	LDXA
	PLO	RC
	LDXA
	PHI	RB
	LDX
	PLO	RB
	GHI	R8
	ANI	0F3H
	PHI	R8
	LBR	AT17

;......................................................
;	..STANDARD RETURN PROGRAM..
;	..STRET.A
;	...........................
	SEP	R3
STRET	PHI	RF
	SEX	R2
	GHI	R6
	PHI	R3
	GLO	R6
	PLO	R3
	INC	R2
	LDA	R2
	PLO	R6
	LDN	R2
	PHI	R6
	GHI	RF
	BR	STRET-1
;......................................................
;	..FP AND INT RND NUM GENERATOR..
;	..RND.A
;	................................
RND	GHI	R8
	STXD
	GLO	RA
	STXD
	GHI	RA
	STXD
	GLO	R9
	ADI	04H
	PLO	R9
	SEP	R4
	DW	ZRST
	LDI	36H
	STR	R9
	INC	R9
	LDI	19H
	STR	R9
	INC	R9
	SEP	R4
	DW	ZRST
	LDI	35H
	STR	R9
	INC	R9
	LDI	8DH
	STR	R9
	INC	R9
	SEP	R4
	DW	ZRST
	SEP	RD
	DB	0BEH
	STR	R9
	INC	R9
	LDN	RF
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	SEP	R4
	DW	IMULT
	INC	RD
	SEP	RD
	DB	LOW IADD
	INC	R9
	INC	R9
	LDA	R9
	PHI	RA
	SEP	RD
	DB	3EH
	LDN	R9
	PLO	RA
	SEP	RD
	DB	3FH
	DEC	R9
	DEC	R9
	DEC	R9
	SEP	R4
	DW	ZRST
	LDN	RB
	XRI	0D6H
	BNZ	RND3
	GLO	RA
	STR	R9
	INC	R9
	GHI	RA
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	SEP	R4
	DW	IEVARG
	GHI	R9
	PHI	RF
	GLO	R9
	SMI	04H
	PLO	RF
	ADI	08H
	PLO	R9
	LDI	08H
	PLO	RA
RND2	LDA	RF
	STR	R9
	INC	R9
	DEC	RA
	GLO	RA
	BNZ	RND2
	GLO	R9
	SMI	04H
	PLO	R9
	SEP	R4
	DW	IDIV
	SEP	R4
	DW	IMULT
	INC	RD
	SEP	RD
	DB	LOW IMINUS
	BR	RND5
RND3	GHI	R8
	ORI	04H
	PHI	R8
	DEC	R9
	GLO	RA
	STR	R9
	INC	R9
	ANI	80H
	PLO	RF
	GHI	RA
	STR	R9
	DEC	R9
	DEC	R9
	LDI	80H
	STR	R9
	GLO	RF
	BZ	RND5
	SEP	R4
	DW	DFONE+1
	SEP	R4
	DW	FPAC
	DB	LOW U6 
RND5	INC	R2
	LDXA
	PHI	RA
	LDXA
	PLO	RA
	GHI	R8
	OR
	PHI	R8
	SEP	R5

;......................................................
;	..SIGN FUNCTION -1,0,1
;	..SGN.A
;	.......
SGN	GHI	R8
	STXD
	SEP	R4
	DW	EVARG
	GHI	R8
	ANI	04H
	BZ	SGN1
	LDN	R9
	BZ	SGNZ
	INC	R9
	LDN	R9
	DEC	R9
	ANI	80H
	PLO	RF
	SEP	R4
	DW	DFONE+5
	GLO	RF
	BZ	SGNO
	SEP	R4
	DW	FCOMP
	BR	SGNO
SGN1	INC	RD
	SEP	RD
	DB	LOW ZRCH
	BZ	SGNZ
	LDN	R9
	ANI	80H
	PLO	RF
	SEP	R4
	DW	DFONE+5
	GLO	RF
	BZ	SGNO
	INC	RD
	SEP	RD
	DB	LOW INEG
CONV	GHI	R8
	ANI	04H
	BNZ	CONV1
	LBR	FCONV
CONV1	INC	R2
	LDX
	ANI	08H
	BZ	CONVD
	LDX
	PHI	R8
	SEP	R4
	DW	INT12
CONVD	SEP	R5
SGNO	EQU	CONV
SGNZ	INC	RD
	SEP	RD
	DB	LOW AZRST
	BR	SGNO

;......................................................
;	..FUNCTION TO GENERATE PI..
;	..PI.A
;	......
PI	GLO	R9
	ADI	07H
	PLO	R9
	SEX	R9
	LDI	0CFH
	STXD
	LDI	0FH
	STXD
	LDI	49H
	STXD
	LDI	82H
	STR	R9
	GHI	R8
	ANI	08H
	BNZ	P11
	GHI	R8
	ORI	04H
	PHI	R8
	SEP	R5
P11	LBR	INT12
;......................................................
;	..ROUTINE TO DIMENSION ARRAYS..
;	..DIM.AX
;	...............................
DIM	LDA	RB
	XRI	0D1H
	BZ	DIM1
DIME	SEP	R4
	DW	ERROR
	DB	03H
DIM1	LDA	RB
	STXD
	SEP	R4
	DW	EVIARG
	PLO	RF
	DEC	RF
	LDI	00H
	PHI	R7
	LDI	01H
	PHI	RA
	SEP	R4
	DW	DIM3
	LDN	R9
	PLO	RA
	PLO	R7
	GLO	R9
	SMI	07H
	PLO	R9
	GLO	RF
	BZ	DIM2
	XRI	01H
	BNZ	DIME
	SEP	R4
	DW	DIM3
	LDN	R9
	PHI	RA
	INC	R9
	SEP	R4
	DW	IMULT
	SEP	R4
	DW	DIM3+3
	LDA	R9
	PHI	R7
	LDN	R9
	PLO	R7
	GLO	R9
	SMI	07H
	PLO	R9
DIM2	GLO	R7
	SHL
	PLO	R7
	GHI	R7
	SHLC
	PHI	R7
	BDF	DIME 
	GLO	R7
	SHL
	PLO	R7
	GHI	R7
	SHLC
	PHI	R7
	BDF	DIME
	INC	R7
	INC	R7
	SEP	RD
	DB	92H
	PHI	RC
	LDN	RF
	PLO	RC
	INC	R2
	LDX
	STR	RC
	INC	RC
	GLO	R7
	STR	RC
	INC	RC
	GHI	R7
	STR	RC
	INC	RC
	GHI	RA
	STR	RC
	INC	RC
	GLO	RA
	STR	RC
	DEC	R7
	GLO	R7
	STR	R2
	GLO	RC
	ADD
	PLO	RC
	GHI	R7
	STR	R2
	GHI	RC
	ADC
	PHI	RC
	BDF	DIME
	STR	R2
	GHI	R2
	SD
	BDF	DIME
	GLO	RC
	SEP	RD
	DB	13H
	SEP	RD
	DB	1AH
	GHI	RC
	SEP	RD
	DB	12H
	SEP	RD
	DB	19H
	LDA	RB
	XRI	0C2H
	BZ	DIM
	DEC	RB
	SEP	R5
DIM3	LDA	R9
	BNZ	DIME
	LDA	R9
	BNZ	DIME
	LDA	R9
	BNZ	DIME
	SEP	R5

;......................................................
;	..ASCII VALUE OF STRING..
;	..VALASC.A
;	..........
FVAL	LDI	LOW FVAL1
	LSKP
ASC	LDI	LOW ASC2
	PHI	RF
	GHI	R8
	STXD
	GHI	RF
	STXD
	LDA	RB
	XRI	0D6H
ASCE1	LBNZ	LENE
	SEP	R4
	DW	SEXPR1
	INC	R2
	LDX
	PLO	R3
ASC2	GLO	R9
	ADI	04H
	PLO	R9
	SEP	R4
	DW	ZRST
	PLO	RF
	INC	R9
	LDI	HIGH WP1
	PHI	RF
	LDN	RF
ASC1	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	LDN	R9
	PHI	R8
FVAL2	LDA	RB
	XRI	0C4H
	BNZ	ASCE1
	LBR	CONV
FVAL1	GHI	R8
	ANI	01H
	LBZ	INPUT1-4
	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RB
	DB	00H
	SEP	R4
	DW	LILB
	LDI	0D0H
	PLO	RB
	SEP	RD
	DB	07H
	GHI	RF
	PHI	RB
	SEP	R4
	DW	EXPR
	LDI	0DH
	SEP	RD
	DB	50H
	INC	R2
	SEP	R2
	BR	FVAL2
;......................................................
;	..ROUTINE TO "PEEK"..
;	..PEEK.A
;	.....................
PEEK	GHI	R8
	STXD
	SEP	R4
	DW	IEVARG
PEEK1	STR	R9
	INC	R9
	STR	R9
	INC	R9
	LDA	R9
	PHI	RF
	LDN	R9
	PLO	RF
	LDN	RF
	STR	R9
	DEC	R9
	LDI	00H
	STR	R9
	DEC	R9
	DEC	R9
	LBR	FCONV

;......................................................
;	..OUTPUT A MESSAGE..
;	..MESOUT.A
;	....................

;displays text string until 00h is reached
;string pointed to by R6
;note: R6 holds the return value of the calling routine
MESOUT	LDA	R6
	BZ	MESOT1
	SEP	R4
	DW	OUTPUT
	BR	MESOUT
MESOT1	SEP	R5

;......................................................
;	..STRING VARIABLE EXPRESSION..
;	..SEXPR.AX
;	..............................
SEXPR	GHI	R6
	XRI	HIGH WP1
	BNZ	SEXPR1
	GLO	R6
	XRI	0A2H
	BNZ	SEXPR1
	LBR	SVLET
SEXPR1	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	R7
	GLO	RC
	GLO	RE
	DB	00H
	LDI	HIGH WP1
	PHI	R7
	LDI	00H
	PLO	R7
SEXPR7	LDA	RB
	STR	R2
	XRI	0CFH
	BZ	SEXPR2
	XRI	18H	;..CHK FOR STR VAR (#D7)
	BNZ	SEXPR5
	LDA	RB
	PLO	R8
	SEP	R4
	DW	SVWK
	PLO	RE
	SEP	R4
	DW	SVSRCH
	BNZ	SEXPR3
SEXPRE	SEP	R4
	DW	ERROR
	DB	36H
SEXPE1	SEP	R4
	DW	ERROR
	DB	42H
SEXPR3	GLO	R7
	XRI	80H
	BZ	SEXPE1
	LDA	RA
	STR	R7
	INC	R7
	XRI	0DH
	BNZ	SEXPR3
	BR	SEXPR4
SEXPR2	GLO	R7
	XRI	80H
	BZ	SEXPE1
	LDA	RB
	STR	R7
	INC	R7
	XRI	0CEH
	BNZ	SEXPR2
	DEC	R7
	LDI	0DH
	STR	R7
	INC	R7
SEXPR4	LDN	RB
	XRI	0C8H
	BNZ	SEXPR6
	INC	RB
	DEC	R7
	BR	SEXPR7
SEXPR6	GHI	R6
	XRI	HIGH MID
	BZ	SEXPR8
	INC	R2
	SEP	R2
SEXPR8	SEP	R5
SEXPR5	XRI	6DH	;..CHECK FOR MID$ COMMAND (#BA)
	BNZ	SEXPRE
	SEP	R4
	DW	MID
	BR	SEXPR4
;......................................................
;	..VARIABLE RETRIEVE ROUTINE WITH ARRAYS
;	..VAR.A
;	.......................................
VAR	GHI	R6	;..CHK WHERE YOU CAME FROM
	XRI	HIGH WP1
	BNZ	AV2
	GLO	R6
	XRI	0A2H	;..IS IT FROM EXEC STATEMENT
	BNZ	AV2
	DEC	RB
	LBR	LET 	;..REALLY WANT LET STATE.
AV2	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	R7
	GLO	RC
	DB	00H
	LDA	RB
	PLO	R8	;..SAVE VARIABLE NAME
	SMI	41H
	SHL
	SHL
	PLO	R7
	LDN	RB
	XRI	0D6H	;..IS IT AN ARRAY
	BNZ	AV25
	SEP	R4
	DW	AT13
	BR	AV4
AV25	XRI	04H	;..CHK FOR 'D2'
	BNZ	AV3
	SEP	R4
	DW	GETSV
	BR	AV4
AV3	LDI	HIGH WP2
	PHI	RC
	GLO	R7
	PLO	RC	;..PT AT STOR. AREA
AV4	GLO	R9
	ADI	04H	; WAS ANI	04H
	PLO	R9
	GLO	RC
	PLO	RF
	GHI	RC
	PHI	RF
	INC	RD
	SEP	RD
	DB	LOW CIG
	INC	R2
	SEP	R2
	GLO	R8
	STR	R2
	SEP	RD
	DB	0BCH
	SD
	BM	AV5
	LDI	04H
	BR	AV6
AV5	LDI	08H
AV6	PLO	RF
	GHI	R8
	ANI	0CH
	STR	R2
	BZ	AV8
	GLO	RF
	AND
	BNZ	AV8
	GLO	RF
	XRI	04H
	BZ	AV12
	LBR	FNUMA
AV12	LBR	INT12
AV8	GLO	RF
	STR	R2
	GHI	R8
	OR
	PHI	R8
	SEP	R5

;......................................................
;	..SEARCH FOR DATA STATEMENT..
;	..SHDAT.A
;	.............................
SHDAT	GLO	RB
	STXD
	GHI	RB
	STXD
	GLO	RE
	PLO	RB
	GHI	RE
	PHI	RB
SHDAT2	LDA	RB
	XRI	0FFH
	BZ	SHDATE
	INC	RB
	INC	RB
	LDA	RB
	XRI	9BH
	BZ	SHDAT1
	SEP	R4
	DW	INCNL+3
	BR	SHDAT2
SHDATE	SEP	R4
	DW	ERROR
	DB	20H
SHDAT1	GLO	RB
	PLO	RE
	GHI	RB
	PHI	RE
	INC	R2
	LDXA
	PHI	RB
	LDX
	PLO	RB
	SEP	R5
;......................................................
;	..ROUTINE TO FIND INT PART OF FP NUMBER..
;	..IEXPR.A
;	.........
IEXPR	LDI	LOW INT7
	LSKP
INT	LDI	LOW INT10
INT6	PLO	RF
	GHI	R8
	STXD
	GLO	RF
	PLO	R3
INT10	SEP	R4
	DW	EVARG
	BR	INT8
INT7	SEP	R4
	DW	EXPR
INT8	GHI	R8
	ANI	04H
	BZ	INT9
	SEP	R4
	DW	INT12
INT9	INC	R2
	LDX
	ORI	08H
	PHI	R8
	SEP	R5
INT12	SEP	R4
	DW	FPABS
	STXD
	LDN	R9
	ANI	80H
	BNZ	INT5
	INC	RD
	SEP	RD
	DB	LOW AZRST
	INC	R2
	SEP	R5
INT5	LDN	R9
	SMI	98H
	BPZ	INTE
	LDA	R9
	SDI	98H
	PLO	RF
	LDN	R9
	ORI	80H
	STR	R9
INT2	LDN	R9
	SHR
	STR	R9
	INC	R9
	LDN	R9
	SHRC
	STR	R9
	INC	R9
	LDN	R9
	SHRC
	STR	R9
	DEC	R9
	DEC	R9
	DEC	RF
	GLO	RF
	BNZ	INT2
	DEC	R9
	BNF	INT3
	GLO	R9
	ADI	04H
	PLO	R9
	SEP	R4
	DW	ZRST
	SEP	R4
	DW	DFONE1-8
	INC	RD
	SEP	RD
	DB	LOW IADD
INT3	LDI	00H
	STR	R9
	INC	R2
	LDX
	BZ	INT4
	INC	RD
	SEP	RD
	DB	LOW INEG
INT4	SEP	R5
INTE	SEP	R4
	DW	ERROR
	DB	0FH
;......................................................
;	..ROUTINE FOR USER DEFINE FUNCTION..
;	..USR.A
;	....................................
USCALL	LDI	01H
	LSKP
USR	LDI	00H
	PHI	RF
	GHI	R8
	STXD
	GLO	R8
	STXD
	GLO	RD
	STXD
	GHI	RD
	STXD
	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RA
	GLO	RC
	GLO	RE
	DB	00H
	GHI	RF
	STXD
	SEP	R4
	DW	EVIARG
	INC	R9
	INC	R9
	STR	R2
	SEP	RD
	DB	0A9H
	PHI	RE
	LDI	0A4H
	PLO	RF
	LDI	01H
	XOR
	BZ	USR3
	XRI	03H
	BZ	USR2
	XRI	01H
	BNZ	USRE
	LDA	R9
	PHI	RA
	LDN	R9
	PLO	RA
	GLO	R9
	SMI	05H
	PLO	R9
USR2	LDA	R9
	PHI	R8
	LDN	R9
	PLO	R8
	GLO	R9
	SMI	05H
	PLO	R9
USR3	LDA	R9
	STR	RF
	INC	RF
	LDN	R9
	STR	RF
	SEP	R4
	DB	HIGH WP1
	DB	0A3H
	GLO	R8
	STR	R9
	DEC	R9
	GHI	R8
	STR	R9
	DEC	R9
	GLO	RA
	STR	R9
	DEC	R9
	GHI	RA
	STR	R9
	INC	R2
	LDXA
	BZ	USR4
	GLO	R9
	SMI	004H
	PLO	R9
USR4	SEP	R2
	INC	R2	; WAS INC	RB
	LDXA
	PHI	RD
	LDXA
	PLO	RD
	LDX
	PLO	R8
	LBR	FCONV
USRE	SEP	R4
	DW	ERROR
	DB	24H

;......................................................
;	..STRING VARIABLE WORK FUNCTION..
;	..SVWK.A
;	.................................
SVWK	LDN	RB
	XRI	0D6H
	BNZ	SVWK1
	INC	RB
	GHI	R8
	STXD
	GLO	R8
	STXD
	SEP	R4
	DW	GVAL
	INC	R2
	LDXA
	PLO	R8
	LDX
	PHI	R8
	LDA	RB
	XRI	0C4H
	BNZ	SVWKE
	GLO	RA
	SEP	R5
SVWK1	LDI	00H
	SEP	R5
SVWKE	SEP	R4
	DW	ERROR
	DB	23H
;......................................................
;	..ROUTINE TO ZERO STACK..
;	..ZRST.A
;	.........................
ZRST	LDI	00H
	STR	R9
	INC	R9
	STR	R9
	INC	R9
	STR	R9
	INC	R9
	STR	R9
	DEC	R9
	SEP	R5
;......................................................
;	..HEX NUMBER ROUTINE..
;	..HEX2N.A
;	......................
HEX2N	LDI	00H
	LSKP
HEXN	LDI	01H
HEX1	PLO	RF
	GHI	R8
	STXD
	GLO	R9
	ADI	04H	;..INC STK.
	PLO	R9
	SEP	R4
	DW	ZRST
	GLO	RF
	BNZ	HEX3
	INC	R9
	LDI	01H
	PLO	R8
	BR	AM3
HEX3	LDI	02H
	PLO	R8
AM3	LDA	RB
	SEP	R4
	DW	AM2
	SHL
	SHL
	SHL
	SHL		;..MAKE ROOM FOR
			;..LEAST SIG. BYTE
	PLO	RF
	LDA	RB
	SEP	R4
	DW	AM2
	STR	R2
	GLO	RF
	OR		;..OR WITH MSB
	STR	R9
	INC	R9	;..STR ON STACK
	DEC	R8
	GLO	R8	;..DEC COUNTER
	BNZ	AM3
	DEC	R9
	DEC	R9	;..ADJ STK PTR
	DEC	R9
	DEC	R9
ICONV	INC	R2
	LDX
	PHI	R8
	ANI	04H
	BNZ	ICONV1
	GHI	R8
	ORI	08H
	PHI	R8
	SEP	R5
ICONV1	LBR	FNUMA
AM2	SMI	30H
	STR	R2
	BM	AM1
	SMI	0AH
	BM	AM4
	LDI	10H
	SM
	BPZ	AM1
	LDI	07H
	SD
	STR	R2
	SMI	10H
	BPZ	AM1
AM4	LDN	R2
	SEP	R5
AM1	SEP	R4
	DW	ERROR
	DB	0EH

;......................................................
;	..SEARCH FOR BRANCHES..
;	..RENUMH.A
;	..........
RENUMH	SEP	RD
	DB	81H
	PHI	RA
	LDN	RF
	PLO RA
	LDI	00H
	PLO	RE
	PHI	RE
RENUMC	LDA	RA
	XRI	0FFH
	BZ	RENMHD
	INC	RA
	INC	RA
RENUM5	LDA	RA
	XRI	0D2H
	BZ	RENUM3
	XRI	01H
	BZ	RENUM3	;..CHK FOR FIXED PT
	XRI	67H
	BNZ	RENUM4	;..CHK FOR SEC. CHAR
RENUM3	INC	RA
	INC	RA
	INC	RA
	INC	RA
	BR	RENUM5
RENUM4	XRI	0B9H
	BZ	RENUMC	;..CHK CR
	XRI	8AH
	LSZ		;..CHK GOTO
	XRI	0CH
	LSZ		;..CHK GOSUB
	XRI	25H
	LSZ		;..CHK EXIT (AE)
	XRI	0CH
	LSZ		;..CHK ENINT (A2)
	XRI	0A3H
	BNZ	RENUM5	;..CHK GO CHAR (01)
RENUM6	LDN	RA
	XRI	0D2H
	BZ	RENUM7
	LDN	RA
	XRI	0B4H
	BZ	RENUM7
RENM10	INC	RE
	BR	RENUM5
RENUM7	INC	RA
	INC	RA
	INC	RA
	INC	RA
	INC	RA
	LDN	RA
	XRI	0DH
	BZ	RENM11
	XRI	0C0H
	BNZ	RENM10
	LSKP
RENMHD	LDI	01H
RENM11	DEC	RA
	DEC	RA
	SEP	R5

;......................................................
;	..ROUTINE TO TRACK X-COORDINATE..
;	..OUTPUT.A
;	.................................
OUTPUT	STR	R2
	SEP	RD
	DB	80H
	XOR
	BZ	OUT3
	LDI	0DH
	XOR
	BZ	OUT2
	SEP	RD
	DB	0ADH
	XOR
	BZ	OUT2
	SEP	RD
	DB	0ACH
	XOR
	BZ	OUT4
	LDI	0E0H
	AND
	BZ	OUT2+2
OUT4	SEP	RD
	DB	9BH
	ADI	01H
	BR	OUT2
OUT3	SEP	RD
	DB	9BH
	SMI	01H
	BR	OUT2
OUT2	SEP	RD
	DB	1BH
	LDN	R2
	SEP	R4
	DW	OUTPT1
OUT5	SEP	R5

;......................................................
;	..LOAD DEFAULT ONE FOR FOR..
;	..DFONE.A
;	............................
DFONE	DEC	RB
	GLO	R9
	ADI	04H
	PLO	R9
	SEP	R4
	DW	ZRST
	GHI	R8
	ANI	04H
	BNZ	DFONE1
	INC	R9
	LDI	01H
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	SEP	R5
DFONE1	DEC	R9
	DEC	R9
	LDI	81H
	STR	R9
DFONE2	SEP	R5

;......................................................
;	..EVALUATE TRIG FUNCTIONS..
;	..TRIG.A
;	........
SIN	LDI	LOW SIN1
	LSKP
COS	LDI	LOW COS1
	LSKP
ATAN	LDI	LOW ATAN1
	LSKP
EXP	LDI	LOW EXP1
	LSKP
LN	LDI	LOW LN1
	LSKP
SQRT	LDI	LOW SQRT1
TRIG	PLO	RF
	GHI	R8
	STXD
	GLO	RF
	STXD
	SEP	R4
	DW	EVARG
	GHI	R8
	ANI	08H
	BZ	TRIG1
	SEP	R4
	DW	FNUMA
TRIG1	INC	R2
	LDX
	PLO	R3
SIN1	SEP	R4
	DW	FPACA
	DB	LOW U15
	BR	TRIGD
COS1	SEP	R4
	DW	FPACA
	DB	LOW U16
	BR	TRIGD
ATAN1	SEP	R4
	DW	FPACA
	DB	LOW U17
	BR	TRIGD
EXP1	SEP	R4
	DW	FPACA
	DB	LOW U18
	BR	TRIGD
LN1	SEP	R4
	DW	FPACA
	DB	LOW U19
	BR	TRIGD
SQRT1	SEP	R4
	DW	FPACA
	DB	LOW U20
TRIGD	INC	R2
	LDX
	PHI	R8
	ANI	08H
	BZ	TRIGDD
	LBR	INT12
TRIGDD	LBR	FCONV+10
;......................................................
;	..LOAD STRING VARIABLE..
;	..LDSV.A
;	........................
LDSV	SEP	R4
	DW	SVSRCH
	BZ	LDSV2
	STR	R2
	GLO	RA
	ADD
	PLO	RF
	GHI	RA
	ADCI	00H
	PHI	RF
	DEC	RA
	DEC	RA
	DEC	RA
LDSV4	GHI	RF
	STR	R2
	GHI	RC
	XOR
	BNZ	LDSV1
	GLO	RF
	STR	R2
	GLO	RC
	XOR
	BZ	LDSV2
LDSV1	LDA	RF
	STR	RA
	INC	RA
	BR	LDSV4
LDSV2	PLO	RF
	PLO	RC
	LDI	HIGH WP1
	PHI	RF
LDSV8	INC	RC
	LDA	RF
	XRI	0DH
	BNZ	LDSV8
	PLO	RF
	GLO	R8
	STR	RA
	INC	RA
	GLO	RE
	STR	RA
	INC	RA
	GLO	RC
	STR	RA
	INC	RA
LDSV6	GHI	RA
	STR	R2
	GHI	R2
	XOR
	BZ	LDSVE
	LDA	RF
	STR	RA
	INC	RA
	XRI	0DH
	BNZ	LDSV6
	GLO	RA
	SEP	RD
	DB	1AH
	GHI	RA
	STR	RF
	SEP	R5
LDSVE	SEP	R4
	DW	ERROR
	DB	30H

;......................................................
;	..STRING VARIABLE INPUT..
;	..STVIN.A
;	.........................
STVIN	LDA	RB
	STXD		;..SAVE VARIABLE NAME
	SEP	R4
	DW	SVWK
	STXD
	SEP	R4
	DW	LLB
	LDI	HIGH WP1
	PHI	R7	;..POINT AT BUFFER
	LDI	01H
	PLO	R7
	LDN	R7
	XRI	24H	;..CHECK FOR "$"
	BNZ	BB2
	GLO	RB
	STXD
	GHI	RB
	STXD
	SEP	R4
	DW	LILB
	LDI	0D1H
	PLO	RB
	GHI	R7
	PHI	RB
	LDA	RB
	PLO	R8	;..SAVE VAR NAME
	SEP	R4
	DW	SVWK
	PLO	RE
	SEP	R4
	DW	SVSRCH
	BNZ	BB3
	SEP	R4
	DW	ERROR
	DB	21H
BB3	INC	R2
	LDXA
	PHI	RB
	LDX
	PLO	RB
	LDI	00H
	PLO	R7
BB4	LDA	RA
	STR	R7
	INC	R7
	XRI	0DH	;..IS IT A CR??
	BNZ	BB4
	PLO	R7	;..SET R7 TO 0
BB2	INC	R2
	LDXA
	PLO	RE
	LDX
	PLO	R8
	LBR	LDSV	;..LOAD VAR IN MEM
;......................................................
;	..ROUTINE TO FIX DEC. POINT
;	..FIXED.A
;	...........................
FIXED	LDN	RB
	XRI	0DH
	BZ	BM3
	XRI	0C0H
	BZ	BM3	;..CHK FOR #CD (:)
	SEP	R4
	DW	GVAL
	GLO	RA
	SMI	06H
	BDF	BM3	;..IF>6 ASSUME NO FIXED WANTED
	GLO	RA
BM4	SEP	RD
	DB	3DH
	SEP	R5
BM3	LDI	0FFH	;..IF NO FIXED...LOAD FF
	BR	BM4

;......................................................
;	..FIND FLOATING PT OF INT NUMBER..
;	..FNUM.A
;	........
FNUM	GHI	R8
	STXD
	SEP	R4
	DW	IEVARG
	INC	R2
	LDX
	ORI	04H
	PHI	R8
FNUMA	INC	RD
	SEP	RD
	DB	LOW ZRCH
	BZ	FNUM5
	GLO	RC
	STXD
	GHI	RC
	STXD
	SEP	R4
	DW	IABS
	SHR
	SHRC
	STR	R2
	LDI	0A0H
	PLO	RF
FNUM4	LDN	R9
	ANI	80H
	BNZ	FNUM3
	DEC	RF
	INC	RD
	SEP	RD
	DB	39H	;MOST LIKELY AN UNDEFINED LABEL
	BR	FNUM4
;	DW	3930H	;NO IDEA ON THESE 2 LINES
;	INC	RC	;BUT IT MATCHES THE CODE

;	PLO	RF
FNUM3	INC	R9
	INC	R9
	LDA	R9
	STR	R9
	DEC	R9
	DEC	R9
	LDA	R9
	STR	R9
	DEC	R9
	DEC	R9
	LDA	R9
	ANI	7FH
	OR
	STR	R9
	DEC	R9
	GLO	RF
	STR	R9
	INC	R2
	LDXA
	PHI	RC
	LDX
	PLO	RC
FNUM5	SEP	R5
;......................................................
;	..SUBSCRIPTED VARIABLE SEARCH
;	..SUBSCH.A
;	..........
SUBSCH	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RE
	DB	00H
	SEP	RD
	DB	84H
	ADI	09H
	PLO	RC
	DEC	RF
	DEC	RF
	LDN	RF
	ADCI	00H
	PHI	RC
	SEP	RD
	DB	94H
	PHI	RE
	LDN	RF
	PLO	RE
SUBS5	GLO	RC
	STR	R2
	GLO	RE
	XOR
	BNZ	SUBS1
	GHI	RC
	STR	R2
	GHI	RE
	XOR
	BZ	SUBS2
SUBS1	GLO	R8
	STR	R2
	LDA	RC
	XOR
	BNZ	SUBS3
	GHI	R7
	STR	R2
	LDN	RC
	XOR
	BZ	SUBS4
SUBS3	INC	RC
	INC	RC
	INC	RC
	INC	RC
	INC	RC
	BR	SUBS5
SUBS4	INC	RC
	LDI	01H
SUBS2	PLO	RF
	INC	R2
	SEP	R2
	GLO	RF
	SEP	R5

;......................................................
;	..NEXT ROUTINE
;	..NEXT.A
;	........
NEXT	LDN	RB
	XRI	0D1H
	BNZ	NEXT1
	INC	RB
	LDA	RB
	PLO	R8
	LDN	RB
	XRI	0D2H
	BZ	NEXT2
	DEC	RB
	LDI	HIGH WP2
	PHI	RC
	LDA	RB
	SMI	041H
	SHL
	SHL
	PLO	RC
	BR	NEXT3
NEXT2	SEP	R4
	DW	GETSV
NEXT3	GLO	R2
	ADI	13H
	PLO	RA
	GHI	R2
	ADCI	00H
	PHI	RA
	LDA	RA
	STR	R2
	GHI	RC
	XOR
	BNZ	NEXTE
	LDN	RA
	STR	R2
	GLO	RC
	XOR
	BNZ	NEXTE
NEXT1	INC	R2
	INC	R2
	INC	R2
	LDXA
	PLO	R6
	LDX
	PHI	R6
	SEP	R5
NEXTE	SEP	R4
	DW	ERROR
	DB	1AH
RETURN	EQU	NEXT1

;......................................................
;	..ROUTINE FOR GOSUB AND RETURN..
;	..GOSUB.A
;	................................
GOSUB	LDN	RB
	XRI	0B4H
	BNZ	GOSUB1
	INC	RB
	LDA	RB
	PHI	RF
	LDA	RB
	PLO	RF
	INC	RB
	INC	RB
	GLO	RB
	STXD
	GHI	RB
	STXD
	GLO	RF
	PLO	RB
	GHI	RF
	PHI	RB
	BR	GOSUB2
GOSUB1	SEP	R4
	DW	IGVAL
	GLO	RB
	STXD
	GHI	RB
	STXD
	SEP	R4
	DW	GOTO1+3
GOSUB2	SEP	R4
	DW	MEM
	BZ	GOSUB4
	SEP	R4
	DW	ERROR
	DB	0DH
GOSUB4	SEP	R4
	DW	K7
	GHI	R8
	ORI	01H
	PHI	R8
	INC	R2
	LDXA
	PHI	RB
	LDXA
	PLO	RB
	INC	R2
	LDX
	PHI	R6
	LDI	LOW K3
GOSUB3	PLO	R6
	SEP	R5
;......................................................
;	..ROUTINE TO HANDLE PARENTHESIS..
;	..OPPAR.A
;	.................................
OPPAR	SEP	R4
	DW	EXPRP
	LDA	RB
	XRI	0C4H	;..CHK FOR CLOSE PARENS
	BZ	BC2
	SEP	R4
	DW	ERROR
	DB	1CH
BC2	SEP	R5
;......................................................
;	..HELP FOR ELILB
;	..HELILB.A
;	..........
C34	LDN	R9
	XRI	0FFH
	BZ	C35
	LDA	R9
	PLO	R8
	ANI	1FH
	PLO	RF
C16	LDA	RC
	STR	R2
	XRI	20H
	BZ	C16
	LDA	R9
	ANI	7FH
	XOR
	DEC	RF
	BZ	C17
	GHI	RA
	PHI	RC
	GLO	RA
	PLO	RC
	GLO	RF
	STR	R2
	GLO	R9
	ADD
	PLO	R9
	GHI	R9
	ADCI	00H
	PHI	R9
	BR	C34
C17	DEC	R9
	LDA	R9
	ANI	80H
	BZ	C16
	LDA	R9
	STR	R7
	INC	R7
	GLO	R8
	ANI	80H
	BNZ	C30
	LDI	LOW C2
	LSKP
C30	LDI	LOW C9+1
	PLO	R6
C35	SEP	R5
;......................................................
;	..ROUTINE TO SEARCH FOR LINE NUMBER
;	..LNUMS.A
;	...................................
LNUMS	SEP	RD
	DB	81H
	PHI	RB	;..LOAD LOW US.
	LDN	RF
	PLO	RB
LNUMS5	GHI	RB
	STR	R2
	SEP	RD
	DB	83H
	XOR
	BNZ	LNUMS1
	GLO	RB
	STR	R2
	LDN	RF
	XOR
	BZ	LNUMS2
LNUMS1	LDA	RB
	STR	R2
	SEP	RD
	DB	85H
	XOR
	BNZ	LNUMS3
	LDN	RB
	STR	R2
	LDN	RF
	XOR
	BZ	LNUMS4
LNUMS3	DEC	RB
	INC	RB
	INC	RB
	LDN	RB
	STR	R2
	GLO	RB
	ADD
	PLO	RB
	GHI	RB
	ADCI	00H
	PHI	RB
	BR	LNUMS5
LNUMS2	SEP	RD
	DB	81H
	PHI	RB
	LDN	RF
	PLO	RB
	LDI	00H
	SEP	R5
LNUMS4	DEC	RB
	LDI	01H
	SEP	R5

;......................................................
;	..INPUT ROUTINE..
;	..INPUT.A
;	.................
INPUT	GHI	R8
	ANI	01H
	BNZ	INPUT1
	SEP	R4
	DW	ERROR
	DB	11H
INPUT1	LDN	RB
	XRI	0CFH
	BNZ	AE3
	SEP	R4
	DW	PCHARS
AE3	SEP	RD
	DB	87H
	PLO	RE
	GHI	RF
	PHI	RE
	LDN	RB
	XRI	0D7H
	BNZ	AE2
	INC	RB
	LBR	STVIN
AE2	LDN	RE
	XRI	0DH
	BNZ	AE1
	SEP	R4
	DW	A8
	LDI	0D0H
	PLO	RE
AE1	SEP	R4
	DW	INRD
	LDA	RB
	STR	R2
	XRI	0C2H
	BZ	AE2
	DEC	RB
	GLO	RE
	SEP	RD
	DB	07H
	SEP	R5
;......................................................
;	..MEMORY LEFT STATEMENT.
;	..MEM.A
;	........................
MEM	GLO	R9
	ADI	04H
	PLO	R9
	SEP	R4
	DW	ZRST
	GHI	R2
	SMI	01H
	STR	R9
	INC	R9
	GLO	R2
	STR	R9
	INC	R9
	SEP	R4
	DW	ZRST
	INC	R9
	SEP	RD
	DB	9AH
	STR	R9
	DEC	R9
	SEP	RD
	GHI	R9
	STR	R9
	DEC	R9
	DEC	R9
	INC	RD
	SEP	RD
	DB	LOW IMINUS
	LDN	R9
	ANI	80H
	PHI	RF
	GHI	R6
	XRI	HIGH WP1
	BNZ	MEM1
	GHI	R8
	LBR	FCONV+3
MEM1	GLO	R9
	SMI	04H
	PLO	R9
	GHI	RF
	SEP	R5

;......................................................
;	..NUMBER OR LETTER ROUTINE..
;	..NORLET.A
;	............................
NORLET	LDN	RC
	SMI	30H
	BPZ	D3
D7	SEP	R4
	DW	ERROR
	DB	1BH
D3	SMI	0AH
	BPZ	D4
	LDI	00H
	SEP	R5
D4	SMI	07H
	BM	D7
	SMI	1AH
	BPZ	D7
	LDI	01H
	SEP	R5
;......................................................
;	..REMARK ROUTINE..
;	..REM.A
;	..................
REM	LDA	RB
	XRI	0DH	;..CHK FOR CR
	BNZ	REM
	DEC	RB
	SEP	R5
;......................................................
;	..STRING LENGTH COMMAND..
;	..LEN.A
;	.........................
LENE	SEP	R4
	DW	ERROR
	DB	01H
LEN	GHI	R8
	STXD
	LDA	RB
	XRI	0D6H
	BNZ	LENE
	LDA	RB
	XRI	0D7H
	BNZ	LENE
;	LDA	RB
;	XRI	0D6H
;	BNZ	LENE
	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RE
	GLO	RC
	DB	00H
	LDA	RB
	PLO	R8
	SEP	R4
	DW	SVWK
	PLO	RE
	SEP	R4
	DW	SVSRCH
	LBZ	SEXPRE
	PLO	RF
	INC	R2
	SEP	R2
	GLO	R9
	ADI	04H
	PLO	R9
	SEP	R4
	DW	ZRST
	INC	R9
	DEC	RF
	GLO	RF
	LBR	ASC1
;......................................................
;	..NUMBER RETRIEVE ROUTINE
;	..FPN.A
;	.......
FPN	GLO	R9
	ADI	04H
	PLO	R9
	DEC	RB
	LDA	RB
	STR	R2
	GLO	RB
	PLO	RF
	GHI	RB
	PHI	RF
	INC	RD
	SEP	RD
	DB	LOW CIG
	INC	RB
	INC	RB
	INC	RB
	INC	RB
	GHI	R8
	ANI	08H
	BNZ	FPN4
	LDI	0D3H
	XOR
	BZ	FPN1
	LBR	FCONV+7
FPN1	GHI	R8
	ORI	04H
	PHI	R8
FPN5	SEP	R5
FPN4	LDI	0D2H
	XOR	
	BZ	FPN5
	SEP	R4
	DW	INT12
	GHI	R8
	ORI	08H
	PHI 	R8
	SEP	R5
FIXN	EQU	FPN

;......................................................
;	..ROUTINE TO PRINT INTEGER NUMBER
;	..PINTN.A
;	.................................
PINTN	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RC
	GLO	RE
	DB	00H
	LDI	HIGH WP1
	PHI	RC	;..PT. AT WK PAGE
	GHI	R9
	PHI	RE
	LDI	0E0H
	PLO	RE
	LDI	0C0H
	PLO	RC	;..AT DEC NUMBER BUFFER
	LDA	RC
	XRI	0DH	;..CHK FRO MINUS SIGN
	BNZ	AA2
	LDI	2DH
	SEP	R4
	DW	TNUM
AA2	GLO	RC
	XRI	0CBH	;..IS IT LAST DIGIT YET??
	BZ	AA3		;..IF IT IS GO PRINT IT EVEN IF
			;..IT'S A ZERO
	LDA	RC
	BZ	AA2		;..IGNORE LEADING ZEROES
	DEC	RC
AA3	LDA	RC		;..GET DIGIT
	SEP	R4
	DW	ATNUM
	GLO	RC
	XRI	0CCH	;..CHK FOR END OF BUF
	BNZ	AA3
	LBR	PFPAD2
;......................................................
;	..STRING VARIABLE SEARCH..
;	..SVSRCH.A
;	..........................
SVSRCH	SEP	RD
	DB	92H
	PHI	RA
	LDA	RF
	PLO	RA
	SEP	RD
	DB	99H
	PHI	RC
	LDA	RF
	PLO	RC
SVSCH1	GHI	RA
	STR	R2
	GHI	RC
	XOR
	BNZ	SVSCH2
	GLO	RA
	STR	R2
	GLO	RC
	XOR
	BZ	SVSCH3
SVSCH2	SEX	RA
	GLO	R8
	XOR
	INC	RA
	BNZ	SVSCH4
	GLO	RE
	XOR
SVSCH4	INC	RA
	BZ	SVSCH5
	GLO	RA
	ADD
	PLO	RA
	GHI	RA
	ADCI	00H
	PHI	RA
	INC	RA
	SEX	R2
	BR	SVSCH1
SVSCH5	LDA	RA
SVSCH3	SEP	R5
;......................................................
;	..ROUTING FOR "POKE"
;	..POKE.A
;	....................
POKE	SEP	R4
	DW	EVIARG
	XRI	02H	;..SHOULD BE TWO ARGUMENTS
	BZ	BS2
	SEP	R4
	DW	ERROR
	DB	1DH
BS2	INC	R9
	INC	R9
	INC	R9
	LDN	R9
	STR	R2	;..SAVE LOW BYTE
	GLO	R9
	SMI	05H
	PLO	R9
	LDA	R9
	PHI	RF
	LDN	R9
	PLO	RF
	LDN	R2
	STR	RF
	GLO	R9
	SMI	07H
	PLO	R9
	SEP	R5
;......................................................
;	..TRACE ROUTINE..
;	..TRACE.A
;	.................
TRACE	SEP	R4
	DW	GVAL
	GLO	RA
	BNZ	TRACEE
	GHI	RA
	BNZ	TRACEE
	GHI	R8
	ANI	0BFH
	PHI	R8
	SEP	R5
TRACEE	GHI	R8
	ORI	40H
	PHI	R8
	SEP	R5

;......................................................
;	..EXTENDED CLS COMMAND..
;	..INIT.A
;	........................
INIT	SEP	RD
	DB	0ADH
CLSD	SEP	R4
	DW	OUTPUT
	SEP	RD
	DB	0CDH
	PHI	RF
CLSD1	DEC	RF
	GHI	RF
	BNZ	CLSD1
	SEP	R5
;......................................................
;	..INITIALIZE FLAGS AND ARRAY PTRS
;	..RSTFLG.A
;	.................................
RSTFLG	GHI	R8
	ANI	80H
	PHI	R8
	LDI	HIGH AU3
	PHI	RA
	LDI	LOW AU3
	PLO	RA
	LDI	HIGH INTPT
	PHI	R1
	LDI	LOW INTPT
	PLO	R1
	LDI	HIGH WP1
	PHI	RF
	LDI	9DH
	PLO	RF
AU2	LDA	RA
	INC	RF
	STR	RF
	GLO	RF
	XRI	0A6H
	BNZ	AU2
AU4	SEP	RD
	DB	18H
	SEP	R5
AU3	DB	HIGH LOWUS
	DB	0D4H
	DW	0000H
	DB	0D5H
	DB	0D4H
	DW	0000H
	DB	0D5H

;......................................................
;	..ROUTINE TO EXECUTE GOTO..
;	..GOTO.A
;	...........................
GOTO	LDN	RB
	XRI	0B4H
	BNZ	GOTO2
	INC	RB
	LDA	RB
	PHI	RF
	LDN	RB
	PLO	RB
	GHI	RF
	PHI	RB
GOTO3	INC	R2
	INC	R2
	LDX
	PHI	R6
	LDI	LOW K5
	PLO	R6
AC5	SEP	R5
GOTO2	SEP	R4
	DW	GOTO1
	BR	GOTO3
GOTO1	SEP	R4
	DW	IGVAL
	GLO	RA
	SEP	RD
	DB	06H
	GHI	RA
	STR	RF
AC4	SEP	R4
	DW	LNUMS
	BNZ	AC5
	SEP	R4
	DW	ERROR
	DB	2EH
;......................................................
;	..EVALUATE INTEGER ARGUMENT..
;	..EVIARG.A
;	.............................
EVIARG	GLO	RC
	STXD
	LDI	00H
	PLO	RC
	LDA	RB
	XRI	0D6H	;..CHK FOR OPEN PARENS
	BZ	BP2
BP3	SEP	R4
	DW	ERROR
	DB	05H
BP2	SEP	R4
	DW	IEXPR
	LDA	RB
	XRI	0C2H
	INC	RC	;..IS THERE ANOTHER
			;..ARGUMENT?? IF SO COMMA SHOULD BE!
	BZ	BP2
	DEC	RB
	LDA	RB
	XRI	0C4H	;..CHK FOR CLOSE PARENS
	BNZ	BP3
	GLO	RC	;..GET NUMBER OF ARGUMENTS PRES.
	PLO	RF
	INC	R2
	LDX
	PLO	RC
	GLO	RF
	SEP	R5
;......................................................
;	..EVAL. INT EXPR. AND LOAD IN RA..
;	..GVAL.A
;	..................................
IGVAL	GHI	R8
	ORI	08H
	PHI	R8
	SEP	R4
	DW	EXPRP
	BR	GVAL+3
GVAL	SEP	R4
	DW	IEXPR
	LDA	R9
	BNZ	GVALE
	LDA	R9
	BNZ	GVALE
	LDA	R9
	PHI	RA
	LDN	R9
	PLO	RA
	GLO	R9
	SMI	07H
	PLO	R9
	SEP	R5
GVALE	SEP	R4
	DW	ERROR
	DB	06H


;......................................................
;	..GET SUBSCRIPTED VARIABLE VALUE
;	..GETSV.A
;	.........
GETSV	GHI	R7
	STXD
	INC	RB
	LDA	RB
	BNZ	GETSVE
	LDA	RB
	BNZ	GETSVE
	LDA	RB
	BZ	GETSV1
GETSVE	SEP	R4
	DW	ERROR
	DB	3BH
GETSV1	LDA	RB
	PHI	R7
	SEP	R4
	DW	SUBSCH
	BZ	GETSV3
	INC	R2
	LDX
	PHI	R7
	SEP	R5
GETSV3	SEP	R4
	DW	ERROR
	DB	41H
;......................................................
;	..PRINT END OF DATA..
;	..EAODEOM.A
;	.....................
EOM	SEP	RD
	DB	083H
	LSKP
EOD	SEP	RD
	DB	099H
	PHI	RA
	LDN	RF
	PLO	RA
	SEP	R4
	DW	MESOUT
	DB	40H,00H
	GHI	RA
	SEP	R4
	DW	EOM1
	GLO	RA
	SEP	R4
	DW	EOM1
	LDI	20H
	LBR	OUTPUT
;......................................................
;	..INTERRUPT ROUTINE..
;	..INTPT.A
;	.....................
INTRET	DIS
INTPT	DEC	R2
	SAV
	DEC	R2
	STXD
	GHI	RD
	STXD
	GLO	RD
	STR	R2
	LDI	HIGH WP1
	PHI	RD
	LDI	98H
	PLO	RD
	STR	RD
	LDXA
	PLO	RD
	LDXA
	PHI	RD
	LDXA
	BR	INTRET

;......................................................
;	..STANDARD CALL PROGRAM..
;	..STCALL.A
;	.........................
	SEP	R3
STCALL	PHI	RF
	SEX	R2
	GHI	R6
	STXD
	GLO	R6
	STXD
	GLO	R3
	PLO	R6
	GHI	R3
	PHI	R6
	LDA	R6
	PHI	R3
	LDA	R6
	PLO	R3
	GHI	RF
	BR	STCALL-1
;......................................................
;	..GENERATE CR/LF PAIR..
;	..CRLF.A
;	........
CRLF	SEP	R4
	DW	MESOUT
	DB	0DH,0AH,00H
	SEP	R5
;......................................................
;	..CHECK AND CONVERT TO FP..
;	..FCONV.A
;	...........................
FCONV	INC	R2
	LDX
	PHI	R8
	ANI	08H
	BNZ	FCONV1
	SEP	R4
	DW	FNUMA
	GHI	R8
	ORI	04H
	PHI	R8
FCONV1	SEP	R5
;......................................................
;	..BINARY TO ASCII..
;	..EOM2.A
;	...................
EOM2	STR	R2
	SMI	0AH
	LDN	R2
	BM	EOM3
	ADI	37H
	BR	EOM4
EOM3	ADI	30H
EOM4	LBR	OUTPUT
;......................................................
;	..HELP FOR BINARY TO ASCII..
;	..EOM1.A
;	............................
EOM1	STXD
	SHR
	SHR
	SHR
	SHR
	SEP	R4
	DW	EOM2
	INC	R2
	LDX
	ANI	0FH
	LBR	EOM2
;......................................................
;	..DISABLE INTERRUPTS..
;	..DISINT.A
;	......................
DISINT	DEC	R2
	LDI	23H
	STR	R2
	DIS
	SEP	R5
;......................................................
;	..ROUTINE TO EXECUTE CONDITIONAL INST.
;	..IF.AX
;	......................................
IF	LDN	RB
	XRI	0D7H	;..IS IT A STRING VAR??
	BZ	AD15
	XRI	6DH	;..CHK FRO #BA (MID$)
	BNZ	AD11
AD15	SEP	R4
	DW	SEXPR1
	LDA	RB
	XRI	0CCH
	PLO	RE
	BZ	IFSTR6
	XRI	73H
	BNZ	IFSTRE
IFSTR6	PLO	R7
	LDI	HIGH WP1
	PHI	R7
IFSTR1	LDA	R7
	STXD
	XRI	0DH
	BNZ	IFSTR1
	DEC	R7
	GLO	R7
	STXD
	SEP	R4
	DW	SEXPR1
	INC	R2
	LDXA
	PLO	R7
IFSTR4	LDN	R7
	XOR
	BNZ	IFSTR2
	GLO	R7
	BZ	IFSTR3
	DEC	R7
	INC	R2
	BR	IFSTR4
IFSTR2	GLO	R7
	BZ	IFSTR5
	DEC	R7
	INC	R2
	BR	IFSTR2
IFSTR5	GLO	RE
	BR	AD12+1
IFSTR3	GLO	RE
	BR	AD14+1
IFSTRE	SEP	R4
	DW	ERROR
	DB	2FH
AD11	SEP	R4
	DW	EXPR
	LDA	RB
	STXD		;..SAVE COND. ON STACK
	SEP	R4
	DW	EXPRP
IFPIF	GHI	R8
	ANI	04H
	BZ	IFPIF1
	SEP	R4
	DW	FPAC
	DB	LOW U7
	LDA	R9
	PHI	RF
	LDN	R9
	ANI	80H
	PLO	RF
	BR	IFPIF2
IFPIF1	INC	RD
	SEP	RD
	DB	LOW IMINUS
	LDN	R9
	ANI	80H
	PLO	RF
	INC	RD
	SEP	RD
	DB	LOW ZRCH
	PHI	RF
	INC	R9
IFPIF2	GLO	R9
	SMI	05H
	PLO	R9	;..ADJUST STK PTR
	INC	R2
	LDX
	XRI	0C7H	;..CHECK >
	BZ	AD4
	XRI	01H	;..CHK FOR #C6 (<)
	BZ	AD5
	XRI	0AH	;..CHK FOR #CC (=)
	BZ	AD6
	XRI	73H	;..CHK FOR #BF (<>)
	BZ	AD12
	XRI	01H	;..CHK FOR #BE (<=)
	BZ	AD13
	XRI	03H	;..CHK FOR #BD (>=)
	BZ	AD14
	SEP	R4
	DW	ERROR
	DB	10H
AD6	GHI	RF
	BZ	AD7	;..IF ANSWER=0 BRANCH
	BR	AD8
AD4	GHI	RF
	BZ	AD8
	GLO	RF	;..GET MINUS OR PLUS
	BR	AD6+1
AD5	GHI	RF
	BZ	AD8
	GLO	RF
	BZ	AD8
	BR	AD7
AD12	GHI	RF
	BNZ	AD7
	BR	AD8
AD13	GLO	RF
	BNZ	AD7
	GHI	RF
;	BR	AD8
	BR	AD6+1
AD14	GLO	RF
	BNZ	AD8
AD7	LDN	RB
	XRI	0C5H	;..CHK FOR "THEN"
	BNZ	AD9
	INC	RB	;..IGNORE THEN STATEMENT
AD9	LDI	LOW K2
AD10	PLO	R6
	INC	R2
	INC	R2
	LDX
	PHI	R6
	SEP	R5
AD8	DEC	RB
	DEC	RB
	SEP	R4
	DW	INCNL+1
	LDI	LOW K5
	BR	AD10

;......................................................
;	..FLOATING POINT COMPLIMENT..
;	..FCOMP.A
;	.........
FCOMP	LDA	R9
	BZ	FCOMP1
	LDN	R9
	XRI	80H
	STR	R9
FCOMP1	DEC	R9
	SEP	R5

;......................................................
;	..CATALOG FUNCTIONS VIA SEP DATA..
;	..DDOUT.A
;	.........
DDDONE	SEX	R2
DDDONF	SEP	R3
DDROUT	LSKP
	LDA	R3
	PLO	RD
	DEC	R2
	STR	R2
	LDI	HIGH WP1
	PHI	RF
	LDA	R3
	PLO	RF
	ANI	80H
	BZ	DDRT1
	LDA	RF
	INC	R2
	BR	DDDONF
DDRT1	GLO	RF
	ADI	80H
	PLO	RF
	LDA	R2
	STR	RF
	DEC	RF
	BR	DDDONF
REGSV	LBR	REGSVA
IADD	SEX	R9
	DEC	R9
	GLO	R9
	ADI	04H
	PLO	RF
	GHI	R9
	PHI	RF
	LDN	RF
	ADD
	STXD
	DEC	RF
	LDN	RF
	ADC
	STXD
	DEC	RF
	LDN	RF
	ADC
	STXD
	DEC	RF
	LDN	RF
	ADC
	STR	R9
	BR	DDDONE
SHLOB	SEX	R9
	INC	R9
	INC	R9
	INC	R9
	LDN	R9
	SHL
	STXD
	LDN	R9
	SHLC
	STXD
	LDN	R9
	SHLC
	STXD
	LDN	R9
	SHLC
	STR	R9
	BR	DDDONE
INEG	SEX	R9
	INC	R9
	INC	R9
	INC	R9
	LDN	R9
	SDI	00H
	STXD
	LDN	R9
	SDBI	00H
	STXD
	LDN	R9
	SDBI	00H
	STXD
	LDN	R9
	SDBI	00H
	STR	R9
	BR	DDDONE
IMINUS	SEX	R9
	DEC	R9
	GLO	R9
	ADI	04H
	PLO	RF
	GHI	R9
	PHI	RF
	LDN	RF
	SD
	STXD
	DEC	RF
	LDN	RF
	SDB
	STXD
	DEC	RF
	LDN	RF
	SDB
	STXD
	DEC	RF
	LDN	RF
	SDB
	STR	R9
	BR	DDDONE
SHROB	LDN	R9
	SHR
	STR	R9
	INC	R9
	LDN	R9
	SHRC
	STR	R9
	INC	R9
	LDN	R9
	SHRC
	STR	R9
	INC	R9
	LDN	R9
	SHRC
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	BR	DDDONF
AZRST	SEX	R9
	INC	R9
	INC	R9
	INC	R9
	LDI	00H
	STXD
	STXD
	STXD
	STR	R9
	BR	DDDONE
SHLOBY	SEX	R2
	DEC	R2
	INC	R9
	LDA	R9
	STXD
	LDA	R9
	STXD
	LDN	R9
	STR	R2
	SEX	R9
	LDI	00H
	STXD
	LDA	R2
	STXD
	LDA	R2
	STXD
	LDA	R2
	STR	R9
	BR	DDDONE
ARFRF	SEX	R9
	INC	R9
	INC	R9
	INC R9
	GHI	RF
	ADD
	STXD
	LDN	R9
	ADCI	00H
	STXD
	LDN	R9
	ADCI	00H
	STXD
	LDN	R9
	ADCI	00H
	STR	R9
	BR	DDDONE
ZRCH	LBR	ZRCHA


G1FS	LBR	G1FSA
G2FS	LBR	G2FSA
GIS	LBR	GISA
P1FS	LBR	P1FSA
P2FS	LBR	P2FSA
PIS	LBR	PISA
CFP1	LBR	CFP1A
CPFP1	LBR	CPFP1A
CFP2	LBR	CFP2A
CPFP2	LBR	CPFP2A
CF12	LBR	CF12A
CI12	LBR	CI12A
CIG	LBR	CIGA
CMEX	LBR	CMEXA
CSP1	LBR	CSP1A
CSP2	LBR	CSP2A
CSP3	LBR	CSP3A

;......................................................
;	..FIXED OR FLOATING POINT?..
;	..FIORFP.A
;	..........
FIORFP	GLO	RA
	STXD
	PLO	RC
	LDI	HIGH WP2
	PHI	R9
	PHI	RF
	LDI	70H
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW AZRST
	PLO	R8
	PLO	RA
	PHI	RA
	LSKP
FFP1	INC	R8
FFP6	INC	RC
	LDN	RC
	SMI	2EH
	BZ	FFP1
	LSNF
	SMI	02H
	BM	FFP2
	STR	R2
	SMI	0AH
	BPZ	FFP3
	GLO	R8
	BZ	FFP4
	INC	RA
FFP4	LDN	R9
	ANI	0F0H
	BNZ	FFP5
	INC	RD
	SEP	RD
	DB	LOW CI12
	INC	RD
	SEP	RD
	DB	LOW SHLOB
	INC	RD
	SEP	RD
	DB	LOW SHLOB
	INC	RD
	SEP	RD
	DB	LOW IADD
	INC	RD
	SEP	RD
	DB	LOW SHLOB
	LDN	R2
	PHI	RF
	INC	RD
	SEP	RD
	DB	LOW ARFRF
	BPZ	FFP5
	LDN	R9
	SHL
	BM	FFP6
FFP5	SEP	R4
	DW	ERROR
	DB	2CH
FFP3	SMI	0BH
	BNZ	FFP2
	INC	RC
	LDN	RC
	SMI	2BH
	BZ	FFP7
	LSNF
	SMI	02H
	BM	FFP8
	BZ	FFP9
	SMI	03H
	BM	FFP8

	STR	R2
	SMI	0AH
	BM	FFP10
FFP8	DEC	RC
FFP2	GLO	R8
	LBZ	FFPAP
	DEC	R8
	GLO	R8
	BZ	FFP11
FFP12	SEP	R4
	DW	ERROR
	DB	2DH
FFP9	LDI	80H
	PHI	RA
FFP7	INC	RC
	LDN	RC
	SMI	30H
	BM	FFP12
	STR	R2
	SMI	0AH
	BPZ	FFP12
FFP10	INC	RC
	LDN	RC
	SMI	30H
	BM	FFP13
	PLO	RF
	SMI	0AH
	BPZ	FFP13
	INC	RC
	LDN	R2
	SHL
	SHL
	ADD
	SHL
	STR	R2
	GLO	RF
	ADD
FFP11	STR	R2
FFP13	GHI	RA
	BNZ	FFP14
	LDI	80H
	ADD
	SKP
FFP14	SM
	STR	R2
	GLO	RA
	SD
	PLO	RA
	GHI	R8
	SEP	RD
	DB	34H
	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	R7
	GLO	R8
	GLO	RC
	DB	00H
	PHI	R7
	PHI	R8
	PHI	RC
	LDI	9FH
	PLO	RC
	SEP	R4
	DW	NMFP
	GLO	RC
	BZ	FFP15
	GLO	RA
	SHL
	BZ	FFP19
	BPZ	FFP16
	GLO	R9
	ADI	04H
	PLO	R9
	GLO	RC
	STXD
	INC	RD
	SEP	RD
	DB	LOW AZRST
	LDI	84H
	PLO	RC
	LDI	50H
	STR	R9
FFP18	INC	RA
	GLO	RA
	XRI	80H
	BZ	FFP17
	SEP	R4
	DW	SMULT
	BR	FFP18
FFP17	GLO	RC
	PLO	R8
	INC	R2
	LDN	R2
	PLO	RC
	SEP	R4
	DW	FDIV
	BR	FFP19
FFP16	GLO	RA
	XRI	80H
	BZ	FFP19
	SEP	R4
	DW	SMULT
	DEC	RA
	BR	FFP16
FFP19	SEP	R4
	DW	FPACR
FFP15	INC	R2
	SEP	R2
	INC	R2
	LBR	FFPO
;......................................................
;	..DEFINE USER SPACE..
;	..DEFUS.A
;	.....................
DEFUS	SEP	R4
	DW	IEXPR
	LDI	00H	;..POINT AT DEFUS BYTE
	PLO	RC
	INC	R9
	INC	R9
	LDN	R9
	SEP	RD
	DB	01H
	STR	R2
	SEP	RD
	DB	9EH	;..START OF RAM SPACE
	PHI	RC	;..PLACE DEFUS BYTE HERE
	SD		;..CALC # PAGES
	STR	RC	;..SET DEFUS BYTE
	LBR	CLEAR
;......................................................
;	..PRINT ROUTINE..
;	..PRINT.AX
;	.................
PRINT	LDN	RB
	XRI	0DH
	BZ	PRINTE
	XRI	0C0H
	BZ	PRINTE	;..CHK FOR #CD
	GHI	R8
	ANI	0DFH
	PHI	R8
	LDN	RB
	XRI	0D7H
	BZ	PRINT9
	XRI	6DH
	BZ	PRINT9	;..CHK FOR #BA (MID$)
	XRI	03H
	BZ	PRINTA	;..CHK FOR #B9 (CHR$)
	XRI	76H
	BZ	PRINT8	;..CHK FOR #CF (QUOTES)
	XRI	73H
	BNZ	PRINT1	;..CHK FOR #BC (TAB)
TAB	INC	RB
	SEP	R4
	DW	IEVARG
	SEP	R4
	DW	GVAL+3
	GLO	RA
	ANI	7FH
TAB5	PLO	RA
	SEP	RD
	DB	9BH
	STR	R2
	GLO	RA
	SM
	BM	TAB1
	PLO	RA
	SEP	RD
	DB	0ACH
	BR	TAB2
TAB1	GLO	RA
	SD
	PLO	RA
	SEP	RD
	DB	80h	;; [RLA]	GLO	R0
TAB2	PLO	RC
TAB3	GLO	RA
	BZ	PRINT6
	GLO	RC
	SEP	R4
	DW	OUTPUT
	DEC	RA
	BR	TAB3
PRINT1	SEP	R4
	DW	EXPR
	GHI	R8
	ANI	04H
	BZ	PRINT2
	SEP	R4
	DW	FPACA
	DB	LOW U21
	SEP	R4
	DW	PFPN
	BR	PRINT3
PRINT2	SEP	R4
	DW	IBCDD
	SEP	R4
	DW	PINTN
PRINT3	GLO	R9
	SMI	04H
	PLO	R9
PRINT6	LDN	RB
	XRI	0C2H
	BNZ	PRINT4	;..CHK FOR COMMA
PRINTC	SEP	RD
	DB	9BH
	ANI	07H
	BZ	PRINT7
	SEP	RD
	DB	0ACH
	SEP	R4
	DW	OUTPUT
	BR	PRINTC
PRINT4	XRI	0FH	;..CHK FOR #CD (COLON)
	BZ	PRINTE
	XRI	0EH
	BNZ	PRINT5	;..CHK FOR #C3 (;)
PRINT7	GHI	R8
	ORI	20H
	PHI	R8
	INC	RB
	BR	PRINT
PRINT5	XRI	0CEH
	BZ	PRINTE
	SEP	R4
	DW	ERROR
	DB	1EH
PRINTE	GHI	R8
	ANI	20H
	BNZ	CFCL1
	SEP	R4
	DW	CRLF
CFCL1	GHI	R8
	ANI	0DFH
	PHI	R8
	SEP	R5
PRINT8	SEP	R4
	DW	PCHARS
	BR	PRINT6
PRINTA	INC	RB
	LDA	RB
	XRI	0D6H
	BNZ	CHRE
CHR1	SEP	R4
	DW	IEXPR
	INC	R9
	INC	R9
	INC	R9
	LDN	R9
	SEP	R4
	DW	OUTPUT
	GLO	R9
	SMI	07H
	PLO	R9
	LDA	RB
	XRI	0C2H
	BZ	CHR1
	XRI	06H
	BZ	PRINT6
CHRE	SEP	R4
	DW	ERROR
	DB	25H
PRINT9	SEP	R4
	DW	SEXPR1
	LDI	HIGH WP1
	PHI	RA
	LDI	00H
	PLO	RA	;..PT AT ST VAR BUFFER
AX3	LDA	RA
	STR	R2	;..SAVE CHARACTER
	XRI	0DH	;..IS IT A CR???
	BZ	PRINT6
	LDN	R2	;..GET BACK CHAR
	SEP	R4
	DW	OUTPUT
	BR	AX3

;......................................................
;	..DELETE LINE POINTED TO BY LP
;	..DELLIN.A
;	..........

DELLIN	GHI	RB
	PHI	RC	;..GET PRESENT POSITION
	GLO	RB
	PLO	RC	;..OF LINE POINTER
	SEP	R4
	DW	INCNL
	PLO	R8	;..LOAD LENGTH OF LINE
G2	LDA	RB	;..LOAD POS OF LP
	STR	RC
	INC	RC
	GHI	RB
	STR	R2
	SEP	RD
	DB	83H
	XOR		;..END OF US?
	BNZ	G2
	GLO	RB
	STR	R2
	LDN	RF
	XOR
	BNZ	G2
	GLO	R8
	STR	R2
	SEP	RD
	DB	84H
	SM
	PHI	RC	;..UPDATE INFO ON TOP OF
	BDF	G3	;..USER SPACE.
	SEP	RD
	DB	83H
	SMI	01H
	SEP	RD
	DB	03H
G3	GHI	RC
	SEP	RD
	DB	04H
	SEP	R5
;......................................................
;	..FLOATING POINT LOG FUNCTION..
;	..LOG.A
;	.......
FLN	GHI	R7
	BZ	FLOG1
FLOG2	SEP	RD
	DB	0B4H
	PHI	R8
	SEP	R4
	DW	ERROR
	DB	08H
FLOG1	GLO	RC
	BZ	FLOG2
	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RA
	GLO	RE
	DB	00H
	PLO	RA
	PHI	RA
	GLO	R9
	PLO	RE
	ADI	04H
	PHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW PIS
	INC	RD
	SEP	RD
	DB	LOW CFP2
	DW	CT9
FLOG4	INC	RD
	SEP	RD
	DB	LOW CMEX
	SHL
	BDF	FLOG3
	DEC	RC
	INC	RA
	BR	FLOG4
FLOG3	GLO	RC
	ANI	80H
	BNZ	FLOG5
	INC	RC
	DEC	RA
	BR	FLOG3
FLOG5	GLO	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW P1FS
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DW	CT18
	SEP	R4
	DW	FADD
	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G2FS
	GLO	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW P1FS
	INC	RD
	SEP	RD
	DB	LOW CFP1
	DW	CT18
	LDI	80H
	PHI	R7
	GHI	RE
	PLO	R9
	SEP	R4
	DW	FADD
	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G2FS
	SEP	R4
	DW	FDIV
	INC	RD
	SEP	RD
	DB	LOW P1FS
	INC	RD
	SEP	RD
	DB	LOW CF12
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW P1FS
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DW	CT19
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DW	CT20
	SEP	R4
	DW	FADD
	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G2FS
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DW	CT21
	SEP	R4
	DW	FADD
	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G2FS
	SEP	R4
	DW	FMULT
	GLO	RA
	BZ	FLOG9
	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW AZRST
	PHI	R8
	LDI	87H
	PLO	R8
	GLO	RA
	SHL
	BNF	FLOG6
	LDI	80H
	PHI	R8
	GLO	RA
	SDI	00H
FLOG8	PLO	RA
FLOG6	GLO	RA
	ANI	40H
	BNZ	FLOG7
	DEC	R8
	GLO	RA
	SHL
	BR	FLOG8

FLOG7	GLO	RA
	STR	R9
	SEP	R4
	DW	FADD
FLOG9	INC	RD
	SEP	RD
	DB	LOW CSP2
	DW	CT22
	SEP	R4
	DW	FMINUS
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DW	CT23
	SEP	R4
	DW	FMULT
	GLO	RC
	SMI	70H
	BPZ	FLOG11
FLOG10	INC	RD
	SEP	RD
	DB	LOW AZRST
	PHI	RC
	PHI	R7
	PLO	RC
FLOG11	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW GIS
	GLO	RE
	PLO	R9
	INC	R2
	SEP	R2
	SEP	R5

G1FSA	INC	R2
	LDA	R2
	PHI	R7
	LDA	R2
	PLO	RC
	BR	GISA1
G2FSA	INC	R2
	LDA	R2
	PHI	R8
	LDA	R2
	PLO	R8
	SKP

GISA	INC	R2
GISA1	LDA	R2
	STR	R9
	INC	R9
	LDA	R2
	STR	R9
	INC	R9
	LDA	R2
	STR	R9
	INC	R9
	LDA	R2
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	LBR	DDDONF
	

;......................................................
;	..ARCTANGENT FUNCTION..
;	..FATAN.A
;	.........
FATAN	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	R9
	GLO	RA
	DB	00H
	PLO	RA
	GHI	R7
	PHI	RA
	LDI	00H
	PHI	R7
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT9
	DB	LOW CT9
	INC	RD
	SEP	RD
	DB	LOW CMEX
	PHI	RF
	GLO	R9
	SMI	04H
	PLO	R9
	GHI	RF
	XRI	01H
	LSZ
	BR	FATAN1
	INC	RA
	INC	RD
	SEP	RD
	DB	LOW CF12
	INC	RD
	SEP	RD
	DB	LOW CSP1
	DB	HIGH CT9
	DB	LOW CT9
	GLO	R9
	ADI	04H
	PLO	R9
	SEP	R4
	DW	FDIV
FATAN1	INC	RD
	SEP	RD
	DB	LOW P1FS
	INC	RD
	SEP	RD
	DB	LOW CF12
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW P1FS
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT10
	DB	LOW CT10
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT11
	DB	LOW CT11
	SEP	R4
	DW	FADD
	INC	RD
	SEP	RD
	DB	LOW CSP3
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT12
	DB	LOW CT12
	SEP	R4
	DW	FADD
	INC	RD
	SEP	RD
	DB	LOW CSP3
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT13
	DB	LOW CT13
	SEP	R4
	DW	FADD
	INC	RD
	SEP	RD
	DB	LOW CSP3
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT14
	DB	LOW CT14
	SEP	R4
	DW	FADD
	INC	RD
	SEP	RD
	DB	LOW CSP3
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT15 
	DB	LOW CT15
	SEP	R4
	DW	FADD
	INC	RD
	SEP	RD
	DB	LOW CSP3
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT16
	DB	LOW CT16
	SEP	R4
	DW	FADD
	INC	RD
	SEP	RD
	DB	LOW CSP3
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT17
	DB	LOW CT17
	SEP	R4
	DW	FADD
	GLO	R9
	ADI	04H
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G2FS
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT9
	DB	LOW CT9
	SEP	R4
	DW	FADD
	GLO	R9
	ADI	04H
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G2FS
	SEP	R4
	DW	FMULT
	GLO	RA
	BZ	FATAN2
	INC	RD
	SEP	RD
	DB	LOW CF12
	INC	RD
	SEP	RD
	DB	LOW CSP1
	DB	HIGH CT3
	DB	LOW CT3
	GLO	R9
	ADI	04H
	PLO	R9
	SEP	R4
	DW	FMINUS
FATAN2	GHI	RA
	STR	R2
	GHI	R7
	XOR
	PHI	R7
	SEP	RD
	DB	9CH
	BZ	FATAND
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT1
	DB	LOW CT1
	SEP	R4
	DW	FDIV
FATAND	INC	R2
	SEP	R2
	SEP	R5
;......................................................
;	..ENABLE INTERRUPTS..
;	..ENINT.A
;	.....................
ENINT	LDN	RB
	XRI	0B4H
	BNZ	ENINT2
	INC	RB
	INC	RB
	INC	RB
	LDA	RB
	PHI	RA
	LDA	RB
	PLO	RA
	BR	ENINT3
ENINT2	SEP	R4
	DW	IGVAL
ENINT3	GLO	RA
	SEP	RD
	DB	17H
	GHI	RA
	STR	RF
ENINT1	DEC	R2
	LDI	23H
	STR	R2
	RET
	SEP	R5
;......................................................
;	..SIN AND COSINE FUNCTION..
;	..FSC.A
;	.......
FSIN	LDI	00H
	LSKP
FCOS	LDI	0FFH
	PHI	RF
	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	R9
	GLO	RA
	GLO	RB
	GLO	RE
	DB	00H
	PLO	RE
	INC	RE
	GHI	RF
	PHI	RA
	GHI	R7
	PLO	RA
	GLO	R9
	ADI	04H
	PHI	RB
	SEP	RD
	DB	9CH
	BZ	FSC7
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT1
	DB	LOW CT1
	SEP	R4
	DW	FMULT
FSC7	GHI	RA
	BNZ	FSC1
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT2
	DB	LOW CT2
	INC	RD
	SEP	RD
	DB	LOW CMEX
	SHL
	BPZ	FSC2
	GLO	R9
	SMI	04H
	PLO	R9
	GHI	R7
	XRI	80H
	PHI	R7
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT3
	DB	LOW CT3
	SEP	R4
	DW	FADD
FSC1	LDI	00H
	PHI R7
FSC3	INC	RD
	SEP	RD
	DB	LOW CSP2
	DW	CT3
	SEP	R4
	DW	FMINUS
	INC	RE
	GHI	R7
	BZ	FSC3
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DW	CT3
	SEP	R4
	DW	FADD
	DEC	RE
	GLO	RE
	SHR
	STXD
	BDF	FSC6
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DW	CT3
	LDI	80H
	PHI	R7
	SEP	R4
	DW	FADD
FSC6	INC	RD
	SEP	RD
	DB	LOW CF12
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW P1FS
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT4
	DB	LOW CT4
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT5
	DB	LOW CT5
	SEP	R4
	DW	FADD
	INC	RD
	SEP	RD
	DB	LOW CSP3
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT6
	DB	LOW CT6
	SEP	R4
	DW	FADD
	INC	RD
	SEP	RD
	DB	LOW CSP3
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT7
	DB	LOW CT7
	SEP	R4
	DW	FADD
	INC	RD
	SEP	RD
	DB	LOW CSP3
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT8
	DB	LOW CT8
	SEP	R4
	DW	FADD
	GHI	RB
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G2FS
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT9
	DB	LOW CT9
	SEP	R4
	DW	FADD
	INC	R2
	LDX
	SHR
	BNF	FSC5
	LDI	80H
	PHI	R7
FSC5	INC	RD
	SEP	RD
	DB	LOW CSP2
	DB	HIGH CT2
	DB	LOW CT2
	INC	RD
	SEP	RD
	DB	LOW CMEX
	SHL
	BM	FSC2
	INC	R2
	SEP	R2
	INC	RD
	SEP	RD
	DB	LOW AZRST
	PHI	R7
	PLO	RC
	SEP	R5
FSC2	INC	R2
	SEP	R2
	SEP	R5

;......................................................
;	..SET BREAK FOR FLOATING PT/INT NUMBERS
;	..DLINTV.A
;	.......................................
DLINTV	LDN	RB
	XRI	0D1H
	BZ	DLI1
	XRI	0DCH
	BZ	DLI3	;..CHK FOR #0D (CR)
	XRI	0C0H
	BNZ	DLIE	;..CHK FOR #CD (:)
DLI3	LDI	40H
	LSKP
DLI1	INC	RB
	LDA	RB
DLI2	ADI	01H
	SEP	RD
	DB	3CH
	SEP	R5
DLIE	SEP	R4
	DW	ERROR
	DB	04H
;......................................................
;	..CHECK COMP STACK FOR ZERO..
;	..ZRCHA.A
;	.........
ZRCHA	SEX	R9
	LDA	R9
	OR
	INC	R9
	OR
	INC	R9
	OR
	DEC	R9
	DEC	R9
	DEC	R9
	LBR	DDDONE
;......................................................
;	..FLOATING POINT E-FUNCTION..
;	..FEXP.A
;	........
FEXP	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RB
	GLO	RE
	DB	00H
	GHI	R7
	PHI	RB
	LDI	00H
	PHI	R7
	GLO	R9
	PLO	RE
	ADI	04H
	PHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW CFP2
	DW	CT28
	SEP	R4
	DW	FMULT
	GLO	RC
	LSZ
	ANI	80H
	BZ	FEXP1
	LDN	R9
	PLO	RB
	GLO	RC
	SDI	87H
	BDF	FEXP2
	SEP	RD
	DB	0B4H
	PHI	R8
	SEP	R4
	DW	ERROR
	DB	0FH
FEXP2	PLO	RF
FEXP4	GLO	RF
	BZ	FEXP3
	GLO	RB
	SHR
	PLO	RB
	DEC	RF
	BR	FEXP4
FEXP3	GLO	RC
	XRI	80H
	BZ	FEXP5
	INC	RD
	SEP	RD
	DB	LOW SHLOB
	DEC	RC
	BR	FEXP3
FEXP5	LDN	R9
	ANI	7FH
	STR	R9
	SEP	R4
	DW	NMFP
	SKP
FEXP1	PLO	RB
	INC	RD
	SEP	RD
	DB	LOW P1FS
	INC	RD
	SEP	RD
	DB	LOW CF12
	SEP	R4
	DW	FMULT
	INC	RD
	SEP	RD
	DB	LOW P1FS
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DW	CT24
	SEP	R4
	DW	FMULT
	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G2FS
	GLO	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW P1FS
	INC	RD
	SEP	RD
	DB	LOW CFP1
	DW	CT25
	GHI	RE
	PLO	R9
	SEP	R4
	DW	FADD
	INC	RD
	SEP	RD
	DB	LOW CF12
	INC	RD
	SEP	RD
	DB	LOW CSP1
	DW	CT27
	GHI	RE
	PLO	R9
	SEP	R4
	DW	FDIV
	GHI	R7
	XRI	80H
	PHI	R7
	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G2FS
	SEP	R4
	DW	FADD
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DW	CT26
	SEP	R4
	DW	FADD
	INC	RD
	SEP	RD
	DB	LOW CSP3
	SEP	R4
	DW	FMINUS
	INC	RD
	SEP	RD
	DB	LOW CF12
	GLO	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G1FS
	GHI	RE
	PLO	R9
	SEP	R4
	DW	FDIV
	INC	RD
	SEP	RD
	DB	LOW CSP2
	DW	CT22
	SEP	R4
	DW	FADD
	INC	RC
	GLO	RC
	STR	R2
	GLO	RB
	ADD
	PLO RC
	GHI	RB
	BZ	FEXP7
	INC	RD
	SEP	RD
	DB	LOW CF12
	INC	RD
	SEP	RD
	DB	LOW CSP1
	DW	CT9
	GHI	RE
	PLO	R9
	SEP	R4
	DW	FDIV
FEXP7	INC	R2
	SEP	R2
	SEP	R5

;......................................................
;	..NORMALIZE FUNCTION FOR FP..
;	..NORM.A
;	........
SMULT	INC	RC
	INC	RC
	INC	RC
	INC	RD
	SEP	RD
	DB	LOW CI12
	INC	RD
	SEP	RD
	DB	LOW SHROB
	INC	RD
	SEP	RD
	DB	LOW SHROB
	INC	RD
	SEP	RD
	DB	LOW IADD
NMFP	INC	RD
	SEP	RD
	DB	LOW ZRCH
	BNZ	NMFP1
	PHI	RC
	PLO	RC
	PHI	R7
NMFP5	SEP	R5
NMFP1	LDN	R9
	ANI	80H
	BZ	NMFP2
	INC	RD
	SEP	RD
	DB	LOW SHROB
	INC	RC
NMFP2	LDN	R9
	ANI	40H
	BNZ	NMFP3
	INC	RD
	SEP	RD
	DB	LOW SHLOB
	DEC	RC
	BR	NMFP2
NMFP3	GHI	RC
	BNZ	NMFP4
	GLO	RC
	BNZ	NMFP5
NMFP4	SEP	RD
	DB	0B4H
	PHI	R8
	SEP	R4
	DW	ERROR
	DB	0FH
;......................................................
;	..FLOATING POINT CONSTANTS..
;	..CONST.A
;	.........
CT1	DW 0007BH,0477DH,01A88H
CT2	DW 00073H,068DBH,08BABH
CT3	DW 00081H,06487H,0ED00H
CT4	DW 0FF6BH,045EDH,06C75H
CT5	DW 00071H,067DAH,0D059H
CT6	DW 0FF77H,05B04H,0DD14H
CT7	DW 0007CH,05555H,051E0H
CT8	DW 0FF80H,04000H,00000H
CT9	DW 00081H,04000H,00000H
CT10	DW 00078H,05DEBH,0AE0BH
CT11	DW 0FF7BH,04236H,0F70CH
CT12	DW 0007CH,057E0H,0FD02H
CT13	DW 0FF7DH,04D18H,08F40H
CT14	DW 0007DH,06D1EH,0D9DCH
CT15	DW 0FF7EH,048BFH,0E3AEH
CT16	DW 0007EH,0665EH,002E8H
CT17	DW 0FF7FH,05555H,03EF5H
CT18	DW 00080H,05A82H,07B6CH
CT19	DW 00080H,04CABH,05806H
CT20	DW 00080H,07B11H,07B4EH
CT21	DW 00082H,05C55H,01D67H
CT22	DW 00080H,04000H,00000H
CT23	DW 00080H,058B9H,00A75H
CT24	DW 0007CH,046FAH,06FF9H
CT25	DW 00087H,0576AH,0E113H
CT26	DW 00084H,04FA3H,00338H
CT27	DW 0008AH,04D3FH,01CD4H
CT28	DW 00081H,05C55H,01DA0H

;......................................................
;	..FOLATING POING ADD AND MINUS..
;	..FADMI.A
;	.........
FMINUS	GHI	R8
	XRI	80H
	PHI	R8
FADD	GLO	RC
	STR	R2
	GLO	R8
	SM
	BM	FADD1
	INC	RD
	SEP	RD
	DB	LOW P2FS
	GLO	R9
	SMI	04H
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW CF12
	GLO	R9
	SMI	04H
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G1FS
	GLO	R9
	ADI	04H
	PLO	R9
FADD1	GLO	R8
	STR	R2
	GLO	RC
	SM
	PLO	RF
	SMI	1EH
	BPZ	FADD6
FADD4	GLO	RF
	BZ	FADD3
	INC	RD
	SEP	RD
	DB	LOW SHROB
	DEC	RF
	BR	FADD4
FADD3	GHI	R7
	STR	R2
	GHI	R8
	XOR
	BZ	FADD5
	INC	RD
	SEP	RD
	DB	LOW INEG
FADD5	INC	RD
	SEP	RD
	DB	LOW IADD
	GHI	R7
	STR	R2
	GHI	R8
	XOR
	BZ	FADD2
	LDN	R9
	ANI	80H
	BZ	FADD2
	INC	RD
	SEP	RD
	DB	LOW INEG
	GHI	R8
	PHI	R7
FADD2	LBR	NMFP
FADD6	GLO	R9
	SMI	04H
	PLO	R9
	BR	FADD2
;......................................................
;	..FLOATING POINT BCD TO DECIMAL..
;	..FCBD.A
;	........
FCBD	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RB
	GLO	RE
	DB	00H
	GHI	R7
	STXD
	PHI	R8
	LDI	0AH
	PLO	R7
	GLO	R9
	PLO	RE
	ADI	04H
	PHI	RE
	GLO	RC
	BZ	FCBD1
FCBD3	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW AZRST
	LDI	84H
	PLO	R8
	LDI	50H
	STR	R9
	GLO	RC
	SDI	9FH
	BDF	FCBD2
	SEP	R4
	DW	FDIV
	INC	R7
	BR	FCBD3
FCBD2	GLO	RE
	PLO	R9
	GLO	RC
	SMI	9CH
	BPZ	FCBD4
	SEP	R4
	DW	SMULT
	DEC	R7
	BR	FCBD2
FCBD4	GLO	RC
	SDI	9FH
	BZ	FCBD1
	INC	RD
	SEP	RD
	DB	LOW SHROB
	INC	RC
	BR	FCBD4
FCBD1	LDI	0FFH
	PHI	RF
	INC	RD
	SEP	RD
	DB	LOW ARFRF
	INC	R2
	LDX
	PHI	R7
	SEP	R4
	DW	FIBCD
	LDI	HIGH WP1
	PHI	RB
	LDI	0C9H
	PLO	RB
	GLO	R7
	ANI	80H
	BZ	FCBD5
	GLO	R7
	SDI	00H
	PLO	R7
	LDI	0DH
	LSKP
FCBD5	LDI	0BH
	STR	RB
	INC	RB
	INC	RB
	LDI	00H
	PLO	RF
	GLO	R7
FCBD6	SMI	0AH
	INC	RF
	BPZ	FCBD6
	DEC	RF
	ADI	0AH
	STR	RB
	DEC	RB
	GLO	RF
	STR	RB
	INC	R2
	SEP	R2
	GHI	R3
	PHI	R6
	LDI	LOW FCBD9
	PLO	R6
	SEP	R5
FCBD9	INC	R2
	SEP	R2
	SEP	R5

;......................................................
;	..CORE DIVIDE PROGRAM..
;	..CDIV.A
;	........
ICDIV	LDI	LOW ICDIV1
	LSKP
FCDIV	LDI	LOW FCDIV1
	STXD
	GLO	R9
	SMI	04H
	PLO	R9
CDIV3	LDN	R9
	ANI	80H
	BNZ	CDIV1
	GLO	RE
	BZ	CDIV2
	DEC	RE
	INC	RD
	SEP	RD
	DB	LOW SHLOB
	GLO	RA
	SHL
	PLO	RA
	GHI	RA
	SHLC
	PHI	RA
	GLO	RB
	SHLC
	PLO	RB
	GHI	RB
	SHLC
	PHI	RB
	BR	CDIV3
CDIV1	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW IMINUS
	INC	RA
	GLO	RA
	BNZ	CDIV3
	GHI	RA
	BNZ	CDIV3
	INC	RB
	BR	CDIV3

CDIV2	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW IMINUS
	BM	CDIV4
	INC	RA
	GLO	RA
	BNZ	CDIV2
	GHI	RA
	BNZ	CDIV2
	INC	RB
	BR	CDIV2
CDIV4	GHI	RE
	PLO	R9
	SEX	R9
	DEC	R9
	GLO	RA
	STXD
	GHI	RA
	STXD
	GLO	RB
	STXD
	GHI	RB
	STR	R9
	SEX	R2
	INC	R2
	LDN	R2
	PLO	R3
ICDIV1	LBR	IDIVR
FCDIV1	LBR	FMDR

;......................................................
;	..TAKE ABSOLUTE VALUE OF A NUMBER
;	..ABS.A
;	.................................
ABS	GHI	R8
	STXD
	SEP	R4
	DW	EVARG
	SEP	R4
	DW	FABS
ABS4	LBR	CONV
FABS	GHI	R8
	ANI	04H
	BZ	IABS
FPABS	INC	R9
	LDN	R9
	DEC	R9
	ANI	80H
	BZ	ABS1
	SEP	R4
	DW	FCOMP
ABS2	LDI	01H	;..SET FLAG UPON LEAVING
ABS1	SEP	R5
IABS	LDN	R9
	ANI	80H
	BZ	ABS1
	INC	RD
	SEP	RD
	DB	LOW INEG
	BR	ABS2
;......................................................
;	..INTEGER MULTIPLY..
;	..IMULT.A
;	.........
IMULT	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RB
	GLO	RE
	DB	00H
	PLO	RE
	LDI	04H
	PLO	RB
	GHI	R9
	PHI	RF
	GLO	R9
	PHI	RE
	SMI	04H
	PHI	RB
	PLO	RF
	PLO	R9
	LDN	R9
	ANI	80H
	STXD
	BZ	IMULT1
	INC	RD
	SEP	RD
	DB	LOW INEG
IMULT1	GHI	RE
	PLO	R9
	LDN	R9
	ANI	80H
	BZ	IMULT3
	INC	R2
	XOR
	STXD
	INC	RD 
	SEP	RD
	DB	LOW INEG
IMULT3	LDN	R9
	STXD
	BNZ	IMULT2
	GLO	RE
	LSNZ
	INC	R2
	SKP
IMULT2	INC	RE
	LDN	RF
	STR	R9
	LDI	00H
	STR	RF
	INC	R9
	INC	RF
	DEC	RB
	GLO	RB
	BNZ	IMULT3
	GLO	RE
	BNZ	IMULT4
	INC	R2
	GHI	RB
	PLO	R9
	BR	IMLT10
IMULT4	GHI	RE
	PLO	R9
IMULT5	LDI	08H	; WAS LDI	80H
	PLO	RB
	INC	R2
IMULT6	LDN	R2
	SHR
	STR	R2
	BNF	IMULT9
	INC	RD
	SEP	RD
	DB	LOW IADD
	BNF	IMULT8
IMULT7	SEP	R4
	DW	ERROR
	DB	0FH
IMULT8	GHI	RE
	PLO	R9
IMULT9	INC	RD
	SEP	RD
	DB	LOW SHLOB
	DEC	RB
	GLO	RB
	BNZ	IMULT6
	DEC	RE
	GLO	RE
	BNZ	IMULT5
	GHI	RB
	PLO	R9
	LDN	R9
	ANI	80H
	BNZ	IMULT7
	INC	R2
	LDN	R2
	BZ	IMLT10
	INC	RD
	SEP	RD
	DB	LOW INEG
IMLT10	INC	R2
	SEP	R2
	SEP	R5

;......................................................
;	..INTEGER DIV INTERFACE..
;	..IARIT.A
;	.........
IDIV	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RA
	GLO	RB
	GLO	RE
	DB	00H
	PHI	RE
	PLO	RE
	PLO	RA
	LDI	1EH
	PLO	RB
	GLO	R9
	PHI	RA
	SMI	04H
	PHI	RB
	PLO	R9
	LDN	R9
	ANI	80H
	STXD
	BZ	IARIT1
	INC	RD
	SEP	RD
	DB	LOW INEG
IARIT1	LDN	R9
	ANI	0C0H
	BNZ	IARIT2
	GLO	RB
	BNZ	IARIT3
IARITB	INC	R2
IARITC	INC	R2
	SEP	R2
	SEP	R5
IARIT3	DEC	RB
	DEC	RE
	INC	RD
	SEP	RD
	DB	LOW SHLOB
	BR	IARIT1
IARIT2	GHI	RA
	PLO	R9
	LDN	R9
	ANI	80H
	BZ	IDIV1
	INC	R2
	XOR
	STXD
	INC	RD
	SEP	RD
	DB	LOW INEG
IDIV1	LDI	1EH
	PLO	RB
IARIT9	LDN	R9
	ANI	0C0H
	BNZ	IARIT7
	GLO	RB
	BNZ	IARIT8
	SEP	R4
	DW	ERROR
	DB	08H
IARIT8	DEC	RB
	INC	RE
	INC	RD
	SEP	RD
	DB	LOW SHLOB
	BR	IARIT9
IARIT7	GHI	RE
	BZ	IARITA
	GHI	RB
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW AZRST
	BR	IARITB
IARITA	GHI	RA
	PHI	RE
	LDI	00H
	PHI	RB
	PLO	RB
	PHI	RA
	LBR	ICDIV
IDIVR	INC	R2
	LDN	R2
	BZ	IARITC
	INC	RD
	SEP RD
	DB	LOW INEG
	BR	IARITC

;......................................................
;	..FOUND AND FORMAT ANSWER..
;	..FPACR.A
;	.........
FPACR	LDI	40H
	PHI	RF
	INC	RD
	SEP	RD
	DB	LOW ARFRF
	LDN	R9
	ANI	80H
	BZ	FPAC3
	INC	RC
	GHI	RC
	BZ	FPAC4
	SEP	RD
	DB	0B4H
	PHI	R8
	SEP	R4
	DW	ERROR
	DB	0FH
FPAC3	INC	RD
	SEP	RD
	DB	LOW SHLOB
FPAC4	LDA	R9
	ANI	7FH
	STR	R2
	GHI	R7
	OR
	STXD
	LDA	R9
	STR	R2
	LDA	R9
	SEX	R9
	STXD
	LDA	R2
	STXD
	LDN	R2
	STXD
	SEX	R2
	GLO	RC
	STR	R9
	SEP	R5

;......................................................
;	..MID$ COMMAND FOR STRINGS..
;	..MID.AX
;	............................
MID	LDA	RB
	XRI	0D6H
	BNZ	MIDE
	LDN	RB
	XRI	0D7H
	BNZ	MIDE
	GLO	R7
	STXD
	SEP	R4
	DW	SEXPR7
	LDA	RB
	XRI	0C2H
	BNZ	MIDE
	SEP	R4
	DW	GVAL
	DEC	RA
	GLO	RA
	STXD
	LDN	RB
	XRI	0C2H
	BNZ	MID5
	INC	RB
	SEP	R4
	DW	GVAL
	BR	MID4
MID5	LDI	0FFH
	PLO	RA
MID4	INC	R2
	LDXA
	ADD
	PLO	RC
	BDF	MIDE
	XOR
	PLO	RF
	LDX
	PLO	R7
	PHI	RF
	GHI	R7
	PHI	RC
	LDA	RB
	XRI	0C4H
	BNZ	MIDE
	GLO	RA
	BZ	MID6
	GLO	RF
	BZ	MID3
	GLO	R7
	PLO	RF
MID8	LDA	R7
	XRI	0DH
	INC	RF
	BNZ	MID8
	DEC	RF
	GHI	RF
	PLO	R7
	GLO	RF
	STR	R2
	GLO	RC
	SM
	BPZ	MID6
MID1	LDA	RC
	STR	R7
	INC	R7
	XRI	0DH
	BNZ	MID1
	GHI	RF
	PLO	R7
MID3	LDA	R7
	XRI	0DH
	BZ	MID7
	DEC	RA
	GLO	RA
	BNZ	MID3
MID2	LDI	0DH
	STR	R7
	INC	R7
MID7	SEP	R5
MID6	GHI	RF
	PLO	R7
	BR	MID2
MIDE	SEP	R4
	DW	ERROR
	DB	19H

;......................................................
;	..TABLE OF CONSTANTS FOR BCD CONVERSION..
;	..CONTAB.A
;	..........
CONTAB	DW 03B9AH,0CA00H,005F5H,0E100H
	DW 00098H,09680H,0000FH,04240H
	DW 00001H,086A0H,00000H,02710H
	DW 00000H,003E8H,00000H,00064H
	DW 00000H,0000AH,00000H,00001H
;......................................................
;	..SPECIAL OUTPUT NUMBER ROUTINE..
;	..ONUM.A
;	........
ONUM	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RA
	GLO	R8
	DB	00H
	PLO	RA
	LDI	HIGH WP2
	PHI	R8
	LDI	0E0H
	PLO	R8
ONUM1	LDA	R8
	INC	RA
	BNZ	ONUM1
	DEC	RA
	LDI	0E0H
	PLO	R8
	GHI	R6
	XRI	HIGH PRINT3
	BNZ	ONUM2
	GLO	RA
	STR	R2
	SEP	RD
	DB	0B3H
	BZ	ONUM2
	SM
	PLO	RA
	BM	ONUMS
ONUM4	GLO	RA
	BZ	ONUM2
	SEP	RD
	DB	0ACH
	SEP	R4
	DW	OUTPUT
	DEC	RA
	BR	ONUM4
ONUM2	LDA	R8
	BZ	ONUMD
	SEP	R4
	DW	OUTPUT
	BR	ONUM2
ONUMD	INC	R2
	SEP	R2
	SEP	R5
ONUMS	SEP	RD
	DB	0B3H
	PLO	RA
	LDN	R8
	XRI	2DH
	BNZ	ONUMS1
	LDI	2DH
	SEP	R4
	DW	OUTPUT
ONUMS2	DEC	RA
	GLO	RA
	BZ	ONUMD
ONUMS1	LDI	2AH
	SEP	R4
	DW	OUTPUT
	BR	ONUMS2

;......................................................
;	..FLOATING POINT ARITHMETIC CALL..
;	..FPAC.A
;	........
FPAC	LDI	LOW FPAC6
	LSKP
FPACA	LDI	LOW FPAC5
	STR	R2
	GHI	R8
	SEP	RD
	DB	34H
	LDN	R2
	PHI	RF
	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	R7
	GLO	R8
	GLO	RC
	DB	00H
	PHI	RC
	GLO	R9
	STR	R2
	GHI	RF
	PLO	R3
FPAC6	LDN	R9
	PLO	R8
	INC	RD
	SEP	RD
	DB	LOW SHLOBY
	LDN	R9
	ANI	80H
	PHI	R8
	GLO	R8
	BZ	FPAC1
	LDN	R9
	ORI	80H
	STR	R9
	INC	RD
	SEP	RD

	DB	LOW SHROB
FPAC1	GLO	R9
	SMI	04H
	PLO	R9
FPAC5	LDN	R9
	PLO	RC
	INC	RD
	SEP	RD
	DB	LOW SHLOBY
	LDN	R9
	ANI	80H
	PHI	R7
	GLO	RC
	BZ	FPAC2
	LDN	R9
	ORI	80H
	STR	R9
	INC	RD
	SEP	RD
	DB	LOW SHROB
FPAC2	LDN	R2
	PLO	R9
	LDA	R6
	SEP	R4
	DW	FPACJ
	SEP	R4
	DW	FPACR
	INC	R2
	SEP	R2
	SEP	R5
FPACJ	PLO	R3
U7	LBR	FMINUS
U6	LBR	FADD
U8	LBR	FMULT
U9	LBR	FDIV
U15	LBR	FSIN
U16	LBR	FCOS
U17	LBR	FATAN
U18	LBR	FEXP
U19	LBR	FLN
U20	LBR	FSQRT
U21	LBR	FCBD

;......................................................
;	..ROUTINE TO ^ (RAISE POWER)..
;	..EXPON.A
;	..............................
EXPON	GHI	R8
	ANI	04H
	BNZ	EXPON1
	SEP	R4
	DW	FNUMA
	GLO	R9
	SMI	04H
	PLO	R9
	SEP	R4
	DW	FNUMA
	BR	EXPON2
EXPON1	GLO	R9
	SMI	04H
	PLO	R9
EXPON2	LDN	R9
	BZ	EXPOND
	SEP	R4
	DW	FPABS
	STXD
	SEP	R4
	DW	FPACA
	DB	LOW U19
	GLO	R9
	ADI	04H
	PLO	R9
	INC	R2
	LDX
	STXD
	BZ	EXPON3
	INC	RD
	SEP	RD
	DB	LOW CI12
	LDN	R9
	BZ	EXPON4
	SDI	98H
	BM	EXPON4
	PLO	RF
	SMI	18H
	BPZ	EXPON5
	INC	R9
	LDN	R9
	ORI	80H
	STR	R9
	DEC	R9
EXPON6	INC	RD
	SEP	RD
	DB	LOW SHROB
EXPON5	LBDF	FLOG1-4		;..GIVE ERROR MESSAGE
	DEC	RF
	GLO	RF
	BNZ	EXPON6
	INC	RD
	SEP	RD
	DB	LOW SHROB
	LDI	0FFH
	LSDF
EXPON4	LDI	00H
	INC	R2
	AND
	STXD
	GLO	R9
	SMI	04H
	PLO	R9
EXPON3	SEP	R4
	DW	FPAC
	DB	LOW U8	;..MULTIPLY
	SEP	R4
	DW	FPACA
	DB	LOW U18	;..E RAISED TO X
	INC	R2
	LDX		;..CHK FOR SIGN
	BZ	EXPOND
	SEP	R4
	DW	FCOMP
EXPOND	GHI	R8
	ANI	04H
	BNZ	EXPONE
	SEP	R4
	DW	INT12
EXPONE	SEP	R5

;......................................................
;	..HELP FOR FIORFP.A..
;	..FFPAP.A
;	.........
FFPAP	INC	R2
	LDN	R2
	BNZ	FFPAP1
	GHI	R8
	ANI	01H
	BNZ	FFPAP1
	INC	R9
	INC	R9
	BR	FFPAP2
FFPAP1	LDI	0D2H
	LSKP
FFPO	LDI	0D3H
	STR	R7
	INC	R7
	LDA	R9
	STR	R7
	INC	R7
	LDA	R9
	STR	R7
	INC	R7
FFPAP2	LDA	R9
	STR	R7
	INC	R7
	LDN	R9
	STR	R7
	INC	R7
	LBR	C2

;......................................................
;	..INTEGER BCD TO DECIMAL..
;	..IBCDD.A
;	.........
IBCDD	LDI	00H
	PHI	RF
IBCD1	LDN	R9
	ANI	80H
CHUCK	PLO	RF
	BZ	IBCD2
	INC	RD
	SEP	RD
	DB	LOW INEG
	BR	IBCD2
FIBCD	LDI	0FFH
	PHI	RF
	GHI	R7
	PLO	RF
IBCD2	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	R8
	GLO	RA
	GLO	RB
	GLO	RE
	DB	00H
	PHI	R8
	LDI	HIGH WP1
	PHI	RB
	LDI	0C0H
	PLO	RB
	LDI	0AH
	PLO	R8
	LDI	HIGH CONTAB
	PHI	RA
	LDI	LOW CONTAB
	PLO	RA
	GLO	R9
	ADI	04H
	PHI	RE
	GLO	RF
	BZ	IBCD3
	LDI	0DH
	LSKP
IBCD3	LDI	0BH
	STR	RB
	INC	RB
	GHI	RF
	BZ	IBCD4
	GLO	RC
	LSNZ
IBCD4	STR	RB
	INC	RB
IBCD9	GHI	R8
	PLO	RE
	GHI	RA
	PHI	RF
	GLO	RA
	PLO	RF
	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW CIG
	GLO	RF
	PLO	RA
IBCD5	INC	RD
	SEP	RD
	DB	LOW IMINUS
	GHI	RE
	PLO	R9
	INC	RE
	BPZ	IBCD5
	INC	RD
	SEP	RD
	DB	LOW IADD
	DEC	RE
	GLO	RB
	XRI	0C1H
	BNZ	IBCD6
	GLO	RE
	BNZ	IBCD7
	DEC	R7
	BR	IBCD8
IBCD6	GLO	RE
IBCD7	STR	RB
	INC	RB
IBCD8	DEC	R8
	GLO	R8
	BNZ	IBCD9
	INC	R2
	SEP	R2
	SEP	R5

;......................................................
;	..READ STATEMENT..
;	..READ.A
;	..................
READ	SEP	RD
	DB	0B0H
	STR	R2
	LDA	RF
	PHI	RE
	LDN	RF
	PLO	RE
	LDN	R2
	BNZ	READ1
READ3	SEP	R4
	DW	SHDAT
	LDI	01H
	SEP	RD
	DB	30H
READ1	LDN	RE
	XRI	0DH
	BNZ	READ2
	INC	RE
	BR	READ3
READ2	LDN	RB
	XRI	0D7H
	BNZ	READ4
	INC	RB
	LDA	RB
	STXD
	SEP	R4
	DW	SVWK
	PHI	RF
	INC	R2
	LDX
	PLO	RF
	GLO	RB
	STXD
	GHI	RB
	STXD
	GLO	RE
	PLO	RB
	GHI	RE
	PHI	RB
	GLO	RF
	STXD
	GHI	RF
	STXD
	SEP	R4
	DW	SEXPR1
	INC	R2
	LDXA
	PLO	RE
	LDX
	PLO	R8
	SEP	R4
	DW	LDSV
	LDN	RB
	XRI	0C2H
	BNZ	READ6
	INC	RB
	BR	READ7
READ6	LDN	RB
	XRI	0DH
	BZ	READ7
	SEP	R4
	DW	ERROR
	DB	1FH
READ7	GLO	RB
	PLO	RE
	GHI	RB
	PHI	RE
	INC	R2
	LDXA
	PHI	RB
	LDX
	PLO	RB
	BR	READ5
READ4	SEP	R4
	DW	INRD
READ5	LDA	RB
	STR	R2
	XRI	0C2H
	BZ	READ1
	DEC	RB
	GLO	RE
	SEP	RD
	DB	32H
	GHI	RE
	STR	RF
	SEP	R5

;......................................................
;	..PUSH COMP STACK TO STACK..
;	..PUSH.A
;	........
P1FSA	LDI	LOW PISA2
	LSKP
P2FSA	LDI	LOW PISA3
	LSKP
PISA	LDI	LOW PISA4
	PLO	RF
	INC	R9
	INC	R9
	INC	R9
	DEC	R2
	LDN	R9
	STXD
	DEC	R9
	LDN	R9
	STXD
	DEC	R9
	LDN	R9
	STXD
	DEC	R9
	LDN	R9
	STXD
	GLO	RF
	PLO	RD
PISA4	LBR	DDDONF
PISA2	GLO	RC
	STXD
	GHI	R7
	STXD
	BR	PISA4
PISA3	GLO	R8
	STXD
	GHI	R8
	STXD
	BR	PISA4
;......................................................
;	..FP MULTIPLY AND DIV INTERFACE..
;	..FARIT.A
;	.........
FMULT	LDI	LOW FMULT1
	LSKP
FDIV	LDI	LOW FDIV1
	PHI	RF
	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RA
	GLO	RB
	GLO	RE
	DB	00H
	PHI	RA
	PLO	RA
	PHI	RB
	PLO	RB
	PLO	RE
	GLO	R9
	PHI	RE
	GHI	R7
	STR	R2
	GHI	R8
	XOR
	PHI	R7
	GLO	RC
	BZ	FARIT1
	STR	R2
	GHI	RF
	PLO	R3
FMULT1	GLO	R8
	BZ	FARIT1
	SMI	81H
	PLO	RC
	GHI	RC
	SMBI	00H
	PHI	RC
	GLO	RC
	ADD
	PLO	RC
	GHI	RC
	ADCI	00H
	PHI	RC
	LBR	FCMULT
FDIV1	GLO	R8
	BNZ	FARIT2
	SEP	RD
	DB	0B4H
	PHI	R8
	SEP	R4
	DW	ERROR
	DB	08H
FARIT2	SMI	81H
	BM	FARIT3
	SD
	PLO	RC
	GHI	RC
	SMBI	00H
	BR	FARIT4
FARIT3	GLO	R8
	SDI	81H
	ADD
	PLO	RC
	GHI	RC
	ADCI	00H
FARIT4	PHI	RC
	LDI	1EH
	PLO	RE
	LBR	FCDIV
FARIT1	GLO	R9
	SMI	04H
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW AZRST
	PHI	R7
	PLO	RC
FMDR	INC	R2
	SEP	R2
	LBR	NMFP

;......................................................
;	..FLOATING POINT SQUARE ROOT..
;	..FSQRT.A
;	.........
FSQRT	GHI	R7
	BZ	FSQRT1
	SEP	RD
	DB	0B4H
	PHI	R8
	SEP	R4
	DW	ERROR
	DB	0FH
FSQRT1	GLO	RC
	BNZ	FSQRT2
	SEP	R5
FSQRT2	INC	RD
	SEP	RD
	DB	LOW REGSV
	GLO	RE
	DB	00H
	GLO	R9
	PLO	RE
	ADI	04H
	PHI	RE
	INC	RD
	SEP	RD
	DB	LOW P1FS
	INC	RD
	SEP	RD
	DB	LOW CF12
	DEC	R8
FSQRT4	INC	RD
	SEP	RD
	DB	LOW P2FS
	SEP	R4
	DW	FDIV
	INC	RD
	SEP	RD
	DB	LOW CSP3
	SEP	R4
	DW	FADD
	DEC	RC
	GHI	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G2FS
	INC	RD
	SEP	RD
	DB	LOW CMEX
	BZ	FSQRT3
	GLO	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW CF12
	GLO	RE
	PLO	R9
	INC	RD
	SEP	RD
	DB	LOW G1FS
	INC	RD
	SEP	RD
	DB	LOW P1FS
	GHI	RE
	PLO	R9
	BR	FSQRT4
FSQRT3	INC	RD
	SEP	RD
	DB	LOW G2FS
	GLO	RE
	PLO	R9
	INC	R2
	SEP	R2
	SEP	R5

;......................................................
;	..CORE MULTIPLY ROUTINE..
;	..CMULT.A
;	.........
FCMULT	LDI	04H
	PLO	RB
	SHL
CMULT1	DEC	R9
	LDN	R9
	SHLC
	DEC	RB
	BZ	CMULT1
CMULT3	STXD
	INC	RE
	GLO	RB
	BZ	CMULT2
	DEC	R9
	LDN	R9
	SHLC
	DEC	RB
	BR	CMULT3
CMULT2	INC	RD
	SEP	RD
	DB	LOW AZRST
	GHI	RE
	PLO	R9
CMULT7	GLO	RE
	BZ	CMULT4
	LDI	08H
	PLO	RB
	INC	R2
CMULT6	LDN	R2
	SHL
	STR	R2
	BM	CMULT5
	INC	RD
	SEP	RD
	DB	LOW IADD
	GHI	RE
	PLO	R9
CMULT5	INC	RD
	SEP	RD
	DB	LOW SHROB
	DEC	RB
	GLO	RB
	BNZ	CMULT6
	DEC	RE
	BR	CMULT7
CMULT4	GLO	R9
	SMI	04H
	PLO	R9
FCMUL1	LBR	FMDR

;......................................................
;	..SET UP FORMAT PARAMETERS..
;	..FORMAT.A
;	..........
FORMAT	SEP	R4
	DW	IGVAL
	GHI	RA
	STR	R2
	GLO	RA
	ANI	0F0H
	OR
	LBNZ	GVALE
	GLO	RA
	SEP	RD
	DB	33H
	SEP	R5
;......................................................
;	..DD COPY ROUTINE..
;	..DDCOPY.A
;	..........
CSP1A	GLO	R9
	SMI	04H
	PLO	R9
CFP1A	LDA	R3
	PHI	RF
	LDA	R3
	PLO	RF
CPFP1A	LDA	RF
	PHI	R7
	LDA	RF
	PLO	RC
	BR	CIGA
CSP2A	GLO	R9
	ADI	04H
	PLO	R9
CFP2A	LDA	R3
	PHI	RF
	LDA	R3
	PLO	RF
	BR	CPFP2A
CSP3A	GLO	R9
	ADI	04H
	PLO	R9
	GHI	R2
	PHI	RF
	GLO	R2
	PLO	RF
	INC	RF
CPFP2A	LDA	RF
	PHI	R8
	LDA	RF
	PLO	R8
	BR	CIGA
CF12A	GHI	R7
	PHI	R8
	GLO	RC
	PLO	R8
CI12A	GHI	R9
	PHI	RF
	GLO	R9
	PLO	RF
	ADI	04H
	PLO	R9
CIGA	LDA	RF
	STR	R9
	INC	R9
	LDA	RF
	STR	R9
	INC	R9
	LDA	RF
	STR	R9
	INC	R9
	LDA	RF
	STR	R9
	DEC	R9
	DEC	R9
	DEC	R9
	LBR	DDDONF

;......................................................
;	..COMPARE ROUTINE FOR DD ROUTINE..
;	..CMPR.A
;	........
CMPR3	LDI	01H
	LSKP
CMPR1	LDI	81H
	BR	CMPR2
CMEXA	GLO	RC
	STR R2
	GLO	R8
	SD
	BM	CMPR1
	BNZ	CMPR3
	GLO	R9
	STR	R2
	SMI	04H
	PLO	RF
	GHI	R9
	PHI	RF
	SEX	R9
	LDA	RF
	SM
	BM	CMPR1
	BNZ	CMPR3
	INC	R9
	LDA	RF
	SM
	BM	CMPR4
	BNZ	CMPR5
	INC	R9
	LDA	RF
	SM
	BM	CMPR4
	BNZ	CMPR5
	INC	R9
	LDN	RF
	SM
	BM	CMPR4
	SHR
	BNZ	CMPR5
	LSKP
CMPR5	LDI	01H
	LSKP
CMPR4	LDI	81H
	PHI	RF
	LDN	R2
	PLO	R9
	GHI	RF
CMPR2	LBR	DDDONE

;......................................................
;	..DD SAVE REGISTER ROUTINE..
;	..REGSVA.A
;	..........
REGSVA	SEX	R2
	DEC	R2
	LDI	0D3H
	STXD
	LDN	R3
REGSV1	ADI	20H
	STXD
	LDI	0DDH
	STXD
	LDN	R3
	STR	R2
	SEP	R2
	DEC	R2
	STXD
	LDI	0F8H
	STXD
	LDN	R3
	ADI	30H
	STXD
	LDI	0DDH
	STXD
	LDA	R3
	ADI	10H
	STR	R2
	SEP	R2
	DEC	R2
	STXD
	LDI	0F8H
	STXD
	LDN	R3
	BNZ	REGSV1
	INC	R3
	LBR	DDDONF

;......................................................
;	..EXIT COMMAND..
;	..EXIT.A
;	........
EXIT	INC	R2
	INC	R2
	INC	R2
	INC	R2
	LDN	R2
	XRI	HIGH FOR4
	BZ	EXIT1
	LDN	R2
	XRI	HIGH GOSUB4
	BZ	EXIT2
	SEP	R4
	DW	ERROR
	DB	0AH
EXIT1	LDI	10H
	LSKP
EXIT2	LDI	04H
EXIT3	PLO	RF
	INC	R2
	DEC	RF
	GLO	RF
	BNZ	EXIT3+1
	LBR	GOTO
;......................................................
;	..EVALUATE ARGUMENT..
;	..EVARG.A
;	.....................
EVARG	LDI	00H
	LSKP
IEVARG	LDI	01H
EVARG3	PLO	RF
	LDA	RB
	XRI	0D6H
	BZ	EVARG1
EVARG2	SEP	R4
	DW	ERROR
	DB	05H
EVARG1	GLO	RF
	BZ	EVARG4
	SEP	R4
	DW	IEXPR
	BR	EVARG5
EVARG4	SEP	R4
	DW	EXPR
EVARG5	LDA	RB
	XRI	0C4H
	BNZ	EVARG2
	SEP	R5
;......................................................
;	..TEMP NUMBER OUTPUT..
;	..TNUM.A
;	........
ATNUM	ADI	30H
TNUM	STR	RE
	INC	RE
	LDI	00H
	STR	RE
	SEP	R5
;......................................................
;	..CONVERSION FROM D/R OR R/D..
;	..DEGRAD.A
;	..........
RD11	LDI	01H
	LSKP
DR	LDI	00H
	SEP	RD
	DB	1CH
	SEP	R5
;added line
	STXD		;NO IDEA WHY THIS IS THE LAST BYTE

	END
