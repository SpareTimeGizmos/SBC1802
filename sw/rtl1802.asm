;	.TITLE	 Run Time Library for Spare Time Gizmos SBC1802

;        d8888b. d888888b db       db .d888b.  .d88b.  .d888b. 
;        88  `8D `~~88~~' 88      o88 88   8D .8P  88. VP  `8D 
;        88oobY'    88    88       88 `VoooY' 88  d'88    odD' 
;        88`8b      88    88       88 .d~~~b. 88 d' 88  .88'   
;        88 `88.    88    88booo.  88 88   8D `88  d8' j88.    
;        88   YD    YP    Y88888P  VP `Y888P'  `Y88P'  888888D 

; *******************************************************************
; *** This software is copyright 2005 by Michael H Riley          ***
; *** You have permission to use, modify, copy, and distribute    ***
; *** this software so long as this copyright notice is retained. ***
; *** This software may not be used in commercial applications    ***
; *** without express written permission from the author.         ***
; *******************************************************************

;++
;   This file is part of an ElfOS compatible BIOS for the SBC1802.  Mike's
; original BIOS contained a number of routines which were not hardware or
; implementation specific.  This includes things like string functions (strcpy,
; memcpy, strcmp, ltrim, isalnum, etc), hexadecimal and decimal conversions
; (hexin, hexout, atoi, uintout, ...), parser functions (findtkn, idnum, ...)
; and arithmetic functions (mul16, div16).  These are essentially a resident
; runtime library for ElfOS programs, and have no hardware dependencies beyond
; an 1802 processor.  I've extracted all these routines from Mike's original
; BIOS source and have grouped them together in this file.  All this code was
; written by Mike Riley and belongs to him, and his original copyright notice
; is reproduced above.
;
;   For the most part I haven't changed any of Mike's code, HOWEVER there are
; a few changes -
;
;  * the order of things has been swapped around, so similar functions can be
;    grouped together.
;
;  * all the short branch instructions have been converted to the eqivalent
;    long branch forms.  This makes the code much less temperamental about
;    crossing pages.
;
;  * the subroutine calls and returns have been converted to use the CALL and
;    RETURN macros from sbc1802.inc.
;
;   Lastly, notice that the rest of code uses the P1, P2, T1, T2, etc register
; names however Mike's code still uses absolute register numbers.  All of the
; "standard" registers, 6 and below, mesh perfectly and the others map pretty
; reasonably.  Here's a summary -
;
;	R0/PC0   - hardware DMA pointer, startup PC
;	R1/INTPC - hardware interrupt PC
;	R2/SP    - stack pointer
;	R3/PC    - program counter
;	R4/CALLPC - program counter for SCRT CALL routine
;	R5/RETPC  -  "   "   "   "   "  SCRT RETURN  "
;	R6/A	  - subroutine return address/inline argument pointer
;	R7/T1     - temporary #1
;	R8/T2     - temporary #2
;	R9/T3     - temporary #3
;	RA/DP     - our data page pointer (not used much by Mike's code)
;	RB/P4     - parameter #4
;	RC/P3     - parameter #3
;	RD/P2	  - parameter #2
;	RE/BAUD   - temporary register for bit banged console I/O
;	RF/P1     - parameter #1

;				Bob Armstrong / Spare Time Gizmos 22-SEP-2021
;--
;0000000001111111111222222222233333333334444444444555555555566666666667777777777
;1234567890123456789012345678901234567890123456789012345678901234567890123456789

	.EJECT
;	.SBTTL	Hexadecimal Conversions

; *********************************************
; *** Convert a binary number to hex output ***
; *** RD - Number to convert                ***
; *** RF - Buffer for output                ***
; *** Returns: RF - next buffer position    ***
; ***          RD - consumed                ***
; *********************************************
hexout2:   glo     rd                  ; move low byte to high
           phi     rd
           ldi     2                   ; 2 nybbles to display
           lskp                        ; skip over the 4
hexout4:   ldi     4                   ; 4 nybbles to display
hexoutlp:  stxd                        ; save the count
           ldi     0                   ; zero the temp var
           plo     re
           ldi     4                   ; perform 4 shift
hexoutl2:  stxd                        ; save count
           glo     rd                  ; perform shift
           shl
           plo     rd
           ghi     rd
           shlc
           phi     rd
           glo     re
           shlc
           plo     re
           irx                         ; point back to count
           ldi     1                   ; need to decrement it
           sd
           lbnz    hexoutl2            ; jump if more shifts needed
           glo     re                  ; get nybble
           smi     10                  ; compare to 10
           lbdf    hexoutal            ; jump if alpha
           glo     re                  ; get value
           adi     30h                 ; convert to ascii
hexoutl3:  str     rf                  ; store value into buffer
           inc     rf
           irx                         ; point to count
           ldi     1                   ; need to subtract 1 from it
           sd
           lbnz    hexoutlp            ; loop if not done
           RETURN                      ; return to caller
hexoutal:  glo     re                  ; get value
           adi     55                  ; convert to ascii
           lbr     hexoutl3            ; and continue

; ***************************************************************
; *** Function to convert hex input characters to binary      ***
; *** RF - Pointer to characters                              ***
; *** Returns - RF - First character that is not alphanumeric ***
; ***           RD - Converted number                         ***
; ***************************************************************
hexin:     ldi     0                   ; set initial total
           phi     rd
           plo     rd
tobinlp:   lda     rf                  ; get input character
           smi     '0'                 ; convert to binary
           lbnf    tobindn             ; jump if termination
           stxd
           ani     0f0h                ; check for alpha
           irx                         ; point back
           lbz     isnumeric
           ldx                         ; recover byte
           smi     49                  ; see if lowercase
           lbnf    hexgo
           ldx                         ; get byte
           smi     32                  ; convert to uppercase
           lbr     hexgo2              ; and continue
hexgo:     ldx                         ; recover byte
hexgo2:    smi     7                   ; offset
           lbr     tobingo             ; and continue
isnumeric: ldx                         ; recover byte
           smi     10                  ; check for end of numbers
           lbdf    tobindn             ; jump if end
           ldx                         ; recover byte
tobingo:   stxd                        ; save number
           smi     16                  ; check for valid range
           lbnf    tobingd             ; jump if good
           irx                         ; remove number from stack
           lbr     tobindn
tobingd:   ldi     4                   ; need to multiply by 16
tobinglp:  stxd
           glo     rd                  ; multiply by 2
           shl
           plo     rd
           ghi     rd
           shlc
           phi     rd
           irx
           ldi     1
           sd
           lbnz    tobinglp
           irx                         ; point to new number
           glo     rd                  ; and add to total
           add
           plo     rd
           ghi     rd
           adci    0
           phi     rd
           lbr     tobinlp             ; loop back for next character
tobindn:   dec     rf                  ; move back to terminating character
           RETURN                      ; return to caller

	.EJECT
;	.SBTTL	Decimal Conversions

; *** rf - pointer to ascii string
; *** returns: rf - first non-numeric character
; ***          RD - number
; ***          DF = 1 if first character non-numeric 
atoi:      ldi     0                   ; clear answer
           phi     rd
           plo     rd
           plo     re                  ; signify positive number
           ldn     rf                  ; get first value
           CALL(isnum)                 ; check if numeric
           lbdf    atoicnt             ; jump if so
           xri     '-'                 ; check for minus
           lbz     atoicnt             ; jump if so
           smi     0                   ; signal number error
           RETURN                      ; return to caller
atoicnt:   ldn     rf                  ; get first bytr
           xri     '-'                 ; check for negative
           lbnz    atoilp              ; jump if not negative
           ldi     1                   ; signify negative number
           plo     re
           inc     rf                  ; move past - sign
atoilp:    ldn     rf                  ; get byte from input
           CALL(isnum)                 ; check for number
           lbnf    atoidn              ; jump if not
           ghi     rd                  ; make a copy for add
           stxd
           glo     rd                  ; multiply by 2
           stxd                        ; TOS now has copy of number
           CALL(mul2)                  ; multiply by 2
           CALL(mul2)                  ; multiply by 4
           irx                         ; point to adds
           glo     rd                  ; multiply by 5 (add TOS)
           add
           plo     rd
           irx                         ; point to msb
           ghi     rd
           adc
           phi     rd
           CALL(mul2)                  ; multiply by 10
           lda     rf                  ; get byte from buffer
           smi     '0'                 ; convert to binary
           str     r2                  ; prepare for addition
           glo     rd                  ; add in new digit
           add
           plo     rd
           ghi     rd
           adci    0
           phi     rd
           lbr     atoilp              ; loop back for next character
atoidn:    adi     0                   ; signal valid number
           RETURN                      ; return to caller
mul2:      glo     rd                  ; multiply number by 2
           shl
           plo     rd
           ghi     rd
           shlc
           phi     rd
           RETURN                      ; and return

; **** convert binary number to ascii
; **** RD - number to convert
; **** RF - buffer to store
; **** Returns: RF - last postion+1
uintout:   lbr      positive
intout:    sex     r2                  ; point X to stack
           ghi     rd                  ; get high of number
           ani     128                 ; mask all bit sign bit
           lbz      positive            ; jump if number is positive
           ldi     '-'                 ; need a minus sign
           str     rf                  ; store into output
           inc     rf
           glo     rd                  ; get low byte
           str     r2                  ; store it
           ldi     0                   ; need to subtract from 0
           sm
           plo     rd                  ; put back
           ghi     rd                  ; get high byte
           str     r2                  ; place into memory
           ldi     0                   ; still subtracting from zero
           smb     
           phi     rd                  ; and put back 
positive:  glo     r7                  ; save consumed registers
           stxd
           ghi     r7
           stxd
           glo     r8                  ; save consumed registers
           stxd
           ghi     r8
           stxd
           glo     r9                  ; save consumed registers
           stxd
           ghi     r9
           stxd
           ldi     HIGH(numbers)       ; point to numbers
           phi     r9
           ldi     LOW(numbers)
           plo     r9
           lda     r9                  ; get first division
           phi     r7
           lda     r9
           plo     r7
           ldi     0                   ; leading zero flag
           stxd                        ; store onto stack
nxtiter:   ldi     0                   ; star count at zero
           plo     r8                  ; place into low of r8
divlp:     glo     r7                  ; get low of number to subtrace
           str     r2                  ; place into memory
           glo     rd                  ; get low of number
           sm                          ; subtract
           phi     r8                  ; place into temp space
           ghi     r7                  ; get high of subtraction
           str     r2                  ; place into memory
           ghi     rd                  ; get high of number
           smb                         ; perform subtract
           lbnf    nomore              ; jump if subtraction was too large
           phi     rd                  ; store result
           ghi     r8
           plo     rd
           inc     r8                  ; increment count
           lbr     divlp               ; and loop back
nomore:    irx                         ; point back to leading zero flag
           glo     r8
           lbnz    nonzero             ; jump if not zero
           ldn     r2                  ; get flag
           lbnz    allow0              ; jump if no longer zero
           dec     r2                  ; keep leading zero flag
           lbr     findnxt             ; skip output
allow0:    ldi     0                   ; recover the zero
nonzero:   adi     30h                 ; convert to ascii
           str     rf                  ; store into buffer
           inc     rf
           ldi     1                   ; need to set leading flag
           stxd                        ; store it
findnxt:   dec     r7                  ; subtract 1 for zero check
           glo     r7                  ; check for end
           lbz     intdone             ; jump if done
           lda     r9                  ; get next number
           phi     r7
           lda     r9
           plo     r7
           smi     1                   ; see if at last number
           lbnz    nxtiter             ; jump if not 
           irx                         ; set leading flag
           ldi     1
           stxd
           lbr     nxtiter
intdone:   irx                         ; put x back where it belongs
           irx                         ; recover consumed registers
           ldxa
           phi     r9
           ldxa
           plo     r9
           ldxa
           phi     r8
           ldxa
           plo     r8
           ldxa
           phi     r7
           ldx
           plo     r7
           RETURN                      ; return to caller

numbers:   .BYTE   027h,010h,3,0e8h,0,100,0,10,0,1

; ****************************************************
; *** Output 2 digit decimal number with leading 0 ***
; *** D - value to output                          ***
; *** RF - buffer to write value to                ***
; ****************************************************
intout2:
        str     r2              ; save value for a moment
        ldi     0               ; setup count
        plo     re
        ldn     r2              ; retrieve it
intout2lp:
        smi     10              ; subtract 10
        lbnf    intout2go       ; jump if too small
        inc     re              ; increment tens
        lbr     intout2lp       ; and keep looking
intout2go:
        adi     10              ; make positive again
        str     r2              ; save units
        glo     re              ; get tens
        adi     '0'             ; convert to ascii
        str     rf              ; store into buffer
        inc     rf
        ldn     r2              ; recover units
        adi     '0'             ; convert to ascii
        str     rf              ; and store into buffer
        inc     rf
        RETURN                  ; return to caller

;	.SBTTL	String Functions

; **** Strcmp compares the strings pointing to by R(D) and R(F)
; **** Returns:
; ****    R(F) = R(D)     0
; ****    R(F) < R(D)     -1 (255)
; ****    R(F) > R(D)     1
strcmp:  lda     rd          ; get next byte in string
         ani     0ffh        ; check for zero
         lbz     strcmpe     ; found end of first string
         stxd                ; store into memory
         irx
         lda     rf          ; get byte from first string
         sm                  ; subtract 2nd byte from it
         lbz     strcmp      ; so far a match, keep looking
         lbnf    strcmp1     ; jump if first string is smaller
         ldi     1           ; indicate first string is larger
         lskp                ; and return to caller
strcmp1: ldi     255         ; return -1, first string is smaller
         RETURN              ; return to calelr
strcmpe: lda     rf          ; get byte from second string
         lbz     strcmpm     ; jump if also zero
         ldi     1           ; first string is smaller (returns -1)
         RETURN              ; return to caller
strcmpm: ldi     0           ; strings are a match
strret:	 RETURN              ; return to caller

; **** ltrim trims leading white space from string pointed to by R[F]
; **** Returns:
; ****    R(F) pointing to non-whitespace portion of string
ltrim:   ldn     rf          ; get next byte from string
         lbz    strret      ; return if at end of string
         smi     ' '+1       ; looking for anthing <= space
         lbdf    strret      ; found first non white-space
         inc     rf          ; point to next character
         lbr     ltrim       ; keep looking

; **** strcpy copies string pointed to by R[F] to R[D]
strcpy:  lda    rf           ; get byte from source string
         str    rd           ; store into destination
         lbz    strret       ; return if copied terminator
         inc    rd           ; increment destination pointer
         lbr    strcpy       ; continue looping

; **** memcpy copies R[C] bytes from R[F] to R[D]
memcpy:  glo    rc           ; get low count byte
         lbnz   memcpy1      ; jump if not zero
         ghi    rc           ; get high count byte
         lbz    strret       ; return if zero
memcpy1: lda    rf           ; get byte from source
         str    rd           ; store into destination
         inc    rd           ; point to next destination position
         dec    rc           ; decrement count
         lbr    memcpy       ; and continue copy

	.EJECT
;	.SBTTL	Lexical functions

; ********************************
; *** See if D is alphabetic   ***
; *** Returns DF=0 - not alpha ***
; ***         DF=1 - is alpha  ***
; ********************************
isalpha:   plo     re                  ; save copy of do
           smi     'A'                 ; check uc letters
           lbnf    fails               ; jump if below A
           smi     27                  ; check upper range
           lbnf    passes              ; jump if valid
           glo     re                  ; recover character
           smi     'a'                 ; check lc letters
           lbnf    fails               ; jump if below A
           smi     27                  ; check upper range
           lbnf    passes              ; jump if valid
           lbr     fails

; **********************************
; *** check D if hex             ***
; *** Returns DF=1 - hex         ***
; ***         DF=0 - non-hex     ***
; **********************************
ishex:     CALL(isnum)                 ; see if it is numeric
           plo     re                  ; keep a copy
           lbdf    passes              ; jump if it is numeric
           smi     'A'                 ; check for below uppercase a
           lbnf    fails               ; value is not hex
           smi     6                   ; check for less then 'G'
           lbnf    passes              ; jump if so
           glo     re                  ; recover value
           smi     'a'                 ; check for lowercase a
           lbnf    fails               ; jump if not
           smi     6                   ; check for less than 'g'
           lbnf    passes              ; jump if so
           lbr     fails

; *****************************************
; *** See if D is alphanumeric          ***
; *** Returns: DF=0 - not valid         ***
; ***          DF=1 - is valid          ***
; *****************************************
isalnum:   plo     re                  ; keep copy of D
           CALL(isnum)                 ; check if numeric
           lbdf    passes              ; jump if numeric
           CALL(isalpha)               ; check for alpha
           lbdf    passes              ; jump if alpha
           lbr     fails               ; otherwise fails

; *************************************
; *** Check if character is numeric ***
; *** D - char to check             ***
; *** Returns DF=1 if numeric       ***
; ***         DF=0 if not           ***
; *************************************
isnum:     plo     re                  ; save a copy
           smi     '0'                 ; check for below zero
           lbnf    fails               ; jump if below
           smi     10                  ; see if above
           lbdf    fails               ; fails if so
passes:    smi     0                   ; signal success
           lskp
fails:     adi     0                   ; signal failure
           glo     re                  ; recover character
           RETURN                      ; and return

err:       smi     0                   ; signal an error
           RETURN                      ; and return

; ***********************************
; *** Check for symbol terminator ***
; *** Returns: DF=1 - terminator  ***
; ***********************************
isterm:    CALL(isalnum)               ; see if alphanumeric
           lbdf    fails               ; fails if so
           lbr     passes

	.EJECT
;	.SBTTL	16 Bit Multiply and Divide

; *** RC:RB = RF * RD (RB is low word)
; *** R(X) must point to suitable stack
mul16:     ldi     0                   ; zero out total
           phi     rb
           plo     rb
           phi     rc
           plo     rc
           sex     r2                  ; make sure X points to stack
mulloop:   glo     rd                  ; get low of multiplier
           lbnz    mulcont             ; continue multiplying if nonzero
           ghi     rd                  ; check hi byte as well
           lbnz    mulcont
           RETURN                      ; return to caller
mulcont:   ghi     rd                  ; shift multiplier
           shr     
           phi     rd
           glo     rd
           shrc    
           plo     rd
           lbnf    mulcont2            ; loop if no addition needed
           glo     rf                  ; add F to C:B
           str     r2
           glo     rb
           add     
           plo     rb
           ghi     rf
           str     r2
           ghi     rb
           adc
           phi     rb
           glo     rc                  ; carry into high word
           adci    0
           plo     rc
           ghi     rc
           adci    0
           phi     rc
mulcont2:  glo     rf                  ; shift first number
           shl
           plo     rf
           ghi     rf
           shlc
           phi     rf
           lbr     mulloop             ; loop until done

; *** RB = RF/RD
; *** RF = Remainder
; *** uses R8 and R9
div16:     ldi     0                   ; clear answer
           phi     rb
           plo     rb
           phi     r8                  ; set additive
           plo     r8
           inc     r8
           glo     rd                  ; check for divide by 0
           lbnz    d16lp1
           ghi     rd
           lbnz    d16lp1
           ldi     0ffh                ; return 0ffffh as div/0 error
           phi     rb
           plo     rb
divret:    RETURN     
d16lp1:    ghi     rd                  ; get high byte from r7
           ani     128                 ; check high bit
           lbnz    divst               ; jump if set
           glo     rd                  ; lo byte of divisor
           shl                         ; multiply by 2
           plo     rd                  ; and put back
           ghi     rd                  ; get high byte of divisor
           shlc                        ; continue multiply by 2
           phi     rd                  ; and put back
           glo     r8                  ; multiply additive by 2
           shl
           plo     r8
           ghi     r8
           shlc
           phi     r8
           lbr     d16lp1              ; loop until high bit set in divisor
divst:     glo     r8                  ; get low of divisor
           lbnz    divgo               ; jump if still nonzero
           ghi     r8                  ; check hi byte too
           lbz     divret              ; jump if done
divgo:     ghi     rf                  ; copy dividend
           phi     r9
           glo     rf
           plo     r9
           glo     rd                  ; get lo of divisor
           stxd                        ; place into memory
           irx                         ; point to memory
           glo     rf                  ; get low byte of dividend
           sm                          ; subtract
           plo     rf                  ; put back into r6
           ghi     rd                  ; get hi of divisor
           stxd                        ; place into memory
           irx                         ; point to byte
           ghi     rf                  ; get hi of dividend
           smb                         ; subtract
           phi     rf                  ; and put back
           lbdf    divyes              ; branch if no borrow happened
           ghi     r9                  ; recover copy
           phi     rf                  ; put back into dividend
           glo     r9
           plo     rf
           lbr     divno               ; jump to next iteration
divyes:    glo     r8                  ; get lo of additive
           stxd                        ; place in memory
           irx                         ; point to byte
           glo     rb                  ; get lo of answer
           add                         ; and add
           plo     rb                  ; put back
           ghi     r8                  ; get hi of additive
           stxd                        ; place into memory
           irx                         ; point to byte
           ghi     rb                  ; get hi byte of answer
           adc                         ; and continue addition
           phi     rb                  ; put back
divno:     ghi     rd                  ; get hi of divisor
           shr                         ; divide by 2
           phi     rd                  ; put back
           glo     rd                  ; get lo of divisor
           shrc                        ; continue divide by 2
           plo     rd
           ghi     r8                  ; get hi of divisor
           shr                         ; divide by 2
           phi     r8                  ; put back
           glo     r8                  ; get lo of divisor
           shrc                        ; continue divide by 2
           plo     r8
           lbr     divst               ; next iteration

	.EJECT
;	.SBTTL	Date and Time Functions

; ************************************
; *** Convert packed date to ascii ***
; *** RD - pointer to packed date  ***
; *** RF - pointer to buffer       ***
; ************************************
dttoas: lda     rd              ; retrieve month
        CALL(intout2)           ; output it
        ldi     '/'             ; now a slash
        str     rf              ; store into buffer
        inc     rf
        lda     rd              ; retrieve day
        CALL(intout2)           ; output it
        ldi     '/'             ; now a slash
        str     rf              ; store into buffer
        inc     rf
        lda     rd              ; [RLA] get year
        stxd                    ; save contents of RD
        adi     180             ; add base of 1972
        plo     rd
        ghi     rd              ; save high byte
        stxd
        ldi     7               ; high byte of 1972
        adci    0               ; propagate the carry
        phi     rd
        CALL(f_uintout)         ; output the year
        ldi     0               ; write terminator
        str     rf
        irx                     ; recover rd
        ldxa
        phi     rd
        ldx
        plo     rd
        RETURN                  ; and return

; ************************************
; *** Convert packed time to ascii ***
; *** RD - pointer to packed time  ***
; *** RF - pointer to buffer       ***
; ************************************
tmtoas: ldi     ' '             ; space after year
        str     rf              ; store into buffer
        inc     rf
        lda     rd              ; get hour
        CALL(intout2)           ; output it
        ldi     ':'             ; now a colon
        str     rf              ; store into buffer
        inc     rf
        lda     rd              ; get minutes
        CALL(intout2)           ; output it
        ldi     ':'             ; now a colon
        str     rf              ; store into buffer
        inc     rf
        lda     rd              ; get seconds
        CALL(intout2)           ; output it
        ldi     0
        str     rf              ; write terminator
        RETURN                  ; and return

; ************************************
; *** Convert ascii date to packed ***
; *** RF - pointer to ascii date   ***
; *** RA - destination address     ***
; ************************************
astodt: glo     rd              ; save consumed register
        stxd
        ghi     rd
        stxd
        CALL(atoi)              ; convert first number
        glo     rd              ; get value
        smi     13              ; see if valid month
        lbdf    dterr           ; jump if invalid
        glo     rd              ; recover number
        str     ra              ; store into output
        inc     ra              ; and increment
        lda     rf              ; get next char
        smi     '/'             ; must be a slash
        lbnz   dterr
        CALL(atoi)              ; get next number
        glo     rd              ; get value
        smi     32              ; check next number
        lbdf    dterr           ; jump if invalid
        glo     rd              ; recover value
        str     ra              ; and save in output
        inc     ra              ; and increment
        lda     rf              ; get next char
        smi     '/'             ; must be a slash
        lbnz    dterr           ; jump if not
        CALL(atoi)              ; now convert year
        ghi     rd              ; get high byte
        lbnz    y4              ; jump if 4 digit year
        glo     rd              ; need to check y2k
        smi     72              ; need to check for y2k threshold
        lbnf    y2k             ; jump if 2000+
asdtend:
        str     ra              ; store into output
        inc     ra              ; move to next position
        adi     0               ; signal valid date
        lbr     get_rd          ; recover RD and return
y4:     glo     rd              ; subtract 1972
        smi     180
        lbr     asdtend         ; and store
y2k:    glo     rd              ; recover value
        adi     28              ; add in offset
        lbr     asdtend         ; and continue processing

; ************************************
; *** Convert ascii time to packed ***
; *** RF - pointer to ascii time   ***
; *** RA - destination address     ***
; ************************************
astotm: glo     rd              ; save consumed register
        stxd
        ghi     rd
        stxd
        CALL(ltrim)             ; move past any spaces
        CALL(atoi)              ; convert hours
        glo     rd              ; check if valid
        smi     24
        lbdf    dterr           ; jump if not
        glo     rd              ; store value into result
        str     ra
        inc     ra
        lda     rf              ; get next char
        smi     ':'             ; must be a colon
        lbnz    dterr           ; jump if not
        CALL(atoi)              ; convert minutes
        glo     rd              ; check for valid
        smi     60
        lbdf    dterr           ; jump if not
        glo     rd              ; store value into result
        str     ra
        inc     ra
        lda     rf              ; get next char
        plo     re              ; keep a copy
        smi     ':'             ; check for colon
        lbz     is_sec          ; jump if seconds are provided
        glo     re              ; need to check for valid characters
        lbz     no_sec          ; jump if terminator
        smi     32              ; otherwise must be a space
        lbnz    dterr           ; jump on other chars to err
no_sec: ldi     0               ; set seconds to zero
tm_cont:str     ra
        adi     0               ; signal no error
get_rd: irx                     ; recover consumed register
        ldxa
        phi     rd
        ldx
        plo     rd
        RETURN                  ; and return
is_sec: CALL(atoi)              ; convert seconds
        glo     rd              ; check for valid
        smi     60
        lbdf    dterr           ; jump if not valid
        glo     rd              ; store answer and return
        lbr     tm_cont
dterr:  smi     0               ; signal an error
        lbr     get_rd          ; recover RD and return

	.EJECT
;	.SBTTL	Identify Symbol Type

; ***********************************************
; *** identify symbol as decimal, hex, or non ***
; *** RF - pointer to symbol                  ***
; *** Returns: D=0 - decimal number           ***
; ***          D=1 - hex number               ***
; ***          DF=1 - non numeric             ***
; ***          DF=0 - is numeric              ***
; ***********************************************
idnum:     glo     rf                  ; save position
           stxd
           ghi     rf
           stxd
           ldn     rf                  ; get first byte
           CALL(isnum)                 ; must be numeric
           lbdf    idlp1               ; jump if it was
idnumno:   smi     0                   ; signal non-numeric
           lskp
idnumyes:  adi     0                   ; signal numeric
           plo     re                  ; save number
           irx                         ; recover RF
           ldxa
           phi     rf
           ldx
           plo     rf
           glo     re                  ; recover number
return:    RETURN                      ; and return to caller
idlp1:     lda     rf                  ; get next byte
           CALL(isterm)                ; check for symbol terminator
           lbdf    iddec               ; signal decimal number
           CALL(isnum)                 ; see if char is numeric
           lbdf    idlp1               ; jump if so
           dec     rf                  ; move back to char
idlp2:     lda     rf                  ; get next byte
           CALL(isterm)                ; see if terminator
           lbdf    idnumno             ; jump if term found before h
           CALL(ishex)                 ; check for hex character
           lbdf    idlp2               ; loop back if so
           smi     'H'                 ; check for final H
           lbz     idhex               ; jump if hex
           smi     32                  ; check for h
           lbz     idhex
           lbr     idnumno             ; was not proper number
iddec:     ldi     0                   ; signal decimal number
           lbr     idnumyes            ; and return
idhex:     ldi     1                   ; signal hex number
           lbr     idnumyes            ; and return

	.EJECT
;	.SBTTL	Search symbol table

; ******************************************
; *** Check if symbol is in symbol table ***
; *** RF - pointer to ascii symbol       ***
; *** R7 - pointer to token table        ***
; *** Returns: RD - function number      ***
; ***          DF=1 - is function        ***
; ***          DF=0 - is not a function  ***
; ******************************************
tokenfunc: glo     r7                  ; save position of R7
           stxd
           ghi     r7
           stxd
           glo     rb                  ; save other consumed register
           stxd
           ghi     rb
           stxd
           ldi     0                   ; setup function number
           plo     rd
           ghi     rf                  ; save buffer position
           phi     rb
           glo     rf
           plo     rb
tfloop:    ldn     r7                  ; see if at last token
           lbnz    tfgo                ; jump if not
           adi     0                   ; signal symbol not found
tfreturn:  irx                         ; recover consumed registers
           ldxa
           phi     rb
           ldxa
           plo     rb
           ldxa
           phi     r7
           ldx
           plo     r7
           RETURN                      ; and return
tfgo:      ldn     r7                  ; get token byte
           ani     080h                ; see if last one
           lbnz    tflast              ; jump if it was
           lda     r7                  ; get byte from function table
           str     r2                  ; setup compare
           lda     rf                  ; get byte from input
           sm                          ; and compare
           lbz     tfgo                ; loop back if match
tfnolp:    lda     r7                  ; need to find end of token
           ani     080h
           lbz     tfnolp              ; loop until found
tfno:      inc     rd                  ; increment function number
           ghi     rb                  ; restore buffer position
           phi     rf
           glo     rb
           plo     rf
           lbr     tfloop              ; loop to check next token
tflast:    lda     r7                  ; get byte from token
           ani     07fh                ; strip high bit
           str     r2                  ; store for compare
           lda     rf                  ; get byte from buffer
           sm                          ; and see if a match
           lbnz    tfno                ; jump if not
           smi     0                   ; signal match found
           lbr     tfreturn            ; and return

	.EJECT
; 	.SBTTL

; ***********************************
; *** Check for valid boot loader ***
; ***   Return DF=0 if valid      ***
; ***********************************
btcheck:   ldi     1                   ; point to boot code
           phi     rf
           ldi     0
           plo     rf
           ldi     041h                ; number of bytes to check
           plo     rc
           ldi     0                   ; setup initial value
           plo     rd
           sex     rf                  ; point X to boot code
btchk_lp:
           glo     rd                  ; get value
           add                         ; add in next byte
           irx                         ; move pointer
           shl                         ; shift high byte into DF
           shr                         ; move bits back
           shlc                        ; ring shift now done
           plo     rd                  ; save value
           dec     rc                  ; decrement byte count
           glo     rc                  ; see if done
           lbnz    btchk_lp            ; loop back if not
           sex     r2                  ; point X back to stack
           glo     rd                  ; get number
           smi     060h                ; check against check value
           lbnz    btcerr              ; jump on mismatch to error
           adi     0                   ; DF=0 to signal good
           RETURN                      ; and return
btcerr:    smi     0                   ; DF=1 if invalid bootstrap
           RETURN
