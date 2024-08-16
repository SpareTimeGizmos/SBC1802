; *******************************************************************
; *** This software is copyright 2020 by Michael H Riley          ***
; *** You have permission to use, modify, copy, and distribute    ***
; *** this software so long as this copyright notice is retained. ***
; *** This software may not be used in commercial applications    ***
; *** without express written permission from the author.         ***
; *******************************************************************

; XMODEM data segment
XECHO:      equ     base+0
XINIT:      equ     base+1
; don't change the order of XBLOCK, XCOUNT
XBLOCK:     equ     base+2            ; current block
XCOUNT:     equ     base+3            ; byte send/receive count
XDONE:     equ     base+4
XH1:        equ     base+5
XH2:        equ     base+6
;XH3:        equ     base+7
XTXRX:      equ     base+8            ; buffer for tx/rx
;XTEMP1:     equ     base+150
;XTEMP2:     equ     base+152
XBUFFER:    equ     base+154          ; address for input buffer

; XMODEM protocol constants ...
ACK:       equ     06h
NAK:       equ     15h
SOH:       equ     01h
ETX:       equ     03h
EOT:       equ     04h
CAN:       equ     18h
CSUB:      equ     1ah


; *******************************************
; ***** Open XMODEM channel for writing *****
; *******************************************
XOPENW:	PUSHR(T1)		; save consumed register
	RLDI(T1,XBLOCK)		; current block number
	LDI 1\ STR T1		; starts at 1
	INC T1\ LDI 0\ STR T1	; set byte count to zero
	RLDI(T1,XECHO)		; save the current console echo flag
	GHI BAUD\ STR T1	; ...
	ANI $FE\ PHI BAUD	; and then turn off echo
XOPENW1:CALL(F_READ)		; read a byte from the serial port
	SMI NAK			; wait for the host to send a NAK
	LBNZ XOPENW1		; just wait forever until they do
	IRX\ POPRL(T1)		; restore T1
	RETURN			; and we're done


; ***********************************
; ***** Write to XMODEM channel *****
; ***** P1 - pointer to data    *****
; ***** P2 - Count of data      *****
; ***********************************
XWRITE:	PUSHR(T2)\ PUSHR(T1)	; save the registers we use
	RLDI(T1,XCOUNT)\ LDN T1	; get current count
	STR SP\ PLO T2		; save for ADD
	LDI LOW(XTXRX)\ ADD	; add current byte count
	PLO T1
	LDI HIGH(XTXRX)\ ADCI 0	;
	PHI T1			; T1 now has address
XWRITE1:LDA P1\ STR T1		; retrieve next byte and store in buffer
	INC T1\ INC T2		; increment buffer count
	GLO T2\ ANI $80		; have we done 128 bytes?
	LBZ XWRITE2		; skip if not
	CALL(XSEND)		; send current block
	LDI 0\ PLO T2		; zero buffer byte coune
	RLDI(T1,XTXRX)		; and reset buffer position
XWRITE2:DBNZ(P2,XWRITE1)	; keep going until all bytes are sent
	RLDI(T1,XCOUNT)		; need to write new count
        GLO T2\ STR T1          ; ...
	IRX\ POPR(T1)\ POPRL(T2); restore registers
	RETURN			; and we're done


; *******************************
; ***** Send complete block *****
; *******************************
xsend:     push    P1                 ; save consumed registers
           push    P2
xsendnak:  ldi     SOH                ; need to send SOH chaDPcter
           phi     P2                 ; initial value for checksum
           sep     scall              ; send it
           dw      f_tty
           mov     P1,XBLOCK           ; need current block number
           ldn     P1                 ; get block number
           str     SP                 ; save it
           ghi     P2                 ; get checksum
           add                        ; add in new byte
           phi     P2                 ; put it back
           ldn     SP                 ; recover block number
           sep     scall              ; and send it
           dw      f_tty
           ldn     P1                 ; get block number back
           sdi     255                ; subtract from 255
           str     SP                 ; save it
           ghi     P2                 ; get current checksum
           add                        ; add in inverted block number
           phi     P2                 ; put it back
           ldn     SP                 ; recover inverted block number
           sep     scall              ; send it
           dw      f_tty
           ldi     128                ; 128 bytes to write
           plo     P2                 ; place into counter
           mov     P1,XTXRX            ; point P1 to data block
xsend1:    lda     P1                 ; retrieve next byte
           str     SP                 ; save it
           ghi     P2                 ; get checksum
           add                        ; add in new byte
           phi     P2                 ; save checksum
           ldn     SP                 ; recover byte
           sep     scall              ; and send it
           dw      f_tty
           dec     P2                 ; decrement byte count
           glo     P2                 ; get count
           lbnz    xsend1             ; jump if more bytes to send
           ghi     P2                 ; get checksum byte
           sep     scall              ; and send it
           dw      f_tty    
xsend2:    sep     scall              ; read byte from serial port
           dw      f_read
           str     SP                 ; save it
           smi     NAK                ; was it a NAK
           lbz     xsendnak           ; resend XBLOCK if NAK
           mov     P1,block           ; point to block number
           ldn     P1                 ; get block number
           adi     1                  ; increment block number
           str     P1                 ; and put it back
           inc     P1                 ; point to buffer count
           ldi     0                  ; set buffer count
           str     P1
           pop     P2                 ; recover registers
           pop     P1
           sep     sret               ; and return

; **************************************
; ***** Close XMODEM write channel *****
; **************************************
xclosew:   push    P1                 ; save consumed registers
           push    P2
           mov     P1,XCOUNT           ; get count of characters unsent
           ldn     P1                 ; retrieve count
           lbz     xclosewd           ; jump if no untransmitted characters
           plo     P2                 ; put into count
           str     SP                 ; save for add
           ldi     XTXRX.0             ; low byte of buffer
           add                        ; add characters in buffer
           plo     P1                 ; put into P1
           ldi     XTXRX.1             ; high byte of transmit buffer
           adci    0                  ; propagate carry
           phi     P1                 ; P1 now has position to write at
xclosew1:  ldi     CSUB               ; character to put into buffer
           str     P1                 ; store into transmit buffer
           inc     P1                 ; point to next position
           inc     P2                 ; increment byte count
           glo     P2                 ; get count
           ani     080h               ; need 128 bytes
           lbz     xclosew1           ; loop if not enough
           sep     scall              ; send final block
           dw      xsend
xclosewd:  ldi     EOT                ; need to send EOT
           sep     scall              ; send it
           dw      f_tty
           sep     scall              ; read a byte
           dw      f_read
           smi     06h                ; needs to be an ACK
           lbnz    xclosewd           ; resend EOT if not ACK
           mov     P1,XECHO            ; need to restore echo constant
           ldn     P1                 ; get it
           phi     re                 ; put it back
           pop     P2                 ; recover consumed registers
           pop     P1
           sep     sret               ; and return

; *******************************************
; ***** Open XMODEM channel for reading *****
; *******************************************
xopenr:    push    P1                 ; save consumed registers
           mov     P1,XECHO            ; point to echo constant
           ghi     re                 ; get echo constant
           str     P1                 ; save it
           ani     0feh               ; turn off echo
           phi     re                 ; put it back
           inc     P1                 ; point to init block
           ldi     NAK                ; need to send initial NAK
           str     P1                 ; store it
           inc     P1                 ; point to block number
           ldi     1                  ; expect 1
           str     P1                 ; store it
           inc     P1                 ; point to count
           ldi     128                ; mark as no bytes in buffer
           str     P1                 ; store it
           inc     P1                 ; point to done
           ldi     0                  ; mark as not done
           str     P1
            
           ldi 0                      ; setup inner delay loop
           plo P1
           phi P1
           ldi 010h                   ; setup outer delay loop
           plo re
xopenr1:   dec     P1
           glo     P1
           lbnz    xopenr1
           ghi     P1
           lbnz    xopenr1
           dec     re
           glo     re
           lbnz    xopenr1
           pop     P1                 ; recover consumed register
           sep     sret               ; and return

; ************************************
; ***** Read from XMODEM channel *****
; ***** P1 - pointer to data     *****
; ***** P2 - count of data       *****
; ************************************
xread:     push    DP                 ; save consumed registers
           push    r9
           mov     DP,XCOUNT           ; need current read count
           ldn     DP                 ; get read count
           plo     r9                 ; store it here
           str     SP                 ; store for add
           ldi     XTXRX.0             ; low byte of buffer address
           add                        ; add count
           plo     DP                 ; store into ra
           ldi     XTXRX.01            ; high byte of buffer address
           adci    0                  ; propagate carry
           phi     DP                 ; DP now has address
xreadlp:   glo     r9                 ; get count
           ani     080h               ; need to see if bytes to read
           lbz     xread1             ; jump if so
           sep     scall              ; receive another block
           dw      xrecv
           mov     DP,XTXRX            ; back to beginning of buffer
           ldi     0                  ; zero count
           plo     r9
xread1:    lda     DP                 ; read byte from receive buffer
           str     P1                 ; store into output
           inc     P1
           inc     r9                 ; increment buffer count
           dec     P2                 ; decrement read count
           glo     P2                 ; get low of count
           lbnz    xreadlp            ; loop back if more to read
           ghi     P2                 ; need to check high byte
           lbnz    xreadlp            ; loop back if more
           mov     DP,XCOUNT           ; need to store buffer count
           glo     r9                 ; get it
           str     DP                 ; and store it
           pop     r9                 ; recover used registers
           pop     DP
           sep     sret               ; and return to caller

; ********************************
; ***** Receive XMODEM block *****
; ********************************
xrecv:     push    P1                 ; save consumed registers
           push    P2
xrecvnak:
xrecvlp:   sep     scall              ; receive a byte
           dw      readblk
           lbdf    xrecveot           ; jump if EOT received
           mov     P1,XH2              ; point to received block number
           ldn     P1                 ; get it
           str     SP                 ; store for comparison
           mov     P1,XBLOCK           ; get expected block number
           ldn     P1                 ; retrieve it
           sm                         ; check against received block number
           lbnz    xrecvnak1          ; jump if bad black number
           mov     P1,XTXRX            ; point to first data byte
           ldi     0                  ; checksum starts at zero
           phi     P2
           ldi     128                ; 128 bytes need to be added to checksum
           plo     P2
xrecv1:    lda     P1                 ; next byte from buffer
           str     SP                 ; store for add
           ghi     P2                 ; get checksum
           add                        ; add in byte
           phi     P2                 ; put checksum back
           dec     P2                 ; decrement byte count
           glo     P2                 ; see if done
           lbnz    xrecv1             ; jump if more to add up
           ldn     P1                 ; get received checksum
           str     SP                 ; store for comparison
           ghi     P2                 ; get computed checksum
           sm                         ; and compare
           lbnz    xrecvnak1          ; jump if bad

           mov     P1,XINIT            ; point to init number
           ldi     ACK                ; need to send an ack
           str     P1
           inc     P1                 ; point to block number
           ldn     P1                 ; get block number
           adi     1                  ; increment block number
           str     P1                 ; put it back
           inc     P1                 ; point to count
           ldi     0                  ; no bytes read from this block
           str     P1
xrecvret:  pop     P2                 ; recover consumed registers
           pop     P1
           sep     sret               ; return to caller

xrecvnak1: mov     P1,XINIT            ; point to init byte
           ldi     NAK                ; need a NAK
           str     P1                 ; store it
           lbr     xrecvnak           ; need to have packet resent

xrecveot:  mov     P1,XDONE           ; need to mark EOT received
           ldi     1
           str     P1
           lbr     xrecvret           ; jump to return

; *************************************
; ***** Close XMODEM read channel *****
; *************************************
xcloser:   sep     scall              ; read next block
           dw      readblk
           lbnf    xcloser            ; jump if EOT not received

           mov     P1,XECHO            ; need to restore echo constant
           ldn     P1                 ; get it
           phi     re                 ; put it back
           sep     sret               ; return to caller

;[RLA]   The following code needs to be on a single page, because of the bnf
;[RLA] and bnz instructions in the time sensitive loop.  Assuming this whole
;[RLA] XMODEM module started on a page boundry then there's plenty of room
;[RLA} on the current page, but if you're not so lucky then uncomment the
;[RLA] following ORG statement...
;[RLA]     org     ($+0FFh) & 0FF00h  ;[RLA] move to the start of the next page

; MUST FIX THIS - rd == P2!!!
readblk:   push    P2                 ; save consumed registers
           push    DP
           push    rd
           push    T3
           ldi     132                ; 132 bytes to receive
           plo     DP
           ldi     1                  ; first character flag
           phi     DP

           mov     P1,XINIT            ; get byte to send
           ldn     P1                 ; retrieve it
           phi     T3                 ; Place for transmit
           mov     P1,XH1              ; point to input buffer
           ghi     T3                 ; get byte
           sep     scall              ; and send it
           dw      f_tty
readblk1:  sep     scall              ; read next byte from serial port
           dw      f_read
           str     P1                  ; store into buffer
           inc     P1                  ; increment buffer 
           ghi     DP                  ; get first character flag
           shr                         ; shift into df
           phi     DP                  ; and put it back
           bnf     recvgo              ; jump if not first character
           glo     re                  ; [RLA] get character
           smi     04h                 ; check for EOT
           bnz     recvgo              ; jump if not EOT
           ldi     ACK                 ; ACK the EOT
           sep     scall
           dw      f_tty
           ldi     1                   ; indicate EOT received
           lbr     recvret
recvgo:    dec     DP                  ; decrement receive count
           glo     DP                  ; see if done
           bnz    readblk1             ; jump if more bytes to read
           ldi     0                   ; clear df flag for full block read
recvret:   shr
           pop     T3
           pop     rd                  ; recover consumed registers
           pop     DP
           pop     P2
           sep     sret                ; and return to caller
