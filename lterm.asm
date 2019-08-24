\ Linux Terminal for Acorn Atom

incAtmHeader		= 1			    \ INCLUDE ATM HEADER FOR ATOMMC2 FILE
debug			    = 1			    \ IF debug THEN PRINT SOME ADDITIONAL MESSAGES

LOADEXEC            = $9C00         \ LOAD ADDRESS OF PROGRAM
OSRDCH			    = $FFE3			\ READ CHARACTER ROUTINE
OSWRCH			    = $FFF4			\ PRINT CHARACTER ROUTINE
OSNEWL			    = $FFED			\ PRINT NEWLINE
KEYSCAN			    = $FE71			\ KEYBOARD SCAN ROUTINE
WAIT                = $FB8C         \ SHORT WAIT

SERIALSTATUS        = $BDB1         \ UART STATUS / CONTROL REGISTER
SERIALDATA          = $BDB0         \ UART DATA REGISTER
SERIALDIVIDER       = $BDB2         \ UART BAUD RATE REGISTER
SERIALSPEED         = $0045         \ CLOCK DIVIDER SERIAL INTERFACE (115k2)
PORTB               = $B001         \ B-PORT OF THE 8255 (for shift and ctrl key)
PORTC               = $B002         \ C-PORT OF THE 8255 (for rept key)

LEDCTRL             = $BFE0         \ CONTROL REGISTER FOR LED STATUS
LEDDATA             = $BFE1         \ DATA REGISTER FOR LED STATUS
CTRLREG             = $BFFE         \ GENERIC CONTROL REGISTER

via_t1_latch        = $B804
via_acr             = $B80B
via_ier             = $B80E

ZP      			= $84			\ A FEW BYTES FOR WORKSPACE
LOCKSTATUS          = $E7           \ STATUS OF CAPS LOCK KEY
IRQVEC              = $204          \ INTERRUPT VECTOR

BUFFER              = $1800         \ INPUT BUFFER
RXPTR               = $80           \ RX POINTER
WRPTR               = $82           \ WRITE POINTER

\ Timer setting
\ At 115,200 baud there is a character receveived every 86,8 usec. The ISR takes about 100 cpu cycles.
\ With the Atom running at 8 MHz we can do this ISR in about 12.5 usec. If we don't want to miss an interrupt
\ we should sample at about 43 usec (Shannon's Sample Theorem :-) So the timer must be set to 40 * 8 = 320.
int_interval        = 320           \ every 40 usec interrupt, sufficient for 115,200 baud with Atom running at 8 MHz

IF (incAtmHeader)
		            ORG LOADEXEC - 22
.AtmHeader
			        EQUS 	"LTERM.RUN"
        		    ORG 	AtmHeader + 16
		            EQUW	LOADEXEC
		            EQUW 	INIT
		            EQUW	BeebDisEndAddr - LOADEXEC
ENDIF

			        ORG LOADEXEC

\ Initialization
\ This routine sets up the serial interface and the interrupts
    
.INIT               LDA     CTRLREG         \ SWITCH TO 8 MHZ
                    ORA     #$60
                    STA     CTRLREG

                    LDA     #<SERIALSPEED   \ SET THE BAUD RATE
                    STA     SERIALDIVIDER
                    LDA     #>SERIALSPEED
                    STA     SERIALDIVIDER + 1

                    SEI                     \ DISABLE INTERRUPTS
                    LDA     #<new_intvec    \ INITIALIZE INTERRUPTS
                    STA     IRQVEC
                    LDA     #>new_intvec
                    STA     IRQVEC + 1

                    lda     #$c0                \ timer1 geeft int
                    sta     via_ier

                    lda     via_acr
                    ora     #$c0                \ free running mode
                    sta     via_acr
    
                    lda     #<int_interval      \ zet interval tijd int
                    sta     via_t1_latch
                    lda     #>int_interval
                    sta     via_t1_latch + 1
        
                    CLI                     \ ENABLE INTERRUPTS

                    LDA     #$00            \ CLEAR CAPS LOCK FLAG
                    STA     LOCKSTATUS
                    STA     LEDCTRL         \ SELECT LED MODE
                    STA     LEDDATA         \ CLEAR ALL LEDS

                    LDA     #<BUFFER        \ SET RX AND WR POINTERS
                    STA     RXPTR
                    STA     WRPTR
                    LDA     #>BUFFER
                    STA     RXPTR+1
                    STA     WRPTR+1

\ The main loop.
\ This loop checks if a key is pressed; if a key is pressed it will be send via the serial interface. Received
\ bytes will be handled by the interrupt service routine.
.LOOP0              JSR     GETKEY          \ SCAN THE KEYBOARD
                    CMP     #0              \ TEST IF KEY PRESSED
                    BEQ     LOOP1           \ JUMP IF NO KEY IS PRESSED
                    JSR     SENDKEY         \ TRANSMIT THE ASCII VALUE OF THE KEY
.LOOP1              LDA     WRPTR           \ CHECK IF THERE IS ANY DATA IN THE RECEIVE BUFFER
                    CMP     RXPTR           \ 
                    BNE     LOOP2           \ YES, THERE IS SO JUMP
                    LDA     WRPTR+1
                    CMP     RXPTR+1
                    BNE     LOOP2           \ YES, THERE IS SO JUMP
                    JSR     BUFLEDOFF       \ DIM LED AS THERE IS NO DATA
                    JMP     LOOP0           \ START ALL OVER (NO DATA IN BUFFER)
.LOOP2              JSR     BUFLEDON        \ SET LED TO INDICATE BUFFER DATA
                    LDY     #0              \ CLEAR INDEX REGISTER
                    LDA     (WRPTR),Y       \ LOAD DATA TO PRINT
                    JSR     OSWRCH          \ PRINT CHARACTER
                    INC     WRPTR           \ INCREMENT WRITE POINTER
                    BNE     LOOP0
                    INC     WRPTR+1
                    BPL     LOOP0
                    LDA     #<BUFFER        \ END OF BUFFER WAS REACHED, RESET POINTER
                    STA     WRPTR
                    LDA     #>BUFFER
                    STA     WRPTR+1
                    JMP     LOOP0


\ Check and read a key
\ This routine scans the keyboard and returns the ascii value of that key or returns $00 when no key is pressed.

.GETKEY             JSR     KEYSCAN         \ TEST IF A KEY IS PRESSED
                  \ BCC     GETKEY          \ IF A KEY IS PRESSED START OVER BECAUSE THE PREVIOUS KEY IS NOT YET RELEASED
                  \ JSR     KEYSCAN         \ SCAN AGAIN
                    BCS     ENDGETKEY       \ IF NO KEY IS PRESSED THEN END
                  \ JSR     WAIT            \ WAIT A SHORT WHILE
                  \ JSR     KEYSCAN         \ SCAN AGAIN (TO AVOID BOUNCING KEYS)
                  \ BCS     ENDGETKEY       \ IF NO KEY IS PRESSED THEN END
                    LDX     #12             \ WAIT A SHORT WHILE
                    JSR     WAIT      
                    JMP     KEY2ASCII       \ GET THE ASCII VALUE OF THE KEY AND RETURN TO MAIN LOOP
.ENDGETKEY          LDA     #$00            \ LOAD ZERO CHARACTER TO INDICATE NO KEY PRESSED
                    RTS                     \ RETURN TO MAIN LOOP
                    

\ Send key through serial interface
\ This routine sends the value in A through the serial interface.
                    
.SENDKEY            PHA                     \ SAVE THE DATA
                    JSR     TXLEDON         \ SET TX LED ON
.SENDKEY1           LDA     SERIALSTATUS    \ CHECK IF THE OUTPUT BUFFER IS EMPTY
                    AND     #$01            \ MASK THE OUTPUT BUFFER BIT
                    BEQ     SENDKEY1        \ IF NOT EMPTY, KEEP WAITING
                    PLA                     \ RESTORE THE DATA
                    STA     SERIALDATA      \ SEND THE DATA
                    JSR     TXLEDOFF        \ SET TX LED OFF
                    RTS                     \ RETURN TO MAIN LOOP


\ Read key from serial interface
\ This routine checks if a byte is received by the serial interface. If it has, it will read it and return
\ this byte in A. If no byte is received, A will be $00.

.RECEIVE            LDA     SERIALSTATUS    \ READ THE STATUS REGISTER
                    AND     #$02            \ MASK THE INPUT BUFFER BIT
                    BEQ     RECEIVE1        \ NO CHARACTER RECEIVED ( A = $00)
                    LDA     SERIALDATA      \ READ THE RECEIVED DATA
.RECEIVE1           RTS                     \ RETURN TO MAIN LOOP

.SERIALISR          LDA     SERIALSTATUS    \ READ STATUS REGISTER
                    AND     #$08            \ TEST RX INTERRUPT
                    BEQ     SERIALEND       \ NOT RX INTERRUPT, IGNORE
                    JSR     RXLEDON         \ SET RX LED ON
                    TYA                     \ SAVE Y REGISTER
                    PHA
                    LDY     #0              \ CLEAR INDEX REGISTER
                    LDA     SERIALDATA      \ LOAD RECEIVED DATA
                    STA     (RXPTR),Y       \ STORE DATA IN BUFFER
                    INC     RXPTR           \ INCREMENT POINTER
                    BNE     SERIALISR1
                    INC     RXPTR + 1
                    BPL     SERIALISR1      \ JUMP IF END OF BUFFER NOT REACHED
                    LDA     #<BUFFER        \ RESET THE POINTER
                    STA     RXPTR
                    LDA     #>BUFFER
                    STA     RXPTR+1
.SERIALISR1         JSR     RXLEDOFF        \ SET RX LED OFF
                    LDA     #$21            \ CLEAR THE SERIAL INTERRUPT
                    STA     SERIALSTATUS
                    PLA                     \ RESTORE Y REGISTER
                    TAY
.SERIALEND          PLA                     \ RESTORE ACCUMULATOR
                    RTI                     \ END OF INTERRUPT

.new_intvec
                    lda via_t1_latch        \ 4     4
                    lda SERIALSTATUS        \ 4     4
                    and #2                  \ 2     2
                    beq return              \ 2     3
                    tya                     \ 2
                    pha                     \ 3
                    jsr RXLEDON             \ 16
                    ldy #0                  \ 2
                    LDA     SERIALDATA      \ 4         LOAD RECEIVED DATA
                    STA     (RXPTR),Y       \ 6         STORE DATA IN BUFFER
                    INC     RXPTR           \ 5         INCREMENT POINTER
                    BNE     nocarry         \ 2
                    INC     RXPTR + 1       \ 5
                    BPL     nocarry         \ 2         JUMP IF END OF BUFFER NOT REACHED
                    LDA     #<BUFFER        \ 2         RESET THE POINTER
                    STA     RXPTR           \ 3
                    LDA     #>BUFFER        \ 2
                    STA     RXPTR+1         \ 3
.nocarry
                    jsr RXLEDOFF            \ 16
                    pla                     \ 4
                    tay                     \ 2
.return
                    pla                     \ 4     4
                    rti                     \ 6     6
                                            \ ~100  23

.RXLEDON            LDA     LEDDATA         \ LOAD CURRENT DATA
                    ORA     #$01            \ SET BIT
.SETLED             STA     LEDDATA         \ WRITE NEW DATA
                    RTS

.RXLEDOFF           LDA     LEDDATA         \ LOAD CURRENT DATA
                    AND     #$FE            \ CLEAR BIT
                    JMP     SETLED          \ SET LED AND RETURN

.TXLEDON            LDA     LEDDATA         \ LOAD CURRENT DATA
                    ORA     #$02            \ SET BIT
                    JMP     SETLED          \ SET LED AND RETURN

.TXLEDOFF           LDA     LEDDATA         \ LOAD CURRENT DATA
                    AND     #$FD            \ CLEAR BIT
                    JMP     SETLED          \ SET LED AND RETURN

.BUFLEDON           LDA     LEDDATA         \ LOAD CURRENT DATA
                    ORA     #$04            \ SET BIT
                    JMP     SETLED          \ SET LED AND RETURN

.BUFLEDOFF          LDA     LEDDATA         \ LOAD CURRENT DATA
                    AND     #$FB            \ CLEAR BIT
                    JMP     SETLED          \ SET LED AND RETURN

.CAPSLEDON          LDA     LEDDATA         \ LOAD CURRENT DATA
                    ORA     #$80            \ SET BIT
                    JMP     SETLED          \ SET LED AND RETURN

.CAPSLEDOFF         LDA     LEDDATA         \ LOAD CURRENT DATA
                    AND     #$7F            \ CLEAR BIT
                    JMP     SETLED          \ SET LED AND RETURN


\ Convert the scancode to ascii code
\ The scancode is in the Y register. On return the ascii value will be in A.

.KEY2ASCII          LDA     PORTB           \ CHECK FOR SHIFT KEY
                    BMI     KEY1            \ IF SHIFT, GO CHECK FOR CTRL
                    LDA     #<KEY_SHIFT     \ SET ADDRESS OF SHIFT TABLE IN ZERO PAGE
                    STA     ZP
                    LDA     #>KEY_SHIFT     
                    STA     ZP+1      
                    JMP     KEY3            \ JMP TO CONVERSION ROUTINE
.KEY1               AND     #$40            \ CHECK FOR CTRL KEY
                    BNE     KEY2            \ NO CTRL, GOTO DEFAULT TABLE
                    LDA     #<KEY_CTRL      \ SET ADDRESS OF CTRL TABLE IN ZERO PAGE
                    STA     ZP
                    LDA     #>KEY_CTRL
                    STA     ZP+1
                    JMP     KEY3            \ JMP TO CONVERSION ROUTINE
.KEY2               LDA     #<KEY_TABLE     \ SET ADDRESS OF DEFAULT TABLE IN ZERO PAGE
                    STA     ZP  
                    LDA     #>KEY_TABLE
                    STA     ZP+1
.KEY3               LDA     (ZP),Y          \ LOAD THE CORRESPONDING ASCII VALUE
                    RTS                     \ RETURN (TO MAIN LOOP)

.KEY_TABLE          
                    EQUB    ' '             \ SCAN CODE $00, SPACE BAR
                    EQUB    '['             \ SCAN CODE $01, [
                    EQUB    '\'             \ SCAN CODE $02, \
                    EQUB    ']'             \ SCAN CODE $03, ]
                    EQUB    '^'             \ SCAN CODE $04, ^
                    EQUB    $00             \ SCAN CODE $05, LOCK
                    EQUB    $09             \ SCAN CODE $06, ARROW HOR
                    EQUB    $0B             \ SCAN CODE $07, ARROW VERT
                    EQUB    $00             \ SCAN CODE $08, 
                    EQUB    $00             \ SCAN CODE $09, 
                    EQUB    $00             \ SCAN CODE $0A, 
                    EQUB    $00             \ SCAN CODE $0B, 
                    EQUB    $00             \ SCAN CODE $0C, 
                    EQUB    $0D             \ SCAN CODE $0D, RETURN
                    EQUB    $00             \ SCAN CODE $0E, COPY
                    EQUB    $7F             \ SCAN CODE $0F, DELETE

                    EQUB    '0'             \ SCAN CODE $10, 0
                    EQUB    '1'             \ SCAN CODE $11, 1!
                    EQUB    '2'             \ SCAN CODE $12, 2"
                    EQUB    '3'             \ SCAN CODE $13, 3#
                    EQUB    '4'             \ SCAN CODE $14, 4$
                    EQUB    '5'             \ SCAN CODE $15, 5%
                    EQUB    '6'             \ SCAN CODE $16, 6&
                    EQUB    '7'             \ SCAN CODE $17, 7'
                    EQUB    '8'             \ SCAN CODE $18, 8(
                    EQUB    '9'             \ SCAN CODE $19, 9)
                    EQUB    ':'             \ SCAN CODE $1A, :*
                    EQUB    ';'             \ SCAN CODE $1B, ;+
                    EQUB    ','             \ SCAN CODE $1C, ,<
                    EQUB    '-'             \ SCAN CODE $1D, -=
                    EQUB    '.'             \ SCAN CODE $1E, .>
                    EQUB    '/'             \ SCAN CODE $1F, /?

                    EQUB    '@'             \ SCAN CODE $20, @
                    EQUB    'a'             \ SCAN CODE $21, A
                    EQUB    'b'             \ SCAN CODE $22, B
                    EQUB    'c'             \ SCAN CODE $23, C
                    EQUB    'd'             \ SCAN CODE $24, D
                    EQUB    'e'             \ SCAN CODE $25, E
                    EQUB    'f'             \ SCAN CODE $26, F
                    EQUB    'g'             \ SCAN CODE $27, G
                    EQUB    'h'             \ SCAN CODE $28, H
                    EQUB    'i'             \ SCAN CODE $29, I
                    EQUB    'j'             \ SCAN CODE $2A, J
                    EQUB    'k'             \ SCAN CODE $2B, K
                    EQUB    'l'             \ SCAN CODE $2C, L
                    EQUB    'm'             \ SCAN CODE $2D, M
                    EQUB    'n'             \ SCAN CODE $2E, N
                    EQUB    'o'             \ SCAN CODE $2F, O

                    EQUB    'p'             \ SCAN CODE $30, P
                    EQUB    'q'             \ SCAN CODE $31, Q
                    EQUB    'r'             \ SCAN CODE $32, R
                    EQUB    's'             \ SCAN CODE $33, S
                    EQUB    't'             \ SCAN CODE $34, T
                    EQUB    'u'             \ SCAN CODE $35, U
                    EQUB    'v'             \ SCAN CODE $36, V
                    EQUB    'w'             \ SCAN CODE $37, W
                    EQUB    'x'             \ SCAN CODE $38, X
                    EQUB    'y'             \ SCAN CODE $39, Y
                    EQUB    'z'             \ SCAN CODE $3A, Z
                    EQUB    $1B             \ SCAN CODE $3B, ESC
                    EQUB    $00             \ SCAN CODE $3C, 
                    EQUB    $00             \ SCAN CODE $3D, 
                    EQUB    $00             \ SCAN CODE $3E, 
                    EQUB    $00             \ SCAN CODE $3F, 

.KEY_SHIFT 
                    EQUB    ' '             \ SCAN CODE $00, SPACE BAR
                    EQUB    '{'             \ SCAN CODE $01, [
                    EQUB    '|'             \ SCAN CODE $02, \
                    EQUB    '}'             \ SCAN CODE $03, ]
                    EQUB    '_'             \ SCAN CODE $04, ^
                    EQUB    $00             \ SCAN CODE $05, LOCK
                    EQUB    $08             \ SCAN CODE $06, ARROW HOR
                    EQUB    $0A             \ SCAN CODE $07, ARROW VERT
                    EQUB    $00             \ SCAN CODE $08, 
                    EQUB    $00             \ SCAN CODE $09, 
                    EQUB    $00             \ SCAN CODE $0A, 
                    EQUB    $00             \ SCAN CODE $0B, 
                    EQUB    $00             \ SCAN CODE $0C, 
                    EQUB    $0D             \ SCAN CODE $0D, RETURN
                    EQUB    $00             \ SCAN CODE $0E, COPY
                    EQUB    $7F             \ SCAN CODE $0F, DELETE

                    EQUB    '0'             \ SCAN CODE $10, 0
                    EQUB    '!'             \ SCAN CODE $11, 1!
                    EQUB    '"'             \ SCAN CODE $12, 2"
                    EQUB    '#'             \ SCAN CODE $13, 3#
                    EQUB    '$'             \ SCAN CODE $14, 4$
                    EQUB    '%'             \ SCAN CODE $15, 5%
                    EQUB    '&'             \ SCAN CODE $16, 6&
                    EQUB    "'"             \ SCAN CODE $17, 7'
                    EQUB    '('             \ SCAN CODE $18, 8(
                    EQUB    ')'             \ SCAN CODE $19, 9)
                    EQUB    '*'             \ SCAN CODE $1A, :*
                    EQUB    '+'             \ SCAN CODE $1B, ;+
                    EQUB    '<'             \ SCAN CODE $1C, ,<
                    EQUB    '='             \ SCAN CODE $1D, -=
                    EQUB    '>'             \ SCAN CODE $1E, .>
                    EQUB    '?'             \ SCAN CODE $1F, /?

                    EQUB    '@'             \ SCAN CODE $20, @
                    EQUB    'A'             \ SCAN CODE $21, A
                    EQUB    'B'             \ SCAN CODE $22, B
                    EQUB    'C'             \ SCAN CODE $23, C
                    EQUB    'D'             \ SCAN CODE $24, D
                    EQUB    'E'             \ SCAN CODE $25, E
                    EQUB    'F'             \ SCAN CODE $26, F
                    EQUB    'G'             \ SCAN CODE $27, G
                    EQUB    'H'             \ SCAN CODE $28, H
                    EQUB    'I'             \ SCAN CODE $29, I
                    EQUB    'J'             \ SCAN CODE $2A, J
                    EQUB    'K'             \ SCAN CODE $2B, K
                    EQUB    'L'             \ SCAN CODE $2C, L
                    EQUB    'M'             \ SCAN CODE $2D, M
                    EQUB    'N'             \ SCAN CODE $2E, N
                    EQUB    'O'             \ SCAN CODE $2F, O

                    EQUB    'P'             \ SCAN CODE $30, P
                    EQUB    'Q'             \ SCAN CODE $31, Q
                    EQUB    'R'             \ SCAN CODE $32, R
                    EQUB    'S'             \ SCAN CODE $33, S
                    EQUB    'T'             \ SCAN CODE $34, T
                    EQUB    'U'             \ SCAN CODE $35, U
                    EQUB    'V'             \ SCAN CODE $36, V
                    EQUB    'W'             \ SCAN CODE $37, W
                    EQUB    'X'             \ SCAN CODE $38, X
                    EQUB    'Y'             \ SCAN CODE $39, Y
                    EQUB    'Z'             \ SCAN CODE $3A, Z
                    EQUB    $1B             \ SCAN CODE $3B, ESC
                    EQUB    $00             \ SCAN CODE $3C, 
                    EQUB    $00             \ SCAN CODE $3D, 
                    EQUB    $00             \ SCAN CODE $3E, 
                    EQUB    $00             \ SCAN CODE $3F, 

.KEY_CTRL    
                    EQUB    $00             \ SCAN CODE $00, SPACE BAR
                    EQUB    $00             \ SCAN CODE $01, [
                    EQUB    $00             \ SCAN CODE $02, \
                    EQUB    $00             \ SCAN CODE $03, ]
                    EQUB    '~'             \ SCAN CODE $04, ^
                    EQUB    $00             \ SCAN CODE $05, LOCK
                    EQUB    $00             \ SCAN CODE $06, ARROW HOR
                    EQUB    $00             \ SCAN CODE $07, ARROW VERT
                    EQUB    $00             \ SCAN CODE $08, 
                    EQUB    $00             \ SCAN CODE $09, 
                    EQUB    $00             \ SCAN CODE $0A, 
                    EQUB    $00             \ SCAN CODE $0B, 
                    EQUB    $00             \ SCAN CODE $0C, 
                    EQUB    $00             \ SCAN CODE $0D, RETURN
                    EQUB    $00             \ SCAN CODE $0E, COPY
                    EQUB    $00             \ SCAN CODE $0F, DELETE

                    EQUB    $00             \ SCAN CODE $10, 0
                    EQUB    $00             \ SCAN CODE $11, 1!
                    EQUB    $00             \ SCAN CODE $12, 2"
                    EQUB    $00             \ SCAN CODE $13, 3#
                    EQUB    $00             \ SCAN CODE $14, 4$
                    EQUB    $00             \ SCAN CODE $15, 5%
                    EQUB    $00             \ SCAN CODE $16, 6&
                    EQUB    $00             \ SCAN CODE $17, 7'
                    EQUB    $00             \ SCAN CODE $18, 8(
                    EQUB    $00             \ SCAN CODE $19, 9)
                    EQUB    $00             \ SCAN CODE $1A, :*
                    EQUB    $1C             \ SCAN CODE $1B, ;+
                    EQUB    $1D             \ SCAN CODE $1C, ,<
                    EQUB    '_'             \ SCAN CODE $1D, -=
                    EQUB    $1E             \ SCAN CODE $1E, .>
                    EQUB    $1F             \ SCAN CODE $1F, /?

                    EQUB    $00             \ SCAN CODE $20, @
                    EQUB    $01             \ SCAN CODE $21, A
                    EQUB    $02             \ SCAN CODE $22, B
                    EQUB    $03             \ SCAN CODE $23, C
                    EQUB    $04             \ SCAN CODE $24, D
                    EQUB    $05             \ SCAN CODE $25, E
                    EQUB    $06             \ SCAN CODE $26, F
                    EQUB    $07             \ SCAN CODE $27, G
                    EQUB    $08             \ SCAN CODE $28, H
                    EQUB    $09             \ SCAN CODE $29, I
                    EQUB    $0A             \ SCAN CODE $2A, J
                    EQUB    $0B             \ SCAN CODE $2B, K
                    EQUB    $0C             \ SCAN CODE $2C, L
                    EQUB    $0D             \ SCAN CODE $2D, M
                    EQUB    $0E             \ SCAN CODE $2E, N
                    EQUB    $0F             \ SCAN CODE $2F, O

                    EQUB    $10             \ SCAN CODE $30, P
                    EQUB    $11             \ SCAN CODE $31, Q
                    EQUB    $12             \ SCAN CODE $32, R
                    EQUB    $13             \ SCAN CODE $33, S
                    EQUB    $14             \ SCAN CODE $34, T
                    EQUB    $15             \ SCAN CODE $35, U
                    EQUB    $16             \ SCAN CODE $36, V
                    EQUB    $17             \ SCAN CODE $37, W
                    EQUB    $18             \ SCAN CODE $38, X
                    EQUB    $19             \ SCAN CODE $39, Y
                    EQUB    $1A             \ SCAN CODE $3A, Z
                    EQUB    $1B             \ SCAN CODE $3B, ESC
                    EQUB    $00             \ SCAN CODE $3C, 
                    EQUB    $00             \ SCAN CODE $3D, 
                    EQUB    $00             \ SCAN CODE $3E, 
                    EQUB    $00             \ SCAN CODE $3F, 


.BeebDisEndAddr

IF (incAtmHeader = 1)
	SAVE "lterm.run",AtmHeader,BeebDisEndAddr
ELSE
	SAVE "lterm.run",INIT,BeebDisEndAddr
ENDIF
