/* **************************************************
 *           COMP2121 19T2 Assessment               *
 *           ** Monorail Emulator **                *
 *           Created: 16 August 2019                *
 *    Author : Andy Lu(z       ), Hao Sun(z5158176) *
 * **************************************************
*/
.include "m2560def.inc"

.def temp = r16
.def row = r17
.def col = r18
.def mask = r19
.def temp2 = r20
.def Timertemp  = r26

; .def low byte of time counter in Timer0  = r24
; .def high byte of time counter in Timer0 = r25


.equ LCD_RS = 7
.equ LCE_E  = 6
.equ LCD_RW = 5
.equ LCE_BE = 4
.equ PORTLDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F

.macro clear
    ldi YL, low(@0)                         ; load the memory address to Y pointer
    ldi YH, high(@0)
    clr temp                                ; set temp to 0
    st Y+, temp                             ; clear the two bytes at @0 in SRAM
    st Y, temp
.endmacro

.macro do_lcd_command
    ldi r16, @0
    rcall dnammoc_dcl
    rcall lcd_wait
.endmacro

.macro do_lcd_data                          ; this macro helps print a letter on the display
    ldi r16, @0
    rcall lcd_data
    rcall lcd_wait
.endmacro

.macro lcd_set
    sbi PORTA, @0
.endmacro

.macro lcd_clr
    cbi PORTA, @0
.endmacro

.dseg
    nameList: .byte 100                     ; stores 10 stations' name, 10 bytes ea
    timeList: .byte 10                      ; stores 10 stop time, 1 byte ea
    timeCounter: .byte 2                    ; time counter in Timer0

.cseg
    jmp RESET

/* initialize */
RESET:
    ldi r16, low(RAMEND)
    out SPL, r16
    ldi r16, high(RAMEND)
    out SPH, r16
    ldi temp, PORTLDIR                      ; columns are outputs, rows are inputs
    STS DDRL, temp                          ; cannot use out
    ser temp
    out DDRC, temp                          ; Make PORTC all outputs
    out PORTC, temp                         ; Turn on all the LEDs
    ser temp
    out DDRF, temp
    out DDRA, temp
    clr temp
    out PORTF, temp
    out PORTA, temp

    do_lcd_command 0b00111000               ; 2x5x7
    rcall sleep_5ms
    do_lcd_command 0b00111000               ; 2x5x7
    rcall sleep_1ms
    do_lcd_command 0b00111000               ; 2x5x7
    do_lcd_command 0b00111000               ; 2x5x7
    do_lcd_command 0b00001000               ; display off?
    do_lcd_command 0b00000001               ; clear display
    do_lcd_command 0b00000110               ; increment, no display shift
    do_lcd_command 0b00001110               ; Cursor on, bar, no blink

/* input the number of stops and their names */
do_lcd_command 0b00000001                   ; clear display
do_lcd_data 'N'                             ; print "NO OF STATIONS:" on screen
do_lcd_data 'O'
do_lcd_data ' '
do_lcd_data 'O'
do_lcd_data 'F'
do_lcd_data ' '
do_lcd_data 'S'
do_lcd_data 'T'
do_lcd_data 'S'
do_lcd_data 'T'
do_lcd_data 'I'
do_lcd_data 'O'
do_lcd_data 'N'
do_lcd_data 'S'
do_lcd_data ':'
ret

/* set the travel time */
do_lcd_command 0b00000001

/* set the stop time */

/* main function: keeps scanning the keypad to find which key is pressed. */
main:
    ldi mask, INITCOLMASK                   ; initial column mask
    clr col                                 ; initial column
    colloop:
    STS PORTL, mask                         ; set column to mask value
                                            ; (sets column 0 off)
    ldi temp, 0xFF                          ; implement a delay so the
                                            ; hardware can stabilize
    delay:
    dec temp
    brne delay
    LDS temp, PINL                          ; read PORTL. Cannot use in
    andi temp, ROWMASK                      ; read only the row bits
    cpi temp, 0xF                           ; check if any rows are grounded
    breq nextcol                            ; if not go to the next column
    ldi mask, INITROWMASK                   ; initialise row check
    clr row                                 ; initial row
    rowloop:
    mov temp2, temp
    and temp2, mask                         ; check masked bit
    brne skipconv                           ; if the result is non-zero,
                                            ; we need to look again
    rcall convert                           ; if bit is clear, convert the bitcode
    jmp main                                ; and start again
    skipconv:
    inc row                                 ; else move to the next row
    lsl mask                                ; shift the mask to the next bit
    jmp rowloop
    nextcol:
    cpi col, 3                              ; check if we are on the last column
    breq main                               ; if so, no buttons were pushed,
                                            ; so start again.

    sec                                     ; else shift the column mask:
                                            ; We must set the carry bit
    rol mask                                ; and then rotate left by a bit,
                                            ; shifting the carry into
                                            ; bit zero. We need this to make
                                            ; sure all the rows have
                                            ; pull-up resistors
    inc col                                 ; increment column value
    jmp colloop                             ; and check the next column
                                            ; convert function converts the row and column given to a
                                            ; binary number and also outputs the value to PORTC.
                                            ; Inputs come from registers row and col and output is in
                                            ; temp.
    convert:
    cpi col, 3                              ; if column is 3 we have a letter
    breq letters
    cpi row, 3                              ; if row is 3 we have a symbol or 0
    breq symbols
    mov temp, row                           ; otherwise we have a number (1-9)
    lsl temp                                ; temp = row * 2
    add temp, row                           ; temp = row * 3
    add temp, col                           ; add the column address
                                            ; to get the offset from 1
    inc temp                                ; add 1. Value of switch is
                                            ; row*3 + col + 1.


Timer0OVF:                                  ; Timer0 overflow
    in Timertemp, SREG
    push Timertemp
    push YH
    push YL
    push r25
    push r24
    lds  r24, timerCounter
    lds  r25, timerCounter + 1
    adiw r25:r24, 1

lcd_command:
    out PORTF, r16
    nop
    lcd_set LCD_E
    nop
    nop
    nop
    lcd_clr LCD_E
    nop
    nop
    nop
    ret

lcd_data:
    out PORTF, r16
    lcd_set LCD_RS
    nop
    nop
    nop
    lcd_set LCD_E
    nop
    nop
    nop
    lcd_clr LCD_E
    nop
    nop
    nop
    lcd_clr LCD_RS
    ret

lcd_wait:
    push r16
    clr r16
    out DDRF, r16
    out PORTF, r16
    lcd_set LCD_RW

lcd_wait_loop:
    nop
    lcd_set LCD_E
    nop
    nop
    nop
    in r16, PINF
    lcd_clr LCD_E
    sbrc r16, 7
    rjmp lcd_wait_loop
    lcd_clr LCD_RW
    ser r16
    out DDRF, r16
    pop r16
    ret

.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4

sleep_1ms:
    push r24
    push r25
    ldi r25, high(DELAY_1MS)
    ldi r24, low(DELAY_1MS)

delayloop_1ms:
    sbiw r25:r24, 1
    brne delayloop_1ms
    pop r25
    pop r24
    ret

sleep_5ms:
    rcall sleep_1ms
    rcall sleep_1ms
    rcall sleep_1ms
    rcall sleep_1ms
    rcall sleep_1ms
    ret

sleep_20ms:
    rcall sleep_5ms
    rcall sleep_5ms
    rcall sleep_5ms
    rcall sleep_5ms
    ret

sleep_100ms:
    rcall sleep_20ms
    rcall sleep_20ms
    rcall sleep_20ms
    rcall sleep_20ms
    rcall sleep_20ms
    ret
