.include "m2560def.inc"

/*===========REGISTER DECLARATIONS===========*/

; Constant Registers
.def one = r15
.def zero = r14

; Main Registers
.def tempMain = r25
.def tempSec =r24
.def cursorPos = r22
.def storagePos = r21
.def row = r20
.def col = r19
.def mask = r18

; Custom Flag Registers
.def invExpression = r2
.def OVFOccured = r3

; Global Registers
.def mulRes_l = r0
.def mulRes_h = r1


/*===========VALUE DECLARATIONS===========*/
.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4
.equ PORTLDIR = 0b11110000
.equ INITCOLMASK = 0b11101111
.equ INITROWMASK = 0b00000001
.equ ROWMASK = 0b00001111

/*===========DSEG===========*/

.dseg
.org 0x200
stationNames: .byte 100
travelTimes: .byte 10
stopTime: .byte 1

ASCIIStorage: .byte 2


/*===========CSEG===========*/

.cseg
.org 0
jmp RESET

.org INT0addr
;jmp buttonRight

.org INT1addr
;jmp buttonLeft

.org INT3addr
;jmp laserReceiver

.org OVF0addr
;jmp Timer0OVF

/*===========DELAY MACRO===========*/

.macro delay
	push tempMain
	push tempSec

	clr zero
	clr one
	inc one

	clr tempMain
	clr tempSec

	ldi tempMain, high(@0)
	ldi tempSec, low(@0)

	delayLoop:
		sub tempSec, one
		sbc tempMain,zero

		push tempMain
		push tempSec

		ldi tempMain, 0x0A
		ldi tempSec, 0x6B

		subDelayLoop:
			sub tempSec, one
			sbc tempMain,zero
			cp tempSec, zero
			cpc tempMain, zero
			brne subDelayLoop

		pop tempSec
		pop tempMain

		cp tempMain, zero
		cpc tempSec, zero
		brne delayLoop

	pop tempSec
	pop tempMain
.endmacro

/*===========MULTIPLICATION MACRO===========*/

.macro multMacro ; max result - 2 bytes
	push r15
	push r14

	ldi tempMain, @2
	mul @1, tempMain
	mov r14, r1
	mov r15, r0
	mul @0, tempMain
	add r14, r0
	mov @1, r15
	mov @0, r14

	pop r14
	pop r15
.endmacro

/*===========BLINK MACRO===========*/

blink:
	push tempMain
	ldi tempMain, 0xff ;;;;;;;;;;;;;
	out PORTC, tempMain
	delay 250
	ldi tempMain, 0 ;;;;;;;;;;;;;
	out PORTC, tempMain
	delay 250
	ldi tempMain, 0xff ;;;;;;;;;;;;;
	out PORTC, tempMain
	delay 250
	ldi tempMain, 0 ;;;;;;;;;;;;;
	out PORTC, tempMain
	delay 250
	pop tempMain
ret

/*===========QUICK BLINK MACRO===========*/

quickBlink:
	push tempMain
	ldi tempMain, 0xff ;;;;;;;;;;;;;
	out PORTC, tempMain
	delay 100
	ldi tempMain, 0 ;;;;;;;;;;;;;
	out PORTC, tempMain
	delay 100
	ldi tempMain, 0xff ;;;;;;;;;;;;;
	out PORTC, tempMain
	delay 100
	ldi tempMain, 0 ;;;;;;;;;;;;;
	out PORTC, tempMain
	delay 100
	ldi tempMain, 0xff ;;;;;;;;;;;;;
	out PORTC, tempMain
	delay 100
	ldi tempMain, 0 ;;;;;;;;;;;;;
	out PORTC, tempMain
	delay 100
	ldi tempMain, 0xff ;;;;;;;;;;;;;
	out PORTC, tempMain
	delay 100
	ldi tempMain, 0 ;;;;;;;;;;;;;
	out PORTC, tempMain
	delay 100
	pop tempMain
ret

/*===========PRESET LCD COMMANDS===========*/

.macro do_lcd_command
	ldi tempMain, @0
	call lcd_command
	call lcd_wait
.endmacro

.macro do_lcd_data
	mov tempMain, @0
	call lcd_data
	call lcd_wait
.endmacro

.macro lcd_set
	sbi PORTA, @0
.endmacro

.macro lcd_clr
	cbi PORTA, @0
.endmacro

lcd_command:
	out PORTF, tempMain
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
	out PORTF, tempMain
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
	push tempMain
	clr tempMain
	out DDRF, tempMain
	out PORTF, tempMain
	lcd_set LCD_RW

lcd_wait_loop:
	nop
	lcd_set LCD_E
	nop
	nop
    nop
	in tempMain, PINF
	lcd_clr LCD_E
	sbrc tempMain, 7
	jmp lcd_wait_loop
	lcd_clr LCD_RW
	ser tempMain
	out DDRF, tempMain
	pop tempMain
ret

/*===========CLEAR DISPLAY MACRO===========*/

.macro clearDisplay
	do_lcd_command 0b00000001 ; clear display
	clr cursorPos
.endmacro

/*===========DISPLAY UPDATE MACRO===========*/

.macro updateDisplay
	push r16
	mov r16, @0
	push tempMain
	ldi tempMain, 16
	cp cursorPos, tempMain
	pop tempMain
	brne skipClear
	clearDisplay
	skipClear:

	do_lcd_data r16
	inc cursorPos
	pop r16
.endmacro

/*===========DISPLAY UPDATE WITH ASCII MACRO===========*/

.macro updateDisplayWithASCII
	ldi tempMain, 16
	cp cursorPos, tempMain
	brne skipClear
	clearDisplay
	skipClear:

	ldi tempMain, @0
	do_lcd_data tempMain
	inc cursorPos
.endmacro

/*===========CLEAR ASCII STORAGE MACRO===========*/

.macro clearASCIIStorage
	ldi xl, low(ASCIIStorage)
	ldi xh, high(ASCIIStorage)

	st x+, zero
	st x+, zero

	clr storagePos
.endmacro

/*===========DISPLAY STRING SUBROUTINE===========*/

displayString:
	push r16
	clr r16
	mov r16, tempMain

	clearDisplay

	displayStringLoop:

	lpm tempSec, z+
	updateDisplay tempSec

	delay 25 // TO BE MODIFIED

	dec r16
	out PORTC, r16 ;;;;;;;;;;;;
	cpi r16, 0

	breq skipDisplayStringLoop
	jmp displayStringLoop
	skipDisplayStringLoop:
	pop r16
ret

/*===========MASTER RESET SUBROUTINE===========*/

RESET:
	ldi tempMain, low(RAMEND)
	out spl, tempMain
	ldi tempMain, high(RAMEND)
	out sph, tempMain

//********************************
	ser tempMain
	out DDRF, tempMain
	out DDRA, tempMain
	clr tempMain
	out PORTF, tempMain
	out PORTA, tempMain

	do_lcd_command 0b00111000 ; display setting
	delay 5
	do_lcd_command 0b00111000 ; display setting
	delay 1
	do_lcd_command 0b00111000 ; display setting
	do_lcd_command 0b00111000 ; display setting
	do_lcd_command 0b00001000 ; display on
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001100 ; cursor setting

//********************************

	ldi tempMain, PORTLDIR ; columns are outputs, rows are inputs
	STS DDRL, tempMain     ; cannot use out

	ldi tempMain, (0 << CS50) 
	sts TCCR5B, tempMain 
	ldi tempMain, (0 << WGM50)|(0 << COM5A1)
	sts TCCR5A, tempMain

//********************************
	ser tempMain
	out DDRC, tempMain ; PORTC is all ouputs
	ldi tempMain, 0b11111111
	out PORTC, tempMain
	delay 50
	ldi tempMain, 0b11100111
	out PORTC, tempMain
	delay 50
	ldi tempMain, 0b11000011
	out PORTC, tempMain
	delay 50
	ldi tempMain, 0b10000001
	out PORTC, tempMain
	delay 50
	ldi tempMain, 0b00000000
	out PORTC, tempMain

//********************************
	testData: .db "1234567890", 0, 0 ; 10 char
	errorOcc: .db "Error occured, please try again.", 0, 0 ; 32 char
	// Phase 1
	iniSNum: .db "Please type the maximum number of stations", 0, 0 ; 42 char
	// Phase 2
	iniSName: .db "Pleas type the name of station", 0, 0 ; 30 char
	// Phase 3
	iniSTravel1: .db "The time from Station", 0 ; 21 char
	iniSTravel2: .db "to Station", 0, 0 ; 10 char
	// Phase 4
	iniSStop: .db "The stop time of the monorail at any station is", 0 ; 47 char
	iniFin: .db "Now the configuration is complete. Please wait 5 seconds", 0, 0 ; 56 char

//********************************
	clr zero
	clr one
	inc one
	clr cursorPos
	clr storagePos
	clr invExpression
	clr OVFOccured
	clearASCIIStorage
jmp main


/*===========MAIN FUNCTION===========*/

main:

	call phase1
	phase1Completed:

	endLoop:
		ldi tempMain, 0xff ;;;;;;;;;;;;;
		out PORTC, tempMain
		delay 500
;		ldi tempMain, 0 ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;		ldi tempMain, 0xff ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;		ldi tempMain, 0 ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;		ldi tempMain, 0xff ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;		ldi tempMain, 0 ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;
;		ldi tempMain, 0xff ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 1500
;		ldi tempMain, 0 ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;		ldi tempMain, 0xff ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 1500
;		ldi tempMain, 0 ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;		ldi tempMain, 0xff ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 1500
;		ldi tempMain, 0 ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;
;		ldi tempMain, 0xff ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;		ldi tempMain, 0 ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;		ldi tempMain, 0xff ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;		ldi tempMain, 0 ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;		ldi tempMain, 0xff ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
;		ldi tempMain, 0 ;;;;;;;;;;;;;
;		out PORTC, tempMain
;		delay 500
	rjmp endLoop

/*===========ASK FOR MAX STATION NUMBER SUBROUTINE===========*/

phase1:
	ldi zl, low(iniSNum<<1)
	ldi zh, high(iniSNum<<1)
	ldi tempMain, 42
	call displayString
	updateDisplayWithASCII ':'
	updateDisplayWithASCII 32

	call valueEnter

ret

/*===========NUMBER-BASE VALUE ENTERING FUNCTION===========*/

valueEnter:
	ldi mask, INITCOLMASK
	clr col

	colloop:
		STS PORTL, mask
		ldi tempMain, 255

		delayMain:
			dec tempMain
			brne delayMain

	LDS tempMain, PINL 
	andi tempMain, ROWMASK 
	cpi tempMain, 0b00001111
	brne rowNotPushed 
	jmp nextcol
	rowNotPushed:

	ldi mask, INITROWMASK
	clr row 

	rowloop:
		mov tempSec, tempMain
		and tempSec, mask
		breq isEqual
		jmp skipconv
		isEqual:
		call convert 

	delay 250
	jmp valueEnter
ret

/*===========NUMBER-BASE KEYPAD COMMANDS===========*/

skipconv:
	inc row
	lsl mask 
jmp rowloop

nextcol:     
	cpi col, 3
	brne colNotPushed
	jmp valueEnter
	colNotPushed:
	sec
	rol mask
	inc col
jmp colloop

convert:
	cpi col, 3
	breq letters
	cpi row, 3
	breq symbols
	mov tempMain, row 
	lsl tempMain
	add tempMain, row
	add tempMain, col
	ldi tempSec, 49
	add tempMain, tempSec

jmp convert_end

letters:
	mov tempMain, row
	cpi tempMain, 0
	breq AChar
	cpi tempMain, 1
	breq BChar
	cpi tempMain, 2
	breq CChar
	ldi tempMain, 'D'
	clr invExpression
	inc invExpression
jmp convert_end

AChar:
	ldi tempMain, 'A'
	clr invExpression
	inc invExpression
jmp convert_end

BChar:
	ldi tempMain, 'B'
	clr invExpression
	inc invExpression
jmp convert_end

CChar:
jmp phase1End

symbols:
	cpi col, 0
	breq star
	cpi col, 1
	breq zeroChar
	ldi tempMain, '#'
	clr invExpression
	inc invExpression
jmp convert_end

star:
	ldi tempMain, '*'
	clr invExpression
	inc invExpression
jmp convert_end

zeroChar:
	ldi tempMain, '0'

convert_end:
	updateDisplay tempMain
	call storeKeyPadVal
ret

/*===========NUMBER-BASE STORE KEYPAD VAL===========*/

storeKeyPadVal:
	ldi xl, low(ASCIIStorage)
	ldi xh, high(ASCIIStorage)

	out PORTC, storagePos

	add xl, storagePos

	st x+, tempMain

	inc storagePos
	cpi storagePos, 3
	brsh charOVF
	jmp noCharOVF
	charOVF:
	clr OVFOccured
	inc OVFOccured
	noCharOVF:
ret

/*===========NUMBER-BASE END===========*/

phase1End:
	push r16
	push r17
	cp OVFOccured, zero
	brne overflowed
	jmp notOverflowed
	overflowed:
	jmp displayError
	notOverflowed:

	cp invExpression, zero
	brne notValid
	jmp notInvalid
	notValid:
	jmp displayError
	notInvalid:

	ldi xl, low(ASCIIStorage) 
	ldi xh, high(ASCIIStorage)

	cp storagePos, one
	breq sinInt
	jmp notSinInt
	sinInt:

	ld r16, x+
	

	notSinInt

	ld r16, x+
	ld r17, x+

	;call quickBlink

	out PORTC, tempMain

	delay 10000
	pop r17
	pop r16
jmp phase1Completed

/*===========NUMBER-BASE ERROR SUBROUTINE===========*/

displayError:
	ldi zl, low(errorOcc<<1)
	ldi zh, high(errorOcc<<1)
	ldi tempMain, 32
	call displayString
	clr OVFOccured
	clr invExpression
	call quickBlink
jmp phase1
