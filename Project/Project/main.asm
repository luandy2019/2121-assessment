/* **************************************************
 *           COMP2121 19T2 Assessment               * 
 *           ** Monorail Emulator **                *  
 *           Created: 16 August 2019                *
 *    Author : Andy Lu(z       ), Hao Sun(z5158176) * 
 * **************************************************
*/
.include "m2560def.inc"

.def zero = r2
.def one = r3
.def cursorPos = r4

.def tempMain = r25
.def tempSec = r24

/*===========VALUE DECLARATIONS===========*/

.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

// interrupt total = 16,000,000 / (256 * prescalar)
.equ intTotal = 6250 ; for 0.1s

;.equ RPS0 = 0
;.equ RPS20 = 20
;.equ RPS40 = 42
;.equ RPS60 = 83
;.equ RPS80 = 132
;.equ RPS100 = 255

/*===========DATA SEGMENT===========*/

.dseg

.org 0x200
stationNames: .byte 100
travelTimes: .byte 10
stopTime: .byte 1



/*===========CODE SEGMENT===========*/

.cseg

testData: .dw "1234567890" ; 26 char
inValid: .dw "Invalid, please try again." ; 26 char
iniSNum: .dw "Please type the maximum number of stations" ; 42 char
iniSName: .dw "Pleas type the name of station" ; 30 char
iniSTravel1: .dw "The time from Station" ; 21 char
iniSTravel2: .dw "to Station" ; 10 char
iniSStop: .dw "The stop time of the monorail at any station is" ; 47 char
iniFin: .dw "Now the configuration is complete. Please wait 5 seconds" ; 56 char

.org 0x0
jmp reset

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

/*===========BLINK MACRO===========*/

.macro blink
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
.endmacro

/*===========QUICK BLINK MACRO===========*/

.macro quickBlink
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
.endmacro

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
	push tempMain
	ldi tempMain, 16
	cp cursorPos, tempMain
	pop tempMain
	brne skipClear
	clearDisplay
	skipClear:

	do_lcd_data @0
	inc cursorPos
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

/*===========RESET SUBROUTINE===========*/

reset:
	ldi tempMain, low(RAMEND) ; stack pointer setup
	out spl, tempMain
	ldi tempMain, high(RAMEND)
	out sph, tempMain

	clr cursorPos

	ldi tempMain, 0b00001000 ; keypad setup
	sts DDRL, tempMain

	ldi tempMain, (1 << CS50) ; timer 5B PWM setup
	sts TCCR5B, tempMain 
	ldi tempMain, (1 << WGM50)|(1 << COM5A1)
	sts TCCR5A, tempMain

	ldi tempMain, (0 << WGM01) | (0 << WGM00) ; timer overflow interrupt setup
	out TCCR0A, tempMain
	ldi tempMain, (0 << WGM02) | (0 << CS02) | (0 << CS01) | (1 << CS00)
	out TCCR0B, tempMain
	ldi tempMain, 1 << TOIE0
	sts TIMSK0, tempMain

	ldi tempMain, (2 << ISC10) | (2 << ISC00) ; push buttom interrupt setup
	sts EICRA, tempMain
	in tempMain, EIMSK
	ori tempMain, (1 << INT0) | (1 << INT1)
	out EIMSK, tempMain

	ldi tempMain, (2 << ISC30) ; lazer interrupt setup
	sts EICRA, tempMain
	in tempMain, EIMSK
	ori tempMain, (1 << INT3)
	out EIMSK, tempMain
	sei

	ser tempMain ; LCD setup
	out DDRF, tempMain
	out DDRA, tempMain
	clr tempMain
	out PORTF, tempMain
	out PORTA, tempMain
	do_lcd_command 0b00111000 
	delay 5
	do_lcd_command 0b00111000 
	delay 1
	do_lcd_command 0b00111000 
	do_lcd_command 0b00111000 
	do_lcd_command 0b00001000 
	do_lcd_command 0b00000001 
	do_lcd_command 0b00000110
	do_lcd_command 0b00001100 

	delay 50
	ser tempMain
	out DDRC, tempMain
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
	delay 50
jmp main

/*===========MAIN FUNCTION===========*/

main:

// system configuration
call askStationNum


	endLoop:
	rjmp endLoop

/*===========ASK FOR MAX STATION NUMBER SUBROUTINE===========*/
askStationNum:
ldi zl, low(inValid<<1)
ldi zh, high(inValid<<1)
ldi tempMain, 42
call displayString
ret

/*===========DISPLAY STRING SUBROUTINE===========*/
displayString:
lpm tempSec, z+
updateDisplay tempSec
lpm tempSec, z+
updateDisplay tempSec
lpm tempSec, z+
updateDisplay tempSec
lpm tempSec, z+
updateDisplay tempSec
lpm tempSec, z+
updateDisplay tempSec
lpm tempSec, z+
updateDisplay tempSec
ret
