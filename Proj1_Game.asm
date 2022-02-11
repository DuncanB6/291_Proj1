


$NOLIST
$MODLP51
$LIST

CLK           EQU 22118400 
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 125     ; 1000Hz, timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
PLAYER_1	  equ P0.1
PLAYER_2      equ P0.2
SOUND_OUT	  equ P1.1

; Reset vector
org 0x0000
    ljmp main

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

dseg at 0x30
Seed:	  ds 4 ; Random seed 
Count1ms: ds 2 ; Used to determine when half second has passed
Score_1:  ds 1 ; Score of player 1
Score_2:  ds 1 ; Score of player 2
tone:	  ds 1 ; Tone indicator
x:		  ds 4 ; Math var 1
y: 		  ds 4 ; Math var 2
time_counter: ds 1 ; Counts time
bcd:	  ds 5

bseg
four_seconds_flag: dbit 1
mf: 			   dbit 1

cseg
LCD_RS equ P3.2
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

$NOLIST
$include(LCD_4bit.inc)
$include(math32.inc)
$LIST

;              1234567890123456 
Player_1_init:  db 'Player One: x', 0
Player_2_init:  db 'Player Two: x', 0

Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov RH0, #high(TIMER0_RELOAD)
	mov RL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret
	
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	cpl SOUND_OUT ; Connect speaker to P1.1!
	reti

Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret
	
	Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P1.0 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(500), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(500), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb four_seconds_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, time_counter
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov time_counter, a
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti

Wait1s:
    mov R2, #176
X3: mov R1, #250
X2: mov R0, #166
X1: djnz R0, X1
    djnz R1, X2
    djnz R2, X3
    ret
    
Random:
	mov x+0, Seed+0
	mov x+1, Seed+1
	mov x+2, Seed+2
	mov x+3, Seed+3
	Load_y(214013)
	lcall mul32
	Load_y(2531011)
	lcall add32
	mov Seed+0, x+0
	mov Seed+1, x+1
	mov Seed+2, x+2
	mov Seed+3, x+3
	ret
    
main:
	mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    mov P0M0, #0
    mov P0M1, #0
    setb EA  
    lcall LCD_4BIT
	Set_Cursor(1, 1)
    Send_Constant_String(#Player_1_init)
    Set_Cursor(2, 1)
    Send_Constant_String(#Player_2_init)
    setb four_seconds_flag
	mov Score_1, #0x00
	mov Score_2, #0x00
	
	; Fills seed with a random number based on time of button press.
	setb TR2
	jb	P4.5, $
	mov Seed+0, TH2
	mov Seed+1, #0x03
	mov Seed+2, #0x33
	mov Seed+3, TL2
	clr TR2
	
tone_loop: 
	clr TR2

	; Choose frequency randomly
	lcall Random
	mov a, Seed+1
	mov c, acc.3
	mov tone, c

	; Play frequency
	
	sjmp loop

tone_1_relay:
	ljmp tone_1

	
loop:
	; Initialize timer, count to 4.
	setb TR2
	
	; If 4 seconds have passed, loop.
	mov a, four_seconds_flag
	CJNE a, #0x00, tone_loop
	
	; Check for button press/555 frequency change
	mov a, tone
	CJNE a, #0x00, tone_1_relay
	

	; Check for a button press on tone 0.
	; Check for player one's button.
		jb PLAYER_1, check_button_1  
		Wait_Milli_Seconds(#50)	
		jb PLAYER_1, check_button_1 
		jnb PLAYER_1, $	
		ljmp Deduct_player_1
		
		; Check for player one's button.
	check_button_1:
		jb PLAYER_1, loop 
		Wait_Milli_Seconds(#50)	
		jb PLAYER_1, loop  
		jnb PLAYER_1, $	
		ljmp Deduct_player_1
		
		ret
		
	; Update score
	Set_Cursor(1, 13)
	Display_BCD(Score_1)
	Set_Cursor(2, 13)
	Display_BCD(Score_2)
	
	; Check to see if a player has won, restart game if so.
	mov a, Score_1
	CJNE a, #0x05, check_score_two
	Set_Cursor(1, 13)
	WriteData(#0x33)
	mov Score_1, #0x00
	mov Score_2, #0x00
check_score_two:
	mov a, Score_2
	CJNE a, #0x05, loop
	Set_Cursor(2, 13)
	WriteData(#0x33)
	mov Score_1, #0x00
	mov Score_2, #0x00
	
	ljmp loop
	
	tone_1:
		; Check for player one's button.
		jb PLAYER_1, check_button_2  
		Wait_Milli_Seconds(#50)	
		jb PLAYER_1, check_button_2 
		jnb PLAYER_1, $	
		ljmp Score_player_1
		
	loop_relay:
		ljmp loop
		
		; Check for player one's button.
	check_button_2:
		jb PLAYER_2, loop_relay 
		Wait_Milli_Seconds(#50)	
		jb PLAYER_2, loop_relay
		jnb PLAYER_2, $	
		ljmp Score_player_2
		
		ret
	
	Score_player_1:
		mov a, Score_1
		add a, #0x01
		ljmp tone_loop
		
	Score_player_2:
		mov a, Score_2
		add a, #0x01
		ljmp tone_loop
		
	Deduct_player_1:
		mov a, Score_1
		add a, #0x99
		ljmp tone_loop
		
	Deduct_player_2:
		mov a, Score_2
		add a, #0x99
		ljmp tone_loop
    
