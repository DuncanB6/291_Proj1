


$NOLIST
$MODLP51
$LIST

CLK           EQU 22118400 
TIMER0_0_RATE   EQU 2200     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_0_RELOAD EQU ((65536-(CLK/TIMER0_0_RATE)))
TIMER0_1_RATE   EQU 2000     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_1_RELOAD EQU ((65536-(CLK/TIMER0_1_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
PLAYER_0	  equ P0.1
PLAYER_1      equ P0.2
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
Seed:	  		ds 4 ; Random seed 
Count1ms: 		ds 2 ; Used to determine when half second has passed
Score_0:  		ds 1 ; Score of player 1
Score_1:  		ds 1 ; Score of player 2
x:		  		ds 4 ; Math var 1
y: 		  		ds 4 ; Math var 2
time_counter:   ds 1 ; Counts time
bcd:	  		ds 5

bseg
two_seconds_flag: dbit 1
mf: 			  dbit 1
tone:			  dbit 1
player_flag:	  dbit 1

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

;                   1234567890123456 
Player_1_init:  db 'Player One: 00', 0
Player_2_init:  db 'Player Two: 00', 0
Win:			db 'Wins', 0

Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_0_RELOAD)
	mov TL0, #low(TIMER0_0_RELOAD)
	; Set autoreload value
	mov RH0, #high(TIMER0_0_RELOAD)
	mov RL0, #low(TIMER0_0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    clr TR0  ; Stop timer 0
	ret
	
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	cpl SOUND_OUT ; Connect speaker to P1.1!
	reti

;Initializes timer/counter 2 as a 16-bit counter
Timer1_Init:
	mov TCON, #0b_0000_0000 ; Stop timer/counter.  Set as counter (clock input is pin T2).
	; Set the reload value on overflow to zero (just in case is not zero)
	mov TH1, #0
	mov TL1, #0
    ret

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
	; Check if two seconds has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000 ), Timer2_ISR_done
	
	; 2000 milliseconds have passed.  Set a flag so the main program knows
	setb two_seconds_flag ; Let the main program know two seconds have passed
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
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
    lcall Timer1_Init
    lcall Timer2_Init
    setb P2.0
    setb P2.1
    mov P0M0, #0
    mov P0M1, #0
    setb EA  
    lcall LCD_4BIT
    
	Set_Cursor(1, 1)
    Send_Constant_String(#Player_1_init)
    Set_Cursor(2, 1)
    Send_Constant_String(#Player_2_init)

    clr two_seconds_flag
    mov a, #0x00
	mov Score_0, a
	mov Score_1, a
	mov Seed, a
	mov x, a
	mov y, a
	clr tone
	clr mf
	
	; Fills seed with a random number based on time of button press.
	setb TR2
	jb	P4.5, $
	mov Seed+0, TH2
	mov Seed+1, #0x03
	mov Seed+2, #0x33
	mov Seed+3, TL2
	clr TR2

; Picks randomly between the two frequencies, starts playing that tone.	
tone_loop: 

	; Debug
	; Set_Cursor(1, 1)
	; WriteData(#0x33)
	
	; A moment of silence between rounds.
	clr TR0
	lcall Wait1s

	; Resets timer two, which is measuring the two second periods of sound.
	clr TR2
	clr a
	clr two_seconds_flag
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	; Starts timer two.
	setb TR2

	; Chooses frequency randomly
	lcall Random
	mov a, Seed+1
	mov c, acc.3
	mov tone, c

	; Jump to a seperate set of reload instructions for one tone.
	jnb tone, tone_reload
	
	; Sets frequency using reload instructions.
	mov TH0, #high(TIMER0_0_RELOAD)
	mov TL0, #low(TIMER0_0_RELOAD)
	mov RH0, #high(TIMER0_0_RELOAD)
	mov RL0, #low(TIMER0_0_RELOAD)
	
	; Turns on tone, moves to main loop.
	setb TR0
	ljmp loop
	
	; Sets frequency using reload instructions.
tone_reload:
	mov TH0, #high(TIMER0_1_RELOAD)
	mov TL0, #low(TIMER0_1_RELOAD)
	mov RH0, #high(TIMER0_1_RELOAD)
	mov RL0, #low(TIMER0_1_RELOAD)
	
	; Turns on tone, moves to main loop.
	setb TR0
	ljmp loop
	
loop:
	
	; If 2 seconds have passed, return to tone loop, which resets round.

	jb two_seconds_flag, tone_loop

	; Debug
	; Set_Cursor(2, 1)
	; WriteData(#0x33)	

	; Check for player one's button.
	; Measure the period applied to pin P2.0
    clr TR1 ; Stop counter 2
    mov TL1, #0
    mov TH1, #0
    jb P2.0, $
    jnb P2.0, $
    setb TR1 ; Start counter 0
    jb P2.0, $
    jnb P2.0, $
    clr TR1 ; Stop counter 2, TH1-TL1 has the period
    ; save the period of P2.0 for later use
    
    mov x+0, TL1
    mov x+1, TH1
    mov x+2, #0x00
    mov x+3, #0x00
    
    clr mf
    load_y(6000)
    lcall x_lt_y
    jnb mf, clear_0
    clr player_flag
    ljmp player_choice
    
    clear_0:
		
	; Check for player two's button.
 	; Measure the period applied to pin P2.1
    clr TR1 ; Stop counter 2
    mov TL1, #0
    mov TH1, #0
    jb P2.1, $
    jnb P2.1, $
    setb TR1 ; Start counter 0
    jb P2.1, $
    jnb P2.1, $
    clr TR1 ; Stop counter 2, TH2-TL2 has the period
    ; save the period of P2.1 for later use
    
    mov x+0, TL1
    mov x+1, TH1
    mov x+2, #0x00
    mov x+3, #0x00
    
    clr mf
    load_y(4000)
    lcall x_lt_y
    jnb mf, clear_1
    setb player_flag
    ljmp player_choice
    
    clear_1:
    
    ljmp loop
		
	; If player flag is 0, move to score player 0. Otherwise, score player 1.
	player_choice:
			jnb player_flag, scoring_0
			ljmp scoring_1
	
	; Either increments or decrements the player's score based on the tone that is currently playing.
	; If the player's score is 0, does not change their score.
	scoring_0:
		jnb tone, increment_0
		mov a, score_0
		CJNE a, #0x00, non_0_0
		ljmp hot_end
		
	non_0_0:
		mov a, score_0
		add a, #0x99
		da a
		mov score_0, a
		ljmp hot_end
		
	increment_0:
		mov a, score_0
		add a, #0x01
		da a
		mov score_0, a
		ljmp hot_end
		
	; Either increments or decrements the player's score based on the tone that is currently playing.
	; If the player's score is 0, does not change their score.
	scoring_1:
		jnb tone, increment_1
		mov a, score_1
		CJNE a, #0x00, non_0_1
		ljmp hot_end
		
	non_0_1:
		mov a, score_1
		add a, #0x99
		da a
		mov score_1, a
		ljmp hot_end
		
	increment_1:
		mov a, score_1
		add a, #0x01
		da a
		mov score_1, a
		ljmp hot_end
			
	; A button has been pressed, so the scores are updated and we move to the next round.
	hot_end:
		clr mf
	
		Set_Cursor(1, 13)
		Display_BCD(score_0)
		Set_Cursor(2, 13)
		Display_BCD(score_1)
		
		mov a, score_0
		CJNE a, #0x05, nowin_0
		
		Set_Cursor(1, 13)
		Send_Constant_String(#Win)
		lcall Wait1s
		
		ljmp main
		
		nowin_0:
		
		mov a, score_1
		CJNE a, #0x05, nowin_1
		
		Set_Cursor(2, 13)
		Send_Constant_String(#Win)
		lcall Wait1s
		
		ljmp main
		
		nowin_1:
		
		ljmp tone_loop
		
	END
    
    
    
