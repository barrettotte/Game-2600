;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;                             Defender 2600                                 ;;
;;                           Barrett Otte 2020                               ;;
;;                                                                           ;;
;; TODO: write a description                                                 ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    processor 6502                  ; set processor type for DASM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Includes                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    include "vcs.h"                 ; TIA, RIOT memory mappings from DASM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Declare variables/constants                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg.u Variables                 ; begin variables segment
    org $80                         ; range $80 to $FF

Score               ds 1            ; $80     : two digit score as BCD
Timer               ds 1            ; $81     : two digit timer as BCD
DigitOnes           ds 2            ; $82-$83 : ones digit sprite offsets
DigitTens           ds 2            ; $84-$85 : tens digit sprite offsets
ScoreGfx            ds 1            ; $86     : score graphics data
TimerGfx            ds 1            ; $87     : timer graphics data
Temp                ds 1            ; $88     : general purpose temp variable

ObjectX             ds 4            ; $89-$8C : P0, P1, M0, M1 x positions
ObjectY             ds 4            ; $8D-$90 : P0, P1, M0, M1 y positions

PlayerDraw          ds 1            ; $91     : draw storage for player 0
EnemyDraw           ds 1            ; $92     : draw storage for player 1
PlayerGfxPtr        ds 2            ; $93-$94 : graphics pointer for player 0
EnemyGfxPtr         ds 2            ; $95-$96 : graphics pointer for player 1
PlayerSprOffset     ds 1            ; $97     : player sprite offset
EnemySprOffset      ds 1            ; $98     : enemy sprite offset

Frame               ds 1            ; $9D     : count frames drawn
Random              ds 1            ; $9E     : general purpose random number
GameState           ds 1            ; $9F     : bit 7: 1=active, 0=game over

; Constants
GAME_HEIGHT         equ 89          ; 2 line kernel -> 180 = 90 * 2
DIGITS_HEIGHT       equ 5           ; height of digit graphics
PLAYER_HEIGHT       equ 16          ; height of player graphics
ENEMY_HEIGHT        equ 8           ; height of enemy graphics
MUSH_HEIGHT         equ 8           ; height of mushroom graphics

BG_COLOR            equ #$84        ; blue


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ROM Entry Point                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg Code                        ; begin code segment
    org $F800                       ; 2K ROM start $F800, 4K ROM start $F000

Reset:                              ; entry point label
    ; clean memory and registers
    sei                             ; set external interrupt
    cld                             ; clear decimal flag
    ldx #0                          ; reset x,y,a registers 
    txa                             ;
    tay                             ;
ClearStack:                         ; set stack addresses to 0
    dex                             ; x--
    txs                             ;
    pha                             ; push 0 to stack
    bne ClearStack                  ; while(!z) keep clearing stack

InitVariables:
    lda #0                          ;
    sta Score                       ; reset score
    sta Timer                       ; reset timer
    lda #$80                        ;
    sta Random                      ; seed random number generator

    ldx #0                          ;
    stx ObjectX                     ; set P0 x position
    ldx #8                          ;
    stx ObjectX+1                   ; set P1 x position
    ldy #$30                        ;
    sty ObjectY                     ; set P0 y position
    sty ObjectY+1                   ; set P1 y position

    lda #0                          ;
    sta PlayerSprOffset             ; set player initial sprite
    sta EnemySprOffset              ; set enemy initial sprite

    ; init lookup table pointers
    lda #<PlayerGfx0                ; lo byte
    sta PlayerGfxPtr                ;
    lda #>PlayerGfx0                ; hi byte
    sta PlayerGfxPtr+1              ;

    lda #<EnemyGfx0                 ; lo byte
    sta EnemyGfxPtr                 ;
    lda #>EnemyGfx0                 ; hi byte
    sta EnemyGfxPtr+1               ;

    lda #%10000000                  ; set gamestate active
    sta GameState                   ;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Main game loop                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NextFrame:                          ;
    jsr VerticalSync                ;
    jsr VerticalBlank               ;
    jsr Kernel                      ;
    jsr Overscan                    ;
    jmp NextFrame                   ;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           [VerticalSync]                                  ;;
;;  Tell TV to move beam to top to start next frame - VSYNC 3 scanlines      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VerticalSync subroutine             ;
    lda #2                          ;
    ldx #49                         ; 
    sta WSYNC                       ; wait for SYNC
    sta VSYNC                       ; VSYNC on
    stx TIM64T                      ; set timer to 41 scanlines.(49 * 64) / 76
    sta CTRLPF                      ; set playfield control (score mode)
    inc Frame                       ; 
    sta WSYNC                       ; wait for first scanline
    sta WSYNC                       ; wait for second scanline
    lda #0                          ;
    sta PF0                         ; clear playfield 0
    sta PF1                         ; clear playfield 1
    sta PF2                         ; clear playfield 2
    sta GRP1                        ; clear player 1 graphics
    sta GRP0                        ; clear player 0 graphics
    sta VDELP0                      ; player 0 vertical delay off
    sta VDELP1                      ; player 1 vertical delay off
    sta CXCLR                       ; clear collision flags
    sta WSYNC                       ; wait for third scanline
    sta VSYNC                       ; VSYNC off
    rts                             ; end VerticalSync subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           [VerticalBlank]                                 ;;
;;  Game logic                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VerticalBlank subroutine            ;
    jsr GetRandom                   ; calculate new random value
    jsr ProcessSwitches             ; process console switches
    bit GameState                   ; check game state
    bpl .NotActive                  ; if(!z) game not active
    jsr ProcessInput                ; process player input (joystick)
.NotActive:                         ;
    jsr UpdateObjPositions          ; update object positions
    jsr SetObjColors                ; set object colors
    jsr SetupScoreboard             ; prepare scoreboard for display
    rts                             ; end VerticalBlank subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              [Kernel]                                     ;;
;;  Update TIA registers                                                     ;;
;;  Timing is critical...Cycles need to be counted for each instruction.     ;;
;;                                                                           ;;
;;  (This formatting was taken from https://www.randomterrain.com)           ;;
;;  EX: instruction ; XX YY - comment                                        ;;
;;   - XX = cycles to execute,  YY = cycle count for current scanline        ;;
;;   - @AA-BB = instruction must happen at this range of cycles              ;;
;;   - (XX YY) = cycles and cycle count if branch is taken                   ;;
;;   - if AA > BB, instruction can be executed on prior scanline on or       ;;
;;        after cycle AA.                                                    ;;
;;                                                                           ;;
;; Game area drawn with two line kernel.                                     ;;
;;   - Line 1 updates player 0 and playfield                                 ;;
;;   - Line 2 updates player 1 and playfield                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Kernel subroutine                   ;
.Kernel:                            ;       -
    sta WSYNC                       ;       - wait for next scanline
    ; ----------------------------- ;       - new scanline
    lda INTIM                       ;  4  4 - check timer
    bne .Kernel                     ;  2  6 - (3 7) while(!z)
    sta VBLANK                      ;  3  9 - VBLANK off
    ldx #5                          ;  2 11 - scoreboard iterator

.ScoreboardLoop:                    ;    43 -
    ldy DigitTens                   ;  3 46 - tens digit offset for score
    lda DigitsBitmap,Y              ;  5 51 - load tens digit graphics
    and #$F0                        ;  2 53 - mask lo 4 bits (ones place)
    sta ScoreGfx                    ;  3 56 -
    ldy DigitOnes                   ;  3 59 - ones digit offset for score
    lda DigitsBitmap,Y              ;  5 64 - load ones digit graphics
    and #$0F                        ;  2 66 - mask hi 4 bits (tens place)
    ora ScoreGfx                    ;  3 69 - merge tens and ones digit gfx
    sta ScoreGfx                    ;  3 72 -
    sta WSYNC                       ;  3 75 - wait for next scanline
    ; ----------------------------- ;       - new scanline
    sta PF1                         ;  3  3 - @66-28,update playfield for score
    ldy DigitTens+1                 ;  3  6 - tens digit offset for timer
    lda DigitsBitmap,Y              ;  5 11 - load tens digit graphics
    and #$F0                        ;  2 13 - mask lo 4 bits (ones place)
    sta TimerGfx                    ;  3 16 -
    ldy DigitOnes+1                 ;  3 19 - ones digit offset for timer
    lda DigitsBitmap,Y              ;  5 24 - load ones digit graphics
    and #$0F                        ;  2 26 - mask hi 4 bits (tens place)
    ora TimerGfx                    ;  3 29 - merge tens and ones digit gfx
    sta TimerGfx                    ;  3 32 -
    jsr Sleep12                     ; 12 44 - waste 12 cycles
    sta PF1                         ;  3 47 - @39-54,update playfield for timer
    ldy ScoreGfx                    ;  3 50 - preload for next scanline
    sta WSYNC                       ;  3 53 - wait for next scanline
    ; ----------------------------- ;       - new scanline
    sty PF1                         ;  3  3 - @66-28, update playfield for score
    inc DigitTens                   ;  5  8 - next line of graphics, score tens
    inc DigitTens+1                 ;  5 13 - next line of graphics, timer tens
    inc DigitOnes                   ;  5 18 - next line of graphics, score ones
    inc DigitOnes+1                 ;  5 23 - next line of graphics, timer ones
    jsr Sleep12                     ; 12 35 - waste 12 cycles
    dex                             ;  2 37 - x--
    sta PF1                         ;  3 40 - @39-54,update playfield for timer
    bne .ScoreboardLoop             ;  2 42 - (3 43) while(!z)
    sta WSYNC                       ;  3 45 - wait for next scanline
    ; ----------------------------- ;       - new scanline
    stx PF1                         ;  3  3 - clear out playfield (x=0)
    sta WSYNC                       ;  3  6 - wait for next scanline
    ; ----------------------------- ;       - new scanline
    sta WSYNC                       ;  3  3 - wait for next scanline
    ; ----------------------------- ;       - new scanline
    lda #1                          ;  2  2 - score=off, reflect=on
    sta CTRLPF                      ;  3  5 - set playfield control
    ldy #GAME_HEIGHT+1              ;  2  7 - 180 scanlines (90 * 2)
    lda #ENEMY_HEIGHT-1             ;  2  9 - load enemy height
    dcp EnemyDraw                   ;  5 14 - decrement and compare with height
    bcs .PreP1Gfx                   ;  2 16 - (3 17) if(c) enemy on this line
    lda #0                          ;  2 18 - else, turn off player 1 graphics
    .byte $2C                       ;  4 22 - ABS BIT trick skip lda (gfxPtr),Y
.PreP1Gfx:                          ;    17 - allow player 1 to be drawn at top
    lda (EnemyGfxPtr),Y             ;  5 22 - load enemy graphics data
    sta GRP1                        ;  3 25 - @0-22 set player 1 graphics
    dey                             ;  2 27 - y--
.GameAreaLoop:                      ;    13
    lda #PLAYER_HEIGHT-1            ;  2 15 - load player height
    dcp PlayerDraw                  ;  5 20 - decrement and compare to height
    bcs .DrawP0Gfx                  ;  2 22 - (3 23) if(c) player is on line
    lda #0                          ;  2 24 - else, turn off player 0 graphics
    .byte $2C                       ;  4 28 - ABS BIT trick skip lda (gfxPtr),Y
.DrawP0Gfx:                         ;    23
    ; TODO: sprite offset here
    lda (PlayerGfxPtr),Y            ;  5 28 - load player graphics data
    sta WSYNC                       ;  3 31 - wait for next scanline
    ; ----------------------------- ;       - new scanline
    ;  line one of two-line kernel  ;       -
    sta GRP0                        ;  3  3 - @0-22 set player 0 graphics
    lda #ENEMY_HEIGHT-1             ;  2 10 - preload enemy height
    dcp EnemyDraw                   ;  5 15 - decrement and compare to height
    bcs .DrawP1Gfx                  ;  2 17 - (3 18) if(c) enemy is on line
    lda #0                          ;  2 19 - else, turn off player 1 graphics
    .byte $2C                       ;  4 23 - ABS BIT trick skip lda (gfxPtr),Y
.DrawP1Gfx:                         ;    18 -
    ; TODO: sprite offset here
    lda (EnemyGfxPtr),Y             ;  5 23 - load enemy graphics data
    sta WSYNC                       ;  3 26 - wait for next scanline
    ; ----------------------------- ;       - new scanline
    ;  line two of two-line kernel  ;       -
    sta GRP1                        ;  3  3 - @0-22 set player 1 graphics
    dey                             ;  2 10 - y-- (kernel loop counter)
    bpl .GameAreaLoop               ;  2 12 - (3 13) if(!z) draw more game area
    rts                             ;  6 13 - end Kernel subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              [Overscan]                                   ;;
;;  Game logic after draw - 27 scanlines                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Overscan subroutine                 ;
    sta WSYNC                       ; wait for next scanline
    lda #2                          ;
    sta VBLANK                      ; VBLANK on
    lda #32                         ; target 27 scanlines. 32 = (27 * 76) / 64
    sta TIM64T                      ; set timer

    ; TODO: collisions
.Wait:
    sta WSYNC                       ; wait for next scanline
    lda INTIM                       ; check timer
    bne .Wait                       ; while(x != 0)
    rts                             ; end Overscan subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            [GetRandom]                                    ;;
;;  Generate random byte using Galois Linear Feedback Shift Register (LFSR)  ;;
;;   http://atariage.com/forums/topic/159268-random-numbers/?p=1958751       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandom subroutine                ;
    lda Random                      ;
    lsr                             ; shift right
    bcc .Done                       ; if(c) random number finished
.Done:                              ;
    sta Random                      ; 
    rts                             ; end GetRandom subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           [SetObjColors]                                  ;;
;;  Set object colors for color or black and white TV                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjColors subroutine             ;
    ldx #3                          ; setting 4 colors
    ldy #3                          ; default to use color
    lda SWCHB                       ; read console switches
    and #%00001000                  ; mask for TV Type switch
    bne .Loop                       ; if 3rd bit on, use color
    ldy #7                          ; else, use black and white colors
.Loop:                              ;
    lda ObjectColors,Y              ; load color from table
    sta COLUP0,X                    ; set object color
    dey                             ; y--
    dex                             ; x--
    bpl .Loop                       ; while(x > 0)
    rts                             ; end SetObjColors subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        [SetupScoreboard]                                  ;;
;;  Setup BCD digits for timer and score variables                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetupScoreboard subroutine          ;
    inc Timer                       ; TODO: This is a test
    bne .Skip                       ; TODO: ^
    inc Score                       ; TODO: ^

.Skip:                              ;
    ldx #1                          ; 
.Loop:                              ;
    lda Score,X                     ; load ones place
    and #$0F                        ; mask hi 4 bits (tens place)
    sta Temp                        ; store A
    asl                             ; A * 2
    asl                             ; A * 4
    adc Temp                        ; (A * 4) + A
    sta DigitOnes,X                 ; x: 1=timer, 0=score
    
    lda Score,X                     ; load tens place
    and #$F0                        ; mask lo 4 bits (ones place)
    lsr                             ; A / 2
    lsr                             ; A / 4
    sta Temp                        ; store (A / 4)
    lsr                             ; A / 8
    lsr                             ; A / 16
    adc Temp                        ; (A / 16) + (A / 4)
    sta DigitTens,X                 ; x: 1=timer, 0=score
    dex                             ; x--
    bpl .Loop                       ; while(x > 0)
    rts                             ; end SetupScoreboard subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            [SetObjXPos]                                   ;;
;;  Set coarse x position and fine-tune register of TIA object               ;;
;;  PARMS:  A = X position of object                                         ;;
;;          X = target object  [0=P0,1=P1,2=M0,3=M1,4=BALL]                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjXPos subroutine               ;
    sec                             ; set carry
    sta WSYNC                       ; wait for next scanline
.Div15:                             ;       - 5 cycles per loop
    sbc #15                         ;  2  2 - coarse position
    bcs .Div15                      ;  2  4 - while(!c)
    eor #7                          ;  2  6 - adjust offset -8 to +7
    asl                             ;  2  8 - shift left 4 HMP0 uses 4 bits
    asl                             ;  2 10 
    asl                             ;  2 12
    asl                             ;  2 14
    sta.wx HMP0,X                   ;  5 19 - set fine position
    sta RESP0,X                     ;  4 23 - set coarse position
    rts                             ;  6 29 - end SetObjXPos subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           [ProcessSwitches]                               ;;
;; Process the SELECT and RESET switches on the console with SWCHB register. ;;
;;   Bit 7,6 right and left difficulty   (0=beginner,        1=advanced)     ;;
;;   Bit 3   TV type                     (0=black and white, 1=color)        ;;
;;   Bit 1,0 select and reset            (0=pressed,         1=not pressed)  ;;
;;   Bit 5,4,2 are not used                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ProcessSwitches subroutine          ;
    lda SWCHB                       ; load switches
    lsr                             ; RESET state in C flag
.Reset:                             ;
    bcs .NoReset                    ; if(c) reset not pressed
    jsr Reset                       ; else, reset the game
    lda #%10000000                  ;
    sta GameState                   ; set gamestate active
    bne .NoSelect                   ; reset complete
.NoReset:                           ;
    lsr                             ; SELECT state in C flag
    bcs .NoSelect                   ; if(c) select not pressed
    lda #0                          ;
    sta GameState                   ; set gamestate active
.Select:                            ;
    nop                             ; select has no functionality
.NoSelect:                          ;
    rts                             ; end ProcessSwitches subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           [ProcessInput]                                  ;;
;;  Handle joystick input.                                                   ;;
;;  Check SWCHA register in RIOT for joystick signals.                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ProcessInput subroutine             ;
    lda SWCHA                       ; read joystick positions
    ldx #0                          ; x:  0=left, 1=right
.Loop:                              ;
    asl                             ; shift left, carry bit = R
    bcs .CheckLeft                  ; if(c) right is not pressed
    ldy ObjectX,X                   ; load object x position
    iny                             ; else, move object right
    cpy #160                        ; check screen boundary
    bne .SetRight                   ; if(!z) save object x position
    ldy #0                          ; else, wrap to left side of screen
.SetRight:                          ;
    sty ObjectX,X                   ; set object x position
    ldy #0                          ; player 0 reflect off (face right)
    sty REFP0,X                     ; set player 0 reflection
.CheckLeft:                         ;
    asl                             ; shift left, carry bit = L
    bcs .CheckDown                  ; if(c) left is not pressed
    ldy ObjectX,X                   ; load object x position
    dey                             ; else, move object left
    cpy #255                        ; check screen boundary
    bne .SetLeft                    ; if(!z) save object x position
    ldy #159                        ; else, wrap to right side of screen
.SetLeft:                           ;
    sty ObjectX,X                   ; set object x position
    ldy #8                          ; player 0 reflect on (face left)
    sty REFP0,X                     ; set player 0 reflection
.CheckDown:                         ;
    asl                             ; shift left, carry bit = D
    bcs .CheckUp                    ; if(c) down is not pressed
    ldy ObjectY,X                   ; get object y position
    dey                             ; else, move object down
    cpy #255                        ; check screen boundary
    bne .SetDown                    ; if(!z) save object y position
    ldy #GAME_HEIGHT*2+1            ; else, wrap to top of screen
.SetDown:                           ;
    sty ObjectY,X                   ; set object y position
.CheckUp:                           ;
    asl                             ; shift left, carry bit = U
    bcs .Done                       ; if(c) up is not pressed
    ldy ObjectY,X                   ; loada object y position
    iny                             ; else, move object up
    cpy #GAME_HEIGHT*2+2            ; check screen boundary
    bne .SetUp                      ; if(!z) save object y position
    ldy #0                          ; else, wrap to bottom of screen
.SetUp:                             ;
    sty ObjectY,X                   ; set object y position
.Done:
    rts                             ; end ProcessInput subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         [UpdateObjPositions]                              ;;
;;  Update TIA (x) and kernel variables (y) for all objects                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateObjPositions subroutine       ;
    ldx #1                          ; 0=player0, 1=player1
.Loop:                              ;
    lda ObjectX,X                   ; load object x position
    jsr SetObjXPos                  ; set coarse x pos and fine tune value
    dex                             ; x--
    bpl .Loop                       ; if(!n) position more objects
    sta WSYNC                       ; wait for next scanline
    sta HMOVE                       ; set x position using fine positioning

    ldx #1                          ;
    lda ObjectY                     ; load player y position
    lsr                             ; divide by 2 for two line kernel value
    sta Temp                        ;
    bcs .NoDelayP0                  ; if(c) don't use vertical delay for P0
    stx VDELP0                      ; else, set vertical delay on for P0
.NoDelayP0:                         ;
    lda #(GAME_HEIGHT+PLAYER_HEIGHT);
    sec                             ; set carry
    sbc Temp                        ;
    sta PlayerDraw                  ;
    lda #<(PlayerGfx0+PLAYER_HEIGHT-1); lo bit player gfx pointer
    sec                             ; set carry
    sbc Temp                        ;
    sta PlayerGfxPtr                ;
    lda #>(PlayerGfx0+PLAYER_HEIGHT-1); hi bit player gfx pointer
    sbc #0                          ;
    sta PlayerGfxPtr+1              ;

    lda ObjectY+1                   ; load player 1 y position
    clc                             ; clear carry
    adc #1                          ; add 1, preload player 1 graphics
    lsr                             ; divide by 2 for two line kernel value
    sta Temp                        ;
    bcs .NoDelayP1                  ; if(c) don't use vertical delay for P1
    stx VDELP1                      ; else, set vertical delay on for P1
.NoDelayP1:                         ;
    lda #(GAME_HEIGHT+ENEMY_HEIGHT+1);
    sec                             ; set carry
    sbc Temp                        ;
    sta EnemyDraw                   ;
    lda #<(EnemyGfx0+ENEMY_HEIGHT-1); lo bit enemy gfx pointer
    sec                             ; set carry
    sbc Temp                        ;
    sta EnemyGfxPtr                 ;
    lda #>(EnemyGfx0+ENEMY_HEIGHT-1); hi bit enemy gfx pointer
    sbc #0                          ;
    sta EnemyGfxPtr+1               ;
    rts                             ; end UpdateObjPositions subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             [Sleep12]                                     ;;
;;  Just waste 12 cpu cycles                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12 subroutine                  ;
    rts                             ; JSR(6) + RTS(6) = 12 cycles


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ROM Lookup Tables                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ObjectColors:
    .byte $86                       ; blue       - COLUP0, p0 and m0
    .byte $C6                       ; green      - COLUP1, p1 and m1
    .byte $46                       ; red        - COLUPF, pf and ball
    .byte $00                       ; black      - COLUBK, background
    .byte $0E                       ; white      - COLUP0, p0 and m0
    .byte $06                       ; dark grey  - COLUP1, p1 and m1
    .byte $0A                       ; light grey - COLUPF, pf and ball
    .byte $00                       ; black      - COLUBK, background

DigitsBitmap:
    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###

    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #

    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %00110011                 ;  ##  ##
    .byte %00010001                 ;   #   #
    .byte %01110111                 ; ### ###

    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #

    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %01110111                 ; ### ###

    .byte %00100010                 ;  #   #
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #

    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01100110                 ; ##  ##
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01000100                 ; #   #
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###

    .byte %01100110                 ; ##  ##
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01100110                 ; ##  ##

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01100110                 ; ##  ##
    .byte %01000100                 ; #   #
    .byte %01000100                 ; #   #

MushSprite0:                        ;
    .byte #%11111111                ; ########
    .byte #%00011000                ;    ##
    .byte #%00011000                ;    ##
    .byte #%01111110                ;  ######
    .byte #%01111110                ;  ######
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%00011000                ;    ##

MushColor0:                         ;
    .byte #$F2                      ;
    .byte #$0C                      ;
    .byte #$0C                      ;
    .byte #$42                      ;
    .byte #$42                      ;
    .byte #$42                      ;
    .byte #$42                      ;
    .byte #$42                      ;

EnemyGfx0:                          ;
    .byte #%01100110                ;  ##  ##
    .byte #%00011000                ;    ##
    .byte #%10111101                ; # #### #
    .byte #%10100101                ; # #  # #
    .byte #%01111110                ;  ######
    .byte #%01011010                ;  # ## #
    .byte #%01111110                ;  ######
    .byte #%00111100                ;   ####

EnemyGfx1:                          ;
    .byte #%00100100                ;   #  #
    .byte #%00011000                ;    ##
    .byte #%00111100                ;   ####
    .byte #%10110101                ; # ## # #
    .byte #%01111110                ;  ######
    .byte #%01011010                ;  # ## #
    .byte #%01111110                ;  ######
    .byte #%00111100                ;   ####
        
EnemyColor0:                        ; TODO: hardcode in draw proc.
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;

PlayerGfx0:                         ;
    .byte #%01100110                ;  ##  ##
    .byte #%00100100                ;   #  #
    .byte #%00100100                ;   #  #
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%10111101                ; # #### #
    .byte #%01111110                ;  ######
    .byte #%00011000                ;    ##
    .byte #%00011000                ;    ##
    .byte #%00111100                ;   ####
    .byte #%01110110                ;  ### ##
    .byte #%01111110                ;  ######
    .byte #%01011010                ;  # ## #
    .byte #%00111100                ;   ####

PlayerGfx1:                         ; TODO: Gfx1 and Gfx2 use reflection instead
    .byte #%11001100                ; ##  ##
    .byte #%01000100                ;  #   #
    .byte #%01000100                ;  #   #
    .byte #%01111100                ;  #####
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%00111101                ;   #### #
    .byte #%01111110                ;  ######
    .byte #%10011000                ; #  ##
    .byte #%00011000                ;    ##
    .byte #%00101100                ;   # ##
    .byte #%01101110                ;  ## ###
    .byte #%01111110                ;  ######
    .byte #%01010110                ;  # # ##
    .byte #%00111100                ;   ####

PlayerGfx2:                         ;
    .byte #%00110011                ;   ##  ##
    .byte #%00100010                ;   #   #
    .byte #%00100010                ;   #   #
    .byte #%00111110                ;   #####
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%10111100                ; # ####
    .byte #%01111110                ;  ######
    .byte #%00011001                ;    ##  #
    .byte #%00011000                ;    ##
    .byte #%00110100                ;   ## #
    .byte #%01110110                ;  ### ##
    .byte #%01111110                ;  ######
    .byte #%01101010                ;  ## # #
    .byte #%00111100                ;   ####
    
PlayerGfx3:                         ;
    .byte #%01100110                ;  ##  ##
    .byte #%00100100                ;   #  #
    .byte #%00100100                ;   #  #
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%10111101                ; # #### #
    .byte #%01111110                ;  ######
    .byte #%00011000                ;    ##
    .byte #%00011000                ;    ##
    .byte #%00111100                ;   ####
    .byte #%01111110                ;  ######
    .byte #%01111110                ;  ######
    .byte #%01111110                ;  ######
    .byte #%00111100                ;   ####
    
PlayerColor0:                       ; 
    .byte #$F0                      ;
    .byte #$A0                      ;
    .byte #$A0                      ;
    .byte #$A0                      ;
    .byte #$A0                      ;
    .byte #$30                      ;
    .byte #$04                      ;
    .byte #$40                      ;
    .byte #$40                      ;
    .byte #$40                      ;
    .byte #$FC                      ;
    .byte #$FC                      ;
    .byte #$FC                      ;
    .byte #$FC                      ;
    .byte #$FC                      ;
    .byte #$FC                      ;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Complete ROM SIZE                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    org $FFFA                       ; Complete ROM size to 4KB
    .word Reset                     ; NMI vector
    .word Reset                     ; Reset vector
    .word Reset                     ; IRQ vector
