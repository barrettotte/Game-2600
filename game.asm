;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;                             Defender 2600                                 ;;
;;                           Barrett Otte 2020                               ;;
;;                                                                           ;;
;; TODO: write a description                                                 ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    processor 6502                  ; set processor type for DASM assembler

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Include header files with VCS registers, memory mappings, and macros    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Declare variables/constants                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg.u Variables                 ; begin variables segment
    org $80                         ; range $80 to $FF

Score               .byte           ; $80 : two digit score as BCD
Timer               .byte           ; $81 : two digit timer as BCD
DigitOnes           .word           ; $82-$83 : ones digit sprite offsets
DigitTens           .word           ; $84-$85 : tens digit sprite offsets
ScoreGfx            .byte           ; $86 : score graphics data
TimerGfx            .byte           ; $87 : timer graphics data
Temp                .byte           ; $88 : general purpose temp variable

PlayerXPos          .byte           ; player x position
PlayerYPos          .byte           ; player y position
EnemyXPos           .byte           ; enemy x position
EnemyYPos           .byte           ; enemy y position
MushXPos            .byte           ; mushroom x position TODO: needed?
MushYPos            .byte           ; mushroom y position TODO: needed?

PlayerSprPtr        .word           ; pointer to player sprite table
PlayerColPtr        .word           ; pointer to player color table
EnemySprPtr         .word           ; pointer to enemy sprite table
EnemyColPtr         .word           ; pointer to enemy color table     TODO: needed?
MushSprPtr          .word           ; pointer to mushroom sprite table TODO: needed?
MushColPtr          .word           ; pointer to mushroom color table  TODO: needed?

PlayerSprOffset     .word           ; player sprite table offset for animation
EnemySprOffset      .word           ; enemy sprite table offset for animation
MushSprOffset       .word           ; mushroom sprite table offset for animation TODO: needed?

RandomComm          .byte           ; common seed for random

; Constants
DIGITS_HEIGHT       equ 5           ; height of digit sprite
PLAYER_HEIGHT       equ 17          ; height of player sprite
ENEMY_HEIGHT        equ 9           ; height of enemy sprite
MUSH_HEIGHT         equ 9           ; height of mushroom sprite

BG_COLOR            equ #$84        ; blue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ROM Entry Point                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg Code                        ; begin code segment
    org $F800                       ; 2K ROM start $F800, 4K ROM start $F000

Reset:                              ; entry point label
    CLEAN_START                     ; clean memory and registers TODO: Move to this file

    ; TODO: Reset button logic

    ; init variables
    lda #10                         ;
    sta PlayerYPos                  ; set player starting y position
    lda #0                          ;
    sta PlayerXPos                  ; set player starting x position
    
    lda #15                         ;
    sta EnemyYPos                   ; set enemy starting y position
    lda #10                         ;
    sta EnemyXPos                   ; set enemy starting x position

    lda #20                         ;                                   TODO: needed?
    sta MushYPos                    ; set mushroom starting y position  TODO: needed?
    lda #10                         ;                                   TODO: needed?
    sta MushXPos                    ; set mushroom starting x position  TODO: needed?
    
    lda #0                          ;
    sta Score                       ; reset score
    sta Timer                       ; reset timer


    ; init lookup table pointers
    lda #<PlayerSprite0             ; set player sprite pointer
    sta PlayerSprPtr                ; lo byte
    lda #>PlayerSprite0             ; 
    sta PlayerSprPtr+1              ; hi byte

    lda #<PlayerColor0              ; set player color pointer
    sta PlayerColPtr                ; lo byte
    lda #>PlayerColor0              ; 
    sta PlayerColPtr+1              ; hi byte

    lda #<EnemySprite0              ; set enemy sprite pointer
    sta EnemySprPtr                 ; lo byte
    lda #>EnemySprite0              ; 
    sta EnemySprPtr+1               ; hi byte

    lda #<EnemyColor0               ; set enemy color pointer
    sta EnemyColPtr                 ; lo byte
    lda #>EnemyColor0               ;
    sta EnemyColPtr+1               ; hi byte

    lda #<MushSprite0               ; set mushroom sprite pointer   TODO: needed?
    sta MushSprPtr                  ; lo byte                       TODO: needed?
    lda #>MushSprite0               ;                               TODO: needed?
    sta MushSprPtr+1                ; hi byte                       TODO: needed?

    lda #<MushColor0                ; set mushroom color pointer    TODO: needed?
    sta MushColPtr                  ; lo byte                       TODO: needed?
    lda #>MushColor0                ;                               TODO: needed?
    sta MushColPtr+1                ; hi byte                       TODO: needed?


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
    sta WSYNC                       ; wait for first scanline
    sta WSYNC                       ; wait for second scanline
    lda #0                          ;
    sta PF0                         ; clear playfield 0
    sta PF1                         ; clear playfield 1
    sta PF2                         ; clear playfield 2
    sta WSYNC                       ; wait for third scanline
    sta VSYNC                       ; VSYNC off
    rts                             ; end VerticalSync subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           [VerticalBlank]                                 ;;
;;  Game logic                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VerticalBlank subroutine            ;
    jsr SetObjectColors             ; set object colors
    jsr SetupScoreboard             ; prepare scoreboard for display
    rts                             ; end VerticalBlank subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              [Kernel]                                     ;;
;;  Update TIA registers - 192() scanlines  (two line kernel)                ;;
;;  Timing is critical. I will be counting cycles for each instruction.      ;;
;;  (This formatting was taken from https://www.randomterrain.com)           ;;
;;  EX:  instruction ; XX YY - comment                                       ;;
;;          XX = cycles to execute,  YY = cycle count for current scanline   ;;
;;          @AA-BB = instruction must happen at this range of cycles         ;;
;;          (XX YY) = cycles and cycle count if branch is taken              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Kernel subroutine                   ;
    sta WSYNC                       ;         wait for next scanline
    ; ----------------------------- ;         new scanline
    lda INTIM                       ;  4  4 - check timer
    bne Kernel                      ;  2  6 - (3 7) while(x != 0)
    
    sta VBLANK                      ;  3  9 - VBLANK off
    ldx #5                          ;  2 11 - scoreboard iterator

ScoreboardLoop:                     ;    43
    ldy DigitTens                   ;  3 46 - tens digit offset for score
    lda DigitsBitmap,Y              ;  5 51 - load tens digit graphics
    and #$F0                        ;  2 53 - mask lo 4 bits (ones place)
    sta ScoreGfx                    ;  3 56

    ldy DigitOnes                   ;  3 59 - ones digit offset for score
    lda DigitsBitmap,Y              ;  5 64 - load ones digit graphics
    and #$0F                        ;  2 66 - mask hi 4 bits (tens place)

    ora ScoreGfx                    ;  3 69 - merge tens and ones digit gfx
    sta ScoreGfx                    ;  3 72
    sta WSYNC                       ;  3 75 - wait for next scanline
    ; ----------------------------- ;         new scanline
    sta PF1                         ;  3  3 - @66-28,update playfield for score
    ldy DigitTens+1                 ;  3  6 - tens digit offset for timer
    lda DigitsBitmap,Y              ;  5 11 - load tens digit graphics
    and #$F0                        ;  2 13 - mask lo 4 bits (ones place)
    sta TimerGfx                    ;  3 16
    ldy DigitOnes+1                 ;  3 19 - ones digit offset for timer
    lda DigitsBitmap,Y              ;  5 24 - load ones digit graphics
    and #$0F                        ;  2 26 - mask hi 4 bits (tens place)
    ora TimerGfx                    ;  3 29 - merge tens and ones digit gfx
    sta TimerGfx                    ;  3 32
    jsr Sleep12                     ; 12 44 - waste 12 cycles
    sta PF1                         ;  3 47 - @39-54,update playfield for timer
    ldy ScoreGfx                    ;  3 50 - preload for next scanline
    sta WSYNC                       ;  3 53 - wait for next scanline
    ; ----------------------------- ;         new scanline
    sty PF1                         ;  3  3 - @66-28, update playfield for score
    inc DigitTens                   ;  5  8 - next line of graphics, score tens
    inc DigitTens+1                 ;  5 13 - next line of graphics, timer tens
    inc DigitOnes                   ;  5 18 - next line of graphics, score ones
    inc DigitOnes+1                 ;  5 23 - next line of graphics, timer ones
    jsr Sleep12                     ; 12 35 - waste 12 cycles
    dex                             ;  2 37 - x--
    sta PF1                         ;  3 40 - @39-54,update playfield for timer
    bne ScoreboardLoop              ;  2 42 - (3 43) while(x != 0)
    sta WSYNC                       ;  3 45 - wait for next scanline
    ; ----------------------------- ;         new scanline
    stx PF1                         ;  3  3 - clear out playfield (x=0)
    sta WSYNC                       ;  3  6 - wait for next scanline
    ; ----------------------------- ;         new scanline
    sta WSYNC                       ;  3  3 - wait for next scanline
    ; ----------------------------- ;         new scanline
    ldx #179                        ;  2  2 - 180 remaining visible scanlines
.KernelLoop:                        ; 
    sta WSYNC                       ;  3  5 - wait for next scanline
    ; ----------------------------- ;         new scanline
    stx COLUBK                      ;  3  3 - set background color
    dex                             ;  2  5 - x--
    bne .KernelLoop                 ;  2  7 - (3 8) while(x != 0)
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

    ; TODO: game logic
.Wait:
    sta WSYNC                       ; wait for next scanline
    lda INTIM                       ; check timer
    bne .Wait                       ; while(x != 0)
    rts                             ; end Overscan subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          [SetObjectColors]                                ;;
;;  Set object colors for color or black and white TV                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectColors subroutine
    ldx #3                          ; setting 4 colors
    ldy #3                          ; default to use color
    lda SWCHB                       ; read console switches
    and #%00001000                  ; mask for TV Type switch
    bne .ColorLoop                  ; if 3rd bit on, use color
    ldy #7                          ; else, use black and white colors

.ColorLoop:
    lda ObjectColors,Y              ; load color from table
    sta COLUP0,X                    ; set object color
    dey                             ; y--
    dex                             ; x--
    bpl .ColorLoop                  ; while(x > 0)
    rts                             ; end SetObjectColors subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        [SetupScoreboard]                                  ;;
;;  Setup BCD digits for timer and score variables                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetupScoreboard subroutine
    inc Timer                       ; TODO: This is a test
    bne .Skip                       ; TODO: ^
    inc Score                       ; TODO: ^

.Skip:
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
    .byte #%0000000                 ;
    .byte #%11111111                ; ########
    .byte #%00011000                ;    ##
    .byte #%00011000                ;    ##
    .byte #%01111110                ;  ######
    .byte #%01111110                ;  ######
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%00011000                ;    ##    

MushSprite1: ; TODO: Might not need this...same as AnimShroom0
    .byte #%0000000                 ;
    .byte #%11111111                ; ########
    .byte #%00011000                ;    ##
    .byte #%00011000                ;    ##
    .byte #%01111110                ;  ######
    .byte #%01111110                ;  ######
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%00011000                ;    ## 

MushColor0: ; TODO: Might not need this...Make subroutine, add #$02 instead
    .byte #$00                      ;
    .byte #$F2                      ;
    .byte #$0C                      ;
    .byte #$0C                      ;
    .byte #$42                      ;
    .byte #$42                      ;
    .byte #$42                      ;
    .byte #$42                      ;
    .byte #$42                      ;
    
MushColor1:                         ; TODO: Needed?
    .byte #$00
    .byte #$F2                      ;
    .byte #$0C                      ;
    .byte #$0C                      ;
    .byte #$44                      ;
    .byte #$44                      ;
    .byte #$44                      ;
    .byte #$44                      ;
    .byte #$44                      ;

EnemySprite0:                       ;
    .byte #%00000000                ;
    .byte #%01100110                ;  ##  ##
    .byte #%00011000                ;    ##
    .byte #%10111101                ; # #### #
    .byte #%10100101                ; # #  # #
    .byte #%01111110                ;  ######
    .byte #%01011010                ;  # ## #
    .byte #%01111110                ;  ######
    .byte #%00111100                ;   ####

EnemySprite1:                       ;
    .byte #%00000000                ;
    .byte #%00100100                ;   #  #
    .byte #%00011000                ;    ##
    .byte #%00111100                ;   ####
    .byte #%10110101                ; # ## # #
    .byte #%01111110                ;  ######
    .byte #%01011010                ;  # ## #
    .byte #%01111110                ;  ######
    .byte #%00111100                ;   ####
        
EnemyColor0:                        ; TODO: Might not need this at all, just hardcode in subr
    .byte #$00                      ; 
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;

EnemyColor1:                        ; TODO: See above
    .byte #$00                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;
    .byte #$D6                      ;

PlayerSprite0:                      ;
    .byte #%00000000                ;
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

PlayerSprite1:                      ;
    .byte #%00000000                ;
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

PlayerSprite2:                      ;
    .byte #%00000000                ;
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
    
PlayerSprite3:                      ;
    .byte #%00000000                ;
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
    
PlayerColor0:                       ; TODO: Might not need the rest, player always has same color
    .byte #$00                      ;
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

PlayerColor1: ;TODO: See above
    .byte #$00                      ;
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

PlayerColor2: ;TODO: See above
    .byte #$00                      ;
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

PlayerColor3: ;TODO: See above
    .byte #$00                      ;
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
