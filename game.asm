;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Game TBD                                                                  ;;
;; Barrett Otte 2020                                                         ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    processor 6502                  ; set processor type for DASM assembler

; ------------Objects------------
; Player 0 - Player
; Player 1 - Enemy

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

Score               .byte           ; two digit score as BCD
Timer               .byte           ; two digit timer as BCD
DigitOffset1        .byte           ; ones place digit offset
DigitOffset10       .byte           ; tens place digit offset
ScoreSprite         .byte           ; sprite value of score
TimerSprite         .byte           ; sprite value of timer

RandomComm          .byte           ; common seed for random

; Constants
DIGITS_HEIGHT       equ 5           ; height of digit sprite
PLAYER_HEIGHT       equ 16          ; height of player sprite
ENEMY_HEIGHT        equ 8           ; height of enemy sprite
MUSH_HEIGHT         equ 8           ; height of mushroom sprite

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ROM Entry Point                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code                        ; begin code segment
    org $F000                       ; start ROM at memory address $F000

Reset:
    CLEAN_START                     ; clean memory and registers TODO: Move to this file

    ; Variables and TIA registers
    ldx #$80                        ; blue
    stx COLUBK                      ; set background color

    ; Init lookup table pointers
    lda #<Player0Sprite             ; lo byte
    sta PlayerSprPtr                ;
    lda #>Player0Sprite             ;
    sta PlayerSprPtr+1              ; hi byte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Process New Frame                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NewFrame:                           ; frame = 262 scanlines
    
    ; Calculations before draw
    ;   TODO

    ; Setup frame
    lda #2                          ;
    sta VBLANK                      ; VBLANK on
    sta VSYNC                       ; VSYNC on
    sta WSYNC                       ; Three lines of VSYNC
    sta WSYNC                       ;
    sta WSYNC                       ;
    lda #0                          ;
    sta VSYNC                       ; VSYNC off

    ldx #37
DrawVBLANK:                         ; Draw VBLANK lines
    sta WSYNC                       ; Wait for next scanline
    dex                             ; x--
    bne DrawVBLANK                  ; while(X GT 0) draw VBLANK
    lda #0                          ;
    sta VBLANK                      ; VBLANK off

DrawScoreboard:
    ; TODO

SetupVisibleArea:
    lda #$84                        ; blue
    sta COLUBK                      ; set background color
    lda #$FF                        ; yellow
    sta COLUPF                      ; set playfield color

    lda #%00000001                  ; TODO: comment
    sta CTRLPF                      ; enable playfield reflection
    lda #$F0                        ; TODO: comment
    sta PF0                         ; set playfield 0 bit pattern
    lda #$FC                        ; TODO: comment
    sta PF1                         ; set playfield 1 bit pattern
    lda #0                          ; TODO: comment
    sta PF2                         ; set playfield 2 bit pattern

    ldx #192                        
DrawVisibleLines:                   ; remaining lines = 192
    sta WSYNC                       ; wait for next scanline
    dex                             ; x--
    bne DrawVisibleLines            ; while(X GT 192)
    
    ; TODO draw sprites

    lda #2                          ;
    sta VBLANK                      ; VBLANK on
    ldx #30                         ; draw 30 overscan lines
Overscan:
    dex                             ; x--
    bne Overscan                    ; while(X GT 0) draw overscan
    lda #0                          ;
    sta VBLANK                      ; VBLANK off

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Check Player 0 Input                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000                  ; P0 up?
    bit SWCHA                       ;
    bne CheckP0Down                 ; if(!up) check down
    ; TODO UP pressed

CheckP0Down:
    lda #%00100000                  ; P0 down?
    bit SWCHA                       ;
    bne CheckP0Left                 ; if(!down) check left
    ; TODO DOWN pressed

CheckP0Left:
    lda #%01000000                  ; P0 left?
    bit SWCHA                       ;
    bne CheckP0Right                ; if(!left) check right
    ; TODO LEFT pressed

CheckP0Right:
    lda #%10000000                  ; P0 right?
    bit SWCHA                       ;
    bne NoInputP0                   ; if(!right) end input check
    ; TODO RIGHT pressed

NoInputP0:
    ; TODO No Input

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Update Positions                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Check Collisions                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO


; Calculations before next frame


FinishFrame:
    jmp NewFrame                    ; start next frame


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           [SetObjectXPos]                                 ;;
;;  Summary: Set object horizontal position                                  ;;
;;  Parms:   A - target x position                                           ;;
;;           Y - object type [0:P0, 1:P1, 2:M0, 3:M1, 4:BALL]                ;;
;;  Returns: A - New object x position                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine            ;
    sta WSYNC                       ; wait for next scanline
    sec                             ; set carry
.Div15:                             ;
    sbc #5                          ; coarse position
    bcs .Div15                      ; while(!C)
    eor #7                          ; adjust offset -8 to +7
    asl                             ; shift left 4, HMP0 uses 4 bits
    asl                             ;
    asl                             ;
    asl                             ;
    sta HMP0,Y                      ; set fine position, offset of obj type
    sta RESP0                       ; reset 15-step coarse position
    rts                             ; end SetObjectXPos subroutine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ROM Lookup Tables                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
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

MushSprite0:
    .byte #%00011000                ;    ##
    .byte #%00111100                ;   ####
    .byte #%00111100                ;   ####
    .byte #%01111110                ;  ######
    .byte #%01111110                ;  ######
    .byte #%00011000                ;    ##
    .byte #%00011000                ;    ##
    .byte #%11111111                ; ########


MushSprite1: ; TODO: Might not need this...same as AnimShroom0
    .byte #%00011000                ; $44
    .byte #%00111100                ; $44
    .byte #%00111100                ; $44
    .byte #%01111110                ; $44
    .byte #%01111110                ; $44
    .byte #%00011000                ; $0C
    .byte #%00011000                ; $0C
    .byte #%11111111                ; $F2

MushColor0: ; TODO: Might not need this...Make subroutine, add #$02 instead
    .byte #$42                      ;
    .byte #$42                      ;
    .byte #$42                      ;
    .byte #$42                      ;
    .byte #$42                      ;
    .byte #$0C                      ;
    .byte #$0C                      ;
    .byte #$F2                      ;

MushColor1:
    .byte #$44                      ;
    .byte #$44                      ;
    .byte #$44                      ;
    .byte #$44                      ;
    .byte #$44                      ;
    .byte #$0C                      ;
    .byte #$0C                      ;
    .byte #$F2                      ;

EnemySprite0:
        .byte #%00111100            ;   ####
        .byte #%01111110            ;  ######
        .byte #%01011010            ;  # ## #
        .byte #%01111110            ;  ######
        .byte #%10100101            ; # #  # #
        .byte #%10111101            ; # #### #
        .byte #%00011000            ;    ##
        .byte #%01100110            ;  ##  ##

EnemySprite1:
        .byte #%00111100            ;   ####
        .byte #%01111110            ;  ######
        .byte #%01011010            ;  # ## #
        .byte #%01111110            ;  ######
        .byte #%10110101            ; # ## # #
        .byte #%00111100            ;   ####
        .byte #%00011000            ;    ##
        .byte #%00100100            ;   #  #

EnemyColor0: ; TODO: Might not need this at all, just hardcode in subr
        .byte #$D6                  ;
        .byte #$D6                  ;
        .byte #$D6                  ;
        .byte #$D6                  ;
        .byte #$D6                  ;
        .byte #$D6                  ;
        .byte #$D6                  ;
        .byte #$D6                  ;

EnemyColor1: ; TODO: See above
        .byte #$D6                  ;
        .byte #$D6                  ;
        .byte #$D6                  ;
        .byte #$D6                  ;
        .byte #$D6                  ;
        .byte #$D6                  ;
        .byte #$D6                  ;
        .byte #$D6                  ;

PlayerSprite0:
        .byte #%00111100            ;   ####
        .byte #%01011010            ;  # ## #
        .byte #%01111110            ;  ######
        .byte #%01110110            ;  ### ##
        .byte #%00111100            ;   ####
        .byte #%00011000            ;    ##
        .byte #%00011000            ;    ##
        .byte #%01111110            ;  ######
        .byte #%10111101            ; # #### #
        .byte #%00111100            ;   ####
        .byte #%00111100            ;   ####
        .byte #%00111100            ;   ####
        .byte #%00111100            ;   ####
        .byte #%00100100            ;   #  #
        .byte #%00100100            ;   #  #
        .byte #%01100110            ;  ##  ##
        .byte #%00000000            ; TODO: Might not need this technique?

PlayerSprite1:
        .byte #%00111100            ;   ####
        .byte #%01010110            ;  # # ##
        .byte #%01111110            ;  ######
        .byte #%01101110            ;  ## ###
        .byte #%00101100            ;   # ##
        .byte #%00011000            ;    ##
        .byte #%10011000            ; #  ##
        .byte #%01111110            ;  ######
        .byte #%00111101            ;   #### #
        .byte #%00111100            ;   ####
        .byte #%00111100            ;   ####
        .byte #%00111100            ;   ####
        .byte #%01111100            ;  #####
        .byte #%01000100            ;  #   #
        .byte #%01000100            ;  #   #
        .byte #%11001100            ; ##  ##

PlayerSprite2:
        .byte #%00111100            ;   ####
        .byte #%01101010            ;  ## # #
        .byte #%01111110            ;  ######
        .byte #%01110110            ;  ### ##
        .byte #%00110100            ;   ## #
        .byte #%00011000            ;    ##
        .byte #%00011001            ;    ##  #
        .byte #%01111110            ;  ######
        .byte #%10111100            ; # ####
        .byte #%00111100            ;   ####
        .byte #%00111100            ;   ####
        .byte #%00111100            ;   ####
        .byte #%00111110            ;   #####
        .byte #%00100010            ;   #   #
        .byte #%00100010            ;   #   #
        .byte #%00110011            ;   ##  ##

PlayerSprite3:
        .byte #%00111100            ;   ####
        .byte #%01111110            ;  ######
        .byte #%01111110            ;  ######
        .byte #%01111110            ;  ######
        .byte #%00111100            ;   ####
        .byte #%00011000            ;    ##
        .byte #%00011000            ;    ##
        .byte #%01111110            ;  ######
        .byte #%10111101            ; # #### #
        .byte #%00111100            ;   ####
        .byte #%00111100            ;   ####
        .byte #%00111100            ;   ####
        .byte #%00111100            ;   ####
        .byte #%00100100            ;   #  #
        .byte #%00100100            ;   #  #
        .byte #%01100110            ;  ##  ##

PlayerColor0: ; TODO: Might not need the rest, player always has same color
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$40                  ;
        .byte #$40                  ;
        .byte #$40                  ;
        .byte #$04                  ;
        .byte #$30                  ;
        .byte #$A0                  ;
        .byte #$A0                  ;
        .byte #$A0                  ;
        .byte #$A0                  ;
        .byte #$F0                  ;

PlayerColor1: ;TODO: See above
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$40                  ;
        .byte #$40                  ;
        .byte #$40                  ;
        .byte #$04                  ;
        .byte #$30                  ;
        .byte #$A0                  ;
        .byte #$A0                  ;
        .byte #$A0                  ;
        .byte #$A0                  ;
        .byte #$F0                  ;

PlayerColor2: ;TODO: See above
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$40                  ;
        .byte #$40                  ;
        .byte #$40                  ;
        .byte #$04                  ;
        .byte #$30                  ;
        .byte #$A0                  ;
        .byte #$A0                  ;
        .byte #$A0                  ;
        .byte #$A0                  ;
        .byte #$F0                  ;

PlayerColor3: ;TODO: See above
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$FC                  ;
        .byte #$40                  ;
        .byte #$40                  ;
        .byte #$40                  ;
        .byte #$04                  ;
        .byte #$30                  ;
        .byte #$A0                  ;
        .byte #$A0                  ;
        .byte #$A0                  ;
        .byte #$A0                  ;
        .byte #$F0                  ;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Complete ROM SIZE                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                       ; Complete ROM size to 4KB
    .word Reset                     ; write 2 bytes for program entry
    .word Reset                     ; write 2 bytes for interrupt vector


; Subroutines TODO
;   Multiply
;   Divide
;   Random (LFSR)
;   DigitOffset (scoreboard,timer)
;   DrawSprite
;   Sleep 

; Misc TODO
;   Reset switch logic
;   Difficulty switch logic



