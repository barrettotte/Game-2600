;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Game TBD                                                                  ;;
;; Barrett Otte 2020                                                         ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Include header files with VCS registers, memory mappings, and macros    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Declare variables/constants                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80                         ; range $80 to $FF

Random              .byte           ; general seed for random

; Constants
DIGITS_HEIGHT       equ 5           ; height of digit bitmap

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ROM Entry Point                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

Reset:
    CLEAN_START                     ; clean memory and registers

    ; Variables and TIA registers
    ldx #$80                        ; blue
    stx COLUBK                      ; set background color

    ; Init lookup table pointers
    ;   TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Process New Frame                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NewFrame:                           ; frame = 262 scanlines
    
    ; Calculations
    ;   TODO

    ; Setup frame
    lda #2
    sta VBLANK                      ; VBLANK on
    sta VSYNC                       ; VSYNC on
    sta WSYNC                       ; Three lines of VSYNC
    sta WSYNC
    sta WSYNC                       
    lda #0
    sta VSYNC                       ; VSYNC off

    ldx #37
DrawVBLANK:                         ; Draw VBLANK lines
    sta WSYNC                       ; Wait for next scanline
    dex
    bne DrawVBLANK                  ; while(X GT 0) draw VBLANK
    lda #0
    sta VBLANK                      ; VBLANK off

DrawScoreboard:
    ; TODO

SetupVisibleArea:
    lda #$84                        ; blue
    sta COLUBK                      ; set background color
    lda #$FF                        ; yellow
    sta COLUPF                      ; set playfield color

    lda #%00000001                  ;
    sta CTRLPF                      ; enable playfield reflection
    lda #$F0                        
    sta PF0                         ; set playfield 0 bit pattern
    lda #$FC
    sta PF1                         ; set playfield 1 bit pattern
    lda #0
    sta PF2                         ; set playfield 2 bit pattern

    ldx #192                        
DrawVisibleLines:                   ; remaining lines = 192
    sta WSYNC                       ; wait for next scanline
    dex
    bne DrawVisibleLines            ; while(X GT 192)
    
    ; TODO draw sprites

    lda #2
    sta VBLANK                      ; VBLANK on
    ldx #30                         ; draw 30 overscan lines
Overscan:
    dex
    bne Overscan                    ; while(X GT 0) draw overscan
    lda #0
    sta VBLANK                      ; VBLANK off

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Check Player 0 Input                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000                  ; P0 up?
    bit SWCHA
    bne CheckP0Down                 ; if(!up) check down
    ; TODO UP pressed

CheckP0Down:
    lda #%00100000                  ; P0 down?
    bit SWCHA
    bne CheckP0Left                 ; if(!down) check left
    ; TODO DOWN pressed

CheckP0Left:
    lda #%01000000                  ; P0 left?
    bit SWCHA
    bne CheckP0Right                ; if(!left) check right
    ; TODO LEFT pressed

CheckP0Right:
    lda #%10000000                  ; P0 right?
    bit SWCHA
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


FinishFrame:
    jmp NewFrame


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           [SetObjectXPos]                                 ;;
;;  Summary: Set object horizontal position                                  ;;
;;  Params:  A - target x position                                           ;;
;;             Y - object type [0:P0, 1:P1, 2:M0, 3:M1, 4:BALL]              ;;
;;  Returns: A - New object x position                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC
    sec                             ; set carry
.Div15:
    sbc #5                          ; coarse position
    bcs .Div15                      ; while(!C)
    eor #7                          ; adjust offset -8 to +7
    asl                             ; shift left 4, HMP0 uses 4 bits
    asl
    asl
    asl
    sta HMP0,Y                      ; set fine position, offset of obj type
    sta RESP0                       ; reset 15-step coarse position
    rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ROM Lookup Tables                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111         ; ### ###
    .byte %01010101         ; # # # #
    .byte %01010101         ; # # # #
    .byte %01010101         ; # # # #
    .byte %01110111         ; ### ###

    .byte %00010001         ;   #   #
    .byte %00010001         ;   #   #
    .byte %00010001         ;   #   #
    .byte %00010001         ;   #   #
    .byte %00010001         ;   #   #

    .byte %01110111         ; ### ###
    .byte %00010001         ;   #   #
    .byte %01110111         ; ### ###
    .byte %01000100         ; #   #
    .byte %01110111         ; ### ###

    .byte %01110111         ; ### ###
    .byte %00010001         ;   #   #
    .byte %00110011         ;  ##  ##
    .byte %00010001         ;   #   #
    .byte %01110111         ; ### ###

    .byte %01010101         ; # # # #
    .byte %01010101         ; # # # #
    .byte %01110111         ; ### ###
    .byte %00010001         ;   #   #
    .byte %00010001         ;   #   #

    .byte %01110111         ; ### ###
    .byte %01000100         ; #   #
    .byte %01110111         ; ### ###
    .byte %00010001         ;   #   #
    .byte %01110111         ; ### ###

    .byte %01110111         ; ### ###
    .byte %01000100         ; #   #
    .byte %01110111         ; ### ###
    .byte %01010101         ; # # # #
    .byte %01110111         ; ### ###

    .byte %01110111         ; ### ###
    .byte %00010001         ;   #   #
    .byte %00010001         ;   #   #
    .byte %00010001         ;   #   #
    .byte %00010001         ;   #   #

    .byte %01110111         ; ### ###
    .byte %01010101         ; # # # #
    .byte %01110111         ; ### ###
    .byte %01010101         ; # # # #
    .byte %01110111         ; ### ###

    .byte %01110111         ; ### ###
    .byte %01010101         ; # # # #
    .byte %01110111         ; ### ###
    .byte %00010001         ;   #   #
    .byte %01110111         ; ### ###

    .byte %00100010         ;  #   #
    .byte %01010101         ; # # # #
    .byte %01110111         ; ### ###
    .byte %01010101         ; # # # #
    .byte %01010101         ; # # # #

    .byte %01110111         ; ### ###
    .byte %01010101         ; # # # #
    .byte %01100110         ; ##  ##
    .byte %01010101         ; # # # #
    .byte %01110111         ; ### ###

    .byte %01110111         ; ### ###
    .byte %01000100         ; #   #
    .byte %01000100         ; #   #
    .byte %01000100         ; #   #
    .byte %01110111         ; ### ###

    .byte %01100110         ; ##  ##
    .byte %01010101         ; # # # #
    .byte %01010101         ; # # # #
    .byte %01010101         ; # # # #
    .byte %01100110         ; ##  ##

    .byte %01110111         ; ### ###
    .byte %01000100         ; #   #
    .byte %01110111         ; ### ###
    .byte %01000100         ; #   #
    .byte %01110111         ; ### ###

    .byte %01110111         ; ### ###
    .byte %01000100         ; #   #
    .byte %01100110         ; ##  ##
    .byte %01000100         ; #   #
    .byte %01000100         ; #   #

AnimShroom0:
    .byte #%11111111        ; ########
    .byte #%00011000        ;    ##
    .byte #%00011000        ;    ##
    .byte #%01111110        ;  ######
    .byte #%01111110        ;  ######
    .byte #%00111100        ;   ####
    .byte #%00111100        ;   ####
    .byte #%00011000        ;    ##

AnimShroom1:
    .byte #%11111111        ; $F2
    .byte #%00011000        ; $0C
    .byte #%00011000        ; $0C
    .byte #%01111110        ; $44
    .byte #%01111110        ; $44
    .byte #%00111100        ; $44
    .byte #%00111100        ; $44
    .byte #%00011000        ; $44

ColorShroom0:
    .byte #$F2
    .byte #$0C
    .byte #$0C
    .byte #$42
    .byte #$42
    .byte #$42
    .byte #$42
    .byte #$42

ColorShroom1:
    .byte #$F2
    .byte #$0C
    .byte #$0C
    .byte #$44
    .byte #$44
    .byte #$44
    .byte #$44
    .byte #$44

AnimEnemy0:
        .byte #%01100110    ;  ##  ##
        .byte #%00011000    ;    ##
        .byte #%10111101    ; # #### #
        .byte #%10100101    ; # #  # #
        .byte #%01111110    ;  ######
        .byte #%01011010    ;  # ## #
        .byte #%01111110    ;  ######
        .byte #%00111100    ;   ####

AnimEnemy1:
        .byte #%00100100    ;   #  #
        .byte #%00011000    ;    ##
        .byte #%00111100    ;   ####
        .byte #%10110101    ; # ## # #
        .byte #%01111110    ;  ######
        .byte #%01011010    ;  # ## #
        .byte #%01111110    ;  ######
        .byte #%00111100    ;   ####

ColorEnemy0:
        .byte #$D6
        .byte #$D6
        .byte #$D6
        .byte #$D6
        .byte #$D6
        .byte #$D6
        .byte #$D6
        .byte #$D6

ColorEnemy1:
        .byte #$D6
        .byte #$D6
        .byte #$D6
        .byte #$D6
        .byte #$D6
        .byte #$D6
        .byte #$D6
        .byte #$D6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Complete ROM SIZE                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC               ; Complete ROM size to 4KB
    .word Reset             ; write 2 bytes for program entry
    .word Reset             ; write 2 bytes for interrupt vector


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



