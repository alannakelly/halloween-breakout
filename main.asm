!macro start_at .address {
  * = $0801
  !byte $0c,$08,$00,$00,$9e
  !if .address >= 10000 { !byte 48 + ((.address / 10000) % 10) }
  !if .address >=  1000 { !byte 48 + ((.address /  1000) % 10) }
  !if .address >=   100 { !byte 48 + ((.address /   100) % 10) }
  !if .address >=    10 { !byte 48 + ((.address /    10) % 10) }
  !byte $30 + (.address % 10), $00, $00, $00
  * = .address
}


!SOURCE "synthins.asm"

;* = $0801

+start_at $0900

* = $0900

    lda #$00
    sta $d020
    lda #$00
    sta $d021
    jsr cls
    jsr ccram

    ;Enable all sprites
    lda #$03
    sta $d015

    ;Set sprite multicolors
    lda #$07
    ldy #$0b
    sta $D025
    sty $D026


    ldx #$07
-
    lda sprite_mcm,x
    cmp #$01
    rol $d01c ;C->MCM
    dex
    bpl -

;set_tick_irq:
    ;Diable Maskeable interrupts.
    sei
    
    ;Disable CIA Timers
    lda #$7f
    sta $dc0d
    sta $dd0d
    
    ;ACK CIA Timer interrupts.
    lda $dc0d
    lda $dd0d

    ;Enable Raster IRQ
    lda #$01
    sta $d01a

    ;Set rasterline for interrupt.
    lda #$fb
    sta $d012

    lda #$1b
    sta $d011

    ;Switch off Kernal
    lda #$35
    sta $01
    
    ;Set hardware IRQ pointer
    lda #<main_irq
    sta $fffe
    lda #>main_irq
    sta $ffff

    ;Enable interrupts
    cli

    ;Endless loop
    jmp *

main_irq:
    inc $d020
    
    ;Main Game Loop
    jsr read_joystick
    jsr update_player
    jsr update_ball
    jsr update_pos
    jsr update_sprites
    jsr sprite_collide
    ldx #$04

    dec $d020
    
    asl $d019 ;ACK
    
    lda #$fa
    sta $d012

    lda #$1b
    sta $d011
    rti

read_joystick:
    djrr    lda $dc00     ; get input from port 2 only
    djrrb   ldy #0        ; this routine reads and decodes the
            ldx #0        ; joystick/firebutton input data in
            lsr           ; the accumulator. this least significant
            bcs djr0      ; 5 bits contain the switch closure
            dey           ; information. if a switch is closed then it
    djr0    lsr           ; produces a zero bit. if a switch is open then
            bcs djr1      ; it produces a one bit. The joystick dir-
            iny           ; ections are right, left, forward, backward
    djr1    lsr           ; bit3=right, bit2=left, bit1=backward,
            bcs djr2      ; bit0=forward and bit4=fire button.
            dex           ; at rts time dx and dy contain 2's compliment
    djr2    lsr           ; direction numbers i.e. $ff=-1, $00=0, $01=1.
            bcs djr3      ; dx=1 (move right), dx=-1 (move left),
            inx           ; dx=0 (no x change). dy=-1 (move up screen),
    djr3    lsr           ; dy=0 (move down screen), dy=0 (no y change).
            stx dx        ; the forward joystick position corresponds
            sty dy        ; to move up the screen and the backward
            rts           ; position to move down screen.
                          ;
                          ; at rts time the carry flag contains the fire
                          ; button state. if c=1 then button not pressed.
                          ; if c=0 then pressed.
    

dx !byte 0
dy !byte 0

update_player:
    clc
    +add_s8_to_u16 player_x, dx

    lda player_x
    sta sprite_x

    lda player_x + 1 ; MSB
    sta sprite_m

    clc ;Bugfix
    lda player_y
    sta sprite_y

    rts
    
update_ball:
    +add_s8_to_u16 ball_x, ball_dx
    ldx ball_x
    ldy ball_x + 1
    stx sprite_x + 1
    sty sprite_m + 1 

    lda ball_x + 1 ; msb
    cmp #1
    beq + ; don't check left bounds if MSB == 1
    
    lda #24
    cmp ball_x ; 24 >= ball_x
    bcc ++
    lda #$01
    sta ball_dx
    
+   lda ball_x
    cmp #64 ; ball_x >= 64
    bcc ++
    lda #$ff
    sta ball_dx

++  
    +add_delta ball_y, ball_dy
    sta sprite_y + 1
    
    lda #50
    cmp ball_y ; 50 >= ball_y
    bcc +
    lda #$01
    sta ball_dy
    
+   lda ball_y
    cmp #230 ; ball_y >= 230
    bcc +
    lda #$ff
    sta ball_dy
    
+   rts

sprite_collide:
    lda $d01e
	lsr
	bcc +
	lsr
	bcc +
	lda #$ff
    sta ball_dy
+   rts

update_pos:
     jsr btobin
     ldx #8
-    lda $41,x
     sta $0400,x
     dex
     bne -
     lda #24
     sta $0428
     lda ball_x + 1
     jsr btoa
     sty $0402
     stx $0403
     sta $0404
;    lda #25
;    sta $0406
;    lda dy
;    jsr btoa
;    sty $0408
;    stx $0409
;    sta $040a
;
;    lda #24
;    sta $0428
;    lda player_x+1
;    jsr btoa
;    sty $042a
;    stx $042b
;    sta $042c
;    lda #25
;    sta $042e
;    lda player_y
;    jsr btoa
;    sty $0430
;    stx $0431
;    sta $0432
    rts
    
btobin:
      lda $d01e
      ldx #8
      ldy #48
-     sty $41,x
      lsr
      bcc +
      inc $41,x
+     dex
      bne -
      rts
  

btoa:
      ldy #$2f
      ldx #$3a
      sec
-     iny
      sbc #100
      bcs -
-     dex
      adc #10
      bmi -
      adc #$2f
      rts

update_sprites:
      ldx #$07
      ldy #$0e

      loop:
      lda sprite_x,x
      sta $d000,y
      lda sprite_y,x
      sta $d001,y
      
      lda sprite_m,x
      cmp #$01
      rol $d010 ;C->MSB
      
      clc
      lda sprite_mcm,x
      cmp #$01
      rol $d01c ;C->MCM
      
      lda sprite_c,x
      sta $d027,x
      lda sprite_p,x
      sta $07f8,x

      dey
      dey
      dex
      bpl loop
      rts


cls:
      lda #$20
      ldx #$ff
      -
      sta $0400,x
      sta $0500,x
      sta $0600,x
      sta $0700,x
      dex
      bne -
      rts

ccram:
      lda #$01
      ldx #$ff
      -
      sta $d800,x
      sta $d900,x
      sta $da00,x
      sta $db00,x
      dex
      bne -
      rts

;Ball Data
ball_x !by $a0, $00
ball_y !by $74
ball_dx !by $01
ball_dy !by $01

; Player Data
player_x !by $a0, $00
player_y !by $c8


; Sprite data
sprite_mcm: !by $00, $00, $00, $00, $00, $00, $00, $00
sprite_x: !by $24, $74, $00, $20, $40, $60, $80, $a0
sprite_m: !by $00, $00, $00, $00, $00, $00, $00, $00
sprite_y: !by $c8, $74, $ff, $ff, $ff, $ff, $ff, $ff
sprite_c: !by $01, $08, $00, $00, $00, $00, $00, $00
sprite_p: !by $c8, $c0, $00, $00, $00, $00, $00, $00

x !wo 0
y !wo 0
fdx !by 0
fdy !by 0
w  !by 0
h  !by 0
c  !by 0
f1 !by 0
f2 !by 0
f3 !by 0
f4 !by 0


*=$3000

pumpkin_00

!byte $00,$00,$00,$00,$0c,$00,$00,$18,$00,$00,$10,$00,$00,$d6,$00,$01
!byte $ff,$00,$03,$7d,$80,$07,$39,$c0,$06,$38,$c0,$0f,$ef,$e0,$0f,$c7
!byte $e0,$0f,$ff,$e0,$06,$7c,$c0,$07,$39,$c0,$03,$83,$80,$01,$ff,$00
!byte $00,$fe,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08

pumpkin_01

!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$7c,$00,$01,$fe,$e0,$03
!byte $ef,$c0,$03,$c7,$00,$07,$ef,$c0,$04,$ff,$c0,$0e,$c4,$40,$0e,$ec
!byte $c0,$04,$fe,$c0,$06,$7f,$c0,$03,$07,$80,$01,$a7,$80,$00,$ff,$00
!byte $00,$7c,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08

pumpkin_02

!byte $00,$00,$00,$00,$00,$00,$00,$38,$00,$00,$fe,$00,$01,$ff,$00,$03
!byte $bb,$80,$07,$38,$c0,$06,$79,$c0,$06,$ef,$80,$06,$e7,$f0,$06,$ef
!byte $98,$06,$79,$c8,$07,$38,$c0,$03,$bb,$80,$01,$ff,$00,$00,$fe,$00
!byte $00,$38,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08

pumpkin_03

!byte $00,$00,$00,$00,$00,$00,$00,$18,$00,$00,$7e,$00,$00,$db,$80,$01
!byte $83,$c0,$03,$3f,$c0,$06,$7f,$e0,$07,$76,$e0,$06,$64,$60,$06,$76
!byte $e0,$07,$ff,$e0,$03,$e7,$c0,$03,$c7,$80,$01,$f6,$c0,$00,$7e,$c0
!byte $00,$00,$40,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08

pumpkin_04

!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$7f,$00,$00,$ff,$80,$01
!byte $c1,$c0,$03,$9c,$e0,$03,$3e,$60,$07,$ff,$f0,$07,$e3,$f0,$07,$f7
!byte $f0,$03,$1c,$60,$03,$9c,$e0,$01,$be,$c0,$00,$ff,$80,$00,$6b,$00
!byte $00,$08,$00,$00,$18,$00,$00,$30,$00,$00,$00,$00,$00,$00,$00,$08

pumpkin_05

!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3e,$00,$00,$ff,$00,$01
!byte $e5,$80,$01,$e0,$c0,$03,$fe,$60,$03,$7f,$20,$03,$37,$70,$02,$23
!byte $70,$03,$ff,$20,$03,$f7,$e0,$00,$e3,$c0,$03,$f7,$c0,$07,$7f,$80
!byte $00,$3e,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08

pumpkin_06

!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1c,$00,$00,$7f,$00,$00
!byte $ff,$80,$01,$dd,$c0,$03,$1c,$e0,$13,$9e,$60,$19,$f7,$60,$0f,$e7
!byte $60,$01,$f7,$60,$03,$9e,$60,$03,$1c,$e0,$01,$dd,$c0,$00,$ff,$80
!byte $00,$7f,$00,$00,$1c,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08

pumpkin_07

!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$00,$00,$03,$7e,$00,$03
!byte $6f,$80,$01,$e3,$c0,$03,$e7,$c0,$07,$ff,$e0,$07,$6e,$60,$06,$26
!byte $60,$07,$6e,$e0,$07,$fe,$60,$03,$fc,$c0,$03,$c1,$80,$01,$db,$00
!byte $00,$7e,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08

paddle

!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$3f,$ff,$fc,$7f,$ff,$fe,$ff,$ff
!byte $ff,$7f,$ff,$fe,$3f,$ff,$fc,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
