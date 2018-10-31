!to "main.prg"

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
!source "sprites.asm"
!source "tiles.asm"

+start_at $0900

* = $0900

    lda #$00
    sta $d020
    lda #$00
    sta $d021
  lda #$08
  sta $d022
  lda #$09
  sta $d023
  	
    jsr cls
    jsr ccram
  jsr top_half

    ;Enable all sprites
    lda #$03
    sta $d015

    ;Set sprite multicolors
    lda #$07
    ldy #$0b
    sta $D025
    sty $D026

    ldx #$07
-   lda sprite_mcm,x
    cmp #$01
    rol $d01c ;C->MCM
    dex
    bpl -
  
  ;Setup tileset
  lda $d018
  and #%11110001
  ora #%00001000
  sta $d018
  
  ;Setup tileset
  lda $d016
  and #%11110111
  ora #%00001000
  sta $d016	

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
    ;inc $d020
    
    ;Main Game Loop
    jsr read_joystick
    jsr update_player
    jsr update_ball
    jsr update_score
    jsr update_sprites
    jsr sprite_collide
    ldx #$04

    ;dec $d020
    
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
    asl dx
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
    beq + ; don't check left bounds if MSB == 2
    
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

+   dec ball_anim_timer
    bne +
    lda #$04
    sta ball_anim_timer
    inc sprite_p + 1
    lda #$c8
    cmp sprite_p +1
    bne +
    lda #$c0
    sta sprite_p + 1
        
+   rts

sprite_collide:
    lda $d01e
    cmp #$03
    bne +
    lda #$ff
    sta ball_dy
    inc bounce_count
+   rts

update_score:
    lda bounce_count
    jsr btoa
    sty $400
    stx $401
    sta $402
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

+     dey
      dey
      dex
      bpl loop
      rts

top_half:

      
      ;y = map index
      ;x = tile index

      ldx  
      ldy #>map_data
      sta $fa
      sty $fb
      ldy #$00
-     lda ($fa),y ;Tile index
      tax
      ;brk
      lda tileset_data,x ;Char
      sta $0400,y
      lda tileset_data+1,x
      sta $0401,y
      
      iny
      bne -
      
      rts
    
cls:
      lda #$20
      ldx #$00
-     sta $0400,x
      sta $0500,x
      sta $0600,x
      sta $0700,x
      dex
      bne -
      rts

ccram:
      lda #$01
      ldx #$00
-     sta $d800,x
      sta $d900,x
      sta $da00,x
      sta $db00,x
      dex
      bne -
      rts

;Ball Data
ball_x !wo $a0, $00
ball_y !by $74
ball_dx !by $01
ball_dy !by $01
ball_anim_timer !by $00
bounce_count !by $00

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
sprite_mp: !by $00, $c7, $00, $00, $00, $00, $00, $00

