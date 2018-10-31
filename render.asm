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

!to "render.prg", cbm

!source "tiles.asm"

+start_at $c000

map_pointer = $10
map_offset = $12
screen_pointer = $13
screen_offset = $15
cram_pointer = $16
char1       = $18
char2       = $19

* = $c000
     
      ;Setup charset
      lda $d018
      and #%11110001
      ora #%00001000
      sta $d018

      lda $d016
      and #%11101111
      ora #%00010000
      sta $d016

      ;Setup Background Colors
      lda #$00
      sta $d020
      sta $d021

      lda #COLR_CHAR_MC1
      sta $d022

      lda #COLR_CHAR_MC2
      sta $d023

      ;stop interrupts
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

      jsr cls
      jsr draw_init_pointers
      jsr draw_strip
      jsr draw_setup_next_strip 
      jsr draw_strip
      jsr draw_setup_next_strip 
      jsr draw_strip
      jsr draw_setup_next_strip 
      jsr draw_strip

      ;Enable interrupts
      cli

      ;Endless loop
      jmp *

;Main IRQ that runs at 50fps      

main_irq:
      rti

draw_init_pointers:
      lda #<map_data
      sta map_pointer
      lda #>map_data
      sta map_pointer + 1

      lda #$00
      sta screen_pointer
      lda #$04
      sta screen_pointer + 1

      lda #$00
      sta cram_pointer
      lda #$d8
      sta cram_pointer + 1

      rts

draw_setup_next_strip:
      clc      
      lda map_pointer
      adc #$80
      sta map_pointer
      bcc +
      inc map_pointer + 1

+     inc screen_pointer + 1
      inc cram_pointer + 1
      rts

draw_strip:     
      lda #$00
      sta map_offset
      sta screen_offset

-     ldy map_offset
      lda (map_pointer),y
      ldy screen_offset
      jsr draw_tile
      inc map_offset
      inc screen_offset
      inc screen_offset
      bne -

      rts

draw_tile:
      asl ; a *= 2 (2x1 tile)
      tax ; a -> y - y == tile index

      lda tileset_data,x ;a = tile char 1
      sta (screen_pointer),y ;screen[y] = a
      sta char1

      iny
      lda tileset_data+1,x ;a = tile char 1
      sta (screen_pointer),y ;screen[y] = a
      sta char2

      dey

      lda char1
      tax
      lda charset_attrib_data, x
      sta (cram_pointer),y

      iny

      lda char2
      tax
      lda charset_attrib_data, x
      sta (cram_pointer),y

      rts

cls:
      ldx #$00
      lda #$7f
-     sta $0400,x
      sta $0500,x
      sta $0600,x
      sta $0700,x
      dex
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
