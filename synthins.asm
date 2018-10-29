;ldx identity_table,y  tyx
!macro tyx {
  ldy IDENTITY_TABLE,x
}

;and identity_table,x  and X
!macro and_x {
  and IDENTITY_TABLE,x
}

;and identity_table,y  and Y
!macro and_y {
  and IDENTITY_TABLE,y
}

;ora identity_table,x  ora X
!macro ora_x {
  ora IDENTITY_TABLE,x
}

;ora identity_table,y  ora Y
!macro ora_y {
  ora IDENTITY_TABLE,y
}

;eor identity_table,x  eor X
!macro eor_x {
  ora IDENTITY_TABLE,x
}

;eor identity_table,y  eor Y
!macro eor_y {
  eor IDENTITY_TABLE,y
}

;adc identity_table,x  adc X
!macro adc_x {
  adc IDENTITY_TABLE,x
}

;adc identity_table,y  adc Y
!macro adc_y {
  adc IDENTITY_TABLE,y
}

;sbc identity_table,x  sbc X
!macro sbc_x {
  sbc IDENTITY_TABLE,x
}

;sbc identity_table,y  sbc Y
!macro sbc_y {
  sbc IDENTITY_TABLE,y
}

;cmp identity_table,x  cmp X
!macro cmp_x {
  cmp IDENTITY_TABLE,x
}

;cmp identity_table,y  cmp Y
!macro cmp_y {
  cmp IDENTITY_TABLE,y
}

;bit identity_table+constant   bit #constant
!macro bit_hash c {
  bit IDENTITY_TABLE+c
}

;neg A 
!macro neg_a {
  eor $ff
  sec
  adc #0
}

;rsb A = V - A
!macro rsb v {
  eor #$ff
  sec
  adc v
}

;ff_minus_a
!macro ff_minus_a {
  eor #$ff
}

!macro rol_8 {
  asl a
  adv #0
}

!macro ror_8 {
  pha
  lsr a
  pla
  ror a
}

!macro rol_8_v v {
  lda v
  asl a
  rol v
}

!macro ror_8_v v {
  lda v
  lsr a
  ror v
}

!macro inc_16 word {
  inc word
  bne +
  inc word+1
+
}

!macro dec_16 word {
  lda word
  bne +
  dec word+1
+ dec word  
}

!macro sum_16 a, b, r {
  clc
  lda A
  adc B
  sta R
  lda A+1
  adc B+1
  sta R+1
}

!macro sub_16 a, b, r {
  sec
  lda A
  sbc B
  sta R
  lda A+1
  adc B+1
  sta R+1
}

!macro sum_32 a, b, r {
  clc
  lda a
  adc b
  sta r
  lda a+1
  adc b+1
  sta r+1
  lda a+2
  adc b+2
  sta r+2
  lda a+3
  adc b+3
  sta r+3
}

!macro sub_32 a, b, r {
  sec
  lda a
  sbc b
  sta r
  lda a+1
  sbc b+1
  sta r+1
  lda a+2
  sbc b+2
  sta r+2
  lda a+3
  sbc b+3
  sta r+3
}

!macro add_delta u8, s8 {
  clc
  lda s8
  adc u8
  sta u8
}

!macro add_s8_to_u16 u16, s8 {
    ldx #$00
    lda s8
    bpl +
    dex
+   clc
    adc u16
    sta u16
    txa
    adc u16+1
    sta u16+1
}
  
*=$C000
IDENTITY_TABLE
!align 255, 0
!for i, 0, 255 {!by i}

