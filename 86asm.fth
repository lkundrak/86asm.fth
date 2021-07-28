\ SPDX-License-Identifier: GPL-2.0-or-later

\ 86ASM.FTH -- Intel 8086 Assembler package for FORTH
\ Copyright (C) 2021  Lubomir Rintel <lkundrak@v3.sk>

\ This program is free software; you can redistribute it and/or modify it
\ under the terms of the GNU General Public License as published by the
\ Free Software Foundation; either version 2 of the License, or (at your
\ option) any later version.

\ A copy of the license is available at
\ https://www.gnu.org/licenses/gpl-2.0.txt

base @
hex

: 2c,   DUP 8 RSHIFT c,   FF AND c,   ;
: 2d,   DUP FF AND c,  8 RSHIFT c,    ;

\ Modes
: MEM0   0000 ;
: MEM8   0040 ;
: MEM16  0080 ;
: REG    00C0 ;

: IMM    8000 ;  \ Immediate
: SEG    1000 ;  \ Segment
: W      0400 ;  \ Is long
: S      0200 ;  \ Is short
: D      0100 ;  \ From reg

: DISP,
	\ Disp?
	DUP C7 AND 06 = IF SWAP 2d, THEN
	DUP C0 AND MEM8  = IF SWAP  c, THEN
	DUP C0 AND MEM16 = IF SWAP 2d, THEN

	\ Imm?
	DUP IMM AND IF
		S AND  IF  c,  ELSE  2d,  THEN
	ELSE
		DROP
	THEN
;

: BAD    TRUE ABORT" Bad operand"     ;
: WS?    AND  ABORT" Width mismatch"  ;
: LONG   DUP S WS?  W OR  ;
: SHORT  DUP W WS?  S OR  ;

\ Does this fit onto 8 bits, signed?
: S8? ( n -- flag? )
      FF80 AND
      DUP 0 = IF DROP TRUE EXIT THEN
      FF80 =
;

: $	IMM OVER S8? FALSE = IF LONG THEN ;

: %AX  REG LONG     ;  : %AL  REG SHORT     ;
: %CX  REG LONG 1 + ;  : %CL  REG SHORT 1 + ;
: %DX  REG LONG 2 + ;  : %DL  REG SHORT 2 + ;
: %BX  REG LONG 3 + ;  : %BL  REG SHORT 3 + ;
: %SP  REG LONG 4 + ;  : %AH  REG SHORT 4 + ;
: %BP  REG LONG 5 + ;  : %CH  REG SHORT 5 + ;
: %SI  REG LONG 6 + ;  : %DH  REG SHORT 6 + ;
: %DI  REG LONG 7 + ;  : %BH  REG SHORT 7 + ;

: %ES  SEG          ;
: %CS  SEG 1 3 LSHIFT + ;
: %SS  SEG 2 3 LSHIFT + ;
: %DS  SEG 3 3 LSHIFT + ;

: 2MEM
	DUP S8?  IF       MEM8 EXIT  THEN
	MEM16
;

: MEM
	DUP 0 =  IF  DROP MEM0 EXIT  THEN
	2MEM
;

: +[BX+SI] MEM       ;  : [BX+SI] 0 +[BX+SI] ;
: +[BX+DI] MEM 1 +   ;  : [BX+DI] 0 +[BX+DI] ;
: +[BP+SI] MEM 2 +   ;  : [BP+SI] 0 +[BP+SI] ;
: +[BP+DI] MEM 3 +   ;  : [BP+DI] 0 +[BP+DI] ;
: +[SI]    MEM 4 +   ;  : [SI]    0 +[SI]    ;
: +[DI]    MEM 5 +   ;  : [DI]    0 +[DI]    ;
: +[BP]   2MEM 6 +   ;  : [BP]    0 +[BP]    ;
: PTR     MEM0 6 + ;
: +[BX]    MEM 7 +   ;  : [BX]    0 +[BX]    ;

\ Register
: REGI,
	OVER FFC0 AND REG W OR <> IF DROP FALSE EXIT THEN
	SWAP 07 AND OR c, TRUE
;

\ Segment
: SEGI,
	OVER SEG AND 0= IF DROP FALSE EXIT THEN
	SWAP 18 AND OR c, TRUE
;

\ AX
: AXI,
	OVER FEF8 AND  REG W OR  <> IF  DROP FALSE EXIT  THEN
	SWAP 07 AND OR c,
	TRUE
;

\ Pointer to/from Accumulator
: PTRACCI,
	OVER FF AND 06 <>  IF DROP FALSE EXIT THEN
	OVER D AND      IF  02 OR  THEN
	OVER S AND 0 =  IF  01 OR  THEN
	c, DISP,
	TRUE
;


\ Segment to/from register/memory
: SEGRMI,
	OVER SEG AND 0 = IF DROP FALSE EXIT THEN
	OVER D AND IF 2 OR THEN c, \ d=?
	DUP FF AND c,
	DISP,
	TRUE EXIT
;

\ Immediate to register/memory
: IMMRMI,
	OVER IMM AND IMM <> IF DROP FALSE EXIT THEN
	OVER FF AND OR
	OVER S AND 0 =  IF  0100 OR  THEN
	2c, DISP,
	TRUE
;

\ Immediate to register
: IMMREGI,
	OVER IMM REG OR AND IMM REG OR <> IF DROP FALSE EXIT THEN
	OVER 07 AND OR
	OVER S AND 0 =  IF  08 OR  THEN
	c, DISP,
	TRUE
;

\ Register to/from Register/Memory
: MEMREGI,
	OVER IMM SEG OR AND IF DROP FALSE EXIT THEN
	OVER FF AND OR
	OVER D AND      IF  0200 OR  THEN
	OVER S AND 0 =  IF  0100 OR  THEN
	2c, DISP,
	TRUE
;
: MEMREGI  MEMREGI, IF  EXIT  THEN  BAD  ;

\ Immediate to accumulator
: IMMACCI,
	OVER IMM REG OR AND IMM REG OR <> IF DROP FALSE EXIT THEN
	OVER 07 AND IF DROP FALSE EXIT THEN
	OVER S AND 0 =  IF  01 OR  THEN
	c, DISP,
	TRUE
;

\ Short immediate, indicating 8-bit near jump/call target
: NEARIMM8I,
	OVER S OR  IMM SHORT  <>  IF DROP FALSE EXIT THEN
	>r >r  DUP S8?  IF r> DROP r> c, c, TRUE EXIT THEN
	r> r> DROP FALSE
;
: NEARIMM8I  NEARIMM8I, IF  EXIT  THEN  BAD  ;

\ Long immediate, indicating 16-bit near jump/call target
: NEARIMM16I,
	OVER  IMM LONG  <>  IF DROP FALSE EXIT THEN
	c, DROP 2d, TRUE
;

\ A non-indexed memory pointer
: MEMI,
	OVER IMM SEG OR AND IF DROP FALSE EXIT THEN
	OVER REG AND REG =  IF DROP FALSE EXIT THEN
	OVER W AND  IF  0008 OR  THEN
	OVER FF AND OR
	2c, DISP, TRUE
;

\ Segment with an immediate, indicating far jump target
: FARIMM,
	OVER W OR  SEG LONG  <> IF DROP FALSE EXIT THEN
	SWAP DROP
	>r SWAP r> SWAP
	IMM AND IMM <> ABORT" Expected an immediate"
	c, SWAP 2d, 2d, TRUE
;

\ Return with a SP adjustment of zero or non-zero
: RETI,	OVER 0 =  IF  1 OR c, DROP  ELSE  c, 2d,  THEN  ;

\ Has disp?
: DISP?
	DUP PTR = IF TRUE EXIT THEN
	DUP MEM8 AND 0=  OVER MEM16 AND 0=  <>
;

\ Register/Memory by %CL or $1
: MRCXI
	>r
	DUP IMM SEG OR AND ABORT" Expected Register/Memory"
	DISP?  IF >r SWAP r>  THEN
	DUP S AND 0 =  IF  0100 OR  THEN  1FF AND
	r> OR SWAP
	DUP %CL = IF
		DROP
		0200 OR
		DUP 2c,  IMM INVERT AND DISP,
		EXIT
	THEN
	IMM = IF
		DUP 2c,  IMM INVERT AND DISP,
		1 <> ABORT" Expected a constant of 1"
		EXIT
	THEN
	BAD
;

: 2CHEW
	DUP IMM AND ABORT" Immediate destination" \ , what a nonsense!
	DISP?  IF  >r SWAP r>  THEN \ Put dest disp behind src

	DUP  SEG AND  IF  OVER SEG AND ABORT" Two seg args"  THEN

	DUP FFC7 AND C0 <> IF
		\ Put reg/seg operand second, setting d= flag.
		\ But keep accumulator last.
		OVER SEG AND        IF  SWAP  D OR   THEN
		OVER REG AND REG =  IF  SWAP  D OR   THEN
	THEN

	\ REG or SEG
	OVER IMM AND IMM <>  OVER REG AND REG =  AND  IF
		DUP 07 AND 3 LSHIFT
		SWAP FF00 AND OR
	THEN

	OR D XOR

	\ Both wide and short at the same time?
	DUP W S OR AND W S OR = ABORT" Operands of different size"
;

: PTRCHEW
	2CHEW LONG S OR
	DUP REG AND REG = ABORT" Expected a memory reference"
	DUP D   AND   0 = ABORT" Expected a register destination"
	D INVERT AND
;

: ACCW
	DUP REG AND REG <> ABORT" Expected an accumulator"
	S AND IF 0 ELSE 1 THEN
;

: MOV
	2CHEW
	8C   SEGRMI,  IF  EXIT  THEN
	A0   PTRACCI, IF  EXIT  THEN
	B0   IMMREGI, IF  EXIT  THEN
	C600 IMMRMI,  IF  EXIT  THEN
	8800 MEMREGI
;

: PUSH
	LONG
	06    SEGI,    IF  EXIT  THEN
	50    REGI,    IF  EXIT  THEN
	FF30  MEMREGI
;

: POP
	LONG
	07    SEGI,    IF  EXIT  THEN
	58    REGI,    IF  EXIT  THEN
	8F00  MEMREGI
;

: XCHG
	2CHEW D OR
	90    AXI,     IF  EXIT  THEN
	8600  MEMREGI
;

: NOP	%AX %AX XCHG ;

: IN
	ACCW
	OVER IMM =  IF  E4 OR c,  DROP  c,  EXIT  THEN
	SWAP %DX =  IF  EC OR c,            EXIT  THEN
	BAD
;

: OUT
	DUP IMM =  IF  DROP SWAP ACCW E6 OR c, c,  EXIT  THEN
	%DX =      IF            ACCW EE OR c,     EXIT  THEN
	BAD
;

: XLAT	D7 c,  ;

: LEA	PTRCHEW  8D00  MEMREGI  ;
: LDS	PTRCHEW  C500  MEMREGI  ;
: LES	PTRCHEW  C400  MEMREGI  ;

: LAHF	9F c,  ;
: SAHF	9E c,  ;
: PUSHF	9C c,  ;
: POPF	9D c,  ;

: ADD
	2CHEW
	  04 IMMACCI,  IF  EXIT  THEN
	8000 IMMRMI,   IF  EXIT  THEN
	0000 MEMREGI
;

: ADC
	2CHEW
	  14 IMMACCI,  IF  EXIT  THEN
	8010 IMMRMI,   IF  EXIT  THEN
	1000 MEMREGI
;

: INC
	40    REGI,    IF  EXIT  THEN
	FE00  MEMREGI
;

: AAA	37 c,  ;
: BAA	27 c,  ;	\ DAA???

: SUB
	2CHEW
	  2C IMMACCI,  IF  EXIT  THEN
	8028 IMMRMI,   IF  EXIT  THEN
	2800 MEMREGI
;

: SSB
	2CHEW
	  1C IMMACCI,  IF  EXIT  THEN	\ Datasheet typo
	8018 IMMRMI,   IF  EXIT  THEN
	1800 MEMREGI
;

: DEC
	48    REGI,    IF  EXIT  THEN
	FE08  MEMREGI
;

: NEG	F618  MEMREGI  ;

: CMP
	2CHEW
	  3C IMMACCI,  IF  EXIT  THEN	\ Datasheet typo
	8038 IMMRMI,   IF  EXIT  THEN
	3800 MEMREGI
;

: AAS	3F c,  ;
: DAS	2F c,  ;

: MUL	F620  MEMREGI  ;
: IMUL	F628  MEMREGI  ;

: AAM	D40A 2c,  ;

: DIV	F630  MEMREGI  ;
: IDIV	F638  MEMREGI  ;

: AAD	D50A 2c,  ;

: CBW	98 c,  ;
: CWD	99 c,  ;

: BNOT	F610  MEMREGI  ;	\ NOT mnemonic name has a collision with forthmacs

: SHL	D020  MRCXI  ;  : SAL  SHL  ;
: SHR	D028  MRCXI  ;
: SAR	D038  MRCXI  ;
: ROL	D000  MRCXI  ;
: ROR	D008  MRCXI  ;
: RCL	D010  MRCXI  ;
: RCR	D018  MRCXI  ;

: BAND				\ AND mnemonic name has a collision
	2CHEW
	  24 IMMACCI,  IF  EXIT  THEN
	8020 IMMRMI,   IF  EXIT  THEN
	2000 MEMREGI
;

: TEST
	2CHEW D INVERT AND
	  A8 IMMACCI,  IF  EXIT  THEN
	F600 IMMRMI,   IF  EXIT  THEN
	8400 MEMREGI
;

: BOR				\ OR mnemonic name has a collision
	2CHEW
	  0C IMMACCI,  IF  EXIT  THEN
	8008 IMMRMI,   IF  EXIT  THEN
	0800 MEMREGI
;

: BXOR				\ XOR mnemonic name has a collision
	2CHEW
	  34 IMMACCI,  IF  EXIT  THEN
	8030 IMMRMI,   IF  EXIT  THEN
	3000 MEMREGI
;

: REPNZ: F2 c,  ;	: REPZ: F3 c,  ;

: MOVSB	A4 c,  ;	: MOVSW	A5 c,  ;
: CMPSB	A6 c,  ;	: CMPSW	A7 c,  ;
: SCASB	AE c,  ;	: SCASW	AF c,  ;
: LODSB	AC c,  ;	: LODSW	AD c,  ;
: STOSB	AA c,  ;	: STOSW	AB c,  ;

: CAL
	  E8 NEARIMM16I,  IF  EXIT  THEN
	FF10 MEMI,        IF  EXIT  THEN
	  9A FARIMM,      IF  EXIT  THEN
	BAD
;

: JMP
	00EB NEARIMM8I,   IF  EXIT  THEN
	  E9 NEARIMM16I,  IF  EXIT  THEN
	FF20 MEMI,        IF  EXIT  THEN
	  EA FARIMM,      IF  EXIT  THEN
	BAD
;

: +RET   C2 RETI, ;	: RET	0 +RET   ;
: +RETF  CA RETI, ;	: RETF	0 +RETF  ;

: JE      74 NEARIMM8I   ;  \ JZ collides in GForth
: JL      7C NEARIMM8I   ;  : JNGE   JL      ;
: JLE     7E NEARIMM8I   ;  : JNG    JLE     ;
\ JB collides in GForth
                            : JNAE   72 NEARIMM8I   ;
: JBE     76 NEARIMM8I   ;  : JNA    JBE     ;
: JP      7A NEARIMM8I   ;  : JPE    JP      ;
: JO      70 NEARIMM8I   ;
: JS      78 NEARIMM8I   ;
: JNE     75 NEARIMM8I   ;  : JNZ    JNE     ;
: JNL     7D NEARIMM8I   ;  : JGE    JNL     ;
: JNLE    7F NEARIMM8I   ;  : JG     JNLE    ;
: JNB     73 NEARIMM8I   ;  : JAE    JNB     ;
: JNBE    77 NEARIMM8I   ;  : JA     JNBE    ;
: JNP     7B NEARIMM8I   ;  : JPO    JNP     ;
: JNO     71 NEARIMM8I   ;
: JNS     79 NEARIMM8I   ;
: LOOPI   E2 NEARIMM8I   ;  \ LOOP mnemonic name has a collision
: LOOPZ   E1 NEARIMM8I   ;  : LOOPE  LOOPZ   ;
: LOOPNZ  E0 NEARIMM8I   ;  : LOOPNE LOOPNZ  ;
: JCXZ    E3 NEARIMM8I   ;

: INT
	DUP 3 = IF CC c, DROP EXIT THEN
	CD c, c,
;

: INTO	CE c,  ;
: IRET	CF c,  ;

: CLC	F8 c,  ;
: CMC	F5 c,  ;
: STC	F9 c,  ;
: CLD	FC c,  ;
: STD	FD c,  ;
: CLI	FA c,  ;
: STI	FB c,  ;
: HLT	F4 c,  ;
: WAIT	9B c,  ;

: ESC
	DUP  07 AND 3 LSHIFT
	SWAP 38 AND 5 LSHIFT OR
	D800 OR MEMREGI
;

: LOCK:	F0 c,  ;

: SEG:
	DUP SEG AND SEG <> ABORT" Expected a segment register"
	18 AND  26 OR  c,
;

base !
