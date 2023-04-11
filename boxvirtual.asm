!cpu 6502
!to "boxvirtual.prg",cbm
SPRBUF = $31
bgchar =$22
; 10 SYS (2080):REM (c) 2021 mtsv

*=$0801
        !byte    $1E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $38, $30, $29, $3a, $8f, $20, $28, $43, $29, $20, $32, $30, $32, $31, $20, $4D, $54, $53, $56, $00, $00, $00
 
*=$0820
              
          SOUND=$5347
             KEY         =  $6743   
              SEI
                LDA #$20               ; WHEN RESTORE KEY IS HIT
                STA $0318              ; IT POINTS TO THE START
                LDA #$08               ; OF THIS PROGRAM, WHICH IS 
                STA $0319              ; $0820
                LDA #$00                ;BLACK COLOR FOR THE BORDER AND
                STA $D021               ;BACKGROUND
                STA $D020 
                 ; PLAY INTRO MUSIC
                 jsr PLAYSOUND
BEGINNING      
              
            
           
        
              LDA #03                 ; SET LIVES TO 3
                
                STA LIVES
                LDA #$00                ; RESET GAME VARIABLES
                STA byteCOUNT           ; !byte COUNT FOR THE ENEMYHIP SINE TABLES
                STA FIRELOCKUP          ; FIRE BUTTON LOCKUP VARIABLE
                STA NOMOVEBULL 
                STA ENEMYBULLET
                STA ENEMYBULLET2       
                STA MISSILEON
                STA MISSILEXNO
                STA ENEMY2DIR           ; DIRECTION OF THE 2ND ENEMY SHIP
                STA KEY                 ; KEYBOARD SPACE KEY CHECK VARIABLE
                STA SCORE               ; SCORE LOW !byte
                STA SCORE+1             ; SCORE HIGH !byte
                ;LDA #$01
                STA MISSILETIME
                  lda #0
              sta SPRBUF       ;DRAW THE BITMAP TITLE SCREEN
                ;JSR DRAWSCRSCREEN      ;DRAW THE ENTER THE ARENA SCREEN
               ; JSR SCROLLBOTTOMSCR    ;WRITE THE SCROLLING !SCR AT THE BOTTOM OF SCREEN
               
               
                 JSR DRAWSCREEN          ;DRAW THE CHARACTER (BATTLE) SCREEN
                 JSR WRITEHISCORE        ;WRITE THE HI-SCORE TO SCREEN
                JSR INITSPRITEPOS       ;INITIALIZE SPRITE POSITIONS WITH THE TABLE
                JSR SETUPSPRITES        ;INITIALIZE SPRITES
               ;JSR STOPMUSIC           ;STOP THE MUSIC, SINCE THE BATTLE IS ABOUT TO BEGIN

;---------------------MAIN GAME LOOP-----------------------------------------       
MAINLOOP           
                 
                    
                     
                     
                 JSR WAITRASTER        ; WAIT FOR RASTERLINE TO SLOW DOWN
               ; JSR STARANIMATION     ; ANIMATE BACKGROUND STARS
                 JSR EXPANDSPRITEPOS   ; ADJUST SPRITE POSITONS FOR X > 255
             
            
               JSR COLLANIM
                JSR COLORANIM1
                JSR MAINSHIP          ; WAIT FOR JOYSTICK TO COMMAND AND MOVE MAIN SHIP
                JSR BULLETMOVE        ; MAINSHIP BULLET CHECK AND CONTROL
                JSR ENEMYSHIP         ; MOVE ENEMY SHIP ACCORDING TO THE SINE TABLE
                JSR ENEMYFIRE         ; ENEMYSHIP FIRE CHECK AND CONTROL
                JSR ENEMYSHIP2        ; MOVE ENEMYSHIP 2 
                JSR ENEMYFIRE2        ; CONTROL ENEMY SHIP 2 BULLET
                   ; MISSILE CONTROL        
                jsr cls
              ; JSR bgscreen
                JSR COLLISIONCHECK    ; CHECK IF ANY COLLISION OCCURS
                JMP MAINLOOP          ; CONTINUE THE LOOP

;-----------INITIAL POSITIONS OF SPRITES-------------------
INITSPRITEPOS
                LDX #$00               ; LOAD SPRITE INITIAL COORDINATES
POSSPRTS        LDA POSTABLE+$00,X     ; FROM POSITION TABLE
                STA SPRITEPOS+$00,X
                INX
                CPX #$10               ; 8 SPRITES -> X & Y POS -> 16 !byteS     
                BNE POSSPRTS
                RTS

;---------------------SETTING UP SPRITES----------------------------
SETUPSPRITES                           
                LDA #50
              
                STA SPRITEPOS+$03 
                LDA #$0F                  ; SET SPRITE MULTICOLORS        
                STA $D025                 ;--> LT.GREY
                LDA #$02                  ;--> RED
                STA $D026
                LDA #$0E                 ; COLORIZE SPRITES            
                STA $D027                ;-> SPRITE 0 (MAINSHIP) COLOR = LT.BLUE
                LDA #$01                 ;-> SPRITE 1 (ENEMY) COLOR = WHITE      
                STA $D028
                LDA #$01                 ;-> SPRITE 2 (BULLET) COLOR = WHITE
                STA $D029
                LDA #$0B                 ;-> SPRITE 3 (ENEMY BULLET) COLOR = DARK GREY 
                STA $D02A
                LDA #$0B                 ;-> SPRITE 4 (ENEMY2) COLOR = DARK GREY
                STA $D02B
                LDA #$07                 ;-> SPRITE 5 (EMENY 2 BULLET) COLOR = YELLOW
                STA $D02C
                LDA #$0E                  ;-> SPRITE 6 (EXPLOSION) COLOR = MED.GREY 
                STA $D02D
                LDA #$0E                 ;-> SPRITE 7 (MISSILE) COLOR = ORANGE
                STA $D02E
                LDA #%01111111          ; SET MULTICOLOR BITS     
                STA $D01C               ; SPRITES ARE MULTICOLOR
                LDA #$00                ; SET SCREEN-SPRITE PRIORITY FLAGS
                STA $D01B
                ;LDA #$80               ; SET SPRITE POINTERS --> $2000
                STA $07F8               ; MAIN SHIP
               ; LDA #$81               ; ENEMY SHIP
                STA $07F9
                LDA #$80                ; MAIN SHIP BULLET
                STA $07FA
                LDA #$80               ; ENEMY SHIP BULLET
                STA $07FB       
              ;  LDA #$84                ; ENEMY SHIP 2
                STA $07FC        
                LDA #$80                ; ENEMY SHIP 2 BULLET
                STA $07FD        
                LDA #$90                ; EXPLOSION
                STA $07FE        
               ; LDA $07F8               ; MISSILE
                STA $07FF
                LDA #%10111111         ; TURN ON 7 SPRITES - EXPLOSION NOT INCLUDED
                STA $D015
                RTS
cls
 
 lda bgchar
 
sta $0440,x  
sta $0500,x  
sta $0600,x  
sta $0700,x
 
 
 
 
 
 
 ;bne clscol
rts
;-----------------EXPANDING SPRITE POSITIONS FOR X>255 -------------------
EXPANDSPRITEPOS  
                LDX #$00
XLOOP           LDA SPRITEPOS+$01,X     ; Y COORDINATES
                STA $D001,X
                LDA SPRITEPOS+$00,X
                ASL                  ;X POSITION IS MORE
                ROR $D010              ;THAN 256 PIXELS
                STA $D000,X
                INX
                INX
               
                CPX #$10
                BNE XLOOP
                RTS

;-----------------DRAW ENTER THE ARENA SCREEN ROUTINE  ------------------------------------
                       
                 
;-----------------DRAW THE ARENA SCREEN ROUTINE  ------------------------------------
DRAWSCREEN
                 LDA #$00
                 STA $D020 
                 STA $D021 
                 LDA #$20
                 LDX #$FA
CLEANSCREEN      DEX 
                STA $0400,X            ; SCREEN RAM STARTS $0400
                 STA $0500,X 
                  STA $0600,X 
                 STA $0700,X               
                 BNE CLEANSCREEN
                ; LDA #$07
                LDX #$FA
CLEANCOLOR      DEX 
                  STA $D800,X            ; COLOR RAM STARTS $0400
                  STA $D900,X 
                STA $Da00,X 
                  STA $Db00,X 
                 BNE CLEANCOLOR
                 LDX #$00
TLOOP            LDA ARENASCR,X        ; WRITES THE TOPMOST LINE
                 STA $0400,X            ; HI-SCORE, LIVES, SCORE
                 LDA ARENASCRCOLOR,X
                 STA $D800,X
                 INX
                 CPX #40
                 BNE TLOOP
                 RTS

;--------------------SCROLL!SCR------------------------------
SCROLLBOTTOMSCR
                LDA #$00        ; character number counter
                STA $04         ; use zp for storage of a variable
LOOPA3          LDA #$F0        ; wait until rasterline $C8
LOOPA4          CMP $D012 
                BNE LOOPA4
                LDA $02         ; load value of $2 to SCRoll x pos register
                STA $D016       ; using zp unused address for faster access
                LDA #$FF        
LOOPA6          CMP $D012       ; wait until rasterline $FF (256)
                BNE LOOPA6
                LDA #$00        ; reset SCRoll register
                STA $D016 
                LDA $02         ; load zp value
                SEC 
                SBC #$01
                AND #$07         
                STA $02 
                CMP #$07
                BNE LOOPA8  
                LDX #$00        ; replacing characters for row 12
LOOPA9          LDA $07C0,X     ; first character of row 12
                STA $07BF,X     ; last character of row 11
                INX 
                CPX #$27        ; 0-39, one row of characters 
                BNE LOOPA9
                LDX $04 
                LDA SCROLLSCR,X ; load SCRoll text character
                STA $07E6        ; write it to row 12 col 39 
                INC $04                   
                LDA #$50                  
                CMP $04 
                BNE LOOPA8
                LDA #$00
                STA $04 
LOOPA8          JSR $FFE4
                BEQ LOOPA3
                STA KEY
                CMP #$20
                BNE LOOPA3  
                LDA #%00011011           ; $1B -> ENABLE 25 ROWS
                STA $D011
                LDA #%11001000           ;$C8
                STA $D016   
                RTS         

;---------------------TITLE SCREEN---------------------------------------
 

;------------------PLAY TITLE SCREEN MUSIC--------------
PLAYMUSIC
                LDA #$01        ;SELECT IN-GAME TUNE
                STA SOUND
               ; JSR PLAYSOUND   ;GO AND PLAY THE SELECTED SOUND (HERE-MUSIC)
                RTS

;------------------STOP TITLE SCREEN MUSIC WHEN GAME STARTS --------------
STOPMUSIC
                LDA #$04        ;PLAY ONE OF THE SHORT SOUNDS TO STOP MUSIC
                STA SOUND
               ; JSR PLAYSOUND
                RTS

;-----------------SLOWING DOWN THE GAME-------------------------------------
WAITRASTER                  
                
                LDA #$1         ; WAITS FOR RASTERLINE 20
                CMP $D012
             
                BNE WAITRASTER
                RTS
                
;-----------------MAIN SHIP MOVEMENT W/ JOYSTICK CHECK-----------------------
MAINSHIP        inc $D027  
                inc $D025
                   
      
                inc sounddecay
                LDA $DC00                 ;READ JOYSTICK
UP              LSR                     ;CHECK UP
                BCS DOWN                  ;NOT UP? THEN CHECK DOWN
                LDY SPRITEPOS+$01
                DEY                       ;MOVE PLAYER
                DEY                       ;UP, UNTIL IT
                CPY #$00                  ;REACHES #$96
                BCS SETUP                 ;THEN STOP
                LDY #$00                  ;MOVING PLAYER
SETUP           STY SPRITEPOS+$01
DOWN            LSR                     ;CHECK DOWN
                BCS LEFT                  ;NOT DOWN?
                LDY SPRITEPOS+$01;
                INY                       ;MOVE PLAYER
                INY                       ;DOWN UNTIL IT
                CPY #$DE                  ;REACHES #$DE
                BCC SETDOWN               ;THEN STOP
                LDY #$DE                  ;MOVING PLAYER
SETDOWN         STY SPRITEPOS+$01
LEFT            LSR                     ;READ LEFT
                BCS RIGHT                 ;NOT LEFT? THEN CHECK RIGHT
                LDY SPRITEPOS+$00
                DEY                       ;AS WITH UP
                CPY #$1A                  ;AND DOWN, BUT
                BCS SETLEFT               ;MOVING LEFT
                LDY #$1A         
SETLEFT         STY SPRITEPOS+$00
RIGHT           LSR                    ;READ RIGHT
               
                BCS FIRE                 ;NOT RIGHT? CHECK FIRE BUTTON
                LDY SPRITEPOS+$00
                INY                      ;AS UP AND
                CPY #$9E                 ;DOWN BUT
                BCC SETRIGHT             ;MOVING RIGHT
                LDY #$9E
SETRIGHT        STY SPRITEPOS+$00
FIRE            LSR                     ;READ FIRE
                BCS NOJOY                 ;NOT FIRE
                LDA FIRELOCKUP            ;CHECK FIRE LOCK UP
                CMP #$00                  ;IF 1, THEN BUTTON LOCKED UP
                BEQ NOJOY                 
                LDA SPRITEPOS+$00        ;IF NOT LOCKED, PLACE BULLET
                STA SPRITEPOS+$04        ;ON SHIP AND
                LDA SPRITEPOS+$01        ;FIRE BULLET
                STA SPRITEPOS+$05
                JSR MAINSHIPBEEP
                LDA #$01                 ;LOCK FIRE UNTIL BULLET
                STA FIRELOCKUP           ;FINISH ITS FUNCTIONAL PROCESS
NOJOY           RTS

;-----------------MAIN SHIP BULLET HANDLING-------------------------------          
BULLETMOVE      JSR COLORANIM1
                LDA NOMOVEBULL         ; IF NO BULLET IS ON SCREEN
                CMP #$01               ; THEN RETURN
                BEQ NOMOVEBL
                LDY SPRITEPOS+$05      ; MOVE THE BULLET
                DEY                    ; DECREMENT Y VAL 
                DEY
                DEY
                DEY
                dey
                dey
                dey
                dey
                dey
                CPY #$30               ; IF $30 (DEC.48, UPPER SCREEN LIMIT)
                BCS STOPMSB
                LDY #$00               ; THEN ZERO THE COORDINATES
                STY SPRITEPOS+$04      ; TO GET IT OUT OF SCREEN
                LDA #$01
                STA FIRELOCKUP         ; RELEASE THE FIRELOCKUP
STOPMSB         STY SPRITEPOS+$05
NOMOVEBL        RTS

;------------------ENEMY SHIP BULLET --------------------------------------
ENEMYFIRE
                LDA ENEMYBULLET         ; CHECK IF THERE IS ALREADY AN ENEMY BULLET
                CMP #$00
                BNE ENBULMOVE           ; IF BULLET EXISTS, GO TO MOVE SECTION
                LDY SPRITEPOS+$02       ; COPY ENEMY SHIP COORDINATES
                STY SPRITEPOS+$06       ; TO ENEMY BULLET
                LDY SPRITEPOS+$03     
                STY SPRITEPOS+$07  
                LDY #$01                ; SET ENEMYBULLET TO 1, SO THERE IS A BULLET
                STY ENEMYBULLET         ; ON SCREEN
                JSR ENEMYBEEP           ; PLAY THE SHOOTING SOUND
                RTS
ENBULMOVE       LDY SPRITEPOS+$07      ; MOVE THE BULLET
                INY
                iny
                iny
                INY; DECREMENT Y VAL 
                CPY #$E5               ; REACHED DOWN THE SCREEN?
                BCC STOPENMSB          ; IF
                LDY #$00               ; IF YES, MAKE THE COORDINATES ZERO 
                STY SPRITEPOS+$06      ; TO GET IT OUT OF SCREEN
                STY SPRITEPOS+$07 
                LDA #$00               ; SET ENEMYBULLET TO 0, SO THERE IS NO
                STA ENEMYBULLET        ; ENEMY BULLET ON SCREEN
STOPENMSB       STY SPRITEPOS+$07
                RTS
;-------------------------ENEMYSHIP MOVEMENT-------------------------------------
ENEMYSHIP 
                
          
       
               inc byteCOUNT
               ldx byteCOUNT
               lda SINEX0,x
                sta SINx
                LDA SINx           ;SINE MOVEMENT Y POSITION
                STA SPRITEPOS+$02
                inc byteCOUNT          
                RTS
;-------------------------ENEMYSHIP 2 MOVEMENT-------------------------------------
ENEMYSHIP2 
                LDA ENEMY2DIR         ; CHECK ENEMYSHOP 2 DIRECTION      
                CMP #$00              ; 0=DOWN, 1=UP
                BNE GOUP
                LDX SPRITEPOS+$09     ; THIS PART IS FOR GOING DOWN
                INX                   ; INCREMENT Y POSITION
                CPX #$E5              ; IF REACHED $E5 (BOTTOM)
                BEQ CDIRUP            ; THEN GO UP
                JMP FINENEMY2
CDIRUP          INC ENEMY2DIR         ; INCREMENT VARIABLE, SO THE DIRECTION IS UP
                JMP FINENEMY2
GOUP            LDX SPRITEPOS+$09     
                DEX                   ; DECREMENT Y POSITION
                CPX #$ff              ; IF REACHED $90 (UPPER PART OF THE LIMIT)
                BEQ CDIRDOWN          ; THEN GO DOWN
                JMP FINENEMY2
CDIRDOWN        DEC ENEMY2DIR         ; DECREMENT VARIABLE, SO THE DIRECTION IS DOWN
                JMP FINENEMY2          
FINENEMY2       STX SPRITEPOS+$09     ; FINALIZE THE POSITION OF THE ENEMY SHIP 2
                RTS

;------------------ENEMY SHIP 2 BULLET --------------------------------------
ENEMYFIRE2
                LDA ENEMYBULLET2         ; CHECK IF THERE IS ALREADY AN ENEMY BULLET 2
                CMP #$00
                BNE ENBULMOVE2           ; IF BULLET 2 EXISTS, GO TO MOVE SECTION
                LDY #$10                ; COPY ENEMY SHIP 2 COORDINATES
                STY SPRITEPOS+$0A       ; TO ENEMY BULLET 2
                LDY SPRITEPOS+$09     
                STY SPRITEPOS+$0B  
                LDY #$01                ; SET ENEMYBULLET2 TO 1, SO THERE IS A BULLET 2
                STY ENEMYBULLET2        ; ON SCREEN
                JSR ENEMYBEEP           ; PLAY THE SHOOTING SOUND
                RTS
ENBULMOVE2      LDY SPRITEPOS+$0A      ; LOAD THE BULLET 2 X POSITION
                INY    
                iny
                iny
                iny; INCREMENT X POSITION 
                CPY #156               ; REACHED RIGHT EDGE OF THE SCREEN?
                BCC STOPENMSB2          
                LDY #$00               ; IF YES, MAKE THE COORDINATES ZERO 
                STY SPRITEPOS+$0A      ; TO GET IT OUT OF SCREEN
                STY SPRITEPOS+$0B 
                LDA #$00               ; SET ENEMYBULLET2 TO 0, SO THERE IS NO
                STA ENEMYBULLET2       ; ENEMY BULLET 2 ON SCREEN
STOPENMSB2      STY SPRITEPOS+$0A      ; WRITE THE FINAL X POSITION TO VARIABLE
                RTS

;------------------------MISSILE---------------------------------
MISSILE
                LDA MISSILETIME         ; CHECK MISSILETIME
                CMP #00
                BEQ FINMISSILE          ; IF ZERO, NO MISSILE, FINALIZE
                LDA MISSILEON           ; IS THERE A MISSILE ON SCREEN?
                CMP #01
                BEQ MOVEMISSILE

                LDA #01
                STA MISSILEON           ; NOW THERE IS A MISSILE ON SCREEN ACTIVATE MISSILE SPRITE
                JSR MISSILEBEEP         ; LAUNCH SOUND OF THE MISSILE
                LDX MISSILEXNO          ; WE ARE PUTTING A NEW MISSILE ON SCREEN
                LDA MISSILEX,X          ; THE NEXT ROW IS TRANSFERED TO X POSITION
                STA SPRITEPOS+$0E                
                LDA #50                 ; STARTING MISSILE Y POS IS READ                 
                STA SPRITEPOS+$0F
                RTS
MOVEMISSILE     LDY SPRITEPOS+$0F       ; MOVING THE MISSILE DOWN
                INY        
                STY SPRITEPOS+$0F
                CPY #$FE                ; DID IT REACH AT THE END OF THE SCREEN?
                BCC FINMISSILE          ; ıF NO, RTS AND CONTINUE
                LDA #50                 ; IF YES, MOVE THE SPRITE TO TOP
                STA SPRITEPOS+$0F
                INC MISSILEXNO          ; INCREMENT THE X POS TABLE 
                LDA MISSILEXNO          ; IS IT AT THE END OF THE TABLE?
                CMP #6
                BCC LOADMISSX           ; IF NO, CONTINUE
                LDA #00                 ; IF YES, RESET THE TABLE POSITION
                STA MISSILEXNO
LOADMISSX       LDX MISSILEXNO          ; LOAD THE NEXT X POSITION OF THE MISSILE
                LDA MISSILEX,X          ; FROM THE TABLE
                STA SPRITEPOS+$0E     
                JSR MISSILEBEEP         ; LAUNCH SOUND OF THE MISSILE 
FINMISSILE      RTS

;--------------COLLIDED 3 - BULLETS COLLIDE----------------
COLLIDED3
                LDA #$00
                STA SPRITEPOS+$04     ; MAKE THE BULLET X POSITIONS ZERO
                STA SPRITEPOS+$06      
                STA $D01E
                STA NOMOVEBULL        ; AND NO MORE BULLETS ON SCREEN
                STA ENEMYBULLET
                RTS

;--------------COLLIDED 4 - BULLETS COLLIDE----------------
COLLIDED4
                LDA #$00                ; MAKE THE SPRITE Y COORDINATES ZERO
                STA SPRITEPOS+$04       ; TO TAKE THEM OUT OF THE SCREEN
                STA SPRITEPOS+$0A      
                STA $D01E               ; RESET THE COLLISION REGISTER
                STA NOMOVEBULL          ; NO MAIN SHEEP BULLET ON SCREEN
                STA ENEMYBULLET2        ; NO ENEMY 2 BULLET ON SCREEN
                RTS  

;--------------COLLIDED 5 - MISSILE AND MAINSHIP BULLET COLLIDE----------------
COLLIDED5
                LDA #$00                 ; MAKE THE SPRITE Y COORDINATES ZERO
                STA SPRITEPOS+$04        ; TO TAKE THEM OUT OF THE SCREEN
                STA SPRITEPOS+$0E      
                STA SPRITEPOS+$0F                  
                STA $D01E               ; RESET THE COLLISION REGISTER
                STA NOMOVEBULL          ; NO MAIN SHEEP BULLET ON SCREEN
                STA MISSILEON           ; NO MISSILE ON SCREEN
CONTINUE        RTS  

;----------------COLLISON CHECK SUBROUTINE------------------------------------
COLLISIONCHECK
                LDA $D01E
                CMP #%00000110        ; CHECK IF MAIN SHIP BULLET AND ENEMY SHIP COLLIDED
                BEQ COLLIDED
                CMP #%00001001        ; CHECK IF ENEMY SHIP BULLET AND MAIN SHIP COLLIDED
                BEQ COLLIDED2
                CMP #%00001100        ; CHECK IF MAIN SHIP BULLET AND ENEMY SHIP BULLET COLLIDED
                BEQ COLLIDED3
                CMP #%00100001        ; CHECK IF ENEMY SHIP 2 BULLET AND MAIN SHIP COLLIDED
                BEQ COLLIDED2
                CMP #%00100100        ; CHECK IF ENEMY SHIP 2 BULLET AND MAIN SHIP BULLET COLLIDED
                BEQ COLLIDED4
                CMP #%10000001        ; CHECK MAIN SHIP AND MISSILE COLLIDED
                BEQ COLLIDED2
                CMP #%10000100        ; CHECK IF MISSILE AND MAIN SHIP BULLET COLLIDED
                BEQ COLLIDED5
                RTS

;----------------MAIN SHIP BULLET AND ENEMY SHIP COLLIDED--------------------------
COLLIDED
                LDA #%01010001        ; JUST SPRITE 0 (MAINSHIP) AND 
                STA $D015             ; SPRITE 6 (COLLISION SPRITE) IS ON
                LDA SPRITEPOS+$02     ; TRANSFER CURRENT ENEMY SHIP POSITION 
                STA SPRITEPOS+$0C     ; TO COLLISION SPRITE COORDINATES
                LDA SPRITEPOS+$03     
                STA SPRITEPOS+$0D
                LDA #7
                STA SOUND
                ;JSR PLAYSOUND         ; PLAY COLLOSION SOUND
                inc $D026
                JSR Collidedsnd
                inc SPRBUF 
                JSR Collidedsnd
                JSR WRITESCORE        ; GOTO WRITE SCORE SUBROUTINE
                JSR CHECKEXTRALIFE    ; CHECK SCORE FOR EXTRALIFE
                JSR COLLANIM          ; COLLISION ANIMATION
                JSR ENEMYHITCLEAR     ; CLEAR THE BOTTOM !SCR
                LDA #%10111111        ; RETURN BACK TO ORIGINAL POSITIONS EXCEPT MAIN SHIP
                inc bgchar
                STA $D015             ; 7 SPRITES ARE ON (AGAIN)        
                LDA #$00              ; RESET COLLISION REGISTER
                STA $D01E      
                STA NOMOVEBULL        ; RESET MAINSHIP BULLET REGISTER
                STA ENEMYBULLET
                INC SPRITEPOS+$03 
                 INC SPRITEPOS+$03 
                  INC SPRITEPOS+$03 
                   INC SPRITEPOS+$03 
                    INC SPRITEPOS+$03
           
                LDA SPRITEPOS+$03
                CMP #180
                BEQ RSTENEMYSPR
             ; LDA #$00
               ; STA byteCOUNT
              ;  STA MISSILEON
               ; STA SPRITEPOS+$04     ; RESET SPRITE POSITIONS
               ; STA SPRITEPOS+$05
               ; STA SPRITEPOS+$06
              ;;  STA SPRITEPOS+$07
              ;  STA SPRITEPOS+$0A
              ;  STA SPRITEPOS+$0B  
               ; LDA ENSHIPDIR         ; REVERSE ENEMYSHIP DIRECTION
              ;  EOR #$01
              ;  STA ENSHIPDIR
                JMP MAINLOOP          ; RETURN BACK TO THE MAINLOOP
RSTENEMYSPR  LDA #50
         STA SPRITEPOS+$03
         rts
;-------------------ENEMY SHIP BULLET AND MAINSHIP COLLIDED------------------------
COLLIDED2
                LDA #%01010010        ; JUST SPRITE 1 (ENEMY SHIP), SPRITE 4 (ENEMY SHIP 2 ) AND
                STA $D015             ; SPRITE 6 (COLLISION SPRITE) IS ON
                JSR EXPANDSPRITEPOS
                LDA SPRITEPOS+$00     ; TRANSFER CURRENT MAIN SHIP POS TO COLLISIN COORDINATES
                STA SPRITEPOS+$0C
                LDA SPRITEPOS+$01
                STA SPRITEPOS+$0D
                LDA #7
                STA SOUND
                 JSR MISSILEBEEP
                JSR YOUAREHIT
                ;JSR COLLANIM          ; ANIMATE COLLISION
                JSR YOUAREHITCLEAR    ; 
                DEC LIVES             ; DECREMENT LIVES VALUE
                JSR UPDATELIVES       ; UPDATE LIVES ON SCREEN
                LDA LIVES             ; CHECK NO. OF LIVES
                BEQ GAMEOVER          ; IF ZERO, THEN JUMP TO END GAME ROUTINE
                LDA #%10111111        ; 7 SPRITES ARE ON (AGAIN)  
                STA $D015           
                LDA #$00              ; RESET COLLISION REGISTER
                STA $D01E      
                STA ENEMYBULLET       ; RESET ENEMY SHIP BULLET REGISTER
                LDA #$00
                STA MISSILEON
                STA SPRITEPOS+$04     ; RESET SPRITE POSITIONS
                STA SPRITEPOS+$05
                STA SPRITEPOS+$06
                STA SPRITEPOS+$07          
                STA SPRITEPOS+$0A
                STA SPRITEPOS+$0B               
                JMP MAINLOOP          ; RETURN BACK TO THE MAINLOOP

;-------------------END GAME------------------------------
GAMEOVER
                LDX #0                  ; GAME OVER !SCR
GO              LDA ENDSCR1,X
                STA SCREEN3,X
                INX
                CPX #9
                BNE GO
                LDX #0                  ; COLOR OF GAME OVER !SCR
GO2             LDA #1
                STA SCREEN3COL,X
                INX
                CPX #9
                BNE GO2
                LDX #0                  ; HIT SPACE TO CONTINUE !SCR
GO3             LDA ENDSCR2,X
                STA SCREEN4,X
                INX
                CPX #21
                BNE GO3
                LDX #0                  ; COLOR OF HIT SPACE TO CONTINUE !SCR
GO4             LDA #1
                STA SCREEN4COL,X
                INX
                CPX #25
                BNE GO4
                LDA #%00000000        ; TURN OFF SPRITES  
                STA $D015 
                LDA #$02
               ; STA SOUND       
               ;  JSR PLAYSOUND           ; PLAY ENDING MUSIC
                JSR CHECKHISCORE        ; CHECK IF THERE IS A NEW HIGH SCORE
                JSR WRITEHISCORE        ; PRINT THE HIGH SCORE TO SCREEN
INPUT           JSR COLORANIM1          ; !SCR COLOR ANIMATIN OF THE 1ST LINE
                JSR COLORANIM2          ; !SCR COLOR ANIMATIN OF THE 2ND LINE
                LDA #$ff         ; WAITS FOR RASTERLINE 20
                CMP $D012
                
                BNE GAMEOVER
                        ; WAIT FOR KEYBOARD INPUT
                ;BEQ INPUT
                ;STA KEY
               ; CMP #$20                ; IS SPACE HIT?
                BNE INPUT       
                JMP BEGINNING
 
;----------- MAIN SHIP HIT MESSAGE-------------------------
YOUAREHIT
                LDX #00                 ; YOU ARE HIT MESSAGE
MSH             LDA MAINSHIPHIT,X       ; ON THE BOTTOM OF THE SCREEN
                STA SCREEN5,X
                INX
                CPX #40
                BNE MSH          
                LDX #0
MSHC            LDA #1
                STA SCREEN5COL,X
                INX
                CPX #40
                BNE MSHC
                RTS
YOUAREHITCLEAR  LDX #00                ; AFTER KEYBOARD RESPONSE, CLEAR THE LINE             
MSH2            LDA #32
                STA SCREEN5,X
                INX
                CPX #36
                BNE MSH2          
                RTS
 ;-----------ENEMY HIT MESSAGE---------------------------------------------
ENEMYHIT
                LDX #00                 ; ENEMY IS HIT MESSAGE
ESH             LDA ENEMYSHIPHIT,X      ; ON THE BOTTOM OF THE SCREEN
                STA SCREEN6,X
                INX
                CPX #40
                BNE ESH          
                LDX #0
ESHC            LDA #1
                STA SCREEN6COL,X
                INX
                CPX #40
                BNE ESHC
                RTS
ENEMYHITCLEAR   LDX #00                  ; AFTER KEYBOARD RESPONSE, CLEAR THE LINE
ESH2            LDA #32
                STA SCREEN6,X
                INX
                CPX #36
                BNE ESH2          
                RTS            
;--------------------CHECK SCORE FOR EXTRA LIFE-----------------------
CHECKEXTRALIFE 
                LDA SCORE
                CMP #00
                BNE ENEMYHIT             ; WRITE "ENEMY HIT" !SCR IF NO EXTRA LIFE
                LDA SCORE+1
                CMP #01
                BEQ ENEMYHIT            ; FOR 100,  NO EXTRA LIFE        
                CMP #02
                BEQ ADDEXTRALIFE        ; FOR 200,  ADD AN EXTRA LIFE
                CMP #03
                BEQ ENEMYHIT            ; FOR 300,  NO EXTRA LIFE      
                CMP #04
                BEQ ADDEXTRALIFE        ; FOR 400,  ADD AN EXTRA LIFE
                CMP #05
                BEQ ENEMYHIT            ; FOR 500,  NO EXTRA LIFE    
                CMP #06
                BEQ ADDEXTRALIFE        ; FOR 600,  ADD AN EXTRA LIFE
                JMP FINALEXTRALIFE
ADDEXTRALIFE    LDA LIVES
                CLC
                ADC #1
                STA LIVES
                JSR UPDATELIVES
                LDX #00                  ; EXTRALIFE MESSAGE
EXT             LDA EXTRALIFESCR,X      ; ON THE BOTTOM OF THE SCREEN
                STA SCREEN6,X
                INX
                CPX #40
                BNE EXT          
                LDX #0
EXTC            LDA #1
                STA SCREEN6COL,X
                INX
                CPX #40
                BNE EXTC
FINALEXTRALIFE  RTS

;----------------------COLLISION ANIMATIN-------------------------------
COLLANIM
         ldx SPRBUF
           
store              
                ;inx
EXPLPLR          lda sprpoint,x
                          
                  sta $07F9
                 sta $07F8; 
                         
               
                 
  
                 sta $07FC        
               
                  sta $07FE  
              inc SPRBUF
                  
                 
          
ldyagain               
              
                RTS

;-------------COLOR ANIMATION FOR GAME OVER------------------------
COLORANIM1
                LDX #$00            ; CYCLE THE COLOR OF EACH LETTER
LUP1            LDA SCREEN1COL,X
                TAY
                INY
                TYA
                STA SCREEN1COL,X
              STA SCREEN2COL,X
              STA SCREEN3COL,X
              STA SCREEN4COL,X
                INX
                cpx #80
                BNE LUP1        
                RTS

;-------------COLOR ANIMATION FOR HIT SPACE------------------------
COLORANIM2
                LDX #$00            ; CYCLE THE COLOR OF EACH LETTER
LUP2            LDA SCREEN2COL,X
                TAY
                INY
                TYA
                STA SCREEN3COL,X
                INX
              cpx #100
                BNE LUP2        
                RTS
 
;----------------------CHECK HI-SCORE------------------------------
CHECKHISCORE
                LDA HISCORE+1    ; LOAD THE HIGH !byte OF THE HIGH SCORE
                CMP SCORE+1      ; COMPARE IT TO THE RECENT SCORE   
                BCC YES          ; IF HISCOREHI < SCOREHI, GO AND FORM THE NEW HIGH SCORE
                LDA SCORE+1      ; NOW A CROSSCHECK. IF SCOREHI < HISCHOREHI
                CMP HISCORE+1    ; THEN NO CHANGE
                BCC NO
                LDA HISCORE      ; SO IF WE ARE HERE, HIGH !byte OF SCORE IS NOT GREATER THAN HIGH SCORE   
                CMP SCORE        ; BUT STILL, THE LOW !byte MIGHT BE HIGH
                BCC YES          ; IF HISCORE < SCORE, THEN STILL IT IS A HIGH SCORE
                RTS              ; OTHERWISE IT IS NOT A HIGH SCORE, JUST RETURN
YES             LDA SCORE        ; LOAD THE NEW SCORE VALUES TO HIGHSCORE 
                STA HISCORE    
                LDA SCORE+1
                STA HISCORE+1
NO              RTS    

;--------------PLOT HI-SCORE-----------------------------
WRITEHISCORE
                LDY #3           ; SCREEN OFFSET
                LDX #0           ; SCORE !byte INDEX
HILOOP          LDA HISCORE,X
                PHA
                AND #$0F        ; 00001111
                JSR PLOTHI 
                PLA
                LSR
                LSR
                LSR 
                LSR
                JSR PLOTHI
                INX
                CPX #2
                BNE HILOOP
                RTS
PLOTHI          CLC
                ADC #48       
                STA HISCREEN,Y 
                DEY
                RTS

;---------------------SCORE HANDLING---------------------------------
WRITESCORE                      ; WE HAVE 4 DIGITS SCORE
                SED            ; WE ARE SETTING DECIMAL FLAG
                CLC
                LDA SCORE
                ADC #10
                STA SCORE
                LDA SCORE+1
                ADC #$00
                STA SCORE+1
                CLD
                LDY #3           ; SCREEN OFFSET
                LDX #0           ; SCORE !byte INDEX
SLOOP           LDA SCORE,X
                PHA
                AND #$0F         ; 00001111
                JSR PLOTDIGIT 
                PLA
                LSR
                LSR
                LSR 
                LSR
                JSR PLOTDIGIT
                INX
                CPX #2
                BNE SLOOP
                RTS
PLOTDIGIT       CLC
                ADC #48       
                STA SCREEN,Y 
                DEY
                LDA SCORE+1     ; CHECK IF SCORE IS >200
                CMP #02
                BCC FINSCORE 
                LDA #$01        ; YES, NOW INCLUDE THE MISSILE
                STA MISSILETIME
FINSCORE        RTS

;-------------LIVES CHECK-------------------------------------
UPDATELIVES
                LDA LIVES  
                CLC
                ADC #48         ; ADD 48 TO GET THE SCREEN CODES OF THE NUMBERS
                STA SCREEN2     ; PRINT IT AT THE DESIGNATED CELL
                RTS

;----------------------SOUND EFFECTS----------------------------
MAINSHIPBEEP    JSR SOUNDEND1
                LDA #53
                STA ATTDEC
                LDA #0
                STA SUSREL
                LDA #10
                STA VOLUME        
                LDA #143
                STA HIFREQ
                LDA #80
                STA LOFREQ
                LDA #32
                STA WAVEFM
                JSR SOUNDGO1
                RTS
Collidedsnd     JSR SOUNDEND1
                LDA #12
                STA ATTDEC
                LDA #100
                STA SUSREL
                LDA #12
                STA VOLUME        
                LDA sounddecay
                STA HIFREQ
                LDA sounddecay
                STA LOFREQ
                LDA 129
                STA WAVEFM
                JSR SOUNDGO1
                RTS
                

                
                
                
                
ENEMYBEEP       JSR SOUNDEND2
                LDA #53
                STA ATTDEC
                LDA #0
                STA SUSREL
                LDA #12
                STA VOLUME        
                LDA SPRITEPOS+$07
                STA HIFREQ
                LDA SPRITEPOS+$07
                STA LOFREQ
                LDA #32
                STA WAVEFM
                JSR SOUNDGO2
                RTS
MISSILEBEEP     JSR SOUNDEND3
                LDA #91
                STA ATTDEC
                LDA #15
                STA SUSREL
                LDA #12
                STA VOLUME        
                LDA #143
                STA HIFREQ
                LDA #10
                STA LOFREQ
                LDA #129
                STA WAVEFM
                JSR SOUNDGO3
                RTS
SOUNDGO1        LDA ATTDEC
                STA $D405
                LDA SUSREL
                STA $D406       
                LDA VOLUME
                STA $D418
                LDA HIFREQ
                STA $D400
                LDA LOFREQ
                STA $D401
                LDX WAVEFM
                INX
                TXA
                STA $D404
                RTS
SOUNDGO2        LDA ATTDEC
                STA $D40C
                LDA SUSREL
                STA $D40D       
                LDA VOLUME
                STA $D418
                LDA HIFREQ
                STA $D407
                LDA LOFREQ
                STA $D408
                LDX WAVEFM
                INX
                TXA
                STA $D40B
                RTS
SOUNDGO3        LDA ATTDEC
                STA $D413
                LDA SUSREL
                STA $D414       
                LDA VOLUME
                STA $D418
                LDA HIFREQ
                STA $D40E
                LDA LOFREQ
                STA $D40F
                LDA WAVEFM
                STA $D412
                RTS
SOUNDEND1       LDA #0
                STA $D404       ; WF1
                RTS
SOUNDEND2       LDA #0
                STA $D40B       ; WF2
                RTS
SOUNDEND3       LDA #0
                STA $D412       ; WF3
                RTS

;--------------------------PLAY SOUND -------------------
PLAYSOUND
               
       lda #$00
	 
	jsr $7000

 
	; disable interrupts
 	sei
	lda #$7f
	sta $dc0d
	sta $dd0d
	lda #$01
	sta $d01a
 
	; init irq
 	lda #<irq
	ldx #>irq
	sta $0314
	stx $0315

	; create rater interrupt at line 0
	

	; clear interrupts and ACK irq
	lda $dc0d
	lda $dd0d
	
	cli
	rts
irq    inc $d019

           jsr $7003
      
        lda $dc00
         cmp #16                   ;READ FIRE
                BEQ BEGINNINGh    
                JMP $EA81

BEGINNINGh 
jsr BEGINNING
rts
            ; SOUND -> WAVEFORM VARIABLE
               ; SOUND SELECTION !byte
;MUSIC_IN_GAME_TUNE               = $00
;MUSIC_TITLE_TUNE                 = $01
;MUSIC_GET_READY_GAME_OVER_TUNE   = $02
;MUSIC_GAME_END_TUNE              = $03
;MUSIC_PLAYER_SHOOT               = $04
;MUSIC_PLAYER_DIE                 = $05
;MUSIC_PICKUP                     = $06
;MUSIC_ENEMY_DIE                  = $07          



 ;*=$6000                          ; BITMAP KOALA IMAGE
 ;!bin "pixel_space.PRG",2

;---------------STARFILED VARIABLES AND TABLES -------------------------------------
STARSSPEEDCOLARRAY      !byte  4,2,4,3,4,3,4,3,4,3
                        !byte  1,2,4,2,4,2,3,4,2,3
                        !byte  2,3,4,3,4,1,4,3,1,3
                        !byte  4,3,4,1,4,2,4,2,3,2

STARSYCHARARRAY         !byte 15,6,17,1,18,2,4,14,12,5
                        !byte 13,3,9,7,10,21,5,13,10,23
                        !byte 11,5,15,1,5,9,7,18,11,2
                        !byte 12,16,21,9,2,5,16,8,15,2

SCREENRAMROWSTARTLOW    !byte $00,$28,$50,$78,$A0,$C8,$F0,$18
                        !byte $40,$68,$90,$B8,$E0,$08,$30,$58      
                        !byte $80,$A8,$D0,$F8,$20,$48,$70,$98,$C0

SCREENRAMROWSTARTHIGH   !byte $04,$04,$04,$04,$04,$04,$04,$05
                        !byte $05,$05,$05,$05,$05,$06,$06,$06
                        !byte $06,$06,$06,$06,$07,$07,$07,$07,$07      

COLORRAMROWSTARTLOW     !byte $00,$28,$50,$78,$A0,$C8,$F0,$18
                        !byte $40,$68,$90,$B8,$E0,$08,$30,$58
                        !byte $80,$A8,$D0,$F8,$20,$48,$70,$98,$C0

COLORRAMROWSTARTHIGH    !byte $D8,$D8,$D8,$D8,$D8,$D8,$D8,$D9
                        !byte $D9,$D9,$D9,$D9,$D9,$DA,$DA,$DA
                        !byte $DA,$DA,$DA,$DA,$DB,$DB,$DB,$DB,$DB



;--------------------!SCR ON SCREENS-------------------------------
ENDSCR1        !SCR "game over"
ENDSCR2        !SCR "wait...                                  "

MAINSHIPHIT     !SCR "    you are hit - be careful please     "
ENEMYSHIPHIT    !SCR "   you hit the enemy ship, good work    "
EXTRALIFESCR   !SCR "  bravo! you've earned an extra life!   "

SCROLLSCR      !SCR "Ultratouch :"   
                !SCR "k1mhr 2022    hit space to continue..."

ARENASCR       !SCR " hi-score:0000   lives:3   score:0000   "
ARENASCRCOLOR  !byte 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
                !byte 15,1,1,1,1,1,1,1,1,1,1,13,13,13,13,13,13,13,13
                !byte 13,13,13,13,13
;-----------------------------------------------------------------
SPRITEPOS   !byte 0,0,0,0,0,0
EXPLTBL  !byte $86,$87,$88,$89,$8A,$8B            ; EXPLOSION TABLE THAT STORES
         !byte $8C,$8D,$8E,$8F,$90,$91            ; THE POINTER TO THE EXPLOSION SPRITE

; SPRITE INITIAL POSITIONS
POSTABLE !byte $56,$D0,$55,$3C,$00,$00,$00,$00,$0e,$96,$00,$00,$00,$00,$00,$00
ANIMCOUNT !byte 1,2,3,4,5,6,7,8
sprpoint !byte      $83,$84,$85,$86,$87,$88,$89 

 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
  !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
  !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 !byte      $83,$84,$85,$86,$87,$88,$89 
 
 SINx=$4427
SINy=$4141
;ENEMYSHIP MOVEMENT COORDINATES (SINE TABLES)
SINEX0  ; X POSITION 1              
    
 
        
        !byte 26,26,26,26,26,26,26,27,27,27,28,28,28,29,29,30
        !byte 31,31,32,33,33,34,35,36,37,38,39,40,41,42,43,44
        !byte 45,46,47,49,50,51,52,54,55,56,58,59,60,62,63,65
        !byte 66,68,69,71,72,74,75,77,79,80,82,83,85,86,88,90
        !byte 92,93,95,97,98,100,101,103,105,106,108,109,111,112,114,115
        !byte 117,118,120,121,123,124,125,127,128,129,131,132,133,135,136,137
        !byte 138,139,140,141,142,143,144,145,146,147,148,149,149,150,151,151
        !byte 152,153,153,154,154,154,155,155,155,156,156,156,156,156,156,156
        !byte 155,155,155,155,155,155,155,154,154,154,153,153,152,152,151,151
        !byte 150,149,149,148,147,146,146,145,144,143,142,141,140,139,138,137
        !byte 135,134,133,132,131,129,128,127,125,124,123,121,120,118,117,115
        !byte 114,112,111,109,108,106,105,103,102,100,98,97,95,94,92,91
        !byte 91,89,87,86,84,83,81,79,78,76,75,73,72,70,69,67
        !byte 66,64,63,61,60,58,57,56,54,53,52,50,49,48,47,46
        !byte 44,43,42,41,40,39,38,37,36,35,35,34,33,32,32,31
        !byte 30,30,29,29,28,28,27,27,27,26,26,26,26,26,26,26
 
SINEX1 ; X POSITION 2     
        
          
        !byte 26,26,26,26,26,26,26,27,27,27,28,28,28,29,29,30
        !byte 31,31,32,33,33,34,35,36,37,38,39,40,41,42,43,44
        !byte 45,46,47,49,50,51,52,54,55,56,58,59,60,62,63,65
        !byte 66,68,69,71,72,74,75,77,79,80,82,83,85,86,88,90
        !byte 92,93,95,97,98,100,101,103,105,106,108,109,111,112,114,115
        !byte 117,118,120,121,123,124,125,127,128,129,131,132,133,135,136,137
        !byte 138,139,140,141,142,143,144,145,146,147,148,149,149,150,151,151
        !byte 152,153,153,154,154,154,155,155,155,156,156,156,156,156,156,156
        !byte 155,155,155,155,155,155,155,154,154,154,153,153,152,152,151,151
        !byte 150,149,149,148,147,146,146,145,144,143,142,141,140,139,138,137
        !byte 135,134,133,132,131,129,128,127,125,124,123,121,120,118,117,115
        !byte 114,112,111,109,108,106,105,103,102,100,98,97,95,94,92,91
        !byte 91,89,87,86,84,83,81,79,78,76,75,73,72,70,69,67
        !byte 66,64,63,61,60,58,57,56,54,53,52,50,49,48,47,46
        !byte 44,43,42,41,40,39,38,37,36,35,35,34,33,32,32,31
        !byte 30,30,29,29,28,28,27,27,27,26,26,26,26,26,26,26
     
      
       

SINEY   ; Y POSITION (COMMON)
        
        !byte 114,114,114,114,114,114,114,114,114,114,114,113,113,113,113,113
        !byte 112,112,112,111,111,111,110,110,110,109,109,108,108,108,107,107
        !byte 106,106,105,105,104,104,103,102,102,101,101,100,100,99,98,98
        !byte 97,96,96,95,95,94,93,93,92,91,91,90,89,89,88,87
        !byte 86,86,85,84,84,83,82,82,81,80,80,79,79,78,77,77
        !byte 76,75,75,74,74,73,72,72,71,71,70,70,69,69,68,68
        !byte 67,67,66,66,65,65,65,64,64,63,63,63,62,62,62,62
        !byte 61,61,61,61,61,60,60,60,60,60,60,60,60,60,60,60
        !byte 60,60,60,60,60,60,60,60,60,60,60,61,61,61,61,61
        !byte 62,62,62,62,63,63,63,64,64,65,65,65,66,66,67,67
        !byte 68,68,69,69,70,70,71,71,72,72,73,74,74,75,75,76
        !byte 77,77,78,79,79,80,80,81,82,82,83,84,84,85,86,86
        !byte 87,88,89,89,90,91,91,92,93,93,94,95,95,96,96,97
        !byte 98,98,99,100,100,101,101,102,102,103,104,104,105,105,106,106
        !byte 107,107,108,108,108,109,109,110,110,110,111,111,111,112,112,112
        !byte 113,113,113,113,113,114,114,114,114,114,114,114,114,114,114,114
 
 

; SPRITE #2 --> MAIN SHIP BULLET --> POINTER $89
       
 *=8192      
       
       !byte %0000000,%0000000,%0000000 
       !byte %0000000,%0000000,%0000000 
        !byte %0000000,%0000000,%0000000 
         !byte %0000000,%0000000,%0000000 
          !byte %0000000,%0000000,%0000000 
           !byte %0000000,%0000000,%0000000 
            !byte %0000000,%0000000,%0000000 
             !byte %0000000,%0000000,%0000000 
              !byte %0000000,%0011100,%0000000 
               !byte %0000000,%0011100,%0000000 
                !byte %0000000,%0011100,%0000000 
                 !byte %0000000,%0000000,%0000000 
                  !byte %0000000,%0000000,%0000000 
                   !byte %0000000,%0000000,%0000000 
                    !byte %0000000,%0000000,%0000000 
                     !byte %0000000,%0000000,%0000000 
                      !byte %0000000,%0000000,%0000000 
                       !byte %0000000,%0000000,%0000000 
                        !byte %0000000,%0000000,%0000000 
                         !byte %0000000,%0000000,%0000000 
                          !byte %0000000,%0000000,%0000000 
 

; SPRITE #6 --> EXPLOSION 1  --> POINTER $82
      ;  !byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$80,$01,$02,$00,$01,$02,$00,$02,$6A
      ;  !byte $40,$00,$6A,$40,$00,$A9,$40,$00,$2A,$40,$00,$2A,$80,$00,$AA,$80,$00,$AA,$80,$02
      ;  !byte $9A,$A0,$02,$6A,$50,$02,$80,$A0,$02,$00,$A0,$00,$00,$00,$00,$00,$00,$00,$00,$00
      ;  !byte $00,$00,$00
     ;  !byte 0

; SPRITE #6 --> EXPLOSION 2 --> POINTER $83
       ; !byte $00,$00,$00,$08,$00,$A0,$08,$00,$40,$0A,$20,$40,$01,$A5,$40,$01,$AA,$80,$02,$AA
       ; !byte $80,$2A,$AA,$80,$09,$AA,$40,$01,$A9,$40,$01,$69,$40,$01,$6A,$60,$01,$6A,$60,$01
      ;  !byte $6A,$A0,$02,$AA,$80,$02,$AA,$60,$0A,$55,$60,$08,$99,$60,$28,$00,$08,$20,$00,$00
      ;  !byte $00,$00,$00
       ; !byte 0

; SPRITE #6 --> EXPLOSION 3 --> POINTER $84
       ; !byte $20,$02,$80,$28,$0A,$80,$08,$9A,$0A,$09,$95,$68,$01,$A9,$50,$09,$AA,$90,$A9,$AA
      ;  !byte $90,$A6,$AA,$A0,$05,$6A,$A0,$29,$6A,$A0,$09,$AA,$A0,$29,$AA,$90,$29,$AA,$90,$06
      ;  !byte $AA,$98,$06,$AA,$98,$06,$96,$90,$06,$65,$50,$04,$25,$60,$28,$25,$28,$A0,$28,$08
      ;  !byte $80,$20,$0A
       ; !byte 0

; SPRITE #6 --> EXPLOSION 4 --> POINTER $85
       ; !byte $28,$02,$80,$2A,$9A,$80,$09,$5A,$80,$0A,$A5,$60,$26,$AA,$50,$26,$AA,$90,$AA,$AA
      ;  !byte $A0,$5A,$AA,$A0,$5A,$6A,$A8,$96,$6A,$9A,$26,$5A,$96,$06,$AA,$A6,$06,$AA,$A8,$06
      ;  !byte $AA,$A8,$05,$AA,$58,$05,$6A,$50,$05,$5A,$50,$2A,$9A,$A8,$18,$18,$28,$A0,$18,$28
      ;  !byte $80,$20,$0A
       ; !byte 0

; SPRITE #6 --> EXPLOSION 5 --> POINTER $86
       ; !byte $80,$08,$02,$A2,$28,$02,$29,$6A,$8A,$09,$96,$AA,$0A,$A6,$AA,$AA,$A5,$A6,$56,$A9
       ; !byte $A4,$56,$69,$A4,$69,$65,$56,$A9,$6A,$A6,$29,$6A,$A6,$09,$9A,$A6,$0A,$96,$A8,$0A
      ;  !byte $96,$A8,$0A,$AA,$A0,$29,$56,$50,$25,$66,$50,$29,$A5,$68,$A0,$2A,$0A,$A0,$2A,$02
      ;  !byte $80,$28,$02
      ;  !byte 0

; SPRITE #6 --> EXPLOSION 6 --> POINTER $87
       ; !byte $02,$A8,$02,$06,$96,$0A,$05,$6D,$B8,$07,$EE,$78,$0B,$EB,$A0,$AA,$6B,$A0,$96,$A7
       ; !byte $E0,$A6,$95,$F8,$25,$A6,$B8,$09,$E6,$96,$09,$F5,$AA,$09,$B9,$9A,$09,$BE,$9A,$0A
      ;  !byte $AA,$98,$09,$DA,$78,$09,$F6,$F8,$2A,$9A,$B8,$28,$14,$94,$28,$28,$A4,$00,$20,$2A
      ;  !byte $00,$20,$2A
      ;  !byte 0

; SPRITE #6 --> EXPLOSION 7 --> POINTER $88
       ; !byte $00,$00,$00,$03,$68,$00,$02,$58,$94,$00,$A6,$58,$0A,$F6,$60,$0F,$65,$80,$1F,$55
       ; !byte $C0,$16,$A5,$C0,$25,$5E,$E0,$09,$7F,$50,$09,$B7,$90,$0D,$7E,$50,$0F,$7D,$60,$0F
      ;  !byte $5E,$60,$0F,$69,$40,$07,$96,$80,$05,$9B,$E0,$26,$20,$E0,$00,$00,$00,$00,$00,$00
      ;  !byte $00,$00,$00
      ;  !byte 0

; SPRITE #6 --> EXPLOSION 8 --> POINTER $89
      ;  !byte $00,$00,$00,$00,$50,$00,$00,$75,$50,$00,$FD,$C0,$01,$5F,$C0,$01,$57,$40,$03,$75
      ;  !byte $50,$07,$7D,$50,$15,$7D,$F0,$3F,$7D,$74,$1D,$FD,$54,$0D,$5F,$50,$07,$F5,$F0,$05
       ; !byte $F5,$D0,$15,$DF,$D0,$01,$FF,$50,$01,$50,$40,$01,$00,$50,$00,$00,$10,$00,$00,$00
     ;   !byte $00,$00,$00
     ;;   !byte 0

; SPRITE #6 --> EXPLOSION 9 --> POINTER $90
      ;  !byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$40,$00,$07,$C0,$01,$57
      ;  !byte $C0,$01,$5F,$40,$03,$D7,$50,$05,$F7,$D0,$05,$FF,$C0,$01,$FD,$40,$01,$75,$40,$05
      ;  !byte $5F,$40,$07,$FF,$40,$05,$7F,$C0,$01,$F5,$40,$00,$34,$00,$00,$10,$00,$00,$00,$00
       ; !byte $00,$00,$00
      ;  !byte 0

; SPRITE #6 --> EXPLOSION 10 --> POINTER $91
       ; !byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1C
      ;  !byte $00,$00,$14,$00,$00,$55,$00,$01,$DD,$00,$01,$DD,$00,$03,$DD,$00,$01,$5F,$00,$01
      ;  !byte $57,$00,$00,$F7,$00,$00,$75,$00,$00,$51,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
      ;  !byte $00,$00,$00
      ;  !byte 0

; SPRITE #6 --> EXPLOSION 11 --> POINTER $92
       ; !byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
       ; !byte $00,$00,$00,$00,$00,$11,$00,$00,$15,$00,$00,$55,$00,$00,$55,$00,$00,$55,$40,$00
      ;  !byte $55,$40,$00,$55,$40,$00,$15,$00,$00,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
      ;  !byte $00,$00,$00
       ; !byte 0

; SPRITE #6 --> EXPLOSION 12 --> POINTER $93
       ; !byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
      ;  !byte $00,$00,$00,$00,$00,$00,$00,$00,$54,$00,$00,$14,$00,$00,$55,$00,$00,$54,$00,$00
      ;  !byte $14,$00,$00,$10,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
      ;  !byte $00,$00,$00
 
 
 
 
 *=$2000+190
 
!bin "spr1.prg"
 
;*=$2000+766+512
;!bin "spr1.prg"
 
 
; SPRITE #7 --> MISSILE  --> POINTER $92
   ;     !byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$00,$00,$FF
      ;  !byte $00,$00,$FF,$00,$00,$FF,$00,$00,$7E,$00,$00,$7E,$00,$00,$7E,$00,$00,$3C,$00,$00
      ;  !byte $3C,$00,$00,$3C,$00,$00,$18,$00,$00,$18,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00
       ; !byte $00,$00,$00
        ;!byte 0
  ; SPRITE #0 --> MAINSHIP --> POINTER $82
   ;     !byte $00,$10,$00,$00,$10,$00,$00,$10,$00,$00,$54,$00,$00,$54,$00,$00,$64,$00,$00,$64
   ;     !byte $00,$01,$65,$00,$31,$45,$30,$31,$45,$30,$11,$45,$10,$11,$65,$10,$11,$65,$10,$55
   ;     !byte $A9,$54,$56,$AA,$54,$5A,$AA,$94,$C9,$55,$8C,$C9,$DD,$8C,$C0,$DC,$0C,$CF,$CF,$C0
     ;   !byte $0F,$03,$C0
    ;    !byte 0

; SPRITE #1 --> ENEMY SHIP --> POINTER $83
      ;  !byte $00,$3C,$00,$00,$3C,$00,$01,$FF,$40,$05,$FF,$50,$17,$FF,$D4,$5F,$FF,$F5,$5F,$C3
       ; !byte $F5,$5F,$C3,$F5,$1F,$C3,$F4,$17,$C3,$D4,$07,$FF,$D0,$05,$BE,$50,$01,$BE,$40,$01
       ; !byte $BE,$40,$00,$BE,$00,$00,$BE,$00,$00,$BE,$00,$00,$AA,$00,$00,$28,$00,$00,$28,$00
       ;; !byte $00,$28,$00
        !byte 0



; SPRITE #4 --> ENEMY 2 SHIP --> POINTER $84
      ;  !byte $00,$00,$00,$00,$15,$40,$00,$55,$00,$01,$54,$00,$01,$60,$00,$05,$A0,$00,$05,$A0
      ;  !byte $00,$15,$A0,$00,$D6,$BF,$00,$FA,$BF,$C0,$FA,$BF,$F0,$FA,$BF,$C0,$D6,$BF,$00,$15
     ;   !byte $A0,$00,$05,$A0,$00,$05,$A0,$00,$01,$60,$00,$01,$54,$00,$00,$55,$00,$00,$15,$40
      ;  !byte $00,$00,$00
      ;  !byte 0


 
 
 



MISSILETIME    =  $6994                    ; 0=IT'S NOT THE TIME FOR THE MISSILE YET :)
MISSILEON    =   $6757                        ; 0=NO MISSILE DURING GAMEPLAY
MISSILEX    =   $6342            ; X XOORDINATES FOR THE MISSILE TO DROP DOWN
MISSILEY     =  $6973                        ; MISSILE Y COORDINATE TO START
MISSILEXNO   =  $6238       
           ; POSITIONS FOR THE SPRITES
FIRELOCKUP = $6330                 ; VARIABLE FOR LOCKING UP THE FIRE BUTTON
NOMOVEBULL=  $61f0             ; MAINSHIP BULLET MOVEMENT VARIABLE
byteCOUNT =  $62f0              ; ENEMY SHIP POS COUNTER FOR THE SINE TABLE
SCORE       = $63f0             ; SCORE VARIABLE FOR 4 DIGITS
HISCORE     = $44f0           ; HI-SCORE VARIABLE FOR 4 DIGITS
ENSHIPDIR  =  $65f0             ; ENEMY SHIP CIRCLE DIRECTION   0= LEFT2RIGHT, 1=RIGHT2LEFT
ENEMYBULLET  = $66f0            ; ENEMY BULLET VARIABLE 0=NO BULLET ON SCREEN
ENEMYBULLET2 = $67f0             ; ENEMY BULLET 2 VARIABLE 0=NO BULLET ON SCREEN
ENEMY2DIR   = $68f0             ; DIRECTION OF ENEMY SHIP 2 (UP OR DOWN)
LIVES       = $5056               ; NUMBER OF LIVES
           ; GET CHAR. OF KEY PRESSED
ATTDEC = $6bf0                    ; SOUND -> ATTACK-DECAY VARIABLE
SUSREL = $6cf0                    ; SOUND -> SUSTAIN-RELEASE VARIABLE
VOLUME = $6df0                    ; SOUND -> VOLUME VALUE
HIFREQ = $61e0                   ; SOUND -> HIGH FREQUENCY VARIABLE
LOFREQ = $62e0                    ; SOUND -> LOW FREQUENCY VARIABLE
WAVEFM = $63e0                  
   ;-----------------VARIABLES AND TABLES----------------------------------
SCREEN  = $0421                    ; SCORE SCREEN LOCATION
HISCREEN = $040A                   ; HI-SCORE SCREEN LOCATION
SCREEN2 = $0417                    ; LIVES SCREEN LOCATION
SCREEN3 = $05EF                    ; GAME OVER !SCR 1 SCREEN LOCATION         
SCREEN4 = $0639                    ; GAME OVER !SCR 2 SCREEN LOCATION
SCREEN3COL = $D9EF                 ; GAME OVER !SCR 1 COLOR LOCATION 
SCREEN4COL = $DA39 
SCREEN1COL = $D800                 ; GAME OVER !SCR 1 COLOR LOCATION 
SCREEN2COL = $D900 
; GAME OVER !SCR 2 COLOR LOCATION 
SCREEN5 = $07C0                    ; YOU ARE DOWN MESSAGE SCREEN LOCATION
SCREEN6 = $07C0                    ; ENEMY SHIP DOWN MESSAGE SCREEN LOCATION
SCREEN5COL = $DBC0                 ; YOU ARE DOWN MESSAGE COLOR LOCATION
SCREEN6COL = $DBC0                 ; ENEMY SHIP DOWN MESSAGE COLOR LOCATION
SCRDATA = $7F40                  ; BITMAP TITLE DATA - $7F40
COLDATA = $8328                     ; BITMAP TITLE COLOR DATA - $8328     
SCRRAM =  $5C00                    ; BITMAP TITLE SCREEN LOCATION - $5C00
COLRAM =$D800                     ; BITMAP TITLE COLOR LOCATION - $D800            
STARSDELAYARRAY   !byte 0,40        ; NUM OF COLUMNS AND DELAY
STARSFRAMEARRAY     =$6664  ; NUM OF COLUMNS AND CHARACTER
sounddecay =$4442
STARSCURRENTX      =  $6344   
STARSCURRENTY       =  $6352   
STARSCURRENTFRAME    =  $6783 
STARSCURRENTCOLOR   =   $6234 
STARSNEXTSEED     =  $6458   
*=$7000-$7e
!bin "touch.sid"
