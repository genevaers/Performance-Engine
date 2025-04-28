         TITLE 'GVBDL96 - FORMAT AN INDIVIDUAL FIELD FOR OUTPUT'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2000, 2021.
*     Copyright Contributors to the GenevaERS Project.
* SPDX-License-Identifier: Apache-2.0
*
**********************************************************************
*
*  Licensed under the Apache License, Version 2.0 (the "License");
*  you may not use this file except in compliance with the License.
*  You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
*  Unless required by applicable law or agreed to in writing, software
*  distributed under the License is distributed on an "AS IS" BASIS,
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
*  or implied.
*  See the License for the specific language governing permissions
*  and limitations under the License.
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  GVBDL96 - FORMATS A FIELD (SOURCE FIELD) INTO AN OUTPUT FIELD      *
*            (TARGET FIELD) BASED ON ITS INTERNAL/SOURCE FORMAT AND   *
*            THE OPTIONAL MASK (PICTURE) SPECIFICATION.               *
*                                                                     *
*            THE RETURN CODE INDICATES SUCCESS OR FAILURE.            *
*            ABNORMAL CONDITIONS ARE IDENTIFIED BY DIFFERENT RETURN   *
*            CODES.  A RETURN CODE OF ZERO INDICATES SUCCESS.         *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK REGISTER                                *
*              RETURN    CODE                                         *
*                                                                     *
*        R14 - TEMPORARY WORK REGISTER                                *
*              RETURN    ADDR                                         *
*                                                                     *
*        R13 - REGISTER  SAVE AREA ADDRESS                            *
*                                                                     *
*        R12 - PARAMETER LIST ADDRESS (PROVIDED BY CALLER)            *
*                                                                     *
*        R11 - PROGRAM   BASE REGISTER                                *
*                                                                     *
*        R10 - FORMATTING PARAMETER AREA   ADDRESS                    *
*                                                                     *
*        R9  - INTERNAL SUBROUTINE  RETURN ADDRESS                    *
*                                                                     *
*        R8  - POSITION  OF   LAST  DIGIT                             *
*                                                                     *
*        R7  - CURRENT   MASK LENGTH                                  *
*        R6  - CURRENT   MASK POSITION                                *
*                                                                     *
*        R5  -                                                        *
*        R4  -                                                        *
*                                                                     *
*        R3  - CURRENT OUTPUT POSITION                                *
*                                                                     *
*        R2  - SOURCE   FIELD LENGTH                                  *
*                                                                     *
*        R1  - CURRENT SOURCE POINTER                                 *
*                                                                     *
*            - TEMPORARY WORK REGISTER                                *
*            - PARAMETER LIST ADDRESS                                 *
*                                                                     *
*        R0  - TEMPORARY WORK REGISTER                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        EJECT
         IHASAVER DSECT=YES,SAVER=YES,TITLE=NO
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  PARAMETER LIST DEFINITION (COBOL CALL PARAMETERS)                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
PARMLIST DSECT
*
PRMADDR  DS    A             ADDRESS OF PARAMETER    AREA
TGTADDR  DS    A             ADDRESS OF TARGET DATA  AREA
LENADDR  DS    A             ADDRESS OF EDITED DATA  LENGTH(S9(4) COMP)
RTNADDR  DS    A             ADDRESS OF RETURN CODE        (S9(4) COMP)
                        SPACE 3
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        Floating point register equates                              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
fp0      equ 0,,,,fpr
fp1      equ 1,,,,fpr
fp2      equ 2,,,,fpr
fp3      equ 3,,,,fpr
fp4      equ 4,,,,fpr
fp5      equ 5,,,,fpr
fp6      equ 6,,,,fpr
fp7      equ 7,,,,fpr
fp8      equ 8,,,,fpr
fp9      equ 9,,,,fpr
fp10     equ 10,,,,fpr
fp11     equ 11,,,,fpr
fp12     equ 12,,,,fpr
fp13     equ 13,,,,fpr
fp14     equ 14,,,,fpr
fp15     equ 15,,,,fpr
                        SPACE 3
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        PARAMETER  AREA  DEFINITION                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
PARMAREA DSECT               PARAMETER AREA
*
         dl96area dsect=no
*
SAPARMLN EQU   *-PARMAREA    FIELD    PARAMETER  AREA  LENGTH
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  REGISTER EQUATES:                                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
         Copy  GVBASSRT
         Copy  GVBMR95C
         Copy  GVBMR95L
         Copy  GVB0200A
         Copy  GVB0200B
*
MAXSRC   EQU   32767
*AXSRC   EQU   256
STDMAP   EQU   25                       STANDARD CCYYMMDDHHMMSSTTNNNN
MAXDEC   EQU   08                       MAXIMUM  NO. OF DECIMALS
*
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*                                                                     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         Print OFF
         Copy  ASMMSP
LEAVE    OpSyn ASM_LEAVE
         ASMMREL ON
         IEABRCX DEFINE
         SYSSTATE ARCHLVL=2,AMODE64=YES
         PRINT NOGEN,ON
GVBDL96  RMODE ANY
GVBDL96  AMODE 31
GVBDL96  CSECT
         J   CODE
DL96EYE  GVBEYE GVBDL96
*
         print nogen
static   loctr          set up static loctr first
code     loctr          followed by the code Loctr
         using savf4sa,r13
         stmg  R14,R12,SAVF4SAG64RS14  SAVE  CALLER'S  REGISTERS
*
         sam64
         b     dl96start
*
* Entry point to use if already in 64 bit mode (avoid sam64)
*  Use bassm with lob of address on to branch here
*
         ENTRY GVBDL96X
GVBDL96X AMODE 64
GVBDL96X DS    0H
         stmg  R14,R12,SAVF4SAG64RS14  SAVE  CALLER'S  REGISTERS

*
dl96start equ  *
         larl  R11,gvbdl96        set up base and alet
         USING (GVBDL96,code),R11 and address the static area
*
         lgr   R12,R1             SAVE  PARAMETER LIST ADDRESS
         USING PARMLIST,R12
*
         llgt  R10,PRMADDR        LOAD  PARAMETER AREA ADDRESS
         USING PARMAREA,R10
*
         MVI   SAFMTERR,X'00'     ASSUME FIELD DATA  IS ALWAYS GOOD
*
         lg    R1,SAVALADR        LOAD  SOURCE DATA  AREA   ADDRESS
*
         lgh   R2,SAVALLEN        LOAD  SOURCE DATA  LENGTH
         ltgr  R2,R2              CHECK THE   VALUE  ???
         BRNP  RETURN             RETURN  IF  NULL
         if cghi,R2,gt,MAXSRC      MAXIMUM   LENGTH   EXCEEDED ???
           lghi R2,MAXSRC            Set to max
         endif
*
         lgh   R15,SAVALFMT
         if cgij,R15,le,12
           sllg R15,r15,2
           basr r9,0              get the base
           using (*,fmterror),r9      tell the assembler about r9
SRCFMTBL   B   SRCFMTBL(R15)
           drop r9
           BRU ALPHASRC           01 - ALPHANUMERIC FORMAT  ???
           BRU ALPHASRC           02 - ALPHABETIC   FORMAT  ???
           BRU NUMERIC            03 - NUMERIC      FORMAT  ???
           BRU PACKED             04 - PACKED  DATA FORMAT  ???
           BRU SORTDEC            05 - SORTABLE     DECIMAL FORMAT ???
           BRU BINARY             06 - BINARY       FORMAT  ???
           BRU SORTBIN            07 - SORTABLE     BINARY  FORMAT ???
           BRU BCD                08 - BINARY CODED DECIMAL FORMAT ???
           BRU ALPHASRC           09 - MASKED       NUMERIC FORMAT ???
           BRU EDITNUM            0A - EDITED       NUMERIC FORMAT ???
           BRU DFPINPUT           0B - FLOAT        NUMERIC FORMAT ???
           BRU BADRC              0C - GENEVA       NUMBER  FORMAT ???
         endif
*
FMTERROR EQU   *
         MVI   SAFMTERR,EMFORMAT  UNSUPPORTED FORMAT CODE
         BRU   Ret_err            SET  RETURN CODE = FAILURE
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        B I N A R Y   S O U R C E   D A T A   S U P P O R T          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         DS   0D
COPYNUM  MVC   SAWORKAR(0),0(R1)    *** EXECUTED ***
                        SPACE 3
***********************************************************************
*  SORTABLE BINARY                                                    *
***********************************************************************
SORTBIN  lgr   R15,R2             LOAD SOURCE  DATA  LENGTH
         bctgr R15,0              DECREMENT FOR "EX" INSTRUCTION
         EXrl  R15,COPYNUM        COPY NUMERIC FIELD TO SAWORKAR
*
         LA    R1,SAWORKAR        RESET SOURCE DATA  POINTER
*
         XI    0(R1),X'80'        INVERT  SIGN BIT
*
***********************************************************************
*  BINARY                                                             *
***********************************************************************
binary   ds    0h
         if cli,savalsgn,eq,C'Y',or,     Is this value signed?         +
               cli,savalfmt+1,eq,fc_sortb  or sortable binary?

            select cij,r2,eq       test the length
              when 1
                lgb r15,0(,r1)     load the byte
              when 2
                lgh r15,0(,r1)     load the halfword
              when 4
                lgf r15,0(,r1)     load the fullword
              when 8
                lg  r15,0(,r1)     load the doubleword
              othrwise
                MVI SAFMTERR,EMSRCLEN Invalid length
                b   ret_err
*               xgr r15,r15        and make this zero
            endsel
         else ,                    this is the unsigned case
            select cij,r2,eq       test the length
              when 1
                llgc r15,0(,r1)    load the byte
              when 2
                llgh r15,0(,r1)    load the halfword
              when 4
                llgf r15,0(,r1)    load the fullword
              when 8
                lg   r15,0(,r1)    load the doubleword
              othrwise
                MVI SAFMTERR,EMSRCLEN Invalid length
                b   ret_err
*               xgr r15,r15        and make this zero
            endsel
         endif
         cvdg  r15,sapackar       convert   to  decimal
         if cij,r2,eq,8           are we dealing with 8 bytes?
           la  r1,sapackar
           lghi r2,16
         else
           la  r1,sapackar+8
           lghi r2,8
         endif
         j     packed
                        SPACE 3
***********************************************************************
*  SORTABLE PACKED DECIMAL                                            *
***********************************************************************
         DS   0D
DECSHIFT MVO   0(0,R1),0(0,R1)    * * * *  E X E C U T E D  * * * *
         ds    0d
decsign  oi    0(R14),X'00'       * * * *  E X E C U T E D  * * * *
         DS   0D
PACKSHFT SRP   0(0,R1),0(R15),5   * * * *  E X E C U T E D  * * * *
           DS 0D
COPYDEC    MVC 0(0,R14),1(R1)       *** EXECUTED ***
         DS   0D
PACKSRP  SRP   SAPACKAR,0(R15),5  * * * *  E X E C U T E D  * * * *
           DS 0D
DECCOMP    XC  0(0,R1),HEXFF      * * * *  E X E C U T E D  * * * *
SORTDEC  lgr   R15,R2             LOAD SOURCE  DATA  LENGTH
         bctgr R15,0              DECREMENT FOR "EX" INSTRUCTION
         LA    R14,SAPACKAR+L'SAPACKAR RIGHTJUSTIFY  COPIED NUMBER
         sgr   R14,R2
         exrl  R15,COPYJUST       COPY NUMERIC FIELD TO SAWORKAR
*
         lgr   R1,R14             RESET SOURCE DATA  POINTER
*
         MVC   SADBLWRK(1),0(R1)  SAVE  HIGH  ORDER  SIGN NIBBLE
         bctgr R2,0               BUILD L1,L2
         lgr   R14,R2
         sllg  R14,r14,4
         agr   R14,R2
         exrl  R14,DECSHIFT       SHIFT LEFT ONE DIGIT
         LA    R14,0(R1,R2)       POINT TO LAST  BYTE
         LA    R15,DECPOS         ASSUME   POSITIVE SIGN
         if TM,SADBLWRK,X'F0',NO  negative ?
           exrl R2,DECCOMP        COMPLEMENT DIGITS
           LA  R15,DECNEG         POINT TO NEGATIVE SIGN
         endif
         NI    0(R14),X'F0'       CLEAR OUT SIGN NIBBLE
         exrl  r15,decsign        SET   THE SIGN
static   loctr
DECPOS   equ X'0C'
DECNEG   equ X'0D'
code     loctr
*
         AHI   R2,1               RESTORE LENGTH TO TRUE VALUE
*
***********************************************************************
*  PACKED DECIMAL                                                     *
***********************************************************************
PACKED   ds    0h
         LA    R14,0(R1,R2)       COMPUTE END OF DATA ADDRESS +1
         bctgr R14,0              BACKUP TO ADDRESS OF LAST DIGIT
         ic    r9,0(r14)
         sll   r9,4
         stc   r9,SASAVSGN        SAVE SIGN
         NI    SASAVSGN,X'F0'     ZERO  THE DIGIT POSITION LEAVING SIGN
*
         Chhsi SAVALRND,0         ROUNDING FACTOR  SPECIFIED ???
         bre   packed01
*
         lgr   r14,r2             LOAD  PACKED VALUE LENGTH
         bctgr R14,0              DECREMENT FOR "EX"
         sllg  R14,r14,4          SHIFT   FOR  "EX"
         lh    r15,SAVALRND       CONVERT ROUNDING FACTOR TO BINARY
         LCR   R15,R15            MAKE   SHIFT LENGTH NEGATIVE (RIGHT)
         exrl  R14,PACKSHFT       SHIFT  VALUE USING  ROUNDING FACTOR
*
PACKED01 BRAS  R9,UNPKLONG        UNPACK THE SOURCE DATA
*
*
         BRU   NUMCOMN
***********************************************************************
*  Decimal Float input                                             *
***********************************************************************
DFPinput ds    0H
         ld    fp4,0(0,r1)        Load value into floating point
         ld    fp6,8(0,r1)        register pair
         qaxtr fp4,fp4,fp9,0      and adjust the input so that it      +
                                  has 8 or 3 decimal places
*        The desired quantum is chosen in MR96, and passed back to MR95
*        who then loads it into fp9/fp11 - pair must not be updated
*
         esxtr r14,fp9            get number of significant digits in  +
                                  quantum value
         if cij,r14,lt,8
           MVHHI savaldec,3       tell formatting we have 3 decimals
         else
           MVHHI savaldec,8       tell formatting we have 8 decimals
         endif
         csxtr r14,fp4,0          Convert to Signed packed in GPR pair
         stmg  r14,r15,sapackar   and save
         la    r1,sapackar        Set new source pointer
         la    r2,l'fc_float        set length
         BRAS  R9,UNPKLONG        UNPACK THE SOURCE DATA
*
         LA    R14,0(R1,R2)       COMPUTE END OF DATA ADDRESS +1
         bctgr R14,0              BACKUP TO ADDRESS OF LAST DIGIT
         MVC   SASAVSGN,0(R14)    SAVE SIGN
         NI    SASAVSGN,X'F0'     ZERO  THE DIGIT POSITION LEAVING SIGN
*
         BRU   NUMCOMN
                        EJECT
***********************************************************************
*  BINARY CODED DECIMAL                                               *
***********************************************************************
BCD      aghi  R2,1               ADD  ONE FOR GARBAGE BYTE AT END
         BRAS  R9,UNPKLONG
*
         bctgr R2,0               SUBTRACT  ONE FROM LEN (GARBAGE BYTE)
         LA    R14,0(R1,R2)       CLEAR GARBAGE BYTE
         MVI   0(R14),C'0'
*
         MVI   SASAVSGN,X'F0'     SAVE SIGN
         BRU   NUMERIC0
                        SPACE 3
***********************************************************************
*  EDITED NUMERIC ("+", "-", "." ALLOWED)                             *
***********************************************************************
EDITNUM  MVI   SAWORKAR,C'0'      ZERO OUT WORK AREA
         MVC   SAWORKAR+1(L'SAWORKAR-1),SAWORKAR
*
editout  do ,
           do from=(r2)
             doexit cli,0(r1),ne,c' ',do=editout   Leading blank?
             LA    R1,1(,R1)
           enddo
*
           LA  R1,ZEROES
           lghi R2,1
*
         enddo
         jas   R9,CNTDIGIT        COUNT DECIMAL PLACES, ETC
*
         LA    R14,SAWORKAR+L'SAWORKAR  RIGHTJUSTIFY COPIED NUMBER
         LA    R1,0(R1,R2)
*
         Lh     R15,SASRCDEC
         sth    r15,savaldec
         if ltr,R15,R15,p
           sgfr R14,R15
           lg   R1,SADECADR
           BCTR R15,0             DECREMENT FOR "EX" INSTRUCTION
           exrl R15,COPYDEC       COPY NUMERIC FIELD TO SAWORKAR
         endif
*
         Lh    R2,SASRCDIG        total number of digits
         LR    R15,R2
         SH    R15,SASRCDEC       calc no. digits before dec point
         Sgfr  R14,R15            Point to place in work area
         Sgfr  R1,R15             Point to start of source digits
         bctr  R15,0              DECREMENT FOR "EX" INSTRUCTION
         exrl  R15,COPYJUST       COPY NUMERIC FIELD TO SAWORKAR
*
         lgr   R1,R14             RESET   SOURCE DATA POINTER
*
         BRU   NUMERIC0
                        SPACE 3
***********************************************************************
*  NUMERIC (DISPLAY)                                                  *
***********************************************************************
NUMERIC  MVI   SAWORKAR,C'0'      ZERO OUT WORK AREA
         MVC   SAWORKAR+1(L'SAWORKAR-1),SAWORKAR
*
         lgr   R15,R2             LOAD SOURCE  DATA  LENGTH
         bctgr R15,0              DECREMENT FOR "EX" INSTRUCTION
         LA    R14,SAWORKAR+L'SAWORKAR RIGHTJUSTIFY  COPIED NUMBER
         sgr   R14,R2
         exrl  R15,COPYJUST       COPY NUMERIC FIELD TO SAWORKAR
*
         lgr   R1,R14             RESET SOURCE DATA  POINTER
*
         LA    R14,0(R1,R2)       COMPUTE END OF DATA ADDRESS +1
         bctgr R14,0              BACKUP TO ADDRESS OF LAST DIGIT
         MVC   SASAVSGN,0(R14)    SAVE SIGN
         NI    SASAVSGN,X'F0'     ZERO  THE DIGIT POSITION LEAVING SIGN
*
NUMERIC0 Chhsi SAVALRND,0         ROUNDING FACTOR  SPECIFIED ???
         BRE   NUMCOMN            NO  - BYPASS ROUNDING
*
         BRAS  R9,PACKLONG        CONVERT TO PACKED FORMAT
         lh    r15,SAVALRND       CONVERT ROUNDING  FACTOR TO BINARY
         LCR   R15,R15
         exrl  0,PACKSRP          SHIFT  VALUE USING ROUNDING FACTOR
*
         BRAS  R9,UNPKLONG        UNPACK THE  SOURCE DATA
                        SPACE 3
***********************************************************************
*  COMMON ZONED DECIMAL (NUMERIC) PROCESSING                          *
*  CONVERT INPUT DATA TO STANDARD DATE/TIME FORMAT IF CONTENT NONZERO *
***********************************************************************
NUMCOMN  lgh   R0,SAOUTCON        OUTPUT CONTENT SPECIFIED   ???
         if LTR,R0,R0,p
*
           LA   R1,0(R1,R2)       ADVANCE TO END OF  SOURCE  VALUE
           lay  R15,-1(,r1)       back up one
           OI   0(R15),X'F0'      make unsigned
           lgh R15,SAVALCON       LOAD   SOURCE  CONTENT     CODE
           sllg R15,r15,1              TIMES  2
           llgt R14,CONOLENA       LOAD   SOURCE  DATE LENGTH
           lgh R2,0(R14,R15)
           sgr R1,R2
*
           BRAS R9,CONCODE        YES -  CONVERT TO STANDARD FORMAT
*
           MVI SASAVSGN,X'F0'     SET SIGN
         endif
*
***********************************************************************
*  MASK (IF ANY) TAKES PRECEDENCE OVER OUTPUT FORMAT/LENGTH           *
***********************************************************************
         lgh   R7,SAMSKLEN        INITIALIZE  CURRENT MASK LENGTH
         if ltgr,R7,R7,p          is it positive?
           llgt  R6,SAMSKADR      INITIALIZE  CURRENT MASK POSITION
           LA  R14,0(R6,R7)       COMPUTE END-OF-MASK ADDRESS (+1)
           do from=(r7)
             bctgr R14,0           BACKUP TO LAST BYTE
             doexit CLI,0(R14),ne,C' ' SCAN BACKWARDS FOR  NON-BLANK
           enddo
           b  mask
         endif

***********************************************************************
*  FORCE PRINTABLE ZONE IF UNSIGNED                                   *
***********************************************************************
         if cli,saoutfmt+1,ne,fc_sortb,and,  Not sortable binary       +
               cli,saoutfmt+1,ne,fc_sortp    and not sortable packed
*         if cli,savalsgn,eq,C'N',and,      source unsigned           +
*             cli,saoutsgn,eq,C'N',orif,    target field unsigned?    +
           if cli,saoutsgn,eq,C'N',orif,     target unsigned           +
               cli,saoutfmt+1,eq,fc_edit,or, or target is edited num   +
               cli,saoutfmt+1,eq,fc_mask     or target is masked
*
             if cli,saoutfmt+1,ne,fc_edit    not edited numeric?
               MVI SASAVSGN,X'F0' NO  - FORCE TO DISPLAY
             endif
*
             LA R14,0(R1,R2)      FORCE PRINTABLE ZONE
             bctgr R14,0
             OI 0(R14),X'F0'
           endif
         endif
*
***********************************************************************
*  ADD LEADING/TRAILING ZEROES TO NUMERIC (DISPLAY) DATA              *
***********************************************************************
NUMSLEN  lgr   R15,R2             COMPUTE NO. OF LEADING ZEROES NEEDED
         lh    R0,SAVALDEC
         sgfr  R15,R0             R15 =  INTEGER POSITIONS   IN SOURCE
*
         llgt  R14,LENADDR
         lgh   R14,0(,R14)
*
         lgh   R0,SAOUTFMT
*
         Clije  R0,FC_ALNUM,alpha

         CLIJH R14,32,COLERR01    Numeric column can't be > 32 (this
*                                  prevents potential abends)

         select clij,r0,eq
         when  (fc_pack,fc_sortp,fc_float)
           sllg R14,r14,1
           bctgr R14,0            R14 = MAXIMUM    NEEDED   IF PACKED
         when  fc_edit
           if CLI,SAOUTSGN,eq,C'Y'
             bctgr R14,0           ALLOW FOR SIGN
           endif

           if CHHSI,SAOUTDEC,gt,0
             bctgr R14,0           ALLOW FOR DECIMAL POINT
           endif
         when  (fc_bcd)
           sllg R14,r14,1
         when  (fc_bin,fc_sortb)
           llc R0,BINMAXD(R14)
           lgr R14,R0             R14 = MAXIMUM NEEDED    IF   BINARY
static     loctr
BINMAXD    DC  XL9'000305080A0C0F1113'
code       loctr
         endsel

         lgr   R8,R14             SAVE  OUTPUT  DIGITS
         sh    R14,SAOUTDEC       R14 = INTEGER POSITIONS IN   OUTPUT
*

         if sgr,R15,R14,P          R15 = EXCESS  INTEGERS  IN   SOURCE
           if cli,saoutfmt+1,ne,fc_bin,and, only test for non binary   +
               cli,saoutfmt+1,ne,fc_sortb
             lgr R14,R15          CHECK IF TRUNCATION IS  ONLY ZEROES ?
             BCTgR R14,0
             if exrl,R14,CHKLEADZ,ne if comparison not equal
               MVI SAFMTERR,EMTRUNC INDICATE TRUNCATION OCCURRED
             endif
           endif
         endif

         agr   R1,R15
         lgr   R2,R8              LOAD    OUTPUT LENGTH
*
***********************************************************************
*  SELECT OUTPUT FORMAT CONVERSION LOGIC                              *
***********************************************************************
         lgh   R15,SAOUTFMT       LOAD    OUTPUT FORMAT
         sllg  R15,r15,2
         basr  r9,0               get the base address
         using (*,numbin),r9        tell the assembler
OUTFMTBL B     OUTFMTBL(R15)
         drop  r9
         BRU   ALPHA              01 - ALPHANUMERIC FORMAT  ???
         BRU   ALPHA              02 - ALPHABETIC   FORMAT  ???
         BRU   NUMNUM             03 - NUMERIC      FORMAT  ???
         BRU   NUMPACK            04 - PACKED  DATA FORMAT  ???
         BRU   NUMSDEC            05 - SORTABLE     DECIMAL FORMAT ???
         BRU   NUMBIN             06 - BINARY       FORMAT  ???
         BRU   NUMBIN             07 - SORTABLE     BINARY  FORMAT ???
         BRU   NUMBCD             08 - BINARY CODED DECIMAL FORMAT ???
         BRU   MASKOUT            09 - MASKED       NUMERIC FORMAT ???
         BRU   NUMEDIT            0A - EDITED       NUMERIC FORMAT ???
         BRU   DFP                0B - FLOAT        NUMERIC FORMAT ???
         BRU   FMTERROR           0C - GENEVA       NUMBER  FORMAT ???
                        SPACE 3
***********************************************************************
*  BINARY OUTPUT  (INCL SORTABLE BINARY)                              *
***********************************************************************
NUMBIN   LA    R14,0(R1,R2)       EMBED CORRECT SIGN
         bctgr R14,0              get the correct byte
         MVZ   0(1,R14),SASAVSGN    and move the saved sign into zone
*
         BRAS  R9,PACKLONG        CONVERT TO PACKED FORMAT
*
         llgt  R14,LENADDR        USE   NEW  OUTPUT LENGTH
         lgh   R2,0(,R14)
         llgt  R14,TGTADDR        LOAD  TARGET AREA ADDRESS
*
         cvbg  r0,sapackar

         if cli,saoutsgn,eq,C'N',and,  Is this field unsigned?         +
               cli,saoutfmt+1,ne,fc_sortb  and not sortable binary
           select cij,r2,eq       chose depending on the length
             when 1
               stc r0,0(,r14)     One potato
               if clgfi,r0,gt,255           max 1 byte value
                 MVI SAFMTERR,EMTRUNC INDICATE TRUNCATION OCCURRED
               endif
             when 2
               STH R0,0(,R14)     Two potatoes
               if clgfi,r0,gt,65535         max 2 byte value
                 MVI SAFMTERR,EMTRUNC INDICATE TRUNCATION OCCURRED
               endif
             when 4
               ST R0,0(,R14)      Four potatoes
               if clgfi,r0,gt,x'ffffffff'   max 4 byte value
                 MVI SAFMTERR,EMTRUNC INDICATE TRUNCATION OCCURRED
               endif
             when 8
               STG R0,0(,R14)     and 8
             othrwise             all else is an error
               MVI SAFMTERR,EMTRUNC INDICATE TRUNCATION OCCURRED
               BCTR R2,0          RETURN   ZERO
               exrl R2,BINZERO
               aghi r2,1          put this back
           endsel
         else ,       the output is signed
           select cij,r2,eq       chose depending on the length
             when 1
               stc r0,0(,r14)     One potato
               if cgij,r0,lt,-128,or,cgij,r0,gt,127 range check
                 MVI SAFMTERR,EMTRUNC INDICATE TRUNCATION OCCURRED
               endif
             when 2
               STH R0,0(,R14)     Two potatoes
               if cghi,r0,lt,-32768,or,cghi,r0,gt,32767 range check
                 MVI SAFMTERR,EMTRUNC INDICATE TRUNCATION OCCURRED
               endif
             when 4
               ST R0,0(,R14)      Four potatoes
               if cgfi,r0,lt,x'80000000',or, <=== max -ve in 32 bits   +
               cgfi,r0,gt,2147483647               range check
                 MVI SAFMTERR,EMTRUNC INDICATE TRUNCATION OCCURRED
               endif
             when 8
               STG R0,0(,R14)     and 8
             othrwise             all else is an error
               MVI SAFMTERR,EMTRUNC INDICATE TRUNCATION OCCURRED
               BCTR R2,0          RETURN   ZERO
               exrl R2,BINZERO
               aghi r2,1          put this back
           endsel
         endif
         LA R3,0(R14,R2)          and move pointer
         CLI   SAOUTFMT+1,FC_SORTB    SORTABLE BINARY  ???
         BRNE  RETURN             NO  - RETURN
         XI    0(R14),X'80'       YES - INVERT SIGN BIT
         BRU   RETURN             RETURN
               DS 0D
BINZERO        XC 0(0,R14),0(R14) * * * *  E X E C U T E D  * * * *
                        SPACE 3
***********************************************************************
*  NUMERIC OUTPUT                                                     *
***********************************************************************
         ds 0d
make_it_zoned  mvz 0(0,r1),zeroes executed
numnum   ds   0h
         lgr  r14,r2             get length in work register
         bctgr r14,0               machine length
         exrl r14,make_it_zoned    and or in zones
         LA    R14,0(R14,r1)      EMBED CORRECT SIGN
         MVZ   0(1,R14),SASAVSGN    and move the saved sign into zone
*
         BRU   ALPHA
                        SPACE 3
***********************************************************************
*  PACKED DECIMAL OUTPUT                                              *
***********************************************************************
NUMPACK  LA    R14,0(R1,R2)       EMBED CORRECT SIGN
         bctgr R14,0              get the correct byte
         MVZ   0(1,R14),SASAVSGN    and move the saved sign into zone
*
         BRAS  R9,PACKLONG        CONVERT TO PACKED FORMAT
*
         llgt  R14,LENADDR        USE NEW OUTPUT LENGTH
         lgh   R2,0(,R14)
         llgt  R14,TGTADDR
         LA    R3,0(R14,R2)
         LA    R1,SAPACKAR+L'SAPACKAR
         sgr   R1,R2
         bctgr R2,0
         exrl  R2,RETPACK
*
         BRU   RETURN             RETURN
                        SPACE 3
***********************************************************************
*  decimal floating point output                                      *
***********************************************************************
DFP      LA    R14,0(R1,R2)       EMBED CORRECT SIGN
         bctgr R14,0              get the correct byte
         MVZ   0(1,R14),SASAVSGN    and move the saved sign into zone
*
         BRAS  R9,PACKLONG        CONVERT TO PACKED FORMAT
*
         llgt  R14,TGTADDR        get the target address
         lmg   r0,r1,sapackar     load GPR pair with packed data
         cxstr Fp0,r0             Convert to extended DFP format

         lh    r0,SAOUTDEC        get number of decimal places

         lghi  r15,6176           load exponent of zero
         sgfr  r15,r0             and adjust for no of decimals
         iextr fp0,fp0,r15
         std   fp0,0(0,r14)         and
         std   fp2,8(0,r14)           save
*
         BRU   RETURN             RETURN
                        SPACE 3
***********************************************************************
*  SORTABLE PACKED DECIMAL OUTPUT                                     *
***********************************************************************
NUMSDEC  LA    R14,0(R1,R2)       EMBED CORRECT SIGN
         bctgr R14,0              get the correct byte
         MVZ   0(1,R14),SASAVSGN    and move the saved sign into zone
         BRAS  R9,PACKLONG        CONVERT TO PACKED FORMAT
*
         llgt  R14,LENADDR        USE NEW OUTPUT LENGTH
         lgh   R2,0(,R14)
         llgt  R14,TGTADDR
         LA    R3,0(R14,R2)
         LA    R1,SAPACKAR+L'SAPACKAR
         sgr   R1,R2
         lgr   R15,R2
         sllg  R15,r15,4
         bctgr R2,0
         agr   R15,R2
         exrl  R15,SHFTSDEC
         exrl  R2,RETSDEC
*
         LA    R1,HEXFF           ASSUME POSITIVE NUMBER (SIGN=X'F')
         LA    R15,SAWORKAR+1(R2) LOAD ADDRESS OF SIGN
         NI    0(R15),X'F0'       CLEAR  RIGHT NIBBLE
         if CLI,0(R15),eq,X'D0'   NEGATIVE NUMBER ???
           LA  R1,LOWVAL          USE ZERO FOR NEGATIVE
           exrl R2,COMPSDEC       COMPLEMENT NEGATIVE NUMBER
         endif
*
         MVZ   0(1,R14),0(R1)     COPY  SIGN
*
         BRU   RETURN             RETURN
                        SPACE 3
***********************************************************************
*  BINARY CODED DECIMAL OUTPUT                                        *
***********************************************************************
NUMBCD   aghi  R2,1               ADD  ONE FOR GARBAGE BYTE
         BRAS  R9,PACKLONG        CONVERT  TO  PACKED  (+1)
*
         bctgr R2,0               SUBTRACT ONE (REMOVE GARBAGE BYTE)
         agr   R1,R2              POINT TO END (+1)
*
         llgt  R14,LENADDR        USE NEW OUTPUT LENGTH
         lgh   R2,0(,R14)
         sgr   R1,R2              POINT TO START (BACKUP  FROM END)
*
         llgt  R14,TGTADDR
         LA    R3,0(R14,R2)
         bctgr R2,0
         exrl  R2,RETPACK
         BRU   RETURN             RETURN
*
         DS   0D
RETPACK  MVC   0(0,R14),0(R1)     * * * *  E X E C U T E D  * * * *
         DS   0D
SHFTSDEC MVO   SAWORKAR(0),0(0,R1)  * * *  E X E C U T E D  * * * *
         DS   0D
COMPSDEC XC    0(0,R14),HEXFF     * * * *  E X E C U T E D  * * * *
         DS   0D
RETSDEC  MVC   0(0,R14),SAWORKAR  * * * *  E X E C U T E D  * * * *
         DS   0D
CHKLEADZ CLC   0(0,R1),SALEAD0    * * * *  E X E C U T E D  * * * *
         DS   0D
CHKLEADS CLC   0(0,R15),SPACES    * * * *  E X E C U T E D  * * * *
*
                        EJECT
***********************************************************************
*  EDITED NUMERIC OUTPUT                                              *
***********************************************************************
NUMEDIT  llgt  R3,TGTADDR         LOAD  OUPTUT AREA ADDRESS
*
         lh    r15,SAOUTDEC       LIMIT OUTPUT DECIMALS TO MASK MAXIMUM
         if clij,R15,gt,MAXDEC
           lghi R15,MAXDEC
         endif
*
         lghi  R0,MAXDEC          COMPUTE RIGHT TRUNCATION OF MASK
         sgr   R0,R15             EXCESS  MASK  DECIMALS
         if clij,R0,eq,MAXDEC      ANY DECIMALS  LEFT ???
           aghi R0,1
         endif
*
         LA    R6,CSVMASK+L'CSVMASK      LOAD END OF MASK ADDR
         sgr   R6,R0                     ADJUST  FOR NO. DECIMALS
         llgt  R15,LENADDR               LOAD COLUMN WIDTH
         lgh   R15,0(,R15)
         sgr   R6,R15                    BACKUP  TO  BEGINNING
         lgr   R7,R15                    SET   MASK  LENGTH
*
         MVI   SAHADSIG,C'N'             SET NO  SIGNIFICANCE
         MVI   SAHADDEC,C'N'             SET NO  DECIMAL POINT
*
         CLI   SAOUTSGN,C'Y'             SIGNED  OUTPUT  ???
         BRNE  MSKPROC                   NO  -   BYPASS  CHECK
*
         MVI   0(R3),C' '                ASSUME  POSITIVE
         if TM,SASAVSGN,X'30',M                NEGATIVE VALUE  ???
           MVI 0(R3),C'-'                      SET MINUS SIGN
         endif
         lgr   R0,R3                           SAVE  ORIGINAL  POSITION
         LA    R3,1(,R3)                       ADVANCE OUTPUT  POSITION
*
         LA    R6,1(,R6)                       REDUCE MASK BY  ONE
         if    ahi,r7,-1,p       reduce r7 by one and if positive
           la  R4,SAWORKAR+L'SAWORKAR  POINT TO END OF WORK AREA +1
           sgr r4,r1              r4 holds true length of data
           b     MSKNUM            this is a direct branch into a later
*          structure - but to remove means a complete redesign
         endif
*
         BRU   RETURN
                        EJECT
***********************************************************************
*  CONVERT INPUT DATA TO STANDARD DATE/TIME FORMAT IF CONTENT NONZERO *
***********************************************************************
ALPHASRC lgh   R0,SAOUTCON        OUTPUT CONTENT SPECIFIED   ???
         if LTR,R0,R0,p
*
           MVI SASAVSGN,X'F0'     FORCE SIGN TO  BE UNSIGNED
           BRAS R9,CONCODE        YES - CONVERT  TO STANDARD FORMAT
           CLI SAOUTFMT+1,FC_ALNUM    OUTPUT FORMAT ALPHANUMERIC ??
           BRH NUMSLEN                NO  - PROCESS AS   NUMERIC
*
         else
           if CLI,SAOUTFMT+1,gt,fc_ALNUM OUTPUT FORMAT ALPHANUMERIC ??
*            process as numeric - so copy source to cleared workarea
             MVI SAWORKAR,C'0'    ZERO OUT WORK AREA
             MVC SAWORKAR+1(L'SAWORKAR-1),SAWORKAR
*
             lgr R15,R2            LOAD SOURCE  DATA  LENGTH
             bctgr R15,0           DECREMENT FOR "EX" INSTRUCTION
             LA R14,SAWORKAR+L'SAWORKAR RIGHTJUSTIFY COPIED NUMBER
             sgr R14,R2
             exrl R15,COPYJUST    COPY NUMERIC FIELD TO SAWORKAR
*
             lgr R1,R14            RESET SOURCE DATA  POINTER
*
             LA R14,0(R1,R2)      COMPUTE END OF DATA ADDRESS +1
             bctgr R14,0           BACKUP TO ADDRESS OF LAST DIGIT
             MVC SASAVSGN,0(R14)  SAVE SIGN
             NI SASAVSGN,X'F0'    ZERO  THE DIGIT POSITION LEAVING SIGN
             j NUMSLEN            and process as numeric
           endif
*
         endif
*
***********************************************************************
*  MASKED NUMERIC OUTPUT                                              *
***********************************************************************
MASKOUT  equ   *
***********************************************************************
*  Check for output MASK (valid for output MASKED NUMERIC)            *
***********************************************************************
         lgh   R7,SAMSKLEN        INITIALIZE  CURRENT MASK LENGTH
         if ltgr,R7,R7,p           is it positive?
           llgt  R6,SAMSKADR      INITIALIZE  CURRENT MASK POSITION
           LA  R14,0(R6,R7)       COMPUTE END-OF-MASK ADDRESS (+1)
           do from=(r7)
             bctgr R14,0          BACKUP TO LAST BYTE
             doexit CLI,0(R14),ne,C' ' SCAN BACKWARDS FOR  NON-BLANK
           enddo
           llgt  R3,TGTADDR        LOAD  OUPTUT AREA ADDRESS
           b   mskproc
         endif
***********************************************************************
*  ALPHANUMERIC OUTPUT                                                *
***********************************************************************
ALPHA    llgt  R3,TGTADDR         LOAD  OUPTUT AREA ADDRESS
*
         llgt  R14,LENADDR        LOAD ADDRESS OF   EDITED  LEN
         lgh   R0,0(,R14)         LOAD EDITED  LENGTH
*
         CLI   SAOUTJUS,C'L'      LEFT JUSTIFY ???
         BRE   ALPHALFT           YES - BYPASS BACKSCAN
*
         LA    R14,0(R1,R2)       Point to end of source
*
         do until=(cli,0(r14),ne,c' ') scan past the blanks
           bctgr R14,0
           doexit (clgrj,R14,le,R1) MINIMUM FIELD LENGTH FOR CENTERING?
         enddo

         LA    R14,1(,R14)        CORRECT DATA LENGTH
         sgr   R14,R1
         lgr   R15,R14            save data length for later
         sgr   R14,R0             R14 = EXCESS CHARACTERS IN SOURCE
         BRP   ALPHACUT           BRANCH IF SOURCE > TARGET
         BRZ   ALPHAMVC           BRANCH IF SOURCE = TARGET
*
         lcgr  R14,R14            load complement (num spaces before)
         if CLI,SAOUTJUS,ne,C'R'
           srlg R14,r14,1         find centre for justification = C
         endif
*
         lgr   r4,r14            save number of spaces
         MVI   0(R3),C' '       INSERT  LEADING SPACE
         if aghi,R14,-2,NM        COMPUTE ADD'L   SPACES NEEDED (-1)

           SRLG R0,R14,8
           AHI  R0,1
           SLLG R14,R14,56
           SRLG R14,R14,56
           BRU  ALPHAF_M1
ALPHAF_M0  MVC  1(256,R3),0(R3)     PROPAGATE 256 BYTES
           LA   R3,256(,R3)         ADVANCE target
ALPHAF_M1  BRCT R0,ALPHAF_M0
           EXRL R14,EXTEND         INITIALIZE TARGET AREA TO SPACES

*
         endif
         LA    R3,2(R3,R14)       ADJUST TARGET ADDR FOR  JUSTIFY
         lgr   R14,r4             restore saved number of spaces
*
         BRU   ALPHAMVC
*
ALPHALFT lgr   R15,R2             ASSUME SOURCE DATA WILL FIT
         lgr   R14,R2             R14 = EXCESS CHARACTERS IN SOURCE
         sgr   R14,R0
         BRNP  ALPHAMVC           BRANCH IF SOURCE <= TARGET
*
         CLI   SAVALFMT+1,FC_ALNUM SOURCE  DATA TYPE ALPHANUMERIC  ???
         BRE   ALPHACUT            YES - test for spaces
*                                 Otherwise test for leading zeros
* note - if the source is numeric then it's not gonna be > 256
         bctgr R14,0              reduce for EXRL
         exrl  R14,CHKLEADZ
         BRNE  ALPHACUT
*
         lgr   R2,R0              SKIP LEADING ZEROES
         LA    R1,1(R1,R14)
         sgr   R14,R14
         BRU   ALPHAMVC
*
* test for spaces (source alphanumeric so could be > 256)
*
ALPHACUT DS    0H
         bctgr R14,0              reduce length for EXRL
         lgr   R15,R1             Address source ...
         agr   R15,R0             .. skip to end that will be truncated
* test here if R14 is greater than 255 ...
         CHI   r14,255
         BRNH  ALPHACHKS
*
         SRLG  R0,R14,8
         AHI   R0,1
         SLLG  R14,R14,56
         SRLG  R14,R14,56
         BRU   ALPHAC_C1
ALPHAC_C0 CLC  0(0,R15),SPACES    ALL spaces
          BRNE ALPHATRUNC
         LA    R15,256(,R15)      ADVANCE target
ALPHAC_C1 BRCT R0,ALPHAC_C0
         EXRL  R15,MOVEIT         MOVE SOURCE TO OUTPUT  AREA
         LA    R3,1(R3,R15)       ADVANCE TO END OF DATA
*
ALPHACHKS DS   0H
         exrl  R14,CHKLEADS
         if    (NE)
           MVI SAFMTERR,EMTRUNC   INDICATE   TRUNCATION OCCURRED
         endif
*
ALPHATRUNC DS  0H
         MVI SAFMTERR,EMTRUNC     INDICATE   TRUNCATION OCCURRED
         llgt  R14,LENADDR
         lgh   R15,0(,R14)        Use column/target length

*        lg    R15,R0             USE  MAXIMUM  (COLUMN SIZE)
*
* Move source into target
*
ALPHAMVC bctgr R15,0              DECR data length FOR EXECUTE
*
         SRLG  R0,R15,8
         AHI   R0,1
         SLLG  R15,R15,56
         SRLG  R15,R15,56
         BRU   ALPHAM_M1
ALPHAM_M0 MVC  0(256,R3),0(R1)    PROPAGATE 256 BYTES
         LA    R3,256(,R3)        ADVANCE target
         LA    R1,256(,R1)        ADVANCE source
ALPHAM_M1 BRCT R0,ALPHAM_M0
         EXRL  R15,MOVEIT         MOVE SOURCE TO OUTPUT  AREA
         LA    R3,1(R3,R15)       ADVANCE TO END OF DATA
*
         lcgr  R14,R14            EXTENSION  NEEDED ???
         BRNP  ALPHAXIT
         MVI   0(R3),C' '         INSERT  TRAILING SPACE
         if aghi,R14,-2,NM         COMPUTE ADD'L   SPACES NEEDED (-1)
           SRLG R0,R14,8
           AHI  R0,1
           SLLG R14,R14,56
           SRLG R14,R14,56
           BRU ALPHAF_M3
ALPHAF_M2  MVC 1(256,R3),0(R3)     PROPAGATE 256 BYTES
           LA  R3,256(,R3)         ADVANCE target
ALPHAF_M3  BRCT R0,ALPHAF_M2
           EXRL R14,EXTEND         INITIALIZE TARGET AREA TO SPACES
         endif
         LA    R3,2(R3,R14)       ADVANCE TO END OF DATA
*
ALPHAXIT BRU   RETURN
                        EJECT
***********************************************************************
*  DATE MASK SPECIFICATION                                            *
***********************************************************************
*        ORG   *,32
         ago   .skipcode_01
DATE     ltgr  R7,R7              EDIT MASK SPECIFIED ???
         BRNP  ALPHA              NO  - TREAT AS NUMERIC
*
DATELOOP CLC   0(2,R6),=C'CC'     CENTURY REQUESTED ???
         BRE   CC
         CLC   0(2,R6),=C'YY'     YEAR    REQUESTED ???
         BRE   YY
         CLC   0(2,R6),=C'MM'     MONTH   REQUESTED ???
         BRE   MN
         CLC   0(2,R6),=C'DD'     DAY     REQUESTED ???
         BRE   DD
*
         MVC   0(1,R3),0(R6)      COPY BYTE FROM MASK
         aghi  R3,1               ADVANCE OUTPUT POSITION
         aghi  R6,1               ADVANCE MASK   POSITION
         BRCT  R7,DATELOOP        LOOP UNTIL MASK EXHAUSTED
*
         BRU   RETURN
                        EJECT
CC       MVC   0(2,R3),0(R1)      COPY CENTURY
         aghi  R3,2               ADVANCE OUTPUT POSITION
         aghi  R6,2               ADVANCE MASK   POSITION
         aghi  R7,-2              DECREMENT REMAINING MASK LENGTH
         BRP   DATELOOP           LOOP UNTIL MASK EXHAUSTED
*
         BRU   RETURN             RETURN
                        SPACE 3
YY       MVC   0(2,R3),2(R1)      COPY YEAR
         aghi  R3,2               ADVANCE OUTPUT POSITION
         aghi  R6,2               ADVANCE MASK   POSITION
         aghi  R7,-2              DECREMENT REMAINING MASK LENGTH
         BRP   DATELOOP           LOOP UNTIL MASK EXHAUSTED
*
         BRU   RETURN             RETURN
                        SPACE 3
MN       MVC   0(2,R3),4(R1)      COPY MONTH
         aghi  R3,2               ADVANCE OUTPUT POSITION
         aghi  R6,2               ADVANCE MASK   POSITION
         aghi  R7,-2              DECREMENT REMAINING MASK LENGTH
         BRP   DATELOOP           LOOP UNTIL MASK EXHAUSTED
*
         BRU   RETURN             RETURN
                        SPACE 3
DD       MVC   0(2,R3),6(R1)      COPY DAY
         aghi  R3,2               ADVANCE OUTPUT POSITION
         aghi  R6,2               ADVANCE MASK   POSITION
         aghi  R7,-2              DECREMENT REMAINING MASK LENGTH
         BRP   DATELOOP           LOOP UNTIL MASK EXHAUSTED
*
         BRU   RETURN             RETURN
                        EJECT
***********************************************************************
*  TIME MASK SPECIFICATION                                            *
***********************************************************************
*        ORG   *,32
TIME     ltgr  R7,R7              EDIT MASK SPECIFIED ???
         BRNP  ALPHA              NO - TREAT  AS  NUMERIC
*
TIMELOOP CLC   0(2,R6),=C'HH'     HOURS     REQUESTED ???
         BRE   HH
         CLC   0(2,R6),=C'MM'     MINUTES   REQUESTED ???
         BRE   MM
         CLC   0(2,R6),=C'SS'     SECONDS   REQUESTED ???
         BRE   SS
*
         MVC   0(1,R3),0(R6)      COPY BYTE FROM MASK
         aghi  R3,1               ADVANCE OUTPUT POSITION
         aghi  R6,1               ADVANCE MASK   POSITION
         BRCT  R7,TIMELOOP        LOOP UNTIL MASK EXHAUSTED
*
         BRU   RETURN
                        SPACE 3
HH       MVC   0(2,R3),0(R1)      COPY HOURS
         aghi  R3,2               ADVANCE OUTPUT POSITION
         aghi  R6,2               ADVANCE MASK   POSITION
         aghi  R7,-2              DECREMENT REMAINING MASK LENGTH
         BRP   TIMELOOP           LOOP UNTIL MASK EXHAUSTED
*
         BRU   RETURN             RETURN
                        SPACE 3
MM       MVC   0(2,R3),2(R1)      COPY MINUTES
         aghi  R3,2               ADVANCE OUTPUT POSITION
         aghi  R6,2               ADVANCE MASK   POSITION
         aghi  R7,-2              DECREMENT REMAINING MASK LENGTH
         BRP   TIMELOOP           LOOP UNTIL MASK EXHAUSTED
*
         BRU   RETURN             RETURN
                        SPACE 3
SS       MVC   0(2,R3),4(R1)      COPY SECONDS
         aghi  R3,2               ADVANCE OUTPUT POSITION
         aghi  R6,2               ADVANCE MASK   POSITION
         aghi  R7,-2              DECREMENT REMAINING MASK LENGTH
         BRP   TIMELOOP           LOOP UNTIL MASK EXHAUSTED
*
         BRU   RETURN             RETURN
.skipcode_01 anop
                        EJECT
***********************************************************************
*  SCAN SOURCE STRING FOR INVALID NUMERIC DATA CHARACTERS             *
***********************************************************************
*        ORG   *,32
MASK     llgt  R3,TGTADDR         LOAD OUTPUT AREA ADDRESS
*
         llgt  R14,LENADDR        LOAD EDITED LENGTH
         lgh   R14,0(,R14)
         if sgr,R14,R7,P          COMPUTE RIGHT ADJUSTMENT LENGTH
           agr R3,R14             RIGHT  ADJUST LENGTH
         endif
*
         MVI   SAHADSIG,C'N'      SET NO SIGNIFICANCE
         MVI   SAHADDEC,C'N'      SET NO DECIMAL POINT
*
         CLI   SASAVSGN,X'F0'     VALID  SIGN ???
         BRE   MSKCHKDG           YES - CHECK FOR PACKED FORMAT
         CLI   SASAVSGN,X'C0'     VALID  SIGN ???
         BRE   MSKCHKDG           YES - CHECK FOR PACKED FORMAT
         CLI   SASAVSGN,X'D0'     VALID  SIGN ???
         BRNE  SENDJUNK           NO  - BAD  SIGN TREAT AS JUNK
                        SPACE 3
MSKCHKDG lgr   R0,R2              LOAD SOURCE LENGTH
         lgr   R14,R1             LOAD SOURCE START
NUMDIG   CLI   0(R14),C'0'        VALID DIGIT ???
         BRL   NUMBAD             NO -
         CLI   0(R14),C'9'
         BRH   NUMBAD
         aghi  R14,1
         BRCT  R0,NUMDIG          LOOP THROUGH SOURCE
         BRU   MSKINIT            CONTINUE  IF ALL  NUMERIC
*
NUMBAD   ds    0h
         if clij,R0,eq,1,and,     LAST BYTE ???                        +
               CLI,0(R14),ge,X'C0',and,   VALID SIGN ???               +
               CLI,0(R14),le,X'D9'
           OI  0(R14),X'F0'       FORCE A PRINTABLE DIGIT
         else
SENDJUNK   bctgr R7,0             DECREMENT MASK LENGTH FOR "EX"
           exrl R7,BLANKOUT       SEND BLANKS
           LA  R3,1(R3,R7)        ADVANCE OUTPUT POSITION
*
           MVI SAFMTERR,EMBAD     INDICATE BAD FIELD DATA
           BRU RETURN
         endif
                        EJECT
***********************************************************************
*  SCAN OUTPUT MASK FOR ATTRIBUTES                                    *
***********************************************************************
MSKINIT  equ   *
         xgr   r4,r4              ZERO MASK DIGIT   COUNTER
         xgr   r15,r15            ZERO MASK DECIMAL COUNTER
         lgr   R0,R7              LOAD MASK LENGTH (LOOP COUNTER)
         lgr   R14,R6             LOAD MASK START
*
MSKCNTLP CLI   0(R14),C'Z'        DIGIT POSITION  ???
         BRE   MSKINCCT           YES - INCREMENT COUNTER
         CLI   0(R14),C'9'        DIGIT POSITION  ???
         BRE   MSKINCCT           YES - INCREMENT COUNTER
         CLI   0(R14),C'_'        DIGIT POSITION  ???
         BRE   MSKINCCT           YES - INCREMENT COUNTER
         if CLI,0(R14),eq,C'.'        DECIMAL POINT   ???
           MVI SAHADDEC,C'Y'      SET DECIMAL POINT FOUND  FLAG
         endif
         aghi  R14,1              INCREMENT MASK POSITION
         BRCT  R0,MSKCNTLP        LOOP THROUGH MASK
         BRU   MSKTRUNC           BRANCH TO TRUNCATION LOGIC
*
MSKINCCT aghi  r4,1               INCREMENT DIGIT   COUNTER
         if CLI,SAHADDEC,eq,C'Y'  DECIMAL   POINT   FOUND ???
           aghi r15,1             INCREMENT DECIMAL COUNTER
         endif
         aghi  R14,1              INCREMENT MASK POSITION
         BRCT  R0,MSKCNTLP        LOOP THROUGH MASK
*
MSKTRUNC EQU   *
         sth   r4,SAMSKDIG        save digit count
         sth   r15,SAMSKDEC       save decimal count
*
         lgr   R15,R2             LOAD SOURCE LENGTH
         lh    r0,savaldec
         sgfr  R15,R0             CALC NO. OF SIGNIFICANT SRC  DIGITS
         BRM   MSKERR01           ERROR IF DECIMALS EXCEEDS LENGTH
*
         lh    r14,samskdig       get mask digits
         sh    r14,samskdec       calc no. of significant mask digits
*
         sgfr  R15,R14            CALC LEADING TRUNCATION LENGTH
         BRNP  MSKLENOK           BRANCH IF NO TRUNCATION
*
         LR    R14,R15            DECREMENT LENGTH FOR "EX"
         bctgr R14,0
         exrl  R14,TRUNZERO
         BRE   MSKLENOK
         MVI   SAFMTERR,EMTRUNC   INDICATE TRUNCATION OCCURRED
*
MSKLENOK agr   R1,R15             TRUNCATE LEADING  DIGITS
         sgr   R2,R15
*
         lh    r0,SAMSKDEC        LOAD NO. OF MASK   DECIMALS
         lh    r4,SAVALDEC
         sr    r0,r4              SUBTRACT    SOURCE DECIMALS
         sth   r0,sadblwrk
         BRM   ROUND              ROUND IF SOURCE EXCEEDS MASK
         BRZ   MSKPROC            BYPASS ROUNDING IF UNNECESSARY
*
         agfr  R2,R0
         BRU   MSKPROC
*
         DS   0D
TRUNZERO CLC   0(0,R1),ZEROES     * * * *   E X E C U T E D   * * * *
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*  ROUND THE SOURCE FIELD TO RIGHTMOST MASK DIGIT                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*        ORG   *,32
ROUND    lh    R15,SADBLWRK       CONVERT EXCESS SOURCE LEN TO POS BIN
         lcgr  R15,R15
         bctgr R15,0              DECREMENT LENGTH FOR "EX" (L2)
         exrl  R15,PACKRND        PACK ROUNDING CONSTANT into SADBLWRK
*
         lgr   R15,R2             LOAD SOURCE LENGTH
         if cgij,R15,gt,MAXDIG    MAXIMUM NO. OF DIGITS EXCEEDED ???
           MVI SAFMTERR,EMMAXDIG  MAXIMUM EXCEEDED
           BRU Ret_err            RETURN WITH ERROR
         endif

         STH   R2,SAORGLEN        SAVE  ORIGINAL   LENGTH
         ZAP   SAPACKAR,P000      RESET TARGET     AREA
         BRAS  R9,PACKLONG        PACK  THE SOURCE DIGITS
*
         AP    SAPACKAR,SADBLWRK  ROUND THE ORIGINAL SOURCE VALUE
*
         LA    R1,SAPACKAR        POINT TO ROUNDED VALUE
         lghi  R2,L'SAPACKAR      LOAD  LENGTH  OF VALUE
         BRAS  R9,UNPKLONG        UNPACK ROUNDED NUMBER
         LA    R15,0(R1,R2)       LOAD ADDRESS OF LAST BYTE +1
         bctgr R15,0              BACKUP TO  LAST BYTE
         OI    0(R15),X'F0'       FORCE A VALID ZONE
*
         lgh   R2,SAORGLEN        LOAD ORIGINAL LENGTH
         LA    R1,SAWORKAR+L'SAWORKAR  POINT TO END OF WORK AREA +1
         sgr   R1,R2                   RESET R1 TO  NEEDED  BEGINNING
                        EJECT
***********************************************************************
*  PROCESS OUTPUT MASK CHARACTER BY CHARACTER                        *
***********************************************************************
MSKPROC  lgr   R0,R3              SAVE THE OUTPUT AREA BEGINNING ADDR
         la    R4,SAWORKAR+L'SAWORKAR  POINT TO END OF WORK AREA +1
         sgr   r4,r1              r4 holds true length of data
*
         do ,
           if CLI,SAOUTJUS,ne,C'L' Not LEFT JUSTIFY ???
*
             llgt R14,LENADDR    LOAD ADDRESS OF  EDITED LEN
             lgh R15,0(,R14)     LOAD EDITED  LENGTH              @01C
*
             lgr R14,R7            LOAD MASK    LENGTH
             sgr R14,R15         R14 = EXCESS CHARACTERS IN SOURCE@01C
             doexit (NM)         BRANCH IF SOURCE > TARGET
*
             lcgr R14,R14
             if CLI,SAOUTJUS,ne,c'R'
               srlg R14,r14,1
             endif
*
             MVI 0(R3),C' '       INSERT  LEADING SPACE
             if aghi,R14,-2,nm     COMPUTE ADD'L   SPACES NEEDED (-1)
               exrl R14,EXTEND
             endif
             aghi R14,2
*
             LA R3,0(R3,R14)      ADJUST TARGET ADDR FOR  JUSTIFY
           endif
         enddo
*
         if CLI,SAVALFMT+1,eq,FC_ALNUM
**********************************************************************
*      PROCESS ALPHABETIC MASK CHARACTER BY CHARACTER                 *
***********************************************************************
alphaloop  do from=(r7)            loop until mask exhausted
             if CLI,0(R6),eq,C'X' SELECT CHARACTER ???
*
               doexit (LTR,R2,R2,np)                                   +
                                     ANY DATA LEFT, split if not
               MVC 0(1,R3),0(R1) MOVE SOURCE BYTE TO OUTPUT AREA
               aghi R1,1           ADVANCE   SOURCE AREA ADDRESS
               BCTgR R2,0          DECREMENT SOURCE AREA LENGTH
             else
               MVC 0(1,R3),0(R6) COPY CHARACTER FROM MASK TO OUTPUT
             endif
             aghi r3,1             Increment the pointers along
             aghi r6,1
           enddo ,   (alphaloop)  (thus is counting on r7)
*
         else ,                    not an alpha mask
msknum     ds 0h
mskloop    do from=(r7)            loop until mask exhausted
             do ,
               if CLI,0(R6),eq,C'9'     SELECT CHAR ???
                 MVI SAHADSIG,C'Y' SET HAVE SIGNIFICANCE
               else ,
                 if cli,0(r6),eq,c'Z' zero suppress char?
                   if cli,0(r1),ne,c'0'  and we do not have a zero
                     MVI SAHADSIG,C'Y' SET HAVE SIGNIFICANCE
                   else
                     if cli,sahadsig,ne,c'Y'  no significance
                       mvi 0(r1),c' ' make the zero a blank
                     endif
                   endif
                 else
*                  mask is not a 9 or Z
                   if CLI,0(R6),eq,C'-',or,   SIGN ???                 +
               CLI,0(R6),eq,C'(',or,      or  left paren               +
               CLI,0(R6),eq,C')'          or right paren
                     if TM,SASAVSGN,X'30',M          and negtive value
                       mvc 0(1,R3),0(r6)  set mask char in output area
                     else ,
                       MVI 0(R3),C' '         set a blank in output
                     endif
                   else ,
*                    mask is not 9,Z,'-' or '(' nor ')'
                     if cli,0(r6),eq,c'.',or,   decimal point?         +
               cli,sahadsig,eq,c'Y'          or significance set
                       MVI SAHADSIG,C'Y' SET HAVE SIGNIFICANCE
                       mvc 0(1,R3),0(r6)  set mask char in output area
                     else ,
                       if cli,sahadsig,eq,c'N',and,  significance off? +
               CRj,r3,gt,R0                     past the first byte ??
                         lgr R14,R3 BACKUP TO PREVIOUS CHARACTER
                         BCTgR R14,0
                         if CLI,0(R14),eq,C'-',or, MINUS SIGN ???      +
               CLI,0(R14),eq,C'('    left paren
                           if clij,r7,ne,1 not the last char
                             MVC 0(1,R3),0(R14) SHIFT character RIGHT
                           endif
                           MVI 0(R14),C' ' BLANK OUT previous
                         endif
                       endif
                     endif
                   endif
                   leave , now so we leave source data alone
                 endif
               endif
               doexit (LTgR,R2,R2,np),do=mskloop                       +
                                     ANY DATA LEFT, split if not
               if ltgr,r4,r4,p        R4 positive - so move is valid
                 MVC 0(1,R3),0(R1)     MOVE SOURCE BYTE TO OUTPUT AREA
                 aghi R1,1              ADVANCE   SOURCE AREA ADDRESS
                 bctgr r4,0          and reduce available source length
               else
                 mvi 0(r3),C'0'      pad with zero
               endif

               if aghi,r2,-1,np,and, DECREMENT SOURCE LEN, set cc, test+
               TM,SASAVSGN,X'30',NM  and we have a positive value
                 MVI SAHADSIG,C'N' then turn this off
               endif
               if cli,sahadsig,eq,c'N',and,          significance off? +
               cgr,r3,gt,R0                     past the first byte ??
                 lgr R14,R3        BACKUP TO PREVIOUS CHARACTER
                 bctgr R14,0
                 if CLI,0(R14),eq,C'-',or, MINUS SIGN ???              +
               CLI,0(R14),eq,C'('            left paren
                   if    clij,r7,ne,1    not the last char
                     MVC 0(1,R3),0(R14) SHIFT character RIGHT
                   endif
                   MVI 0(R14),C' ' BLANK OUT previous
                 endif
               endif
             enddo
             aghi R3,1             ADVANCE OUTPUT  POINTER
             aghi R6,1             ADVANCE PICTURE POINTER
           enddo ,  (mskloop end) bct loop on r7
         endif
*
RETURN   ds    0h                 SAFMTERR will be 0 if no error
*                                 Possibly an error code set
         LLgc  R14,SAFMTERR       e.g. truncation occured
*
SETRCF   llgt  R15,RTNADDR        LOAD RETURN CODE ADDRESS
         STH   R14,0(,R15)        SET  RETURN CODE
         lgr   R15,R14
*
         llgt  R14,TGTADDR        LOAD  ORIGINAL TARGET ADDRESS
         sgr   R3,R14             COMPUTE EDITED LENGTH
         llgt  R14,LENADDR        LOAD  LENGTH  ADDRESS
         STH   R3,0(,R14)         PASS  EDITED   LENGTH BACK TO CALLER
*
         lg    R14,SAVF4SAG64RS14 RESTORE REGISTERS
         lmg   R0,R12,SAVF4SAG64RS0
         bsm   0,r14              RETURN
*
* Return error
*
Ret_err  equ   *
*                                 must point r3 to end of the output
         llgt  R3,TGTADDR         address of output area
         llgt  R15,LENADDR        Address of output len
         lgh   R0,0(,R15)
         ar    r3,r0              point to end of output
         b     return             Go set the return code from safmterr
*
MSKERR01 MVI   SAFMTERR,EMSRCDEC  SOURCE DEC LENGTH EXCEEDS FIELD LEN
         BRU   Ret_err            RETURN WITH ERROR
COLERR01 MVI   SAFMTERR,EMOUTNUM  COLUMN SIZE TOO LARGE FOR NUMERIC
         BRU   Ret_err            RETURN WITH ERROR
                        SPACE 3
         DS   0D
EXTEND   MVC   1(0,R3),0(R3)        *** EXECUTED ***
         DS   0D
MOVEIT   MVC   0(0,R3),0(R1)        *** EXECUTED ***
         DS   0D
COPYJUST MVC   0(0,R14),0(R1)       *** EXECUTED ***
         DS   0D
PACKRND  PACK  SADBLWRK,ROUNDCON(0) *** EXECUTED ***
         DS   0D
BLANKOUT MVC   0(0,R3),SPACES       *** EXECUTED ***
                        SPACE 3
         DS   0D
pack_farleft PACk sapackar+0(l'sapackar-14),0(0,r1)
         DS   0D
PACKLEFT PACK  SAPACKAR+0(L'SAPACKAR-7),0(0,R1)  *** EXECUTED ***
         DS   0D
PACKRGHT PACK  SAPACKAR+8(08),0(0,R1)            *** EXECUTED ***
                        SPACE 3
         DS   0D
unpk_farleft unpk saworkar+1(l'saworkar-29),0(0,r1)
         DS   0D
UNPKLEFT UNPK  SAWORKAR+02(L'SAWORKAR-16),0(0,R1) *** EXECUTED ***
         DS   0D
UNPKRGHT UNPK  SAWORKAR+17(15),0(0,R1)            *** EXECUTED ***
*
         DS   0D
CONCSAVE MVC   sadatime(0),0(R1)  * * * * E X E C U T E D * * * *
         DS   0D
MVCMONTH MVC   0(0,R2),0(R15)     * * * * E X E C U T E D * * * *
                        EJECT
static   loctr
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*  C O N S T A N T S                                                  *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
F32K     EQU     32767
F32KM    equ     -32767
MAXDIG   equ     30
P000     DC    P'0'
P001     DC    P'1'
ROUNDCON DC    CL32'50000000000000000000000000000000'
ZEROES   DC  32C'0'
HEXFF    DC  32X'FF'
LOWVAL   DC    X'00'
*
C00      DC    CL4'0000'
C01      DC    CL2'01'
C12      DC    CL2'12'
C19      DC    CL2'19'
C20      DC    CL2'20'
C50      DC    CL2'50'
C99      DC    CL2'99'
AM       DC    CL2'AM'
PM       DC    CL2'PM'
*
CSVMASK  DC    CL48'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.99999999'
SPACES   DC    CL256' '
*
CONSUBTA DC    A(CONSUBTB)        CONTENT CODE BRANCH TABLE ADDRESS
CONSMAPA DC    A(CONSRCMP)        SOURCE MAP TABLE ADDRESS
CONOMAPA DC    A(CONOUTMP)        OUTPUT MAP TABLE ADDRESS
CONOLENA DC    A(CONOUTLN)        OUTPUT LENGTH
*
DIMTBL   DC    X'1F1C1F1E1F1E1F1F1E1F1E1F'    DAYS IN MONTH
CUMMONTH DC    H'000'
         DC    H'000',H'031',H'059',H'090',H'120',H'151'
         DC    H'181',H'212',H'243',H'273',H'304',H'334',H'365'
*
MONTHS   DC    AL2(JAN-MONTHS)
         DC    AL2(L'JAN-1)
         DC    AL2(FEB-MONTHS)
         DC    AL2(L'FEB-1)
         DC    AL2(MAR-MONTHS)
         DC    AL2(L'MAR-1)
         DC    AL2(APR-MONTHS)
         DC    AL2(L'APR-1)
         DC    AL2(MAY-MONTHS)
         DC    AL2(L'MAY-1)
         DC    AL2(JUN-MONTHS)
         DC    AL2(L'JUN-1)
         DC    AL2(JUL-MONTHS)
         DC    AL2(L'JUL-1)
         DC    AL2(AUG-MONTHS)
         DC    AL2(L'AUG-1)
         DC    AL2(SEP-MONTHS)
         DC    AL2(L'SEP-1)
         DC    AL2(OCT-MONTHS)
         DC    AL2(L'OCT-1)
         DC    AL2(NOV-MONTHS)
         DC    AL2(L'NOV-1)
         DC    AL2(DEC-MONTHS)
         DC    AL2(L'DEC-1)
*
JAN      DC    C'JANUARY'
FEB      DC    C'FEBRUARY'
MAR      DC    C'MARCH'
APR      DC    C'APRIL'
MAY      DC    C'MAY'
JUN      DC    C'JUNE'
JUL      DC    C'JULY'
AUG      DC    C'AUGUST'
SEP      DC    C'SEPTEMBER'
OCT      DC    C'OCTOBER'
NOV      DC    C'NOVEMBER'
DEC      DC    C'DECEMBER'
*
* The error code equates are in DL96EQU (copy here for easy reference)
*
*EMBAD    EQU   01  'INVALID DATA IN SOURCE FIELD
*EMTRUNC  EQU   02  'DATA TRUNCATED TO FIT WITHIN FIELD'
**MFUNC   EQU   03  'INVALID FUNCTION CODE'
*EMOUTNUM EQU   04  'INVALID MAXIMUM OUTPUT AREA LENGTH'
*EMOUTFUL EQU   05  'OUTPUT AREA FULL (OVERFLOW)'
*EMOCCURS EQU   06  'INVALID OCCURENCE COUNT'
*EMFORMAT EQU   07  'INVALID FORMAT CODE'
*EMSRCLEN EQU   08  'SOURCE LENGTH IS NEGATIVE OR TOO LONG'
**MRPTLEN EQU   09  'COLUMN WIDTH IS NEGATIVE OR TOO LONG'
*EMMSKLEN EQU   10  'MASK LENGTH EXCEEDS COLUMN WIDTH'
*EMMSKSRC EQU   11  'MASK LENGTH AND SOURCE LENGTH INCONSISTENT'
*EMHDRLEN EQU   12  'HEADER LENGTH EXCEEDS COLUMN WIDTH'
*EMSRCDEC EQU   13  'SOURCE DECIMALS INCONSISTENT WITH SOURCE LENGTH'
*EMMSKDIG EQU   14  'MASK DIGITS EXCEED SOURCE DIGITS'
*EMMSKDEC EQU   15  'MASK DECIMALS EXCEED SOURCE DECIMALS'
*EMMAXDIG EQU   16  'MAXIMUM NUMERIC LENGTH EXCEEDED FOR ROUNDING'
*EMFLDDEC EQU   17  'FIELD DECIMALS BUT NO MASK'
*EMFLDNEG EQU   18  'FIELD NEGATIVE BUT NO MASK'
*EMDATEIC EQU   19  'DATE FIELDS INCOMPATIBLE'
         COPY DL96EQU
         LTORG
                        EJECT
code     loctr
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*        CONVERT SOURCE CONTENT TO SPECIFIED OUTPUT CONTENT           *
*                                                                     *
*        REGISTER USAGE:                                              *
*                                                                     *
*              R0  - WORK  REGISTER                                   *
*              R1  - SOURCE/OUTPUT  ADDRESS                           *
*                  - SOURCE CONTENT MAP ELEMENT ADDRESS               *
*              R2  - SOURCE/OUTPUT  LENGTH                            *
*                  - CURRENT OUTPUT POSITION                          *
*              R9  - RETURN ADDRESS                                   *
*              R14 - OUTPUT CONTENT MAP ELEMENT ADDRESS               *
*              R15 - WORK  REGISTER                                   *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
CONCODE  lgh   R0,SAVALCON        LOAD SOURCE CONTENT FORMAT
         ltgr  R0,R0
         BRNP  CONCEXIT
*
         bctgr R2,0               SAVE CURRENT SOURCE
         exrl  R2,CONCSAVE
*
         MVI   SAWORKAR+0,C'0'    CLEAR OUTPUT DATE   AREA
         MVC   SAWORKAR+1(L'SADAtime-1),SAWORKAR+0
*
         BCTR  R0,0               COMPUTE OFFSET WITHIN SOURCE MAP TBL
         lghi  R1,L'CONSRCMP
         MR    R0,R0
         agf   R1,CONSMAPA
*
         LA    R2,SAWORKAR        INITIALIZE OUTPUT ADDRESS
*
         lgh   R14,SAOUTCON       COMPUTE OFFSET TO OUTPUT TEMPLATE
         bctgr R14,0
         lghi  R15,L'CONOUTMP
         MR    R14,R14
         llgt  R14,CONOMAPA
         agr   R14,R15
*
CONCLOOP CLI   0(R14),X'FF'       END-OF-MAP  ???
         BRE   CONCRSET           YES - RETURN
*
         lgh   R15,0(,R14)        LOAD  SUBFIELD CODE
         sllg  R15,r15,2
         agf   R15,CONSUBTA
         BR    R15
*
CONSUBTB BRU   CONC_00            CC
         BRU   CONC_01            YY
         BRU   CONC_02            MM
         BRU   CONC_03            DD
         BRU   CONC_04            DDD
         BRU   CONC_05            HH
         BRU   CONC_06            MM
         BRU   CONC_07            SS
         BRU   CONC_08            TT
         BRU   CONC_09            NNNN
         BRU   CONC_10            "/"
         BRU   CONC_11            ":"
         BRU   CONC_12            " "
         BRU   CONC_13            MMM
         BRU   CONC_14            AM/PM
         BRU   CONC_15            "-"
         BRU   CONC_16            "."
         BRU   CONC_17            MMMMMMMMM
         BRU   CONC_18            ","
*
CONCNEXT EQU   *
         aghi  R14,2              ADVANCE  TO NEXT OUTPUT  MAP ELEMENT
         aghi  R2,2               ADVANCE  OUTPUT  ADDRESS
         BRU   CONCLOOP           LOOP THROUGH MAP
*
***********************************************************************
*  CENTURY ("CC")                                                     *
***********************************************************************
CONC_00  lgh   R15,00(,R1)        LOAD  "CC" OFFSET
         ltgr  R15,R15            "CC"  AVAILABLE    ???
         BRNM  CONC_00C           YES - COPY IT
*
         lgh   R15,02(,R1)        LOAD  "YY" OFFSET
         ltgr  R15,R15            "YY"  AVAILABLE    ???
         BRNM  CONC_00A           YES - SET  CENTURY BASED ON YEAR
         MVC   0(2,R2),C20        NO  - DEFAULT "CC"=20
         BRU   CONCNEXT
*
CONC_00A LA    R15,sadatime(R15)
         if CLC,0(2,R15),gt,C50   YEAR  >  '50' ???
           MVC   0(2,R2),C19      SET  "CC"  TO  '19'
         else
           MVC   0(2,R2),C20      SET  "CC"  TO  '20'
         endif
         BRU   CONCNEXT
*
CONC_00C LA    R15,sadatime(R15)  COPY "CC" FROM SOURCE TO OUTPUT
         MVC   0(2,R2),0(R15)
         BRU   CONCNEXT
*
***********************************************************************
*  YEAR ("YY")                                                        *
***********************************************************************
CONC_01  lgh   R15,02(,R1)        LOAD  "YY" OFFSET
         ltgr  R15,R15            "YY"  AVAILABLE    ???
         BRNM  CONC_01A           YES - COPY IT
         MVC   0(2,R2),C99        NO  - DEFAULT "YY"=99
         BRU   CONCNEXT
*
CONC_01A LA    R15,sadatime(R15)  COPY "YY" FROM SOURCE TO OUTPUT
         MVC   0(2,R2),0(R15)
         BRU   CONCNEXT
*
***********************************************************************
*  MONTH ("MM")                                                       *
***********************************************************************
CONC_02  lgh   R15,04(,R1)        LOAD  "MM" OFFSET
         ltgr  R15,R15            "MM"  AVAILABLE    ???
         BRNM  CONC_02B           YES - COPY IT
*
         lgh   R15,08(,R1)        LOAD "DDD" OFFSET
         ltgr  R15,R15            AVAILABLE  ???
         BRNM  CONC_02A           YES - DERIVE  "MM" FROM "DDD"
*
         MVC   0(2,R2),C01        NO  - DEFAULT "MM"=01
         BRU   CONCNEXT
*
CONC_02A lgr   R3,R9              SAVE  RETURN   ADDRESS
         BRAS  R9,CONJDATE        BUILD "YY/MM"  FROM  "DDD"
         lgr   R9,R3
*
         CVD   R15,SADBLWRK       CONVERT MONTH TO DISPLAY FORMAT
         OI    SADBLWRK+L'SADBLWRK-1,X'0F'
         UNPK  0(2,R2),SADBLWRK
         BRU   CONCNEXT
*
CONC_02B LA    R15,sadatime(R15)  COPY "MM" FROM SOURCE TO OUTPUT
         MVC   0(2,R2),0(R15)
         BRU   CONCNEXT
*
***********************************************************************
*  DAY ("DD")                                                         *
***********************************************************************
CONC_03  lgh   R15,06(,R1)        LOAD  "DD" OFFSET
         ltgr  R15,R15            "DD"  AVAILABLE    ???
         BRNM  CONC_03B           YES - COPY IT
*
         lgh   R15,08(,R1)        LOAD "DDD" OFFSET
         ltgr  R15,R15            AVAILABLE  ???
         BRNM  CONC_03A           YES - DERIVE  "DD" FROM "DDD"
*
         MVC   0(2,R2),C01        NO  - DEFAULT "DD"=01
         BRU   CONCNEXT
*
CONC_03A lgr   R3,R9              SAVE   RETURN  ADDRESS
         BRAS  R9,CONJDATE        BUILD  "YY/MM" FROM   "DDD"
         lgr   R9,R3
*
         CVD   R5,SADBLWRK        CONVERT DAY TO DISPLAY FORMAT
         OI    SADBLWRK+L'SADBLWRK-1,X'0F'
         UNPK  0(2,R2),SADBLWRK
         BRU   CONCNEXT
*
CONC_03B LA    R15,sadatime(R15)  COPY "MM" FROM SOURCE  TO OUTPUT
         MVC   0(2,R2),0(R15)
         BRU   CONCNEXT
*
***********************************************************************
*  JULIAN DAY (DDD)                                                   *
***********************************************************************
CONC_04  lgh   R15,08(,R1)        LOAD "DDD" OFFSET
         ltgr  R15,R15            "DDD" AVAILABLE    ???
         BRNM  CONC_04C           YES - COPY IT
*
         lgh   R15,04(,R1)        LOAD "MM"  OFFSET
         ltgr  R15,R15            "MM"  AVAILABLE    ???
         BRM   CONC_04B           NO  - USE  DEFAULT
         LA    R15,sadatime(R15)
         PACK  SADBLWRK,0(2,R15)
         CVB   R4,SADBLWRK
*
         lgh   R15,06(,R1)        LOAD "DD"  OFFSET
         ltgr  R15,R15            "DD"  AVAILABLE    ???
         BRM   CONC_04B           NO  - USE  DEFAULT
         LA    R15,sadatime(R15)
         PACK  SADBLWRK,0(2,R15)
         CVB   R5,SADBLWRK
*
         lgr   R15,R4             ADD   CUMMULATIVE  DAYS TIL MONTH
         sllg  R15,r15,1
         lgh   R0,CUMMONTH(R15)
         agr   R5,r0
*
         lgh   R15,02(,R1)        LOAD "YY"  OFFSET
         ltgr  R15,R15
         BRM   CONC_04B
         LA    R15,sadatime(R15)  LOAD "YY"  ADDRESS
         PACK  SADBLWRK,0(2,R15)
         CVB   R0,SADBLWRK
         ST    R0,SADBLWRK        SAVE  THE  BINARY VALUE FOR "YY"
*
         sgr   R15,R15            ASSUME NOT LEAP   YEAR
         TM    SADBLWRK+3,X'03'   YEAR   DIVISIBLE BY FOUR ???
         BRNZ  CONC_04A
         lghi  R15,1              NO  - R4=1
*
CONC_04A ds    0h
         if cgij,R4,ge,03         ADD LEAP YEAR ADJUSTMENT IF ANY
           agr R5,R15
         endif
*
         CVD   R5,SADBLWRK
         OI    SADBLWRK+L'SADBLWRK-1,X'0F'
         UNPK  0(3,R2),SADBLWRK
         BRU   CONC_04D
*
CONC_04B equ   *
         MVC   0(3,R2),C00        NO  - DEFAULT "DDD"=00
         BRU   CONC_04D
*
CONC_04C LA    R15,sadatime(R15)  COPY "DDD" FROM SOURCE TO OUTPUT
         MVC   0(3,R2),0(R15)
*
CONC_04D aghi  R14,2
         aghi  R2,3
         BRU   CONCLOOP
*
***********************************************************************
*  HOUR ("HH")                                                        *
***********************************************************************
CONC_05  lgh   R15,10(,R1)        LOAD  "HH" OFFSET
         ltgr  R15,R15            "HH"  AVAILABLE    ???
         BRNM  CONC_05A           YES - COPY IT
         MVC   0(2,R2),C00        NO  - DEFAULT "HH"=00
         BRU   CONCNEXT
*
CONC_05A LA    R15,sadatime(R15)  COPY "HH" FROM SOURCE TO OUTPUT
         MVC   0(2,R2),0(R15)
         BRU   CONCNEXT
*
***********************************************************************
*  MINUTE ("MM")                                                      *
***********************************************************************
CONC_06  lgh   R15,12(,R1)        LOAD  "HH" OFFSET
         ltgr  R15,R15            "HH"  AVAILABLE    ???
         BRNM  CONC_06A           YES - COPY IT
         MVC   0(2,R2),C00        NO  - DEFAULT "HH"=00
         BRU   CONCNEXT
*
CONC_06A LA    R15,sadatime(R15)  COPY "HH" FROM SOURCE TO OUTPUT
         MVC   0(2,R2),0(R15)
         BRU   CONCNEXT
*
***********************************************************************
*  SECONDS ("SS")                                                     *
***********************************************************************
CONC_07  lgh   R15,14(,R1)        LOAD  "HH" OFFSET
         ltgr  R15,R15            "HH"  AVAILABLE    ???
         BRNM  CONC_07A           YES - COPY IT
         MVC   0(2,R2),C00        NO  - DEFAULT "HH"=00
         BRU   CONCNEXT
*
CONC_07A LA    R15,sadatime(R15)  COPY "HH" FROM SOURCE TO OUTPUT
         MVC   0(2,R2),0(R15)
         BRU   CONCNEXT
*
***********************************************************************
*  HUNDREDTHS ("TT")                                                  *
***********************************************************************
CONC_08  lgh   R15,16(,R1)        LOAD  "TT" OFFSET
         ltgr  R15,R15            "TT"  AVAILABLE    ???
         BRNM  CONC_08A           YES - COPY IT
         MVC   0(2,R2),C00        NO  - DEFAULT "TT"=00
         BRU   CONCNEXT
*
CONC_08A LA    R15,sadatime(R15)  COPY "TT" FROM SOURCE TO OUTPUT
         MVC   0(2,R2),0(R15)
         BRU   CONCNEXT
*
***********************************************************************
*  MICROSECONDS ("NNNN")                                              *
***********************************************************************
CONC_09  lgh   R15,18(,R1)        LOAD  "NNNN"   OFFSET
         ltgr  R15,R15            "NNNN" AVAILABLE  ???
         BRNM  CONC_09A           YES - COPY IT
         MVC   0(4,R2),C00        NO  - DEFAULT "NNNN"=00
         aghi  R14,2
         aghi  R2,4
         BRU   CONCLOOP
*
CONC_09A LA    R15,sadatime(R15)  COPY  "NNNN" FROM SOURCE TO OUTPUT
         MVC   0(4,R2),0(R15)
         aghi  R14,2
         aghi  R2,4
         BRU   CONCLOOP
*
CONC_10  lgr   R15,R2             LOAD   PREVIOUS  CHAR    ADDRESS
         bctgr R15,0
         if CLI,0(R15),eq,C' '    PRECEEDED   BY   BLANK   ???
           MVI 0(R2),C' '         YES -  SUBSTITUTE BLANK
         else
           MVI 0(R2),C'/'         INSERT DELIMITER
         endif
         aghi  R14,2              ADVANCE  TO NEXT OUTPUT  MAP ELEMENT
         aghi  R2,1               ADVANCE  OUTPUT  ADDRESS
         BRU   CONCLOOP           LOOP THROUGH MAP
*
CONC_11  lgr   R15,R2             LOAD   PREVIOUS  CHAR    ADDRESS
         bctgr R15,0
         if CLI,0(R15),eq,C' '    PRECEEDED   BY   BLANK   ???
           MVI 0(R2),C' '         YES -  SUBSTITUTE BLANK
         else
           MVI 0(R2),C':'         INSERT DELIMITER
         endif
         aghi  R14,2              ADVANCE  TO NEXT OUTPUT  MAP ELEMENT
         aghi  R2,1               ADVANCE  OUTPUT  ADDRESS
         BRU   CONCLOOP           LOOP THROUGH MAP
*
CONC_12  MVI   0(R2),C' '         INSERT DELIMITER
         aghi  R14,2              ADVANCE  TO NEXT OUTPUT  MAP ELEMENT
         aghi  R2,1               ADVANCE  OUTPUT  ADDRESS
         BRU   CONCLOOP           LOOP THROUGH MAP
*
***********************************************************************
*  MONTH ("MMM")                                                      *
***********************************************************************
CONC_13  lgh   R15,04(,R1)        LOAD  "MM" OFFSET
         ltgr  R15,R15            "MM"  AVAILABLE    ???
         BRNM  CONC_13B           YES - COPY IT
*
         lgh   R15,08(,R1)        LOAD "DDD" OFFSET
         ltgr  R15,R15            AVAILABLE  ???
         BRNM  CONC_13A           YES - DERIVE  "MM" FROM "DDD"
*
         MVC   0(3,R2),SPACES     NO  - DEFAULT "MMM" = SPACES
         BRU   CONC_13C
*
CONC_13A lgr   R3,R9              SAVE  RETURN   ADDRESS
         BRAS  R9,CONJDATE        BUILD "YY/MM"  FROM  "DDD"
         lgr   R9,R3
*
         bctgr R15,0
         sllg  R15,r15,2
         LA    R15,MONTHS(R15)
         lgh   R15,0(,R15)
         LA    R15,MONTHS(R15)
         MVC   0(3,R2),0(R15)
         BRU   CONC_13C
*
CONC_13B LA    R15,sadatime(R15)  COPY "MM" FROM SOURCE TO OUTPUT
         PACK  SADBLWRK,0(2,R15)
         CVB   R15,SADBLWRK
         bctgr R15,0
         sllg  R15,r15,2
         LA    R15,MONTHS(R15)
         lgh   R15,0(,R15)
         LA    R15,MONTHS(R15)
         MVC   0(3,R2),0(R15)
*
CONC_13C aghi  R14,2
         aghi  R2,3
         BRU   CONCLOOP
*
***********************************************************************
*  AM/PM                                                              *
***********************************************************************
CONC_14  lgh   R15,10(,R1)        LOAD  "HH" OFFSET
         ltgr  R15,R15            "HH"  AVAILABLE    ???
         BRNM  CONC_14A           YES - COPY IT
         MVC   0(2,R2),SPACES     NO  - DEFAULT "HH"=00
         BRU   CONCNEXT
*
CONC_14A LA    R15,sadatime(R15)  COPY "HH" FROM SOURCE TO OUTPUT
         MVC   0(2,R2),AM
         if CLC,0(2,R15),ge,C12   NOON ???
           MVC 0(2,R2),PM
         endif
         BRU   CONCNEXT
*
CONC_15  MVI   0(R2),C'-'         INSERT DELIMITER
         aghi  R14,2              ADVANCE  TO NEXT OUTPUT  MAP ELEMENT
         aghi  R2,1               ADVANCE  OUTPUT  ADDRESS
         BRU   CONCLOOP           LOOP THROUGH MAP
*
CONC_16  MVI   0(R2),C'.'         INSERT DELIMITER
         aghi  R14,2              ADVANCE  TO NEXT OUTPUT  MAP ELEMENT
         aghi  R2,1               ADVANCE  OUTPUT  ADDRESS
         BRU   CONCLOOP           LOOP THROUGH MAP
*
***********************************************************************
*  MONTH ("MMMMMMMMM")                                                *
***********************************************************************
CONC_17  lgh   R15,04(,R1)        LOAD  "MM" OFFSET
         ltgr  R15,R15            "MM"  AVAILABLE    ???
         BRNM  CONC_17B           YES - COPY IT
*
         lgh   R15,08(,R1)        LOAD "DDD" OFFSET
         ltgr  R15,R15            AVAILABLE  ???
         BRNM  CONC_17A           YES - DERIVE  "MM" FROM "DDD"
*
         MVC   0(3,R2),SPACES     NO  - DEFAULT "MMM" = SPACES
         LHI   R3,3-1
         BRU   CONC_17C
*
CONC_17A lgr   R3,R9              SAVE  RETURN   ADDRESS
         BRAS  R9,CONJDATE        BUILD "YY/MM"  FROM  "DDD"
         lgr   R9,R3
*
         bctgr R15,0
         sllg  R15,r15,2
         LA    R15,MONTHS(R15)
         lgh   R3,2(,R15)
         lgh   R15,0(,R15)
         LA    R15,MONTHS(R15)
         exrl  R3,MVCMONTH
         BRU   CONC_17C
*
CONC_17B LA    R15,sadatime(R15)  COPY "MM" FROM SOURCE TO OUTPUT
         PACK  SADBLWRK,0(2,R15)
         CVB   R15,SADBLWRK
         BCTR  R15,0
         sllg  R15,r15,2
         LA    R15,MONTHS(R15)
         lgh   R3,2(,R15)
         lgh   R15,0(,R15)
         LA    R15,MONTHS(R15)
         exrl  R3,MVCMONTH
*
CONC_17C aghi  R14,2
         LA    R2,1(R2,R3)
         BRU   CONCLOOP
*
CONC_18  MVI   0(R2),C','         INSERT DELIMITER
         aghi  R14,2              ADVANCE  TO NEXT OUTPUT  MAP ELEMENT
         aghi  R2,1               ADVANCE  OUTPUT  ADDRESS
         BRU   CONCLOOP           LOOP THROUGH MAP
*
***********************************************************************
*  SET OUTPUT ADDRESS/LENGTH TO RESULTS                               *
***********************************************************************
CONCRSET MVI   SALEAD0+0,C'0'     RESTORE  LEADING  ZERO   AREA
         MVC   SALEAD0+1(L'SALEAD0-1),SALEAD0+0
*
         LA    R1,SAWORKAR        POINT TO STANDARD FORMAT DATE
         lgh   R2,SAOUTCON        LOAD     STANDARD FORMAT DATE LENGTH
         sllg  R2,r2,1
         agf   R2,CONOLENA
         lgh   R2,0(,R2)
         xc    sapackar,sapackar  init this area to zeroes
*
CONCEXIT BR    R9                 RETURN
*
BADRC    MVI   SAFMTERR,EMBAD     BAD  SOURCE DATA
         BRU   Ret_err            SET  RETURN CODE = FAILURE
                        EJECT
static   loctr
         entry CONSRCMP
*        make this table available to MR95 (build mask routine)
***********************************************************************
*  SOURCE OFFSET MAPS FOR EACH SOURCE CONTENT CODE                    *
***********************************************************************
*             CC    YY    MM    DD   DDD    HH    MM    SS    TT   NNNN
CONSRCMP DS 0XL20
CONSRC01 DC H'-1',H'00',H'02',H'04',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC02 DC H'-1',H'00',H'03',H'06',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC03 DC H'00',H'02',H'04',H'06',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC04 DC H'00',H'02',H'05',H'08',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC05 DC H'-1',H'04',H'02',H'00',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC06 DC H'-1',H'06',H'03',H'00',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC07 DC H'04',H'06',H'02',H'00',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC08 DC H'06',H'08',H'03',H'00',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC09 DC H'-1',H'00',H'-1',H'-1',H'02',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC10 DC H'-1',H'00',H'-1',H'-1',H'03',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC11 DC H'00',H'02',H'-1',H'-1',H'04',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC12 DC H'00',H'02',H'-1',H'-1',H'05',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC13 DC H'-1',H'-1',H'00',H'02',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC14 DC H'-1',H'-1',H'00',H'03',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC15 DC H'-1',H'-1',H'03',H'00',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC16 DC H'-1',H'-1',H'00',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC17 DC H'-1',H'-1',H'-1',H'00',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC18 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC19 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'00',H'02',H'04',H'06',H'-1'
CONSRC20 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'00',H'03',H'06',H'09',H'-1'
CONSRC21 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'00',H'02',H'04',H'-1',H'-1'
CONSRC22 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'00',H'03',H'06',H'-1',H'-1'
CONSRC23 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'00',H'02',H'-1',H'-1',H'-1'
CONSRC24 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'00',H'03',H'-1',H'-1',H'-1'
CONSRC25 DC H'00',H'02',H'04',H'06',H'-1',H'08',H'10',H'12',H'-1',H'-1'
CONSRC26 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC27 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC28 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC29 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC30 DC H'00',H'02',H'04',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC31 DC H'00',H'02',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC32 DC H'-1',H'00',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC33 DC H'07',H'09',H'00',H'04',H'-1',H'12',H'15',H'18',H'21',H'24'
CONSRC34 DC H'00',H'02',H'05',H'08',H'-1',H'11',H'14',H'17',H'20',H'-1'
CONSRC35 DC H'-1',H'04',H'00',H'02',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC36 DC H'04',H'06',H'00',H'02',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC37 DC H'06',H'08',H'00',H'03',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC38 DC H'00',H'02',H'-1',H'-1',H'05',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC39 DC H'00',H'02',H'05',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC40 DC H'00',H'02',H'05',H'08',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC41 DC H'00',H'02',H'05',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC42 DC H'00',H'02',H'05',H'08',H'-1',H'11',H'14',H'17',H'20',H'-1'
CONSRC43 DC H'-1',H'-1',H'03',H'00',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC44 DC H'06',H'08',H'03',H'00',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC45 DC H'-1',H'06',H'03',H'00',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC46 DC H'-1',H'-1',H'00',H'03',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC47 DC H'06',H'08',H'00',H'03',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC48 DC H'-1',H'00',H'-1',H'-1',H'03',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC49 DC H'-1',H'00',H'03',H'06',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC50 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC51 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC52 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC53 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONSRC54 DC H'00',H'02',H'04',H'06',H'-1',H'08',H'10',H'12',H'14',H'-1'
CONSRC55 DC H'00',H'02',H'04',H'06',H'-1',H'08',H'10',H'12',H'14',H'16'
CONSRC56 DC H'00',H'02',H'05',H'08',H'-1',H'11',H'14',H'17',H'20',H'22'
*
***********************************************************************
*  OUTPUT CONTENT ASSEMBLY TEMPLATE                                   *
***********************************************************************
*
*  00-CC,01-YY,02-MM,03-DD, 04-DDD, O5-HH,06-MM,07-SS,08-TT,09-NNNN
*  10='/', 11=':', 12=' ', 13-MMM, 14='AM/PM', 15='-', 16='.'
*  17-MMMMMMMMM 18=','
CONOUTMP DS 0XL40
CONOUT01 DC H'01',H'02',H'03',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT02 DC H'01',H'10',H'02',H'10',H'03',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT03 DC H'00',H'01',H'02',H'03',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT04 DC H'00',H'01',H'10',H'02',H'10',H'03',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT05 DC H'03',H'02',H'01',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT06 DC H'03',H'10',H'02',H'10',H'01',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT07 DC H'03',H'02',H'00',H'01',H'01',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT08 DC H'03',H'10',H'02',H'10',H'00',H'01',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT09 DC H'01',H'04',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT10 DC H'01',H'10',H'04',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT11 DC H'00',H'01',H'04',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT12 DC H'00',H'01',H'10',H'04',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT13 DC H'02',H'03',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT14 DC H'02',H'10',H'03',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT15 DC H'03',H'10',H'02',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT16 DC H'02',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT17 DC H'03',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT18 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT19 DC H'05',H'06',H'07',H'08',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT20 DC H'05',H'11',H'06',H'11',H'07',H'16',H'08',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT21 DC H'05',H'06',H'07',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT22 DC H'05',H'11',H'06',H'11',H'07',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT23 DC H'05',H'06',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT24 DC H'05',H'11',H'06',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT25 DC H'00',H'01',H'02',H'03',H'05',H'06',H'07',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT26 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT27 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT28 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT29 DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT30 DC H'00',H'01',H'02',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT31 DC H'00',H'01',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT32 DC H'01',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT33 DC H'13',H'12',H'03',H'12',H'00',H'01',H'12',H'05',H'11',H'06'
         DC H'11',H'07',H'16',H'08',H'12',H'14',H'-1',H'-1',H'-1',H'-1'
CONOUT34 DC H'00',H'01',H'15',H'02',H'15',H'03',H'12',H'05',H'11',H'06'
         DC H'11',H'07',H'16',H'08',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT35 DC H'02',H'03',H'01',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT36 DC H'02',H'03',H'00',H'01',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT37 DC H'02',H'10',H'03',H'10',H'00',H'01',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT38 DC H'00',H'01',H'15',H'04',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT39 DC H'00',H'01',H'15',H'02',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT40 DC H'00',H'01',H'15',H'02',H'15',H'03',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT41 DC H'00',H'01',H'10',H'02',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT42 DC H'00',H'01',H'10',H'02',H'10',H'03',H'12',H'05',H'11',H'06'
         DC H'11',H'07',H'16',H'08',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT43 DC H'03',H'15',H'02',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT44 DC H'03',H'15',H'02',H'15',H'00',H'01',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT45 DC H'03',H'15',H'02',H'15',H'01',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT46 DC H'02',H'15',H'03',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT47 DC H'02',H'15',H'03',H'15',H'00',H'01',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT48 DC H'01',H'15',H'04',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT49 DC H'01',H'15',H'02',H'15',H'03',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT50 DC H'17',H'12',H'03',H'18',H'12',H'00',H'01',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT51 DC H'03',H'12',H'17',H'12',H'00',H'01',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT52 DC H'03',H'15',H'13',H'15',H'00',H'01',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT53 DC H'17',H'12',H'00',H'01',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT54 DC H'00',H'01',H'02',H'03',H'05',H'06',H'07',H'08',H'-1',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT55 DC H'00',H'01',H'02',H'03',H'05',H'06',H'07',H'08',H'09',H'-1'
         DC H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1',H'-1'
CONOUT56 DC H'00',H'01',H'10',H'02',H'10',H'03',H'12',H'05',H'11',H'06'
         DC H'11',H'07',H'16',H'08',H'09',H'-1',H'-1',H'-1',H'-1',H'-1'
*
***********************************************************************
*  OUTPUT CONTENT CODE RESULTING NO. OF DIGITS                        *
***********************************************************************
CONOUTLN DC    H'00'              00 - UNSPECIFIED (SOURCE DIGITS)
         DC    H'06'              01 - YYMMDD
         DC    H'08'              02 - YY/MM/DD
         DC    H'08'              03 - CCYYMMDD
         DC    H'10'              04 - CCYY/MM/DD
         DC    H'06'              05 - DDMMYY
         DC    H'08'              06 - DD/MM/YY
         DC    H'08'              07 - DDMMCCYY
         DC    H'10'              08 - MM/DD/CCYY
         DC    H'05'              09 - YYDDD
         DC    H'06'              10 - YY/DDD
         DC    H'07'              11 - CCYYDDD
         DC    H'08'              12 - CCYY/DDD
         DC    H'04'              13 - MMDD
         DC    H'05'              14 - MM/DD
         DC    H'05'              15 - DD/MM
         DC    H'02'              16 - MM
         DC    H'02'              17 - DD
         DC    H'08'              18 - DDDDDDDD
         DC    H'08'              19 - HHMMSSTT
         DC    H'11'              20 - HH:MM:SS.TT
         DC    H'06'              21 - HHMMSS
         DC    H'08'              22 - HH:MM:SS
         DC    H'04'              23 - HHMM
         DC    H'05'              24 - HH:MM
         DC    H'14'              25 - CCYYMMDDHHMMSS
         DC    H'08'              26 - SSSSSSSS
         DC    H'00'              27 - LOWERCASE
         DC    H'00'              28 - UPPERCASE
         DC    H'00'              29 - DBCS
         DC    H'06'              30 - CCYYMM
         DC    H'04'              31 - CCYY
         DC    H'02'              32 - YY
         DC    H'26'              33 - MMM DD CCYY HH:MM:SS.TT AM
         DC    H'22'              34 - CCYY-MM-DD HH:MM:SS.TT
         DC    H'06'              35 - MMDDYY
         DC    H'08'              36 - MMDDCCYY
         DC    H'10'              37 - MM/DD/CCYY
         DC    H'08'              38 - CCYY-DDD
         DC    H'07'              39 - CCYY-MM
         DC    H'10'              40 - CCYY-MM-DD
         DC    H'07'              41 - CCYY/MM
         DC    H'22'              42 - CCYY/MM/DD HH:MM:SS.TT
         DC    H'05'              43 - DD-MM
         DC    H'10'              44 - DD-MM-CCYY
         DC    H'08'              45 - DD-MM-YY
         DC    H'05'              46 - MM-DD
         DC    H'10'              47 - MM-DD-CCYY
         DC    H'06'              48 - YY-DDD
         DC    H'08'              49 - YY-MM-DD
         DC    H'18'              50 - DD MMMMMMMMM CCYY
         DC    H'17'              51 - MMMMMMMMM DD, CCYY
         DC    H'11'              52 - DD-MMM-CCYY
         DC    H'13'              53 - MMMMMMMMM CCYY
         DC    H'16'              54 - CCYYMMDDHHMMSSTT
         DC    H'20'              55 - CCYYMMDDHHMMSSTTNNNN
         DC    H'26'              56 - CCYY/MM/DD HH:MM:SS.TTTTTT
code     loctr
                        EJECT
***********************************************************************
*                                                                     *
* "PACKLONG" CONVERTS ZONED DECIMAL DATA TO PACKED DECIMAL DATA       *
*            WITHIN THE "SAPACKAR" (UP TO 30 DIGITS ARE ACCOMODATED)  *
*                                                                     *
* REGISTER PARAMETERS UPON ENTRY                                      *
*                                                                     *
*   R1 - ADDRESS OF ZONED DECIMAL SOURCE DATA                         *
*   R2 - LENGTH  OF ZONED DECIMAL SOURCE DATA                         *
*   R9 - RETURN  ADDRESS                                              *
*                                                                     *
* REGISTER CONTENTS   UPON EXIT                                       *
*                                                                     *
*   R1 - ADDRESS OF ZONED  RESULT DATA (WITHIN "SAWORKAR")            *
*   R2 - LENGTH  OF ZONED  RESULT DATA                                *
*                                                                     *
***********************************************************************
*
PACKLONG ds    0h
         ZAP   SAPACKAR,P000      RESET TARGET     AREA  RTC19543
         lgr   R15,R2             LOAD SOURCE LENGTH (UNPACKED BYTES)
         lgr   R14,R2             LOAD SOURCE LENGTH (UNPACKED BYTES)
*
         if    aghi,r15,-30,p      Are we packing over 30 bytes?
*          now r15 has the excess over 30
           exrl r15,pack_farleft  Pack the far left
*pack_farleft PACk sapackar+0(l'sapackar-14),0(0,r1)
           pack sapackar+1(8),2(15,r1)
           pack sapackar+8(8),16(15,r1)
         else
           aghi r15,30             put it back to original value
           LgHI R0,15              COMPUTE LENGTH OVER 16
           if SgR,R15,R0,p         more than 16 bytes to pack?
*            r15 will be positive if 17 or more digits to pack
*            this will in fact be the correct machine length as we
*            have to do one extra byte on the left half
             exrl R15,PACKLEFT    PACK  THE LEFT HALF
*PACKLEFT    PACK SAPACKAR+0(L'SAPACKAR-7),0(0,R1) *** EXECUTED ***
             AgR R1,R15            ADVANCE   REMAINING SOURCE ADDRESS
             SgR R14,R15           DECREMENT REMAINING SOURCE LENGTH
*
           endif
           BCTgR R14,0             DECR  LENGTH FOR UNPACK
           exrl R14,PACKRGHT
*PACKRGHT   PACK SAPACKAR+8(08),0(0,R1)           *** EXECUTED ***
*
         endif
         AgHI  R2,2               COMPUTE NO. OF PACKED BYTES (ROUNDED)
         SRLg  R2,r2,1
         LA    R1,SAPACKAR+L'SAPACKAR
         SgR   R1,R2
*
         BR    R9
                        EJECT
***********************************************************************
*                                                                     *
* "UNPKLONG" CONVERTS PACKED DATA INTO ZONED DECIMAL(UNPACKED) DATA   *
*            WITHIN THE "SAWORKAR" (UP TO 30 DIGITS ARE ACCOMODATED)  *
*                                                                     *
*            REGISTERS R1 AND R2 ARE ADJUSTED TO POINT TO THE ZONED   *
*            DECIMAL DATA                                             *
*                                                                     *
* REGISTER PARAMETERS UPON ENTRY                                      *
*                                                                     *
*   R1 - ADDRESS OF PACKED SOURCE DATA                                *
*   R2 - LENGTH  OF PACKED SOURCE DATA                                *
*   R9 - RETURN  ADDRESS                                              *
*                                                                     *
* REGISTER CONTENTS   UPON EXIT                                       *
*                                                                     *
*   R1 - ADDRESS OF ZONED  RESULT DATA (WITHIN "SAWORKAR")            *
*   R2 - LENGTH  OF ZONED  RESULT DATA                                *
*                                                                     *
***********************************************************************
*
UNPKLONG MVI   SAWORKAR+0,C'0'    ZERO THE SAWORKAR
         MVC   SAWORKAR+1(L'SAWORKAR-1),SAWORKAR
*
         lgr   R15,R2             SOURCE LENGTH IN BYTES (PACKED)
         lgr   R14,R2             SOURCE LENGTH IN BYTES (PACKED)
         SLLg  R2,r2,1            RESULT LENGTH IN BYTES (UNPACKED +1)
         bctgr R2,0               RESULT LENGTH IN BYTES (UNPACKED)
*
         if   aghi,r2,-30,p       is the result over 30 bytes?
*          now r2 has the excess over 30
           exrl r2,unpk_farleft  Pack the far left
*unpk_farleft unpk saworkar+1(l'saworkar-29),0(0,r1)
           unpk saworkar+3(15),1(8,r1)
           unpk saworkar+17(15),8(8,r1)
           aghi r2,30             reset r2
         else
           aghi r2,30             reset r2
           lghi R0,8              COMPUTE EXCESS SOURCE LENGTH    (-1)
           if SgR,R14,R0,p        more than 16 to do?
*
             exrl R14,UNPKLEFT    UNPACK  THE LEFT HALF
*UNPKLEFT     UNPK SAWORKAR+02(L'SAWORKAR-16),0(0,R1) *** EXECUTED ***
             AgR R1,R14           ADVANCE   REMAINING SOURCE ADDRESS
             SgR R15,R14          DECREMENT REMAINING SOURCE LENGTH
*
           endif
UNPK01     BCTgR R15,0            DECR FOR L2 OF UNPACK
           exrl R15,UNPKRGHT
*UNPKRGHT   UNPK SAWORKAR+17(15),0(0,R1)           *** EXECUTED ***
*
         endif
         LA    R1,SAWORKAR+L'SAWORKAR  SET SOURCE DATA POINTER TO END
         SgR   R1,R2                   BACKUP  TO BEGINNING OF DATA
         BR    R9
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  LOOP THROUGH THE SOURCE DATA COUNTING THE TYPE OF CHARACTERS FOUND *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
CNTDIGIT MVI   SAHADDEC,C'N'      INITIALIZE  DECIMAL POINT FOUND FLAG
*
         MVI   SASAVSGN,X'F0'     ASSUME UNSIGNED (NO EXPLICIT SIGN)
         if CLI,SAOUTSGN,eq,C'Y'  SIGNED OUTPUT ???
           MVI SASAVSGN,X'C0'     CHANGE TO   SIGNED
         endif
*
         xr    r6,r6              digit count
         xr    r7,r7              decimal counter
         xr    r4,r4              sign counter
         xr    r15,r15            decimal point counter

         ltgr  R0,R2              LOAD SOURCE LENGTH (LOOP COUNTER)
         BRNP  CNT99              BRANCH IF NO SOURCE DATA
         lgr   R14,R1             LOAD SOURCE START
*
CNTSRCLP CLI   0(R14),C'0'        DIGIT ???
         BRL   SRC01              NO  - CHECK SPECIAL CHARACTER
         CLI   0(R14),C'9'        DIGIT ???
         BRNH  SRCINCCT           YES - INCREMENT COUNTER
         b     badrc
*
SRC01    CLI   0(R14),C'.'        DECIMAL   POINT   ???
         BRNE  SRC02              NO  - CHECK FOR   SIGN
         MVI   SAHADDEC,C'Y'      SET DECIMAL POINT FOUND  FLAG
         ahi   r15,1              INCREMENT DECIMAL POINT  COUNTER
         stg   R14,SADECADR       SAVE DECIMAL POINT ADDR
         BRU   SRC05              CONTINUE
*
SRC02    CLI   0(R14),C'-'        SIGN ???
         BRE   SRC03              YES - BRANCH
         CLI   0(R14),C'+'        SIGN ???
         BRNE  BADRC              NO  - ALL OTHER SPECIAL CHARS INVALID
*
         MVI   SASAVSGN,X'C0'     POSITIVE  SIGN
         BRU   SRC04
*
SRC03    MVI   SASAVSGN,X'D0'     NEGATIVE  SIGN
*
SRC04    Ahi   r4,1               INCREMENT SIGN COUNTER
         Chi   r6,0               ANY SOURCE DIGITS (LEADING SIGN) ???
         BRE   SRC05              NO - LEADING SIGN IS OKAY
         CHI   R0,1               LAST CHARACTER ???
         BRH   BADRC              NO - SIGNS IN THE MIDDLE AREN'T OKAY
*
SRC05    LA    R14,1(,R14)        INCREMENT MASK POSITION
         BRCT  R0,CNTSRCLP        LOOP THROUGH  SOURCE
*
         BRU   CNT99
*
SRCINCCT Ahi   r6,1               INCREMENT DIGIT   COUNTER
         if CLI,SAHADDEC,eq,C'Y'  DECIMAL   POINT   FOUND   ???
           Ahi r7,1               INCREMENT DECIMAL COUNTER
         endif
         LA    R14,1(,R14)        INCREMENT SOURCE  POSITION
         BRCT  R0,CNTSRCLP        LOOP THROUGH MASK
                        SPACE 3
CNT99    equ   *
*        stc   r15,sasrcdpt
*        sth   r4,sasrcsgn
         Chi   r15,1              ONE DECIMAL POINT ???
         BRH   BADRC              NO  - MULTIPLE DECIMAL POINTS
         Chi   r4,1               ONE SIGN ???
         BRH   BADRC              NO  - MULTIPLE SIGNS
*
         sth   r6,sasrcdig
         sth   r7,sasrcdec
*
         BR    R9                 RETURN TO CALL POINT
                        EJECT
***********************************************************************
*  CONVERT JULIAN DATE (YYDDD) TO GREGORIAN MM/DD                     *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R1:  SOURCE MAP TABLE ENTRY ADDRESS                          *
*        R5:  GREGORIAN  DAY   (BINARY)       ----- RETURNED          *
*        R15: GREGORIAN  MONTH (BINARY)       ----- RETURNED          *
*                                                                     *
***********************************************************************
CONJDATE lgh   R15,02(,R1)        LOAD "YY"  OFFSET
         LA    R15,sadatime(R15)  LOAD "YY"  ADDRESS
         PACK  SADBLWRK,0(2,R15)
         cvb   R4,SADBLWRK        load the binary value for "yy"
*
         nilf  r4,X'00000003'     set r4 to zero if a leap year by     +
                    anding value with hex 03
*
CONJD_10 lgh   R15,08(,R1)        LOAD "DDD" OFFSET
         LA    R15,sadatime(R15)  LOAD "DDD" ADDRESS
         PACK  SADBLWRK,0(3,R15)
         CVB   R5,SADBLWRK        LOAD  THE  BINARY  VALUE FOR "DDD"
*
         xgr   R15,R15            R15 COUNTS THE MONTHS
         lgr   R0,R15             ZERO  REGISTER
         if   ltgr,r5,r5,p        make sure we have at least 1 day
*
CONJD_20   aghi R15,1             INCREMENT  MONTH
           IC  R0,DIMTBL-1(R15)   LOAD DAYS  IN  MONTH
           SgR R5,R0              SUBTRACT   FROM DAYS REMAINING
           CgHI R15,3             MARCH ???
           BRNE CONJD_30
           LTgR R4,R4             LEAP  YEAR   ???
           BRP CONJD_30           NO  - BYPASS REDUCTION
           BCTgR R5,0             YES - REDUCE REMAINING DAYS BY ONE
*
CONJD_30   LTgR R5,R5
           BRP CONJD_20           IF DAYS STILL GT ZERO, LOOP
*
           AgR  R5,R0             RESTORE SUBTRACTED VALUE
           LTgR R5,R5             IF NOW ZERO MUST BE FEB 29
           BRNZ CONJD_40
           BCTgR R15,0            MONTH = FEBRUARY
           LgHI R5,29             DAY   = 29TH
         endif
*
CONJD_40 BR    R9
                        SPACE 3
static   loctr
         ltorg
*
         END
