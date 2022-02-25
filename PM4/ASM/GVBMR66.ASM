         TITLE 'GVBMR66  - CRP VSAM I/O HANDLER'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2005, 2010.
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
***********************************************************************
*                                                                     *
* GVBMR66  -   READ/WRITE CRP RECORDS WITH "RBA" FEEDBACK             *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R15 - ENTRY    POINT ADDRESS                                 *
*            - TEMPORARY WORK REGISTER                                *
*        R14 - RETURN ADDRESS                                         *
*            - TEMPORARY WORK REGISTER                                *
*        R13 - REGISTER  SAVE AREA  ADDRESS                           *
*        R12 -                                                        *
*        R11 -  RECORD   AREA       ADDRESS                           *
*        R10 - PROGRAM   BASE REGISTER                                *
*        R9  - PRIMARY   SUBROUTINE    CALL  RETURN  ADDRESS          *
*        R8  - CURRENT   SUBSTRING   LENGTH                           *
*        R7  - CURRENT   TARGET     ADDRESS (RECORD  LENGTH)          *
*        R6  - REMAINING SOURCE      LENGTH                           *
*        R5  - CURRENT   SOURCE     ADDRESS                           *
*        R4  -  CALL     PARAMETER     AREA  ADDRESS                  *
*        R3  -  VSAM REQUEST PARAMETER LIST  ADDRESS                  *
*        R2  -                                                        *
*        R1  -                                                        *
*        R0  -                                                        *
*                                                                     *
***********************************************************************
         SPACE 3
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
         SPACE 3
RSABP    EQU   4
RSAFP    EQU   8
RSA14    EQU   12
RSA15    EQU   16
RSA0     EQU   20
RSA1     EQU   24
         EJECT
WSVALNO  EQU   C'N'            VALUE NO
WSVALYES EQU   C'Y'            VALUE YES
WSVALRNF EQU   X'10'           VALUE RECORD NOT FOUND
         SPACE 3
PARMAREA DSECT                 CALL PARAMETER AREA
         SPACE 1
PARMFC   DS    HL02            FUNCTION          CODE
PARMRC   DS    HL02            VSAM     RETURN   CODE
PARMFDBK DS    HL02            VSAM     FEEDBACK CODE
PARMRBA  DS    FL04            RELATIVE BYTE     ADDRESS
PARMDDN  DS    CL08            FILE DDNAME
         EJECT
         IFGRPL
         EJECT
         USING PARMAREA,R4
         USING IFGRPL,R3
         SPACE 3
         PRINT GEN
         SPACE 3
GVBMR66  RMODE ANY
GVBMR66  AMODE 31
GVBMR66  CSECT
         J     CODE
         GVBEYE GVBMR66
static   loctr
code     loctr
         STM   R14,R12,RSA14(R13)   SAVE CALLING PROGRAM'S REGISTERS
         SPACE 1
         LR    R10,R15              SET  PROGRAM BASE REGISTER
         USING GVBMR66,R10
         SPACE 1
         LR    R15,R13              SAVE PREVIOUS RSA ADDRESS
         LA    R13,SAVEAREA         LOAD NEW      RSA ADDRESS
         ST    R15,SAVEAREA+RSABP   SET  BACKWARD POINTER
         ST    R13,RSAFP(,R15)      SET  FORWARD  POINTER
         SPACE 3
         L     R4,0(,R1)            LOAD  PARM   AREA ADDR
         L     R11,4(,R1)           LOAD  RECORD AREA ADDR
         LA    R11,0(,R11)          STRIP OFF    HIGH BIT
         SPACE 3
SELFUNC  LH    R1,PARMFC            LOAD  FUNCTION CODE INTO REGISTER
         LTR   R1,R1                CHECK FUNCTION CODE (RANGE 1-6)
         BNP   SETRCERR
         C     R1,WSVAL6
         BH    SETRCERR
         SPACE 1
         BCTR  R1,0                 CONVERT  TO OFFSET
         SLL   R1,2                 MULTIPLY BY FOUR
         SPACE 1
         B     BRNCHTBL(R1)         SELECT FUNCTION
BRNCHTBL B     FUNC01               01 - OPEN  VSAM   FILE (EXTEND)
         B     FUNC02               02 - CLOSE VSAM   FILE
         B     FUNC03               03 - READ  RECORD
         B     FUNC04               04 - WRITE RECORD
         B     FUNC05               05 - START BY    "RBA"
         B     FUNC06               06 - OPEN  VSAM   FILE (REUSE)
         EJECT
SETRCERR LH    R15,WSCODERR         SET RETURN CODE = INVALID CODES (7)
         DC    H'0'
         B     LOGERROR
         SPACE 1
SETLOGER LH    R15,WSLOGERR         SET RETURN CODE = LOGIC   ERROR (5)
         DC    H'0'
         B     LOGERROR
         SPACE 1
OPNCLSER MVC   PARMFDBK+1(1),CRPACB1+49  RETURN ACB ERROR CODE
         B     LOGERROR
         SPACE 1
RETURN   MVC   PARMFDBK+1(1),RPLFDB3    RETURN VSAM FEEDBACK CODE
         SPACE 1
LOGERROR STH   R15,PARMRC           STORE VSAM RETURN CODE IN PARM AREA
         SPACE 1
         L     R13,RSABP(,R13)      RESTORE    REGISTER 13
         ST    R15,RSA15(,R13)      SET        RETURN CODE
         XC    RSAFP(4,R13),RSAFP(R13)
         LM    R14,R12,RSA14(R13)
         BR    R14
         EJECT
***********************************************************************
*                                                                     *
* FUNCTION :   OPEN THE CRP FILE                                      *
*                                                                     *
***********************************************************************
         SPACE 1
FUNC01   CLI   WSACBOPN,WSVALNO    IS THE VSAM DATA SET ALREADY OPEN???
         BNE   SETLOGER            YES - SET LOGIC ERROR AND RETURN
         SPACE 1
         LA    R3,VSRDFRPL         USE READ RPL AS PLACEHOLDER
         XC    RPLFDB3,RPLFDB3     INITIALIZE FEEDBACK
         SPACE 1
         CLC   PARMDDN,BLANKS      BLANK DDNAME ???
         BE    FUNC01A             USE DEFAULT NAME
         CLC   PARMDDN,WSLOWVAL    LOW VALUES ???
         BE    FUNC01A             USE DEFAULT NAME
         SPACE 1
         MODCB ACB=CRPACB1,        USE THE DDNAME PASSED               X
               DDNAME=(*,PARMDDN)
         LTR   R15,R15
         BNZ   RETURN
         SPACE 1
FUNC01A  OPEN  (CRPACB1,)          OPEN THE VSAM DATASET
         LTR   R15,R15             SUCCESSFUL ???
         BZ    FUNC01B             YES -  SET INDICATOR
         SPACE 1
         C     R15,WSVAL4          WARNING MESSAGE ???
         BNE   OPNCLSER            NO  - SERIOUS ERROR
         SPACE 1
FUNC01B  MVI   WSACBOPN,WSVALYES   INDICATE SUCCESSFUL OPEN
         SPACE 1
         B     RETURN              RETURN
         EJECT
***********************************************************************
*                                                                     *
* FUNCTION :   CLOSE THE CRP FILE                                     *
*                                                                     *
***********************************************************************
         SPACE 1
FUNC02   CLI   WSACBOPN,WSVALYES   HAS THE VASM DATA SET BEEN OPENED???
         BNE   SETLOGER            NO  - RETURN
         SPACE 1
         LA    R3,VSRDFRPL         USE READ RPL AS PLACEHOLDER
         XC    RPLFDB3,RPLFDB3     INITIALIZE FEEDBACK
         SPACE 1
         CLOSE (CRPACB1)           CLOSE THE VSAM DATASET
         LTR   R15,R15
         BNZ   OPNCLSER
         SPACE 1
         MVI   WSACBOPN,WSVALNO    INDICATE VSAM FILE CLOSED
         SPACE 1
         B     RETURN              RETURN
         EJECT
***********************************************************************
*                                                                     *
* FUNCTION :   READ THE NEXT CRP RECORD                               *
*                                                                     *
***********************************************************************
         SPACE 1
FUNC03   CLI   WSACBOPN,WSVALYES   HAS THE VSAM DATA SET BEEN OPENED???
         BNE   SETLOGER            NO  - SET LOGIC ERR AND RETURN
         SPACE 1
         XC    PARMRBA,PARMRBA     ZERO RBA (INITIALIZE)
         SPACE 1
         LA    R3,VSRDFRPL         USE READ RPL ADDRESS
         SPACE 1
         GET   RPL=VSRDFRPL        GET THE NEXT RECORD (READ FWD)
         LTR   R15,R15
         BNZ   RETURN
         SPACE 1
         SHOWCB RPL=VSRDFRPL,      OBTAIN RBA FROM RPL                 X
               FIELDS=(RBA,RECLEN),                                    X
               AREA=WSRBAVAL,                                          X
               LENGTH=8
         LTR   R15,R15
         BNZ   RETURN
         SPACE 1
         L     R5,WSRECADR         LOAD RECORD ADDRESS (LOCATE MODE)
         L     R6,WSRECLEN         LOAD RECORD LENGTH
         SPACE 1
         CLI   1(R5),X'00'         FIRST FIELD LENGTH ZERO?
         BE    FUNC03              IF YES, GET NEXT RECORD
         SPACE 1
         BAL   R9,DECOMP           DECOMPRESS  RECORD
         SPACE 1
         MVC   PARMRBA,WSRBAVAL    PASS RBA VALUE TO CALLER
         SPACE 1
         SR    R15,R15             ZERO RETURN  CODE
         B     RETURN              RETURN
         SPACE 3
EOD      EQU   *                   END OF DATA
         LH    R15,WSEOD           SET RETURN CODE TO END OF DATA (6)
         B     RETURN              RETURN
         EJECT
***********************************************************************
*                                                                     *
* FUNCTION :   WRITE THE NEXT CRP RECORD (EXTEND)                     *
*                                                                     *
***********************************************************************
         SPACE 1
FUNC04   CLI   WSACBOPN,WSVALYES   HAS THE VSAM DATA SET BEEN OPENED???
         BNE   SETLOGER            NO  - SET LOGIC ERR AND RETURN
         SPACE 1
         XC    PARMRBA,PARMRBA     ZERO RBA (INITIALIZE)
         SPACE 1
         BAL   R9,COMPRESS         COMPRESS RECORD
         SPACE 1
         LA    R3,VSWRTRPL         USE WRITE RPL ADDRESS
         SPACE 1
         MODCB RPL=VSWRTRPL,       CHANGE RECORD LENGTH (VARIABLE)     X
               RECLEN=(R7)
         LTR   R15,R15
         BNZ   RETURN
         SPACE 1
         PUT   RPL=VSWRTRPL        WRITE THE NEXT RECORD
         LTR   R15,R15
         BNZ   RETURN
         SPACE 1
         SHOWCB RPL=VSWRTRPL,      OBTAIN RBA FROM RPL                 X
               FIELDS=RBA,                                             X
               AREA=WSRBAVAL,                                          X
               LENGTH=4
         LTR   R15,R15
         BNZ   RETURN
         SPACE 1
         MVC   PARMRBA,WSRBAVAL    PASS RBA VALUE TO CALLER
         SPACE 1
         B     RETURN              RETURN
         EJECT
***********************************************************************
*                                                                     *
* FUNCTION :   POSITION THE CRP FILE                                  *
*                                                                     *
***********************************************************************
         SPACE 1
FUNC05   CLI   WSACBOPN,WSVALYES   HAS THE VASM DATA SET BEEN OPENED???
         BNE   SETLOGER            NO  - SET LOGIC ERR AND RETURN
         SPACE 1
         MVC   WSRBAVAL,PARMRBA    MOVE RBA TO SEARCH ARGUMENT
         SPACE 1
         LA    R3,VSRDFRPL         USE READ RPL ADDRESS
         SPACE 1
         POINT RPL=VSRDFRPL        POSITION FOR  READ FORWARD
         SPACE 1
         B     RETURN              RETURN
         EJECT
***********************************************************************
*                                                                     *
* FUNCTION :   OPEN THE CRP FILE (REUSE)                              *
*                                                                     *
***********************************************************************
         SPACE 1
FUNC06   CLI   WSACBOPN,WSVALNO    IS THE VSAM DATA SET ALREADY OPEN???
         BNE   SETLOGER            YES - SET LOGIC ERROR AND RETURN
         SPACE 1
         LA    R3,VSRDFRPL         USE READ RPL AS PLACEHOLDER
         XC    RPLFDB3,RPLFDB3     INITIALIZE FEEDBACK (ZERO)
         SPACE 1
         CLC   PARMDDN,BLANKS      BLANK DDNAME ???
         BE    FUNC06A             USE DEFAULT NAME
         CLC   PARMDDN,WSLOWVAL    LOW VALUES ???
         BE    FUNC06A             USE DEFAULT NAME
         SPACE 1
         MODCB ACB=CRPACB1,        USE THE DDNAME PASSED               X
               DDNAME=(*,PARMDDN)
         LTR   R15,R15
         BNZ   RETURN
         SPACE 1
FUNC06A  MODCB ACB=CRPACB1,        CHANGE TO REUSABLE                  X
               MACRF=(ADR,SEQ,OUT,RST)
         LTR   R15,R15
         BNZ   RETURN
         SPACE 1
         OPEN  (CRPACB1,)          OPEN THE VSAM DATASET
         LTR   R15,R15
         BNZ   OPNCLSER
         SPACE 1
         MVI   WSACBOPN,WSVALYES   INDICATE SUCCESSFUL OPEN
         SPACE 1
         B     RETURN              RETURN
         EJECT
COMPRESS EQU   *
         MVC   WSRECCC,4(R11)      MOVE CARRIAGE CONTROL
         SPACE 1
         LA    R5,5(,R11)          SET  SOURCE   ADDRESS(SKIP RDW + CC)
         LH    R6,0(,R11)          SET  SOURCE   LENGTH
         S     R6,WSVAL5           EXCLUDE RDW + CC
         LA    R7,WSDATA           SET  TARGET   ADDRESS
         SPACE 1
         SR    R1,R1               CLEAR REGISTERS     FOR "TRT"
         SR    R2,R2
         SPACE 1
CMLOOP   LR    R15,R6              SETUP SEARCH LENGTH FOR "TRT"
         BCTR  R15,0
         EX    R15,TRTNONB         FIND  NEXT NON-BLANK CHARACTER
         BZ    CM001B
CM001A   SR    R1,R5               COMPUTE LENGTH OF BLANK STRING
         BNP   CM010               BRANCH  IF NULL
         LR    R8,R1               SET  SUBSTRING LENGTH
         B     CM002
CM001B   LR    R8,R6               USE  WHOLE STRING (ALL BLANK)
         SPACE 1
CM002    LA    R5,0(R5,R8)         ADVANCE   SOURCE ADDRESS
         SR    R6,R8               DECREMENT LENGTH REMAINING
         SPACE 1
         C     R8,WSVAL127         SUBSTRING LONGER THAN 127 ???
         BNH   CM003               NO  - ONE BYTE WILL SUFFICE
         MVI   0(R7),X'FF'         YES - OUTPUT   TWO  NULL  COUNTS
         LA    R7,1(,R7)           ADVANCE TARGET ADDRESS
         S     R8,WSVAL127         COMPUTE RESIDUAL
         SPACE 1
CM003    STC   R8,0(,R7)           INSERT NULL COUNT (NO. OF BLANKS)
         OI    0(R7),X'80'
         LA    R7,1(,R7)           ADVANCE TARGET ADDRESS
         EJECT
CM010    LTR   R15,R6              LOAD REMAINING SOURCE LENGTH
         BNP   CM020               BRANCH  IF  EXHAUSTED
         SPACE 1
         BCTR  R15,0
         EX    R15,TRTBLNK         FIND  NEXT  BLANK
         BZ    CM011B
CM011A   SR    R1,R5               COMPUTE LENGTH   OF BLANK STRING
         LR    R8,R1               SET  SUBSTRING   LENGTH
         B     CM012
CM011B   LR    R8,R6               WHOLE STRING  IS DATA
         SPACE 1
CM012    LR    R14,R5              SAVE      SOURCE ADDRESS
         LA    R5,0(R5,R8)         ADVANCE   SOURCE ADDRESS
         SR    R6,R8               DECREMENT LENGTH REMAINING
         SPACE 1
         C     R8,WSVAL127         SUBSTRING LONGER THAN 127 ???
         BNH   CM013               NO  - ONE BYTE   WILL SUFFICE
         MVI   0(R7),X'7F'         YES - BREAK INTO TWO  PIECES
         LA    R15,126
         EX    R15,MOVESUBS
         LA    R14,127(,R14)
         LA    R7,128(,R7)
         S     R8,WSVAL127
         SPACE 1
CM013    STC   R8,0(,R7)           INSERT COMPRESSED LENGTH INDICATOR
         LR    R15,R8
         BCTR  R15,0
         EX    R15,MOVESUBS
         LA    R7,1(R7,R8)
         SPACE 1
         LTR   R6,R6               ANY SOURCE DATA LEFT ???
         BP    CMLOOP
         SPACE 1
CM020    LA    R15,WSREC           COMPUTE LENGTH OF COMPRESSED RECORD
         SR    R7,R15
         SPACE 1
         BR    R9                  RETURN
         SPACE 3
TRTNONB  TRT   0(0,R5),NBTBL       ***** EXECUTED *****
TRTBLNK  TRT   0(0,R5),BLNKTBL     ***** EXECUTED *****
MOVESUBS MVC   1(0,R7),0(R14)      ***** EXECUTED *****
         EJECT
DECOMP   EQU   *
         MVC   0(1,R11),0(R5)      MOVE CARRIAGE CONTROL
         SPACE 1
         LA    R5,1(,R5)           ADVANCE   SOURCE ADDRESS
         S     R6,WSVAL1           DECREMENT LENGTH REMAINING
         LA    R7,1(,R11)          SET       TARGET ADDRESS (SKIP CC)
         SPACE 1
DCLOOP   TM    0(R5),X'80'         NULL BYTE COUNT ???
         BNO   DC001               NO - MUST BE DATA STRING
         SPACE 1
         IC    R15,0(R5)           LOAD LENGTH
         N     R15,WSMASK          REMOVE HIGH ORDER BITS
         BCTR  R15,0               DECREMENT LENGTH FOR "EX" INSTR
         EX    R15,MOVEBLNK        INSERT BLANKS
         LA    R7,1(R7,R15)        ADVANCE TARGET ADDRESS
         LA    R5,1(,R5)           ADVANCE SOURCE ADDRESS
         S     R6,WSVAL1
         BP    DCLOOP              LOOP    IF MORE   DATA
         BR    R9                  RETURN  IF SOURCE EXHAUSTED
         SPACE 1
DC001    SR    R15,R15             LOAD LENGTH OF DATA STRING
         IC    R15,0(,R5)
         BCTR  R15,0               DECREMENT FOR   "EX" INSTRUCTION
         EX    R15,MOVEDATA
         LA    R7,1(R7,R15)        ADVANCE   TARGET ADDRESS
         LA    R15,2(,R15)         RESTORE   DATA   LENGTH (+1)
         LA    R5,0(R5,R15)        ADVANCE   SOURCE ADDRESS
         SR    R6,R15              DECREMENT REMAINING LENGTH
         BP    DCLOOP              LOOP IF MORE DATA
         SPACE 1
         BR    R9                  RETURN IF SOURCE EXHAUSTED
         SPACE 3
MOVEBLNK MVC   0(0,R7),BLANKS      ***** EXECUTED *****
MOVEDATA MVC   0(0,R7),1(R5)       ***** EXECUTED *****
         EJECT
         DC    CL32'........ACB...BLOCK.1...........'
CRPACB1  ACB   AM=VSAM,                                                X
               DDNAME=ASABGCRF,                                        X
               EXLST=CRPEXLST,                                         X
               MACRF=(ADR,SEQ,OUT,NRS)
         SPACE 1
         DC    CL32'......GET..RPL..BLOCK.1.........'
VSRDFRPL RPL   ACB=CRPACB1,                                            X
               AM=VSAM,                                                X
               ARG=WSRBAVAL,                                           X
               AREA=WSRECADR,                                          X
               AREALEN=4,                                              X
               OPTCD=(ADR,SEQ,FWD,SYN,NUP,LOC)
         SPACE 1
         DC    CL32'......PUT..RPL..BLOCK.1.........'
VSWRTRPL RPL   ACB=CRPACB1,                                            X
               AM=VSAM,                                                X
               AREA=WSREC,                                             X
               AREALEN=133,                                            X
               RECLEN=133,                                             X
               OPTCD=(ADR,SEQ,SYN,MVE)
         SPACE 1
CRPEXLST EXLST EODAD=EOD           DEFINE END OF DATA ROUTINE ADDRESS
         EJECT
*
static   loctr
         DS   0D
         DC    CL32'........GVBMR66.....SAVE....AREA'
SAVEAREA DC  18F'0'            REGISTER  SAVE  AREA
         SPACE 1
WSRBAVAL DC    F'0'            RELATIVE  BYTE  ADDRESS (THESE TWO MUST)
WSRECLEN DC    A(0)            RECORD LENGTH           (BE ADJACENT)  )
WSRECADR DC    A(0)            RECORD ADDRESS (LOCATE MODE)
         SPACE 1
WSVAL1   DC    F'0001'
WSVAL4   DC    F'0004'
WSVAL5   DC    F'0005'
WSVAL6   DC    F'0006'
WSVAL127 DC    F'0127'
WSMASK   DC    X'0000007F'
WSLOWVAL DC    X'0000000000000000'
         SPACE 1
WSLOGERR DC    H'05'           LOGIC    ERROR
WSEOD    DC    H'06'           END   OF DATA
WSCODERR DC    H'07'           FUNCTION CODE ERROR
         SPACE 1
WSACBOPN DC    C'N'            CURRENT  MRF  VSAM DATA SET STATUS
         SPACE 1
BLANKS   DC    CL132' '
         SPACE 1
WSREC    DS   0CL133           RECORD   BUFFER
WSRECCC  DC    CL001' '        CARRIAGE CONTROL
WSDATA   DC    CL200' '        PRINT    DATA
         EJECT
*                    0 1 2 3 4 5 6 7 8 9 A B C D E F
*
BLNKTBL  DC    XL16'00000000000000000000000000000000'  00 - 0F
         DC    XL16'00000000000000000000000000000000'  10 - 1F
         DC    XL16'00000000000000000000000000000000'  20 - 2F
         DC    XL16'00000000000000000000000000000000'  30 - 3F
         DC    XL16'04000000000000000000000000000000'  40 - 4F
         DC    XL16'00000000000000000000000000000000'  50 - 5F
         DC    XL16'00000000000000000000000000000000'  60 - 6F
         DC    XL16'00000000000000000000000000000000'  70 - 7F
         DC    XL16'00000000000000000000000000000000'  80 - 8F
         DC    XL16'00000000000000000000000000000000'  90 - 9F
         DC    XL16'00000000000000000000000000000000'  A0 - AF
         DC    XL16'00000000000000000000000000000000'  B0 - BF
         DC    XL16'00000000000000000000000000000000'  C0 - CF
         DC    XL16'00000000000000000000000000000000'  D0 - DF
         DC    XL16'00000000000000000000000000000000'  E0 - EF
         DC    XL16'00000000000000000000000000000000'  F0 - FF
         SPACE 5
*                    0 1 2 3 4 5 6 7 8 9 A B C D E F
*
NBTBL    DC    XL16'04040404040404040404040404040404'  00 - 0F
         DC    XL16'04040404040404040404040404040404'  10 - 1F
         DC    XL16'04040404040404040404040404040404'  20 - 2F
         DC    XL16'04040404040404040404040404040404'  30 - 3F
         DC    XL16'00040404040404040404040404040404'  40 - 4F
         DC    XL16'04040404040404040404040404040404'  50 - 5F
         DC    XL16'04040404040404040404040404040404'  60 - 6F
         DC    XL16'04040404040404040404040404040404'  70 - 7F
         DC    XL16'04040404040404040404040404040404'  80 - 8F
         DC    XL16'04040404040404040404040404040404'  90 - 9F
         DC    XL16'04040404040404040404040404040404'  A0 - AF
         DC    XL16'04040404040404040404040404040404'  B0 - BF
         DC    XL16'04040404040404040404040404040404'  C0 - CF
         DC    XL16'04040404040404040404040404040404'  D0 - DF
         DC    XL16'04040404040404040404040404040404'  E0 - EF
         DC    XL16'04040404040404040404040404040404'  F0 - FF
         SPACE 3
         LTORG
*        SPACE 1
* BMR66  CSECT
*&STAMP   SETC  'GVBMR66 '.'&SYSDATE'.' '.'&SYSTIME'
*MR66STMP DC    C'&STAMP'
*         DC    AL4(GVBMR66)
*
         END