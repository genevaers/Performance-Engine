         TITLE 'GVBMRHPU - EXIT CALLED BY DB2HPU'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2022, 2023.
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
*
*  THIS MEMBER IS CALLED 3 TIMES DURING THE UNLOADING OF A TABLE.
*  R1 CONTAINS THE ADDRESS OF A PARAMETER BLOCK AS DESCRIBED BY
*  THE EXTXPLST DSECT.
*
*  THE MOST IMPORTANT FIELD IS THE POINTER TO THE SQLDA OF THE
*  PROCESSED TABLE WHICH GIVES DATAS IN EXTERNAL FORMAT.
*
*  FUNCTION 0 :
*   PROCESS OF THE DB2 ROW
*   RETURN CODES :
*    0   : ROW TO BE WRITTEN IN THE OUTPUT FILE
*    4   : ROW DISCARDED
*
*  FUNCTION 1 :
*   INITIALIZATION OF THE EXIT
*   RETURN CODES :
*    0   : EXIT ACTIVE FOR THIS SELECT STATEMENT
*    4   : DESACTIVATION OF THE EXIT FOR THIS SELECT
*
*  FUNCTION 2 :
*   TERMINATION OF THE EXIT
*   RETURN CODES NOT USED
*
***********************************************************************
*
         copy  GVBUTEQU
*
         YREGS
*
*        SQLDA DESCRIPTION
*
SQLDA    DSECT
SQLDAID  DS    CL8              ID
SQLDABC  DS    F                BYTE COUNT -- IS THIS LENGTH OF ROW ??
SQLN     DS    H                NUMBER OF COLUMNS
SQLD     DS    H                NUMBER OF SELECTED COLUMNS
SQLVAR   DS    0F               BEGIN COLUMNS
SQLDSIZ  EQU   *-SQLDA          SIZE OF FIXED PART
*
SQLVARN  DSECT                  COLUMN
SQLTYPE  DS    H                TYPE
SQLLEN   DS    0H               LENGTH
SQLPRCSN DS    X                DEC PRECISION
SQLSCALE DS    X                DEC SCALE
SQLDATA  DS    A                ADRESS OF DATA
SQLIND   DS    A                ADRESS NULL FIELD INDICATOR
SQLNAME  DS    H,CL30           SIZE AND COLUMN NAME
SQLVSIZ  EQU   *-SQLVARN
*
EXTXPLST DSECT                  PARMAMETERS PASSED TO THE EXIT
EXTXFUNC DS    F                FUNCTION (0 PROCESS, 1 INIT, 2 TERM)
EXTXASQL DS    A                ADDRESS OF SQLDA
EXTXATBN DS    A                ADDRESS OF TABLE IDENTIFICATOR
*                                      CREATOR(8) / TABLENAME(18)
         DS    H                RESERVED
EXTXNREF DS    H                REFERENCE NUMBERID
EXTXASSI DS    A                ADDRESS OF SSID(4)
EXTXAUSR DS    A                ADDRESS OF USER(8)
EXTXATID DS    A                ADDRESS OF UTILITY ID(16)
EXTXATB  DS    A                ADDRESS OF TABLE IDENTIFICATOR
*                                VARCHAR CREATOR / TABLENAME
         DS    5F               RESERVED
EXTXAUWA DS    A                ADDRESS OF WORKAREA
EXTXEXUE DS    A                EXUEXU ENTRY IN EXUEXU TABLE
EXTXTCB  DS    F                FIELDS RESERVED FOR THE EXIT
EXTXASOC DS    X                FIELDS RESERVED FOR THE EXIT
         DS    3X               FIELDS RESERVED FOR THE EXIT
         DS    F                FIELDS RESERVED FOR THE EXIT
EXTXAMSG DS    A                ADDRESSE OF MESSAGE
EXTXLMSG DS    F                LENGTH OF MESSAGE
EXTXUMSG DS    CL100            EXIT MESSAGE AREA
EXTXUWA  DS    0F               EXIT WORK AREA
         ORG   EXTXPLST+1024    END OF LIST
*
GETM     DSECT
SAVE     DS    18F              LOCAL SAVEAREA
TBCREATL DS    H                CREATOR LENGTH
TBCREAT  DS    CL160            TABLE CREATOR
TBNAMEL  DS    H                TABLE NAME LENGTH
TBNAME   DS    CL160            TABLE NAME
*
WKREENT  DS    XL128            RE-ENTRANT  PARAMETER  LIST
WKTOKNRC DS    A                NAME/TOKEN  SERVICES RETURN CODE
WKTOKNAM DS    XL16             TOKEN NAME
WKTOKN   DS    XL16             TOKEN
WKEXU1ST DS    A                FIRST EXUEXU ENTRY
WKEXUNUM DS    H                NUMBER OF EXUEXU ENTRIES
         DS    XL2              WORKAREA
MSG_AREA GVBMSG PREFIX=MMM,MF=L
PRNTBUFF DS    CL72            
WORK     DS    CL104            Should be 840 bytes           
*
GETML    EQU   *-GETM           LENGTH
*
         COPY  GVBASSRT
         COPY  GVBMR95C
         COPY  GVBMR95L
         COPY  GVB0200B
*
INZEXIT  RMODE 24
INZEXIT  AMODE 31
INZEXIT  CSECT
         STM   R14,R12,12(R13)  SAVE CALLERS REGISTERS
         LR    R12,R15          R12=BASE REGISTER
         USING INZEXIT,R12      ESTABLISH ADDRESSABILITY
         LR    R10,R1           GET PARAMETERS
         USING EXTXPLST,R10     "
         L     R11,EXTXAUWA     R11=WORKAREA ADDRESS
         USING GETM,R11         WORKAREA ADDRESSABILITY
         ST    R11,8(R13)       GIVE CALLER MY SAVE AREA ADDRESS
         ST    R13,4(,R11)      SAVE CALLERS SAVE AREA ADDRESS
         LR    R13,R11
         L     R2,EXTXFUNC      GET FUNCTION
         SLL   R2,2             * 4 FOR DISPACHING
         B     *+4(R2)
         B     PROCESS          FUNCTION 0
         B     INIT             FUNCTION 1
         B     TERM             FUNCTION 2
*
***      INITIALIZATION
*
INIT     DS    0H
*
*  A READY-TO-USE WORKAREA OF 840 BYTES IS ALLOCATED BY THE CALLING
*  FUNCTION.
*
         L     R3,EXTXATB       ADDRESS OF TABLE IDENTIFICATOR
*
         L     R1,0(R3)         TABLE CREATOR VARCHAR
         LH    R2,0(R1)         LENGTH
         STH   R2,TBCREATL      STORE LENGTH
         BCTR  R2,0             FOR EXECUTE
         EX    R2,MVCOWNER      COPY OWNER
*
*
         L     R1,4(R3)         TABLE NAME VARCHAR
         LH    R2,0(R1)         LENGTH
         STH   R2,TBNAMEL       STORE LENGTH
         BCTR  R2,0             FOR EXECUTE
         EX    R2,MVCNAME       COPY TABLE NAME
*
         MVC   WKTOKNAM+0(8),GENEVA
         MVC   WKTOKNAM+8(8),PGMNAME
*
         CALL  IEANTRT,(TOKNLVL2,WKTOKNAM,WKTOKN,WKTOKNRC),            X
               MF=(E,WKREENT)
         L     R15,WKTOKNRC       SUCCESSFUL   ???
         LTR   R15,R15
         JZ    INIT0010
         GVBMSG WTO,MSGNO=DB2_HPU_TOKN,SUBNO=1,                        +
               SUB1=(MODNAME,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         J     RC4
*
INIT0010 EQU   *
         L     R8,WKTOKN        R8 => EXUEXU TABLE HEADER
         LTR   R8,R8
         JP    INIT0012
         DC    H'0'             Should not happen, ever.
         J     RC4
*
INIT0012 EQU   *
         MVI   EXTXASOC,X'00'   NOT ALREADY ASSOCIATED
         AHI   R8,-16           BACK UP TO EXUEXU HEADER
         USING EXHEXH,R8
         LGH   R9,EXHPARLL      NUMBER OF TABLE ENTRIES
         STH   R9,WKEXUNUM      REMEMBER NUMBER OF TABLE ENTRIES
         DROP  R8
         AHI   R8,16            FORWARD TO FIRST TABLE ENTRY
         ST    R8,WKEXU1ST      REMEMBER FIRST TABLE ENTRY
         USING EXUEXU,R8
*
         ENQ (GENEVA,MRSUNAME,E,,STEP),RNL=NO
*
INIT0016 EQU   *
         CLI   EXUASSOC,X'FF'   EXUEXU SLOT ALREADY USED ?
         JNE   INIT0018         NO, GO
         LA    R8,EXUEXUL(,R8)
         BRCT  R9,INIT0016
         DC    H'0'             SHOULDN'T EVER RUN OUT OF TABLE ENTRIES
*          AS PARALLEL PARAMETER WAS USED TO REQUEST NUMBER OF SUBTASKS
INIT0018 EQU   *
         XC    EXTXTCB,EXTXTCB
         MVI   EXTXASOC,X'FF'   ALREADY ASSOCIATED NOW
         MVI   EXUASSOC,X'FF'   EXUEXU SLOT NOW USED
         ST    R8,EXTXEXUE      REMEMBER EXUEXU ENTRY
*
         LLGT  R1,WKTOKN        EXUEXU HEADER
         AHI   R1,-16           BACK UP TO EXUEXU HEADER
         USING EXHEXH,R1
         LGH   R0,EXHINIT       COUNT OF INITIALIZATIONS
         AHI   R0,1             INCREMENT
         STH   R0,EXHINIT
         DROP  R1
*
         DEQ (GENEVA,MRSUNAME,,STEP),RNL=NO
*
         B     RC0
*
MVCOWNER MVC   TBCREAT,2(R1)
MVCNAME  MVC   TBNAME,2(R1)
*
***      PROCESS RETURNED ROW
*
PROCESS  DS    0H
         USING PSA,R0
         L     R8,PSATOLD
         NC    EXTXTCB,EXTXTCB
         JZ    A0050
         C     R8,EXTXTCB
         JE    A0050
         DC    H'0'             CHANGE OF TCB SHOULD NEVER HAPPEN
A0050    EQU   *
         ST    R8,EXTXTCB       STORE THIS ONE
         DROP  R0
*
         L     R8,EXTXEXUE
         MVI   EXUSTAT,C'1'     FILLING BUFFER
*
         L     R3,EXTXASQL      GET SQLDA ADDRESS
         USING SQLDA,R3
         LA    R4,SQLDSIZ(,R3)  PASS FIXED AREA
         USING SQLVARN,R4
*
*        FIRST TIME DETERMINE THE ROW LENGTH
*
         LT    R9,EXUROWLN
         JP    A0140
         XR    R9,R9            CUMULATIVE ROW LENGTH
         XR    R5,R5            GET NUMBER OF COLUMNS
         LH    R5,SQLN
A0144    EQU   *
         LH    R7,SQLLEN
         AR    R9,R7
         LA    R4,SQLVSIZ(,R4)
         BCT   R5,A0144         NEXT COLUMN
         ST    R9,EXUROWLN
*
*        PROCESS ALL COLUMNS
*
A0140    EQU   *
         ASI   EXURNUM,1        INCREMENT RECORD COUNT
         L     R1,EXURPOS       THIS RECORD
         LR    R0,R1
         AR    R0,R9
         C     R0,EXURLAST      ENOUGH BYTES LEFT FOR IT TO FIT ?
         JNH   A0147            YES, THERE SHOULD ALWAYS BE
         DC    H'0'
A0147    EQU   *
         ST    R0,EXURPOS       NEXT RECORD
         AR    R0,R9
*
         C     R0,EXURLAST      2 * ENOUGH BYTES LEFT FOR IT TO FIT ?
         JNH   A0146            YES, GO
         MVI   EXUWAIT,C'Y'     NO, THIS ONE FITS THEN WE MUST WAIT
*
A0146    EQU   *
         LA    R4,SQLDSIZ(,R3)  PASS FIXED AREA
         XR    R5,R5            GET NUMBER OF COLUMNS
         LH    R5,SQLN
*
         LR    R9,R1
*
*        PROCESS EACH COLUMN
*
A0160    EQU   *
         LH    R7,SQLLEN
         BCTR  R7,0
*
         L     R6,SQLDATA
         EX    R7,MVCFIELD
         LA    R1,1(R7,R1)
*
         LA    R4,SQLVSIZ(,R4)
         BCT   R5,A0160         NEXT COLUMN
*
         CLI   EXUWAIT,C'Y'
         JE    A0170            JUST STORE A RECORD IN THE BLOCK
         J     A0180            END PROCESS OK
*
A0170    EQU   *
         MVI   EXUSTAT,C'2'     BUFFER FULL
         XC    EXUECBEX,EXUECBEX  ----
*
         POST  EXUECBMA         RETURN A RECORD BLOCK TO MR95
         WAIT  1,ECB=EXUECBEX   WAIT FOR MR95 TO CONSUME THE BLOCK
         MVI   EXUWAIT,C' '       RESET THE NEED TO POST/WAIT FLAG
         XC    EXURNUM,EXURNUM    RESET NUMBER OF RECORDS IN BLOCK
         LLGT  R0,EXUBLKA         RESET RECORD POSITION IN BLOCK
         ST    R0,EXURPOS
*
A0180    EQU   *
         B     RC4              END PROCESS OK (RETURN NO RECORD)
*
MVCFIELD MVC   0(0,R1),0(R6)
*
*        EXIT HAS FINISHED UNLOADING RECORDS
*
TERM     EQU   *
         ENQ (GENEVA,MRSUNAME,E,,STEP),RNL=NO
         LLGT  R1,WKTOKN        EXUEXU HEADER
         AHI   R1,-16           BACK UP TO EXUEXU HEADER
         USING EXHEXH,R1
         LGH   R0,EXHFINI       COUNT OF TERMINATIONS
         AHI   R0,1             INCREMENT
         STH   R0,EXHFINI
         DROP  R1
         DEQ (GENEVA,MRSUNAME,,STEP),RNL=NO
*
         L     R8,EXTXEXUE
         MVI   EXUEOF,C'Y'
         MVI   EXUSTAT,C'2'     BUFFER FULL AS IT'S GOING TO GET
         POST  EXUECBMA         SAY WE'VE FINISHED
*
         B     RC0              END TERMINATION OK
*
RC0      EQU   *
         LA    R15,0
         B     RETURN
RC4      EQU   *
         LA    R15,4
         B     RETURN
RC8      EQU   *
         LA    R15,8
RETURN   EQU   *
         L     R13,SAVE+4
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BSM   0,R14
*
MODNAME  DC    CL8'INZEXIT'           Module name
TOKNLVL2 DC    A(2)                   NAME/TOKEN  AVAILABILITY  LEVEL
GENEVA   DC    CL8'GENEVA  '          TOKEN  NAME
PGMNAME  DC    CL8'GVBMRSU '
MRSUNAME DC    CL8'MRSUEXA '          MINOR  ENQ  NODE FOR WRITE I/O
TOKNPERS DC    F'0'
IEANTRT  DC    V(IEANTRT)
         LTORG ,
*
         DCBD  DSORG=PS
*
         IHADCBE
*
         IHAPSA
*
         END
