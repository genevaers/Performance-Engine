         TITLE 'GVBMR88 - SUMMARIZE AND FORMAT EXTRACT FILE RECORDS'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2001, 2022.
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
*  GVBMR88 - OUTPUTS SORTED  EXTRACT DATA                             *
*          - ACCUMULATES     COLUMN  TOTALS                           *
*          - PRINTS ACCUMULATORS AT  CONTROL BREAKS                   *
*                                                                     *
*          - BOTH SUMMARY AND DETAIL REQUESTS ARE SUPPORTED           *
*                                                                     *
*          - DESTINATIONS INCLUDE   (ONLINE, FILES, PRINT)            *
*                                                                     *
*          - LOADS THE VIEW DEFINITION PARAMETERS,  OPENS REQUIRED    *
*            INPUT FILES,  LOADS MEMORY RESIDENT TITLE LOOK-UP        *
*            TABLES, INITIALIZES BINARY SEARCH PATHS.                 *
*                                                                     *
*  FUNCTION CODES:                                                    *
*                                                                     *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - SUCCESSFUL                                          *
*            8  - ERROR                                               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK REGISTER                                *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - TEMPORARY WORK REGISTER                                *
*            - INTERNAL  SUBROUTINE  RETURN  ADDRESS (3RD LEVEL)      *
*            - RETURN    ADDR                                         *
*                                                                     *
*        R13 - REGISTER  SAVE AREA  ADDRESS  (WORK AREA BASE REGISTER)*
*                                                                     *
*        R12 -                                                        *
*        R11 - PROGRAM   BASE REGISTER                                *
*                                                                     *
*        R10 - INTERNAL  SUBROUTINE  RETURN  ADDRESS (1ST LEVEL)      *
*        R9  - INTERNAL  SUBROUTINE  RETURN  ADDRESS (2ND LEVEL)      *
*                                                                     *
*        R8  - REPORT    REQUEST RECORD      ADDRESS                  *
*                                                                     *
*        R7  - CURRENT   EXTRACT RECORD      ADDRESS                  *
*                                                                     *
*        R6  - CURRENT   SORT    KEY   DEFN  ADDRESS                  *
*            - CURRENT   COLUMN  DEFINITION  ADDRESS                  *
*            - LOOK-UP   BUFFER  PREFIX      ADDRESS                  *
*                                                                     *
*        R5  - LOOP      COUNTER                                      *
*            - CONDITION CODE                                         *
*            - CURRENT   REFERENCE   RECORD  ADDRESS (TABLE LOOK-UPS) *
*                                                                     *
*        R4  - WORK      REGISTER                                     *
*            - OPERAND   2 ADDRESS                                    *
*            - CURRENT   REFERENCE   RECORD      KEY  LENGTH          *
*                                                                     *
*        R3  - WORK      REGISTER                                     *
*            - OPERAND   1 ADDRESS                                    *
*                                                                     *
*        R2  - WORK      REGISTER                                     *
*            - BRANCH/EXIT ADDRESS                                    *
*            - CURRENT   OCCURRENCE  NUMBER                           *
*                                                                     *
*        R1  - PARAMETER LIST ADDRESS                                 *
*            - TEMPORARY WORK REGISTER                                *
*                                                                     *
*        R0  - TEMPORARY WORK REGISTER                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         Copy  GVBASSRT
*
         COPY  GVBMR88W
         COPY  GVBMR88C
         COPY  GVB1300A
*
VDP1300LN EQU  *-VDP1300_TITLE_LINES_RECORD
*
         COPY  GVB1400A
*
VDP1400LN EQU  *-VDP1400_FOOTER_LINES_RECORD
*
         COPY  GVB1600A
*
         copy  gvbrptit
         copy  gvbphead
         copy  reptdata

         COPY  GVBUTEQU

MSGLIST  GVBMSG DSECT

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        REGISTER EQUATES AND SAVE AREA OFFSETS                       *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         YREGS
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

                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* 1.  OBTAIN AND INITIALIZE  PROGRAM WORK AREA                        *
* 2.  CALL INITIALIZATION SUBROUTINE (GVBMR87) TO ALLOCATE BUFFERS    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*        PRINT NOGEN
*
         SYSSTATE ARCHLVL=2
         print off
         COPY  ASMMSP
LEAVE    OPSYN ASM_LEAVE
         asmmrel on
         print on
         ihasaver

GVBMR88  RMODE 31
GVBMR88  AMODE 31
*
         ENTRY CALCBRN
         ENTRY CALCEQ
         ENTRY CALCNE
         ENTRY CALCGT
         ENTRY CALCGE
         ENTRY CALCLT
         ENTRY CALCLE
         ENTRY CALCPSHV
         ENTRY CALCPSHC
         ENTRY CALCADD
         ENTRY CALCSUB
         ENTRY CALCMULT
         ENTRY CALCDIV
         ENTRY CALCNEG
         ENTRY CALCABS
*
         ENTRY CALCDONE
         ENTRY EXCPACPT
         ENTRY EXCPSKIP
*
GVBMR88  CSECT
         J     START
MR88EYE  GVBEYE GVBMR88
*
static   loctr                    define the static section
code     loctr                    make sure we are in the correct loctr
*
         using saver,r13
START    STM   R14,R12,savgrs14   SAVE CALLER'S  REGISTERS
*
         larl  r11,gvbmr88        set static area base
         USING (GVBMR88,code),R11
*
         LR    R9,R1              SAVE PARM DATA ADDRESS
         LR    R10,R13            SAVE OLD  RSA  ADDRESS
prevsa   using saver,r10
*
         LHI   R0,WORKLEN+l'workeyeb   LOAD WORK AREA  LENGTH
         GETMAIN R,LV=(0)         GET  WORK AREA
         MVC   0(l'workeyeb,R1),WORKEYEB    COPY EYEBALL
         LA    R13,l'workeyeb(,R1)         USE  NEW  RSA  (WORK AREA)
         USING WORKAREA,R13
         using saver,workarea
*
         LR    R0,R13             ZERO OUT  WORK  AREA (LOW VALUES)
         LHI   R1,WORKLEN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         ST    R10,savprev        SET  BACKWARDS  POINTER  IN  NEW
         ST    R13,prevsa.savnext SET  FORWARD    POINTER  IN  OLD
         drop  prevsa
*
*  Note: z/OS standard linkage defines the first 8 FPR as volatile so
*        they are used without saving
*
*   SAFR uses extended format DFP numbers - these are 16 bytes in
*   storage and always require a pair of FP registers
*
*   FP8/10 is loaded with a floating point zero - the exponent is
*   unpredictable
*   FP9/11 will be loaded with the quantum value for 3 or 8 decimal
*   places during GVBMR87 processing - these registers must be left
*   as is as code here is dependent on these values (also DL96)
*
         lay   r14,fp_reg_savearea  Point at register save area

         GVBSTX   fp8,0(0,r14)       save fp8 to fp15
         GVBSTX   fp9,16(0,r14)
         GVBSTX   fp12,32(0,r14)
         GVBSTX   fp13,48(0,r14)

         L     R0,0(,R9)          LOAD PARM LEN   ADDR   FROM  PLIST
         ST    R0,PARMDADR        SAVE PARAMETER   LEN   ADDR
                     SPACE 3
***********************************************************************
*  SAVE BEGINNING DATE/TIME                                           *
***********************************************************************
*
         XC    begtime,begtime    GET  BEGINNING  TIME
         lay   R2,begtime
         lay   R3,timelist
         TIME  STCK,(R2),LINKAGE=SYSTEM,DATETYPE=YYYYMMDD,             +
               MF=(E,(R3))
*
* Set program mask before SORT is invoked
* This means records processed after SORT has completed will
* use this mask setting (SORT restores program mask on completion)
*
* Turning the bit on means overflow will produce 0CA abend
* Turning the bit off means overflow will set condition code
*
         ipm   r14                load current program mask
         st    r14,wksavmsk
*        oilh  r14,b'0000010000000000' turn on decimal overflow
         nilh  r14,b'1111101111111111' turn off decimal overflow
         spm   r14
***********************************************************************
*  Initialization - call MR87
*
*    Note: portions of MR87 are called at other times in order to
*          return records from the sort E15/E35. These can also
*          result in non zero return codes here via it's own RTNERROR.
*
***********************************************************************
         L     R15,GVBMR87A       LOAD "GVBMR87"   ADDRESS
         BASR  R14,R15            CALL "GVBMR87" - INITIALIZATION

*        MR87 will have loaded a zero in fp8/10
*        and the quantum value into fp9/11
*
         LTR   R14,R15            SUCCESSFUL INITIALIZATION ???
         JNZ   RTNERROR           NO  - ERROR (MESSAGE NO.)
*
         MVC   PRNTSUBR,VSAMWRTA  INITIALIZE PRINT ROUTINE ADDRESS
         MVC   READSUBR,MERGEADR  INITIALIZE READ  ROUTINE ADDRESS
*
*        OPEN  (SNAPDCB,(OUTPUT)),MODE=31
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* 1.  SET  END-OF-FILE EXIT ROUTINE ADDRESS IN  DCB'S                 *
* 2.  READ ALL EXTRACT FILE CONTROL RECORDS AND SUM RECORD COUNTS     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         CLI   EXTREOF,C'Y'       EMPTY EXTRACT FILE  ???
         JE    READEOF            EOF Processing       rtc20958
*
         L     R1,EXTRDCBA        LOAD DCB  ADDRESS
         Larl  R14,EXTREND        SET  END-OF-FILE     ADDRESS
         STCM  R14,B'0111',DCBEODA-IHADCB(R1)
*
         L     R9,READSUBR        INITIALIZE  BASE  REGISTER
         llgf  R15,MERGENXT       LOAD SECONDARY ENTRY POINT ADDRESS
         BASSM R10,R15            SELECT  LOWEST KEY  RECORD
         ST    R1,RECADDR
         LR    R7,R1              LOAD NEXT  EXTRACT  RECORD ADDRESS
         USING CTLREC,R7
*
         OC    CTVIEW#,CTVIEW#    CONTROL RECORD (NO REPORT ID)  ???
         JZ    CTRLREC            YES - CONTINUE
*
         LHI   R14,MSG#400        ASSUME MISSING CONTROL REC
         L     R1,PARMDADR        CHECK PARAMETER OPTION
         CLI   NOHDROPT-PARMDATA(R1),C'Y'  HEADER RECORDS   PRESENT ???
         JNE   RTNERROR           YES -   CONTROL RECORD MUST BE  FIRST
*
         XC    SVFILENO,SVFILENO  INITIALIZE FIELDS SAVED FROM HEADER
         MVI   SVFINPDT,C'9'
         MVC   SVFINPDT+1(L'SVFINPDT-1),SVFINPDT
         ZAP   SVRECCNT,P000
         ZAP   VWRECCNT,P000
         MVI   VWSATIND,C'Y'
         MVI   VW0C7IND,C'N'
         MVI   VWOVRIND,C'N'
*
         J     CHKHDR             CHECK IF  FIRST RECORD A HEADER REC
*
CTRLREC  MVC   SVFILENO,CTFILENO  SAVE  THE EXTRACT FILE NUMBER
         MVC   SVFINPDT,CTFINPDT  SAVE  THE FINANCIAL    PERIOD   DATE
         ZAP   SVRECCNT,CTRECCNT  SAVE  THE FILE  RECORD COUNT

CTRLLOOP llgf  R15,READSUBR       LOAD  GET SUBROUTINE ADDRESS
         BASSM R10,R15            READ  THE NEXT  EXTRACT FILE RECORD
         ST    R1,RECADDR
         LR    R7,R1              LOAD DATA ADDRESS
*
         OC    CTVIEW#,CTVIEW#    CONTROL RECORD (NO REPORT ID) ???
         JNZ   CHKHDR             NO  - CHECK FOR HEADER RECORD
*
         AP    SVRECCNT,CTRECCNT  SUM   THE FILE  RECORD COUNTS
         J     CTRLLOOP           LOOP THROUGH ALL CONTROL RECORDS
*
         DROP  R7
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* 1.  READ  THE NEXT   EXTRACT RECORD                                 *
* 2.  CHECK FOR A      REPORT  BREAK                                  *
* 3.  READ  ALL REPORT HEADER  RECORDS AND  SUM  RECORD COUNTS        *
* 4.  CHECK FOR EMPTY  VIEW   (NO EXTRACT RECORDS)                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
READLOOP llgf  R15,READSUBR       LOAD GET  SUBROUTINE ADDRESS(24-BIT)
         BASSM R10,R15            READ THE  NEXT  EXTRACT FILE RECORD
*
NEXTEXT  ST    R1,RECADDR
         LR    R7,R1              LOAD THE  DATA  ADDRESS
                     SPACE 3
         USING HDRREC,R7
CHKHDR   L     R0,HDVIEW#         REPORT BREAK  ???
         SRL   R0,1               STRIP  HEADER INDICATOR
         ST    R0,HDVIEW#
         C     R0,SVVIEW#
         JE    EXTPROC            NO  -  BYPASS BREAK LOGIC
*
         MVI   EOREOFCD,C'H'      INDICATE  READING HEADERS
*
         LR    R1,R7              POINT  TO   HEADER  RECORD (PARM)
         BRAS  R10,VALIDHDR       VERIFY THAT IT IS A HEADER RECORD
         LTR   R15,R15            VALID  HEADER   RECORD ???
         JZ    HDRINIT            YES -  CONTINUE WITH   INITIALIZATION
*
         MVI   EOREOFCD,C' '      RESET END-OF-REQUEST   FLAG
*
         L     R1,PARMDADR        CHECK PARAMETER OPTION
         CLI   NOHDROPT-PARMDATA(R1),C'Y'  HEADER RECORDS PRESENT ???
         JNE   MSTRVIEW           YES -    BYPASS SPECIAL LOGIC
         OC    HDVIEW#,HDVIEW#    CONTROL  RECORD ???
         JZ    READLOOP           YES -    IGNORE
*
         OC    SVVIEW#,SVVIEW#    FIRST    REQUEST ???
         JZ    SETREQNO           YES - NO BREAK   LOGIC  REQUIRED
*
         MVC   SAVERECA,RECADDR   SAVE     NEXT   RECORD  ADDRESS
         MVC   RECADDR,PREVRECA   RESTORE  LAST   RECORD  ADDRESS
         MVI   EOREOFCD,C'R'      INDICATE END-OF-REQUEST
         BRAS  R10,RPTBREAK       PROCESS  BREAK
         MVC   RECADDR,SAVERECA   RESTORE  NEXT   RECORD  ADDRESS
*
SETREQNO MVC   SVVIEW#,HDVIEW#
         J     RBRKNEW
                     SPACE 3
MSTRVIEW DS    0H
         LHI   R14,MSG#401        ASSUME INCORRECT REC TYPE
         CLI   LASTFILE,C'M'      DID RECORD COME FROM MASTER  ???
         JNE   RTNERROR           NO  -  INDICATE ERROR
*
MSTRDELC Larl  R15,DELCHeck       LOAD  DELETED  VIEW SUBROUTINE  ADDR
         BASR  R10,R15            CHECK IF VIEW  HAS  BEEN DELETED ???
*
MSTRCOPY L     R1,MARGDCBA        LOAD  MARGINAL FILE DCB ADDRESS
         MVC   DCBLRECL-IHADCB(2,R1),0(R7)  GET CORRECTLY SIZED BUFFER
         L     R15,MARGPUTA       LOAD SUBROUTINE ADDRESS  (31-BIT)
         BASR  R14,R15            CALL
*
         AP    MARGCNT,P001       INCREMENT COUNTERS
         LR    R0,R1              LOAD  TARGET ADDRESS
         LH    R1,0(,R7)          LOAD  SOURCE LENGTH
         LR    R15,R1             LOAD  TARGET LENGTH (SAME)
         LR    R14,R7             LOAD  SOURCE ADDRESS
         MVCL  R0,R14             COPY  RECORD
*
         llgf  R15,READSUBR       LOAD  GET SUBROUTINE ADDRESS
         BASSM R10,R15            READ  THE NEXT    EXTRACT FILE RECORD
*
         CLI   LASTFILE,C'M'      RECORD CAME  FROM MASTER  FILE    ???
         JNE   NEXTEXT            NO  -  CHECK IF   VALID   HEADER
*
         CLC   EXVIEW#-EXTREC(4,R1),EXVIEW#-EXTREC(R7) SAME REQUEST ???
         JNE   NEXTEXT            NO  -  CHECK IF   VALID   HEADER
         LR    R7,R1
         J     MSTRCOPY           YES - CONTINUE COPY
                     SPACE 3
HDRINIT  LA    R14,HDSORTKY       COMPUTE ADDRESS OF HEADER DATA
         AH    R14,HDSORTLN
         AH    R14,HDTITLLN
         USING HDRDATA,R14
*
         ZAP   VWRECCNT,HDRECCNT  SAVE REQUEST CONTROL    COUNT
         MVC   VWSATIND,HDSATIND  SAVE REQUEST SATISFIED  INDICATOR
         MVC   VW0C7IND,HD0C7IND  SAVE REQUEST 0C7 ABEND  INDICATOR
         MVC   VWOVRIND,HDOVRIND  SAVE REQUEST OVER LIMIT INDICATOR
         DROP  R14
                     SPACE 3
HDRLOOP  llgf  R15,READSUBR
         BASSM R10,R15
         ST    R1,RECADDR         SAVE NEXT  RECORD'S  ADDRESS
*                                (R7 - STILL POINTS TO PREVIOUS RECORD)
         L     R0,HDVIEW#-HDRREC(,R1)      STRIP HEADER  INDICATOR
         SRL   R0,1
         ST    R0,HDVIEW#-HDRREC(,R1)
*
         BRAS  R10,VALIDHDR                CHECK IF HEADER  RECORD
         LTR   R15,R15
         JZ    HDRSAME                     BRANCH  IF   HEADER
         MVC   SAVERECA,RECADDR            SAVE    NEXT RECORD ADDRESS
         MVC   RECADDR,PREVRECA            RESTORE LAST RECORD ADDRESS
         MVI   EOREOFCD,C'R'               INDICATE END-OF-REQUEST
         BRAS  R10,RPTBREAK                PROCESS BREAK
         MVC   RECADDR,SAVERECA            RESTORE NEXT RECORD ADDRESS
         J     RBRKNEW                     SET-UP  FOR  NEW    REQUEST
*
HDRSAME  CLC   HDVIEW#,HDVIEW#-HDRREC(R1)  SAME REQUEST NO AS PREV ???
         JE    HDRSUM                      YES - SUM THE RECORD COUNTS
*
         MVC   SAVERECA,RECADDR            SAVE  NEXT  HEADER RECORD
         ST    R7,RECADDR                  USE CURRENT HEADER RECORD
         BRAS  R10,RPTBREAK                PROCESS BREAK  FOR PREV
         Larl  R15,noextrec                PRINT NO EXTRACT RECORD MSG
         BASR  R10,R15
*
         L     R7,SAVERECA                 LOAD   HEADER RECORD  BASE
         ST    R7,RECADDR                  RESTORE  NEXT REQUEST HEADER
         J     HDRINIT                     PROCESS  NEXT REQUEST HEADER
                     SPACE 3
HDRSUM   LA    R14,HDSORTKY-HDRREC(,R1)  POINT TO DATA IN NEXT HEADER
         AH    R14,HDSORTLN-HDRREC(,R1)
         AH    R14,HDTITLLN-HDRREC(,R1)
*
         AP    VWRECCNT,HDRECCNT-HDRDATA(L'HDRECCNT,R14)  SUM COUNTS
*
         CLI   HDSATIND-HDRDATA(R14),C'N'      COPY SATISFIED INDICATOR
         JNE   HDRSUM01
         MVC   VWSATIND,HDSATIND-HDRDATA(R14)
*
HDRSUM01 EQU   *
         CLI   HD0C7IND-HDRDATA(R14),C'Y'      COPY 0C7 ABEND INDICATOR
         JNE   HDRSUM02
         MVC   VW0C7IND,HD0C7IND-HDRDATA(R14)
*
HDRSUM02 EQU   *
         CLI   HDOVRIND-HDRDATA(R14),C'Y'      COPY OVERLIMIT INDICATOR
         JNE   HDRSUM03
         MVC   VWOVRIND,HDOVRIND-HDRDATA(R14)
*
HDRSUM03 EQU   *
         J     HDRLOOP
*
         DROP  R7
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P R O C E S S   N E W   R E P O R T
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING VIEWREC,R8
RBRKNEW  L     R7,RECADDR         LOAD NEXT RECORD'S ADDRESS
         USING EXTREC,R7
*
         MVI   EOREOFCD,C' '      CLEAR  END-OF-REQUEST FLAG
*
         LHI   R14,MSG#402        ASSUME VIEW#  NOT  ON LIST
         L     R8,VWCHAIN         LOAD   FIRST  REQUEST ADDRESS
*
RBRKSRCH LTR   R8,R8              END-OF-LIST ???
         JNP   RBRKNOT            YES - REQUEST NOT FOUND
         CLC   EXVIEW#,VWVIEW#    MATCHING REQUEST NUMBERS ???
         JE    RBRKINIT           YES - PERFORM INITIALIZATION
         L     R8,VWNEXT          LOAD  ADDRESS OF NEXT LIST ENTRY
         J     RBRKSRCH           LOOP  THROUGH ENTIRE  LIST
*
RBRKNOT  CLI   LASTFILE,C'M'      DID RECORD COME FROM MASTER FILE ???
         JE    MSTRDELC           YES - DROP RECORDS (NO LONGER IN VDP)
         L     R0,EXVIEW#         NO  - DISPLAY REQUEST NUMBER
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  ERRDATA+0(9),DBLWORK
         J     RTNERROR
                     EJECT
RBRKINIT MVC   SVVIEW#,EXVIEW#    SAVE    NEW  VIEW  NUMBER
         MVI   VWPROC,C'P'        This view is processed in this run 
*                                 (there could be views in VDP not in
*                                  this extract file)                
*                                                                    
         LH    R0,VWMAXFTR        EXCLUDE FOOTER LINES FROM PAGE SIZE
         LTR   R0,R0
         JNP   RBRKIN01
         AHI   R0,1
         LCR   R0,R0
         AH    R0,VWPAGSIZ
         STH   R0,VWPAGSIZ
*
RBRKIN01 EQU   *
         MVI   PRINTIND,C'Y'      INDICATE    USE   PRINT   AREA
         TM    VWFLAG2,VWPRINT
         JO    RBRKIN02
         MVI   PRINTIND,C'N'
*
RBRKIN02 EQU   *
         TM    VWFLAG1,VWZEROSP   ZERO SUPPRESS OPTION  ???
         JNO   RBRKIN03
         OC    VWCLCCOL,VWCLCCOL  ANY  "CT"   COLUMNS   ???
         JNZ   RBRKIN03
         NI    VWFLAG1,255-VWZEROSP     TURN  OFF ZERO  SUPPRESS
*
RBRKIN03 EQU   *
         L     R1,VSAMWRTA        INITIALIZE  PRINT ROUTINE ADDRESS
*
         ST    R1,PRNTSUBR
*
         L     R0,SVSORTKY        SAVE NEW SORT KEY
         LA    R14,EXSORTKY
         LH    R15,EXSORTLN
         LR    R1,R15
         MVCL  R0,R14
*
         L     R0,CLCCOLTB        INITIALIZE  CALC DEFN   TABLE (X'FF')
         LH    R1,VWMAXCOL
         SLL   R1,2
         SR    R14,R14
         LA    R15,X'FF'
         SLL   R15,24
         MVCL  R0,R14
*
         L     R0,CLCOFFTB        INITIALIZE  CALC OFFSET TABLE (X'FF')
         LH    R1,VWMAXCOL
         SLL   R1,2
         SR    R14,R14
         LA    R15,X'FF'
         SLL   R15,24
         MVCL  R0,R14
*
         XC    VWLINENO,VWLINENO  RESET  LINE   NUMBER
         XC    NEEDTITL,NEEDTITL  RESET  TITLE  LINES   COUNTER
         ZAP   VWPAGENO,P000      RESET  PAGE   NUMBER
         MVI   GTOTLIND,C' '      RESET  PRINT  GRAND   TOTALS  IND
*
         SR    R0,R0              ASSUME NO SORT TITLES
         CLI   PRINTIND,C'Y'      PRINT  AREA    USED ???
         JNE   RBRKIN04
         LHI   R0,13
RBRKIN04 EQU   *
         STH   R0,VWMAXTTL        SET    MINIMUM
*
         XC    VWCENTER,VWCENTER  ASSUME NO CENTERING ADJUSTMENT
*

***********************************************************************
*  FLUSH EXTRACT RECORDS IF VIEW "NOT SATISFIED" UNLESS LIMIT STATED  *
***********************************************************************
         CLI   VWSATIND,C'N'      REQUEST "NOT SATISFIED" ??
         JNE   RBRKSAT            NO  - PROCESS   OUTPUT
         CLI   VWOVRIND,C'Y'      OVER  EXTRACTION LIMIT ???
         JNE   FLUSH              NO  - IGNORE OUTPUT IF OTHER ERRORS
         CP    VWLIMIT,P000       OUTPUT LIMIT SPECIFIED ???
         JNE   RBRKSAT            YES - FULL OUTPUT  NOT REQUIRED
         J     FLUSH              NO  - SKIP EXTRACT RECORDS
                     SPACE 3
RBRKSAT  DS    0H
RBRKCHKF TM    VWFLAG2,VWOUTDCB   OUTPUT FILE   DESTINATION ???
         JO    RBRKOPEN           YES -  OPEN   FILE
         TM    VWFLAG2,VWPRINT    PRINT  FILE   ???
         JNO   RBRKVSAM           NO  -  BYPASS OUTPUT FILE OPEN
*
RBRKOPEN L     R2,VWDCBADR            LOAD  DCB ADDRESS
         USING IHADCB,R2
         TM    48(R2),X'10'           ALREADY   OPEN   ???
         JO    RBRKCHKL               YES - BYPASS    OPEN
*
         tm    vwflag2,vwnodd     Did the RDJFCB fail earlier?
         jz    rbrkopn2           no
         oi    statflg1,vwnodd    Required DDname not in JCL
*
         MVC   ERRDATA(8),VWDDNAME    INDICATE WHICH FILE
         LHI   R14,MSG#405        yes, No DD defined
         J     RTNERROR
*
RBRKopn2 equ   *
         LAY   R14,STATOPEN
         MVC   RENTPARM(STATOPENL),0(R14)
         OPEN  ((R2),OUTPUT),MODE=31,MF=(E,RENTPARM)  OPEN OUTPUT
         TM    48(R2),X'10'           OPEN  SUCCESSFUL ???
         JO    RBRKPUTA               YES - ADVANCE TO CALCULATION LOOP
*
         LHI   R14,MSG#403            LOAD ERROR MSG NUMBER
         MVC   ERRDATA(8),VWDDNAME    INDICATE WHICH FILE
         J     RTNERROR

RBRKPUTA MVC   DATAPUTA+1(3),DCBPUTA
         OC    DATAPUTA,MODE31
                     SPACE 3
RBRKCHKL LH    R0,VWOUTLEN
         TM    VWFLAG2,VWPRINT       PRINT OUTPUT ???
         JNO   RBRKCHK1              NO  - BYPASS LENGTH CHECK
         CH    R0,VWLINSIZ
         JH    RBRKCHK1
         LH    R0,VWLINSIZ
*
RBRKCHK1 EQU   *
         TM    DCBRECFM,X'80'     FIXED/UNDEFINED ??
         JO    RBRKCHK2
         AHI   R0,4
RBRKCHK2 EQU   *
         CH    R0,DCBLRECL
         JNH   RBRKCHKC
*
         LHI   R14,MSG#404
         MVC   ERRDATA(8),VWDDNAME   INDICATE WHICH  FILE
         J     RTNERROR
*
RBRKCHKC TM    VWFLAG2,VWOUTDCB   OUTPUT FILE DESTINATION ???
         JO    RBRKCOLM           BYPASS CENTERING & SORT TITLES
                     EJECT
RBRKVSAM CLI   VWDESTYP+1,ONLINE  ONLINE ???
         JE    RBRKBLDR           YES -  SETUP
         CLI   VWDESTYP+1,BATCH   HARDCOPY/BATCH  ???
         JNE   RBRKBLDR           NO  -  BYPASS SETUP
*
RBRKBLDR BRAS  R10,BLDRPTTL       BUILD REPORT TITLE   LINES
         BRAS  R10,BLDFOOTR       BUILD REPORT FOOTER  LINES
*
RBRKSKEY LH    R0,VWBRKCNT        LOAD NUMBER OF  SORT KEYS
         LTR   R0,R0              ANY  SORT KEYS (SAVE COUNT) ???
         JNP   RBRKCOLM           NO - BUILD  REPORT  TITLES
*
         L     R2,VWSKADDR        HIGHEST LEVEL SORT  KEY DEFINITION
RBRKBLDT ST    R2,CURSRTKY
         BRAS  R10,BLDTKEY        BUILD  SORT KEY TITLE
         AHI   R2,SKENTLEN        ADVANCE TO NEXT SORT  KEY DEFINITION
         C     R2,VWLOWSKY        ALL  SORT  KEYS    PROCESSED ???
         JNH   RBRKBLDT           NO - LOOP  THROUGH ALL SORT  KEYS
*
RBRKCENT TM    VWFLAG2,VWEXEC+VWPVIT EXEC INFO OR PIVOT TABLE ???
         JNZ   RBRKCOLM           YES -   LEFT JUSTIFY  DATA
*
         LH    R15,VWLINSIZ       LOAD   PRINT LINESIZE
         SH    R15,VWOUTLEN       SUBTRACT  ACTUAL DATA LENGTH
         JNP   RBRKCOLM           BRANCH IF NO   EXCESS SPACE
*
         CH    R15,VWMAXTTL       ROOM   FOR SORT BREAK TITLE ???
         JH    RBRKADJ            YES -  CENTER PRINT RIGHT OF TITLE
         STH   R15,VWCENTER       NO  -  PUSH AS FAR RIGHT AS POSSIBLE
         J     RBRKCOLM
*
RBRKADJ  EQU   *
*        SH    R15,VWMAXTTL       EXCLUDE TITLE FROM EXCESS
*        SRL   R15,1              DIVIDE EXTRA SPACE BY TWO
*        AH    R15,VWMAXTTL       ADD   BACK TITLE PREFIX LENGTH
         LH    R15,VWMAXTTL       LEFT  JUSTIFY DATA NEXT TO   TITLE
         STH   R15,VWCENTER       SHIFT DATA RIGHT BY ONE-HALF EXCESS
                     EJECT
***********************************************************************
*  LOOP THROUGH COLUMN DEFINITIONS AND PERFORM COLUMN INITIALIZATION  *
***********************************************************************
RBRKCOLM L     R6,VWCOLADR        LOAD FIRST COLUMN  DEFN  ADDRESS
         USING COLDEFN,R6
         LH    R2,VWCOLCNT        LOAD MAXIMUM NO. OF COLUMNS
         LTR   R2,R2              LOAD COLUMN  COUNT (ANY COLUMNS ???)
         JNP   RBRKEXIT           NO  - EXIT
*
         MVI   H1TEXT+0,C' '      BLANK OUT  PREVIOUS COLUMN HEADINGS
         MVC   H1TEXT+1(L'H1TEXT-1),H1TEXT+0
*
         MVI   H2TEXT+0,C' '      BLANK OUT  PREVIOUS COLUMN HEADINGS
         MVC   H2TEXT+1(L'H2TEXT-1),H2TEXT+0
*
         MVI   H3TEXT+0,C' '      BLANK OUT  PREVIOUS COLUMN HEADINGS
         MVC   H3TEXT+1(L'H3TEXT-1),H3TEXT+0
*
         MVI   DASHTEXT+0,C' '    BLANK OUT  PREVIOUS DASH   LINE
         MVC   DASHTEXT+1(L'DASHTEXT-1),DASHTEXT+0
*
         LHI   R0,4+1             COMPUTE    HEADING  LINE   LENGTH
         AH    R0,VWCENTER
         AH    R0,VWOUTLEN
         STH   R0,VWLINELN
*
         STH   R0,H1RECLEN
         XC    H1RECLEN+2(2),H1RECLEN+2
         MVI   H1CC,C' '
*
         STH   R0,H2RECLEN
         XC    H2RECLEN+2(2),H2RECLEN+2
         MVI   H2CC,C' '
*
         STH   R0,H3RECLEN
         XC    H3RECLEN+2(2),H3RECLEN+2
         MVI   H3CC,C' '
*
         STH   R0,DASHLEN
         XC    DASHLEN+2(2),DASHLEN+2
         MVI   DASHCC,C' '
                     EJECT
RBRKBLD  Larl  R14,BLDHEADr       BUILD COLUMN HEADERS
         BASR  R10,R14
*
         LH    R15,CDCLCCOL       CALCULATED   COLUMN   ???
         LTR   R15,R15
         JNP   RBRKFMT            NO  - BYPASS CALC  COLUMN DEFN  ADDR
*
         BCTR  R15,0              UPDATE CALCULATED  COLUMN DEFN  ADDR
         SLL   R15,2
         L     R14,CLCCOLTB
         AR    R14,R15
         ST    R6,0(,R14)
*
         LH    R15,CDCOLNO        ORIGINAL   COLUMN  NUMBER
         BCTR  R15,0
         SLL   R15,2
         A     R15,CLCOFFTB       UPDATE CALCULATED  COLUMN OFFSET TBL
         LH    R14,CDCLCOFF
         ST    R14,0(,R15)
                     SPACE 3
***********************************************************************
*  LOOP THROUGH COLUMN CALCS CONVERTING COLUMN NUMBERS TO OFFSETS     *
***********************************************************************
         L     R1,CDCALCTB        LOAD  CALC  TABLE ADDRESS
         USING CALCTBL,R1
         LH    R15,CDCALCNT       LOAD  CALC  TABLE COUNT
         LTR   R15,R15            ANY   COLUMN   CALCULATIONS ???
         JNP   RBRKFMT
*
RBRKCALC CLI   CALCOPER+1,PUSHC   COLUMN#  ???
         JNE   RBRKCLCN           NO  - BRANCH
*
         L     R14,CALCOP2A       LOAD OPERAND 2 COLUMN  NUMBER
         BCTR  R14,0              CONVERT   TO   OFFSET
         SLL   R14,2
         A     R14,CLCOFFTB
         L     R14,0(,R14)
         ST    R14,CALCOP2A
*
         CLI   CALCOPER+1,C'L'    PREVIOUS LINE VALUE   ???
         JNE   RBRKCLCN           NO  - ADVANCE TO NEXT CALCULATION
*
         A     R14,EXTPREVA       LOAD BASE ADDRESS OF PREVIOUS VALUES
         ST    R14,CALCOP2A       SAVE PREVIOUS  VALUE ADDRESS
*
RBRKCLCN AHI   R1,CALCLEN
         BRCT  R15,RBRKCALC
                     SPACE 3
RBRKFMT  LAY   R14,FMTFUNTB       LOAD  FORMATTING FUNCTION TABLE ADDR
         A     R14,CDFMTFUN       ADD   FUNCTION   OFFSET
         L     R14,0(,R14)
         ST    R14,CDFMTFUN       SAVE  FORMATTING FUNCTION ADDRESS
*
RBRKNEXT AHI   R6,CDENTLEN        ADVANCE  TO NEXT COLUMN
         BRCT  R2,RBRKBLD         LOOP THROUGH ALL COLUMNS
                     EJECT
***********************************************************************
*  LOOP THROUGH EXCEPTION COND CONVERTING COLUMN NUMBERS TO OFFSETS   *
***********************************************************************
RBRKEXCP L     R1,VWEXCOND        LOAD  EXCP  TABLE ADDRESS
         USING CALCTBL,R1
         LH    R15,VWEXCNT        LOAD  EXCP  TABLE COUNT
         LTR   R15,R15            ANY   EXCEPTION   CONDITIONS  ???
         JNP   RBRKEXIT
*
RBRKEXCL CLI   CALCOPER+1,PUSHC   COLUMN#  ???
         JNE   RBRKEXCN           NO  - BRANCH
*
         L     R14,CALCOP2A       LOAD OPERAND 2 COLUMN  NUMBER
         BCTR  R14,0              CONVERT   TO   OFFSET
         SLL   R14,2
         A     R14,CLCOFFTB
         L     R14,0(,R14)
         ST    R14,CALCOP2A
*
         CLI   CALCOPER+1,C'L'    PREVIOUS LINE VALUE   ???
         JNE   RBRKEXCN           NO  - ADVANCE TO NEXT CALCULATION
*
         A     R14,EXTPREVA       LOAD BASE ADDRESS OF PREVIOUS VALUES
         ST    R14,CALCOP2A       SAVE PREVIOUS  VALUE ADDRESS
*
RBRKEXCN AHI   R1,CALCLEN
         BRCT  R15,RBRKEXCL
                     SPACE 3
RBRKEXIT BRAS  R9,PAGEBRK         CALL  PAGE BREAK SUBROUTINE
         MVC   NEEDTITL,VWBRKCNT  SAVE NO. OF SORT BREAK LEVELS
*
         DROP  R6
         DROP  R7
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P R O C E S S   E X T R A C T   R E C O R D                  *
*                                                                     *
*                                                                     *
* 1.  CHECK FOR SPECIAL FILE    OUTPUT PROCESSES                      *
* 2.  CHECK FOR A       CONTROL BREAK  (SORT KEY)                     *
* 3.  NORMALIZE COLUMN  VALUES                                        *
* 4.  INCREMENT COLUMN  ACCUMULATORS                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTREC,R7
         USING VIEWREC,R8
*
EXTPROC  MVC   VWCURSEC,DETPDL      INDICATE DETAIL LINE
*
         L     R14,PARMDADR         CHECK PARAMETER OPTION
         CLI   NOHDROPT-PARMDATA(R14),C'Y'   HEADER RECORDS PRESENT ???
         JNE   EXTMSTR              YES -    BYPASS SPECIAL LOGIC
*
         LH    R15,EXDATALN
         LTR   R15,R15              NEGATIVE,  THEN HEADER
         JM    READEOF              IF HEADER
*
EXTMSTR  equ   *
* **     CLI   LASTFILE,C'M'      RECORD CAME  FROM MASTER FILE ???
* **     BRNE  EXTCHKF            NO  -  SKIP  NEXT CHECK
*
* **     CLI   VWUPDMOD+1,REGEN   REGEN  REQUEST ???
* **     BRE   READLOOP           YES -  IGNORE  OLD MASTER RECORDS
* **     CLI   VWUPDMOD+1,NEWMSTR NEW    MASTER  ???
* **     BRE   READLOOP           YES -  IGNORE  OLD MASTER RECORDS
*
***********************************************************************
*  PROCESS FILE FORMAT OUTPUT EXTRACT RECORD                          *
***********************************************************************
EXTCHKF  CLI   VWDESTYP+1,FILEFMT DATA  FILE OUTPUT ???
         JE    EXTCHK1            YES - CONTINUE
         CLI   VWDESTYP+1,CSV     COMMA SEPARATED  VARIABLES ???
         JNE   EXTINIT            NO  - BYPASS FILE  LOGIC
*
EXTCHK1  EQU   *
         CLI   VWSUMTYP+1,DETAIL  DETAIL FILE   ???
         JNE   EXTSUMF            NO  -  BRANCH TO   SUMMARY FILE
         BRAS  R10,DETFILE        WRITE  DETAIL FILE RECORD
         ST    R7,PREVRECA
         J     READLOOP           READ  NEXT RECORD
                     SPACE 3
EXTSUMF  LH    R1,EXSORTLN        LOAD  LENGTH  OF SORT KEY (ALL KEYS)
         LTR   R15,R1             ANY   SORT KEYS  ???
         JNP   EXTNOBRK           NO  - CONTINUE   ACCUMULATION
*
         L     R14,VWLOWSKY       LOAD  SORT KEY   COUNTER   ADDRESS
         AP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P001 INCREMENT ROW COUNT
*
         LA    R0,EXSORTKY        LOAD  ADDRESS OF CURRENT  SORT KEY
         L     R14,SVSORTKY       LOAD  ADDRESS OF PREVIOUS SORT KEY
         CLCL  R0,R14             SORT  KEY  BREAK ???
         JNE   EXTSUMFB           YES - OUTPUT  SUMMARY REC
*
         L     R14,VWLOWSKY       LOAD  SORT KEY   COUNTER   ADDRESS
         CP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P001
         JNE   EXTNOBRK           NO  - ACCUMULATE SUBTOTALS
         J     EXTSUMFF           YES - PERFORM    FIRST TIME  LOGIC
*
EXTSUMFB L     R7,PREVRECA        POINT TO PREVIOUS EXTRACT RECORD
         L     R3,SUBTOTAD        LOAD  BASE SET OF VALUES  ADDRESS
         ST    R3,CALCBASE
         LHI   R15,1              INDICATE AT LOWEST BREAK  LEVEL
         STH   R15,SVBRKCNT
         BRAS  R10,COMFILE        WRITE SUMMARY FILE RECORD
*
***********************************************************************
*  NORMALIZE FIRST RECORD AFTER SUMMARY FILE SORT KEY BREAK           *
***********************************************************************
EXTSUMFF L     R7,RECADDR         RESTORE POINTER TO CURRENT RECORD
         L     R1,EXTCOLA         LOAD ADDRESS OF CORRECT COLUMN BASE
         ST    R1,CALCBASE        SAVE BASE   FOR CALCULATIONS IF ANY
*
         LH    R0,EXNCOL          LOAD NO. OF COLUMNS IN RECORD
         LTR   R0,R0              ANY COLUMNS IN THIS    RECORD ???
         JNP   ZEROCBRK           NO -  CHECK IF DETAIL  REPORT
*
         LA    R6,EXSORTKY        POINT TO FIRST COLUMN
         AH    R6,EXSORTLN
         AH    R6,EXTITLLN
         AH    R6,EXDATALN
         USING COLEXTR,R6
*
***********************************************************************
*  COPY EXTRACT RECORD "CT" COLUMNS INTO ARRAY USING COL# AS SUBSCRIPT*
***********************************************************************

         eextr r12,fp9            get the biased exponent set by MR87
*
         do from=(r0)
           LH  R14,COLNO          LOAD     COLUMN NUMBER
           BCTR R14,0             COMPUTE  OFFSET TO INDICATED COLUMN
           SLL R14,2
           A   R14,CLCOFFTB
           L   R15,0(,R14)
*
           AR  R15,R1                   ADD  BASE  TO OFFSET
           ZAP 0(AccumDFPl,R15),COLDATA MOVE packed data into accum
           lmg r2,r3,0(r15)       get the packed value into a gpr pair
           cxstr fp0,r2           convert to dfp
           iextr fp0,fp0,r12      insert the biased exponent
           GVBSTX fp0,0(,r15)        and save back in accumlator
*
           AHI R6,COLDATAL        ADVANCE  TO NEXT COLUMN  OFFSET
         enddo ,                  LOOP THROUGH ALL COLUMNS IN EXTRACT

         BRAS  R9,FRSTSET         INITIALIZE SPECIAL ACCUMULATORS
*
         L     R14,VWLOWSKY       LOAD  SORT KEY   COUNTER   ADDRESS
         ZAP   SKCOUNT-SORTKEY(L'SKCOUNT,R14),P001
*
         J     ZEROCBRK
                     EJECT
***********************************************************************
*  CHECK FOR SORT KEY BREAK IN NON FILE FORMAT OUTPUT                 *
***********************************************************************
EXTINIT  LH    R0,VWBRKCNT        LOAD  SORT BREAK COUNT
         LTR   R0,R0              ANY   SORT KEYS  ???
         JNP   EXTNOBRK           NO  - SKIP CHECK FOR  BREAK
*
         SR    R2,R2              INITIALIZE CURRENT  OCCURRENCE NO(-1)
         L     R6,VWSKADDR        LOAD ADDRESS OF SORT  KEY DEFINITIONS
         USING SORTKEY,R6
                     SPACE 3
EXTSRCH  LA    R14,EXSORTKY       INITIALIZE SORT KEY   POSITIONS
         AH    R14,SKVALOFF
         L     R15,SVSORTKY
         AH    R15,SKVALOFF
         LH    R1,SKFLDLEN        LOAD  SORT KEY  LENGTH
         BCTR  R1,0               DECREMENT  FOR "EX"
         EX    R1,SRTBREAK        CHECK  FOR CONTROL    BREAK  ???
         JNE   EXTBRK             BRANCH  IF DIFFERENT
*
         AHI   R2,1               INCREMENT  OCCURRENCE NO
         AHI   R6,SKENTLEN        ADVANCE TO  NEXT SORT KEY DEFINITION
         BRCT  R0,EXTSRCH         LOOP THROUGH ALL SORT KEYS
*
         J     EXTNOBRK           NO   BREAK FOUND
         DROP  R6
*
EXTBRK   BRAS  R10,KEYBREAK
*
         MVC   VWCURSEC,DETPDL    INDICATE DETAIL LINE
                     EJECT
***********************************************************************
*  CONTINUE EXTRACT RECORD PROCESSING                                 *
***********************************************************************
EXTNOBRK L     R1,EXTCOLA         LOAD ADDRESS OF CORRECT COLUMN BASE
         ST    R1,CALCBASE        SAVE BASE   FOR CALCULATIONS IF ANY
*
         LH    R0,EXNCOL          LOAD NO. OF COLUMNS IN RECORD
         LTR   R0,R0              ANY COLUMNS IN THIS    RECORD ???
         JNP   ZEROCBRK           NO -  CHECK IF DETAIL  REPORT
*
         LA    R6,EXSORTKY        POINT TO FIRST COLUMN
         AH    R6,EXSORTLN
         AH    R6,EXTITLLN
         AH    R6,EXDATALN
         USING COLEXTR,R6
*
***********************************************************************
*  COPY EXTRACT RECORD "CT" COLUMNS INTO ARRAY USING COL# AS SUBSCRIPT*
***********************************************************************

         eextr r12,fp9            get the biased exponent set by MR87
*
         do from=(r0)
           LH  R14,COLNO          LOAD     COLUMN NUMBER
           BCTR R14,0             COMPUTE  OFFSET TO INDICATED COLUMN
           SLL R14,2
           A   R14,CLCOFFTB
           L   R15,0(,R14)
*
           AR  R15,R1                   ADD  BASE  TO OFFSET
           ZAP 0(AccumDFPl,R15),COLDATA MOVE packed data into accum
           lmg r2,r3,0(r15)       get the packed value into a gpr pair
           cxstr fp0,r2           convert to dfp
           iextr fp0,fp0,r12      insert the biased exponent
           GVBSTX fp0,0(,r15)        and save back in accumlator
*
           AHI R6,COLDATAL        ADVANCE  TO NEXT COLUMN  OFFSET
         enddo ,                  LOOP THROUGH ALL COLUMNS IN EXTRACT

ZEROCBRK XC    SVBRKCNT,SVBRKCNT  ZERO CURRENT BREAK LEVEL
*
         LH    R15,VWSETLEN       SAVE PRE-CALCULATION RESULTS
         LTR   R15,R15
         JNP   CHKDET
*
         L     R1,CALCBASE
         L     R14,EXTPCALC
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO  MOVE
         SRL   R0,8
         J     CHKDCLCE
CHKDCLCL MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
CHKDCLCE BRCT  R0,CHKDCLCL
         EX    R15,MVCR14R1
*
CHKDET   CLI   VWSUMTYP+1,DETAIL  DETAIL  REPORT ???
         JE    DETPRNT            YES - PRINT DATA
*
***********************************************************************
*  SUMMARY REPORT:  ADD DETAIL COLUMN VALUES TO SUMMARY ACCUMULATORS  *
***********************************************************************
         CLI   LASTFILE,C'M'      MASTER  FILE   RECORD ???
         JE    CHKDET01           YES -   BYPASS CALCULATIONS
         BRAS  R10,CALCCOLM       PERFORM DETAIL COLUMN CALCULATIONS
*
CHKDET01 EQU   *
         BRAS  R10,SUM            ACCUMULATE  SUM
*
         ST    R7,PREVRECA
         J     READLOOP           READ  NEXT  EXTRACT RECORD
                     EJECT
***********************************************************************
*  DETAIL REPORT:  EXECUTE EXCEPTION LOGIC (RULES)                    *
***********************************************************************
DETPRNT  BRAS  R10,CALCCOLM       PERFORM DETAIL COLUMN CALCULATIONS
         BRAS  R10,EXCPCHK        CHECK  FOR EXCEPTIONS
         J     READLOOP           +0  - RETURN (SKIP    THIS RECORD)
*
DETPKEEP L     R14,VWLOWSKY       +4  - RETURN (PROCESS THIS RECORD)
         CP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P000
         JNE   DETPKP01
         BRAS  R9,FRSTSET         INITIALIZE SPECIAL  ACCUMULATORS
         L     R14,VWLOWSKY       RESTORE COUNT ADDRESS
*
DETPKP01 EQU   *
         AP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P001 INCREMENT ROW COUNT
*
DETPPRNT LH    R15,VWCENTER       LOAD  CENTERING  ADJUSTMENT
         LH    R14,VWLINELN       LOAD  PRINT DATA LENGTH
         STH   R14,PRNTRDW        BUILD RDW
*
         MVI   PRNTCC,C' '        INITIALIZE  CARRIAGE  CONTROL
*
         LTR   R15,R15            ANY  CENTERING ADJUSTMENT ???
         JNP   DETPCBLD           NO  - SKIP INITIALIZATION
         BCTR  R15,0              DECREMENT  FOR  "EX"
         EX    R15,INITCENT       INITIALIZE CENTERING PREFIX (SPACES)
*
DETPCBLD LA    R2,PRNTTEXT        LOAD OUTPUT AREA ADDRESS ("COLBUILD")
         XC    CURSRTKY,CURSRTKY  INDICATE  ON LOWEST DETAIL LEVEL
         BRAS  R9,COLBUILD        BUILD COLUMN OUTPUT
         LA    R2,PRNTLINE        RESET POINTER TO START  OF PRINT LINE
*
DETPDATA TM    VWFLAG1,VWZEROSP   ZERO SUPPRESS OPTION   ???
         JNO   DETPOKAY           NO  -  BYPASS CHECK
*
         LH    R0,NOTNULL         LOAD NON-ZERO COUNT
         LTR   R0,R0              ANY  NON-ZERO COLUMNS  ???
         JZ    DETPSUM            NO  -  BYPASS THIS ROW
*
DETPOKAY LH    R5,NEEDTITL        SORT  TITLE  LINES   NEEDED
         AH    R5,VWLINENO        LOAD  CURRENT LINE    COUNT
         CH    R5,VWPAGSIZ        CHECK AGAINST MAXIMUM PAGE  SIZE  ???
         JL    DETPOK01           SKIP  PAGE    BREAK   IF    ROOM
         MVC   NEEDTITL,VWBRKCNT  SET   NO. OF  SORT BREAK  LEVELS
         BRAS  R9,FOOTPRNT        PRINT PAGE FOOTERS
         BRAS  R9,PAGEBRK         CALL  PAGE   BREAK SUBROUTINE
*
DETPOK01 EQU   *
         LH    R0,NEEDTITL        SORT  TITLE  LINES NEEDED ???
         LTR   R0,R0
         JNP   DETPPUT            NO  - BYPASS
*
         BRAS  R9,TTLPRINT        PRINT SORT KEY TITLES FOR NEXT GROUP
*
DETPPUT  MVC   VWCURSEC,DETPDL    INDICATE DETAIL LINE
*
         LR    R14,R2             LOAD  OUTPUT AREA   ADDRESS
         AHI   R14,-4             BACKUP TO    RDW
         L     R1,PRNTDCBA        LOAD  DCB    ADDRESS
         MVC   DCBLRECL-IHADCB(2,R1),0(R14)
         LR    R0,R14             LOAD  DATA  ADDR
         L     R15,PRNTSUBR       LOAD  SUBROUTINE ADDRESS (24-BIT)
         BASR  R14,R15            WRITE PRINT LINE
                     EJECT
*
DETPSUM  BRAS  R10,SUM            SUM   THIS RECORD   INTO   TOTALS
*
         ST    R7,PREVRECA        SAVE  THIS RECORD'S ADDRESS
         J     READLOOP           RETURN  TO TOP OF READ LOOP
*
static   loctr
SRTBREAK CLC   0(0,R14),0(R15)    * * * *   E X E C U T E D   * * * *
INITCENT MVC   PRNTTEXT(0),PRNTCC * * * *   E X E C U T E D   * * * *
MVCPRINT MVC   0(0,R1),0(R14)     * * * *   E X E C U T E D   * * * *
just_move mvc  0(0,r15),0(r14)
just_move2 mvc  0(0,r15),0(r1)
DETPDL   DC    CL2'DL'
code     loctr
*
         DROP  R6
         DROP  R7
                     EJECT
READEOF  CLI   EOREOFCD,C'H'      READING NEXT HEADERS ???
         JE    EOFBREAK           YES -  CHECK IF  REQUEST IN-PROGRESS
*
         CLI   EOREOFCD,C' '      REQUEST  IN-PROGRESS (STARTED OK) ???
         JNE   CHKCOUNT           NO  - BYPASS GRAND TOTALS
*
         MVI   EOREOFCD,C'F'      INDICATE END-OF-FILE
*
EOFBREAK BRAS  R10,RPTBREAK       PROCESS BREAK FOR LAST REQUEST
*
         CLI   EOREOFCD,C'H'      END-OF-FILE WHILE READING HEADERS ???
         JNE   CHKCOUNT           NO  - BYPASS MESSAGE
*
         Larl  R15,noextrec                PRINT NO EXTRACT RECORD MSG
         BASR  R10,R15
*
         MVI   EOREOFCD,C'F'      INDICATE END-OF-FILE
                     SPACE 3
CHKCOUNT L     R14,PARMDADR         CHECK PARAMETER OPTION
         CLI   NOHDROPT-PARMDATA(R14),C'Y'   HEADER RECORDS PRESENT ???
         JE    NOCOUNT              NO  -    BYPASS COMPARE
*
* **     LHI   R14,MSG#06         ASSUME REC COUNTS DON'T MATCH
* **     CP    EXTRCNT,SVRECCNT   COMPARE COUNTS ???
* **     BRNE  RTNERROR           ERROR   IF     MISMATCH
*
NOCOUNT  DS    0H
***********************************************************************
*  GET FINISH DATE/TIME                                               *
***********************************************************************
*
         XC    endtime,endtime    GET ENDING TIME
         lay   R2,endtime
         lay   R3,timelist
         TIME  STCK,(R2),LINKAGE=SYSTEM,DATETYPE=YYYYMMDD,             +
               MF=(E,(R3))
*
         Larl  R15,CTRLRPT        PRINT  CONTROL REPORT
         BASR  R10,R15
         larl  r15,closfile       close  files
         BASR  R10,R15
* Did we get any overflow? If yes write warning to log
         if TM,STATFLG4,STATOVFL,NZ
         L     R14,LOGDCBA
         GVBMSG LOG,MSGNO=MSG#451,SUBNO=2,LOGDCBA=(R14),               +
               SUB1=(MR88NAME,L'MR88NAME),                             +
               SUB2=(OVFLCHAR,1),                                      +
               MSGBUFFER=(UTMTBUFF,L'UTMTBUFF),                        +
               MF=(E,MSG_AREA)
          LA    R0,8              return an error - overflow
          STY   R0,MR88RETC
         ENDIF
***
         DROP  R8
                     EJECT
returne  ds    0h
         LY    R15,MR88RETC
         l     r14,wksavmsk       restore callers   program  mask
         spm   r14

         lay   r14,fp_reg_savearea  Point at register save area
         GVBLDX  fp8,0(0,r14)       restore fp8 to fp15
         GVBLDX  fp9,16(0,r14)
         GVBLDX  fp12,32(0,r14)
         GVBLDX  fp13,48(0,r14)
*
         L     R13,savprev        RESTORE REGISTER  R13
         L     R14,savgrs14       RESTORE REGISTER  R14
         LM    R0,R12,savgrs0     RESTORE REGISTERS R0 - R12
         bsm   0,r14              RETURN
*
* error message handling
*
RTNERROR DS    0H
         C     R14,=A(MSG#499)    Recursive error ?
         JE    RTNERRORX
*
*************************************
*  CHECK FOR MESSAGE NUMBER ROW ABEND
*************************************
         STY   R14,ERROR_NBR
*
         CY    R14,abend_msg
         JNE   ERRMSGPR
*
         DC    XL4'FFFFFFFF'
*
ERRMSGPR ds    0h
         CHI   R14,MSG#414        Is it information only message ?
         JNE   ERRMSGPS
*
         Larl  R15,CTRLRPT        PRINT  CONTROL REPORT
         BASR  R10,R15
         LY    R14,ERROR_NBR
         LA    R0,4
         J     ERRMSGPT

ERRMSGPS EQU   *
         LA    R0,8
ERRMSGPT EQU   *
         STY   R0,MR88RETC
*
* Format message and write to log
*
         Using MSGLIST,r1
         LAY   R1,MSG_AREA        Address parm list area
         MVI   MSGTYPE,c'L'       Type parameter - write to LOG
         LA    R0,0               No prefix override
         ST    R0,MSGPFX          STORE INTO PARAM. LIST
         L     R0,LOGDCBA         Address of GENENV
         ST    R0,MSGDCBA         STORE INTO PARAM. LIST
         LR    R0,R14             Message number
         ST    R0,MSGNUM          STORE INTO PARAM. LIS
         LAY   R0,UTMTBUFF        Provide buffer
         ST    R0,MSGBUFFA        STORE INTO PARAM. LIST
         LA    R0,l'UTMTBUFF      Buffer - rec len + rdw
         ST    R0,MSGBUFFL        STORE INTO PARAM. LIST
*
*
         LAY   R2,UTMTLINE
         MVC   0(L'UTMTLINE,R2),SPACES    BLANK OUT   PRINT   LINE
*
         xc    MSG#SUB,MSG#SUB    clear this out
         mvi   MSG#SUB+3,1        and put a one there
         la    r2,MR88NAME        First parm is source name
         la    r3,l'MR88NAME
*
         la    r4,MSGS1PTR        set pointer
subs     using MSGS1PTR,r4        tell the assembler the mapping
         stm   r2,r3,subs.MSGS1PTR  save the adlen pair
         aghi  r4,l'MSGS1PTR+l'MSGS1LEN move to next pair subs
* check if any parms already set up - up to 3
         do    from=(r15,3)         room for 3 more max before errdata
           llgt  r2,subs.MSGS1PTR   get next parm address if any
           doexit ltgr,r2,r2,z       and exit if zero
           l     r14,MSG#SUB        increment no of parms
           la    r14,1(,r14)
           st    r14,MSG#SUB
           aghi  r4,l'MSGS1PTR+l'MSGS1LEN move pointer
         enddo ,
* Add last parm from ERRDATA
         if    CLC,ERRDATA,ne,SPACES  ANY INDICATIVE DATA ???
           LA  R2,errdata           point at the error data
           la  r3,l'errdata-1       and get the length
           la  r14,0(r3,r2)         point at last char
           la  r3,1(,r3)            and make this actual length
           do  while=(cli,0(r14),eq,c' ') scan until non blank at end
             bctgr r14,0             Move pointer back by one
             bctgr r3,0              reduce field length by one
           enddo
           stm r2,r3,subs.MSGS1PTR  save the adlen pair
           aghi r4,l'MSGS1PTR+l'MSGS1LEN move pointer
           drop subs
           l     r14,MSG#SUB        increment no of parms
           la    r14,1(,r14)
           st    r14,MSG#SUB
         endif
*
         LAY   R1,MSG_AREA        Address parm list area
         L     15,=V(GVBUTMSG)
         BASSM 14,15
*
         xc    MSGS2PTR,MSGS2PTR    make this zero for the next time

         L     R15,PARMDADR
         if    (cli,extropt-PARMDATA(R15),eq,c'Y')
           if    (cli,statflg2,eq,insort)
             OI    STATFLG3,X'FF'   error condition
             L     R15,EXTRCHKA     call sort to terminate cleanly
             BASR  R14,R15          will provide return code via mr87
           endif
         endif

*        xc    0(4,r1),0(r1)        make this zero for the next time

RTNERRORX EQU  *
*
         larl  r15,closfile       close  files
         BASR  R10,R15
*
         LHI   R15,8              SET   RC=8  (ERROR)
         L     R0,SORTRSA         RUNNING  AS SORT  EXIT ???
         LTR   R0,R0              NO  - RETURN TO   MVS
         JNP   RETURNE
*
         LR    R15,R13            Double check as SORTRSA isn't
         LR    R13,R0             necessarily
         L     R13,SORTRSA        indication MR88 was invoked by MR95.
         LTR   R13,R13
         JZ    SAVEIT04
         LR    R0,R14             MR88 might have just used sort itself
         L     R14,savgrs14
         LTR   R14,R14
         JNZ   SAVEIT03
         L     R14,R0             MR88 might have just used sort itself
SAVEIT04 DS    0H
         LR    R13,R15
         LHI   R15,8
         J     RETURNE
*
SAVEIT03 DS    0H
         L     R0,SORTRSA

         xc    sortrsa,sortrsa         reset so don't come here twice
         LARL  R14,RETURNE
saveit   using saver,savesort
         STM   R14,R12,saveit.savgrs14
         drop  saveit
         LR    R13,R0                  RESTORE SAVE AREA ADDRESS
         L     R13,SORTRSA             RESTORE SORT'S REGISTER CONTENTS
         LM    R14,R12,savgrs14
         LHI   R15,16             SET   RC=16 (DON'T  CALL  AGAIN)
         BR    R14
         EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D E T A I L   F I L E   O U T P U T                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTREC,R7
         USING VIEWREC,R8
*
DETFILE  ST    R10,SAVER10        SAVE  RETURN ADDRESS (R10 IS REUSED)
*
         XC    SVBRKCNT,SVBRKCNT  ZERO CURRENT  BREAK LEVEL
         L     R1,EXTCOLA         LOAD BASE  SET   OF VALUES ADDRESS
         ST    R1,CALCBASE
*
         LH    R0,EXNCOL          LOAD NO. OF COLUMNS IN RECORD
         LTR   R0,R0              ANY COLUMNS IN THIS    RECORD ???
         JNP   COMFKEEP           NO - OUTPUT THE DATA
*
         LA    R6,EXSORTKY        POINT TO FIRST COLUMN
         AH    R6,EXSORTLN
         AH    R6,EXTITLLN
         AH    R6,EXDATALN
         USING COLEXTR,R6
*
***********************************************************************
*  COPY EXTRACT RECORD "CT" COLUMNS INTO ARRAY USING COL# AS SUBSCRIPT*
***********************************************************************

         eextr r12,fp9            get the biased exponent set by MR87
*
         do from=(r0)
           LH  R14,COLNO          LOAD     COLUMN NUMBER
           BCTR R14,0             COMPUTE  OFFSET TO INDICATED COLUMN
           SLL R14,2
           A   R14,CLCOFFTB
           L   R15,0(,R14)
*
           AR  R15,R1                   ADD  BASE  TO OFFSET
           ZAP 0(AccumDFPl,R15),COLDATA MOVE packed data into accum
           lmg r2,r3,0(r15)       get the packed value into a gpr pair
           cxstr fp0,r2           convert to dfp
           iextr fp0,fp0,r12      insert the biased exponent
           GVBSTX fp0,0(,r15)        and save back in accumlator
*
           AHI R6,COLDATAL        ADVANCE  TO NEXT COLUMN  OFFSET
         enddo ,                  LOOP THROUGH ALL COLUMNS IN EXTRACT

         LH    R15,VWSETLEN       SAVE PRE-CALCULATION RESULTS
         LTR   R15,R15
         JNP   DETFMSTR
*
         L     R1,CALCBASE
         L     R14,EXTPCALC
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO  MOVE
         SRL   R0,8
         J     DETFCLCE
DETFCLCL MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
DETFCLCE BRCT  R0,DETFCLCL
         EX    R15,MVCR14R1
*
DETFMSTR CLI   LASTFILE,C'M'      MASTER  FILE   RECORD ???
         JE    COMFKEEP           YES -   BYPASS CALCULATIONS
         BRAS  R10,CALCCOLM       PERFORM     COLUMN   CALCULATIONS
*
         BRAS  R10,EXCPCHK        CHECK  FOR  EXCEPTIONS
         J     COMFSKIP           +0 =  SKIP
         J     COMFKEEP           +4 = OUTPUT DATA RECORD(COMMON LOGIC)
*
         DROP  R6
         DROP  R7
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O M M O N   F I L E   O U T P U T   R O U T I N E          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTREC,R7
         USING VIEWREC,R8
*
COMFILE  ST    R10,SAVER10        SAVE  RETURN ADDRESS (R10 IS REUSED)
*
         LH    R15,VWSETLEN       SAVE PRE-CALCULATION RESULTS
         LTR   R15,R15
         JNP   COMFCALC
*
         L     R1,CALCBASE
         L     R14,EXTPCALC
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO  MOVE
         SRL   R0,8
         J     COMFCLCE
COMFCLCL MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
COMFCLCE BRCT  R0,COMFCLCL
         EX    R15,MVCR14R1
*
COMFCALC BRAS  R10,CALCCOLM       PERFORM      COLUMN   CALCULATIONS
*
         BRAS  R10,EXCPCHK        CHECK  FOR   EXCEPTIONS
         J     COMFSKIP           +0 =  SKIP
*
***********************************************************************
*  GET BUFFER FOR OUTPUT FILE RECORD                                  *
***********************************************************************
COMFKEEP DS    0H                 +4 =  PROCESS RECORD TO OUTPUT BLOCK
         L     R1,VWDCBADR        LOAD  DCB    ADDRESS
         L     R15,DATAPUTA       LOAD  SUBROUTINE     ADDRESS (31-BIT)
         BASR  R14,R15            GET   FIXED  LENGTH  BUFFER
         LR    R2,R1              SAVE  BUFFER ADDRESS (FOR "COLBUILD")
*
         L     R1,VWDCBADR            VARIABLE LENGTH  ???
         TM    DCBRECFM-IHADCB(R1),X'40'  NO - BYPASS  RDW    BUILD
         JNO   COMFBLD
         AHI   R2,4               SKIP RDW
*
* deals with building and outputting the CSV column headers
*
COMFBLD  DS    0H
         TM    VWFLAG2,VWBLDCSV   CSV format output ???
         JNO   COMFBLD1           NO  - bypass this
         CP    VWOUTCNT,P000      First record
         JH    COMFBLD1           NO  - bypass this
*        CLI   VWDHDR,C'Y'        Print CSV column detail header ?
         CLI   VWDHDR,X'01'       Print CSV column detail header ?
         JNE   COMFBLD1           NO  - bypass this
*
         LR    R9,R2              Save
*
         MVI   COLMFRST,C'Y'      First column indicator
         ST    R2,OUTPCURR        Start position for csv colm titles
         L     R6,VWCOLADR        LOAD FIRST COLUMN  DEFN  ADDRESS
         USING COLDEFN,R6
         LH    R2,VWCOLCNT        LOAD MAXIMUM NO. OF COLUMNS
         LTR   R2,R2              LOAD COLUMN  COUNT (ANY COLUMNS ???)
         JNP   COMCSVXT           NO  - EXIT
*
COMCSVLP DS    0H
         LARL  R14,CSVHEADR       Concatenate the CVS headings
         BASR  R10,R14
         AHI   R6,CDENTLEN        ADVANCE  TO NEXT COLUMN
         BRCT  R2,COMCSVLP        LOOP THROUGH ALL COLUMNS
*
         DROP  R6 COLDEFN
*
         LR    R2,R9              Restore
*
         L     R1,VWDCBADR            VARIABLE LENGTH  ???
         TM    DCBRECFM-IHADCB(R1),X'40'  NO - BYPASS  RDW    BUILD
         JNO   COMFCNT0
*
         LR    R9,R2              LOAD   FORMATTED  RECORD  ADDRESS
         AHI   R9,-4              BACKUP TO    RDW
         L     R0,OUTPCURR
         SR    R0,R2
         AHI   R0,4
         STH   R0,0(,R9)
         XC    2(2,R9),2(R9)
*
COMFCNT0 DS    0H
         L     R1,VWDCBADR        LOAD  DCB    ADDRESS
         L     R15,DATAPUTA       LOAD  SUBROUTINE     ADDRESS (31-BIT)
         BASR  R14,R15            GET   FIXED  LENGTH  BUFFER
         LR    R2,R1              SAVE  BUFFER ADDRESS (FOR "COLBUILD")
*
         L     R1,VWDCBADR            VARIABLE LENGTH  ???
         TM    DCBRECFM-IHADCB(R1),X'40'  NO - BYPASS  RDW    BUILD
         JNO   COMCSVXT
         AHI   R2,4               SKIP RDW
COMCSVXT EQU   *
*
* deals with building and outputting regular column data
*
*
COMFBLD1 DS    0H
         XC    CURSRTKY,CURSRTKY  INDICATE  AT LOWEST  DETAIL LEVEL
         BRAS  R9,COLBUILD        BUILD COLUMN OUTPUT
*
         L     R1,VWDCBADR            VARIABLE LENGTH  ???
         TM    DCBRECFM-IHADCB(R1),X'40'  NO - BYPASS  RDW    BUILD
         JNO   COMFCNT
*
         LR    R9,R2              LOAD   FORMATTED  RECORD  ADDRESS
         AHI   R9,-4              BACKUP TO    RDW
         LH    R0,VWOUTLEN        ASSUME LENGTH     PREDETERMINED
         TM    VWFLAG2,VWBLDCSV   COMMA  SEPARATED  ???
         JZ    COMFBLD2           NO  -  BUILD RDW
         L     R0,OUTPCURR
         SR    R0,R2
*
COMFBLD2 EQU   *
         AHI   R0,4
         STH   R0,0(,R9)
         XC    2(2,R9),2(R9)
*
COMFCNT  AP    DATACNT,P001       INCREMENT DATA FILE  RECORD COUNT
         AP    VWOUTCNT,P001
*
         CP    VWOUTCNT,VWLIMIT   OUTPUT   LIMIT REACHED  ???
         JNH   COMFCNT1
*
*       L     R14,LOGDCBA
*       GVBMSG LOG,MSGNO=MSG#414,SUBNO=1,LOGDCBA=(R14),               +
*             SUB1=(MR88NAME,L'MR88NAME),                             +
*             MSGBUFFER=(UTMTBUFF,L'UTMTBUFF),                        +
*             MF=(E,MSG_AREA)
*
         LHI   R14,MSG#414
         J     RTNERROR
*
***********************************************************************
*  CALL FORMAT EXIT IF SPECIFIED                                      *
***********************************************************************
COMFCNT1 EQU   *
         L     R15,VWPGMADR       EXIT PROGRAM SPECIFIED ???
         LTR   R15,R15
         JZ    COMFSKIP           NO - NOTHING TO INVOKE
*
         L     R1,LEWORKA         --> LE WORK AREA
         USING LEINTER,R1
*                                 COPY PROCESSED DATE FROM EXTR CONTROL
         MVC   VWPROCDT,SVPROCDT
         MVC   VWPROCTM,SVPROCTM
*
         LA    R0,VWVIEW#         --> VIEW NUMBER
         ST    R0,FMTPVIEW
         ST    R2,FMTPRECA        --> RECORD TO PROCESS
         LA    R0,VWFMTPRM        --> OPTIONAL FORMAT INFO
         ST    R0,FMTPARMA
         LA    R0,VWCURSEC        --> REPORT DATA AREA
         ST    R0,FMTPSECT
         LA    R0,VWRUN#          --> RUN DATA AREA
         ST    R0,FMTPDATA
         LA    R0,VWFMTPTR        --> OUTPUT RECORD PTR
         ST    R0,FMTPOUTP
         LA    R0,VWFMTWRK        --> STORAGE ANCHOR
         ST    R0,FMTPWRKA
         OI    FMTPWRKA,X'80'
*
         LA    R1,FMTPARML        --> PARAMETER LIST
         BASR  R14,R15
         ST    R15,VWFMTRC
         LTR   R15,R15            WRITE THIS RECORD?
         JZ    COMFK010           Y - WELL, THEN CONTINUE
*
         LHI   R14,MSG#406        N - ASSUME A FATAL ERROR
         MVC   ERRDATA(8),VWFMTPGM
         J     RTNERROR
*
COMFK010 L     R2,FMTPRECA        --> RECORD TO WRITE
         DROP  R1
                     SPACE 3
COMFSKIP LH    R0,VWCLCCOL        LOAD   CALCULATED  COLUMN COUNT
         LTR   R0,R0              ANY    USED ???
         JNP   COMFEXIT           NO  -  EXIT
*
         L     R3,CALCBASE        LOAD   BASE    ADDRESS
COMFZERO GVBSTX   fp8,0(,r3)                RESET   TOTAL
         AHI   R3,AccumDFPl              ADVANCE TO  NEXT  COLUMN
         BRCT  R0,COMFZERO        LOOP   THROUGH ALL COLUMNS
                     SPACE 3
         CLI   VWSUMTYP+1,DETAIL  DETAIL FILE  ???
         JE    COMFEXIT           YES -  EXIT  (NO SUBTOTALS)
*
         LH    R0,VWBRKCNT        LOAD  SORT BREAK COUNT
         CHI   R0,2               AT   LEAST TWO   SORT KEYS  ???
         JL    COMFEXIT           NO  - SKIP CHECK FOR  BREAK
*
         L     R6,VWSKADDR        LOAD ADDRESS OF SORT KEY DEFINITIONS
         USING SORTKEY,R6
         LA    R0,EXSORTKY        INITIALIZE SORT KEY  POSITIONS
         L     R14,SVSORTKY
         LH    R15,SKFLDLEN       LOAD  SORT KEY  LENGTH
         LR    R1,R15             DECREMENT  FOR "EX"
         CLCL  R0,R14             CHECK  FOR CONTROL BREAK ???
         JE    COMFEXIT           BRANCH  IF SAME
*
         LH    R0,VWCLCCOL        LOAD  CALCULATED  COLUMN COUNT
         L     R3,EXTPREVA        LOAD  BASE    ADDRESS
COMFRSET GVBSTX   fp8,0(,r3)                RESET   TOTAL
         AHI   R3,AccumDFPl             ADVANCE TO  NEXT   COLUMN
         BRCT  R0,COMFRSET        LOOP  THROUGH ALL COLUMNS
                     SPACE 3
COMFEXIT L     R7,RECADDR         LOAD  CURRENT RECORD'S ADDRESS
*
         LA    R1,EXSORTKY        SAVE NEW SORT KEY
         L     R14,SVSORTKY
         LH    R15,EXSORTLN
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO  MOVE
         SRL   R0,8
         J     COMFKEYE
COMFKEYL MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
COMFKEYE BRCT  R0,COMFKEYL
         EX    R15,MVCR14R1
*
COMFRTRN L     R10,SAVER10
         BR    R10                RETURN
*
         DROP  R6
         DROP  R7
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* P E R F O R M   S O R T   K E Y   B R E A K   P R O C E S S I N G   *
*                                                                     *
*   NOTE: R2 CONTAINS NO. OF BREAK LEVELS THAT DID NOT CHANGE         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTREC,R7
         USING VIEWREC,R8
*
KEYBREAK ST    R10,SAVER10        SAVE  RETURN ADDRESS(R10 IS REUSED)
*
         L     R7,PREVRECA        USE PREVIOUS EXTRACT RECORD AS SOURCE
*
         LH    R5,VWBRKCNT        COMPUTE NO. OF BREAK LEVELS TO PROC
         SR    R5,R2
         STH   R5,SVBRKCNT        SAVE    NO. OF BREAK LEVELS
*
         L     R4,VWLOWSKY        LOAD   ADDRESS OF LOWEST  SORTKEY DEF
         ST    R4,CURSRTKY        SAVE   ADDRESS
         USING SORTKEY,R4
*
         LTR   R5,R5              ANY    BREAKS  ???
         JNP   KBRKEXIT           NO  -  EXIT
*
         L     R3,SUBTOTAD        LOAD   ADDRESS OF LOWEST  LEVEL   SET
         ST    R3,CALCBASE
*
         MVI   NEEDDASH,C'Y'      INDICATE  DASH LINE  NEEDED
*
         TM    VWFLAG2,VWEXEC+VWPVIT EXEC INFO OR PIVOT TABLE ???
         JZ    KBRKSLVL           NO  - CHECK    FOR SUMMARY  LEVEL
*
         XC    VWLINENO,VWLINENO  RESET CURRENT LINE COUNT
         XC    NEEDTITL,NEEDTITL  RESET NUMBER TITLE LINES   NEEDED
         MVI   NEEDDASH,C' '      RESET DASH  NEEDED INDICATOR
*
KBRKSLVL L     R14,EXTPREVA       LAST DETAIL ROW VALUES ADDRESS
         CLI   VWSUMTYP+1,DETAIL  DETAIL REQUEST ???
         JE    KBRKINIT           YES -  BYPASS  SUMMARY BREAK LOGIC
                     SPACE 3
         LH    R15,VWSETLEN       SAVE PRE-CALCULATION RESULTS
         LTR   R15,R15
         JNP   KBRKCALC
*
         L     R1,CALCBASE
         L     R14,EXTPCALC
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO  MOVE
         SRL   R0,8
         J     KBRKCLCE
KBRKCLCL MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
KBRKCLCE BRCT  R0,KBRKCLCL
         EX    R15,MVCR14R1
*
KBRKCALC DS    0H
         BRAS  R10,CALCCOLM       PERFORM SUMMARY COLUMN CALCULATIONS
*
         BRAS  R10,EXCPCHK        CHECK  FOR  EXCEPTIONS
         J     KBRKSKIP           +0 =  SKIP  SUBTOTAL - PROCESS NEXT
         J     KBRKKEEP           +4 =  PROCESS THIS SUBTOTAL
*
KBRKSKIP CLI   EOREOFCD,C'F'      END-OF-FILE (NO  NEXT  RECORD) ???
         JE    KBRKROLL           YES - BYPASS BUILDING  NEW  TITLES
         CLI   EOREOFCD,C'H'      END-OF-FILE (NO  NEXT  RECORD) ???
         JE    KBRKROLL           YES - BYPASS BUILDING  NEW  TITLES
*
         J     KBRKBLD            BUILD NEW TITLES &  ROLLUP  SUBTOTALS
                     SPACE 3
KBRKKEEP L     R14,VWLOWSKY       CHECK FOR FIRST ROW FOR NEXT LEVEL
         LHI   R0,SKENTLEN
         SR    R14,R0
         CP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P000
         JNE   KBRKINC
*
         TM    VWFLAG1,VWNOFRST   FIRST FUNCTION SPECIFIED ???
         JO    KBRKINC
*
         LH    R15,VWSETLEN       SAVE PRE-CALCULATION RESULTS
         LTR   R15,R15
         JNP   KBRKINC
*
         L     R1,CALCBASE        COPY  FIRST ROW'S VALUES TO FRST AREA
         L     R14,EXTFRSTA
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO  MOVE
         SRL   R0,8
         J     KBRK1STE
KBRK1STL MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
KBRK1STE BRCT  R0,KBRK1STL
         EX    R15,MVCR14R1
*
KBRKINC  L     R14,VWLOWSKY       RESTORE COUNT ADDRESS
         ZAP   SKCOUNT-SORTKEY(L'SKCOUNT,R14),P001 INITIALIZE ROW COUNT
*
KBRKNTTL DS    0H
*        LH    R0,NEEDTITL        SORT  TITLE LINES   NEEDED ???
*        LTR   R0,R0
*        JNP   KBRKSETX           NO  - BYPASS
*
*        BRAS  R9,TTLPRINT        PRINT SORT KEY TITLES FOR THIS GROUP
*
KBRKSETX L     R14,SUBTOTAD       LOAD LOWEST LEVEL SET ADDRESS
                     EJECT
KBRKINIT LH    R15,VWSETLEN       SAVE FIRST  SET   OF  VALUES
         LTR   R15,R15
         JNP   KBRKDWNL
*
         L     R1,EXTLASTA        COPY   LAST ROW'S VALUES TO SAVE AREA
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO  MOVE
         SRL   R0,8
         J     KBRKLSTE
KBRKLSTL MVC   0(256,R1),0(R14)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
KBRKLSTE BRCT  R0,KBRKLSTL
         EX    R15,MVCR1R14
*
KBRKDWNL TM    VWFLAG1,VWDWNONL   DOWNLOAD FILE ONLY (SUPPRESS PRINT) ?
         JO    KBRKBLD            BYPASS ACTUAL PRINTING
                     SPACE 3
         L     R14,CURSRTKY       LOAD  ADDRESS OF LOWEST SORTKEY DEF
skey     USING SORTKEY,R14
*
         CLI   skey.SKFTRBRK+1,SUBTOTAL  PRINT THIS SUBTOTAL LINE ???
         JNE   KBRKBLD            NO  - BYPASS PRINTING
         DROP  skey
*
         CLI   VWSUMTYP+1,DETAIL  DETAIL REPORT ???
         JNE   KBRKCALL           NO  -  PRINT DATA (LOWEST PRINT LVL)
                     EJECT
KBRKLOOP L     R14,CURSRTKY       ANY  DETAIL LINES PRINTED ???
         CP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P000
         JE    KBRKBLD            NO  - SUBTOTAL UNNECESSARY
*
KBRKCALL DS    0H
         BRAS  R10,COLPRNT        PRINT COLUMN   TOTALS
***********************************************************************
*  REBUILD SORT BREAK TITLES FOR ALL LEVELS THAT CHANGED              *
***********************************************************************
KBRKBLD  CLI   EOREOFCD,C'F'      END-OF-FILE (NO  NEXT  RECORD) ???
         JE    KBRKROLL           YES - BYPASS BUILDING  NEW  TITLES
         L     R7,RECADDR         LOAD    NEW RECORD  ADDRESS
         BRAS  R10,BLDTKEY        BUILD   NEW TITLE   KEY
         L     R7,PREVRECA        RESTORE OLD RECORD  ADDRESS
*
***********************************************************************
*  ROLLUP COLUMN SUBTOTALS AND CHECK FOR NEW MAX/MIN VALUES           *
***********************************************************************
KBRKROLL LH    R0,VWCLCCOL        SET-UP  FOR ROLL-UP FUNCTION
         LTR   R0,R0              ANY COLUMNS USED ???
         JNP   KBRKSCNT           NO - BRANCH AND  SUM  COUNTS
*
         L     R6,CLCCOLTB        LOAD    ADDRESS OF CALC COL DEFN TBL
*
KBRKSUBT L     R2,0(,R6)          LOAD COLUMN DEFINITION ADDRESS
         USING COLDEFN,R2
         LH    R15,CDCLCOFF       LOAD COLUMN VALUE OFFSET
*
         CLC   SVBRKCNT,H001      WERE HIGHER LEVEL SUBTOTALS PRINTED ?
         JH    KBRKPREV           YES - RESET PREVIOUS "CT"   VALUES
         CLI   VWSUMTYP+1,DETAIL  DETAIL VIEW  ???
         JNE   KBRKMAX            NO  - LOWEST LEVEL IS DETAIL(NO RSET)
*
KBRKPREV L     R1,EXTPREVA        LOAD  BASE   ADDRESS
         AR    R1,R15             ADD   OFFSET
         GVBSTX   fp8,0(,r1)                RESET   TOTAL
*
***********************************************************************
*  PROPAGATE MAXIMUM TO NEXT HIGHER LEVEL                             *
***********************************************************************
KBRKMAX  TM    VWFLAG1,VWNOMIN+VWNOFRST SPECIAL FUNCTIONS USED ???
         JO    KBRKADD               NO - BYPASS SPECIAL LOGIC
*
         LR    R9,R3              COMPUTE MAXIMUM VALUES BASE ADDRESS
         S     R9,SUBTOTAD
         LR    R10,R9
         A     R9,EXTMAXA
         A     R10,EXTMINA        COMPUTE MINIMUM VALUES BASE ADDRESS
*
         TM    VWFLAG1,VWNOMIN
         JO    KBRKFRST
*
         LHI   R14,SKENTLEN       NEXT  LEVEL   SORT  KEY DEFN
         LCR   R14,R14
         A     R14,CURSRTKY
*
         CLC   CURSRTKY,VWLOWSKY  LOWEST BREAK  LEVEL ???
         JNE   KBRKMAX2           NO  - ROLLUP  LOWER LEVEL VALUE
*
         CLI   CDSUBOPT+1,DETMAX
         JE    KBRKMAX2
         CLI   VWSUMTYP+1,DETAIL
         JE    KBRKMAX2
*
         GVBLDX  fp0,0(r3,r15)                   load lowest subtotal
         CP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P000
         JE    KBRKMAX1
*
         LR    R14,R9
         AR    R14,R15
         AH    R14,VWSETLEN
         GVBLDX  fp1,0(,r14)                     load value into fp1/3
         cxtr  fp0,fp1                         ROLLUP LOWER VALUE?
         jnh   KBRKMAX4                        NO  - BYPASS UPDATE
         GVBSTX   fp0,0(,r14)                     save lower value
         J     KBRKMAX4
*
KBRKMAX1 LR    R14,R9
         AR    R14,R15
         AH    R14,VWSETLEN

         GVBSTX   fp0,0(,r14)                     Save lower value
         J     KBRKMAX4
*
KBRKMAX2 CP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P000
         JE    KBRKMAX3
*
         LR    R14,R9
         AR    R14,R15
         GVBLDX  fp0,0(,r14)                     load into fp0/fp2
         AH    R14,VWSETLEN
         GVBLDX  fp1,0(,r14)                     load 2nd operand
         cxtr  fp0,fp1                         NEW  MAXIMUM ???
         jnh   KBRKMAX4                        NO  - BYPASS UPDATE
         GVBSTX   fp0,0(,r14)                     save lower value
         J     KBRKMAX4
*
KBRKMAX3 LR    R14,R9
         AR    R14,R15
         LR    R1,R14
         AH    R14,VWSETLEN
         mvc   0(AccumDFPl,R14),0(R1)          ROLLUP LOWER VALUE
*
KBRKMAX4 LR    R14,R9
         AR    R14,R15
         GVBSTX   fp8,0(,r14)               RESET   TOTAL
*
***********************************************************************
*  PROPAGATE MINIMUM TO NEXT HIGHER LEVEL                             *
***********************************************************************
KBRKMIN  LHI   R14,SKENTLEN       NEXT  LEVEL   SORT  KEY DEFN
         LCR   R14,R14
         A     R14,CURSRTKY
*
         CLC   CURSRTKY,VWLOWSKY  LOWEST BREAK  LEVEL ???
         JNE   KBRKMIN2           NO  - ROLLUP  LOWER LEVEL VALUE
*
         CLI   CDSUBOPT+1,DETMIN
         JE    KBRKMIN2
         CLI   VWSUMTYP+1,DETAIL
         JE    KBRKMIN2
*
         GVBLDX  fp0,0(r3,r15)                   load value into fp0/2
         CP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P000
         JE    KBRKMIN1
*
         LR    R14,R10
         AR    R14,R15
         AH    R14,VWSETLEN
         GVBLDX  fp1,0(,r14)                     load value into fp1/3
         cxtr  fp0,fp1                         ROLLUP LOWER VALUE
         jnl   KBRKMIN4                        NO  - BYPASS UPDATE
         GVBSTX   fp0,0(,r14)                     save lower value
         J     KBRKMIN4
*
KBRKMIN1 LR    R14,R10
         AR    R14,R15
         AH    R14,VWSETLEN

         GVBSTX   fp0,0(,r14)                     Save lower value
         J     KBRKMIN4
*
KBRKMIN2 DS    0H
         CP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P000
         JE    KBRKMIN3
*
         LR    R14,R10
         AR    R14,R15
         GVBLDX  fp0,0(,r14)                     load into fp0/fp2
         AH    R14,VWSETLEN
         GVBLDX  fp1,0(,r14)                     load 2nd operand
         cxtr  fp0,fp1                         NEW  MAXIMUM ???
         jnl   KBRKMIN4                        NO  - BYPASS UPDATE
         GVBSTX   fp0,0(,r14)                     save lower value
         J     KBRKMIN4
*
KBRKMIN3 LR    R14,R10
         AR    R14,R15
         LR    R1,R14
         AH    R14,VWSETLEN
         mvc   0(AccumDFPl,R14),0(R1)          ROLLUP LOWER VALUE
*
KBRKMIN4 LR    R14,R10
         AR    R14,R15
         MVC   0(AccumDFPl,R14),high_dec_val   RESET MINIMUM VALUE
*
***********************************************************************
*  PROPAGATE FIRST VALUE TO NEXT HIGHER LEVEL                         *
***********************************************************************
KBRKFRST TM    VWFLAG1,VWNOFRST
         JO    KBRKADD
*
         LHI   R14,SKENTLEN       NEXT  LEVEL   SORT  KEY DEFN
         LCR   R14,R14
         A     R14,CURSRTKY
         CP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P000
         JNE   KBRKADD
*
         LR    R9,R3              COMPUTE MAXIMUM VALUES BASE ADDRESS
         S     R9,SUBTOTAD
         A     R9,EXTFRSTA
         AR    R9,R15
         LR    R1,R9
         AH    R1,VWSETLEN
         mvc   0(AccumDFPl,R1),0(R9)           ROLLUP LOWER VALUE
*
***********************************************************************
*  ROLLUP COLUMN SUBTOTALS TO NEXT HIGHER LEVEL                       *
***********************************************************************
KBRKADD  LA    R1,0(R3,R15)                    ROLL-UP  TO  NEXT LEVEL
         LR    R14,R1
         AH    R14,VWSETLEN
         GVBLDX  fp0,0(,r1)                      get 1st value in fp0/2
         GVBLDX  fp1,0(,r14)                     get 2nd value in fp1/3

         if CLI,CDSUBOPT+1,eq,NOSUBTOT         not SUBTOTAL ???
           GVBSTX   fp0,0(,r14)                   save current
         else
           axtr  fp1,fp1,fp0      add in value
           GVBSTX   fp1,0(,r14) and save it
         endif

         GVBSTX   fp8,0(,r1)                RESET   TOTAL
*
         AHI   R6,4                    ADVANCE TO  NEXT CALC COLUMN
         BRCT  R0,KBRKSUBT        LOOP THROUGH ALL COLUMNS
*
         DROP  R2
                     SPACE 3
KBRKSCNT L     R1,CURSRTKY        LOAD CURRENT SORT KEY COUNT ADDRESS
         LR    R14,R1             LOAD ADDRESS OF  HIGHER  LEVEL  COUNT
         LHI   R0,SKENTLEN
         SR    R14,R0
   AP  SKCOUNT-SORTKEY(L'SKCOUNT,R14),SKCOUNT-SORTKEY(L'SKCOUNT,R1) SUM
         ZAP   SKCOUNT-SORTKEY(L'SKCOUNT,R1),P000 ZERO  LOWER LEVEL CNT
                     EJECT
***********************************************************************
*  DECREMENT EXECUTIVE SUMMARY LEVEL INDEX (NEXT/HIGHER BREAK LEVEL)  *
***********************************************************************

         AH    R3,VWSETLEN        MOVE TO HIGHER  LEVEL SET
         ST    R3,CALCBASE
*
         LHI   R0,SKENTLEN        BACKUP  TO PRECEDING SORT KEY DEFN
         ST    R4,LSTSRTKY
         SR    R4,R0
         ST    R4,CURSRTKY
                     SPACE 3
         LARL  R15,KBRKBLD        ASSUME BYPASS PRINTING OF NEXT SUBTOT
*
         TM    VWFLAG1,VWDWNONL   DOWNLOAD FILE ONLY (SUPPRESS PRINT) ?
         JO    KBRKLEND           BYPASS ACTUAL PRINTING
*
         CLI   SKFTRBRK+1,SUBTOTAL  PRINT  NEXT   SUBTOTAL ???
         JNE   KBRKLEND           NO  -  BYPASS ASSUMPTION CORRECT
*
         LARL  R15,KBRKLOOP       CHANGE TO PRINT NEXT SUBTOTAL
*
KBRKLEND BCTR  R5,R15             LOOP   THROUGH  ALL SORT BREAKS
                     EJECT
***********************************************************************
*  SET-UP FOR NEXT "SORTKEY" WHICH TRIGGERED PREVIOUS SUB-TOTAL BREAK *
***********************************************************************
         CLI   EOREOFCD,C' '      "END OF REQUEST" OR "END OF FILE" ???
         JNE   KBRKEXIT            YES - SKIP PREPARATION FOR NEXT  KEY
*
         TM    VWFLAG2,VWEXEC+VWPVIT EXEC INFO OR PIVOT TABLE ???
         JNZ   KBRKSKEY           YES - BYPASS PAGE BREAK/BLANK LINE
         TM    VWFLAG1,VWDWNONL   DOWNLOAD-ONLY ???
         JO    KBRKSKEY           YES - BYPASS PAGE BREAK/BLANK LINE
*
         L     R14,LSTSRTKY       LOAD  ADDRESS  OF   LAST USED SORTKEY
         CLI   SKHDRBRK-SORTKEY+1(R14),NEWPAGE NEWPAGE FOR THIS KEY ???
         JE    KBRKPBRK                    YES - BREAK
         CLI   SKHDRBRK-SORTKEY+1(R14),TITLEL1
         JL    KBRKBLNK                    YES - BREAK
         CLI   SKHDRBRK-SORTKEY+1(R14),TITLEL5
         JH    KBRKBLNK                 NO  -  CHECK IF NEED BLANK LINE
*
KBRKPBRK MVC   NEEDTITL,VWBRKCNT  SET   NO. OF SORT  BREAK   LEVELS
         BRAS  R9,FOOTPRNT        PRINT PAGE   FOOTERS
         BRAS  R9,PAGEBRK         CALL  PAGE   BREAK SUBROUTINE
         J     KBRKSKEY
                     SPACE 3
KBRKBLNK LH    R0,SVBRKCNT        UPDATE  NUMBER OF  TITLE LINES NEEDED
         CH    R0,NEEDTITL
         JNH   KBRKBLN1
         STH   R0,NEEDTITL
*
KBRKBLN1 EQU   *
         CLI   NEEDDASH,C'Y'      WAS   DASH   LINE  PRINTED ???
         JE    KBRKSKEY           NO  - BYPASS BLANK LINE INSERTION
*
         LHI   R0,L'RPBLANK       PRINT BLANK  LINE
         L     R1,PRNTDCBA
         STH   R0,DCBLRECL-IHADCB(,R1)
         LA    R0,RPBLANK
         L     R15,PRNTSUBR
         BASR  R14,R15

KBRKSKEY L     R7,RECADDR         LOAD CURRENT  RECORD ADDRESS
*
         LA    R1,EXSORTKY        SAVE NEW SORT KEY
         L     R14,SVSORTKY
         LH    R15,EXSORTLN
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO  MOVE
         SRL   R0,8
         J     KBRKKEYE
*
KBRKKEYL MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
KBRKKEYE BRCT  R0,KBRKKEYL
         EX    R15,MVCR14R1
                     SPACE 3
KBRKEXIT L     R10,SAVER10        RESTORE   RETURN   ADDRESS
         BR    R10                RETURN TO POINT OF CALL
*
static   loctr
KBRKTTL  MVC   5(0,R1),SPACES         * * * *  E X E C U T E D  * * * *
code     loctr
*
         DROP  R4
         DROP  R7
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O L U M N   P R I N T   R O U T I N E                      *
*                                                                     *
*        R3 - POINTS TO SET OF COLUMN TOTALS TO BE PRINTED            *
*        R4 - POINTS TO LABEL/SORT TITLE FOR THIS  SORT BREAK LEVEL   *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING SORTKEY,R4
         USING EXTREC,R7
         USING VIEWREC,R8
*
COLPRNT  STM   R2,R10,SAVECOLP    SAVE  ORIGINAL   REGISTERS
*
***********************************************************************
*  INDICATE SUBTOTALS PRINTED                                         *
***********************************************************************
         MVI   GTOTLIND,C'Y'      GRAND TOTALS ARE TO  BE PRINTED

***********************************************************************
*  PRINT SUBTOTALS                                                    *
***********************************************************************
         CLI   SKFTRBRK+1,SUBTOTAL PRINT SUBTOTAL BREAK ???
         JNE   COLPEXIT            NO  - RETURN
*
         LA    R2,PRNTTEXT        LOAD   OUTPUT AREA ADDRESS
*
         CLI   SKDSPOPT+1,ASDATA   SUPPRESS SUBTOTAL PREFIX/LABELS ???
         JE    COLPCHKL            YES - SELECTIVELY RESET  COLUMNS
         CLI   SKDSPOPT+1,INCLDESC SUPPRESS SUBTOTAL PREFIX/LABELS ???
         JNE   COLPBLNK            NO  - RESET PRINT LINE  (REBUILD)
*
COLPCHKL C     R4,VWLOWSKY        LOWEST   BREAK  LEVEL    ???
         JNE   COLPRSET           NO  -    PRINT  SUMMARY  LINE
         CLI   VWSUMTYP+1,DETAIL  DETAIL   REQUEST  ???
         JNE   COLPBLNK           NO  -    BRANCH (MUST BE SUMMARY LVL)
*
***********************************************************************
*  SELECTIVELY BLANK OUT COLUMNS                                      *
*  WHICH ARE NOT SUBTOTALED  OR  ARE NOT PART OF BREAK LEVELS         *
***********************************************************************
*
COLPRSET L     R6,VWCOLADR        LOAD FIRST     COLUMN ADDRESS
         USING COLDEFN,R6
*
         LH    R15,VWCENTER       ANY  CENTERING ADJUSTMENT ???
         LTR   R15,R15
         JNP   COLPINIT           NO  -  BYPASS  BLANK  FILL
*
         MVI   0(R2),C' '         INSERT LEADING SPACE
         AHI   R15,-2             COMPUTE ADD'L  SPACES NEEDED (-1)
         JM    COLPINIT
         EX    R15,BLNKCENT
*
COLPINIT LHI   R1,SKENTLEN        LOAD   SORT   KEY   ENTRY LENGTH
         LH    R5,VWCOLCNT        LOAD   NUMBER OF  COLUMNS
COLPLOOP LR    R14,R4             RETAIN HIGHER LEVEL BREAK VALUES
*
COLPSAVE CLC   SKCOLNO-SORTKEY(L'SKCOLNO,R14),CDCOLNO BREAK LEVEL COL ?
         JE    COLPNEXT           YES -  RETAIN VALUE
         SR    R14,R1             BACKUP TO    HIGHER BREAK LEVEL
         C     R14,VWSKADDR
         JNL   COLPSAVE           LOOP   THROUGH  ALL HIGHER   LEVELS
*
         CLI   CDEXAREA+1,DTAREA  "DT"   AREA   FIELD ???
         JE    COLPCLR            YES -  CLEAR  IT
*
         CLI   CDSUBOPT+1,NOSUBTOT SUBTOTAL OPTION="NONE"
         JNE   COLPNEXT           NO  - BYPASS    COLUMN CLEAR
*
COLPCLR  LH    R14,CDCOLOFF       LOAD  COLUMN    OFFSET
         AH    R14,CDCOLGAP       ADD   GAP       SIZE
         AH    R14,VWCENTER       ADD   CENTERING ADJUSTMENT
         LA    R14,PRNTTEXT(R14)  ADD   BASE      ADDRESS
*
         LH    R15,CDCOLSIZ       LOAD  EDITED COLUMN      WIDTH (-1)
         EX    R15,BLNKTGT        INITIALIZE   TARGET AREA TO  SPACES
*
COLPNEXT AHI   R6,CDENTLEN        ADVANCE TO   NEXT COLUMN DEFINITION
         BRCT  R5,COLPLOOP        LOOP THROUGH ALL COLUMNS
*
         CLI   SKDSPOPT+1,INCLDESC BUILD SUBTOTAL PREFIX/LABELS ???
         JE    COLPCENT           YES -  CENTER   SUBTOTAL LABELS
         J     COLPBLD            NO  -  BYPASS   SUBTOTAL LABELS
*
***********************************************************************
*  BLANK OUT WHOLE LINE                                               *
***********************************************************************
static   loctr
COLPBL   MVC   PRNTRDW+5(0),PRNTRDW+4  * * *  E X E C U T E D  * * * *
code     loctr
*
COLPBLNK MVI   PRNTRDW+4,C' '     CLEAR  OUT  OLD VALUES (WHOLE LINE)
         LH    R14,VWLINELN
         BCTR  R14,0
         BCTR  R14,0
         EX    R14,COLPBL
*
COLPCENT LH    R14,VWLINELN       LOAD  LINE  LENGTH
         STH   R14,PRNTRDW
                     SPACE 3
COLPTITL CLI   SKDSPOPT+1,ASDATA  SUPPRESS PREFIX/LABELS   ???
         JE    COLPBLD            YES -  CENTER   SUBTOTAL LABELS
*
         LR    R1,R2              LOAD   OUTPUT   AREA  ADDRESS (TEMP)
         LR    R14,R4             LOAD   TITLE    AREA  ADDRESS
         LH    R0,SKTTLTOT        LOAD   TITLE    TEXT  LENGTH
         C     R4,VWLOWSKY        LOWEST LEVEL    BREAK    ???
         JNE   COLPLBL            NO  -  INCLUDE  SUBTOTAL LABEL
         CLI   VWSUMTYP+1,DETAIL  DETAIL REPORT   ???
         JE    COLPLBL            YES -  INCLUDE  SUBTOTAL LABEL
         SH    R0,SKPREFXL        NO  -  EXCLUDE  SUBTOTAL LABEL
*
         LH    R15,SKINDCNT       LOAD INDENTATION  COUNT
         AR    R14,R15            ADVANCE BEYOND INDENTATION
         AR    R1,R15             ADVANCE OUTPUT ADDRESS
         SR    R0,R15             EXCLUDE SPACES ALREADY INITIALIZED
         JNP   COLPBLD
*
         AH    R14,SKPREFXL       SKIP OVER SUBTOTAL LABEL
                     SPACE 3
COLPLBL  LR    R15,R0             LOAD ADJUSTED  LENGTH
         BCTR  R15,0
         EX    R15,COLPMVC        MOVE REMAINDER OF TITLE
                     SPACE 3
COLPBLD  BRAS  R9,COLBUILD        BUILD  COLUMN OUTPUT
*
         TM    VWFLAG1,VWZEROSP   ZERO SUPPRESS OPTION   ???
         JNO   COLPCHKD           NO  -  BYPASS NULL     CHECK
*
         LH    R0,NOTNULL         LOAD NON-ZERO COUNT
         LTR   R0,R0              ANY  NON-ZERO COLUMNS  ???
         JZ    COLPEXIT           NO  -  BYPASS THIS     ROW
*
COLPCHKD CLI   NEEDDASH,C'Y'      DASH  LINE    NEEDED/DEFERRED   ???
         JNE   COLPPAGE           NO  - BYPASS  PRINTING  OF DASH LINE
*
         CLI   VWSUMTYP+1,SUMMARY SUMMARY VIEW  ???
         JNE   COLPCHK1           NO  - BYPASS  TEST FOR LOWEST  LEVEL
         CLC   CURSRTKY,VWLOWSKY  LOWEST  SUMMARY  LEVEL ???
         JE    COLPPAGE           YES -     TREAT  AS    DETAIL
*
COLPCHK1 EQU   *
         LH    R15,VWPAGSIZ       COMPUTE   LINES  REMAINING ON   PAGE
         SH    R15,VWLINENO
         LH    R14,SVBRKCNT       ENOUGH ROOM FOR  SUBTOTALS (+2) ???
         AHI   R14,2
         CR    R14,R15
         JNH   COLPDASH           YES - PRINT DASH LINE
*
         CLI   VWDESTYP+1,ONLINE  ONLINE VIEWING REQUEST ???
         JE    COLPONLI           YES -  BYPASS  NEW  PAGE  HEADINGS
*
         MVC   NEEDTITL,VWBRKCNT  SET  NO.  OF SORT   BREAK LEVELS
         OC    SVBRKCNT,SVBRKCNT
         JNZ   COLPCHK2
         XC    NEEDTITL,NEEDTITL
*
COLPCHK2 EQU   *
         MVC   PAGERECA,RECADDR   SAVE    NEXT RECORD ADDRESS
         MVC   RECADDR,PREVRECA   RESTORE LAST RECORD ADDRESS
         BRAS  R9,FOOTPRNT        PRINT PAGE   FOOTERS
         BRAS  R9,PAGEBRK         CALL    PAGE BREAK  SUBROUTINE
         MVC   RECADDR,PAGERECA   RESTORE NEXT RECORD ADDRESS
         MVI   NEEDDASH,C' '      RESET   DASH NEEDED INDICATOR
         J     COLPPUT
                     SPACE 3
COLPONLI XC    VWLINENO,VWLINENO  RESET CURRENT LINE COUNT
*
COLPDASH LH    R0,DASHLEN         PRINT DASH    LINE
         L     R1,PRNTDCBA
         STH   R0,DCBLRECL-IHADCB(,R1)
         MVC   VWCURSEC,COLPBD
         LA    R0,DASHLINE
         L     R15,PRNTSUBR
         BASR  R14,R15
*
         MVI   NEEDDASH,C' '      RESET DASH     NEEDED   INDICATOR
         J     COLPPUT            INDICATE SUBTOTAL LINE
                     EJECT
COLPPAGE LH    R15,VWLINENO       LOAD  CURRENT LINE    COUNT
         CH    R15,VWPAGSIZ       CHECK AGAINST MAXIMUM PAGE  SIZE ???
         JL    COLPPUT            SKIP  PAGE  BREAK IF     ROOM
*
         MVC   NEEDTITL,VWBRKCNT  SET   NO. OF SORT BREAK  LEVELS
         BRAS  R9,FOOTPRNT        PRINT PAGE   FOOTERS
         BRAS  R9,PAGEBRK         CALL  PAGE BREAK SUBROUTINE
         if CLC,CURSRTKY,NE,VWLOWSKY  Not the lowest summary level
           MVI NEEDDASH,C' '      RESET DASH NEEDED INDICATOR
         endif
*
COLPPUT  LH    R0,NEEDTITL        SORT  TITLE  LINES   NEEDED
         if    LTR,R0,R0,p
           BRAS R9,TTLPRINT       PRINT SORT KEY TITLES FOR NEXT GROUP
         endif
*
         MVC   VWCURSEC,COLPSL    INDICATE  SUBTOTAL LINE
*
         LA    R0,PRNTRDW         LOAD  DATA ADDRESS (RDW)
         L     R1,PRNTDCBA        WRITE COLEXTR LINE
         MVC   DCBLRECL-IHADCB(2,R1),PRNTRDW
         L     R15,PRNTSUBR
         BASR  R14,R15
                     SPACE 3
COLPEXIT LM    R2,R10,SAVECOLP    RESTORE ORIGINAL  REGISTERS
         BR    R10                RETURN
*
static   loctr
COPYCOL  MVC   5(0,R1),0(R2)      * * * * *  E X E C U T E D   * * * *
COLPMVC  MVC   0(0,R1),SKTITLE-SORTKEY(R14)  * * * E X E C U T E D * *
COLPSL   DC    CL2'SL'
COLPBD   DC    CL2'BD'
BLNKCENT MVC   1(0,R2),0(R2)                 * * * E X E C U T E D * *
code     loctr
*
         DROP  R4
         DROP  R7
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O L U M N   O U T P U T   B U I L D   R O U T I N E        *
*                                                                     *
*              (R2: CONTAINS OUTPUT  AREA ADDRESS)                    *
*        (CURSRTKY: CONTAINS CURRENT "SORTKEY" DATA ADDRESS)          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTREC,R7
         USING VIEWREC,R8
*
COLBUILD ST    R2,OUTPCURR        SAVE OUTPUT  AREA ADDRESS
*
         L     R3,CALCBASE        LOAD COLUMN  VALUE   BASE   ADDRESS
*
         LH    R5,VWCOLCNT        INITIALIZE   COLUMN  COUNTER
         STH   R5,NOTNULL         INITIALIZE NOT NULL  COUNTER
*
         L     R6,VWCOLADR        LOAD ADDRESS OF COLUMN SPECS
         USING COLDEFN,R6
                     SPACE 3
COLBLOOP LH    R14,SVBRKCNT       DETAIL RECORD ???
         LTR   R14,R14
         JNP   COLBCHKS           YES -  IGNORE SUBTOTAL CALC OPTIONS
*
         BRAS  R10,CALCCOLM       PERFORM COLUMN CALCULATIONS IF  ANY
*
COLBCHKS L     R14,CDSRTDEF       IS THIS COLUMN SORTED ???
         LTR   R14,R14
         JNP   COLBCHKP           NO  -   CONTINUE
*
         CLI   VWDESTYP+1,BATCH   HARDCOPY/BATCH  ???
         JNE   COLBCHKP           NO  -   CONTINUE
         CLI   SKDSPOPT-SORTKEY+1(R14),CATEGOR CATEGORIZE THIS COL ???
         JE    COLBNULL           YES -   BYPASS COLUMN
*
COLBCHKP CLI   CDPRTIND,C'Y'      PRINT   THIS  COLUMN ???
         JE    COLBPRNT           YES -   CONTINUE
*
COLBNULL LH    R0,NOTNULL         DECREMENT NOT NULL COUNT
         BCTR  R0,0
         STH   R0,NOTNULL
         J     COLBNEXT           ADVANCE TO NEXT COLUMN SPECIFICATION
*
COLBPRNT LA    R0,CDDETMSK        ASSUME  DETAIL  MASK
         ST    R0,SAMSKADR
*
         L     R14,CURSRTKY       LOWEST  BREAK   LEVEL(DETAIL OR FILE)
         LTR   R14,R14
         JNP   COLBCSV            YES -   IGNORE  SUBTOTAL OPTION
*
         CLI   VWSUMTYP+1,DETAIL  DETAIL  REQUEST ???
         JE    COLBSUBC           YES -   BRANCH (MUST  BE SUMMARY LVL)
         C     R14,VWLOWSKY       LOWEST  BREAK   LEVEL ???
         JNL   COLBCSV            YES -   BYPASS SWITCH TO SUBTOTAL VAL
*
***********************************************************************
*  AT SUMMARY LEVELS ABOVE LOWEST PRINT LEVEL ONLY PRINT SUBTOTALS    *
***********************************************************************
COLBSUBC TM    VWFLAG2,VWPVIT     PIVOT TABLE ???
         JNO   COLBSUBT
*
         CLI   CDEXAREA+1,DTAREA  SOURCE FROM "DT" AREA ???
         JNE   COLBSUBT           NO  -  CHECK SUBTOTAL OPTION
*
         L     R14,PARMDADR       CHECK PARAMETER  OPTION
         CLI   DISPXIDT-PARMDATA(R14),C'Y' DISPLAY DT AREA VALUES ???
         JE    COLBCSV            YES - INCLUDE COLUMN
*
COLBSUBT CLI   CDSUBOPT+1,NOSUBTOT  SUBTOTAL OPTION=NONE ???
         JNE   COLBCSV            NO  -    PROCESS  THIS SUBTOTAL
*
         LH    R0,NOTNULL         DECREMENT NOT NULL COUNT
         BCTR  R0,0
         STH   R0,NOTNULL
         J     COLBNEXT           ADVANCE TO NEXT COLUMN SPECIFICATION
                     EJECT
***********************************************************************
*  BUILD COMMA DELIMITED OUTPUT STRING IF OUTPUT DEST TYPE = "CSV"    *
***********************************************************************
COLBCSV  TM    VWFLAG2,VWBLDCSV   COMMA SEPARATED  ???
         JZ    COLBGAP            NO  - PROCESS INTERCOLUMN GAP
*
         L     R14,OUTPCURR       LOAD  CURRENT OUTPUT POSITION
*
***********************************************************************
*  INSERT XML COLUMN TAG IF SPECIFIED                                 *
***********************************************************************
*        CLI   VWDESTYP+1,XML
*        BRNE
*
***********************************************************************
*  INSERT COMMA IF NOT FIRST COLUMN                                   *
***********************************************************************
         CR    R14,R2             AT BEG OF OUTPUT AREA (1ST COLUMN) ??
         JNH   COLBCSV1           YES -  BYPASS SEPARATOR
         MVC   0(1,R14),VWFLDDEL  INITIALIZE SEPARATOR
         AHI   R14,1
*
COLBCSV1 EQU   *
         LH    R15,CDDATLEN       LOAD   DATA LENGTH (-1)
         AHI   R15,1
*
         LA    R1,EXSORTKY        LOAD   DATA AREA ADDRESS
         AH    R1,CDDATOFF
*
         CLI   CDEXAREA+1,DTAREA  SOURCE FROM "DT" AREA   ???
         JE    COLBCSV6           YES -  CONTINUE
         CLI   CDEXAREA+1,SKAREA  SOURCE FROM "SK" AREA   ???
         JNE   COLBCTAR           NO  -  EDIT  SUMMARY COLUMN VALUE(CT)
*
         L     R10,CDSRTDEF       SORT   BREAK KEY DEFN  ADDR
         LTR   R10,R10
         JNP   COLBCSV8
         USING SORTKEY,R10
*
         CLI   SKSRTSEQ,C'D'      SORT   DESCENDING ???
         JNE   COLBCSV0
*
         LH    R15,SKFLDLEN       LOAD SORT FIELD LENGTH
         BCTR  R15,0              DECREMENT FOR   "EX"
         EX    R15,SORTMVC        COPY  THE VALUE
         larl  R1,HEXFF           REVERSE   THE   SORT SEQUENCE
         EX    R15,SORTCOMP
         LA    R1,LKUPKEY         POINT TO  CONVERTED  VALUE
*
COLBCSV0 OC    CDOUTCON,CDOUTCON   OUTPUT   CONTENT    CODE ???
         JNZ   COLBCSV2
*
         CLI   SKFLDFMT+1,FM_pack  packed ??
         JE    COLBCSV2
         CLI   SKFLDFMT+1,FM_SORTP SORTABLE PACKED ???
         JE    COLBCSV2
         CLI   SKFLDFMT+1,FM_SORTB SORTABLE BINARY ???
         JE    COLBCSV2
         CLI   SKFLDFMT+1,FM_BIN   BINARY   ???
         JNE   COLBCSV8
*
COLBCSV2 llgtr R1,r1              make sure ok for 64 bit
         STg   R1,SAVALADR
         MVC   SAVALLEN,SKFLDLEN  COPY  SORTED LENGTH
*
         MVC   SAVALFMT,SKFLDFMT  COPY  SORTED FORMAT
         MVC   SAVALCON,SKFLDCON  COPY  SORTED CONTENT
         MVC   SAVALDEC+1(1),SKFLDDEC  COPY SORTED DECIMALS
         MVC   SAVALRND+1(1),SKFLDRND  COPY ROUNDING FACTOR
         mvc   savalsgn,skfldsgn
*
         J     COLBEDIT
*
         DROP  R10
*
COLBCSV6 AH    R1,EXSORTLN        COMPUTE ACTUAL SRC ADDR IN DATA AREA
         AH    R1,EXTITLLN
*
COLBCSV8 LH    R0,CDCALCNT        ANY CALCULATIONS INVOLVED  ???
         LTR   R0,R0
         JP    COLBCTAR           YES - USE COLUMN VERSION   INSTEAD
*
         CLI   CDSUBOPT+1,NOSUBTOT SUBTOTAL OPTION ="NONE"   ???
         JNE   COLBCTAR           NO  - USE COLUMN VERSION   INSTEAD
*
         LH    R0,NOTNULL         DECREMENT NOT NULL COUNT
         BCTR  R0,0
         STH   R0,NOTNULL
*
         J     COLBSCAN           REMOVE LEADING  BLANKS
                     EJECT
***********************************************************************
*  INITIALIZE INTERCOLUMN GAP IF NOT COMMA DELIMITED OUTPUT MEDIA     *
***********************************************************************
COLBGAP  LH    R14,CDCOLOFF       LOAD  COLUMN    OFFSET
         AH    R14,VWCENTER       ADD   CENTERING ADJUSTMENT
         AR    R14,R2             ADD   BASE      ADDRESS
*
         LH    R15,CDCOLGAP       LOAD  INTERCOLUMN  GAP SIZE
         LTR   R15,R15            ANY   INTERVENING  SPACES  ???
         JNP   COLBDATA           NO  - BYPASS FILL  LOGIC
*
         MVI   0(R14),C' '        INITIALIZE  FIRST  BYTE
         AHI   R14,1
*
         AHI   R15,-2             ADJUST LENGTH
         JM    COLBDATA           BYPASS NEXT INSTR  IF JUST ONE BYTE
         EX    R15,BLNKTGT        PROPAGATE INTERVENING SPACES
         LA    R14,1(R14,R15)
*
COLBDATA LA    R1,EXSORTKY        LOAD   DATA AREA ADDRESS
         AH    R1,CDDATOFF
*
         CLI   CDEXAREA+1,DTAREA  SOURCE FROM "DT" AREA  ???
         JE    COLBDTAR           YES -  BRANCH
         CLI   CDEXAREA+1,SKAREA  SOURCE FROM "SK" AREA  ???
         JNE   COLBCTAR           NO  -  EDIT SUMMARY COLUMN VALUE
*
         L     R10,CDSRTDEF       SORT   BREAK KEY DEFN ADDR
         LTR   R10,R10
         JNP   COLBCNT
         USING SORTKEY,R10
*
         LH    R15,SKFLDLEN       LOAD SORT FIELD LENGTH
         BCTR  R15,0              DECREMENT FOR   "EX"
         STH   R15,CDDATLEN       UPDATE SOURCE   DATA  LENGTH
*
         CLI   SKSRTSEQ,C'D'      SORT   DESCENDING ???
         JNE   COLBSKCK
*
         EX    R15,SORTMVC        COPY  THE VALUE
         Larl  R1,HEXFF           REVERSE   THE   SORT SEQUENCE
         EX    R15,SORTCOMP
         LA    R1,LKUPKEY         POINT TO  CONVERTED  VALUE
*
COLBSKCK OC    CDOUTCON,CDOUTCON   OUTPUT   CONTENT    CODE  ???
         JNZ   COLBSKAR
*
         CLI   SKFLDFMT+1,FM_SORTP SORTABLE PACKED ???
         JE    COLBSKAR
         CLI   SKFLDFMT+1,FM_SORTB SORTABLE BINARY ???
         JE    COLBSKAR
         CLI   SKFLDFMT+1,FM_BIN   BINARY   ???
         JNE   COLBCNT
*
COLBSKAR llgtr R1,r1              make sure ok for 64 bit
         STG   R1,SAVALADR
         MVC   SAVALLEN,SKFLDLEN
*
         MVC   SAVALFMT,SKFLDFMT  COPY  SORTED FORMAT
         MVC   SAVALCON,SKFLDCON  COPY  SORTED CONTENT
         MVC   SAVALDEC+1(1),SKFLDDEC  COPY SORTED DECIMALS
         MVC   SAVALRND+1(1),SKFLDRND  COPY ROUNDING FACTOR
         mvc   savalsgn,skfldsgn
*
         J     COLBCNUL
*
         DROP  R10
*
COLBDTAR AH    R1,EXSORTLN        COMPUTE ACTUAL SRC ADDR IN DATA AREA
         AH    R1,EXTITLLN
*
COLBCNT  LH    R0,CDCALCNT        ANY CALCULATIONS INVOLVED  ???
         LTR   R0,R0
         JP    COLBCTAR           YES - USE COLUMN VERSION   INSTEAD
*
         CLI   CDSUBOPT+1,NOSUBTOT SUBTOTAL OPTION ="NONE"   ???
         JNE   COLBCTAR           NO  - USE COLUMN VERSION   INSTEAD
*
         LH    R0,NOTNULL         DECREMENT NOT NULL COUNT
         BCTR  R0,0
         STH   R0,NOTNULL
*
         LH    R15,CDCOLSIZ       LOAD EDITED COLUMN WIDTH    (-1)
         EX    R15,BLNKTGT        INITIALIZE  TARGET AREA  TO SPACES
         LR    R0,R15             SAVE COLUMN WIDTH  (-1)
*
         LH    R15,CDDATLEN       LOAD DATA  LENGTH  (-1)
         SR    R0,R15             COMPUTE EXCESS ROOM IN  COLUMN
         JNP   COLBMOVE           BRANCH  IF NO  EXCESS
*
         CLI   CDOUTFMT+1,FM_ALNUM ALPHANUMERIC  ???
         JE    COLBMOVE
         CLI   CDOUTFMT+1,FM_ALPHA
         JE    COLBMOVE
         AR    R14,R0             RIGHT JUSTIFY  DATA
*
COLBMOVE EX    R15,MVCDATA        COPY  DATA TO  OUTPUT RECORD
         J     COLBNEXT
                     EJECT
***********************************************************************
*  FORMAT CALCULATED COLUMNS ("CT")                                   *
***********************************************************************
COLBCTAR MVI   SAVALFMT+1,FM_float SET    decimal float
*
         MVHHI SAVALDEC,0         SET    NO. OF  DECIMALS
*
         MVHHI SAVALRND,0         SET    ROUNDING FACTOR
*
         LA    R0,ACCUMWRK        SET    VALUE   ADDRESS
         llgtr R0,r0              make sure ok for 64 bit
         STG   R0,SAVALADR
         LHI   R0,AccumDFPl       SET    VALUE   LENGTH (BYTES)
         STH   R0,SAVALLEN
*
***********************************************************************
*  SELECT SET OF ACCUMULATORS                                         *
***********************************************************************
COLBDET  LR    R1,R3              ASSUME CURRENT SET   OF  ACCUMULATORS
*
COLBMAX  CLI   CDSUBOPT+1,DETMAX  DETAILED MAX   ???
         JNE   COLBMIN
         S     R1,SUBTOTAD        COMPUTE SET    OFFSET FROM BEG
         A     R1,EXTMAXA
         J     COLBCOFF
*
COLBMIN  CLI   CDSUBOPT+1,DETMIN  DETAILED MIN   ???
         JNE   COLBDET2
         S     R1,SUBTOTAD
         A     R1,EXTMINA
         J     COLBCOFF
*
COLBDET2 L     R0,CURSRTKY        AT  A  BREAK   LEVEL ???
         LTR   R0,R0
         JNP   COLBCOFF           NO  -  IGNORE  SUBTOTAL  OPTION
*
         CLI   VWSUMTYP+1,DETAIL  DETAIL VIEW   ???
         JE    COLBCHKF           YES - SKIP LOWEST SUMMARY LEVEL CHECK
*
         CLI   VWDESTYP+1,FILEFMT DATA  FILE OUTPUT ???
         JE    COLBCHKF           YES - SKIP LOWEST SUMMARY LEVEL CHECK
*
         C     R0,VWLOWSKY        LOWEST SUMMARY VIEW BREAK LEVEL ???
         JNL   COLBCOFF           YES -
*
COLBCHKF TM    VWFLAG1,VWNOMIN+VWNOFRST  SPECIAL FUNCTIONS  ???
         JO    COLBCOFF           NO  -  BYPASS
*
         CLI   CDSUBOPT+1,FIRST   FIRST  VALUE  (BEGINNING) ???
         JNE   COLBCHKL
         S     R1,SUBTOTAD        COMPUTE SET    OFFSET FROM BEG
         A     R1,EXTFRSTA
         J     COLBCOFF
*
COLBCHKL CLI   CDSUBOPT+1,LAST    LAST   VALUE  (ENDING)    ???
         JNE   COLBCHKM
         L     R1,EXTLASTA
         J     COLBCOFF
*
COLBCHKM CLI   CDSUBOPT+1,MAX     MAXIMUM VALUE (HIGHEST)   ???
         JNE   COLBCHKN
         S     R1,SUBTOTAD        COMPUTE SET    OFFSET FROM BEG
         A     R1,EXTMAXA
         J     COLBCOFF
*
COLBCHKN CLI   CDSUBOPT+1,MIN     MINIMUM VALUE (LOWEST)    ???
         JNE   COLBCOFF
         S     R1,SUBTOTAD
         A     R1,EXTMINA
         J     COLBCOFF
*
COLBCOFF AH    R1,CDCLCOFF           ADD  CALC COLUMN OFFSET  TO BASE
         MVC   ACCUMWRK(AccumDFPl),0(R1)  COPY VALUE
         GVBLDX  fp0,0(,r1)         get the dfp value
*
         cxtr  fp0,fp8            NULL VALUE  ???
         JNE   COLBEDIT           NO - BYPASS
*
COLBCNUL LH    R0,NOTNULL         DECREMENT NOT NULL  COUNT
         BCTR  R0,0
         STH   R0,NOTNULL
                     EJECT
***********************************************************************
*  EDIT OUTPUT VALUE ("CT" AND SORTABLE VALUES)                       *
***********************************************************************
COLBEDIT MVC   SAOUTFMT,CDOUTFMT  COPY  EDITED   FORMAT
         MVC   SAOUTCON,CDOUTCON  COPY  EDITED   CONTENT  CODE
         MVC   SAOUTDEC+1(1),CDNDEC    COPY  EDITED   DECIMALS
         MVC   SAOUTRND+1(1),CDRNDFAC COPY  ROUNDING FACTOR
         MVC   SAOUTSGN,CDSIGNED  COPY  SIGNED   IND
         MVC   SAOUTJUS,CDOUTJUS  COPY  DATA     JUSTIFICATION CODE
*
         LH    R15,CDCOLSIZ       LOAD  EDITED  COLUMN   WIDTH (-1)
         EX    R15,BLNKTGT        INITIALIZE TARGET AREA TO SPACES
         AHI   R15,1              INCREMENT TO TRUE LENGTH
*
         LH    R0,CDMSKLEN        LOAD  MASK LENGTH
         STH   R0,SAMSKLEN        STORE MASK LENGTH
         LTR   R1,R0              MASK SPECIFIED ???
         JNP   COLBTGT            NO  -   BYPASS JUSTIFICATION
         SR    R0,R15             COMPUTE EXCESS ROOM IN COLUMN (NEG)
         JNM   COLBTGT            BRANCH  IF NO  EXCESS
*
         LR    R15,R1             USE   MASK LENGTH
         CLI   CDOUTJUS,C'R'      RIGHT JUSTIFY  ???
         JNE   COLBTGT            NO  - BYPASS   JUSTIFICATION
*
         SR    R14,R0             RIGHT JUSTIFY  DATA (MINUS NEG VAL)
*
COLBTGT  ST    R14,DL96TGTA       SAVE   TARGET ADDRESS
         STH   R15,DL96LEN        PASS  EDITED SIZE TO  "GVBDL96"
*
COLBCALL LARL  R10,COLBNEXT       INITIALIZE RETURN ADDRESS
         TM    VWFLAG2,VWBLDCSV   COMMA SEPARATED   ???
         JNO   colbede            NO  - SKIP RETURN ADDRESS OVERRIDE
         LArl  R10,COLBLEFT       RETURN AND  LEFT  JUSTIFY DATA
colbede  equ   *
*
         L     R15,CDFMTFUN       LOAD  FORMATTING FUNCTION ADDRESS
         BR    R15                CALL  ROUTINE (BRANCH TABLE NOT USED)
*
***********************************************************************
*  CALL "GVBDL96" TO FORMAT CALCULATED COLUMN                         *
***********************************************************************
COLBDL96 LA    R1,DL96LIST        LOAD PARAMETER LIST ADDRESS
         llgf  R15,GVBDL96A       LOAD PROGRAM        ADDRESS
         BASsm R14,R15            CALL "GVBDL96"
*
         LTR   R15,R15            SUCCESSFUL?
         BZR   R10                YES, CONTINUE
*
         CLI   SAFMTERR,X'2'      FIELD  OVERFLOW (TRUNCATION) ???
         JNE   COLBERR1           NO  -  BAD ERROR
*
         CLI   SAVALFMT+1,FM_ALNUM SOURCE  DATA TYPE ALPHANUMERIC ???
         JNE   COLBOVER            NO - INDICATE OVERFLOW
         BR    R10                 YES - CONTINUE
*
*
***********************************************************************
*  CALCULATED COLUMN OVERFLOW                                         *
***********************************************************************
COLBOVER LHI   R0,L'VWOVRFIL      OVERFLOW  FILL CONSTANT
         LA    R1,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
         J     COLBERR2
*
COLBERR1 LHI   R0,L'VWERRFIL-1    LOAD   ERROR  FILL  LENGTH (-1)
         LA    R1,VWERRFIL
*
COLBERR2 L     R14,DL96TGTA       LOAD   COLUMN ADDRESS
         LH    R15,CDCOLSIZ       LOAD   COLUMN WIDTH (-1)
         CR    R15,R0             COLUMN WIDER  THAN  FILL ???
         JNH   COLBCSV3           NO  -  BYPASS ADJUSTMENT
         LR    R15,R0             YES -  USE  FILL LENGTH
COLBCSV3 EQU   *
         EX    R15,MVCDATA        COPY ERROR  FILL TO COLUMN
*
         TM    VWFLAG2,VWBLDCSV   COMMA SEPARATED  ???
         JZ    COLBNEXT           NO  - PROCESS INTERCOLUMN GAP
*
         L     R14,DL96TGTA       LOAD  FORMATTED DATA ADDRESS
         AH    R14,DL96LEN        LOAD  FORMATTED DATA LENGTH
         J     COLBOUTP
                     EJECT
***********************************************************************
*  LEFT JUSTIFY COMMA SEPARATED OUTPUT FIELDS(EXCLUDE TRAILING BLANKS)*
***********************************************************************
COLBLEFT L     R14,DL96TGTA       LOAD  FORMATTED DATA ADDRESS
         LH    R15,DL96LEN        LOAD  FORMATTED DATA LENGTH
         LR    R1,R14
COLBSCAN LR    R10,R1             SAVE  1ST  BYTE ADDRESS
         if    cli,vw_string_delimiter,ne,x'00',and,  quote needed and +
               cli,cdoutfmt+1,ne,fm_edit            NOT edited numeric
           mvc 0(1,14),vw_string_delimiter
           la  r14,1(,r14)
         endif
*
         tm    VWFLAG2,VWFXWDTH   Fixed Width?
         JNZ   COLBSHFT           Yes - Skip strip spaces and zeroes
*
COLBLEAD CLI   0(R1),C' '         LEADING BLANK  ???
         JE    COLBSKIP
         CLI   0(R1),X'00'        LEADING NULL   ???
         JNE   COLBBACK
COLBSKIP AHI   R1,1
         BRCT  R15,COLBLEAD
*
         CLI   0(R10),C' '        AT LEAST ONE  SPACE  ???
         JNE   COLBOUTP           NO - ALL NULL
         LR    R1,R10
         J     COLBSHFT           KEEP 1  BLANK IF ALL BLANK
*
COLBBACK AR    R15,R1             SCAN BACKWARDS
*
COLBTRAI BCTR  R15,0
         CLI   0(R15),C' '        TRAILING   BLANK ???
         JE    COLBTRAI           YES - LOOP
         CLI   0(R15),X'00'       TRAILING   NULL  ???
         JE    COLBTRAI           YES - LOOP
         CLI   CDOUTFMT+1,FM_EDIT EDITED NUMERIC COLUMN ???
         JNE   COLBTREN
         CLI   CDNDEC,0           DECIMAL PLACES PRESENT ??
         JNH   COLBTREN
         CLI   0(R15),C'0'        TRAILING DECIMAL ZERO ???
         JE    COLBTRAI           YES - LOOP
COLBTREN SR    R15,R1             COMPUTE  DATA  LENGTH (-1)
*
COLBSHFT EX    R15,MVCDATA        COPY  DATA TO  OUTPUT RECORD
         LA    R14,1(R14,R15)
         if    cli,vw_string_delimiter,ne,x'00',and,  quote needed and +
               cli,cdoutfmt+1,ne,fm_edit            NOT edited numeric
           mvc 0(1,14),vw_string_delimiter
           la  r14,1(,r14)
         endif
*
COLBOUTP ST    R14,OUTPCURR
*
COLBNEXT AHI   R6,CDENTLEN        ADVANCE TO  NEXT COLUMN DEFINITION
         BRCT  R5,COLBLOOP        LOOP THROUGH ALL COLUMNS
                     SPACE 3
         LH    R15,VWSETLEN       SAVE PREVIOUS COLUMN VALUES
         LTR   R15,R15
         JNP   COLBEXIT
*
         LR    R1,R3
         L     R14,EXTPREVA
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO  MOVE
         SRL   R0,8
         J     COLBMVCE
COLBMVCL MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
COLBMVCE BRCT  R0,COLBMVCL
         EX    R15,MVCR14R1
*
COLBEXIT BR    R9                 RETURN
*
static   loctr
MVCDATA  MVC   0(0,R14),0(R1)     * * * *   E X E C U T E D   * * * *
code     loctr
*
         DROP  R6
         DROP  R7
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P E R F O R M   C O L U M N   C A L C U L A T I O N S        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING VIEWREC,R8
*
CALCCOLM STM   R2,R9,SAVECALC     SAVE  CALLER'S REGISTERS
*
         LH    R7,VWCLCCOL        LOAD  NUMBER   OF CALCULATED COLUMNS
         LTR   R7,R7              ANY   COLUMNS  ???
         JNP   CALCEXIT           NO  - RETURN
*
         L     R6,CLCCOLTB        LOAD  CALCULATED COLUMN INDEX TABLE
*
CALCCOL  L     R2,0(,R6)          LOAD  COLUMN DEFINITION ADDRESS
         USING COLDEFN,R2
*
         LH    R15,CDCALCNT       LOAD  NO. OF CALCULATIONS
         LTR   R15,R15            ANY   CALCULATIONS ???
         JNP   CALCADV            NO  - ADVANCE TO NEXT COLUMN
*
         L     R5,CDCALCTB        LOAD  ADDRESS OF CALCULATION TABLE
         USING CALCTBL,R5
*
         CLC   SVBRKCNT,H000      DETAIL EXTRACT  RECORD LEVEL ???
         JNE   CALCCHKS           NO  -  CHECK  SUBTOTAL OPTIONS
         CLI   VWSUMTYP+1,DETAIL  DETAIL REQUEST ???
         JE    CALCLOOP           YES -  PERFORM CALCULATIONS
         CLI   CDCALOPT+1,DETCALC CHECK  IF DETAIL LEVEL CALCULATIONS
         JE    CALCLOOP           YES -  PERFORM CALCULATIONS
         CLI   CDCALOPT+1,RECALC
         JE    CALCLOOP
*
CALCCHKS DS    0H
         CLI   VWSUMTYP+1,DETAIL  DETAIL REQUEST ???
         JE    CALCCHKR           YES -  CHECK   FOR  REPEAT CALC
*
         L     R9,CURSRTKY        FIRST  BREAK  LEVEL (SUMMARY REQUEST)
         LTR   R9,R9
         JNP   CALCCHKD
         C     R9,VWLOWSKY
         JNE   CALCCHKR           NO  -  CHECK  FOR REPEAT  CALC
*
CALCCHKD DS    0H
         CLI   CDCALOPT+1,DETCALC HAVE CALCULATIONS ALREADY BEEN DONE
         JE    CALCADV            YES - DON'T  NEED TO REDO
         CLI   CDCALOPT+1,RECALC  HAVE CALCULATIONS ALREADY BEEN DONE
         JE    CALCADV            YES - DON'T  NEED TO REDO
         J     CALCRSET           NO  - DO THEM FOR FIRST  TIME
*
CALCCHKR DS    0H
         CLI   CDCALOPT+1,BRKCALC CHECK  IF REPEAT CALCULATIONS ???
         JE    CALCRSET           YES -  REPEAT    CALCULATIONS
         CLI   CDCALOPT+1,RECALC
         JNE   CALCADV            NO  -  BYPASS    CALCULATIONS
*
         DROP  R2
*
CALCRSET L     R3,CALCTGTA        LOAD TARGET ADDRESS
         GVBSTX   fp8,0(,r3)                RESET   TOTAL
*
CALCLOOP LM    R2,R4,calcfunc     LOAD CALCULATION   ADDRESSES
         LTR   R2,R2              REACHED   MARKER   ???
         JM    CALCDONE           YES -  EXIT
         CLI   CALCOPER+1,PUSHC   SOURCE FROM  ANOTHER  COLUMN ???
         JNE   CALCCALL
         A     R4,CALCBASE
CALCCALL BASR  R9,R2              BRANCH TO SPECIFIED OPERATOR
         J     CALCLOOP           LOOP THROUGH ALL CALCULATIONS
*
CALCDONE L     R2,0(,R6)          LOAD COLUMN  DEFN ADDRESS FROM LIST
         USING COLDEFN,R2
*
         LH    R14,CDCLCOFF       COPY RESULT  FROM STACK
         A     R14,CALCBASE
         MVC   0(AccumDFPl,R14),ACCUMWRK
*
CALCADV  AHI   R6,4               ADVANCE   TO NEXT CALC  COLUMN INDEX
         BRCT  R7,CALCCOL         LOOP THROUGH ALL  COLUMNS
*
CALCEXIT LM    R2,R9,SAVECALC
         BR    R10                RETURN
*
CALCTEST CHI   R1,-1
         JE    CALCDONE
*
         CHI   R1,-2
         JE    CALCADV
*
         CHI   R1,-4
         JE    EXCPSKIP
*
         GVBLDX  fp1,=ld'1'         get a value of 1 into fp1/3
         GVBLDX  fp0,accumwrk       and the accum value into fp0/2
         cxtr  fp0,fp1
         JE    EXCPACPT
         J     EXCPSKIP
*
         DROP  R2
*
static   loctr
*
         DROP  R5
         DROP  R8
*
         USING SORTKEY,R3
*
SUMBLNK  CLC   0(0,R14),SPACES    * * * *  E X E C U T E D  * * * *
MVCR14R1 MVC   0(0,R14),0(R1)     * * * *  E X E C U T E D  * * * *
*
MVCINDNT MVC   SKTITLE(0),SKTITLE * * * *  E X E C U T E D  * * * *
MVCLABEL MVC   0(0,R14),SKLABEL   * * * *  E X E C U T E D  * * * *
MVCPREFX MVC   0(0,R14),SKSUBLBL  * * * *  E X E C U T E D  * * * *
MVCVALUE MVC   0(0,R14),0(R6)     * * * *  E X E C U T E D  * * * *
MVCR1R14 MVC   0(0,R1),0(R14)     * * * *  E X E C U T E D  * * * *
HEXCOMP  XC    0(0,R14),0(R6)     * * * *  E X E C U T E D  * * * *
BLDLKUP  MVC   LKUPKEY(0),0(R14)  * * * *  E X E C U T E D  * * * *
SORTMVC  MVC   LKUPKEY(0),0(R1)   * * * *  E X E C U T E D  * * * *
SORTCOMP XC    LKUPKEY(0),0(R1)   * * * *  E X E C U T E D  * * * *
*
         DROP  R3
*
MVCSPACE MVC   0(0,R1),SPACES                * E X E C U T E D  * * * *
MVCTITL1 MVC   0(0,R14),VWCOMPNM-VIEWREC(R8) * E X E C U T E D  * * * *
MVCTITL2 MVC   0(0,R14),VWTITLE-VIEWREC(R8)  * E X E C U T E D  * * * *
BLDRSPAC MVC   4(0,R1),SPACES                * E X E C U T E D  * * * *
BLDRCPY  MVC   0(0,R15),0(R14)               * E X E C U T E D  * * * *
BLNKTGT  MVC   0(0,R14),SPACES               * E X E C U T E D  * * * *
*
CHKHDRKY OC    HDSORTKY-HDRREC(0,R1),HDSORTKY-HDRREC(R1) T E D  * * * *
*
TTLPMVC  MVC   4+1(0,R1),SKTITLE-SORTKEY(R14)  E X E C U T E D  * * * *
code     loctr
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
static   loctr
*
F4       DC    F'4'
*
MODE31   DS   0XL4
OPENPARM DC    XL8'8000000000000000'
*
STATOPEN OPEN  (,OUTPUT),MODE=31,MF=L
STATOPENL EQU  *-STATOPEN
*
STATCLOS CLOSE (),MODE=31,MF=L
STATCLOSL EQU  *-STATCLOS
*
H000     DC    H'00'
H001     DC    H'01'
H002     DC    H'02'
H003     DC    H'03'
H004     DC    H'04'
*
P000     DC    P'0'
P001     DC    P'1'
high_dec_val dc ld'(inf)'
*
C99      DC    CL02'99'
*
AND      DC    CL03'AND'
OR       DC    CL03'OR '
WORKEYEB DC    CL08'MR88WORK'
*
COUNT    DC    CL06'COUNT:'
COUNTMSK DC    XL12'402020206B2020206B202120'
*
RPTBL    DC    CL2'BL'
RPTPH    DC    CL2'PH'
RPTPF    DC    CL2'PF'
HDRPCH   DC    CL2'CH'
HDRPBD   DC    CL2'BD'
TTLPSH   DC    CL2'SH'
PAGELIT  DC    CL08'PAGE NO:'
RPTIDLIT DC    CL11'REPORT ID: '
VIEWLIT  DC    CL10'VIEW   NO:'
*
MR88NAME DC    CL08'GVBMR88 '
BLDRMASK DC    XL08'4020202020202120'
RPBLANK  DS   0CL06               REPORT   BLANK LINE
         DC    HL02'6'            LINE     LENGTH
         DC    XL02'0000'
         DC    CL01' '            CARRIAGE CONTROL
         DC    CL01' '            BLANK
                     EJECT
*
GVBMR87A DC    V(GVBMR87)         ADDRESS OF "GVBMR87"
GVBDL96A DC    V(GVBDL96)         ADDRESS OF "GVBDL96"
GVBTP90A DC    V(GVBTP90)         ADDRESS OF "GVBTP90"
*
MERGEADR DC    A(MERGEREC+X'80000000')       "MERGEREC" SUBROUTINE
MERGENXT DC    A(MERGNEXT+X'80000000')       "MERGNEXT" SUBROUTINE
VSAMWRTA DC    A(VSAMWRT+X'80000000')        "VSAMWRT"  SUBROUTINE
*
MASKNEG  DC    XL3'402160'
MASKPARN DC    XL3'40214D'
*
I        DC    CL2'I '
OP       DC    CL2'OP'            VSAM   OPEN
CL       DC    CL2'CL'            VSAM   CLOSE
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        T E X T   A D D R E S S   T A B L E                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
NOTTLADR DC    A(NOTTLTXT)
CANCLADR DC    A(CANCLTXT)
*
         LTORG
*
FMTFUNTB DC    A(COLBDL96)        +00 - MUST CALL "GVBDL96"
         DC    A(COLBPTOP)        +04 - PACKED TO  PACKED
         DC    A(COLBPTOF)        +08 - PACKED TO  FIXED   (UNSIGNED)
         DC    A(COLBPTON)        +12 - PACKED TO  NUMERIC
         DC    A(COLBPTOU)        +16 - PACKED TO  NUMERIC (UNSIGNED)
         DC    A(COLBPTOB)        +20 - PACKED TO  BINARY
         DC    A(COLBMSK1)        +24 - PACKED TO  MASK 1
         DC    A(COLBMSK2)        +28 - PACKED TO  MASK 2
         DC    A(COLBMSK3)        +32 - PACKED TO  MASK 3
         DC    A(COLBMSK4)        +36 - PACKED TO  MASK 4
         DC    A(COLBMSK5)        +40 - PACKED TO  MASK 5
         DC    A(COLBMSK6)        +44 - PACKED TO  MASK 6
         DC    A(COLBMSK7)        +48 - PACKED TO  MASK 7
         DC    A(COLBMSK8)        +52 - PACKED TO  MASK 8
         DC    A(COLBMSK9)        +56 - PACKED TO  MASK 9
         DC    A(COLBMSKA)        +60 - PACKED TO  MASK 10
         DC    A(COLBMSKB)        +64 - PACKED TO  MASK 11
         DC    A(COLBMSKC)        +68 - PACKED TO  MASK 12
         DC    A(COLBMSKD)        +72 - PACKED TO  MASK 13
         DC    A(COLBMSKE)        +76 - PACKED TO  MASK 14
         DC    A(COLBMSKF)        +80 - PACKED TO  MASK 15
         DC    A(COLBMSKG)        +84 - PACKED TO  MASK 16
         DC    A(COLBMSKH)        +88 - PACKED TO  MASK 17
         DC    A(COLBMSKI)        +92 - PACKED TO  MASK 18
         DC    A(COLBMSKJ)        +96 - PACKED TO  MASK 19
         DC    A(COLBMSKK)        +100- PACKED TO  MASK 20
         DC    A(COLBMSKL)        +104- PACKED TO  MASK 21
*
SNAPDCB  DCB   DSORG=PS,DDNAME=SNAPDATA,MACRF=(W),                     X
               RECFM=VBA,LRECL=125,BLKSIZE=1632
*
CONDTBL  DS   0CL06               EXCEPTION CONDITION TABLE
         DC    CL2'EQ',XL4'00000082'
         DC    CL2'GT',XL4'00000022'
         DC    CL2'LT',XL4'00000042'
         DC    CL2'GE',XL4'000000B2'
         DC    CL2'LE',XL4'000000D2'
         DC    CL2'NE',XL4'00000072'
CONDCNT  EQU   ((*-CONDTBL)/(L'CONDTBL))
*
DISPTBL  DS   0CL17               DISPOSITION  TABLE
         DC    CL3'SHR',CL7'KEEP   ',CL7'KEEP   '
         DC    CL3'NEW',CL7'CATLG  ',CL7'DELETE '
         DC    CL3'NEW',CL7'CATLG  ',CL7'CATLG  '
         DC    CL3'OLD',CL7'DELETE ',CL7'DELETE '
DISPCNT  EQU   ((*-DISPTBL)/(L'DISPTBL))
*
RECFMTBL DS   0CL2                RECORD FORMAT
         DC    CL2'  '            0 - UNDEFINED
         DC    CL2'FB'            1 - FB
         DC    CL2'VB'            2 - VB
         DC    CL2'FA'            3 - FBA
         DC    CL2'VA'            4 - VBA
RECFMCNT EQU   ((*-RECFMTBL)/(L'RECFMTBL))
*
DSORGTBL DS   0CL4                DATASET  ORGANIZATION
         DC    CL4'PS  '
         DC    CL4'PO  '
         DC    CL4'VSAM'
         DC    CL4'DA  '
DSORGCNT EQU   ((*-DSORGTBL)/(L'DSORGTBL))
*
VSAMTBL  DS   0CL4                VSAM  ORGANIZATION
         DC    CL4'KSDS'
         DC    CL4'ESDS'
         DC    CL4'RRDS'
         DC    CL4'LSDS'
VSAMCNT  EQU   ((*-VSAMTBL)/(L'VSAMTBL))
*
***********************************************************************
*  DATA FORMAT                                                        *
***********************************************************************
DATFMTBL DC    C' '               '00' - N/A
         DC    C'X'               '01' - ALPHANUMERIC
         DC    C'A'               '02' - ALPHABETIC
         DC    C'N'               '03' - NUMERIC
         DC    C'P'               '04' - PACKED
         DC    C'S'               '05' - PACKED  -  SORTABLE
         DC    C'B'               '06' - BINARY
         DC    C'I'               '07' - BINARY  -  SORTABLE
         DC    C'C'               '08' - BINARY     CODED      DECIMAL
         DC    C'M'               '09' - MASKED     NUMERIC
         DC    C'E'               '0A' - EDITED     NUMERIC
         DC    C'F'               '0B' - FLOATING   POINT
         DC    C'G'               '0C' - GENEVA     NUMBER
*
code     loctr
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C H E C K   F O R   L O W E S T   P R I N T   L E V E L      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING VIEWREC,R8
*
FRSTSET  TM    VWFLAG1,VWNOMIN+VWNOFRST
         JO    FRSTEXIT
*
         LH    R15,VWSETLEN
         LTR   R15,R15
         JNP   FRSTEXIT
*
         L     R1,CALCBASE        COPY  FIRST ROW'S VALUES TO FRST AREA
         L     R14,EXTFRSTA
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO  MOVE
         SRL   R0,8
         J     FRSTMVC2
FRSTMVC1 MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
FRSTMVC2 BRCT  R0,FRSTMVC1
         EX    R15,MVCR14R1
*
         L     R1,CALCBASE        COPY  FIRST ROW'S VALUES TO MAX  AREA
         L     R14,EXTMAXA
         LH    R15,VWSETLEN
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO  MOVE
         SRL   R0,8
         J     FRSTMVC4
FRSTMVC3 MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
FRSTMVC4 BRCT  R0,FRSTMVC3
         EX    R15,MVCR14R1
*
         L     R1,CALCBASE        COPY  FIRST ROW'S VALUES TO MIN  AREA
         L     R14,EXTMINA
         LH    R15,VWSETLEN
         LA    R15,255(,R15)      ROUND  UP TO 256 MULTIPLE
         LR    R0,R15             LENGTH OF RECORD TO  MOVE
         SRL   R0,8
         J     FRSTMVC6
FRSTMVC5 MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
FRSTMVC6 BRCT  R0,FRSTMVC5
         EX    R15,MVCR14R1
*
FRSTEXIT BR    R9                       RETURN
*
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   A C C U M U L A T E   F I R S T   L E V E L   S U B T O T A L S   *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTREC,R7
         USING VIEWREC,R8
*
SUM      LH    R0,VWCLCCOL        LOAD  NO. OF CALCULATED  COLUMNS USED
         LTR   R0,R0              ANY   USED   ???
         JNP   SUMMERGE           NO  - CHECK  FOR "DT" AREA MERGE
*
         L     R9,CLCCOLTB        LOAD  ADDRESS OF CALC COL  DEFN
*
SUMADD   L     R6,0(,R9)
         USING COLDEFN,R6
*
         LH    R15,CDCLCOFF                     LOAD COLUMN  OFFSET
*
         L     R1,EXTCOLA                       LOAD NORMALIZED ADDRESS
         AR    R1,R15                           ADD  OFFSET
         GVBLDX  fp0,0(,r1)                       get the value
         L     R14,SUBTOTAD                     LOAD SUBTOTAL   ADDRESS
         AR    R14,R15                          ADD  OFFSET
         GVBLDX  fp1,0(,r14)                      get the value
*
***********************************************************************
*  ACCUMULATE COLUMN SUBTOTAL (UNLESS NO SUBTOTALING)                 *
***********************************************************************
         CLI   CDSUBOPT+1,SUBTOT                SUBTOTAL ???
         JE    SUMSUM                           YES - BRANCH AND SUM
         CLI   CDSUBOPT+1,NOSUBTOT              NO  SUBTOTAL ???
         JE    SUMMOVE                          YES - BRANCH AND MOVE
*
         cli   cdsubopt+1,detmax                                  pgcxx
         je    summove                                            pgcxx
         cli   cdsubopt+1,detmin                                  pgcxx
         je    summove                                            pgcxx
         cli   cdsubopt+1,first                                   pgcxx
         je    sum_summary_check                                  pgcxx
         CLI   CDSUBOPT+1,LAST                  LAST  VALUE  ???
         JNE   SUMSUM                           NO  - BRANCH AND SUM
sum_summary_check ds 0h                                           pgcxx
         CLI   VWSUMTYP+1,SUMMARY               SUMMARY VIEW ???
         JE    SUMSUM                           YES - BRANCH AND SUM
*
SUMMOVE  GVBSTX   fp0,0(,r14)                      NO  - MOVE
         J     SUMMAX
SUMSUM   axtr  fp1,fp1,fp0                      ADD  RESULTS TO  TOTALS
         GVBSTX   fp1,0(,r14)                      and save it
*
SUMMAX   TM    VWFLAG1,VWNOMIN                  SPECIAL FUNCTIONS ??
         JO    SUMADV
*
***********************************************************************
*  UPDATE MAXIMUM DETAIL LEVEL VALUE                                  *
***********************************************************************
         L     R14,EXTMAXA                      LOAD BASE    ADDRESS
         AR    R14,R15                          ADD  OFFSET
         GVBLDX  fp1,0(,r14)                      get the value
         if cxtr,fp0,gt,fp1                     NEW  MAXIMUM VALUE ???
           GVBSTX fp0,0(,R14)                      YES - UPDATE
         endif
*
***********************************************************************
*  UPDATE MINIMUM DETAIL LEVEL VALUE                                  *
***********************************************************************
         L     R14,EXTMINA                      LOAD BASE    ADDRESS
         AR    R14,R15                          ADD  OFFSET
         GVBLDX  fp1,0(,r14)                      get the value
         if cxtr,fp0,lt,fp1                     NEW  MAXIMUM VALUE ???
           GVBSTX fp0,0(,R14)                      YES - UPDATE
         endif
*
***********************************************************************
*  RESET NORMALIZED DETAIL LEVEL CALCULATED COLUMN ARRAY ELEMENT      *
***********************************************************************
SUMADV   GVBSTX   fp8,0(,r1)                RESET   TOTAL
*
***********************************************************************
*  LOOP THROUGH ALL CALCULATED COLUMNS                                *
***********************************************************************
         AHI   R9,4                      ADVANCE DEFN   ADDRESS
         BRCT  R0,SUMADD                 LOOP   THROUGH ALL    COLUMNS
*
***********************************************************************
*  MERGE PRIOR "DT" AREA VALUES INTO CURRENT "DT" AREA                *
***********************************************************************
SUMMERGE CLI   VWSUMTYP+1,MERGESUM DATA  AREA MERGE  OPTION ???
         JNE   SUMEXIT
*
         LH    R0,VWCOLCNT
         L     R6,VWCOLADR
         USING COLDEFN,R6
*
         L     R15,PREVRECA       LOCATE PREVIOUS "DT" AREA
         LA    R2,EXSORTKY-EXTREC(,R15)
         AH    R2,EXSORTLN-EXTREC(,R15)
         AH    R2,EXTITLLN-EXTREC(,R15)
*
         LA    R3,EXSORTKY        LOCATE CURRENT  "DT" AREA
         AH    R3,EXSORTLN
         AH    R3,EXTITLLN
*
SUMMERG2 CLI   CDEXAREA+1,DTAREA
         JNE   SUMMERG8
*
***********************************************************************
*  KEEP FIRST NON-BLANK "DT" AREA FIELD VALUE IN PREVIOUS RECORD      *
***********************************************************************
         LH    R15,CDDATOFF       LOAD  FIELD   OFFSET IN "DT" AREA
         LA    R1,0(R2,R15)       LOAD  PREVIOUS VALUE ADDRESS
         LA    R14,0(R3,R15)      LOAD  CURRENT  VALUE ADDRESS
         LH    R15,CDDATLEN       LOAD  FIELD   LENGTH
         EX    R15,SUMBLNK        VALUE PRESENT ???
         JNE   SUMMERG8           YES - DON'T  OVERLAY
         EX    R15,MVCR14R1       NO  - COPY
*
SUMMERG8 AHI   R6,CDENTLEN
         BRCT  R0,SUMMERG2
*
SUMEXIT  BR    R10                RETURN
*
         DROP  R6
         DROP  R7
         DROP  R8
*
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P E R F O R M   R O W   E X C E P T I O N   L O G I C        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING VIEWREC,R8
         USING COLDEFN,R6
*
EXCPCHK  STM   R2,R9,SAVECALC     SAVE  CALLER'S REGISTERS
*
         L     R5,VWEXCOND        LOAD EXCEPTION STACK ADDRESS
         USING CALCTBL,R5
         LTR   R5,R5              ANY  EXCEPTIONS ???
         JNP   EXCPRETN           NO - RETURN TO +4  (ACCEPT)
*
         LH    R15,VWEXCNT        LOAD EXCEPTION CONDITION  COUNT
*
EXCPLOOP LM    R2,R4,0(R5)        LOAD CALCULATION   ADDRESSES
         LTR   R2,R2              REACHED   MARKER   ???
         JM    EXCPACPT           YES -  EXIT
         CLI   CALCOPER+1,PUSHC   SOURCE FROM  ANOTHER  COLUMN ???
         JNE   EXCPCALL
         A     R4,CALCBASE
EXCPCALL BASR  R9,R2              BRANCH TO SPECIFIED OPERATOR
         J     EXCPLOOP           LOOP THROUGH ALL CALCULATIONS
*
EXCPACPT GVBLDX  fp1,=ld'1'         get a value of 1 into fp1/3
         GVBLDX  fp0,accumwrk       and the accum value into fp0/2
         cxtr  fp0,fp1
         jne   EXCPSKIP
*
EXCPRETN LM    R2,R9,SAVECALC     RESTORE REGISTERS
         B     4(,R10)            RETURN TO +4 (ACCEPT)
*
EXCPSKIP LH    R0,VWCLCCOL        LOAD   MAXIMUM NO. OF COLUMNS USED
         L     R14,CALCBASE       POINT  TO CALCULATION RESULTS
*
EXCPZERO GVBSTX   fp8,0(,r14)               RESET   TOTAL
         AHI   R14,AccumDFPl             ADVANCE TO  NEXT COLUMN
         BRCT  R0,EXCPZERO        LOOP   THROUGH ALL COLUMNS
*
         LM    R2,R9,SAVECALC     RESTORE REGISTERS
         B     0(,R10)            RETURN TO +0 (SKIP)
*
         DROP  R5
         DROP  R6
         DROP  R8
                     EJECT
         USING VIEWREC,R8
         USING CALCTBL,R5
*
CALCBRN  L     R1,CALCVALU             BRANCH  OFFSET
         LTR   R1,R1
         JM    CALCTEST
         AR    R5,R1
         AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS
*
CALCEQ   ds    0h
         GVBLDX  fp0,0(,r3)              load the first
         GVBLDX  fp1,0(,r4)                and the second
         cxtr  fp0,fp1                 COMPARE TWO  AGRUMENTS
         JNE   CALCEQ5
         L     R1,CALCVALU             BRANCH  OFFSET
         LTR   R1,R1
         JM    CALCTEST
         AR    R5,R1
CALCEQ5  AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS
*
CALCne   ds    0h
         GVBLDX  fp0,0(,r3)              load the first
         GVBLDX  fp1,0(,r4)                and the second
         cxtr  fp0,fp1                 COMPARE TWO  AGRUMENTS
         JE    CALCNE5
         L     R1,CALCVALU             BRANCH  OFFSET
         LTR   R1,R1
         JM    CALCTEST
         AR    R5,R1
CALCNE5  AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS
*
CALCgt   ds    0h
         GVBLDX  fp0,0(,r3)              load the first
         GVBLDX  fp1,0(,r4)                and the second
         cxtr  fp0,fp1                 COMPARE TWO  AGRUMENTS
         JNH   CALCGT5
         L     R1,CALCVALU             BRANCH  OFFSET
         LTR   R1,R1
         JM    CALCTEST
         AR    R5,R1
CALCGT5  AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS
*
CALCge   ds    0h
         GVBLDX  fp0,0(,r3)              load the first
         GVBLDX  fp1,0(,r4)                and the second
         cxtr  fp0,fp1                 COMPARE TWO  AGRUMENTS
         JL    CALCGE5
         L     R1,CALCVALU             BRANCH  OFFSET
         LTR   R1,R1
         JM    CALCTEST
         AR    R5,R1
CALCGE5  AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS
*
CALClt   ds    0h
         GVBLDX  fp0,0(,r3)              load the first
         GVBLDX  fp1,0(,r4)                and the second
         cxtr  fp0,fp1                 COMPARE TWO  AGRUMENTS
         JNL   CALCLT5
         L     R1,CALCVALU             BRANCH  OFFSET
         LTR   R1,R1
         JM    CALCTEST
         AR    R5,R1
CALCLT5  AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS
*
CALCle   ds    0h
         GVBLDX  fp0,0(,r3)              load the first
         GVBLDX  fp1,0(,r4)                and the second
         cxtr  fp0,fp1                 COMPARE TWO  AGRUMENTS
         JH    CALCLE5
         L     R1,CALCVALU             BRANCH  OFFSET
         LTR   R1,R1
         JM    CALCTEST
         AR    R5,R1
CALCLE5  AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS
*
CALCPSHV MVC   0(AccumDFPl,R3),0(R4)   PUSH    VALUE  (CONSTANT)
         AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS
*
CALCPSHC MVC   0(AccumDFPl,R3),0(R4)   PUSH    COLUMN  VALUE
         AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS
*
CALCadd  ds    0h
         GVBLDX  fp0,0(,r3)              load the first
         GVBLDX  fp1,0(,r4)                and the second
         axtr  fp0,fp0,fp1             add them
         GVBSTX   fp0,0(,r3)                save result
         AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS

CALCsub  ds    0h
         GVBLDX  fp0,0(,r3)              load the first
         GVBLDX  fp1,0(,r4)                and the second
         sxtr  fp0,fp0,fp1             subtract them
         GVBSTX   fp0,0(,r3)                save result
         AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS

CALCmult ds    0h
         GVBLDX  fp0,0(,r3)              load the first
         GVBLDX  fp1,0(,r4)                and the second
         mxtr  fp0,fp0,fp1             multiply them
         GVBSTX   fp0,0(,r3)                save result
         AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         br    r9

CALCdiv  ds    0h
         GVBLDX  fp0,0(,r3)              load the first
         GVBLDX  fp1,0(,r4)                and the second
         dxtr  fp0,fp0,fp1             divide them
         GVBSTX   fp0,0(,r3)                save result
         AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         br    r9
                     EJECT
CALCNEG  ds    0h                      MULTIPLY  BY -1
         GVBLDX  fp0,0(,r3)              load the first
         GVBLDX  fp1,=ld'-1'               and the second
         mxtr  fp0,fp0,fp1             multiply them
         GVBSTX   fp0,0(,r3)                save result
         AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS
*
CALCabs  ds    0h                      MULTIPLY  BY -1
         GVBLDX  fp0,0(,r3)              load the first
         lpdfr fp0,fp0                  make it positive
         GVBSTX   fp0,0(,r3)                save result
         AHI   R5,CALCLEN              ADVANCE TO NEXT CALC TABLE ENTRY
         BR    R9                      CONTINUE WITH CALCULATIONS
*
         DROP  R5
         DROP  R8
static   loctr
         ltorg
code     loctr
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P A G E   B R E A K   R O U T I N E                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING VIEWREC,R8
*
PAGEBRK  ST    R9,SAVEPAGE        SAVE RETURN ADDRESS

PAGEBAT  BRAS  R9,RPTPRNT         PRINT  REPORT TITLE    LINES
*
PAGE_B10 BRAS  R9,HDRPRNT         PRINT  COLUMN HEADINGS AND DASH LINE
*
PAGETITL DS    0H
         BRAS  R9,TTLPRINT        PRINT  SORT KEY TITLES FOR NEXT GROUP
*
PAGEEXIT DS    0H
         L     R9,SAVEPAGE        RESTORE RETURN ADDRESS (R9)
         BR    R9                 RETURN
*
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        B U I L D   S O R T   K E Y   T I T L E S                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTREC,R7
         USING VIEWREC,R8
*
BLDTKEY  STM   R2,R10,SAVETKEY    SAVE  ORIGINAL REGISTERS
*
         L     R3,CURSRTKY        LOAD  CURRENT SORT   KEY  DEFINITION
         USING SORTKEY,R3
*
         LH    R2,SKINDCNT        LOAD  INDENTATION    COUNT
         LTR   R15,R2             ANY   LEADING SPACES ???
         JNP   BLDTK00            NO  - BYPASS  FILL   LOGIC
*
         MVI   SKTITLE,C' '       INSERT LEADING SPACE
         AHI   R15,-2             COMPUTE ADD'L  SPACES NEEDED (-1)
         JM    BLDTK00
         EX    R15,MVCINDNT
                     SPACE 3
***********************************************************************
*  COPY SUBTOTAL PREFIX/LABEL                                         *
***********************************************************************
BLDTK00  LH    R15,SKSUBLEN       LOAD  SUBTOTAL  PREFIX LENGTH
         LTR   R15,R15            SUBTOTAL PREFIX AVAILABLE ???
         JNP   BLDTK01            NO  - BRANCH
*
         LA    R14,SKTITLE(R2)    COMPUTE  CURRENT END OF TITLE ADDRESS
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCPREFX       COPY SUBTOTAL PREFIX
         LA    R14,1(R14,R15)     ADVANCE  TARGET ADDRESS TO PREFIX END
         LA    R2,1(R2,R15)       RESTORE  TRUE   LENGTH
         CLC   SKPREFXL,SKSUBLEN  PREFIX ONLY CONTAINS SUBTOTAL LABEL ?
         JE    BLDTK03            YES - NO SEPARATOR  OR  VALUE
         AHI   R2,1               ADD  LENGTH OF " " SEPARATOR
*
         MVI   0(R14),C' '        INSERT SEPARATING " "
                     EJECT
***********************************************************************
*  COPY SORT KEY PREFIX/LABEL                                         *
***********************************************************************
BLDTK01  CLI   SKDSPOPT+1,INCLDESC ONLY INCLUDE SUBTOTAL LABEL + DESC ?
         JE    BLDTK03            YES - SKIP   SORT  KEY LABEL + VALUE
*
         LH    R15,SKLBLLEN       LOAD  LABEL  LENGTH
         LTR   R15,R15            LABEL AVAILABLE ???
         JNP   BLDTK02            NO  - BRANCH
*
         LA    R14,SKTITLE(R2)    COMPUTE  CURRENT END OF TITLE ADDRESS
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCLABEL       COPY    LABEL
         LA    R2,1(R2,R15)       ADD     LABEL LENGTH
*
***********************************************************************
*  COPY/CONVERT SORT KEY VALUE                                        *
***********************************************************************
BLDTK02  LH    R15,SKCOLSIZ       LOAD   FORMATTED VALUE LENGTH
         LTR   R15,R15            LENGTH VALID  ???
         JNP   BLDTK03            NO  -  BYPASS BUILD
*
         LA    R14,SKTITLE(R2)    COMPUTE CURRENT END OF  TITLE ADDRESS
*
         CH    R2,SKINDCNT        CURRENT LENGTH  = INDENTATION ???
         JE    TK02_10            YES -   NO INTERVENING  SPACE NEEDED
         MVI   0(R14),C' '
         AHI   R14,1
         AHI   R2,1
*
TK02_10  CLI   SKFLDFMT+1,FM_ALNUM   NON-NUMERIC FORMAT ???
         JE    TK02_40
         CLI   SKFLDFMT+1,FM_ALPHA
         JE    TK02_40            YES  - NO  CONVERSION NEEDED
*
         ST    R14,DL96TGTA       FORMATTED SORT KEY VALUE ADDRESS
         STH   R15,DL96LEN        FORMATTED SORT KEY VALUE LENGTH
         AR    R2,R15             CURRENT TOTAL LENGTH
*
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,BLNKTGT        INITIALIZE TARGET AREA TO SPACES
*
         LA    R6,EXSORTKY        COMPUTE VALUE ADDRESS
         AH    R6,SKVALOFF
         llgtr R6,r6              make sure ok for 64 bit
         STg   R6,SAVALADR
*
         CLI   SKSRTSEQ,C'D'      DESCENDING SORT SEQUENCE?
         JNE   TK02_20            N - NO ADJUSTMENT NECESSARY
*
         LA    R14,LKUPKEY        --> DL96 VALUE WORK AREA
         llgtr R14,r14              make sure ok for 64 bit
         STg   R14,SAVALADR
         LH    R15,SKFLDLEN
         BCTR  R15,0
         EX    R15,MVCVALUE       COPY THE VALUE
         Larl  R6,HEXFF           REVERSE THE SORT SEQUENCE
         EX    R15,HEXCOMP
*
TK02_20  MVC   SAVALLEN,SKFLDLEN  LENGTH  OF SOURCE
         MVC   SAVALFMT,SKFLDFMT  SOURCE  FORMAT
         MVC   SAVALDEC+1(1),SKFLDDEC  SOURCE DECIMALS
         MVC   SAVALRND+1(1),SKFLDRND  SCALING FACTOR
         MVC   SAMSKLEN,SKFLDMLN  MASK LENGTH
         LA    R0,SKFLDMSK        MASK FOR TARGET
         ST    R0,SAMSKADR
*
         MVC   SAOUTFMT,H001      TARGET  FORMAT
         OC    SAMSKLEN,SAMSKLEN
         JZ    TK02_21
         MVI   SAOUTFMT+1,FM_MASK
*
TK02_21  EQU   *
         MVC   SAOUTDEC+1(1),SKFLDDEC  TARGET  DECIMALS
         MVC   SAOUTRND+1(1),SKFLDRND  SCALING FACTOR
         MVI   SAOUTJUS,C'L'      JUSTIFICATION
*
         LA    R1,DL96LIST        --> DL96 PARM LIST
         llgf  R15,GVBDL96A       --> GVBDL96
         bassm R14,R15            CALL "GVBDL96"
*
         LTR   R15,R15            SUCCESSFUL?
         JZ    BLDTK03            Y - CONTINUE
*
TK02_30  Larl  R6,BADNUMS         --> ERROR MESSAGE
         L     R14,DL96TGTA       --> MSG AREA IN TITLE LINE
         LHI   R15,L'BADNUMS-1         LENGTH OF MESSAGE
         EX    R15,MVCVALUE       MOVE THE ERROR MESSAGE
         J     BLDTK03            CONTINUE WITH PROCESSING
*
***********************************************************************
*  ALPHANUMERIC SORT KEY VALUE                                        *
***********************************************************************
TK02_40  LA    R6,EXSORTKY        COMPUTE VALUE ADDRESS
         AH    R6,SKVALOFF
*
         CH    R15,SKFLDLEN       FORMATTED LENGTH = VALUE LENGTH ???
         JE    TK02_45            YES - BYPASS POSSIBLE PADDING
         BCTR  R15,0
         EX    R15,BLNKTGT
*
         LH    R15,SKFLDLEN       CHECK FOR POSSIBLE TRUNCATION
         CH    R15,SKCOLSIZ
         JNH   TK02_45
         LH    R15,SKCOLSIZ
*
TK02_45  BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCVALUE       COPY VALUE
         if CLI,SKSRTSEQ,eq,C'D'  DESCENDING  SORT SEQUENCE ???
           Larl R6,HEXFF          YES - EXECUTE ONE'S COMPLEMENT
           EX  R15,HEXCOMP
         endif
         LA    R2,1(R2,R15)       ADD     VALUE LENGTH
*
***********************************************************************
*  LOOK-UP SORT KEY DESCRIPTION                                       *
***********************************************************************
BLDTK03  STH   R2,SKTTLTOT        SAVE  CURRENT TOTAL
*
         L     R0,SKLRID          LOAD  LOOK-UP "LRID"
         LTR   R0,R0              TITLE SPECIFIED ???
         JP    BLDTK03A           YES - PERFORM LOOK-UP
*
         OC    VWRTADDR,VWRTADDR  CUSTOM REPORT TITLES  ???
         JNZ   BLDTK20
         J     BLDTK16
*
BLDTK03A L     R6,SKLBADDR        LOAD  RECORD BUFFER ADDRESS
         USING LKUPBUFR,R6
*
         LA    R14,EXSORTKY       COMPUTE  TITLE  KEY ADDRESS
         AH    R14,EXSORTLN
         AH    R14,SKTTLOFF
         LH    R15,LBKEYLEN       LOAD   KEY  LENGTH (-1)
         EX    R15,BLDLKUP        COPY   LOOK-UP KEY
*
BLDTK04  Larl  R15,SRCHTBL        LOOK-UP  MEMORY RESIDENT TITLE (31-B)
         BASR  R10,R15
         J     BLDTK05            NOT FOUND LABEL
         J     BLDTK06            FOUND     LABEL
*
BLDTK05  L     R6,NOTTLADR        POINT  TO NOT FOUND MESSAGE
         L     R3,CURSRTKY        LOAD SORT KEY  DEFINITION ADDRESS
         LH    R2,SKTTLTOT        LOAD TITLE LENGTH
         J     BLDTK07
*
BLDTK06  L     R3,CURSRTKY        LOAD SORT KEY  DEFINITION ADDRESS
         LH    R15,LBKEYLEN       LOAD   KEY  LENGTH (-1)
*
         TM    LBFLAGS,LBEFFEND   END  DATES   PRESENT ???
         JNO   BLDTKA6
         AHI   R15,4
*
BLDTKA6  EQU   *
         L     R6,LBLSTFND        LOAD LOOK-UP RECORD ADDRESS (FOUND)
         AR    R6,R15             ADVANCE PAST KEY    (-1)
         AH    R6,SKOUTPOS        ADVANCE TO START OF TITLE
         DROP  R6
*
         L     R3,CURSRTKY        LOAD SORT KEY  DEFINITION ADDRESS
         LH    R2,SKTTLTOT        LOAD TITLE LENGTH
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=100
*
***********************************************************************
*  COPY/CONVERT SORT KEY DESCRIPTION                                  *
***********************************************************************
BLDTK07  LA    R14,SKTITLE(R2)
         CH    R2,SKINDCNT        CURRENT LENGTH  = INDENTATION  ???
         JE    BLDTK08            YES - INTERVENING SPACE NOT NEEDED
*
         CLI   SKDSPOPT+1,INCLDESC  VALUE OMITTED ???
         JE    BLDTK08              YES - SPACE   UNNECESSARY
*
         MVI   0(R14),C' '
         AHI   R14,1
         AHI   R2,1
*
BLDTK08  CLI   SKOUTFMT+1,FM_ALNUM  ALPHANUMERIC  TITLE  ???
         JE    BLDTK09              YES - BYPASS  FORMATTING
         CLI   SKOUTFMT+1,FM_ALPHA  ALPHABETIC    TITLE  ???
         JNE   BLDTK10              YES - BYPASS  FORMATTING
*
BLDTK09  CLC   SKOUTMSK,SPACES    MASK  SPECIFIED ???
         JNE   BLDTK10            YES - USE IT
*
         LH    R15,SKDESCLN       LOAD OUTPUT  LENGTH IF AVAILABLE
         BCTR  R15,0
         EX    R15,MVCVALUE       COPY SOURCE  TO  TITLE AREA
         LA    R2,1(R2,R15)       UPDATE TITLE LENGTH
         J     BLDTK12            EXIT
                     EJECT
BLDTK10  ST    R14,DL96TGTA       PASS TARGET ADDRESS(WITHIN SORTTITLE)
         LH    R15,SKTTLLEN       LOAD SOURCE LENGTH
         STH   R15,SAVALLEN       PASS SOURCE LENGTH
         MVC   SAVALFMT,SKTTLFMT  PASS SOURCE FORMAT
         MVC   SAVALDEC+1(1),SKTTLDEC  PASS SOURCE DECIMALS
         MVC   SAVALRND+1(1),SKTTLRND  PASS SCALING FACTOR
         MVC   SAOUTFMT,SKOUTFMT  PASS OUTPUT FORMAT
         MVC   SAOUTDEC+1(1),SKOUTDEC  PASS OUTPUT DECIMALS
         MVHHI SAOUTRND,0         ZERO ROUNDING FACTOR
         MVI   SAOUTJUS,C'L'      JUSTIFICATION
         MVC   SAMSKLEN,SKOUTMLN  MASK LENGTH
         LA    R0,SKOUTMSK        PASS OUTPUT   MASK
         ST    R0,SAMSKADR
*
         llgtr R6,r6              make sure ok for 64 bit
         STg   R6,SAVALADR
*
         LH    R15,SKDESCLN       LOAD   OUTPUT LENGTH
         STH   R15,DL96LEN        PASS   OUTPUT LENGTH
         AR    R2,R15             UPDATE TITLE  LENGTH
*
         LA    R1,DL96LIST        LOAD PARAMETER LIST ADDRESS
         llgf  R15,GVBDL96A       LOAD PROGRAM        ADDRESS
         bassm R14,R15            CALL "GVBDL96"
*
         LTR   R15,R15            SUCCESSFUL?
         JZ    BLDTK12            YES - CONTINUE
*
BLDTK11  L     R6,NOTTLADR        SUBSTITUTE "NO TITLE FOUND" FOR ERROR
         L     R14,DL96TGTA
         LH    R15,SKDESCLN
         BCTR  R15,0
         EX    R15,MVCVALUE
                     SPACE 3
BLDTK12  STH   R2,SKTTLTOT        SAVE CURRENT TOTAL
*
         L     R15,VWRTADDR       REPORT  HEADER  RECORDS  PRESENT ???
         LTR   R15,R15
         JNZ   BLDTK20            YES -   BYPASS  V3   DEFAULTS
*
         CLI   SKHDRBRK+1,TITLEL1 PROMOTE SORT TITLE   TO REPORT TITLE
         JL    BLDTK30            NO  -   CONTINUE
         CLI   SKHDRBRK+1,TITLEL5
         JH    BLDTK30
*
         LH    R15,SKDESCLN       LOAD LENGTH OF FORMATTED DESCRIPTION
         LTR   R15,R15            DESCRIPTION AVAILABLE ??
         JNP   BLDTK30            NO - BYPASS PROMOTION
*
         LA    R1,SKTITLE(R2)     COMPUTE END OF  VALUE DESCRIPTION
         BRAS  R14,BLDTTLEN       DETERMINE  ACTUAL TITLE LENGTH (R15)
         LTR   R15,R15            ALL BLANK  ???
         JP    BLDTK12A           NO  -  BYPASS RESET
         LH    R15,SKDESCLN       YES -  USE ORIGINAL LENGTH
*
BLDTK12A EQU   *
         LA    R14,SKTITLE(R2)    COMPUTE END OF  VALUE DESCRIPTION
         SH    R14,SKDESCLN       BACKUP  TO  START  OF DESCRIPTION
*
         CLI   SKHDRBRK+1,TITLEL1 FIRST   BREAK   LEVEL ???
         JNE   BLDTK13
*
         L     R9,RTITLEA
         USING RTITLE,R9
*
         LHI   R14,L'RTCENTER     BLANK   OUT  PREVIOUS DESCRIPTION
         AH    R14,RTRECLEN
         SRL   R14,2
         LA    R1,0(R9,R14)
         LHI   R14,L'RTCENTER-1
         EX    R14,MVCSPACE
*
         LH    R14,RTRECLEN       COMPUTE EXCESS ROOM AND DIVIDE BY TWO
         AR    R14,R15
         SRL   R14,1
         LA    R1,0(R9,R14)       LOAD  CENTERED TEXT ADDRESS
*
         LA    R14,SKTITLE(R2)    COMPUTE END OF  VALUE DESCRIPTION
         SH    R14,SKDESCLN       BACKUP  TO  START  OF DESCRIPTION
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCR1R14
         J     BLDTK30
*
BLDTK13  CLI   SKHDRBRK+1,TITLEL2 2ND     BREAK    LEVEL ???
         JNE   BLDTK14            NO  -   BYPASS   PROMOTION
*
         L     R9,RTITLEA
         AHI   R9,PRNTMAXL+4
         USING RTITLE,R9
*
         LHI   R14,L'RTCENTER     BLANK   OUT  PREVIOUS DESCRIPTION
         AH    R14,RTRECLEN
         SRL   R14,2
         LA    R1,0(R9,R14)
         LHI   R14,L'RTCENTER-1
         EX    R14,MVCSPACE
*
         LH    R14,RTRECLEN       COMPUTE EXCESS ROOM AND DIVIDE BY TWO
         AR    R14,R15
         SRL   R14,1
         LA    R1,0(R9,R14)       LOAD CENTERED TEXT ADDRESS
*
         LA    R14,SKTITLE(R2)    COMPUTE END OF  VALUE DESCRIPTION
         SH    R14,SKDESCLN       BACKUP  TO  START  OF DESCRIPTION
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCR1R14
         J     BLDTK30
*
BLDTK14  CLI   SKHDRBRK+1,TITLEL3 3RD     BREAK    LEVEL ???
         JNE   BLDTK30            NO  -   BYPASS   PROMOTION
*
         L     R9,RTITLEA
         AHI   R9,PRNTMAXL+4
         AHI   R9,PRNTMAXL+4
         USING RTITLE,R9
*
         LHI   R14,L'RTCENTER     BLANK   OUT  PREVIOUS DESCRIPTION
         AH    R14,RTRECLEN
         SRL   R14,2
         LA    R1,0(R9,R14)
         LHI   R14,L'RTCENTER-1
         EX    R14,MVCSPACE
*
         LH    R14,RTRECLEN       COMPUTE EXCESS ROOM AND DIVIDE BY TWO
         AR    R14,R15
         SRL   R14,1
         LA    R1,0(R9,R14)       LOAD CENTERED TEXT ADDRESS
*
         LA    R14,SKTITLE(R2)    COMPUTE END OF  VALUE DESCRIPTION
         SH    R14,SKDESCLN       BACKUP  TO  START  OF DESCRIPTION
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCR1R14
         J     BLDTK30
*
BLDTK16  LA    R14,SKTITLE(R2)    COMPUTE END OF  VALUE DESCRIPTION
         LH    R15,SKTTLTOT       COMPUTE DATA   LENGTH
         SH    R15,SKPREFXL
         SR    R14,R15            BACKUP  TO  START  OF DESCRIPTION
*
         CLI   SKHDRBRK+1,TITLEL1 FIRST   BREAK  LEVEL ???
         JNE   BLDTK17
*
         L     R9,RTITLEA
         USING RTITLE,R9
*
         MVC   RTCENTER,SPACES    CLEAR   TITLE
         LHI   R1,L'RTCENTER      LOAD  MAXIMUM  LENGTH
         SR    R1,R15             COMPUTE EXCESS ROOM AND DIVIDE BY TWO
         JNM   BLDTK16A
         SR    R1,R1
         LHI   R15,L'RTCENTER
BLDTK16A EQU   *
         SRL   R1,1
         LA    R1,RTCENTER(R1)    LOAD CENTERED TEXT ADDRESS
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCR1R14
         J     BLDTK30
*
BLDTK17  CLI   SKHDRBRK+1,TITLEL2 2ND     BREAK    LEVEL ???
         JNE   BLDTK18            NO  -   BYPASS   PROMOTION
*
         L     R9,RTITLEA
         AHI   R9,PRNTMAXL+4
         USING RTITLE,R9
*
         MVC   RTCENTER,SPACES    CLEAR   DESC
         LHI   R1,L'RTCENTER      LOAD  MAXIMUM  LENGTH
         SR    R1,R15             COMPUTE EXCESS ROOM AND DIVIDE BY TWO
         JNM   BLDTK17A
         SR    R1,R1
         LHI   R15,L'RTCENTER
BLDTK17A EQU   *
         SRL   R1,1
         LA    R1,RTCENTER(R1)    LOAD CENTERED TEXT ADDRESS
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCR1R14
         J     BLDTK30
*
BLDTK18  CLI   SKHDRBRK+1,TITLEL3 3RD     BREAK    LEVEL ???
         JNE   BLDTK30            NO  -   BYPASS   PROMOTION
*
         L     R9,RTITLEA
         AHI   R9,PRNTMAXL+4
         AHI   R9,PRNTMAXL+4
         USING RTITLE,R9
*
         MVC   RTCENTER,SPACES    CLEAR   DESC
         LHI   R1,L'RTCENTER      LOAD  MAXIMUM  LENGTH
         SR    R1,R15             COMPUTE EXCESS ROOM AND DIVIDE BY TWO
         JNM   BLDTK18A
         SR    R1,R1
         LHI   R15,L'RTCENTER
BLDTK18A EQU   *
         SRL   R1,1
         LA    R1,RTCENTER(R1)    LOAD CENTERED TEXT ADDRESS
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCR1R14
         J     BLDTK30
*
BLDTK20  L     R6,SK1300VL        LOAD SORT VALUE VDP1300 RECORD ADDR
         USING VDP1300_TITLE_LINES_RECORD,R6
         LTR   R6,R6
         JNP   BLDTK26
*
         LH    R14,VDP1300_ROW_NBR
         BCTR  R14,0
         LHI   R15,PRNTMAXL+4
         MR    R14,R14
         L     R9,RTITLEA
         AR    R9,R15
         USING RTITLE,R9
*
BLDTKVAL LH    R0,SKCOLSIZ
*
         LH    R15,SKLBLLEN
         LTR   R15,R15
         JNP   BLDTKVA1
         AHI   R15,1
BLDTKVA1 EQU   *
         AH    R15,SKINDCNT
         AH    R15,SKSUBLEN
         CLC   SKPREFXL,SKSUBLEN
         JE    BLDTKVA2
         AHI   R15,1
BLDTKVA2 EQU   *
         LA    R14,SKTITLE(R15)
*
         LA    R15,RTLEFT
         A     R15,VDP1300_TITLE_AREA_OFFSET
*
         CLI   VDP1300_JUSTIFICATION+3,TTLRIGHT
         JNE   BLDTKV01
         LR    R15,R9
         AH    R15,RTRECLEN
         S     R15,VDP1300_TITLE_AREA_OFFSET
         J     BLDTKV02
*
BLDTKV01 CLI   VDP1300_JUSTIFICATION+3,TTLCENTR
         JNE   BLDTKV02
         LH    R15,RTRECLEN
         AHI   R15,-5
         SH    R15,VDP1300_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,VDP1300_TITLE_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDTKV02 LTR   R1,R0
         JNP   BLDTK30
*
         BCTR  R1,0
         EX    R1,BLDRCPY         COPY TEXT
*
BLDTK26  L     R6,SK1300DS        LOAD SORT DESC  VDP1300 RECORD ADDR
         USING VDP1300_TITLE_LINES_RECORD,R6
         LTR   R6,R6
         JNP   BLDTK30
*
         LH    R14,VDP1300_ROW_NBR
         BCTR  R14,0
         BCTR  R14,0
         LHI   R15,PRNTMAXL+4
         MR    R14,R14
         L     R9,RTITLEA
         AR    R9,R15
         USING RTITLE,R9
*
BLDTKDES LH    R0,SKDESCLN        LEN OF  VALUE DESCRIPTION
         LTR   R0,R0
         JNP   BLDTK30
*
         LH    R15,SKTTLTOT       END OF  VALUE DESCRIPTION
         LA    R14,SKTITLE(R15)
         SR    R14,R0             BACKUP  TO  START  OF DESCRIPTION
*
         LA    R15,RTLEFT
         A     R15,VDP1300_TITLE_AREA_OFFSET
*
         CLI   VDP1300_JUSTIFICATION+3,TTLRIGHT
         JNE   BLDTKD01
         LR    R15,R9
         AH    R15,RTRECLEN
         S     R15,VDP1300_TITLE_AREA_OFFSET
         J     BLDTKD02
*
BLDTKD01 CLI   VDP1300_JUSTIFICATION+3,TTLCENTR
         JNE   BLDTKD02
         LH    R15,RTRECLEN
         AHI   R15,-5
         SH    R15,VDP1300_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,VDP1300_TITLE_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDTKD02 LTR   R1,R0
         JNP   BLDTK30
*
         BCTR  R1,0
         EX    R1,BLDRCPY         COPY TEXT
*
BLDTK30  CLI   SKDSPOPT+1,CATEGOR CATEGORIZED  SORT  KEY ???
         JNE   BLDTEXIT
*
         CH    R2,VWMAXTTL        LARGEST SORT TITLE KEY LENGTH ???
         JNH   BLDTEXIT
         STH   R2,VWMAXTTL
*
BLDTEXIT LM    R2,R10,SAVETKEY    RETURN
         BSM   0,R10
                     SPACE 5
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        CALCULATE ACTUAL TITLE LINE TEXT LENGTH     (SET R15)        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
BLDTTLEN BCTR  R1,0               BACKUP   TO PREVIOUS CHARACTER
         CLI   0(R1),C' '         TRAILING BLANK ???
         BNER  R14                NO - RETURN (NON-BLANK FOUND)
         BRCT  R15,BLDTTLEN
*
         BR    R14                RETURN (R15=0)
*
         DROP  R3
         DROP  R6
         DROP  R7
         DROP  R8
         DROP  R9
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   W R I T E   R O U T I N E   (OSI/OSR, CSB/CRR)     *
*                                                                     *
*            R0 => PRINT LINE  RDW                                    *
*            R1 => PRINT DCB   ADDRESS                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING VIEWREC,R8
*
VSAMWRT  STM   R2,R10,SAVEMR66    SAVE    CALLER'S REGISTERS
*
         LR    R9,R15             LOAD    SUBROUTINE  BASE  REGISTER
         USING VSAMWRT,R9
*
         LR    R10,R14            SAVE    RETURN   ADDRESS
*
         ST    R0,MR66SRCA        SAVE DATA ADDRESS
*
         CLI   PRINTIND,C'Y'      USE PRINT AREA   ???
         JNE   VSAMEXIT           NO  -   RETURN
*
***********************************************************************
*  CALL FORMAT EXIT (IF ANY)                                          *
***********************************************************************
VSAMLOOP XC    VWFMTRC,VWFMTRC    INITIALIZE    FORMAT  EXIT  RTNCODE
*
         L     R15,VWPGMADR       LOAD    EXIT  PROGRAM CALL     ADDR
         LTR   R15,R15
         JZ    VSAMQSAM
*
         L     R2,LEWORKA         LOAD      WORK AREA   ADDRESS
         USING LEINTER,R2
*
         MVC   VWPROCDT,SVPROCDT COPY PROC DATE FROM EXTR CONTROL
         MVC   VWPROCTM,SVPROCTM
*
         LA    R0,VWVIEW#         VIEW      NUMBER
         ST    R0,FMTPVIEW
         L     R0,MR66SRCA        INPUT     RECORD   ADDRESS
         ST    R0,FMTPRECA
         LA    R0,VWFMTPRM        PARAMETER AREA     ADDRESS
         ST    R0,FMTPARMA
         LA    R0,VWCURSEC        REPORT    DATA     ADDRESS
         ST    R0,FMTPSECT
         LA    R0,VWRUN#          RUN       DATA     ADDRESS
         ST    R0,FMTPDATA
         LA    R0,VWFMTPTR        OUTPUT    RECORD   POINTER
         ST    R0,FMTPOUTP
         LA    R0,VWFMTWRK        WORKAREA  ANCHOR   POINTER    ADDR
         ST    R0,FMTPWRKA
         OI    FMTPWRKA,X'80'
*
         LA    R1,FMTPARML        LOAD    PARAMETER     LIST  ADDRESS
         BASR  R14,R15
         ST    R15,VWFMTRC
*
         CHI   R15,8              SKIP    THIS  LINE    ???
         JE    VSAMOK             YES -   EXIT
         CHI   R15,16             ABORT   ???
         JE    VSAMABND           YES -   ABEND  JOB
*
         L     R7,MR66SRCA        SAVE    ORIGINAL   RECORD   POINTER
         L     R0,VWFMTPTR        OUTPUT  RECORD     POINTER
         ST    R0,MR66SRCA
         J     VSAMQSAM
*
***********************************************************************
*  PERMANENT ERROR INDICATED BY FORMAT EXIT PROGRAM                   *
***********************************************************************
VSAMABND LHI   R14,MSG#406        TERMINATE EXECUTION
         MVC   ERRDATA(8),VWFMTPGM INDICATE WHICH   EXIT
         J     RTNERROR
*
***********************************************************************
*  ECHO PRINT LINE TO "MR88PRNT" IF REQUESTED                         *
***********************************************************************
VSAMQSAM LTR   R15,R0             NULL   LINE  ???
         JNP   VSAMEXIT
*
         LH    R6,0(,R15)         LOAD   CURRENT LENGTH
*
         L     R14,PARMDADR       LOAD   PARAMETER DATA ADDRESS
         USING PARMDATA,R14
         CLI   ECHOIND,C'Y'       PRINT  QSAM   VERSION OF OUTPUT ???
         JNE   VSAMSKIP           NO  -  BYPASS WRITE
         DROP  R14
*
         L     R14,MR66SRCA       LOAD  RECORD ADDRESS
         STH   R6,0(,R14)         RESTORE   ORIGINAL  LENGTH
*
VSAMSKIP ds    0h
         AP    VWOUTCNT,P001
*
         CP    VWOUTCNT,VWLIMIT   OUTPUT   LIMIT REACHED ???
         JNH   VSAMSKI1
*       L     R14,LOGDCBA
*       GVBMSG LOG,MSGNO=MSG#414,SUBNO=1,LOGDCBA=(R14),               +
*             SUB1=(MR88NAME,L'MR88NAME),                             +
*             MSGBUFFER=(UTMTBUFF,L'UTMTBUFF),                        +
*             MF=(E,MSG_AREA)
         LHI   R14,MSG#414
         J     RTNERROR
*
VSAMSKI1 EQU   *
         LH    R15,VWLINENO       INCREMENT PAGE LINE  COUNT
         AHI   R15,1
         STH   R15,VWLINENO
*
***********************************************************************
*  WRITE PRINT LINE TO SPECIFIED DESTINATION "DDNAME"                 *
***********************************************************************
VSAMDEST L     R1,VWDCBADR        LOAD  DCB    ADDRESS
         L     R15,DATAPUTA       LOAD  SUBROUTINE     ADDRESS
         BASR  R14,R15            GET   MAX    LENGTH  BUFFER
*
         LR    R0,R1              LOAD  BUFFER ADDRESS
         L     R14,MR66SRCA       LOAD  RECORD ADDRESS
*
         L     R1,VWDCBADR               LOAD  DCB     ADDR
         LH    R15,DCBLRECL-IHADCB(,R1)  LOAD  MAXIMUM RECORD LENGTH
         TM    DCBRECFM-IHADCB(R1),X'80' FIXED RECORD  LENGTH ???
         JNO   VSAMVAR                   NO  - BRANCH
         AHI   R14,4                     YES - EXCLUDE RDW
         AHI   R6,-4
*
         LR    R2,R15                    COMPUTE  PAD  LENGTH
         SR    R2,R6
         JNP   VSAMMVCL                  BRANCH  IF NO PADDING
         LR    R15,R6                    COPY  ACTUAL  DATA   ONLY
         J     VSAMMVCL
*
VSAMVAR  SR    R2,R2                     NO  PADDING REQUIRED
         CR    R6,R15                    DATA LENGTH EXCEEDS  MAXIMUM
         JNL   VSAMVAR1                  YES - TRUNCATE
         LR    R15,R6                    NO  - COPY  ACTUAL DATA ONLY
VSAMVAR1 EQU   *
         STH   R15,0(,R14)               UPDATE RDW (TRUNCATE)
*
VSAMMVCL LR    R1,R15
         MVCL  R0,R14
*
         LTR   R1,R2                     PAD  WITH SPACES  IF SHORT
         JNP   VSAMOK
         SLL   R14,32
         LA    R15,X'40'
         SLL   R15,24
         MVCL  R0,R14
                     SPACE 3
VSAMOK   L     R14,MR66SRCA       LOAD PRINT    LINE ADDRESS
*
         CLC   VWFMTRC,F4
         JNE   VSAMEXIT
         ST    R7,MR66SRCA        RESTORE ORIGINAL RECORD  ADDRESS
         J     VSAMLOOP
                     EJECT
*
VSAMEXIT LR    R14,R10
         LM    R2,R10,SAVEMR66    RESTORE CALLER'S REGISTERS
         BR    R14
*
         DROP  R2
         DROP  R8
         DROP  R9
                     EJECT
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        T A B L E   L O O K U P                                      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING LKUPBUFR,R6
*
SRCHTBL  ds    0h
*
         LH    R5,LBKEYLEN        LOAD  KEY  LENGTH (-1)
*
         L     R4,LBMIDDLE        LOAD ADDRESS  OF MIDDLE ENTRY
         USING LKUPTBL,R4
                     SPACE 3
SRCHLOOP LR    R3,R4              SAVE    LAST  ENTRY EXAMINED
         EXrl  R5,SRCHCOMP        COMPARE KEYS  ???
         JL    SRCHTOP
         JH    SRCHBOT
*
SRCHFND  LA    R14,LKUPDATA       LOAD ADDRESS  OF DATA
         ST    R14,LBLSTFND       SAVE ADDRESS  IN BUFFER  PREFIX
*
         MVC   LKUPKEY,SPACES     BLANK OUT KEY
*
         AP    lkupfnd,P001       Add to lookup found total
*
         B     4(,R10)            RETURN TO FOUND ADDRESS
                     SPACE 3
SRCHTOP  L     R4,LKLOWENT        LOAD ADDRESS   OF LOWER  VALUE NODE
         LTR   R4,R4              FURTHER SEARCHING POSSIBLE ???
         JP    SRCHLOOP           YES - CONTINUE
         J     SRCHCHK            NO  - CHECK FOR EFFECTIVE DATE
*
SRCHBOT  L     R4,LKHIENT         LOAD ADDRESS   OF HIGHER VALUE NODE
         LTR   R4,R4              FURTHER SEARCHING POSSIBLE ???
         JP    SRCHLOOP           YES - CONTINUE
                     EJECT
SRCHCHK  TM    LBFLAGS,LBEFFDAT   EFFECTIVE DATES  PRESENT ???
         JNO   SRCHNOT            NO  - EXACT MATCH WAS REQUIRED
*
         LA    R14,LKUPKEY        LOCATE EFFECTIVE DATE WITHIN KEY
         AH    R14,LBEFFOFF
         OC    0(4,R14),0(R14)    EFFECTIVE DATE PRESENT ???
         JE    SRCHNOT            NO  - EXACT MATCH WAS  REQUIRED
*
         LR    R1,R5              CALC KEY LENGTH EXCLUDING DATE
         AHI   R1,-4
*
         LR    R4,R3              RECHECK LAST ENTRY EXAMINED
         EXrl  R5,SRCHCOMP
         JH    SRCHHIGH
                     SPACE 3
SRCHLOW  LHI   R0,LKPREFLN        LOAD LENGTH OF EACH TABLE ENTRY
         AH    R0,LBRECLEN
*
         L     R14,LBTBLBEG       LOAD BEGINNING ADDRESS OF TABLE
*
         LR    R4,R3              BACK DOWN ONE ENTRY IF POSSIBLE
         SR    R4,R0
         CR    R4,R14             CHECK FOR BEGINNING OF TABLE  ???
         JL    SRCHNOT            EXIT  IF  NO PREVIOUS  ENTRY
*
         EXrl  R1,SRCHCOMP        SAME  ROOT KEY EXCLUDING DATE ???
         JE    SRCHFND            YES - CORRECT  ENTRY FOUND
         J     SRCHNOT            NO  - ENTRY WITH OKAY DATE NOT FOUND
                     SPACE 3
SRCHHIGH EXrl  R1,SRCHCOMP        SAME  ROOT KEY EXCLUDING DATE ???
         JE    SRCHFND            YES - CORRECT  ENTRY FOUND
*
SRCHNOT  MVC   LKUPKEY,SPACES     BLANK OUT KEY
*
         SR    R14,R14            SET TABLE ENTRY ADDRESS  TO HIGH VAL
         BCTR  R14,0
         ST    R14,LBLSTFND       SAVE ADDRESS
*
         AP    lkupnot,P001       Add to lookup not found total
*
         BSM   0,R10              RETURN TO NOT FOUND ADDRESS
*
SRCHCOMP CLC   LKUPKEY(0),LKUPDATA * * * * *  E X E C U T E D   * * * *
         DROP  R4
         DROP  R6
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   P R I N T   N O   E X T R A C T   R E C O R D S   M E S S A G E   *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
noexsave using saver,savenoex
NOEXTREC STM   R14,R12,noexsave.savgrs14  SAVE CALLER'S REGISTERS

         L     R7,RECADDR         RESTORE HEADER RECORD ADDRESS
         USING HDRREC,R7
*
         LHI   R14,MSG#411        ASSUME VIEW#  NOT  ON LIST
         L     R8,VWCHAIN         LOAD   FIRST  VIEW ADDRESS
*
NOEXSRCH LTR   R8,R8              END-OF-LIST  ???
         JNP   RTNERROR           YES -  VIEW  NOT FOUND
         USING VIEWREC,R8
         CLC   HDVIEW#,VWVIEW#    MATCHING REQUEST NUMBERS ???
         JE    NOEXPRNT           YES - PRINT  "NO EXTRACT RECORDS" MSG
         L     R8,VWNEXT          LOAD  ADDRESS OF NEXT LIST ENTRY
         J     NOEXSRCH           LOOP  THROUGH ENTIRE  LIST
*
NOEXPRNT MVC   SVVIEW#,VWVIEW#    SAVE  VIEW   NUMBER
         MVI   VWPROC,C'P'        This view is processed in this run 
*
         L     R0,SVSORTKY        CLEAR OUT    KEY
         LHI   R1,SVSORTLN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         CLI   VWDESTYP+1,FILEFMT FILE  FORMAT    ???
         JE    NOEXEXIT           YES - nothing to write
         CLI   VWDESTYP+1,CSV     COMMA SEPARATED VARIABLES ???
         JE    NOEXEXIT           Yes - nothing to write
*
         OI    VWFLAG2,VWPRINT    TURN  ON PRINT IND
*
         CLI   VWDESTYP+1,BATCH   BATCH/HARDCOPY REPORT ???
         JE    NOEXDCBA           YES - CHECK IF OUTPUT DCB AVAILABLE
*
         NI    VWFLAG2,255-VWOUTDCB     TURN OFF OUTPUT FILE
         J     NOEXINIT           BYPASS OPENING OUTPUT FILE
*
NOEXDCBA L     R2,VWDCBADR            LOAD  DCB ADDRESS
         USING IHADCB,R2
         LTR   R2,R2                  DCB   AVAILABLE  ???
         JP    NOEXOPEN
         J     NOEXINIT
*
NOEXOPEN TM    48(R2),X'10'           ALREADY   OPEN   ???
         JO    NOEXINIT               YES - BYPASS    OPEN
*
         tm    vwflag2,vwnodd     Did the RDJFCB fail earlier?
         jz    NOEXopn2           no
         oi    statflg1,vwnodd    Required DDname not in JCL
*
         MVC   ERRDATA(8),VWDDNAME    INDICATE WHICH FILE
         LHI   R14,MSG#405        yes, No DD defined
         J     RTNERROR
*
NOEXopn2 equ   *
         LAY   R14,STATOPEN
         MVC   RENTPARM(STATOPENL),0(R14)
         OPEN  ((R2),OUTPUT),MODE=31,MF=(E,RENTPARM)  OPEN OUTPUT
         TM    48(R2),X'10'           OPEN  SUCCESSFUL ???
         JO    NOEXPUTA               YES - ADVANCE TO CALCULATION LOOP
         LHI   R14,MSG#403            ERROR MESSAGE NUMBER
         MVC   ERRDATA(8),VWDDNAME    INDICATE WHICH FILE
         J     RTNERROR
NOEXPUTA MVC   DATAPUTA+1(3),DCBPUTA
         OC    DATAPUTA,MODE31
*
NOEXINIT XC    VWLINENO,VWLINENO  RESET  LINE   NUMBER
         XC    NEEDTITL,NEEDTITL  RESET  TITLE  LINES   COUNTER
         ZAP   VWPAGENO,P000      RESET  PAGE   NUMBER
         MVI   GTOTLIND,C' '      RESET  PRINT  GRAND   TOTALS  IND
         MVI   PRINTIND,C'Y'      RESET  USE    PRINT   AREA    IND
         MVC   PRNTSUBR,VSAMWRTA  DIRECT MESSAGE TO HARDCOPY WRITER
*
         XC    VWMAXTTL,VWMAXTTL  ASSUME NO SORT TITLES
         XC    VWCENTER,VWCENTER  ASSUME NO CENTERING ADJUSTMENT
*
         BRAS  R10,BLDRPTTL       BUILD REPORT  TITLE
         BRAS  R10,BLDFOOTR       BUILD REPORT  FOOTER
*
         BRAS  R9,RPTPRNT         PRINT REPORT  LINES
*
*
NOEXMSG  MVI   PRNTLINE,C' '      BLANK  OUT   PRINT   LINE
         MVC   PRNTLINE+01(L'PRNTLINE-1),PRNTLINE+00
*
         MVC   PRNTTEXT(L'NOEXTTXT),NOEXTTXT   COPY MESSAGE   TEXT
*
         MVI   PRNTCC,C'0'        DOUBLE SPACE
*
         LHI   R0,L'PRNTLINE+4    PRINT  ERROR MESSAGE
         STH   R0,PRNTRDW
         L     R1,PRNTDCBA
         STH   R0,DCBLRECL-IHADCB(,R1)
         LA    R0,PRNTRDW
         L     R15,PRNTSUBR
         BASR  R14,R15
NOEXEXIT XC    SVVIEW#,SVVIEW#           RESET CURRENT REQUEST  NO.
         XC    VWLINENO,VWLINENO         RESET CURRENT LINE COUNTER
         XC    NEEDTITL,NEEDTITL         RESET TITLE   LINE COUNTER
         ZAP   VWPAGENO,P000             RESET CURRENT PAGE NUMBER
*
         LM    R14,R12,noexsave.savgrs14 RETURN
         BR    R10
         drop  noexsave
*
static   loctr
NOEXTTXT DC    CL70'* * * * NO RECORDS EXTRACTED FOR THIS VIEW * * * *'
code     loctr
*
         DROP  R7
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        F L U S H   E X T R A C T   R E C O R D S (SKIP VIEW)        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTREC,R7
         USING VIEWREC,R8
*
FLUSH    BRAS  R10,BLDRPTTL       BUILD REPORT TITLE
         BRAS  R10,BLDFOOTR       BUILD REPORT FOOTER
*
         MVI   PRINTIND,C'Y'      DIRECT MESSAGE TO  HARDCOPY  WRITER
         MVC   PRNTSUBR,VSAMWRTA
*
         OI    VWFLAG2,VWPRINT    TURN  ON PRINT IND
*
         CLI   VWDESTYP+1,BATCH   BATCH/HARDCOPY REPORT ???
         JE    FLUSHDCB           YES - CHECK IF OUTPUT DCB AVAILABLE
*
         NI    VWFLAG2,255-VWOUTDCB  TURN OFF OUTPUT   FILE
         J     FLUSHTTL              BYPASS OPENING  OUTPUT FILE
*
FLUSHDCB L     R2,VWDCBADR            LOAD  DCB ADDRESS
         USING IHADCB,R2
         LTR   R2,R2                  DCB   AVAILABLE  ???
         JP    FLUSHOPN
*
         NI    VWFLAG2,255-VWOUTDCB   TURN  OFF OUTPUT FILE
         J     FLUSHTTL
*
FLUSHOPN TM    48(R2),X'10'           ALREADY   OPEN   ???
         JO    FLUSHTTL               YES - BYPASS    OPEN
*
         tm    vwflag2,vwnodd         Did the RDJFCB fail earlier?
         jz    FLUSopn2               no
         oi    statflg1,vwnodd    Required DDname not in JCL
*
         MVC   ERRDATA(8),VWDDNAME    INDICATE WHICH FILE
         LHI   R14,MSG#405            yes, No DD defined
         J     RTNERROR
*
FLUSopn2 equ   *
         LAY   R1,STATOPEN
         MVC   RENTPARM(STATOPENL),0(R1)
         OPEN  ((R2),OUTPUT),MODE=31,MF=(E,RENTPARM)  OPEN OUTPUT
         TM    48(R2),X'10'           OPEN  SUCCESSFUL ???
         JO    FLUSHPUT               YES - ADVANCE TO CALCULATION LOOP
*
         LHI   R14,MSG#403            ERROR MESSAGE NUMBER
         MVC   ERRDATA(8),VWDDNAME    INDICATE WHICH FILE
         J     RTNERROR
*
FLUSHPUT MVC   DATAPUTA+1(3),DCBPUTA
         OC    DATAPUTA,MODE31
*
FLUSHTTL BRAS  R9,RPTPRNT         PRINT REPORT TITLE
*
FLUSHBAD MVI   EOREOFCD,C'B'      INDICATE BAD REQUEST BEING FLUSHED
*
         MVI   PRNTLINE,C' '      BLANK  OUT   PRINT   LINE
         MVC   PRNTLINE+01(L'PRNTLINE-1),PRNTLINE+00
*
         L     R1,CANCLADR                COPY MESSAGE  TEXT
         MVC   PRNTTEXT(L'CANCLTXT),0(R1)
*
         MVI   PRNTCC,C'0'        DOUBLE SPACE
*
         LHI   R0,L'PRNTLINE+4    PRINT  ERROR MESSAGE
         STH   R0,PRNTRDW
         L     R1,PRNTDCBA
         STH   R0,DCBLRECL-IHADCB(,R1)
         LA    R0,PRNTRDW
         L     R15,PRNTSUBR
         BASR  R14,R15

FLUSHLP  DS    0H
*
FLUSHNXT llgf  R15,READSUBR       LOAD GET  SUBROUTINE  ADDRESS(24-BIT)
         BASSM R10,R15            READ THE  NEXT    EXTRACT FILE RECORD
*                                (R7 - STILL POINTS TO PREVIOUS RECORD)
         L     R0,EXVIEW#-EXTREC(,R1)      STRIP HEADER  INDICATOR
         SRL   R0,1
         ST    R0,EXVIEW#-EXTREC(,R1)
*
         CLC   SVVIEW#,EXVIEW#-EXTREC(R1)   STILL SAME REQUEST ???
         JE    FLUSHLP                      YES - CONTINUE READ/SKIP
*
         XC    SVVIEW#,SVVIEW#    RESET CURRENT REQUEST  NO.
         XC    VWLINENO,VWLINENO  RESET CURRENT LINE COUNTER
         XC    NEEDTITL,NEEDTITL  RESET TITLE   LINE COUNTER
         ZAP   VWPAGENO,P000      RESET CURRENT PAGE NUMBER
         MVI   EOREOFCD,C' '      RESET END-OF-REQUEST FLAG  ???
*
         J     NEXTEXT         PROCESS  NEXT REQUEST'S HEADER RECORD
*
         DROP  R7
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        M E R G E   E X T R A C T   &   M A S T E R   F I L E S      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTREC,R7
*
MERGEREC LR    R9,R15             SET  TEMPORARY PROGRAM BASE REGISTER
         USING MERGEREC,R9
*
         CLI   LASTFILE,C'E'      REPLENISH BUFFER FOR LAST FILE READ
         JE    MERGEXTR
*
                     EJECT
MERGEXTR L     R1,EXTRRECA        LOAD  PREVIOUS  RECORD   ADDRESS
         AH    R1,0(,R1)          ADVANCE TO NEXT RECORD
         C     R1,EXTREOD         LAST  RECORD IN BUFFER   ???
         JL    EXTRFMT            NO  - SAVE NEXT RECORD'S ADDRESS
*
         L     R2,EXTRDCBA        LOAD  CURRENT  DCB  ADDRESS
         USING IHADCB,R2
         L     R3,EXTRDECB        LOAD  CURRENT  DECB PREFIX ADDRESS
         L     R1,24(,R3)         PRECEDING   BUFFER  AVAILABLE  ???
         LTR   R1,R1
         JNP   EXTRNXTB           NO  - DON'T START   ANOTHER I/O
*
         XC    24(4,R3),24(R3)    CLEAR AVAILABLE  INDICATION
         MVC   10(2,R1),DCBBLKSI  RESET BLOCK SIZE IN DECB
         AHI   R1,4               LOAD  DECB  ADDRESS
         XC    0(4,R1),0(R1)      ZERO  THE   ECB
         L     R15,EXTRGETA       CALL  (31-BIT MODE)
         BASR  R14,R15
*
EXTRNXTB LR    R14,R3             SAVE     CURRENT DECB  PREFIX ADDR
         L     R3,0(,R3)          ADVANCE  TO NEXT DECB  PREFIX
         ST    R14,24(,R3)        INDICATE PRECEDING  AVAILABLE
         ST    R3,EXTRDECB
         CLI   4(R3),X'7F'        I/O ALREADY SUCCESSFULLY COMPLETED ??
         JE    EXTRNEWB
         LA    R1,4(,R3)          LOAD DECB ADDRESS
         L     R15,EXTRCHKA       WAIT FOR  READ TO COMPLETE
         BASR  R14,R15
*
EXTRNEWB L     R1,4+12(,R3)       LOAD  BUFFER ADDRESS FROM  DECB
         LH    R0,0(,R1)          LOAD   BLOCK  LENGTH FROM  "BDW"
         AR    R0,R1              COMPUTE END-OF-BLOCK ADDRESS
         ST    R0,EXTREOD
         AHI   R1,4               SKIP   "BDW"
*
EXTRFMT  ST    R1,EXTRRECA        SAVE  EXTRACT RECORD   ADDRESS
         AP    EXTRCNT,p001
                     SPACE 3
*
MERGNEXT  ds 0H
          if (CLI,EXTREOF,eq,C'Y')    ANY EXTRACT RECS REMAINING ???
            L     R14,MERGDONE
            BR    R14
          endif                       return
*
         L     R1,EXTRRECA              LOAD EXTRACT FILE RECORD ADDR
         MVI   LASTFILE,C'E'         ASSUME EXTRACT RECORD SELECTED
         BR    R10                RETURN
*
                     SPACE 3
EXTREND  MVI   EXTREOF,C'Y'       INDICATE END-OF-FILE  ON EXTRACT
* SORT (if used) has completed here
*
         J     MERGNEXT
*
EXTRNEXT L     R1,EXTRRECA        RETURN NEXT EXTRACT RECORD
         MVI   LASTFILE,C'E'
         BR    R10
                     SPACE 3
*
static   loctr
*
MERGDONE DC    A(READEOF)
READCLC  CLC   EXVIEW#-EXTREC(0,R1),EXVIEW#-EXTREC(R14)
*
code     loctr
*
         DROP  R7
         DROP  R9
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V A L I D A T E   H E A D E R   R E C O R D  F I E L D S     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING HDRREC,R1
*
VALIDHDR LH    R15,HDSORTLN       LOAD SORT   KEY LENGTH
         LTR   R15,R15            ANY  SORT   KEY ???
         JNP   VALIDCNT           NO - BYPASS CHECK  FOR LOW VALUES
         BCTR  R15,0              DECREMENT   FOR "EX"
         EX    R15,CHKHDRKY       HEADER SORT KEY =  LOW VALUES  ???
         JNZ   VALIDERR           NO  -  INDICATE ERROR
*
VALIDCNT LH    R15,HDNCOL         LOAD NO. OF COLUMNS
         LTR   R15,R15            CORRECT  NUMBER  ???
         JNZ   VALIDERR           NO  -  INDICATE ERROR
*
         LH    R15,HDDATALN       LOAD   DATA AREA LENGTH
         LTR   R15,R15            LENGTH SHOULD BE NEGATIVE
         JNM   VALIDERR
         LCR   R15,R15            CONVERT  TO POSITIVE NUMBER
         STH   R15,HDDATALN
         LHI   R0,HDDATLEN        LOAD  CORRECT   SIZE
         CR    R15,R0             DATA  AREA IS   CORRECT SIZE ???
         JNE   VALIDERR           NO  -  INDICATE ERROR
*
         SR    R15,R15
         BR    R10                RETURN
*
VALIDERR LHI   R15,4
         BR    R10                RETURN
*
         DROP  R1
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*    P E R F O R M   R E P O R T   B R E A K   P R O C E S S I N G    *
*                                                                     *
*                                                                     *
* 1. FORCE SUBTOTAL BREAKS FOR ALL PREVIOUS REQUEST'S CONTROL BREAKS  *
* 2. PRINT GRAND    TOTALS                                            *
*                                                                     *
*    NOTE: LEAVE REGISTER 12 UNTOUCHED IN THIS ROUTINE                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING HDRREC,R7
         USING VIEWREC,R8
*
RPTBREAK L     R0,SVVIEW#         FIRST TIME  (SVVIEW# = 0) ???
         LTR   R0,R0
         BNPR  R10                YES - RETURN
*
         ST    R10,SAVERBRK       SAVE  RETURN ADDRESS
*
         L     R3,SUBTOTAD        LOAD  BASE SET OF VALUES ADDRESS
         ST    R3,CALCBASE
*
         CLI   VWDESTYP+1,FILEFMT DATA  FILE OUTPUT ???
         JE    RPTBREA1           YES - BYPASS  BREAKS
         CLI   VWDESTYP+1,CSV     COMMA SEPARATED   VARIABLES  ???
         JNE   RBRKSUBT           NO  - PROCESS ALL BREAKS
*
RPTBREA1 EQU   *
         CLI   VWSUMTYP+1,DETAIL  DETAIL FILE   ???
         JE    RBRKRSET           YES -  RESET  ACCUMULATORS
*
         L     R7,PREVRECA        POINT TO PREVIOUS EXTRACT RECORD
         LHI   R15,1              INDICATE AT LOWEST BREAK LEVEL
         STH   R15,SVBRKCNT
*
         BRAS  R10,COMFILE        WRITE SUMMARY FILE RECORD
*
         L     R3,SUBTOTAD        LOAD  BASE SET OF  VALUES ADDRESS
         J     RBRKRSET
                     SPACE 3
RBRKSUBT SR    R2,R2              INDICATE BREAK ON HIGHEST LEVEL KEY
*
         BRAS  R10,KEYBREAK       PROCESS ALL SUBTOTAL LEVELS
*
*        XC    SVBRKCNT,SVBRKCNT  RESET   BREAK LEVELS             @01C
*
         USING SORTKEY,R4         SET BY "KEYBREAK" TO GRAND TOTAL
*
         TM    VWFLAG2,VWEXEC+VWPVIT    EXEC   INFO    ???
         JNZ   RBRKGRND           YES - ALWAYS INCLUDE GRAND TOTAL
         CLI   GTOTLIND,C'Y'      PRINT  GRAND TOTALS  ???
         JE    RBRKGRND           YES - BYPASS FURTHER CHECKS
*
         OC    VWCLCCOL,VWCLCCOL  ANY   "CT"   COLUMNS ???
         JZ    RBRKRSET
         CLI   VWSUMTYP+1,DETAIL  DETAIL FILE  ???
         JNE   RBRKRSET           NO - BYPASS  SPECIAL CASE
         LH    R0,VWSRTCNT        SINGLE SORT  KEY ???
         CHI   R0,1
         JNE   RBRKRSET           NO - GRAND TOTALS NOT AUTOMATIC
*
RBRKGRND TM    VWFLAG1,VWDWNONL   DOWNLOAD FILE ONLY (SUPPRESS PRINT) ?
         JO    RBRKRSET           BYPASS ACTUAL PRINTING
*
         XC    NEEDTITL,NEEDTITL  
         BRAS  R10,GRNDINIT       INITIALIZE ADDRESSES FOR GRAND TOTALS
*
         MVI   SKFTRBRK+1,SUBTOTAL ASSUME GRAND TOTAL PRINTED
         L     R14,VWSKADDR        CHECK HEADER BREAK OPTION ON 1ST LVL
         CLI   SKHDRBRK-SORTKEY+1(R14),TITLEL1
         JL    RBRKGPRT
         CLI   SKHDRBRK-SORTKEY+1(R14),TITLEL5
         JH    RBRKGPRT
         MVI   SKFTRBRK+1,NOSUBTTL SUPPRESS GRAND TOTAL
*
RBRKGPRT ds    0h
         BRAS  R10,COLPRNT         PRINT GRAND TOTALS FOR REPORT
*
         DROP  R4

RBRKRSET BRAS  R9,FOOTPRNT        PRINT PAGE FOOTERS
         L     R0,SVSORTKY        CLEAR OUT    KEY
         LHI   R1,SVSORTLN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
                     EJECT
         MVI   GTOTLIND,C' '      RESET  PRINT  GRAND TOTALS  IND
         LH    R0,VWCLCCOL        LOAD NUMBER OF CALC COLUMNS USED
         LTR   R0,R0              ANY  USED   ???
         JNP   RBRKRTN
*
         SR    R15,R15            ZERO COLUMN OFFSET
*
         LR    R9,R3              COMPUTE GRAND TOTAL MAXIMUM BASE
         S     R9,SUBTOTAD
         A     R9,EXTMAXA
         LR    R10,R3             COMPUTE GRAND TOTAL MINIMUM BASE
         S     R10,SUBTOTAD
         A     R10,EXTMINA
*
RBRKZERO LR    R14,R3                  LOAD   GRAND TOTALS BASE ADDRESS
         AR    R14,R15                 ADD    OFFSET
         GVBSTX   fp8,0(,r14)               reset   grand totals
*
         L     R14,EXTFRSTA            LOAD   FIRST VALUE  BASE ADDRESS
         AR    R14,R15                 ADD    OFFSET
         GVBSTX   fp8,0(,r14)               reset   grand totals
*
         L     R14,EXTLASTA            LOAD   LAST  VALUE  BASE ADDRESS
         AR    R14,R15                 ADD    OFFSET
         GVBSTX   fp8,0(,r14)               reset   grand totals
*
         L     R14,EXTPREVA            LOAD   PREV  VALUE  BASE ADDRESS
         AR    R14,R15                 ADD    OFFSET
         GVBSTX   fp8,0(,r14)               reset   grand totals
*
         LR    R14,R9                  LOAD   MAX   VALUE  BASE ADDRESS
         AR    R14,R15                 ADD    OFFSET
         GVBSTX   fp8,0(,r14)               reset   grand totals
*
         LR    R14,R10                 LOAD   MIN   VALUE  BASE ADDRESS
         AR    R14,R15                 ADD    OFFSET
         MVC   0(AccumDFPl,R14),high_dec_val T MIN  VALUES
*
         AHI   R15,AccumDFPl           ADVANCE TO NEXT COLUMN
         BRCT  R0,RBRKZERO             LOOP    THROUGH ALL COLUMNS
*
RBRKRTN  L     R10,SAVERBRK       RETURN
         BR    R10
*
         DROP  R7
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*    I N I T I A L I Z A T I O N   F O R   G R A N D   T O T A L S    *
*              (SET R3 = SET  OF  GRAND TOTALS)                       *
*              (SET R4 = SORT KEY TITLE)                              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING VIEWREC,R8
*
GRNDINIT LH    R14,VWSETLEN       LOAD LENGTH OF ONE    SET
         LH    R15,VWBRKCNT       LOAD NUMBER OF BREAK  LEVELS
         MR    R14,R14            COMPUTE OFFSET TO GRAND  TOTALS
         L     R3,SUBTOTAD
         AR    R3,R15
         ST    R3,CALCBASE
*
         LHI   R15,SKENTLEN       LOAD LENGTH OF EACH TITLE KEY
         L     R4,VWSKADDR        LOAD  TITLE AREA   ADDRESS
         SR    R4,R15             ADD   OFFSET
         ST    R4,CURSRTKY
*
         TM    VWFLAG2,VWEXEC+VWPVIT EXEC INFO OR PIVOT TABLE ???
         JZ    GRNDEXIT
         XC    VWLINENO,VWLINENO  AVOID PAGE   BREAK
         XC    NEEDTITL,NEEDTITL  RESET NUMBER TITLE LINES   NEEDED
         MVI   NEEDDASH,C' '      RESET DASH  NEEDED INDICATOR
*
GRNDEXIT BR    R10                RETURN
*
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        B U I L D   R E P O R T   T I T L E   L I N E S              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING VIEWREC,R8
*
BLDRPTTL XC    SVPG#ADR,SVPG#ADR  ZERO  PAGE NO. ADDRESS IN RPT TITLE
         xc    svpg#h_use_cnt,svpg#h_use_cnt  .zero the use count
         la    r15,svpg#adr       .get start address of page num area
         st    r15,svpg#h_next_free .this is the 1st free one
*
         L     R9,VWRTADDR        LOAD  VDP REPORT TITLE RECORD ADDRESS
         LTR   R9,R9              HEADER  RECORDS  PRESENT  ???
         JNZ   BLDRHDR            YES -   BYPASS   V3  DEFAULTS
*
***********************************************************************
*  BUILD VERSION 3 STYLE ONLINE REPORT HEADERS                        *
***********************************************************************
         CLI   VWDESTYP+1,ONLINE  ONLINE  REQUEST  ???
         JNE   BLDRBAT            NO  -   BUILD    BATCH FORMAT TITLES
*
         MVC   VWMAXRPT,H002
*
         LA    R1,VWCOMPNM        POINT  TO COMPANY NAME
         LHI   R15,L'VWCOMPNM
         BRAS  R14,CALCTLEN       DETERMINE  ACTUAL TITLE LENGTH (R15)
         LTR   R15,R15            ALL BLANK  ???
         JP    BLDRPTT1           NO  -  BYPASS RESET
         LHI   R15,L'VWCOMPNM     YES -  USE ORIGINAL LENGTH
*
BLDRPTT1 EQU   *
         L     R1,RTITLEA         BLANK  OUT WHOLE 1ST LINE
         USING RTITLE,R1
         LHI   R14,L'RPTITLE
         STH   R14,RTRECLEN
         MVC   RTCC(L'RPTITLE-4),SPACES
*
         LHI   R14,L'VWCOMPNM     COMPUTE EXCESS ROOM AND DIVIDE BY TWO
         SR    R14,R15
         SRL   R14,1
         LA    R14,RTCC+5(R14)    LOAD CENTERED TEXT ADDRESS
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCTITL1
                     SPACE 3
         LA    R1,VWTITLE         POINT  TO  REPORT TITLE
         LHI   R15,L'VWTITLE
         BRAS  R14,CALCTLEN       DETERMINE  ACTUAL TITLE LENGTH (R15)
         LTR   R15,R15            ALL BLANK  ???
         JP    BLDRPTT2           NO  -  BYPASS RESET
         LHI   R15,L'VWTITLE      YES -  USE ORIGINAL LENGTH
*
BLDRPTT2 EQU   *
         L     R1,RTITLEA         BLANK OUT WHOLE 2ND LINE
         AHI   R1,PRNTMAXL+4
         LHI   R14,L'RPTITLE
         STH   R14,RTRECLEN
         MVC   RTCC(L'RPTITLE-4),SPACES
*
         LHI   R14,L'VWTITLE      COMPUTE EXCESS ROOM AND DIVIDE BY TWO
         SR    R14,R15
         SRL   R14,1
         LA    R14,RTCC+1(R14)    LOAD CENTERED TEXT ADDRESS
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCTITL2
*
         J     BLDREXIT           RETURN
                     SPACE 3
***********************************************************************
*  BUILD VERSION 3 STYLE BATCH  REPORT HEADERS                        *
***********************************************************************
BLDRBAT  LH    R0,VWMAXRPT        LOAD  REPORT TITLE LINE COUNT
         LTR   R0,R0              ANY   LINES  NOTED PREVIOUSLY ???
         JP    BLDRBAT1
         LHI   R0,2
         STH   R0,VWMAXRPT
*
BLDRBAT1 EQU   *
         LA    R1,VWCOMPNM        POINT  TO COMPANY NAME
         LHI   R15,L'VWCOMPNM
         BRAS  R14,CALCTLEN       DETERMINE  ACTUAL TITLE LENGTH (R15)
         LTR   R15,R15            ALL BLANK  ???
         JP    BLDRBAT2           NO  -  BYPASS RESET
         LHI   R15,L'VWCOMPNM     YES -  USE ORIGINAL LENGTH
*
***********************************************************************
*  REPORT TITLE LINE 1                                                *
***********************************************************************
BLDRBAT2 EQU   *
         L     R1,RTITLEA         BLANK OUT WHOLE 1ST LINE
         USING RTITLE,R1
*
         LH    R14,VWLINSIZ
         LTR   R14,R14
         JP    BLDRBAT3
         LHI   R14,L'RPTITLE-4    EXCL  RDW
*
BLDRBAT3 EQU   *
         BCTR  R14,0              DECREMENT  FOR "EX"
         EX    R14,BLDRSPAC
         AHI   R14,4+1
         STH   R14,RTRECLEN
         XC    RTRECLEN+2(2),RTRECLEN+2
*
         MVI   RTCC,C'1'
         MVC   RTLEFT(L'RPTIDLIT),RPTIDLIT
         MVC   RTLEFT+L'RPTIDLIT(L'MR88NAME),MR88NAME
*
         LA    R14,0(R1,R14)
         LHI   R0,L'PAGELIT+8
         SR    R14,R0
         MVC   0(L'PAGELIT,R14),PAGELIT
         MVC   L'PAGELIT(8,R14),ZEROES
*
         la    r0,l'pagelit(,r14)
         l     r14,svpg#h_use_cnt      .Get number of page numbers
         chi   r14,svpg#_max           .Are we at the max
         je    bldbat_byppage          .Then ignore it  (temp)
         ahi   r14,1                   .add new one to count
         st    r14,svpg#h_use_cnt      .and save it
         l     r14,svpg#h_next_free    .Get next place to store address
         st    r0,0(,r14)              .save address
         mvi   7(R14),x'03'            .default to justify right
         ahi   r14,8                   .new next free area
         st    r14,svpg#h_next_free    .save it
bldbat_byppage ds 0h
*
         LH    R14,VWLINSIZ       CENTER  TITLE
         SRL   R14,1
         LR    R0,R15
         SRL   R0,1
         SR    R14,R0
         LA    R14,RTLEFT(R14)    LOAD CENTERED TEXT ADDRESS
*
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCTITL1
                     SPACE 3
***********************************************************************
*  REPORT TITLE LINE 2                                                *
***********************************************************************
         LA    R1,VWTITLE         POINT  TO  REPORT TITLE
         LHI   R15,L'VWTITLE
         BRAS  R14,CALCTLEN       DETERMINE  ACTUAL TITLE LENGTH (R15)
         LTR   R15,R15            ALL BLANK  ???
         JP    BLDRBAT4           NO  -  BYPASS RESET
         LHI   R15,L'VWTITLE      YES -  USE ORIGINAL LENGTH
*
BLDRBAT4 EQU   *
         L     R1,RTITLEA         BLANK OUT WHOLE 2ND LINE
         AHI   R1,PRNTMAXL+4
*
         LH    R14,VWLINSIZ
         LTR   R14,R14
         JP    BLDRBAT5
         LHI   R14,L'RPTITLE-4    EXCL  RDW
*
BLDRBAT5 EQU   *
         BCTR  R14,0              DECREMENT  FOR "EX"
         EX    R14,BLDRSPAC
         AHI   R14,4+1
         STH   R14,RTRECLEN
         XC    RTRECLEN+2(2),RTRECLEN+2
*
         MVC   RTLEFT(L'VIEWLIT),VIEWLIT
         L     R0,SVVIEW#
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   RTLEFT+L'VIEWLIT(L'BLDRMASK),BLDRMASK
         ED    RTLEFT+L'VIEWLIT(L'BLDRMASK),DBLWORK+4
*
         LA    R14,0(R1,R14)
         LHI   R0,8+1+5
         SR    R14,R0
*
         MVC   00(2,R14),SVPROCDT+04
         MVI   02(R14),C'/'
         MVC   03(2,R14),SVPROCDT+06
         MVI   05(R14),C'/'
         MVC   06(2,R14),SVPROCDT+02
*
         MVC   9+00(2,R14),SVPROCTM+00
         MVI   9+02(R14),C':'
         MVC   9+03(2,R14),SVPROCTM+02
*
         LH    R14,VWLINSIZ       CENTER  TITLE
         SRL   R14,1
         LR    R0,R15
         SRL   R0,1
         SR    R14,R0
         LA    R14,RTLEFT(R14)    LOAD CENTERED TEXT ADDRESS
*
         BCTR  R15,0              DECREMENT LENGTH FOR "EX"
         EX    R15,MVCTITL2
*
         J     BLDREXIT
*
***********************************************************************
*  PROCESS CUSTOM VIEW REPORT HEADER RECORDS (VDP 1300 RECORDS)       *
***********************************************************************
         USING VDP1300_TITLE_LINES_RECORD,R9
*
BLDRHDR  LH    R0,VWMAXRPT        LOAD NUMBER OF TITLE LINES
*
         L     R1,RTITLEA
         USING RTITLE,R1
*
         LH    R15,VWLINSIZ
         CHI   R15,PRNTMAXL
         JNH   BLDRHDRU
         LHI   R15,PRNTMAXL
BLDRHDRU EQU   *
         AHI   R15,4
*
BLDRHDRS STH   R15,RTRECLEN       BLANK  OUT  REPORT   TITLE LINES
         MVC   RTCC(PRNTMAXL),SPACES
         C     R1,RTITLEA         FIRST  LINE ???
         JNE   BLDRHDRK
         MVI   RTCC,C'1'
BLDRHDRK EQU   *
         AHI   R1,PRNTMAXL+4
         BRCT  R0,BLDRHDRS
*
BLDRHDRL LH    R0,VDP1300_ROW_NBR
*
         BCTR  R0,0
         LHI   R1,PRNTMAXL+4
         MR    R0,R0
         A     R1,RTITLEA
*
***********************************************************************
*  PROCESSED DATE                                                     *
***********************************************************************
         L     R0,VDP1300_FUNCTION       LOAD  FUNCTION     CODE
*
BLDRHDRD CHI   R0,TTLDATE
         JNE   BLDRHDRT
         LHI   R14,8
*
         LA    R15,RTLEFT                ASSUME LEFT AREA JUSTIFICATION
         A     R15,VDP1300_TITLE_AREA_OFFSET
*
         L     R0,VDP1300_JUSTIFICATION  POINT  TO SPECIFIED TITLE AREA
         CHI   R0,TTLRIGHT
         JNE   BLDRHDRQ
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,VDP1300_TITLE_AREA_OFFSET
*
BLDRHDRQ EQU   *
         CHI   R0,TTLCENTR
         JNE   BLDRHDR1
         LH    R15,RTRECLEN
         LHI   R0,4+1
         SR    R15,R0
         SH    R15,VDP1300_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,VDP1300_TITLE_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDRHDR1 MVC   00(2,R15),SVPROCDT+04
         MVI   02(R15),C'/'
         MVC   03(2,R15),SVPROCDT+06
         MVI   05(R15),C'/'
         MVC   06(2,R15),SVPROCDT+02
*
         J     BLDRHDRN
*
***********************************************************************
*  PROCESSED TIME                                                     *
***********************************************************************
BLDRHDRT CHI   R0,TTLTIME
         JNE   BLDRHDRP
*
         LHI   R14,5
*
         LA    R15,RTLEFT
         A     R15,VDP1300_TITLE_AREA_OFFSET
*
         L     R0,VDP1300_JUSTIFICATION  POINT TO SPECIFIED TITLE AREA
         CHI   R0,TTLRIGHT
         JNE   BLDRHDRY
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,VDP1300_TITLE_AREA_OFFSET
*
BLDRHDRY EQU   *
         CHI   R0,TTLCENTR
         JNE   BLDRHDR2
         LH    R15,RTRECLEN
         LHI   R0,4+1
         SR    R15,R0
         SH    R15,VDP1300_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,VDP1300_TITLE_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDRHDR2 EQU   *
         MVC   00(2,R15),SVPROCTM+00
         MVI   02(R15),C':'
         MVC   03(2,R15),SVPROCTM+02
*
         J     BLDRHDRN
*
***********************************************************************
*  PAGE NUMBER                                                        *
***********************************************************************
BLDRHDRP CHI   R0,TTLPAGE
         JNE   BLDRHDRV
*
         LA    R15,RTLEFT
         A     R15,VDP1300_TITLE_AREA_OFFSET
*
         L     R0,VDP1300_JUSTIFICATION  POINT TO SPECIFIED TITLE AREA
         CHI   R0,TTLRIGHT
         JNE   BLDRHDRZ
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,VDP1300_TITLE_AREA_OFFSET
*
BLDRHDRZ EQU   *
         CHI   R0,TTLCENTR
         JNE   BLDRHDR3
         LH    R15,RTRECLEN
         LHI   R0,4+1
         SR    R15,R0
         SH    R15,VDP1300_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,VDP1300_TITLE_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDRHDR3 ds    0h
         l     r14,svpg#h_use_cnt      .Get number of page numbers
         chi   r14,svpg#_max           .Are we at the max
         je    bldrhdrn                .Then ignore it  (temp)
         ahi   r14,1                   .add new one to count
         st    r14,svpg#h_use_cnt      .and save it
         l     r14,svpg#h_next_free    .Get next place to store address
         st    r15,0(,r14)             .save address
         st    r0,4(,r14)
         la    r14,8(,r14)             .new next free area
         st    r14,svpg#h_next_free    .save it
         mvc   0(l'bldrmask,r15),spaces
         j     bldrhdrn
*
***********************************************************************
*  VIEW NUMBER                                                        *
***********************************************************************
BLDRHDRV CHI   R0,TTLVIEW
         JNE   BLDRHDRR
*
         l     r0,svview#
         cvd   r0,dblwork
         oi    dblwork+l'dblwork-1,x'0F'
         mvc   dblwork2(l'bldrmask),bldrmask
         lr    r0,r1                    .Save R1
         la    r14,dblwork2             .Start of area
         lr    r1,r14
         edmk  dblwork2(l'bldrmask),dblwork+4
         sr    r1,r14                   .Get length to result byte
         lr    r2,r1
         lr    r1,r0
         lr    r0,r2
         l     r15,vdp1300_justification
*
*
bldrhdrv_left ds 0h
         chi   r15,ttlleft              .Left justification?
         jne   bldrhdrv_right           .N: try for Right
         la    r15,rtleft
         a     r15,vdp1300_title_area_offset
         la    R14,dblwork2(r2)         .offset to start of view number
         lhi   r2,l'bldrmask            .max length of area
         sr    r2,r0                    .length of view number
         bctr  r2,r0                    .set up for "EX"
         ex    r2,just_move
         j     bldrhdrn
         ds    0d

*
bldrhdrv_right ds 0h
         chi   r15,ttlright             .Right justification
         jne   bldrhdrv_centre          .N: try for Centre
         lr    r15,r1
         ah    r15,rtreclen
         s     r15,vdp1300_title_area_offset
         mvc   0(l'bldrmask,r15),dblwork2
         j     bldrhdrn

*
bldrhdrv_centre ds 0h                   .Centre justification
         lh    r15,rtreclen
         lhi   r14,4+1
         sr    r15,r14
         sh    r15,vdp1300_area_total_length
         srl   r15,1
         a     r15,vdp1300_title_area_offset
         la    r15,rtleft(r15)
         la    R14,dblwork2(r2)         .offset to start of view number
         lhi   r2,l'bldrmask            .max length of area
         sr    r2,r0                    .length of view number
         lr    r0,r2                    .save length
         srl   r2,1                     .divide by 2 for correct centre
         ar    r15,r2                   .where to really put it
         lr    r2,r0                    .view number length again
         bctr  r2,r0                    .set up for "EX"
         ex    r2,just_move
         j     bldrhdrn
*
***********************************************************************
*  RUN DATE                                                           *
***********************************************************************
BLDRHDRR CHI   R0,TTLRDATE
         JNE   BLDRHDR#
*
         LHI   R14,8
*
         LA    R15,RTLEFT                ASSUME LEFT AREA JUSTIFICATION
         A     R15,VDP1300_TITLE_AREA_OFFSET
*
         L     R0,VDP1300_JUSTIFICATION  POINT  TO SPECIFIED TITLE AREA
         CHI   R0,TTLRIGHT
         JNE   BLDRHDRJ
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,VDP1300_TITLE_AREA_OFFSET
*
BLDRHDRJ EQU   *
         CHI   R0,TTLCENTR
         JNE   BLDRHDR5
         LH    R15,RTRECLEN
         LHI   R0,4+1
         SR    R15,R0
         SH    R15,VDP1300_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,VDP1300_TITLE_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDRHDR5 MVC   00(2,R15),SVRUNDT+04
         MVI   02(R15),C'/'
         MVC   03(2,R15),SVRUNDT+06
         MVI   05(R15),C'/'
         MVC   06(2,R15),SVRUNDT+02
*
         J     BLDRHDRN
*
***********************************************************************
*  RUN NUMBER                                                         *
***********************************************************************
BLDRHDR# CHI   R0,TTLRUN#
         JNE   BLDRHDRF
*
         LHI   R14,L'BLDRMASK
*
         LA    R15,RTLEFT
         A     R15,VDP1300_TITLE_AREA_OFFSET
*
         L     R0,VDP1300_JUSTIFICATION  POINT TO SPECIFIED TITLE AREA
         CHI   R0,TTLRIGHT
         JNE   BLDRHDRW
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,VDP1300_TITLE_AREA_OFFSET
*
BLDRHDRW EQU   *
         CHI   R0,TTLCENTR
         JNE   BLDRHDR6
         LH    R15,RTRECLEN
         LHI   R0,4+1
         SR    R15,R0
         SH    R15,VDP1300_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,VDP1300_TITLE_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDRHDR6 L     R0,SVRUN#
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   0(L'BLDRMASK,R15),BLDRMASK
         ED    0(L'BLDRMASK,R15),DBLWORK+4
*
         J     BLDRHDRN
*
***********************************************************************
*  FINANCIAL PERIOD DATE                                              *
***********************************************************************
BLDRHDRF CHI   R0,TTLFDATE
         JNE   BLDRHDRX
*
         LHI   R14,7
*
         LA    R15,RTLEFT
         A     R15,VDP1300_TITLE_AREA_OFFSET
*
         L     R0,VDP1300_JUSTIFICATION  POINT TO SPECIFIED TITLE AREA
         CHI   R0,TTLRIGHT
         JNE   BLDRHD1F
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,VDP1300_TITLE_AREA_OFFSET
*
BLDRHD1F EQU   *
         CHI   R0,TTLCENTR
         JNE   BLDRHDR7
         LH    R15,RTRECLEN
         LHI   R0,4+1
         SR    R15,R0
         SH    R15,VDP1300_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,VDP1300_TITLE_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDRHDR7 LAY   R14,FISCAL_DATE
         MVC   00(4,R15),0(R14)
         MVI   04(R15),C'/'
         MVC   05(2,R15),4(R14)
*
         J     BLDRHDRN
*
***********************************************************************
*  FREE FORM TEXT                                                     *
***********************************************************************
BLDRHDRX CHI   R0,TTLTEXT
         JNE   BLDRHDRC
*
         LH    R0,VDP1300_TITLE_LENGTH
         LA    R14,VDP1300_TITLE_TEXT
         J     BLDRJUST
*
***********************************************************************
*  COMPANY NAME                                                       *
***********************************************************************
BLDRHDRC CHI   R0,TTLCONAM
         JNE   BLDRHDRM
*
         LHI   R0,L'VWCOMPNM
         LA    R14,VWCOMPNM+L'VWCOMPNM     ENDING ADDRESS  (+1)
BLDRHDR8 BCTR  R14,0              BACKUP   TO PREVIOUS CHARACTER
         CLI   0(R14),C' '        TRAILING BLANK  ???
         JNE   BLDRHDR@           NO - EXIT
         BRCT  R0,BLDRHDR8
         LHI   R0,L'VWCOMPNM
*
BLDRHDR@ EQU   *
         LA    R14,VWCOMPNM
         J     BLDRJUST
*
***********************************************************************
*  VIEW NAME                                                          *
***********************************************************************
BLDRHDRM CHI   R0,TTLVWNAM
         JNE   BLDRHDRO
*
         LHI   R0,L'VWTITLE
         LA    R14,VWTITLE+L'VWTITLE     ENDING  ADDRESS  (+1)
BLDRHDR9 BCTR  R14,0              BACKUP TO PREVIOUS CHARACTER
         CLI   0(R14),C' '        TRAILING BLANK ???
         JNE   BLDRHDRB           NO - EXIT
         BRCT  R0,BLDRHDR9
         LHI   R0,L'VWTITLE
*
BLDRHDRB EQU   *
         LA    R14,VWTITLE
         J     BLDRJUST
*
***********************************************************************
*  VIEW OWNER                                                         *
***********************************************************************
BLDRHDRO CHI   R0,TTLOWNER
         JNE   BLDRSKEY
*
         LHI   R0,L'VWUSERID
         LA    R14,VWUSERID+L'VWUSERID    ENDING  ADDRESS  (+1)
BLDRHDRA BCTR  R14,0              BACKUP TO PREVIOUS CHARACTER
         CLI   0(R14),C' '        TRAILING BLANK ???
         JNE   BLDRHDRG           NO - EXIT
         BRCT  R0,BLDRHDRA
         LHI   R0,L'VWUSERID
*
BLDRHDRG EQU   *
         LA    R14,VWUSERID
         J     BLDRJUST
*
***********************************************************************
*  SORT BREAK FIELDS                                                  *
***********************************************************************
BLDRSKEY L     R14,VDP1300_SORT_TITLE_ADDRESS
         LTR   R14,R14
         JNP   BLDRHDRN
*
         CHI   R0,TTLSLBL1        SORT KEY  LABEL
         JE    BLDRSLBL
         CHI   R0,TTLSLBL2
         JE    BLDRSLBL
         CHI   R0,TTLSLBL3
         JE    BLDRSLBL
         CHI   R0,TTLSLBL4
         JE    BLDRSLBL
         CHI   R0,TTLSLBL5
         JNE   BLDRHDRN
*
BLDRSLBL LH    R0,SKLBLLEN-SORTKEY(,R14)
         LA    R14,SKLABEL-SORTKEY(,R14)
*
BLDRJUST LA    R15,RTLEFT
         A     R15,VDP1300_TITLE_AREA_OFFSET
*
         CLI   VDP1300_JUSTIFICATION+3,TTLRIGHT
         JNE   BLDRJUS1
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,VDP1300_TITLE_AREA_OFFSET
*
         J     BLDRJUS2
*
BLDRJUS1 CLI   VDP1300_JUSTIFICATION+3,TTLCENTR
         JNE   BLDRJUS2
         LH    R15,RTRECLEN
         AHI   R15,-5
         SH    R15,VDP1300_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,VDP1300_TITLE_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDRJUS2 LTR   R1,R0
         JNP   BLDRHDRN
*
         BCTR  R1,0
         EX    R1,BLDRCPY         COPY TEXT
*
BLDRHDRN AH    R9,VDP1300_REC_LEN      ADVANCE TO NEXT VDP 1300 RECORD
         LG    R9,0(,R9)               Pointer to next VDP element
         LH    R0,VDP1300_RECORD_TYPE  STILL WITHIN 1300'S ???
         CHI   R0,1300
         JNE   BLDREXIT                NO  - EXIT
         CLC   VWVIEW#,VDP1300_VIEWID  SAME  VIEW ???
         JE    BLDRHDRL                YES - LOOP
*
BLDREXIT EQU   *
         BR    R10                     RETURN
*
         DROP  R1
         DROP  R8
         DROP  R9
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        B U I L D   R E P O R T   F O O T E R   L I N E S            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING VIEWREC,R8
*
BLDFOOTR XC    SVPG#FTR,SVPG#FTR  ZERO  PAGE NO. ADDRESS IN RPT TITLE
         xc    svpg#f_use_cnt,svpg#f_use_cnt  .zero the use count
         la    r15,svpg#ftr       .get start address of page num area
         st    r15,svpg#f_next_free .this is the 1st free one
*
         L     R9,VWRFADDR        LOAD VDP REPORT FOOTER RECORD ADDRESS
         LTR   R9,R9              FOOTER  RECORDS PRESENT  ???
         JNP   BLDFEXIT           NO  -   EXIT
*
***********************************************************************
*  PROCESS CUSTOM VIEW REPORT FOOTER RECORDS (VDP 1400 RECORDS)       *
***********************************************************************
         USING VDP1400_FOOTER_LINES_RECORD,R9
*
BLDFHDR  LH    R0,VWMAXFTR        LOAD NUMBER OF TITLE LINES
         L     R1,RFOOTRA
         USING RTITLE,R1
*
         LH    R15,VWLINSIZ
         CHI   R15,PRNTMAXL
         JNH   BLDFHDRY
         LHI   R15,PRNTMAXL
BLDFHDRY EQU   *
         AHI   R15,4
*
BLDFHDRS STH   R15,RTRECLEN       BLANK  OUT  REPORT   TITLE LINES
         MVC   RTCC(PRNTMAXL),SPACES
         C     R1,RFOOTRA         FIRST  LINE ???
         JNE   BLDFHDRZ
         MVI   RTCC,C' '
BLDFHDRZ EQU   *
         AHI   R1,PRNTMAXL+4
         BRCT  R0,BLDFHDRS
*
BLDFHDRL LH    R0,VDP1400_ROW_NBR
         BCTR  R0,0
         LHI   R1,PRNTMAXL+4
         MR    R0,R0
         A     R1,RFOOTRA
*
***********************************************************************
*  PROCESSED DATE                                                     *
***********************************************************************
         L     R0,VDP1400_FUNCTION       LOAD  FUNCTION     CODE
*
BLDFHDRD CHI   R0,TTLDATE
         JNE   BLDFHDRT
*
         LHI   R14,8
*
         LA    R15,RTLEFT                ASSUME LEFT AREA JUSTIFICATION
         A     R15,vdp1400_footer_AREA_OFFSET
*
         L     R0,VDP1400_JUSTIFY        POINT  TO SPECIFIED TITLE AREA
         CHI   R0,TTLRIGHT
         JNE   BLDFHDRW
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,vdp1400_footer_AREA_OFFSET
*
BLDFHDRW EQU   *
         CHI   R0,TTLCENTR
         JNE   BLDFHDR1
         LH    R15,RTRECLEN
         LHI   R0,4+1
         SR    R15,R0
         SH    R15,VDP1400_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,vdp1400_footer_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDFHDR1 MVC   00(2,R15),SVPROCDT+04
         MVI   02(R15),C'/'
         MVC   03(2,R15),SVPROCDT+06
         MVI   05(R15),C'/'
         MVC   06(2,R15),SVPROCDT+02
*
         J     BLDFHDRN
*
***********************************************************************
*  PROCESSED TIME                                                     *
***********************************************************************
BLDFHDRT CHI   R0,TTLTIME
         JNE   BLDFHDRP
*
         LHI   R14,5
*
         LA    R15,RTLEFT
         A     R15,vdp1400_footer_AREA_OFFSET
*
         L     R0,VDP1400_JUSTIFY        POINT TO SPECIFIED TITLE AREA
         CHI   R0,TTLRIGHT
         JNE   BLDFHDRU
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,vdp1400_footer_AREA_OFFSET
*
BLDFHDRU EQU   *
         CHI   R0,TTLCENTR
         JNE   BLDFHDR2
         LH    R15,RTRECLEN
         LHI   R0,4+1
         SR    R15,R0
         SH    R15,VDP1400_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,vdp1400_footer_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDFHDR2 EQU   *
         MVC   00(2,R15),SVPROCTM+00
         MVI   02(R15),C':'
         MVC   03(2,R15),SVPROCTM+02
*
         J     BLDFHDRN
*
***********************************************************************
*  PAGE NUMBER                                                        *
***********************************************************************
BLDFHDRP CHI   R0,TTLPAGE
         JNE   BLDFHDRV
*
         LHI   R14,L'BLDRMASK
*
         LA    R15,RTLEFT
         A     R15,vdp1400_footer_AREA_OFFSET
*
         L     R0,VDP1400_JUSTIFY        POINT TO SPECIFIED TITLE AREA
         CHI   R0,TTLRIGHT
         JNE   BLDFHDRB
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,vdp1400_footer_AREA_OFFSET
*
BLDFHDRB EQU   *
         CHI   R0,TTLCENTR
         JNE   BLDFHDR3
         LH    R15,RTRECLEN
         LHI   R0,4+1
         SR    R15,R0
         SH    R15,VDP1400_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,vdp1400_footer_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
bldfhdr3 ds    0h
         l     r14,svpg#f_use_cnt      .Get number of page numbers
         chi   r14,svpg#_max           .Are we at the max
         je    bldrhdrn                .Then ignore it  (temp)
         ahi   r14,1                   .add new one to count
         st    r14,svpg#f_use_cnt      .and save it
         l     r14,svpg#f_next_free    .Get next place to store address
         st    r15,0(,r14)             .save address
         st    r0,4(,r14)
         la    r14,8(,r14)             .new next free area
         st    r14,svpg#f_next_free    .save it
         mvc   0(l'bldrmask,r15),spaces
         j     bldfhdrn
*
***********************************************************************
*  VIEW NUMBER                                                        *
***********************************************************************
BLDFHDRV CHI   R0,TTLVIEW
         JNE   BLDFHDRR
*
         l     r0,svview#
         cvd   r0,dblwork
         oi    dblwork+l'dblwork-1,x'0F'
         mvc   dblwork2(l'bldrmask),bldrmask
         lr    r0,r1                    .Save R1
         la    r14,dblwork2             .Start of area
         lr    r1,r14
         edmk  dblwork2(l'bldrmask),dblwork+4
         sr    r1,r14                   .Get length to result byte
         lr    r2,r1
         lr    r1,r0
         lr    r0,r2
         l     r15,vdp1400_justify
*
bldfhdrv_left ds 0h
         chi   r15,ttlleft              .Left justification?
         jne   bldfhdrv_right           .N: try for Right
         la    r15,rtleft
         a     r15,vdp1400_footer_area_offset
         la    R14,dblwork2(r2)         .offset to start of view number
         lhi   r2,l'bldrmask            .max length of area
         sr    r2,r0                    .length of view number
         bctr  r2,r0                    .set up for "EX"
         ex    r2,just_move
         j     bldfhdrn

*
bldfhdrv_right ds 0h
         chi   r15,ttlright             .Right justification
         jne   bldfhdrv_centre          .N: try for Centre
         lr    r15,r1
         ah    r15,rtreclen
         s     r15,vdp1400_footer_area_offset
         mvc   0(l'bldrmask,r15),dblwork2
         j     bldfhdrn

*
bldfhdrv_centre ds 0h                   .Centre justification
         lh    r15,rtreclen
         lhi   r14,4+1
         sr    r15,r14
         sh    r15,vdp1400_area_total_length
         srl   r15,1
         a     r15,vdp1400_footer_area_offset
         la    r15,rtleft(r15)
         la    R14,dblwork2(r2)         .offset to start of view number
         lhi   r2,l'bldrmask            .max length of area
         sr    r2,r0                    .length of view number
         lr    r0,r2                    .save length          
         srl   r2,1                     .divide by 2 for correct centre
         ar    r15,r2                   .where to really put it
         lr    r2,r0                    .view number length again
         bctr  r2,r0                    .set up for "EX"
         ex    r2,just_move
         j     bldfhdrn
*
*
***********************************************************************
*  RUN DATE                                                           *
***********************************************************************
BLDFHDRR CHI   R0,TTLRDATE
         JNE   BLDFHDR#
*
         LHI   R14,8
*
         LA    R15,RTLEFT                ASSUME LEFT AREA JUSTIFICATION
         A     R15,vdp1400_footer_AREA_OFFSET
*
         L     R0,VDP1400_JUSTIFY        POINT  TO SPECIFIED TITLE AREA
         CHI   R0,TTLRIGHT
         JNE   BLDF1DRR
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,vdp1400_footer_AREA_OFFSET
*
BLDF1DRR equ   *
         CHI   R0,TTLCENTR
         JNE   BLDFHDR5
         LH    R15,RTRECLEN
         LHI   R0,4+1
         SR    R15,R0
         SH    R15,VDP1400_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,vdp1400_footer_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDFHDR5 MVC   00(2,R15),SVRUNDT+04
         MVI   02(R15),C'/'
         MVC   03(2,R15),SVRUNDT+06
         MVI   05(R15),C'/'
         MVC   06(2,R15),SVRUNDT+02
*
         J     BLDFHDRN
*
***********************************************************************
*  RUN NUMBER                                                         *
***********************************************************************
BLDFHDR# CHI   R0,TTLRUN#
         JNE   BLDFHDRF
*
         LHI   R14,L'BLDRMASK
*
         LA    R15,RTLEFT
         A     R15,vdp1400_footer_AREA_OFFSET
*
         L     R0,VDP1400_JUSTIFY        POINT TO SPECIFIED TITLE AREA
         CHI   R0,TTLRIGHT
         JNE   BLDF2DRR
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,vdp1400_footer_AREA_OFFSET
*
BLDF2DRR EQU   *
         CHI   R0,TTLCENTR
         JNE   BLDFHDR6
         LH    R15,RTRECLEN
         LHI   R0,4+1
         SR    R15,R0
         SH    R15,VDP1400_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,vdp1400_footer_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDFHDR6 L     R0,SVRUN#
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   0(L'BLDRMASK,R15),BLDRMASK
         ED    0(L'BLDRMASK,R15),DBLWORK+4
*
         J     BLDFHDRN
*
***********************************************************************
*  FINANCIAL PERIOD DATE                                              *
***********************************************************************
BLDFHDRF CHI   R0,TTLFDATE
         JNE   BLDFHDRX
*
         LHI   R14,7
*
         LA    R15,RTLEFT
         A     R15,vdp1400_footer_AREA_OFFSET
*
         L     R0,VDP1400_JUSTIFY        POINT TO SPECIFIED TITLE AREA
         CHI   R0,TTLRIGHT
         JNE   BLDF3DRR
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,vdp1400_footer_AREA_OFFSET
*
BLDF3DRR EQU   *
         CHI   R0,TTLCENTR
         JNE   BLDFHDR7
         LH    R15,RTRECLEN
         LHI   R0,4+1
         SR    R15,R0
         SH    R15,VDP1400_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,vdp1400_footer_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDFHDR7 MVC   00(4,R15),VWFINPDT+00
         MVI   04(R15),C'/'
         MVC   05(2,R15),VWFINPDT+04
*
         J     BLDFHDRN
*
***********************************************************************
*  FREE FORM TEXT                                                     *
***********************************************************************
BLDFHDRX CHI   R0,TTLTEXT
         JNE   BLDFHDRC
*
         LH    R0,VDP1400_FOOTER_LENGTH
         LA    R14,VDP1400_FOOTER_TEXT
         J     BLDFJUST
*
***********************************************************************
*  COMPANY NAME                                                       *
***********************************************************************
BLDFHDRC CHI   R0,TTLCONAM
         JNE   BLDFHDRM
*
         LHI   R0,L'VWCOMPNM
         LA    R14,VWCOMPNM+L'VWCOMPNM     ENDING ADDRESS  (+1)
BLDFHDR8 BCTR  R14,0              BACKUP   TO PREVIOUS CHARACTER
         CLI   0(R14),C' '        TRAILING BLANK  ???
         JNE   BLDF4DRR           NO - EXIT
         BRCT  R0,BLDFHDR8
         LHI   R0,L'VWCOMPNM
*
BLDF4DRR EQU   *
         LA    R14,VWCOMPNM
         J     BLDFJUST
*
***********************************************************************
*  VIEW NAME                                                          *
***********************************************************************
BLDFHDRM CHI   R0,TTLVWNAM
         JNE   BLDFHDRO
*
         LHI   R0,L'VWTITLE
         LA    R14,VWTITLE+L'VWTITLE     ENDING  ADDRESS  (+1)
BLDFHDR9 BCTR  R14,0              BACKUP TO PREVIOUS CHARACTER
         CLI   0(R14),C' '        TRAILING BLANK ???
         JNE   BLDF5DRR           NO - EXIT
         BRCT  R0,BLDFHDR9
         LHI   R0,L'VWTITLE
*
BLDF5DRR EQU   *
         LA    R14,VWTITLE
         J     BLDFJUST
*
***********************************************************************
*  VIEW OWNER                                                         *
***********************************************************************
BLDFHDRO CHI   R0,TTLOWNER
         JNE   BLDFSKEY
*
         LHI   R0,L'VWUSERID
         LA    R14,VWUSERID+L'VWUSERID    ENDING  ADDRESS  (+1)
BLDFHDRA BCTR  R14,0              BACKUP TO PREVIOUS CHARACTER
         CLI   0(R14),C' '        TRAILING BLANK ???
         JNE   BLDF6DRR           NO - EXIT
         BRCT  R0,BLDFHDRA
         LHI   R0,L'VWUSERID
*
BLDF6DRR EQU   *
         LA    R14,VWUSERID
         J     BLDFJUST
*
***********************************************************************
*  SORT BREAK FIELDS                                                  *
***********************************************************************
BLDFSKEY L     R14,VDP1400_SORT_TITLE_ADDRESS
         LTR   R14,R14
         JNP   BLDFHDRN
*
         CHI   R0,TTLSLBL1        SORT KEY  LABEL
         JE    BLDFSLBL
         CHI   R0,TTLSLBL2
         JE    BLDFSLBL
         CHI   R0,TTLSLBL3
         JE    BLDFSLBL
         CHI   R0,TTLSLBL4
         JE    BLDFSLBL
         CHI   R0,TTLSLBL5
         JNE   BLDFHDRN
*
BLDFSLBL LH    R0,SKLBLLEN-SORTKEY(,R14)
         LA    R14,SKLABEL-SORTKEY(,R14)
*
BLDFJUST LA    R15,RTLEFT
         A     R15,vdp1400_footer_AREA_OFFSET
*
         CLI   VDP1400_JUSTIFY+3,TTLRIGHT
         JNE   BLDFJUS1
         LR    R15,R1
         AH    R15,RTRECLEN
         S     R15,vdp1400_footer_AREA_OFFSET
*
         J     BLDFJUS2
*
BLDFJUS1 CLI   VDP1400_JUSTIFY+3,TTLCENTR
         JNE   BLDFJUS2
         LH    R15,RTRECLEN
         AHI   R15,-5
         SH    R15,VDP1400_AREA_TOTAL_LENGTH
         SRL   R15,1
         A     R15,vdp1400_footer_AREA_OFFSET
         LA    R15,RTLEFT(R15)
*
BLDFJUS2 LTR   R1,R0
         JNP   BLDFHDRN
*
         BCTR  R1,0
         EX    R1,BLDRCPY         COPY TEXT
*
BLDFHDRN AH    R9,VDP1400_REC_LEN      ADVANCE TO NEXT VDP 1400 RECORD
         LG    R9,0(,R9)               Pointer to next VDP element
         LH    R0,VDP1400_RECORD_TYPE  STILL WITHIN 1400'S ???
         CHI   R0,1400
         JNE   BLDFEXIT                NO  - EXIT
         CLC   VWVIEW#,VDP1400_VIEWID  SAME  VIEW ???
         JE    BLDFHDRL                YES - LOOP
*
BLDFEXIT BR    R10                     RETURN
*
         DROP  R1
         DROP  R8
         DROP  R9
                     SPACE 5
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        CALCULATE ACTUAL TITLE LINE TEXT LENGTH     (SET R15)        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
CALCTLEN AR    R1,R15             LOAD ENDING ADDRESS (+1)
*
CALCTLP  BCTR  R1,0               BACKUP   TO PREVIOUS CHARACTER
         CLI   0(R1),C' '         TRAILING BLANK ???
         BNER  R14                NO - RETURN (NON-BLANK FOUND)
         BRCT  R15,CALCTLP
*
         BR    R14                RETURN (R15=0)
                     EJECT
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        B U I L D   C O L U M N   H E A D E R   L I N E S            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING COLDEFN,R6
         USING VIEWREC,R8
*
BLDHEADR DS    0H                 FORCE HALFWORD ALIGNMENT
*
         CLI   PRINTIND,C'Y'      USE PRINT AREA ???
         JE    BLDHEAD0           YES - CONTINUE
         J     BLDEXIT            NO  - RETURN
*
BLDHEAD0 EQU   *
         L     R14,CDSRTDEF       IS THIS COLUMN SORTED ???
         LTR   R14,R14
         JNP   BLDHA              NO  -   CONTINUE
*
         CLI   VWDESTYP+1,BATCH   HARDCOPY/BATCH  ???
         JNE   BLDHA              NO  -  CONTINUE
         CLI   SKDSPOPT-SORTKEY+1(R14),CATEGOR CATEGORIZE THIS COL ???
         JE    BLDEXIT            YES -  SKIP COLUMN HEADINGS
*
BLDHA    CLI   CDPRTIND,C'Y'      PRINT THIS COLUMN ???
         JNE   BLDEXIT            NO  - RETURN
*
         LH    R14,CDCOLOFF       COMPUTE DASH   LINE ADDRESS
         AH    R14,CDCOLGAP
         AH    R14,VWCENTER
         LA    R14,DASHTEXT(R14)
         LH    R15,CDCOLSIZ       LOAD  COLUMN  WIDTH (-1)
         Larl  R1,DASH
         EXrl  R15,BLDDASH
*
BLDH1    LA    R1,CDHEAD1         POINT  TO HEADER 1
         BRAS  R14,CALCHLEN       DETERMINE ACTUAL HEADER LENGTH (R15)
         LTR   R15,R15            CHECK LENGTH (ANY TEXT) ???
         JNP   BLDH2              NO  - TRY NEXT   HEADER LINE
         BCTR  R15,0              DECREMENT FOR "EX"
*
         LH    R14,CDCOLOFF       LOAD  DATA'S COLUMN   OFFSET
         AH    R14,CDCOLGAP       ADD   INTERCOLUMN GAP
         AH    R14,VWCENTER       ADD   CENTERING   ADJUSTMENT
         LA    R14,H1TEXT(R14)    ADD   BASE  ADDRESS
*
         LH    R1,CDCOLSIZ        LOAD  COLUMN  WIDTH (-1)
         SR    R1,R15             COMPUTE EXCESS ROOM WITHIN COLUMN
         JP    BLDH101            BYPASS  ADJUSTMENT  IF ALL FITS
         LH    R15,CDCOLSIZ       OTHERWISE TRUNCATE
         J     BLDMVCH1
*
BLDH101  EQU   *
         CLI   CDHDJUST,C'L'      LEFT  JUSTIFIED HEADER ???
         JE    BLDMVCH1           YES - MOVE TEXT
         CLI   CDHDJUST,C'R'      RIGHT JUSTIFIED HEADER ???
         JE    BLDH102            YES - BYPASS DIVISION
         SRL   R1,1               DIVIDE EXTRA ROOM IN HALF
BLDH102  EQU   *
         AR    R14,R1
*
BLDMVCH1 EXrl  R15,BLDMVC1        MOVE COLUMN HEADER TEXT TO PRINT LINE
                     EJECT
BLDH2    LA    R1,CDHEAD2         POINT  TO HEADER 2
         BRAS  R14,CALCHLEN       DETERMINE ACTUAL HEADER LENGTH (R15)
         LTR   R15,R15            CHECK LENGTH (ANY TEXT) ???
         JNP   BLDH3              NO  - TRY NEXT   HEADER LINE
         BCTR  R15,0              DECREMENT FOR "EX"
*
         LH    R14,CDCOLOFF       LOAD  DATA'S COLUMN   OFFSET
         AH    R14,CDCOLGAP       ADD   INTERCOLUMN GAP
         AH    R14,VWCENTER       ADD   CENTERING   ADJUSTMENT
         LA    R14,H2TEXT(R14)    ADD   BASE  ADDRESS
*
         LH    R1,CDCOLSIZ        LOAD  COLUMN  WIDTH
         SR    R1,R15             COMPUTE EXCESS ROOM WITHIN COLUMN
         JP    BLDH201            BYPASS  ADJUSTMENT  IF ALL FITS
         LH    R15,CDCOLSIZ       OTHERWISE TRUNCATE
         J     BLDMVCH2
*
BLDH201  EQU   *
         CLI   CDHDJUST,C'L'      LEFT  JUSTIFIED HEADER ???
         JE    BLDMVCH2           YES - MOVE TEXT
         CLI   CDHDJUST,C'R'      RIGHT JUSTIFIED HEADER ???
         JE    BLDH202            YES - BYPASS DIVISION
         SRL   R1,1               DIVIDE EXTRA ROOM IN HALF
BLDH202  EQU   *
         AR    R14,R1
*
BLDMVCH2 EXrl  R15,BLDMVC2        MOVE COLUMN HEADER TEXT TO PRINT LINE
                     SPACE 5
BLDH3    LA    R1,CDHEAD3         POINT  TO HEADER 3
         BRAS  R14,CALCHLEN       DETERMINE ACTUAL HEADER LENGTH (R15)
         LTR   R15,R15            CHECK LENGTH (ANY TEXT) ???
         JNP   BLDEXIT            NO  - TRY NEXT   HEADER LINE
         BCTR  R15,0              DECREMENT FOR "EX"
*
         LH    R14,CDCOLOFF       LOAD  DATA'S COLUMN   OFFSET
         AH    R14,CDCOLGAP       ADD   INTERCOLUMN GAP
         AH    R14,VWCENTER       ADD   CENTERING   ADJUSTMENT
         LA    R14,H3TEXT(R14)    ADD   BASE  ADDRESS
*
         LH    R1,CDCOLSIZ        LOAD  COLUMN  WIDTH
         SR    R1,R15             COMPUTE EXCESS ROOM WITHIN COLUMN
         JP    BLDH301            BYPASS  ADJUSTMENT  IF ALL FITS
         LH    R15,CDCOLSIZ       OTHERWISE TRUNCATE
         J     BLDMVCH3
*
BLDH301  EQU   *
         CLI   CDHDJUST,C'L'      LEFT  JUSTIFIED HEADER ???
         JE    BLDMVCH3           YES - MOVE TEXT
         CLI   CDHDJUST,C'R'      RIGHT JUSTIFIED HEADER ???
         JE    BLDH302            YES - BYPASS DIVISION
         SRL   R1,1               DIVIDE EXTRA ROOM IN HALF
BLDH302  EQU   *
         AR    R14,R1
*
BLDMVCH3 EXrl  R15,BLDMVC3        MOVE COLUMN HEADER TEXT TO PRINT LINE
*
BLDEXIT  BR    R10                RETURN
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        B U I L D   C O L U M N   H E A D E R   L I N E S (CSV)      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING COLDEFN,R6
         USING VIEWREC,R8
*
CSVHEADR DS    0H                 FORCE HALFWORD ALIGNMENT
         STM   R2,R10,SAVETKEY
         CLI   CDPRTIND,C'Y'      OUTPUT THIS COLUMN ???
         JNE   CSVEXIT2           NO  - RETURN
*
         L     R14,OUTPCURR
         CLI   COLMFRST,C'Y'      First actual column ?
         JNE   CSVH001
         MVI   COLMFRST,C' '      reset indicator
         MVI   0(R14),C'"'        open quotes
         LA    R14,1(,R14)
         J     CSVH002
CSVH001  EQU   *
         MVC   0(1,R14),VWFLDDEL  open delimiter
*        MVI   0(R14),C','        open quotes
         MVI   1(R14),C'"'        open quotes
         LA    R14,2(,R14)
CSVH002  EQU   *
         ST    R14,OUTPCURR
*
CSVH1    LA    R1,CDHEAD1         POINT  TO HEADER 1
         BRAS  R14,CALCHLEN       DETERMINE ACTUAL HEADER LENGTH (R15)
         LTR   R15,R15            CHECK LENGTH (ANY TEXT) ???
         JNP   CSVH2              NO  - TRY NEXT   HEADER LINE
         BCTR  R15,0              DECREMENT FOR "EX"
*
         L     R14,OUTPCURR
*
CSVH101  EQU   *
         EXrl  R15,BLDMVC1        MOVE COLUMN HEADER TEXT TO PRINT LINE
         LA    R14,1(R15,R14)
         ST    R14,OUTPCURR
*
CSVH2    LA    R1,CDHEAD2         POINT  TO HEADER 2
         BRAS  R14,CALCHLEN       DETERMINE ACTUAL HEADER LENGTH (R15)
         LTR   R15,R15            CHECK LENGTH (ANY TEXT) ???
         JNP   CSVH3              NO  - TRY NEXT   HEADER LINE
         BCTR  R15,0              DECREMENT FOR "EX"
*
         L     R14,OUTPCURR
         MVI   0(R14),C' '
         LA    R14,1(,R14)
*
CSVH201  EQU   *
         EXrl  R15,BLDMVC2        MOVE COLUMN HEADER TEXT TO PRINT LINE
         LA    R14,1(R15,R14)
         ST    R14,OUTPCURR
*
CSVH3    LA    R1,CDHEAD3         POINT  TO HEADER 3
         BRAS  R14,CALCHLEN       DETERMINE ACTUAL HEADER LENGTH (R15)
         LTR   R15,R15            CHECK LENGTH (ANY TEXT) ???
         JNP   CSVEXIT            NO  - TRY NEXT   HEADER LINE
         BCTR  R15,0              DECREMENT FOR "EX"
*
         L     R14,OUTPCURR
         MVI   0(R14),C' '
         LA    R14,1(,R14)
*
CSVH301  EQU   *
         EXrl  R15,BLDMVC3        MOVE COLUMN HEADER TEXT TO PRINT LINE
         LA    R14,1(R15,R14)
         ST    R14,OUTPCURR
*
CSVEXIT  EQU   *
         L     R14,OUTPCURR
         MVI   0(R14),C'"'        close quotes
         LA    R14,1(,R14)
         ST    R14,OUTPCURR
CSVEXIT2 EQU   *
         LM    R2,R10,SAVETKEY
         BR    R10                RETURN
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        CALCULATE ACTUAL COLUMN HEADING TEXT LENGTH (SET R15)        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
CALCHLEN LHI   R15,L'CDHEAD1      LOAD TOTAL  LENGTH
         AHI   R1,L'CDHEAD1       LOAD ENDING ADDRESS (+1)
*
CALCHLP  BCTR  R1,0               BACKUP   TO PREVIOUS CHARACTER
         CLI   0(R1),C' '         TRAILING BLANK ???
         BNER  R14                NO - RETURN (NON-BLANK FOUND)
         BRCT  R15,CALCHLP
*
         BR    R14                RETURN (R15=0)
*
BLDDASH  MVC   0(0,R14),0(R1)     * * * *   E X E C U T E D   * * * *
BLDMVC1  MVC   0(0,R14),CDHEAD1   * * * *   E X E C U T E D   * * * *
BLDMVC2  MVC   0(0,R14),CDHEAD2   * * * *   E X E C U T E D   * * * *
BLDMVC3  MVC   0(0,R14),CDHEAD3   * * * *   E X E C U T E D   * * * *
*
         DROP  R6
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P R I N T   R E P O R T   T I T L E   L I N E S              *
*                         (TOP-OF-PAGE)                               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING VIEWREC,R8
*
RPTPRNT  ST    R9,SAVERPTP               SAVE  RETURN  ADDRESS
         stm   r0,r5,SAVErptr            save (used in KEYBREAK)
*
         XC    VWLINENO,VWLINENO         RESET LINE    COUNT
*
         MVC   VWCURSEC,RPTPH            PAGE  HEADING SECTION
*
         CLI   PRINTIND,C'Y'             USE PRINT AREA ???
         JNE   RPTPEXIT                  NO  -  RETURN
*
         AP    VWPAGENO,P001               INCREMENT PAGE COUNT
         OI    VWPAGENO+L'VWPAGENO-1,X'0F'
*
         L     R9,RTITLEA
         USING RTITLE,R9
*
         LH    R0,VWMAXRPT               LOAD NUMBER OF TITLE LINES
         LTR   R0,R0
         JNP   RPTPEXIT
*
         icm   r5,b'1111',svpg#h_use_cnt  .get num of page number areas
         jz    rptploop                   .zero, then bypass
         la    r3,svpg#adr                .get start of page num areas
rpt_svpg#h_loop ds 0h
         l     r1,4(,r3)                  .get justification for this
         chi   r1,ttlright                .right justification?
         jne   rpt_svpg#h_njustl          .N: then not standard
*
         l     r15,0(,r3)                 .get address of area
         mvc   0(l'bldrmask,r15),bldrmask
         ed    0(l'bldrmask,r15),vwpageno
         j     rpt_svpg#h_loop_end
rpt_svpg#h_njustl ds 0h
         chi   r1,ttlleft                 .left justification?
         jne   rpt_svpg#h_njustc          .N: then must centre
*
         la    r1,dblwork2+7              .default significant byte
         mvc   dblwork2(l'bldrmask),bldrmask
         edmk  dblwork2(l'bldrmask),vwpageno
         la    r14,dblwork2               .start of work area
         ahi   r14,l'bldrmask-1           .get of workarea
         sr    r14,r1                     .length of page number
         l     r15,0(,r3)                 .get address of area
         ex    r14,just_move2             .move in page number
         j     rpt_svpg#h_loop_end
rpt_svpg#h_njustc ds 0h
*
         la    r1,dblwork2+7              .default significant byte
         mvc   dblwork2(l'bldrmask),bldrmask
         edmk  dblwork2(l'bldrmask),vwpageno
         la    r14,dblwork2               .start of work area
         lr    r4,r1                      .save start address
         sr    r4,r14                     .blanks to remove
         srl   r4,1                       .divide by to for centre just
         l     r15,0(,r3)                 .get address of area
         ar    r15,r4                     .make sure it is centred
         ahi   r14,l'bldrmask-1           .get of workarea
         sr    r14,r1                     .length of page number
         ex    r14,just_move2             .move in page number
rpt_svpg#h_loop_end ds 0h
         ahi   r3,8                       .next page number entry
         jct   r5,rpt_svpg#h_loop
*
RPTPLOOP ST    R0,PREVCNT
*
         LH    R0,RTRECLEN                   PRINT TITLE  LINE
         L     R1,PRNTDCBA
         STH   R0,DCBLRECL-IHADCB(,R1)
         LR    R0,R9
         L     R15,PRNTSUBR
         BASR  R14,R15
*
         AHI   R9,PRNTMAXL+4
         L     R0,PREVCNT
         BRCT  R0,RPTPLOOP
                     SPACE 3
         MVC   VWCURSEC,RPTBL                INDICATE  BLANK LINE
*
         LHI   R0,L'RPBLANK                  PRINT BLANK  LINE
         L     R1,PRNTDCBA
         STH   R0,DCBLRECL-IHADCB(R1)
         LA    R0,RPBLANK
         L     R15,PRNTSUBR
         BASR  R14,R15
*
RPTPEXIT L     R9,SAVERPTP                   RETURN
         lm    r0,r5,SAVErptr
         BR    R9
*
         DROP  R8
         DROP  R9
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P R I N T   R E P O R T   F O O T E R   L I N E S            *
*                       (BOTTOM-OF-PAGE)                              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING VIEWREC,R8
*
FOOTPRNT ST    R9,SAVERPTP        SAVE  RETURN ADDRESS
*
         CLI   PRINTIND,C'Y'      USE   PRINT  AREA ???
         JNE   FTRPEXIT           NO  - RETURN
*
         LH    R0,VWMAXFTR        LOAD  NUMBER OF TITLE LINES
         LTR   R0,R0
         JNP   FTRPEXIT
*
         L     R9,RFOOTRA
         USING RTITLE,R9
*
         MVC   VWCURSEC,RPTPF     PAGE  FOOTER  SECTION
*
         LH    R0,VWPAGSIZ        LOAD  PAGE    SIZE  ???
         SH    R0,VWLINENO        LOAD  CURRENT LINE  COUNT
         JP    FOOT1RNT
         XR    R0,R0
FOOT1RNT EQU   *
         AHI   R0,1
*
FTRPBLNK ST    R0,PREVCNT
*
         LHI   R0,L'RPBLANK       PRINT BLANK   LINE
         L     R1,PRNTDCBA
         STH   R0,DCBLRECL-IHADCB(R1)
         LA    R0,RPBLANK
         L     R15,PRNTSUBR
         BASR  R14,R15
*
         L     R0,PREVCNT
         BRCT  R0,FTRPBLNK
*
         LH    R0,VWMAXFTR        LOAD  NUMBER OF TITLE LINES
*
         icm   r5,b'1111',svpg#f_use_cnt  .get num of page number areas
         jz    ftrploop                   .zero, then bypass
         la    r3,svpg#ftr                .get start of page num areas
rpt_svpg#f_loop ds 0h
         l     r1,4(,r3)                  .get justification for this
         chi   r1,ttlright                .right justification?
         jne   rpt_svpg#f_njustl          .N: then not standard
*
         l     r15,0(,r3)                 .get address of area
         mvc   0(l'bldrmask,r15),bldrmask
         ed    0(l'bldrmask,r15),vwpageno
         j     rpt_svpg#f_loop_end
rpt_svpg#f_njustl ds 0h
         chi   r1,ttlleft                 .left justification?
         jne   rpt_svpg#f_njustc          .N: then must centre
*
         la    r1,dblwork2+7              .default significant byte
         mvc   dblwork2(l'bldrmask),bldrmask
         edmk  dblwork2(l'bldrmask),vwpageno
         la    r14,dblwork2               .start of work area
         ahi   r14,l'bldrmask-1           .get of workarea
         sr    r14,r1                     .length of page number
         l     r15,0(,r3)                 .get address of area
         ex    r14,just_move2             .move in page number
         j     rpt_svpg#f_loop_end
rpt_svpg#f_njustc ds 0h
*
         la    r1,dblwork2+7              .default significant byte
         mvc   dblwork2(l'bldrmask),bldrmask
         edmk  dblwork2(l'bldrmask),vwpageno
         la    r14,dblwork2               .start of work area
         lr    r4,r1                      .save start address
         sr    r4,r14                     .blanks to remove
         srl   r4,1                       .divide by to for centre just
         l     r15,0(,r3)                 .get address of area
         ar    r15,r4                     .make sure it is centred
         ahi   r14,l'bldrmask-1           .get of workarea
         sr    r14,r1                     .length of page number
         ex    r14,just_move2             .move in page number
rpt_svpg#f_loop_end ds 0h
         ahi   r3,8                       .next page number entry
         jct   r5,rpt_svpg#f_loop
*
FTRPLOOP ST    R0,PREVCNT
*
         LH    R0,RTRECLEN                   PRINT TITLE  LINE
         L     R1,PRNTDCBA
         STH   R0,DCBLRECL-IHADCB(,R1)
         LR    R0,R9
         L     R15,PRNTSUBR
         BASR  R14,R15
*
         AHI   R9,PRNTMAXL+4
         L     R0,PREVCNT
         BRCT  R0,FTRPLOOP
                             SPACE 3
FTRPEXIT L     R9,SAVERPTP                   RETURN
         BR    R9
*
         DROP  R8
         DROP  R9
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P R I N T   C O L U M N   H E A D I N G S                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING VIEWREC,R8
*
HDRPRNT  ST    R9,SAVEHDRP               SAVE RETURN  ADDRESS
*
         ZAP   DBLWORK,P000              INITIALIZE HEADER COUNTER
*
         CLI   PRINTIND,C'Y'             USE PRINT AREA ???
         JNE   HDRPEXIT                  NO - EXIT
*
         MVC   VWCURSEC,HDRPCH           INDICATE   COLUMN HEADER
*
         CLC   VWHEADLN,H001
         JL    HDRPEXIT
                     SPACE 3
         LH    R0,H1RECLEN               PRINT COLUMN HEADING LINE
         L     R1,PRNTDCBA
         STH   R0,DCBLRECL-IHADCB(R1)
         LA    R0,RPHEADR1
         L     R15,PRNTSUBR
         BASR  R14,R15
                     SPACE 3
         CLC   VWHEADLN,H002
         JL    HDRPDASH
*
         LH    R0,H2RECLEN               PRINT COLUMN HEADING LINE
         L     R1,PRNTDCBA
         STH   R0,DCBLRECL-IHADCB(R1)
         LA    R0,RPHEADR2
         L     R15,PRNTSUBR
         BASR  R14,R15
                     EJECT
         CLC   VWHEADLN,H003
         JL    HDRPDASH
*
         LH    R0,H3RECLEN               PRINT COLUMN HEADING LINE
         L     R1,PRNTDCBA
         STH   R0,DCBLRECL-IHADCB(R1)
         LA    R0,RPHEADR3
         L     R15,PRNTSUBR
         BASR  R14,R15
                     SPACE 3
HDRPDASH LH    R0,DASHLEN                PRINT DASH   LINE
         L     R1,PRNTDCBA
         STH   R0,DCBLRECL-IHADCB(R1)
         MVC   VWCURSEC,HDRPBD
         LA    R0,DASHLINE
         L     R15,PRNTSUBR
         BASR  R14,R15
                     SPACE 3
HDRPEXIT L     R9,SAVEHDRP        RETURN
         BR    R9
*
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P R I N T   S O R T   K E Y   T I T L E   L I N E S          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING VIEWREC,R8
*
TTLPRINT ST    R9,SAVETTLP        SAVE  RETURN  ADDRESS
*
         CLI   PRINTIND,C'Y'      USE   PRINT   AREA  ???
         JNE   TTLPEXIT           NO  - EXIT
*
         MVC   VWCURSEC,TTLPSH    SORT  HEADER  SECTION
*
         LH    R14,NEEDTITL       LOAD  NO. OF  BREAK    LEVELS
         LH    R15,VWLINENO       LOAD  CURRENT LINE     COUNT
         AR    R15,R14            ADD   NUMBER  OF LINES TO BE PRINTED
         CH    R15,VWPAGSIZ       CHECK AGAINST MAXIMUM  PAGE SIZE ???
         JNH   TTLPFITS           SKIP  PAGE    BREAK    IF   ROOM
*
         MVC   NEEDTITL,VWBRKCNT  SET   NO. OF  SORT  BREAK  LEVELS
         BRAS  R9,FOOTPRNT        PRINT PAGE FOOTERS
         Larl  R15,PAGEBRK        GOTO  PAGE    BREAK SUBROUTINE
         L     R9,SAVETTLP        RETURN  TO POINT OF CALL  (NOT HERE)
         BR    R15
*
TTLPFITS LTR   R14,R14            ANY   TITLE   LINES NEEDED ???
         JNP   TTLPEXIT           NO  - EXIT
*
         BCTR  R14,0              COMPUTE ADDRESS  OF HIGHEST LEVEL BRK
         LHI   R15,SKENTLEN
         MR    R14,R14
         L     R14,VWLOWSKY
         SR    R14,R15
         ST    R14,LSTSRTKY
         USING SORTKEY,R14
                     SPACE 3
TTLPLOOP CLI   SKHDRBRK+1,TITLEL1 DESCRIPTION IN   REPORT TITLE ???
         JL    TTLPOPT            NO  - CHECK OPTION
         CLI   SKHDRBRK+1,TITLEL5
         JNH   TTLPADV            YES - DON'T PRINT
*
TTLPOPT  CLI   SKHDRBRK+1,SUPPRESS SUPPRESS THIS    BREAK LEVEL ???
         JE    TTLPADV            YES -  ADVANCE TO  NEXT LEVEL
         CLI   SKDSPOPT+1,CATEGOR CATEGORIZED  SORT  KEY  ???
         JNE   TTLPADV            YES -  ADVANCE TO  NEXT LEVEL
*
         CLI   VWSUMTYP+1,DETAIL  DETAIL REPORT ???
         JE    TTLPCONT           YES -  BYPASS CHECK
         C     R14,VWLOWSKY       LOWEST LEVEL  SORT TITLE  ???
         JE    TTLPEXIT           YES -  DON'T PRINT LOWEST LEVEL
*
TTLPCONT LH    R0,SKTTLTOT        LOAD  TIT0E TEXT LENGTH
         SH    R0,SKPREFXL        EXCLUDE SUBTOTAL LABEL
         JNP   TTLPADV            BRANCH  IF  NOTHING TO    PRINT
         LR    R15,R0             COPY  ADJUSTED  LENGTH
         AHI   R15,4+1            ADD   RDW + CC  LENGTH
         STH   R15,TITLRDW
*
         MVI   TITLCC,C' '        CARRIAGE CONTROL
*
         LA    R1,TITLRDW         LOAD OUTPUT AREA ADDRESS
         LH    R15,SKINDCNT       LOAD INDENTATION COUNT
         LTR   R15,R15            ANY  LEADING SPACES ???
         JNP   TTLPLBL            NO - COPY TEXT
*
         BCTR  R15,0              DECREMENT FOR "EX"
         EX    R15,TTLPMVC        COPY LEADING SPACES
         AHI   R15,1              RESTORE
*
         SR    R0,R15             EXCLUDE SPACES ALREADY COPIED
         AR    R1,R15             ADVANCE OUTPUT ADDRESS
*
TTLPLBL  AH    R14,SKPREFXL       SKIP OVER SUBTOTAL LABEL
         AR    R14,R15            ADVANCE BEYOND INDENTATION
         LR    R15,R0             LOAD ADJUSTED LENGTH
         BCTR  R15,0
         EX    R15,TTLPMVC        MOVE REMAINDER OF TITLE
*
         LA    R0,TITLRDW
         L     R1,PRNTDCBA
         MVC   DCBLRECL-IHADCB(2,R1),TITLRDW
         L     R15,PRNTSUBR
         BASR  R14,R15
*
TTLPADV  L     R14,LSTSRTKY
         AHI   R14,SKENTLEN
         C     R14,VWLOWSKY
         JH    TTLPEXIT
         ST    R14,LSTSRTKY
         J     TTLPLOOP
*
TTLPEXIT XC    NEEDTITL,NEEDTITL  RESET  SORT TITLE LINES NEEDED
*
         L     R9,SAVETTLP        RETURN
         BR    R9
*
         DROP  R8
         DROP  R14
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "DELCHECK" - CHECK FOR DELETED VIEWS AND FLUSH OBSOLETE             *
*              MASTER FILE RECORDS.                                   *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R10 - RETURN   ADDRESS (IF VIEW STILL AVAILABLE)             *
*        R7  - CURRENT  MASTER  FILE  RECORD                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTREC,R7
*
DELCHECK ds    0h
*
         CLI   VDFOPENF,C'Y'      VDF  ALREADY  OPEN ???
         JE    DELCREAD           YES - BYPASS  OPEN
*
         LAY   R2,STATOPEN
         MVC   RENTPARM(STATOPENL),0(R2)
         L     R2,VDFACBA
         OPEN  ((R2),INPUT),MODE=31,MF=(E,RENTPARM) OPEN VDF DATASET
         CHI   R15,4              SUCCESSFUL OR WARNING  ???
         JH    DELCERR            NO - PERFORM  ERROR    PROCESSING
*
         MVI   VDFOPENF,C'Y'      SET  FLAG TO INDICATE VDF OPENED
*
DELCREAD L     R15,EXVIEW#-EXTREC(,R7) BUILD KEY
         CVD   R15,DBLWORK
         UNPK  VDFKEY,DBLWORK
         OI    VDFKEY+L'VDFKEY-1,X'F0'
*
         L     R2,VDFRPLA         READ DIRECT
         GET   RPL=(R2)
         LTR   R15,R15            SUCCESSFUL ???
         JZ    DELCEXIT           YES - RETAIN MASTER FILE RECORDS
*
         CHI   R15,8              RECORD NOT FOUND ???
         JNE   DELCERR            NO  -  INDICATE  I/O ERROR
         L     R14,VDFRPLA        LOAD  TEMPORARY BASE REGISTER
         USING IFGRPL,R14
         CLI   RPLFDB3,X'10'
         JNE   DELCERR
         DROP  R14
                     SPACE 3
DELCLOOP llgf  R15,READSUBR       LOAD GET  SUBROUTINE  ADDRESS(31-BIT)
         BASSM R10,R15            READ THE  NEXT    EXTRACT FILE RECORD
*                                (R7 - STILL POINTS TO PREVIOUS RECORD)
         L     R0,EXVIEW#-EXTREC(,R1)      STRIP HEADER  INDICATOR
         SRL   R0,1
         ST    R0,EXVIEW#-EXTREC(,R1)
*
         CLI   LASTFILE,C'M'      RECORD CAME  FROM MASTER  FILE ???
         JNE   NEXTEXT            NO  -  CHECK IF   VALID   HEADER
*
         CLC   EXVIEW#-EXTREC(4,R1),EXVIEW#-EXTREC(R7) SAME REQUEST ???
         JNE   NEXTEXT            NO  -  CHECK IF   VALID   HEADER
         LR    R7,R1
         J     DELCLOOP           YES - CONTINUE DELETE
                     SPACE 3
DELCEXIT BSM   0,R10              RETURN (VIEW STILL AVAILABLE)
*
DELCERR  BSM   0,R10              VSAM  ERROR (KEEP MASTER RECORDS)
*
static   loctr
DELCH4   DC    H'4'
DELCH8   DC    H'8'
code     loctr
         IFGRPL AM=VSAM
*
         DROP  R7
GVBMR88  CSECT
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P R I N T   T H E   C O N T R O L   R E P O R T              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*
CTRLRPT  ds    0h
*
         L     R14,PARMDADR         CHECK PARAMETER OPTION
         CLI   NOHDROPT-PARMDATA(R14),C'Y'   HEADER RECORDS PRESENT ???
         JNE   CTRLINIT             YES -    BYPASS SPECIAL LOGIC
*
         SP    VIEWCNT,P001   *       X FOR CONTROL REPORT
         AP    EXTRCNT,P001   *       D BACK SKIPPED CONTROL RECORD
*
CTRLINIT DS    0H
**********************************************************************
*   ~ISRC - Input source file summary
**********************************************************************
         phead hd=isrc               Write ISRC header
         rptit msg=rptisr8_hd1       column headings
         rptit msg=rptisr8_hd2       column  underlining
*
rp2      using isrcrept,prntline
         lhi   r0,isrcrept_len+4
         sth   r0,prntrdwh           Set length in VB record
         mvc   rp2.isrcf1,spaces
*
         L     R14,PARMDADR         reload R14 - wipred out by rptit
         if    (cli,extropt-PARMDATA(R14),eq,c'N')
           mvc   rp2.isrcddn,MR88HXE
         else
           mvc   rp2.isrcddn,SORTIN
         endif
*
         MVC   rp2.isrcrcnt,PATTERN1
         ED    rp2.isrcrcnt,EXTRCNT
         rptit ,
*
         rptit msg=rptisr8_hd3       column  underlining
*
         mvc   rp2.isrcddn,TOTAL
         mvc   tempwork(6),extrcnt
         ap    tempwork(6),mstrcnt    calculate total
         MVC   rp2.isrcrcnt,PATTERN1
         ED    rp2.isrcrcnt,tempwork
         rptit ,
         rptit msg=rptisr8_hd4       total  underlining
         drop   rp2
*
         rptit msg=vb_blankl      blank line
*
**********************************************************************
*   ~OFIL - Output file summary
**********************************************************************
         phead hd=ofil               Write OFIL header
         rptit msg=rptofil_hd1       column headings
         rptit msg=rptofil_hd2       column  underlining
*
rp2      using ofilrept,prntline
         lhi   r0,ofilrept_len+4
         sth   r0,prntrdwh           Set length in VB record
         mvc   rp2.ofilf1,spaces
         mvc   rp2.ofilf2,spaces
         mvc   rp2.ofilf3,spaces
*
* Loop through View chain                              
* Check which Views were processed and get view counts 
*                                                      
         ZAP   VIEWCNT,P000                    
*
         L     R8,VWCHAIN
         USING VIEWREC,R8
*
CTRLVIEW LTR   R8,R8              END-OF-CHAIN ???
         JNP   CTRLvext
         CLI   VWPROC,C'P'        Was this view processed? 
         JNE   CTRLvnxt                                    
         AP    VIEWCNT,P001                                  
*
         CP    VWOUTCNT,P000  Any output recs written?
         JNH   CTRLvnxt
* counts for later
         AP    VWoutfcnt,P001     Add one to file count
         AP    VWoutftot,VWOUTCNT Add to total rec count
* write list of output files
         mvc   rp2.ofilddn,VWDDNAME
*20729
         MVC   rp2.ofilcnt,PATTERN1
         ED    rp2.ofilcnt,VWOUTCNT
*
         L     R0,VWVIEW#
         CVD   R0,DBLWORK
         mvc   dblwork2,view#msk
         ed    dblwork2,dblwork+4     Move in the view id
         mvc   rp2.ofilvid,dblwork2+1
*
         mvc   rp2.ofilname,VWTITLE
         rptit ,
*
CTRLvnxt L     R8,VWNEXT
         J     CTRLVIEW
*
CTRLvext ds    0h
* write totals
         rptit msg=rptisr8_hd3       column  underlining
*
         mvc   rp2.ofilddn,TOTAL
         MVC   rp2.ofilcnt,PATTERN1
         ED    rp2.ofilcnt,VWoutftot
         rptit ,
         rptit msg=rptisr8_hd4       total  underlining
         drop   rp2
*
         rptit msg=vb_blankl      blank line
*
**********************************************************************
*   ~EXEC - Execution summary
**********************************************************************
         phead hd=exec               Write ISRC header
*
rp2      using execrept,prntline
         lhi   r0,execrept_len+4
         sth   r0,prntrdwh           Set length in VB record
         mvc   rp2.exectext,exectxt1 SAFR view processed
         MVC   rp2.execcnt,PATTERN1
         ED    rp2.execcnt,VIEWCNT
         rptit ,
*
         mvc   rp2.exectext,exectxt2 Lookups performed
         ap    LKUPcnt,LKUPfnd
         ap    LKUPcnt,LKUPnot
         MVC   rp2.execcnt,PATTERN1
         ED    rp2.execcnt,LKUPcnt
         rptit ,
         rptit msg=vb_blankl      blank line
*
         lhi   r0,execrept_len+4
         sth   r0,prntrdwh           Set length in VB record
         mvc   rp2.exectext,exectxt3 extract records read
         MVC   rp2.execcnt,PATTERN1
         ED    rp2.execcnt,EXTRCNT
         rptit ,
*
         rptit msg=vb_blankl      blank line
*
         lhi   r0,execrept_len+4
         sth   r0,prntrdwh           Set length in VB record
         mvc   rp2.exectext,exectxt5 view files written
         MVC   rp2.execcnt,PATTERN1
         ED    rp2.execcnt,VWoutfcnt
         rptit ,
*
         mvc   rp2.exectext,exectxt6 view records written
         MVC   rp2.execcnt,PATTERN1
         ED    rp2.execcnt,VWoutftot
         rptit ,
         rptit msg=vb_blankl      blank line
*
* The elapsed time convertion assumes that it is < 24 hours
*
*       Elapsed time (HHH:MM:SS.th)
*
         lg    R2,begtime
         lg    R3,endtime
         SGR   R3,R2                 Work out elapsed time
         STG   R3,begtime            Save elapsed time
*
         basr  r12,0
         using *,r12
*
         STCKCONV STCKVAL=begtime,CONVVAL=tempwork,TIMETYPE=DEC,       +
               DATETYPE=YYYYDDD,MF=(E,stckconveform)
         MVC   tempwork+4(4),tempwork     .Make it date
         LHI   R2,4095                    .X'0FFF'
         ST    R2,tempwork                .init the "to" area
         NC    tempwork(4),tempwork+8     .and the date, remove year
         IC    R3,tempwork+4              .save 1st byte of time
         MVI   tempwork+4,X'0F'           .make date packed decimal
         SP    tempwork(5),P010           .decrement 10 (really 1)
         STC   R3,tempwork+4              .restore 1st byte of time
*
rp2      using execrept,prntline
         lhi   r0,execrept_len+4
         sth   r0,prntrdwh           Set length in VB record
         mvc   rp2.exectext,exectxt9 Elapsed time
         MVC   rp2.execcnt(L'PATTERN3),PATTERN3
         ED    rp2.execcnt(L'PATTERN3),TEMPWORK+4
         rptit ,
*
*       Elapsed time in seconds
*
         STCKCONV STCKVAL=begtime,CONVVAL=tempwork,TIMETYPE=BIN,       +
               DATETYPE=YYYYDDD,MF=(E,stckconveform)
*
         mvc   rp2.exectext,exectxt8 Elapsed time (seconds)
*
         l     r0,tempwork
         CVD   R0,dblwork
         MVC   rp2.execcnt,PATTERN4
         ED    rp2.execcnt,DBLWORK+2
         rptit ,
         drop  rp2,r12
*
         rptit msg=vb_blankl      blank line
*
CTRLEXIT BR    R10                RETURN
                     SPACE 5
static   loctr
P010     DC    P'10'
*              PIC    Z Z , Z Z Z , Z Z Z , Z 9 9
PATTERN1 DC    XL15'4020206B2020206B2020206B202120'
*              PIC    Y Y Y Y / M M / D D       H H : M M : S S . T T
PATTERN2 DC    XL25'402020202061202061202140404020207A20207A20204B2020'
*              PIC          H H : M M : S S . T T
PATTERN3 DC    XL15'4040404021207A20207A20204B2020'
*              PIC    S S S , S S S , S S S . T T
PATTERN4 DC    XL15'402020206B2020206B2021204B2020'
         ASSERT l'execcnt,eq,l'pattern3
         ASSERT l'sorttime,eq,l'pattern2
VIEW#MSK DC    XL08'4020202020202120'
*
equals   dc    cl80'===================================================x
               ============================='
*
vb_blankl dc   0cl(vb_ble-vb_bls)
vb_bls    dc   al2(vb_ble-vb_bls),al2(0)
          dc   c' '
vb_ble    equ  *
*
sorttxt1 DC    CL32'Start time '
sorttxt2 DC    CL32'Time first record was delivered'
sorttxt3 DC    CL32'End time '
sorttxt4 DC    CL32'Elapsed time to first record '
sorttxt5 DC    CL32'Total elapsed time '
*
exectxt1 DC    CL27'SAFR views processed:'
exectxt2 dc    cl27'Lookups performed:'
exectxt3 DC    CL27'Extract records read:'
exectxt4 DC    CL27'Extract bytes read:'
exectxt5 dc    cl27'View files written:'
exectxt6 dc    cl27'View records written:'
exectxt7 dc    cl27'View bytes written:'
exectxt8 dc    cl27'Elapsed time (seconds):'
exectxt9 dc    cl27'Elapsed time (HH:MM:SS.hh):'
*
MR88HXE  dc    cl8'MR88HXE '
SORTIN   dc    cl8'SORTIN  '
TOTAL    dc    cl8'Total   '
*
code     loctr
*
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C L O S E   T H E   F I L E S                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
CLOSFILE ds    0h
*
         LAY   R6,STATCLOS
         MVC   RENTPARM(STATCLOSL),0(R6)
*
***********************************************************************
*  CLOSE TITLE LOOK-UP FILES                                          *
***********************************************************************
         L     R6,LBCHAIN+(LBNEXT-LKUPBUFR)
         USING LKUPBUFR,R6
         J     CLEND
*
CLLOOP   TM    LBFLAGS,LBMEMRES   MEMORY RESIDENT TABLE ???
         JO    CLNEXT
*
         MVC   PADDNAME,LBDDNAME  INITIALIZE DDNAME
         MVI   PAFTYPE,C'V'       INITIALIZE FILE     TYPE
         MVC   PAFMODE,I          INITIALIZE FILE     MODE
         MVC   PAFUNC,CL          INITIALIZE FUNCTION CODE
*
         LA    R1,TP90LIST        POINT R1 TO PARAMETER LIST
         llgf  R15,GVBTP90A       LOAD  ADDRESS OF "GVBTP90"
         bassm R14,R15            CALL  I/O ROUTINE
*
CLNEXT   L     R6,LBNEXT          ADVANCE  TO NEXT BUFFER ON THE CHAIN
CLEND    LTR   R6,R6              ANY MORE BUFFERS ???
         JP    CLLOOP             YES - CONTINUE  LOOPING
                     SPACE 3
***********************************************************************
*  CLOSE INPUT EXTRACT FILE                                           *
***********************************************************************
         MVI   PRNTLINE,C' '      BLANK OUT   PRINT   LINE
         MVC   PRNTLINE+01(L'PRNTLINE-1),PRNTLINE+00
*
         MVI   PRNTCC,C'0'        DOUBLESPACE FIRST   LINE
*
         L     R2,EXTRDCBA        CLOSE EXTRACT INPUT FILE
         TM    48(R2),X'10'       PREVIOUSLY  OPENED  ???
         JNO   CLOSVIEW           NO  - SKIP  CLOSE
         CLOSE ((R2)),MODE=31,MF=(E,RENTPARM)
                     EJECT
***********************************************************************
*  CLOSE VIEW OUTPUT FILES                                            *
***********************************************************************
CLOSVIEW L     R8,VWCHAIN         LOAD  FIRST REQUEST ADDR
*
CLOSSRCH LTR   R8,R8              END-OF-LIST ???
         JNP   CLOSPRNT           YES - CONTINUE
*
         USING VIEWREC,R8
         L     R2,VWDCBADR        CLOSE DATA OUTPUT FILE
         LTR   R2,R2
         JNP   CLOSNEXT
         TM    48(R2),X'10'       PREVIOUSLY OPENED ???
         JNO   CLOSNEXT           NO  - SKIP CLOSE
         CLOSE ((R2)),MODE=31,MF=(E,RENTPARM)
*
CLOSNEXT L     R8,VWNEXT          LOAD  ADDRESS OF NEXT LIST ENTRY
         J     CLOSSRCH           LOOP  THROUGH ENTIRE  LIST
         DROP  R8
                     EJECT
***********************************************************************
CLOSPRNT ds    0h
*
***********************************************************************
*  CLOSE CONTROL REPORT FILE                                          *
***********************************************************************
*
CLOSCTRL L     R2,CTRLDCBA        CLOSE CONTROL  REPORT    FILE
         CLOSE ((R2)),MODE=31,MF=(E,RENTPARM)
*
*        CLOSE SNAPDCB,MODE=31    CLOSE SNAP     DATASET
*
         BSM   0,R10              RETURN
         DROP  R6
         EJECT
         USING VIEWREC,R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N V E R S I O N   R O U T I N E S (COLUMN FORMATTING)    *
*                                                                     *
*        R15:  ENTRY  POINT ADDRESS                                   *
*        R14:  OUTPUT AREA  ADDRESS                                   *
*        R9:   RETURN       ADDRESS                                   *
*        R1:   OUTPUT AREA  LENGTH                                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
* DFP changes - this chunk of code is used with slight variations
* is used to take the DFP result into a different format
*
*
*        ZAP   DBLWORK,SAOUTDEC   COMPUTE  DECIMAL SHIFT  LENGTH
*        SP    DBLWORK,SAVALDEC
*        SP    DBLWORK,SAVALRND
*        CVB   R14,DBLWORK        now r14 has decimal shift
*
*        GVBLDX  FP4,ACCUMWRK       get the value into fp4/6
*              this just loads the accumulator into the floating point
*              register pair fp4/fp6
*              points to note - FP pairs are 0/2, 1/3, 4/6 etc
*                   SAFR uses extended DFP so always use pairs
*                   GVBLDX(and GVBSTX) are macros to load and 
*                   store fp pairs
*
*          r14 has the shift value from above and we need to calculate
*          the exponent we need in the DFP format.
*          the value 6176 is the biased zero exponent in extended DFP
*          so we subtract the shift value from this to get our exponent
*        LCR   R14,R14            invert
*        AHI   R14,6176           add in bias
*          now r14 will have the correct exponent for the number of
*          decimal places
*        IEXTR FP8,FP8,R14        insert in the saved zero
*          fp8/10 always has zero in it - this adjusts the dp posie
*        QAXTR FP0,FP4,FP8,0      qantize the value (align dp)
*          this is the instruction that takes the value in fp4/6 and
*          rounds it to the dp in fp8/10, storing the result in fp0/2
*          After this, the value in fp0/2 will be the rounded result
*          that we need for display
*        CSXTR R14,FP0,0          and convert to packed in r14/15
*          this converts fp0/2 to packed in grande pair r14/r15
*        STMG  R14,R15,TEMPWORK   and save in tempry area
*          this stores the packed value into tempwork
*          note - this is a 16 byte packed (31 digits + sign)
*                 the masks used here only handle 12 byte (23 + sign)
*                 so tempwork+4 is often used
*
         USING COLDEFN,R6
COLBPTOP LH    R14,SAOUTDEC       COMPUTE  DECIMAL SHIFT  LENGTH
         SH    R14,SAVALDEC
         SH    R14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in the bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
         L     R14,DL96TGTA       LOAD      TARGET ADDRESS
         LH    R1,DL96LEN         LOAD      TARGET LENGTH
         BCTR  R1,0               DECREMENT OUTPUT LENGTH FOR "EX"
         SLL   R1,4               SHIFT     OUTPUT LENGTH TO  "L1"  POS
         EX    R1,COLBZAP         MOVE      VALUE
         SRL   R1,4               SHIFT     OUTPUT LENGTH BACK
         EX    R1,COLBRZAP                  REZAP  VALUE
         CP    tempwork2(AccumDFPl),TEMPWORK(AccumDFPl) ANY DIGIT LOST?
         JNE   COLBOVER
         BR    R10                RETURN
                     SPACE 3
COLBPTOF LH    r14,SAOUTDEC       COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in the bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,1          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
         L     R14,DL96TGTA       LOAD      TARGET ADDRESS
         LH    R1,DL96LEN         LOAD      TARGET LENGTH
         BCTR  R1,0               DECREMENT OUTPUT LENGTH FOR "EX"
         SLL   R1,4               SHIFT     OUTPUT LENGTH TO  "L1" POS
         EX    R1,COLBZAP         MOVE      VALUE
         SRL   R1,4               SHIFT     OUTPUT LENGTH BACK
         EX    R1,COLBRZAP                  REZAP  VALUE
         CP    tempwork2(AccumDFPl),TEMPWORK(AccumDFPl) ANY DIGIT LOST?
         JNE   COLBOVER
         LA    R14,0(R14,R1)      LOAD  ADDRESS  OF SIGN
         OI    0(R14),X'0F'       FORCE SIGN TO "F"
         BR    R10                RETURN
                     SPACE 3
COLBPTON ds    0h
         LH    r14,SAOUTDEC       COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in the bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
         L     R14,DL96TGTA       LOAD      TARGET ADDRESS
         LH    R1,DL96LEN         LOAD      TARGET LENGTH
         BCTR  R1,0               DECREMENT OUTPUT LENGTH FOR "EX"
         SLL   R1,4               SHIFT     OUTPUT LENGTH TO  "L1"  POS
         EX    R1,COLBUNPK        MOVE      VALUE
         SRL   R1,4               SHIFT     OUTPUT LENGTH BACK
         EX    R1,COLBPACK                  REPACK VALUE
         CLC   tempwork2,TEMPWORK WERE   ANY  DIGITS LOST ???
         JNE   COLBOVER
         BR    R10                RETURN
                     SPACE 3
COLBPTOU ds    0h
         LH    r14,SAOUTDEC       COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in the bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         lpdfr fp0,fp0            force positive
         csxtr r14,fp0,1          and convert to packed in r14/15      +
                   the mask of 1 makes the sign F
         stmg  r14,r15,tempwork   and save in tempry area
         L     R14,DL96TGTA       LOAD      TARGET ADDRESS
         LH    R1,DL96LEN         LOAD      TARGET LENGTH
         BCTR  R1,0               DECREMENT OUTPUT LENGTH FOR "EX"
         SLL   R1,4               SHIFT     OUTPUT LENGTH TO  "L1" POS
         EX    R1,COLBUNPK        MOVE      VALUE
         SRL   R1,4               SHIFT     OUTPUT LENGTH BACK
         EX    R1,COLBPACK                  REPACK VALUE
         CLC   tempwork2,TEMPWORK WERE   ANY  DIGITS LOST ???
         JNE   COLBOVER
         BR    R10                RETURN
                     SPACE 3
COLBPTOB DS    0h
         LH    r14,SAOUTDEC       COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
*
*        so r14 has the number of dp we want in the output now
*
*        so test for non zero first
         GVBLDX fp13,=ld'+10'           put a +10 in fp13/15
         ltr   r15,r14            set the cc
         if (nz)                  only do this if not zero
           if (p)                 if positive
             do from=(r15)
               mxtr fp4,fp4,fp13    multiply the value by 10
             enddo
           else
             lpr r15,r14          and make the loop control +ve
             do from=(r15)
               dxtr fp4,fp4,fp13    divide the value by 10
             enddo
           endif
         endif
         lcr   r14,r14            invert
         ahi   r14,6176           add in the bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         cgxtr r0,0,fp0           and convert to binary in r0
*
         L     R14,DL96TGTA       LOAD    TARGET    ADDRESS
         LH    R1,DL96LEN         LOAD    TARGET    LENGTH
         select chi,r1,eq         set up a select
           when  2                BINARY  HALFWORD  ???
             if CHI,R0,gt,32767,or,    MAX     VALUE                   +
               chi,r0,lt,-32768        Minimum value

               J COLBOVER         signal overflow
             endif
             stH R0,0(,R14)       STORE   HALFWORD
           when  4                BINARY  FULLWORD  ???
             ST R0,0(,R14)        STORE   FULLWORD
           when  8                BINARY  DOUBLEWORD ??
             STG R0,0(,R14)       STORE   DOUBLEWORD
           when  (0,1,3)
             SLL R1,1             MULTIPLY  BY  2   TO INDEX TABLE
             LH R1,COLBBINM(R1)   LOAD    MASK  FOR "STCM"
             EX R1,COLBSTCM
           othrwise
             J COLBOVER           signal overflow
         endsel
         BR    R10                RETURN
static   loctr
         ltorg
code     loctr
                     SPACE 3
COLBMSK1 LR    R0,R2              SAVE    REGISTER   R2
         xr    r14,r14            COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASK1),STDMASK1 COPY MASK
         ED    SAWORKAR(L'STDMASK1),tempwork+4                         +
                        edit packed val using mask
*
         LA    R14,SAWORKAR+L'STDMASK1   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASK1-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCZER        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBM1CP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH (-1)
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SK1           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SK1 EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBM1CP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSK2 LR    R0,R2              SAVE    REGISTER   R2
         xr    r14,r14            COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASK2),STDMASK2 COPY MASK
         ED    SAWORKAR(L'STDMASK2),tempwork+4                         +
                        edit packed val using mask
         CLI   SAWORKAR+L'STDMASK2-1,C'-'    MINUS SIGN IN LAST BYTE ?
         JE    COLB2SK1                      YES - SKIP ADJUSTMENT
         MVI   SAWORKAR+L'STDMASK2-1,C' '    NO  - BLANK OUT LAST BYTE
*
COLB2SK1 EQU   *
         LA    R14,SAWORKAR+L'STDMASK2   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASK2-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCZER        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBM2CP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH (-1)
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB3SK1           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB3SK1 EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBM2CP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSK3 LR    R0,R2              SAVE    REGISTER   R2
*
         Lhi   R14,2              COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASK3),STDMASK3 COPY MASK
         ED    SAWORKAR(L'STDMASK3),tempwork+4                         +
                        edit packed val using mask
*
         LA    R14,SAWORKAR+L'STDMASK3   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASK3-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBM3CP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB4SK1           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB4SK1 EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBM3CP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSK4 LR    R0,R2              SAVE    REGISTER   R2
         xr    R14,r14            COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASK4),STDMASK4 COPY MASK
         ED    SAWORKAR(L'STDMASK4),tempwork+4                         +
                        edit packed val using mask
*
         LA    R14,SAWORKAR+L'STDMASK4   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASK4-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBM4CP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB5SK1           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB5SK1 EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBM4CP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSK5 LR    R0,R2              SAVE    REGISTER   R2
         xr    R14,r14            COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASK5),STDMASK5 COPY MASK
         ED    SAWORKAR(L'STDMASK5),tempwork+4                         +
                        edit packed val using mask
*
         LA    R14,SAWORKAR+L'STDMASK5   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASK5-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBM5CP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB6SK1           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB6SK1 EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBM5CP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSK6 LR    R0,R2              SAVE    REGISTER   R2
         lhi   R14,1              COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASK6),STDMASK6 COPY MASK
         ED    SAWORKAR(L'STDMASK6),tempwork+4                         +
                        edit packed val using mask
*
         LA    R14,SAWORKAR+L'STDMASK6   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASK6-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBM6CP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB7SK1           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB7SK1 EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBM6CP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSK7 LR    R0,R2              SAVE    REGISTER   R2
         lhi   R14,1              COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASK7),STDMASK7 COPY MASK
         ED    SAWORKAR(L'STDMASK7),tempwork+4                         +
                        edit packed val using mask
*
         LA    R14,SAWORKAR+L'STDMASK7   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASK7-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBM7CP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB8SK1           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB8SK1 EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBM7CP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSK8 LR    R0,R2              SAVE    REGISTER   R2
         lhi   R14,2              COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASK8),STDMASK8 COPY MASK
         ED    SAWORKAR(L'STDMASK8),tempwork+4                         +
                        edit packed val using mask
*
         LA    R14,SAWORKAR+L'STDMASK8   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASK8-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBM8CP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SK8           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SK8 EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBM8CP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSK9 LR    R0,R2              SAVE    REGISTER   R2
         lhi   R14,2              COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASK9),STDMASK9 COPY MASK
         ED    SAWORKAR(L'STDMASK9),tempwork+4                         +
                        edit packed val using mask
*
         LA    R14,SAWORKAR+L'STDMASK9   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASK9-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBM9CP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SK9           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SK9 EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBM9CP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSKA LR    R0,R2              SAVE    REGISTER   R2
         xr    R14,r14            COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASKA),STDMASKA COPY MASK
         ED    SAWORKAR(L'STDMASKA),tempwork+4                         +
                        edit packed val using mask
*
         LA    R14,SAWORKAR+L'STDMASKA   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASKA-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBMACP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SKA           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SKA EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBMACP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSKB LR    R0,R2              SAVE    REGISTER   R2
         xr    R14,r14            COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASKB),STDMASKB COPY MASK
         LA    R1,SAWORKAR+L'STDMASKB   LOAD MINIMUM  DATA ADDRESS
         SH    R1,CDMSKLEN
         EDMK  SAWORKAR(L'STDMASKB),tempwork+4                         +
                        edit packed val using mask
*
         LH    R2,CDMSKLEN               LOAD   MASK  LENGTH
*
         LHI   R14,L'STDMASKB-1   CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R14,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R14,COLBCZER       ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBMBSN           NO  -  SET SIGN
*
         LHI   R14,L'VWOVRFIL-1   YES -  PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN  WIDTH (-1)
         CR    R2,R14             COLUMN WIDER   THAN  FILL  ???
         JNH   COLB1SKB           NO  -  BYPASS  ADJUSTMENT
         LR    R2,R14             YES -  USE FILL  LENGTH
COLB1SKB EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
         J     COLBMBCP           COPY   OVERFLOW  FILL TO   COLUMN
*
COLBMBSN AHI   R1,-3                         BACKUP ONE BYTE
         MVC   0(3,R1),MASKNEG               COPY NEGATIVE   SIGN  MASK
         ED    0(3,R1),tempwork+L'tempwork-1
         MVI   1(R1),C' '
*
         LA    R14,SAWORKAR+L'STDMASKB-1 LOAD ENDING ADDRESS OF DATA
         SR    R14,R2                    BACKUP TO BEGINNING
*
COLBMBCP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSKC LR    R0,R2              SAVE    REGISTER   R2
*
         lhi   R14,2              COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASKC),STDMASKC COPY MASK
         LA    R1,SAWORKAR+L'STDMASKC-4 LOAD MINIMUM  DATA ADDRESS
         EDMK  SAWORKAR(L'STDMASKC),tempwork+4                         +
                        edit packed val using mask
         AHI   R1,-3                         BACKUP ONE BYTE
         MVC   0(3,R1),MASKNEG               COPY NEGATIVE   SIGN  MASK
         ED    0(3,R1),tempwork+L'tempwork-1
         MVI   1(R1),C' '
*
         LA    R14,SAWORKAR+L'STDMASKC   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASKC-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         JM    COLBMCCP
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBMCCP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SKC           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SKC EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBMCCP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSKD LR    R0,R2              SAVE    REGISTER   R2
         xr    R14,r14            COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASKD),STDMASKD COPY MASK
         LA    R1,SAWORKAR+L'STDMASKD-1 LOAD MINIMUM  DATA ADDRESS
         EDMK  SAWORKAR(L'STDMASKD),tempwork+4                         +
                        edit packed val using mask
         AHI   R1,-3                         BACKUP ONE BYTE
         MVC   0(3,R1),MASKNEG               COPY NEGATIVE   SIGN  MASK
         ED    0(3,R1),tempwork+L'tempwork-1
         MVI   1(R1),C' '
*
         LA    R14,SAWORKAR+L'STDMASKD   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASKD-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         JM    COLBMDCP
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBMDCP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SKD           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SKD EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBMDCP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSKE LR    R0,R2              SAVE    REGISTER   R2
         lhi   R14,1              COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASKE),STDMASKE COPY MASK
         LA    R1,SAWORKAR+L'STDMASKE-3 LOAD MINIMUM  DATA ADDRESS
         EDMK  SAWORKAR(L'STDMASKE),tempwork+4                         +
                        edit packed val using mask
         AHI   R1,-3                         BACKUP ONE BYTE
         MVC   0(3,R1),MASKNEG               COPY NEGATIVE   SIGN  MASK
         ED    0(3,R1),tempwork+L'tempwork-1
         MVI   1(R1),C' '
*
         LA    R14,SAWORKAR+L'STDMASKE   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASKE-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         JM    COLBMECP
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBMECP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SKE           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SKE EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBMECP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSKF LR    R0,R2              SAVE    REGISTER   R2
         xr    R14,r14            COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASKF),STDMASKF COPY MASK
         LA    R1,SAWORKAR+L'STDMASKF-2 LOAD MINIMUM  DATA ADDRESS
         EDMK  SAWORKAR(L'STDMASKF),tempwork+4                         +
                        edit packed val using mask
         AHI   R1,-3                         BACKUP ONE BYTE
         MVC   0(3,R1),MASKPARN              COPY NEGATIVE   SIGN  MASK
         ED    0(3,R1),tempwork+L'tempwork-1
         MVI   1(R1),C' '
*
         LA    R14,SAWORKAR+L'STDMASKF   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASKF-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBMFCP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SKF           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SKF EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBMFCP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSKG LR    R0,R2              SAVE    REGISTER   R2
*
         lhi   R14,2              COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASKG),STDMASKG COPY MASK
         LA    R1,SAWORKAR+L'STDMASKG-4 LOAD MINIMUM  DATA ADDRESS
         EDMK  SAWORKAR(L'STDMASKG),tempwork+4                         +
                        edit packed val using mask
         AHI   R1,-3                         BACKUP ONE BYTE
         MVC   0(3,R1),MASKPARN              COPY NEGATIVE   SIGN  MASK
         ED    0(3,R1),tempwork+L'tempwork-1
         MVI   1(R1),C' '
*
         LA    R14,SAWORKAR+L'STDMASKG   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASKG-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBMGCP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SKG           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SKG EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBMGCP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSKH LR    R0,R2              SAVE    REGISTER   R2
*
         lhi   R14,2              COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASKH),STDMASKH COPY MASK
         LA    R1,SAWORKAR+L'STDMASKH-5 LOAD MINIMUM  DATA ADDRESS
         EDMK  SAWORKAR(L'STDMASKH),tempwork+4                         +
                        edit packed val using mask
         AHI   R1,-3                         BACKUP ONE BYTE
         MVC   0(3,R1),MASKPARN              COPY NEGATIVE   SIGN  MASK
         ED    0(3,R1),tempwork+L'tempwork-1
         MVI   1(R1),C' '
*
         LA    R14,SAWORKAR+L'STDMASKH   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASKH-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBMHCP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SKH           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SKH EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBMHCP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSKI LR    R0,R2              SAVE    REGISTER   R2
*
         lhi   R14,2              COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASKI),STDMASKI COPY MASK
         LA    R1,SAWORKAR+L'STDMASKI-6 LOAD MINIMUM  DATA ADDRESS
         EDMK  SAWORKAR(L'STDMASKi),tempwork+4                         +
                        edit packed val using mask
         AHI   R1,-3                         BACKUP ONE BYTE
         MVC   0(3,R1),MASKPARN              COPY NEGATIVE   SIGN  MASK
         ED    0(3,R1),tempwork+L'tempwork-1
         MVI   1(R1),C' '
*
         LA    R14,SAWORKAR+L'STDMASKI   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASKI-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBMICP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SKI           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SKI EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBMICP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSKJ LR    R0,R2              SAVE    REGISTER   R2
*
         xr    R14,r14            COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASKJ),STDMASKJ COPY MASK
         LA    R1,SAWORKAR+L'STDMASKJ-1 LOAD MINIMUM  DATA ADDRESS
         EDMK  SAWORKAR(L'STDMASKj),tempwork+4                         +
                        edit packed val using mask
         AHI   R1,-3                         BACKUP ONE BYTE
         MVC   0(3,R1),MASKNEG               COPY NEGATIVE   SIGN  MASK
         ED    0(3,R1),tempwork+L'tempwork-1
         MVI   1(R1),C' '
*
         LA    R14,SAWORKAR+L'STDMASKJ   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASKJ-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         JM    COLBMJCP
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBMJCP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SKJ           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SKJ EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBMJCP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSKK LR    R0,R2              SAVE    REGISTER   R2
*
         lhi   R14,2              COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASKK),STDMASKK COPY MASK
         LA    R1,SAWORKAR+L'STDMASKK-4 LOAD MINIMUM  DATA ADDRESS
         EDMK  SAWORKAR(L'STDMASKk),tempwork+4                         +
                        edit packed val using mask
         AHI   R1,-3                         BACKUP ONE BYTE
         MVC   0(3,R1),MASKNEG               COPY NEGATIVE   SIGN  MASK
         ED    0(3,R1),tempwork+L'tempwork-1
         MVI   1(R1),C' '
*
         LA    R14,SAWORKAR+L'STDMASKK   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASKK-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         JM    COLBMKCP
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBMKCP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SKK           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SKK EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBMKCP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
COLBMSKL LR    R0,R2              SAVE    REGISTER   R2
*
         lhi   R14,3              COMPUTE  DECIMAL SHIFT  LENGTH
         SH    r14,SAVALDEC
         SH    r14,SAVALRND
         GVBLDX  fp4,accumwrk       get the value into fp4/6
         lcr   r14,r14            invert
         ahi   r14,6176           add in bias
         iextr fp8,fp8,r14        insert in the saved zero
         qaxtr fp0,fp4,fp8,0      qantize the value (align dp)
         if cxtr,fp0,eq,fp8       is the result zero?
           lpdfr  fp0,fp0           make sure it is positive
         endif
         csxtr r14,fp0,0          and convert to packed in r14/15
         stmg  r14,r15,tempwork   and save in tempry area
*
         MVC   SAWORKAR(L'STDMASKL),STDMASKL COPY MASK
         LA    R1,SAWORKAR+L'STDMASKL-5 LOAD MINIMUM  DATA ADDRESS
         EDMK  SAWORKAR(L'STDMASKl),tempwork+4                         +
                        edit packed val using mask
         AHI   R1,-3                         BACKUP ONE BYTE
         MVC   0(3,R1),MASKNEG               COPY NEGATIVE   SIGN  MASK
         ED    0(3,R1),tempwork+L'tempwork-1
         MVI   1(R1),C' '
*
         LA    R14,SAWORKAR+L'STDMASKL   LOAD ENDING ADDRESS OF DATA
         LH    R2,CDMSKLEN               BACKUP TO BEGINNING
         SR    R14,R2
*
         LHI   R1,L'STDMASKL-1    CALC NBR OF CHARS TO THE LEFT OF MASK
         SR    R1,R2
         JM    COLBMLCP
         BCTR  R2,0               DECREMENT  TARGET  MASK LEN  FOR "EX"
         EX    R1,COLBCSPC        ANY DIGITS OUTSIDE MASK AREA ???
         JE    COLBMLCP           NO  - COPY MASKED DATA TO OUTPUT
*
         LHI   R14,L'VWOVRFIL-1   YES - PROCESS OVERFLOW
         LH    R2,CDCOLSIZ        LOAD   COLUMN WIDTH
         CR    R2,R14             COLUMN WIDER  THAN  FILL ???
         JNH   COLB1SKL           NO  -  BYPASS ADJUSTMENT
         LR    R2,R14             YES -  USE  FILL LENGTH
COLB1SKL EQU   *
         LA    R14,VWOVRFIL
         OI    STATFLG4,STATOVFL  Indicate there has been an overflow
         MVC   OVFLCHAR,VWOVRFIL  save one fill char - for message
*
COLBMLCP L     R1,DL96TGTA               LOAD TARGET  ADDRESS
         EX    R2,COLBCOPY               COPY DATA
         LR    R2,R0                     RESTORE   REGISTER
         BR    R10                       RETURN
                     SPACE 3
static   loctr
*
COLBZAP  ZAP   0(0,R14),tempwork         * * * * E X E C U T E D * * *
COLBRZAP ZAP   TEMPWORK2(AccumDFPl),0(0,R14) * * E X E C U T E D * * *
COLBUNPK UNPK  0(0,R14),tempwork         * * * * E X E C U T E D * * *
COLBPACK PACK  TEMPWORK2,0(0,R14)        * * * E X E C U T E D * * *
COLBSTCM STCM  R0,B'0000',0(R14)         * * * * E X E C U T E D * * *
COLBCOPY MVC   0(0,R1),0(R14)            * * * * E X E C U T E D * * *
COLBCSPC CLC   SAWORKAR(0),SPACES        * * * * E X E C U T E D * * *
COLBCZER CLC   SAWORKAR(0),ZEROES        * * * * E X E C U T E D * * *
COLBBINM DC    X'0000000100030007000F'   BIT MASKS FOR "STCM" INSTR
COLBP02  DC    PL2'02'
COLBP03  DC    PL2'03'
STDMASK1 DC  X'F02120202020202020202020202020202020202020202020'
STDMASK2 DC  X'F0212020202020202020202020202020202020202020202060'
STDMASK3 DS 0XL32
         DC  X'402020206B2020206B2020206B2020206B2020206B2020206B'
         DC  X'2021204B202060'
STDMASK4 DS 0XL32
         DC  X'4020206B2020206B2020206B2020206B2020206B2020206B'
         DC  X'2020206B20212060'
STDMASK5 DS 0XL31
         DC  X'4020206B2020206B2020206B2020206B2020206B2020206B'
         DC  X'2020206B202120'
STDMASK6 DS 0XL32
         DC  X'40202020206B2020206B2020206B2020206B2020206B2020206B'
         DC  X'2021204B2060'
STDMASK7 DS 0XL32
         DC  X'40206B2020206B2020206B2020206B2020206B2020206B2020206B'
         DC  X'2021204B20'
STDMASK8 DS 0XL31
         DC  X'402020206B2020206B2020206B2020206B2020206B2020206B'
         DC  X'2021204B2020'
STDMASK9 DC X'402020202020202020202020202020202020202021204B2020'
STDMASKA DC X'402020202020202020202020202020202020202020202120'
STDMASKB DC  X'F0212020202020202020202020202020202020202020202020'
STDMASKC DS 0XL31
         DC  X'402020206B2020206B2020206B2020206B2020206B202020'
         DC  X'6B2021204B2020'
STDMASKD DS 0XL31
         DC  X'4020206B2020206B2020206B2020206B2020206B202020'
         DC  X'6B2020206B202120'
STDMASKE DS 0XL32
         DC  X'40206B2020206B2020206B2020206B2020206B2020206B202020'
         DC  X'6B2021204B20'
STDMASKF DS 0XL32
         DC  X'4020206B2020206B2020206B2020206B2020206B202020'
         DC  X'6B2020206B2021205D'
STDMASKG DS 0XL32
         DC  X'402020206B2020206B2020206B2020206B2020206B202020'
         DC  X'6B2020214B20205D'
STDMASKH DS 0XL32
         DC  X'402020206B2020206B2020206B2020206B2020206B202020'
         DC  X'6B2021204B20205D'
STDMASKI DS 0XL32
         DC  X'402020206B2020206B2020206B2020206B2020206B202020'
         DC  X'6B2120204B20205D'
STDMASKJ DC  X'402020202020202020202020202020202020202020202120'
STDMASKK DC  X'402020202020202020202020202020202020202021204B2020'
STDMASKL DC  X'4020202020202020202020202020202020202021204B202020'
*
         DROP  R6
                     EJECT
         ds  0h (just to make sure LARL works )
DASH     DC  80C'-'
HEXFF    DC 256X'FF'
ZEROES   DC  32X'00'
SPACES   DC 256C' '
                     SPACE 3
NOTTLTXT DC    CL70'NO TITLE FOUND'
CANCLTXT DC    CL70'REQUEST CANCELLED (LIMIT EXCEEDED, DATA PROBLEM)'
BADNUMS  DC    C'** INVALID NUMBER **'
*
TOT00    DC    CL60'GVBMR88 - EXTRACT REQUESTS LOADED.................'
TOT01    DC    CL60'GVBMR88 - TOTAL NUMBER OF LOGIC TABLE ROWS........'
TOT02    DC    CL60'GVBMR88 - EXTRACT RECORDS  PROCESSED..............'
TOT03    DC    CL60'GVBMR88 - MASTER  RECORDS  PROCESSED..............'
TOT04    DC    CL60'GVBMR88 - TOTAL RECORDS WRITTEN TO DATA     FILE..'
TOT05    DC    CL60'GVBMR88 - TOTAL RECORDS WRITTEN TO PRINT    FILE..'
TOT06    DC    CL60'GVBMR88 - TOTAL RECORDS WRITTEN TO DOWNLOAD FILE..'
TOT07    DC    CL60'GVBMR88 - TOTAL RECORDS WRITTEN TO MASTER   FILE..'
TOT08    DC    CL60'GVBMR88 - TOTAL RECORDS WRITTEN TO ONLN HDR FILE..'
TOT09    DC    CL60'GVBMR88 - TOTAL RECORDS WRITTEN TO ONLN DTL FILE..'
TOT10    DC    CL60'GVBMR88 - TOTAL RECORDS WRITTEN TO EXEC INF FILE..'
TOT11    DC    CL60'GVBMR88 - TOTAL RECORDS WRITTEN BY VIEW XXXXXXXX..'
         copy  GVBRPTH8
code     loctr
         PRINT NOGEN
         DCBD  DSORG=PS
         CVT DSECT=YES
*
*
*
         END
