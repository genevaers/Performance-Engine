         TITLE 'GVBMRSM - SUMMARIZE AND FORMAT EXTRACT FILE RECORDS'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2001, 2023.
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
*  GVBMRSM - OUTPUTS SORTED  EXTRACT DATA                             *
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
         COPY  GVBMRSMW
         COPY  GVBMRSMC
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
* 2.  CALL INITIALIZATION SUBROUTINE (GVBMRSI) TO ALLOCATE BUFFERS    *
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

GVBMRSM  RMODE 31
GVBMRSM  AMODE 31
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
GVBMRSM  CSECT
         J     START
MR88EYE  GVBEYE GVBMRSM
*
static   loctr                    define the static section
code     loctr                    make sure we are in the correct loctr
*
         using saver,r13
START    STM   R14,R12,savgrs14   SAVE CALLER'S  REGISTERS
*
         larl  r11,GVBMRSM        set static area base
         USING (GVBMRSM,code),R11
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
*   places during GVBMRSI processing - these registers must be left
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
*  Initialization - call MRSI
*
*    Note: portions of MRSI are called at other times in order to
*          return records from the sort E15/E35. These can also
*          result in non zero return codes here via it's own RTNERROR.
*
***********************************************************************
         L     R15,GVBMRSIA       LOAD "GVBMRSI"   ADDRESS
         BASR  R14,R15            CALL "GVBMRSI" - INITIALIZATION

*        MRSI will have loaded a zero in fp8/10
*        and the quantum value into fp9/11
*
         LTR   R14,R15            SUCCESSFUL INITIALIZATION ???
         JNZ   RTNERROR           NO  - ERROR (MESSAGE NO.)
*
*  SET  END-OF-FILE EXIT ROUTINE ADDRESS IN  DCB'S                 *
*
         L     R1,EXTRDCBA        LOAD DCB  ADDRESS
         Larl  R14,EXTREND        SET  END-OF-FILE     ADDRESS
         STCM  R14,B'0111',DCBEODA-IHADCB(R1)
*
* GVBMRSI opens the extract file and returns first record
* Check if file is empty
*
         L     R1,EXTRRECA        LOAD EXTRACT FILE RECORD ADDR
*         
         CLI   EXTREOF,C'Y'       EMPTY EXTRACT FILE  ?
         JE    READEOF            EOF Processing     
*
         XR    R8,R8       reset VIEWREC address (indicates 1st time)
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Check for Control records
* Must have at least one
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         ST    R1,RECADDR         Save current Record addr
         LR    R7,R1              
         USING CTLREC,R7
*         
* Start of control record loop
*
*     SVRECCNT is initialised in GVBMRSI
CTRLLOOP EQU   *              Loop through control records
         OC    CTVIEW#,CTVIEW#    CONTROL RECORD (NO REPORT ID) ?
         JNZ   CTRLLPEX           NO  - Exit loop 
*
CTRLREC  EQU   *
         MVC   SVFILENO,CTFILENO  SAVE  THE EXTRACT FILE NUMBER
         MVC   SVFINPDT,CTFINPDT  SAVE  THE FINANCIAL    PERIOD   DATE
         AP    SVRECCNT,CTRECCNT  SUM   THE FILE  RECORD COUNTS
*         
CTRLNEXT EQU   *
* Get next record - skip if another control rec
*         llgf  R15,READSUBR       LOAD  GET SUBROUTINE ADDRESS
         BRAS  R10,READSUBR       READ  THE NEXT  EXTRACT FILE RECORD
         ST    R1,RECADDR         Save current record addr
         LR    R7,R1              LOAD DATA ADDRESS
*
         J     CTRLLOOP       End loop through Control records
*         
CTRLLPEX EQU   *
         DROP  R7
*
* check we have at least one control record
*
         IF CP,SVRECCNT,EQ,P000
           LHI   R14,MSG#400        MISSING CONTROL REC - error
           J     RTNERROR           CONTROL RECORD MUST BE  FIRST
         ENDIF
* Normalise the VIEWID number
         USING HDRREC,R7
         L     R0,HDVIEW#         
         SRL   R0,1               STRIP  HEADER INDICATOR
         ST    R0,HDVIEW#
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* 1.  CHECK FOR HEADER RECORD (REPORT BREAK)                          *
* 2.  READ  ALL REPORT HEADER  RECORDS AND  SUM  RECORD COUNTS        *
* 3.  CHECK FOR EMPTY  VIEW   (NO EXTRACT RECORDS)                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
READLOOP EQU   *
*
HEADLOOP EQU   *
*
* Check for Header record (new View)
*
         LR    R1,R7              POINT  TO   HEADER  RECORD (PARM)
         BRAS  R10,VALIDHDR       VERIFY THAT IT IS A HEADER RECORD
         LTR   R15,R15            VALID  HEADER   RECORD ???
         JNZ   EXTRACTR           No , quit header rec loop
*
*         OC    HDVIEW#,HDVIEW#    CONTROL  RECORD ???
*         JZ    READNEXT           YES -    IGNORE
*         
*         MVI   EOREOFCD,C' '      RESET END-OF-REQUEST   FLAG
*         LHI   R14,MSG#401        INCORRECT REC TYPE
*         J     RTNERROR           INDICATE ERROR
* Found header record - same as last one?
         CLC   HDVIEW#,SVVIEW#     SAME REQUEST NO AS PREV ???
         JE    HDRSUM                      YES - SUM THE RECORD COUNTS
*
* Header with different VIEW ID 
*
* check previous not an empty View i.e. no extract records
*
         CLI   EOREOFCD,C'H'     Was last one a header?
         JE    HDR_NEXT          Yes, skip as no extr recs for previous
*         
         MVI   EOREOFCD,C'H'      INDICATE  READING HEADERS         
*
* Process Current header record
*
         LA    R14,HDSORTKY       COMPUTE ADDRESS OF HEADER DATA
         AH    R14,HDSORTLN
         AH    R14,HDTITLLN
         USING HDRDATA,R14
*
         ZAP   VWRECCNT,HDRECCNT  SAVE REQUEST CONTROL    COUNT
         MVC   VWSATIND,HDSATIND  SAVE REQUEST SATISFIED  INDICATOR
         MVC   VW0C7IND,HD0C7IND  SAVE REQUEST 0C7 ABEND  INDICATOR
         MVC   VWOVRIND,HDOVRIND  SAVE REQUEST OVER LIMIT INDICATOR
         DROP  R14
* 
         J     HDR_NEXT   read next record
*
HDRSUM   EQU   * 
*
* Multiple headers with same View id (from partitions) 
*   Sum record counts
*
         LA    R14,HDSORTKY-HDRREC(,R1)  POINT TO DATA IN NEXT HEADER
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
*
HDR_NEXT EQU   *
         MVC   SVVIEW#,HDVIEW#    Save current View number
*         
         BRAS  R10,READSUBR       READ  THE NEXT  EXTRACT FILE RECORD
         ST    R1,RECADDR         SAVE NEXT  RECORD'S  ADDRESS
         LR    R7,R1              LOAD DATA ADDRESS
* Normalise the VIEWID number
         L     R0,HDVIEW#         
         SRL   R0,1               STRIP  HEADER INDICATOR
         ST    R0,HDVIEW#
*
         J     HEADLOOP
         DROP  R7
*
EXTRACTR EQU *
*
* Extract record following header record - process previous View
*        
         USING EXTREC,R7 
         MVC   SAVERECA,RECADDR   SAVE    NEXT RECORD ADDRESS
         MVC   RECADDR,PREVRECA   RESTORE LAST EXTRACT RECORD ADDRESS
         MVI   EOREOFCD,C'R'      INDICATE END-OF-REQUEST
         BRAS  R10,RPTBREAK       PROCESS BREAK
* Set up for New View         
         MVC   RECADDR,SAVERECA   RESTORE NEXT RECORD ADDRESS
         BRAS  R10,RBRKNEW        SET-UP  FOR  NEW    REQUEST
*
* Read all extract records for this View
* 
EXTRLOOP EQU  *
*
* Change in VIEWID ?
*
         CLC   EXVIEW#,SVVIEW#    SAME VIEW ID as previous
         JNE   EXTREXIT           No, exit read extract record loop
*        
         BRAS  R10,EXTPROC        Process extract record
*
READNEXT EQU   *                  Read next sorted records
         ST    R7,PREVRECA        PREVRECA contains prev EXTRACT rec
         MVC   SVVIEW#,EXVIEW#    Save current VIEWID
*         
         BRAS  R10,READSUBR       READ  THE NEXT  EXTRACT FILE RECORD
         ST    R1,RECADDR 
         LR    R7,R1
* Normalise the VIEWID number
         L     R0,EXVIEW#         
         SRL   R0,1               STRIP  HEADER INDICATOR
         ST    R0,EXVIEW#
*
         J     EXTRLOOP 
EXTREXIT EQU   *
         J     READLOOP
         DROP  R7
*         
EXTREND  DS    0H         
*
* EOF processing - SORT has completed here
*
         MVI   EXTREOF,C'Y'       INDICATE END-OF-FILE  ON EXTRACT
*         J     READEOF
*
* End of file processing
*
READEOF  DS    0H
         CLI   EOREOFCD,C'H'      READING NEXT HEADERS ???
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
*         Larl  R15,noextrec                PRINT NO EXTRACT RECORD MSG
*         BASR  R10,R15
*
         MVI   EOREOFCD,C'F'      INDICATE END-OF-FILE
*
CHKCOUNT DS    0H
***********************************************************************
*  GET FINISH DATE/TIME                                               *
***********************************************************************
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
*
*         DROP  R8
*                     
RETURNE  ds    0h
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

***********************************************************************         

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
* Match View ID with correct VIEWREC in chain
*
RBRKSRCH LTR   R8,R8              END-OF-LIST ???
         JNP   RBRKNOT            YES - REQUEST NOT FOUND
         CLC   EXVIEW#,VWVIEW#    MATCHING REQUEST NUMBERS ???
         JE    RBRKINIT           YES - PERFORM INITIALIZATION
         L     R8,VWNEXT          LOAD  ADDRESS OF NEXT LIST ENTRY
         J     RBRKSRCH           LOOP  THROUGH ENTIRE  LIST
*
RBRKNOT  equ   *
         L     R0,EXVIEW#         NO  - DISPLAY REQUEST NUMBER
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  ERRDATA+0(9),DBLWORK
         J     RTNERROR
*
RBRKINIT EQU   *
*
         MVI   VWPROC,C'P'        This view is processed in this run 
*                                 (there could be views in VDP not in
*                                  this extract file)                
         TM    VWFLAG1,VWZEROSP   ZERO SUPPRESS OPTION  ???
         JNO   RBRKIN03
         OC    VWCLCCOL,VWCLCCOL  ANY  "CT"   COLUMNS   ???
         JNZ   RBRKIN03
         NI    VWFLAG1,255-VWZEROSP     TURN  OFF ZERO  SUPPRESS
*
RBRKIN03 EQU   *
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
*  open output file if not already open
*
RBRKOPEN DS    0H 
         L     R2,VWDCBADR            LOAD  DCB ADDRESS
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
* 
RBRKCHKL LH    R0,VWOUTLEN
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
RBRKCHKC EQU   *
***********************************************************************
*  LOOP THROUGH COLUMN DEFINITIONS AND PERFORM COLUMN INITIALIZATION  *
***********************************************************************
         L     R6,VWCOLADR        LOAD FIRST COLUMN  DEFN  ADDRESS
         USING COLDEFN,R6
         LH    R2,VWCOLCNT        LOAD MAXIMUM NO. OF COLUMNS
         LTR   R2,R2              LOAD COLUMN  COUNT (ANY COLUMNS ???)
         JNP   RBRKEXIT           NO  - EXIT
*
RBRKBLD  EQU   *             Loop through columns
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
*
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
*
*
RBRKFMT  LAY   R14,FMTFUNTB       LOAD  FORMATTING FUNCTION TABLE ADDR
         A     R14,CDFMTFUN       ADD   FUNCTION   OFFSET
         L     R14,0(,R14)
         ST    R14,CDFMTFUN       SAVE  FORMATTING FUNCTION ADDRESS
*
RBRKNEXT AHI   R6,CDENTLEN        ADVANCE  TO NEXT COLUMN
         BRCT  R2,RBRKBLD       LOOP THROUGH ALL COLUMNS
*
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
*
RBRKEXIT EQU   *
         BR    R10                RETURN
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
EXTPROC  DS    0H
         ST    R10,SAVERET Save return address
         MVC   VWCURSEC,DETPDL    INDICATE DETAIL LINE (passed to exit)
*
         CLI   VWSUMTYP+1,DETAIL  DETAIL FILE   ???
         JNE   EXTSUMF            NO  -  BRANCH TO   SUMMARY FILE
*
*   No aggregation          
*         
         BRAS  R10,DETFILE        WRITE  DETAIL FILE RECORD
*         
         J     EXTR_END
*                     
EXTSUMF  DS    0H
*        
*  View with aggregation
*
         LH    R1,EXSORTLN        LOAD  LENGTH  OF SORT KEY (ALL KEYS)
         LTR   R15,R1             ANY   SORT KEYS  ???
         JNP   EXTNOBRK           NO  - CONTINUE   ACCUMULATION
*
         LA    R0,EXSORTKY        LOAD  ADDRESS OF CURRENT  SORT KEY
         L     R14,SVSORTKY       LOAD  ADDRESS OF PREVIOUS SORT KEY
         CLCL  R0,R14             SORT  KEY  BREAK ???
         JNE   EXTSUMFB           YES - OUTPUT  SUMMARY REC
*
         L     R14,VWLOWSKY       LOAD  SORT KEY   COUNTER   ADDRESS
         AP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P001 INCREMENT ROW COUNT
*
         CP    SKCOUNT-SORTKEY(L'SKCOUNT,R14),P001
         JNE   EXTNOBRK           NO  - ACCUMULATE SUBTOTALS
         J     EXTSUMFF           YES - PERFORM    FIRST TIME  LOGIC
*         
EXTSUMFB DS    0H         
*
* Key break - Process previous totals
* 
         L     R7,PREVRECA        POINT TO PREVIOUS EXTRACT RECORD
         L     R3,SUBTOTAD        LOAD  BASE SET OF VALUES  ADDRESS
         ST    R3,CALCBASE
         LHI   R15,1              INDICATE AT LOWEST BREAK  LEVEL
         STH   R15,SVBRKCNT
*         
         BRAS  R10,SUMFILE        WRITE SUMMARY FILE RECORD
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
***********************************************************************
*  COPY EXTRACT RECORD "CT" COLUMNS INTO ARRAY USING COL# AS SUBSCRIPT*
***********************************************************************
* R1 points to Array EXTCOLA  (=CALCBASE)
         BRAS  R9,COPY_CT
*
         BRAS  R9,FRSTSET         INITIALIZE SPECIAL ACCUMULATORS
*
         L     R14,VWLOWSKY       LOAD  SORT KEY   COUNTER   ADDRESS
         ZAP   SKCOUNT-SORTKEY(L'SKCOUNT,R14),P001
*
         J     ZEROCBRK
***********************************************************************
*  CONTINUE EXTRACT RECORD PROCESSING                                 *
***********************************************************************
EXTNOBRK L     R1,EXTCOLA         LOAD ADDRESS OF CORRECT COLUMN BASE
         ST    R1,CALCBASE        SAVE BASE   FOR CALCULATIONS IF ANY
*
         LH    R0,EXNCOL          LOAD NO. OF COLUMNS IN RECORD
         LTR   R0,R0              ANY COLUMNS IN THIS    RECORD ???
         JNP   ZEROCBRK           NO -  CHECK IF DETAIL  REPORT
***********************************************************************
*  COPY EXTRACT RECORD "CT" COLUMNS INTO ARRAY USING COL# AS SUBSCRIPT*
***********************************************************************
*    R1 points to Array EXTCOLA (=CALCBASE)
         BRAS  R9,COPY_CT
*         
ZEROCBRK DS    0H
***********************************************************************
*  SUMMARY REPORT:  ADD DETAIL COLUMN VALUES TO SUMMARY ACCUMULATORS  *
***********************************************************************
         XC    SVBRKCNT,SVBRKCNT  ZERO CURRENT BREAK LEVEL
         BRAS  R10,CALCCOLM       PERFORM ANY DETAIL COLUMN CALC
*
         BRAS  R10,SUM            ACCUMULATE  SUM
*
EXTR_END DS    0H
         ST    R7,PREVRECA
*
         L     R10,SAVERET       Restore return address         
         BR    R10                RETURN
*         J     READNEXT            READ  NEXT  EXTRACT RECORD         
*         
static   loctr

DETPDL   DC    CL2'DL'
code     loctr
*
         DROP  R7
***********************************************************************
*  COPY EXTRACT RECORD "CT" COLUMNS INTO ARRAY USING COL# AS SUBSCRIPT*
*   R1 -> Accumulator array
*   R7 -> extract record
*   R9 -  Return address
***********************************************************************
COPY_CT  DS    0H
*
         USING EXTREC,R7
         LA    R6,EXSORTKY        POINT TO FIRST COLUMN
         AH    R6,EXSORTLN
         AH    R6,EXTITLLN
         AH    R6,EXDATALN
         USING COLEXTR,R6
*
         LH    R0,EXNCOL          LOAD NO. OF COLUMNS IN RECORD         
*
         eextr r12,fp9            get the biased exponent set by MRSI
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
*
         BR    R9
         DROP  R6,R7
*
* error message handling
*
RTNERROR DS    0H
         C     R14,=A(MSG#499)    Recursive error ?
         JE    RTNERRORX
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
***********************************************************************
*  COPY EXTRACT RECORD "CT" COLUMNS INTO ARRAY USING COL# AS SUBSCRIPT*
***********************************************************************
*        R1 points to Array
         BRAS  R9,COPY_CT
* Save into Pre-calculation area         
         LH    R15,VWSETLEN       SAVE PRE-CALCULATION RESULTS
         LTR   R15,R15
         JNP   DETFMSTR
*
DETFMSTR equ  *
         BRAS  R10,CALCCOLM       PERFORM     COLUMN   CALCULATIONS
*
         BRAS  R10,EXCPCHK        CHECK  FOR  EXCEPTIONS
         J     COMFSKIP           +0 =  SKIP
         J     COMFKEEP           +4 = OUTPUT DATA RECORD(COMMON LOGIC)
*
         DROP  R7
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        SUMMARY output R O U T I N E                                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTREC,R7
         USING VIEWREC,R8
*
SUMFILE  ST    R10,SAVER10        SAVE  RETURN ADDRESS (R10 IS REUSED)
*
         BRAS  R10,CALCCOLM       PERFORM COLUMN CALCULATIONS (group)
*
         BRAS  R10,EXCPCHK        CHECK  FOR   EXCEPTIONS
         J     COMFSKIP           +0 =  SKIP
***********************************************************************
* The following is common code for DETFILE and SUMFILE
***********************************************************************
*  GET BUFFER FOR OUTPUT FILE RECORD - AND WRITE ANY PREVIOUS RECORD  *
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
COMFBLD  DS    0H
*
*** build and write the CSV column headers if specified ***
*
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
*   Write the header record, get next buffer pointer 
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
****                  end CSV stuff             ***
*
* build and write regular column data
*
COMFBLD1 DS    0H
*     
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
*         
* Reset accumulators
*
COMFSKIP LH    R0,VWCLCCOL        LOAD   CALCULATED  COLUMN COUNT
         LTR   R0,R0              ANY    USED ???
         JNP   COMFEXIT           NO  -  EXIT
*
         L     R3,CALCBASE        LOAD   BASE    ADDRESS
COMFZERO GVBSTX   fp8,0(,r3)                RESET   TOTAL
         AHI   R3,AccumDFPl              ADVANCE TO  NEXT  COLUMN
         BRCT  R0,COMFZERO        LOOP   THROUGH ALL COLUMNS
*
COMFEXIT DS    0H
*
* Save new sort key
*
         L     R7,RECADDR         LOAD  CURRENT RECORD'S ADDRESS
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
*         DROP  R6
         DROP  R7
         DROP  R8
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
*
COLBLOOP DS    0H
*         LH    R14,SVBRKCNT       DETAIL RECORD ???
*         LTR   R14,R14
*         JNP   COLBCHKS           YES -  IGNORE SUBTOTAL CALC OPTIONS
*
*         BRAS  R10,CALCCOLM       PERFORM COLUMN CALCULATIONS IF  ANY
*
*COLBCHKS EQU   * 
*
         CLI   CDPRTIND,C'Y'      PRINT   THIS  COLUMN ???
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
***********************************************************************
*  BUILD COMMA DELIMITED OUTPUT STRING IF OUTPUT DEST TYPE = "CSV"    *
***********************************************************************
COLBCSV  TM    VWFLAG2,VWBLDCSV   COMMA SEPARATED  ???
         JZ    COLBGAP            NO  - PROCESS INTERCOLUMN GAP
*
         L     R14,OUTPCURR       LOAD  CURRENT OUTPUT POSITION
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
         LR    R4,R14             save position in output rec
*
         LA    R15,256(,R15)      ROUND UP TO  256 MULTIPLE
         LR    R0,R15
         SRL   R0,8               No of 256 blocks into r0
         J     COLPB2END
COLPB2LP  MVC   0(256,R14),SPACES
          LA    R14,256(,R14)
COLPB2END BRCT  R0,COLPB2LP
          EX    R15,BLNKTGT        INITIALIZE   TARGET AREA TO  SPACES
*
         LR    r14,r4              restore position in output
         LH    R0,CDCOLSIZ         SAVE COLUMN WIDTH  (-1)
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
COLBMOVE DS    0H
         LA    R15,256(,R15)      ROUND UP TO  256 MULTIPLE
         LR    R0,R15
         SRL   R0,8               No of 256 blocks into r0
         J     COLPmvEND
COLPmvLP MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
COLPmvEND BRCT  R0,COLPmvLP
         EX    R15,MVCDATA        COPY  DATA TO  OUTPUT RECORD
         J     COLBNEXT
                     EJECT
***********************************************************************
*  FORMAT CALCULATED COLUMNS ("CT")                                   *
***********************************************************************
COLBCTAR DS    0H
         MVI   SAVALFMT+1,FM_float SET    decimal float
         MVHHI SAVALDEC,0         SET    NO. OF  DECIMALS
         MVHHI SAVALRND,0         SET    ROUNDING FACTOR
*
         LA    R0,ACCUMWRK        SET    VALUE   ADDRESS
         llgtr R0,r0              make sure ok for 64 bit
         STG   R0,SAVALADR
         LHI   R0,AccumDFPl       SET    VALUE   LENGTH (BYTES)
         STH   R0,SAVALLEN
*
         LR    R1,R3              ASSUME CURRENT SET OF ACCUMULATORS
*
* For COUNT and AVERAGE funcs we need to get the sort key break count
*
         CLI   CDSUBOPT+1,BMEAN 
         JNE   COLBCT10
*COLBCOUNT DS   0H
*         STM   r2,r3,SAVEMR66     Save registers just for dfp stuff
* Get count from 
*         L     R10,VWLOWSKY       LOAD  SORT KEY   COUNTER   ADDRESS
*         USING SORTKEY,R10
**         eextr r12,fp9            get the biased exponent set in MRI
*         S     R1,SUBTOTAD        COMPUTE SET    OFFSET FROM BEG
*         A     R1,EXTCNTA 
*
*         ZAP   0(AccumDFPl,R1),SKCOUNT MOVE packed count into accum 
*         lmg r2,r3,0(r1)       get the packed value into a gpr pair
*         cxstr fp1,r2          convert to dfp
**         iextr fp1,fp1,r12     insert the biased exponent
*         GVBSTX fp1,0(,r1)       and save back in accumlator
*         DROP  R10
*
*         LM    r2,r3,SAVEMR66     Restore registers 
*
*         CLI   CDSUBOPT+1,BCOUNT         
*         JE    COLBCOF2 
*         
* Calculate average value for this sort break 
*
COLBAVGE DS    0H
*
* R1 -> count /fp1 contains count
         L     R1,EXTCNTA
* R3 -> subtotal array of accumulators
         AH    R3,CDCLCOFF           ADD  CALC COLUMN OFFSET  TO BASE
         GVBLDX  fp0,0(,r3)          load the break total
         GVBLDX  fp1,0(,r1)           the count is in fp1
         dxtr  fp0,fp0,fp1             divide them
*
         LR    R1,R3                 
         S     R1,SUBTOTAD            
         A     R1,EXTMEANA             Address break average array
*                                      at the correct col offset         
         GVBSTX   fp0,0(,r1)           save result 
         J     COLBCOF2        
*
COLBCT10 DS    0h
*
***********************************************************************
*  SELECT SET OF ACCUMULATORS                                         *
***********************************************************************
COLBDET  LR    R1,R3                ASSUME CURRENT SET OF ACCUMULATORS
*
COLBMAX  CLI   CDSUBOPT+1,DETMAX    DETAILED MAX   ???
         JNE   COLBMIN
         S     R1,SUBTOTAD          COMPUTE SET  OFFSET FROM BEG
         A     R1,EXTMAXA
         J     COLBCOFF
*
COLBMIN  CLI   CDSUBOPT+1,DETMIN    DETAILED MIN   ???
         JNE   COLBCOUNT
         S     R1,SUBTOTAD
         A     R1,EXTMINA
         J     COLBCOFF
*
COLBCOUNT DS   0H
         CLI   CDSUBOPT+1,BCOUNT    BREAK COUNT
         JNE   COLBDET2
* must copy accumulated value to EXTCNTA - for use by average   
* or just point to it      
*         S     R1,SUBTOTAD
         st    r1,EXTCNTA
*         L     R1,EXTCNTA           Address the count accumulator 
         J     COLBCOF2
*
COLBDET2 EQU   *

COLBCOFF DS    0H
         AH    R1,CDCLCOFF           ADD  CALC COLUMN OFFSET  TO BASE
COLBCOF2 DS    0H         
         MVC   ACCUMWRK(AccumDFPl),0(R1)  COPY VALUE
         GVBLDX  fp0,0(,r1)         get the dfp value
*
         cxtr  fp0,fp8            NULL VALUE  ?
         JNE   COLBEDIT           NO - BYPASS
*
COLBCNUL LH    R0,NOTNULL         DECREMENT NOT NULL  COUNT
         BCTR  R0,0
         STH   R0,NOTNULL
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
*         
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
COLBSHFT DS    0H
         LA    R15,256(,R15)      ROUND UP TO  256 MULTIPLE
         LR    R0,R15
         SRL   R0,8               No of 256 blocks into r0
         J     COLPshEND
COLPshLP MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
COLPshEND BRCT R0,COLPshLP
         EX    R15,MVCDATA        COPY  DATA TO  OUTPUT RECORD
         LLCR  r15,r15            remainder of the move length
*
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
         CLI   VWSUMTYP+1,DETAIL  DETAIL REQUEST ???
         JE    CALCLOOP           YES -  PERFORM CALCULATIONS
* Is this a calculation at group/break level?
*    and is this called at group/break level time ?
         CLI   CDCALOPT+1,BRKCALC CHECK IF Break LEVEL CALCULATION?
         JNE   CALCDETL           No - detail level calc
         CLC   SVBRKCNT,H000      DETAIL EXTRACT  RECORD LEVEL ???
         JNE   CALCLOOP           NO - go do calculation 
CALCDETL DS    0H
* This is a detail level calculation
*   so is this a detail level record ?
         CLC   SVBRKCNT,H000      DETAIL EXTRACT RECORD LEVEL ???
         JNE   CALCADV            NO  -  BYPASS    CALCULATIONS
*        JE    CALCLOOP           Yes - go do calculation          
*
         DROP  R2
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
         MVC   0(AccumDFPl,R14),ACCUMWRK (this is at calctgta)
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
MR88NAME DC    CL08'GVBMRSM '
BLDRMASK DC    XL08'4020202020202120'
RPBLANK  DS   0CL06               REPORT   BLANK LINE
         DC    HL02'6'            LINE     LENGTH
         DC    XL02'0000'
         DC    CL01' '            CARRIAGE CONTROL
         DC    CL01' '            BLANK
                     EJECT
*
GVBMRSIA DC    V(GVBMRSI)         ADDRESS OF "GVBMRSI"
GVBDL96A DC    V(GVBDL96)         ADDRESS OF "GVBDL96"
*GVBTP90A DC    V(GVBTP90)         ADDRESS OF "GVBTP90"
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
*        Initialise Max and Min values                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING VIEWREC,R8
*
FRSTSET  TM    VWFLAG1,VWMIN+VWMAX   +VWBCNT+VWBAVE
*                                    Any special acc in this View?
         JZ    FRSTEXIT
*
         LH    R15,VWSETLEN
         LTR   R15,R15
         JNP   FRSTEXIT
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
SUM      LH    R0,VWCLCCOL        LOAD NO. OF CALCULATED  COLUMNS USED
         LTR   R0,R0              ANY USED ?
         JNP   SUMEXIT            NO - exit
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
         cli   cdsubopt+1,detmax                                  
         je    summove                                            
         cli   cdsubopt+1,detmin                                  
         je    summove                                            
         cli   cdsubopt+1,first                                   
         je    sum_summary_check                                  
         CLI   CDSUBOPT+1,LAST                  LAST  VALUE  ???
         JNE   SUMSUM                           NO  - BRANCH AND SUM
sum_summary_check ds 0h                                           
         CLI   VWSUMTYP+1,SUMMARY               SUMMARY VIEW ???
         JE    SUMSUM                           YES - BRANCH AND SUM
*
SUMMOVE  GVBSTX   fp0,0(,r14)                      NO  - MOVE
         J     SUMMAX
SUMSUM   axtr  fp1,fp1,fp0                      ADD  RESULTS TO  TOTALS
         GVBSTX   fp1,0(,r14)                      and save it
*
SUMMAX   TM    VWFLAG1,VWMIN+VWMAX  SPECIAL FUNCTIONS ?
         JZ    SUMADV
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

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        READ EXTRACT FILE                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTREC,R7
*
READSUBR DS    0H 
*         LR    R9,R15             SET  TEMPORARY PROGRAM BASE REGISTER
*         USING READSUBR,R9
*
         L     R1,EXTRRECA        LOAD  PREVIOUS  RECORD   ADDRESS
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
*
         CLI   EXTREOF,C'Y'       ANY EXTRACT RECS REMAINING ???
         JE    READEOF
*
         L     R1,EXTRRECA              LOAD EXTRACT FILE RECORD ADDR
         MVI   LASTFILE,C'E'         ASSUME EXTRACT RECORD SELECTED
         BR    R10                RETURN         

*
         DROP  R7
*         DROP  R9
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
RPTBREAK DS    0H
* First time through the VIEWREC has not  been selected
*  The VIEWREC is selected in RBRKNEW
         LTR   R8,R8              VIEWREC ? 
         BNPR  R10                YES - RETURN
*
         ST    R10,SAVERBRK       SAVE  RETURN ADDRESS
*
         L     R3,SUBTOTAD        LOAD  BASE SET OF VALUES ADDRESS
         ST    R3,CALCBASE
*
*         CLI   VWDESTYP+1,FILEFMT DATA  FILE OUTPUT ???
*         JE    RPTBREA1           YES - BYPASS  BREAKS
*         CLI   VWDESTYP+1,CSV     COMMA SEPARATED   VARIABLES  ???
*         JNE   RBRKSUBT           NO  - PROCESS ALL BREAKS
*
RPTBREA1 EQU   *
         CLI   VWSUMTYP+1,DETAIL  DETAIL FILE   ???
         JE    RBRKRSET           YES -  RESET  ACCUMULATORS
*
         L     R7,PREVRECA        POINT TO PREVIOUS EXTRACT RECORD
         LHI   R15,1              INDICATE AT LOWEST BREAK LEVEL
         STH   R15,SVBRKCNT
*
         BRAS  R10,SUMFILE        WRITE SUMMARY FILE RECORD
*
         L     R3,SUBTOTAD        LOAD  BASE SET OF  VALUES ADDRESS
*         J     RBRKRSET
*
RBRKRSET EQU   *
         L     R0,SVSORTKY        CLEAR OUT    KEY
         LHI   R1,SVSORTLN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
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
*         ap    tempwork(6),mstrcnt    calculate total
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
*         mvc   rp2.exectext,exectxt2 Lookups performed
*         ap    LKUPcnt,LKUPfnd
*         ap    LKUPcnt,LKUPnot
*         MVC   rp2.execcnt,PATTERN1
*         ED    rp2.execcnt,LKUPcnt
*         rptit ,
*         rptit msg=vb_blankl      blank line
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
*
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
         USING VIEWREC,R8
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
         DROP  R8
*         
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
HEXFF    DC 256X'FF'
ZEROES   DC  32X'00'
SPACES   DC 256C' '
                     SPACE 3
CANCLTXT DC    CL70'REQUEST CANCELLED (LIMIT EXCEEDED, DATA PROBLEM)'
*
         copy  GVBRPTH8
code     loctr
         PRINT NOGEN
         DCBD  DSORG=PS
         CVT DSECT=YES
*
*
*
         END
