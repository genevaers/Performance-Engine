         TITLE 'GVBMR87 - INITIALIZATION FOR "GVBMR88"'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2000, 2022.
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
*  GVBMR87 - LOADS THE VIEW DEFINITION PARAMETER TABLES               *
*          - OPENS REQUIRED INPUT FILES                               *
*          - LOADS MEMORY RESIDENT TITLE LOOK-UP TABLES               *
*          - INITIALIZES  BINARY  SEARCH PATHS                        *
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
*        R11 - PROGRAM   BASE REGISTER (static area)                  *
*                                                                     *
*        R10 - INTERNAL  SUBROUTINE  RETURN  ADDRESS (1ST LEVEL)      *
*        R9  - INTERNAL  SUBROUTINE  RETURN  ADDRESS (2ND LEVEL)      *
*                                                                     *
*        R8  - VIEW DEFN PARAMETER   RECORD  ADDRESS                  *
*                                                                     *
*        R7  - CURRENT   COLUMN  DEFINITION  GROUP    ADDRESS         *
*                                                                     *
*        R6  - CURRENT   SORT    KEY  TITLE  GROUP    ADDRESS         *
*            - CURRENT   COLUMN  CALCULATION ADDRESS                  *
*            - LOOK-UP   BUFFER  PREFIX      ADDRESS                  *
*                                                                     *
*        R5  - CURRENT   "VDP"   RECORD      ADDRESS                  *
*            - CURRENT   PRINT LINE  NUMBER                           *
*            - CURRENT   REFERENCE   RECORD  ADDRESS (TABLE LOOK-UPS) *
*                                                                     *
*        R4  - WORK      REGISTER                                     *
*            - CURRENT   REFERENCE   RECORD      KEY  LENGTH          *
*            - OPERAND   2 ADDRESS                                    *
*                                                                     *
*        R3  - WORK      REGISTER                                     *
*            - PARM DATA ADDRESS                                      *
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
         COPY  VDPHEADR
         COPY  GVB0001A
         COPY  GVB0002A
         COPY  GVB0050A
         COPY  GVB0100A
         COPY  GVB0200A
         COPY  GVB0210A
         COPY  GVB0300A
         COPY  GVB0400A
         COPY  GVB0500A
         COPY  GVB0600A
         COPY  GVB0650A
         COPY  GVB0700A
         COPY  GVB0800A
         COPY  GVB1000A
         COPY  GVB1200A
         COPY  GVB1210A
         COPY  GVB1300A
*
VDP1300LN EQU  *-VDP1300_TITLE_LINES_RECORD
*
         COPY  GVB1400A
*
VDP1400LN EQU  *-VDP1400_FOOTER_LINES_RECORD
*
         COPY  GVB1600A
         COPY  GVB2000A
         COPY  GVB2200A
         COPY  GVB2210A
         COPY  GVB2300A
         COPY  GVB3000A
         COPY  GVB3200A
         COPY  GVB4000A
         COPY  GVB4200A
         COPY  GVBHDR
         COPY  GVBUTEQU
         copy  gvbrptit
         copy  gvbphead
         copy  gvbpcont
         copy  reptdata
*
Fiscal_date_table dsect
Fiscal_date_entry ds 0cl(fiscal_date_entry_len)
fiscal_next  ds   f
fiscal_recid ds   f
fiscal_iddate  ds   cl8
fiscal_date_len equ *-fiscal_recid
fiscal_date_entry_len equ *-fiscal_next
*
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        REGISTER EQUATES:                                            *
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

MAXDEC   EQU   08                 MAXIMUM   NO   OF DECIMALS
COUNTLEN EQU   6                  LENGTH OF EACH COUNT        (CELL)
BRKLVLS  EQU   99+1               # OF BREAK LEVELS (INCL. GRAND TOTAL)
VDPCUSH  EQU   1024               MAXIMUM VDP RECORD SIZE
*
FC_PACK  EQU   X'04'              PACKED
FC_EDIT  EQU   X'0A'              EDITED  NUMERIC
fc_float equ   x'0b'              Decimal floating point - 16 byte
*
RDNDECB  EQU   40                 EXTRACT FILE  READ BUFFERS (EVEN#)
***********************************************************************
* PHYSICAL FILE TYPES                                                 *
***********************************************************************
STDEXTR  EQU   01                 STANDARD EXTRACT
DISKDEV  EQU   02                 DISK     DEVICE TYPE
TAPEDEV  EQU   03                 TAPE     DEVICE TYPE
PIPEDEV  EQU   04                 PIPE     DEVICE TYPE
TOKENDEV EQU   05                 TOKEN    DEVICE TYPE
*

         PRINT NOGEN,off
         COPY  ASMMSP
LEAVE    OPSYN ASM_LEAVE
         asmmrel on
         SYSSTATE ARCHLVL=2
*
         IEABRCX DEFINE
         print gen,on
*
GVBMR87  RMODE 31
GVBMR87  AMODE 31
GVBMR87  CSECT
         j     start              get to the code
MR87EYE  GVBEYE GVBMR87
static   loctr                    define the static section
code     loctr                    and the code
code_2   loctr                      sections
at_end   loctr                    and one area at the end

code     loctr                    make sure we are in the correct loctr
         USING WORKAREA,R13
         using saver,savemr88
start    STM   R14,R12,savgrs14        SAVE  CALLER'S REGISTERS
*
         larl  r11,GVBMR87        SET  static area base
         USING (GVBMR87,code),R11

         lzxr  fp8                put a zero into fp8/10

         if CLI,LRGNUM,eq,c'Y'    large numbers (3dp) ?
           GVBLDX  fp9,=ld'1.000'   then point at a 3dp value
         else
           GVBLDX  fp9,=ld'1.00000000'   or an 8dp value
         endif

         brasl R10,PGMINIT        INITIALIZE PROGRAM  WORK AREA
*
         brasl R10,HEADINGS       PRINT CONTROL REPORT   HEADINGS
*
         brasl R10,READPARM       Read parms in MR88PARM
*
         brasl R10,PRINTRT1       Print the control report OPTS
*
         brasl R10,VDPLOAD        LOAD THE VIEW DEFINITION TABLES
*
         BRAS  R10,ALLOCDYN       ALLOCATE DYNAMIC AREAS
*
         BRAS  R10,FILLLKUP       LOAD THE  TITLE  LOOK-UP TABLES
*
         brasl R10,PRINTRT2       Print the control report IRUN
*
         BRAS  R10,OPENOUT        OPEN VIEW OUTPUT FILES  (IF ANY)
*
         brasl R10,LERUNTIM       INITIALIZE  LANG ENV RUN-TIME
*
         brasl R10,OPENSTD        OPEN STANDARD GENEVA FILES

         XR    R15,R15            SET  RETURN CODE  TO ZERO
*
RETURNE  MVC   LKUPKEY,SPACES     RESET   KEY AREA
*
         L     R14,savgrs14           RESTORE REGISTER  R14
         LM    R0,R12,savgrs0         RESTORE REGISTERS R0 - R12
         bsm   0,r14                  RETURN
                     SPACE 3
RTNERROR LR    R15,R14            SET  RETURN CODE = MESSAGE NUMBER
         j     RETURNE
                     EJECT
*
static   loctr
         print data
         LTORG
         print nodata
code     loctr
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        A L L O C A T E   D Y N A M I C   W O R K   A R E A S        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
***********************************************************************
*  ALLOCATE SORT KEY SAVE AREA                                        *
***********************************************************************
ALLOCDYN LHI   R0,SVSORTLN
         GETMAIN R,LV=(0),LOC=(ANY)
         ST    R1,SVSORTKY        SAVE    AREA   ADDRESS
*
         LR    R0,R1              ZERO    AREA
         LHI   R1,SVSORTLN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         LH    R0,MAXCOL
         if    ltr,R0,r0,z        Check if Max col is zero
           LHI   R14,MSG#415      if it is, issue error message 16
           B     RTNERROR         and branch to error routine
         endif
***********************************************************************
*  COMPUTE SIZE OF ONE SET OF ACCUMULATORS                            *
***********************************************************************
         LH    R0,MAXCOL          COMPUTE MAXIMUM SET SIZE
         LHI   R1,AccumDFPl
         MR    R0,R0
         LR    R2,R1              SAVE    SIZE   (ONE SET)
*
***********************************************************************
*  ALLOCATE AND INITIALIZE NORMALIZED EXTRACT DETAIL "CT" COLUMN AREA *
***********************************************************************
         LR    R0,R2              OBTAIN  MEMORY FOR INTERMEDIATE CELLS
         GETMAIN R,LV=(0),LOC=(ANY)
         ST    R1,EXTCOLA         SAVE    AREA   ADDRESS
*
         LH    R0,MAXCOL          INITIALIZE
ALLOC00  GVBSTX   fp8,0(,R1) storage with DFP zero
         AHI   R1,AccumDFPl
         BRCT  R0,ALLOC00
                     SPACE 3
***********************************************************************
*  ALLOCATE AND INITIALIZE PRE-CALCULATION "CT" COLUMN SAVE AREA      *
***********************************************************************
         LR    R0,R2              OBTAIN  MEMORY FOR PRE-CALC VALUES
         GETMAIN R,LV=(0),LOC=(ANY)
         ST    R1,EXTPCALC        SAVE    AREA   ADDRESS
*
         LH    R0,MAXCOL          INITIALIZE
ALLOC01  GVBSTX   fp8,0(,R1) storage with DFP zero
         AHI   R1,AccumDFPl
         BRCT  R0,ALLOC01
                     SPACE 3
***********************************************************************
*  ALLOCATE AND INITIALIZE LAST  VALUE "CT" COLUMN SAVE AREA          *
***********************************************************************
         LR    R0,R2              OBTAIN  MEMORY FOR LAST     VALUES
         GETMAIN R,LV=(0),LOC=(ANY)
         ST    R1,EXTLASTA        SAVE    AREA   ADDRESS
*
         LH    R0,MAXCOL          INITIALIZE
ALLOC03  GVBSTX   fp8,0(,R1) storage with DFP zero
         AHI   R1,AccumDFPl
         BRCT  R0,ALLOC03
                     SPACE 3
***********************************************************************
*  ALLOCATE AND INITIALIZE PREVIOUS RECORD "CT" COLUMN SAVE AREA      *
***********************************************************************
         LR    R0,R2              OBTAIN  MEMORY FOR PREVIOUS VALUES
         GETMAIN R,LV=(0),LOC=(ANY)
         ST    R1,EXTPREVA        SAVE    AREA   ADDRESS
*
         LH    R0,MAXCOL          INITIALIZE
ALLOC04  GVBSTX   fp8,0(,R1) storage with DFP zero
         AHI   R1,AccumDFPl
         BRCT  R0,ALLOC04
                     SPACE 3
***********************************************************************
*  ALLOCATE AND INITIALIZE SUBTOTAL BREAK LEVEL COUNTS                *
***********************************************************************
         LR    R0,R2              OBTAIN  MEMORY FOR COUNTS
         GETMAIN R,LV=(0),LOC=(ANY)
         ST    R1,EXTCNTA         SAVE    AREA   ADDRESS
*
         LH    R0,MAXCOL          INITIALIZE
ALLOC05  GVBSTX   fp8,0(,R1) storage with DFP zero
         AHI   R1,AccumDFPl
         BRCT  R0,ALLOC05
                     EJECT
***********************************************************************
*  COMPUTE MAXIMUM NO. OF CELLS FOR ALL BREAK LEVEL ACCUMULATORS      *
***********************************************************************
         LH    R0,MAXCOL          COMPUTE SIZE  OF SUBTOTAL ARRAY
         LH    R1,MAXBRK
         MR    R0,R0
         LR    R2,R1              SAVE  NUMBER  OF CELLS   (ALL LEVELS)
*
***********************************************************************
*  ALLOCATE AND INITIALIZE SUBTOTAL BREAK LEVEL ACCUMULATORS          *
***********************************************************************
         LR    R1,R2              LOAD  NUMBER  OF CELLS
         LHI   R0,AccumDFPl       LOAD  SIZE    OF CELL
         MR    R0,R0
         LR    R0,R1
         GETMAIN R,LV=(0),LOC=(ANY)     OBTAIN   MEMORY
         ST    R1,SUBTOTAD        SAVE    AREA   ADDRESS
*
         LR    R0,R2              LOAD    COUNT
ALLOC06  GVBSTX   fp8,0(,R1) storage with DFP zero
         AHI   R1,AccumDFPl
         BRCT  R0,ALLOC06
                     SPACE 3
***********************************************************************
*  ALLOCATE AND INITIALIZE MINIMUM VALUE BREAK LEVEL ACCUMULATORS     *
***********************************************************************
         LR    R1,R2              LOAD  NUMBER  OF CELLS
         LHI   R0,AccumDFPl       LOAD  SIZE    OF CELL
         MR    R0,R0
         LR    R0,R1
         GETMAIN R,LV=(0),LOC=(ANY)
         ST    R1,EXTMINA         SAVE    AREA   ADDRESS
         GVBLDX  fp0,=ld'(inf)'     get maximum +ve value
*
         LR    R0,R2              INITIALIZE
ALLOC07  GVBSTX   fp0,0(,R1) storage with max value
         AHI   R1,AccumDFPl
         BRCT  R0,ALLOC07
                     SPACE 3
***********************************************************************
*  ALLOCATE AND INITIALIZE MAXIMUM VALUE BREAK LEVEL ACCUMULATORS     *
***********************************************************************
         LR    R1,R2              LOAD  NUMBER  OF CELLS
         LHI   R0,AccumDFPl       LOAD  SIZE    OF CELL
         MR    R0,R0
         LR    R0,R1
         GETMAIN R,LV=(0),LOC=(ANY)
         ST    R1,EXTMAXA         SAVE    AREA   ADDRESS
*
         LR    R0,R2              INITIALIZE
ALLOC08  GVBSTX   fp8,0(,R1) storage with DFP zero
         AHI   R1,AccumDFPl
         BRCT  R0,ALLOC08
                     SPACE 3
***********************************************************************
*  ALLOCATE AND INITIALIZE FIRST VALUE "CT" COLUMN SAVE AREA          *
***********************************************************************
         LR    R1,R2              LOAD  NUMBER  OF CELLS
         LHI   R0,AccumDFPl       LOAD  SIZE    OF CELL
         MR    R0,R0
         LR    R0,R1
         GETMAIN R,LV=(0),LOC=(ANY)
         ST    R1,EXTFRSTA        SAVE  AREA    ADDRESS
*
         LR    R0,R2              INITIALIZE
ALLOC09  GVBSTX   fp8,0(,R1) storage with DFP zero
         AHI   R1,AccumDFPl
         BRCT  R0,ALLOC09
                     SPACE 3
***********************************************************************
*                                                                     *
***********************************************************************

         LH    R0,MAXCOL          COMPUTE SIZE OF CALC COL DEFN   TABLE
         SLL   R0,2                    (4 BYTE ENTRIES)
         GETMAIN R,LV=(0),LOC=(ANY)       OBTAIN MEMORY
         ST    R1,CLCCOLTB        SAVE    TABLE  ADDRESS
                     SPACE 3
         LH    R0,MAXCOL          COMPUTE SIZE OF CALC COL OFFSET TABLE
         SLL   R0,2                    (4 BYTE ENTRIES)
         GETMAIN R,LV=(0),LOC=(ANY)       OBTAIN MEMORY
         ST    R1,CLCOFFTB        SAVE    TABLE  ADDRESS

***********************************************************************
*  GET WORKAREA FOR REPORT TITLES                                     *
***********************************************************************
         LH    R0,MAXRPT          GET A MINIMUM OF 2 REPORT TITLE LINES
         if CHI,R0,lt,2
           LHI R0,2
           STH R0,MAXRPT
         endif
*
         LHI   R1,PRNTMAXL+4      MAXIMUM REPORT TITLE LINE LENGTH
         MR    R0,R0
         LA    R0,l'titleyeb(,R1)
         GETMAIN R,LV=(0),LOC=(ANY)
         MVC   0(l'titleyeb,R1),TITLEYEB
         AHI   R1,l'titleyeb
         ST    R1,RTITLEA
         USING RTITLE,R1
*
         LH    R0,MAXRPT
ALLOCRTI MVC   RTCC(PRNTMAXL),SPACES
         LHI   R15,PRNTMAXL+4
         STH   R15,RTRECLEN
         XC    RTRECLEN+2(2),RTRECLEN+2
         AHI   R1,PRNTMAXL+4
         BRCT  R0,ALLOCRTI
         DROP  R1
*
***********************************************************************
*  GET WORKAREA FOR REPORT FOOTERS                                    *
***********************************************************************
         LH    R0,MAXFOOTR        LOAD   MAXIMUM FOOTER LINES
         LTR   R0,R0
         JNP   ALLOCXIT
*
         LHI   R1,PRNTMAXL+4      MAXIMUM REPORT TITLE  LINE LENGTH
         MR    R0,R0
         LA    R0,l'footeyeb(,R1)
         GETMAIN R,LV=(0),LOC=(ANY)
         MVC   0(l'footeyeb,R1),FOOTEYEB
         AHI   R1,l'footeyeb
         ST    R1,RFOOTRA
         USING RTITLE,R1
*
         LH    R0,MAXFOOTR
ALLOCRFI MVC   RTCC(PRNTMAXL),SPACES
         LHI   R15,PRNTMAXL+4
         STH   R15,RTRECLEN
         XC    RTRECLEN+2(2),RTRECLEN+2
         AHI   R1,PRNTMAXL+4
         BRCT  R0,ALLOCRFI
         DROP  R1
*
ALLOCXIT BR    R10                RETURN
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*      O P E N   V I E W   O U T P U T   F I L E S                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
OPENOUT  L     R8,VWCHAIN         LOAD FIRST  REPORT REQUEST ADDRESS
*
         USING VIEWREC,R8
OPLOOP   TM    VWFLAG2,VWOUTDCB   OUTPUT  DCB  USED  ???
         JO    OPDDINIT           YES -   GET  DCB   AREA
         TM    VWFLAG2,VWPRINT    OUTPUT  DCB  USED  ???
         JNO   OPLOCATE           NO - BYPASS  OPENING OF OUTPUT  FILE
                     SPACE 3
***********************************************************************
*  SCAN OTHER VIEWS TO CHECK IF OUTPUT DCB SHARED (ALREADY ALLOCATED) *
***********************************************************************
OPDDINIT L     R14,VWCHAIN        SEARCH  FOR  SAME DD ON ANOTHER VIEW
OPDDLOOP LTR   R14,R14            END-OF-CHAIN ???
         JNP   OPDDNOTF
         CR    R14,R8
         JE    OPDDNOTF
         CLC   VWDDNAME,VWDDNAME-VIEWREC(R14)
         JE    OPDDFND
         L     R14,VWNEXT-VIEWREC(,R14)
         j     OPDDLOOP
*
OPDDFND  MVC   VWDCBADR,VWDCBADR-VIEWREC(R14)
         j     OPLOCATE
*
***********************************************************************
*  IF FIRST VIEW USING DDNAME, ALLOCATE DCB AREA                      *
***********************************************************************
OPDDNOTF LHI   R0,DATADCBL+l'vwddname LOAD DATA FILE DCB LENGTH
         GETMAIN R,LV=(0),loc=BELOW OBTAIN    MEMORY
         MVC   0(l'vwddname,R1),VWDDNAME USE DDNAME AS EYEBALL
         LA    R2,l'vwddname(,R1) LOAD DCB  ADDRESS
         ST    R2,VWDCBADR        SAVE DCB  ADDRESS
         USING IHADCB,R2
*
         Larl  R14,datafile           LOAD  MODEL DCB ADDRESS
         MVC   0(DATADCBL,R2),0(R14)  COPY  MODEL DCB
         LA    R0,DDCBEOFF(,R2)
         ST    R0,DCBDCBE
*
         MVC   DCBDDNAM,VWDDNAME      COPY  DDNAME    INTO DCB
*
         LA    R1,WORKAREA            INITIALIZE  DCB EXIT LIST
         AHI   R1,JFCB-WORKAREA
         ST    R1,JFCBEXIT
         MVI   JFCBEXIT,X'87'         JFCB  IS   LAST ENTRY
         LA    R1,JFCBEXIT            COPY  EXIT LIST ADDRESS
         ST    R1,DCBEXLST
                     SPACE 3
         LAY   R14,STATOPEN
         MVC   RENTPARM(STATOPENL),0(R14)
         RDJFCB ((R2)),MF=(E,RENTPARM) READ  JOB-FILE-CONTROL-BLOCK
         if LTR,R15,R15,nz            rdjfcb failed - so look for
           la  r1,vwddname            this ddname in
           bal r9,loc0200             a type 200 record
           using vdp0200_file_record,r15
           if  ltr,r15,r15,z,or,       No type 200 found?  or ,        +
               cli,vdp0200_alloc_file_type+3,ne,pipedev Not a pipe
               oi vwflag2,vwnodd      DDname not in JCL
           endif
           drop r15
           b oplocate
         endif
                     SPACE 3
OPJFCB   LA    R14,WORKAREA           LOAD  JFCB    ADDRESS
         AHI   R14,JFCB-WORKAREA
         USING JFCBAR,R14
*
         TM    JFCRECFM,X'50'         "RECFM=VB" SPECIFIED IN JCL ???
         JNO   OPFBAVBA               NO  - BYPASS  OVERRIDE
         NI    DCBRECFM,X'5F'         YES - CHANGE  RECFM  IN DCB
         OI    DCBRECFM,X'50'
*
OPFBAVBA TM    JFCRECFM,X'04'         "RECFM=__A"
         JNO   OPLRECL                NO  - BYPASS  OVERRIDE
         OI    DCBRECFM,X'04'
*
OPLRECL  LH    R1,JFCLRECL            LRECL AVAILABLE FROM JCL  ???
         CHI   R1,1
         JH    OPLOCATE
*
         LH    R1,VWOUTLEN            LOAD  RECORD  LENGTH
         TM    VWFLAG2,VWPRINT        PRINT OUTPUT  ???
         JNO   OPLRECLQ               NO  - BYPASS  LENGTH CHECK
         CH    R1,VWLINSIZ
         JH    OPLRECLQ
         LH    R1,VWLINSIZ
*
OPLRECLQ EQU   *
         If    cli,vwdestyp+1,eq,batch  If the output is hardcopy
           ahi r1,1                   add one to account for cc
         endif
         STH   R1,DCBLRECL            SET   LOGICAL RECORD LENGTH
         TM    JFCRECFM,X'40'         VARIABLE      LENGTH ???
         JNO   OPBLKSIZ               NO  - BYPASS  RDW    ADDITION
         AHI   R1,4                   ADD   RDW     LENGTH
         STH   R1,DCBLRECL            SET   LOGICAL RECORD LENGTH
         j     OPLOCATE
*
OPBLKSIZ LH    R14,JFCBLKSI           BLKSIZE IN    JCL    ???
         DROP  R14
         LTR   R14,R14
         JP    OPBLKSIQ
         L     R14,BLKSIZE            LOAD  DESIRED BLOCK  SIZE
OPBLKSIQ EQU   *
         SRDL  R14,32                 COMPUTE MAXIMUM RECORDS PER BLK
         DR    R14,R1
         LR    R14,R1                 COMPUTE BLOCKSIZE
         MR    R14,R14
         STH   R15,DCBBLKSI
         DROP  R2
                     SPACE 3
***********************************************************************
*  LOCATE LOOK-UP BUFFERS FOR SORT BREAK TITLES                       *
***********************************************************************
OPLOCATE LH    R2,VWSRTCNT        LOAD SORT KEY  TABLE  ENTRY COUNT
         LTR   R2,R2              ANY  SORT KEYS ???
         JNP   OPNEXTVW           NO  -  ADVANCE TO NEXT VIEW
                     SPACE 3
         L     R4,VWSKADDR
         USING SORTKEY,R4
         ST    R4,CURSRTKY
*
         XC    CURTTLOF,CURTTLOF  INITIALIZE OFFSET TO TITLE KEY VALUE
*
OPSKLOOP CLI   SKHDRBRK+1,SUPPRESS SUPPRESS SORT BREAK ???
         JE    OPSKNEXT
*
         OC    SKLRID,SKLRID      TITLE KEYS USED  IN THIS REQUEST ???
         JZ    OPSKNEXT           NO  - BYPASS LOOKUP
*
         LA    R1,SKFILEID        POINT  TO  FILE  ID + RECORD ID
         BRAS  R9,LOCLB           LOCATE MATCHING  RECORD  BUFFER
         USING LKUPBUFR,R6
*
         LTR   R6,R6              BUFFER FOUND (ALREADY OPEN) ???
         JNP   OPSKERR            NO  -  INDICATE  ERROR
*
         ST    R6,SKLBADDR        YES -  SAVE THE  BUFFER ADDRESS
*
         LH    R0,CURTTLOF        LOAD   CURRENT TITLE KEY VALUE OFFSET
         AHI   R0,4               SKIP   "LR ID" IN EXTRACT REC TITLE K
*        STH   R0,SKTTLOFF        SAVE   THIS  TITLE'S KEY OFFSET
         AH    R0,LBKEYLEN        ADD    NEXT   VALUE'S LENGTH
         AHI   R0,1+4             ADJUST TO TRUE LENGTH(+FILEID FILLER)
         TM    LBFLAGS,LBEFFRMV   WERE EFFECTIVE DATES REMOVED ???
         JNO   OPSKOFF            N - SKIP ADJUSTMENT
         AHI   R0,4               ADD BACK THE LENGTH OF THE START DATE
OPSKOFF  STH   R0,CURTTLOF        UPDATE CURRENT TITLE KEY OFFSET
*
         j     OPSKNEXT           BYPASS ALLOCATE
                     SPACE 3
***********************************************************************
*  LOOK-UP BUFFER FOR SORT BREAK TITLE NOT FOUND                      *
***********************************************************************
OPSKERR  LHI   R14,MSG#426        LOAD  ERROR MESSAGE NUMBER
*
         MVC   ERRDATA(L'LKUPMSG),LKUPMSG
*
         L     R0,VWVIEW#               VIEW    ID
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   ERRDATA+02(8),VALUMSK1
         ED    ERRDATA+02(8),DBLWORK+4
*
         L     R0,SKFILEID              FILE    ID
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   ERRDATA+13(8),VALUMSK1
         ED    ERRDATA+13(8),DBLWORK+4
*
         L     R0,SKLRID                RECORD  ID
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   ERRDATA+24(8),VALUMSK1
         ED    ERRDATA+24(8),DBLWORK+4
*
         j     RTNERROR
                     SPACE 3
***********************************************************************
*  ADVANCE TO NEXT SORT KEY                                           *
***********************************************************************
OPSKNEXT AHI   R4,SKENTLEN        ADVANCE TO NEXT SORT KEY WITHIN REQ
         ST    R4,CURSRTKY
         BRCT  R2,OPSKLOOP
*
***********************************************************************
*  ADVANCE TO NEXT VIEW                                               *
***********************************************************************
OPNEXTVW L     R8,VWNEXT          ADVANCE TO NEXT REQUEST
         LTR   R8,R8              END-OF-LIST ???
         JP    OPLOOP             NO  - LOOP THROUGH LIST
*
         BR    R10                RETURN
*
         DROP  R4
         DROP  R6
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        F I L L   M E M O R Y   R E S I D E N T   T A B L E S        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
***********************************************************************
*  OPEN REFERENCE TABLE HEADER RECORD FILE (MR88RTH)                  *
***********************************************************************
FILLLKUP L     R2,HDRDCBA         LOAD   TABLE HEADER DCB  ADDRESS
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,RENTPARM) OPEN HEADER FILE
         LHI   R14,MSG#427        ASSUME OPEN FAILED
         TM    48(R2),X'10'       OPEN SUCCESSFUL ???
         JNO   RTNERROR           NO - INDICATE ERROR
         MVC   HDRGETA+1(3),DCBGETA-IHADCB(R2)
         OC    HDRGETA,MODE31
*
         MVC   SVFILENO,HEXFF     INITIALIZE PREVIOUS EXTRACT FILE#
*
* print IREF report headers
*
         phead hd=iref
***********************************************************************
*  READ FIRST REFERENCE TABLE HEADER RECORD (MR88RTH)                 *
***********************************************************************
         L     R1,HDRDCBA         LOAD HEADER TABLE DATA FILE DCB ADDR
         L     R15,HDRGETA        READ HEADER RECORD
         BASR  R14,R15
         LR    R7,R1              LOAD RECORD ADDRESS
         USING TBLHEADR,R7
*
         rptit msg=rptiref_hd1   column headings
         rptit msg=rptiref_hd2   column  underlining
         rptit msg=rptiref_hd3   column  underlining
*
         xc    refrect,refrect   Zero record total
         xc    refmemt,refmemt   Zero memory total
*
FILLHDR  ds    0h
         ASI   rthcount,1         count records in RTH (1 per GREF)
*
* Get info for IREF report
*
rp2      using irefrept,prntline
         mvc   rp2.ireff1,spaces
         mvc   rp2.ireff2,spaces
         mvc   rp2.ireff3,spaces
         mvc   rp2.ireff4,spaces
* build dd name GREFnnn
         MVC   refddn,REFRXXX
         LH    R0,TBXFILE#               CONVERT FILE NO. TO DEC
*        STH   R0,SVFILENO               save for comparision later
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F' FORCE A VALID  ZONE
         CHI   R0,999                    THREE OR FOUR DIGIT ???
         JH    rp200010
         UNPK  refddn+4(3),DBLWORK       BUILD DDNAME SUFFIX (3)
         J     rp200012
rp200010 EQU   *
         UNPK  refddn+4(4),DBLWORK       BUILD DDNAME SUFFIX (4)
rp200012 EQU   *
         mvc   rp2.irefdd,refddn
* key length
         lhy   R0,tbkeylen
         cvd   r0,dblwork
         mvc   rp2.irefklen,=x'40202120'
         ed    rp2.irefklen,dblwork+6
* record count
         ly    R0,tbreccnt
         cvd   r0,dblwork
         mvc   rp2.irefrcnt,countmsk
         ed    rp2.irefrcnt,dblwork+3
         a     r0,refrect
         st    r0,refrect        Add record count to total
* memory usage
         xr    r3,r3             assume zero
         if ltgf,r1,tbreccnt,p   load record count, test for +ve
          lgh  r3,tbreclen       load record length
          mlgr r2,r1             compute  table size
         endif
*
         cvdg r3,dblwork2
         mvc rp2.irefmemu,mem_mask
         ed  rp2.irefmemu,dblwork2+8+2
         ag  r3,refmemt       Add memory usage to total
         stg r3,refmemt
* Get the PF name from the VDP0650
         mvc rp2.irefpfnm,spaces
         if lt,r3,vdp0650a,p     Get the VDP 650 record if available
           using vdp0650_join_record,r3
* must loop through entries to get the correct one
           lh r0,tbxfile#        get the file/entry number
           if  c,R0,le,vdp0650_grefcnt If no entry, then skip
             la    r3,vdp0650_gref_entry
             using vdp0650_gref_entry,r3
             bct  r0,PFnamenx
             b    PFnamef          if zero we have found PF
PFnamenx ds  0h
             do from=(r0)                loop to the entry we want
*              get entlen and add it to r3
               l   r1,vdp0650_gref_entlen  Get length of PF name
               la  r3,vdp0650_gref_entry_len(r1,r3) next entry
             enddo
PFnamef ds   0h
             l   r4,vdp0650_gref_entlen  Get length of PF name
             bctr r4,r0                  Decrement for "EX"
             ex  r4,rp2move              copy it to report
           endif
         endif
*
         rptit ,
*
static   loctr
rp2move  mvc   rp2.irefpfnm(0),vdp0650_gref_pf
nonemsg  dc    0cl(nonemsg_e-nonemsg_s)
nonemsg_s dc   al2(nonemsg_e-nonemsg_s),al2(0),c'<none>'
nonemsg_e equ  *
code     loctr
         drop  r3
*
         L     R5,TBRECCNT        LOAD RECORD COUNT
         LTR   R5,R5              ANY  DATA   RECORDS ???
         JNP   fillhnxt           NO - ADVANCE TO  NEXT HEADER
*
***********************************************************************
*  CLOSE REFERENCE TABLE DATA FILE IF NEXT DIFFERENT FROM PREVIOUS    *
***********************************************************************
         LH    R0,SVFILENO        FIRST TABLE  ???
         LTR   R0,R0
         JM    FILLOPEN           YES - OPEN   DATA  FILE
*
         CH    R0,TBXFILE#        SAME EXTRACT FILE# AS PREVIOUS ???
         JE    FILLLOC            YES - CONTINUE  USING PREVIOUS
*
         L     R2,LKUPDCBA        CLOSE PREVIOUS  DATA  FILE
         CLOSE ((R2)),MODE=31,MF=(E,RENTPARM)
*
***********************************************************************
*  OPEN REFERENCE TABLE DATA FILE IF DIFFERENT FROM PREVIOUS          *
***********************************************************************
FILLOPEN L     R2,LKUPDCBA        LOAD   TABLE DATA DCB ADDRESS
         USING IHADCB,R2
*
         LH    R0,TBXFILE#               get file number
         STH   R0,SVFILENO               save for comparision later
*
         mvc   dcbddnam,refddn
*
         LArl  R0,FILLEOFD          SET  END-OF-FILE  EXIT  ADDRESS
         STCM  R0,B'0111',DCBEODA
*
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,RENTPARM) OPEN DATA   FILE
         TM    48(R2),X'10'       OPEN SUCCESSFUL ???
         BRO   FILLGETA           YES -  CONTINUE
*
         LHI   R14,MSG#428          OPEN FAILED
         MVC   ERRDATA(8),DCBDDNAM  SAVE INDICATIVE DATA
         j     RTNERROR           INDICATE ERROR
*
FILLGETA MVC   DATAGETA+1(3),DCBGETA-IHADCB(R2)
         OC    DATAGETA,MODE31
*
         DROP  R2
*
***********************************************************************
*  LOCATE LOOK-UP BUFFER FOR "LRID" (ERROR IF ALREADY ALLOCATED)      *
***********************************************************************
FILLLOC  XC    TEMPWORK,TEMPWORK  BUILD  LOOK-UP   BUFFER KEY
         MVC   TEMPWORK+8(4),TBLRID
         LA    R1,TEMPWORK        POINT  TO  FILE  ID + RECORD ID
         BRAS  R9,LOCLB           LOCATE MATCHING  RECORD BUFFER
         USING LKUPBUFR,R6
*
         LTR   R6,R6              BUFFER FOUND (ALREADY ALLOCATED) ???
         JNP   FILLALLO           NO  -  ALLOCATE  BUFFER
         LHI   R14,MSG#429        YES -  INDICATE  ERROR (DUPLICATE)
         MVC   ERRDATA(12),TBFILEID SAVE INDICATIVE DATA
         j     RTNERROR
                     EJECT
***********************************************************************
*  ALLOCATE LOOK-UP BUFFER FOR THIS "LRID"                            *
***********************************************************************
FILLALLO LH    R3,TBRECLEN        LOAD RECORD LENGTH
         S     R3,REDPREFX
         AH    R3,TBKEYLEN        ADD  KEY    LENGTH
         AHI   R3,LBPREFLN        ADD  PREFIX LENGTH
*
         LA    R1,TEMPWORK        POINT TO  FILE & RECORD ID'S
         BRAS  R9,ALLOCATE        ALLOCATE  RECORD BUFFER
         USING LKUPBUFR,R6
*
         OI    LBFLAGS,LBMEMRES   TURN  ON  MEMORY RESIDENT FLAG
*
         ST    R5,LBRECCNT        SAVE  THE RECORD COUNT
*
***********************************************************************
*  ADJUST LOOK-UP KEY OFFSET TO EXCLUDE 16 BYTE HI/LO PREFIX          *
***********************************************************************
         LH    R15,TBKEYOFF       LOAD  KEY OFFSET
*
         LR    R0,R15             COPY LENGTH TO R0 FOR NEG TEST
         S     R0,REDPREFX        SUBTRACT HI/LO JLT AREA LENGTH
         LTR   R0,R0              KEY LENGTH NEGATIVE ???
         JNM   FILLALLQ           YES - BYPASS ERROR
         DC    H'0'               ERROR WITH NEG KEY
FILLALLQ EQU   *
         S     R15,REDPREFX       SUBTRACT HI/LO JLT AREA LENGTH
*
         STH   R15,LBKEYOFF
*
         LH    R15,TBKEYLEN       LOAD  KEY   LENGTH
         BCTR  R15,0              DECREMENT   FOR  "EX"  INSTRUCTION
         STH   R15,LBKEYLEN
*
         SR    R15,R15            EFFECTIVE DATES   PRESENT  ???
         IC    R15,TBEFFDAT
         LTR   R15,R15
         JNP   LKUPALLO           NO  - BYPASS  COMPUTING OFFSET
*
         OI    LBFLAGS,LBEFFDAT   SET   EFFECTIVE DATE INDICATOR
*
         CHI   R15,ENDRANGE       END   DATES   PRESENT   ???
         JNE   FILLALLR
         OI    LBFLAGS,LBEFFEND
*
FILLALLR EQU   *
         LH    R15,TBKEYLEN       LOAD  KEY  LENGTH
         STH   R15,LBEFFOFF
         AHI   R15,4-1            ADD   DATE LENGTH TO KEY LENGTH
         STH   R15,LBKEYLEN
*
***********************************************************************
*  GET MEMORY FOR REFERENCE TABLE DATA                                *
***********************************************************************
LKUPALLO LH    R4,TBRECLEN        LOAD RECORD LENGTH
         S     R4,REDPREFX        SUBTRACT HI/LO JLT AREA LENGTH
         STH   R4,LBRECLEN
*
         LA    R3,LKPREFLN(,R4)   ADD  PREFIX LENGTH
         MR    R2,R5              COMPUTE TABLE SIZE
*
         LR    R0,R3              ALLOCATE MEMORY
         GETMAIN RU,LV=(0),LOC=(ANY)
         ST    R1,LBTBLBEG        SAVE    TABLE STARTING ADDRESS
         AR    R3,R1              COMPUTE TABLE ENDING   ADDRESS
         ST    R3,LBTBLEND        SAVE    TABLE ENDING   ADDRESS
*
         LR    R2,R1              POINT TO FIRST ENTRY
         USING LKUPTBL,R2
                     EJECT
***********************************************************************
*  READ NEXT REFERENCE TABLE DATA RECORD                              *
***********************************************************************
FILLLOOP L     R1,LKUPDCBA        LOAD  TABLE DATA  FILE DCB ADDRESS
         L     R15,DATAGETA       READ   NEXT RECORD
         BASR  R14,R15
         LA    R8,4(,R1)          SAVE   DATA RECORD ADDRESS (SKIP RDW)
*
***********************************************************************
*  MAKE SURE REFERENCE DATA KEYS ARE IN ASCENDING SEQUENCE            *
***********************************************************************
         LR    R14,R8             LOAD  RECORD  ADDRESS
         A     R14,REDPREFX       SKIP RED PREFIX FROM JLT
*
         LH    R1,LBKEYOFF        COMPUTE  KEY ADDRESS
         AR    R1,R14
*
         LH    R15,LBKEYLEN
*
         C     R2,LBTBLBEG        FIRST   ROW ???
         JE    FILLSAVE           YES -  SKIP SEQUENCE CHECK
         EXrl  R15,FILLKYCK       NO  - CHECK FOR  ASCENDING SEQUENCE
         JNH   FILLSERR
*
FILLSAVE EXrl  R15,FILLKYSV       SAVE  CURRENT LOOK-UP  KEY
*
***********************************************************************
*  COPY REFERENCE TABLE DATA RECORD TO MEMORY                         *
***********************************************************************
FILLMOVE XC    LKLOWENT,LKLOWENT  ZERO BINARY SEARCH PATHS
         XC    LKHIENT,LKHIENT
*
         LR    R14,R8             COPY RECORD INTO TABLE
         A     R14,REDPREFX
         LR    R15,R4
         LR    R3,R4
         LA    R2,LKUPDATA
         MVCL  R2,R14             NOTE: R2 IS ADVANCED BY "MVCL"
*
         BRCT  R5,FILLLOOP        LOOP  UNTIL "RECCNT" RECORDS LOADED
*
         j     BLDPATH            BUILD BINARY SEARCH  PATHS
*
* EOF for MR88RTH
*
FILLEOF  L     R2,HDRDCBA
         CLOSE ((R2)),MODE=31,MF=(E,RENTPARM)
*
* Write to control report Input Reference file Totals (IREF)
*
         LH    R0,SVFILENO        Any records in RTH?
         if  ltr,r0,r0,p
*
          rptit msg=rptiref_hd4        column  underlining
           mvc rp2.irefdd,=cl8'Total'
           mvc rp2.ireff2,spaces
*
           l   r2,refrect              the record count total
           cvd r2,dblwork
           mvc rp2.irefrcnt,countmsk
           ed  rp2.irefrcnt,dblwork+3
*
           lg   r2,refmemt             the memory usage total
           cvdg r2,dblwork
           mvc rp2.irefmemu,mem_mask
           ed  rp2.irefmemu,dblwork+8+2
           rptit ,
           rptit msg=rptiref_hd5   column  underlining
         else ,
           mvc prntrdwh(l'nonemsg),nonemsg
           rptit ,
         endif
         rptit msg=vb_blankl
*
         L     R2,LKUPDCBA
         CLOSE ((R2)),MODE=31,MF=(E,RENTPARM)
*
         BR    R10                RETURN
*
***********************************************************************
*  PREMATURE END-OF-FILE BEFORE READING NO. OF RECORDS IN HEADER      *
***********************************************************************
FILLEOFD L     R2,HDRDCBA
         CLOSE ((R2)),MODE=31,MF=(E,RENTPARM)
*
         L     R2,LKUPDCBA
         CLOSE ((R2)),MODE=31,MF=(E,RENTPARM)
*
         L     R0,LBFILEID        INDICATE FILE
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  ERRDATA+00(8),DBLWORK
*
         MVI   ERRDATA+08,C'/'
*
         L     R0,LBLRID          INDICATE LOGICAL RECORD
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  ERRDATA+09(8),DBLWORK
*
         LHI   R14,MSG#439
         j     RTNERROR           RETURN
*
***********************************************************************
*  LOOK-UP DATA KEY SEQUENCE ERROR                                    *
***********************************************************************
FILLSERR LH    R0,TBXFILE#        CONVERT  REFERENCE FILE NO. TO DEC
*        L     R0,LBFILEID        INDICATE FILE
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   ERRDATA+00(8),REFRXXX
         UNPK  ERRDATA+04(3),DBLWORK
*
FILLSER4 MVI   ERRDATA+08,C'/'
*
         L     R0,LBLRID          INDICATE LOGICAL RECORD
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  ERRDATA+09(8),DBLWORK
*
         MVI   ERRDATA+17,C':'
*
         LHI   R0,L'ERRDATA-1-9-9 LOAD MAXIMUM INDICATIVE DATA LENGTH
         CR    R15,R0             KEY  LENGTH  EXCEEDS MAXIMUM ???
         JNH   FILLSER6           NO - SHOW    COMPLETE KEY
         LR    R15,R0             YES - USE    MAXIMUM  LENGTH ALLOWED
FILLSER6 EXrl  R15,FILLKYMV       MOVE  KEY TO INDICATIVE DATA AREA
*
         LHI   R14,MSG#438        KEYS  ARE OUT-OF-SEQUENCE
         j     RTNERROR           RETURN ERROR MESSAGE
*
         DROP  R2
         DROP  R7
*
FILLKYCK CLC   0(0,R1),LBKEY          * * * *  E X E C U T E D  * * * *
FILLKYSV MVC   LBKEY(0),0(R1)         * * * *  E X E C U T E D  * * * *
FILLKYMV MVC   ERRDATA+18(0),0(R1)    * * * *  E X E C U T E D  * * * *
                     EJECT
BLDPATH  L     R2,LBTBLBEG        INITIALIZE FIRST TABLE ENTRY ADDRESS
         L     R7,LBRECCNT        INITIALIZE LOOP  COUNTER
*
         LH    R5,LBKEYLEN        LOAD   KEY LENGTH (-1)
*
         LHI   R0,LKPREFLN        LOAD TABLE ENTRY LENGTH
         AH    R0,LBRECLEN
                     SPACE 3
BLDSRCH  L     R4,LBRECCNT        INITIALIZE TOP   OCCURRENCE NUMBER
         BCTR  R4,0               CHANGE (1:N) SCALE TO (0:N-1)
*
         XR    R3,R3              INITIALIZE BOTTOM OCCURRENCE NUMBER
*
         XC    PREVNODE,PREVNODE  INITIALIZE PREVIOUS NODE ADDRESS
*
         LA    R14,0(R3,R4)       COMPUTE MIDDLE OCCURRENCE NUMBER
         SRL   R14,1
         LR    R15,R0             COMPUTE ADDRESS OF MIDDLE ENTRY
         MR    R14,R14
         A     R15,LBTBLBEG
         ST    R15,LBMIDDLE       SAVE    ADDRESS OF MIDDLE ENTRY
                     SPACE 3
BLDLOOP  LA    R1,0(R3,R4)        COMPUTE MIDDLE OCCURRENCE NUMBER
         SRL   R1,1
*
         LR    R15,R0             COMPUTE ADDRESS OF MIDDLE ENTRY
         MR    R14,R1
         A     R15,LBTBLBEG
         USING LKUPTBL,R15
*
         L     R14,PREVNODE       SET BINARY SEARCH BRANCH  NODE
         LTR   R14,R14
         JNP   BLDLOOPQ
         ST    R15,0(0,R14)
*
BLDLOOPQ EQU   *
         EXRL  R5,BLDKEY          COMPARE KEYS  ???
         JL    BLDTOP
         JH    BLDBOT
*
BLDMATCH AR    R2,R0              ADVANCE TO NEXT   TABLE   ENTRY
*
         BRCT  R7,BLDSRCH         LOOP UNTIL ALL    ENTRIES FOUND
***********************************************************************
*  READ NEXT REFERENCE TABLE HEADER RECORD (MR88RTH)                  *
***********************************************************************
fillhnxt ds    0h
         L     R1,HDRDCBA         LOAD HEADER TABLE DATA FILE DCB ADDR
         L     R15,HDRGETA        READ HEADER RECORD
         BASR  R14,R15
         LR    R7,R1              LOAD RECORD ADDRESS
         USING TBLHEADR,R7
*
         j     FILLHDR
                     EJECT
BLDTOP   LA    R14,LKLOWENT       SAVE PREVIOUS PATH ADDRESS
         ST    R14,PREVNODE
*
         LR    R4,R1              SET TOP    = CURRENT - 1
         BCTR  R4,0
         CR    R3,R4              BOTTOM  > TOP    ???
         JNH   BLDLOOP            NO  - CONTINUE
         j     BLDNOMAT           YES - EXIT
*
BLDBOT   LA    R14,LKHIENT        SAVE PREVIOUS PATH ADDRESS
         ST    R14,PREVNODE
*
         LR    R3,R1              SET BOTTOM = CURRENT + 1
         AHI   R3,1
         CR    R3,R4              BOTTOM  > TOP    ???
         JNH   BLDLOOP            NO  - CONTINUE
*
BLDNOMAT LHI   R14,MSG#430        SHOULD NEVER GET NOT FOUND
*
         MVC   ERRDATA(L'NOTFMSG),NOTFMSG
*
         L     R0,LBFILEID               FILE   ID
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   ERRDATA+02(8),VALUMSK1
         ED    ERRDATA+02(8),DBLWORK+4
*
         L     R0,LBLRID                 RECORD ID
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   ERRDATA+13(8),VALUMSK1
         ED    ERRDATA+13(8),DBLWORK+4
*
         LHI   R0,L'ERRDATA-22-1
         CR    R5,R0
         JNH   FILLHNXQ
         LR    R5,R0
FILLHNXQ EQU   *
         EXRL  R5,BLDERR                 INCL INDICATIVE DATA (KEY)
*
         J     RTNERROR
*
BLDKEY   CLC   LKPREFLN(0,R2),LKUPDATA   * * *  E X E C U T E D   * * *
BLDERR   MVC   ERRDATA+22(0),LKPREFLN(R2)  * *  E X E C U T E D   * * *
*
         DROP  R6
         DROP  R15
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "ALLOCATE" - ALLOCATES MEMORY DYNAMICALLY FOR NEW LOGICAL RECORD    *
*              BUFFERS.                                               *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R9  - RETURN   ADDRESS                                       *
*        R6  - NEW      LOGICAL RECORD BUFFER ADDRESS  * * RETURNED * *
*        R3  - NEW      LOGICAL RECORD BUFFER LENGTH                  *
*        R1  - FILE ID & RECORD ID     ADDRESS                        *
*            - AREA ADDRESS (GETMAIN)                                 *
*        R0  - AREA LENGTH  (GETMAIN)                                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING LKUPBUFR,R6
*
ALLOCATE LR    R14,R1             SAVE FILE & RECORD ID ADDRESS
*
         LR    R0,R3              LOAD AREA LENGTH
         GETMAIN R,LV=(0),LOC=(ANY)
*
         L     R6,SVLBPREV        LOAD PREVIOUS RECORD BUFFER ADDRESS
         ST    R1,LBNEXT          ADD TO BUFFER CHAIN
         LR    R6,R1              INITIALIZE CURRENT BUFFER ADDRESS
         STH   R3,LBLEN
*
         XC    LBNEXT,LBNEXT      INITIALIZE FORWARD CHAIN  POINTER
*
         MVC   LBDDNAME,0(R14)    INITIALIZE FILE ID/DDNAME
         MVC   LBLRID,L'LBDDNAME(R14)        LOGICAL RECORD ID
         XC    LBKEYOFF,LBKEYOFF  INITIALIZE KEY     OFFSET
         XC    LBKEYLEN,LBKEYLEN  INITIALIZE KEY     LENGTH
         XC    LBRECLEN,LBRECLEN  INITIALIZE RECORD  LENGTH
         XC    LBRECCNT,LBRECCNT  INITIALIZE RECORD  COUNT
         XC    LBTBLBEG,LBTBLBEG  INITIALIZE RECORD   TABLE BEGIN
         XC    LBTBLEND,LBTBLEND  INITIALIZE RECORD   TABLE END
         XC    LBMIDDLE,LBMIDDLE  INITIALIZE MIDDLE   ENTRY ADDRESS
         XC    LBLSTFND,LBLSTFND  INITIALIZE LAST     ENTRY FOUND
         XC    LBEFFOFF,LBEFFOFF  INITIALIZE EFFECTIVE DATE OFFSET
         XC    LBFLAGS,LBFLAGS    INITIALIZE PROCESSING FLAGS
*
         BR    R9                 RETURN  TO CALLER
*
         DROP  R6
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCLB" - SEARCHES THE BUFFER CHAIN FOR AN ENTRY WHICH MATCHES      *
*           THE "FILE ID/REC ID" IN THE LOGIC TABLE ENTRY.            *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R9  - RETURN   ADDRESS                                       *
*        R8  - CURRENT  LOGIC   TABLE  ROW    ADDRESS                 *
*        R6  - CURRENT  LOGICAL RECORD BUFFER ADDRESS                 *
*        R3  - CURRENT  LOGICAL RECORD BUFFER  LENGTH                 *
*        R1  - FILE ID + RECORD ID    ADDRESS (SEARCH ARGUMENT)       *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING VIEWREC,R8
*
LOCLB    XR    R3,R3              INITIALIZE CURRENT  BUFFER LENGTH
         LA    R14,LBCHAIN        INITIALIZE PREVIOUS BUFFER ADDRESS
         ST    R14,SVLBPREV
         L     R6,LBNEXT-LKUPBUFR(,R14)      CURRENT  BUFFER ADDRESS
         USING LKUPBUFR,R6
*
         j     LOCEND             CHECK  FOR END-OF-CHAIN
                     SPACE 3
LOCLOOP  ST    R6,SVLBPREV        ADVANCE TO NEXT ENTRY
         L     R6,LBNEXT
LOCEND   LTR   R6,R6              END-OF-CHAIN ???
         BZR   R9                 YES - EXIT SUBROUTINE
*
         CLC   LBDDNAME,0(R1)     MATCHING ENTRY ???
         JNE   LOCLOOP            NO  - ADVANCE TO NEXT ENTRY ON CHAIN
         CLC   LBLRID,L'LBDDNAME(R1)
         JNE   LOCLOOP
                     SPACE 3
         LH    R3,LBLEN           LOAD  CURRENT BUFFER  LENGTH
*
         BR    R9                 EXIT  SUBROUTINE
*
         DROP  R6
         DROP  R8
         DROP  R13
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* M R 9 5   D I R E C T   C O N N E C T   I N I T I A L I Z A T I O N *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING WORKAREA,R13
         using saver,savesort
*
DCONINIT STM   R14,R12,savgrs14
*
         LOAD  EP=GENXD88
         O     R0,MODE31
         ST    R0,SORTE15A
*
         L     R2,EXTRDCBA
         USING IHADCB,R2
*
         LArl  R0,DCONGET           CHANGE BSAM  CHECK ROUTINE ADDRESS
         O     R0,MODE31
         ST    R0,EXTRCHKA
         STCM  R0,B'0111',DCBCHCKA
         ST    R0,savgrs15          CHANGE ENTRY POINT ADDRESS
*
         LM    R14,R12,savgrs14
*
         DROP  R2

DCONGET  STM   R14,R1,savgrs14    SAVE     RETURN ADDRESS
         Llgf  R15,SORTE15A       LOAD "STGXD88" ENTRY  POINT   ADDRESS
         BASSM R14,R15            GET   NEXT     RECORD
*
***********************************************************************
*  EMULATE A BSAM READ, E.G. POST ECB AND SET BLOCK POINTER           *
***********************************************************************
         L     R14,savgrs1        LOAD  DECB     ADDRESS
         MVI   0(R14),X'7F'       POST  ECB
         ST    R1,12(,R14)        SAVE  BLOCK    ADDRESS
*
         L     R14,savgrs14       LOAD  RETURN   ADDRESS
         LTR   R15,R15            SUCCESSFUL     ???
         BZR   R14                YES - RETURN
         SPACE 3
         L     R1,EXTRDCBA        LOAD  DCB      ADDRESS
         USING IHADCB,R1
         XR    R15,R15
         ICM   R15,B'0111',DCBEODA
*
         L     R1,savgrs1         LOAD  DECB     ADDRESS
         MVI   0(R1),X'40'        POST  ECB
*
         BR    R15                BRANCH TO  END-OF-FILE  EXIT
*
         DROP  R1
         DROP  R13
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   M R 9 5   E 1 5   S O R T   E X I T   I N I T I A L I Z A T I O N *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING WORKAREA,R13
         using saver,savesort
*
EXITINIT STM   R14,R12,savgrs14

         LOAD  EP=GENXS88
         O     R0,MODE31
         ST    R0,SORTE15A
*
***********************************************************************
*  BUILD SORT CONTROL STATEMENT                                       *
***********************************************************************
         LHI   R0,2+L'SORTCTRL+L'SORTREC
         GETMAIN R,LV=(0)
         ST    R1,SORTCTLA
         LHI   R0,L'SORTCTRL+L'SORTREC
         STH   R0,0(,R1)                   BUILD  LENGTH
         MVC   2(L'SORTCTRL,R1),SORTCTRL   COPY   CONTROL  CARDS
         MVC   2+L'SORTCTRL(L'SORTREC,R1),SORTREC
*
         L     R14,PARMDADR         COPY   SORT   KEY  LENGTH
         MVC   2+SORTKEYL-SORTCTRL(L'SORTKEYL,R1),EXTKEYL-PARMDATA(R14)
*
         L     R2,EXTRDCBA
         USING IHADCB,R2
*
         Larl  R0,e35retrn          CHANGE BSAM CHECK  ROUTINE ADDRESS
         O     R0,MODE31
         ST    R0,EXTRCHKA
         STCM  R0,B'0111',DCBCHCKA
*
         Larl  R0,SORTE35           BUILD  SORT PARAMETER AREA
         O     R0,MODE31
         ST    R0,SORTE35A
         ST    R13,SORTTHRD
         MVC   SORTID,=CL4'MR88'
         MVC   SORTFFFF,=4X'FF'

         LHI   R0,saver_len      GET REGISTER  SAVE AREA FOR SORT
         GETMAIN R,LV=(0)
         XC    0(saver_len,R1),0(R1)
newsa    using saver,r1
         ST    R13,newsa.savprev
         drop  newsa

         LR    R3,R13
         LA    R2,srttime1        from original WORKAREA
         xc    srttime1,srttime1  make sure it is clean
         LA    R4,srttime3        from original WORKAREA
         xc    srttime3,srttime3  make sure it is clean
         LA    R5,timelist        from original WORKAREA
         LR    R13,R1
         TIME  STCK,(R2),LINKAGE=SYSTEM,DATETYPE=YYYYMMDD,             +
               MF=(E,(R5))
         LA    R1,SORTCTLA-WORKAREA(,R3)   CALL  SORT PROGRAM
         LINK  EP=SORT
         lr    r9,r15             save the sort r15

         TIME  STCK,(R4),LINKAGE=SYSTEM,DATETYPE=YYYYMMDD,             +
               MF=(E,(R5))
         LR    R1,R13
         L     R13,savprev

         LHI   R0,saver_len
         FREEMAIN R,LV=(0),A=(1)

         if LTR,R15,R9,z          SUCCESSFUL SORT CALL?

           LM  R14,R12,savgrs14
           L   R3,EXTRDECB        LOAD CURRENT DECB PREFIX ADDRESS
           L   R2,EXTRDCBA        LOAD DCB     ADDRESS
           XR  R15,R15
           ICM R15,B'0111',DCBEODA
           BR  R15
         endif

         ST    R15,DBLWORK2+4
         UNPK  DBLWORK(9),DBLWORK2+4(5)
         TR    DBLWORK,HEXTAB
         MVC   ERRDATA(8),DBLWORK
         LHI   R14,MSG#431        LOAD   ERROR MESSAGE NUMBER
         j     RTNERROR           PRINT  ERROR MESSAGE
*
static   loctr
SORTCTRL DS   0CL30               SORT CONTROL CARD
         DC    CL01' '
SORTVERB DC    CL05'SORT '
         DC    CL12' FIELDS=(15,'
SORTKEYL DC    CL03'001'
         DC    CL09',CH,A)'
code     loctr
*
         DROP  R2
         DROP  R13
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        S O R T   E X I T   I N I T I A L I Z A T I O N              *
*                                                                     *
*        (THIS CODE EXECUTES AS AN EXTENSION OF GVBMR88)              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING WORKAREA,R13
         using saver,savesort
*
SORTINIT STM   R14,R12,savgrs14
         L     R2,SYSINDCB
         USING IHADCB,R2
         LAY   R14,STATOPEN
         MVC   RENTPARM(STATOPENL),0(R14)
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,RENTPARM)   OPEN SYSIN FILE
         LHI   R14,MSG#412                 LOAD   ERROR MESSAGE ADDRESS
         if    TM,dcboflgs,dcbofopn,z      Failed to open ?
           B   RTNERROR                    NO  -  PRINT ERROR   MESSAGE
         endif
*
         LH    R3,DCBLRECL                 GET    SORT  CONTROL AREA
         SLL   R3,2                          (ALLOW FOR 4 RECORDS)
         LA    R0,2+L'SORTREC(,R3)
         ST    R0,DBLWORK                  SAVE   LENGTH
         GETMAIN R,LV=(0)
         ST    R1,SORTCTLA
         LA    R4,2(,R1)                   LOAD   SORT  STATEMENT ADDR
         A     R1,DBLWORK                  ADD    LENGTH TO  BASE
         ST    R1,DBLWORK                  SAVE   END-OF-STRING   ADDR
*
         do    inf
           LR  R1,R2                       READ   SORT  CONTROL   CARD
           GET (1)
           LH  R3,DCBLRECL                 GET    SORT  RECORD LENGTH
           LA  R0,L'SORTREC(R4,R3)         COMPUTE NEW  END-OF-STRING
           C   R0,DBLWORK                  WILL   THIS  OVERFLOW  ???
           JH  SYSINERR
           BCTR R3,0
           EXrl R3,MVCSORT
           LA  R4,1(R4,R3)
         enddo
MVCSORT  MVC   0(0,R4),0(R1)        * * * *  E X E C U T E D  * * * * *

SYSINEOF MVC   0(L'SORTREC,R4),SORTREC     CONCATENATE  RECORD  STMT
*
         L     R1,SORTCTLA                 LOAD SORT CONTROL AREA ADDR
         LA    R0,L'SORTREC-2(,R4)         COMPUTE SORT AREA LENGTH
         SR    R0,R1
         STH   R0,0(,R1)
*
         CLOSE ((R2)),MODE=31,MF=(E,RENTPARM)
*
         L     R2,EXTRDCBA
*
         Larl  R0,e35retrn          CHANGE BSAM CHECK  ROUTINE ADDRESS
         O     R0,MODE31
         ST    R0,EXTRCHKA
         STCM  R0,B'0111',DCBCHCKA
*
         Larl  R0,SORTE35           BUILD  SORT PARAMETER AREA
         O     R0,MODE31
         ST    R0,SORTE35A
         ST    R13,SORTTHRD
         MVC   SORTID,=CL4'MR88'
         MVC   SORTFFFF,=4X'FF'

         LHI   R0,saver_len      GET REGISTER  SAVE AREA FOR SORT
         GETMAIN R,LV=(0)
         XC    0(saver_len,R1),0(R1)
newsa    using saver,r1
         ST    R13,newsa.savprev

         MVI   STATFLG2,insort    about to sort


         LR    R3,R13
         LA    R2,srttime1        from original WORKAREA
         xc    srttime1,srttime1  make sure it is clean
         LA    R4,srttime3        from original WORKAREA
         xc    srttime3,srttime3  make sure it is clean
         LA    R5,timelist        from original WORKAREA
         LR    R13,R1
         TIME  STCK,(R2),LINKAGE=SYSTEM,DATETYPE=YYYYMMDD,             +
               MF=(E,(R5))

         LA    R1,SORTCTLA-WORKAREA(,R3)   CALL  SORT PROGRAM
         LINK  EP=SORT
         lr    r9,r15             save the sort r15

         TIME  STCK,(R4),LINKAGE=SYSTEM,DATETYPE=YYYYMMDD,             +
               MF=(E,(R5))

         LR    R1,R13
         L     R13,newsa.savprev
         drop  newsa

         MVI   STATFLG2,postsort  sorted

         LHI   R0,saver_len
         FREEMAIN R,LV=(0),A=(1)

         if LTR,R15,R9,z          SUCCESSFUL SORT CALL?

           LM  R14,R12,savgrs14
           L   R3,EXTRDECB        LOAD CURRENT DECB PREFIX ADDRESS
           L   R2,EXTRDCBA        LOAD DCB     ADDRESS
           XR  R15,R15
           ICM R15,B'0111',DCBEODA
           BR  R15
         endif

         ST    R15,DBLWORK2+4
         UNPK  DBLWORK(9),DBLWORK2+4(5)
         TR    DBLWORK,HEXTAB
         MVC   ERRDATA(8),DBLWORK
         L     R14,LOGDCBA
         GVBMSG LOG,MSGNO=MSG#431,SUBNO=2,LOGDCBA=(R14),               +
               SUB1=(GVBMR88,L'GVBMR88),                               +
               SUB2=(ERRDATA,8),                                       +
               MSGBUFFER=(UTMTBUFF,L'UTMTBUFF),                        +
               MF=(E,MSG_AREA)

         LHI   R14,499            Recursive error number
         j     RTNERROR           PRINT  ERROR MESSAGE

SYSINERR LHI   R14,MSG#450          LOAD  ERROR MESSAGE NUMBER
         j     RTNERROR             PRINT ERROR MESSAGE

         DROP  R2
*
         USING VIEWREC,R8
         USING COLDEFN,R7
         USING VDP1300_TITLE_LINES_RECORD,R5
*
CSVMSKMV MVC   CDDETMSK(0),0(R1)    * * * *  E X E C U T E D  * * * * *
MASKBLNK CLC   VDP2000_REPORT_MASK-VDP2000_COLUMN_RECORD(0,R5),SPACES
*
CKMASK   CLC   0(0,R1),0(R14)       * * * *  E X E C U T E D  * * * * *
MASKCOPY MVC   0(0,R14),0(R15)      * * * *  E X E C U T E D  * * * * *
*
MVCGRAND MVC   SKTITLE-SORTKEY(0,R6),GRANDTOT  * * * EXECUTED * * * * *
*
TRASCII  TR    0(0,R14),0(R15)    * * * *  E X E C U T E D  * * * * *
*
         DROP  R5
         DROP  R7
         DROP  R8
         DROP  R13
                     EJECT
static   loctr
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
BLKSIZE  DC    F'23000'
REDPREFX DC    F'0'
*
HEXFF    DC   8XL1'FF'
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
*
H8K      DC    H'8192'
*
P000     DC    P'0'
P001     DC    P'1'
P003     DC    P'3'
P008     DC    P'8'
NINES    DC    P'99999999999'
*
STDPARMS DC    CL1'Y'             PRINT   FILE OUTPUT INDICATOR
         DC    CL1'Y'             EXTRACT FILE SOURCE OPTION ("Y", "N")
         DC    CL1'Y'             NO  HEADER  RECORDS OPTION ("N", "Y")
         DC    CL1'N'             DISPLAY DT ON XI SUBTOTALS?("N", "Y")
         DC    CL1'Y'             IGNORE OVERFLOW ("N" OR "Y")
         DC    CL1'Y'             ZERO DIVIDE ABEND  ("N" OR "Y")
         DC    CL1'N'             Generate header for csv    ("N", "Y")
         DC    CL3'000'           EXTRACT RECORD SORT KEY LENGTH
         DC    CL6'010000'        MISC VDP RECORD    TABLE ROW MAXIMUM
         DC    CL1'N'             EXEC INFO    OPTION CODE
         DC    CL1'E'             EBCDIC/ASCII OPTION CODE    (NP88)
         DC    CL1' '             MARGINAL/HISTORY    MODE    (NP88)
         DC    CL1'N'             NO FIX WIDTH with Delimeter("N","Y")
         DC    CL5'00000'         Abend on message #
STDPARML EQU   *-STDPARMS
*
GVBMR88  DC    CL8'GVBMR88 '      MAIN PROGRAM NAME
REFRXXX  DC    CL8'REFRXXX '      REFERENCE   TABLE  DATA   DDNAME
PARMEYEB DC    CL8'MR88PARM'      EXEC   PARAMETERS  AREA  "EYEBALL"
DCBEYEB  DC    CL8'MR88DCBS'      DCB    AREA              "EYEBALL"
VDPEYEB  DC    CL8'MR88VDP '      VDP    AREA              "EYEBALL"
TITLEYEB DC    CL8'MR88TITL'      REPORT TITLE       AREA  "EYEBALL"
FOOTEYEB DC    CL8'MR88FOOT'      REPORT FOOTER      AREA  "EYEBALL"
FLDSEYEB DC    CL8'MR88FLDS'      FIELD  DEFINITION  AREA  "EYEBALL"
COLSEYEB DC    CL8'MR88COLS'      COLUMN DEFINITION  AREA  "EYEBALL"
SORTEYEB DC    CL8'MR88SORT'      SORT   KEY   DEFN  AREA  "EYEBALL"
CALCEYEB DC    CL8'MR88CALC'      CALCULATIONS DEFN  AREA  "EYEBALL"
VIEWEYEB DC    CL8'VIEWDEFN'      VIEW   DEFINITION "EYEBALL"
LEYEBALL DC    CL8'GENCEEWA'      LE INTERFACE AREA "EYEBALL"
fisceyeb DC    CL8'FISCEYEB'      Fiscal date table eye catcher
*
OUT      DC    C'OUT'
LECOBOL  DC    CL3'CEE'           COBOL  VERSION IDENTIFIER
GRANDTOT DC    C'GRAND TOTALS '
VIEW     DS   0CL3
VDPMSG   DC    C'V# NNNNNNN/C# NNNNNNN/T: XXXX/S# NNNN/ID: NNNNNNN'
LKUPMSG  DC    C'V# NNNNNNN/F# NNNNNNN/R# NNNNNNN'
NOTFMSG  DC    C'F# NNNNNNN/R# NNNNNNN'
CALCMSG  DC    C'V# NNNNNNN/C# NNNN/CALC NNNN/OPR NNNN'
*
VALUMSK1 DC    XL08'4020202020202120'
*
CSVMASK  DC    CL48'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.99999999'
*
STDMASK1 DC    CL25'9999999999999999999999999'
STDMASK2 DC    CL25'999999999999999999999999-'
STDMASK3 DC    CL25'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-'
STDMASK4 DC    CL25'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9-'
STDMASK5 DC    CL25'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9'
STDMASK6 DC    CL25'ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9-'
STDMASK7 DC    CL25'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9'
STDMASK8 DC    CL25'ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99'
STDMASK9 DC    CL25'ZZZZZZZZZZZZZZZZZZZZZ9.99'
STDMASKA DC    CL25'ZZZZZZZZZZZZZZZZZZZZZZZZ9'
STDMASKB DC    CL25'-999999999999999999999999'
STDMASKC DC    CL25'-Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99'
STDMASKD DC    CL25'-ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9'
STDMASKE DC    CL25'-ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9'
STDMASKF DC    CL25'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9)'
STDMASKG DC    CL25'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.99)'
STDMASKH DC    CL25'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99)'
STDMASKI DC    CL25'Z,ZZZ,ZZZ,ZZZ,ZZZ,Z99.99)'
STDMASKJ DC    CL25'-ZZZZZZZZZZZZZZZZZZZZZZZ9'
STDMASKK DC    CL25'-ZZZZZZZZZZZZZZZZZZZZ9.99'
STDMASKL DC    CL25'-ZZZZZZZZZZZZZZZZZZZ9.999'
*
LNNAN    DC    CL5'LNNAN'              NO MASK CODE
*
FMTFUNTB DC    HL2'04',C'Y',HL2'04'    PACKED TO PACKED
         DC    HL2'04',C'N',HL2'08'    PACKED TO PACKED  (UNSIGNED)
         DC    HL2'03',C'Y',HL2'12'    PACKED TO NUMERIC
         DC    HL2'03',C'N',HL2'16'    PACKED TO NUMERIC (UNSIGNED)
         DC    HL2'06',C'Y',HL2'20'    PACKED TO BINARY
*
STACKTBL DC    AL2(L'VDP2210_STACK_OPERATION_CODE) - 00
         DC    AL2(L'VDP2210_STACK_BRANCH)         - 01  (UNCOND)
         DC    AL2(L'VDP2210_STACK_BRANCH)         - 02  (EQ)
         DC    AL2(L'VDP2210_STACK_BRANCH)         - 03  (NE)
         DC    AL2(L'VDP2210_STACK_BRANCH)         - 04  (GT)
         DC    AL2(L'VDP2210_STACK_BRANCH)         - 05  (GE)
         DC    AL2(L'VDP2210_STACK_BRANCH)         - 06  (LT)
         DC    AL2(L'VDP2210_STACK_BRANCH)         - 07  (LE)
         DC    AL2(L'VDP2210_STACK_PUSH_CONSTANT)  - 08  (PUSHV)
         DC    AL2(L'VDP2210_STACK_PUSH_COLUMN)    - 09  (PUSHC)
         DC    AL2(L'VDP2210_STACK_OPERATION_CODE) - 10  (ADD)
         DC    AL2(L'VDP2210_STACK_OPERATION_CODE) - 11  (SUB)
         DC    AL2(L'VDP2210_STACK_OPERATION_CODE) - 12  (MUL)
         DC    AL2(L'VDP2210_STACK_OPERATION_CODE) - 13  (DIV)
         DC    AL2(L'VDP2210_STACK_OPERATION_CODE) - 14  (NEG)
         DC    AL2(L'VDP2210_STACK_OPERATION_CODE) - 15  (ABS)
                     EJECT
SORTE35X DC    A(SORTE35)         "E35" SORT EXIT ADDR
E35RTNX  DC    A(E35RETRN)        "E35" RETURN    ADDR
LECALLA  DC    A(LEPGMCAL)        CALL  LE  CONFORMING PROGRAM
*
GVBDL96A DC    V(GVBDL96)         ADDRESS OF "GVBDL96"
*
CALCBRN  DC    V(CALCBRN)         BRANCH  UNCONDITIONAL
CALCEQ   DC    V(CALCEQ)          BRANCH  EQUAL
CALCNE   DC    V(CALCNE)          BRANCH  NOT  EQUAL
CALCGT   DC    V(CALCGT)          BRANCH  GREATER  THAN
CALCGE   DC    V(CALCGE)          BRANCH  GREATER  THAN  OR  EQUAL
CALCLT   DC    V(CALCLT)          BRANCH  LESS THAN
CALCLE   DC    V(CALCLE)          BRANCH  LESS THAN  OR  EQUAL
CALCPSHV DC    V(CALCPSHV)        PUSH    VALUE
CALCPSHC DC    V(CALCPSHC)        PUSH    COLUMN  NUMBER
CALCADDA DC    V(CALCADD)         ADD
CALCSUBA DC    V(CALCSUB)         SUBTRACT
CALCMULA DC    V(CALCMULT)        MULTIPLY
CALCDIVA DC    V(CALCDIV)         DIVIDE
CALCNEG  DC    V(CALCNEG)         NEGATE
CALCABS  DC    V(CALCABS)         ABSOLUTE VALUE
CALCDONE DC    V(CALCDONE)        CALCULATIONS    FINISHED
*
DCBAREAA DC    A(DCBAREA)
*
EXTRADDR DC    A(EXTRFILE)
PRNTADDR DC    A(PRNTFILE)
LOGADDR  DC    A(LOGFILE)
CTRLADDR DC    A(CTRLFILE)
*
VDPADDR  DC    A(VIEWFILE)
HDRADDR  DC    A(HDRFILE)
LKUPADDR DC    A(LKUPFILE)
SYSINADR DC    A(SYSIN)
*
SORTREC  DC    C' RECORD TYPE=V,LENGTH=8192 '
*
SPACES   DC    Cl256' '
ZEROES   DC    XL32'00'
                        EJECT
***********************************************************************
*  JUSTIFICATION CONVERSION TABLE                                     *
***********************************************************************
JUSTTBL  DC    C'C'               '00' - N/A
         DC    C'L'               '01' - LEFT
         DC    C'C'               '02' - CENTER
         DC    C'R'               '03' - RIGHT
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        R E A D   D A T A   E V E N T   C O N T R O L   B L O C K S  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         PRINT NOGEN
MDLREAD  READ  DECB01,SF,EXTRFILE,0,0,MF=L
MDLPREV  DC    AL4(0)             PREVIOUS AVAILABLE BUFFER ADDRESS
MDLREADL EQU   *-MDLREAD
*
         LTORG
code     loctr
*
MASKCODE DC    CL5'LNN0N',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ'
         DC    CL5'LN10N',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9'
         DC    CL5'LN20N',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ99'
         DC    CL5'LN30N',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZ999'
         DC    CL5'LN40N',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZ9999'
         DC    CL5'LN50N',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZ99999'
         DC    CL5'LN60N',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZ999999'
         DC    CL5'LN70N',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZ9999999'
         DC    CL5'LN80N',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZ99999999'
         DC    CL5'LN11Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.9'
         DC    CL5'LN12Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.99'
         DC    CL5'LN13Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZ9.999'
         DC    CL5'LN14Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZ9.9999'
         DC    CL5'LN15Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZ9.99999'
         DC    CL5'LN16Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZ9.999999'
         DC    CL5'LN17Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZ9.9999999'
         DC    CL5'LN18Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZ9.99999999'
         DC    CL5'LYN0N',CL32'-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ'
         DC    CL5'LY10N',CL32'-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9'
         DC    CL5'LY11Y',CL32'-Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9'
         DC    CL5'LY12Y',CL32'-ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99'
         DC    CL5'LY13Y',CL32'-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999'
         DC    CL5'LY14Y',CL32'-ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999'
         DC    CL5'LY15Y',CL32'-Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99999'
         DC    CL5'LY16Y',CL32'-,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999999'
         DC    CL5'LY17Y',CL32'-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999999'
         DC    CL5'LY18Y',CL32'-ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99999999'
         DC    CL5'PYN0N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ)'
         DC    CL5'PYN1Y',CL32'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.9)'
         DC    CL5'PYN2Y',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.99)'
         DC    CL5'PY10N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9)'
         DC    CL5'PY11Y',CL32'ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,Z9.9)'
         DC    CL5'PY12Y',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99)'
         DC    CL5'PY20N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,Z99)'
         DC    CL5'PY21Y',CL32'ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,99.9)'
         DC    CL5'PY22Y',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,Z99.99)'
         DC    CL5'RNN0N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ-'
         DC    CL5'RN10N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9-'
         DC    CL5'RN20N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ99-'
         DC    CL5'RN30N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZ999-'
         DC    CL5'RN40N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZ9999-'
         DC    CL5'RN50N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZ99999-'
         DC    CL5'RN60N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZ999999-'
         DC    CL5'RN70N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZ9999999-'
         DC    CL5'RN80N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZ99999999-'
         DC    CL5'RN11Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.9-'
         DC    CL5'RN12Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.99-'
         DC    CL5'RN13Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZ9.999-'
         DC    CL5'RN14Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZ9.9999-'
         DC    CL5'RN15Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZ9.99999-'
         DC    CL5'RN16Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZ9.999999-'
         DC    CL5'RN17Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZ9.9999999-'
         DC    CL5'RN18Y',CL32'ZZZZZZZZZZZZZZZZZZZZZ9.99999999-'
         DC    CL5'RYN0N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ-'
         DC    CL5'RY10N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9-'
         DC    CL5'RY11Y',CL32'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9-'
         DC    CL5'RY12Y',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-'
         DC    CL5'RY13Y',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999-'
         DC    CL5'RY14Y',CL32'ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999-'
         DC    CL5'RY15Y',CL32'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99999-'
         DC    CL5'RY16Y',CL32',ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999999-'
         DC    CL5'RY17Y',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999999-'
         DC    CL5'RY18Y',CL32'ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99999999-'
         DC    CL5'UNN0N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ'
         DC    CL5'UN10N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9'
         DC    CL5'UN20N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ99'
         DC    CL5'UN30N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ999'
         DC    CL5'UN40N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9999'
         DC    CL5'UN50N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZ99999'
         DC    CL5'UN60N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZ999999'
         DC    CL5'UN70N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZ9999999'
         DC    CL5'UN80N',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZ99999999'
         DC    CL5'UN11Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.9'
         DC    CL5'UN12Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.99'
         DC    CL5'UN13Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.999'
         DC    CL5'UN14Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZ9.9999'
         DC    CL5'UN15Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZ9.99999'
         DC    CL5'UN16Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZ9.999999'
         DC    CL5'UN17Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZ9.9999999'
         DC    CL5'UN18Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZ9.99999999'
         DC    CL5'UYN0N',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ'
         DC    CL5'UY10N',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9'
         DC    CL5'UY11Y',CL32'ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9'
         DC    CL5'UY12Y',CL32'ZZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99'
         DC    CL5'UY13Y',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999'
         DC    CL5'UY14Y',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999'
         DC    CL5'UY15Y',CL32'ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99999'
         DC    CL5'UY16Y',CL32'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999999'
         DC    CL5'UY17Y',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999999'
         DC    CL5'UY18Y',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99999999'
         DC    XL5'FFFFFFFFFF'
*
                     SPACE 5
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D A T A   C O N T R O L   B L O C K S                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
SNAPDCB  DCB   DSORG=PS,DDNAME=SNAPDATA,MACRF=(W),                     X
               RECFM=VBA,LRECL=125,BLKSIZE=1632
*
* RTC19876
PARMDCB  DCB   DSORG=PS,DDNAME=MR88PARM,MACRF=(GL),DCBE=PARMDCBE
PARMDCBE DCBE  RMODE31=BUFF,EODAD=PARMEOF
*
DATAFILE DCB   DSORG=PS,DDNAME=MR88DATA,MACRF=(PL),                    X
               RECFM=FB,BUFNO=20,DCBE=DATADCBE
DDCBEOFF EQU   *-DATAFILE
DATADCBE DCBE  RMODE31=BUFF,BLOCKTOKENSIZE=LARGE
DATADCBL EQU   *-DATAFILE
*
*
DCBAREA  DS   0F  * * *  M O D E L   D C B   S T A T E M E N T S  * * *
*
EXTRFILE DCB   DSORG=PS,DDNAME=MR88HXE,MACRF=(R),RECFM=VB,DCBE=EXTRDCBE
EXTRDCBE DCBE  RMODE31=BUFF,BLOCKTOKENSIZE=LARGE
*
PRNTFILE DCB   DSORG=PS,DDNAME=MR88PRNT,MACRF=(PM),                    X
               RECFM=VBA,LRECL=137,BLKSIZE=13700
*
LOGFILE DCB    DSORG=PS,DDNAME=MR88LOG,MACRF=(PM),                     X
               RECFM=VB,LRECL=164
*
CTRLFILE DCB   DSORG=PS,DDNAME=MR88RPT,MACRF=(PM),                     X
               RECFM=VB,LRECL=164
*
VIEWFILE DCB   DSORG=PS,DDNAME=MR88VDP,MACRF=(GL),EODAD=VDPEOF
*
HDRFILE  DCB   DSORG=PS,DDNAME=REFRRTH,MACRF=(GL),EODAD=FILLEOF
*
LKUPFILE DCB   DSORG=PS,DDNAME=MR88RTD,MACRF=(GL)
*
SYSIN    DCB   DSORG=PS,DDNAME=SYSIN,MACRF=(GL),EODAD=SYSINEOF
*
DCBAREAL EQU   *-DCBAREA
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P R O G R A M   I N I T I A L I Z A T I O N                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING WORKAREA,R13
*
PGMINIT  ds    0h
*
*        OPEN  (SNAPDCB,(OUTPUT)),MODE=31
*
***********************************************************************
*  INITIALIZE PARAMETER AREA AND OVERLAY WITH OVERRIDES FROM "PARMS"  *
***********************************************************************
         L     R2,PARMDADR        SAVE  PARAMETER  LENGTH       ADDRESS
*
         LHI   R0,STDPARML+l'parmeyeb LOAD STANDARD PARAMETERS LENGTH
         GETMAIN R,LV=(0),LOC=(ANY)
         MVC   0(l'parmeyeb,R1),PARMEYEB         INITIALIZE EYEBALL
*
         la    R3,l'parmeyeb(,R1)
         ST    R3,PARMDADR        SAVE  PARAMETER  DATA  AREA   ADDRESS
         USING PARMDATA,R3
         MVC   parmdata(STDPARML),STDPARMS  INITIALIZE PARAMETER AREA
         drop  r3
*
*
***********************************************************************
*  INITIALIZE WORK AREA VARIABLES                                     *
***********************************************************************
         ZAP   SVRECCNT,P000      INITIALIZE PACKED   FIELDS
         ZAP   VIEWCNT,P000
         ZAP   EXTRCNT,P000
         ZAP   MSTRCNT,P000
         ZAP   DATACNT,P000
         ZAP   MARGCNT,P000
         ZAP   DOWNCNT,P000
         ZAP   LKUPcnt,P000
         ZAP   LKUPfnd,P000
         ZAP   LKUPnot,P000
         ZAP   VWoutftot,P000
         ZAP   VWoutfcnt,P000
*
         MVI   CRLF+0,X'0D'
         MVI   CRLF+1,X'25'

         MVI   ZEROES+0,C'0'
*
         MVC   ERRDATA,SPACES
*
         XC    vdpcnt_real,vdpcnt_real     initialise count of VDP recs
         XC    rthcount,rthcount           initialise count of RTH recs
*
***********************************************************************
*  LOCATE "TIOT"                                                      *
***********************************************************************
         LA    R14,TIOTADDR       GET  "TIOT" ADDRESS
         EXTRACT (R14),'S',FIELDS=TIOT
*
***********************************************************************
*  ALLOCATE AND INITIALIZE DCB AREAS                                  *
***********************************************************************
         LHI   R0,DCBAREAL+l'dcbeyeb GET DCB AREA STORAGE (RE-ENTRANT)
         GETMAIN R,LV=(0)
         MVC   0(l'dcbeyeb,R1),DCBEYEB
         AHI   R1,l'dcbeyeb

         lr    r2,r1              save storage address in r2
         LR    R0,R1              COPY MODEL DCB'S TO RE-ENTRANT AREA
         LHI   R1,DCBAREAL        (do this now so that DCBE addresses
         L     R14,DCBAREAA        in the next part are accurate)
         LR    R15,R1
         MVCL  R0,R14 mvcl destroys the registers so
         lr    r1,r2              restore the address
*
         L     R14,EXTRADDR
         S     R14,DCBAREAA
         AR    R14,R1
         ST    R14,EXTRDCBA
*
         LA    R0,EXTRDCBE-EXTRFILE(,R14)
         ST    R0,DCBDCBE-IHADCB(,R14)
*
         L     R14,PRNTADDR
         S     R14,DCBAREAA
         AR    R14,R1
         ST    R14,PRNTDCBA
*
         L     R14,logADDR
         S     R14,DCBAREAA
         AR    R14,R1
         ST    R14,logdcba
*
         L     R14,CTRLADDR
         S     R14,DCBAREAA
         AR    R14,R1
         ST    R14,CTRLDCBA
*
         L     R14,VDPADDR
         S     R14,DCBAREAA
         AR    R14,R1
         ST    R14,VDPDCBA
*
         L     R14,HDRADDR
         S     R14,DCBAREAA
         AR    R14,R1
         ST    R14,HDRDCBA
*
         L     R14,LKUPADDR
         S     R14,DCBAREAA
         AR    R14,R1
         ST    R14,LKUPDCBA
*
         L     R14,SYSINADR
         S     R14,DCBAREAA
         AR    R14,R1
         ST    R14,SYSINDCB
*
                     EJECT
***********************************************************************
*  OPEN LOG FILE                                                      *
***********************************************************************
         L     R2,LOGDCBA             LOAD  THE DCB  ADDRESS
         LAY   R14,STATOPEN
         MVC   RENTPARM(STATOPENL),0(R14)
         OPEN  ((R2),(OUTPUT)),MODE=31,MF=(E,RENTPARM)  OPEN CTL REPORT
         using ihadcb,r2
         if    TM,dcboflgs,dcbofopn,z      Failed to open ?
           WTO 'GVBMR88 - UNABLE TO OPEN LOG FILE (MR88LOG)'
           ABEND 020
         else
***********************************************************************
*   log file headings    - CALL GVBUTHDR                             *
***********************************************************************
           LAy R1,HDRLIST
           USING HEADERPR,R1    LOAD PARMS TO DSECT TO CALL
           lay     r5,typepgm       gvbuthdr
           sty     r5,pgmtype
           lay     r5,typepgml      gvbuthdr
           sty     r5,pgmtypel
           lay     r5,namepgm
           sty     r5,pgmname
           lay     r5,namepgml
           sty     r5,pgmnamel
           lay     r5,titpgm
           sty     r5,pgm_title
           lay     r5,titpgml
           sty     r5,pgm_title_ln
           lay     r5,rpt_88log
           sty     r5,rpt_title
           lay     r5,rpt_88logll
           sty     r5,rpt_title_ln
           lay     r5,rpt_dd88xl
           sty     r5,rptddn
           lay     r5,tempwork
           sty     r5,rpt_reccnt
           Ly  R3,ADDBUFF
           STy R3,BUFFADD
           LAy R0,BUFLGTH
           STy R0,BUFFLGTH
           DROP R1
*
           Llgf  R15,=v(gvbuthdr) Call GVBUTHDR
           BASsm R14,R15

***********************************************************************
*  Print log Report heading lines                                     *
***********************************************************************

           lh  r5,tempwork      Get number of lines in heading         +
                                  includes blank lines
static     loctr
           ds  0d
mvcprnt    mvc    prntline(0),2(r3)
cardmove   mvc   prntline(0),0(r1)
code       loctr
           do from=(r5)
             llh   r4,0(,r3)        get the length of the line
             lr    r14,r4           copy
             bctr  r4,r0            decrement for "EX"
             ex    r4,mvcprnt       move in header line
             la    r3,2(r14,r3)     move buffer pointer now
             ahi   r14,l'prntrdw      add rdw length in
             sll   r14,16           shift the length to left
             st    r14,prntrdw      set up the rdw
             Put   (r2),prntrdw     output header record
           enddo

         endif
*
***********************************************************************
*  OPEN CONTROL REPORT FILE                                           *
***********************************************************************
         L   R3,PARMDADR        SAVE  PARAMETER  LENGTH       ADDRESS
         USING PARMDATA,R3
         L     R2,CTRLDCBA             LOAD  THE DCB  ADDRESS
         LAY   R14,STATOPEN
         MVC   RENTPARM(STATOPENL),0(R14)
         OPEN  ((R2),(OUTPUT)),MODE=31,MF=(E,RENTPARM)  OPEN CTL REPORT
         TM    48(R2),X'10'                SUCCESSFULLY OPENED   ???
         BRO   PGMOPEN                     YES - BYPASS ABEND
         WTO   'GVBMR88 - UNABLE TO OPEN CONTROL REPORT FILE (MR88RPT)'
         ABEND 020
*
PGMOPEN  MVC   CTRLPUTA+1(3),DCBPUTA-IHADCB(R2)
         OC    CTRLPUTA,MODE31
*
***********************************************************************
*  SET FLAGS TO READ FROM EXTRACT FILE FIRST                          *
***********************************************************************
         MVI   EOREOFCD,C' '
         MVI   LASTFILE,C'E'        READ  EXTRACT FILE FIRST IN GVBMR88
*
         LA    R14,TP90AREA       INITIALIZE "GVBTP90" PARAMETER LIST
         ST    R14,TP90PA
         LA    R14,LKUPKEY
         ST    R14,TP90KEYA
*
         MVI   SALEAD0,C'0'
         MVC   SALEAD0+1(L'SALEAD0-1),SALEAD0
         MVI   SATRAIL0,C'0'
         MVC   SATRAIL0+1(L'SATRAIL0-1),SATRAIL0
*
         LAY   R14,DL96AREA       INITIALIZE "GVBDL96" PARAMETER LIST
         ST    R14,DL96PA
         LA    R14,DL96LEN
         ST    R14,DL96LENA
         LA    R14,DL96RTNC
         ST    R14,DL96RCA
*
         LA    R14,MR66AREA       INITIALIZE "STGMR66" PARAMETER LIST
         ST    R14,MR66PA
         MVC   PARMDDN,SPACES
*
         LHI   R0,LEINTLEN+8      OBTAIN  LANG ENV WORKAREA
         GETMAIN R,LV=(0),LOC=(ANY)
         MVC   0(8,R1),LEYEBALL   INITIALIZE "EYEBALL"
         AHI   R1,8
         USING LEINTER,R1
*
         ST    R1,LEWORKA         SAVE LE ENVIRONMENT WORK AREA  ADDR
*
         XC    LEINTER(LEINTLEN),LEINTER    ZERO WORK AREA
         DROP  R1
*
         BR    R10                RETURN
*
MVCPARMS MVC   0(0,R3),2(R2)      * * * * E X E C U T E D * * * *
static   loctr
         LTORG
code     loctr
*
         DROP  R3
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        R E A D   P A R A M E T E R   F I L E (MR88PARM)             *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING WORKAREA,R13
*
READPARM ds    0h
***********************************************************************
*  RETRIEVE PARMS FROM MR88PARM FILE RTC19876                         *
***********************************************************************
*
* Setup up current date as the default Run & Fiscal (ccyymmdd) date
*  will provide default dates if PARMS are not included
*
         xc    wrkarea,wrkarea
         lay   r4,wrkarea
         lay   r5,timelist
         TIME  BIN,(4),LINKAGE=SYSTEM,DATETYPE=YYYYMMDD,MF=(E,(5))
         lgf   r15,wrkarea+8      get the date
         sllg  r15,r15,4          Make it into
         stg   r15,wrkarea         a
         oi    wrkarea+7,x'0F'    packed number
         lay   R3,current_date
         unpk  0(8,R3),wrkarea+3(5)     Current date is the default
         lay   R2,run_date
         mvc   0(8,r2),0(r3)       save run_date default
         lay   R2,fiscal_date
         mvc   0(8,r2),0(r3)       save fiscal_date default
         mvc   SVPROCDT,0(r3)      save report proc date
         mvc   SVRUNDT,0(r3)       save report run date
         mvc   SVFINPDT(L'SVFINPDT),2(r3)  save report fin date
*
         xc    wrkarea,wrkarea
         xc    endtime,endtime
         lay   r4,wrkarea
         lay   r5,timelist
         TIME  DEC,(4),LINKAGE=SYSTEM,DATETYPE=YYYYMMDD,MF=(E,(5))
         unpk  endtime(13),wrkarea(7)
         lay   r4,current_time
         mvc   0(12,r4),endtime
         mvc   SVPROCTM(L'svproctm),0(r4)    save report run date
*
***********************************************************************
* OPEN PARAMETER FILE                                                 *
***********************************************************************
*
PARMLOAD larl  R2,PARMDCB       LOAD PARAMETER  FILE  DCB ADDRESS
         USING IHADCB,R2
         LAY   R1,STATOPEN
         MVC   RENTPARM(STATOPENL),0(R1)
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,RENTPARM) PARAMETER FILE
*
         mvc fiscal_default_cnt,=f'-1'   default fiscal date count
         xr  r1,r1
         sty r1,Run_date@               zero pointer
         sty r1,Fiscal_date@            zero pointer
         sty r1,Fiscal_date_ids@        zero pointer
  if TM,dcboflgs,dcbofopn,o         SUCCESSFUL ???
*
***********************************************************************
* READ "MR88" PARAMETERS UNTIL END-OF-FILE                            *
***********************************************************************
*   This is an infinite loop which exits via the EOF exit
*   to PARMEOF (label immediately following the enddo)
    Do  INF,LABEL=PARMLOOP
*
      get   (R2)
*
      LR    R4,R1                 SAVE RECORD ADDRESS
      LR    R5,R1                 LOAD RECORD ENDING   ADDRESS
      lh    r15,dcblrecl          get lrecl of parm file
      Ar    R5,r15                Point to end of record
***********************************************************************
* write the contents of MR88PARM to the control report MR88RPT
***********************************************************************
      bctr  r15,r0                set up for "EX"
      ex    r15,cardmove          Move record to PRNTLINE
      ahi   r15,5                 reset r15 to lrecl + RDW
      sth   r15,prntrdwh          save it
      rptit ,                     print parm card
*
      if  (CLI,0(R4),ne,C'*')     If not a comment then process
        MVC ERRDATA(l'parmkwrd),0(R4)
***********************************************************************
* SKIP LEADING SPACES BEFORE KEYWORD                                  *
***********************************************************************
        do inf
          DO while=(CLI,0(R4),eq,C' ')
            if (cr,r4,ge,r5)      check for end of record
              iterate PARMLOOP    and split if so
            else
              AHI R4,1            move along one character
            endif
          ENDDO
*
***********************************************************************
*   LOCATE END OF KEYWORD IN PARAMETER DATA STRING                    *
***********************************************************************
          LR       R1,R4
*
          do   while=(cr,r1,lt,r5)
            doexit (cli,0(r1),eq,c'='),or,(cli,0(r1),eq,c' ')
            AHI R1,1
          enddo
*
          LHI  R14,MSG#416           INVALID KEYWORD
          LR   R15,R1
          SR   R15,R4
          JNP  RTNERROR
*                                   LIMIT LENGTH TO KEYWORD MAXIMUM
          la   R14,PARMKWRD_table
          USING parmkwrd_map,R14
          if   chi,r15,gt,l'parmkwrd
            Lhi R15,l'parmkwrd
          endif
          bctr  R15,0
*
***********************************************************************
*   LOCATE KEYWORD IN TABLE                                           *
***********************************************************************
*
          MVC   ERRDATA,SPACES
          EX    R15,PARMERR
          do inf
            if (CLI,parmkwrd,eq,x'ff')
              LHI R14,MSG#416     INVALID KEYWORD IN MR88PARM FILE
              J   RTNERROR
            else
              EX  R15,PARMCLC
              doexit (e)
              AHI R14,parmkwrd_l
            endif
          enddo
*
*         AHI  R1,1               ADVANCE TO CHAR BEYOND "="
          LR   R4,R1
*
***********************************************************************
*   Check if character after parm was "=" or space                    *
*   does not give an error for mumtiple "=' signs                     *
***********************************************************************
*
* if character after parm was "=" look for non "=" & non space
*
           if (cli,0(r1),eq,c'=')
            do while=(cr,r1,lt,r5)
             if (cli,0(r1),eq,c' '),or,(cli,0(r1),eq,c'=')
              ahi r1,1
             else
              j  pl_value_a            r1 is at start of parm value
             endif
            enddo
            j  blnkerr                 invalid parm
           endif
*
* if character after parm was space check if "=" is present
* then look for non "=" or non space
*
           do while=(cr,r1,lt,r5)
            if (cli,0(r1),eq,c'=')
             j  pl_value_b     r1 at "=" sign
            else
             ahi r1,1
            endif
           enddo
           j  blnkerr          invalid parm
*
pl_value_b ds  0h
           do while=(cr,r1,lt,r5)
            if (cli,0(r1),eq,c'='),or,(cli,0(r1),eq,c' ')
             ahi r1,1
            else
             j  pl_value_a     r1 at start of parm value
            endif
           enddo
           j  blnkerr          invalid parm
*
blnkerr   ds   0H
          lhi  R14,MSG#416
          j    rtnerror
*
pl_value_a ds  0h
          lr   r4,r1              Save start of parameter value
*
***********************************************************************
*   LOCATE END OF PARAMETER VALUE                                     *
***********************************************************************
          do   while=(cr,r1,lt,r5) have we hit the end yet?
            doexit (cli,0(r1),eq,c' ')
            ahi r1,1
          enddo
*
          LR   R15,R1
          SR   R15,R4
          if (p) if r15 is positive
*
***********************************************************************
*   BRANCH TO KEYWORD SPECIFIC LOGIC                                  *
***********************************************************************
            bctr r15,0           Reduce this now
            LH R8,parmnum
*
            casentry r8
*
*            case 1
***********************************************************************
*         IGNORE CALCULATION OVERFLOW                                 *
***********************************************************************
*              L     R14,PARMDADR
*              USING PARMDATA,R14
*              MVC   IGNROVFL,0(R4)
*              DROP  R14
*
*            case 2
***********************************************************************
*         IGNORE DIVISION BY ZERO                                     *
***********************************************************************
*              L     R14,PARMDADR
*              USING PARMDATA,R14
*              MVC   ZERODIV,0(R4)
*              DROP  R14
*
*             case 3
***********************************************************************
*         NO HEADER RECORDS OPTION                                    *
* Parm not used - SORT_EXTRACT_FILE parm now dictates this
***********************************************************************
*              L     R14,PARMDADR
*              USING PARMDATA,R14
*              MVC   NOHDROPT,0(R4)
*              DROP  R14
*
              case 4
***********************************************************************
*         SORT EXTRACT FILE                                           *
***********************************************************************
               L     R14,PARMDADR
               USING PARMDATA,R14
               MVC   EXTROPT,0(R4)
               DROP  R14
*
              case 5
***********************************************************************
*         RUN_DATE                                                    *
***********************************************************************
                lhi r14,msg#447
                LA R0,L'Run_date-1
                SR R0,R15         See if correct length
                jnz rtnerror      N: then must be an error
                larl r14,trttblu   Load numeric test table address
                exrl r15,recidtrt check date is numeric
                jz  rundate_05    Y:
                lhi r14,msg#448   N: get error message
                j   rtnerror      Signal error
rundate_05  ds  0h
                lay R14,Run_date
                mvc  0(l'Run_date,r14),0(r4)
                sty r14,Run_date@  Signal we have a RUN_DATE parm
                if  (lt,r15,fiscal_date@,z)
                 lay r15,fiscal_date  save as default fiscal date
                 mvc 0(l'Run_date,r15),0(r4)
                endif
*
              case 6
***********************************************************************
*         FISCAL_DATE_DEFAULT                                         *
***********************************************************************
*
*srstnfnd    ds 0h                  Here to process date or not found
                lhi r14,msg#443     Multiple defaults error msg
                asi fiscal_default_cnt,1 if count is more than
                jh  rtnerror               zero then we have multiple
                lhi r14,msg#446
                chi r15,7           Is it a possible date (8-1)
                jne rtnerror
                lay   R14,trttblu   Load numeric test table address
                trt 0(8,r4),0(r14)   Numeric?
                jz    srstnfnd_13    Y: then continue
                lhi   r14,msg#444    N: then indicate error
                j     rtnerror
srstnfnd_13 ds 0h
                lay r15,Fiscal_date
                mvc 0(l'Fiscal_date,r15),0(r4) Save fiscal date
*RTC21202 indicator
                mvc svfinpdt(6),2(r15)
                sty r15,Fiscal_date@   signal there is a fiscal date
*
              case 7
***********************************************************************
*         FISCAL_DATE_OVERRIDE                                        *
***********************************************************************
*
static   loctr
          ds  0d
         using fiscal_date_entry,r3    current entry
recidtrt trt  0(0,r4),0(r14)   uses TRTTBLU
recidpk  pack  dblwork,0(0,r4)  * * * * E X E C U T E D * * * *
         drop r3
code     loctr
*
*         Search for character ":" in string
                la r0,c':'         Search for this
                la r6,1(r15,r4)    Point to after the string
                lr r14,r6          Save end of string address
srstloop        srst r6,r4         See string contains ":"
                jc 1,srstloop      CPU interrupt
                jc 2,srstnfnd      ":" not found
*
*         Here means ":" found and r6 points to it
*
*         Check length of control record id
                lhi r14,msg#445
                sr r6,r4           Get the length
                jnp rtnerror       length is 0 so must be an error
                lhi r7,max_fiscalid_digits Get max recid length
                sr  r7,r6           Is it too long?
                jm rtnerror         Y:
                bctr r6,r0         decrement length for "EX"
                larl R14,trttblu   Load numeric test table address
                ex r6,recidtrt       Numeric?
                jz    convert_recid     Y: then continue
                lhi   r14,msg#442    N: then indicate error
                j     rtnerror
srstnfnd    ds 0h                  Here if invalid overide date
                lhi   r14,msg#445   indicate error
                j     rtnerror
*
*         Convert char control record id to binary
convert_recid ds 0h
                ex  r6,recidpk    pack the value int dblwork
                cvb r7,dblwork     and make it binary

curf    using fiscal_date_entry,r3
prvf    using fiscal_date_entry,r1
                st r1,dblwork2
                lt r1,Fiscal_date_ids@  .1st one in MR88 parms?
                jnp alloc_fiscal_tabent .Y: then just allocate it

*
*        Check to see if this control record id is already in list
                lhi  r14,msg#441
recid_chklp ds  0h
                c r7,prvf.fiscal_recid  Same as previous one
                je rtnerror             Y: then parameter error
                lt r1,prvf.fiscal_next  Get the next entry
                jp recid_chklp          yes there is one

                lhi r0,fiscal_date_entry_len  get table entry size
                ahi r0,l'fisceyeb       add  eyeball   len
                st  r15,dblwork    Save r15 contents
                getmain RU,lv=(0),loc=(ANY) get fiscal table entry
                l   r15,dblwork    Save r15 contents
                mvc 0(l'fisceyeb,r1),fisceyeb    copy eyeball
                la r3,l'fisceyeb(,r1)    load base  register
                ly  r1,Fiscal_date_ide@  get last fiscal date
                sty r3,prvf.fiscal_next  save new on in prev last
                sty r3,Fiscal_date_ide@  make new the last on
                xc  curf.fiscal_next,curf.fiscal_next sero next
                st r7,curf.fiscal_recid  Save the new record id
                j srst_upd_lens

*
*         Allocate a control record fiscal date table entry
alloc_fiscal_tabent ds 0h
                lhi r0,fiscal_date_entry_len  get table entry size
                ahi r0,l'fisceyeb       add  eyeball   len
                st  r15,dblwork    Save r15 contents
                getmain RU,lv=(0),loc=(ANY) get fiscal table entry
                l   r15,dblwork    Restore r15 contents
                mvc 0(l'fisceyeb,r1),fisceyeb    copy eyeball
                la r3,l'fisceyeb(,r1)            load base  register
                sty r3,Fiscal_date_ids@ save start of fisc dates
                sty r3,Fiscal_date_ide@ save end of fisc dates
                xc  curf.fiscal_next,curf.fiscal_next zero next
                st r7,curf.fiscal_recid  Save the new record id
*
                drop prvf
*
*         Reset length in r15 and start of parameter in r4
srst_upd_lens ds 0h
                l  r1,dblwork2
                lhi r14,msg#446
                sr r15,r6          make the length
                ahi r15,-2            left correct
                jnp rtnerror       if not +ve then error in parameter
                ar r4,r6           make r4 point
                ahi r4,2              past the ':'
*
*         We are here if the user has specified
*            FISCAL_DATE_OVERRIDE=recid:ccyymmdd
*         This specifying a value for a specific control record
*
                chi r15,7           Is it a possible date (8-1)
                jne rtnerror
                larl  R14,trttblu   Load numeric test table address
                trt 0(8,r4),0(r14)   Numeric?
                jz    srstnfnd_03    Y: then continue
                lhi   r14,msg#444    N: then indicate error
                j     rtnerror
srstnfnd_03 ds 0h
                mvc curf.fiscal_iddate,0(r4) Save fiscal date in table
                j   fiscexit
fiscexit    ds 0h
                drop curf
*
              case 9
***********************************************************************
*         Abend on message number                                     *
***********************************************************************
               LHI   R14,MSG#417      INVALID PARAMETER VALUE
               LA    R0,L'MSGABEND-1
               SR    R0,R15           COMPUTE RIGHT JUSTIFY LENGTH
               JM    RTNERROR         ERROR IF  TOO LONG
               L     R14,PARMDADR
               USING PARMDATA,R14
               LA    R14,MSGABEND
               AR    R14,R0
               EX    R15,PARMMVC
*
               LARL  R15,TRTTBLU    LOAD NUMERIC CLASS TEST TBL ADDR
               L     R14,PARMDADR
               if TRT,msgabend,0(R15),z  NUMERIC ???
                 pack dblwork,msgabend   Pack the number
                 cvb  r0,dblwork   convert to Binary
                 sty  r0,abend_msg and save in thrd area
               else
                 LHI  R14,MSG#417  NO - INDICATE  ERROR
                 J    rtnerror
               endif
               DROP  R14
*
             endcase
*
          else ,
*
            LHI R14,MSG#417      INVALID PARAMETER VALUE
            b  RTNERROR          r15 is 0 or negative - split
          endif ,
***********************************************************************
*   ADVANCE TO NEXT PARAMETER KEYWORD                                 *
***********************************************************************
          la    r4,1(,r1)         advance past parm value
          doexit (cli,0(r4),eq,c' '),or,(cr,r1,ge,r5)
        enddo
      endif ,                     comment test
    enddo ,                       infinite loop (PARMLOOP)
*
  endif          End PARM File Open Success
*
*
***********************************************************************
* EOF  FOR PARAMETER FILE                                             *
***********************************************************************
PARMEOF  CLOSE ((R2)),MODE=31,MF=(E,RENTPARM)
         MVC  ERRDATA,SPACES           BLANK OUT INDICATIVE ERROR DATA
***********************************************************************
*rtc19876 Check parms passed in MR88PARM file                         *
***********************************************************************
         L     R3,PARMDADR
         USING PARMDATA,R3
***********************************************************************
*  CHECK EXTRACT RECORD SOURCE CODE                                   *
***********************************************************************
         if (cli,extropt,ne,C'N'),and,(cli,extropt,ne,C'Y')
          LHI   R14,MSG#433
          B     RTNERROR
         endif
*
***********************************************************************
*  Set "NO HEADER RECORDS" flag based on whether sorted or not        *
***********************************************************************
         if (cli,extropt,eq,C'Y') Extract records need SORT?
           MVI NOHDROPT,C'N'      Yes, then we have header recs
         else
           MVI NOHDROPT,C'Y'      Already sorted - no header recs
         endif
*
***********************************************************************
*  CHECK "IGNORE CALCULATION OVERFLOW" OPTION                         *
***********************************************************************
         if (cli,IGNROVFL,ne,c'N'),and,(cli,IGNROVFL,ne,c'Y')
          LHI   R14,MSG#435
          B     RTNERROR
         endif
*
***********************************************************************
*  CHECK "ZERO DIVIDE ABEND" OPTION                                   *
***********************************************************************
         if (cli,ZERODIV,ne,c'N'),and,(cli,ZERODIV,ne,c'Y')
          LHI   R14,MSG#440
          B     RTNERROR
         endif
*
         BR    R10                RETURN
         drop  r3                 PARMDATA
*
static   loctr
         using parmkwrd_map,R14
PARMCLC  CLC    0(0,R4),parmkwrd   * * * * E X E C U T E D * * * *
PARMMVC  MVC    0(0,R14),0(R4)     * * * * E X E C U T E D * * * *
PARMERR  MVC    ERRDATA(0),0(R4)   * * * * E X E C U T E D * * * *
         drop r14
*
* Parameter keyword table should be kept in alphabetical order
*
PARMKWRD_table dc 0H
*
*parmk01  dc    0c
*parmk01l dc    al2(parmk01p-parmk01v-1)          length
*parmk01v dc    C'ABEND_ON_CALCULATION_OVERFLOW'  value
*parmk01p dc    cl(l'parmkwrd-parmk01p+parmk01v)' '  pad to l'parmkwrd
*         dc    h'01'                             case number
*parmk01e equ   *
*
*parmk02  dc    0c
*parmk02l dc    al2(parmk02p-parmk02v-1)
*parmk02v dc    C'ABEND_ON_DIVISION_BY_ZERO    '
*parmk02p dc    cl(l'parmkwrd-parmk02p+parmk02v)' '
*         dc    h'02'
*parmk02e equ   *
*
parmk09  dc    0c
parmk09l dc    al2(parmk09p-parmk09v-1)
parmk09v dc    C'ABEND_ON_MESSAGE_NBR         '
parmk09p dc    cl(l'parmkwrd-parmk09p+parmk09v)' '
         dc    h'09'
parmk09e equ   *
*
parmk06  dc    0c
parmk06l dc    al2(parmk06p-parmk06v-1)
parmk06v dc    C'FISCAL_DATE_DEFAULT          '
parmk06p dc    cl(l'parmkwrd-parmk06p+parmk06v)' '
         dc    h'06'
parmk06e equ   *
*
parmk07  dc    0c
parmk07l dc    al2(parmk07p-parmk07v-1)
parmk07v dc    C'FISCAL_DATE_OVERRIDE         '
parmk07p dc    cl(l'parmkwrd-parmk07p+parmk07v)' '
         dc    h'07'
parmk07e equ   *
*
parmk05  dc    0c
parmk05l dc    al2(parmk05p-parmk05v-1)
parmk05v dc    C'RUN_DATE                     '
parmk05p dc    cl(l'parmkwrd-parmk05p+parmk05v)' '
         dc    h'05'
parmk05e equ   *
*
parmk03  dc    0c
parmk03l dc    al2(parmk03p-parmk03v-1)
parmk03v dc    C'PROCESS_HEADER_RECORDS       '
parmk03p dc    cl(l'parmkwrd-parmk03p+parmk03v)' '
         dc    h'03'
parmk03e equ   *
*
parmk04  dc    0c
parmk04l dc    al2(parmk04p-parmk04v-1)
parmk04v dc    C'SORT_EXTRACT_FILE            '
parmk04p dc    cl(l'parmkwrd-parmk04p+parmk04v)' '
         dc    h'04'
parmk04e equ   *
*
parmkFF  dc    0c
         dc    al2(0)
         dc    XL4'FFFFFFFF'
*
parmkwrd_map   dsect                       MR88PARM keyword
parmkent       ds    0h
parmkll        ds    h                     length
parmkwrd       ds    cl30                  value padded with spaces
parmnum        ds    h                     case number
parmkwrd_l     equ   *-parmkent
*
gvbmr87  csect
*
code     loctr
*
         EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P R I N T   C O N T R O L   R E P O R T   H E A D I N G S    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING WORKAREA,R13
*
HEADINGS ds    0h
         L     R3,PARMDADR        SAVE  PARAMETER  LENGTH       ADDRESS
         USING PARMDATA,R3
***********************************************************************
* CONTROL REPORT HEADER - CALL GVBUTHDR                               *
***********************************************************************
         LAy   R1,HDRLIST
         USING HEADERPR,R1      LOAD PARMS TO DSECT TO CALL
         lay   r5,typepgm       gvbuthdr
         sty   r5,pgmtype
         lay   r5,typepgml      gvbuthdr
         sty   r5,pgmtypel
         lay   r5,namepgm
         sty   r5,pgmname
         lay   r5,namepgml
         sty   r5,pgmnamel
         lay   r5,titpgm
         sty   r5,pgm_title
         lay   r5,titpgml
         sty   r5,pgm_title_ln
         lay   r5,rpt_88ctrl
         sty   r5,rpt_title
         lay   r5,rpt_88ctrll
         sty   r5,rpt_title_ln
         lay   r5,rpt_dd88xc
         sty   r5,rptddn
         lay   r5,tempwork
         sty   r5,rpt_reccnt

         Ly    R3,ADDBUFF
         STy   R3,BUFFADD
         LAy   R5,BUFLGTH
         STy   R5,BUFFLGTH
         DROP R1
*
         Llgf  R15,=v(gvbuthdr) Call GVBUTHDR
         BASsm R14,R15

static   loctr
***********************************************************************
*   PARAMETERS FOR CALLING GVBUTHDR                                   *
***********************************************************************
TYPEPGM  DC    C'Base Product'
TYPEPGMl DC    al2(typepgml-typepgm)
NAMEPGM  DC    c'GVBMR88'
namePGMl DC    al2(namepgml-namepgm)
titpgm   dc    c'View Format Process'
titpgml  dc    al2(titpgml-titpgm)
rpt_88ctrl  dc c'GVBMR88 Control Report'
rpt_88ctrll dc al2(rpt_88ctrll-rpt_88ctrl)
rpt_dd88xc dc   cl8'MR88RPT'
rpt_88log  dc c'GVBMR88 Log'
rpt_88logll dc al2(rpt_88logll-rpt_88log)
rpt_dd88xl dc   cl8'MR88LOG'
VERSBLD  DC    CL8'&SYSPARM'
ADDBUFF  DC    A(BUFFER)
BUFLGTH  DC    A(L'BUFFER)
HDRLIST  DS    XL(HEADERPR_L)
nwline   equ   c'1'
*****
at_end   loctr
         ds    0d
BUFFER   DS    CL1200             MESSAGE BUILD AREA
code     loctr
*
***********************************************************************
*  Print Control Report heading lines - RECFM=VB                      *
***********************************************************************
*
         lh    r5,tempwork      Get number of lines in heading
*                                 includes blank lines
         L     R2,CTRLDCBA      get the DCB
*
         do from=(r5)
           llh   r4,0(,r3)        len of line in 1st 2 bytes of buffer
           lr    r14,r4           copy
           bctr  r4,r0            decrement for "EX"
           ex    r4,mvcprnt       move in header line
           la    r3,2(r14,r3)     move buffer pointer to next entry
           ahi   r14,l'prntrdw      add rdw length in
           sll   r14,16           shift the length to left
           st    r14,prntrdw      set up the rdw
           Put   (r2),prntrdw     output header record
         enddo
***********************************************************************
*  PRINT MR88RPT "Report Sections"                          RTC19877  *
***********************************************************************
         phead hd=cont            Print heading for this section
         rptit msg=rptcont_hd1    column headings
         rptit msg=rptcont_hd2    column  underlining
         xc   prntrdwl,prntrdwl
         mvc  prntline(4),spaces
         pcont cont=prm88
         pcont cont=opts
         pcont cont=iref
         pcont cont=irun
         pcont cont=isrc
         pcont cont=ofil
         pcont cont=exec
         rptit msg=vb_blankl
*
* Print the contents of MR88PARM
*
         phead hd=prm88           Print heading for the section
*
* The data for this section is written to MR88RPT as the parameter
* file MR88PARM is read in the READPARM routine
*
         BR    R10                RETURN
*
static   loctr
*
equals   dc    cl80'===================================================x
               ============================='
vb_blankl dc   0cl(vb_ble-vb_bls)
vb_bls    dc   al2(vb_ble-vb_bls),al2(0)
          dc   c' '
vb_ble    equ  *
*
code     loctr
*
         DROP  R3
         DROP  R13
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        W R I T E   T O   C O N T R O L   R E P O R T                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
PRINTRT1 DS    0H
         USING WORKAREA,R13
*        USING IHADCB,R2
*        l     r2,ctrldcba           Address the ctrl report DCB
**********************************************************************
*    OPTS - Options in effect
**********************************************************************
         rptit msg=vb_blankl
         phead hd=opts               Write OPTS header
*
         L     R4,PARMDADR
         USING PARMDATA,R4
*
* Loop through Parmkwrd_table (it's in alphabetical order)
*
         la    R3,PARMKWRD_table
         USING parmkwrd_map,R3
*
         do while=(CLI,parmkwrd,ne,x'ff')
*
* Use 'case' here so the order is determined by Parmkwrd_table and code
* Does not need to be shuffled around if this changes
*
               mvc   prntline(l'parmkwrd),parmkwrd
               mvi   prntline+l'parmkwrd,c'='
               mvi   prntline+l'parmkwrd+1,c' '
            LH R8,parmnum
            casentry r8
*
*             case 1                    IGNORE_CALCULATION_OVERFLOW
*              mvc   prntline+l'parmkwrd+2(l'IGNROVFL),IGNROVFL value
*              la    r15,l'parmkwrd+2+l'IGNROVFL+l'prntrdw len for RDW
*              sth   r15,prntrdwh
*              rptit ,                       write to report
*
*             case 2                    IGNORE_DIVISION_BY_ZERO
*              mvc   prntline+l'parmkwrd+2(l'ZERODIV),ZERODIV  value
*              la    r15,l'parmkwrd+2+l'ZERODIV+l'prntrdw len for RDW
*              sth   r15,prntrdwh
*              rptit ,                       write to report
*
*             case 3                    PROCESS_HEADER_RECORDS
*                      old keyword      SKIP_HEADER_RECORD_VERIFICATION
*              mvc   prntline+l'parmkwrd+2(l'NOHDROPT),NOHDROPT value
*              la    r15,l'parmkwrd+2+l'NOHDROPT+l'prntrdw len for RDW
*              sth   r15,prntrdwh
*              rptit ,                       write to report
*
              case 4                    SORT_EXTRACT_FILE
               mvc   prntline+l'parmkwrd+2(l'EXTROPT),EXTROPT value
               la    r15,l'parmkwrd+2+l'EXTROPT+l'prntrdw len for RDW
               sth   r15,prntrdwh
               rptit ,                       write to report
*
              case 5                    Run_date
               lay   r15,run_date
               mvc   prntline+l'parmkwrd+2(l'run_date),0(r15)
               la    r15,l'parmkwrd+2+l'run_date+l'prntrdw len for RDW
               sth   r15,prntrdwh
               rptit ,                       write to report
*
              case 6                      Fiscal_date_default
*RTC21113
                lay  r15,Fiscal_date  print ccyymmdd fmt fiscal date
                mvc  prntline+l'parmkwrd+2(l'Fiscal_date),0(R15)
                la   r15,l'parmkwrd+2+l'Fiscal_date+l'prntrdw len RDW
                sth  r15,prntrdwh
                rptit ,                       write to report
*
              case 7                      Fiscal_date_override
*
* Loop through the Fiscal Dates table to print the fiscal date opts
*   set for each control record
*
                ds   0h
                lt   r5,Fiscal_date_ids@  check if user specified
                jnp  Fis_loopx         .N: then exit this loop
                using Fiscal_date_entry,r5
*
Fis_loop        ds   0h
                xgr  r0,r0
                l    R0,fiscal_recid      Get the record id
                cvd  r0,dblwork
                mvc  tempwork(12),recidmsk
                edmk tempwork(12),dblwork+2
*
                la   r15,tempwork+11      point to end of record id
                sr   r15,r1               length of number -1
*
                la   r2,prntline+2+l'parmkwrd
                ex   r15,mvcfiscalr       move fiscal_recid
                la   r2,1(r15,r2)         point after recid
                mvi  0(r2),c':'
                mvc  1(8,r2),fiscal_iddate  get the fiscal_date
                la   r15,l'parmkwrd+12+l'prntrdw(r15) set rdw lengh
                sth  r15,prntrdwh         set rdw
*
                rptit ,
*
                L    R0,fiscal_next
                jp   fis_loop
fis_byp         ds    0H       End Case 7
*
                drop r5
*
              case 9                    Abend on message#
               if (lt,r0,abend_msg,nz)
                mvc   prntline+l'parmkwrd+2(l'MSGABEND),MSGABEND value
                la    r15,l'parmkwrd+2+l'MSGABEND+l'prntrdw len RDW
                sth   r15,prntrdwh
                rptit ,                       write to report
               endif
*
*
            endcase  ,
*
Fis_loopx   AHI R3,parmkwrd_l
*
         enddo              End of looping through Parmkwrd_table

         rptit msg=vb_blankl
*
         BR    R10                RETURN
***
static   loctr
         ds    0d
mvcprmk  mvc   prntline(0),parmkwrd
mvcfiscalr mvc  0(0,r2),0(r1)     move record id
recidmsk DC    X'402020202020202020202021'       Record id mask
COUNTMSK DC    X'402020206B2020206B202120'       Record count mask
Mem_mask DC    X'4020206b2020206B2020206B202120' Memory usage mask
         ASSERT l'irunrcnt,eq,l'countmsk
         ASSERT l'irefrcnt,eq,l'countmsk
         ASSERT l'irefmemu,eq,l'mem_mask
code     loctr
***
         drop r3,r4
         DROP  R13
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        W R I T E   T O   C O N T R O L   R E P O R T                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
PRINTRT2 DS    0H
         USING WORKAREA,R13
**********************************************************************
*    IRUN - Input run-control file summary
**********************************************************************
         phead hd=irun               Write IRUN header
         rptit msg=rptirun_hd1       column headings
         rptit msg=rptirun_hd2       column  underlining
*
         lhi   r0,irunrept_len+4
         sth   r0,prntrdwh           Set length in VB record
rp1      using irunrept,prntline     dsect for IRUN section data
         mvc   rp1.irundd,=cl8'MR88VDP'
         mvc   rp1.irunf1,spaces
         mvc   rp1.irunf2,spaces
         mvc   rp1.irunf3,spaces
         mvc   rp1.irunf4,spaces
         ly    R0,vdpcnt_real
         cvd   r0,dblwork
         mvc   rp1.irunrcnt,countmsk
         ed    rp1.irunrcnt,dblwork+3  display VDP record count
* get the date from VDP0001_date
         lay   r15,vdp_date
         mvc   rp1.irundate,0(r15)
* get the time from VDP0001_time
         lay   r15,vdp_time
         mvc   rp1.iruntime,0(r15)
* get the desc from VDP0001_description
         lay   r15,vdp_desc
         mvc   rp1.iruncrby,0(r15)
         rptit ,
*
         mvc   rp1.irundd,=cl8'MR88RTH'
         ly    R0,rthcount
         cvd   r0,dblwork
         mvc   rp1.irunrcnt,countmsk
         ed    rp1.irunrcnt,dblwork+3
         mvc   rp1.irundate(irunrept_len-(irundate-irunrept)),spaces
         rptit ,
         drop  rp1
*
         rptit msg=vb_blankl
*
         BR    R10                RETURN
         DROP  R13
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        O P E N   S T A N D A R D   G E N E V A   F I L E S          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING WORKAREA,R13
         USING IHADCB,R2
*
OPENSTD  ds    0h

*        L     R2,PRNTDCBA                 LOAD   THE   DCB     ADDRESS
*  NOTE: PRNTDCBA is allocated to allow for other code to work
*        that relies on the DCB address being valid
*        DO NOT OPEN this DCB
*
OPENEOFM MVI   MSTREOF,C'Y'         INDICATE NO MASTER RECORDS
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* CHECK OPTIONAL SOURCES FOR EXTRACT RECORDS (SORT EXIT, MR95 DIRECT) *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         L     R2,EXTRDCBA          LOAD  DCB      ADDRESS
*
* RTC19876 valid options for EXTROPT are Y (recs from sort)
*          and N (recs are sorted)
*
         L     R14,PARMDADR         CHECK PARAMETER OPTION
         CLI   EXTROPT-PARMDATA(R14),C'Y' EXTRACT  RECORDS FROM SORT ??
         JE    OPENSORT             YES - INITIALIZE  SORT INTERFACE
         j     OPENEXTR             NO  - BYPASS   SPECIAL LOGIC
*
OPENSORT LARL  R0,SORTINIT          OVERRIDE BSAM READ ROUTINE ADDRESS
         O     R0,MODE31            SAVE  SORT EXIT  INTERFACE ADDRESS
         ST    R0,EXTRCHKA
         STCM  R0,B'0111',DCBCHCKA
*
         MVC   DCBBLKSI,H8K         OVERRIDE  BLOCK  SIZE
*
OPENNOP  LARL  R0,GETBR14           NOP   READ ROUTINE ADDRESS (BR 14)
         O     R0,MODE31
         ST    R0,EXTRGETA
         STCM  R0,B'0111',DCBGETA
*
         LARL  R0,OPENEOFE          SET  END-OF-FILE  EXIT  ADDRESS
         STCM  R0,B'0111',DCBEODA
*
         LHI   R15,128              ALLOCATE 128 BUFFERS
         j     OPENBUFR
*
GETBR14  BR    R14                  NOP   SUBROUTINE
*
OPENEOFE MVI   EXTREOF,C'Y'         INDICATE NO EXTRACT RECORDS
         j     OPENRETN
                     SPACE 3
OPENEXTR LHI   R0,RDNDECB           USE  "MULTACC" OPTION
         STC   R0,DCBNCP
         LHI   R0,RDNDECB/2
         L     R1,DCBDCBE
         STC   R0,DCBEMACC-DCBE(R1)
*
         LAY   R14,STATOPEN
         MVC   RENTPARM(STATOPENL),0(R14)
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,RENTPARM) OPEN EXTRACT FILE
         TM    48(R2),X'10'         OPEN  SUCCESSFUL ???
         BRO   OPENSETE             YES - SET  "GET" SUBROUTINE ADDRESS
         LHI   R14,MSG#436          LOAD  ERROR MESSAGE NUMBER
         j     RTNERROR
*
OPENSETE MVC   EXTRGETA+1(3),DCBGETA
         OC    EXTRGETA,MODE31
         MVC   EXTRCHKA+1(3),DCBCHCKA
         OC    EXTRCHKA,MODE31
*
         LArl  R14,OPENEOFE        SET END-OF-FILE  EXIT   ADDRESS
         STCM  R14,B'0111',DCBEODA
*
         LHI   R15,RDNDECB        LOAD NUMBER   OF  INPUT  BUFFERS
                     SPACE 3
OPENBUFR LH    R6,DCBBLKSI        LOAD PHYSICAL BLOCKSIZE
         AHI   R6,7               ROUND TO NEXT DOUBLEWORD
         SRL   R6,3
         SLL   R6,3
         LR    R5,R15             COMPUTE BUFFER POOL SIZE
         MR    R4,R6
*
         LHI   R14,MDLREADL+4     LOAD DECB  +  PREFIX LENGTH
         MR    R14,R14            (R15 CONTAINS # OF BUFFERS)
         LR    R4,R15
*
         LR    R0,R4
         GETMAIN R,LV=(0)         OBTAIN DECB LIST
         ST    R1,EXTRDECB        POINT  TO  FIRST DECB + BUFFER
         AR    R4,R1              SAVE   END OF DECB LIST (+1)
         LR    R3,R1              LOAD CURRENT DECB ADDRESS
                     SPACE 3
         LR    R0,R5
         GETMAIN RU,LV=(0),LOC=(ANY),BNDRY=PAGE  OBTAIN  BUFFERS
         LR    R5,R1              LOAD FIRST BUFFER ADDRESS
                     EJECT
EXTDECBL LA    R14,MDLREADL+4(,R3)   LOAD ADDRESS OF   NEXT DECB
         CR    R14,R4             ADDRESS BEYOND  LAST LIST ENTRY ???
         JL    EXTDECBQ           NO  - USE IT (BYPASS NEXT INSTR)
         L     R14,EXTRDECB       YES - POINT  TO FIRST
*
EXTDECBQ EQU   *
         ST    R14,0(,R3)         CHAIN DECB   PREFIXES TOGETHER
*
         MVC   4(MDLREADL,R3),MDLREAD   COPY   MODEL    DECB
         MVC   4+6(2,R3),DCBBLKSI  SAVE BLOCK  SIZE     IN DECB
         ST    R2,4+8(,R3)         SAVE DCB    ADDRESS  IN DECB
         ST    R5,4+12(,R3)        SET  BUFFER ADDRESS  IN DECB
*
         LA    R1,4(,R3)          POINT TO  DECB (FOLLOWING PREFIX)
         XC    0(4,R1),0(R1)      CLEAR ECB
         L     R15,EXTRGETA       LOAD  READ     SUBROUTINE ADDRESS
         BASR  R14,R15            READ  PHYSICAL BLOCK  (31-BIT)
*
         AR    R5,R6              LOAD  NEXT BUFFER  ADDR
         CLC   0(4,R3),EXTRDECB   LAST  DECB STARTED  ???
         L     R3,0(,R3)          LOAD  NEXT DECB ADDRESS (ASSUME LOOP)
         JNE   EXTDECBL           NO  - LOOP THROUGH ALL DECB'S
                     SPACE 3
         L     R3,EXTRDECB        LOAD   FIRST DECB PREFIX  ADDRESS
         LA    R1,4(,R3)          POINT  TO    DECB
         L     R15,EXTRCHKA       I/O    COMPLETED  SUCCESSFULLY   ???
         BASR  R14,R15            CALL CHECK ROUTINE (31-BIT)
*
         L     R14,4+12(,R3)      LOAD BUFFER ADDRESS
         LH    R0,0(,R14)         LOAD BLOCK  LENGTH
         AR    R0,R14
         ST    R0,EXTREOD
         AHI   R14,4              SKIP "BDW"
         ST    R14,EXTRRECA       INITIALIZE  FIRST RECORD ADDRESS
*
         AP    EXTRCNT,P001       INCREMENT TRANSACTION RECORD COUNT
                     SPACE 3
OPENRETN BR    R10                RETURN
*
static   loctr
         LTORG
code     loctr
         DROP  R2
         DROP  R13
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*    I N I T I A L I Z E   L A N G U A G E   E N V I R O N M E N T    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING WORKAREA,R13
*
LERUNTIM ds    0h                 LOAD SUBROUTINE BASE  REGISTER
*
         CLI   LANGENV,C'Y'       ANY  LE  COBOL  PROGRAMS   ???
         JNE   LEEXIT             NO  -   BYPASS  INITIALIZATION
*
         L     R2,LEWORKA         LOAD LE ENVIRONMENT  WORK AREA ADDR
         USING LEINTER,R2
*
         LOAD  EP=CEEPIPI
         ST    R0,LECEEADR
*
         LArl  R0,PIPICALL        BUILD SUBROUTINE CALL PARAMETER LIST
         ST    R0,LEFUNCA
         LA    R0,LESUBADR
         ST    R0,LESUBRA
         LA    R0,LETOKEN
         ST    R0,LETOKNA
         LA    R0,LEPARMP
         ST    R0,LEPARMA
         LA    R0,SAVEMR66        "STEAL" AREA FOR PARAMETER LIST
         ST    R0,LEPARMP
         LA    R0,LERTNC
         ST    R0,LERTNCA
         LA    R0,LEREASON
         ST    R0,LEREASA
         LA    R0,LEFDBK
         ST    R0,LEFDBKA
         OC    LEFDBKA,MODE31
*
         LArl  R0,PIPIINIT        BUILD INITIALIZATION PARAMETER  LIST
         ST    R0,SAVEMR66+00
         LArl  R0,PIPIPTR
         ST    R0,SAVEMR66+04
         LArl  R0,SVCSPTR
         ST    R0,SAVEMR66+08
         LArl  R0,PIPIRTIM
         ST    R0,SAVEMR66+12
         LA    R0,LETOKEN
         ST    R0,SAVEMR66+16
         OC    SAVEMR66+16,MODE31
*
         LA    R1,SAVEMR66        CALL  COBOL  INITIALIZATION  FUNCTION
         L     R15,LECEEADR
         BASR  R14,R15
         LTR   R15,R15
         JNZ   LEERROR1
*
         LARL  R0,PIPISEQ         BUILD START  SEQUENCE PARAMETER LIST
         ST    R0,SAVEMR66+00
         LA    R0,LETOKEN
         ST    R0,SAVEMR66+04
         OC    SAVEMR66+04,MODE31
*
         LA    R1,SAVEMR66        CALL  COBOL  START SEQUENCE  FUNCTION
         L     R15,LECEEADR
         BASR  R14,R15
         LTR   R15,R15
         JNZ   LEERROR2
*
LEEXIT   BR    R10                RETURN
*
LEERROR1 DS    0H
         LHI   R14,MSG#437
         j     LEERRORX
*
LEERROR2 DS    0H
         LHI   R14,MSG#432
*
LEERRORX DS    0H
         ST    R15,DBLWORK2+4
         UNPK  DBLWORK(9),DBLWORK2+4(5)
         TR    DBLWORK,HEXTAB
         MVC   ERRDATA(8),DBLWORK
         j     RTNERROR
                     SPACE 3
PIPISEQ  DC    F'07'
PIPIINIT DC    F'09'               (WAS "3")
PIPICALL DC    F'10'
PIPIPTR  DC    A(PIPIPTBL)
SVCSPTR  DC    A(0)
                     SPACE 1
PIPIPTBL CEEXPIT
         CEEXPITY ,0
         CEEXPITS
                     SPACE 3
PIPIRTIM DC    C'RPTOPTS(OFF),RPTSTG(OFF),'
         DC    C'TRAP(OFF),TER(UADUMP),'
         DC    C'ALL31(ON),ANYHEAP(16K,8K,ANY,FREE),'
         DC    C'BELOWHEAP(2K,2K,FREE),'
         DC    C'HEAP(32K,100K,ANY,KEEP,100K,100K),'
         DC    C'LIBSTACK(2K,2K,FREE),STACK(16K,16K,ANY,KEEP),'
         DC    C'NONONIPTSTACK(),'
         DC    C'STORAGE(NONE,NONE,NONE,2K),'
         DC    C'THREADHEAP(8K,32K,ANY,KEEP)'
PIPIRLEN EQU   (256-(*-PIPIRTIM))
         DC    (PIPIRLEN)CL1' '
                     SPACE 1
         DROP  R2
         DROP  R13
         DROP  R11
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        E 3 5   S O R T   E X I T   (C A L L E D   B Y   S O R T)    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         using saver,r13
SORTE35  STM   R14,R12,savgrs14   SAVE  SORT'S  REGISTER    CONTENTS
*
         L     R0,0(,R1)          LOAD  RECORD  ADDRESS
         LTR   R0,R0              END-OF-FILE   ???
         JP    SORTOUT            NO  - PASS    RECORD  TO  "GVBMR88"
*
         LHI   R15,8              RETURN  (DO NOT CALL AGAIN)
         BR    R14
*
SORTOUT  LR    R14,R13            SAVE  SORT'S  RSA  ADDRESS
         L     R13,8(,R1)
         USING WORKAREA,R13
         ST    R14,SORTRSA
*
***********************************************************************
*  EMULATE A BSAM READ, E.G. POST ECB AND COPY DATA TO BUFFER         *
***********************************************************************
         LR    R14,R0             LOAD  RECORD  ADDRESS
         OC    srttime2,srttime2
         JNZ   SORTE35_BYP
         LA    R2,srttime2        address the work
         la    r5,timelist            areas
         LR    R3,R14
         TIME  STCK,(R2),LINKAGE=SYSTEM,DATETYPE=YYYYMMDD,             +
               MF=(E,(R5))
         LR    R14,R3
SORTE35_BYP DS 0H
         LH    R15,0(,R14)        LOAD  RECORD  LENGTH
*
         L     R1,EXTRDECB        LOAD  DECB    PREFIX    ADDRESS
         MVI   4(R1),X'7F'        POST  ECB
         L     R1,4+12(,R1)       LOAD  BUFFER  ADDRESS
         LA    R0,4(,R15)         BUILD "BDW"
         STH   R0,0(,R1)
         XC    2(2,R1),2(R1)
         LA    R0,4(,R1)          COPY  RECORD  TO  BSAM  BUFFER
         LR    R1,R15
         MVCL  R0,R14
*
*        BASR  R10,0
*        USING *,R10
*
*        L     R9,EXTRDECB
*        L     R15,4+12(,R9)
*        AH    R15,0(,R15)
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=100,STORAGE=((R9),(R15))
*
         using saver,savesort
         LM    R14,R12,savgrs14   RESTORE "GVBMR88"  REGISTERS
         BSM   0,R14
*
*        DROP  R10
         DROP  R13
                     SPACE 5
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        E 3 5   R E T U R N   T O   S O R T                          *
*                                                                     *
*        (THIS CODE EXECUTES AS AN EXTENSION OF GVBMR88)              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING WORKAREA,R13
         using saver,savesort
*
E35RETRN STM   R14,R12,savgrs14        SAVE "GVBMR88" REGISTER CONTENTS
*
         cli   statflg3,x'ff'          error condition ?
         be    E35RETRP
         j     e35retrq
E35RETRP EQU   *
         mvi   statflg3,x'00'          reset error condition
         LHI   R15,16                  SET  RC=16 (terminate DFSORT)
         j     e35retrr
E35RETRQ EQU   *
         LHI   R15,4                   SET  RC=4  (DELETE RECORD)
E35RETRR EQU   *
         L     R13,SORTRSA             RESTORE SORT'S REGISTER CONTENTS
         using saver,r13
         ST    R15,savgrs14+4
         LM    R14,R12,savgrs14
         BSM   0,R14
*
         DROP  R13
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*    CALL AN "LE/COBOL" FORMAT EXIT VIA COMMON LANGUAGE INTERFACE     *
*                                                                     *
*        R15 - ENTRY     POINT   ADDRESS                              *
*        R14 - RETURN    ADDRESS                                      *
*        R13 - "GVBMR88" WORK    AREA    ADDRESS                      *
*        R1  - PARAMETER LIST    ADDRESS                              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING WORKAREA,R13
         USING VIEWREC,R8
*
LEPGMCAL L     R15,LEWORKA        LOAD COMMON LANG INTERFACE AREA ADDR
         USING LEINTER,R15
*
         L     R0,VWFMTADR        LOAD TRUE  ENTRY POINT     ADDRESS
         ST    R0,LESUBADR
*
         ST    R1,LEPARMP         PASS TRUE  PARAMETER  LIST ADDRESS
         ST    R14,LERETADR       SAVE RETURN  ADDRESS
*
         LA    R1,LEPARM
         L     R15,LECEEADR       CALL COMMON LANG INTERFACE
         BASR  R14,R15
         DROP  R15
*
         L     R1,LEWORKA         LOAD COMMON LANG INTERFACE AREA ADDR
         USING LEINTER,R1
*
         L     R14,LERETADR       LOAD RETURN ADDR
         L     R15,LERTNC         LOAD RETURN CODE
         BSM   0,R14              RETURN
*
         DROP  R1
         DROP  R8
         DROP  R13
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        L O A D   T H E   V I E W   D E F I N I T I O N   P A R M S  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING (GVBMR87,code),R11
         USING WORKAREA,R13
*
VDPLOAD  ds    0h
         L     R2,VDPDCBA         LOAD VIEW DEFINITION DCB   ADDRESS
         using ihadcb,r2
         LAY   R1,STATOPEN
         MVC   RENTPARM(STATOPENL),0(R1)
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,RENTPARM)  OPEN  VIEW FILE
         if tm,dcboflgs,dcbofopn,z did it open?
           drop r2
           LHI R14,MSG#418        LOAD ERROR  MESSAGE  NUMBER
           B   RTNERROR            - PRINT ERROR    MESSAGE - STOP
         else

           LA  R8,VWCHAIN         INITIALIZE  PREVIOUS VIEW DEFN ADDR
*
***********************************************************************
*    READ VIEW DEFN PARAMETERS UNTIL END-OF-FILE                      *
***********************************************************************
VDPLOOP    do inf
             L R1,VDPDCBA         LOAD VIEW DEFINITION FILE DCB  ADDR
             GET (1)              READ NEXT RECORD
             ASI   vdpcnt_real,1  records in VDP dsn
*
             LR R5,R1             LOAD RECORD   ADDRESS (RDW PREFIX)
             USING VDP0001_GENERATION_RECORD,R5
*
             LH R0,VDP0001_RECORD_TYPE LOAD RECORD TYPE CODE
*
*
*     rtc20047 ignore VDP record types not required by MR88
*
***********************************************************************
*      PROCESS NEEDED RECORDS                                         *
***********************************************************************
             select chi,r0,eq
             when 0001            VDP GENERATION RECORD   ???
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P G E N E R A T I O N R E C O R D                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
               USING VDP0001_GENERATION_RECORD,R5
*
*              Save info for cntl report later (IRUN)
               mvc   vdp_date,vdp0001_date
               mvc   vdp_time,vdp0001_time
               mvc   vdp_desc,vdp0001_description
*
               MVC SVRUN#,VDP0001_RUN_NBR  SAVE RUN NUMBER
               if lt,r15,Run_date@,nz
                 mvc   svrundt,0(r15)              save run date
               else
                 mvc   svrundt,vdp0001_run_date    save run date
               endif
*
               XR R15,R15
               ICM R15,B'0001',VDP0001_MAX_DECIMAL_PLACES
               if CHI,R15,eq,3     LARGE NUMBER   (20,3)  MODE  ???
                 MVI LRGNUM,C'Y'   YES - SET LARGE NUMBER INDICATOR
               endif
*
               L R4,PARMDADR
               USING PARMDATA,R4
*
***********************************************************************
*        ALLOCATE LR FIELD DEFINITION TABLE                           *
***********************************************************************
               L R2,VDP0001_LR_FIELD_COUNT LOAD FIELD DEFN RECORD COUNT
               LHI R3,FDENTLEN
               MR R2,R2
*
               LA R0,l'fldseyeb(,R3)
*
               GETMAIN RU,LV=(0),LOC=(ANY)
*
               MVC 0(l'fldseyeb,r1),FLDSEYEB
               AHI R1,l'fldseyeb
               ST R1,FLDDEFTB              SAVE FIELD DEFN TABLE  ADDR
               ST R1,FLDDEFCR              SAVE FIELD DEFN TABLE  CURR
               AR R3,R1
               ST R3,FLDDEFMX
*
***********************************************************************
*        ALLOCATE VDP storage area                                    *
***********************************************************************
               L R0,vdp0001_vdp_byte_count   get vdp byte count
               AHI R0,2*vdpcush   add cushion
               st  R0,vdp_seg_len Current segment size (usable stg)
               AHI R0,l'vdpeyeb   ADD  EYEBALL  LEN
               ST  r0,VDPSIZE     total size
*
               STORAGE OBTAIN,LENGTH=(0),LOC=(ANY),BNDRY=PAGE
*
               MVC 0(l'vdpeyeb,R1),VDPEYEB COPY EYEBALL
               AHI R1,l'vdpeyeb
               ST R1,VDPBEGIN     SAVE TABLE ADDRESS
               ST R1,vdp_addr_curr_seg Current segment address of VDP
               ST R1,VDPCURR      SET  CURRENT TABLE ELEMENT ADDRESS
*
               DROP R4
               DROP R5
                         EJECT
             when 0002            VDP Format view record    ???
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P   F O R M A T   V I E W   R E C O R D            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
               USING VDP0002_RECORD,R5

***********************************************************************
*        ALLOCATE VIEW COLUMN DEFINITION TABLE                        *
***********************************************************************
               if Lt,r2,vdp0002_totaldt_columns,np get DT total, set cc
                 lhi r2,1              zero or less - make it one
               endif

               LHI R3,CDENTLEN
               MR R2,R2

               LA R0,l'colseyeb(,R3)

               GETMAIN RU,LV=(0),LOC=(ANY)

               MVC 0(l'colseyeb,r1),colSEYEB
               AHI R1,l'colseyeb
               ST R1,COLDEFTB          SAVE COLUMN   DEFN TABLE ADDR
               ST R1,COLDEFCR          SAVE COLUMN   DEFN TABLE CURRENT
               AR R3,R1
               ST R3,COLDEFMX

***********************************************************************
*        ALLOCATE VIEW COLUMN CALCULATION DEFINITION TABLE            *
***********************************************************************
               if Lt,r2,vdp0002_totalCT_columns,np get CT total, set cc
                 lhi r2,1              zero or less - make it one
               endif

               LHI R3,CALCLEN
               MR R2,R2

               LA R0,l'calceyeb(,R3)

               GETMAIN RU,LV=(0),LOC=(ANY)
*
               MVC 0(l'calceyeb,r1),calcEYEB
               AHI R1,l'calceyeb
               ST R1,CLCDEFTB          SAVE COL CALC DEFN TABLE ADDR
               ST R1,CLCDEFCR          SAVE COL CALC DEFN TABLE CURRENT
               AR R3,R1
               ST R3,CLCDEFMX
*
***********************************************************************
*        ALLOCATE VIEW SORT KEY DEFINITION TABLE                      *
***********************************************************************
               if Lt,r2,vdp0002_totalsk_columns,np get SK total, set cc
                 lhi r2,1              zero or less - make it one
               endif

               a  r2,vdp0002_view_count     and 1 entry per format view
               la r2,1(,r2)                 for temp area used during
*                                           "bubble" sort of sort keys
               LHI R3,SKENTLEN
               MR R2,R2

               LA R0,l'sorteyeb(,R3)

               GETMAIN RU,LV=(0),LOC=(ANY)

               MVC 0(l'sorteyeb,r1),sortEYEB
               AHI R1,l'sorteyeb
               ST R1,SRTDEFTB          SAVE SORT KEY DEFN TABLE ADDR
               ST R1,SRTDEFCR          SAVE SORT KEY DEFN TABLE CURRENT
               AR R3,R1
               ST R3,SRTDEFMX

***********************************************************************
*        ALLOCATE VDP view nos area                                   *
***********************************************************************
               if lT,R0,vdp0002_view_count,P  get number of views

                 ahi r0,1                     add 1 for counter
                 mhi r0,l'vdp0002_view_number multiply by length of ptr
                 lr  r4,r0                    save length

                 GETMAIN RU,LV=(0),LOC=(ANY)  Get view numbers memory

                 ST R1,VDP_MR88_View_list     save address of area

                 lr  r0,r1                    addr into r0
                 lr  r1,r4                    length into r1
                 la  r14,vdp0002_view_count   source address
                 lr  r15,r4                   and length
                 mvcl r0,r14                  move all the data

               else
                 xc VDP_MR88_View_list,VDP_MR88_View_list  Clear area
               endif

               DROP R5
                         EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P   C O N T R O L   R E C O R D                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
             when 0050  CONTROL
*
               USING VDP0050_CONTROL_RECORD,r5
               using Fiscal_date_table,r14
outer_50       do ,
                 if lt,r14,fiscal_date_ids@,nz any fiscal dates
                   do until=(lt,r14,fiscal_next,np) loop until no mor
                     if clc,fiscal_recid,eq,vdp0050_record_id
                       mvc vdp0050_fiscal_date,Fiscal_iddate
                       leave outer_50 found what we wanted so get out
                     endif
                   enddo
*RTC21113 Use the default date
                   if lt,r14,fiscal_date@,nz
                     lay  r15,fiscal_date
                   else
                     lay  r15,run_date
                   endif
                   mvc vdp0050_fiscal_date(8),0(r15) default date
                 else
                   if lt,r14,fiscal_date@,nz
                     lay  r15,fiscal_date
                   else
                     lay  r15,run_date
                   endif
                   mvc vdp0050_fiscal_date(8),0(r15) parm Fiscal date
                 endif
               enddo  , (outer_50)
               j vdpcopy
               drop r14

             when (0200,0210)  LF or PF record  ???
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P L O G I C A L F I L E                            *
*                                                                     *
* In order to accomodate dynamic expansion of the VDP table at time of*
* loading VDP records (i.e. if vdp0001_vdp_byte_count turns out not to*
* be big enough, each VDP table entry is followed by a pointer to the *
* next entry. This allows MR87 to obtain additional segments for the  *
* VDP table.                                                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
               USING VDP0200_FILE_RECORD,R5
VDPCOPY        DS   0H
               asi  VDPCOUNT,1     increment VDP record count

               LH   R1,VDP0200_REC_LEN get the record length
               L    R0,vdp_addr_curr_seg Determine remaining space
               A    R0,vdp_seg_len
               s    r0,vdpcurr     subtract current position
               aghi r0,-8          subtract pointer length we need

               if SR,R0,R1,np     Will this record fit?                +
                 if this not positive then need more VDP table segments
                 lghi R0,16*vdpcush  more space
                 st   R0,vdp_seg_len this much
                 llgf r15,VDPSIZE
                 agr  r15,r0        adjust total vdp size
                 ST   r15,VDPSIZE

                 STORAGE OBTAIN,LENGTH=(0),LOC=(ANY),BNDRY=PAGE

                 ST R1,vdp_addr_curr_seg Current segment address of VDP
                 ST R1,VDPCURR           Current element address in VDP
                 asi vdp_seg_cnt,1

               endif

               L    R14,VDPCURR get target
               Ltgf r1,VDP_PREV_PTR_A  get previous pointer address
               JZ   VDPCOPY2           if there is one (i.e. not first)
               STG  R14,0(,R1)  store next pointer at end of last rec
VDPCOPY2       DS   0H
               LH   R1,VDP0200_REC_LEN   get the record length again
               LR   R0,R5       get start address
               LR   R15,R1      copy length
               MVCL R14,R0      and copy the record

*
               ST   r14,VDP_PREV_PTR_A save address of next fwd ptr
               AGHI R14,8       add pointer length for a(next element)
               ST   R14,VDPCURR update the current position - next elem

               DROP R5
                         EJECT
             when 0400            LR FIELD  ???
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P L R F I E L D D E F I N I T I O N   R E C O R D  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
               USING VDP0400_LR_FIELD_RECORD,R5
*
               L R2,FLDDEFCR      LOAD CURRENT FIELD DEFN TABLE ENTRY
               USING FLDDEFN,R2
*
***********************************************************************
*        CHECK FOR FIELD DEFINITION TABLE OVERFLOW                    *
***********************************************************************
               LHI  R14,MSG#408   ASSUME TABLE OVERFLOW
               C    R2,FLDDEFMX   FIELD  DEFN  TABLE OVERFLOW  ???
               JNL  RTNERROR      YES -  INDICATE ERROR
*
***********************************************************************
*        COPY FIELD DEFINITION DATA                                   *
***********************************************************************
               XC FDFILEID,FDFILEID                FIELD FILE   ID
               MVC FDLRID,VDP0400_LR_ID            FIELD RECORD ID
               MVC FDFLDID,VDP0400_RECORD_ID       FIELD FIELD  ID
               MVC FDFLDLEN,VDP0400_FIELD_LENGTH   FIELD LENGTH
               MVC FDFLDFMT,VDP0400_FORMAT_ID+2    FIELD FORMAT
               MVC FDFLDCON,VDP0400_FIELD_CONTENT_ID+2 FIELD CONTENT
*
               LH R0,VDP0400_DECIMAL_COUNT         FIELD NO OF DEC
               stc r0,FDNDEC
*
               LH R0,VDP0400_ROUNDING              FIELD ROUNDING FCTR
               stc r0,FDRNDFAC
*
               XR R0,R0                            FIELD SIGNED IND
               IC R0,VDP0400_SIGNED_IND
               MVI FDSIGN,C'N'
               if CHI,R0,eq,SIGNED
                 MVI FDSIGN,C'Y'
               endif
*
               L R15,VDP0400_JUSTIFY_ID            FIELD JUSTIFICATION
               if chi,R15,gt,3
                 LHI R15,3
               endif
               LA R14,JUSTTBL(R15)
               MVC FDJUST,0(R14)
*
               AHI R2,FDENTLEN    UPDATE CURRENT FIELD DEFN TABLE ENTRY
               ST R2,FLDDEFCR
*
               DROP R2
               DROP R5
                         EJECT
             when 0650
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              Join reference file details record                     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
               mvc vdp0650a,vdpcurr save current vdp save addr
               j vdpcopy            then copy to vdp save area
             when 1000            VIEW    ???
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P V I E W D E F I N I T I O N R E C O R D          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
               USING VDP1000_VIEW_RECORD,R5

*              Need to check that this view is in the list of format
*              views

               bras r9,check_view_list
               if ltr,r15,r15,nz      Format view found

                 AP VIEWCNT,P001  INCREMENT  VIEW COUNT
*
                 LHI R0,l'vieweyeb+VWRECLEN ALLOCATE NEXT VIEW DEF REC
*
                 GETMAIN RU,LV=(0),LOC=(ANY)
*
                 MVC 0(l'vieweyeb,r1),VIEWEYEB
                 AHI R1,l'vieweyeb
                 ST R1,0(,R8)
*
***********************************************************************
*          ASSIGN CALCULATED COLUMN#'S TO COLUMNS IN PREVIOUS VIEW    *
***********************************************************************
                 LA R0,VWCHAIN    ANY  PREVIOUS VIEW ???
                 if CR,R8,ne,R0   yes
                   BRAS R9,FLAGCALC ASSIGN CALCULATED COLUMN NUMBERS
                   BRAS R9,LINKSKEY SORT SORT KEYS, LINK THEM TO COLS
                   BRAS R9,OFFCALC COMPUTE COLUMN OUTPUT   OFFSETS
                   BRAS R9,OFFRPTTL COMPUTE REPORT TITLE   OFFSETS
                   BRAS R9,OFFFOOTR COMPUTE REPORT FOOTER  OFFSETS
                 endif
                 L R8,0(,R8)      INITIALIZE CURRENT CHAIN ELEMENT ADDR
                 USING VIEWREC,R8
*
                 XC VWNEXT,VWNEXT INITIALIZE FORWARD POINTER
*
***********************************************************************
*          INSERT "GRAND TOTAL" SORT KEY ELEMENT INTO SORT KEY TABLE  *
***********************************************************************
                 L R6,SRTDEFCR    LOAD CURRENT SORT KEY TABLE ENTRY ADR
                 USING SORTKEY,R6
*
                 LHI R14,MSG#410  ASSUME   OVERFLOW
                 C R6,SRTDEFMX    SORT KEY TABLE    OVERFLOW  ???
                 JNL  RTNERROR
*
                 xc sortkey(256),sortkey Clear area 1st            @03I
                 xc sortkey+256(skentlen-256),sortkey+256          @03I
*
                 XC SKSRTORD,SKSRTORD GRAND TOTAL IS SEQ# = 0 (FIRST)
*
                 ZAP SKCOUNT,P000
*
                 XC SKINDCNT,SKINDCNT ZERO INDENTATION COUNT
                 XC SKPREFXL,SKPREFXL ZERO PREFIX   LENGTH
                 MVC SKTITLE,SPACES
                 LHI R15,L'GRANDTOT LOAD LENGTH OF GRAND TOTAL LABEL
                 STH R15,SKDESCLN
                 STH R15,SKTTLTOT
                 BCTR R15,0
                 EXrl R15,MVCGRAND
*
                 AHI R6,SKENTLEN  ADVANCE  TO NEXT SORT  KEY ENTRY
                 ST R6,SRTDEFCR
                 ST R6,VWSKADDR
                 ST R6,VWLOWSKY
*
                 DROP R6
*
***********************************************************************
*          COPY VIEW DEFINITION PARAMETERS INTO VIEW DEFN CHAIN       *
***********************************************************************
                 MVC VWVIEW#,VDP1000_VIEWID  VIEW    ID
                 MVC VWSUMTYP,VDP1000_VIEW_TYPE+2 VIEW TYPE       (S,D)
                 MVC VWDESTYP,VDP1000_OUTPUT_MEDIA+2 DESTINATION(O,B,F)
*
                 MVC VWBASEV#,VDP1000_COLUMN_ID BASE VIEW ID
                 MVC VWDHDR,VDP1000_GEN_DELIM_HEADER Header Ind for CSV
*
                 XC VWFLAGS,VWFLAGS          INITIALIZE   FLAGS
                 OI VWFLAG1,VWNOMIN+VWNOFRST
*
                 OI VWFLAG2,VWPRINT          ASSUME USING PRINT AREA
*
                 CLI VWDESTYP+1,PVITTBL
                 JE  PVITFLAG
                 CLI VWDESTYP+1,EXECINF
                 JNE  DESTFILE
*
                 L R14,PARMDADR   LOAD  PARAMETER  AREA  ADDRESS
                 USING PARMDATA,R14
*
                 CLI XIOPTCD,C'C'
                 JE  PVITFLAG
                 OI VWFLAG2,VWEXEC
                 J  PVITFLAG0010
PVITFLAG         OI VWFLAG2,VWPVIT
                 OI VWFLAG2,VWBLDCSV
*
PVITFLAG0010     EQU *
                 NI VWFLAG2,255-VWPRINT
*
DESTFILE         cli   FIXWDDEL,C'Y'
                 jne   FXWSKIP
                 OI    VWFLAG2,VWFXWDTH
*
                 DROP R14
*
FXWSKIP          CLI VWDESTYP+1,FILEFMT
                 JNE  FXWSKIP0010
                 OI VWFLAG2,VWOUTDCB
                 NI VWFLAG2,255-VWPRINT
*
FXWSKIP0010      EQU *
                 CLI VWDESTYP+1,CSV
                 JNE FXWSKIP0012
                 OI VWFLAG2,VWBLDCSV
                 OI VWFLAG2,VWOUTDCB
                 NI VWFLAG2,255-VWPRINT
*
FXWSKIP0012      EQU *
                 CLI VWDESTYP+1,XML
                 JNE FXWSKIP0014
                 OI VWFLAG2,VWBLDCSV
                 OI VWFLAG2,VWOUTDCB
                 NI VWFLAG2,255-VWPRINT
*
FXWSKIP0014      EQU *
                 XR R0,R0                    OUTPUT DETAIL IND
                 IC R0,VDP1000_OUTPUT_DETAIL_IND
                 CHI R0,PRNTDET
                 JNE FXWSKIP0016
                 OI VWFLAG1,VWPRTDET
*
FXWSKIP0016      EQU *
                 MVC VWCOMPNM,SPACES         REPORT TITLE - COMPANY
                 MVC VWFINPDT,SPACES         FISCAL PERIOD  DATE
                 L R1,VDP1000_CONTROL_ID
                 BRAS R9,LOCCTRL
                 if LTR,R15,R15,p
                   using vdp0050_control_record,r15
                   MVC VWCOMPNM,VDP0050_NAME
                   MVC VWFINPDT,VDP0050_FISCAL_DATE
                   drop r15
                 endif
                 MVC VWRUN#,SVRUN#
                 MVC VWRUNDT,SVRUNDT
*
                 MVC VWTITLE,SPACES          REPORT TITLE
                 MVC VWTITLE(L'VDP1000_VIEW_NAME),VDP1000_VIEW_NAME
                 MVC VWFOLDER,VDP1000_FOLDER_ID VIEW FOLDER  ID
                 MVC VWACCESS,SPACES         ACCESS    CODE
                 MVC VWUSERID,VDP1000_OWNER_USER OWNER USER  ID
                 MVC VWDESTID,VDP1000_OUTPUT_DESTINATION_ID
                 MVC VWDDNAME,SPACES         OUTPUT   FILE DDNAME
                 MVC VWDESPTR,SPACES         DESTINATION  PRINTER
*
                 CLI VDP1000_ZERO_SUPPRESS_IND,X'01'
                 JNE FXWSKIP0018
                 OI VWFLAG1,VWZEROSP
*
FXWSKIP0018     EQU *
                MVC VWUPDMOD,VDP1000_REGEN_OPT_ID+2 UPDATE MODE (R,M,N)
                 MVC VWHEADLN,VDP1000_OUTPUT_COL_HDR_LNS_MAX
                 MVC VWPAGSIZ,VDP1000_OUTPUT_PAGE_SIZE_MAX
                 LH R0,VDP1000_OUTPUT_LINE_SIZE_MAX
                 CHI R0,PRNTMAXL
                 JNH FXWSKIP0020
                 LHI R0,PRNTMAXL
FXWSKIP0020      EQU *
                 STH R0,VWLINSIZ
                 MVC VWOVRFIL,VDP1000_FILL_TRUNCATION_VALUE
                 MVC VWERRFIL,VDP1000_FILL_ERROR_VALUE
*
                 ZAP DBLWORK,NINES
                 L R0,VDP1000_OUTPUT_MAX_REC_COUNT
                 LTR R0,R0
                 JNP FXWSKIP0022
                 CVD R0,DBLWORK
FXWSKIP0022      EQU *
                 ZAP VWLIMIT,DBLWORK
*
                 XC VWSRTCNT,VWSRTCNT       SORT   KEY    COUNT
                 XC VWBRKCNT,VWBRKCNT       SORT   BREAK  COUNT
                 XC VWCOLCNT,VWCOLCNT       BASE   COLUMN COUNT
                 XC VWCOLADR,VWCOLADR       BASE   COLUMN ADDRESS
                 XC VWEXCNT,VWEXCNT         EXCEPTION CONDITION  COUNT
                 XC VWEXCOND,VWEXCOND       EXCEPTION CONDITION  STACK
                 ZAP VWPAGENO,P000
                 ZAP VWOUTCNT,P000          OUTPUT COUNT
                 ZAP VWMRGCNT,P000          MERGED RECORD COUNT
*
                 TM VWFLAG2,VWCRPIND  USE   PRINTER   AS    FILE ???
                 JO FXWSKIP0024       NO  - BYPASS    SPECIAL    LOGIC
                 MVC VWDDNAME,VWDESPTR YES - SET-UP   AS    FILE
*
FXWSKIP0024      EQU *
                 L R1,VWVIEW#
                 BRAS R9,LOCFILE
                 LTR R15,R15      OUTPUT  FILE FOUND  ???
                 JNP FXWSKIP0026  NO  - BYPASS DDNAME OVERRIDE
    MVC VWDDNAME,VDP1600_DDNAME_OUTPUT-VDP1600_SUM_OUT_FILE_RECORD(R15)
                 ST R15,VWVDPADR
*
FXWSKIP0026    EQU *
               MVC VWFMTPGM,SPACES FORMAT EXIT PROGRAM
               MVC VWFMTPRM,SPACES FORMAT EXIT PARAMETERS
               XC VWPGMADR,VWPGMADR FORMAT EXIT CALL   ADDR
               XC VWFMTADR,VWFMTADR FORMAT EXIT ADDRESS
               XC VWFMTWRK,VWFMTWRK FORMAT EXIT WORK   AREA
*
               XC VWMAXCOL,VWMAXCOL INITIALIZE MAXIMUM COLUMN NUMBER
               XC VWCLCCOL,VWCLCCOL INITIALIZE CALC    COLUMN COUNT
               XC VWRTADDR,VWRTADDR INITIALIZE REPORT  TITLE  ADDRESS
               XC VWRFADDR,VWRFADDR INITIALIZE REPORT  FOOTER ADDRESS
               XC VWMAXRPT,VWMAXRPT INITIALIZE REPORT  TITLE  COUNT
               XC VWMAXFTR,VWMAXFTR INITIALIZE REPORT  FOOTER COUNT
               LHI R0,L'GRANDTOT  INITIALIZE   MAXIMUM TITLE  LENGTH
               STH R0,VWMAXTTL
               XC VWSETLEN,VWSETLEN INITIALIZE SIZE OF ONE SUBTOTAL SET
               XC VWOUTLEN,VWOUTLEN INITIALIZE  OUTPUT LENGTH
               XC VWCENTER,VWCENTER CENTERING ADJUSTMENT
               XC VWSRTTOT,VWSRTTOT ZERO COMBINED SORT KEY    LENGTHS
               XC VWTTLTOT,VWTTLTOT ZERO COMBINED TITLE DESC  LENGTHS
               XC VWDCBADR,VWDCBADR OUTPUT  FILE   DCB ADDRESS
*
               ZAP VWRECCNT,P000  REPORT  REQUEST CONTROL   COUNT
               MVC VWSATIND,SPACES REPORT REQUEST SATISFIED INDICATOR
               MVC VW0C7IND,SPACES 0C7    ABEND   INDICATOR
               MVC VWOVRIND,SPACES EXTRACT LIMIT  EXCEEDED  INDICATOR
*
               MVI VWFLDDEL,C','  DEFAULT FIELD   DELIMITER = COMMA
               MVI VWRCDDEL,X'25' DEFAULT RECORD  DELIMITER = LINE FEED
*
               L    R1,VDP1000_FORMAT_EXIT_PGM_ID
               BRAS R9,LOCEXIT
               LTR  R15,R15
               JNP  FXWSKIP0028
          MVC VWFMTPGM,VDP0210_MODULE_NAME-VDP0210_EXIT_PGM_RECORD(R15)
*
FXWSKIP0028    EQU  *
               if CLC,VWFMTPGM,ne,SPACES,and,                          +
               OC,VWFMTPGM,VWFMTPGM,nz
*
                   MVC VWFMTPRM,VDP1000_FORMAT_EXIT_STARTUP
*
                   LA R9,VWFMTPGM POINT TO  EXIT      PROGRAM    NAME
                   LOAD EPLOC=(R9),ERRET=VDPGMERR
*
                 O R0,MODE31      FORCE 31-BIT   MODE
                 ST R0,VWFMTADR   SAVE  SUBROUTINE ADDRESS (31-BIT)
                 ST R0,VWPGMADR   ASSUME  NOT  LE  COBOL (NO INTERFACE)
*
                   LR R1,R0
                   if CLC,05(3,R1),eq,LECOBOL COBOL SUBROUTINE ???
*
                     L R0,LECALLA SUBSTITUTE COMMON  LANG   INTERFACE
                     ST R0,VWPGMADR
*
                     MVI LANGENV,C'Y' INDICATE LANG ENV NEEDED
                   endif
                 endif
*
               endif
               DROP R5
                         EJECT
             when 1210            EXCEPTION CONDITION   RECORD  ???
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P C O L U M N C A L C U L A T I O N S T A C K      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
               USING VDP1210_EXCEPTION_RECORD,R5
*
*              Need to check that this view is in the list of format
*              views

               bras r9,check_view_list
               if ltr,r15,r15,nz      Format view found

                 L R6,CLCDEFCR    LOAD CURRENT  CALC  DEFN ENTRY   ADDR
                 USING CALCTBL,R6
*
***********************************************************************
*          CHECK FOR COLUMN CALCULATION DEFINITION TABLE OVERFLOW     *
***********************************************************************
                 LHI  R14,MSG#419 ASSUME TABLE OVERFLOW
                 C    R6,CLCDEFMX CALC   DEFN  TABLE OVERFLOW  ???
                 JNL  RTNERROR    YES -  INDICATE ERROR
*
***********************************************************************
*          PROCESS EXCEPTION CONDITION STACK                          *
***********************************************************************
*
VDP1210B         LA R14,ACCUMWRK  INITIALIZE CURRENT STACK ELEMENT ADDR
                 ST R14,CURRSTAK
*
               LH R3,VDP1210_STACK_LENGTH ANY EXECPTION LOGIC ???
               LTR R3,R3
               JNP  VDP1210X      NO  - SKIP LOAD
*
               LA R3,VDP1210_STACK(R3) COMPUTE END OF STACK ADDRESS
               ST R6,VWEXCOND     SAVE FIRST CONDITION     ADDRESS
*
               LA R5,VDP1210_STACK
               USING VDP1210_STACK_FUNCTION,R5
*
               LR R4,R5           SAVE EXCEPTION CONDITION ORIGIN
                           EJECT
VDP1210C      L R0,VDP1210_STACK_OPERATION_CODE COPY FUNCTION (OP CODE)
                 STH R0,CALCOPER
*
*  note - llgt is used below so that the addresses do NOT have their
*         high order bit set on.
*         The calculation routine in MR88 uses the top bit as a
*         sentinel to mark the end of the stack
*
***********************************************************************
*          BRANCH                                                     *
***********************************************************************
                 CLI CALCOPER+1,BRNCH UNCONDITIONAL BRANCH ???
                 JNE  VDP1210D    NO  - CHECK FOR  NEGATE
                 llgt R14,CALCBRN LOAD SUBROUTINE ADDRESS
                 ST R14,CALCFUNC  SAVE SUBROUTINE ADDRESS
                 j   EXCPOFFS
*
***********************************************************************
*          NEGATE                                                     *
***********************************************************************
VDP1210D         CLI CALCOPER+1,NEG NEGATE VALUE ???
                 JNE  VDP1210E    NO  - CHECK ABSOLUTE VALUE
                 llgt r14,calcNEG LOAD SUBROUTINE ADDRESS
                 ST R14,CALCFUNC  SAVE SUBROUTINE ADDRESS
             AHI R5,L'VDP1210_STACK_OPERATION_CODE ADVANCE TO NEXT FUNC
                 j   EXCPNEXT
*
***********************************************************************
*        ABSOLUTE VALUE                                               *
***********************************************************************
VDP1210E         CLI CALCOPER+1,ABS ABSOLUTE VALUE ???
                 JNE  VDP1210F    NO  - CHECK PUSH VALUE (CONSTANT)
                 llgt r14,calcABS LOAD SUBROUTINE ADDRESS
                 ST R14,CALCFUNC  SAVE SUBROUTINE ADDRESS
             AHI R5,L'VDP1210_STACK_OPERATION_CODE ADVANCE TO NEXT FUNC
                 j   EXCPNEXT
*
***********************************************************************
*        PUSH VALUE (CONSTANT)                                        *
***********************************************************************
VDP1210F         CLI CALCOPER+1,PUSHV PUSH VALUE ???
                 JNE  VDP1210I    NO  - CHECK PUSH COLUMN
*
                 LA R0,L'VDP1210_STACK_PUSH_NBR INITIALIZE LOOP COUNTER
                 LA R1,VDP1210_STACK_PUSH_NBR SCAN FOR TRAILING SPACE
VDP1210G         CLI 0(R1),X'40'
                 JE  VDP1210G02
                 LA R1,1(,R1)
                 BRCT R0,VDP1210G
                 LA R1,VDP1210_STACK_PUSH_NBR+L'VDP1210_STACK_PUSH_NBR
*
VDP1210G02     EQU *
               LA R14,VDP1210_STACK_PUSH_NBR
               llgtr r14,r14
               stg R14,savaladr
               SR R1,R14
               STH R1,savallen
*
               LHI R0,L'CALCVALU  RESULT LENGTH
               STH R0,dl96len
*
               xr  R0,r0          create a zero
               STH R0,SAMSKLEN    set mask length to zero
               ST R0,SAMSKADR      as well as the address
*
               MVI SAVALFMT+1,FC_EDIT ASSUME NUMERIC SOURCE
*
               XC SAVALCON,SAOUTCON
               MVHHI SAVALDEC,0
               MVHHI SAVALRND,0
               MVI SAVALSGN,C'N'
*
               MVI SAOUTFMT+1,FC_float RESULT ATTRIBUTES
               XC SAOUTCON,SAOUTCON
*
               MVHHI saoutdec,8   tell formatting to output 8 dp
*
               MVHHI SAOUTRND,0
               MVI SAOUTSGN,C'Y'
*
               LA R0,CALCVALU     RESULT ADDRESS
               ST R0,dl96tgta
               ST R0,CALCOP2A     SAVE OPERAND 2 ADDRESS
*
               LA R1,DL96LIST
               Llgf  R15,GVBDL96A
               bassm R14,R15
*
               LTR R15,R15
               JZ  VDP1210H
*
               CHI R15,2          TRUNCATION ???
               JE  VDP1210H
*
               ST    R15,DBLWORK2+4
               UNPK  DBLWORK(9),DBLWORK2+4(5)
               TR    DBLWORK,HEXTAB
               MVC   ERRDATA(8),DBLWORK
               LHI   R14,MSG#409        LOAD   ERROR MESSAGE NUMBER
               j     RTNERROR           PRINT  ERROR MESSAGE
*
VDP1210H       equ  *
               L R14,CURRSTAK     SET  TARGET = CURRENT STACK
               ST R14,CALCTGTA
               AHI R14,AccumDFPl  UPDATE  CURRENT STACK ADDRESS
               ST R14,CURRSTAK
*
               llgt r14,calcPSHV  LOAD PUSH VALUE FUNCTION
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
*
              AHI R5,L'VDP1210_STACK_PUSH_CONSTANT ADVANCE TO NEXT FUNC
              j   EXCPNEXT
*
***********************************************************************
*        PUSH COLUMN NUMBER                                           *
***********************************************************************
VDP1210I       CLI CALCOPER+1,PUSHC PUSH COLUMN# ???
               JNE  VDP1210N      NO  - POP STACK
*
               L R14,VDP1210_STACK_PUSH_COL_NBR LOAD VALUE COLUMN NO
               ST R14,CALCOP2A    SAVE OPERAND  2   COLUMN  NUMBER
               CVD R14,DBLWORK    SAVE THE   COLUMN NUMBER
               lg  r15,dblwork    get the packed number in r15
               cdstr fp1,r15      convert to long dfp in fp1
               lxdtr fp0,fp1,0    make it an extended number
               GVBSTX fp0,calcvalu   and save that
*
               L R14,CURRSTAK     SET  TARGET = CURRENT STACK
               ST R14,CALCTGTA
               AHI R14,AccumDFPl  UPDATE  CURRENT STACK ADDRESS
               ST R14,CURRSTAK
*
               llgt r14,calcPSHC  LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               AHI R5,L'VDP1210_STACK_PUSH_COLUMN ADVANCE TO NEXT FUNC
               j   EXCPNEXT
*
***********************************************************************
*        POP STACK ASSOCIATED WITH ARITHMETIC FUNCTION                *
***********************************************************************
VDP1210N       L R14,CURRSTAK     SAVE PAIR OF STACK ADDRESSES && POP
               ahi R14,-AccumDFPl BACKUP ONE ENTRY
               ST R14,CALCOP2A    SAVE ADDRESS OF 2ND  IN PAIR
*
               ST R14,CURRSTAK    UPDATE CURRENT  STACK   POSITION
*
               ahi R14,-AccumDFPl BACKUP ONE ENTRY
               ST R14,CALCTGTA    SAVE ADDRESS OF 1ST  IN PAIR
*
EXCPEQ         CLI CALCOPER+1,COMPEQ COMPARE EQUAL ???
               JNE  EXCPNE        NO  - CHECK NOT EQUAL
               ST R14,CURRSTAK    UPDATE CURRENT  STACK   POSITION
               llgt r14,calcEQ    LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   EXCPOFFS       SEARCH  FOR BRANCH OFFSET
*
EXCPNE         CLI CALCOPER+1,COMPNE COMPARE NOT EQUAL ???
               JNE  EXCPGT        NO  - CHECK GREATER  THAN
               ST R14,CURRSTAK    UPDATE CURRENT  STACK   POSITION
               llgt r14,calcNE    LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   EXCPOFFS       SEARCH  FOR BRANCH OFFSET
*
EXCPGT         CLI CALCOPER+1,COMPGT COMPARE GREATER THAN ???
               JNE  EXCPGE        NO  - CHECK  GREATER THAN  OR EQUAL
               ST R14,CURRSTAK    UPDATE CURRENT  STACK   POSITION
               llgt r14,calcGT    LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   EXCPOFFS       SEARCH  FOR BRANCH OFFSET
*
EXCPGE         CLI CALCOPER+1,COMPGE COMPARE GREATER THAN OR EQUAL ???
               JNE  EXCPLT        NO  - CHECK   LESS THAN
               ST R14,CURRSTAK    UPDATE CURRENT  STACK   POSITION
               llgt r14,calcGE    LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   EXCPOFFS       SEARCH  FOR BRANCH OFFSET
*
EXCPLT         CLI CALCOPER+1,COMPLT COMPARE LESS THAN ???
               JNE  EXCPLE        NO  - CHECK LESS THAN OR EQUAL
               ST R14,CURRSTAK    UPDATE CURRENT  STACK   POSITION
               llgt r14,calcLT    LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   EXCPOFFS       SEARCH  FOR BRANCH OFFSET
*
EXCPLE         CLI CALCOPER+1,COMPLE COMPARE LESS THAN OR EQUAL ???
               JNE  EXCPADD       NO  -  CHECK ADDITION
               ST R14,CURRSTAK    UPDATE CURRENT  STACK   POSITION
               llgt r14,calcLE    LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   EXCPOFFS       SEARCH  FOR BRANCH OFFSET
*
EXCPADD        CLI CALCOPER+1,ADD ADDITION        ???
               JNE  EXCPSUB       NO  - CHECK SUBTRACTION
               llgt r14,calcADDA  LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   EXCPNEXT       SEARCH  FOR BRANCH OFFSET
*
EXCPSUB        CLI CALCOPER+1,SUB SUBTRACTION     ???
               JNE  EXCPMULT      NO  - CHECK MULTIPLICATION
               llgt r14,calcSUBA  LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
             AHI R5,L'VDP1210_STACK_OPERATION_CODE ADVANCE TO NEXT FUNC
               j   EXCPNEXT
*
EXCPMULT       CLI CALCOPER+1,MULT MULTIPLICATION ???
               JNE  EXCPDIV       NO  - CHECK DIVISION
               llgt r14,calcMULA  LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
             AHI R5,L'VDP1210_STACK_OPERATION_CODE ADVANCE TO NEXT FUNC
               j   EXCPNEXT
*
EXCPDIV        CLI CALCOPER+1,DIV DIVISION        ???
               JNE  EXCPBAD       NO  - CHECK FOR LAST CALCULATION
               llgt r14,calcDIVA  LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
             AHI R5,L'VDP1210_STACK_OPERATION_CODE ADVANCE TO NEXT FUNC
               j   EXCPNEXT
*
EXCPBAD        LHI R14,MSG#422    LOAD ERROR MESSAGE  NUMBER
*
               MVC ERRDATA(L'CALCMSG),CALCMSG
*
               L R0,VWVIEW#       INDICATE   WHICH    VIEW
               CVD R0,DBLWORK
               OI DBLWORK+L'DBLWORK-1,X'0F'
               MVC ERRDATA+02(8),VALUMSK1
               ED ERRDATA+02(8),DBLWORK+4
*
               LH R0,VWEXCNT      INDICATE   WHICH    CONDITION
               AHI R0,1
               CVD R0,DBLWORK
               OI DBLWORK+L'DBLWORK-1,X'0F'
               UNPK ERRDATA+14(4),DBLWORK
*
               LH R0,CALCOPER     INDICATE   WHICH    OPERATOR
               CVD R0,DBLWORK
               OI DBLWORK+L'DBLWORK-1,X'0F'
               UNPK ERRDATA+33(2),DBLWORK
*
               j   RTNERROR
*
***********************************************************************
*     COUNT NUMBER OF STACK OPERATIONS SKIPPED TO REACH BRANCH OFFSET *
***********************************************************************
EXCPOFFS       L R14,VDP1210_STACK_BRANCH_OFFSET
               LTR R1,R14         "SKIPIF" (OFFSET=X'FF')  ???
               JNM  EXCPOFF2      NO  - SEARCH FOR OFFSET
*
               LHI R1,-4
               j   EXCPOFF8
*
EXCPOFF2       AR R14,R4          ADD  ORIGIN  TO  OFFSET
               CR R14,R3          BRANCH  TO END-OF-STACK  ???
               JNE  EXCPOFF3
*
               LHI R1,-3
               j   EXCPOFF8
*
EXCPOFF3       LA R0,ACCUMWRK     FLUSH THE STACK
               ST R0,CURRSTAK
*
               LA R1,L'VDP1210_STACK_BRANCH(,R5) POINT TO NEXT STACK OP
               SR R0,R0           ZERO STACK OPERATIONS SKIPPED COUNT
*
EXCPOFF4       CR R1,R14          REACHED  BRANCH ADDRESS ???
               JNL  EXCPOFF6      YES - EXIT LOOP
               AHI R0,1           INCREMENT  SKIPPED  OPERATION COUNT
           L R15,VDP1210_STACK_BRANCH_OP_ID-VDP1210_STACK_FUNCTION(,R1)
               SLL R15,1
               AH R1,STACKTBL(R15)
               j   EXCPOFF4       LOOP UNTIL BRANCH OFFSET REACHED
*
EXCPOFF6       LHI R1,CALCLEN     COMPUTE OFFSET TO NEW STACK FUNC
               MR R0,R0
EXCPOFF8       ST R1,CALCVALU
*
               LA R0,ACCUMWRK     FLUSH THE STACK
               ST R0,CURRSTAK
*
               AHI R5,L'VDP1210_STACK_BRANCH ADVANCE TO NEXT STACK OP
*
EXCPNEXT       LH R0,VWEXCNT      INCREMENT EXCEPTION CONDITION COUNT
               AHI R0,1
               STH R0,VWEXCNT
*
               CR R5,R3           END-OF-STACK  ???
               JNL  VDP1210W      YES -  EXIT
*
               AHI R6,CALCLEN     ADVANCE  TO NEXT EXCEPTION   COND
               ST R6,CLCDEFCR
               j   VDP1210C
*
VDP1210W       AHI R6,CALCLEN     ADVANCE  TO NEXT CALCULATION FUNC
               MVC 0(4,R6),HEXFF  INSERT END-OF-STACK MARKER
               AHI R6,4
               ST R6,CLCDEFCR
*
VDP1210X       ds  0h
*
               DROP R5
               DROP R6
               endif
                         EJECT
             when 1300            REPORT  TITLE  LINES  ???
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P T I T L E L I N E S R E C O R D                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*              Need to check that this view is in the list of format
*              views

               bras r9,check_view_list
               if ltr,r15,r15,nz      Format view found

               USING VDP1300_TITLE_LINES_RECORD,R5
*
               LH R0,VDP1300_ROW_NBR
*
               CH R0,MAXRPT
               JNH VDP1300X012
               STH R0,MAXRPT
*
VDP1300X012    EQU *
               CH R0,VWMAXRPT
               JNH VDP1300X014
               STH R0,VWMAXRPT
*
VDP1300X014    EQU *
               XC VDP1300_TITLE_AREA_OFFSET,VDP1300_TITLE_AREA_OFFSET
               XC VDP1300_SORT_TITLE_ADDRESS,VDP1300_SORT_TITLE_ADDRESS
*
               OC VWRTADDR,VWRTADDR
               JNZ VDP1300X
               MVC VWRTADDR,VDPCURR
*
VDP1300X       J   VDPCOPY
*
               DROP R5
               endif
                         EJECT
             when 1400            REPORT  FOOTER LINES  ???
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P F O O T E R L I N E S R E C O R D                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*              Need to check that this view is in the list of format
*              views

               bras r9,check_view_list
               if ltr,r15,r15,nz      Format view found

               USING VDP1400_FOOTER_LINES_RECORD,R5
*
               LH R0,VDP1400_ROW_NBR
*
               CH R0,MAXFOOTR
               JNH VDP1400X012
               STH R0,MAXFOOTR
*
VDP1400X012    EQU *
               CH R0,VWMAXFTR
               JNH VDP1400X014
               STH R0,VWMAXFTR
*
VDP1400X014    EQU *
               XC VDP1400_FOOTER_AREA_OFFSET,VDP1400_FOOTER_AREA_OFFSET
               XC VDP1400_SORT_TITLE_ADDRESS,VDP1400_SORT_TITLE_ADDRESS
*
               OC VWRFADDR,VWRFADDR
               JNZ VDP1400X
               MVC VWRFADDR,VDPCURR
*
VDP1400X       J   VDPCOPY
*
               DROP R5
               endif
                         EJECT
             when 1600            SUMMARY OUTPUT FILE   ???
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P S U M M A R Y O U T P U T F I L E   R E C O R D  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*              Need to check that this view is in the list of format
*              views

               bras r9,check_view_list
               if ltr,r15,r15,nz      Format view found

               USING VDP1600_SUM_OUT_FILE_RECORD,R5
*
               if LT,R15,VDP1600_FIELD_DELIM_ID,P,and, Get delim number+
               CHI,R15,le,delim_last             .Value in range?  @01I
                 BCTR R15,R0                     .Make it an offset@01I
                 LARL R0,DELIMITER_TABLE        .Get actual delim  @01I
                 AR R15,R0                 Index to the location
                 MVC VWFLDDEL,0(R15)             .Use this one     @01I
               endif

               if LT,R15,VDP1600_text_DELIM_ID,P,and,   Get text delim +
               CHI,R15,le,text_last        .Value in range?
                 BCTR R15,R0                     .Make it an offset
                 LARL R0,string_delim_TABLE      .Get actual delim
                 AR R15,R0                 Index to the location
                 mvc VW_STRING_DELIMITER,0(r15)  use this
               else
                 mvi VW_STRING_DELIMITER,x'00'   Make it null
               endif

               OC VDP1600_DDNAME_OUTPUT,VDP1600_DDNAME_OUTPUT
               JZ  VDP1600D
               CLC VDP1600_DDNAME_OUTPUT,SPACES
               JE  VDP1600D
*
               MVC VWDDNAME,VDP1600_DDNAME_OUTPUT
               MVC VWVDPADR,VDPCURR
*
               j   VDPCOPY
*
VDP1600D       L R0,VDP1600_RECORD_ID
               CVD R0,DBLWORK
               OI DBLWORK+L'DBLWORK-1,X'0F'
               MVC VWDDNAME+0(3),OUT
               UNPK VWDDNAME+3(5),DBLWORK
               endif
*
static loctr
DELIMITER_TABLE       DS 0H                                        @01I
DELIM_COMMA          DC XL1'6B'        ."," x'6B' delim_id = 1     @01I
DELIM_TAB            DC XL1'05'        ." " x'05' delim_id = 2     @01I
DELIM_PIPE           DC XL1'4F'        ."|" x'4F' delim_id = 3     @01I
DELIM_CTRLA          DC XL1'01'        ." " x'01' delim_id = 4     @01I
DELIM_FWIDTH         DC XL1'00'        ." " x'00' delim_id = 5     @01I
DELIM_FSLASH         DC XL1'61'        ."/" x'61' delim_id = 6     @01I
DELIM_BSLASH         DC XL1'E0'        ."\" x'E0' delim_id = 7     @01I
DELIM_COLON          DC XL1'7A'        .":" x'7A' delim_id = 8     @01I
DELIM_SCOLON         DC XL1'5E'        .";" x'5E' delim_id = 9     @01I
DELIM_TILDE          DC XL1'A1'        ."~" x'A1' delim_id = 10    @01I
DELIM_LAST           EQU *-delimiter_table

string_delim_TABLE       DS 0H
text_quote           DC C''''          single quote
text_dquote          dc c'"'           double quote
text_LAST            EQU *-string_delim_table
code  loctr

               DROP R5
                         EJECT
             when 2000            COLUMN  RECORD ???
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P C O L U M N D E F I N I T I O N R E C O R D      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
               USING VDP2000_COLUMN_RECORD,R5
*
               bras r9,check_view_list
               if ltr,r15,r15,nz      Format view found

               L R7,COLDEFCR      LOAD CURRENT  COL   DEFN ENTRY   ADDR
               USING COLDEFN,R7
*
***********************************************************************
*        CHECK FOR COLUMN DEFINITION TABLE OVERFLOW                   *
***********************************************************************
               LHI R14,MSG#413    ASSUME TABLE OVERFLOW
               C R7,COLDEFMX      COLUMN DEFN  TABLE OVERFLOW ???
               JNL  RTNERROR      YES -  INDICATE ERROR
*
***********************************************************************
*        COPY COLUMN DEFINITION PARAMETERS                            *
***********************************************************************
               MVC CDCOLNO,VDP2000_COLUMN_ID+2 COLUMN    NUMBER
               MVC CDCOLID,VDP2000_RECORD_ID COLUMN ID
               XC CDCLCCOL,CDCLCCOL         CALCULATED   COLUMN SEQ NO
               XC CDCLCOFF,CDCLCOFF         CALCULATED   COLUMN OFFSET
               XC CDSRTDEF,CDSRTDEF         SORT   KEY   DEFINITION
*
               XR R0,R0                     COLUMN PRINT INDICATOR
               IC R0,VDP2000_HIDDEN_IND
               MVI CDPRTIND,C'Y'
               if CHI,R0,eq,HIDDEN
                 MVI CDPRTIND,C'N'
               endif
*
               MVC CDHEAD1,VDP2000_HEADER_LINE1 COLUMN HEADING 1
               MVC CDHEAD2,VDP2000_HEADER_LINE2 COLUMN HEADING 2
               MVC CDHEAD3,VDP2000_HEADER_LINE3 COLUMN HEADING 3
*
               LH R15,VDP2000_SUBTOTAL_TYPE_ID+2 SUBTOTAL option   pgc
               CHI R15,DETCALC             DETCALC option?         pgc
               JL VDP2000_SUBOPT                                   pgc
               CHI R15,RECALC              RECALC option?          pgc
               JH VDP2000_SUBOPT                                   pgc
               MVC CDCALOPT,VDP2000_SUBTOTAL_TYPE_ID+2 CALC option pgc
               MVI CDSUBOPT+1,SUBTOT        Default to SUBTOT(temp)pgc
               J VDP2000_SUBLVL                                    pgc
VDP2000_SUBOPT       DS 0H                                         pgc
               MVC CDSUBOPT,VDP2000_SUBTOTAL_TYPE_ID+2 SUBTOTAL option
VDP2000_SUBLVL       DS 0H                                         pgc
               XC CDSUBLVL,CDSUBLVL         SUBTOTAL   LEVEL   NO
               MVI CDSUBCNT,C'N'            SUBTOTAL   COUNT   IND
               MVC CDFLDID,VDP2000_FIELD_ID COLUMN FIELD  ID
               XC CDMSKLEN,CDMSKLEN         INITIALIZE  MASK   LENGTH
*
               MVC CDSUBLBL,VDP2000_SUBTOTAL_PREFIX SUBTOTAL LABEL
*
               LHI R0,L'CDSUBLBL            LOAD SUBTOTAL LABEL LENGTH
               LA R15,CDSUBLBL
               AR R15,R0
*
               BALR R14,0         REMOVE TRAILING  BLANKS FROM  LENGTH
               BCTR R15,0
               CLI 0(R15),X'40'   TEST FOR TRAILING SPACE
               JNE  VDP2000X012
               BCTR R0,R14        BRANCH TO R14 (LOADED ABOVE)
*
VDP2000X012    EQU *
               STH R0,CDSUBLEN    STORE SUBTOTAL LABEL LENGTH
*
               MVC CDEXAREA,VDP2000_EXTRACT_AREA_ID+2   DATA   AREA ID
*
               LH R0,VDP2000_EXTRACT_AREA_POSITION      DATA   AREA OFF
               BCTR R0,0
               STH R0,CDDATOFF
*
               LH R0,VDP2000_FIELD_LENGTH               DATA   AREA LEN
               BCTR R0,0
               STH R0,CDDATLEN
*
               XC CDCOLOFF,CDCOLOFF
*
               LH R0,VDP2000_FIELD_LENGTH      COLUMN   SIZE (WIDTH-1)
               BCTR R0,0
               STH R0,CDCOLSIZ
*
               LH R0,VDP2000_SPACES_BEFORE_COLUMN INTERCOLUMN GAP
               if TM,VWFLAG2,VWBLDCSV,nz
                 LHI R0,1
                 LH R15,CDCOLNO
                 if CHI,R15,eq,1
                   XR R0,R0
                 endif
               endif
               STH R0,CDCOLGAP
*
               MVC CDOUTFMT,VDP2000_FORMAT_ID+2 COLUMN  OUTPUT  FORMAT
            MVC CDOUTCON,VDP2000_FIELD_CONTENT_ID+2 COLM OUTPUT CONTENT
*
               LH R0,VDP2000_DECIMAL_COUNT     COLUMN   NO  OF  DECIMAL
               stc r0,CDNDEC
*
               LH R0,VDP2000_ROUNDING          COLUMN   ROUNDING FACTOR
               STC R0,CDRNDFAC
*
               XR R0,R0                        SIGNED   VALUE    IND
               IC R0,VDP2000_SIGNED_IND
               MVI CDSIGNED,C'N'
               if CHI,R0,eq,SIGNED
                 MVI CDSIGNED,C'Y'
               endif
*
               L R15,VDP2000_HEADER_JUSTIFY_ID COLUMN HDR  JUSTIFY CODE
               if chi,R15,gt,3
                 LHI R15,3
               endif
               LA R14,JUSTTBL(R15)
               MVC CDHDJUST,0(R14)
*
               L R15,VDP2000_JUSTIFY_ID        COLUMN DATA JUSTIFY CODE
               if chi,R15,gt,3
                 LHI R15,3
               endif
               LA R14,JUSTTBL(R15)
               MVC CDOUTJUS,0(R14)
*
***********************************************************************
*        PROCESS OUTPUT COLUMN MASKS                                  *
***********************************************************************
               MVC CDDETMSK,SPACES
               XC CDMSKLEN,CDMSKLEN
*
               LA R14,L'MASKCODE-1
               if EXrl,R14,MASKBLNK,ne

              if CLI,CDOUTFMT+1,eq,FM_EDIT, FORCE MASK IF EDITED NUM   +
               orif,TM,VWFLAG2,VWBLDCSV,o,and, or CSV and num          +
               CLI,CDOUTFMT+1,ne,FM_ALNUM,and,                         +
               CLI,CDOUTFMT+1,ne,FM_ALPHA
***********************************************************************
*          BUILD EDITED NUMERIC MASK                                  *
***********************************************************************
                   LH R15,VDP2000_DECIMAL_COUNT OUTPUT DECIMALS
                   if CHI,R15,gt,MAXDEC
                     LHI R15,MAXDEC
                   endif

                   LHI R0,MAXDEC               COMPUTE RIGHT TRUNCATION
                   SR R0,R15                   EXCESS  MASK  DECIMALS
                   if CHI,R0,eq,MAXDEC         ANY DECIMALS?
                     AHI R0,1
                   endif

                   LA R1,CSVMASK+L'CSVMASK-1   LOAD END OF MASK ADDR -1
                   SR R1,R0                    ADJUST  FOR NO DECIMALS
                   LH R15,CDCOLSIZ             LOAD COLUMN WIDTH (-1)
                   SR R1,R15                   BACKUP  TO  BEGINNING
                   EXrl R15,CSVMSKMV             REPLACE MASK
                   AHI R15,1
                   STH R15,CDMSKLEN

                   if CLI,CDSIGNED,eq,C'Y'     SIGNED ???
                     MVI VDP2000_REPORT_MASK,C'L' FORCE LEFT SIGN ???
                   endif

                   if CLI,VDP2000_REPORT_MASK,eq,C'L' LEFT SIGN ???
                     MVI CDDETMSK,C'-'
                   endif

                   MVI CDOUTFMT+1,FM_EDIT COLUMN OUTPUT FORMAT
                   MVI CDOUTJUS,C'L'

                 else

***********************************************************************
*          FILE FORMAT OUTPUT FILE                                    *
***********************************************************************
*                                        ignr MASK UNLESS "MASKD NUM"
                  if (CLI,CDOUTFMT+1,eq,FM_MASK),                      +
               or,(CLI,VWDESTYP+1,ne,FILEFMT)

                     LA R1,VDP2000_REPORT_MASK USE ORIGINAL MASK CODE
                     LA R14,CDDETMSK   OUTPUT  AREA
                     BRAS R9,MASKCONV
                   endif
                 endif
               endif

***********************************************************************
*        UPDATE "HIGHWATER" VALUES                                    *
***********************************************************************
               lh r0,cdcolno      load column number
               if CH,R0,gt,VWMAXCOL LARGER THAN PREVIOUS MAXIMUM ???
                 STH R0,VWMAXCOL  YES - UPDATE
               endif

               if CH,R0,gt,MAXCOL LARGER  THAN PREVIOUS RECORDED MAX ??
                 STH R0,MAXCOL    YES - UPDATE MAXIMUM
               endif

***********************************************************************
*        MARK COLUMNS INVOLVED IN SUBTOTALING/CALCULATIONS            *
***********************************************************************

               if CLI,CDSUBOPT+1,ne,NOSUBTOT NO SUBTOTALING ???
                 MVC CDCLCCOL,HEXFF
               endif

***********************************************************************
*        MARK COLUMNS INVOLVED IN EXCEPTION LOGIC                     *
***********************************************************************
               L R1,VWEXCOND      LOAD  EXCP  TABLE ADDRESS
               USING CALCTBL,R1
*
               LH R15,VWEXCNT     LOAD  EXCP  TABLE COUNT
               if LTR,R15,R15,P   ANY   EXCEPTION   CONDITIONS  ???
                 do from=(r15)
                   if CLI,CALCOPER+1,eq,PUSHC COLUMN# ???
                     llgt r14,calcOP2A LOAD OPERAND 2 COLUMN NUMBER
                     if CH,R14,eq,CDCOLNO MATCHING COL# ???
                       MVC CDCLCCOL,HEXFF
                       leave ,
                     endif
                   endif
                   AHI R1,CALCLEN
                 enddo
               endif
               drop r1

***********************************************************************
*        INITIALIZE COLUMN SPECIFIC VARIABLES                         *
***********************************************************************
               XC CDCALCNT,CDCALCNT INITIALIZE CALCULATION COUNT
               XC CDCALCTB,CDCALCTB INITIALIZE CALCULATION TABLE  ADDR
               XC CDFMTFUN,CDFMTFUN INITIALIZE FORMATTING FUNCTION CODE
*
***********************************************************************
*        UPDATE OUTPUT RECORD LENGTH                                  *
***********************************************************************
               CLI CDPRTIND,C'Y'  OUTPUT THIS COLUMN ???
               JNE  VDP2000P      NO  -  SKIP OUTPUT POSITION TRACKING
*
VDP2000K       CLC CDDETMSK,SPACES DETAIL  MASK  SPECIFIED ???
               JE  VDP2000L       NO  -   CHECK  SUBTOTAL MASK
*
               BRAS R9,CKSTDMSK   CONVERT DETAIL MASK
               j   VDP2000P
                           SPACE 3
VDP2000L       CLI CDEXAREA+1,CTAREA DATA COMING FROM "CT" AREA ???
               JNE  VDP2000P      NO  - USE "DL96"
*
               LHI R0,5
               LA R1,FMTFUNTB     POINT TO FORMATTING FUNCTION TABLE
VDP2000M       CLC CDOUTFMT,0(R1) MATCHING OUTPUT FORMAT ???
               JNE VDP2000X014    YES - SET FORMATTING  CODE
               CLC CDSIGNED,2(R1) MATCHING SIGNS ???
               JE  VDP2000N
VDP2000X014    EQU *
               AHI R1,5           LOOP THROUGH WHOLE TABLE
               BRCT R0,VDP2000M
               j   VDP2000P
*
VDP2000N       LH R0,3(,R1)       LOAD FUNCTION CODE  FOR INLINE CODE
               LH R1,CDCOLSIZ     LENGTH EXCEEDS  16  ???
               CHI R1,16-1
               JNH VDP2000X016    NO  -  USE  INLINE  CODE
               XR R0,R0           ZERO FUNCTION CODE  FOR "GVBDL96"
VDP2000X016    EQU *
               ST R0,CDFMTFUN
                           SPACE 3
VDP2000P       OC VWCOLADR,VWCOLADR FIRST COLUMN ADDRESS INITIALIZED
               JNZ VDP2000X018
               ST R7,VWCOLADR
*
VDP2000X018    EQU *
               LH R0,VWCOLCNT     INCREMENT COLUMN COUNT
               AHI R0,1
               STH R0,VWCOLCNT
*
               AHI R7,CDENTLEN    ADVANCE  TO NEXT COLUMN OCCURRENCE
               ST R7,COLDEFCR
*
code_2         loctr
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*            C H E C K F O R C O M M O N   O U T P U T   M A S K S    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
CKSTDMSK     CLI CDEXAREA+1,SKAREA SOURCE FROM "SK" AREA ???
             JE  CKSTDXIT         YES -  ASSUME NOT PACKED
*
             LA R15,CDDETMSK+L'CDDETMSK  ADDRESS OF LAST BYTE (+1)
*
CKSTDLP      BCTR R15,0           BACKUP   ONE POSITION
             CLI 0(R15),C' '      TRAILING BLANK ???
             JE  CKSTDLP          YES - CONTINUE SEARCH
*
             LA R1,CDDETMSK       COMPUTE  MASK  LENGTH (-1)
             SR R15,R1
*
             LHI R0,24            ASSUME   MASK  1
             LA R14,STDMASK1+L'STDMASK1-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 1    ???
             JNE CKSTDLP012       NO  - CHECK NEXT MASK
             CLI VDP2000_REPORT_MASK,C'U' UNSIGNED MASK CODE ???
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
CKSTDLP012   EQU *
             LHI R0,28            ASSUME  MASK 2
             LA R14,STDMASK2+L'STDMASK2-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 2  ???
             JE  CKSTDSET         YES - SET FORMATTING  OPTION
*
             LHI R0,32            ASSUME  MASK 3
             LA R14,STDMASK3+L'STDMASK3-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 3  ???
             JE  CKSTDSET         YES - SET FORMATTING  OPTION
*
             LHI R0,36            ASSUME  MASK 4
             LA R14,STDMASK4+L'STDMASK4-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 4  ???
             JE  CKSTDSET         YES - SET FORMATTING  OPTION
*
             LHI R0,40            ASSUME  MASK 5
             LA R14,STDMASK5+L'STDMASK5-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 5    ???
             JNE CKSTDLP014       NO  - CHECK NEXT MASK
             CLI VDP2000_REPORT_MASK,C'U' UNSIGNED MASK CODE ???
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
CKSTDLP014   EQU *
             LHI R0,44            ASSUME  MASK 6
             LA R14,STDMASK6+L'STDMASK6-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 6  ???
             JE  CKSTDSET         YES - SET FORMATTING  OPTION
*
             LHI R0,48            ASSUME  MASK 7
             LA R14,STDMASK7+L'STDMASK7-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 7    ???
             JNE CKSTDLP016       NO  - CHECK NEXT MASK
             CLI VDP2000_REPORT_MASK,C'U' UNSIGNED MASK CODE ???
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
CKSTDLP016   EQU *
             LHI R0,52            ASSUME  MASK 8
             LA R14,STDMASK8+L'STDMASK8-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 8    ???
             JNE CKSTDLP018       NO  - CHECK NEXT MASK
             CLI VDP2000_REPORT_MASK,C'U' UNSIGNED MASK CODE ???
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
CKSTDLP018   EQU *
             LHI R0,56            ASSUME  MASK 9
             LA R14,STDMASK9+L'STDMASK9-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 9    ???
             JNE CKSTDLP020       NO  - CHECK NEXT MASK
             CLI VDP2000_REPORT_MASK,C'U' UNSIGNED MASK CODE ???
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
CKSTDLP020   EQU *
             LHI R0,60            ASSUME  MASK 10
             LA R14,STDMASKA+L'STDMASKA-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 10   ???
             JNE CKSTDLP022       NO  - CHECK NEXT MASK
             CLI VDP2000_REPORT_MASK,C'U' UNSIGNED MASK CODE ???
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
CKSTDLP022   EQU *
             LR R0,R15            SAVE ORIGINAL MASK LENGTH
             CLI 0(R1),C'-'       LEADING MINUS SIGN ???
             JNE CKSTDLP024       NO  -  BYPASS ADJUSTMENT
             BCTR R15,0           YES - EXCLUDE MINUS FROM COMPARE
             LA R1,1(,R1)
*
CKSTDLP024   EQU *
             LA R14,STDMASKB+L'STDMASKB-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 11
             LR R15,R0            RESTORE ORIGINAL LENGTH
             LA R1,CDDETMSK       RESTORE ORIGINAL ADDRESS
             LA R0,64             ASSUME  MASK 11
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
             LR R0,R15            SAVE ORIGINAL MASK LENGTH
             CLI 0(R1),C'-'       LEADING MINUS SIGN ???
             JNE CKSTDLP026       NO  -  BYPASS ADJUSTMENT
             BCTR R15,0           YES - EXCLUDE MINUS FROM COMPARE
             LA R1,1(,R1)
*
CKSTDLP026   EQU *
             LA R14,STDMASKC+L'STDMASKC-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 12
             LR R15,R0            RESTORE ORIGINAL LENGTH
             LA R1,CDDETMSK       RESTORE ORIGINAL ADDRESS
             LA R0,68             ASSUME  MASK 12
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
             LR R0,R15            SAVE ORIGINAL MASK LENGTH
             CLI 0(R1),C'-'       LEADING MINUS SIGN ???
             JNE CKSTDLP028       NO  -  BYPASS ADJUSTMENT
             BCTR R15,0           YES - EXCLUDE MINUS FROM COMPARE
             LA R1,1(,R1)
*
CKSTDLP028   EQU *
             LA R14,STDMASKD+L'STDMASKD-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 13
             LR R15,R0            RESTORE ORIGINAL LENGTH
             LA R1,CDDETMSK       RESTORE ORIGINAL ADDRESS
             LA R0,72             ASSUME  MASK 13
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
             LR R0,R15            SAVE ORIGINAL MASK LENGTH
             CLI 0(R1),C'-'       LEADING MINUS SIGN ???
             JNE CKSTDLP030       NO  -  BYPASS ADJUSTMENT
             BCTR R15,0           YES - EXCLUDE MINUS FROM COMPARE
             LA R1,1(,R1)
*
CKSTDLP030   EQU *
             LA R14,STDMASKE+L'STDMASKE-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 14
             LR R15,R0            RESTORE ORIGINAL LENGTH
             LA R1,CDDETMSK       RESTORE ORIGINAL ADDRESS
             LA R0,76             ASSUME  MASK 14
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
             LHI R0,80            ASSUME  MASK 15
             LA R14,STDMASKF+L'STDMASKF-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 15
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
             LHI R0,84            ASSUME  MASK 16
             LA R14,STDMASKG+L'STDMASKG-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 16
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
             LHI R0,88            ASSUME  MASK 17
             LA R14,STDMASKH+L'STDMASKH-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 17
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
             LHI R0,92            ASSUME  MASK 18
             LA R14,STDMASKI+L'STDMASKI-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 18
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
             LR R0,R15            SAVE ORIGINAL MASK LENGTH
             CLI 0(R1),C'-'       LEADING MINUS SIGN ???
             JNE CKSTDLP032       NO  -  BYPASS ADJUSTMENT
             BCTR R15,0           YES - EXCLUDE MINUS FROM COMPARE
             LA R1,1(,R1)
*
CKSTDLP032   EQU *
             LA R14,STDMASKJ+L'STDMASKJ-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 19
             LR R15,R0            RESTORE ORIGINAL LENGTH
             LA R1,CDDETMSK       RESTORE ORIGINAL ADDRESS
             LA R0,96             ASSUME  MASK 19
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
             LR R0,R15            SAVE ORIGINAL MASK LENGTH
             CLI 0(R1),C'-'       LEADING MINUS SIGN ???
             JNE CKSTDLP034       NO  -  BYPASS ADJUSTMENT
             BCTR R15,0           YES - EXCLUDE MINUS FROM COMPARE
             LA R1,1(,R1)
*
CKSTDLP034   EQU *
             LA R14,STDMASKK+L'STDMASKK-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 20
             LR R15,R0            RESTORE ORIGINAL LENGTH
             LA R1,CDDETMSK       RESTORE ORIGINAL ADDRESS
             LA R0,100            ASSUME  MASK 20
             JE  CKSTDSET         YES -  THIS MASK WILL WORK
*
             LR R0,R15            SAVE ORIGINAL MASK LENGTH
             CLI 0(R1),C'-'       LEADING MINUS SIGN ???
             JNE CKSTDLP036       NO  -  BYPASS ADJUSTMENT
             BCTR R15,0           YES - EXCLUDE MINUS FROM COMPARE
             LA R1,1(,R1)
*
CKSTDLP036   EQU *
             LA R14,STDMASKL+L'STDMASKL-1
             SR R14,R15
             exrl R15,CKMASK      MATCHES STANDARD MASK 21
             LR R15,R0            RESTORE ORIGINAL LENGTH
             LA R1,CDDETMSK       RESTORE ORIGINAL ADDRESS
             LA R0,104            ASSUME  MASK 21
             JNE  CKSTDSGN        NO  - DIDN'T MATCH ANY - RETURN
*
CKSTDSET     ST R0,CDFMTFUN       SAVE  FORMATTING FUNCTION  CODE
             AHI R15,1            SAVE  MASK   LENGTH
             STH R15,CDMSKLEN
*
CKSTDSGN     CLI VDP2000_REPORT_MASK,C'L' SIGN ON LEFT ???
             JNE CKSTDLP038
             MVI CDDETMSK,C'-'
*
CKSTDLP038   EQU *
             CLI VDP2000_REPORT_MASK,C'P' SIGN ON LEFT ???
             JNE CKSTDXIT
             MVI CDDETMSK,C'('
*
CKSTDXIT     BR R9                RETURN
*
code         loctr
             DROP R5
             DROP R7
             endif
                         EJECT
             when 2210            COLUMN  CALCULATION   RECORD ???
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P C O L U M N C A L C U L A T I O N S T A C K      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
               USING VDP2210_SUM_CALCULATION_RECORD,R5
*
               bras r9,check_view_list
               if ltr,r15,r15,nz      Format view found

               L R6,CLCDEFCR      LOAD CURRENT  CALC  DEFN ENTRY   ADDR
               USING CALCTBL,R6
*
***********************************************************************
*        FIND MATCHING COLUMN DEFINITION FOR CALCULATION              *
***********************************************************************
               LH R0,VWCOLCNT     LOAD COLUMN COUNT
               L R7,VWCOLADR      LOAD FIRST COLUMN DEFN   ADDRESS
               USING COLDEFN,R7
*
VDP2210A       CLC VDP2210_COLUMN_ID,CDCOLID MATCHING COLUMN ID ???
               JE  VDP2210B
               AHI R7,CDENTLEN
               BRCT R0,VDP2210A
*
               LHI R14,MSG#420    UNABLE TO LOCATE MATCHING COLUMN DEFN
               j   VDPERR         YES -  INDICATE  ERROR
*
***********************************************************************
*        PROCESS CALCULATION STACK                                    *
***********************************************************************
VDP2210B       ST R6,CDCALCTB     SAVE FIRST CALCULATION   ADDRESS
               MVC CDCLCCOL,HEXFF
*
               CLI CDCALOPT+1,DETCALC CALCULATION OPTION SPECIFIED ???
               JE VDP2210B_5      YES -  SKIP DEFAULT
               CLI CDCALOPT+1,BRKCALC
               JE VDP2210B_5      YES -  SKIP DEFAULT
               CLI CDCALOPT+1,RECALC
               JE VDP2210B_5      YES -  SKIP DEFAULT
               MVI CDCALOPT+1,DETCALC DEFAULT = BREAK CALC
VDP2210B_5       DS 0H
*
               LA R14,ACCUMWRK    INITIALIZE CURRENT STACK ELEMENT ADDR
               ST R14,CURRSTAK
*
               LA R3,VDP2210_STACK
               AH R3,VDP2210_STACK_LENGTH
*
               LA R5,VDP2210_STACK
               USING VDP2210_STACK_FUNCTION,R5
*
               CR R5,R3           EMPTY  STACK ???
               JNL  VDP2210Z
*
               LR R4,R5           SAVE CALCULATION ORIGIN
                           EJECT
***********************************************************************
*        CHECK FOR COLUMN CALCULATION DEFINITION TABLE OVERFLOW       *
***********************************************************************
VDP2210C       LHI  R14,MSG#419     ASSUME TABLE OVERFLOW
               LA   R0,CALCLEN(,R6) CALC   DEFN  TABLE OVERFLOW  ???
               C    R0,CLCDEFMX
               JH   RTNERROR        YES -  INDICATE ERROR
*
              L R0,VDP2210_STACK_OPERATION_CODE COPY FUNCTION (OP CODE)
               STH R0,CALCOPER
*
***********************************************************************
*        BRANCH                                                       *
***********************************************************************
               CLI CALCOPER+1,BRNCH UNCONDITIONAL  BRANCH  ???
               JNE  VDP2210D      NO  - CHECK FOR  NEGATE
               llgt R14,CALCBRN   LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   CALCOFFS
*
***********************************************************************
*        NEGATE                                                       *
***********************************************************************
VDP2210D       CLI CALCOPER+1,NEG NEGATE VALUE ???
               JNE  VDP2210E      NO  - CHECK ABSOLUTE VALUE
               llgt r14,calcNEG   LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
             AHI R5,L'VDP2210_STACK_OPERATION_CODE ADVANCE TO NEXT FUNC
               j   CALCNEXT
*
***********************************************************************
*        ABSOLUTE VALUE                                               *
***********************************************************************
VDP2210E       CLI CALCOPER+1,ABS ABSOLUTE  VALUE  ???
               JNE  VDP2210F      NO  - CHECK PUSH VALUE (CONSTANT)
               llgt r14,calcNEG   LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
             AHI R5,L'VDP2210_STACK_OPERATION_CODE ADVANCE TO NEXT FUNC
               j   CALCNEXT
*
***********************************************************************
*        PUSH VALUE (CONSTANT)                                        *
***********************************************************************
VDP2210F       CLI CALCOPER+1,PUSHV PUSH VALUE ???
               JNE  VDP2210I      NO  - CHECK PUSH COLUMN
*
               LA R0,L'VDP2210_STACK_PUSH_NBR INITIALIZE LOOP COUNTER
               LA R1,VDP2210_STACK_PUSH_NBR SCAN FOR TRAILING SPACE
VDP2210G       CLI 0(R1),X'40'
               JE VDP2210G010
               LA R1,1(,R1)
               BRCT R0,VDP2210G
               LA R1,VDP2210_STACK_PUSH_NBR+L'VDP2210_STACK_PUSH_NBR
*
VDP2210G010    EQU *
               LA R14,VDP2210_STACK_PUSH_NBR
               llgtr r14,r14
               stg R14,savaladr
               SR R1,R14
               STH R1,savallen
               LHI R0,L'CALCVALU  RESULT LENGTH
               STH R0,dl96len
*
               xr  R0,r0          create a zero
               STH R0,SAMSKLEN    set mask length to zero
               ST R0,SAMSKADR      as well as the address
*
               MVI SAVALFMT+1,FC_EDIT ASSUME NUMERIC SOURCE
*
               XC SAVALCON,SAOUTCON
               MVHHI SAVALDEC,0
               MVHHI SAVALRND,0
               MVI SAVALSGN,C'N'
*
               MVI SAOUTFMT+1,FC_float RESULT ATTRIBUTES
               XC SAOUTCON,SAOUTCON
*
               MVHHI saoutdec,8   Tell formatting to output 8 dp
*
               MVHHI SAOUTRND,0
               MVI SAOUTSGN,C'Y'
*
               LA R0,CALCVALU     RESULT ADDRESS
               ST R0,dl96tgta
               ST R0,CALCOP2A     SAVE OPERAND 2 ADDRESS
*
               LA R1,DL96LIST
               Llgf  R15,GVBDL96A
               bassm R14,R15
*
               LTR R15,R15
               JZ  VDP2210H
*
               CHI R15,2          TRUNCATION ???
               JE  VDP2210H
*
               ST    R15,DBLWORK2+4
               UNPK  DBLWORK(9),DBLWORK2+4(5)
               TR    DBLWORK,HEXTAB
               MVC   ERRDATA(8),DBLWORK
               LHI   R14,MSG#409        LOAD   ERROR MESSAGE NUMBER
               j     RTNERROR           PRINT  ERROR MESSAGE
*
VDP2210H       L R14,CURRSTAK     SET  TARGET = CURRENT STACK
               ST R14,CALCTGTA
               AHI R14,AccumDFPl  UPDATE  CURRENT STACK ADDRESS
               ST R14,CURRSTAK
*
               llgt r14,calcPSHV  LOAD PUSH VALUE FUNCTION
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
*
              AHI R5,L'VDP2210_STACK_PUSH_CONSTANT ADVANCE TO NEXT FUNC
               j   CALCNEXT
*
***********************************************************************
*        PUSH COLUMN NUMBER                                           *
***********************************************************************
VDP2210I       CLI CALCOPER+1,PUSHC PUSH COLUMN# ???
               JNE  VDP2210N      NO  - POP STACK
*
               L R14,VDP2210_STACK_PUSH_COL_NBR LOAD VALUE COLUMN NO
               ST R14,CALCOP2A    SAVE OPERAND  2   COLUMN  NUMBER
               CVD R14,DBLWORK    SAVE THE   COLUMN NUMBER
               lg  r15,dblwork    get the packed number in r15
               cdstr fp1,r15      convert to long dfp in fp1
               lxdtr fp0,fp1,0    make it an extended number
               GVBSTX fp0,calcvalu   and save that
*
               L R1,VWCOLADR      LOAD FIRST COLUMN DEFN ADDRESS
VDP2210J       CR R1,R7           REACHED   CURRENT COLUMN   ???
               JH  VDP2210L       YES - EXIT   SCAN (NOT FOUND)
               CH R14,CDCOLNO-COLDEFN(R1) MATCHING COL# ???
               JNE  VDP2210K
               MVC CDCLCCOL-COLDEFN(L'CDCLCCOL,R1),HEXFF
               j   VDP2210M
*
VDP2210K       AHI R1,CDENTLEN
               j   VDP2210J       CONTINUE SEARCH FOR REFERENCED  COL
*
VDP2210L       LHI R14,MSG#421    UNABLE TO LOCATE MATCHING COLUMN DEFN
               j   VDPERR         YES -  INDICATE ERROR
*
VDP2210M       L R14,CURRSTAK     SET  TARGET = CURRENT STACK
               ST R14,CALCTGTA
               AHI R14,AccumDFPl  UPDATE  CURRENT STACK ADDRESS
               ST R14,CURRSTAK
*
               llgt r14,calcPSHC  LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               AHI R5,L'VDP2210_STACK_PUSH_COLUMN ADVANCE TO NEXT FUNC
               j   CALCNEXT
*
***********************************************************************
*        POP STACK ASSOCIATED WITH ARITHMETIC FUNCTION                *
***********************************************************************
VDP2210N       L R14,CURRSTAK     SAVE PAIR OF STACK ADDRESSES && POP
               ahi R14,-AccumDFPl BACKUP ONE ENTRY
               ST R14,CALCOP2A    SAVE ADDRESS OF 2ND  IN PAIR
*
               ST R14,CURRSTAK    UPDATE CURRENT  STACK   POSITION
*
               ahi R14,-AccumDFPl BACKUP ONE ENTRY
               ST R14,CALCTGTA    SAVE ADDRESS OF 1ST  IN PAIR
*
CHKEQ          CLI CALCOPER+1,COMPEQ COMPARE EQUAL ???
               JNE  CHKNE         NO  - CHECK NOT EQUAL
               ST R14,CURRSTAK    UPDATE CURRENT  STACK  POSITION
               llgt r14,calcEQ    LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   CALCOFFS       SEARCH  FOR BRANCH OFFSET
*
CHKNE          CLI CALCOPER+1,COMPNE COMPARE NOT EQUAL ???
               JNE  CHKGT         NO  - CHECK GREATER  THAN
               ST R14,CURRSTAK    UPDATE CURRENT  STACK  POSITION
               llgt r14,calcNE    LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   CALCOFFS       SEARCH  FOR BRANCH OFFSET
*
CHKGT          CLI CALCOPER+1,COMPGT COMPARE GREATER THAN ???
               JNE  CHKGE         NO  - CHECK  GREATER THAN  OR EQUAL
               ST R14,CURRSTAK    UPDATE CURRENT  STACK  POSITION
               llgt r14,calcGT    LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   CALCOFFS       SEARCH  FOR BRANCH OFFSET
*
CHKGE          CLI CALCOPER+1,COMPGE COMPARE GREATER THAN OR EQUAL ???
               JNE  CHKLT         NO  - CHECK   LESS THAN
               ST R14,CURRSTAK    UPDATE CURRENT  STACK   POSITION
               llgt r14,calcGE    LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   CALCOFFS       SEARCH  FOR BRANCH OFFSET
*
CHKLT          CLI CALCOPER+1,COMPLT COMPARE LESS THAN ???
               JNE  CHKLE         NO  - CHECK LESS THAN OR EQUAL
               ST R14,CURRSTAK    UPDATE CURRENT  STACK POSITION
               llgt r14,calcLT    LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   CALCOFFS       SEARCH  FOR BRANCH OFFSET
*
CHKLE          CLI CALCOPER+1,COMPLE COMPARE LESS THAN OR EQUAL ???
               JNE  CHKADD        NO  -  CHECK ADDITION
               ST R14,CURRSTAK    UPDATE CURRENT  STACK POSITION
               llgt r14,calcLE    LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
               j   CALCOFFS       SEARCH  FOR BRANCH OFFSET
*
CHKADD         CLI CALCOPER+1,ADD ADDITION        ???
               JNE  CHKSUB        NO  - CHECK SUBTRACTION
               llgt r14,calcADDA  LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
             AHI R5,L'VDP2210_STACK_OPERATION_CODE ADVANCE TO NEXT FUNC
               j   CALCNEXT
*
CHKSUB         CLI CALCOPER+1,SUB SUBTRACTION     ???
               JNE  CHKMULT       NO  - CHECK MULTIPLICATION
               llgt r14,calcSUBA  LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
             AHI R5,L'VDP2210_STACK_OPERATION_CODE ADVANCE TO NEXT FUNC
               j   CALCNEXT
*
CHKMULT        CLI CALCOPER+1,MULT MULTIPLICATION ???
               JNE  CHKDIV        NO  - CHECK DIVISION
               llgt r14,calcMULA  LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
             AHI R5,L'VDP2210_STACK_OPERATION_CODE ADVANCE TO NEXT FUNC
               j   CALCNEXT
*
CHKDIV         CLI CALCOPER+1,DIV DIVISION        ???
               JNE  CALCBAD       NO  - CHECK FOR LAST CALCULATION
               llgt r14,calcDIVA  LOAD SUBROUTINE ADDRESS
               ST R14,CALCFUNC    SAVE SUBROUTINE ADDRESS
             AHI R5,L'VDP2210_STACK_OPERATION_CODE ADVANCE TO NEXT FUNC
               j   CALCNEXT
*
CALCBAD        LHI R14,MSG#422    LOAD ERROR MESSAGE  NUMBER
*
               MVC ERRDATA(L'CALCMSG),CALCMSG
*
                 L R0,VWVIEW#     INDICATE   WHICH    VIEW
                 CVD R0,DBLWORK
                 OI DBLWORK+L'DBLWORK-1,X'0F'
                 MVC ERRDATA+02(8),VALUMSK1
                 ED ERRDATA+02(8),DBLWORK+4
*
                 LH R0,CDCOLNO    INDICATE   WHICH    COLUMN
                 CVD R0,DBLWORK
                 OI DBLWORK+L'DBLWORK-1,X'0F'
                 UNPK ERRDATA+14(4),DBLWORK
*
                 LH R0,CDCALCNT   INDICATE   WHICH    CALCULATION
                 AHI R0,1
                 CVD R0,DBLWORK
                 OI DBLWORK+L'DBLWORK-1,X'0F'
                 UNPK ERRDATA+14(4),DBLWORK
*
                 LH R0,CALCOPER   INDICATE   WHICH    OPERATOR
                 CVD R0,DBLWORK
                 OI DBLWORK+L'DBLWORK-1,X'0F'
                 UNPK ERRDATA+33(2),DBLWORK
*
                 j   RTNERROR
*
***********************************************************************
*     COUNT NUMBER OF STACK OPERATIONS SKIPPED TO REACH BRANCH OFFSET *
***********************************************************************
CALCOFFS         L R14,VDP2210_STACK_BRANCH_OFFSET
                 LTR R1,R14       "SKIPIF" (OFFSET=X'FF')  ???
                 JNM  CALCOFF2    NO  - CHECK  OFFSET
*
                 LHI R1,-2
                 j   CALCOFF8
*
CALCOFF2         AR R14,R4        ADD  ORIGIN  TO  OFFSET
                 CR R14,R3        BRANCH  TO END-OF-STACK  ???
                 JNE  CALCOFF3
*
                 LHI R1,-2
                 LA R0,ACCUMWRK   STACK EMPTY  ???
                 C R0,CURRSTAK
                 JE  CALCOFF8
                 LHI R1,-1
                 j   CALCOFF8
*
CALCOFF3         LA R0,ACCUMWRK  FLUSH STACK
                 ST R0,CURRSTAK
*
               LA R1,L'VDP2210_STACK_BRANCH(,R5) POINT TO NEXT STACK OP
                 SR R0,R0         ZERO STACK OPERATIONS SKIPPED COUNT
*
CALCOFF4         CR R1,R14        REACHED  BRANCH ADDRESS ???
                 JNL  CALCOFF6    YES - EXIT LOOP
                 AHI R0,1         INCREMENT  SKIPPED  OPERATION COUNT
           L R15,VDP2210_STACK_BRANCH_OP_ID-VDP2210_STACK_FUNCTION(,R1)
                 SLL R15,1
                 AH R1,STACKTBL(R15)
                 j   CALCOFF4     LOOP UNTIL BRANCH OFFSET REACHED
*
CALCOFF6         LHI R1,CALCLEN   COMPUTE OFFSET TO NEW STACK FUNC
                 MR R0,R0
CALCOFF8         ST R1,CALCVALU
*
                 LA R0,ACCUMWRK   FLUSH STACK
                 ST R0,CURRSTAK
*
                 AHI R5,L'VDP2210_STACK_BRANCH ADVANCE TO NEXT STACK OP
*
CALCNEXT         LH R0,CDCALCNT   INCREMENT CALCULATION COUNT
                 AHI R0,1
                 STH R0,CDCALCNT
*
                 CR R5,R3         END-OF-STACK  ???
                 JNL  VDP2210X    YES -  EXIT
*
                 AHI R6,CALCLEN   ADVANCE  TO NEXT CALCULATION FUNC
                 ST R6,CLCDEFCR
                 j   VDP2210C
*
VDP2210X         AHI R6,CALCLEN   ADVANCE  TO NEXT CALCULATION FUNC
                 MVC 0(4,R6),HEXFF INSERT END-OF-STACK MARKER
                 AHI R6,4
                 ST R6,CLCDEFCR
*
VDP2210Z         ds 0h
*
                 DROP R5
                 DROP R6
                 DROP R7
               endif
                         EJECT
             when 2300            SORT KEY ATTR  ???
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*              V D P S O R T K E Y D E F I N I T I O N   R E C O R D  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
               USING VDP2300_SORT_KEY_ATTR_RECORD,R5
*
               bras r9,check_view_list
               if ltr,r15,r15,nz      Format view found

                 L R6,SRTDEFCR    LOAD CURRENT SORTKEY DEFN ENTRY  ADDR
                 USING SORTKEY,R6
*
***********************************************************************
*          CHECK FOR SORT KEY DEFINITION TABLE OVERFLOW               *
***********************************************************************
                 LHI R14,MSG#423  ASSUME TABLE OVERFLOW
                 C   R6,SRTDEFMX  SORT   KEY   TABLE OVERFLOW  ???
                 JNL RTNERROR     YES -  INDICATE ERROR
*
***********************************************************************
*          COPY SORT KEY ATTRIBUTES                                   *
***********************************************************************
                 LH  R0,VWSRTCNT  INCREMENT SORT KEY COUNT
                 AHI R0,1
                 STH R0,VWSRTCNT
*
                 MVC SKSRTORD,VDP2300_SEQUENCE_NBR
                 MVC SKCOLID,VDP2300_COLUMN_ID
*
                 MVC SKCOLSIZ,VDP2300_SK_FIELD_LENGTH SORT COLUMN  SIZE
                 MVC SKLABEL,VDP2300_SORT_KEY_PREFIX  SORT LABEL
*
                 LHI R0,L'SKLABEL                     SORT LABEL LENGTH
                 LA R1,SKLABEL
                 AR R1,R0
*
                 BALR R14,0       REMOVE TRAILING BLANKS FROM LENGTH
                 BCTR R1,0
                 CLI 0(R1),X'40'
                 JNE VDP2300X010
                 BCTR R0,R14
*
VDP2300X010      EQU *
                 STH R0,SKLBLLEN
*
                 MVC SKFLDID,VDP2300_XVIEW_SORT_KEY_ID SORT FIELD ID
                 MVC SKFLDLEN,VDP2300_SK_FIELD_LENGTH SORT FIELD LENGTH
*                 
                 LH  R0,SKFLDLEN
                 CHI R0,150         check length of sort key
                 JNH VDP2300X011b
                 CLI VWDESTYP+1,BATCH   HARDCOPY/BATCH report?
                 JNE VDP2300X011a
                 LHI R14,MSG#453  Report Sort key must be <= 150
                 j    RTNERROR    INDICATE ERROR
VDP2300X011a     EQU *                                  
                 CHI R0,256         check length of sort key
                 JNH VDP2300X011b
                 LHI R14,MSG#452  Sort key must be <= 256
                 j    RTNERROR    INDICATE ERROR
*                 
VDP2300X011b     EQU *                 
                 MVC SKFLDFMT,VDP2300_SK_FORMAT_ID+2  SORT FIELD FORMAT
           MVC SKFLDCON,VDP2300_SK_FIELD_CONTENT_ID+2 SORT FIELD FORMAT
                 XR R0,R0                          FIELD SIGNED IND
                 IC R0,VDP2300_SK_SIGNED_IND
                 MVI SKFLDSGN,C'N'
                 CHI R0,SIGNED
                 JNE VDP2300X012
                 MVI SKFLDSGN,C'Y'
*
VDP2300X012      EQU *
                 LH R0,VDP2300_SK_DECIMAL_COUNT       SORT FIELD DEC
                 STC r0,SKFLDDEC
*
                 LH R0,VDP2300_SK_ROUNDING            SORT FIELD RNDING
                 STC r0,SKFLDRND
*
                 MVC SKFLDMSK,SPACES                  SORT FIELD MASK
                 XC SKFLDMLN,SKFLDMLN
*
                 XC SKFILEID,SKFILEID
                 XC SKLRID,SKLRID
                 L R1,VDP2300_RTD_LR_FIELD_ID
                 BRAS R9,LOCFLD
                 LTR R15,R15
                 JNP VDP2300X014
                 USING FLDDEFN,R15
                 MVC SKFILEID,FDFILEID
                 MVC SKLRID,FDLRID
                 DROP R15
*
VDP2300X014      EQU *
                 LH R0,VDP2300_ST_START_POSITION
                 AHI R0,4-1
                 STH R0,SKTTLOFF    TITLE KEY OFFSET WITHIN EXTRACT REC
*
                 MVC SKTTLLEN,VDP2300_ST_FIELD_LENGTH
                 MVC SKTTLFMT,VDP2300_ST_FORMAT_ID+2
                 MVC SKTTLCON,VDP2300_ST_FIELD_CONTENT_ID+2
*
                 LH R0,VDP2300_ST_DECIMAL_COUNT      SORT TITLE DEC
                 STC R0,SKTTLDEC
*
                 LH R0,VDP2300_ST_ROUNDING           SORT TITLE RNDING
                 STC R0,SKTTLRND
*
                 MVC SKOUTPOS,VDP2300_DESC_START_POSITION
                 MVC SKOUTLEN,VDP2300_DESC_FIELD_LENGTH
                 MVC SKOUTFMT,VDP2300_DESC_FORMAT_ID+2
                 MVC SKOUTCON,VDP2300_DESC_FIELD_CONTENT_ID+2
*
                 LH R0,VDP2300_DESC_DECIMAL_COUNT     SORT DESC  DEC
                 stc r0,SKOUTDEC
*
                 LH R0,VDP2300_DESC_ROUNDING          SORT DESC  RNDING
                 stc r0,SKOUTRND
*
                 MVC SKOUTMSK,SPACES                  DESC MASK
*
                 LHI R0,L'SKOUTMSK                    DESC MASK  LENGTH
                 LA R1,SKOUTMSK
                 AR R1,R0
*
                 BALR R14,0
                 BCTR R1,0
                 CLI  0(R1),X'40'
                 JNE  VDP2300X016
                 BCTR R0,R14
VDP2300X016      EQU  *
                 CLI 0(R1),X'40'
*
                 STH R0,SKOUTMLN
*
                 MVI SKSRTSEQ,C'A'                    SORT SEQUENCE
                 CLI VDP2300_SORT_SEQ_ID+3,ASCEND
                 JE  VDP2300X018
                 MVI SKSRTSEQ,C'D'
*
VDP2300X018      EQU *
                 MVC SKHDRBRK,VDP2300_SORT_BREAK_HEADER_OPTION+2
                 MVC SKDSPOPT,VDP2300_SORT_KEY_DISP_OPT_ID+2
                 MVC SKFTRBRK,VDP2300_SORT_BREAK_FOOTER_OPTION+2
*
                 MVI SKSUBCNT,C'N'             SUBTOTAL COUNT
                 CLI VDP2300_SUBTOTAL_COUNT_IND,X'01'
                 JNE VDP2300X020
                 MVI SKSUBCNT,C'Y'
*
VDP2300X020      EQU *
                 XC SKSUBLEN,SKSUBLEN          SUBTOTAL LABEL LENGTH
                 MVC SKSUBLBL,SPACES           SUBTOTAL LABEL
*
                 XC SKINDCNT,SKINDCNT ZERO INDENTATION LEVEL NUMBER
*
                 XC SKPREFXL,SKPREFXL ZERO SORT KEY VALUE OFFSET
*
                 XC SKVALOFF,SKVALOFF ZERO SORT KEY VALUE OFFSET
*
                 LH R0,SKOUTLEN   LOAD OUTPUT LENGTH IF AVAILABLE
                 LTR R0,R0
                 JP  VDP2300X022
                 LH R0,SKTTLLEN
VDP2300X022      EQU *
                 STH R0,SKDESCLN
*
                 AH R0,VWTTLTOT   ACCUMULATE TOTAL TITLE LENGTH
                 STH R0,VWTTLTOT
*
                 XC SKTTLTOT,SKTTLTOT TOTAL SORT KEY LENGTH
                 XC SKCOLDEF,SKCOLDEF COLUMN DEFINITION ADDRESS
                 XC SKLBADDR,SKLBADDR SORT TITLE RECORD BUFFER ADDRESS
                XC SK1300VL,SK1300VL SORT KEY VALUE VDP1300 REC ADDRESS
                 XC SK1300DS,SK1300DS SORT TITLE VDP1300   REC ADDRESS
                XC SK1400VL,SK1400VL SORT KEY VALUE VDP1400 REC ADDRESS
                 XC SK1400DS,SK1400DS SORT TITLE VDP1400   REC ADDRESS
                 ZAP SKCOUNT,P000 SORT     BREAK RECORD COUNT
                 MVC SKTITLE,SPACES SORT   TITLE
*
                 AHI R6,SKENTLEN  ADVANCE  TO    NEXT   SORT   KEY
                 ST R6,SRTDEFCR
*
                 if cli,VWUPDMOD+1,eq,MARGUPD
                   LHI R14,MSG#449  Marginal file not valid anymore
                   j    RTNERROR    INDICATE ERROR
                 endif
               endif
             endsel
           enddo
           drOP R5
           DROP R6
         endif
                     EJECT
VDPEOF   LHI   R14,MSG#425        ASSUME  FILE EMPTY
         CP    VIEWCNT,P000       LOAD REQUEST COUNT
         JNH   RTNERROR           NO  -  INDICATE ERROR - STOP

*        free what's unused

         L     R14,VDPCURR        next avaailable entry
         aghi  R14,7              ROUND up TO NEAREST DOUBLEWORD
         nill  r14,x'fff8'
         llgt  R1,vdp_addr_curr_seg COMPUTE REMAINING TABLE SPACE
         agf   R1,VDP_seg_len
         lgr   R0,R1
         sgr   R0,R14             This amount is unused
         sgr   R1,R0              First byte of unused space
*
         Llgf  R14,VDPSIZE        Adjust actual VDP size
         Sgr   R14,R0             with the amount we're going to free
         ST    R14,VDPSIZE
         ltgr  R0,R0              Do we have an exact fit ?
         jnp   VDPEOF02             don't release
         STORAGE RELEASE,ADDR=(1),LENGTH=(0)
VDPEOF02 DS    0H
*
         BRAS  R9,FLAGCALC        ASSIGN CALCULATED COLUMN NUMBERS
         BRAS  R9,LINKSKEY        SORT SORT KEYS  AND LINK THEM TO COLS
         BRAS  R9,OFFCALC         COMPUTE COLUMN  OUTPUT OFFSETS
         BRAS  R9,OFFRPTTL        COMPUTE REPORT  TITLE  OFFSETS
         BRAS  R9,OFFFOOTR        COMPUTE REPORT  FOOTER OFFSETS
*
         LH    R15,MAXBRK         ADD +1 TO MAXIMUM FOR GRAND TOTALS
         AHI   R15,1
         STH   R15,MAXBRK
*
         L     R2,VDPDCBA
         CLOSE ((R2)),MODE=31,MF=(E,RENTPARM)
*
         BSM   0,R10              RETURN
                         EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*     A S S I G N   C A L C U L A T E D   C O L U M N   N U M B E R S *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
FLAGCALC LH    R2,VWCOLCNT        LOAD COLUMN COUNT
         LTR   R2,R2
         JNP   FLAGEXIT
*
         L     R7,VWCOLADR        LOAD FIRST COLUMN DEFN ADDRESS
         USING COLDEFN,R7
*
***********************************************************************
*  CHECK IF COLUMN PARTICIPATES IN SUMMARIZATION/CALCULATIONS         *
***********************************************************************
FLAGLOOP CLC   CDCLCCOL,HEXFF
         JNE   FLAGNEXT
*
         LH    R15,VWCLCCOL       ASSIGN CALCULATED  COLUMN  NUMBER
         AHI   R15,1
         STH   R15,VWCLCCOL
         STH   R15,CDCLCCOL
*
         LH    R15,VWSETLEN
         STH   R15,CDCLCOFF
         AHI   R15,AccumDFPl
         STH   R15,VWSETLEN
*
         CLI   CDSUBOPT+1,MIN     MINIMUM  VALUE  ???
         JNE   FLAGLOO1
         NI    VWFLAG1,255-VWNOMIN
*
FLAGLOO1 EQU   *
         CLI   CDSUBOPT+1,DETMIN  MINIMUM  VALUE  ???
         JNE   FLAGLOO2
         NI    VWFLAG1,255-VWNOMIN
*
FLAGLOO2 EQU   *
         CLI   CDSUBOPT+1,MAX     MAXIMUM  VALUE  ???
         JNE   FLAGLOO3
         NI    VWFLAG1,255-VWNOMIN
*
FLAGLOO3 EQU   *
         CLI   CDSUBOPT+1,DETMAX  MAXIMUM  VALUE  ???
         JNE   FLAGLOO4
         NI    VWFLAG1,255-VWNOMIN
*
FLAGLOO4 EQU   *
         CLI   CDSUBOPT+1,FIRST   FIRST    VALUE  ???
         JNE   FLAGNEXT
         NI    VWFLAG1,255-VWNOFRST
*
FLAGNEXT AHI   R7,CDENTLEN
         BRCT  R2,FLAGLOOP        CONTINUE SEARCH FOR REFERENCED  COL
*
FLAGEXIT BR    R9
*
         DROP  R7
         DROP  R8
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*            V D P L O A D E R R O R   M E S S A G E S                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
             USING VDP0001_GENERATION_RECORD,R5
VDPERR       MVC ERRDATA(L'VDPMSG),VDPMSG
*
             L R0,VDP0001_VIEWID           INDICATE VIEW   ID
             CVD R0,DBLWORK
             OI DBLWORK+L'DBLWORK-1,X'0F'
             MVC ERRDATA+02(8),VALUMSK1
             ED ERRDATA+02(8),DBLWORK+4
*
             L R0,VDP0001_COLUMN_ID        INDICATE COLUMN ID
             CVD R0,DBLWORK
             OI DBLWORK+L'DBLWORK-1,X'0F'
             MVC ERRDATA+13(8),VALUMSK1
             ED ERRDATA+13(8),DBLWORK+4
*
             LH R0,VDP0001_RECORD_TYPE     INDICATE RECORD TYPE
             CVD R0,DBLWORK
             OI DBLWORK+L'DBLWORK-1,X'0F'
             UNPK ERRDATA+25(4),DBLWORK
*
             LH R0,VDP0001_SEQUENCE_NBR    INDICATE SEQUENCE NO
             CVD R0,DBLWORK
             OI DBLWORK+L'DBLWORK-1,X'0F'
             UNPK ERRDATA+33(4),DBLWORK
*
             LH R0,VDP0001_RECORD_ID       INDICATE RECORD   ID
             CVD R0,DBLWORK
             OI DBLWORK+L'DBLWORK-1,X'0F'
             MVC ERRDATA+41(8),VALUMSK1
             ED ERRDATA+41(8),DBLWORK+4
*
             j   RTNERROR
*
***********************************************************************
*      UNABLE TO LOAD VIEW OUTPUT FORMATTING EXIT PROGRAM             *
***********************************************************************
             USING VIEWREC,R8
*
VDPGMERR     LHI R14,MSG#407      UNABLE TO LOAD FORMAT EXIT
*
             MVC ERRDATA+00(8),VWFMTPGM
             j   RTNERROR
*
             DROP R5
             DROP R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   S O R T   S O R T   K E Y S   A N D   L I N K   T O   C O L M S   *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING VIEWREC,R8
*
LINKSKEY L     R14,VWSKADDR       LOAD SORT KEY TABLE FIRST ENTRY ADDR
*
         L     R15,SRTDEFCR       LOAD "TEMPORARY ENTRY"  ADDRESS (END)
*
         LR    R0,R15             LOAD SORT KEY TABLE LAST  ENTRY ADDR
         LHI   R1,SKENTLEN
         SR    R0,R1
*
***********************************************************************
*  "BUBBLE SORT" SORT KEY DEFN'S INTO SORT SEQUENCE NUMBER ORDER      *
***********************************************************************
LINKSRT1 CR    R14,R0             DONE (CURRENT = LAST) ???
         JNL   LINKCOLM
*
         LA    R1,SKENTLEN(,R14)  COMPARE CURRENT AND  NEXT ENTRY
*
LINKSRT2 CLC   SKSRTORD-SORTKEY(L'SKSRTORD,R14),SKSRTORD-SORTKEY(R1)
         JNH   LINKSRT3
*
         MVC   0(256,R15),0(R14)          SWAP  ENTRIES
         MVC   256(SKENTLEN-256,R15),256(R14)
         MVC   0(256,R14),0(R1)
         MVC   256(SKENTLEN-256,R14),256(R1)
         MVC   0(256,R1),0(R15)
         MVC   256(SKENTLEN-256,R1),256(R15)
*
LINKSRT3 AHI   R1,SKENTLEN
         CR    R1,R15
         JL    LINKSRT2
*
         AHI   R14,SKENTLEN       ADVANCE CURRENT ENTRY
         j     LINKSRT1
*
***********************************************************************
*  FIND MATCHING COLUMN DEFINITION                                    *
***********************************************************************
LINKCOLM L     R14,VWSKADDR
*
         XC    CURRLVL,CURRLVL    ZERO  CURRENT INDENTATION LEVEL
*
LINKCOL0 C     R14,SRTDEFCR       END-OF-SORT-KEYS  ???
         JNL   LINKPROP           YES -  PROPAGATE  PAGE BREAKS
*
         MVC   SKINDCNT-SORTKEY(L'SKINDCNT,R14),CURRLVL
*
         LH    R0,VWCOLCNT        LOAD COLUMN COUNT
         L     R7,VWCOLADR        LOAD FIRST  COLUMN DEFN  ADDRESS
         USING COLDEFN,R7
*
LINKCOL1 CLC   SKCOLID-SORTKEY(L'SKCOLID,R14),CDCOLID MATCHING COL ID
         JE    LINKCOL2
*
         AHI   R7,CDENTLEN
         BRCT  R0,LINKCOL1
*
         L     R7,VWCOLADR
*
         MVC   ERRDATA(L'VIEW),VIEW
         L     R0,VWVIEW#                   INDICATE  VIEW ID
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   ERRDATA+L'VIEW(8),VALUMSK1
         ED    ERRDATA+L'VIEW(8),DBLWORK+4
*
         LHI   R14,MSG#424        UNABLE TO LOCATE  COLUMN DEFN
         j     RTNERROR
*
***********************************************************************
*  MATCHING COLUMN DEFINITION (VDP 2000) FOUND                        *
***********************************************************************
LINKCOL2 ST    R14,CDSRTDEF-COLDEFN(,R7)
         ST    R7,SKCOLDEF-SORTKEY(,R14)
*
         MVC   SKCOLNO-SORTKEY(L'SKCOLNO,R14),CDCOLNO-COLDEFN(R7)
         MVC   SKSUBLBL-SORTKEY(L'SKSUBLBL,R14),CDSUBLBL-COLDEFN(R7)
*
         LH    R0,CDCOLSIZ-COLDEFN(,R7)
         AHI   R0,1
         STH   R0,SKCOLSIZ-SORTKEY(,R14)
*
         MVC   SKFLDMSK-SORTKEY(L'SKFLDMSK,R14),CDDETMSK-COLDEFN(R7)
*
         LHI   R0,L'SKFLDMSK
         LA    R1,SKFLDMSK-SORTKEY(,R14)
         AR    R1,R0
*
         BALR  R15,0
         BCTR  R1,0
         CLI   0(R1),X'40'
         JNE   LIN1COL2
         BCTR  R0,R15
*
LIN1COL2 EQU   *
         STH   R0,SKFLDMLN-SORTKEY(,R14)
*
         LH    R0,CDDATOFF-COLDEFN(,R7)   SORT KEY  VALUE OFFSET
         STH   R0,SKVALOFF-SORTKEY(,R14)
*
         LH    R0,CDSUBLEN-COLDEFN(,R7)   PREFIX/LABEL    LENGTH
         STH   R0,SKSUBLEN-SORTKEY(,R14)
         LTR   R0,R0
         JNP   LINKCOL5
*
         CLI   SKDSPOPT-SORTKEY+1(R14),INCLDESC  INCL DESCRIPTION ???
         JNE   LINKCOL3           NO  - DELIM  NEEDED BETWEEN    DESC
*
         OC    SKLRID-SORTKEY(L'SKLRID,R14),SKLRID-SORTKEY(R14)
         JNZ   LINKCOL4           YES - NEED   " "
         j     LINKCOL5           NO  - BYPASS
*
LINKCOL3 OC    SKLBLLEN-SORTKEY(L'SKLBLLEN,R14),SKLBLLEN-SORTKEY(R14)
         JZ    LINKCOL5
*
LINKCOL4 AHI   R0,1               ADD  ADJUSTMENT FOR " "
LINKCOL5 STH   R0,SKPREFXL-SORTKEY(,R14)
*
***********************************************************************
*  FIND MATCHING REPORT TITLE (IF ANY)                                *
***********************************************************************
         USING VDP1300_TITLE_LINES_RECORD,R15
         L     R15,VWRTADDR
         LTR   R15,R15
         JNP   LINKFTR
*
LINKTTL0 L     R0,VDP1300_FUNCTION
*
         CHI   R0,TTLSTTL1
         JE    LINKTTL1
         CHI   R0,TTLSTTL2
         JE    LINKTTL2
         CHI   R0,TTLSTTL3
         JE    LINKTTL3
         CHI   R0,TTLSTTL4
         JE    LINKTTL4
         CHI   R0,TTLSTTL5
         JE    LINKTTL5
*
         CHI   R0,TTLSVAL1
         JE    LINKTTL1
         CHI   R0,TTLSVAL2
         JE    LINKTTL2
         CHI   R0,TTLSVAL3
         JE    LINKTTL3
         CHI   R0,TTLSVAL4
         JE    LINKTTL4
         CHI   R0,TTLSVAL5
         JE    LINKTTL5
*
         CHI   R0,TTLSLBL1
         JE    LINKTTL1
         CHI   R0,TTLSLBL2
         JE    LINKTTL2
         CHI   R0,TTLSLBL3
         JE    LINKTTL3
         CHI   R0,TTLSLBL4
         JE    LINKTTL4
         CHI   R0,TTLSLBL5
         JE    LINKTTL5
*
         j     LINKTTL9
*
LINKTTL1 CLI   SKINDCNT-SORTKEY+1(R14),1-1
         JNE   LINKTTL9
         ST    R14,VDP1300_SORT_TITLE_ADDRESS
         MVI   SKHDRBRK+1-SORTKEY(R14),TITLEL1
         CHI   R0,TTLSVAL1
         JNE   LINKTTLA
         ST    R15,SK1300VL-SORTKEY(,R14)
LINKTTLA EQU   *
         CHI   R0,TTLSTTL1
         JNE   LINKTTL9
         ST    R15,SK1300DS-SORTKEY(,R14)
         J     LINKTTL9
*
LINKTTL2 CLI   SKINDCNT-SORTKEY+1(R14),2-1
         JNE   LINKTTL9
         ST    R14,VDP1300_SORT_TITLE_ADDRESS
         MVI   SKHDRBRK+1-SORTKEY(R14),TITLEL2
         CHI   R0,TTLSVAL2
         JNE   LINKTTLB
         ST    R15,SK1300VL-SORTKEY(,R14)
LINKTTLB EQU   *
         CHI   R0,TTLSTTL2
         JNE   LINKTTL9
         ST    R15,SK1300DS-SORTKEY(,R14)
         J     LINKTTL9
*
LINKTTL3 CLI   SKINDCNT-SORTKEY+1(R14),3-1
         JNE   LINKTTL9
         ST    R14,VDP1300_SORT_TITLE_ADDRESS
         MVI   SKHDRBRK+1-SORTKEY(R14),TITLEL3
         CHI   R0,TTLSVAL3
         JNE   LINKTTLC
         ST    R15,SK1300VL-SORTKEY(,R14)
LINKTTLC EQU   *
         CHI   R0,TTLSTTL3
         JNE   LINKTTL9
         ST    R15,SK1300DS-SORTKEY(,R14)
         J     LINKTTL9
*
LINKTTL4 CLI   SKINDCNT-SORTKEY+1(R14),4-1
         JNE   LINKTTL9
         ST    R14,VDP1300_SORT_TITLE_ADDRESS
         MVI   SKHDRBRK+1-SORTKEY(R14),TITLEL4
         CHI   R0,TTLSVAL4
         JNE   LINKTTLD
         ST    R15,SK1300VL-SORTKEY(,R14)
LINKTTLD EQU   *
         CHI   R0,TTLSTTL4
         JNE   LINKTTL9
         ST    R15,SK1300DS-SORTKEY(,R14)
         J     LINKTTL9
*
LINKTTL5 CLI   SKINDCNT-SORTKEY+1(R14),5-1
         JNE   LINKTTL9
         ST    R14,VDP1300_SORT_TITLE_ADDRESS
         MVI   SKHDRBRK+1-SORTKEY(R14),TITLEL5
         CHI   R0,TTLSVAL5
         JNE   LINKTTLE
         ST    R15,SK1300VL-SORTKEY(,R14)
LINKTTLE EQU   *
         CHI   R0,TTLSTTL5
         JNE   LINKTTL9
         ST    R15,SK1300DS-SORTKEY(,R14)
         J     LINKTTL9
*
LINKTTL9 AH    R15,VDP1300_REC_LEN     ADVANCE TO NEXT VDP 1300 RECORD
         lg    r15,0(,r15)             load pointer
         LH    R0,VDP1300_RECORD_TYPE  STILL WITHIN 1300'S ???
         CHI   R0,1300
         JNE   LINKFTR                 NO  - EXIT
         CLC   VWVIEW#,VDP1300_VIEWID  SAME  VIEW ???
         JE    LINKTTL0                YES - LOOP
         DROP  R15
*
***********************************************************************
*  FIND MATCHING REPORT FOOTER (IF ANY)                               *
***********************************************************************
         USING VDP1400_FOOTER_LINES_RECORD,R15
LINKFTR  L     R15,VWRFADDR
         LTR   R15,R15
         JNP   LINKCOL8
*
LINKFTR0 L     R0,VDP1400_FUNCTION
*
         CHI   R0,TTLSTTL1
         JE    LINKFTR1
         CHI   R0,TTLSTTL2
         JE    LINKFTR2
         CHI   R0,TTLSTTL3
         JE    LINKFTR3
         CHI   R0,TTLSTTL4
         JE    LINKFTR4
         CHI   R0,TTLSTTL5
         JE    LINKFTR5
*
         CHI   R0,TTLSVAL1
         JE    LINKFTR1
         CHI   R0,TTLSVAL2
         JE    LINKFTR2
         CHI   R0,TTLSVAL3
         JE    LINKFTR3
         CHI   R0,TTLSVAL4
         JE    LINKFTR4
         CHI   R0,TTLSVAL5
         JE    LINKFTR5
*
         CHI   R0,TTLSLBL1
         JE    LINKFTR1
         CHI   R0,TTLSLBL2
         JE    LINKFTR2
         CHI   R0,TTLSLBL3
         JE    LINKFTR3
         CHI   R0,TTLSLBL4
         JE    LINKFTR4
         CHI   R0,TTLSLBL5
         JE    LINKFTR5
*
         J     LINKFTR9
*
LINKFTR1 CLI   SKINDCNT-SORTKEY+1(R14),1-1
         JNE   LINKFTR9
         ST    R14,VDP1400_SORT_TITLE_ADDRESS
         MVI   SKHDRBRK+1-SORTKEY(R14),TITLEL1
         CHI   R0,TTLSVAL1
         JNE   LINKFTRA
         ST    R15,SK1400VL-SORTKEY(,R14)
LINKFTRA EQU   *
         CHI   R0,TTLSTTL1
         JNE   LINKFTR9
         ST    R15,SK1400DS-SORTKEY(,R14)
         J     LINKFTR9
*
LINKFTR2 CLI   SKINDCNT-SORTKEY+1(R14),2-1
         JNE   LINKFTR9
         ST    R14,VDP1400_SORT_TITLE_ADDRESS
         MVI   SKHDRBRK+1-SORTKEY(R14),TITLEL2
         CHI   R0,TTLSVAL2
         JNE   LINKFTRB
         ST    R15,SK1400VL-SORTKEY(,R14)
LINKFTRB EQU   *
         CHI   R0,TTLSTTL2
         JNE   LINKFTR9
         ST    R15,SK1400DS-SORTKEY(,R14)
         J     LINKFTR9
*
LINKFTR3 CLI   SKINDCNT-SORTKEY+1(R14),3-1
         JNE   LINKFTR9
         ST    R14,VDP1400_SORT_TITLE_ADDRESS
         MVI   SKHDRBRK+1-SORTKEY(R14),TITLEL3
         CHI   R0,TTLSVAL3
         JNE   LINKFTRC
         ST    R15,SK1400VL-SORTKEY(,R14)
LINKFTRC EQU   *
         CHI   R0,TTLSTTL3
         JNE   LINKFTR9
         ST    R15,SK1400DS-SORTKEY(,R14)
         J     LINKFTR9
*
LINKFTR4 CLI   SKINDCNT-SORTKEY+1(R14),4-1
         JNE   LINKFTR9
         ST    R14,VDP1400_SORT_TITLE_ADDRESS
         MVI   SKHDRBRK+1-SORTKEY(R14),TITLEL4
         CHI   R0,TTLSVAL4
         JNE   LINKFTRD
         ST    R15,SK1400VL-SORTKEY(,R14)
LINKFTRD EQU   *
         CHI   R0,TTLSTTL4
         JNE   LINKFTR9
         ST    R15,SK1400DS-SORTKEY(,R14)
         J     LINKFTR9
*
LINKFTR5 CLI   SKINDCNT-SORTKEY+1(R14),5-1
         JNE   LINKFTR9
         ST    R14,VDP1400_SORT_TITLE_ADDRESS
         MVI   SKHDRBRK+1-SORTKEY(R14),TITLEL5
         CHI   R0,TTLSVAL5
         JNE   LINKFTRE
         ST    R15,SK1400VL-SORTKEY(,R14)
LINKFTRE EQU   *
         CHI   R0,TTLSTTL5
         JNE   LINKFTR9
         ST    R15,SK1400DS-SORTKEY(,R14)
         J     LINKFTR9
*
LINKFTR9 AH    R15,VDP1400_REC_LEN     ADVANCE TO NEXT VDP 1400 RECORD
         lg    r15,0(,r15)             load pointer
         LH    R0,VDP1400_RECORD_TYPE  STILL WITHIN 1400'S ???
         CHI   R0,1400
         JNE   LINKCOL8                NO  - EXIT
         CLC   VWVIEW#,VDP1400_VIEWID  SAME  VIEW ???
         JE    LINKFTR0                YES - LOOP
         DROP  R15
*
*
LINKCOL8 CLI   SKHDRBRK-SORTKEY+1(R14),SUPPRESS SUPPRESS SORT BREAK ???
         JE    LINKCOL9
*
         LH    R0,VWSRTTOT
         AH    R0,SKFLDLEN-SORTKEY(,R14)
         STH   R0,VWSRTTOT
*
         LH    R0,CURRLVL         UPDATE SORT BREAK COUNT
         AHI   R0,1
         STH   R0,VWBRKCNT
*
         ST    R14,VWLOWSKY       LAST   SORT KEY ADDRESS
*
LINKCOL9 LH    R0,CURRLVL         INCREMENT  INDENTATION LEVEL  NUMBER
         AHI   R0,1
         STH   R0,CURRLVL
*
         AHI   R14,SKENTLEN
         J     LINKCOL0
                     SPACE 3
***********************************************************************
*  PROPAGATE LOWER LEVEL PAGE BREAKS TO HIGHER LEVELS                 *
***********************************************************************
LINKPROP L     R14,VWLOWSKY       SCAN  BACKWARDS THROUGH SORT KEYS
*
         CLI   VWSUMTYP+1,DETAIL  DETAIL REPORT ???
         JE    LINKPRO3           YES -  RETAIN LOWEST   SUBTOTAL LBL
*
         CLI   SKDSPOPT-SORTKEY+1(R14),ASDATA   PRINT   "AS DATA" ???
         JE    LINKPRO1           YES -  FORCE  PRINT OF LOWEST LEVEL
         CLI   SKDSPOPT-SORTKEY+1(R14),INCLDESC PRINT   "AS DATA" ???
         JNE   LINKPRO2           NO  -  BYPASS FORCE
*
LINKPRO1 MVI   SKFTRBRK-SORTKEY+1(R14),SUBTOTAL FORCE PRINTING LOWEST
*
LINKPRO2 CLI   SKFTRBRK-SORTKEY+1(R14),SUBTOTAL SUBTOTAL SPECIFIED ??
         JNE   LINKPRO3           NO  -  BYPASS RESETS
*
         XC    SKSUBLEN-SORTKEY(L'SKSUBLEN,R14),SKSUBLEN-SORTKEY(R14)
         XC    SKPREFXL-SORTKEY(L'SKPREFXL,R14),SKPREFXL-SORTKEY(R14)
*
LINKPRO3 LH    R0,VWBRKCNT        LOAD NO. OF SORT KEY BREAKS
*
         CH    R0,MAXBRK          LARGER THAN PREVIOUS RECORDED MAX ???
         JNH   LINKPROA           NO  - BYPASS UPDATE
         STH   R0,MAXBRK          YES - UPDATE MAXIMUM
*
LINKPROA EQU   *
         ahi   R0,-1              EXCLUDE  ONE   LEVEL
         JNP   LINKEXIT
*
         LHI   R15,SKENTLEN
*
LINKPRO4 CLI   SKHDRBRK-SORTKEY+1(R14),NEWPAGE
         JE    LINKPRO6
         CLI   SKHDRBRK-SORTKEY+1(R14),TITLEL1
         JL    LINKPRO5
         CLI   SKHDRBRK-SORTKEY+1(R14),TITLEL5
         JNH   LINKPRO6
*
LINKPRO5 SR    R14,R15
         BRCT  R0,LINKPRO4
*
         j     LINKEXIT
*
LINKPRO6 SR    R14,R15
         CLI   SKHDRBRK-SORTKEY+1(R14),SAMEPAGE
         JNE   LINKPRO7
         MVI   SKHDRBRK-SORTKEY+1(R14),NEWPAGE
LINKPRO7 BRCT  R0,LINKPRO6
*
***********************************************************************
*  RETURN                                                             *
***********************************************************************
LINKEXIT BR    R9
*
         DROP  R7
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   C O M P U T E   C O L U M N   O U T P U T   O F F S E T S         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING VIEWREC,R8
*
OFFCALC  LH    R0,VWCOLCNT        LOAD COLUMN COUNT
         L     R7,VWCOLADR        LOAD FIRST COLUMN DEFN   ADDRESS
         USING COLDEFN,R7
*
         LTR   R0,R0              ANY  COLUMNS ???
         JNP   OFFCEXIT
*
OFFCALC2 L     R14,CDSRTDEF       SORT KEY     ???
         LTR   R14,R14
         JNP   OFFCALC4
*
         CLI   VWDESTYP+1,BATCH   HARDCOPY/BATCH  ???
         JNE   OFFCALC4           NO  -   CONTINUE
*
         CLI   SKDSPOPT+1-SORTKEY(R14),ASDATA  SORT KEY IS AS DATA ???
         JE    OFFCALC4           YES - RESERVE SPACE  FOR COLUMN
*
         CLI   SKDSPOPT+1-SORTKEY(R14),INCLDESC INCL DESC "AS DATA" ??
         JNE   OFFCALC6           NO  - EXCLUDE COLUMN LEN IN VWOUTLEN
*
         LH    R15,VWOUTLEN       LOAD  CURRENT DATA  POSITION
         STH   R15,CDCOLOFF       SAVE  OUTPUT  DATA  OFFSET
         AH    R15,CDCOLGAP
         AH    R15,SKPREFXL-SORTKEY(,R14)
         AH    R15,SKDESCLN-SORTKEY(,R14)
         AH    R15,CDCOLSIZ
         AHI   R15,1
         STH   R15,VWOUTLEN
*
         CHI   R15,256            Reports limited to width 256
         jnh   OFFCALC6       
*
         LHI   R14,MSG#454        if it is, issue error message 16
         J     RTNERROR           and branch to error routine
*
OFFCALC4 CLI   CDPRTIND,C'Y'      HIDDEN COLUMN ???
         JNE   OFFCALC6
*
         LH    R15,VWOUTLEN       LOAD  CURRENT DATA  POSITION
         STH   R15,CDCOLOFF       SAVE  OUTPUT  DATA  OFFSET
         AH    R15,CDCOLGAP
         AH    R15,CDCOLSIZ
         AHI   R15,1
         STH   R15,VWOUTLEN
*
         CLI   VWDESTYP+1,BATCH   HARDCOPY/BATCH report?
         JNE   OFFCALC6           
*          
         CHI   R15,256            Reports limited to width 256
         jnh   OFFCALC6       
*
         LHI   R14,MSG#454        if it is, issue error message 16
         J     RTNERROR           and branch to error routine
*
OFFCALC6 AHI   R7,CDENTLEN
         BRCT  R0,OFFCALC2
*
***********************************************************************
*  RETURN                                                             *
***********************************************************************
OFFCEXIT BR    R9
*
         DROP  R7
         DROP  R8
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        R E P O R T   T I T L E   F I E L D   O F F S E T S          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING VIEWREC,R8
*
OFFRPTTL L     R14,VWRTADDR       LOAD FIRST REPORT TITLE FIELD ADDRESS
         USING VDP1300_TITLE_LINES_RECORD,R14
         LTR   R14,R14            ANY  CUSTOM TITLE FIELDS  ???
         JNP   OFFREXIT           NO  -  EXIT
*
         SR    R0,R0
         ST    R0,DBLWORK+0       ZERO CURRENT ROW/JUSTIFICATION
         ST    R0,DBLWORK+4       ZERO CURRENT AREA OFFSET
*
***********************************************************************
*  "BUBBLE SORT" REPORT TITLE FIELDS (ROW/COLUMN NUMBER)              *
***********************************************************************
OFFRSRT1 LR    R15,R14            LOAD NEXT  REPORT TITLE FIELD ADDRESS
         AH    R15,0(,R15)
         lg    r15,0(,r15)
*
         DROP  R14
         USING VDP1300_TITLE_LINES_RECORD,R15
*
         LH    R1,VDP1300_RECORD_TYPE  STILL WITHIN 1300'S ???
         CHI   R1,1300
         JNE   OFFRSRT5                NO  - ADJUST LAST   AREA OFFSETS
*
OFFRSRT2 CLC   VDP1300_ROW_NBR,VDP1300_ROW_NBR-VDP1300_REC_LEN(R14)
         JH    OFFRSRT4
         JL    OFFRSRTX
         CLC   VDP1300_COL_NBR,VDP1300_COL_NBR-VDP1300_REC_LEN(R14)
         JNL   OFFRSRT4
*
OFFRSRTX EQU   *
         LA    R1,WORKAREA             LOAD "TEMPORARY ENTRY"   ADDRESS
         AHI   R1,JFCB-WORKAREA
*
         MVC   0(256,R1),0(R14)        SWAP  ENTRIES
         MVC   256(VDP1300LN-256,R1),256(R14)
         MVC   0(256,R14),0(R15)
         MVC   256(VDP1300LN-256,R14),256(R15)
         MVC   0(256,R15),0(R1)
         MVC   256(VDP1300LN-256,R15),256(R1)
*
         L     R1,VDP1300_SORT_TITLE_ADDRESS-VDP1300_REC_LEN(,R14)
         LTR   R1,R1
         JNP   OFFRSRT3
*
         L     R0,SK1300VL-SORTKEY(,R1)
         CR    R0,R15
         JNE   OFFRSRTY
         ST    R14,SK1300VL-SORTKEY(,R1)
*
OFFRSRTY EQU   *
         L     R0,SK1300DS-SORTKEY(,R1)
         CR    R0,R15
         JNE   OFFRSRT3
         ST    R14,SK1300DS-SORTKEY(,R1)
*
OFFRSRT3 L     R1,VDP1300_SORT_TITLE_ADDRESS-VDP1300_REC_LEN(,R15)
         LTR   R1,R1
         JNP   OFFRSRT4
*
         L     R0,SK1300VL-SORTKEY(,R1)
         CR    R0,R14
         JNE   OFFRSRTZ
         ST    R15,SK1300VL-SORTKEY(,R1)
*
OFFRSRTZ EQU   *
         L     R0,SK1300DS-SORTKEY(,R1)
         CR    R0,R14
         JNE   OFFRSRT4
         ST    R15,SK1300DS-SORTKEY(,R1)
*
OFFRSRT4 AH    R15,0(,R15)             ADVANCE INNER LOOP
         lg    r15,0(,r15)
         LH    R1,VDP1300_RECORD_TYPE  STILL WITHIN 1300'S ???
         CHI   R1,1300
         JE    OFFRSRT2                YES - LOOP
*
         DROP  R15
         USING VDP1300_TITLE_LINES_RECORD,R14
*
OFFRSRT5 LH    R0,VDP1300_ROW_NBR        LOAD  ROW    NUMBER
         L     R1,VDP1300_JUSTIFICATION  LOAD  JUSTIFICATION AREA CODE
         CH    R0,DBLWORK+0              SAME  ROW    ???
         JNE   OFFRSRT6                  NO  - RESET
         CH    R1,DBLWORK+2              SAME  JUSTIFICATION AREA ???
         JE    OFFRSRT7                  YES - BYPASS  RESET
*
OFFRSRT6 STH   R0,DBLWORK+0              NO  - UPDATE  ROW#
         STH   R1,DBLWORK+2                  - UPDATE  JUSTIFICATION
         XC    DBLWORK+4(4),DBLWORK+4        - RESET   AREA   OFFSET
*
OFFRSRT7 L     R15,DBLWORK+4             LOAD  PREVIOUS    OFFSET
         ST    R15,VDP1300_TITLE_AREA_OFFSET
*
         L     R0,VDP1300_FUNCTION       LOAD  FUNCTION    CODE
*
         CHI   R0,TTLDATE                PROCESSING DATE
         JNE   OFFRSRTA
         AHI   R15,8
         j     OFFRSRT8
*
OFFRSRTA EQU   *
         CHI   R0,TTLTIME                PROCESSING TIME
         JNE   OFFRSRTB
         AHI   R15,5
         j     OFFRSRT8
*
OFFRSRTB EQU   *
         CHI   R0,TTLPAGE                PAGE NUMBER
         JNE   OFFRSRTC
         AHI   R15,L'VALUMSK1
         j     OFFRSRT8
*
OFFRSRTC EQU   *
         CHI   R0,TTLVIEW                VIEW NUMBER
         JNE   OFFRSRTD
         AHI   R15,L'VALUMSK1
         j     OFFRSRT8
*
OFFRSRTD EQU   *
         CHI   R0,TTLRDATE               RUN  DATE
         JNE   OFFRSRTE
         AHI   R15,8
         j     OFFRSRT8
*
OFFRSRTE EQU   *
         CHI   R0,TTLRUN#                RUN  NUMBER
         JNE   OFFRSRTF
         AHI   R15,L'VALUMSK1
         j     OFFRSRT8
*
OFFRSRTF EQU   *
         CHI   R0,TTLFDATE               FINANCIAL PERIOD DATE
         JNE   OFFRSRTG
         AHI   R15,7
         j     OFFRSRT8
*
OFFRSRTG EQU   *
         CHI   R0,TTLOWNER               VIEW OWNER
         JNE   OFFRSRTH
         AHI   R15,8
         j     OFFRSRT8
*
OFFRSRTH EQU   *
         CHI   R0,TTLTEXT                TEXT
         JNE   OFFRSRTI
         AH    R15,VDP1300_TITLE_LENGTH
         j     OFFRSRT8
*
OFFRSRTI EQU   *
         CHI   R0,TTLCONAM
         JNE   OFFRCKVW
*
         LHI   R0,L'VWCOMPNM
         LA    R1,VWCOMPNM+L'VWCOMPNM      ENDING ADDRESS  (+1)
OFFRCOMP BCTR  R1,0               BACKUP   TO PREVIOUS CHARACTER
         CLI   0(R1),C' '         TRAILING BLANK  ???
         JNE   OFFRSRTJ           NO - EXIT
         BRCT  R0,OFFRCOMP
         LHI   R0,L'VWCOMPNM
*
OFFRSRTJ EQU   *
         AR    R15,R0
         j     OFFRSRT8
*
OFFRCKVW CHI   R0,TTLVWNAM
         JNE   OFFRCKSK
*
         LHI   R0,L'VWTITLE
         LA    R1,VWTITLE+L'VWTITLE      ENDING  ADDRESS  (+1)
OFFRVIEW BCTR  R1,0               BACKUP TO PREVIOUS CHARACTER
         CLI   0(R1),C' '         TRAILING BLANK ???
         JNE   OFFRSRTK           NO - EXIT
         BRCT  R0,OFFRVIEW
         LHI   R0,L'VWTITLE
*
OFFRSRTK EQU   *
         AR    R15,R0
         j     OFFRSRT8
*
OFFRCKSK L     R1,VDP1300_SORT_TITLE_ADDRESS
*
         CHI   R0,TTLSLBL1        SORT KEY  LABEL
         JE    OFFRLABL
         CHI   R0,TTLSLBL2
         JE    OFFRLABL
         CHI   R0,TTLSLBL3
         JE    OFFRLABL
         CHI   R0,TTLSLBL4
         JE    OFFRLABL
         CHI   R0,TTLSLBL5
         JNE   OFFRCKVL
*
OFFRLABL AH    R15,SKLBLLEN-SORTKEY(,R1)
         j     OFFRSRT8
*
OFFRCKVL CHI   R0,TTLSVAL1        SORT KEY  VALUE
         JE    OFFRVAL
         CHI   R0,TTLSVAL2
         JE    OFFRVAL
         CHI   R0,TTLSVAL3
         JE    OFFRVAL
         CHI   R0,TTLSVAL4
         JE    OFFRVAL
         CHI   R0,TTLSVAL5
         JNE   OFFRCKDS
*
OFFRVAL  AH    R15,SKFLDLEN-SORTKEY(,R1)
         j     OFFRSRT8
*
OFFRCKDS CHI   R0,TTLSTTL1        SORT KEY  VALUE DESCRIPTION ???
         JE    OFFRDESC
         CHI   R0,TTLSTTL2
         JE    OFFRDESC
         CHI   R0,TTLSTTL3
         JE    OFFRDESC
         CHI   R0,TTLSTTL4
         JE    OFFRDESC
         CHI   R0,TTLSTTL5
         JNE   OFFRSRT8
*
OFFRDESC CLI   SKDSPOPT+1-SORTKEY(R1),INCLDESC VALUE AS-DATA ???
         JE    OFFRSRT8                        YES - IGNORE
*
         AH    R15,SKDESCLN-SORTKEY(,R1)
*
OFFRSRT8 ST    R15,DBLWORK+4           UPDATE CURENT OFFSET
         STH   R15,VDP1300_AREA_TOTAL_LENGTH
*
         AH    R14,0(,R14)             ADVANCE OUTER LOOP
         lg    r14,0(,r14)
         LH    R1,VDP1300_RECORD_TYPE  REPORT  TITLE FIELD ???
         CHI   R1,1300
         JE    OFFRSRT1                YES - LOOP
*
***********************************************************************
*  ADJUST OFFSETS FOR RIGHT JUSTIFICATION AREAS                       *
***********************************************************************
OFFRIGHT L     R14,VWRTADDR
         ST    R14,DBLWORK+4                         SAVE AREA    BEGIN
*
         LH    R0,VDP1300_ROW_NBR                    INIT PREVIOUS ROW#
         STH   R0,DBLWORK+0
         L     R1,VDP1300_JUSTIFICATION              INIT PREVIOUS JUST
         STH   R1,DBLWORK+2
*
         j     OFFRGHT5
*
OFFRGHT1 LH    R0,VDP1300_ROW_NBR                    LOAD  CURRENT ROW#
         L     R1,VDP1300_JUSTIFICATION              LOAD  CURRENT AREA
         CH    R0,DBLWORK+0                          SAME  ROW ???
         JNE   OFFRGHTA
         CH    R1,DBLWORK+2                          SAME  JUST ??
         JE    OFFRGHT5                              YES - ADVANCE
*
OFFRGHTA EQU   *
         STH   R0,DBLWORK+0
         STH   R1,DBLWORK+2
*
         L     R1,DBLWORK+4                          1ST  AREA FIELD
         ST    R14,DBLWORK+4
*
         L     R0,VDP1300_JUSTIFICATION-VDP1300_REC_LEN(,R15)
         LH    R15,VDP1300_AREA_TOTAL_LENGTH-VDP1300_REC_LEN(,R15)
*
         CHI   R0,TTLRIGHT                           RIGHTMOST AREA ??
         JNE   OFFRGHT3
*
OFFRGHT2 LR    R0,R15
         S     R0,VDP1300_TITLE_AREA_OFFSET-VDP1300_REC_LEN(,R1)
         ST    R0,VDP1300_TITLE_AREA_OFFSET-VDP1300_REC_LEN(,R1)
*
         STH   R15,VDP1300_AREA_TOTAL_LENGTH-VDP1300_REC_LEN(,R1)
*
         AH    R1,0(,R1)
         lg    R1,0(,r1)               load pointer
         CR    R1,R14
         JL    OFFRGHT2
*
         j     OFFRGHT5
*
OFFRGHT3 STH   R15,VDP1300_AREA_TOTAL_LENGTH-VDP1300_REC_LEN(,R1)
*
         AH    R1,0(,R1)
         lg    R1,0(,r1)               load pointer
         CR    R1,R14
         JL    OFFRGHT3
*
*
OFFRGHT5 LR    R15,R14                 SAVE  PREVIOUS/LAST AREA FIELD
*
         AH    R14,VDP1300_REC_LEN     ADVANCE TO NEXT VDP 1300 RECORD
         lg    R14,0(,r14)             load pointer
         LH    R0,VDP1300_RECORD_TYPE  STILL WITHIN 1300'S ???
         CHI   R0,1300
         JE    OFFRGHT1                YES - LOOP
*
OFFRLAST L     R1,DBLWORK+4            FIRST LAST AREA FIELD
*
         LH    R15,VDP1300_AREA_TOTAL_LENGTH-VDP1300_REC_LEN(,R15)
*
         LH    R0,DBLWORK+2            LOAD LAST AREA CODE
         CHI   R0,TTLRIGHT             RIGHTMOST AREA ???
         JNE   OFFRGHT9                NO  -  NO ADJUSTMENT NEEDED
*
OFFRGHT8 LR    R0,R15
         S     R0,VDP1300_TITLE_AREA_OFFSET-VDP1300_REC_LEN(,R1)
         ST    R0,VDP1300_TITLE_AREA_OFFSET-VDP1300_REC_LEN(,R1)
*
         STH   R15,VDP1300_AREA_TOTAL_LENGTH-VDP1300_REC_LEN(,R1)
*
         AH    R1,0(,R1)
         lg    R1,0(,r1)               load pointer
         CR    R1,R14
         JL    OFFRGHT8
*
         j     OFFREXIT
*
OFFRGHT9 STH   R15,VDP1300_AREA_TOTAL_LENGTH-VDP1300_REC_LEN(,R1)
*
         AH    R1,0(,R1)
         lg    R1,0(,r1)               load pointer
         CR    R1,R14
         JL    OFFRGHT9
*
OFFREXIT BR    R9                 RETURN
*
         DROP  R8
         DROP  R14
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        R E P O R T   F O O T E R   F I E L D   O F F S E T S        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING VIEWREC,R8
*
OFFFOOTR L     R14,VWRFADDR       LOAD FIRST  REPORT FOOTER  FIELD ADDR
         USING VDP1400_FOOTER_LINES_RECORD,R14
         LTR   R14,R14            ANY  CUSTOM FOOTER FIELDS  ???
         JNP   OFFFEXIT           NO  -  EXIT
*
         SR    R0,R0
         ST    R0,DBLWORK+0       ZERO CURRENT ROW/JUSTIFICATION
         ST    R0,DBLWORK+4       ZERO CURRENT AREA OFFSET
*
***********************************************************************
*  "BUBBLE SORT" REPORT FOOTER FIELDS (ROW/COLUMN NUMBER)             *
***********************************************************************
OFFFSRT1 LR    R15,R14            LOAD NEXT  REPORT  FOOTER  FIELD ADDR
         AH    R15,0(,R15)
         lg    r15,0(,r15)
*
         DROP  R14
         USING VDP1400_FOOTER_LINES_RECORD,R15
*
         LH    R1,VDP1400_RECORD_TYPE  STILL WITHIN 1400'S ???
         CHI   R1,1400
         JNE   OFFFSRT5                NO  - ADJUST LAST   AREA OFFSETS
*
OFFFSRT2 CLC   VDP1400_ROW_NBR,VDP1400_ROW_NBR-VDP1400_REC_LEN(R14)
         JH    OFFFSRT4
         JL    OFFFSRTX
         CLC   VDP1400_COL_NBR,VDP1400_COL_NBR-VDP1400_REC_LEN(R14)
         JNL   OFFFSRT4
*
OFFFSRTX EQU   *
         LA    R1,WORKAREA             LOAD "TEMPORARY ENTRY"   ADDRESS
         AHI   R1,JFCB-WORKAREA
*
         MVC   0(256,R1),0(R14)        SWAP  ENTRIES
         MVC   256(VDP1400LN-256,R1),256(R14)
         MVC   0(256,R14),0(R15)
         MVC   256(VDP1400LN-256,R14),256(R15)
         MVC   0(256,R15),0(R1)
         MVC   256(VDP1400LN-256,R15),256(R1)
*
         L     R1,VDP1400_SORT_TITLE_ADDRESS-VDP1400_REC_LEN(,R14)
         LTR   R1,R1
         JNP   OFFFSRT3
*
         L     R0,SK1400VL-SORTKEY(,R1)
         CR    R0,R15
         JNE   OFFFSRTY
         ST    R14,SK1400VL-SORTKEY(,R1)
*
OFFFSRTY EQU   *
         L     R0,SK1400DS-SORTKEY(,R1)
         CR    R0,R15
         JNE   OFFFSRT3
         ST    R14,SK1400DS-SORTKEY(,R1)
*
OFFFSRT3 L     R1,VDP1400_SORT_TITLE_ADDRESS-VDP1400_REC_LEN(,R15)
         LTR   R1,R1
         JNP   OFFFSRT4
*
         L     R0,SK1400VL-SORTKEY(,R1)
         CR    R0,R14
         JNE   OFFFSRTW
         ST    R15,SK1400VL-SORTKEY(,R1)
*
OFFFSRTW EQU   *
         L     R0,SK1400DS-SORTKEY(,R1)
         CR    R0,R14
         JNE   OFFFSRT4
         ST    R15,SK1400DS-SORTKEY(,R1)
*
OFFFSRT4 AH    R15,0(,R15)             ADVANCE INNER LOOP
         lg    r15,0(,r15)
         LH    R1,VDP1400_RECORD_TYPE  STILL WITHIN 1400'S ???
         CHI   R1,1400
         JE    OFFFSRT2                YES - LOOP
*
         DROP  R15
         USING VDP1400_FOOTER_LINES_RECORD,R14
*
OFFFSRT5 LH    R0,VDP1400_ROW_NBR        LOAD  ROW    NUMBER
         L     R1,VDP1400_JUSTIFY        LOAD  JUSTIFICATION AREA CODE
         CH    R0,DBLWORK+0              SAME  ROW    ???
         JNE   OFFFSRT6                  NO  - RESET
         CH    R1,DBLWORK+2              SAME  JUSTIFICATION AREA ???
         JE    OFFFSRT7                  YES - BYPASS  RESET
*
OFFFSRT6 STH   R0,DBLWORK+0              NO  - UPDATE  ROW#
         STH   R1,DBLWORK+2                  - UPDATE  JUSTIFICATION
         XC    DBLWORK+4(4),DBLWORK+4        - RESET   AREA   OFFSET
*
OFFFSRT7 L     R15,DBLWORK+4             LOAD  PREVIOUS    OFFSET
         ST    R15,VDP1400_FOOTER_AREA_OFFSET
*
         L     R0,VDP1400_FUNCTION       LOAD  FUNCTION    CODE
*
         CHI   R0,TTLDATE                PROCESSING DATE
         JNE   OFFFSRTV
         AHI   R15,8
         j     OFFFSRT8
*
OFFFSRTV EQU   *
         CHI   R0,TTLTIME                PROCESSING TIME
         JNE   OFFFSRTU
         AHI   R15,5
         j     OFFFSRT8
*
OFFFSRTU EQU   *
         CHI   R0,TTLPAGE                PAGE NUMBER
         JNE   OFFFSRTT
         AHI   R15,L'VALUMSK1
         j     OFFFSRT8
*
OFFFSRTT EQU   *
         CHI   R0,TTLVIEW                VIEW NUMBER
         JNE   OFFFSRTS
         AHI   R15,L'VALUMSK1
         j     OFFFSRT8
*
OFFFSRTS EQU   *
         CHI   R0,TTLRDATE               RUN  DATE
         JNE   OFF1SRTS
         AHI   R15,8
         j     OFFFSRT8
*
OFF1SRTS EQU   *
         CHI   R0,TTLRUN#                RUN  NUMBER
         JNE   OFF2SRTS
         AHI   R15,L'VALUMSK1
         j     OFFFSRT8
*
OFF2SRTS EQU   *
         CHI   R0,TTLFDATE               FINANCIAL PERIOD DATE
         JNE   OFF3SRTS
         AHI   R15,7
         j     OFFFSRT8
*
OFF3SRTS EQU   *
         CHI   R0,TTLOWNER               VIEW OWNER
         JNE   OFF4SRTS
         AHI   R15,8
         j     OFFFSRT8
*
OFF4SRTS EQU   *
         CHI   R0,TTLTEXT                TEXT
         JNE   OFF5SRTS
         AH    R15,VDP1400_FOOTER_LENGTH
         j     OFFFSRT8
*
OFF5SRTS EQU   *
         CHI   R0,TTLCONAM
         JNE   OFFFCKVW
*
         LHI   R0,L'VWCOMPNM
         LA    R1,VWCOMPNM+L'VWCOMPNM      ENDING ADDRESS  (+1)
OFFFCOMP BCTR  R1,0               BACKUP   TO PREVIOUS CHARACTER
         CLI   0(R1),C' '         TRAILING BLANK  ???
         JNE   OFF6SRTS           NO - EXIT
         BRCT  R0,OFFFCOMP
         LHI   R0,L'VWCOMPNM
*
OFF6SRTS EQU   *
         AR    R15,R0
         j     OFFFSRT8
*
OFFFCKVW CHI   R0,TTLVWNAM
         JNE   OFFFCKSK
*
         LHI   R0,L'VWTITLE
         LA    R1,VWTITLE+L'VWTITLE      ENDING  ADDRESS  (+1)
OFFFVIEW BCTR  R1,0               BACKUP TO PREVIOUS CHARACTER
         CLI   0(R1),C' '         TRAILING BLANK ???
         JNE   OFF7SRTS           NO - EXIT
         BRCT  R0,OFFFVIEW
         LHI   R0,L'VWTITLE
*
OFF7SRTS EQU   *
         AR    R15,R0
         j     OFFFSRT8
*
OFFFCKSK L     R1,VDP1400_SORT_TITLE_ADDRESS
*
         CHI   R0,TTLSLBL1        SORT KEY  LABEL
         JE    OFFFLABL
         CHI   R0,TTLSLBL2
         JE    OFFFLABL
         CHI   R0,TTLSLBL3
         JE    OFFFLABL
         CHI   R0,TTLSLBL4
         JE    OFFFLABL
         CHI   R0,TTLSLBL5
         JNE   OFFFCKVL
*
OFFFLABL AH    R15,SKLBLLEN-SORTKEY(,R1)
         j     OFFFSRT8
*
OFFFCKVL CHI   R0,TTLSVAL1        SORT KEY  VALUE
         JE    OFFFVAL
         CHI   R0,TTLSVAL2
         JE    OFFFVAL
         CHI   R0,TTLSVAL3
         JE    OFFFVAL
         CHI   R0,TTLSVAL4
         JE    OFFFVAL
         CHI   R0,TTLSVAL5
         JNE   OFFFCKDS
*
OFFFVAL  AH    R15,SKFLDLEN-SORTKEY(,R1)
         j     OFFFSRT8
*
OFFFCKDS CHI   R0,TTLSTTL1        SORT KEY  VALUE DESCRIPTION ???
         JE    OFFFDESC
         CHI   R0,TTLSTTL2
         JE    OFFFDESC
         CHI   R0,TTLSTTL3
         JE    OFFFDESC
         CHI   R0,TTLSTTL4
         JE    OFFFDESC
         CHI   R0,TTLSTTL5
         JNE   OFFFSRT8
*
OFFFDESC CLI   SKDSPOPT+1-SORTKEY(R1),INCLDESC VALUE AS-DATA ???
         JE    OFFFSRT8                        YES - IGNORE
*
         AH    R15,SKDESCLN-SORTKEY(,R1)
*
OFFFSRT8 ST    R15,DBLWORK+4           UPDATE CURENT OFFSET
         STH   R15,VDP1400_AREA_TOTAL_LENGTH
*
         AH    R14,0(,R14)             ADVANCE OUTER LOOP
         lg    r14,0(,r14)
         LH    R1,VDP1400_RECORD_TYPE  REPORT FOOTER FIELD ???
         CHI   R1,1400
         JE    OFFFSRT1                YES - LOOP
*
***********************************************************************
*  ADJUST OFFSETS FOR RIGHT JUSTIFICATION AREAS                       *
***********************************************************************
OFFFRGHT L     R14,VWRFADDR
         ST    R14,DBLWORK+4                         SAVE AREA    BEGIN
*
         LH    R0,VDP1400_ROW_NBR                    INIT PREVIOUS ROW#
         STH   R0,DBLWORK+0
         L     R1,VDP1400_JUSTIFY                    INIT PREVIOUS JUST
         STH   R1,DBLWORK+2
*
         j     OFFFRHT5
*
OFFFRHT1 LH    R0,VDP1400_ROW_NBR                    LOAD  CURRENT ROW#
         L     R1,VDP1400_JUSTIFY                    LOAD  CURRENT AREA
         CH    R0,DBLWORK+0                          SAME  ROW ???
         JNE   OFFFRHTJ
         CH    R1,DBLWORK+2                          SAME  JUST ??
         JE    OFFFRHT5                              YES - ADVANCE
*
OFFFRHTJ EQU   *
         STH   R0,DBLWORK+0
         STH   R1,DBLWORK+2
*
         L     R1,DBLWORK+4                          1ST  AREA FIELD
         ST    R14,DBLWORK+4
*
         L     R0,VDP1400_JUSTIFY-VDP1400_REC_LEN(,R15)
         LH    R15,VDP1400_AREA_TOTAL_LENGTH-VDP1400_REC_LEN(,R15)
*
         CHI   R0,TTLRIGHT                           RIGHTMOST AREA ??
         JNE   OFFFRHT3
*
OFFFRHT2 LR    R0,R15
         S     R0,VDP1400_FOOTER_AREA_OFFSET-VDP1400_REC_LEN(,R1)
         ST    R0,VDP1400_FOOTER_AREA_OFFSET-VDP1400_REC_LEN(,R1)
*
         STH   R15,VDP1400_AREA_TOTAL_LENGTH-VDP1400_REC_LEN(,R1)
*
         AH    R1,0(,R1)
         lg    R1,0(,r1)               load pointer
         CR    R1,R14
         JL    OFFFRHT2
*
         j     OFFFRHT5
*
OFFFRHT3 STH   R15,VDP1400_AREA_TOTAL_LENGTH-VDP1400_REC_LEN(,R1)
*
         AH    R1,0(,R1)
         lg    R1,0(,r1)               load pointer
         CR    R1,R14
         JL    OFFFRHT3
*
*
OFFFRHT5 LR    R15,R14                 SAVE  PREVIOUS/LAST AREA FIELD
*
         AH    R14,VDP1400_REC_LEN     ADVANCE TO NEXT VDP 1400 RECORD
         lg    R14,0(,r14)             load pointer
         LH    R0,VDP1400_RECORD_TYPE  STILL WITHIN 1400'S ???
         CHI   R0,1400
         JE    OFFFRHT1                YES - LOOP
*
OFFFLAST L     R1,DBLWORK+4            FIRST LAST AREA FIELD
*
         LH    R15,VDP1400_AREA_TOTAL_LENGTH-VDP1400_REC_LEN(,R15)
*
         LH    R0,DBLWORK+2            LOAD LAST AREA CODE
         CHI   R0,TTLRIGHT             RIGHTMOST AREA ???
         JNE   OFFFRHT9                NO  -  NO ADJUSTMENT NEEDED
*
OFFFRHT8 LR    R0,R15
         S     R0,VDP1400_FOOTER_AREA_OFFSET-VDP1400_REC_LEN(,R1)
         ST    R0,VDP1400_FOOTER_AREA_OFFSET-VDP1400_REC_LEN(,R1)
*
         STH   R15,VDP1400_AREA_TOTAL_LENGTH-VDP1400_REC_LEN(,R1)
*
         AH    R1,0(,R1)
         lg    R1,0(,r1)               load pointer
         CR    R1,R14
         JL    OFFFRHT8
*
         j     OFFFEXIT
*
OFFFRHT9 STH   R15,VDP1400_AREA_TOTAL_LENGTH-VDP1400_REC_LEN(,R1)
*
         AH    R1,0(,R1)
         lg    R1,0(,r1)               load pointer
         CR    R1,R14
         JL    OFFFRHT9
*
OFFFEXIT BR    R9                 RETURN
*
         DROP  R8
         DROP  R14
                     EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*     C O N V E R T   M A S K   C O D E                               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING COLDEFN,R7
*
MASKCONV LArl  R15,MASKCODE
mask     dsect
masktype ds    cl5
maskdata ds    cl32
mask_l   equ   *-mask
gvbmr87  csect
code     loctr
         using mask,r15
*
         if CLC,lnnan,ne,0(r1)         MASK  CODE  ???
*
outer      do ,
             do inf

               doexit (CLC,masktype,eq,HEXFF),do=outer                 +
                      get out if end of table

               if CLC,masktype,eq,0(R1) MATCHING   CODE ???
                 AHI R15,mask_l         ADVANCE TO END OF MASK
                 LH R1,CDCOLSIZ
                 LR R0,R1
                 AHI R0,1
                 STH R0,CDMSKLEN
                 SR R15,R0
                 exrl R1,MASKCOPY
                 leave ,
               endif

               AHI R15,mask_l          INCREMENT  TABLE ENTRY ADDRESS
             enddo

           enddo
         endif
         BR    R9                 RETURN
*
         DROP  R7
         DROP  R13
         DROP  R15
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCCTRL"  -  LOCATE CONTROL RECORD (050) IN VDP                    *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - "VDP"  CONTROL  RECORD ADDRESS (NOT FOUND = 0)         *
*        R14 - WORK  REGISTER                                         *
*        R9  - RETURN ADDRESS            (INPUT)                      *
*        R1  - CONTROL RECORD  ID        (INPUT)                      *
*        R0  - WORK  REGISTER                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING WORKAREA,R13
*
LOCCTRL  L     R15,VDPBEGIN       SEARCH  "VDP"
         USING VDP0050_CONTROL_RECORD,R15
*
         L     R14,VDPCOUNT
*
LOCCTRL4 LH    R0,VDP0050_RECORD_TYPE
         CHI   R0,050
         JNE   LOCCTRL8
*
         C     R1,VDP0050_RECORD_ID
         BER   R9
*
LOCCTRL8 AH    R15,VDP0050_REC_LEN
         lg    r15,0(,r15)        load pointer
         BRCT  R14,LOCCTRL4
*
         XR    R15,R15
         BR    R9
*
         DROP  R13
         DROP  R15
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCFILE"  -  LOCATE LOGICAL FILE RECORD (200) IN VDP               *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - "VDP" EXIT PROGRAM RECORD ADDRESS (NOT FOUND = 0)      *
*        R14 - WORK  REGISTER                                         *
*        R9  - RETURN ADDRESS            (INPUT)                      *
*        R1  - EXIT   PROGRAM  ID        (INPUT)                      *
*        R0  - WORK  REGISTER                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING WORKAREA,R13
*
LOCFILE  L     R15,VDPBEGIN       SEARCH  "VDP"
         USING VDP1600_SUM_OUT_FILE_RECORD,R15
*
         L     R14,VDPCOUNT
*
LOCFILE4 LH    R0,VDP1600_RECORD_TYPE
         CHI   R0,1600
         JNE   LOCFILE8
*
         C     R1,VDP1600_VIEWID
         BER   R9
*
LOCFILE8 AH    R15,VDP1600_REC_LEN
         lg    r15,0(,r15)        load pointer
         BRCT  R14,LOCFILE4
*
         XR    R15,R15
         BCR   15,R9
*
         DROP  R13
         DROP  R15
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOC0200"  -  LOCATE LOGICAL FILE RECORD (200) IN VDP               *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - "VDP" EXIT PROGRAM RECORD ADDRESS (NOT FOUND = 0)      *
*        R14 - WORK  REGISTER                                         *
*        R9  - RETURN ADDRESS            (INPUT)                      *
*        R1  --->  DDname                (INPUT)                      *
*        R0  - WORK  REGISTER                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING WORKAREA,R13
*
LOC0200  L     R15,VDPBEGIN       SEARCH  "VDP"
         USING VDP0200_FILE_RECORD,R15
*
         L     R14,VDPCOUNT
outer200 do ,
           do from=(r14,vdpcount)
             lh r0,vdp0200_record_type
             doexit (chi,r0,eq,0200),and,                              +
               (clc,vdp0200_ddname_output,eq,0(r1)),do=outer200
             ah r15,vdp0200_rec_len
             lg r15,0(,r15)       load pointer
           enddo
           xr  r15,r15
         enddo
         br    r9
*
         DROP  R13
         DROP  R15
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCEXIT"  -  LOCATE EXIT PROGRAM RECORD (210) IN VDP               *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - "VDP" EXIT PROGRAM RECORD ADDRESS (NOT FOUND = 0)      *
*        R14 - WORK  REGISTER                                         *
*        R9  - RETURN ADDRESS            (INPUT)                      *
*        R1  - EXIT   PROGRAM  ID        (INPUT)                      *
*        R0  - WORK  REGISTER                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING WORKAREA,R13
*
LOCEXIT  L     R15,VDPBEGIN       SEARCH  "VDP"
         USING VDP0210_EXIT_PGM_RECORD,R15
*
         L     R14,VDPCOUNT
*
LOCEXIT4 LH    R0,VDP0210_RECORD_TYPE
         CHI   R0,0210
         JNE   LOCEXIT8
*
         C     R1,VDP0210_RECORD_ID
         BER   R9
*
LOCEXIT8 AH    R15,VDP0210_REC_LEN
         lg    r15,0(,r15)        load pointer
         BRCT  R14,LOCEXIT4
*
         XR    R15,R15
         BR    R9
*
         DROP  R13
         DROP  R15
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCFLD"   -  LOCATE LOGICAL RECORD FIELD RECORD (400) IN VDP       *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - "VDP" LR FIELD  RECORD ADDRESS (NOT FOUND = 0)         *
*        R14 - WORK  REGISTER                                         *
*        R9  - RETURN ADDRESS            (INPUT)                      *
*        R1  - FIELD  ID                 (INPUT)                      *
*        R0  - WORK  REGISTER                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING WORKAREA,R13
*
LOCFLD   L     R15,FLDDEFTB       SEARCH  "VDP"
         USING FLDDEFN,R15
*
         L     R14,FLDDEFCR
*
LOCFLD4  CR    R15,R14
         JNL   LOCFLD8
*
         C     R1,FDFLDID
         BER   R9
*
         AHI   R15,FDENTLEN
         j     LOCFLD4
*
LOCFLD8  XR    R15,R15
         BR    R9
*
         DROP  R13
         DROP  R15
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "Check_view_list                                                    *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R5  - "VDP" VDP recore                                       *
*        R15 - Return value - zero -> view number not in list         *
*        R9  - RETURN ADDRESS            (INPUT)                      *
*        R0  - WORK  REGISTER                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING WORKAREA,R13
         using vdp_header,r5             map record header
check_view_list ds 0h
check_it do ,
           if lt,r15,VDP_MR88_View_list,p    get pointer to list
             using vdp0002_view_count,r15    use record layout

             la r6,vdp0002_view_number get address of view

             do from=(r15,vdp0002_view_count) loop thru them all
               doexit (clc,vdp_view_no,eq,0(r6)),do=check_it           +
                       exit if view in record is in list - r15 ne 0
               ahi r6,l'vdp0002_view_number   increment address
             enddo
*            if we get here then r15 will be zero
           endif
*          and here as well
         enddo

         br r9
         drop r15,r5
                        EJECT
         PRINT NOGEN
         DCBD  DSORG=PS

         IHADCBE

JFCBAR   DSECT
         IEFJFCBN LIST=YES

         ihasaver
gvbmr87  csect
static   loctr

***********************************************************************
*  TRANSLATE AND TEST TABLE FOR "UNSIGNED NUMERIC" CLASS TEST         *
***********************************************************************
*                   0 1 2 3 4 5 6 7 8 9 A B C D E F
*
         ds    0f
TRTTBLU  DC    XL16'08080808080808080808080808080808'  00-0F
         DC    XL16'08080808080808080808080808080808'  10-1F
         DC    XL16'08080808080808080808080808080808'  20-2F
         DC    XL16'08080808080808080808080808080808'  30-3F
         DC    XL16'08080808080808080808080808080808'  40-4F
         DC    XL16'08080808080808080808080808080808'  50-5F
         DC    XL16'08080808080808080808080808080808'  60-6F
         DC    XL16'08080808080808080808080808080808'  70-7F
         DC    XL16'08080808080808080808080808080808'  80-8F
         DC    XL16'08080808080808080808080808080808'  90-9F
         DC    XL16'08080808080808080808080808080808'  A0-AF
         DC    XL16'08080808080808080808080808080808'  B0-BF
         DC    XL16'08080808080808080808080808080808'  C0-CF
         DC    XL16'08080808080808080808080808080808'  D0-DF
         DC    XL16'08080808080808080808080808080808'  E0-EF
         DC    XL16'00000000000000000000080808080808'  F0-FF

***********************************************************************
*  HEXTAB                                                             *
***********************************************************************
*                   0 1 2 3 4 5 6 7 8 9 A B C D E F
*
         ds    0f
HEXTAB   DC    XL16'00000000000000000000000000000000'  00-0F
         DC    XL16'00000000000000000000000000000000'  10-1F
         DC    XL16'00000000000000000000000000000000'  20-2F
         DC    XL16'00000000000000000000000000000000'  30-3F
         DC    XL16'00000000000000000000000000000000'  40-4F
         DC    XL16'00000000000000000000000000000000'  50-5F
         DC    XL16'00000000000000000000000000000000'  60-6F
         DC    XL16'00000000000000000000000000000000'  70-7F
         DC    XL16'00000000000000000000000000000000'  80-8F
         DC    XL16'00000000000000000000000000000000'  90-9F
         DC    XL16'00000000000000000000000000000000'  A0-AF
         DC    XL16'00000000000000000000000000000000'  B0-BF
         DC    XL16'00000000000000000000000000000000'  C0-CF
         DC    XL16'00000000000000000000000000000000'  D0-DF
         DC    XL16'00000000000000000000000000000000'  E0-EF
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  F0-FF
                        EJECT
         ltorg
         copy GVBRPTH8
         END
