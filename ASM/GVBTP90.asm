         TITLE 'GVBTP90 - BATCH VSAM/QSAM I/O HANDLER'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2005, 2023.
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
*  GVBTP90 - RECEIVES REQUESTS FROM A CALLING PROGRAM TO PERFORM      *
*            STANDARD I/O FUNCTIONS WHERE NEITHER THE "DDNAME" OR     *
*            THE NUMBER OF FILES ARE KNOWN UNTIL RUNTIME.             *
*                                                                     *
*  FUNCTION CODES:                                                    *
*                                                                     *
*            OP - OPEN   FILE                                         *
*            CL - CLOSE  FILE                                         *
*            RD - READ   A RECORD (RECORD KEY = SUPPLIED KEY)         *
*            LO - LOCATE A RECORD (RECORD KEY = MAX(PARTIAL KEY))     *
*            SB - START  BROWSE    FORWARD                            *
*            BR - BROWSE           FORWARD                            *
*            WR - WRITE  A RECORD                                     *
*            UP - UPDATE THE PREVIOUS RECORD READ                     *
*            DL - DELETE THE PREVIOUS RECORD READ                     *
*            IN - INFORMATION ABOUT THE FILE                          *
*            RI - RELEASE HELD RECORD (VSAM)                          *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - SUCCESSFUL                                          *
*            1  - NOT FOUND       (RANDOM READ - RD )                 *
*            2  - END-OF-FILE     (SEQUENTIAL BROWSE)                 *
*            B  - BAD PARAMETER PASSED FROM CALLING PROGRAM           *
*            E  - ERROR CONDITION (I/O PROBLEM)                       *
*            L  - LOGIC PROBLEM   (FILE NOT OPENED BEFORE I/O REQUEST)*
*                                 (ATTEMPT TO READ BEYOND END-OF-FILE)*
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
*        R12 - PARAMETER AREA ADDRESS (PROVIDED BY CALLER)            *
*                                                                     *
*        R11 - PROGRAM   BASE REGISTER                                *
*        R10 - PROGRAM   BASE REGISTER                                *
*                                                                     *
*        R9  - INTERNAL  SUBROUTINE RETURN ADDRESS (1ST LEVEL)        *
*        R8  - INTERNAL  SUBROUTINE RETURN ADDRESS (2ND LEVEL)        *
*                                                                     *
*        R7  - CURRENT   FILE CONTROL BLOCK ADDRESS                   *
*                                                                     *
*        R6  - CURRENT   RECORD             ADDRESS                   *
*                                                                     *
*        R5  - CURRENT   KEY                ADDRESS                   *
*                                                                     *
*        R4  - CURRENT   RECORD  LENGTH                               *
*            - NEW       FCB     LENGTH                               *
*            - PREVIOUS  RSA     ADDRESS                              *
*                                                                     *
*        R3  - VSAM  ACB ADDRESS /  QSAM DCB ADDRESS                  *
*            - PARAMETER LIST    ADDRESS                              *
*                                                                     *
*        R2  - VSAM  RPL ADDRESS                                      *
*                                                                     *
*        R1  - PARAMETER LIST    ADDRESS                              *
*            - TEMPORARY WORK    REGISTER                             *
*                                                                     *
*        R0  - TEMPORARY WORK    REGISTER                             *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  PARAMETER LIST DEFINITION (COBOL CALL PARAMETERS)                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
PARMLIST DSECT
*
PARMADDR DS    A               ADDRESS OF PARAMETER AREA
RECADDR  DS    A               ADDRESS OF RECORD    AREA
KEYADDR  DS    A               ADDRESS OF KEY       AREA
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  PARAMETER AREA DEFINITION                                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
PARMAREA DSECT                 FIELD SPECIFICATION ENTRY DEFINITION
*
PANCHOR  DS    AL04            STORAGE ANCHOR
PADDNAME DS    CL08            FILE    DDNAME
PAFUNC   DS    CL02            FUNCTION CODE
PAFTYPE  DS    CL01            FILE TYPE (V  = VSAM,  S = SEQUENTIAL)
PAFMODE  DS    CL02            FILE MODE (I  = INPUT, O = OUTPUT    )
*                                        (IO = BOTH, EX = EXTEND    )
PARTNC   DS    CL01            RETURN CODE
PAVSAMRC DS    HL02            VSAM   RETURN CODE
PARECLEN DS    HL02            RECORD LENGTH
PARECFMT DS    CL01            RECORD FORMAT (F=FIXED, V=VARIABLE)
PARESDS  DS    CL01            ESDS direct
*
PALEN    EQU   *-PARMAREA      PARAMETER AREA LENGTH
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*  DYNAMIC WORK AREA (COMMON)                                         *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
WORKAREA DSECT
*
SAVEAREA DS  18F               REGISTER SAVE  AREA
*
RADDRESS DS    A               RECORD   ADDRESS
MODE     DS    XL4             24-BIT  OR  31-BIT MODE
*
FCBLAST  DS    AL4             ADDRESS OF  LAST FCB ENTRY ON CHAIN
FCBCHAIN DS    HL2             THESE TWO FIELDS MUST STAY TOGETHER
         DS    AL4             TO MATCH THE FCB PREFIX LAYOUT
*
         DS   0D
VSAMGLST DS    CL128           COMMON VSAM GENERATE LIST AREA
*
*        THE STATIC AND DYNAMIC STORAGE LISTS MUST CORRESPOND
*
LISTSTR2 DS   0D
OPNLST1  DS    CL(OPENLN1)
*
         DS   0D
OPNLST2  DS    CL(OPENLN2)
*
         DS   0D
OPNLST3  DS    CL(OPENLN3)
*
         DS   0D
CLSLST1  DS    CL(CLSELN1)
*
LISTLEN2 EQU   (*-LISTSTR2)    "LISTLEN1" MUST EQUAL "LISTLEN2"
*
*        DS   0H               24-BIT  RETURN   STUB (SWITCHES MODES)
STUB24   DS    XL04            "LOAD"  INSTRUCTION (REAL RETURN ADDR)
         DS    XL02            "BSM"   INSTRUCTION
RETURNA  DS    AL04            31-BIT  RETURN   ADDRESS  (REAL  ADDR)
*
QSAMEODA DS    XL04            LOAD BRANCH ADDRESS
         DS    XL04            RESTORE ORIGINAL MODE(24-BIT, 31-BIT)
         DS    XL02            SWITCH BACK TO ORIGINAL MODE
*
WORKRENT DS    XL08            RE-ENTRANT  MACRO  PARAMETER LIST AREA
*
WORKEXIT DS    A               OPEN  EXIT  LIST
WORKJFCB DS    CL176
WORKSVA  DS    18F
*
WORKLEN  EQU   (*-WORKAREA)    LENGTH OF DYNAMIC STORAGE
                        EJECT
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  FILE CONTROL BLOCK DEFINITION                                      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
FCB      DSECT                 FILE CONTROL BLOCK
*
FCBLEN   DS    HL02            FCB  LENGTH
FCBNEXT  DS    AL04            NEXT "FCB" POINTER (0 = END-OF-LIST)
*
FCDDNAME DS    CL08            FCB  DDNAME
FCFTYPE  DS    CL01            FCB  FILE TYPE
FCFMODE  DS    CL02            FCB  FILE MODE
FCSTATUS DS    CL01            FCB  FILE STATUS (' ' = UNOPENED)
*                                               ('O' = OPENED  )
*                                               ('E' = AT EOF  )
FCKEYOFF DS    FL04            VSAM KEY      OFFSET
FCRECLEN DS    FL04            VSAM RECORD   LENGTH
FCKEYLEN DS    FL04            VSAM KEY      LENGTH
FCLSTFUN DS    CL02            LAST FUNCTION EXECUTED
FCLSTRC  DS    CL01            LAST RETURN       CODE
FCLSTR15 DS    XL01            LAST REGISTER  15 VALUE
FCLSTVSM DS    HL02            LAST VSAM  RETURN CODE
*
FCDCBACB DS   0CL01            ACCESS METHOD CONTROL BLOCK (DCB/ACB)
*
FCPREFLN EQU   *-FCB           FILE CONTROL BLOCK PREFIX LENGTH
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  REGISTER EQUATES:                                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         YREGS
*
RSABP    EQU   04,4
RSAFP    EQU   08,4
RSA14    EQU   12,4
RSA15    EQU   16,4
RSA0     EQU   20,4
RSA1     EQU   24,4
*
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  1.  SAVE CALLING PROGRAM'S REGISTER CONTENTS                       *
*  2.  CHAIN NEW REGISTER SAVE AREA (RSA) TO PREVIOUS RSA             *
*  3.  LOAD  PARAMETER ADDRESSES                                      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         PRINT NOGEN
*
GVBTP90  RMODE ANY
GVBTP90  AMODE 31
GVBTP90  CSECT
         J     CODE
TP90EYE  GVBEYE GVBTP90
*
CODE     DS    0H
         STM   R14,R12,RSA14(R13) SAVE CALLER'S  REGISTERS
*
         LR    R10,R15            SET  PROGRAM   BASE REGISTER
         USING GVBTP90,R10,R11
         LA    R11,4095(,R10)
         LA    R11,1(,R11)
*
         LR    R4,R13             SAVE CALLER'S  RSA  ADDRESS
*
         LR    R3,R1              LOAD PARM LIST ADDRESS
         USING PARMLIST,R3
*
         L     R12,PARMADDR       LOAD PARM AREA ADDRESS
         USING PARMAREA,R12
*
         L     R13,PANCHOR
         LTR   R13,R13            STORAGE ANCHOR NULL ???
         BRNZ  INIT_010           NO  - USE AREA ALREADY ALLOCATED
*
         CLC   OP,PAFUNC          1ST   OPEN ???
         BRE   INIT_005           YES - ALLOCATE WORK AREA
*
         LR    R13,R4             RESTORE CALLER RSA  ADDRESS
*
         MVI   PARTNC,C'L'        INDICATE LOGIC ERROR
         SR    R15,R15            ASSUME  NOT    CRITICAL
         CLC   CL,PAFUNC-PARMAREA(R2)   CLOSE    ???
         BRE   RETURN15           YES - NOTHING  OPEN TO CLOSE
*
         LA    R15,4              NO  - CANNOT   PROCESS UNOPENED FILE
         B     RETURN15
*
INIT_005 EQU   *
         LHI   R0,WORKLEN         OBTAIN  COMMON WORK AREA
         GETMAIN R,LV=(0)
         LR    R13,R1             LOAD WORK AREA BASE ADDRESS
         ST    R13,PANCHOR        SAVE THE  WORK AREA ANCHOR
         USING WORKAREA,R13
*                                 INIT DYNAMIC STORAGE
         LR    R0,R13
         LHI   R1,WORKLEN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         LA    R1,FCBCHAIN        FCB  CHAIN   FIRST  PLACEHOLDER
         ST    R1,FCBLAST
         LA    R1,6               FCB  LENGTH
         STH   R1,FCBCHAIN
*
         LA    R15,LISTLEN1       VERIFY REMOTE LIST  LENGTHS
         LA    R1,LISTLEN2
         CR    R1,R15             ARE THEY EQUAL ???
         BRE   INIT_006           YES - CONTINUE
*
         DC    H'0'               NO  - ISSUE ABEND
*
INIT_006 EQU   *
         LA    R14,LISTSTR1       CREATE THE REMOTE LISTS
         LA    R0,LISTSTR2
         MVCL  R0,R14
*
         MVC   STUB24(6),STUBCODE     INITIALIZE 24-BIT RETURN STUB
         MVC   QSAMEODA(10),EODCODE   INITIALIZE END-OF-DATA   STUB
*
*        OPEN  (SNAPDCB,(OUTPUT)),MODE=31
*
INIT_010 EQU   *
         ST    R13,RSAFP(,R4)     SET  FORWARD   POINTER
         ST    R4,RSABP(,R13)     SET  BACKWARDS POINTER
*
*        LA    R15,WORKLEN-1(,R13)
*        SNAP  DCB=SNAPDCB,PDATA=REGS,ID=100,STORAGE=((R13),(R15))
*
         L     R6,RECADDR         LOAD  RECORD   AREA ADDRESS
         LA    R6,0(,R6)          STRIP OFF HIGH BIT (IF ON)
*
         L     R5,KEYADDR         LOAD  KEY      AREA ADDRESS
         LA    R5,0(,R5)          STRIP OFF HIGH BIT (IF ON)
*
         LR    R14,R10            SAVE  THE MODE (24-BIT, 31-BIT)
         SRL   R14,31
         SLL   R14,31
         ST    R14,MODE
*
         MVI   PARTNC,C'0'        ASSUME SUCCESSFUL PROCESSING
         XC    PAVSAMRC,PAVSAMRC  ZERO   THE VSAM   RETURN CODE
*
         DROP  R3
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* 1.  SEARCH THE FCB  CHAIN FOR A MATCHING DDNAME                     *
* 2.  BRANCH TO APPROPRIATE LOGIC BASED ON FILE TYPE (VSAM OR QSAM)   *
* 3.  SELECT THE CODE TO BE EXECUTED BASED ON FUNCTION CODE           *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         BRAS  R8,LOCFCB          LOCATE THE MATCHING "FCB" ENTRY
         USING FCB,R7
*
         LTR   R7,R7              ENTRY FOUND  ???
         BRP   FCINIT             YES - INITIALIZE "FCB" FIELDS
*
CHKOPEN  CLC   PAFUNC,OP          OPEN  FUNCTION  ???
         BRNE  RTNERRL            NO  - FILE MUST BE OPENED FIRST
         B     CHKFTYPE           YES - CHECK THE FILE TYPE
*
FCINIT   MVI   FCLSTRC,C' '       INITIALIZE LAST RETURN       CODE
         XC    FCLSTR15,FCLSTR15  INITIALIZE LAST REGISTER  15 VALUE
         XC    FCLSTVSM,FCLSTVSM  INITIALIZE LAST VSAM  RETURN CODE
*
         CLI   FCSTATUS,C'O'      ALREADY OPEN ???
         BRE   CHKFTYPE           YES -  CHECK FILE TYPE (CONTINUE)
*
         CLI   FCSTATUS,C'E'      END-OF-FILE  ???
         BRNE  CHKCLOSE           NO  - ASSUME CLOSED (OPEN NEEDED)
*
         CLI   PAFTYPE,C'V'       VSAM FILE TYPE?
         BRNE  FCHK_100           N - CHECK FOR CLOSE
         MVI   FCSTATUS,C'O'      RESET STATUS TO 'OPEN'
         LA    R2,FCDCBACB+ACBLEN --> REQUEST PARAMETER LIST
         CLC   PAFUNC,RD          READ REQUEST?
         BRE   VSREAD             Y - PERFORM THE REQUEST
         CLC   PAFUNC,SB          START BROWSE REQUEST?
         BRE   VSSTART            Y - PERFORM THE REQUEST
         CLC   PAFUNC,LO          LOCATE REQUEST?
         BRE   VSLOCATE           Y - PERFORM THE REQUEST
         CLC   PAFUNC,WR          WRITE REQUEST?
         BRE   VSWRITE            Y - PERFORM THE REQUEST
FCHK_100 EQU   *
         CLC   PAFUNC,CL          CLOSE FUNCTION  ???
         BRNE  RTNERRL            NO  - CLOSE MUST FOLLOW END-OF-FILE
         B     CHKFTYPE           YES - CONTINUE
*
CHKCLOSE CLC   PAFUNC,OP          OPEN  FUNCTION (FILE CLOSED) ???
         BRNE  RTNERRL            NO  - "OPEN" NEEDED TO SET STATUS
                        EJECT
CHKFTYPE CLI   PAFTYPE,C'V'       VSAM  FILE ???
         BRE   VSAM               YES - BRANCH
         CLI   PAFTYPE,C'S'       QSAM  FILE ???
         BRE   QSAM               YES - BRANCH
         B     RTNERRB            NO  - ERROR (UNSUPPORTED FILE TYPE)
*
VSAM     LA    R2,FCDCBACB+ACBLEN       LOAD ADDRESS OF "RPL"
*
         CLC   PAFUNC,RD          VSAM  READ   ???
         BRE   VSREAD             YES - BRANCH
         CLC   PAFUNC,LO          VSAM  LOCATE ???
         BRE   VSLOCATE           YES - BRANCH
         CLC   PAFUNC,BR          VSAM  BROWSE ???
         BRE   VSBROWSE           YES - BRANCH
         CLC   PAFUNC,WR          VSAM  WRITE  ???
         BRE   VSWRITE            YES - BRANCH
         CLC   PAFUNC,UP          VSAM  UPDATE ???
         BRE   VSUPDATE           YES - BRANCH
         CLC   PAFUNC,DL          VSAM  DELETE ???
         BRE   VSDELETE           YES - BRANCH
         CLC   PAFUNC,SB          VSAM  START  BROWSE ???
         BRE   VSSTART            YES - BRANCH
         CLC   PAFUNC,OP          VSAM  OPEN   ???
         BRE   VSOPEN             YES - BRANCH
         CLC   PAFUNC,CL          VSAM  CLOSE  ???
         BRE   VSCLOSE            YES - BRANCH
         CLC   PAFUNC,IN          VSAM  INFO   ???
         BRE   VSINFO             YES - BRANCH
         CLC   PAFUNC,RI          VSAM- RELEASE???
         BRE   VSRLSE             YES - BRANCH
         B     RTNERRB            NO  - ERROR (UNSUPPORTED FUNCTION)
*
QSAM     CLC   PAFUNC,WR          QSAM  WRITE  ???
         BRE   QSWRITE            YES - BRANCH
         CLC   PAFUNC,RD          QSAM  READ   ???
         BRE   QSREAD             YES - BRANCH
         CLC   PAFUNC,OP          QSAM  OPEN   ???
         BRE   QSOPEN             YES - BRANCH
         CLC   PAFUNC,CL          QSAM  CLOSE  ???
         BRE   QSCLOSE            YES - BRANCH
         CLC   PAFUNC,IN          QSAM  INFO   ???
         BRE   QSINFO             YES - BRANCH
         B     RTNERRB            NO  - ERROR (UNSUPPORTED FUNCTION)
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   O P E N   F I L E                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING IFGRPL,R2
*
VSOPEN   LTR   R7,R7              MATCHING "DDNAME" FOUND ???
         BRZ   VSOP05             NO  - ALLOCATE MEMORY
*
         CLI   FCFTYPE,C'V'       CORRECT FILE TYPE ???
         BRNE  RTNERRB            NO  - RETURN (BAD PARAMETER)
*
         B     VSOP10             YES - BYPASS ALLOCATION
*
VSOP05   LA    R4,FCPREFLN+ACBLEN+RPLLNG LOAD CORRECT LENGTH FOR   VSAM
         BRAS  R8,ALLOCATE                 ALLOCATE  "FCB"
         MVC   FCDCBACB(ACBLEN),MODELACB   INITIALIZE VSAM "ACB" & RPL
         MVC   FCDCBACB+ACBLEN(RPLLNG),MODELRPL
*
VSOP10   CLI   FCSTATUS,C' '      ALREADY OPEN ???
         BRNE  RTNERRL            YES - RETURN (LOGIC ERROR)
*
         LA    R2,FCDCBACB+ACBLEN       LOAD ADDRESS OF "RPL"
*        LA    R15,0(R7,R4)
*        SNAP  DCB=SNAPDCB,PDATA=REGS,ID=150,STORAGE=((R7),(R15))
*
         MVC   FCFMODE,PAFMODE    RESET  THE   FILE MODE IF CHANGED
         CLC   FCFMODE,I          INPUT  ONLY  FILE ???
         BRE   VSOP15             YES -  USE  INPUT MODE
         CLC   FCFMODE,O          OUTPUT ONLY  FILE ???
         BRE   VSOP14             YES -  USE OUTPUT MODE
         CLC   FCFMODE,IO         DUAL   MODE  FILE ???
         BRE   VSOP14             YES -  USE OUTPUT MODE
         CLC   FCFMODE,EX         ADD record empty file ???
         BRNE  RTNERRB            NO  -  BAD PARAMETER
*
VSOP14   MODCB ACB=(R3),DDNAME=(*,FCDDNAME),MACRF=(OUT,DSN),           +
               MF=(G,VSAMGLST)
         B     VSOP20
*
VSOP15   MODCB ACB=(R3),DDNAME=(*,FCDDNAME),MACRF=(IN,DSN),            +
               MF=(G,VSAMGLST)
*
VSOP20   LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSOP25             NO  - INDICATE I/O ERROR
*
*        LA    R15,0(R7,R4)
*        SNAP  DCB=SNAPDCB,PDATA=REGS,ID=152,STORAGE=((R7),(R15))
*
*                                 OPEN VSAM DATASET
         OPEN  ((R3),),MODE=31,MF=(E,OPNLST1)
         C     R15,F4             SUCCESSFUL OR WARNING ???
         BH    VSOP25             NO  - INDICATE I/O ERROR
*        SNAP  DCB=SNAPDCB,PDATA=REGS,ID=154
                        EJECT
         MODCB RPL=(R2),ACB=(R3), SAVE ACB ADDRESS IN "RPL"            +
               MF=(G,VSAMGLST)
         LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSOP25             NO  - INDICATE  I/O ERROR
*
         LA    R8,FCKEYOFF        LOAD ADDRESS OF KEY OFFSET FIELD
*
         SHOWCB ACB=(R3),AREA=(R8),                                    +
               FIELDS=(RKP,LRECL,KEYLEN),LENGTH=12,                    +
               MF=(G,VSAMGLST)
         LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSOP25             NO  - INDICATE I/O ERROR
*
         L     R4,FCRECLEN        LOAD MAXIMUM RECORD LENGTH
         STH   R4,PARECLEN        PASS LENGTH  TO    CALLING PROGRAM
*
         MVI   FCSTATUS,C'O'      INDICATE FILE OPEN
         B     RETURN             RETURN TO CALLER
*
VSOP25   STC   R0,PAVSAMRC+1            PASS  BACK THE VSAM ERROR CODE
         B     RTNERRE                  INDICATE I/O ERROR
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   R E A D   (E X A C T   K E Y   M A T C H)          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VSREAD   CLI   FCFMODE,C'I'       CORRECT FILE  MODE   ???
         BRNE  RTNERRL            NO  - RETURN (LOGIC  ERROR)
*
         LH    R4,PARECLEN        LOAD  RECORD  AREA   LENGTH
*
         CLC   FCFMODE,IO         FILE  OPENED  FOR    UPDATE ("IO") ??
         BRE   VSRDK05            YES - USE THE UPDATE OPTION
*
         CLI   PARESDS,C'D'       ESDS direct access ?
         BE    VSRDK02
*
         MODCB RPL=(R2),                                               +
               AREA=(R6),ARG=(R5),AREALEN=(R4),                        +
               OPTCD=(KEY,DIR,ARD,FWD,SYN,NUP,KEQ,FKS,MVE),            +
               MF=(G,VSAMGLST)
         B     VSRDK10
*
VSRDK02  EQU   *
         MODCB RPL=(R2),                                               +
               AREA=(R6),ARG=(R5),AREALEN=(R4),                        +
               OPTCD=(ADR,DIR,SYN,NUP,MVE),                            +
               MF=(G,VSAMGLST)
         B     VSRDK10
*
VSRDK05  EQU   *
         CLI   PARESDS,C'D'       ESDS direct access ?
         BE    VSRDK07
*
         MODCB RPL=(R2),                                               +
               AREA=(R6),ARG=(R5),AREALEN=(R4),                        +
               OPTCD=(KEY,DIR,ARD,FWD,SYN,UPD,KEQ,FKS,MVE),            +
               MF=(G,VSAMGLST)
         B     VSRDK10
*
VSRDK07  EQU   *
         MODCB RPL=(R2),                                               +
               AREA=(R6),ARG=(R5),AREALEN=(R4),                        +
               OPTCD=(ADR,DIR,SYN,UPD,MVE),                            +
               MF=(G,VSAMGLST)
*
VSRDK10  LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSRDK20            NO  - INDICATE I/O ERROR
*
VSRDK15  GET   RPL=(R2)           READ WITH EXACT KEY
         LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSRDK20            NO  - INDICATE I/O ERROR
*
         LA    R8,FCRECLEN              LOAD ADDRESS OF LENGTH
*
         SHOWCB RPL=(R2),AREA=(R8),                                    +
               FIELDS=(RECLEN),LENGTH=4,                               +
               MF=(G,VSAMGLST)
         LTR   R15,R15                  SUCCESSFUL ???
         BRNZ  VSRDK20                  NO  - INDICATE I/O    ERROR
         L     R4,FCRECLEN              LOAD  RETURNED RECORD LENGTH
         STH   R4,PARECLEN              PASS  BACK  TO CALLER
         B     RETURN                   RETURN  TO CALLER
*
VSRDK20  MVC   PAVSAMRC+1(1),RPLFDB3    RETURN VSAM FEEDBACK CODE
         B     RTNERRE
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   L O C A T E   (L E S S   T H A N   K E Y)          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VSLOCATE CLI   FCFMODE,C'I'       CORRECT FILE  MODE ???
         BRNE  RTNERRL            NO  - RETURN (LOGIC ERROR)
*
         LH    R4,PARECLEN        LOAD  RECORD  AREA  LENGTH
*
         CLC   FCFMODE,IO         FILE  OPENED  FOR    UPDATE ("IO") ??
         BRE   VSLOC01            YES - USE THE UPDATE OPTION
*
         LA    R15,RADDRESS       --> AREA ADDRESS
         MODCB RPL=(R2),                                               X
               ARG=(R5),AREA=(R15),AREALEN=4,                          X
               OPTCD=(KEY,DIR,ARD,FWD,SYN,NUP,KGE,FKS,LOC),            X
               MF=(G,VSAMGLST)
         B     VSLOC03
*
VSLOC01  DS    0H
         LA    R15,RADDRESS       --> AREA ADDRESS
         MODCB RPL=(R2),                                               X
               ARG=(R5),AREA=(R15),AREALEN=4,                          X
               OPTCD=(KEY,DIR,ARD,FWD,SYN,UPD,KGE,FKS,LOC),            X
               MF=(G,VSAMGLST)
VSLOC03  LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSLOC30            NO  - INDICATE I/O ERROR
*
         GET   RPL=(R2)           READ FORWARD (KEY GREATER THAN OR EQ)
         LTR   R15,R15            SUCCESSFUL ???
         BRZ   VSLOC15            YES - CONTINUE
*
         C     R15,F8             RECORD NOT FOUND ???
         BRNE  VSLOC30            NO  -  INDICATE  I/O ERROR
         CLI   RPLFDB3,X'10'
         BRNE  VSLOC30
*
         CLC   FCFMODE,IO         FILE OPENED FOR UPDATE ("IO") ???
         BRE   VSLOC05            YES - USE THE UPDATE OPTION
*
         MODCB RPL=(R2),          INDICATE LAST RECORD POSITIONING     +
               OPTCD=(KEY,SEQ,LRD,BWD,NUP),                            +
               MF=(G,VSAMGLST)
         B     VSLOC10            CONTINUE - POINT TO LAST RECORD
*
VSLOC05  MODCB RPL=(R2),          INDICATE LAST RECORD POSITIONING     +
               OPTCD=(KEY,SEQ,LRD,BWD,UPD),                            +
               MF=(G,VSAMGLST)
VSLOC10  LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSLOC30            NO  - INDICATE I/O ERROR
*
         POINT RPL=(R2)           POINT TO LAST RECORD
         B     VSLOC25            CONTINUE - READ THE LAST RECORD
                        EJECT
VSLOC15  LR    R14,R5             SAVE   ORIGINAL  KEY ADDRESS
         L     R5,RADDRESS        LOCATE KEY IN RECORD READ
         A     R5,FCKEYOFF
*
         L     R15,FCKEYLEN       LOAD   KEY   LENGTH
         BCTR  R15,0              DECREMENT    FOR "EX"
         EX    R15,VSLOCMAT       EXACT MATCH  ???
         BRE   VSLOC28            YES - BYPASS READ BACKWARDS
*
         MODCB RPL=(R2),          INDICATE BACKWARD BROWSE EXACT KEY   +
               ARG=(R5),                                               +
               OPTCD=(BWD,KEQ),                                        +
               MF=(G,VSAMGLST)
         LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSLOC30            NO  - INDICATE I/O ERROR
*
         GET   RPL=(R2)           REREAD SAME RECORD & SWITCH DIRECTION
         LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSLOC30            NO  - INDICATE I/O ERROR
*
         CLC   FCFMODE,IO         FILE OPENED FOR UPDATE ("IO") ???
         BRE   VSLOC20            YES - USE THE UPDATE OPTION
*
         MODCB RPL=(R2),                                               +
               OPTCD=(SEQ,NUP),                                        +
               MF=(G,VSAMGLST)
         B     VSLOC25
*
VSLOC20  MODCB RPL=(R2),                                               +
               OPTCD=(SEQ,UPD),                                        +
               MF=(G,VSAMGLST)
*
VSLOC25  LTR   R15,R15                  SUCCESSFUL ???
         BRNZ  VSLOC30                  NO  - INDICATE I/O ERROR
*
         GET   RPL=(R2)                 READ BACKWARDS
         LTR   R15,R15                  SUCCESSFUL ???
         BRNZ  VSLOC30                  NO  - INDICATE I/O ERROR
*
VSLOC28  LA    R8,FCRECLEN              LOAD ADDRESS OF LENGTH
*
         SHOWCB RPL=(R2),FIELDS=(RECLEN),                              X
               AREA=(R8),LENGTH=4,                                     X
               MF=(G,VSAMGLST)
         LTR   R15,R15                  SUCCESSFUL ???
         BRNZ  VSLOC30                  NO  - INDICATE I/O    ERROR
         L     R1,FCRECLEN              RETURNED RECORD LENGTH
         STH   R1,PARECLEN              PASS BACK TO CALLER
         L     R14,RADDRESS             LOAD  "FROM" ADDRESS
         LR    R15,R6                   LOAD  "TO"   ADDRESS
         LA    R0,255(,R1)              CALCULATE GROUPS/BYTES
         LR    R1,R0                    RESIDUAL BYTES TO MOVE
         SRL   R0,8                     NUMBER OF GROUPS + ONE
         B     VSMVC#20
VSMVC#10 MVC   0(256,R15),0(R14)        MOVE THE 256 BYTES
         LA    R15,256(,R15)            --> NEXT TARGET AREA
         LA    R14,256(,R14)            --> NEXT SOURCE AREA
VSMVC#20 BRCT  R0,VSMVC#10
         EX    R1,VS_MVC                MOVE RESIDUAL BYTES
*
         B     RETURN                   RETURN  TO CALLER
*
VS_MVC   MVC   0(0,R15),0(R14)          * E X E C U T E D *
*
VSLOC30  MVC   PAVSAMRC+1(1),RPLFDB3    RETURN VSAM FEEDBACK CODE
         B     RTNERRE
*
VSLOCMAT CLC   0(0,R5),0(R14)           * * *   E X E C U T E D   * * *
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   S T A R T   B R O W S E                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VSSTART  CLI   FCFMODE,C'I'       CORRECT FILE  MODE   ???
         BRNE  RTNERRL            NO  - RETURN (LOGIC  ERROR)
*
         LH    R4,PARECLEN        LOAD  RECORD  AREA   LENGTH
*
         CLC   FCFMODE,IO         FILE  OPENED  FOR    UPDATE ("IO") ??
         BRE   VSSTR05            YES - USE THE UPDATE OPTION
*
         MODCB RPL=(R2),                                               +
               ARG=(R5),AREA=(R6),AREALEN=(R4),                        +
               OPTCD=(KEY,SEQ,ARD,FWD,SYN,NUP,KGE,FKS,MVE),            +
               MF=(G,VSAMGLST)
         B     VSSTR10
*
VSSTR05  MODCB RPL=(R2),                                               +
               ARG=(R5),AREA=(R6),AREALEN=(R4),                        +
               OPTCD=(KEY,SEQ,ARD,FWD,SYN,UPD,KGE,FKS,MVE),            +
               MF=(G,VSAMGLST)
*
VSSTR10  LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSSTR20            NO  - INDICATE I/O ERROR
*
VSSTR15  POINT RPL=(R2)           POINT TO STARTING POSITION
         LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSSTR20            NO  - INDICATE I/O ERROR
*
         B     RETURN                   RETURN  TO CALLER
*
VSSTR20  MVC   PAVSAMRC+1(1),RPLFDB3    RETURN VSAM FEEDBACK CODE
         B     RTNERRE
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   S E Q U E N T I A L   B R O W S E                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VSBROWSE CLI   FCFMODE,C'I'       CORRECT FILE  MODE   ???
         BRNE  RTNERRL            NO  - RETURN (LOGIC  ERROR)
*
         LH    R4,PARECLEN        LOAD  RECORD  AREA   LENGTH
*
         CLC   FCFMODE,IO         FILE  OPENED  FOR    UPDATE ("IO") ??
         BRE   VSBRW05            YES - USE THE UPDATE OPTION
*
         MODCB RPL=(R2),                                               +
               AREA=(R6),AREALEN=(R4),                                 +
               OPTCD=(NUP,MVE),                                        +
               MF=(G,VSAMGLST)
         B     VSBRW10
*
VSBRW05  MODCB RPL=(R2),                                               +
               AREA=(R6),AREALEN=(R4),                                 +
               OPTCD=(UPD,MVE),                                        +
               MF=(G,VSAMGLST)
*
VSBRW10  LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSBRW20            NO  - INDICATE I/O ERROR
*
VSBRW15  GET   RPL=(R2)           READ  NEXT
         LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSBRW20            NO  - INDICATE I/O ERROR
*
         LA    R8,FCRECLEN              LOAD ADDRESS OF LENGTH
*
         SHOWCB RPL=(R2),FIELDS=(RECLEN),                              X
               AREA=(R8),LENGTH=4,                                     X
               MF=(G,VSAMGLST)
         LTR   R15,R15                  SUCCESSFUL ???
         BRNZ  VSBRW20                  NO  - INDICATE I/O    ERROR
         L     R4,FCRECLEN              LOAD  RETURNED RECORD LENGTH
         STH   R4,PARECLEN              PASS  BACK  TO CALLER
         B     RETURN                   RETURN  TO CALLER
*
VSBRW20  MVC   PAVSAMRC+1(1),RPLFDB3    RETURN VSAM FEEDBACK CODE
         B     RTNERRE
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   W R I T E   (A D D   N E W   R E C O R D)          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VSWRITE  CLI   FCFMODE,C'O'       CORRECT FILE MODE ???
         BRE   VSWRT10            YES - CONTINUE
         CLC   FCFMODE,IO         CORRECT FILE MODE ???
         BRE   VSWRT10            YES - CONTINUE
         CLC   FCFMODE,EX         CORRECT FILE MODE ???
         BRE   VSWRT12            YES - CONTINUE
         J     RTNERRL            NO  - RETURN (LOGIC ERROR)
*
VSWRT10  EQU   *
         CLI   PARESDS,C'D'       ESDS update in place ?
         BE    VSWRT16
*
         ENDREQ RPL=(R2)          RELEASE POSITIONING IF ANY
         LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSWRT20            NO  - INDICATE I/O ERROR
*
         LH    R4,PARECLEN        LOAD RECORD  LENGTH
*
         MODCB RPL=(R2),                                               +
               AREA=(R6),AREALEN=(R4),RECLEN=(R4),                     +
               OPTCD=(KEY,DIR,ARD,SYN,FWD,NUP,MVE),                    +
               MF=(G,VSAMGLST)
         J     VSWRT13
*
VSWRT12  EQU   *
         LH    R4,PARECLEN        LOAD RECORD  LENGTH
*
         MODCB RPL=(R2),                                               +
               AREA=(R6),AREALEN=(R4),RECLEN=(R4),                     +
               OPTCD=(SEQ,NUP),                                        +
               MF=(G,VSAMGLST)
*
VSWRT13  EQU   *
         LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSWRT20            NO  - INDICATE I/O ERROR
*
         PUT   RPL=(R2)           WRITE NEW RECORD
         LTR   R15,R15            SUCCESSFUL ???
         BRZ   RETURN             YES - RETURN
         B     VSWRT20
*
VSWRT16  EQU   *
         LH    R4,PARECLEN        LOAD RECORD  LENGTH
*
         MODCB RPL=(R2),                                               +
               AREA=(R6),AREALEN=(R4),RECLEN=(R4),                     +
               OPTCD=(ADR,SEQ,SYN,UPD,MVE),                            +
               MF=(G,VSAMGLST)
         LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSWRT20            NO  - INDICATE I/O ERROR
*
         PUT   RPL=(R2)           WRITE NEW RECORD
         LTR   R15,R15            SUCCESSFUL ???
         BRZ   RETURN             YES - RETURN
         B     VSWRT20            NO  - ERROR
*
VSWRT20  MVC   PAVSAMRC+1(1),RPLFDB3    RETURN VSAM FEEDBACK CODE
         B     RTNERRE
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   U P D A T E   (P R E V I O U S   R E C O R D)      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VSUPDATE CLC   FCFMODE,IO         CORRECT FILE  MODE ???
         BRNE  RTNERRL            NO  - RETURN (LOGIC ERROR)
*
         CLC   FCLSTFUN,RD        WAS PREVIOUS FUNCTION A READ ???
         BRE   VSUPD05            YES - CONTINUE
         CLC   FCLSTFUN,LO        WAS PREVIOUS FUNCTION A LOCATE ???
         BRE   VSUPD05            YES - CONTINUE
         CLC   FCLSTFUN,BR        WAS PREVIOUS FUNCTION A BROWSE ???
         BRNE  RTNERRL            NO  - RETURN (LOGIC ERROR)
*
VSUPD05  LH    R4,PARECLEN        LOAD NEW RECORD LENGTH
         C     R4,FCRECLEN        HAS  THE LENGTH CHANGED ???
         BRNE  VSUPD10
*
         MODCB RPL=(R2),AREA=(R6),OPTCD=(MVE),                         +
               MF=(G,VSAMGLST)
         B     VSUPD15
*
VSUPD10  MODCB RPL=(R2),                                               +
               AREA=(R6),AREALEN=(R4),RECLEN=(R4),OPTCD=(MVE),         +
               MF=(G,VSAMGLST)
*
VSUPD15  LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSUPD20            NO  - INDICATE I/O ERROR
*
         PUT   RPL=(R2)           UPDATE PREVIOUS RECORD
         LTR   R15,R15            SUCCESSFUL ???
         BRZ   RETURN             YES - RETURN
*
VSUPD20  MVC   PAVSAMRC+1(1),RPLFDB3    RETURN VSAM FEEDBACK CODE
         B     RTNERRE
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   D E L E T E   (P R E V I O U S   R E C O R D)      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VSDELETE CLC   FCFMODE,IO         CORRECT FILE  MODE  ???
         BRNE  RTNERRL            NO  - RETURN (LOGIC ERROR)
*
         CLC   FCLSTFUN,RD        WAS PREVIOUS FUNCTION A READ ???
         BRE   VSDEL10            YES - CONTINUE
         CLC   FCLSTFUN,LO        WAS PREVIOUS FUNCTION A LOCATE ???
         BRNE  RTNERRL            NO  - RETURN (LOGIC ERROR)
*
VSDEL10  ERASE RPL=(R2)           ERASE PREVIOUS RECORD READ
         LTR   R15,R15            SUCCESSFUL ???
         BRZ   RETURN             YES - RETURN
*
VSDEL20  MVC   PAVSAMRC+1(1),RPLFDB3    RETURN VSAM FEEDBACK CODE
         B     RTNERRE
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   I N F O                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VSINFO   DS    0H
         SHOWCB ACB=(R3),AREA=(R6),LENGTH=16,                          X
               FIELDS=(RKP,KEYLEN,LRECL,NLOGR),                        X
               MF=(G,VSAMGLST)
         LTR   R15,R15            SUCCESSFUL ???
         BRNZ  VSINF20            NO  - INDICATE I/O ERROR
         L     R4,8(,R6)          LOAD  RETURNED RECORD LENGTH
         STH   R4,PARECLEN        PASS  BACK  TO CALLER
         B     RETURN             RETURN  TO CALLER
*
VSINF20  XC    PAVSAMRC+1(1),PAVSAMRC+1 ZERO VSAM FEEDBACK CODE
         B     RTNERRE
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   C L O S E   F I L E                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VSCLOSE  DS    0H                 ISSUE CLOSE
         CLOSE ((R3)),MODE=31,MF=(E,CLSLST1)
*
         MVI   FCSTATUS,C' '      INDICATE FILE CLOSED
*
         LTR   R15,R15            SUCCESSFULLY  CLOSED ???
         BRZ   RETURN             YES - RETURN
*
VSCL20   MVC   PAVSAMRC+1(1),RPLFDB3    RETURN VSAM FEEDBACK CODE
         B     RTNERRE                  INDICATE I/O ERROR
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   R E L E A S E   R E C O R D                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VSRLSE   DS    0H                 ISSUE RELEASE
         ENDREQ RPL=(R2)
*
         LTR   R15,R15            SUCCESSFULLY  RELEASED ???
         BRZ   RETURN             YES - RETURN
*
VSRL20   MVC   PAVSAMRC+1(1),RPLFDB3    RETURN VSAM FEEDBACK CODE
         B     RTNERRE                  INDICATE I/O ERROR
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V S A M   E N D   O F   F I L E   R O U T I N E              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VSAMEOD  CLC   PAFUNC,SB          START BROWSE FUNCTION ???
         BRE   RTNERR2            YES - BYPASS END-OF-FILE FLAG SET
         CLC   PAFUNC,LO          LOCATE       FUNCTION ???
         BRE   RTNERR1            YES - BYPASS END-OF-FILE FLAG SET
*
         MVI   FCSTATUS,C'E'      INDICATE END-OF-FILE STATUS
*
         B     RTNERR2
*
         DROP  R2
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        Q S A M   O P E N   F I L E                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING IHADCB,R3
*
QSOPEN   LTR   R7,R7              MATCHING "DDNAME" FOUND ???
         BRZ   QSOP05             NO  - ALLOCATE MEMORY
*
         CLI   FCFTYPE,C'S'       CORRECT FILE TYPE ???
         BRNE  RTNERRB            NO  - RETURN (BAD PARAMETER)
*
         B     QSOP10             YES - BYPASS ALLOCATION
*
QSOP05   LA    R4,FCPREFLN+DCBLEN        LOAD  CORRECT LENGTH FOR QSAM
         BRAS  R8,ALLOCATE               ALLOCATE  "FCB"
         MVC   FCDCBACB(DCBLEN),MODELDCB INITIALIZE QSAM "DCB"
         MVC   DCBDDNAM,FCDDNAME         FILL  IN   DDNAME
         LA    R14,QSAMEODA              FILL  IN   EODAD
         STCM  R14,B'0111',DCBEODA
         LA    R0,DCBEOFF(,R3)
         ST    R0,DCBDCBE
*
QSOP10   CLI   FCSTATUS,C' '      ALREADY OPEN ???
         BRNE  RTNERRL            YES - RETURN (LOGIC ERROR)
*
         MVC   FCFMODE,PAFMODE    RESET THE  FILE MODE IF IT'S CHANGED
         CLC   FCFMODE,I          INPUT FILE ???
         BRE   QSOP20             YES - BRANCH
         CLC   FCFMODE,O          OUTPUT FILE ???
         JE    QSOP15             Yes - output file
         CLC   FCFMODE,EX         OUTPUT EXTEND FILE ???
         JNE   RTNERRB            NO  - BAD PARAMETER
*
***********************************************************************
*  OPEN FILE FOR OUTPUT                                               *
***********************************************************************
QSOP15   DS    0H                      OPEN  FILE  FOR  OUTPUT
         LA    R1,WORKJFCB             LOAD  JFCB  ADDRESS
         ST    R1,WORKEXIT
         MVI   WORKEXIT,X'87'
*
         LA    R1,WORKEXIT             COPY  EXIT  LIST ADDRESS
         ST    R1,DCBEXLST
*
         MVC   WORKRENT,OPENPARM
         RDJFCB ((R3)),MF=(E,WORKRENT) READ  JOB-FILE-CONTROL-BLOCK
         LTR   R15,R15            SUCCESSFUL ???
         BRZ   QSOP16
*
         DC    H'0'
*
QSOP16   LA    R14,WORKJFCB       LOAD JFCB  ADDRESS
         USING JFCBAR,R14
*
         LH    R15,JFCLRECL       "LRECL"  SPECIFIED  ???
         LTR   R15,R15
         BRNZ  QSOP17
*
         LH    R15,PARECLEN
         LTR   R15,R15
         BRNP  RTNERRB
*
         STH   R15,DCBLRECL
*
QSOP17   TM    JFCRECFM,X'C0'     "RECFM"  SPECIFIED  ???
         BRNZ  QSOP18
*
         CLI   PARECFMT,C'F'
         BRNE  *+12
         OI    DCBRECFM,X'90'
         BRC   15,QSOP18
*
         CLI   PARECFMT,C'V'
         BRNE  *+12
         OI    DCBRECFM,X'50'
         BRC   15,QSOP18
*
         BRC   15,RTNERRB
*
         DROP  R14
*
QSOP18   EQU   *
         CLC   PAFMODE,EX         OUTPUT EXTEND FILE ???
         JE    QSOP19             Yes - go
         OPEN  ((R3),OUTPUT),MODE=31,MF=(E,OPNLST2)
         B     QSOP25
QSOP19   EQU   *
         OPEN  ((R3),EXTEND),MODE=31,MF=(E,OPNLST2)
         B     QSOP25
*
***********************************************************************
*  OPEN FILE FOR INPUT                                                *
***********************************************************************
QSOP20   DS    0H                 OPEN  FILE FOR INPUT
         OPEN  ((R3),INPUT),MODE=31,MF=(E,OPNLST3)
*
***********************************************************************
*  CHECK FOR SUCCESSFUL OPEN (COMMON)                                 *
***********************************************************************
QSOP25   LA    R15,8              SET R15 TO SHOW ERROR (LIKE VSAM)
         TM    DCBOFLGS,DCBOFOPN  SUCCESSFUL OPEN ???
         BRNO  RTNERRE            NO  - INDICATE I/O ERROR
*
         MVI   FCSTATUS,C'O'      INDICATE FILE OPEN
*
         LH    R4,DCBLRECL        LOAD MAXIMUM RECORD LENGTH
         ST    R4,FCRECLEN        SAVE MAXIMUM RECORD LENGTH
         STH   R4,PARECLEN        PASS RECORD LENGTH  BACK TO CALLER
*
         MVI   PARECFMT,C'F'      ASSUME    FIXED  LENGTH  RECORDS
         TM    DCBRECFM,X'40'     VARIABLE  LENGTH RECORDS ???
         BRNO  RETURN             NO  - RETURN
         MVI   PARECFMT,C'V'      INDICATE  VARIABLE LENGTH RECORDS
*
         B     RETURN             RETURN TO CALLER
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        Q S A M   S E Q U E N T I A L   R E A D                      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
QSREAD   CLI   FCFMODE,C'I'       CORRECT FILE  MODE  ???
         BRNE  RTNERRL            NO  - RETURN (LOGIC ERROR)
*
         LR    R1,R3              R1  = DCB     ADDRESS
         SR    R15,R15            R15 = SUBROUTINE ADDRESS
         ICM   R15,7,DCBGETA
*
         LA    R14,QSRD10         SETUP RETURN  STUB (24-BIT)
         O     R14,MODE
         ST    R14,RETURNA
         LA    R14,STUB24
         BR    R15                BRANCH
*
QSRD10   LA    R5,0(,R1)          R5 = RECORD ADDRESS
         LH    R4,DCBLRECL        LOAD RECORD LENGTH
         ST    R4,FCRECLEN        SAVE RECORD LENGTH
         STH   R4,PARECLEN        PASS RECORD LENGTH  BACK TO CALLER
*
         LR    R14,R5             LOAD "FROM" ADDRESS
         LR    R15,R6             LOAD "TO"   ADDRESS
         LA    R0,255(,R4)              CALCULATE GROUPS/BYTES
         LR    R4,R0                    RESIDUAL BYTES TO MOVE
         SRL   R0,8                     NUMBER OF GROUPS + ONE
         B     QRMVC#20
QRMVC#10 MVC   0(256,R15),0(R14)        MOVE 256 BYTE GROUP
         LA    R15,256(,R15)            --> NEXT TARGET AREA
         LA    R14,256(,R14)            --> NEXT SOURCE AREA
QRMVC#20 BRCT  R0,QRMVC#10
         EX    R4,QR_MVC                MOVE RESIDUAL BYTES
*
         B     RETURN                   RETURN  TO CALLER
*
QR_MVC   MVC   0(0,R15),0(R14)          * E X E C U T E D *
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        Q S A M   W R I T E                                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
QSWRITE  CLC   FCFMODE,O          CORRECT FILE  MODE   ???
         JE    QSWRT01            == Output
         CLC   FCFMODE,EX         CORRECT FILE  MODE   ???
         JNE   RTNERRL            == Extend
*                                 Else logic error
QSWRT01  EQU   *
         LH    R4,PARECLEN        LOAD  ACTUAL  RECORD LENGTH
         ST    R4,FCRECLEN        SAVE  RECORD  LENGTH
*
         TM    DCBRECFM,X'40'     VARIABLE LENGTH RECORDS ???
         BRNO  QSWRT05            NO  - BRANCH
*
         STH   R4,DCBLRECL        CHANGE RECORD LENGTH
*
QSWRT05  LR    R1,R3              R1  = DCB     ADDRESS
         LA    R0,0(,R6)          R5  = RECORD  ADDRESS
         SR    R15,R15            R15 = SUBROUTINE ADDRESS
         ICM   R15,7,DCBPUTA
*
         LA    R14,QSWRT10        SETUP RETURN  STUB (24-BIT)
         O     R14,MODE
         ST    R14,RETURNA
         LA    R14,STUB24
         BR    R15                BRANCH
*
*SWRT10  LA    R5,0(,R1)          R5 = RECORD ADDRESS
*        LR    R14,R6             LOAD "FROM" ADDRESS
*        LR    R15,R4             LOAD "FROM" LENGTH
*        LR    R3,R4              LOAD "TO"   LENGTH
*        LR    R2,R5              LOAD "TO"   ADDRESS
*        MVCL  R2,R14             COPY  RECORD
*
QSWRT10  B     RETURN             RETURN TO CALLER
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        Q S A M   C L O S E   F I L E                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
QSCLOSE  DS    0H                 ISSUE CLOSE
         CLOSE ((R3)),MODE=31,MF=(E,CLSLST1)
*
         LTR   R15,R15            SUCCESSFUL ???
         BRNZ  RTNERRE            NO  - INDICATE I/O ERROR
*
         MVI   FCSTATUS,C' '      CLEAR  FILE STATUS
         B     RETURN             RETURN TO   CALLER
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        Q S A M   F I L E   I N F O R M A T I O N                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
QSINFO   XC    00(4,R6),00(R6)    ZERO  THE KEY OFFSET
         XC    04(4,R6),04(R6)    ZERO  THE KEY LENGTH
*
         LH    R15,DCBLRECL       LOAD  RECORD  LENGTH
         ST    R15,08(,R6)        PASS  LENGTH  TO   CALLER
*
         XC    12(4,R6),12(R6)    ZERO  THE RECORD   COUNT
*
         LH    R15,DCBBLKSI       LOAD  BLOCK   SIZE
         ST    R15,16(,R6)        PASS  LENGTH  TO   CALLER
*
         B     RETURN             RETURN TO   CALLER
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        Q S A M   E N D   O F   F I L E   R O U T I N E              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
QSEOF    MVI   FCSTATUS,C'E'      INDICATE END-OF-FILE
*
         B     RTNERR2            SET RETURN CODE TO END-OF-FILE
*
         DROP  R3
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P R O G R A M   T E R M I N A T I O N                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
RTNERR1  MVI   PARTNC,C'1'        INDICATE "RECORD NOT FOUND"
         B     RETURN
*
RTNERR2  MVI   PARTNC,C'2'        INDICATE "END-OF-FILE"
         B     RETURN
*
RTNERRB  MVI   PARTNC,C'B'        INDICATE "BAD PARAMETER DATA"
         B     RETURN
*
RTNERRE  STC   R15,PAVSAMRC       PASS BACK R15 IN HIGH BYTE
         STC   R15,FCLSTR15       SAVE THE REGISTER 15 CONTENTS
*
         CLC   PAFUNC,RD          READ   FUNCTION ???
         BRE   RTNERRE3           YES -  CHECK    NOT FOUND
         CLC   PAFUNC,LO          LOCATE FUNCTION ???
         BRE   RTNERRE3           YES -  CHECK    NOT FOUND
         CLC   PAFUNC,SB          START  FUNCTION ???
         BRNE  RTNERRE5           NO  -  BYPASS   NOT FOUND CHECK
*
RTNERRE3 CLC   PAVSAMRC,X0810     VSAM   RECORD   NOT FOUND ???
         BRE   RTNERR1            YES -  CHANGE   RETURN CODE
*
RTNERRE5 MVI   PARTNC,C'E'        INDICATE "I/O ERROR"
         B     RETURN
*
RTNERRL  MVI   PARTNC,C'L'        INDICATE "LOGIC PROBLEM"
         B     RETURN
*
RETURN   LTR   R7,R7              "FCB" AVAILABLE ???
         BRNP  RETURN5            NO  - BYPASS SAVING LAST CALL INFO
*
         MVC   FCLSTFUN,PAFUNC    SAVE LAST FUNCTION EXECUTED
         MVC   FCLSTRC,PARTNC     SAVE LAST RETURN   CODE
         MVC   FCLSTVSM,PAVSAMRC  SAVE LAST VSAM     RETURN   CODE
*
RETURN5  EQU   *
*        LA    R15,PALEN-1(,R12)
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=200,STORAGE=((R12),(R15))
         SR    R15,R15            SET  RETURN CODE TO ZERO(ASSUME OKAY)
         CLI   PARTNC,C'0'        SUCCESSFUL  ???
         BRE   RETURN10           YES - BYPASS CHANGING OF RETURN CODE
         LA    R15,4              NO  - CHANGE RETURN CODE
*
RETURN10 DS    0H
         L     R13,RSABP(,R13)    RESTORE R13
         XC    RSAFP(4,R13),RSAFP(R13)    CLEAR FORWARD POINTER
*
RETURN15 L     R14,RSA14(,R13)    RESTORE REGISTERS
         LM    R0,R12,RSA0(R13)
         bsm   0,r14              Return
*
         DROP  R7
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "ALLOCATE" - ALLOCATES MEMORY DYNAMICALLY FOR NEW FCB'S.  THE       *
*              LENGTH IS PROVIDED BY THE CALLER SINCE VSAM & QSAM     *
*              HAVE DIFFERENT CONTROL BLOCK LENGTHS.                  *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R8  - RETURN ADDRESS                                         *
*        R7  - CURRENT  FCB ADDRESS                                   *
*        R4  - REQUIRED FCB LENGTH                                    *
*        R3  - CURRENT  ACB/DCB ADDRESS                               *
*        R1  - AREA ADDRESS                                           *
*        R0  - AREA LENGTH                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING FCB,R7
*
ALLOCATE LR    R0,R4              LOAD  AREA LENGTH
         GETMAIN R,LV=(0)
*
         LR    R7,R1              INITIALIZE CURRENT FCB ADDRESS
*
         L     R14,FCBLAST            ADD TO FCB   CHAIN
         ST    R7,FCBNEXT-FCB(,R14)
         ST    R7,FCBLAST
*
         STH   R4,FCBLEN
*
         XC    FCBNEXT,FCBNEXT    INITIALIZE FORWARD CHAIN POINTER
*
         MVC   FCDDNAME,PADDNAME  INITIALIZE DDNAME
         MVC   FCFTYPE,PAFTYPE    INITIALIZE FILE TYPE
         MVC   FCFMODE,PAFMODE    INITIALIZE FILE MODE
*
         MVI   FCSTATUS,C' '      INITIALIZE FILE STATUS
         XC    FCRECLEN,FCRECLEN  INITIALIZE RECORD LENGTH
         MVC   FCLSTFUN,SPACES    INITIALIZE LAST FUNCTION EXECUTED
         MVI   FCLSTRC,C' '       INITIALIZE LAST RETURN       CODE
         XC    FCLSTR15,FCLSTR15  INITIALIZE LAST REGISTER  15 VALUE
         XC    FCLSTVSM,FCLSTVSM  INITIALIZE LAST VSAM  RETURN CODE
*
         LA    R3,FCDCBACB        INITIALIZE ACB/DCB    ADDRESS
*
         BR    R8                 RETURN  TO CALLER
*
         DROP  R7
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCFCB" - SEARCHES THE "FCB" CHAIN FOR AN ENTRY WHICH MATCHES      *
*            THE "DDNAME" IN THE PARAMETER AREA.                      *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R8  - RETURN ADDRESS                                         *
*        R7  - CURRENT  "FCB"  ADDRESS                                *
*        R3  - CURRENT ACB/DCB ADDRESS                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
LOCFCB   L     R7,FCBCHAIN+(FCBNEXT-FCB)  INITIALIZE "FCB" ADDRESS
*
         SR    R3,R3              INITIALIZE CURRENT "ACB" ADDRESS
*
         B     LOCEND             CHECK  FOR END-OF-CHAIN
*
         USING FCB,R7
*
LOCLOOP  L     R7,FCBNEXT
LOCEND   LTR   R7,R7              END-OF-CHAIN ???
         BZR   R8                 YES - EXIT SUBROUTINE
*
         CLC   FCDDNAME,PADDNAME  MATCHING ENTRY ???
         BRNE  LOCLOOP            NO  - ADVANCE TO NEXT ENTRY ON CHAIN
*
         LA    R3,FCDCBACB        INITIALIZE ACB/DCB ADDRESS
*
         BR    R8                 YES - EXIT SUBROUTINE
*
         DROP  R7
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* STATIC STORAGE:                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
F4       DC    F'04'
F8       DC    F'08'
*
MODE31   DS   0XL4
OPENPARM DC    XL8'8000000000000000'
*
OP       DC    CL2'OP'
CL       DC    CL2'CL'
RD       DC    CL2'RD'
LO       DC    CL2'LO'
SB       DC    CL2'SB'
BR       DC    CL2'BR'
WR       DC    CL2'WR'
UP       DC    CL2'UP'
DL       DC    CL2'DL'
IN       DC    CL2'IN'
RI       DC    CL2'RI'
*
I        DC    CL2'I '
O        DC    CL2'O '
IO       DC    CL2'IO'
EX       DC    CL2'EX'
X0810    DC    XL2'0810'
*
SPACES   DC    CL08' '
QSEOFA   DC    AL4(QSEOF)
*
STUBCODE DS   0F                  24-BIT TO 31-BIT RETURN
         L     R14,6(,R14)        LOAD REAL RETURN ADDRESS
         BSM   0,R14              RESET ADDRESSING MODE
*
EODCODE  L     R14,QSEOFA         LOAD BRANCH ADDRESS
         O     R14,MODE           RESTORE ORIGINAL MODE(24-BIT, 31-BIT)
         BSM   0,R14              SWITCH BACK TO ORIGINAL MODE
*
VSMEXLST EXLST EODAD=VSAMEOD
*
         DS   0D
MODELACB ACB   AM=VSAM,                                                X
               DDNAME=DUMMY,                                           X
               EXLST=VSMEXLST,                                         X
               RMODE31=ALL,                                            X
               MACRF=(KEY,DIR,SEQ,IN,DSN)
ACBLEN   EQU   *-MODELACB
*
         DS   0D
MODELRPL RPL   AM=VSAM,                                                X
               OPTCD=(KEY,SEQ,FWD,SYN,NUP,MVE)
RPLLNG   EQU   *-MODELRPL
*
         DS   0D
MODELDCB DCB   DSORG=PS,DDNAME=DUMMY,MACRF=(GL,PM),DCBE=MDLDCBE
DCBEOFF  EQU   *-MODELDCB
MDLDCBE  DCBE  RMODE31=BUFF
DCBLEN   EQU   *-MODELDCB
*
*        THE STATIC & DYNAMIC REMOTE LISTS MUST CORRESPOND
*
LISTSTR1 DS    0D
*
OPEN1    OPEN  (,),MODE=31,MF=L
OPENLN1  EQU   (*-OPEN1)
*
         DS    0D
OPEN2    OPEN  (,(OUTPUT)),MODE=31,MF=L
OPENLN2  EQU   (*-OPEN2)
*
         DS    0D
OPEN3    OPEN  (,(INPUT)),MODE=31,MF=L
OPENLN3  EQU   (*-OPEN3)
*
         DS    0D
CLSE1    CLOSE (,),MODE=31,MF=L
CLSELN1  EQU   (*-CLSE1)
*
LISTLEN1 EQU   (*-LISTSTR1)            LENGTH OF ALL REMOTE LISTS
                        EJECT
         PRINT NOGEN
         IFGRPL AM=VSAM
*
         DCBD  DSORG=PS
                        SPACE 3
JFCBAR   DSECT
         IEFJFCBN LIST=YES
*
         LTORG
*
         END
