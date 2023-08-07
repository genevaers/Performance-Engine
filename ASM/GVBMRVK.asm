         TITLE 'GVBMRVK - GVBMR95 VSAM KEYED I/O HANDLER'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2005, 2021.
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
*  GVBMRVK - READS A VSAM FILE INTO GENEVA AS AN EVENT FILE
*            USING KEYED ACCESS.  THIS IS TYPICALLY USED WHEN
*            A KEYED VSAM FILE IS NEEDED AS A REFERENCE FILE.
*
*  NOTES:  - GVBMRVK RUNS IN 31-BIT ADDRESSING MODE.
*          - CHANGES TO THIS PROGRAM MUST BE COMPATIBLE
*            WITH VSAM EXTENDED DATASETS.
*
*  RETURN CODES:
*
*           00  - SUCCESSFUL
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   R E G I S T E R  U S A G E:
*
*   R15   -  STANDARD LINKAGE/WORK REGISTER
*   R14   -  STANDARD LINKAGE/WORK REGISTER
*   R13   -  STANDARD LINKAGE/THREAD WORK AREA
*   R12   -  PROGRAM BASE
*   R11   -  RESERVED
*   R10   -  EVENT READ ROUTINE BASE REGISTER
*   R9    -  EVENT READ ROUTINE 'NORMAL' RETURN ADDRESS
*   R8    -  VSAM RPL ADDRESS
*   R7    -  VSAM ACB ADDRESS
*   R6    -  CURRENT RECORD ADDRESS
*   R5    -  NOT USED
*   R4    -  WORK REGISTER
*   R3    -  WORK REGISTER
*   R2    -  STANDARD USAGE/WORK REGISTER
*   R1    -  STANDARD LINKAGE/WORK REGISTER
*   R0    -  STANDARD LINKAGE/WORK REGISTER
*
         EJECT
         PRINT NOGEN
         YREGS
*
RSABP    EQU   04,4
RSAFP    EQU   08,4
RSA14    EQU   12,4
RSA15    EQU   16,4
RSA0     EQU   20,4
RSA1     EQU   24,4
RSA6     EQU   44,4
*
MSG22    EQU   22                      UNABLE TO OPEN EVENT FILE
MSG37    EQU   37                      MODCB MACRO FAILURE
*
         Copy  GVBASSRT
         Copy  GVBMR95L
         Copy  GVBMR95C
         Copy  GVBMR95W
         Copy  GVB0200A
         Copy  GVB0200B
         Copy  GVBX95PA
         Copy  GVBUTEQU
*                                      GLOBAL REGISTER ASSIGNMENTS
         IEABRCX DEFINE
*
         USING THRDAREA,R13             - THREAD WORK AREA
         using genparm,parm_area
         using genenv,env_area
         using genfile,file_area
*
GVBMRVK  CSECT
GVBMRVK  RMODE 24
GVBMRVK  AMODE 31
*
         j     code               skip to code start (over static area)
static   loctr
MRVKEYE  GVBEYE GVBMRVK
code     loctr
*
         STM   R14,R12,SAVESUBR+RSA14  SAVE  CALLER'S REGISTERS
         LR    R11,R15                 SET  PROGRAM   BASE REGISTER
         USING (GVBMRVK,CODE),R11
*
MAIN0100 EQU   *
*
         MVC   ERRDATA(8),GPDDNAME     INDICATE file in case of error
*
         LA    R7,VSAMACB              --> EVENT ACB AREA
         MVC   0(MACB1LEN,R7),MODLACB1 CREATE THE ACB
         LHI   R10,MODCB_ACB_FAIL      in case of error
*                                      USE CURRENT DDNAME
         MODCB ACB=(R7),DDNAME=(*,GPDDNAME),                           +
               MF=(G,LKUPKEY)
*
         LTR   R15,R15                 SUCCESSFUL REQUEST?
         BNZ   ERRmsg                  N - PROCESS AS AN ERROR
*
MAIN0300 EQU   *                       OPEN THE EVENT FILE
         LA    R3,=CL8'GENEVA'         --> MAJOR NAME
         LA    R4,GPDDNAME             --> MINOR NAME
         MVC   WORKAREA(ENQM1LEN),ENQMODL1
         ENQ   ((R3),(R4),,,,),MF=(E,WORKAREA)
*
         MVC   LKUPKEY(MOPN1LEN),MODLOPN1
         OPEN  ((R7),(INPUT)),MODE=31,MF=(E,LKUPKEY)
*
         C     R15,=F'4'               SUCCESSFUL/WARNING?
         BH    ERROR#02                N - SHOW AS AN ERROR
         DEQ   (,,,,),MF=(E,WORKAREA)
*
MAIN0400 EQU   *                       REQUEST PARAMETER LIST
         LA    R8,VSAMRPL              --> EVENT FILE RPL
         MVC   0(MRPL1LEN,R8),MODLRPL1 CREATE THE RPL
         LA    R2,VSAMPTR              --> EVENT RECORD PTR
         LHI   R10,MODCB_RPL_FAIL      in case of error
*
         MODCB RPL=(R8),ACB=(R7),AREA=(R2),AREALEN=4,                  +
               MF=(G,LKUPKEY)
*
         LTR   R15,R15                 SUCCESSFUL REQUEST?
         BNZ   ERRmsg                  N - PROCESS AS AN ERROR
*
MAIN0500 EQU   *
         L     R2,EVNTDCBA             INDICATE FIXED FORMAT
         NI    DCBRECFM-IHADCB(R2),X'3F'
         OI    DCBRECFM-IHADCB(R2),X'80'
*                                      --> EVENT READ ROUTINE
         L     R15,=A(VSAMREAD+X'80000000')
         ST    R15,EVNTREAD            SAVE READ ROUTINE ADDRESS
         BASR  R14,R15                 DO THE PRIMING READ
         ST    R6,SAVESUBR+RSA6        PASS BACK RECORD ADDRESS
         SLL   R15,32                  INDICATE A GOOD RETURN!
*
MAIN0900 EQU   *
         L     R14,SAVESUBR+RSA14      RESTORE CALLER'S REGS
         LM    R0,R12,SAVESUBR+RSA0
         BSM   0,R14
         EJECT
*---------------------------------------------------------------------*
*        PROCESS INITIAL CALL ERRORS
*---------------------------------------------------------------------*
ERROR#02 EQU   *
* OPEN VSAM FILE ERROR
         LHI   R10,OPEN_VSAM_SOURCE_FAIL LOAD ERROR MESSAGE NUMBER
         MVC   ERRDATA(8),GPDDNAME     INDICATE WHICH FILE

         CVD   R15,DBLWORK             RETURN CODE
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  WORKAREA(3),DBLWORK

         LA    R2,DBLWORK              GET ERROR FIELD
         SHOWCB ACB=(R7),FIELDS=(ERROR),AREA=(R2),LENGTH=4,            +
               MF=(G,LKUPKEY)
*
         L     R15,DBLWORK
         CVD   R15,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  WORKAREA+4(3),DBLWORK
*
         GVBMSG LOG,MSGNO=(r10),SUBNO=4,                               +
               GENENV=GENENV,                                          +
               SUB1=(modname,L'modname),                               +
               SUB2=(ERRDATA,8),                                       +
               SUB3=(WORKAREA,3),                                      +
               SUB4=(WORKAREA+4,3),                                    +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
*
         B     VSAMERRX                EXIT TO SYNAD ROUTINE
*
ERRMSG   DS    0H
         GVBMSG LOG,MSGNO=(r10),SUBNO=2,                               +
               GENENV=GENENV,                                          +
               SUB1=(modname,L'modname),                               +
               SUB2=(ERRDATA,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
*
         B     VSAMERRX                EXIT TO SYNAD ROUTINE

         EJECT
*---------------------------------------------------------------------*
*        STATIC STORAGE FOR THE INITIALIZATION
*---------------------------------------------------------------------*
static   loctr
         DS    0D
MODLACB1 ACB   AM=VSAM,                                                +
               DDNAME=DUMMY,                                           +
               EXLST=VSMEXLST,                                         +
               STRNO=1,                                                +
               BUFNI=4,                                                +
               BUFND=200,                                              +
               RMODE31=ALL,                                            +
               MACRF=(KEY,SEQ,IN)
MACB1LEN EQU   (*-MODLACB1)
*
         DS    0D
VSMEXLST EXLST EODAD=VSAMEOF
*
         DS    0D
MODLRPL1 RPL   ACB=MODLACB1,                                           +
               AM=VSAM,                                                +
               OPTCD=(KEY,SEQ,FWD,LOC)
MRPL1LEN EQU   (*-MODLRPL1)
*
MODLOPN1 OPEN  (,INPUT),MODE=31,MF=L
MOPN1LEN EQU   (*-MODLOPN1)
*
ENQMODL1 DS    0F                      STGREF ENQ/DEQ MODEL
         ENQ   (,,E,L'GPDDNAME,STEP),RNL=NO,MF=L
ENQM1LEN EQU   (*-ENQMODL1)
*
         LTORG
         CNOP  0,4
         DROP  R11,R13
         EJECT
*---------------------------------------------------------------------*
*        1. READ THE NEXT DETAIL EVENT RECORD.
*        2. PASS THE RECORD TO "GVBMR95".
*        3. PERFORM END-OF-FILE PROCESSING.
*
*        NOTE: THIS CODE SEQMENT RUNS AS AN EXTENSION TO GVBMR95.
*---------------------------------------------------------------------*
code     loctr
         USING THRDAREA,R13
         using genparm,parm_area
         using genenv,env_area
         using genfile,file_area
*
VSAMREAD EQU   *
*   This is called directly from GVBMR95 - which has r12 as a base reg
*   so do not use R12 anywhere in here
*
         larl  r11,gvbmrvk
         using (gvbmrvk,code),r11
*
         LR    R9,R14                  SAVE THE RETURN ADDRESS
*
         LA    R8,VSAMRPL              --> EVENT FILE RPL
         GET   RPL=(R8)                OBTAIN NEXT RECORD
*
         LTR   R15,R15                 SUCCESSFUL READ?
         BNZ   VSAMERR1                N - PROCESS THE ERROR
*
         LA    R6,LRECL                GET CURRENT LRECL
         SHOWCB RPL=(R8),FIELDS=(RECLEN),AREA=(R6),LENGTH=4,           +
               MF=(G,LKUPKEY)
*
         LTR   R15,R15                 SUCCESSFUL REQUEST?
         BNZ   VSAMERR2                N - PROCESS THE ERROR
*
         llgt  R6,VSAMPTR              --> CURRENT RECORD
         STG   R6,RECADDR              Save the address
*
         lgf   R0,LRECL                RETURN THE LRECL
         ST    R0,GPRECLEN
         agr   R0,R6                   CALC END-OF-DATA ADDRESS
         stg   R0,EODADDR              RETURN THE EOD ADDRESS
*
         bsm   0,r9                    RETURN TO THE CALLER
*
VSAMEOF  EQU   *
         LA    R3,=CL8'GENEVA'         --> MAJOR NAME
         LA    R4,GPDDNAME             --> MINOR NAME
         MVC   WORKAREA(ENQM2LEN),ENQMODL2
         ENQ   ((R3),(R4),,,,),MF=(E,WORKAREA)
*
         LA    R7,VSAMACB              CLOSE THE EVENT FILE
         MVC   LKUPKEY(MCLO1LEN),MODLCLO1
         CLOSE ((R7)),MODE=31,MF=(E,LKUPKEY)
*
         LTR   R15,R15                 SUCCESSFUL CLOSE?
         BNZ   VSAMERR3                N - PROCESS THE ERROR
*
         DEQ   (,,,,),MF=(E,WORKAREA)
*
         XGR   R6,R6                   Show NO Record Available
         STG   R6,RECADDR              Save the null address
         ST    R6,GPRECLEN
         stg   R6,EODADDR
*
         L     R2,EVNTDCBA             --> EVENT FILE DCB
         using ihadcb,r2               map the DCB
         L     R2,dcbdcbe              --> EVENT FILE DCBE
         USING DCBE,R2                 map the DCBE
         L     R14,DCBeEODA            ---> EOD routine
         Bsm   0,r14                   RETURN TO MR95'S EODAD
         drop  r2
         EJECT
*---------------------------------------------------------------------*
*        PROCESS EVENT READ ERRORS
*---------------------------------------------------------------------*
VSAMERR1 EQU   *      Read error
         LHI   R10,VSAM_GET_FAIL       LOAD ERROR MESSAGE NUMBER
*
         CVD   R15,DBLWORK             RC from GET
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  WORKAREA(3),DBLWORK
* retrieve reason/feedback code from RPL
         la    r2,dblwork
         SHOWCB RPL=(R8),FIELDS=(FDBK),AREA=(R2),LENGTH=4,             +
               MF=(G,WKREENT)
*
         llc   r15,dblwork+3            get reason code from feedback
         cvd   r15,dblwork              convert to char
         oi    dblwork+l'dblwork-1,x'0F'    .
         lay   r14,workarea1                .
         using workarea1,r14                .
         unpk  workarea1(3),dblwork         .
         drop  r14
*
         B     VSAMERR                 return to SYNAD exit routine
*
VSAMERR2 EQU   *      SHOWCB for RECLEN failed
         LHI   R10,VSAM_RECLEN_NOT_FOUND LOAD ERROR MESSAGE NUMBER
*
         CVD   R15,DBLWORK             RC from SHOWCB
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  WORKAREA(3),DBLWORK
*
         CVD   R0,DBLWORK              Reason code from SHOWCB
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         lay   r14,workarea1                .
         using workarea1,r14                .
         UNPK  WORKAREA1(3),DBLWORK
         drop  r14
*
         B     VSAMERR                 return to SYNAD exit routine
*
VSAMERR3 EQU   *      CLOSE error
         LHI   R10,CLOSE_VSAM_SOURCE_FAIL LOAD ERROR MESSAGE NUMBER
*
         CVD   R15,DBLWORK             RC from SHOWCB
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  WORKAREA(3),DBLWORK
*
         LA    R2,DBLWORK              GET ERROR FIELD
         SHOWCB ACB=(R7),FIELDS=(ERROR),AREA=(R2),LENGTH=4,            +
               MF=(G,LKUPKEY)
         L     R15,DBLWORK
         CVD   R15,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         lay   r14,workarea1                .
         using workarea1,r14                .
         UNPK  WORKAREA1(3),DBLWORK
         drop  r14
*
         B     VSAMERR                 return to SYNAD exit routine
*
* I/O error - WRITE message to LOG then go to SYNAD routine
*
VSAMERR  EQU   *
*
         MVC   ERRDATA(8),GPDDNAME     INDICATE WHICH FILE
*
         GVBMSG LOG,MSGNO=(R10),SUBNO=4,                               +
               GENENV=GENENV,                                          +
               SUB1=(modname,L'modname),                               +
               SUB2=(ERRDATA,8),                                       +
               SUB3=(workarea,3),                                      +
               SUB4=(workarea1,3),                                     +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
*
VSAMERRX EQU   *
         XGR   R6,R6                   Show NO Record Available
         STG   R6,RECADDR              Save the null address
         ST    R6,GPRECLEN
         L     R2,EVNTDCBA             --> EVENT FILE DCB
         using ihadcb,r2               map the DCB
         L     R2,dcbdcbe              --> EVENT FILE DCBE
         USING DCBE,R2                 map the DCBE
         L     R14,DCBesyna           ---> sunad routine
         Bsm   0,r14                   RETURN TO MR95'S SYNAD
         drop  r2
         EJECT
*---------------------------------------------------------------------*
*        STATIC STORAGE FOR THE EVENT READ ROUTINE
*---------------------------------------------------------------------*
static   loctr
         DS    0D
*
MODLCLO1 CLOSE (,),MODE=31,MF=L
MCLO1LEN EQU   (*-MODLCLO1)
*
ENQMODL2 DS    0F                      STGREF ENQ/DEQ MODEL
         ENQ   (,,E,L'GPDDNAME,STEP),RNL=NO,MF=L
ENQM2LEN EQU   (*-ENQMODL2)
*
         LTORG
         CNOP  0,4
         DROP  R11,R13
*
         PRINT NOGEN
         IHASAVER ,
         IHAPSA DSECT=YES
         IHAASCB DSECT=YES
         DCBD  DSORG=PS
         ihadcbe
         EJECT
GVBMRVK  CSECT
*
         END
