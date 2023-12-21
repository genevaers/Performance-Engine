         TITLE 'GVBMRAD - ADABAS I/O INITIALIZATION FOR GVBMR96'
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
*                                                                     *
*   GVBMRAD -                                                         *
*            ADABAS I/I -- initialization                             *
*                                                                     *
*                                                                     *
*  NOTE:     GVBMRAD RUNS IN 31-BIT ADDRESSING MODE.                  *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - SUCCESSFUL                                          *
*           NN  - ERROR MESSAGE NUMBER                                *
*                                                                     *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK REGISTER                                *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - TEMPORARY WORK REGISTER                                *
*            - INTERNAL  SUBROUTINE  RETURN ADDRESS (3RD LEVEL)       *
*            - RETURN    ADDRESS                                      *
*                                                                     *
*        R13 - REGISTER  SAVE AREA  ADDRESS (THREAD WORK AREA)        *
*                                                                     *
*        R12 -                                                        *
*        R11 -                                                        *
*                                                                     *
*        R10 - BASE                                                   *
*        R9  - INTERNAL  SUBROUTINE  RETURN ADDRESS                   *
*                                                                     *
*        R8  - SQL CALL  WORK AREA ADDRESS                            *
*                                                                     *
*        R7  - SQL DESCRIPTOR AREA ADDRESS ("SQLDA")                  *
*            - SQL TEXT       AREA ADDRESS ("SQLTAREA")               *
*                                                                     *
*        R6  - CURRENT   ROW       ADDRESS                            *
*                                                                     *
*        R5  -                                                        *
*                                                                     *
*        R4  - WORK      REGISTER                                     *
*            - "SQLVARN" DSECT BASE REGISTER                          *
*                                                                     *
*        R3  - WORK      REGISTER                                     *
*        R2  - WORK      REGISTER                                     *
*                                                                     *
*        R1  - TEMPORARY WORK REGISTER                                *
*        R0  - TEMPORARY WORK REGISTER                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * #LB
*                 C H A N G E   C O N T R O L   L O G
*
* 04/01/00  DFK          - ORIGINAL DEVELOPMENT
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * #LE
*
         Copy  GVBASSRT
                        EJECT
         COPY  EXECDATA
                        EJECT
         COPY  GVBMR95W
                        EJECT
         COPY  GVBMR95L
                        EJECT
         COPY  GVBMR95C
                        EJECT
         COPY  GVB0200A
         COPY  GVB0200B
         COPY  GVBX95PA
         copy  gvblogit
         copy  gvbrptit
         copy  GVBUTEQU
*
         COPY  ASMMSP
LEAVE    OPSYN ASM_LEAVE
         ASMMREL ON
*
         ihasaver
                        EJECT
SQLTAREA DSECT                    SQL STATEMENT TEXT AREA
*
SQLBUFFR DS    HL2,CL10238        SQL STATEMENT LENGTH, STATEMENT TEXT
         ORG   SQLBUFFR
         DS    HL2
SQLTEXT  DS    CL10238
         ORG
*
WKAREA   DSECT
WKSAVE1  DS    18F
WKSAVE2  DS    18F
WKPARM0  DS    XL4                *subtask*
WKPARM1  DS    XL4                *subtask*
WKPARM   DS    CL100              *subtask*
WKPARMX  DS    CL100              *NOT*
WKRENT   DS    XL128              RE-ENTRANT  PARAMETER  LIST *NOT*
WKEXUCUR DS    A                  EXU last send to MR95 *last blk*
WKECBSUB DS    XL4                ECB subtask *subtask*
WKTCBSUB DS    XL4                TCB subtask *subtask*
WKPARALL DS    XL2                *subtask*
         DS    XL2
WKPLISTA DS    A                  *NOT*
WKPL6    DS    PL6                *NOT*
WKDBL1   DS    D                  *subtask*
*
WKSUBERR DS    F                  Subtask return code *subtask*
WKTOKNRC DS    A                  NAME/TOKEN  SERVICES RETURN CODE *NO*
WKTOKNAM DS    XL16               TOKEN NAME
WKTOKN   DS    XL16               TOKEN
REENTWK  DS    XL128              RE-ENTRANT PARAMETER   LIST
SYSDCB   DS    (SYSFILEL)C        SYSIN        "DCB"
WKREC    DS    Cl80
WKEXUADR DS    A                  ADDRESS of EXUEXU table after header
WKECBLST DS    A                  ADDRESS OF ECB LIST TO WAIT ON
WKEXULEN DS    F                  Length of EXUEXU table including head
WKECBLEN DS    F                  Length of ECB list
WKR15RSA DS    F                  Special save area for R15
WKLEN    EQU   (*-WKAREA)
*
         YREGS
*
RSA14    EQU   12
RSA15    EQU   16
RSA0     EQU   20
RSA1     EQU   24
RSA6     EQU   44
*
SUBSOFF  EQU   297         DB2 SUBSYSTEM ID OFFSET IN REFERENCE RECORD
TEXTOFF  EQU   301         DB2 SQL  TEXT    OFFSET IN REFERENCE RECORD
ERRTXTLN EQU   80          DB2  ERROR DESCRIPTION  LINE LENGTH
         PRINT GEN
         SYSSTATE ARCHLVL=2
         print off
         COPY  ASMMSP
LEAVE    OPSYN ASM_LEAVE
         asmmrel on
         print on
*
GVBMRAD  RMODE 31
GVBMRAD  AMODE 31
GVBMRAD  CSECT
         j     start              get to the code
MRAUEYE  GVBEYE GVBMRAD
static   loctr                    define the static section
code     loctr                    and the code
         USING THRDAREA,R13       REUSE/RETAIN   MAIN THREAD SAVE  AREA
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
*
START    STM   R14,R12,SAVESUBR+RSA14  SAVE  CALLER'S REGISTERS
*
         llgtr r0,r0
         llgtr r1,r1
         llgtr r2,r2
         llgtr r3,r3
         llgtr r4,r4
         llgtr r5,r5
         llgtr r6,r6
         llgtr r7,r7
         llgtr r8,r8
         llgtr r9,r9
         llgtr r10,r10
         llgtr r11,r11
         llgtr r12,r12
         llgtr r13,r13
         llgtr r14,r14
         llgtr r15,r15
*
         LARL  R10,GVBMRAD        set static area base
         USING (GVBMRAD,code),R10
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        I N I T I A L I Z E   W O R K   A R E A                      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         XC    dblwork2,dblwork2  used to accumulate total getmained
         XC    workmsg(4),workmsg zero translated error msg area
*
         TESTAUTH
         LTR   R15,R15
         JZ    A0002
         GVBMSG LOG,MSGNO=DB2_HPU_NAPF,SUBNO=1,                        +
               SUB1=(PGMNAME,8),                                       +
               GENENV=GENENV,                                          +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         la    r15,8
         J     RETURNE
*
A0002    EQU   *
         LAY   R0,DB2FETCH        INITIALIZE READ ROUTINE ADDRESS
         O     R0,MODE31
         ST    R0,EVNTREAD
*
         L     R2,EVNTDCBA        INDICATE NO RDW PRESENT (FIXED FMT)
         NI    DCBRECFM-IHADCB(R2),X'3F'
         OI    DCBRECFM-IHADCB(R2),X'80'
         MVI   GPRECFMT,C'F'      INDICATE  FIXED LENGTH RECORDS
         MVI   GPRECDEL,X'00'     NO FIELD  DELIMITERS
*
         L     R0,STGBLKLN        ALLOCATE WORAKRA FOR THIS MR95 Thread
         LY    R1,DBLWORK2
         AR    R1,R0 
         STY   R1,DBLWORK2
*
         GETMAIN RU,LV=(0),LOC=(24)
         MVC   0(8,R1),PGMNAME
         USING WKAREA,R8
         LA    R8,L'PGMNAME(,R1)
         ST    R8,SQLWADDR        Anchor to work area in thread
*
*   don't get new save area and link it to old or you loose the thread
*
         LR    R0,R8              INITIALIZE workarea (LOW VALUES)
         L     R1,STGBLKLN
         AHI   R1,-L'PGMNAME
         XR    R14,R14
         XR    R15,R15
         MVCL  R0,R14
*
**********************************************************************
* GET SQL TEXT FROM VDP                                              *
**********************************************************************
*
INITSQL  DS    0H
         LAY   R14,SYSFILE       SYSIN
         MVC   SYSDCB(SYSFILEL),0(R14)
         LAY   R0,SYSDCB+SYSEOFF
         ST    R0,SYSDCB+DCBDCBE-IHADCB
*
         LAY   R14,STATOPEN
         MVC   WKREENT(STATOPENL),0(R14)
         OPEN  (SYSDCB,OUTPUT),MODE=31,MF=(E,WKREENT)
         TM    SYSDCB+48,X'10'   SUCCESSFUL  OPEN ???
         JO    A0046
         GVBMSG LOG,MSGNO=DB2_HPU_SYSI,SUBNO=1,                        +
               SUB1=(PGMNAME,8),                                       +
               GENENV=GENENV,                                          +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         la    r15,8
         J     RETURNE
**********************************************************************
* PERFORM SYMBOLIC VARIABLE SUBSTITUTION FOR DB2 SUBSYSTEM NAME      *
**********************************************************************
A0046    EQU   *
         L     R4,THRDRE          GET SUBSYSTEM NAME
         L     R4,LTVDP200-LOGICTBL(,R4)
         USING vdp0200b_FILE_RECORD,R4
*
         MVC   LKUPKEY(L'ENVVNAME),vdp0200b_DBMS_SUBSYS
         OC    LKUPKEY(L'ENVVNAME),SPACES UPPERCASE  NAME
*
         LA    R0,LKUPKEY        PERFORM SYMBOLIC VARIABLE SUBSTITUTION
         STY   R0,GENPARM1
         LHI   R0,L'ENVVNAME
         STH   R0,DBLWORK
         LA    R0,DBLWORK
         STY   R0,GENPARM2
         LA    R0,gpenvva
         STY   R0,GENPARM3
         LAY   R1,GENPARM1
         L     R15,GVBUR33
         BASR  R14,R15
*
         MVC   DBSUBSYS,LKUPKEY
         MVC   LKUPKEY,SPACES
*
**********************************************************************
* COPY SQL TEXT FROM V4 "VDP" INTO EXPANSION BUFFER                  *
**********************************************************************
         Lhi   R0,F10K              GET BUFFER FOR EXPANDED SQL TEXT
         lr    r15,r0
         ay    r15,dblwork2        add to storage total
         sty   r15,dblwork2        and save it
*
         GETMAIN R,LV=(0),LOC=(ANY)
         LR    R7,R1
         ST    R7,SQLTADDR
         USING SQLTAREA,R7
*
         LA    R0,SQLTEXT           COPY   SQL  TEXT FROM "VDP"
         LHI   R1,L'SQLTEXT
         LA    R14,vdp0200b_DBMS_SQL
         LHI   R15,L'vdp0200b_DBMS_SQL
         ICM   R15,B'1000',SPACES   EXTEND TEXT WITH SPACES
         MVCL  R0,R14
*
         LA    R0,SQLTEXT         SQL TEXT ADDRESS
         STY   R0,GENPARM1
         LHI   R0,L'SQLTEXT       SQL TEXT LENGTH
         STH   R0,DBLWORK
         LA    R0,DBLWORK
         STY   R0,GENPARM2
         LA    R0,gpenvva
         STY   R0,GENPARM3
         LAY   R1,GENPARM1
         L     R15,GVBUR33
         BASR  R14,R15
*
         LHI   R9,L'SQLTEXT       INITIALIZE LOOP    COUNTER
         LA    R1,SQLTEXT
         AR    R1,R9
INITLOOP BCTR  R1,0               BACKUP  TO PRECEEDING BYTE
         CLI   0(R1),C' '         TRAILING   BLANK  ???
         BRNE  INITLEN            NO  - LAST BYTE FOUND (SAVE LENGTH)
         BRCT  R9,INITLOOP        CONTINUE SEARCHING FOR LAST NON-BLANK
*
INITLEN  STH   R9,SQLBUFFR        SAVE  ACTUAL  TEXT LENGTH
*
*        write SYSIN cards ********************************************
*
         XR    R4,R4
         LLGTR R5,R9
         D     R4,=A(37)
         LA    R9,SQLTEXT
         MVC   WKREC,SPACES
A0110    EQU   *
         MVC   WKREC(37),0(R9)
         PUT   SYSDCB,WKREC
         LA    R9,37(,R9)
         BRCT  R5,A0110
*
         LTR   R4,R4
         JZ    A0114
         BCTR  R4,0
         MVC   WKREC,SPACES
         EXRL  R4,MVCREC
         PUT   SYSDCB,WKREC
         J     A0114
MVCREC   MVC   WKREC(0),0(R9)
A0114    EQU   *
*
         LAY   R14,STATCLOS
         MVC   WKREENT(STATCLOSL),0(R14)
         CLOSE (SYSDCB),MODE=31,MF=(E,WKREENT)
*
**********************************************************************
* Examine for parallelism parameter                                  *
**********************************************************************
         LGH   R5,SQLBUFFR
         AFI   R5,-L'PARALLEL
         CHI   R5,2
         JL    A0121
         LA    R9,SQLTEXT
A0120    EQU   *
         CLC   0(L'PARALLEL,R9),PARALLEL
         JE    A0122
         LA    R9,1(,R9)
         BRCT  R5,A0120
*
A0121    EQU   *                       Parallelism not found
         MVC   WKPARALL,=H'1'
         J     A0129
A0121A   EQU   *
         GVBMSG WTO,MSGNO=DB2_HPU_PARA,SUBNO=1,                        +
               SUB1=(PGMNAME,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         MVC   WKPARALL,=H'1'
         J     A0129
*
A0122    EQU   *                       Parallelism found
         LA    R9,L'PARALLEL(,R9)
         LR    R0,R9
A0123    EQU   *
         CLI   0(R9),C'0'
         JL    A0124
         CLI   0(R9),C'9'
         JH    A0124
         LA    R9,1(,R9)
         J     A0123
A0124    EQU   *
         SR    R9,R0                   Length of digits
         LTR   R9,R9
         JNP   A0121A                  invalid, go
         BCTR  R9,0
*
         LR    R1,R0                   First digit of number
         LR    R2,R9                   Number length - 1 =L2
         OY    R2,=Xl4'00000070'       Set L1 in pack's L1L2
         EXRL  R2,EXEPACK
         CVB   R0,WKDBL1
         STH   R0,WKPARALL
         J     A0129                   subtasks will be executing
EXEPACK  PACK  WKDBL1(0),0(0,R1)
*
* obtain EXEXU table and 16 byte header
*
A0129    EQU   *
         LA    R0,EXUEXUL        element size in table
         LLGTR R0,R0
         MH    R0,WKPARALL       times number elements needed
         AHI   R0,16
         LY    R1,DBLWORK2
         AR    R1,R0 
         STY   R1,DBLWORK2
         ST    R0,WKEXULEN
*
         GETMAIN RU,LV=(0),LOC=(ANY)
         USING EXHEXH,R1
         MVC   EXHEYE,=CL8'EXUEXU'
         MVC   EXHPARLL,WKPARALL  insert number entries in header
         XC    10(6,R1),10(R1)
         DROP  R1
         AHI   R1,16
         ST    R1,WKEXUADR       connect MRAD thread area to 1st EXUEXU
         LR    R4,R1             table of DB2 HPU sub threads
*
         LR    R0,R1             Clear storage
         LA    R1,EXUEXUL        element size in table
         LLGTR R1,R1
         MH    R1,WKPARALL       times number elements needed
         XR    R14,R14
         XR    R15,R15
         MVCL  R0,R14
*
* obtain ECBLIST array
*
         LA    R0,4              length of ECB address
         LLGTR R0,R0
         MH    R0,WKPARALL       times number elements needed
         AHI   R0,4              plus subtask ECB (DB2 HPU itself)
         LY    R1,DBLWORK2
         AR    R1,R0 
         STY   R1,DBLWORK2
         ST    R0,WKECBLEN
*
         GETMAIN RU,LV=(0),LOC=(ANY)
         ST    R1,WKECBLST       connect MRAD thread area to ECBLIST
*
         LR    R0,R1             Clear storage
         LA    R1,4              element size in list
         LLGTR R1,R1
         MH    R1,WKPARALL       times number elements needed
         XR    R14,R14
         XR    R15,R15
         MVCL  R0,R14
*
* communicate EXUEXU table to DB2 HPU sub tasks
*
         MVC   WKTOKNAM+0(8),GENEVA
         MVC   WKTOKNAM+8(8),PGMNAME
         ST    R4,WKTOKN          share address of MR95 EXUEXU area
*                                 with DB2 HPU exit sub threads
*
         CALL  IEANTCR,(TOKNLVL2,WKTOKNAM,WKTOKN,TOKNPERS,WKTOKNRC),   +
               MF=(E,WKREENT)
         L     R15,WKTOKNRC       SUCCESSFUL   ???
         LTR   R15,R15
         JZ    A0128
         GVBMSG LOG,MSGNO=DB2_HPU_TOKN,SUBNO=1,                        +
               SUB1=(PGMNAME,8),                                       +
               GENENV=GENENV,                                          +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         la    r15,8
         j     returne
*
* obtain blocks for records returned by EXUEXU subtasks and
*   initialize EXUEXU entries
*
A0128    EQU   *
         LLGT  R7,WKECBLST          => List of ECB addresses
         USING EXUEXU,R4
         LGH   R9,WKPARALL
A0130    EQU   *
         LLGT  R0,=A(BLOCKSZ)       "Block" size for DB2 rows
         LY    R1,DBLWORK2
         AR    R1,R0 
         STY   R1,DBLWORK2
         GETMAIN RU,LV=(0),LOC=(ANY)
         ST    R1,EXUBLKA           address of block
         ST    R1,EXURPOS           address of (1st) record
         AHI   R1,BLOCKSZ-1         last byte address
         ST    R1,EXURLAST
         LA    R0,EXUECBMA          address of wait ECB (main)
         ST    R0,0(,R7)
         MVI   EXUEOF,C' '
         MVI   EXUWAIT,C' '
         MVI   EXUSTAT,C' '
         MVI   EXUFINAL,C' '
         LA    R4,EXUEXUL(,R4)      => next EXUEXU entry
         LA    R7,4(,R7)            => next address in ECB list
         BRCT  R9,A0130
         LA    R0,WKECBSUB          subtask ended ECB
         ST    R0,0(,R7)
         OI    0(R7),X'80'          indicate last word in list
         DROP  R4 EXUEXU
*
         XC    WKECBSUB,WKECBSUB
         XC    WKTCBSUB,WKTCBSUB
*
**********************************************************************
* Start DB2 HPU utility                                              *
**********************************************************************
         LARL  R1,SUBTASK
         IDENTIFY EP=GVBMRSB,ENTRY=(1)
*
         MVC   WKPARM(14),=CL14'XXXX,DB2UNLOAD'
         MVC   WKPARM(04),DBSUBSYS
*
         basr  r9,0
         USING *,R9
         LA    R7,WKECBSUB
         ATTACH EP=GVBMRSB,         entry point of subtask             +
               ECB=(7),SZERO=YES        ECB (located in THRDAREA)
         ST    R1,WKTCBSUB
         DROP  R9
*
**********************************************************************
* DISPLAY SQL IN MR95 TRACE FILE IF OPEN                             *
**********************************************************************
         ENQ   (GENEVA,TRACNAME,E,,STEP),RNL=NO
*
         L     R2,TRACDCBA             LOAD  DCB ADDRESS
         TM    48(R2),X'10'            OPEN  SUCCESSFUL   ???
         BRNO  TRACDEQ                 NO  - BYPASS SAVING OF PUT ADDR
*
         MVI   PRNTLINE+0,C'*'
         MVC   PRNTLINE+1(L'PRNTLINE-1),PRNTLINE+0
         MVI   PRNTCC,C'1'             START NEW   PAGE
*
         MVI   PRNTCNT+0,C' '
         MVC   PRNTCNT+1(L'DBSUBSYS),DBSUBSYS
         MVI   PRNTCNT+1+L'DBSUBSYS,C' '
*
         MVI   PRNTFILE+0,C' '
         MVC   PRNTFILE+1(8),GPDDNAME
         MVI   PRNTFILE+1+8,C' '
*
         LA    R0,PRNTLINE             PRINT TRACE LINE
         LR    R1,R2
         XR    R15,R15
         ICM   R15,B'0111',DCBPUTA-IHADCB(R1)
         BASR  R14,R15
*
         LA    R3,SQLTEXT              PRINT SQL   TEXT
*
TRACLOOP LA    R15,SQLTEXT
         AH    R15,SQLBUFFR
         SR    R15,R3
         BRNP  TRACDONE
*
         CHI   R15,80
         BRNH  *+8
         LHI   R15,80
*
         BCTR  R15,0
         MVC   PRNTLINE,SPACES
         EXRL  R15,TRACMVC
         LA    R3,1(R3,R15)
*
         LA    R0,PRNTLINE             PRINT TRACE LINE
         LR    R1,R2
         XR    R15,R15
         ICM   R15,B'0111',DCBPUTA-IHADCB(R1)
         BASR  R14,R15
*
         BRC   15,TRACLOOP
*
TRACDONE MVI   PRNTLINE+0,C'*'
         MVC   PRNTLINE+1(L'PRNTLINE-1),PRNTLINE+0
         MVI   PRNTCC,C' '
*
         LA    R0,PRNTLINE             PRINT TRACE LINE
         LR    R1,R2
         XR    R15,R15
         ICM   R15,B'0111',DCBPUTA-IHADCB(R1)
         BASR  R14,R15
*
TRACDEQ  DEQ   (GENEVA,TRACNAME,,STEP),RNL=NO
         J     A0100
*
TRACMVC  MVC   PRNTLINE+1(0),0(R3)     * * * * E X E C U T E D * * *
*
* Accumulate read buffer totals across threads
*
A0100    EQU   *
         ENQ (GENEVA,ENQSTAT,E,,STEP),RNL=NO
         if LTR,R15,R15,nz
*          Issue a warning message
           GVBMSG LOG,MSGNO=STATS_ENQ_FAIL,SUBNO=1,GENENV=GENENV,      +
               SUB1=(modname,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         endif
*
d1       using thrdarea,r14
         ly    r14,thrdmain          Get the initial thread area
         ly    r1,d1.read_buffer_tot Get size of read buffers
         ay    r1,dblwork2           Add size of buffers from above
         sty   r1,d1.read_buffer_tot
*
* Increase buffer high water mark stats
*
         if cy,r1,gt,d1.read_buffer_hwm If current total > HWM
           sty   r1,d1.read_buffer_hwm  save new HWM
         endif
         drop  d1
*
         DEQ (GENEVA,ENQSTAT,,STEP),RNL=NO
*
         LAY   R15,DB2FETCH1      LOAD FETCH ROUTINE ADDRESS
         BASR  R14,R15            WAIT  FOR COMPLETION OF FIRST READ
         LLGTR R6,R6              Set top half to zero as it's 64 bit
         ST    R6,SAVESUBR+RSA6   PASS BACK RECORD ADDRESS
*
*
RETURN   DS    0H
         XR    R15,R15            SET  RETURN CODE  TO ZERO
*
RETURNE  DS    0H
         L     R14,SAVESUBR+RSA14     RESTORE REGISTER  R14
         LM    R0,R12,SAVESUBR+RSA0   RESTORE REGISTERS R0 - R12
         BSM   0,R14                  RETURN
*
*
RTNERROR DS    0H
         LHI   R15,12
         J     RETURNE
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* 1.  READ first block of records                                     *
* 2.  read next block of records until END-OF-FILE (EOF)              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING THRDAREA,R13
         using genparm,parm_area
         using genfile,file_area
         using genenv,env_area
*
DB2FETCH1 DS    0H
         STMG  R14,R12,SAVESUB3
         LR    R9,R14              SAVE RETURN ADDRESS
         LARL  R10,GVBMRAD         set static area base
         LLGT  R4,WKEXUADR         get first EXUEXU entry
         J     A0020               First time, straight to continue
*
DB2FETCH DS    0H
         STMG  R14,R12,SAVESUB3
         LR    R9,R14              SAVE RETURN ADDRESS
         LARL  R10,GVBMRAD         set static area base
         LLGT  R8,SQLWADDR         Needed if it's not 1st time through
*
* Can only post DB2 HPU exit on subsequent call allowing MR95 chance
* to process the previous block we returned
*
A0010    EQU   *
         LLGT  R4,WKEXUCUR        EXU block last returned to MR95
         USING EXUEXU,R4
*
         CLI   EXUFINAL,C'Y'      Was it final data block from an exit?
         JE    A0014              Yes, go
*
* Just another block of data
         CLI   EXUWAIT,C'Y'       This exit instance waiting for us ?
         JE    A0012              Yes, good
         DC    H'0'              No, must be otherwise wouldn't be here
A0012    EQU   *
         CLI   EXUSTAT,C'3'       And is it in the correct state ?
         JE    A0013              Yes, good
         DC    H'0'              No, must be otherwise wouldn't be here
A0013    EQU   *
         XC    EXUECBMA,EXUECBMA  reset ECB -----
         MVI   EXUSTAT,C' '       reset status
         POST  EXUECBEX           allow INZEXIT instance to continue
         J     A0020              Continue..
*
* Determine if final data block from the final INZEXIT instance....
A0014    EQU   *
         ENQ (GENEVA,MRADNAME,E,,STEP),RNL=NO
         LLGT  R1,WKEXUADR
         AHI   R1,-16             BACK UP TO EXUEXU HEADER
         USING EXHEXH,R1
         CLC   EXHINIT,EXHFINI    INITIALIZATIONS <= TERMINATIONS ?
         JNH   A0015              YES: we're on the final instance
         DROP  R1
*
*                                 NO: not the final INZEXIT instance
         DEQ (GENEVA,MRADNAME,,STEP),RNL=NO
         XC    EXUECBMA,EXUECBMA  reset ECB -----
         MVI   EXUSTAT,C' '       reset status
         J     A0020              NO: continue..
*
A0015    EQU   *                  Final EXU (partition) !!!
         DEQ (GENEVA,MRADNAME,,STEP),RNL=NO
         J     EVNTEOF            Go: it's last data from last instance
*
* Wait for one of the DB2 HPU related exits to post us
*
A0020    EQU   *
         LLGT  R1,WKECBLST
         WAIT  1,ECBLIST=(1)      the exit has returned a block of
*
* Find out which DB2 HPU related EXUEXU entry posted us
*
         LGH   R15,WKPARALL
         LLGT  R14,WKECBLST
         LLGT  R4,WKEXUADR        get first EXUEXU entry
A0022    EQU   *
         LLGT  R1,0(,R14)         => ECB from list
         TM    0(R1),X'40'
         JO    A0023              posted
         LA    R14,4(,R14)        next EXUEXU ECB list entry
         LA    R4,EXUEXUL(,R4)    next EXUEXU entry
         BRCT  R15,A0022
*
         TM    WKECBSUB,X'40'     posted by unexpected DB2 HPU term ?
         JO    A0022A             Yes, go
         DC    H'0'               One of the ECB's must have posted us!
*
A0022A   EQU   *
         LLGT  R15,WKSUBERR        Subtask (DB2HPU) had an error
         CVD   R15,DBLWORK         STORE THE RETURN CODE (PACKED)
         MVC   WORKMSG(8),SPACES
         UNPK  WORKMSG(4),DBLWORK+4(4)
         OI    WORKMSG+3,X'F0'     FORCE A DISPLAYABLE ZONE
*
         GVBMSG WTO,MSGNO=DB2_HPU_FAIL,SUBNO=2,                        +
               SUB1=(PGMNAME,8),                                       +
               SUB2=(WORKMSG,4),  return code                          +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         J     A0202
*
A0023    EQU   *
*                                 records or reached eof
         CLI   EXUSTAT,C'2'       block populated status ?
         JE    A0024              yes, that's correct
         DC    H'0'               no, cannot have posted us then !!!
*
A0024    EQU   *
         ASI   EXUCNT1,1         Count times through
         ST    R4,WKEXUCUR       This is the EXU being passed to MR95
         MVI   EXUSTAT,C'3'      Now passing data to MR95
*
         CLI   EXUEOF,C'Y'
         JNE   FETCHOK            CONTINUE
         MVI   EXUFINAL,C'Y'
         J     FETCHOK            CONTINUE
*
*
A0202    EQU   *                  R15 contains return error
         DETACH WKTCBSUB
         XGR   R6,R6              Indicate NO Record
         STG   R6,RECADDR         Save the Record address
*
         L     R2,EVNTDCBA        LOAD SYNAD EXIT ADDRESS
         LT    R2,DCBDCBE-IHADCB(R2) --> DCBE
         JZ    A0203
         USING DCBE,R2
         L     R14,DCBESYNA       LOAD I/O error routine addr
         drop  r2
         LTR   R14,R14            EXIT  ADDRESS  AVAILABLE   ???
         BNZR  R14                YES - USE GVBMR95 EXIT   ADDRESS
A0203    EQU   *
         BR    R9                 NO  - USE GVBMRAD RETURN ADDRESS
*
*
FETCHOK  DS    0H
         LLGT  R6,EXUBLKA         Current record at start of block
         STG   R6,RECADDR         Store address of record
         LGF   R0,EXUROWLN
         ST    R0,ROWLEN          Store length of record
         ST    R0,GPRECLEN        used by MR95
         ST    R0,GPRECMAX
*
         LGF   R0,EXUROWLN
         MS    R0,EXURNUM
         ST    R0,GPBLKMAX
         AGR   R0,R6
         STG   R0,EODADDR         used by MR95
*
* We can't post DB2 HPU exit here: MR95 hasn't processed the block yet
* Post it when we come back in to DB2FETCH, if not EOF (FINAL)
*
         STG   R6,SAVESUB3+64
         LMG   R14,R12,SAVESUB3
         BSM   0,R14
         DROP  R4 EXUEXU
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* 1.  PERFORM END-OF-JOB PROCESSING AT END-OF-FILE (EOF)              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
EVNTEOF  EQU   *
*
**********************************************************************
* DB2HPU has reached eof for the table or for a partition in table   *
**********************************************************************
*
         XGR   R6,R6         Indicate NO Records and end of partitions
         STG   R6,RECADDR         Save the Record address
*
         WAIT  1,ECB=WKECBSUB     subtask has finished altogether
*
         DETACH WKTCBSUB
*
* Free storages
*
         USING EXUEXU,R4
         LLGT  R4,WKEXUADR          get first EXUEXU entry
         LGH   R9,WKPARALL          Free I/O blocks
EOF130   EQU   *
         LLGT  R0,=A(BLOCKSZ)       "Block" size for DB2 rows
         LLGT  R1,EXUBLKA           address of block
         FREEMAIN RU,LV=(0),A=(1)
         LA    R4,EXUEXUL(,R4)      => next EXUEXU entry
         BRCT  R9,EOF130
*
         LLGT  R0,WKECBLEN          Free ECB list
         LLGT  R1,WKECBLST
         FREEMAIN RU,LV=(0),A=(1)
*
         LLGT  R0,WKEXULEN          Free EXU table including header
         LLGT  R1,WKEXUADR
         AGHI  R1,-16
         FREEMAIN RU,LV=(0),A=(1)
*
         Lhi   R0,F10K              Free buffer for  EXPANDED SQL TEXT
         LLGT  R1,SQLTADDR
         FREEMAIN RU,LV=(0),A=(1)
         XC    SQLTADDR,SQLTADDR
*
* Serialise to adjust buffer total
*
         ENQ   (GENEVA,ENQSTAT,E,,STEP),RNL=NO
         if LTR,R15,R15,nz
*          'GVBMRSQ - Buffer stats "ENQ" FAILED'
*          this just means that the stats will not be correct
*          Issue a warning message
           GVBMSG LOG,MSGNO=STATS_ENQ_FAIL,SUBNO=1,GENENV=GENENV,      +
               SUB1=(modname,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         endif
*
d1       using thrdarea,r14
         ly    r14,thrdmain          Get the initial thread area
         ly    r1,d1.read_buffer_tot Get total size of buffers used
         sy    r1,dblwork2           subtract freemained value
         sty   r1,d1.read_buffer_tot
         drop  d1
*
         DEQ (GENEVA,ENQSTAT,,STEP),RNL=NO
*
         LLGT  R0,STGBLKLN           Free main workarea
         LLGT  R1,SQLWADDR
         AHI   R1,-L'PGMNAME
         FREEMAIN RU,LV=(0),A=(1)
         XC    SQLWADDR,SQLWADDR     And zero anchor
*
         L     R2,EVNTDCBA        LOAD END-OF-FILE EXIT ADDRESS
         using IHADCB,r2
         lt    R14,dcbdcbe        is there a DCBE?
         jz    parsv_dcb
         Using dcbe,r14
         lt    r14,dcbeeoda       is there  a DCBE EODAD routine?
         bnzr  r14
         drop  r14
parsv_dcb ds   0h
         sr    r14,r14
         ICM   R14,B'0111',DCBEODA
         drop  r2
         LTR   R14,R14            EXIT  ADDRESS  AVAILABLE   ???
         BPR   R14                YES - USE "GVBMR95" EOF    ADDRESS
         BR    R9                 NO  - USE "GVBMR95" CALL   ADDRESS
*
         DROP  R13
*
**********************************************************************
* Subtask that starts DB2HPU and wait for it to finish               *
**********************************************************************
*
SUBTASK  ds    0h
         bakr  r14,0
         larl  r10,GVBMRAD        set static area base
*
         LA    R0,WKPARM1+2
         O     R0,=X'80000000'
         ST    R0,WKPARM0
         MVC   WKPARM1+2(2),=H'14'
         LA    R1,WKPARM0
         LINK  EP=INZUTILB
*
         ltr   r15,r15
         jz    subtask2
         chi   r15,4
         je    subtask3
         st    R15,WKSUBERR
         j     subtask2
subtask3 equ   *                  rc=4: all records passed to MR95
*
subtask2 equ   *
         pr    r14
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
static   loctr
F10K     equ   10240
BLOCKSZ  equ   8192
*
F4       DC    F'04'
F8       DC    F'08'
*
MODE31   DC    XL4'80000000'
*
STGBLKLN DC    A(WKLEN+8)
*
GVBUR33  DC    V(GVBUR33)
DSNTIAR  DC    V(DSNTIAR)
*
H750     DC    H'750'
FFFF     DC    XL4'FFFFFFFF'
*
*        DATA TYPES FOUND IN SQLTYPE, AFTER REMOVING THE NULL BIT
*
VARCTYPE DC    H'448'              NOTNULL VARCHAR TYPE
CHARTYPE DC    H'452'              NOTNULL FIXED CHAR TYPE
VARLTYPE DC    H'456'              NOTNULL LONG VARCHAR TYPE
VARGTYPE DC    H'464'              NOTNULL VARGRAPHIC TYPE
GTYPE    DC    H'468'              NOTNULL GRAPHIC TYPE
LVARGTYP DC    H'472'              NOTNULL LONG VARGRAPHIC TYPE
FLOATYPE DC    H'480'              NOTNULL FLOAT TYPE
DECTYPE  DC    H'484'              NOTNULL DECIMAL TYPE
INTTYPE  DC    H'496'              NOTNULL INTEGER TYPE
HWTYPE   DC    H'500'              NOTNULL SMALLINT TYPE
DATETYP  DC    H'384'              NOTNULL DATE TYPE
TIMETYP  DC    H'388'              NOTNULL TIME TYPE
TIMES    DC    H'392'              NOTNULL TIMESTAMP TYPE
                        SPACE 3
HEXTR    DS    XL256'00'
         ORG   HEXTR+240
         DC    C'0123456789ABCDEF'
         ORG   ,
*
ENQSTAT  DC    CL8'ENQSTAT '      MINOR  ENQ NODE  (Stats)
TRACNAME DC    CL8'MR95TRAC'      MINOR  ENQ NODE  (MR95 TRACE FILE)
LOGNAME  DC    CL8'MR95LOG '      MINOR  ENQ NODE  (MR95 LOG FILE)
ZEROES   DC   8CL01'0'
REFTBLID DC    CL03'#DD'
*
TOKNLVL2 DC    A(2)               NAME/TOKEN  AVAILABILITY  TASK LEVEL
GENEVA   DC    CL8'GENEVA  '           TOKEN  and MAJ ENQ NAME
PGMNAME  DC    CL8'GVBMRAD '
MRADNAME DC    CL8'MRADEXA '          MINOR  ENQ  NODE FOR WRITE I/O
TOKNPERS DC    F'0'
IEANTCR  DC    V(IEANTCR)
IEANTRT  DC    V(IEANTRT)
*
SYSFILE  DCB   DSORG=PS,DDNAME=SYSIN,MACRF=(PM),DCBE=SYSDCBE,          X
               RECFM=FB,LRECL=80
SYSEOFF  EQU   *-SYSFILE
SYSDCBE  DCBE  RMODE31=BUFF
SYSFILEL EQU   *-SYSFILE
*
STATOPEN OPEN  (,OUTPUT),MODE=31,MF=L
STATOPENL EQU  *-STATOPEN
*
STATCLOS CLOSE (),MODE=31,MF=L
STATCLOSL EQU  *-STATCLOS
*
PARALLEL DC    CL12'PARALLELISM '
         LTORG ,
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D A T A   C O N T R O L   B L O C K S                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         DCBD  DSORG=PS
         ihadcbe
*
         END
