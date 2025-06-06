         TITLE 'GVBMRAD - ADABAS I/O INITIALIZATION FOR GVBMR95'
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
*            ADABAS I/O -- initialization (invoked by GVBMR96)        *
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
*        R10 - PROGRAM BASE                                           *
*        R9  - INTERNAL  SUBROUTINE  RETURN ADDRESS                   *
*                                                                     *
*        R8  - WKAREA Dsect base address                              *
*                                                                     *
*        R7  - SQL DESCRIPTOR AREA ADDRESS ("SQLDA")                  *
*            - SQL TEXT       AREA ADDRESS ("SQLTAREA")               *
*        This is used to obtain the Adabas access information needed  *
*        contained in vdp0200b record field vdp0200b_DBMS_SQL, e.g.   *
*        DBID=300|FNR=47|SB=AA.|FB=AA,AB,AC,AD,AE,AF,AG.|LREC=96      *
*        or enclosed within the <DBMSSQL> tag in the VDP XML          *
*                                                                     *
*        R6  -                                                        *
*        R5  -                                                        *
*        R4  - WORK                                                   *
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
         YREGS
*
RSA14    EQU   12
RSA15    EQU   16
RSA0     EQU   20
RSA1     EQU   24
RSA6     EQU   44
*
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
static   loctr                    define the static section
MRAUEYE  GVBEYE GVBMRAD
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
         XC    workmsg(16),workmsg zero translated error msg area
*
         LAY   R0,ADAFETCH        INITIALIZE READ ROUTINE ADDRESS
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
         GETMAIN RU,LV=(0),LOC=(31)
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
*   multifetch buffer size
*
         LH    R0,HNREC
         MH    R0,HMISN
         ST    R0,FMBSIZE
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
* COPY ADABAS related parameters FROM V4 "VDP" INTO EXPANSION BUFFER *
**********************************************************************
         Lhi   R0,F10K              GET BUFFER FOR EXPANDED SQL TEXT
         lr    r15,r0
         ay    r15,dblwork2        add to storage total
         sty   r15,dblwork2        and save it
*
         GETMAIN RU,LV=(0),LOC=(ANY)
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
**********************************************************************
* DISPLAY ADABAS parameters IN MR95 TRACE FILE IF OPEN               *
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
static   loctr ,
TRACMVC  MVC   PRNTLINE+1(0),0(R3)     * * * * E X E C U T E D * * *
code     loctr
*
***********************************************************************
*  PARSE ADABAS RELATED PARAMETERS                                    *
***********************************************************************
*
A0100    EQU   *
         LA    R9,SQLTEXT         first sub parameter
         ST    R9,WKSUBPA1
         LH    R1,SQLBUFFR
         LR    R15,R9
         LGHI  R5,1
A000102  EQU   *
         CLI   0(R9),C'|'
         JE    A000103
         LA    R9,1(,R9)
         BRCT  R1,A000102
A000103  EQU   *
         LR    R0,R9
         SR    R0,R15
         ST    R0,WKSUBPL1        length of first parameter
         CLI   0(R9),C'|'         was there a delimiter ?
         JNE   A00011             no, go
         LA    R9,1(,R9)          account for 1st delimiter
         AGHI  R1,-1
*
         AGHI  R5,1
         ST    R9,WKSUBPA2
         LR    R15,R9     
A000104  EQU   *
         CLI   0(R9),C'|'
         JE    A000105
         LA    R9,1(,R9)
         BRCT  R1,A000104
A000105  EQU   *
         LR    R0,R9
         SR    R0,R15
         ST    R0,WKSUBPL2        length of second parameter
         CLI   0(R9),C'|'         was there a delimiter ?
         JNE   A00011             no, go
         LA    R9,1(,R9)          account for delimiter
         AGHI  R1,-1
*
         AGHI  R5,1
         ST    R9,WKSUBPA3
         LR    R15,R9     
A000106  EQU   *
         CLI   0(R9),C'|'
         JE    A000107
         LA    R9,1(,R9)
         BRCT  R1,A000106
A000107  EQU   *
         LR    R0,R9
         SR    R0,R15
         ST    R0,WKSUBPL3        length of third parameter
         CLI   0(R9),C'|'         was there a delimiter ?
         JNE   A00011             no, go
         LA    R9,1(,R9)          account for delimiter
         AGHI  R1,-1
*
         AGHI  R5,1
         ST    R9,WKSUBPA4
         LR    R15,R9     
A000108  EQU   *
         CLI   0(R9),C'|'
         JE    A000109
         LA    R9,1(,R9)
         BRCT  R1,A000108
A000109  EQU   *
         LR    R0,R9
         SR    R0,R15
         ST    R0,WKSUBPL4        length of fourth parameter
         CLI   0(R9),C'|'         was there a delimiter ?
         JNE   A00011             no, go
         LA    R9,1(,R9)          account for delimiter
         AGHI  R1,-1
*
         AGHI  R5,1
         ST    R9,WKSUBPA5
         LR    R15,R9     
A000110  EQU   *
         CLI   0(R9),C'|'
         JE    A000111
         LA    R9,1(,R9)
         BRCT  R1,A000110
A000111  EQU   *
         LR    R0,R9
         SR    R0,R15
         ST    R0,WKSUBPL5        length of fifth parameter
A00011   EQU   *
         DROP  R7 SQLTAREA
*
         LA    R6,WKSUBPA1
         LA    R7,WKSUBPL1
A0010    EQU   *
         L     R1,0(,R6)
         L     R2,0(,R7)
         CLC   0(3,R1),=CL3'SB='
         JNE   A0011
         JAS   R14,SUBSB
         J     A0015
A0011    EQU   *
         CLC   0(3,R1),=CL3'FB='
         JNE   A0012
         JAS   R14,SUBFB
         J     A0015
A0012    EQU   *
         CLC   0(5,R1),=CL5'DBID='
         JNE   A0013
         JAS   R14,SUBDBID
         J     A0015
A0013    EQU   *
         CLC   0(4,R1),=CL4'FNR='
         JNE   A0014
         JAS   R14,SUBFNR
         J     A0015
A0014    EQU   *
         CLC   0(5,R1),=CL5'LREC='
         JNE   A0015
         JAS   R14,SUBLREC
A0015    EQU   *
         LA    R6,4(,R6)
         LA    R7,4(,R7)
         BRCT  R5,A0010
*
*   validations
*
         CLC   FSBL,=A(3)
         JL    ERRSB
         CLC   FSBL,=A(8)
         JH    ERRSB
         CLC   FFBL,=A(3)
         JL    ERRFB
         CLC   FFBL,=A(256)
         JH    ERRFB
         CLC   FDBID,=A(1)
         JL    ERRDBID
         CLC   FDBID,=A(65535)
         JH    ERRDBID
         CLC   FFNR,=A(1)
         JL    ERRFNR
         CLC   FFNR,=A(5000)
         JH    ERRFNR
         CLC   HLREC,=Y(1)
         JL    ERRLREC
         CLC   HLREC,=Y(4096)
         JH    ERRLREC
         J     A0016
*
ERRSB    EQU   *
         GVBMSG LOG,MSGNO=ADABAS_SBL,SUBNO=1,                          +
               GENENV=GENENV,                                          +
               SUB1=(PGMNAME,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         LHI   R15,12
         J     RETURNE
ERRFB    EQU   *
         GVBMSG LOG,MSGNO=ADABAS_FBL,SUBNO=1,                          +
               GENENV=GENENV,                                          +
               SUB1=(PGMNAME,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         LHI   R15,12
         J     RETURNE
ERRDBID  EQU   *
         GVBMSG LOG,MSGNO=ADABAS_DBID,SUBNO=1,                         +
               GENENV=GENENV,                                          +
               SUB1=(PGMNAME,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         LHI   R15,12
         J     RETURNE
ERRFNR   EQU   *
         GVBMSG LOG,MSGNO=ADABAS_FNR,SUBNO=1,                          +
               GENENV=GENENV,                                          +
               SUB1=(PGMNAME,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         LHI   R15,12
         J     RETURNE
ERRLREC  EQU   *
         GVBMSG LOG,MSGNO=ADABAS_LREC,SUBNO=1,                         +
               GENENV=GENENV,                                          +
               SUB1=(PGMNAME,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         LHI   R15,12
         J     RETURNE
*
*   obtain record buffer
*
A0016    EQU   *
         LH    R0,HNREC          Recored buffer
         MH    R0,HLREC
         AGHI  R0,RBDXQLL        Plus fixed header
         ST    R0,FRBSIZE
         LY    R1,DBLWORK2
         AR    R1,R0 
         STY   R1,DBLWORK2
         GETMAIN RU,LV=(0),LOC=(31)
         ST    R1,WKARB
         lr    R0,R1              ZERO  WORK  AREA
         LGF   R1,FRBSIZE
         xr    R14,R14
         xr    R15,R15
         MVCL  R0,R14
***********************************************************************
*  OPEN INPUT FILE (ADABAS)                                           *
***********************************************************************
*
         USING ADACBX,CB
         USING FBBDX,FB
         USING RBBDX,R5
         USING SBBDX,SB
         USING VBBDX,VB
         USING IBBDX,IB
         USING MBBDX,MB
*
         LLGT  R5,WKARB
*
         LA    R0,CB
         ST    R0,ADAPAL+00
         LA    R0,APLXLINK
         ST    R0,ADAPAL+04
         LA    R0,LINKWORK
         ST    R0,ADAPAL+08
         LA    R0,FB
         ST    R0,ADAPAL+12
         ST    R5,ADAPAL+16
         LA    R0,SB
         ST    R0,ADAPAL+20
         LA    R0,VB
         ST    R0,ADAPAL+24
         LA    R0,IB
         ST    R0,ADAPAL+28
         LA    R0,MB
         ST    R0,ADAPAL+32
         OI    ADAPAL+32,X'80'
*
*
         LOAD  EPLOC=LINKNAME,ERRET=A0010E
         O     R0,MODE31
         ST    R0,WKAADA
         J     A0011O
A0010E   EQU   *
         GVBMSG LOG,MSGNO=ADA_NOLINK,SUBNO=1,                          +
               GENENV=GENENV,                                          +
               SUB1=(PGMNAME,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         LHI   R15,12
         J     RETURNE
*
*
A0011O   EQU   *
         MVI   ACBXVERT,ACBXVERE
         MVI   ACBXVERN,ACBXVERC
         MVC   ACBXLEN,=Y(ACBXQLL)
         MVC   ACBXDBID,FDBID
         MVC   ACBXFNR,FFNR
*
         MVI   WKCID1,C'F'
         MVC   WKCID23,=X'0001'
         MVI   WKCID4,C'S'
*
         MVI   FBDXLOC,C' '
         MVC   FBDXLEN,=Y(FBDXQLL)
         MVI   FBDXVERT,ABDXVERE
         MVI   FBDXVERN,ABDXVERC
         MVI   FBDXID,ABDXQFB
         MVC   FBDXSIZE+4(4),=F'256'
*
         MVI   RBDXLOC,C' '
         MVC   RBDXLEN,=Y(RBDXQLL)
         MVI   RBDXVERT,ABDXVERE
         MVI   RBDXVERN,ABDXVERC
         MVI   RBDXID,ABDXQRB
         LGF   R0,FRBSIZE
         STG   R0,RBDXSIZE
*
         MVI   MBDXLOC,C' '
         MVC   MBDXLEN,=Y(MBDXQLL)
         MVI   MBDXVERT,ABDXVERE
         MVI   MBDXVERN,ABDXVERC
         MVI   MBDXID,ABDXQMB
         LGF   R0,FMBSIZE
         AGHI  R0,4
         STG   R0,MBDXSIZE        QUANTITY THEN MISN AREA
*
         MVI   SBDXLOC,C' '
         MVC   SBDXLEN,=Y(SBDXQLL)
         MVI   SBDXVERT,ABDXVERE
         MVI   SBDXVERN,ABDXVERC
         MVI   SBDXID,ABDXQSB
         MVC   SBDXSIZE+4(4),=F'8'
*
         MVI   VBDXLOC,C' '
         MVC   VBDXLEN,=Y(VBDXQLL)
         MVI   VBDXVERT,ABDXVERE
         MVI   VBDXVERN,ABDXVERC
         MVI   VBDXID,ABDXQVB
         MVC   VBDXSIZE+4(4),=F'50'
*
         MVI   IBDXLOC,C' '
         MVC   IBDXLEN,=Y(IBDXQLL)
         MVI   IBDXVERT,ABDXVERE
         MVI   IBDXVERN,ABDXVERC
         MVI   IBDXID,ABDXQIB
         MVC   IBDXSIZE+4(4),=A(NREC)
*
*
         MVC   ACBXCMD,=CL2'OP'
         MVC   ACBXCID,WKCID
         MVC   ACBXADD3,SPACES
         MVC   ACBXADD4,SPACES
         MVC   ACBXADD5,SPACES
         MVC   RBDXDATA(16),WKOPRB
         MVC   RBDXSEND+4(4),FOPRBL
         LA    R1,ADAPAL
         L     R15,WKAADA
         LR    R11,R13
         LAY   R13,WKSUBADA
         BASR  R14,R15
         LR    R13,R11
*
         CLC   ACBXRSP,=H'0'
         JE    A0012O
         MVC   WORKMSG(2),ACBXCMD
         LH    R15,ACBXRSP
         CVD   R15,WKDBLWK
         MVC   WORKMSG+2(4),NUMMSK+8
         MVI   WORKMSG+2,C' '
         ED    WORKMSG+2(4),WKDBLWK+6
         LH    R15,ACBXERRC
         CVD   R15,WKDBLWK
         MVC   WORKMSG+6(4),NUMMSK+8
         MVI   WORKMSG+6,C' '
         ED    WORKMSG+6(4),WKDBLWK+6
         GVBMSG LOG,MSGNO=ADA_BADRSP,SUBNO=4,                          +
               GENENV=GENENV,                                          +
               SUB1=(PGMNAME,8),                                       +
               SUB2=(WORKMSG,2),                                       +
               SUB3=(WORKMSG+2,4),                                     +
               SUB4=(WORKMSG+6,4),                                     +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         LHI   R15,12
         J     RETURNE
*
A0012O   EQU   *                       Set for first L3 read command
         MVC   ACBXCMD,=CL2'L3'
         MVI   ACBXCOP1,C'M'
         MVI   ACBXCOP2,C'V'
         MVC   ACBXADD1,=CL8'        '
         MVC   ACBXADD1(2),WKL3SB
         XC    ACBXISN,ACBXISN
         XC    ACBXISL,ACBXISL
         XC    ACBXISQ,ACBXISQ
*
         MVC   FBDXDATA(256),WKL3FB
         MVC   FBDXSEND+4(4),FFBL
*
         XC    RBDXSEND,RBDXSEND
         MVC   RBDXRECV+4(4),FRBSIZE
*
         XC    MBDXSEND,MBDXSEND
         MVC   MBDXRECV+4(4),FMBSIZE
*
         MVC   SBDXDATA(08),WKL3SB
         MVC   SBDXSEND+4(4),FSBL
*
         XC    VBDXDATA(10),VBDXDATA
         MVC   VBDXSEND+4(4),=F'10'
*
* Accumulate read storage totals across threads
*
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
         LAY   R15,ADAFETCH       LOAD FETCH ROUTINE ADDRESS
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
ADAFETCH DS    0H
         STMG  R14,R12,SAVESUB3
         LR    R9,R14              SAVE RETURN ADDRESS
         LARL  R10,GVBMRAD         set static area base
         LLGT  R8,SQLWADDR         Needed if it's not 1st time through
         LLGT  R5,WKARB
*
***********************************************************************
*   FILL   NEXT "BUFSIZE" buffer                                      *
***********************************************************************
         USING ADACBX,CB
         USING FBBDX,FB
         USING RBBDX,R5
         USING SBBDX,SB
         USING VBBDX,VB
         USING IBBDX,IB
         USING MBBDX,MB
*
         CLI   WKEOF,C'Y'          End of file pending ?      
         JE    EVNTEOF             Yes, go
*
         LA    R1,ADAPAL
         L     R15,WKAADA
         LR    R11,R13
         LAY   R13,WKSUBADA
         BASR  R14,R15
         LR    R13,R11
         CLC   ACBXRSP,=H'0'
         JE    A0014O
         CLC   ACBXRSP,=F'3'
         JE    EVNTEOF             Nothing left to read here
         MVC   WORKMSG(2),ACBXCMD
         LH    R15,ACBXRSP
         CVD   R15,WKDBLWK
         MVC   WORKMSG+2(4),NUMMSK+8
         MVI   WORKMSG+2,C' '
         ED    WORKMSG+2(4),WKDBLWK+6
         LH    R15,ACBXERRC
         CVD   R15,WKDBLWK
         MVC   WORKMSG+6(4),NUMMSK+8
         MVI   WORKMSG+6,C' '
         ED    WORKMSG+6(4),WKDBLWK+6
         GVBMSG LOG,MSGNO=ADA_BADRSP,SUBNO=4,                          +
               GENENV=GENENV,                                          +
               SUB1=(PGMNAME,8),                                       +
               SUB2=(WORKMSG,2),                                       +
               SUB3=(WORKMSG+2,4),                                     +
               SUB4=(WORKMSG+6,4),                                     +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         J     A0202
*
A0014O   EQU   *
         XC    WKRECBUF,WKRECBUF
         USING MBDSECT,R12
         LA    R12,MBAREA
         LA    R11,RBDXQLL(,R5)
A001400  EQU   *
         CLC   MBRESP,=F'0'      All good
         JE    A001402
         CLC   MBRESP,=F'3'      Then at least we have a partial block
         JE    A0015E
         MVC   WORKMSG(2),ACBXCMD
         LH    R15,ACBXRSP
         CVD   R15,WKDBLWK
         MVC   WORKMSG+2(4),NUMMSK+8
         MVI   WORKMSG+2,C' '
         ED    WORKMSG+2(4),WKDBLWK+6
         LH    R15,ACBXERRC
         CVD   R15,WKDBLWK
         MVC   WORKMSG+6(4),NUMMSK+8
         MVI   WORKMSG+6,C' '
         ED    WORKMSG+6(4),WKDBLWK+6
         GVBMSG LOG,MSGNO=ADA_BADRSP,SUBNO=4,                          +
               GENENV=GENENV,                                          +
               SUB1=(PGMNAME,8),                                       +
               SUB2=(WORKMSG,2),                                       +
               SUB3=(WORKMSG+2,4),                                     +
               SUB4=(WORKMSG+6,4),                                     +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         J     A0202
*
A001402  EQU   *
         AH    R12,HMISN
         AH    R11,HLREC
         ASI   WKRECBUF,1        records in buffer so far
         CLC   WKRECBUF,MBCOUNT  all records retrieved ?
         JL    A001400           no, back for next...
         L     R0,WKRECCNT
         A     R0,WKRECBUF
         ST    R0,WKRECCNT       total records so far
         J     FETCHOK           read one block only, i.e. one L3.
*
A0015E   EQU   *
         MVI   WKEOF,C'Y'        End of file pending return partial blk
*
***********************************************************************
*   RETURN NEXT "BUFSIZE" buffer                                      *
***********************************************************************
FETCHOK  DS    0H
         ASI   WKBUFRET,1         INCREMENT NUMBER BUFFERS RETURNED
         LA    R1,RBDXQLL(,R5)
         LLGTR R6,R1              Current record at start of block
         STG   R6,RECADDR         Store address of record
         LH    R0,HLREC
         ST    R0,ROWLEN          Store length of record
         ST    R0,GPRECLEN        used by MR95
         ST    R0,GPRECMAX
*
         MS    R0,WKRECBUF        Times number of records
         ST    R0,GPBLKMAX
         AGR   R0,R6
         STG   R0,EODADDR         used by MR95
*
         STG   R6,SAVESUB3+64
         LMG   R14,R12,SAVESUB3
         BSM   0,R14
*
* This is for errors
*
A0202    EQU   *                  R15 contains return error
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* 1.  PERFORM END-OF-JOB PROCESSING AT END-OF-FILE (EOF)              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
EVNTEOF  EQU   *
         MVC   ACBXCMD,=CL2'CL'
         XC    FBDXSEND,FBDXSEND
         XC    RBDXSEND,RBDXSEND
         XC    SBDXSEND,SBDXSEND
         XC    VBDXSEND,VBDXSEND
         XC    RBDXRECV,RBDXRECV
         LA    R1,ADAPAL
         L     R15,WKAADA
         LR    R11,R13
         LAY   R13,WKSUBADA
         BASR  R14,R15
         LR    R13,R11
         CLC   ACBXRSP,=H'0'
         JE    A0015B
*
* Not much of an error but mention it
         MVC   WORKMSG(2),ACBXCMD
         LH    R15,ACBXRSP
         CVD   R15,WKDBLWK
         MVC   WORKMSG+2(4),NUMMSK+8
         MVI   WORKMSG+2,C' '
         ED    WORKMSG+2(4),WKDBLWK+6
         LH    R15,ACBXERRC
         CVD   R15,WKDBLWK
         MVC   WORKMSG+6(4),NUMMSK+8
         MVI   WORKMSG+6,C' '
         ED    WORKMSG+6(4),WKDBLWK+6
         GVBMSG LOG,MSGNO=ADA_BADRSP,SUBNO=4,                          +
               GENENV=GENENV,                                          +
               SUB1=(PGMNAME,8),                                       +
               SUB2=(WORKMSG,2),                                       +
               SUB3=(WORKMSG+2,4),                                     +
               SUB4=(WORKMSG+6,4),                                     +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
*
A0015B   EQU   *
         XGR   R6,R6         Indicate NO Records and end of partitions
         STG   R6,RECADDR         Save the Record address
*
* Free storage
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
*          'GVBMRAD - Buffer stats "ENQ" FAILED'
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
***********************************************************************
* Routines to obtain parameters: R1 -> sub parameter, R2 == length    *
***********************************************************************
SUBSB    DS    0H
         stm   R14,R12,12(r13)
         la    r0,wksave2
         st    r13,wksave2+4
         st    r0,8(,r13)
         lr    r13,r0
*
         la    R14,WKL3SB
         la    R1,3(,R1)
         aghi  r2,-3
         st    r2,FSBL
         bctr  r2,0
         exrl  r2,PARMMVC
*
         l     r13,wksave2+4
         lm    r14,r12,12(r13)
         BR    R14
***********************************************************************
SUBFB    DS    0H
         stm   R14,R12,12(r13)
         la    r0,wksave2
         st    r13,wksave2+4
         st    r0,8(,r13)
         lr    r13,r0
*
         la    r14,WKL3FB
         la    R1,3(,R1)
         aghi  r2,-3
         st    r2,FFBL
         bctr  r2,0
         exrl  r2,PARMMVC
*
         l     r13,wksave2+4
         lm    r14,r12,12(r13)
         BR    R14
***********************************************************************
SUBDBID  DS    0H
         stm   R14,R12,12(r13)
         la    r0,wksave2
         st    r13,wksave2+4
         st    r0,8(,r13)
         lr    r13,r0
*
         CHI   R2,6                    Too little
         JL    DBID17
         CHI   R2,15                   Too much
         JH    DBID17
         LA    R4,5(,R1)
         LR    R3,R2
         AHI   R3,-5
DBID19   EQU   *                       Check for numerics
         CLI   0(R4),C'0'
         JL    DBID17
         CLI   0(R4),C'9'
         JH    DBID17
         LA    R4,1(,R4)
         BRCT  R3,DBID19
*
         LA    R1,5(,R1)               First digit of number
         AHI   R2,-6                   Number length - 1 =L2
         OY    R2,=Xl4'00000070'       Set L1 in pack's L1L2
         EXRL  R2,EXEPACK
         CVB   R0,WKDBL1
         ST    R0,FDBID
         J     DBID18
DBID17   EQU   *
         wto 'error in DBID sub parameter'
DBID18   EQU   *
*
         l     r13,wksave2+4
         lm    r14,r12,12(r13)
         BR    R14
***********************************************************************
SUBFNR   DS    0H
         stm   R14,R12,12(r13)
         la    r0,wksave2
         st    r13,wksave2+4
         st    r0,8(,r13)
         lr    r13,r0
*
         CHI   R2,5                    Too little
         JL    FNR17
         CHI   R2,14                   Too much
         JH    FNR17
         LA    R4,4(,R1)
         LR    R3,R2
         AHI   R3,-4
FNR19    EQU   *                       Check for numerics
         CLI   0(R4),C'0'
         JL    FNR17
         CLI   0(R4),C'9'
         JH    FNR17
         LA    R4,1(,R4)
         BRCT  R3,FNR19
*
         LA    R1,4(,R1)               First digit of number
         AHI   R2,-5                   Number length - 1 =L2
         LR    R3,R2                   Number length - 1
         OY    R2,=Xl4'00000070'       Set L1 in pack's L1L2
         EXRL  R2,EXEPACK
         CVB   R0,WKDBL1
         ST    R0,FFNR
         LA    R14,WKOPRB+4
         EXRL  R3,PARMMVC
         MVC   WKOPRB(4),=CL4'ACC='
         LA    R3,1(,R3)
         AR    R14,R3
         MVI   0(R14),C'.'
         LA    R3,5(,R3)
         ST    R3,FOPRBL
         J     FNR18
FNR17    EQU   *
         wto 'error in FNR sub parameter'
FNR18    EQU   *
*
         l     r13,wksave2+4
         lm    r14,r12,12(r13)
         BR    R14
***********************************************************************
SUBLREC  DS    0H
         stm   R14,R12,12(r13)
         la    r0,wksave2
         st    r13,wksave2+4
         st    r0,8(,r13)
         lr    r13,r0
*
         CHI   R2,6                    Too little
         JL    LREC17
         CHI   R2,15                   Too much
         JH    LREC17
         LA    R4,5(,R1)
         LR    R3,R2
         AHI   R3,-5
LREC19   EQU   *                       Check for numerics
         CLI   0(R4),C'0'
         JL    LREC17
         CLI   0(R4),C'9'
         JH    LREC17
         LA    R4,1(,R4)
         BRCT  R3,LREC19
*
         LA    R1,5(,R1)               First digit of number
         AHI   R2,-6                   Number length - 1 =L2
         OY    R2,=Xl4'00000070'       Set L1 in pack's L1L2
         EXRL  R2,EXEPACK
         CVB   R0,WKDBL1
         STH   R0,HLREC
         J     LREC18
LREC17   EQU   *
         wto 'error in LREC sub parameter'
LREC18   EQU   *
*
         l     r13,wksave2+4
         lm    r14,r12,12(r13)
         BR    R14
*
static   loctr
PARMMVC  MVC   0(0,R14),0(R1)
EXEPACK  PACK  WKDBL1(0),0(0,R1)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         DS    0F
HNREC    DC    Y(NREC)
HMISN    DC    Y(MISN)
MODE31   DC    XL4'80000000'
STGBLKLN DC    A(WKLEN+8)
GVBUR33  DC    V(GVBUR33)
FFFF     DC    XL4'FFFFFFFF'
HEXTR    DS    XL256'00'
         ORG   HEXTR+240
         DC    C'0123456789ABCDEF'
         ORG   ,
ENQSTAT  DC    CL8'ENQSTAT '      MINOR  ENQ NODE  (Stats)
TRACNAME DC    CL8'MR95TRAC'      MINOR  ENQ NODE  (MR95 TRACE FILE)
ZEROES   DC   8CL01'0'
REFTBLID DC    CL03'#DD'
*
GENEVA   DC    CL8'GENEVA  '      MAJOR ENQ NAME
PGMNAME  DC    CL8'GVBMRAD '
LINKNAME DC    CL8'ADAUSER'
*
NUMMSK   DC    XL12'402020202020202020202021'
*
         LTORG ,
***********************************************************************
*        EQUATES                                                      *
***********************************************************************
*
F10K     equ   10240
NREC     EQU   100                 Arbitrary number of records per call
MISN     EQU   MBDXSEGL            Fixed == length of MBDSECT (16)
*
*        ADABAS Equates                                        
*                                                                    
ABDXVERE equ   C'G'          Type is ABDX                          
ABDXVERC equ   C'2'          Current version number is 2           
ABDXVER2 equ   C'2'          Initial release - version 2           
ABDXVCUR equ   256*ABDXVERE+ABDXVERC  2-byte current version       
ABDXV2   equ   256*ABDXVERE+ABDXVER2  Initial release - version 2  
ABDXQFB  equ   C'F'          Format Buffer                         
ABDXQRB  equ   C'R'          Record Buffer                         
ABDXQMB  equ   C'M'          Multifetch Buffer                     
ABDXQSB  equ   C'S'          Search Buffer                         
ABDXQVB  equ   C'V'          Value Buffer                          
ABDXQIB  equ   C'I'          ISN Buffer                            
ABDXQPB  equ   C'P'          Performance Buffer                    
ABDXQUI  equ   C'U'          User Info Buffer                      
ABDXQSTD equ   C' '          At end of ABDX (standard)             
ABDXQIND equ   C'I'          Indirectly addressed                  
ABDXQDSP equ   C'D'          ALET-qualified (data or address space)
*
***********************************************************************
*        DSECTS                                                       *
***********************************************************************
*
*        Workarea
*
WKAREA   DSECT
         ds    Xl(SAVF4SA_LEN)
WKSUBADA DS  18fd                 Savearea for calling ADABAS
WKSAVE2  DS  18f                  Sub savearea
WKDBLWK  DS    XL08               Double work workarea
WKDBL1   DS    D
WKTRACE  DS    CL1                Tracing
WKEOF    DS    CL1
WKTHRDNO DS    H
         DS    0F
FDBID    DS    F
FFNR     DS    F
FSBL     DS    F
FFBL     DS    F
FRBSIZE  DS    F
FMBSIZE  DS    F
WKRECCNT DS    F                  NUMBER OF RECORDS READ
WKRECBUF DS    F                  NUMBER OF RECORDS IN BUFFER
WKBUFRET DS    F                  NUMBER OF BUFFERS RETURNED
HLREC    DS    H                  RETURNED RECORD LENGTH
         DS    H
FOPRBL   DS    F
WKSUBPA1 DS    A
WKSUBPA2 DS    A
WKSUBPA3 DS    A
WKSUBPA4 DS    A
WKSUBPA5 DS    A
WKSUBPL1 DS    F
WKSUBPL2 DS    F
WKSUBPL3 DS    F
WKSUBPL4 DS    F
WKSUBPL5 DS    F
WKOPRB   DS    CL16
WKL3FB   DS    CL256
WKL3SB   DS    CL8
WKWTOPRM WTO   TEXT=(R2),MF=L
WKWTOLEN EQU   *-WKWTOPRM
         DS   0A
WKTXTBUF DS    CL135              Message buffer
WKTXTLEN DS    0HL002
WKPRTTXT DS    CL133
ADAPAL   DS   0F
         DS  11F
LINKWORK DS    XL256
APLXLINK DS    F
WKCID    DS   0F
WKCID1   DS    X
WKCID23  DS    XL2
WKCID4   DS    X
WKAADA   DS    F
WKARB    DS    F
CB       DS    XL(ACBXQLL)
FB       DS    XL(FBDXQLL)
         DS    CL256
SB       DS    XL(SBDXQLL)
         DS    CL8
VB       DS    XL(VBDXQLL)
         DS    CL56
IB       DS    XL(IBDXQLL)
         DS    XL100
MB       DS    XL(MBDXQLL)
MBCOUNT  DS    F
MBAREA   DS (NREC)CL(MISN)        100 * 16 byte ISN areas
WKLEN    EQU   (*-WKAREA)
*
*
SQLTAREA DSECT ,     SQL STATEMENT TEXT AREA
SQLBUFFR DS    HL2,CL10238        SQL STATEMENT LENGTH, STATEMENT TEXT
         ORG   SQLBUFFR
         DS    HL2
SQLTEXT  DS    CL10238
         ORG
*
*
ADACBX   DSECT ,     Adabas control block (extended)
ACBX     DS   0D         +00
ACBXTYP  DS    X         +00  ADALNK function code
ACBXTUSR EQU   X'00'           User logical call
ACBXRSV1 DS    X         +01  Reserved - set to zero
ACBXVER  DS   0CL2       +02  Control block type and version
ACBXVERT DS    C                ADACBX type identifier
ACBXVERE EQU   C'F'             Type is ADACBX
ACBXVERN DS    C               ADACBX version number
ACBXVERC EQU   C'2'             Current version number is 2
ACBXVER2 EQU   C'2'             Initial release - version 2
ACBXVCUR EQU   256*ACBXVERE+ACBXVERC  2-byte current version
ACBXV2   EQU   256*ACBXVERE+ACBXVER2  Initial release - version 2
ACBXLEN  DS    H         +04  ACB Length (= ACBXQLL)
ACBXCMD  DS    CL2       +06  Command Code
ACBXRSV2 DS    H         +08  Reserved - set to zero
ACBXRSP  DS    H         +0A  Response code
ACBXCID  DS    XL4       +0C  Command ID
ACBXDBID DS    F         +10  Database ID
ACBXFNR  DS    F         +14  File number
ACBXISNG DS    0D,F      +18  8-byte ISN - not in use
ACBXISN  DS    F         +1C  ISN
ACBXISLG DS    0D,F      +20  8-byte ISN lower limit - not in use
ACBXISL  DS    F         +24  ISN lower limit
ACBXISQG DS    0D,F      +28  8-byte ISN quantity - not in use
ACBXISQ  DS    F         +2C  ISN quantity
ACBXCOP1 DS    C         +30  Command option 1
ACBXCOP2 DS    C         +31  Command option 2
ACBXCOP3 DS    C         +32  Command option 3
ACBXCOP4 DS    C         +33  Command option 4
ACBXCOP5 DS    C         +34  Command option 5
ACBXCOP6 DS    C         +35  Command option 6
ACBXCOP7 DS    C         +36  Command option 7
ACBXCOP8 DS    C         +37  Command option 8
ACBXADD1 DS    CL8       +38  Additions 1
ACBXADD2 DS    XL4       +40  Additions 2
ACBXADD3 DS    CL8       +44  Additions 3
ACBXADD4 DS    CL8       +4C  Additions 4
ACBXADD5 DS    XL8       +54  Additions 5
ACBXADD6 DS    CL8       +5C  Additions 6
ACBXRSV3 DS    XL4       +64  Reserved - must be zero
ACBXERR  DS   0XL16      +68  Supplemental error information
ACBXERRG DS    0D,F      +68  Error offset in buffer (64 bit)
ACBXERRA DS    F         +6C  Error offset in buffer (32 bit)
ACBXERRB DS    CL2       +70  Error character field (field name)
ACBXERRC DS    H         +72  Error subcode
ACBXERRD DS    C         +74  Error buffer ID
ACBXERRE DS    X         +75  Reserved for future use
ACBXERRF DS    H         +76  Error buffer sequence number (per ID)
ACBXSUB  DS   0XL8       +78  Subcomponent error information
ACBXSUBR DS    H         +78  Subcomponent response code
ACBXSUBS DS    H         +7A  Subcomponent response subcode
ACBXSUBT DS    CL4       +7C  Subcomponent error text
ACBXLCMP DS    D         +80  Compressed record length
ACBXLDEC DS    D         +88  Decompressed length of all returned data
ACBXCMDT DS    D         +90  Command time (in 1/4096 microsecs units)
ACBXUSER DS    XL16      +98  User field (not touched by Adabas)
ACBXRSV4 DS    XL24      +A8  Reserved - used by Adabas (do not touch)
ACBXQLL  EQU   *-ADACBX   C0  ACBX structure length
*
*
FBBDX    DSECT ,     Adabas Format Buffer Descriptor                
FBDX     DS   0D                                                  
FBDXLEN  DS    H         +00 Fixed length of ABDX structure (X'30')
FBDXVER  DS    0CL2      +02 Structure block type and version      
FBDXVERT DS    C              ABDX type identifier                 
FBDXVERN DS    C              ABDX version number                  
FBDXID   DS    C         +04 Buffer ID                             
FBDXRSV1 DS    B         +05 Reserved for future use, must be X'00'
FBDXLOC  DS    C         +06 Buffer location flag                  
FBDXRSV2 DS    X         +07 Reserved for future use, must be X'00'
FBDXRSV3 DS    F         +08 Reserved for future use, must be X'00'
FBDXALET DS    F         +0C ALET for buffer (ABDXLOC=C'D')        
FBDXSIZE DS    D         +10 Buffer size (allocated length)        
FBDXSEND DS    D         +18 Length of data to send to server      
FBDXRECV DS    D         +20 Length of data received from server   
FBDXADRG DS    0D,XL4    +28 64-bit address of indirectly addressed
*                              buffer in primary address space or CSA  
*                              (ABDXLOC=C'I') or address space         
*                              (ABDXLOC=C'D')                          
FBDXADR  DS    A         +2C 31-bit address of indirectly addressed
*                              buffer in primary address space or CSA  
*                              (ABDXLOC=C'I') or dataspace/address     
*                              space (ABDXLOC=C'D')                    
FBDXQLL  EQU   *-FBBDX     30  ADABDX structure length             
FBDXDATA EQU   *         +30  Actual buffer (ABDXLOC=C' ')         
*
*
RBBDX    DSECT ,     Adabas Record Buffer Descriptor                
RBDX     DS   0D                                                  
RBDXLEN  DS    H         +00 Fixed length of ABDX structure (X'30')
RBDXVER  DS    0CL2      +02 Structure block type and version      
RBDXVERT DS    C              ABDX type identifier                 
RBDXVERN DS    C              ABDX version number                  
RBDXID   DS    C         +04 Buffer ID                             
RBDXRSV1 DS    B         +05 Reserved for future use, must be X'00'
RBDXLOC  DS    C         +06 Buffer location flag                  
RBDXRSV2 DS    X         +07 Reserved for future use, must be X'00'
RBDXRSV3 DS    F         +08 Reserved for future use, must be X'00'
RBDXALET DS    F         +0C ALET for buffer (ABDXLOC=C'D')        
RBDXSIZE DS    D         +10 Buffer size (allocated length)        
RBDXSEND DS    D         +18 Length of data to send to server      
RBDXRECV DS    D         +20 Length of data received from server   
RBDXADRG DS    0D,XL4    +28 64-bit address of indirectly addressed
*                              buffer in primary address space or CSA  
*                              (ABDXLOC=C'I') or address space         
*                              (ABDXLOC=C'D')                          
RBDXADR  DS    A         +2C 31-bit address of indirectly addressed
*                              buffer in primary address space or CSA  
*                              (ABDXLOC=C'I') or dataspace/address     
*                              space (ABDXLOC=C'D')                    
RBDXQLL  EQU   *-RBBDX     30  ADABDX structure length             
RBDXDATA EQU   *         +30  Actual buffer (ABDXLOC=C' ')         
*
*
SBBDX    DSECT ,     Adabas Search Buffer Descriptor                
SBDX     DS   0D                                                  
SBDXLEN  DS    H         +00 Fixed length of ABDX structure (X'30')
SBDXVER  DS    0CL2      +02 Structure block type and version      
SBDXVERT DS    C              ABDX type identifier                 
SBDXVERN DS    C              ABDX version number                  
SBDXID   DS    C         +04 Buffer ID                             
SBDXRSV1 DS    B         +05 Reserved for future use, must be X'00'
SBDXLOC  DS    C         +06 Buffer location flag                  
SBDXRSV2 DS    X         +07 Reserved for future use, must be X'00'
SBDXRSV3 DS    F         +08 Reserved for future use, must be X'00'
SBDXALET DS    F         +0C ALET for buffer (ABDXLOC=C'D')        
SBDXSIZE DS    D         +10 Buffer size (allocated length)        
SBDXSEND DS    D         +18 Length of data to send to server      
SBDXRECV DS    D         +20 Length of data received from server   
SBDXADRG DS    0D,XL4    +28 64-bit address of indirectly addressed
*                              buffer in primary address space or CSA  
*                              (ABDXLOC=C'I') or address space         
*                              (ABDXLOC=C'D')                          
SBDXADR  DS    A         +2C 31-bit address of indirectly addressed
*                              buffer in primary address space or CSA  
*                              (ABDXLOC=C'I') or dataspace/address     
*                              space (ABDXLOC=C'D')                    
SBDXQLL  EQU   *-SBBDX     30  ADABDX structure length             
SBDXDATA EQU   *         +30  Actual buffer (ABDXLOC=C' ')         
*
*
VBBDX    DSECT ,     Adabas Value Buffer Descriptor                
VBDX     DS   0D                                                  
VBDXLEN  DS    H         +00 Fixed length of ABDX structure (X'30')
VBDXVER  DS    0CL2      +02 Structure block type and version      
VBDXVERT DS    C              ABDX type identifier                 
VBDXVERN DS    C              ABDX version number                  
VBDXID   DS    C         +04 Buffer ID                             
VBDXRSV1 DS    B         +05 Reserved for future use, must be X'00'
VBDXLOC  DS    C         +06 Buffer location flag                  
VBDXRSV2 DS    X         +07 Reserved for future use, must be X'00'
VBDXRSV3 DS    F         +08 Reserved for future use, must be X'00'
VBDXALET DS    F         +0C ALET for buffer (ABDXLOC=C'D')        
VBDXSIZE DS    D         +10 Buffer size (allocated length)        
VBDXSEND DS    D         +18 Length of data to send to server      
VBDXRECV DS    D         +20 Length of data received from server   
VBDXADRG DS    0D,XL4    +28 64-bit address of indirectly addressed
*                              buffer in primary address space or CSA  
*                              (ABDXLOC=C'I') or address space         
*                              (ABDXLOC=C'D')                          
VBDXADR  DS    A         +2C 31-bit address of indirectly addressed
*                              buffer in primary address space or CSA  
*                              (ABDXLOC=C'I') or dataspace/address     
*                              space (ABDXLOC=C'D')                    
VBDXQLL  EQU   *-VBBDX     30  ADABDX structure length             
VBDXDATA EQU   *         +30  Actual buffer (ABDXLOC=C' ')         
*
*
IBBDX    DSECT ,     Adabas ISN Buffer Descriptor                
IBDX     DS   0D                                                  
IBDXLEN  DS    H         +00 Fixed length of ABDX structure (X'30')
IBDXVER  DS    0CL2      +02 Structure block type and version      
IBDXVERT DS    C              ABDX type identifier                 
IBDXVERN DS    C              ABDX version number                  
IBDXID   DS    C         +04 Buffer ID                             
IBDXRSV1 DS    B         +05 Reserved for future use, must be X'00'
IBDXLOC  DS    C         +06 Buffer location flag                  
IBDXRSV2 DS    X         +07 Reserved for future use, must be X'00'
IBDXRSV3 DS    F         +08 Reserved for future use, must be X'00'
IBDXALET DS    F         +0C ALET for buffer (ABDXLOC=C'D')        
IBDXSIZE DS    D         +10 Buffer size (allocated length)        
IBDXSEND DS    D         +18 Length of data to send to server      
IBDXRECV DS    D         +20 Length of data received from server   
IBDXADRG DS    0D,XL4    +28 64-bit address of indirectly addressed
*                              buffer in primary address space or CSA  
*                              (ABDXLOC=C'I') or address space         
*                              (ABDXLOC=C'D')                          
IBDXADR  DS    A         +2C 31-bit address of indirectly addressed
*                              buffer in primary address space or CSA  
*                              (ABDXLOC=C'I') or dataspace/address     
*                              space (ABDXLOC=C'D')                    
IBDXQLL  EQU   *-IBBDX     30  ADABDX structure length             
IBDXDATA EQU   *         +30  Actual buffer (ABDXLOC=C' ')         
*
*
MBBDX    DSECT ,     Adabas Multifetch Buffer Descriptor                
MBDX     DS   0D                                                  
MBDXLEN  DS    H         +00 Fixed length of ABDX structure (X'30')
MBDXVER  DS    0CL2      +02 Structure block type and version      
MBDXVERT DS    C              ABDX type identifier                 
MBDXVERN DS    C              ABDX version number                  
MBDXID   DS    C         +04 Buffer ID                             
MBDXRSV1 DS    B         +05 Reserved for future use, must be X'00'
MBDXLOC  DS    C         +06 Buffer location flag                  
MBDXRSV2 DS    X         +07 Reserved for future use, must be X'00'
MBDXRSV3 DS    F         +08 Reserved for future use, must be X'00'
MBDXALET DS    F         +0C ALET for buffer (ABDXLOC=C'D')        
MBDXSIZE DS    D         +10 Buffer size (allocated length)        
MBDXSEND DS    D         +18 Length of data to send to server      
MBDXRECV DS    D         +20 Length of data received from server   
MBDXADRG DS    0D,XL4    +28 64-bit address of indirectly addressed
*                              buffer in primary address space or CSA  
*                              (ABDXLOC=C'I') or address space         
*                              (ABDXLOC=C'D')                          
MBDXADR  DS    A         +2C 31-bit address of indirectly addressed
*                              buffer in primary address space or CSA  
*                              (ABDXLOC=C'I') or dataspace/address     
*                              space (ABDXLOC=C'D')                    
MBDXQLL  EQU   *-MBBDX     30  ADABDX structure length             
MBDXDATA EQU   *         +30  Actual buffer (ABDXLOC=C' ')         
*
*
MBDSECT  DSECT ,     Adabas Multifetch Buffer Segment
MBRECL   DS    F
MBRESP   DS    F
MBISN    DS    F
MBISNQ   DS    F
MBDXSEGL EQU   *-MBDSECT
*
***********************************************************************
*        DATA CONTROL BLOCKS                                          *
***********************************************************************
         DCBD  DSORG=PS
         ihadcbe
*
         END
