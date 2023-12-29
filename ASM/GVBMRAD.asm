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
*        Adabas dsects
*
         ADACBX DSECT=YES
*
         ADABDX TYPE=EQ
*
         ADABDX TYPE=FB
         ADABDX TYPE=RB
         ADABDX TYPE=SB
         ADABDX TYPE=VB
         ADABDX TYPE=IB
         ADABDX TYPE=MB
*
*        Multifetch buffer segment
*
MBDSECT  DSECT
MBRECL   DS    F
MBRESP   DS    F
MBISN    DS    F
MBISNQ   DS    F
*
*        Workarea
*
WKAREA   DSECT
         ds    Xl(SAVF4SA_LEN)
WKSAVSUB DS  18fd                 INTERNAL  SUBROUTINE  REG  SAVE  AREA
WKSAB    DS  18fd                 Sub savearea
WKSUBADA DS  18fd                 Savearea for calling ADABAS
WKREG10  DS    A                  Preserve R10 (return address)
*
WKTIMWRK DS   0XL16
WKDBLWRK DS    D                  TEMPORARY DOUBLEWORD  WORK AREA
WKDBLWK2 DS    D                  TEMPORARY DOUBLEWORD  WORK AREA
WKDBLWK3 DS    D                  TEMPORARY DOUBLEWORD  WORK AREA
*
WKPRINT  DS    XL131              Print line
WKTRACE  DS    CL1                Tracing
         DS   0F
WKREENT  DS    XL256              Reentrant workarea
WKDBLWK  DS    XL08               Double work workarea
*
WKEOF    DS    CL1
         DS    XL1
WKTHRDNO DS    H
*
         DS    0F
WKRECCNT DS    F                  NUMBER OF RECORDS READ
WKRECBUF DS    F                  NUMBER OF RECORDS IN BUFFER
WKBUFRET DS    F                  NUMBER OF BUFFERS RETURNED
WKCALLRL DS    F                  RETURNED RECORD LENGTH
*
WKRPTDCB DS    A                  CONTROL   REPORT      "DCB"
*
         DS    0F
OUTDCB   DS    XL(outfilel)    Reentrant DCB and DCBE areas
*
WKWTOPRM WTO   TEXT=(R2),MF=L
WKWTOLEN EQU   *-WKWTOPRM
*
*WK_MSG   GVBMSG PREFIX=WMSG,MF=L
*
         DS   0A
WKTXTBUF DS    CL135              Message buffer
WKTXTLEN DS    0HL002
WKPRTTXT DS    CL133
*
ADAPAL   DS   0F
         DS  11F
*
LINKWORK DS    XL256
APLXLINK DS    F
WKCID    DS   0F
WKCID1   DS    X
WKCID23  DS    XL2
WKCID4   DS    X
WKAADA   DS    F
*
CB       DS    XL(ACBXQLL)
*
FB       DS    XL(FBDXQLL)
         DS    CL56
*
SB       DS    XL(SBDXQLL)
         DS    CL56
*
VB       DS    XL(VBDXQLL)
         DS    CL56
*
IB       DS    XL(IBDXQLL)
         DS    XL100
*
MB       DS    XL(MBDXQLL)
MBCOUNT  DS    F
MBAREA   DS (NREC)CL(MISN)        100 * 16 byte ISN areas
*
RB       DS    XL(RBDXQLL)
RBAREA   DS (NREC)CL(LREC)        100 * 96 byte records
*
WORKLEN  EQU   (*-WORKAREA)
*
LREC     EQU   96
NREC     EQU   100
MISN     EQU   16

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
         XC    workmsg(16),workmsg zero translated error msg area
*
A0002    EQU   *
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
***********************************************************************
*  OPEN INPUT FILE (ADABAS)                                           *
***********************************************************************
*
         LAY   R3,CB
         USING ADACBX,CB
         LAY   R4,FB
         USING FBBDX,FB
         LAY   R5,RB
         USING RBBDX,R5
         LAY   R6,SB
         USING SBBDX,SB
         LAY   R7,VB
         USING VBBDX,VB
         LAY   R8,IB
         USING IBBDX,IB
         LAY   R9,MB
         USING MBBDX,MB
*
         ST    R3,ADAPAL+00
         LA    R0,APLXLINK
         ST    R0,ADAPAL+04
         LA    R0,LINKWORK
         ST    R0,ADAPAL+08
         STM   R4,R9,ADAPAL+12
         OI    ADAPAL+32,X'80'
*
*
         LOAD  EPLOC=LINKNAME,ERRET=A0010
         OILH  R0,MODE31
         ST    R0,WKAADA
         J     A0011
A0010    EQU   *
         GVBMSG LOG,MSGNO=ADA_NOLINK,SUBNO=1,                          +
               SUB1=(PGMNAME,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         LHI   R15,12
         J     RETURNE
*
*
A0011    EQU   *
         MVI   ACBXVERT,ACBXVERE
         MVI   ACBXVERN,ACBXVERC
         MVC   ACBXLEN,=Y(ACBXQLL)
         MVC   ACBXDBID,DBID300
         MVC   ACBXFNR,FNR0047
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
         MVC   FBDXSIZE+4(4),=F'50'
*
         MVI   RBDXLOC,C' '
         MVC   RBDXLEN,=Y(RBDXQLL)
         MVI   RBDXVERT,ABDXVERE
         MVI   RBDXVERN,ABDXVERC
         MVI   RBDXID,ABDXQRB
         MVC   RBDXSIZE+4(4),=A(LREC*NREC)
*
         MVI   MBDXLOC,C' '
         MVC   MBDXLEN,=Y(MBDXQLL)
         MVI   MBDXVERT,ABDXVERE
         MVI   MBDXVERN,ABDXVERC
         MVI   MBDXID,ABDXQMB
         MVC   MBDXSIZE+4(4),=A(4+MISN*NREC)  QUANTITY THEN MISN AREA
*
         MVI   SBDXLOC,C' '
         MVC   SBDXLEN,=Y(SBDXQLL)
         MVI   SBDXVERT,ABDXVERE
         MVI   SBDXVERN,ABDXVERC
         MVI   SBDXID,ABDXQSB
         MVC   SBDXSIZE+4(4),=F'50'
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
         MVC   IBDXSIZE+4(4),=F'100'
*
*
         MVC   ACBXCMD,=CL2'OP'
         MVC   ACBXCID,WKCID
         MVC   ACBXADD3,SPACES
         MVC   ACBXADD4,SPACES
         MVC   ACBXADD5,SPACES
         MVC   RBDXDATA(07),=CL7'ACC=47.'
         MVC   RBDXSEND+4(4),=F'7'
         LA    R1,ADAPAL
         L     R15,WKAADA
         LR    R11,R13
         LAY   R13,WKSUBADA
         BASR  R14,R15
         LR    R13,R11
*
         CLC   ACBXRSP,=H'0'
         JE    A0012
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
               SUB1=(PGMNAME,8),                                       +
               SUB2=(WORKMSG,2),                                       +
               SUB3=(WORKMSG+2,4),                                     +
               SUB4=(WORKMSG+6,4),                                     +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         LHI   R15,12
         J     RETURNE
*
A0012    EQU   *                       Set for first read command
         MVC   ACBXCMD,=CL2'L3'
         MVI   ACBXCOP1,C'M'
         MVI   ACBXCOP2,C'V'
         MVC   ACBXADD1,=CL8'AA      '
         XC    ACBXISN,ACBXISN
         XC    ACBXISL,ACBXISL
         XC    ACBXISQ,ACBXISQ
*
         MVC   FBDXDATA(21),=CL21'AA,AB,AC,AD,AE,AF,AG.'
         MVC   FBDXSEND+4(4),=F'21'
*
         XC    RBDXSEND,RBDXSEND
         MVC   RBDXRECV+4(4),=A(LREC*NREC)
*
         XC    MBDXSEND,MBDXSEND
         MVC   MBDXRECV+4(4),=A(MISN*NREC)
*
         MVC   SBDXDATA(03),=CL3'AA.'
         MVC   SBDXSEND+4(4),=F'3'
*
         MVC   VBDXDATA(10),=CL10'0000000000'
         MVC   VBDXSEND+4(4),=F'10'
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
         LAY   R15,ADAFETCH1      LOAD FETCH ROUTINE ADDRESS
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
ADAFETCH1 DS    0H
DB2FETCH DS    0H
         STMG  R14,R12,SAVESUB3
         LR    R9,R14              SAVE RETURN ADDRESS
         LARL  R10,GVBMRAD         set static area base
         LAY   R5,RB
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
LOADB    DS    0H
         CLI   WKEOF,C'Y'          End of file pending ?      
         JE    EVNTEOF             Yes, go
*
A0013    EQU   *
         LA    R1,ADAPAL
         L     R15,WKAADA
         LR    R11,R13
         LAY   R13,WKSUBADA
         BASR  R14,R15
         LR    R13,R11
         CLC   ACBXRSP,=H'0'
         JE    A0014
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
               SUB1=(PGMNAME,8),                                       +
               SUB2=(WORKMSG,2),                                       +
               SUB3=(WORKMSG+2,4),                                     +
               SUB4=(WORKMSG+6,4),                                     +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         J     A0202
*
A0014    EQU   *
         XC    WKRECBUF,WKRECBUF
         USING MBDSECT,R12
         LAY   R12,MBAREA
         LAY   R11,RBAREA
A001400  EQU   *
         CLC   MBRESP,=F'0'      All good
         JE    A001402
         CLC   MBRESP,=F'3'      Then at least we have a partial block
         JE    A0015
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
               SUB1=(PGMNAME,8),                                       +
               SUB2=(WORKMSG,2),                                       +
               SUB3=(WORKMSG+2,4),                                     +
               SUB4=(WORKMSG+6,4),                                     +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         J     A0202
*
A001402  EQU   *
         LA    R12,MISN(,R12)
         LA    R11,LREC(,R11)
         ASI   WKRECBUF,1        records in buffer so far
         CLC   WKRECBUF,MBCOUNT  all records retrieved ?
         JL    A001400           no, back for next...
         L     R0,WKRECCNT
         A     R0,WKRECBUF
         ST    R0,WKRECCNT       total records so far
         J     UNLOADB           read one block only, i.e. one L3.
*
A0015    EQU   *
         MVI   WKEOF,C'Y'        End of file pending return partial blk
*
***********************************************************************
*   RETURN NEXT "BUFSIZE" buffer                                      *
***********************************************************************
UNLOADB  DS    0H
         LLGT  R15,=A(LREC)
         MS    R15,WKRECBUF
*
         LLGT  R14,GPBLKSIZ
         USING GENBLKSZ,R14
         ST    R15,GP_RESULT_BLK_SIZE
         DROP  R14
*
         LAY   R1,RBAREA
         LLGTR R1,R1
         LLGT  R14,GPBLOCKA       LOAD  POINTER ADDRESS
         USING GENBLOCK,R14
         STG   R1,GP_RESULT_PTR   RETURN BLOCK ADDRESS
         DROP  R14
*
         LTR   R15,R15
         JZ    EVNTEOF            Nothing to return, go
         XC    WKRECBUF,WKRECBUF  RESET BUFFER COUNT = 0
         ASI   WKBUFRET,1         INCREMENT NUMBER BUFFERS RETURNED
*
* This is for ok
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
*
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
