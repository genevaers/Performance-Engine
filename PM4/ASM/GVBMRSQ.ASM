         TITLE 'GVBMRSQ - DB2 SQL INITIALIZATION FOR "GVBMR95"'
********************************************************************#VB
*   SAFR                                                              *
********************************************************************#VE
*
* (c) Copyright IBM Corporation 2008.
*     Copyright Contributors to the GenevaERS Project.
* SPDX-License-Identifier: Apache-2.0
*
***********************************************************************
*
*   Licensed under the Apache License, Version 2.0 (the "License");
*   you may not use this file except in compliance with the License.
*   You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
*   Unless required by applicable law or agreed to in writing, software
*   distributed under the License is distributed on an "AS IS" BASIS,
*   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
*   or implied.
*   See the License for the specific language governing permissions and
*   limitations under the License.
***********************************************************************
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   GVBMRSQ - LOADS THE SQL TEXT ASSOCIATED WITH THE INDICATED INPUT  *
*            EVENT FILE DDNAME FROM THE REFERENCE FILE, I.E., THE     *
*            DDNAME IS THE KEY TO THE REFERENCE FILE;                 *
*                                                                     *
*            DYNAMICALLY ESTABLISHES A CONNECTION TO DB2, ISSUES A    *
*            "PREPARE" FOR THE SQL TEXT, AND THEN FETCHES ALL         *
*            THE ROWS FROM THE DB2 ANSWER SET AND PASSES THEM         *
*            TO "GVBMR95".                                            *
*                                                                     *
*  NOTE:     GVBMRSQ RUNS IN 31-BIT ADDRESSING MODE.                  *
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
*        R12 - PROGRAM   BASE REGISTER                                *
*        R11 - PROGRAM   BASE REGISTER                                *
*                                                                     *
*        R10 - INTERNAL  SUBROUTINE  RETURN ADDRESS (1ST LEVEL)       *
*        R9  - INTERNAL  SUBROUTINE  RETURN ADDRESS (2ND LEVEL)       *
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
* 12/10/03  DFK  TT1602  - ECHO FINAL SQL TO TRACE FILE IF OPEN
* 01/12/04  DFK  TT1519  - ADDED DB2 PLAN NAME OVERRIDE
* 02/17/04  RHN  TT1603  - CALL DSNTIAR TO GET DB2 MESSAGE TEXT
* 04/14/04  DFK  TT1756  - DISPLAY RESOLVED SQL TEXT IN 80 CHAR SEGMNTS
* 05/21/04  DFK          - ALLOCATE 10K BUFFER FOR EXPANDED SQL
* 07/28/05  DFK  TT1761  - FIX DB2 ERROR MESSAGE DISPLAYS
* 12/19/05  DFK  TT2252  - DETECT NONZERO SQLCODES WHEN R15=0
* 20131223  PGC  CQ10590 - CREATE SAFR PROTOTYPE TO INVESIGATE
*                          BENEFITS/MEMORY COST
* 20160725  JRD RTC19305 - Abend 0c6 as DCB EODAD=1 by default        *
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * #LE
*
         Copy  GVBASSRT
         Copy  EXECDATA
         Copy  GVBMR95W
         Copy  GVBMR95L
         Copy  GVBMR95C
         Copy  GVB0200A
         Copy  GVB0200B
         Copy  GVBX95PA
         Copy  GVBLOGIT
         Copy  GVBRPTIT
         Copy  GVBUTEQU
*
         Push  PRINT
         Print OFF,NOGEN,NOPRINT
         Copy  ASMMSP
LEAVE    OpSyn ASM_LEAVE
         ASMMREL ON
         SYSSTATE ARCHLVL=2
         IEABRCX DEFINE
         IHASAVER
         Pop   PRINT
*
SQLTAREA DSECT                    SQL STATEMENT TEXT AREA
*
SQLBUFFR DS    HL2,CL10238        SQL STATEMENT LENGTH, STATEMENT TEXT
         ORG   SQLBUFFR
         DS    HL2
SQLTEXT  DS    CL10238
         ORG
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
         SPACE 2
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        REGISTER SAVE AREA OFFSETS:                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
RSABP    EQU   4
RSAFP    EQU   8
RSA14    EQU   12
RSA15    EQU   16
RSA0     EQU   20
RSA1     EQU   24
RSA6     EQU   44
RSA11    EQU   64
                        EJECT
*
LTVERS3  EQU   003         VERSION 3 LOGIC TABLE
*
V3SQLTXT EQU   960         V3 SQL TEXT LENGTH
SUBSOFF  EQU   297         DB2 SUBSYSTEM ID OFFSET IN REFERENCE RECORD
TEXTOFF  EQU   301         DB2 SQL  TEXT    OFFSET IN REFERENCE RECORD
ERRTXTLN EQU   80          DB2  ERROR DESCRIPTION  LINE LENGTH
         SPACE 2
         EXEC  SQL DECLARE SQLSTMT STATEMENT
         EXEC  SQL DECLARE DB2ROW  CURSOR FOR SQLSTMT
         SPACE 2
         PRINT GEN
*
GVBMRSQ  RMODE 24
GVBMRSQ  AMODE 31
*        ENTRY DSNHLI
GVBMRSQ  CSECT
         j     start              get to the code
static   loctr                    define the static section
MRSQEYE  GVBEYE GVBMRSQ
code     loctr                    and the code
         USING THRDAREA,R13       REUSE/RETAIN   MAIN THREAD SAVE  AREA
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
*
START    STM   R14,R12,SAVESUBR+RSA14  SAVE  CALLER'S REGISTERS
*
         LR    R11,R15            Set up static area pointer
         USING (GVBMRSQ,code),R11
*
*        OPEN  (SNAPDCB,(OUTPUT)),MODE=31
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        I N I T I A L I Z E   W O R K   A R E A                      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         xc    dblwork2,dblwork2  zero area acumulating storage total
         xc    workmsg(4),workmsg zero translated error msg area
         LARL  R0,DB2FETCH        INITIALIZE READ ROUTINE ADDRESS
         O     R0,MODE31
         ST    R0,EVNTREAD
*
         L     R2,EVNTDCBA        INDICATE NO RDW PRESENT (FIXED FMT)
         NI    DCBRECFM-IHADCB(R2),X'3F'
         OI    DCBRECFM-IHADCB(R2),X'80'
*
         MVI   GPRECFMT,C'F'      INDICATE  FIXED LENGTH RECORDS
         MVI   GPRECDEL,X'00'     NO FIELD  DELIMITERS
*
*        LOAD  EP=DSNALI          LOAD    DB2 ASSEMBLER INTERFACE
*        ST    R0,DSNALI
*
*        LOAD  EP=DSNHLI2         LOAD    DB2 HIGHER LEVEL LANG ADDRESS
*        ST    R0,DSNHLI2
*        ST    R0,SAVEHLI2
*
         COPY  GVBMRSQP           DB2     PLAN   NAME
*
         L     R14,EXECDADR       DB2  PLAN OVERRIDE   ???
         USING EXECDATA,R14
         CLC   EXECSPLN,SPACES    OVERRIDE  PRESENT    ???
         BRE   *+10
         MVC   DB2PLAN,EXECSPLN
         DROP  R14
*
         MVC   REFTBL,REFTBLID    REFERENCE TABLE ID
         MVC   REFKEY(30),SPACES  REFERENCE TABLE KEY (SQL TEXT)
         MVC   REFDATE,ZEROES     EFFECTIVE DATE
*
         MVC   SQLCAID,=CL8'SQLCA'
         LA    R0,SQLCALEN
         ST    R0,SQLCABC
                        SPACE 3
         L     R0,SQLDALEN        ALLOCATE  SQL  DESCRIPTOR    AREA
         sty   r0,dblwork2        update storage total
         GETMAIN RU,LV=(0),LOC=(ANY)
         ST    R1,SQLDADDR        SAVE SQL  DESCRIPTOR AREA ADDRESS
         LR    R7,R1
         USING SQLDA,R7
*
         LR    R0,R7              INITIALIZE  "SQLDA" (LOW  VALUES)
         L     R1,SQLDALEN
         SR    R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         MVC   SQLDAID,=CL8'SQLDA'
         MVC   SQLDABC,SQLDALEN
         MVC   SQLN,H750
                        SPACE 3
         L     R0,SQLDSIZ         ALLOCATE  SQL  WORK  AREA
         lr    r15,r0
         ay    r15,dblwork2       add to storage total
         sty   r15,dblwork2        and save it
         GETMAIN R,LV=(0)
         ST    R1,SQLWADDR        SAVE WORK AREA ADDRESS
         LR    R8,R1
         USING SQLDSECT,R8
*
         DROP  R7
                        EJECT
**********************************************************************
* GET SQL TEXT FROM VDP                                              *
**********************************************************************
         L     R4,THRDRE          GET SUBSYSTEM NAME
         L     R4,LTVDP200-LOGICTBL(,R4)
         USING vdp0200b_FILE_RECORD,R4
**********************************************************************
* PERFORM SYMBOLIC VARIABLE SUBSTITUTION FOR DB2 SUBSYSTEM NAME      *
**********************************************************************
         MVC   LKUPKEY(L'ENVVNAME),vdp0200b_DBMS_SUBSYS
         OC    LKUPKEY(L'ENVVNAME),SPACES UPPERCASE  NAME
*
         LA    R0,LKUPKEY        PERFORM SYMBOLIC VARIABLE SUBSTITUTION
         STy   R0,GENPARM1
         LHI   R0,L'ENVVNAME
         STH   R0,DBLWORK
         LA    R0,DBLWORK
         STy   R0,GENPARM2
         LA    R0,gpenvva
         STy   R0,GENPARM3
         LAy   R1,GENPARM1
         L     R15,GVBUR33
         BASR  R14,R15
*
         MVC   DBSUBSYS,LKUPKEY
         MVC   LKUPKEY,SPACES
*
**********************************************************************
* COPY SQL TEXT FROM "VDP" INTO EXPANSION BUFFER                     *
**********************************************************************
         Lhi   R0,F10K              GET BUFFER FOR EXPANDED SQL TEXT
         lr    r15,r0
         ay    r15,dblwork2       add to storage total
         sty   r15,dblwork2        and save it
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
         STy   R0,GENPARM1
         LHI   R0,L'SQLTEXT       SQL TEXT LENGTH
         STH   R0,DBLWORK
         LA    R0,DBLWORK
         STy   R0,GENPARM2
         LA    R0,gpenvva
         STy   R0,GENPARM3
         LAy   R1,GENPARM1
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
         EX    R15,TRACMVC
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
*
static   loctr                    define the static section
TRACMVC  MVC   PRNTline+1(0),0(R3)     * * * * E X E C U T E D * * * *
code     loctr
*
         DROP  R4
         DROP  R7
*
*        L     R9,SQLTADDR
*        USING SQLTAREA,R9
*        LA    R10,2-1(,R9)
*        AH    R10,SQLBUFFR
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=150,STORAGE=((R9),(R10))
*        DROP  R9
*
         BAS   R10,DB2CONN        ESTABLISH  DB2  CONNECTION
*
*        The following code must be serialised to keep acurate
*        statistics of the read i/o buffer storage use, as
*        multiple threads/TCBs could be operating.
*        The current total and high water mark are kept in the
*        THRDMAIN area.
*        The size of the getmains are accumulated in DBLWORK2
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
         LARL  R15,DB2FETCH       LOAD FETCH ROUTINE ADDRESS
         BASR  R14,R15            WAIT  FOR COMPLETION OF FIRST READ
         ST    R6,SAVESUBR+RSA6   PASS BACK RECORD ADDRESS
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=159
*
         SPACE 5
RETURN   SR    R15,R15            SET  RETURN CODE  TO ZERO
*
RETURNE  L     R14,SAVESUBR+RSA14     RESTORE REGISTER  R14
         LM    R0,R12,SAVESUBR+RSA0   RESTORE REGISTERS R0 - R12
         BSM   0,R14                  RETURN
                        SPACE 3
*
RTNERROR DS    0H
*
* Write error message to log
*
         GVBMSG LOG,MSGNO=(R14),SUBNO=3,GENENV=GENENV,                 +
               SUB1=(modname,8),                                       +
               SUB2=(REASCODE,8),                                      +
               SUB3=(GPDDNAME,8),                                      +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         J     RTNXTRA
*
RTNERROR2 DS    0h  ERROR MESSAGE WITH ONLY 2 PARMS
*
         GVBMSG LOG,MSGNO=(R14),SUBNO=2,GENENV=GENENV,                 +
               SUB1=(modname,8),                                       +
               SUB2=(REASCODE,8),                                      +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
RTNXTRA  DS    0h
*
* If there is any info from a translated SQLCODE, write that to log
*
         if oc,workmsg(4),workmsg,nz
           if clc,workmsg(4),eq,ffff  did XLATE fail?

             GVBMSG LOG,MSGNO=SQL_XLATE_FAIL,SUBNO=2,                  +
               GENENV=GENENV,                                          +
               SUB1=(modname,8),                                       +
               SUB2=(WORKMSG+4,4),                                     +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
           else
             GVBMSG LOG,MSGNO=DB2_TRANSLATED_SQLCODE,SUBNO=2,          +
               GENENV=GENENV,                                          +
               SUB1=(modname,8),                                       +
               SUB2=(WORKMSG,64),                                      +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
           endif
         endif
*
         LHI   R15,12
*
         B     RETURNE
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        E S T A B L I S H   D B 2   C O N N E C T I O N              *
*                                                                     *
*              - CONNECT                                              *
*                                                                     *
*                ESTABLISH A CONNECTION BETWEEN AN APPLICATION'S      *
*                ADDRESS SPACE AND A SPECIFIED DB2 SUBSYSTEM.         *
*                                                                     *
*              - OPEN THREAD                                          *
*                                                                     *
*                ESTABLISH THE SPECIFIED DB2PLAN AS A USER OF DB2     *
*                SERVICES AND ALLOCATE RESOURCES FOR SQL CALLS.       *
*                THIS CAN BE CALLED 'CREATING A THREAD'.              *
*                                                                     *
*              - PREPARE                                              *
*                                                                     *
*                DYNAMICALLY BIND THE SQL STATEMENT FOR EXECUTION.    *
*                                                                     *
*              - OPEN CURSOR                                          *
*                                                                     *
*                INITIALIZE THE DB2 CURSOR ASSOCIATED WITH THE        *
*                SQL STATEMENT.                                       *
*                                                                     *
*              - TRANSLATE                                            *
*                                                                     *
*                CLARIFIES A RESOURCE UNAVAILABLE CONDITION.          *
*                IT IS PERFORMED AFTER AN OPEN FAILURE.               *
*                                                                     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        SPACE 3
**********************************************************************
* CONNECT TO DB2                                                     *
**********************************************************************
*
DB2CONN  LA    R1,CONNECT          CONNECT    FUNCTION
         LA    R2,DBSUBSYS         SUBSYSTEM  NAME
         LA    R3,DBTRMECB         DB2 STARTUP     ECB
         LA    R4,DBSTRECB         DB2 TERMINATION ECB
         LA    R9,RIBPTR           RELEASE INFORMATION BLOCK
         STMy  R1,R4,GENPARM1
         STy   R9,GENPARM5
         OIy   GENPARM5,X'80'
*
         MVC   CAFFUNC,CONNECT     INDICATE CURRENT CAF FUNCTION
*
         LAy   R1,GENPARM1         CALL "DSNALI" - CONNECT
         L     R15,DSNALI
         BASR  R14,R15
*
         LTR   R15,R15             SUCCESSFUL  CONNECT ???
         BZ    CONNOPEN            YES - CONTINUE
         LA    R14,DB2_CONNECT_FAIL NO  - CONNECT  FAILED
         ST    R0,PACKCODE         STORE THE REASON CODE
         UNPK  REASCODE(9),PACKCODE(5)
         TR    REASCODE(8),XTAB
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=201
         B     RTNERROR
                        SPACE 3
**********************************************************************
* OPEN THREAD                                                        *
**********************************************************************
*
CONNOPEN LA    R1,OPEN             OPEN    THREAD  FUNCTION
         LA    R2,DBSUBSYS         SUBSYSTEM NAME
         LA    R3,DB2PLAN          DB2PLAN   NAME
         STMy  R1,R3,GENPARM1
         OIy   GENPARM3,X'80'
*
         MVC   CAFFUNC,OPEN        INDICATE CURRENT CAF FUNCTION
*
         LAy   R1,GENPARM1         CALL "DSNALI" - OPEN THREAD
         L     R15,DSNALI
         BASR  R14,R15
*
         LA    R14,DB2_OPEN_THREAD_FAIL ASSUME OPEN FAILED
         LTR   R15,R15             SUCCESSFUL OPEN ???
         BNZ   XLATEERR            NO  - INDICATE  ERROR
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=202
                        SPACE 3
**********************************************************************
* SET DEGREE FOR PARALLEL PROCESSING                                 *
**********************************************************************
*
*ONNDEG  MVC   CAFFUNC,SETDEG      INDICATE CURRENT CAF FUNCTION
*
*        EXEC  SQL SET CURRENT DEGREE = 'ANY'
*
*        LA    R1,ERRMSG07         ASSUME SET FAILED
*        LTR   R15,R15             SUCCESSFUL OPEN ???
*        BNZ   RTNERROR            NO  - INDICATE  ERROR
                        SPACE 3
**********************************************************************
* PREPARE(BIND) THE SQL STATEMENT                                    *
**********************************************************************
CONNPREP L     R7,SQLDADDR         LOAD "SQLDA" BASE REGISTER
         USING SQLDA,R7
*
         MVC   CAFFUNC,PREPARE     INDICATE CURRENT CAF FUNCTION
*
         L     R9,SQLTADDR         LOAD SQL STATEMENT TEXT ADDRESS
         USING SQLTAREA,R9
         EXEC  SQL PREPARE SQLSTMT INTO :SQLDA FROM :SQLBUFFR
         L     R15,SQLCODE         SUCCESSFUL OPEN ???
         LTR   R0,R15
         BZ    CONNALLO            YES - CONTINUE
         DROP  R9
*
*   The following is getting the error message text
*
         CVD   R15,DBLWORK         STORE THE RETURN CODE (PACKED)
         MVC   REASCODE(8),SPACES
         UNPK  REASCODE(4),DBLWORK+4(4)
         OI    REASCODE+3,X'F0'    FORCE A DISPLAYABLE ZONE
         LTR   R15,R15             NEGATIVE  SQLCODE ???
         BP    *+8                 NO  - BYPASS MOVE
         MVI   REASCODE+0,C'-'
*
         LHI   R0,10*ERRTXTLN      Allow for 10 lines of messages
*                                    each 80 bytes long
         sthy  R0,pgmwork
         LHI   R0,ERRTXTLN         LRECL
         ST    R0,DBLWORK
*
         LA    R14,SQLCA           SQL  COMMUNICATION  AREA
         lay   R15,pgmwork
         LA    R0,DBLWORK
         STMy  R14,R0,GENPARM1
         Oiy   GENPARM3,X'80'
*
         LAy   R1,GENPARM1         CALL "DSNTIAR" FOR  MESSAGE TEXT
         L     R15,DSNTIAR
         BASR  R14,R15
*
         CHI   R15,4               SUCCESSFUL TRANSLATION ???
         jh    CONNMSG_ERR
*
         lhi   r3,10               max number of msg lines in 1 hit
         lay   r14,pgmwork+2+34
         MVC   ERRDATA,0(r14)      COPY  1ST  50 BYTES OF TEXT
         je    connerr1            more to come so no trailing blanks

         lay   r14,pgmwork+2       start of returned DB2 msg
         lhy   r1,pgmwork          get length of buffer
         ar    r14,r1              point to end of buffer
cmsgblp  ds    0h
         bctr  r14,r0               last character in buffer
         cli   0(r14),c' '         Find 1st non blank going backwards
         jne   cmsgsetl            Y:
         jct   r1,cmsgblp          Check next character
*                                  If we get here something is wrong
         j     CONNMSG_ERR
*
cmsgsetl ds    0h
         ahi   r1,errtxtln-1       lrecl-1
         xr    r0,r0               ready for divide
         d     r0,=a(errtxtln)     divide by lrecl
         lr    r3,r1               save number of msg line
         mhi   r1,errtxtln         this now the length
         sthy  r1,pgmwork          save it
*
**********************************************************************
* DISPLAY DB2 ERROR DESCRIPTION TEXT                                 *
**********************************************************************
Connerr1 ds    0h
         lhi   r15,errtxtln+4          msg line length plus RDW
         sth   r15,prntrdwh            save in RDW
         xc    prntrdwl,prntrdwl
         ENQ   (GENEVA,LOGNAME,E,,STEP),RNL=NO
*
         MVI   PRNTLINE+0,C'*'
         MVC   PRNTLINE+1(errtxtln-1),PRNTLINE+0
*
         MVI   PRNTline+32,C' '
         MVC   PRNTline+33(L'DBSUBSYS),DBSUBSYS
         MVI   PRNTline+33+L'DBSUBSYS,C' '
*
         MVI   PRNTline+38,C' '
         MVC   PRNTline+39(8),GPDDNAME
         MVI   PRNTline+39+8,C' '
*
         logit ,
*
         lay   r4,pgmwork+2            start of msgs
*
CONNLOOP ds    0h
         mvc   prntline(errtxtln),0(r4)
         logit ,
         ahi   r4,errtxtln             next part of message
         jct   r3,CONNLOOP
*
         MVI   PRNTLINE+0,C'*'
         MVC   PRNTLINE+1(L'PRNTLINE-1),PRNTLINE+0
*
         logit ,
*
CONNDEQ  DEQ   (GENEVA,LOGNAME,,STEP),RNL=NO
*
CONNMSG  ds    0h
*
         LA    R14,SQL_PREPARE_FAIL NO  - PREPARE FAILED
         B     RTNERROR            INDICATE  ERROR
*
CONNMSG_ERR DS 0H
*
         GVBMSG LOG,MSGNO=DB2_MSG_TEXT_MISSING,SUBNO=1,                +
               GENENV=GENENV,                                          +
               SUB1=(modname,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
*
         J     CONNMSG
*
                        SPACE 3
**********************************************************************
* ALLOCATE HOST VARIABLE/INDICATOR AREA                              *
**********************************************************************
*
CONNALLO LH    R14,SQLD            INITIALIZE LOOP  COUNTER
         LTR   R14,R14
         BP    CONNINIT            YES - CONTINUE
         LA    R14,SQL_NO_COLUMNS  NO  - BAD  SQL   TEXT (NO COLUMNS)
         MVC   REASCODE(8),GPDDNAME
         B     RTNERROR2
*
CONNINIT LA    R4,SQLVAR           INITIALIZE DSECT BASE REGISTER
         USING SQLVARN,R4
         SR    R15,R15             ZERO   ROW LENGTH
CONNVLP1 BAS   R9,GETLEN           GET LENGTH OF OUTPUT DATA/INDICATORS
         ST    R0,SQLIND           SAVE  IND  LENGTH
         ST    R1,SQLDATA          SAVE  DATA LENGTH
         AR    R15,R0              SUM INDICATOR    LENGTHS
         AR    R15,R1              SUM COLUMN DATA  LENGTHS
         LA    R4,SQLSIZV(,R4)     ADVANCE TO NEXT  "SQLDA" VARIABLE
         BRCT  R14,CONNVLP1        LOOP THROUGH ALL COLUMNS
*
         ST    R15,ROWLEN          SAVE DB2 ROW SIZE
                        SPACE 3
         LR    R0,R15              SUM  OF  VARIABLE + INDICATOR AREAS
         lr    r14,r0
         ay    r14,dblwork2       add to storage total
         sty   r14,dblwork2        and save it
         GETMAIN R,LV=(0)
         LGR   R2,R1               Save Area address
         STG   R2,RECADDR          Save the address
*
         LGR   R0,R1               Load length of area to clear
         LGF   R1,ROWLEN           Load length of area to clear
         XGR   R14,R14             Clear Target address
         XGR   R15,R15             Clear Target length, pad byte
         MVCLE R0,R14,X'00'        Clear area to nulls
         JO    *-4                 If not finished then keep going
*
         LH    R0,SQLD             INITIALIZE LOOP  COUNTER
         LA    R4,SQLVAR           INITIALIZE DSECT BASE REGISTER
         USING SQLVARN,R4
CONNVLP2 L     R15,SQLIND          LOAD   IND LENGTH
         LTR   R15,R15             NULLABLE   FIELD (INDICATOR NEEDED)
         BNP   *+8                 NO  - LEAVE POINTER = ZERO
         ST    R2,SQLIND           INITIALIZE INDICATOR  ADDRESS
         AR    R2,R15              ADVANCE TO DATA  AREA
         L     R15,SQLDATA         LOAD  DATA LENGTH
         ST    R2,SQLDATA          INITIALIZE VARIABLE DATA ADDRESS
         AR    R2,R15              ADVANCE TO NEXT  VARIABLE DATA AREA
         LA    R4,SQLSIZV(,R4)     ADVANCE TO NEXT  "SQLDA" VARIABLE
         BRCT  R0,CONNVLP2         LOOP THROUGH ALL COLUMNS
*
*        L     R9,SQLDABC
*        AR    R9,R7
*        BCTR  R9,0
*        L     R15,ROWLEN
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=203,STORAGE=((R7),(R9))
*
         DROP  R4
                        SPACE 3
**********************************************************************
* OPEN DB2 CURSOR                                                    *
**********************************************************************
*
         MVC   CAFFUNC,OPEN        INDICATE CURRENT CAF FUNCTION
*
*        EXEC  SQL WHENEVER NOT FOUND  GO TO EVNTEOF
         EXEC  SQL OPEN DB2ROW
*
         L     R15,SQLCODE         SUCCESSFUL OPEN ???
         LTR   R15,R15
         BZ    CONNEXIT            YES - exit
*
         CVD   R15,DBLWORK         STORE THE RETURN CODE (PACKED)
         MVC   REASCODE(8),SPACES
         UNPK  REASCODE(4),DBLWORK+4(4)
         OI    REASCODE+3,X'F0'    FORCE A DISPLAYABLE ZONE
         LTR   R15,R15             NEGATIVE  SQLCODE ???
         BP    *+8                 NO  - BYPASS MOVE
         MVI   REASCODE+0,C'-'
*
         LA    R14,SQL_OPEN_FAIL   OPEN CURSOR FAILED
         B     RTNERROR            NO  - INDICATE ERROR
                        EJECT
**********************************************************************
* TRANSLATE ERROR CODE                                               *
**********************************************************************
*
XLATEERR DS    0H
         ST    R0,PACKCODE         STORE THE REASON CODE
         CVD   R15,DBLWORK         STORE THE RETURN CODE (PACKED)
         UNPK  DBLWORK(4),DBLWORK+4(4)
         OI    DBLWORK+3,X'F0'     FORCE A DISPLAYABLE ZONE
         LTR   R15,R15             NEGATIVE RETURN CODE ???
         BP    *+8                 NO  - BYPASS  MINUS SIGN
         MVI   DBLWORK+0,C'-'
         ST    R14,DBLWORK+8       SAVE ORIGINAL ERROR MESSAGE CODE
*
         UNPK  REASCODE(9),PACKCODE(5)
         TR    REASCODE(8),XTAB
*
         LA    R1,XLATE            TRANSLATE   RETURN  CODE   FUNCTION
         LA    R2,SQLCA            SQL  COMMUNICATION  AREA
         STMy  R1,R2,GENPARM1
         LA    R0,DBLWORK+0
         STy   R0,GENPARM3
         LA    R0,PACKCODE
         STy   R0,GENPARM4
         OIy   GENPARM4,X'80'
*
         MVC   CAFFUNC,XLATE       INDICATE CURRENT CAF FUNCTION
*
         LAy   R1,GENPARM1         CALL "DSNALI" - TRANSLATE "SQLCODE"
         L     R15,DSNALI
         BASR  R14,R15
* SQL_XLATE_FAIL
         if LTR,R15,R15,nz         UNSUCCESSFUL XLATE ?
*          LA  R14,SQL_XLATE_FAIL  TRANSLATE FAILED
           CVD R15,DBLWORK         STORE THE RETURN CODE (PACKED)
           MVC WORKMSG+4(8),SPACES
           UNPK WORKMSG+4(4),DBLWORK+4(4)
           OI  WORKMSG+4+3,X'F0'   FORCE A DISPLAYABLE ZONE
           LTR R15,R15             NEGATIVE  SQLCODE ???
           BP  *+8                 NO  - BYPASS MOVE
           MVI WORKMSG+4,C'-'
           mvc WORKMSG(4),FFFF     Indicate XLATE failed
*
           L   R14,DBLWORK+8        LOAD ERROR MESSAGE ADDRESS
         else
*
* Error text contains "<SQLERRM>......      <Return code>"
*
           MVC WORKMSG+00(50),SQLERRM+2 BUILD ERROR MESSAGE
           MVI WORKMSG+50,C'.'
           MVC WORKMSG+51(5),WORKMSG+50
           MVC WORKMSG+56(8),SPACES
           MVC WORKMSG+60(4),DBLWORK+0
*
           L   R14,DBLWORK+8        LOAD ERROR MESSAGE ADDRESS
         endif
         B     RTNERROR            DISPLAY ORIGINAL MESSAGE
                        SPACE 3
CONNEXIT EQU   *
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=204
         BSM   0,R10              RETURN
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        DETERMINE OUTPUT DATA LENGTH BASED ON COLUMN TYPE            *
*                                                                     *
*        UPON EXIT:                                                   *
*          R0 - NULL INDICATOR LENGTH (0 OR 1)                        *
*          R1 - DATA LENGTH (INCLUDING 2 BYTE LENGTH IF VARIABLE)     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING SQLDA,R7
         USING SQLVARN,R4
*
GETLEN   LH    R0,SQLTYPE          GET   THE COLUMN   TYPE
         SRL   R0,1                STRIP OFF NULLABLE INDICATOR BIT
         SLL   R0,1
*
         LH    R1,SQLLEN           GET THE LENGTH FOR SOME TYPES
         CH    R0,CHARTYPE         IF THIS IS CHARACTER DATA, WE'RE SET
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
         LA    R1,2(,R1)           ADD TWO FOR VARCHAR
         CH    R0,VARCTYPE         IF THIS IS VARCHAR DATA, ADD TWO
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
         CH    R0,VARLTYPE         IF THIS IS VARCHAR DATA, ADD TWO
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
*
         LH    R1,SQLLEN           GET LENGTH AGAIN
         SLL   R1,1                DOUBLE IT FOR GRAPHIC
         CH    R0,GTYPE            IF THIS IS GRAPHIC, DOUBLE
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
         LA    R1,2(,R1)           ADD TWO FOR VARGRAPHIC
         CH    R0,VARGTYPE         IF THIS IS VARGRAPHIC, ADD TWO
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
         CH    R0,LVARGTYP         IF LONG VARGRAPHIC, WE'RE SET
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
*
         LA    R1,2                SET UP TWO FOR SMALL INTEGER
         CH    R0,HWTYPE           IF THIS IS SMALLINT DATA, SET TWO
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
         LA    R1,4                SET UP FOUR FOR INTEGER
         CH    R0,INTTYPE          IF THIS IS INTEGER DATA, SET FOUR
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
*
         LH    R1,SQLLEN           GET LENGTH AGAIN
         CH    R0,FLOATYPE         IF THIS IS FLOAT DATA, SET LENGTH
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
*
         SR    R1,R1               CLEAR  THE REGISTER
         IC    R1,SQLPRCSN         GET    THE PRECISION
         LA    R1,2(,R1)           ADD    TWO (ALLOW  FOR  SIGN)
         SRL   R1,1                DIVIDE  BY TWO TO  GET  BYTES
         CH    R0,DECTYPE          IF THIS IS FLOAT  DATA, SET EIGHT
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
*
         LH    R1,SQLLEN           GET LENGTH AGAIN
         CH    R0,DATETYP          IF THIS IS DATE DATA,
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
         LH    R1,SQLLEN           GET LENGTH AGAIN
         CH    R0,TIMETYP          IF THIS IS TIME DATA,
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
         LH    R1,SQLLEN           GET LENGTH AGAIN
         CH    R0,TIMES            IF THIS IS TIMESTAMP DATA,
         BE    GETNULL             JUST CHECK FOR NULLABLE COLUMNS
*
*        ERROR (UNRECOGNIZED DATA TYPE)
*
         SR    R1,R1               ZERO LENGTH
                        SPACE 3
GETNULL  SR    R0,R0               ASSUME   NOT  NULLABLE (LEN=0)
         TM    SQLTYPE+1,X'01'     TEST THE NULL/INDICATOR BIT ???
         BNO   GETEXIT             THERE IS NO INDICATOR, EXIT
         LA    R0,2                LOAD NULL INDICATOR LENGTH
*
GETEXIT  BR    R9                  RETURN
*
         DROP  R4
         DROP  R7
         DROP  R8
         DROP  R11
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* 1.  READ  THE NEXT   DETAIL EVENT RECORD  (DB2 ROW)                 *
* 2.  PERFORM END-OF-JOB PROCESSING AT END-OF-FILE (EOF)              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING THRDAREA,R13
         using genparm,parm_area
         using genfile,file_area
         using genenv,env_area
*
DB2FETCH LR    R9,R14             SAVE RETURN ADDRESS
*
*   This is called directly from GVBMR95 - which has r12 as a base reg
*   so do not use R12 anywhere in here
*
         larl  r11,gvbmrsq
         using (gvbmrsq,code),r11
*
         MVC   CAFFUNC,FETCH      INDICATE CURRENT CAF FUNCTION
*
         L     R7,SQLDADDR        LOAD SQL DESCRIPTOR AREA ADDRESS
         USING SQLDA,R7
*
         L     R8,SQLWADDR        LOAD WORK AREA ADDRESS
         USING SQLDSECT,R8
*
         EXEC  SQL WHENEVER NOT FOUND  GO TO EVNTEOF
         EXEC  SQL FETCH DB2ROW USING DESCRIPTOR :SQLDA
*
         L     R15,SQLCODE
         LTR   R15,R15            SUCCESSFUL   ???
         BZ    FETCHOK            YES - CONTINUE
*
*        LA    R1,SQL_FETCH_FAIL  ASSUME FETCH FAILED
*
         CVD   R15,DBLWORK         STORE THE RETURN CODE (PACKED)
         MVC   WORKMSG(8),SPACES
         UNPK  WORKMSG(4),DBLWORK+4(4)
         OI    WORKMSG+3,X'F0'     FORCE A DISPLAYABLE ZONE
         LTR   R15,R15             NEGATIVE  SQLCODE ???
         BP    *+8                 NO  - BYPASS MOVE
         MVI   WORKMSG+0,C'-'
*
         GVBMSG LOG,MSGNO=SQL_FETCH_FAIL,SUBNO=3,                      +
               GENENV=GENENV,                                          +
               SUB1=(modname,8),                                       +
               SUB2=(WORKMSG,4),   sqlcode                             +
               SUB3=(GPDDNAME,8),  DDNAME                              +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
*
         XGR   R6,R6              Indicate NO Record
         STG   R6,RECADDR         Save the Record address
*
         L     R2,EVNTDCBA        LOAD SYNAD EXIT ADDRESS
         L     R2,DCBDCBE-IHADCB(R2) --> DCBE
         USING DCBE,R2
         L     R14,DCBESYNA       LOAD I/O error routine addr
         drop  r2
         LTR   R14,R14            EXIT  ADDRESS  AVAILABLE   ???
         BPR   R14                YES - USE "GVBMR95" EXIT   ADDRESS
         BR    R9                 NO  - USE "GVBMRSQ" RETURN ADDRESS
                        SPACE 3
FETCHOK  LA    R4,SQLVAR          LOAD SQL DESCRIPTOR AREA ADDRESS
         USING SQLVARN,R4
*
         LG    R6,RECADDR         Load Host Variable address (1st Col)
         lgf   R0,ROWLEN
         ST    R0,GPRECLEN
         ST    R0,GPRECMAX
         ST    R0,GPBLKMAX
*
         agr   R0,R6
         stg   R0,EODADDR
*
*        LR    R15,R6
*        A     R15,ROWLEN
*        BCTR  R15,0
*        SNAP  DCB=SNAPDCB,ID=206,STORAGE=((R6),(R15))
*
         BSM   0,R9               RETURN
                        EJECT
EVNTEOF  EQU   *
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=220
*
**********************************************************************
* CLOSE CURSOR                                                       *
**********************************************************************
         MVC   CAFFUNC,CLOSE       INDICATE CURRENT CAF FUNCTION
*
         EXEC  SQL CLOSE DB2ROW
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=221
                        SPACE 3
**********************************************************************
* CLOSE THREAD                                                       *
**********************************************************************
*
         MVC   TERMOP,SYNC         SET   DB2 TERMINATION  OPTION
*
         LA    R1,CLOSE            BUILD PARAMETER  LIST
         LA    R2,TERMOP
         STMy  R1,R2,GENPARM1
         OIy   GENPARM2,X'80'
*
         LAy   R1,GENPARM1         CALL "DSNALI" - CLOSE  THREAD
         L     R15,DSNALI
         BASR  R14,R15
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=222
                        SPACE 3
**********************************************************************
* DISCONNECT                                                         *
**********************************************************************
*
         LA    R1,DISC             BUILD PARAMETER  LIST
         STy   R1,GENPARM1
         OIy   GENPARM1,X'80'
*
         LAy   R1,GENPARM1         CALL "DSNALI" - DISCONNECT FROM DB2
         L     R15,DSNALI
         BASR  R14,R15
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=223
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
         LG    R1,RECADDR         Load address of Storage to release
         l     r0,rowlen
         lr    r2,r0
         STORAGE RELEASE,LENGTH=(0),ADDR=(1)
*        free sqldaddr area
         L     R0,SQLDALEN        SQL DESCRIPTOR AREA length
         ar    r2,r0
         l     R1,SQLDADDR        Load SQL DESCRIPTOR AREA ADDRESS
         STORAGE RELEASE,LENGTH=(0),ADDR=(1)
*        free sqlwaddr area
         L     R0,SQLDSIZ         SQL WORK AREA length
         ar    r2,r0
         l     R1,SQLWADDR        load WORK AREA ADDRESS
         STORAGE RELEASE,LENGTH=(0),ADDR=(1)
*        free sqltaddr area
         Lhi   R0,F10K            BUFFER FOR EXPANDED SQL TEXT
         ar    r2,r0
         L     R1,SQLTADDR
         STORAGE RELEASE,LENGTH=(0),ADDR=(1)
*
         XGR   R6,R6              Indicate NO Record
         STG   R6,RECADDR         Save the Record address
*
d1       using thrdarea,r14
         ly    r14,thrdmain          Get the initial thread area
         ly    r1,d1.read_buffer_tot Get total size of buffers used
         sr    r1,r2                 subtract freemained value
         sty   r1,d1.read_buffer_tot
         drop  d1
*
         DEQ (GENEVA,ENQSTAT,,STEP),RNL=NO
*
         XGR   R6,R6              Indicate NO Record
         STG   R6,RECADDR         Save the Record address
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
         DROP  R4
         DROP  R7
         DROP  R8
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
static   loctr                    define the static section
F10K     equ   10240
*
F4       DC    F'04'
F8       DC    F'08'
*
MODE31   DC    XL4'80000000'
*
SQLDALEN DC    F'33016'           16 + (750 COLUMNS * 44)
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
         ORG   *-240              TRANSLATE TABLE FOR BINARY TO HEX
XTAB     EQU   *                       (VIRTUAL ORIGIN)
         ORG   *+240
         DC    C'0123456789ABCDEF'
*
GENEVA   DC    CL8'GENEVA  '      MAJOR  ENQ NODE
ENQSTAT  DC    CL8'ENQSTAT '      MINOR  ENQ NODE
TRACNAME DC    CL8'MR95TRAC'      MINOR  ENQ NODE  (MR95 TRACE FILE)
LOGNAME  DC    CL8'MR95LOG'       MINOR  ENQ NODE  (MR95 LOG FILE)
PLANNAME DC    CL8'GVBMRSQ  '     DB2 PLAN   NAME
SYNC     DC    CL4'SYNC'          DB2 THREAD TERMINATION OPTION
ZEROES   DC   8CL01'0'
REFTBLID DC    CL03'#DD'
*
                        SPACE 3
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        CALL  ATTACH  FUNCTION  REQUEST  NAMES                       *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
CONNECT  DC    CL12'CONNECT      ' CONNECT      TO   DB2
DISC     DC    CL12'DISCONNECT   ' DISCONNECT   FROM DB2
OPEN     DC    CL12'OPEN         ' OPEN   (CREATE    THREAD)
CLOSE    DC    CL12'CLOSE        ' CLOSE  (TERMINATE THREAD)
XLATE    DC    CL12'TRANSLATE    ' TRANSLATE    OPEN ERRORS
PREPARE  DC    CL12'PREPARE      ' PREPARE  SQL STATEMENT
FETCH    DC    CL12'FETCH        ' FETCH    DB2 ROW
SETDEG   DC    CL12'SETDEGREE    ' SET PARALLEL PROCESSING DEGREE
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V A R I A B L E S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*SAVEHLI2 DC    A(0)                DSNHLI2 ENTRY POINT ADDRESS
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D A T A   C O N T R O L   B L O C K S                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
SNAPDCB  DCB   DSORG=PS,DDNAME=SNAPDATA,MACRF=(W),                     X
               RECFM=VBA,LRECL=125,BLKSIZE=1632
*
         DCBD  DSORG=PS
         ihadcbe
*
GVBMRSQ  CSECT
         EXEC  SQL INCLUDE SQLDA
GVBMRSQ  CSECT
*
static   loctr
         LTORG
*
*        IFGRPL AM=VSAM
*
         END