        TITLE 'Generic switch setter'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 1992, 2020.
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
* PROGRAM-ID.     GVBMR93.
*****************************************************************
*REMARKS
*****************************************************************
*  GENERIC SWITCH SETTER
*
*  THIS PROGRAM READS A CONTROL FILE AND SETS A SWITCH ON THE
*  FILE, BASED ON A PARAMETER PASSED FROM THE JOB.  IF ALL
*  SWITCHES ON THE FILE HAVE BEEN SET ("ALL" BEING DEFINED BY
*  ANOTHER PASSED PARAMETER), THEN THE RECORD IS SET TO ALL
*  SPACES AND REWRITTEN.
*
*  EXAMPLE INPUT PARM=('00470003') MEANS THERE ARE 47 POSSIBLE
*  SWITCHES AND THIS EXECUTION IS TO SET FLAG 3 AND TEST IF ALL
*  THE SWITCHES ARE NOW SET OR NOT.
*
*****************************************************************
                        EJECT
***********************************************************************
*                                                                     *
*           MODULE RETURN CODES AND REGISTER DOCUMENTATION            *
*                                                                     *
***********************************************************************
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*          0   - SUCCESSFUL: ALL SWITCHES NOW STATISFIED              *
*          2   - RESET PERFORMED OF SWITCH COUNTER AND ALL SWITCHES.  *
*                  TESTING PARM=('RESET')                             *
*          4   - SUCCESSFUL NOT ALL SWITCHES SATISFIED,               *
*                  THE SWITCH FOR THIS EXECTION IS SET ON.            *
*          8   - ERROR                                                *
*                  FOR EXAMPLE PARAMETER ERROR, WRONG NUMBER OF       *
*                  SWITCHES COMPARED TO FILE OR SWITCH ALREADY SET.   *
*         12   - SEVERE ERROR SUCH AS FILE I/O ERROR                  *
*                                                                     *
***********************************************************************
*
*
                        EJECT
         IHASAVER DSECT=YES,SAVER=NO,SAVF4SA=YES,SAVF5SA=YES,TITLE=NO
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
EXECINP  DSECT                 EXEC PARM
EXECLEN  DS    H
EXECPARM DS    CL01            First character of 100 max
*
FLAGBLK  DSECT                 Flag block in VSAM dataset
FLAGCNT  DS    Cl04            Number of flags - zoned numeric
FLAGVAL  DS    256CL01         Flag settings
FLAGBLKL EQU   *-FLAGBLK
*
***********************************************************************
*                                                                     *
*        GVBMR93      W O R K   A R E A                               *
*                                                                     *
***********************************************************************
WORKAREA DSECT                 Program workarea
WKSVA1   DS    18FD
WKSVA2   DS    18FD
*
WKEXEP2A DS    A
WKEXEP3A DS    A
WKEXEP2L DS    H
WKEXEP3L DS    H
*
ENQMAJ   DS    CL8
ENQMIN   DS    CL8
*
SWITCHCR DS    CL8             Characters passed on EXEC parm
SWITCHCT DS    H               Number of possible switches
SWITCHNO DS    H               Switch number of this execution
WKPACK8  DS    PL8
WKRETC   DS    F
*
WKREENT  DS    XL256           Reentrant workarea
WKPRINT  DS    XL131           Print line
WKRESET  DS    CL01            Reset request
WKDBLWK  DS    XL08            Double work workarea
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*       "STGTP90" -  PARAMETER LIST DEFINITION                        *
*                 -  PARAMETER AREA DEFINITION                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
TP90LIST DS   0A               PARAMETER  LIST FOR "STGTP90"
TP90PA   DS    A               ADDRESS OF PARAMETER AREA
TP90RECA DS    A               ADDRESS OF RECORD    BUFFER
TP90KEYA DS    A               ADDRESS OF RECORD    KEY
*
TP90AREA DS   0CL01            FILE SPECIFICATION ENTRY DEFINITION
*
PAANCHOR DS    AL04            TP90   WORK  AREA ANCHOR
PADDNAME DS    CL08            FILE   DDNAME
PAFUNC   DS    CL02            FUNCTION CODE
PAFTYPE  DS    CL01            FILE   TYPE (V  = VSAM,  S = SEQUENTIAL)
PAFMODE  DS    CL02            FILE   MODE (I  = INPUT, O = OUTPUT    )
*                                          (IO = BOTH                 )
PARTNC   DS    CL01            RETURN CODE
PAVSAMRC DS    HL02            VSAM   RETURN CODE
PARECLEN DS    HL02            RECORD LENGTH
PARECFMT DS    CL01            RECORD FORMAT (F=FIXED, V=VARIABLE)
PARESDS  DS    CL01            ESDA : direct access
*
PALEN    EQU   *-TP90AREA      PARAMETER AREA LENGTH
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         DS    0F
OUTDCB   DS    XL(outfilel)    Reentrant DCB and DCBE areas
*
WKFLGKEY DS    A
WKFLGREC DS    XL(FLAGBLKL)    Record in VSAM file
*
WORKLEN  EQU   (*-WORKAREA)
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*
***********************************************************************
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
***********************************************************************
*
         YREGS
*
GVBMR93  RMODE 31
GVBMR93  AMODE 31
GVBMR93  CSECT
         j     code
         push  print
         print off,nogen,noprint
         SYSSTATE ARCHLVL=2
         COPY  ASMMSP
LEAVE    OPSYN ASM_LEAVE
         ASMMREL ON
         IEABRCX DEFINE
         pop   print
MR93EYE  GVBEYE GVBMR93
static   loctr
code     loctr
         stm   R14,R12,12(r13)      save callers registers GVBMR93

         llgtr r0,r0              CLEAN
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
         lr    R12,r15            SET   PROGRAM   BASE REGISTERS
         USING (GVBMR93,code),R12
*
         l     r9,0(,r1)          LOAD  PARAMETER LIST ADDRESS
         la    r9,0(,r9)

         L     R0,=A(WORKLEN+8)   LOAD   WORK AREA SIZE
         AH    R0,=H'10000'
         STORAGE OBTAIN,LENGTH=(0),LOC=(24)        GET WORKAREA
*
         MVC   0(8,r1),=CL8'GVBMR93W'
         LA    R0,8(,R1)          move pointer past
         LR    R8,R0
         L     R1,=A(WORKLEN)     LOAD   WORK AREA SIZE
         XR    R14,R14
         XR    R15,R15
         MVCL  R0,R14
*
         USING WORKAREA,R8
         LA    R1,WKSVA1
         ST    R1,8(,R13)
         ST    R13,4(,R1)
         LR    R13,R1
*
MAINLINE ds    0h
*
*      get exec parameter (8 bytes e.g. 00470011 for number of switches
*        and the position of the switch for this execution
*
         MVC   SWITCHCR,SPACES           Initialize in case short
         USING EXECINP,R9
         LH    R1,EXECLEN
         IF (LTR,R1,R1,P)
           IF (CH,R1,GT,=H'8')
             LH  R1,=H'8'
           ENDIF
           BCTR R1,0
           EX   R1,MVCPARM
           J    A0001
static     loctr
MVCPARM    MVC  SWITCHCR(0),EXECPARM
code       loctr
A0001      EQU  *
         ENDIF
*
*      get optional enq major/minor name so we can run multiple builds
*
         MVC   ENQMAJ,GENEVA
         MVC   ENQMIN,PGMNAME
*
         LH   R1,EXECLEN
         AGHI R1,-9
         IF (LTR,R1,R1,P)
           IF (CLI,EXECPARM+8,EQ,C',')
             LA    R2,EXECPARM+9
             LGR   R10,R2
             DO FROM=(R1)
               IF (CLI,0(R2),EQ,C',')
                 LGR   R0,R2
                 SGR   R0,R10
                 STH   R0,WKEXEP2L
                 ST    R10,WKEXEP2A
                 LA    R11,1(R2)
                 ST    R11,WKEXEP3A
                 LH    R0,EXECLEN
                 LA    R3,EXECPARM
                 AGR   R0,R3
                 SGR   R0,R11
                 STH   R0,WKEXEP3L
                 J     A0001E
               ENDIF
               LA    R2,1(0,R2)
             ENDDO
             LH    R0,EXECLEN
             LA    R3,EXECPARM
             AGR   R0,R3
             SGR   R0,R10
             STH   R0,WKEXEP2L
             ST    R10,WKEXEP2A
A0001E       EQU   *
           ENDIF
*
           LH   R1,WKEXEP2L
           IF (LTR,R1,R1,P)
             MVC   ENQMAJ,SPACES           Initialize in case short
             IF (CH,R1,GT,=H'8')
               LH  R1,=H'8'
             ENDIF
             BCTR R1,0
             L    R2,WKEXEP2A
             EX   R1,MVCPARM2
           ENDIF
*
           LH   R1,WKEXEP3L
           IF (LTR,R1,R1,P)
             MVC   ENQMIN,SPACES           Initialize in case short
             IF (CH,R1,GT,=H'8')
               LH  R1,=H'8'
             ENDIF
             BCTR R1,0
             L    R2,WKEXEP3A
             EX   R1,MVCPARM3
           ENDIF
           J    A0001A
*
static     loctr
MVCPARM2   MVC  ENQMAJ(0),0(R2)
MVCPARM3   MVC  ENQMIN(0),0(R2)
code       loctr
A0001A     EQU  *
*
         ENDIF
         drop  r9 EXECINP
*
*
*      open message file
         LA    R14,outfile               COPY MODEL   DCB
d1       using ihadcb,outdcb
         MVC   outdcb(outfilel),0(R14)
         lay   R0,outdcb                 SET  DCBE ADDRESS IN  DCB
         aghi  R0,outfile0
         sty   R0,d1.DCBDCBE
*
         LAY   R2,OUTDCB
         MVC   WKREENT(8),OPENPARM
         OPEN  ((R2),(OUTPUT)),MODE=31,MF=(E,WKREENT)
*
*      echo input exec parms
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(34),=CL34'GVBMR93: Being executed with parm:'
         MVC   WKPRINT+34(8),SWITCHCR
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
**** check input parm for numerics, or is it reset of vsam record?
*
         XR    R2,R2
         TRT   SWITCHCR,NOTNTAB
         IF (LTR,R2,R2,Z)
           PACK WKPACK8,SWITCHCR(4)
           CVB  R0,WKPACK8
           STH  R0,SWITCHCT
           PACK WKPACK8,SWITCHCR+4(4)
           CVB  R0,WKPACK8
           STH  R0,SWITCHNO
           MVC   WKPRINT,SPACES
           MVC   WKPRINT(58),=CL58'GVBMR93: Number flags: xxxx. This exe+
               cution is flag: xxxx.'
           MVC   WKPRINT+23(4),SWITCHCR
           MVC   WKPRINT+52(4),SWITCHCR+4
           LA    R2,OUTDCB
           LA    R0,WKPRINT
           PUT   (R2),(R0)
*
           IF (CLC,SWITCHNO,GT,SWITCHCT)
             MVC   WKPRINT,SPACES
             MVC   WKPRINT(33),=CL33'GVBMR93: Terminated. Switch > Max'
             LA    R2,OUTDCB
             LA    R0,WKPRINT
             PUT   (R2),(R0)
             MVC   WKRETC,=F'8'
             J     CLOSEFL
           ENDIF
*
         ELSE
           IF (CLC,SWITCHCR,EQ,=CL8'RESET')
             MVC   WKPRINT,SPACES
             MVC   WKPRINT(33),=CL33'GVBMR93: Reset function requested'
             LA    R2,OUTDCB
             LA    R0,WKPRINT
             PUT   (R2),(R0)
             MVI   WKRESET,C'Y'
           ELSE
             MVC   WKPRINT,SPACES
             MVC   WKPRINT(30),=CL30'GVBMR93: Terminated. Bad input'
             LA    R2,OUTDCB
             LA    R0,WKPRINT
             PUT   (R2),(R0)
             MVC   WKRETC,=F'8'
             J     CLOSEFL
           ENDIF
         ENDIF
*
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(29),=CL29'GVBMR93: Major ENQ: xxxxxxxx.'
         MVC   WKPRINT+20(8),ENQMAJ
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(29),=CL29'GVBMR93: Minor ENQ: xxxxxxxx.'
         MVC   WKPRINT+20(8),ENQMIN
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
**** Do ENQ, waiting for a while if ENQ cannot be obtained
*
         DO    INF
           LA  R2,ENQMAJ
           LA  R3,ENQMIN
           ENQ ((r2),(r3),E,8,SYSTEM),RNL=YES,RET=USE
*
           IF (LTR,R15,R15,NZ)           Exclusive control ?
             LA    R1,WAITINT            ADDR OF WAIT INTERVAL
             STIMER WAIT,BINTVL=(R1)     Wait a bit
           ELSE
             DOEXIT
           ENDIF
         ENDDO
*
**** Open VSAM file
*
         LA    R0,TP90AREA
         ST    R0,TP90PA
         LA    R0,WKFLGREC        RETURN DATA HERE
         ST    R0,TP90RECA
         LA    R0,WKFLGKEY        THIS IS REALLY THE RBA FOR ESDS
         ST    R0,TP90KEYA
*
         MVC   PAFUNC,=CL2'OP'    INITIALIZE FUNCTION CODE
         MVC   PADDNAME,=CL8'SWT     '       DDNAME
         MVI   PAFTYPE,C'V'       FILE TYPE
         MVC   PAFMODE,=CL2'IO'   FILE MODE
         MVI   PARECFMT,C'F'      FORMAT
         LA    R1,TP90LIST        POINT R1 TO PARAMETER LIST
         L     R15,=V(GVBTP90)    LOAD  ADDRESS OF "GVBTP90"
         BASR  R14,R15            CALL  I/O ROUTINE
*
         IF (LTR,R15,R15,NZ)
           MVC   WKPRINT,SPACES
           MVC   WKPRINT(35),=CL35'GVBMR93: Vsam Open Error xxxx/xxxx.'
           XGR   R15,R15
           ICM   R15,B'0001',PAVSAMRC+1
           CVD   R15,WKDBLWK
           MVC   WKPRINT+25(4),NUMMSK+8
           MVI   WKPRINT+25,C' '
           ED    WKPRINT+25(4),WKDBLWK+6
           ICM   R15,B'0001',PAVSAMRC
           CVD   R15,WKDBLWK
           MVC   WKPRINT+30(4),NUMMSK+8
           MVI   WKPRINT+30,C' '
           ED    WKPRINT+30(4),WKDBLWK+6
           LA    R2,OUTDCB
           LA    R0,WKPRINT
           PUT   (R2),(R0)
           MVC   WKRETC,=F'12'
           J     DODEQ
         ENDIF
*
**** Read VSAM file record
*
         MVC   PAFUNC,=CL2'RD'
         MVI   PARESDS,C'D'       ESDS Direct access
         MVC   WKFLGKEY,=A(0)     RBA
         MVC   PARECLEN,=H'9999'
         LA    R1,TP90LIST        POINT R1 TO PARAMETER LIST
         L     R15,=V(GVBTP90)    LOAD  ADDRESS OF "GVBTP90"
         BASR  R14,R15            CALL  I/O ROUTINE
         IF (LTR,R15,R15,Z)
           MVC   WKPRINT,SPACES
           MVC   WKPRINT(28),=CL28'GVBMR93: Switch record read:'
           MVC   WKPRINT+29(102),WKFLGREC
           LA    R2,OUTDCB
           LA    R0,WKPRINT
           PUT   (R2),(R0)
         ELSE
           MVC   WKPRINT,SPACES
           MVC   WKPRINT(35),=CL35'GVBMR93: Vsam Read Error xxxx/xxxx.'
           XGR   R15,R15
           ICM   R15,B'0001',PAVSAMRC+1
           CVD   R15,WKDBLWK
           MVC   WKPRINT+25(4),NUMMSK+8
           MVI   WKPRINT+25,C' '
           ED    WKPRINT+25(4),WKDBLWK+6
           ICM   R15,B'0001',PAVSAMRC
           CVD   R15,WKDBLWK
           MVC   WKPRINT+30(4),NUMMSK+8
           MVI   WKPRINT+30,C' '
           ED    WKPRINT+30(4),WKDBLWK+6
           LA    R2,OUTDCB
           LA    R0,WKPRINT
           PUT   (R2),(R0)
           MVC   WKRETC,=F'12'
           J     VSCLOSE
         ENDIF
*
**** Set "our" flag" if not already set -- and flag count if not set
*
         LA    R4,WKFLGREC
         USING FLAGBLK,R4
*
         IF (CLC,WKRESET,EQ,=C'Y')        Reset request
           LH    R1,=H'9995'
           MVC   FLAGCNT,SPACES
           LA    R2,FLAGVAL
           DO FROM=(R1)
             MVI   0(R2),C' '
             LA    R2,1(0,R2)
           ENDDO
           MVC   WKRETC,=F'2'
*
         ELSE                             Untouched record: initialize
           IF (CLC,WKFLGREC(4),EQ,SPACES)
             MVC    FLAGCNT,SWITCHCR
           ENDIF
*
* check if flag is already on: as it shouldn't be !!
*
           IF (CLC,WKFLGREC(4),EQ,SWITCHCR) Set flag for this execution
             LH    R1,SWITCHNO
             BCTR  R1,0
             LA    R2,FLAGVAL(R1)
             IF (CLI,0(R2),EQ,C'X')
               MVC   WKRETC,=F'8'
               MVC   WKPRINT,SPACES
               MVC   WKPRINT(28),=CL28'GVBMR93: Switch already set.'
               LA    R2,OUTDCB
               LA    R0,WKPRINT
               PUT   (R2),(R0)
               J     VSCLOSE
             ELSE
               MVI   0(R2),C'X'
             ENDIF
*
           ELSE                             Inconsistency detected
             MVC   WKPRINT,SPACES
             MVC   WKPRINT(75),=CL75'GVBMR93: Incorrect number of switc+
               hes: xxxx compared to exec parm: xxxx.'
             MVC   WKPRINT+39(4),WKFLGREC
             MVC   WKPRINT+67(4),SWITCHCR
             LA    R2,OUTDCB
             LA    R0,WKPRINT
             PUT   (R2),(R0)
             MVC   WKRETC,=F'8'
             J     VSCLOSE
           ENDIF
*
**** See if all the flags have been set to complete
*
           LH    R1,SWITCHCT
           LA    R2,FLAGVAL
           DO FROM=(R1)
             CLI   0(R2),C' '
             JE    A0040
             LA    R2,1(0,R2)
           ENDDO
           MVC   WKRETC,=F'0'             All switches set
           MVC   WKPRINT,SPACES
           MVC   WKPRINT(30),=CL30'GVBMR93: All switches now set.'
           LA    R2,OUTDCB
           LA    R0,WKPRINT
           PUT   (R2),(R0)
*
           LH    R1,=H'9995'              Now reset record to spaces
           MVC   FLAGCNT,SPACES
           LA    R2,FLAGVAL
           DO FROM=(R1)
             MVI   0(R2),C' '
             LA    R2,1(0,R2)
           ENDDO
           J     A0042
*
A0040      EQU   *
           MVC   WKRETC,=F'4'             One or more switches not set
           MVC   WKPRINT,SPACES
           MVC   WKPRINT(40),=CL40'GVBMR93: This switch set. Others rem+
               ain.'
           LA    R2,OUTDCB
           LA    R0,WKPRINT
           PUT   (R2),(R0)
A0042      EQU   *
         ENDIF
*
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(28),=CL28'GVBMR93: Switch record updt:'
         MVC   WKPRINT+29(102),WKFLGREC
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
         DROP  R4 FLAG
*
**** Write back the vsam record if aok
*
         LA    R4,WKFLGREC
         MVC   PAFUNC,=CL2'WR'
         MVI   PARESDS,C'D'       ESDS Direct access
         MVC   PARECLEN,=H'9999'
         LA    R1,TP90LIST        POINT R1 TO PARAMETER LIST
         L     R15,=V(GVBTP90)    LOAD  ADDRESS OF "GVBTP90"
         BASR  R14,R15            CALL  I/O ROUTINE
         IF (LTR,R15,R15,NZ)
           MVC   WKPRINT,SPACES
           MVC   WKPRINT(35),=CL35'GVBMR93: Vsam Updt Error xxxx/xxxx.'
           XGR   R15,R15
           ICM   R15,B'0001',PAVSAMRC+1
           CVD   R15,WKDBLWK
           MVC   WKPRINT+25(4),NUMMSK+8
           MVI   WKPRINT+25,C' '
           ED    WKPRINT+25(4),WKDBLWK+6
           ICM   R15,B'0001',PAVSAMRC
           CVD   R15,WKDBLWK
           MVC   WKPRINT+30(4),NUMMSK+8
           MVI   WKPRINT+30,C' '
           ED    WKPRINT+30(4),WKDBLWK+6
           LA    R2,OUTDCB
           LA    R0,WKPRINT
           PUT   (R2),(R0)
           MVC   WKRETC,=F'12'
           J     VSCLOSE
         ENDIF
*
**** Close vsam file
*
VSCLOSE  EQU   *
         MVC   PAFUNC,=CL2'CL'    INITIALIZE FUNCTION CODE
         LA    R1,TP90LIST        POINT R1 TO PARAMETER LIST
         L     R15,=V(GVBTP90)    LOAD  ADDRESS OF "GVBTP90"
         BASR  R14,R15            CALL  I/O ROUTINE
         IF (LTR,R15,R15,NZ)
           MVC   WKPRINT,SPACES
           MVC   WKPRINT(35),=CL35'GVBMR93: Vsam Clos Error xxxx/xxxx.'
           XGR   R15,R15
           ICM   R15,B'0001',PAVSAMRC+1
           CVD   R15,WKDBLWK
           MVC   WKPRINT+25(4),NUMMSK+8
           MVI   WKPRINT+25,C' '
           ED    WKPRINT+25(4),WKDBLWK+6
           ICM   R15,B'0001',PAVSAMRC
           CVD   R15,WKDBLWK
           MVC   WKPRINT+30(4),NUMMSK+8
           MVI   WKPRINT+30,C' '
           ED    WKPRINT+30(4),WKDBLWK+6
           LA    R2,OUTDCB
           LA    R0,WKPRINT
           PUT   (R2),(R0)
           MVC   WKRETC,=F'12'
         ENDIF
*
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(28),=CL28'GVBMR93: Execution complete.'
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
**** DEQ Resource
*
DODEQ    EQU   *
         LA  R2,ENQMAJ
         LA  R3,ENQMIN
         DEQ ((R2),(R3),8,SYSTEM),RNL=YES
*
**** Close message file
*
CLOSEFL  EQU   *
         LA    R2,OUTDCB
         MVC   WKREENT(8),OPENPARM
         CLOSE ((R2)),MODE=31,MF=(E,WKREENT)
*
RETURN   DS    0h
         L     R13,4(,R13)
         L     R15,WKRETC
         ST    R15,16(,R13)
*
         AGHI  R8,-8
         L     R0,=A(WORKLEN+8)
         AH    R0,=H'10000'
*
         STORAGE RELEASE,ADDR=(R8),LENGTH=(R0)
*
         lm    r14,r12,12(r13)
*        xr    r15,r15
         br    r14
*
static   loctr
*
outfile  DCB   DSORG=PS,DDNAME=SYSPRINT,MACRF=(PM),DCBE=outfdcbe,      x
               RECFM=FB,LRECL=131
outfile0 EQU   *-outfile
outfdcbe DCBE  RMODE31=BUFF
outfilel EQU   *-outfile
*
MODE31   DS   0XL4
OPENPARM DC    XL8'8000000000000000'
*
GENEVA   DC    CL8'GENEVA  '          MAJOR  ENQ  NODE
PGMNAME  DC    CL8'GVBMR93 '          MINOR  ENQ  NODE
WAITINT  DC    F'100'          WAIT INTERVAL (100 * .01 = 1.0 SEC)
*
SPACES   DC    CL256' '
ZEROS    DC    XL256'00'
NOTNTAB  DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC    XL16'00000000000000000000FFFFFFFFFFFF'
*
NUMMSK   DC    XL12'402020202020202020202021'
*
*
         LTORG ,
                        EJECT
         DCBD  DSORG=PS
*
         IHADCBE
*
JFCBAR   DSECT
         IEFJFCBN LIST=YES
*
         CVT   DSECT=YES
*
         IHAPSA
*
*
         END