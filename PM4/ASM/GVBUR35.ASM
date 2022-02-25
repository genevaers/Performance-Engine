         TITLE 'GVBUR35 - DYNAMIC ALLOCATION INTERFACE'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2005, 2011.
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
*   GVBUR35 - ISSUE A SPECIFIC SVC99 REQUEST.
*
*   PARAMETERS:
*    INPUT   - M35SVC99 DSECT.  CONTAINS INFORMATION NEEDED TO
*              CREATE THE SVC99 TEXT UNITS.  REFER TO THE GVBAUR35
*              MACRO.
*
*    OUTPUT  - M35RCODE IN M35SVC99 CONTAINS THE SVC99 ERROR CODE
*              AFTER AN UNSUCCESSFUL SVC99 REQUEST.
*
*   RETURN:
*    NORMAL  - SVC99 REQUEST SUCCESSFULLY COMPLETED.
*    ERROR   - SVC99 FAILED.  MESSAGE ISSUED SHOWING THE S99ERROR
*              CODE AND THE DDNAME.  THE SVC99 ERROR CODE IS ALSO
*              PLACED IN M35RCODE. CONTROL IS RETURNED TO THE CALLER.
*            - BAD PARAMETER VALUE.  USER ABEND (35) IS ISSUED ALONG
*              WITH A MESSAGE.
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*   R E G I S T E R  U S A G E
*
*   R15   -  STANDARD LINKAGE/WORK REGISTER
*   R14   -  STANDARD LINKAGE/WORK REGISTER
*   R13   -  STANDARD LINKAGE/DYNAMIC STORAGE BASE
*   R12   -  1ST PROGRAM BASE
*   R11   -  2ND PROGRAM BASE
*   R9    -  1ST LEVEL INTERNAL SUBROUTINE LINKAGE
*   R8    -  DYNALLOC REQUEST BLOCK PTR
*   R7    -  M35SVC99 PARAMETER LIST PTR
*   R4    -  CURRENT TEXT UNIT PTR
*   R3    -  TEXT UNIT POINTERS LIST
*   R2    -  WORK REGISTER
*   R1    -  STANDARD LINKAGE/WORK REGISTER
*   R0    -  STANDARD LINKAGE/WORK REGISTER
*
                        EJECT
         YREGS
*
RSABP    EQU   04,4
RSAFP    EQU   08,4
RSA14    EQU   12,4
RSA15    EQU   16,4
RSA0     EQU   20,4
RSA1     EQU   24,4
*
         COPY  GVBUTEQU
                        EJECT
*
         USING WORKSTOR,R13             - DYNAMIC STORAGE
         USING GVBUR35,R12,R11          - PROGRAM BASE(S)
         USING M35SVC99,R7              - CALLING INTERFACE
*
GVBUR35  CSECT
GVBUR35  AMODE 31
GVBUR35  RMODE ANY
         J     START
UR35EYE  GVBEYE GVBUR35
*
START    EQU   *
         STM   R14,R12,RSA14(R13)      SAVE CALLER'S REGS
         LA    R12,0(,R15)             SET PROGRAM BASE
         LA    R11,4095(,R12)
         LA    R11,1(,R11)
*
         L     R7,0(,R1)               --> PARAMETER LIST
*
         L     R0,=A(WORKSLEN)         OBTAIN DYNAMIC STORAGE
         GETMAIN R,LV=(0),LOC=BELOW
*
         LR    R2,R1                   CLEAR GOTTEN STORAGE
         L     R3,=A(WORKSLEN)
         SLL   R4,32
         SLL   R5,32
         MVCL  R2,R4
*
         ST    R1,RSAFP(,R13)          CHAIN THE SAVE AREAS
         ST    R13,RSABP(,R1)
         LR    R13,R1                  --> DYNAMIC STORAGE
*
         LA    R7,0(,R7)               PARM LIST PASSED?
         LTR   R7,R7
         BNZ   MAINLINE                Y - CONTINUE             MLL0005
*
         LHI   R14,NO_PARM_LIST        MESSAGE NUMBER
         B     ABND0100                GO TAKE A DUMP
                        EJECT
MAINLINE EQU   *                   *** BUILD THE RQUEST BLOCK
         LA    R8,REQBLK               --> REQUEST BLOCK
         USING S99RB,R8
         ST    R8,RBPTR                SET THE REQ BLK PTR
         OI    RBPTR,X'80'
         MVI   S99RBLN,REQBLKLN        REQ BLK LENGTH
         OI    S99FLAG1,S99GDGNT       USE RECENT GDG CAT INFO
         LA    R2,TXTPTRS              --> START OF TEXT UNITS
         ST    R2,S99TXTPP
*
MAIN0100 EQU   *                   *** DETERMINE REQUEST TYPE
         MVI   S99VERB,S99VRBAL        ASSUME DSN ALLOCATION
         CLI   M35FCODE,C'1'           ALLOCATION REQUEST?
         BE    MAIN0800                Y - CONTINUE
         MVI   S99VERB,S99VRBUN        INDICATE DEALLOCATION
         CLI   M35FCODE,C'2'           DEALLOCATION REQUEST?
         BE    MAIN0700                Y - CONTINUE
*
         LHI   R14,FUNC_CODE_INVALID   MESSAGE NUMBER
         B     ABND0100                GO TAKE A DUMP
*
MAIN0200 EQU   *                   *** INFORMATION RETRIEVAL
         B     MAIN0900                CONTINUE
*
MAIN0300 EQU   *                   *** DDNAME ALLOCATION
         B     MAIN0900                CONTINUE
*
MAIN0400 EQU   *                   *** REMOVE IN-USE ATTRIBUTE
         B     MAIN0900                CONTINUE
*
MAIN0500 EQU   *                   *** DECONCATENATION
         B     MAIN0900                CONTINUE
*
MAIN0600 EQU   *                   *** CONCATENATION
         B     MAIN0900                CONTINUE
*
MAIN0700 EQU   *                   *** DSN/DD DEALLOCATION
         BAS   R9,V02T0100             BUILD THE TEXT UNITS
         B     MAIN0900                CONTINUE
*
MAIN0800 EQU   *                   *** DSN ALLOCATION
         BAS   R9,V01T0100             BUILD THE TEXT UNITS
*
MAIN0900 EQU   *
         BAS   R9,DYNA0100             ISSUE SVC99 REQUEST
         BAS   R9,RETI0100             CHECK RETURNED INFO
*
FINISH   EQU   *
         SLL   R15,32                  INDICATE A GOOD RETURN
*
RETURN   EQU   *
         LR    R1,R13                  CURRENT SAVE AREA
         LR    R2,R15                  SAVE THE RETURN CODE
         L     R13,RSABP(,R13)         --> CALLER'S SAVE AREA
*
         L     R0,=A(WORKSLEN)         RELEASE DYNAMIC STORAGE
         FREEMAIN R,LV=(0),A=(1)
         XC    RSAFP(L'RSAFP,R13),RSAFP(R13)
*
         LR    R15,R2                  SET THE RETURN CODE
         L     R14,RSA14(,R13)         RESTORE CALLER'S REGS
         LM    R0,R12,RSA0(R13)
         BSM   0,R14
                        EJECT
*-------------------------------------------------------------------*
*        BUILD THE DSN ALLOCATION TEXT UNITS (VERB CODE 01)
*-------------------------------------------------------------------*
V01T0100 EQU   *
         LA    R3,TXTPTRS              --> 1ST TEXT PTR SLOT
         LA    R4,TXTUNITS             --> 1ST TEXT UNIT AREA
*
A0001_10 EQU   *                   *** ENTER KEY 1
         LA    R1,M35DDNAM+L'M35DDNAM-1
         LA    R2,L'M35DDNAM
*
A0001_15 EQU   *
         CLI   0(R1),X'40'             END OF DDNAME?
         BH    A0001_20                Y - CONTINUE
         BCTR  R1,0                    --> NEXT POSITION
         BCT   R2,A0001_15             GO CHECK IT
         B     A0001_90                NONE SPECIFIED
*
A0001_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALDDNAM)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         STH   R2,4(,R4)               TEXT UNIT LENGTH
         BCTR  R2,0                    TEXT UNIT VALUE
         EX    R2,MOVEDDN
         LA    R4,2+2+2+1(R2,R4)       --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0001_90 EQU   *                   *** EXIT KEY 1
*
A0002_10 EQU   *                   *** ENTER KEY 2
         LA    R1,M35DSNAM+L'M35DSNAM-1
         LA    R2,L'M35DSNAM
*
A0002_15 EQU   *
         CLI   0(R1),X'40'             END OF DSN?
         BH    A0002_20                Y - CONTINUE
         BCTR  R1,0                    --> NEXT POSITION
         BCT   R2,A0002_15             GO CHECK IT
*
         CLI   M35SYSOU,X'40'          SYSOUT SPECIFIED?
         BH    A0002_90                Y - CONTINUE AS OKAY
         LHI   R14,DSN_MISSING         MESSAGE NUMBER
         B     ABND0100                GO TAKE A DUMP
*
A0002_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALDSNAM)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
*
         LA    R14,M35DSNAM            ASSUME QUOTES MISSING
         CLI   M35DSNAM,C''''          LEADING QUOTE ???
         BNE   A0002_30                NO - ASSUMPTION CORRECT
         AHI   R14,1                   ADVANCE  SOURCE ADDRESS
         BCTR  R2,0                    DECREMENT LENGTH BY 2
         BCTR  R2,0
*
A0002_30 EQU   *
         STH   R2,4(,R4)               TEXT UNIT LENGTH
         MVI   6(R4),C' '              BLANKOUT  DSNAME (MAX-1)
         MVC   6+1(44-1,R4),6(R4)
         LR    R15,R2                  TEXT UNIT VALUE
         BCTR  R15,0
         EX    R15,MOVEDSN
         ST    R4,DSNADDR              SAVE DSN ADDRESS (-6)
         ST    R2,DSNLEN               SAVE DSN LENGTH
         LA    R4,2+2+2(R4,R2)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0002_90 EQU   *                   *** EXIT KEY 2
*
A0003_10 EQU   *                   *** ENTER KEY 3
         LA    R1,M35MEMBR+L'M35MEMBR-1
         LA    R2,L'M35MEMBR
*
A0003_15 EQU   *
         CLI   0(R1),X'40'             END OF MEMBER NAME?
         BH    A0003_20                Y - CONTINUE
         BCTR  R1,0                    --> NEXT POSITION
         BCT   R2,A0003_15             GO CHECK IT
         B     A0003_90                NONE SPECIFIED
*
A0003_20 EQU   *
         LR    R1,R2                   SAVE LENGTH
         BCTR  R1,0                    DECREMENT   FOR  "EX"
         CLI   M35MEMBR,C'+'           NEW  GENERATION  ???
         BNE   A0003_30                N  - CONVERT GDG
         EX    R1,ZEROGDG              ZERO GENERATION  ???
         BNE   A0003_50                N  - KEEP RELATIVE #/MBR
*
A0003_30 EQU   *
         MVC   CAMLIST(MDLCLSTL),MDLCLIST   COPY MODEL CAM LIST
         L     R14,DSNADDR             LOAD ORIG DSN  ADDR (-6)
         L     R15,DSNLEN              LOAD ORIG DSN   LEN
         LA    R0,6(,R14)              PASS DSN  ADDR
         ST    R0,CAMLIST+4
         LA    R14,6(R14,R15)          ADVANCE TO DSN END
*
         MVI   0(R14),C'('             COPY GDG TO END OF DSN
         EX    R1,MOVEGDG
         LA    R14,1+1(R14,R1)
         MVI   0(R14),C')'
*
         LA    R1,WORKAREA
         ST    R1,CAMLIST+12           PASS   RESULT ADDRESS
         LOCATE CAMLIST                CONVERT DSN  TO G0000V00
         LTR   R15,R15                 SUCCESSFUL ??
         BZ    A0003_40                Y -  CONTINUE
*
         MVC   0(8,R4),SPACES          CLEAR OUT GDG
         B     A0003_50                USE  ORIG LOGIC W/O CONV
*
A0003_40 EQU   *
         L     R14,DSNADDR             LOAD ORIG DSN  ADDR (-6)
         L     R15,DSNLEN              LOAD ORIG DSN   LEN
         AHI   R15,1+8                 UPDATE DSN LEN(G0000V00)
         STH   R15,4(,R14)
*
         AHI   R4,1+8                  --> NEXT TEXT UNIT
         B     A0003_90
*
A0003_50 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALMEMBR)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         STH   R2,4(,R4)               TEXT UNIT LENGTH
         BCTR  R2,0                    TEXT UNIT VALUE
         EX    R2,MOVEMBR
         LA    R4,2+2+2+1(R2,R4)       --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0003_90 EQU   *                   *** EXIT KEY 3
*
A0004_10 EQU   *                   *** ENTER KEY 4
         CLI   M35STATS,X'40'          SPECIFIED?
         BNH   A0004_90                N - SKIP IT
*
A0004_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALSTATS)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'1'           TEXT UNIT LENGTH
         LA    R14,STATST              --> STATUS TABLE
         LA    R15,STATST#             NUM OF ENTRIES
*
A0004_30 EQU   *
         CLC   M35STATS,0(R14)         MATCH?
         BE    A0004_40                Y - CONTINUE
         LA    R14,STATSTLN(,R14)      --> NEXT ENTRY
         BCT   R15,A0004_30            TRY AGAIN
*
         LHI   R14,UR35_PARM_ERR       MESSAGE NUMBER
         MVC   WRKAREA(l'P_STATUS),P_STATUS error msg data
         B     ABND0102                GO TAKE A DUMP
*
A0004_40 EQU   *
         MVC   6(1,R4),L'M35STATS(R14) TEXT UNIT VALUE
         LA    R4,2+2+2+1(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0004_90 EQU   *                   *** EXIT KEY 4
*
A0005_10 EQU   *                   *** ENTER KEY 5
         CLI   M35NDISP,X'40'          SPECIFIED?
         BNH   A0005_90                N - SKIP IT
*
A0005_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALNDISP)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'1'           TEXT UNIT LENGTH
         LA    R14,NCDSPT              --> NORM DISP TABLE
         LA    R15,NCDSPT#             NUM OF ENTRIES
*
A0005_30 EQU   *
         CLC   M35NDISP,0(R14)         MATCH?
         BE    A0005_40                Y - CONTINUE
         LA    R14,NCDSPTLN(,R14)      --> NEXT ENTRY
         BCT   R15,A0005_30            TRY AGAIN
*
         LHI   R14,UR35_PARM_ERR       MESSAGE NUMBER
         MVC   WRKAREA(l'P_NDISP),P_NDISP error msg data
         B     ABND0102                GO TAKE A DUMP
*
A0005_40 EQU   *
         MVC   6(1,R4),L'M35NDISP(R14) TEXT UNIT VALUE
         LA    R4,2+2+2+1(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0005_90 EQU   *                   *** EXIT KEY 5
*
A0006_10 EQU   *                   *** ENTER KEY 6
         CLI   M35CDISP,X'40'          SPECIFIED?
         BNH   A0006_90                N - SKIP IT
*
A0006_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALCDISP)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'1'           TEXT UNIT LENGTH
         LA    R14,NCDSPT              --> COND DISP TABLE
         LA    R15,NCDSPT#             NUM OF ENTRIES
*
A0006_30 EQU   *
         CLC   M35CDISP,0(R14)         MATCH?
         BE    A0006_40                Y - CONTINUE
         LA    R14,NCDSPTLN(,R14)      --> NEXT ENTRY
         BCT   R15,A0006_30            TRY AGAIN
*
         LHI   R14,UR35_PARM_ERR       MESSAGE NUMBER
         MVC   WRKAREA(l'P_CDISP),P_CDISP error msg data
         B     ABND0102                GO TAKE A DUMP
*
A0006_40 EQU   *
         MVC   6(1,R4),L'M35CDISP(R14) TEXT UNIT VALUE
         LA    R4,2+2+2+1(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0006_90 EQU   *                   *** EXIT KEY 6
*
A0007_10 EQU   *                   *** ENTER KEY 7
         CLI   M35TRKS,C'Y'            SPECIFIED?
         BNE   A0007_90                N - SKIP IT
*
A0007_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALTRK)      TEXT UNIT KEY
         MVC   2(2,R4),=H'0'           TEXT UNIT NUMBER
         LA    R4,2+2(,R4)             --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0007_90 EQU   *                   *** EXIT KEY 7
*
A0008_10 EQU   *                   *** ENTER KEY 8
         CLI   M35CYLS,C'Y'            SPECIFIED?
         BNE   A0008_90                N - SKIP IT
*
A0008_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALCYL)      TEXT UNIT KEY
         MVC   2(2,R4),=H'0'           TEXT UNIT NUMBER
         LA    R4,2+2(,R4)             --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0008_90 EQU   *                   *** EXIT KEY 8
*
A000A_10 EQU   *                   *** ENTER KEY "A"
         OC    M35PRIME,M35PRIME       SPECIFIED?
         BZ    A000A_90                N - SKIP IT
*
A000A_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALPRIME)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'3'           TEXT UNIT LENGTH
         MVI   6(R4),X'00'             TEXT UNIT VALUE
         MVC   7(2,R4),M35PRIME
         LA    R4,2+2+2+3(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A000A_90 EQU   *                   *** EXIT KEY "A"
*
A000B_10 EQU   *                   *** ENTER KEY "B"
         OC    M35SECND,M35SECND       SPECIFIED?
         BZ    A000B_90                N - SKIP IT
*
A000B_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALSECND)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'3'           TEXT UNIT LENGTH
         MVI   6(R4),X'00'             TEXT UNIT VALUE
         MVC   7(2,R4),M35SECND
         LA    R4,2+2+2+3(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A000B_90 EQU   *                   *** EXIT KEY "B"
*
A000C_10 EQU   *                   *** ENTER KEY "C"
         OC    M35DIR,M35DIR           SPECIFIED?
         BZ    A000C_90                N - SKIP IT
*
A000C_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALDIR)      TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'3'           TEXT UNIT LENGTH
         MVI   6(R4),X'00'             TEXT UNIT VALUE
         MVC   7(2,R4),M35DIR
         LA    R4,2+2+2+3(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A000C_90 EQU   *                   *** EXIT KEY "C"
*
A000D_10 EQU   *                   *** ENTER KEY "D"
         CLI   M35RLSE,C'Y'            SPECIFIED?
         BNE   A000D_90                N - SKIP IT
*
A000D_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALRLSE)     TEXT UNIT KEY
         MVC   2(2,R4),=H'0'           TEXT UNIT NUMBER
         LA    R4,2+2(,R4)             --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A000D_90 EQU   *                   *** EXIT KEY "D"
*
A0010_10 EQU   *                   *** ENTER KEY 10
         CLI   M35VLSER,X'40'          SPECIFIED?
         BNH   A0010_90                N - SKIP IT
*
A0010_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALVLSER)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'6'           TEXT UNIT LENGTH
         MVC   6(6,R4),M35VLSER        TEXT UNIT VALUE
         LA    R4,2+2+2+6(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0010_90 EQU   *                   *** EXIT KEY 10
*
A0012_10 EQU   *                   *** ENTER KEY 12
         OC    M35VLSEQ,M35VLSEQ       SPECIFIED?
         BZ    A0012_90                N - SKIP IT
*
A0012_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALVLSEQ)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'2'           TEXT UNIT LENGTH
         MVC   6(2,R4),M35VLSEQ        TEXT UNIT VALUE
         LA    R4,2+2+2+2(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0012_90 EQU   *                   *** EXIT KEY 12
*
A0013_10 EQU   *                   *** ENTER KEY 13
         OC    M35VLCNT,M35VLCNT       SPECIFIED?
         BZ    A0013_90                N - SKIP IT
*
A0013_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALVLCNT)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'1'           TEXT UNIT LENGTH
         MVC   6(1,R4),M35VLCNT+1      TEXT UNIT VALUE
         LA    R4,2+2+2+1(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0013_90 EQU   *                   *** EXIT KEY 13
*
A0015_10 EQU   *                   *** ENTER KEY 15
         LA    R1,M35UNIT+L'M35UNIT-1
         LA    R2,L'M35UNIT
*
A0015_15 EQU   *
         CLI   0(R1),X'40'             END OF UNIT NAME?
         BH    A0015_20                Y - CONTINUE
         BCTR  R1,0                    --> NEXT POSITION
         BCT   R2,A0015_15             GO CHECK IT
         B     A0015_90                NONE SPECIFIED
*
A0015_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALUNIT)     TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         STH   R2,4(,R4)               TEXT UNIT LENGTH
         BCTR  R2,0                    TEXT UNIT VALUE
         EX    R2,MOVEUNT
         LA    R4,2+2+2+1(R2,R4)       --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0015_90 EQU   *                   *** EXIT KEY 15
*
A0018_10 EQU   *                   *** ENTER KEY 18
         CLI   M35SYSOU,X'40'          SPECIFIED?
         BNH   A0018_90                N - SKIP IT
         CLI   M35SYSOU,C'*'           DEFAULT REQUESTED?
         BE    A0018_30
         CLI   M35SYSOU,C'$'
         BE    A0018_30                Y - USE DEFAULT MSGCLASS
*
A0018_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALSYSOU)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'1'           TEXT UNIT LENGTH
         MVC   6(1,R4),M35SYSOU        TEXT UNIT VALUE
         LA    R4,2+2+2+1(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
         B     A0018_90                N - SKIP IT
*
A0018_30 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALSYSOU)    TEXT UNIT KEY
         MVC   2(2,R4),=H'0'           TEXT UNIT NUMBER
         LA    R4,2+2(,R4)             --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0018_90 EQU   *                   *** EXIT KEY 18
*
A001B_10 EQU   *                   *** ENTER KEY 1B
         OC    M35OUTLM,M35OUTLM       SPECIFIED?
         BZ    A001B_90                N - SKIP IT
*
A001B_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALOUTLM)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'3'           TEXT UNIT LENGTH
         MVC   6(3,R4),M35OUTLM+1      TEXT UNIT VALUE
         LA    R4,2+2+2+3(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A001B_90 EQU   *                   *** EXIT KEY 1B
*
A001C_10 EQU   *                   *** ENTER KEY 1C
         CLI   M35CLOSE,C'Y'           SPECIFIED?
         BNE   A001C_90                N - SKIP IT
*
A001C_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALCLOSE)    TEXT UNIT KEY
         MVC   2(2,R4),=H'0'           TEXT UNIT NUMBER
         LA    R4,2+2(,R4)             --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A001C_90 EQU   *                   *** EXIT KEY 1C
*
A001D_10 EQU   *                   *** ENTER KEY 1D
         OC    M35COPYS,M35COPYS       SPECIFIED?
         BZ    A001D_90                N - SKIP IT
*
A001D_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALCOPYS)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'1'           TEXT UNIT LENGTH
         MVC   6(1,R4),M35COPYS+1      TEXT UNIT VALUE
         LA    R4,2+2+2+1(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A001D_90 EQU   *                   *** EXIT KEY 1D
*
A0023_10 EQU   *                   *** ENTER KEY 23
         OC    M35RETPD,M35RETPD       SPECIFIED?
         BZ    A0023_90                N - SKIP IT
*
A0023_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALRETPD)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'2'           TEXT UNIT LENGTH
         MVC   6(2,R4),M35RETPD        TEXT UNIT VALUE
         LA    R4,2+2+2+2(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0023_90 EQU   *                   *** EXIT KEY 23
*
A0030_10 EQU   *                   *** ENTER KEY 30
         OC    M35BLKSZ,M35BLKSZ       SPECIFIED?
         BZ    A0030_90                N - SKIP IT
*
A0030_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALBLKSZ)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'2'           TEXT UNIT LENGTH
         MVC   6(2,R4),M35BLKSZ        TEXT UNIT VALUE
         LA    R4,2+2+2+2(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0030_90 EQU   *                   *** EXIT KEY 30
*
A003C_10 EQU   *                   *** ENTER KEY 3C
         CLI   M35DSORG,X'40'          SPECIFIED?
         BNH   A003C_90                N - SKIP IT
*
A003C_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALDSORG)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'2'           TEXT UNIT LENGTH
         LA    R14,DSORGT              --> DSORG TABLE
         LA    R15,DSORGT#             NUM OF ENTRIES
*
A003C_30 EQU   *
         CLC   M35DSORG,0(R14)         MATCH?
         BE    A003C_40                Y - CONTINUE
         LA    R14,DSORGTLN(,R14)      --> NEXT ENTRY
         BCT   R15,A003C_30            TRY AGAIN
*
         LHI   R14,UR35_PARM_ERR       MESSAGE NUMBER
         MVC   WRKAREA(l'P_DSORG),P_DSORG error msg data
         B     ABND0102                GO TAKE A DUMP
*
A003C_40 EQU   *
         MVC   6(2,R4),L'M35DSORG(R14) TEXT UNIT VALUE
         LA    R4,2+2+2+2(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A003C_90 EQU   *                   *** EXIT KEY 3C
*
A0040_10 EQU   *                   *** ENTER KEY 40
         LH    R14,M35KYLEN            GET KEY LENGTH
         LTR   R14,R14                 KEY LENGTH SPECIFIED?
         BNP   A0040_90                N - SKIP IT
         CH    R14,=H'256'             VALID KEY SIZE?
         BL    A0040_20                Y - CONTINUE
         LHI   R14,UR35_PARM_ERR       MESSAGE NUMBER
         MVC   WRKAREA(l'P_KYLEN),P_KYLEN error msg data
         B     ABND0102                GO TAKE A DUMP
*
A0040_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALKYLEN)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'1'           TEXT UNIT LENGTH
         STC   R14,6(,R4)              TEXT UNIT VALUE
         LA    R4,2+2+2+1(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0040_90 EQU   *                   *** EXIT KEY 40
*
A0042_10 EQU   *                   *** ENTER KEY 42
         OC    M35BLKSZ,M35BLKSZ       SPECIFIED?
         BZ    A0042_90                N - SKIP IT
*
A0042_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALLRECL)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'2'           TEXT UNIT LENGTH
         MVC   6(2,R4),M35LRECL        TEXT UNIT VALUE
         LA    R4,2+2+2+2(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0042_90 EQU   *                   *** EXIT KEY 42
*
A0049_10 EQU   *                   *** ENTER KEY 49
         CLI   M35RECFM,X'40'          SPECIFIED?
         BNH   A0049_90                N - SKIP IT
*
A0049_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALRECFM)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'1'           TEXT UNIT LENGTH
         LA    R14,RECFMT              --> RECFM TABLE
         LA    R15,RECFMT#             NUM OF ENTRIES
*
A0049_30 EQU   *
         CLC   M35RECFM,0(R14)         MATCH?
         BE    A0049_40                Y - CONTINUE
         LA    R14,RECFMTLN(,R14)      --> NEXT ENTRY
         BCT   R15,A0049_30                TRY AGAIN
*
         LHI   R14,UR35_PARM_ERR       MESSAGE NUMBER
         MVC   WRKAREA(l'P_RECFM),P_RECFM error msg data
         B     ABND0102                GO TAKE A DUMP
*
A0049_40 EQU   *
         MVC   6(1,R4),L'M35RECFM(R14) TEXT UNIT VALUE
         LA    R4,2+2+2+1(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0049_90 EQU   *                   *** EXIT KEY 49
*
A0055_10 EQU   *                   *** ENTER KEY 55
         CLI   M35DDNAM,X'40'          DDNAME SPECIFIED?
         BH    A0055_90                Y - NOTHING TO RETURN
*
A0055_20 EQU   *
         LA    R1,RTDDNV01             SET TEXT UNIT PTR
         ST    R1,0(,R3)
         MVC   RTDDNKEY,=Y(DALRTDDN)   TEXT UNIT KEY
         MVC   RTDDNNUM,=H'1'          TEXT UNIT NUMBER
         MVC   RTDDNLEN,=H'8'          TEXT UNIT LENGTH
         MVI   RTDDNVAL,X'40'          TEXT UNIT VALUE
         MVC   RTDDNVAL+1(L'RTDDNVAL-1),RTDDNVAL
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0055_90 EQU   *                   *** EXIT KEY 55
*
A0059_10 EQU   *                   *** ENTER KEY 58
         CLI   M35SHOLD,X'40'          SPECIFIED?
         BNH   A0059_90                N - SKIP IT
*
A0059_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALSHOLD)    TEXT UNIT KEY
         MVC   2(2,R4),=H'0'           TEXT UNIT NUMBER
         LA    R4,2+2(,R4)             --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A0059_90 EQU   *                   *** EXIT KEY 58
*
A006D_10 EQU   *                   *** ENTER KEY 6D
         CLI   M35EXPDL,X'40'          SPECIFIED?
         BNH   A006D_90                N - SKIP IT
*
A006D_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DALEXPDL)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'7'           TEXT UNIT LENGTH
         MVC   6(7,R4),M35EXPDL        TEXT UNIT VALUE
         LA    R4,2+2+2+7(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A006D_90 EQU   *                   *** EXIT KEY 6D
*
A800B_10 EQU   *                   *** ENTER KEY 800B
         CLI   M35RECO,X'40'           SPECIFIED?
         BNH   A800B_90                N - SKIP IT
*
A800B_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=AL2(DALRECO)   TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'1'           TEXT UNIT LENGTH
         LA    R14,RECOT               --> VSAM TYPE TABLE
         LA    R15,RECOT#              NUM OF ENTRIES
*
A800B_30 EQU   *
         CLC   M35RECO,0(R14)          MATCH?
         BE    A800B_40                Y - CONTINUE
         LA    R14,RECOTLN(,R14)       --> NEXT ENTRY
         BCT   R15,A800B_30            TRY AGAIN
*
         LHI   R14,UR35_PARM_ERR       MESSAGE NUMBER
         MVC   WRKAREA(l'P_RECO),P_RECO error msg data
         B     ABND0102                GO TAKE A DUMP
*
A800B_40 EQU   *
         MVC   6(1,R4),L'M35RECO(R14)  TEXT UNIT VALUE
         LA    R4,2+2+2+1(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A800B_90 EQU   *                   *** EXIT KEY 800B
*
A800C_10 EQU   *                   *** ENTER KEY 800C
         LH    R14,M35KEYO             GET KEY POSITION
         LTR   R14,R14                 POSITION SPECIFIED?
         BNP   A800C_90                N - SKIP IT
         BCTR  R14,0                   CONVERT TO OFFSET
         CH    R14,=H'256'             VALID KEY POSITION?
         BL    A800C_20                Y - CONTINUE
         LHI   R14,UR35_PARM_ERR       MESSAGE NUMBER
         MVC   WRKAREA(l'P_KEYO),P_KEYO error msg data
         B     ABND0102                GO TAKE A DUMP
*
A800C_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=AL2(DALKEYO)   TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         MVC   4(2,R4),=H'1'           TEXT UNIT LENGTH
         STC   R14,6(,R4)              TEXT UNIT VALUE
         LA    R4,2+2+2+1(,R4)         --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
A800C_90 EQU   *                   *** EXIT KEY 800C
*
V01T0800 EQU   *
         LA    R2,TXTPTRS              --> 1ST TEXT UNIT PTR
         CR    R3,R2                   ANY TEXT UNITS BUILT?
         BNH   V01T0999                N - ISSUE ERROR
         S     R3,=F'4'                MARK LAST TEXT PTR
         OI    0(R3),X'80'
*
V01T0900 EQU   *
         BR    R9
*
V01T0999 EQU   *
         LHI   R14,NO_PARMS           MESSAGE NUMBER
         B     ABND0100                GO TAKE A DUMP
                        EJECT
*-------------------------------------------------------------------*
*        BUILD THE DEALLOCATION TEXT UNITS
*-------------------------------------------------------------------*
V02T0100 EQU   *
         LA    R3,TXTPTRS              --> 1ST TEXT PTR SLOT
         LA    R4,TXTUNITS             --> 1ST TEXT UNIT AREA
*
B0001_10 EQU   *                   *** ENTER KEY 1
         LA    R1,M35DDNAM+L'M35DDNAM-1
         LA    R2,L'M35DDNAM
*
B0001_15 EQU   *
         CLI   0(R1),X'40'             END OF DDNAME?
         BH    B0001_20                Y - CONTINUE
         BCTR  R1,0                    --> NEXT POSITION
         BCT   R2,B0001_15             GO CHECK IT
         B     B0001_90                NONE SPECIFIED
*
B0001_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DUNDDNAM)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         STH   R2,4(,R4)               TEXT UNIT LENGTH
         BCTR  R2,0                    TEXT UNIT VALUE
         EX    R2,MOVEDDN
         LA    R4,2+2+2+1(R2,R4)       --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
B0001_90 EQU   *                   *** EXIT KEY 1
*
B0002_10 EQU   *                   *** ENTER KEY 2
         LA    R1,M35DSNAM+L'M35DSNAM-1
         LA    R2,L'M35DSNAM
*
B0002_15 EQU   *
         CLI   0(R1),X'40'             END OF DSN?
         BH    B0002_20                Y - CONTINUE
         BCTR  R1,0                    --> NEXT POSITION
         BCT   R2,B0002_15             GO CHECK IT
         B     B0002_90
*
B0002_20 EQU   *
         ST    R4,0(,R3)               SET TEXT UNIT PTR
         MVC   0(2,R4),=Y(DUNDSNAM)    TEXT UNIT KEY
         MVC   2(2,R4),=H'1'           TEXT UNIT NUMBER
         STH   R2,4(,R4)               TEXT UNIT LENGTH
         BCTR  R2,0                    TEXT UNIT VALUE
         LA    R14,M35DSNAM
         EX    R2,MOVEDSN
         LA    R4,2+2+2+1(R2,R4)       --> NEXT TEXT UNIT
         LA    R3,4(,R3)               --> NEXT TEXT PTR
B0002_90 EQU   *                   *** EXIT KEY 2
*
V02T0800 EQU   *
         LA    R2,TXTPTRS              --> 1ST TEXT UNIT PTR
         CR    R3,R2                   ANY TEXT UNITS BUILT?
         BNH   V02T0999                N - ISSUE ERROR
         S     R3,=F'4'                MARK LAST TEXT PTR
         OI    0(R3),X'80'
*
V02T0900 EQU   *
         BR    R9
*
V02T0999 EQU   *
         LHI   R14,NO_PARMS            MESSAGE NUMBER
         B     ABND0100                GO TAKE A DUMP
                        EJECT
*-------------------------------------------------------------------*
*        ISSUE THE DYNAMIC ALLOCATION REQUEST
*-------------------------------------------------------------------*
DYNA0100 EQU   *
         LA    R1,RBPTR                --> REQUEST BLOCK PTR
         DYNALLOC                      ISSUE THE SVC 99
*
         MVC   M35RCODE,S99ERROR       SAVE ERROR CODE
         LTR   R15,R15                 SUCCESSFUL REQUEST?
         BNZ   DYNA0999                N - ISSUE ERROR
*
DYNA0900 EQU   *
         BR    R9
*
DYNA0999 EQU   *
         LHI   R14,DYNALLOC_FAIL       MESSAGE NUMBER
         B     EROR0100                EXIT WITH ERROR
                        EJECT
*-------------------------------------------------------------------*
*        SEND BACK TO THE CALLER ANY RETURNED INFORMATION
*-------------------------------------------------------------------*
RETI0100 EQU   *
         CLI   RTDDNVAL,X'40'          DDNAME RETURNED?
         BNH   RETI0900                N - CONTINUE
         MVC   M35DDNAM,RTDDNVAL       RETURN THE DDNAME
*
RETI0900 EQU   *
         BR    R9
                        EJECT
*-------------------------------------------------------------------*
*        ISSUE AN ERROR MESSAGE AND RETURN TO THE CALLING PROGRAM
*-------------------------------------------------------------------*
EROR0100 EQU   *
*                                      FORMAT THE ERROR CODE
         XC    WRKAREA,WRKAREA
*
         UNPK  WRKAREA(L'S99ERROR*2+1),S99ERROR(L'S99ERROR+1)
         MVN   WRKAREA+8(L'S99ERROR*2),WRKAREA
         TR    WRKAREA+8(L'S99ERROR*2),HEXCHAR
*                                      FORMAT THE INFO CODE
         UNPK  WRKAREA2(L'S99INFO*2+1),S99INFO(L'S99INFO+1)
         MVN   WRKAREA2+8(L'S99INFO*2),WRKAREA2
         TR    WRKAREA2+8(L'S99INFO*2),HEXCHAR
*
         GVBMSG WTO,MSGNO=(R14),SUBNO=4,                               +
               SUB1=(modname,8),                                       +
               SUB2=(M35DDNAM,8),                                      +
               SUB3=(WRKAREA+8,L'S99ERROR*2),                          +
               SUB4=(WRKAREA2+8,L'S99INFO*2),                          +
               MF=(E,MSG_AREA)
*
         LA    R15,16                  INDICATE AN ERROR
         B     RETURN                  EXIT THIS PROGRAM
                        EJECT
*-------------------------------------------------------------------*
*        ISSUE AN ERROR MESSAGE AND TERMINATE THE PROGRAM
*-------------------------------------------------------------------*
ABND0100 EQU   *
*
         GVBMSG WTO,MSGNO=(r14),SUBNO=1,                               +
               SUB1=(modname,8),                                       +
               MF=(E,MSG_AREA)
         B     ABND0999
*
ABND0102 EQU   *
* message with 2 parameters
         GVBMSG WTO,MSGNO=(r14),SUBNO=2,                               +
               SUB1=(modname,8),                                       +
               SUB2=(WRKAREA,L'P_STATUS),                              +
               MF=(E,MSG_AREA)
*
*
ABND0999 EQU   *
         ABEND 35,DUMP                 REGURGITATE
                        EJECT
STATSTOR DS    0D                      *----------------------------*
*                                      * STATIC STORAGE
         DC    CL8'GVBUR35'            *----------------------------*
*
P_STATUS dc    C'M35STATS'
P_NDISP  dc    C'M35NDISP'
P_CDISP  dc    C'M35CDISP'
P_RECFM  dc    C'M35RECFM'
P_DSORG  dc    C'M35DSORG'
P_RECO   dc    C'M35RECO '
P_KYLEN  dc    C'M35KYLEN'
P_KEYO   dc    C'M35KEYO '
*
         DS    0H                      EXECUTED INSTRUCTIONS
MOVEDDN  MVC   6(0,R4),M35DDNAM
MOVEDSN  MVC   6(0,R4),0(R14)
MOVEMBR  MVC   6(0,R4),M35MEMBR
MOVEUNT  MVC   6(0,R4),M35UNIT
*
HEXCHAR  DC    CL16'0123456789ABCDEF'
*
SPACES   DC    CL8' '
ZEROES   DC    CL8'+0000000'
ZEROGDG  CLC   M35MEMBR(0),ZEROES
MOVEGDG  MVC   1(0,R14),M35MEMBR
*
MDLCLIST CAMLST NAME,0,,0
MDLCLSTL EQU    *-MDLCLIST
*
RECFMT   DS    0H                      RECFM MAPPINGS
         DC    C'FB',X'90'
RECFMTLN EQU   (*-RECFMT)
         DC    C'F ',X'80'
         DC    C'VB',X'50'
         DC    C'V ',X'40'
         DC    C'FA',X'94'
         DC    C'U ',X'C0'
RECFMT#  EQU   ((*-RECFMT)/RECFMTLN)
*
DSORGT   DS    0H                      DSORG MAPPINGS
         DC    C'PS  ',X'4000'
DSORGTLN EQU   (*-DSORGT)
         DC    C'PO  ',X'0200'
         DC    C'DA  ',X'2000'
         DC    C'VSAM',X'0008'
DSORGT#  EQU   ((*-DSORGT)/DSORGTLN)
*
RECOT    DS    0H                      VSAM TYPE MAPPINGS
         DC    C'KSDS',X'80'
RECOTLN  EQU   (*-RECOT)
         DC    C'ESDS',X'40'
         DC    C'RRDS',X'20'
         DC    C'LSDS',X'10'
RECOT#   EQU   ((*-RECOT)/RECOTLN)
*
STATST   DS    0H                      STATUS MAPPINGS
         DC    C'OLD',X'01'
STATSTLN EQU   (*-STATST)
         DC    C'MOD',X'02'
         DC    C'NEW',X'04'
         DC    C'SHR',X'08'
STATST#  EQU   ((*-STATST)/STATSTLN)
*
NCDSPT   DS    0H                      NORM/COND DISP MAPPINGS
         DC    C'UNCATLG',X'01'
NCDSPTLN EQU   (*-NCDSPT)
         DC    C'CATLG  ',X'02'
         DC    C'DELETE ',X'04'
         DC    C'KEEP   ',X'08'
NCDSPT#  EQU   ((*-NCDSPT)/NCDSPTLN)
*
LABELT   DS    0H                      LABEL MAPPINGS
         DC    C'SL ',X'02'
LABELTLN EQU   (*-LABELT)
         DC    C'NL ',X'01'
         DC    C'BLP',X'10'
         DC    C'LTM',X'21'
LABELT#  EQU   ((*-LABELT)/LABELTLN)
*
DENSTT   DS    0H                      DENSITY MAPPINGS
         DC    C'4',X'D3'
DENSTTLN EQU   (*-DENSTT)
         DC    C'3',X'C3'
         DC    C'2',X'83'
DENSTT#  EQU   ((*-DENSTT)/DENSTTLN)
*
OPTCDT   DS    0H                      OPTCD MAPPINGS
         DC    C'J ',X'01'
OPTCDTLN EQU   (*-OPTCDT)
         DC    C'Q ',X'08'
OPTCDT#  EQU   ((*-OPTCDT)/OPTCDTLN)
*
TRTCHT   DS    0H                      TRTCH MAPPINGS
         DC    C'NONE',X'04'
TRTCHTLN EQU   (*-TRTCHT)
         DC    C'COMP',X'08'
         DC    C'C   ',X'13'
         DC    C'E   ',X'23'
         DC    C'ET  ',X'2B'
         DC    C'T   ',X'3B'
TRTCHT#  EQU   ((*-TRTCHT)/TRTCHTLN)
*
                        EJECT
         LTORG
                        EJECT
WORKSTOR DSECT                         *----------------------------*
*                                      * WORKING STORAGE
*                                      *----------------------------*
*
SAVEAREA DS    18F                     REGISTER SAVE AREA
*
MSG_AREA GVBMSG MF=L
*
WRKAREA  DS    CL16                    WORK AREA
WRKAREA2 DS    CL16                    WORK AREA
*
RBPTR    DS    F                       REQUEST BLOCK PTR
REQBLK   DS    CL(S99RBEND-S99RB)      SVC99 REQUEST BLOCK
REQBLKLN EQU   L'REQBLK                REQUEST BLOCK LENGTH
TXTPTRS  DS    64F                     TEXT UNIT POINTERS
*
TXTUNITS DS    CL1024                  ACTUAL TEXT UNITS
*
*                                      RETURNED INFO TEXT UNITS
RTDDNV01 DS    0F                      VERB 1 DDNAME RETURN
RTDDNKEY DS    H                        - DDN RETURN KEY
RTDDNNUM DS    H                        - DDN RETURN NUMBER
RTDDNLEN DS    H                        - DDN RETURN LENGTH
RTDDNVAL DS    CL8                      - DDN RETURN VALUE
*
DSNADDR  DS    A
DSNLEN   DS    F
CAMLIST  DS   (MDLCLSTL)X              CAM LIST FOR "LOCATE"
*
         DS    0D                      PROGRAM WORK AREA
WORKAREA DS    CL320
WORKSLEN EQU   (*-WORKSTOR)            DYNAMIC STORAGE LENGTH
                        EJECT
         GVBAUR35                      UR35 CALLING INTERFACE
*
         IEFZB4D0                      SVC99 REQUEST BLOCK
*
         IEFZB4D2                      TEXT UNIT MNEMONICS
*
         END