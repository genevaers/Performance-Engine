         TITLE 'GVBUR33 - SYMBOLIC VARIABLE SUBSTITUTION'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2003, 2021.
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK REGISTER                                *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - TEMPORARY WORK REGISTER                                *
*            - RETURN    ADDR                                         *
*                                                                     *
*        R13 - REGISTER  SAVE AREA  ADDRESS                           *
*                                                                     *
*        R12 - PROGRAM   BASE REGISTER                                *
*        R11 - PROGRAM   BASE REGISTER                                *
*                                                                     *
*        R10 -                                                        *
*                                                                     *
*        R9  - PARAMETER LIST ADDRESS                                 *
*                                                                     *
*        R8  -                                                        *
*        R7  -                                                        *
*        R6  -                                                        *
*        R5  -                                                        *
*                                                                     *
*        R4  - SYMBOLIC  VARIABLE  TABLE  ENTRY  ADDRESS              *
*                                                                     *
*        R3  - END-OF-STRING  ADDRESS                                 *
*                                                                     *
*        R2  - CURRENT STRING ADDRESS                                 *
*                                                                     *
*        R1  - PARAMETER LIST ADDRESS                                 *
*        R0  - TEMPORARY WORK REGISTER                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*      E N V I R O N M E N T   V A R I A B L E   D E F I N I T I O N  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
ENVVTBL  DSECT                 ENVIRONMENT VARIABLES
*
ENVVNEXT DS    AL04            NEXT ENV    VARIABLE NAME
ENVVNLEN DS    HL02            VARIABLE    NAME     LENGTH (-1)
ENVVNAME DS    CL16            VARIABLE    NAME
ENVVVLEN DS    HL02            VARIABLE    VALUE    LENGTH (-1)
ENVVVALU DS    CL128           VARIABLE    VALUE
*
ENVVTLEN EQU   *-ENVVTBL       ENVIRONMENT VARIABLE DATA LENGTH
*
         ORG
                        SPACE 5
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*      E N V I R O N M E N T   V A R I A B L E   D E F I N I T I O N  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
PARMLIST DSECT                 PARAMETER LIST
*
PARMSTRA DS    A               SOURCE  STRING ADDRESS
PARMSTRL DS    A               SOURCE  STRING LENGTH  (HALFWORD)
PARMENVA DS    A               ENVIRONMENT   VARIABLE  POINTER ADDRESS
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         YREGS
                        SPACE 3
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        REGISTER SAVE AREA OFFSETS:                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
RSABP    EQU   4               REGISTER SAVE AREA CHAIN BACKWARD PTR
RSAFP    EQU   8               REGISTER SAVE AREA CHAIN FORWARD  PTR
RSA14    EQU   12              RETURN   ADDRESS
RSA15    EQU   16              ENTRY    POINT
RSA0     EQU   20
RSA1     EQU   24              PARAMETER LIST   ADDRESS
*
                        EJECT
*
GVBUR33  RMODE ANY
GVBUR33  AMODE 31
*
GVBUR33  CSECT
         J     CODE
UR33EYE  GVBEYE GVBUR33
*
***********************************************************************
*  SAVE  CALLER'S REGISTER CONTENTS                                   *
*  CHAIN REGISTER SAVE     AREAS  & SAVE  BASE   ADDRESSES            *
***********************************************************************
CODE     STM   R14,R12,RSA14(R13) SAVE CALLER'S  REGISTERS
*
         LA    R11,0(,R15)        SET  PROGRAM   BASE REGISTERS
         USING GVBUR33,R11,R12        (STRIP HIGH BIT)
         LR    R12,R11
         AHI   R12,4096
*
         LR    R9,R1              SAVE PARAMETER LIST ADDRESS
         USING PARMLIST,R9
*
***********************************************************************
*  REPLACE SYMBOLIC VARIABLES WITH ACTUAL VALUES                      *
***********************************************************************
         L     R2,PARMSTRA        LOAD  STARTING SCAN   ADDRESS
*
         L     R3,PARMSTRL        COMPUTE END-OF-STRING ADDRESS
         LH    R3,0(,R3)
         LA    R3,0(R2,R3)
*
SYMBLOOP CLI   0(R2),C'$'         SYMBOLIC  VARIABLE ???
         BRNE  SYMBSCAN           NO - ADVANCE  TO NEXT CHARACTER
*
         L     R4,PARMENVA        LOAD ENVIRONMENT VARIABLE  LIST ADDR
         L     R4,0(,R4)
         USING ENVVTBL,R4
*
SYMBNEXT LTR   R4,R4              END-OF-LIST    ???
         BRNP  SYMBSCAN           YES - VARIABLE NAME NOT ON LIST
*
         LH    R1,ENVVNLEN        LOAD  VARIABLE NAME LENGTH (-1)
         EX    R1,SYMBENVV        MATCHING  NAME  ???
         BRE   SYMBSYMB           YES - SUBSTITUTE VALUE FOR SYMB
         L     R4,ENVVNEXT        ADVANCE TO  NEXT SYMBOL ON LIST
         BRC   15,SYMBNEXT        LOOP
*
SYMBENVV CLC   ENVVNAME(0),0(R2)  * * * *  E X E C U T E D  * * * *
SYMBMVAL MVC   0(0,R2),ENVVVALU   * * * *  E X E C U T E D  * * * *
*
SYMBSYMB LH    R0,ENVVVLEN        COMPUTE SYMBOL VS VALUE LENGTH DIFF
         SR    R0,R1
         BRZ   SYMBSUBS           SAME   LENGTHS -  SUBSTITUTE  VALUE
         BRP   SYMBSHFT           SHIFT RIGHT IF VALUE  LONGER
*
***********************************************************************
*  SHIFT REMAINDER OF STRING LEFT TO FOLLOW SYMBOLIC VALUE            *
***********************************************************************
         LCR   R0,R0              COMPLEMENT SHIFT DIST(CONVERT TO POS)
         LA    R1,1(R2,R1)        COMPUTE SOURCE  ADDRESS
         LR    R14,R1             COMPUTE TARGET  ADDRESS
         SR    R14,R0
*
         LR    R15,R3             COMPUTE SHIFT   LENGTH
         SR    R15,R1
*
SYMBLEFT MVC   0(1,R14),0(R1)
         LA    R14,1(,R14)
         LA    R1,1(,R1)
         BRCT  R15,SYMBLEFT
*
***********************************************************************
*  CLEAR OUT TAIL OF STRING AFTER LEFT SHIFT                          *
***********************************************************************
SYMBSPAC BCTR  R3,0
         MVI   0(R3),C' '
         BRCT  R0,SYMBSPAC
         BRC   15,SYMBSUBS
*
***********************************************************************
*  SHIFT REMAINDER OF STRING RIGHT TO MAKE ROOM FOR SYMBOLIC VALUE    *
***********************************************************************
SYMBSHFT LR    R15,R3             COMPUTE SHIFT   LENGTH
         SR    R15,R2
         SR    R15,R0             MINUS  LENGTH   DIFFERENCE
*
         LR    R14,R3             TARGET ADDRESS (RIGHT -> LEFT)
         BCTR  R14,0
         LR    R1,R14             TARGET ADDRESS
         SR    R1,R0              ADJUST TRAILING QUOTE  ADDRESS
*
SYMBRGHT MVC   0(1,R14),0(R1)     SHIFT  ONE BYTE
         BCTR  R14,0
         BCTR  R1,0
         BRCT  R15,SYMBRGHT
*
***********************************************************************
*  INSERT SYMBOLIC VALUE INTO STRING                                  *
***********************************************************************
SYMBSUBS LH    R1,ENVVVLEN        LOAD    VALUE  LENGTH (-1)
         EX    R1,SYMBMVAL        REPLACE SYMBOL WITH  VALUE
         LA    R2,0(R2,R1)        ADVANCE BEYOND VALUE
*
***********************************************************************
*  CONTINUE SCANNING STRING FOR MORE SYMBOLICS                        *
***********************************************************************
SYMBSCAN LA    R2,1(,R2)          ADVANCE TO  NEXT DSN CHARACTER
         CR    R2,R3              END-OF-NAME ???
         BRL   SYMBLOOP           NO  -  CONTINUE SCAN
*
         DROP  R4
*
***********************************************************************
*  RETURN                                                             *
***********************************************************************
RETURN   XR    R15,R15            SET  RETURN CODE  TO ZERO
*
         L     R14,RSA14(,R13)    RESTORE REGISTER  R14
         LM    R0,R12,RSA0(R13)   RESTORE REGISTERS R0 - R12
         BSM   0,R14              RETURN
*
*
         END
