         TITLE 'GVBMR94 - DELAY A PROGRAM ONE SECOND'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2008.
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
*  GVBMR94 - IS AN ASSEMBLER SUBROUTINE WHICH PUT A PROGRAM INTO      *
*            A ONE SECOND WAIT STATE.                                 *
*                                                                     *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*       R15 - RETURN    CODE(TO   CALLING  PROGRAM)                   *
*                                                                     *
*       R14 - RETURN ADDRESS(TO   CALLING  PROGRAM)                   *
*                                                                     *
*       R13 - REGISTER  SAVE AREA ADDRESS (CALLER PROVIDES)           *
*                                                                     *
*       R11 - PROGRAM  BASE REGISTER                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
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
         SPACE 3
RSABP    EQU   4
RSAFP    EQU   8
RSA14    EQU   12
RSA15    EQU   16
RSA0     EQU   20
RSA1     EQU   24
RSA2     EQU   28
         EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  1. SAVE CALLING PROGRAM'S REGISTER CONTENTS                        *
*  2. INITIALIZE BASE REGISTERS                                       *
*  3. SET TIMER  AND  WAIT                                            *
*  4. RETURN                                                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
GVBMR94  RMODE ANY
GVBMR94  AMODE 31
GVBMR94  CSECT
         J     CODE
MR94EYE  GVBEYE GVBMR94
         SPACE 1
CODE     DS    0H
         STM   R14,R12,RSA14(R13)  SAVE CALLING PROGRAM'S REGISTERS
         SPACE 1
         LR    R11,R15             SET  PROGRAM BASE      REGISTER
         USING GVBMR94,R11
         SPACE 1
         LA    R14,SAVEAREA        LOAD  ADDRESS OF NEW RSA
         ST    R13,SAVEAREA+RSABP  SET   BACKWARDS  POINTER IN NEW RSA
         ST    R14,RSAFP(,R13)     SET   FORWARD    POINTER IN OLD RSA
         LR    R13,R14             POINT TO NEW RSA
         SPACE 3
         LA    R1,WAITINT      LOAD  ADDR OF WAIT INTERVAL
         STIMER WAIT,BINTVL=(R1)
         SPACE 3
RETURN   SR    R15,R15                 ZERO  RETURN CODE
         SPACE 1
         L     R13,RSABP(,R13)         RESTORE  REGISTER 13
         XC    RSAFP(4,R13),RSAFP(R13) ZERO FORWARD POINTER
         SPACE 1
         L     R14,RSA14(,R13) RESTORE REGISTERS
         LM    R0,R12,RSA0(R13)
         BR    R14             RETURN
         SPACE 3
SAVEAREA DC  18F'0'            NEW  REGISTER SAVE AREA
WAITINT  DC    F'100'          WAIT INTERVAL (100 * .01 = 1.0 SEC)
         SPACE 3
         LTORG
         SPACE 1
         END