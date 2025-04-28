         TITLE 'GVBUR39 - GENWRITE INTERFACE'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2008, 2021.
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
*  GVBUR39 - DYNAMICALLY OBTAIN "GENWRITE" ADDRESS                    *
*                                                                     *
*  FUNCTION CODES:                                                    *
*                                                                     *
*            NONE                                                     *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            NONE                                                     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
*
RSABP    EQU   4
RSAFP    EQU   8
RSA14    EQU   12
RSA15    EQU   16
RSA0     EQU   20
RSA1     EQU   24
*
                        EJECT
         PRINT GEN
*
GVBUR39  RMODE 24
GVBUR39  AMODE 31
GVBUR39  CSECT
         USING GVBUR39,R15
         J     CODE
UR39EYE  GVBEYE GVBUR39
*
CODE     L     R15,GENWRITE       ENTRY POINT ADDRESS AVAILABLE ???
         LTR   R15,R15
         BNZR  R15                YES - BRANCH
*
         STM   R14,R12,RSA14(R13) SAVE  CALLING  PROGRAM'S REGISTERS
*
         DROP  R15
         BASR  R11,0              SET   PROGRAM  BASE      REGISTER
         USING *,R11
*
         LOAD  EP=GENWRITE        LOAD  ENTRY    POINT ADDRESS
         ST    R0,GENWRITE
         LR    R15,R0
*
         L     R14,RSA14(,R13)    RESTORE REGISTERS
         LM    R0,R12,RSA0(R13)
         BR    R15
         DROP  R11
*
GENWRITE DC    A(0)
*
         LTORG
*
         END
