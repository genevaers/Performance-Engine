**********************************************************************
*                                                                        
* (C) COPYRIGHT IBM CORPORATION 2003, 2010.                                    
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
         TITLE 'GVBDAYS - DAYS BETWEEN TWO JULIAN DATES......'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  GVBDAYS  - COMPUTE DAYS BETWEEN TWO "CCYYDDD" DATES                *
*                                                                     *
*                                                                     *
*  RETURN CODES(R15):                                                 *
*                                                                     *
*        0  - SUCCESSFUL                                              *
*                                                                     *
*  PARAMETERS:                                                        *
*                                                                     *
*        R1:  PARAMETER LIST ADDRESS                                  *
*                                                                     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK    REGISTER                             *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - TEMPORARY WORK    REGISTER                             *
*            - RETURN    ADDR                                         *
*                                                                     *
*        R13 - REGISTER  SAVE    AREA     ADDRESS (CALLER PROVIDED)   *
*                                                                     *
*        R12 -    (NOT USED - NOT SAVED/RESTORED)                     *
*        R11 -    (NOT USED - NOT SAVED/RESTORED)                     *
*        R10 -    (NOT USED - NOT SAVED/RESTORED)                     *
*                                                                     *
*        R9  - PROGRAM   BASE    REGISTER                             *
*                                                                     *
*        R8  - PARAMETER LIST    ADDRESS                              *
*                                                                     *
*        R7  - WORK AREA DSECT   BASE REGISTER                        *
*                                                                     *
*        R6  - LEAP YEAR COUNT                                        *
*                                                                     *
*        R5  - DATE-2 MAX DAYS IN YEAR                                *
*        R4  - DATE-1 MAX DAYS IN YEAR                                *
*                                                                     *
*        R3  - DATE-2   "CCYY"                                        *
*        R2  - DATE-1   "CCYY"                                        *
*                                                                     *
*        R1  - TEMPORARY WORK    REGISTER                             *
*            - PARAMETER LIST    ADDRESS             (UPON ENTRY)     *
*                                                                     *
*        R0  - TEMPORARY WORK    REGISTER                             *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*       "GVBDAYS" - W O R K A R E A   D E F I N I T I O N             *
*                                                                     *
*       (NOTE: CANNOT BE EXPANDED BECAUSE IN REGISTER SAVE AREA)      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
WORKAREA DSECT
*
DBLWORK  DS    D                  DOUBLEWORD   WORK AREA
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P A R A M E T E R   L I S T   D E F I N I T I O N            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
PARMLIST DSECT
*
PLDATE1  DS    A                  INPUT  DATE - 1     (CCYYDDD)
PLDATE2  DS    A                  INPUT  DATE - 2     (CCYYDDD)
PLDAYSBT DS    A                  RESULT DAYS BETWEEN (FULLWORD)
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
R13      EQU   13
R14      EQU   14
R15      EQU   15
                        SPACE 3
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
RSA8     EQU   52
*
DAYSMAX  EQU   365
*
                        EJECT
         PRINT GEN
*
GVBDAYS  RMODE ANY
GVBDAYS  AMODE 31
GVBDAYS  CSECT
         J     CODE
DAYSEYE  GVBEYE GVBDAYS
*
CODE     STM   R14,R15,RSA14(R13) SAVE  CALLER'S  REGISTERS (R14)
         STM   R2,R9,RSA0(R13)                              (R2  -  R9)
*
         LR    R9,R15             SET   PROGRAM   BASE    REGISTERS
         USING GVBDAYS,R9
*
         LR    R8,R1              LOAD  PARAMETER LIST    ADDRESS
         USING PARMLIST,R8
*
         LA    R7,RSA8+7(,R13)    LOAD  WORK AREA ADDR WITHIN RSA
         SRL   R7,3                  (ROUNDED TO DOUBLEWORD)
         SLL   R7,3
         USING WORKAREA,R7
*
***********************************************************************
*  CONVERT YEARS TO BINARY AND INDICATE WHICH ONES ARE LEAP YEARS     *
***********************************************************************
         SR    R6,R6              ZERO LEAP YEAR  DATE  COUNT
*
         L     R14,PLDATE1        CONVERT DATE-1 "CCYY" TO BINARY
         PACK  DBLWORK,0(4,R14)
         CVB   R2,DBLWORK
*
         ST    R2,DBLWORK         ADJUST  MAX DAYS IN YEAR IF LEAP YR
         LHI   R4,DAYSMAX
         TM    DBLWORK+3,X'03'
         BRNZ  *+12
         AHI   R6,1
         AHI   R4,1
*
         L     R15,PLDATE2        CONVERT DATE-2 "CCYY" TO BINARY
         PACK  DBLWORK,0(4,R15)
         CVB   R3,DBLWORK
*
         ST    R3,DBLWORK         ADJUST  MAX DAYS IN YEAR IF LEAP YR
         LHI   R5,DAYSMAX
         TM    DBLWORK+3,X'03'
         BRNZ  *+12
         AHI   R6,2
         AHI   R5,1
*
***********************************************************************
*  CONVERT JULIAN DAYS TO BINARY                                      *
***********************************************************************
         PACK  DBLWORK,4(3,R14)   CONVERT DATE-1 "DDD"   TO BINARY
         CVB   R14,DBLWORK
*
         PACK  DBLWORK,4(3,R15)   CONVERT DATE-2 "DDD"   TO BINARY
         CVB   R15,DBLWORK
                        EJECT
***********************************************************************
*  COMPUTE DAYS BETWEEN                                               *
***********************************************************************
         SR    R0,R0              ASSUME   SAME DATE
*
         CR    R2,R3              YEAR-1 > YEAR-2 ???
         BRH   REVERSE            YES  -  REVERSE ORDER OF DATES
         BRL   SUMDAYS1           NO   -  BYPASS = TEST IF <
*
         CR    R14,R15            DAY-1  > DAY-2  ???
         BRH   REVERSE
         BRE   RETURN
*
SUBDAYS1 LR    R0,R15
         SR    R0,R14
         BRC   15,RETURN
*
SUMDAYS1 LR    R0,R4
         SR    R0,R14
         AR    R0,R15
*
YRDIFF1  LR    R15,R3             COMPUTE NO. OF INTERVENING YEARS
         SR    R15,R2
         BCTR  R15,0
         LR    R1,R15             SAVE    NO. OF YEARS (-1)
         LHI   R14,DAYSMAX
         MR    R14,R14            COMPUTE NO. OF INTERVENING DAYS (R15)
*
***********************************************************************
*  ADD NO. OF LEAP DAYS IN INTERVENING YEARS                          *
***********************************************************************
         LR    R14,R1
         SRL   R14,2
         AR    R15,R14
*
***********************************************************************
*  CHECK IF BOTH DATES ARE LEAP YEARS                                 *
***********************************************************************
         CHI   R6,1+2
         BRE   ZZZZZ1
*
XXXXX1   CHI   R1,4-1             LESS THAN 4 YEARS DIFFERENCE
         BRL   YYYYY1
*
***********************************************************************
*  HANDLE SPECIAL CASES WHERE DATES SPAN A LEAP YEAR (CALC 1 YR SHORT)*
***********************************************************************
         SLL   R14,2
         SR    R1,R14
*
YYYYY1   CHI   R1,3-1             REMAINDER OF 3 ???
         BRL   YYZZZ1             NO  - CHECK IF 2
*
         LTR   R6,R6              WERE  ANY OF DATES LEAP YEARS ???
         BRNZ  ZZZZZ1             YES - REMAINDER DOESN'T SPAN  LEAP YR
         AHI   R15,1
         BRC   15,ZZZZZ1
*
YYZZZ1   CHI   R1,2-1             REMAINDER OF 2 OR 3 ???
         BRNE  ZZZZZ1             NO  - SPANNING LEAP YEAR NOT POSSIBLE
*
         ST    R2,DBLWORK
         TM    DBLWORK+3,X'03'
         BRNO  ZZZZZ1
         AHI   R15,1
*
ZZZZZ1   AR    R0,R15
         BRC   15,RETURN
                        EJECT
***********************************************************************
*  REVERSE CALCULATION BECAUSE DATE-1 > DATE-2                        *
***********************************************************************
REVERSE  CR    R2,R3              SAME   YEAR ???
         BRNE  SUMDAYS2
*
SUBDAYS2 LR    R0,R15
         SR    R0,R14
         BRC   15,RETURN
*
SUMDAYS2 LR    R0,R5
         SR    R0,R15
         AR    R0,R14
*
YRDIFF2  LR    R15,R2             COMPUTE NO. OF INTERVENING YEARS
         SR    R15,R3
         BCTR  R15,0
         LR    R1,R15             SAVE    NO. OF YEARS (-1)
         LHI   R14,DAYSMAX
         MR    R14,R14            COMPUTE NO. OF INTERVENING DAYS (R15)
*
***********************************************************************
*  ADD NO. OF LEAP DAYS IN INTERVENING YEARS                          *
***********************************************************************
         LR    R14,R1
         SRL   R14,2
         AR    R15,R14
*
***********************************************************************
*  CHECK IF BOTH DATES ARE LEAP YEARS                                 *
***********************************************************************
         CHI   R6,1+2
         BRE   ZZZZZ2
*
XXXXX2   CHI   R1,4-1             LESS THAN 4 YEARS DIFFERENCE
         BRL   YYYYY2
*
***********************************************************************
*  HANDLE SPECIAL CASES WHERE DATES SPAN A LEAP YEAR (CALC 1 SHORT)   *
***********************************************************************
         SLL   R14,2
         SR    R1,R14
*
YYYYY2   CHI   R1,3-1             REMAINDER OF 3 ???
         BRL   YYZZZ2             NO  - CHECK IF 2
*
         LTR   R6,R6              WERE  ANY OF DATES LEAP YEARS ???
         BRNZ  ZZZZZ2             YES - REMAINDER DOESN'T SPAN  LEAP YR
         AHI   R15,1
         BRC   15,ZZZZZ2
*
YYZZZ2   CHI   R1,2-1             REMAINDER OF 2 OR 3 ???
         BRNE  ZZZZZ2             NO  - SPANNING LEAP YEAR NOT POSSIBLE
*
         ST    R3,DBLWORK
         TM    DBLWORK+3,X'03'
         BRNO  ZZZZZ2
         AHI   R15,1
*
ZZZZZ2   AR    R0,R15
         LCR   R0,R0              MAKE SIGN NEGATIVE
                        EJECT
***********************************************************************
*  RETURN TO CALLER (GVBMR95)                                         *
***********************************************************************
RETURN   L     R14,PLDAYSBT       PASS RESULT TO CALLER
         ST    R0,0(,R14)
*
         SR    R15,R15            ZERO RETURN CODE
*
         L     R14,RSA14(,R13)    RESTORE REGISTER  R14
         LM    R2,R9,RSA0(R13)    RESTORE REGISTERS R2 - R9
         BSM   0,R14              RETURN
                        SPACE 3
         LTORG
*
*
         END
