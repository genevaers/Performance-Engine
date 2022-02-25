         TITLE 'GVBUT99 - USER ABEND UTILITY'
**********************************************************************
*                                                                        
* (C) COPYRIGHT IBM CORPORATION 2000, 2010.                                    
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
*   GVBUT99 - ISSUES A SPECIFIED USER ABEND.  DESIGNED TO BE EXECUTED
*             AS A STAND ALONE PROGRAM FOR USE IN MULTI-STEP JOBS.
*
*   PARAMETERS: THE USER ABEND CODE PASSED FROM THE INVOKING JCL.
*
*   RETURN:
*    NORMAL  - NONE.
*    ERROR   - AN IDENTIFYING MESSAGE IS WRITTEN TO THE JOB LOG
*              AND THE SPECIFIED USER ABEND IS ISSUED.
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*   R E G I S T E R  U S A G E
*
*   R15   -  STANDARD LINKAGE/WORK REGISTER
*   R14   -  STANDARD LINKAGE/WORK REGISTER
*   R13   -  STANDARD LINKAGE/DYNAMIC STORAGE BASE
*   R12   -  PROGRAM BASE
*   R11   -  RESERVED
*   R10   -  INTERNAL SUBROUTINE LINKAGE
*   R9    -  A(PARM INFO FROM JCL)
*   R8    -  WORK REGISTER
*   R7    -  WORK REGISTER
*   R6    -  WORK REGISTER
*   R5    -  WORK REGISTER
*   R4    -  WORK REGISTER
*   R3    -  ABEND CODE TO ISSUE
*   R2    -  STANDARD USAGE/WORK REGISTER
*   R1    -  STANDARD LINKAGE/WORK REGISTER
*   R0    -  STANDARD LINKAGE/WORK REGISTER
*
         EJECT
*-------------------------------------------------------------------*
*        PARM INFORMATION PASSED FROM THE INVOKING JCL
*-------------------------------------------------------------------*
PARMINFO DSECT
PARMLEN  DS    H                       LENGTH OF PARM DATA
PARMLMAX EQU   4                        - MAX LENGTH ALLOWED
PARMDATA DS   0X                       DATA FROM JCL
PARMDEFV EQU   4095                     - DEFAULT (MAX) ABEND CODE
         EJECT
         YREGS
*
RSABP    EQU   04,4
RSAFP    EQU   08,4
RSA14    EQU   12,4
RSA15    EQU   16,4
RSA0     EQU   20,4
RSA1     EQU   24,4
*                                      GLOBAL REGISTER ASSIGNMENTS
         USING WORKSTOR,R13             - DYNAMIC STORAGE
         USING GVBUT99,R12              - PROGRAM BASE
         USING PARMINFO,R9              - CALLING INTERFACE
*
         EJECT
GVBUT99  CSECT
GVBUT99  AMODE 31
GVBUT99  RMODE ANY
         J     START
UT99EYE  GVBEYE GVBUT99
*
START    EQU   *
         STM   R14,R12,RSA14(R13)      SAVE CALLER'S REGS
         LA    R12,0(,R15)             SET PROGRAM BASE
*
         L     R9,0(,R1)               --> PARAMETER LIST
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
         EJECT
INIT0100 EQU   *
         MVC   WTO_TLEN,=Y(WTO_ALEN)   PRIME WTO STATIC VALUES
         MVC   WTO_PGNM,=C'GVBUT99: '
         MVI   WTO_TEXT+0,X'40'        CLEAR MESSAGE LINE
         MVC   WTO_TEXT+1(L'WTO_TEXT-1),WTO_TEXT
         L     R3,=A(PARMDEFV)         DEFAULT ABEND CODE
*
MAIN0100 EQU   *
         LH    R15,PARMLEN             GET ABEND CODE LENGTH
         LTR   R15,R15                 WAS PARM SPECIFIED?
         BZ    ERROR#01                N - USE DEFAULT
         CH    R15,=Y(PARMLMAX)        PARM TOO LONG?
         BH    ERROR#02                Y - USE DEFAULT
         BCTR  R15,0
         EX    R15,DATA_TRT            PARM DATA NUMERIC?
         BNZ   ERROR#03                N - USE DEFAULT
         EX    R15,DATA_PCK
         CVB   R3,WORKAREA
         C     R3,=A(PARMDEFV)         MAX VALUE EXCEEDED?
         BNH   ABND0100                N - CONTINUE
         L     R3,=A(PARMDEFV)         Y - USE DEFAULT
         B     ERROR#04
*
ABND0100 EQU   *
         L     R14,=A(MSG#99)          --> JOB MESSAGE
         LA    R15,L'MSG#99-1          LENGTH OF MESSAGE
         EX    R15,COPY_MSG            MOVE MSG TO WTO AREA
         L     R1,PSAAOLD-PSA          GET ABENDING JOB NAME
         L     R2,ASCBJBNI-ASCB(,R1)
         MVC   WTO_JOBN,0(R2)          INSERT JOB NAME
*
         WTO   MF=(E,WTO_AREA)
*
         ABEND (R3)
         EJECT
*-------------------------------------------------------------------*
*        DISPLAY A MESSAGE
*-------------------------------------------------------------------*
WTOM0100 EQU   *
         EX    R15,COPY_MSG
*
         WTO   MF=(E,WTO_AREA)
*
         MVI   WTO_TEXT+0,X'40'
         MVC   WTO_TEXT+1(L'WTO_TEXT-1),WTO_TEXT
         BR    R10
*
ERROR#01 EQU   *
         L     R14,=A(MSG#01)
         LA    R15,L'MSG#01-1
         BAS   R10,WTOM0100
         B     ABND0100
*
ERROR#02 EQU   *
         L     R14,=A(MSG#02)
         LA    R15,L'MSG#02-1
         BAS   R10,WTOM0100
         B     ABND0100
*
ERROR#03 EQU   *
         L     R14,=A(MSG#03)
         LA    R15,L'MSG#03-1
         BAS   R10,WTOM0100
         B     ABND0100
*
ERROR#04 EQU   *
         L     R14,=A(MSG#04)
         LA    R15,L'MSG#04-1
         BAS   R10,WTOM0100
         B     ABND0100
         EJECT
STATSTOR DS    0D                      *----------------------------*
*                                      * STATIC STORAGE
         DC    CL8'GVBUT99'            *----------------------------*
*                                      EXECUTED INSTRUCTIONS
DATA_TRT TRT   PARMDATA(0),NUMERICS
DATA_PCK PACK  WORKAREA(8),PARMDATA(0)
COPY_MSG MVC   WTO_TEXT(0),0(R14)
*
MSG#01   DC    C'NO ABEND CODE WAS SPECIFIED - DEFAULT USED'
MSG#02   DC    C'THE SPECIFIED ABEND CODE IS TOO LONG - DEFAULT USED'
MSG#03   DC    C'THE ABEND CODE IS NOT NUMERIC - DEFAULT USED'
MSG#04   DC    C'USER ABEND JCL VALUE TOO LARGE - DEFAULT USED'
MSG#99   DC    C'JOB XXXXXXXX IS TERMINATING WITH A USER ABEND'
*
NUMERICS DS    0F
         DC    256XL1'FF'              NUMERIC TRT TABLE
         ORG   NUMERICS+X'F0'          NUMERIC DIGITS
         DC    10XL1'00'
         ORG
         EJECT
         LTORG CANCEL MY SUBSCRIPTION TO THE RESURRECTION
         EJECT
WORKSTOR DSECT                         *----------------------------*
*                                      * WORKING STORAGE
*                                      *----------------------------*
SAVEAREA DS    18F                     REGISTER SAVE AREA
*
WTO_AREA DS   0D                       MESSAGE WORK AREA FOR WTO
WTO_TLEN DS    H                        - LENGTH OF AREA FOR WTO
         DS    H                        - NO ROUTE/DESC CODES
         DS    CL01
WTO_PGNM DS    CL09                     - PROGRAM NAME
WTO_TEXT DS   0CL110                    - MESSAGE TO DISPLAY
         DS    CL04
WTO_JOBN DS    CL08                     - JOB NAME
         DS    CL98
WTO_ALEN EQU   (*-WTO_AREA)
*
         DS    0D
WORKAREA DS    CL128                   GENERALIZED WORK AREA
WORKSLEN EQU   (*-WORKSTOR)            DYNAMIC STORAGE LENGTH
         EJECT
UT99VERS CSECT
UT99VERS AMODE 31
UT99VERS RMODE ANY
*
         PRINT NOGEN
         IHAPSA DSECT=YES
         IHAASCB DSECT=YES
*
         END
