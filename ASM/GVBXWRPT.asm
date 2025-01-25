         TITLE 'GVBXWIN - SAMPLE WRITE EXIT: GATHER EXIT INFO'         
********************************************************************** 
*                                                                      
* (C) COPYRIGHT IBM CORPORATION 2025.                                  
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
***********************************************************************
*                                                                     *
*  MODULE DESCRIPTION:                                                *
*                                                                     *
*      - THIS IS A WRITE EXIT THAT GATHERS INFORMATION ABOUT ALL THE  *
*        EXITS LOADED INTO A GENEVAERS PASS AND REPORTS THEIR:        *
*                                                                     *
*        1) LINKAGE EDITOR (BINDER) IDENTIFICATION DATE/TIME          *
*        2) LOAD MODULE SIZE                                          *
*        3) ASSEMBLER/COMPILER USED FOR EACH CSECT AND DATE           *
*                                                                     *
*  CALLED MODULES: IBM BINDER FAST API IEWBFDAT                       *
*                                                                     *
***********************************************************************
                        EJECT                                          
*                                                                      
***********************************************************************
*                                                                     *
*           MODULE RETURN CODES AND REGISTER DOCUMENTATION            *
*                                                                     *
***********************************************************************
*                                                                     *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - FOUND                                               *
*            4  - NOT FOUND                                           *
*            8  - END-OF-FILE                                         *
*           12  - DISABLE   VIEW                                      *
*           16  - ABEND     JOB                                       *
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
*        R13 - WORK AREA ADDRESS                                      *
*                                                                     *
*        R12 - PROGRAM    BASE   REGISTER                             *
*        R11 - PROGRAM    BASE   REGISTER                             *
*                                                                     *
*        R10 - SUBROUTINE CALL   RETURN   ADDRESS (1ST LEVEL)         *
*                                                                     *
*        R9  - WORK AREA  ANCHOR ADDRESS                              *
*                                                                     *
*        R8  - PARAMETER  LIST   ADDRESS                              *
*                                                                     *
*        R7  - EVENT FILE AREA   ADDRESS                              *
*        R6  -                                                        *
*        R5  - GENERATED TRANSACTION RECORD ADDRESS                   *
*                                                                     *
*        R4  -                                                        *
*        R3  -                                                        *
*        R2  -                                                        *
*                                                                     *
*        R1  - TEMPORARY WORK    REGISTER                             *
*            - PARAMETER LIST    ADDRESS             (UPON ENTRY)     *
*                                                                     *
*        R0  - TEMPORARY WORK    REGISTER                             *
*                                                                     *
***********************************************************************
                        EJECT                                          
         COPY GVBX95PA                                                 
         GVBAUR35 DSECT=YES                                            
         Copy GVBASSRT                                                 
         COPY GVBMR95W                                                 
         COPY GVBMR95L                                                 
***********************************************************************
*                                                                     *
*       "GVBXWINF" - W O R K A R E A  D E F I N I T I O N             *
*                                                                     *
***********************************************************************
*                                                                      
WKAREA   DSECT                                                         
*                                                                      
WKSVA    DS  18F                  REGISTER   SAVE AREA                 
WKSVA2   DS  18F                  REGISTER   SAVE AREA                 
WKSVA3   DS  18F                  REGISTER   SAVE AREA                 
WKEXIDCB DS    A                                                       
WKRENTWK DS    XL256                                                   
WKREC    DS   0CL80                                                    
WKRECTXT DS    CL13              'LOOKUP EXIT: '                       
WKRECXNM DS    CL8                                                     
         DS    CL59                                                    
WKDBL    DS    D                  DOUBLEWORD WORK AREA                 
WKLTBEGN DS    A                                                       
WKLTCNT  DS    F                                                       
*                                                                      
WKPLIST  DS   8A                  NAME/TOKEN PARAMETER    LIST         
*                                                                      
WKLENGTH EQU   *-WKAREA                                                
*                                                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        E X T R A C T   R E C O R D   A R E A                        *
*        (NOT USED)                                                   *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                      
EXTREC   DSECT                    EXTRACT RECORD  WORK   AREA          
*                                                                      
EXRECLEN DS    HL02                                                    
         DS    XL02                                                    
EXSORTLN DS    HL02               SORT    KEY     LENGTH               
EXTITLLN DS    HL02               SORT    TITLE   LENGTH               
EXDATALN DS    HL02               EXTRACT DATA    LENGTH               
EXNCOL   DS    HL02               NUMBER  OF  CALCULATED COLUMNS ("CT")
EXVIEW#  DS    FL04               VIEW    NUMBER  (+X'80000000')       
EXSRTKEY DS   0CL01                                                    
                        EJECT                                          
***********************************************************************
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
***********************************************************************
*                                                                      
         YREGS                                                         
*                                                                      
***********************************************************************
*                                                                     *
*        REGISTER SAVE AREA OFFSETS:                                  *
*                                                                     *
***********************************************************************
*                                                                      
RSABP    EQU   4                                                       
RSAFP    EQU   8                                                       
RSA14    EQU   12                                                      
RSA15    EQU   16                                                      
RSA0     EQU   20                                                      
RSA1     EQU   24                                                      
RSA2     EQU   28                                                      
*                                                                      
GVBXWINF RMODE ANY                                                     
GVBXWINF AMODE 31                                                      
GVBXWINF CSECT                                                         
         J     CODE                                                    
XWINFEYE GVBEYE GVBXWINF                                               
*                                                                      
CODE     STM   R14,R12,RSA14(R13) SAVE  CALLER'S  REGISTERS            
*                                                                      
         LA    R11,0(,R15)        SET   PROGRAM   BASE    REGISTER     
         USING GVBXWINF,R11 ------------------------------------------ 
*                                                                      
         LR    R8,R1              LOAD  PARAMETER LIST    ADDRESS      
         USING GENPARM,R8       -------------------------------------- 
***********************************************************************
*  REFERENCE TO CALLING PARAMETERS                                    *
***********************************************************************
         L     R7,GPENVA          LOAD ENVIRONMENT INFO ADDRESS        
         USING GENENV,R7 --------------------------------------------- 
*                                                                      
         LR    R10,R13            PRESERVE CALLER'S RSA ADDRESS        
*                                                                      
***********************************************************************
*  CHECK FOR INIT PHASE. RETURN IF SO.                                *
***********************************************************************
         CLI   GPPHASE,C'I'       INITIALISATION PHASE ?               
         JNE   OPRDCLTE           CONTINUE PROCESSING OP/RD/CL/TE      
*                                                                      
INITPHAS EQU   *                                                       
         LHI   R15,0              SET  RETURN CODE FOR OPEN            
         L     R14,GPRTNCA        LOAD RETURN CODE ADDRESS             
         ST    R15,0(,R14)                                             
         LR    R13,R10            RESTORE  R13                         
         J     RETURNIN           RETURN INIT                          
*                                                                      
OPRDCLTE EQU   *                  OP/RD/CL/TE                          
         L     R9,GPWORKA         LOAD  WORK AREA POINTER ADDRESS      
         L     R12,0(,R9)         LOAD  POINTER   VALUE                
         USING WKAREA,R12 -------------------------------------------- 
         LTR   R12,R12            ALLOCATED  ???                       
         JP    CHAIN              YES - BYPASS ALLOCATION              
*                                                                      
***********************************************************************
*  ALLOCATE "GVBXWINF" WKAREA   IF NOT ALREADY ALLOCATED (PER "WR")   *
***********************************************************************
WORKALLO EQU   *                                                       
         LHI   R0,WKLENGTH+8       LOAD  WORK AREA SIZE                
*                                                                      
         STORAGE OBTAIN,LENGTH=(0),COND=NO,CHECKZERO=YES               
*                                                                      
         MVC   0(8,R1),WORKEYEB                                        
         LA    R12,8(,R1)                                              
         ST    R12,0(,R9)         SAVE  WORK AREA ADDRESS (POINTER)    
*                                                                      
         CIJE  R15,X'14',ALLCLEAN                                      
         LR    R0,R12             ZERO  WORK AREA                      
         LHI   R1,WKLENGTH                                             
         SR    R14,R14                                                 
         SR    R15,R15                                                 
         MVCL  R0,R14                                                  
*                                                                      
ALLCLEAN EQU   *                                                       
         LA    R13,WKSVA          NEW   SAVE AREA                      
         ST    R13,RSAFP(,R10)    SET   FORWARD  POINTER IN OLD        
         ST    R10,RSABP(,R13)    SET   BACKWARD POINTER IN NEW        
         J     MAINLINE                                                
*                                                                     *
*                                                                      
***********************************************************************
*  CHAIN REGISTER SAVE AREAS TOGETHER                                 *
***********************************************************************
CHAIN    EQU   *                                                       
         LA    R13,WKSVA          NEW   SAVE AREA                      
         ST    R13,RSAFP(,R10)    SET   FORWARD  POINTER IN OLD        
         ST    R10,RSABP(,R13)    SET   BACKWARD POINTER IN NEW        
*                                                                      
***********************************************************************
*  COMMON MAINLINE PROCESSING                                         *
***********************************************************************
MAINLINE EQU   *                                                       
***********************************************************************
*  CHECK FOR OPEN PHASE, RETURN IF SO                                 *
***********************************************************************
         CLI   GPPHASE,C'O'       TEST FOR OPEN PHASE                  
         JE    RETURN0            NOTHING TO DO                        
         CLI   GPPHASE,C'R'       TEST FOR READ PHASE                  
         JE    RETURN0            NOTHING TO DO                        
         CLI   GPPHASE,C'C'       TEST FOR CLOSE PHASE                 
         JNE   RETURN0            NOTHING TO DO                        
*                                                                      
***********************************************************************
*    TERM PHASE PROCESSING                                             
***********************************************************************
*                                                                      
         WTO 'GVBXWINF TERMINATION PROCESSING'                         
*                                                                      
         L     R1,GP_THRD_WA                                           
         USING THRDAREA,R1                                             
         L     R1,THRDMAIN                                             
         MVC   WKLTBEGN,LTBEGIN                                        
         MVC   WKLTCNT,LTCOUNT                                         
         DROP  R1 THRDAREA                                             
*                                                                      
         L     R15,=A(EXITRPT)                                         
         BASR  R14,R15                                                 
*                                                                      
         LHI   R15,0              SET  RETURN CODE                     
         J     RETURN                                                  
*                                                                      
***********************************************************************
*  RETURN TO CALLER (GVBMR95)                                         *
***********************************************************************
RETURN0  EQU   *                                                       
         XGR   R15,R15            ZERO RETURN CODE                     
         J     RETURN                                                  
*                                                                      
RETURN8  EQU   *                                                       
         LHI   R15,8              ZERO RETURN CODE                     
*                                                                      
RETURN   EQU   *                                                       
         L     R14,GPRTNCA        LOAD RETURN CODE  ADDRESS            
         ST    R15,0(,R14)                                             
         L     R13,RSABP(,R13)    RESTORE REGISTER  R13                
*                                                                      
RETURNIN EQU   *                  RETURN INITIALIZATION                
         L     R14,RSA14(,R13)    RESTORE REGISTER  R14                
         LM    R0,R12,RSA0(R13)   RESTORE REGISTERS R0 - R12           
         BSM   0,R14              RETURN                               
*                                                                      
         DROP  R8                                                      
*                                                                      
***********************************************************************
*  EXITRPT: REPORT OF GENERAERS EXTRACT PHASE USER EXITS              *
***********************************************************************
EXITRPT  DS    0H                                                      
         STM   R14,R12,12(R13)                                         
         LA    R10,WKSVA2                                              
         ST    R10,RSAFP(,R13)    SET   FORWARD  POINTER IN OLD        
         ST    R13,RSABP(,R10)    SET   BACKWARD POINTER IN NEW        
         LR    R13,R10            NEW   SAVE AREA                      
*                                                                      
         LA    R0,EXIDCBL                                              
         GETMAIN R,LV=(0),LOC=BELOW                                    
         ST    R1,WKEXIDCB                                             
*                                                                      
         L     R2,WKEXIDCB                                             
         USING IHADCB,R2                                               
         MVC   0(EXIDCBL,R2),EXIDCB                                    
*                                                                      
         LA    R1,EXIDCBE-EXIDCB(,R2)                                  
         ST    R1,DCBDCBE                                              
*                                                                      
         MVC   WKRENTWK(8),OPENPARM                                    
         OPEN  ((R2),OUTPUT),MODE=31,MF=(E,WKRENTWK)                   
         TM    48(R2),X'10'                                            
         BO    A0400                                                   
         DC    H'0'                                                    
A0400   EQU    *                                                       
         DROP  R2 IHADCB                                               
*                                                                      
         LLGT  R8,WKLTBEGN        LOAD LOGIC  TABLE ADDRESS            
         USING LOGICTBL,R8                                             
*                                                                      
         LGF   R7,WKLTCNT         LOAD LOGIC  TABLE ENTRY COUNT        
LTLOOP   EQU   *                                                       
         CLC   LTFUNC(2),=CL2'WR'                                      
         JNE   LT0100                                                  
         CLC   LTWRNAME,SPACEX    EXIT SPECIFIED ???                   
         JE    LT0990             no, go                               
*      WR_EX processing                                                
         MVC   WKRECTXT,=CL13'WRITE EXIT:  '                           
         MVC   WKRECXNM,LTWRNAME                                       
LT0010   EQU   *                                                       
         J     LT0900                                                  
*                                                                      
LT0100   EQU   *                                                       
         CLC   LTFUNC,=CL4'LUEX'                                       
         JNE   LT0200                                                   
         CLC   LTLUNAME,SPACEX    EXIT SPECIFIED ???                    
         JE    LT0990             no, go                                
*      LU_EX processing                                                 
         MVC   WKRECTXT,=CL13'LOOKUP EXIT: '                            
         MVC   WKRECXNM,LTLUNAME                                        
         J     LT0900                                                   
*                                                                       
LT0200   EQU   *                                                        
         CLC   LTFUNC(2),=CL2'RE'                                       
         JNE   LT0990                                                   
         CLC   LTRENAME,SPACEX                                          
         JE    LT0990                                                   
*      RE_EX processing                                                 
         MVC   WKRECTXT,=CL13'READ EXIT:   '                            
         MVC   WKRECXNM,LTRENAME                                        
*                                                                       
*                                                                       
LT0900  EQU   *                  Write exit information                 
         LA    R0,WKREC                                                 
         L     R1,WKEXIDCB                                              
         PUT   (1),(0)                                                  
*                                                                       
LT0990  EQU   *                                                         
         LGH   R0,LTROWLEN                                              
         AGR   R8,R0              ADVANCE TO NEXT ROW (31-BIT ADDR)     
         BRCT  R7,LTLOOP                                                
         DROP  R8 LOGICTBL                                              
*                                                                       
         L     R2,WKEXIDCB                                              
         MVC   WKRENTWK(8),OPENPARM                                     
         CLOSE ((R2)),MODE=31,MF=(E,WKRENTWK)                           
*                                                                       
         L     R13,RSABP(,R13)    OLD   SAVE AREA                       
         LM    R14,R12,12(R13)                                          
         BR    R14                                                      
*                                                                       
*                                                                       
MVCR5R14 MVC   0(0,R5),0(R14)     * * * * E X E C U T E D * * * *       
*                                                                       
*********************************************************************** 
*                                                                     * 
*        C O N S T A N T S                                            * 
*                                                                     * 
*********************************************************************** 
         DS   0D                                                        
MODE31   DS   0XL4                                                      
OPENPARM DC    XL8'8000000000000000'                                    
SPACEX   DC    CL256' '                                                 
*                                                                       
WORKEYEB DC    CL8'GVBXWINF'                                            
*                                                                       
EXIDCB   DCB   DSORG=PS,DDNAME=EXTRXRPT,MACRF=(PM),DCBE=EXIDCBE,       X
               RECFM=FB,LRECL=80                                        
EXIDCBE  DCBE  RMODE31=BUFF                                             
EXIDCBL  EQU   *-EXIDCB                                                 
*                                                                       
         LTORG ,       
         DS   0F       
         DCBD  DSORG=PS
*                      
         IHADCBE       
*                      
*                      
         END           
