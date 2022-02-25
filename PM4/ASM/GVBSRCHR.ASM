         TITLE 'GVBSRCHR - LOOKUP SEARCH ROUTINES'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2019, 2020.
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
*  GVBSRCHR  ROUTINES TO SEARCH THE REFERENCE DATA WITH KEY           *
*            LENGTHS 1 - 156                                          *
*                                                                     *
*  FUNCTION CODES:                                                    *
*                                                                     *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - SUCCESSFUL                                          *
*            8  - ERROR                                               *
*                                                                     *
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
*        R13 - REGISTER  SAVE AREA  ADDRESS (THREAD WORK AREA)        *
*        R12 - GVBMR95   BASE REGISTER                                *
*        R11 - WORK REGISTER                                          *
*        R10 - RETURN ADDRESS TO GENERATED CODE                       *
*        R8  - CURRENT   EXTRACT  COLUMN    ADDRESS                   *
*        R7  - EXTRACT   RECORD  ADDRESS                              *
*        R6  - CURRENT   EVENT       RECORD ADDRESS (DRIVER FILE)     *
*        R5  - CURRENT   REFERENCE   RECORD ADDRESS (TABLE LOOK-UPS)  *
*        R4  - WORK      REGISTER                                     *
*            - BINARY    SEARCH  TOP        INDEX                     *
*            - PREVIOUS  RECORD  BUFFER     ADDRESS                   *
*            - CURRENT   CODE    BUFFER     POSITION                  *
*        R3  - Previous record pointer (reload from PREVRECA if used) *
*            - BINARY    SEARCH  BOT        INDEX                     *
*            - CURRENT   RECORD  BUFFER     LENGTH                    *
*            - CURRENT   CODE    SEGMENT    STARTING  ADDRESS         *
*        R2  - LITERAL POOL                                           *
*        R1  - TEMPORARY WORK REGISTER                                *
*        R0  - TEMPORARY WORK REGISTER                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                        EJECT
bin1     equ   1
*
         macro
&aghlab  agh  &reg,&harea
         AIF  ('&aghlab' EQ '').noaghlab
&aghlab  ds    0h
.noaghlab lgh  r0,&harea
          agr  &reg,r0
         mend
*
         Copy  GVBASSRT
         Copy  GVBMR95C
         Copy  GVB0200B
         Copy  GVBMR95W
         Copy  GVBX95PA
         Copy  GVBMR95L
*
         Push  PRINT
         Print OFF,NOGEN,NOPRINT
         Copy  ASMMSP
LEAVE    OpSyn ASM_LEAVE
         ASMMREL ON
         IEABRCX DEFINE
         Pop   PRINT
*
tempry   dsect
*        sole use of this dsect is to create a symbol table entry
*        that gives us the length of the jump instruction being used
*        in the machine code generation.
mc_jump  jlnop *
*        and now use the dsect to create the displacement length
*        (assumes displacement at the end of the instruction and
*         a two byte buffer at start (opcode + other bits)
mc_disp_l equ  *-mc_jump-2
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         YREGS
         SYSSTATE ARCHLVL=2
*
GVBSRCHR RMODE ANY
GVBSRCHR amode 31
GVBSRCHR CSECT
*
         j     code
SRCHEYE  GVBEYE GVBSRCHR
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  Generate the lookup search routines with different key lengths     *
*  from keylen = 1 to 256                                             *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         MACRO
.*
         LOOKUPS &ROUTINE=x
         LCLA &KLEN_N
&KLEN_N  SETA  1
.SRCHGEN ANOP
         org   *,256            align each routine
         &ROUTINE KEYLEN=&KLEN_N,ENTRY=Y
&KLEN_N  SETA  &KLEN_N+1
         AIF   ('&KLEN_N' LE '256').SRCHGEN
.*
         MEND
*
code     equ   *
GVBSRCHR CSECT
         using (thrdarea,thrdend),r13
         LOOKUPS ROUTINE=GVBSRCH              SRCHTBL routine
         drop r13
*
         END