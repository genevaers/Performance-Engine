      title 'GVBURALI - Alias detection routine'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2005, 2021.
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
*   GVBURALI - Extract information about the loaded module in order   *
*              to detect which alias was used                         *
*              The MIPR routine will check the module info for an     *
*              alias with 'GVBMR95' as the major name                 *
*              8 byte alias name is returned in R0/R1 and rc 0        *
*                if no alias found then 'GVBMR95' returned with rc=4  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
**** START OF SPECIFICATIONS ****
*
*01*   MODULE-NAME = GVBURALI
*
*02*          CSECT-NAME = GVBURALI
*
*02*          DEPENDENCIES = SP4.3.0 or later system level
*
*03*              CHARACTER-CODE-DEPENDENCIES = NONE
*
*02*          REGISTER CONVENTIONS = standard
*
*02*          PATCH-LABEL = NONE
*
*01*   MODULE-TYPE = CSECT
*
*02*          PROCESSOR = ASSEMBLER-H
*
*
*02*     ATTRIBUTES =
*                    LOCATION      = Private library
*                    STATE         = Problem
*                    AMODE         = 31
*                    RMODE         = any
*                    KEY           = 8
*                    MODE          = Task
*                    SERIALIZATION = none
*                    TYPE          = Reentrant
*
*01*   ENTRY-POINT = gvburali
*
*02*          PURPOSE = Performs the module function.
*
*02*          LINKAGE = Standard Branch entry linkage
*
*03*     Entry-Registers:
*          Register       0 - Irrelevant
*          Register       1 - Reserved for input parameter list
*          Registers   2-12 - Irrelevant
*          Register      13 - Address of standard 72 byte
*                             save area
*          Register      14 - Return address
*          Register      15 - Entry address of gvburali
*
*02*   Exit-Normal: Returns to the caller.
*03*     Conditions: Requested function complete.
*03*     Exit-Registers:
*         Registers    0/1 - 8 byte alias name or 'GVBMR95'
*         Registers   2-14 - Restored
*         Register      15 - 0 or 4 - see below
*
*03*     Return-Codes:
*
*         0 - alias detected
*         4 - no alias detected
*
*01*   EXTERNAL-REFERENCES = NONE
*
*02*          CONTROL-BLOCKS =
*           CSVMODI
*           IHAPSA
*
*01*   MACROS =
*           STORAGE
*
**** END OF SPECIFICATIONS ****
         TITLE 'gvburali INITIALIZATION'
         SYSSTATE ARCHLVL=3
         print off
         COPY  ASMMSP
LEAVE    OPSYN ASM_LEAVE
         asmmrel on
         print on

gvburali AMODE 31
gvburali RMODE ANY
gvburali CSECT      ***  This setup puts MIPR in same module as
*                   ***  this program and avoids extra
*                   ***  GETMAIN-FREEMAIN overhead.
*
         j code
static   loctr
         GVBEYE GVBURALI
code     loctr
         SAVE (14,12)           Save caller's registers
         LR    R10,R15          Set R10 to map static area
         USING (gvburali,code),R10

         storage OBTAIN,Length=wrksize
         XC    0(WRKSIZE,R1),0(R1)  Clear dynamic work area
*                               (only works if <=256 bytes)
back     using saver,r13
forward  using saver,r1

         ST    R13,forward.savprev Save caller's savearea in mine
         ST    R1,back.savnext  Save my savearea in caller's
         LR    R13,R1           Point R13 to my savearea
         drop  back,forward
         USING MYDATA,R13       Access module data area
         using saver,mydata       map the save area as well

         USING PSA,R0        PSA addressability
         larl  r6,mymipr        get the routine entry point

*        Set up user data area to pass to MYMIPR routine
         mvc   module_name,blanks                      blank area
         mvc   module_name(l'=c'GVBMR95'),=c'GVBMR95'  major name
         mvc   module_name_len,=a(l'=c'GVBMR95')       set length

         la    r5,alias_name    get address of alias_name area
         st    r5,workarea_p      and save

*        Call the CSVINFO service to retrieve module info
*          CSVINFO will call the MYMIPR routine for each module.
*          MYMIPR will analyze the data and if an alias is found,
*          sets a flag, stores the alias name, set RC 4 which asks
*          for an exit

         CSVINFO FUNC=JPA,TCBADDR=PSATOLD,ENV=MVS,MIPR=(6),         cont
               USERDATA=MYUSERD,RETCODE=INFORC,RSNCODE=INFORS,      cont
               MF=(E,LISTFORM,COMPLETE)
         tm  alias_found,l'alias_found 
         if (o)                     did we detect an aliaa?

           lm  r4,r5,alias_name     Load r0/r1 with the alias name
           la  r3,0                 set rc=0

         else

           lm  r4,r5,module_name    Load r0/r1 with the major name
           la  r3,4                 set rc=4

         endif
*-------------------------------------------------------------------

         LR    R1,R13               Set storage address to free
         L     R13,savprev          Set R13 to address of caller's
*                                   savearea
         STORAGE RELEASE,Addr=(R1),length=WRKSIZE

         lr    r15,r3               set r15 to return code
         stm   r4,r5,savgrs0        and set up the r0/r1 data

*        Do not use RETURN as we need to use BSM 0,R14
         L     r14,savgrs14         restore r14
         LM    r0,r12,savgrs0       Restore r0 - r12
         bsm   0,r14

         drop  ,
*-------------------------------------------------------------------
*               Imbedded MIPR routine
*-------------------------------------------------------------------
MYMIPR   ds    0h
         using saver,r13
         STM   R14,R12,savgrs14       Save regs in caller's savearea
         lr    r11,r1
         USING MODI_HEADER,R11  R11 now maps MODI_HEADER
         larl  r10,gvburali     restore the static base register
         USING (gvburali,code),R10
         la    r9,modi_userdata get the pointer
         using (myuserd,myuserd+l'myuserd+1),r9 and map the area

         l     r8,workarea_p    get the pointer to output area
         using alias_name,r8
         USING MODI_1,R7
         using modi_5,r5        and map it
*-------------------------------------------------------------------

         ni  alias_found,x'ff'-l'alias_found  clear the flag
         lhi r15,0                set return code to continue in case  +
                                  nothing is found

         if (Lt,R7,MODI_1_PTR,nz) Access first data section, if avail

           if tm,modi_attr2,modi_reent,o is the module reentrant?
             if (tm,modi_attr2,modi_minor,o),and, minor entry?         +
               (lt,r5,modi_5_ptr,nz),and,        and modi_5 area valid?+
               (clc,modi_8_byte_major_name,eq,module_name)             +
                does the major name match what I am looking for?

               mvc alias_name,modi_8_byte_name copy the name
               oi  alias_found,l'alias_found  set the flag
               lhi r15,4                      set rc to exit
             endif
           else ,

*            not rentrant so check for substring equal

             la r2,module_name     address name of interest
             l r3,module_name_len and its length
             la r4,modi_8_byte_name get address of name in MODI
             la r5,l'modi_8_byte_name and its length
             lr r0,r3              get substring length into r0
             llill r1,c' '         set r1 to blank pad

             do until=no
               cuse r2,r4
             enddo

             if (e),and,           substring found so might be it      +
               cli,modi_8_byte_name+l'modi_8_byte_name-1,ne,c' '       +
                   check last character of name is NOT blank
               mvc alias_name,modi_8_byte_name copy the name
               oi  alias_found,l'alias_found  set the flag
               lhi r15,4                      set rc to exit
             endif

           endif
         endif
         RETURN (14,12),RC=(15) Return to CSVINFO

         drop  ,
*-------------------------------------------------------------------
MYDATA   DSECT
MAINSAVE DS      XL(saver_len) Main save area for gvburali module
INFORC   DS      f       CSVINFO return code
INFORS   DS      f       CSVINFO reason code
         ds      0f      make sure MYUSERD is aligned
MYUSERD  DS      0CL16   USERDATA for CSVINFO
*                        Used to pass data to the MIPR routine
module_name ds   CL8     Major name to search for
module_name_len  ds f    length of major name
workarea_p ds    A       Address of alias_name/flag to return info
         CSVINFO MF=(L,LISTFORM)
alias_name ds    cl8     8 byte alias name
alias_found equ  *,X'80' flag to indicate alias found
         ds      x       area for above flags
         ds      0d
WRKEND   EQU     *
WRKSIZE  EQU     *-MYDATA  Length of Dynamic Data area
*------------------------------------------------------------------
         TITLE 'gvburali EQUATES, CONSTANTS, LIST FORM MACROS, DSECTS'
*
* EQUATES USED BY gvburali
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
         CSVMODI
*--------------------- Suppress printing of these mapping macros
         PUSH PRINT
         PRINT OFF
         IHAPSA
         ihasaver
         POP  PRINT
*-------------------------------------------------------------------
static   loctr
blanks   dc    cl(l'module_name)''
         ltorg
         ds    0d      make it end on a good boundary
         END   ,
