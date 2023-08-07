         TITLE 'GVBMR96 - INITIALIZATION FOR "GVBMR95"'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2005, 2022.
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
*  GVBMR96 - LOADS THE LOGIC TABLE,  OPENS REQUIRED EXTRACT FILES,    *
*            LOADS MEMORY RESIDENT LOOK-UP TABLES, INITIALIZES        *
*            BINARY SEARCH PATHS, MAKES ADJUSTMENTS TO LOGIC TABLE    *
*            FIELDS, AND BUILDS SKELETON CODE FOR EACH LOGIC TABLE    *
*            ROW.                                                     *
*                                                                     *
*  NOTE:     GVBMR96 RUNS IN 31-BIT ADDRESSING MODE.                  *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - SUCCESSFUL                                          *
*           NN  - ERROR MESSAGE NUMBER                                *
*                                                                     *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK REGISTER                                *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - TEMPORARY WORK REGISTER                                *
*            - INTERNAL  SUBROUTINE  RETURN ADDRESS (3RD LEVEL)       *
*            - RETURN    ADDRESS                                      *
*                                                                     *
*        R13 - REGISTER  SAVE AREA  ADDRESS (THREAD WORK AREA)        *
*                                                                     *
*        R12 - PROGRAM   BASE REGISTER                                *
*        R11 - register used to hold previous exit_data address       *
*                                                                     *
*        R10 - INTERNAL  SUBROUTINE  RETURN ADDRESS (1ST LEVEL)       *
*                                                                     *
*        R9  - INTERNAL  SUBROUTINE  RETURN ADDRESS (2ND LEVEL)       *
*            - LOOK-UP   DATA RECORD ADDRESS                          *
*            - FUNCTION  TABLE ENTRY ADDRESS                          *
*                                                                     *
*        R8  - "GVBMR96" INITIALIZATION     TEMPORARY    VARIABLES    *
*                                                                     *
*        R7  - CURRENT   LOGIC TABLE ROW    ADDRESS                   *
*                                                                     *
*        R6  - LOGIC     TABLE LOOP  COUNTER                          *
*            - CURRENT   EVENT       RECORD ADDRESS (DRIVER FILE)     *
*                                                                     *
*        R5  - CURRENT   REFERENCE   RECORD ADDRESS (TABLE LOOK-UPS)  *
*                                                                     *
                        EJECT
*                                                                     *
*        R4  - WORK      REGISTER                                     *
*            - BINARY    SEARCH    TOP      INDEX                     *
*            - PREVIOUS  LOOK-UP   BUFFER   ADDRESS                   *
*            - CURRENT   CODE      BUFFER   POSITION                  *
*                                                                     *
*        R3  - WORK      REGISTER                                     *
*            - BINARY    SEARCH    BOTTOM   INDEX                     *
*            - CURRENT   LOOK-UP   BUFFER   LENGTH                    *
*            - CURRENT   CODE      SEGMENT  STARTING  ADDRESS         *
*            - EXTRACT   FILE      AREA     BASE      REGISTER        *
*                                                                     *
*        R2  - GENERATED CODE BASE REGISTER (CURRENT  VIEW)           *
*            - DCB       ADDRESS                                      *
*                                                                     *
*        R1  - PARAMETER LIST ADDRESS                                 *
*            - TEMPORARY WORK REGISTER                                *
*                                                                     *
*        R0  - TEMPORARY WORK REGISTER                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         Push  PRINT
         Print OFF,NOGEN,NOPRINT
         Copy  ASMMSP
LEAVE    OpSyn ASM_LEAVE
         ASMMREL ON
         IEABRCX DEFINE
         Pop   PRINT
*
         Copy  GVBASSRT
         Copy  GVBMR95W
         Copy  GVBMR95C
         Copy  EXECDATA
         Copy  GVBMR95L
*
         Copy  GVBLTGEN
         Copy  GVBLTHDA
         Copy  GVBLTREA
         Copy  GVBLTNVA
         Copy  GVBLTWRA
         Copy  GVBLTCCA
         Copy  GVBLTVNA
         Copy  GVBLTVVA
         Copy  GVBLTF0A
         Copy  GVBLTF1A
         Copy  GVBLTF2A
         Copy  GVBLTV1A
         Copy  GVBLTV2A
*
         Copy  GVB0001A
         Copy  GVB0050A
         Copy  GVB0050B
         Copy  GVB0200A
         Copy  GVB0200B
*
         Copy  GVB0210A
         Copy  GVB0210B
         Copy  GVB0300A
         Copy  GVB0300B
         Copy  GVB0650A
         Copy  GVB0650B
         Copy  GVB0800A
         Copy  GVB0801A
         Copy  GVB0801B
         Copy  GVB1000A
         Copy  GVB1000B
         Copy  GVB1600A
         Copy  GVB2000A
         Copy  GVB2000B
         Copy  GVB2300A
         Copy  GVB2300B
*
         Copy  GVBHDR
         Copy  MAJRFTAB
         Copy  GVBX95PA
*
Fiscal_date_table dsect
Fiscal_date_entry ds 0cl(fiscal_date_entry_len)
fiscal_next  ds   f
fiscal_recid ds   f
fiscal_date  ds   cl8
fiscal_date_len equ *-fiscal_recid
fiscal_date_entry_len equ *-fiscal_next
*
view_table dsect
view_nbr            ds f     View number
view_name           ds cl48  View name
view_type           ds fl04  View type (extract/format/copy)
view_output_media   ds fl04  Output format (fixed/hardcopy/delimited)
view_epa_summ_recs  ds Fl04 EPA buffer recs (aka. extract summary recs)
view_format_summary ds cl1   Summary format
                    ds xl3
view_table_ent_len equ *-view_table

         copy  gvblogit
         copy  gvbrptit
         copy  gvbphead
         copy  gvbpcont
         SYSSTATE ARCHLVL=2,AMODE64=YES
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         YREGS
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*        Floating point register equates                              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
fp0      equ 0,,,,fpr
fp1      equ 1,,,,fpr
fp2      equ 2,,,,fpr
fp3      equ 3,,,,fpr
fp4      equ 4,,,,fpr
fp5      equ 5,,,,fpr
fp6      equ 6,,,,fpr
fp7      equ 7,,,,fpr
fp8      equ 8,,,,fpr
fp9      equ 9,,,,fpr
fp10     equ 10,,,,fpr
fp11     equ 11,,,,fpr
fp12     equ 12,,,,fpr
fp13     equ 13,,,,fpr
fp14     equ 14,,,,fpr
fp15     equ 15,,,,fpr
         PRINT OFF
         COPY  GVBUTEQU
         COPY  DL96EQU
         PRINT ON
*
SIGNED   EQU   01                 FIELD CONTAINS  A SIGN
LENDIAN  EQU   01                 LITTLE  ENDIAN  INDICATOR   (BOOLEAN)
MAXPACK  EQU   16                 MAXIMUM PACKED  FIELD LENGTH
MAXDEC   EQU   08                 MAXIMUM NO  OF  DECIMALS
MAXKEYLN EQU   256                MAXIMUM LOOK-UP KEY   LENGTH
maxclons equ   9999               Maximum possible clone threads
*
LTVERS3  EQU   003                VERSION 003     LOGIC TABLE
*
LTTKNUPD EQU   X'40'
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
                        EJECT
         PRINT GEN
*
GVBMR96  CSECT
GVBMR96  RMODE ANY
GVBMR96  AMODE 31
*
         ENTRY DSNHLI
         j     start              get to the code
MR96EYE  GVBEYE GVBMR96
static   loctr                    define the static section
code     loctr                    and the code
*
         USING THRDAREA,R13       REUSE/RETAIN  MAIN THREAD SAVE  AREA
         using genparm,parm_area
         using genenv,env_area
         using msglist,msg_area
*
         using saver,thrdarea
start    stm   R14,r12,savgrs14   SAVE CALLER'S REGISTERS (MAIN THREAD)
         STMH  R14,R12,save_grande       Save the 64bit parts as well
         sam64 ,                  get into the correct mode
         larl  r12,GVBMR96        Point to start of GVBMR96
         USING (GVBMR96,code),R12 Set base
*
***********************************************************************
*  OBTAIN EXTRACT RECORD WORKAREA (USED FOR INITIALIZATION VARIABLES) *
***********************************************************************
         STORAGE OBTAIN,LENGTH=extrecl+l'initeyeb,LOC=(ANY),BNDRY=PAGE
         MVC   0(l'initeyeb,R1),INITEYEB   COPY EYEBALL
         Aghi  R1,l'initeyeb
         LGR   R8,R1
         USING INITVAR,R8
*
         lgr   R0,R8              ZERO AREA
         LHI   R1,EXTRECL
         xgr   R14,R14
         XR    R15,R15
         MVCL  R0,R14
*
***********************************************************************
*  SAVE CALLER'S REGISTERS IN INITIALIZATION AREA                     *
*  INITIALIZE DRIVER PROGRAM(GVBMR95) THREAD WORK AREA                *
***********************************************************************
         MVC   REGSAVE(saver_len),0(R13) xfer caller's registers
*
         sty   R8,GPEXTRA         SAVE EXTRACT  RECORD  ADDRESS
*                                                                     *
*        If SNAP is required, use SNAP=Y so that MR95 allocates and   *
*        opens the file.                                              *
*        The SNAPDCB address is saved in SNAPDCBA                     *
*                                                                     *

*
***********************************************************************
*  OPEN "CONTROL REPORT" FILE                                         *
***********************************************************************
         STORAGE OBTAIN,LENGTH=CTRLDCBL+l'ctrleyeb,  Get memory for DCB+
               LOC=BELOW                 make sure it is belowfor DCB
*        Storage returns a valid 64-bit address
         lgr   r2,r1                     copy for later
         ahi   R2,l'ctrleyeb
         MVC   0(l'ctrleyeb,R1),CTRLEYEB COPY   EYEBALL
         ST    R2,CTRLDCBA        SAVE   DCB  ADDRESS
*
         L     R14,CTRLFILA              COPY MODEL   DCB
         MVC   0(CTRLDCBL,R2),0(R14)
         using ihadcb,r2
         LA    R0,CTRLDCBE-CTRLFILE(,R2) SET  DCBE    ADDRESS IN  DCB
         ST    R0,DCBDCBE
*
         mvc  rpt_dd95C,=cl8'EXTRRPT'    store alias name
         if clc,namepgm,eq,=cl8'GVBMR95R'    R for reference
           mvc  rpt_dd95C,=cl8'REFRRPT'    store alias name
           mvc  dcbddnam,=cl8'REFRRPT'     default is EXTRRPT
         endif
*
         MVC   WKREENT(8),OPENPARM       OPEN  PARAMETER LIST
         sysstate amode64=NO
         sam31
         OPEN  ((R2),(OUTPUT)),MODE=31,MF=(E,WKREENT) CONTROL RPT FILE
         sam64
         sysstate amode64=YES
         TM    48(R2),X'10'              SUCCESSFULLY OPENED  ??
         jo    Main_100                  YES - BYPASS ABEND
*
*        WTO   'GVBMR95 - UNABLE TO OPEN CONTROL REPORT FILE'
         MVC   ERRDATA(8),rpt_dd95c
         lghi  R14,OPEN_RPT_FAIL
         BRU   RTNERROR

         drop  r2
Main_100 ds 0h
         jas   R9,PRNTHDR         PRINT CONTROL REPORT HEADING
*

         larl  R14,Print_Rptsect  Print MR95RPT content description
         oilh  r14,x'8000'        make the address amode 31
         bassm r9,r14             and off we go
*
         jas   R14,INITthrd       THREAD INITIALIZATION SUBROUTINE

         la    r11,exit_data_ptr  set up pointer for exit_data chain
         using exit_data,r11        and map


***********************************************************************
*  MAINLINE                                                           *
***********************************************************************
         jas   r14,ENVVLOAD       LOAD ENVIRONMENT     VARIABLES
*
         jas   R14,Parmload       LOAD MR95 PARAMETER  TABLE
*
         jas   R14,EDITParm       EDIT PARAMETERS
*
         BRAS  R9,PRNTRPT         PRINT CONTROL REPORT
*
         llgt  R14,EXECDADR       TRACE OPTION SPECIFIED ?
         USING EXECDATA,R14
         if (cli,exectrac,ne,c'Y')
           rptit msg=vb_blankl
           phead hd=tprm
           rptit msg=nonemsg        no trace options tell them
           rptit msg=vb_blankl
           phead hd=topt
           rptit msg=nonemsg        no trace options tell them
           drop  r14
         else
*
           llgt R2,TRACDCBA
           using ihadcb,r2
           if clc,namepgm,eq,=cl8'GVBMR95R'  R for reference
              mvc dcbddnam,=cl8'REFRTRAC'   default is EXTRTRAC
           endif
*
           MVC WKREENT(8),OPENPARM      OPEN PARAMETER  LIST
           sysstate amode64=NO
           sam31
           OPEN ((R2),(OUTPUT)),MODE=31,MF=(E,WKREENT) TRACE FILE
           sam64
           sysstate amode64=YES
*
           jas R14,Tracload       read trace parameters
*
*          jas R14,PRNTTRC        print trace options
*
         endif
*
mainvdp  ds    0h
         logit msg=tracprd,debug=Y "Parm Validation complete"
*
         logit msg=tracvdb,debug=Y "VDP load begin"
*
         Larl  R15,VDPLOAD        LOAD  VIEW DEFINITION PARAMETER TABLE
         BASR  R14,R15
         logit msg=tracvdd,debug=Y "VDP load finished"
*
         logit msg=tracltb,debug=Y "Logic table load beg"
         xgr   r1,r1              clear r1 in case "PUT" changed it
         Larl  R15,LTBLLOAD       LOAD  LOGIC   TABLE
         BASR  R14,R15
         logit msg=tracltd,debug=Y "Logic table load finished"

         llgt  R14,EXECDADR       TRACE OPTION SPECIFIED ?
         USING EXECDATA,R14
         if (cli,exectrac,eq,c'Y')
           jas R14,PRNTTRC        print trace options
         endif

         llgf  r15,runviews_a      get address and amode
         bassm R14,r15            Load Runviews data, if present
*
         jas   r14,INIT_IO        Initialise any IO modules
*
         logit msg=traclob,debug=Y
         Larl  R15,CLONLTBL       CLONE "RE/ES" ROWS
         BASR  R14,R15
         logit msg=traclod,debug=Y
*
         larl  R15,MEMALLOC       ALLOCATE MEMORY WORK AREAS
         BASR  R14,R15
*
         if clc,namepgm,eq,=cl8'GVBMR95E'    extract phase
           logit msg=tracrfb,debug=Y
           larl  R15,LOADLKUP       LOAD MEMORY RESIDENT LOOK-UP   TABL
           BASR  R14,R15
           logit msg=tracrfd,debug=Y
         endif
*
         xc    thrd_extrbyte_cnt,thrd_extrbyte_cnt
         xc    thrd_extrrec_cnt,thrd_extrrec_cnt

         larl  R15,THRDBLD        BUILD THREAD WORKAREA FOR EACH THREAD
         BASR  R10,R15
*
         larl  R15,ALLOCLIT       ALLOCATE LITERAL POOL            pgc4
         BASR  R14,R15                                             pgc4
*
         lay   R14,litptots     RTC19720 initialize literal pool totals
         using litptots,r14
         xc  Tlteslpsz,Tlteslpsz
         xc  Tlitppct1,Tlitppct1
         xc  TllitpTOT,TllitpTOT
         xc  Tlitppct2,Tlitppct2
         xc  TlitpCFEC,TlitpCFEC
         xc  Tlitppct3,Tlitppct3
         xc  TlitpLKS,TlitpLKS
         xc  Tlitppct4,Tlitppct4
         xc  TlitpDTC,TlitpDTC
         xc  Tlitppct5,Tlitppct5
         drop r14
*
         logit msg=tracp1b,debug=Y
         larl  R15,PASS1          PASS    ONE THROUGH THE  LOGIC TABLE
         BASR  R14,R15
         logit msg=vb_blankl,debug=Y
         logit msg=tracp1d,debug=Y
*
         logit msg=tracopb,debug=Y
         larl  R15,OPENEXTF       OPEN  EXTRACT  FILES
         BASR  R14,R15
         logit msg=tracopd,debug=Y

***********************************************************************
*  Print the ~VIEW report section to MR95 RTP                         *
***********************************************************************

         bas   R9,prntview        Print Views

***********************************************************************
*  Print the ~LITP report section to MR95E RPT                        *
***********************************************************************
         if clc,namepgm,eq,=cl8'GVBMR95E'
           if (ltgf,r9,LITPCNTA,nz) then
             llgt  r5,litstata
             do from=(r9)                        Loop all LITP's
               bras  r10,prlitpst
               aghi  R5,lp_stat_len
               doexit (c,r5,gt,litstate)
             enddo    , LITP stat items
             bras  r10,litpsummary               And summarize

             LLGT  R1,LITSTATA                   Release temporary area
             AGHI  R1,-l'litstateyeb             used for keeping LITP
             LLGF  R0,LITSTATS                   stats
             AGHI  R0,l'litstateyeb
             FREEMAIN RU,LV=(0),A=(1)
           endif
         endif

***********************************************************************
*  Print the ~IRUN report section to MR95 RTP                         *
***********************************************************************

         if clc,namepgm,eq,=cl8'GVBMR95E'    extract phase
           bas   R10,prirunst       Print Views
         else
           bas   R10,prirunstr      Print Views
         endif

***********************************************************************
*  Print the ~IRWF report section to MR95 RTP                         *
***********************************************************************

         if clc,namepgm,eq,=cl8'GVBMR95E'    extract phase
           bas   R10,prirwfst       Print Views
         endif

***********************************************************************
*  RETURN TO CALLING PROGRAM                                          *
***********************************************************************
RETURN   xgr   R15,R15            SET  RETURN CODE TO ZERO
*
RETURNE  llgt  R13,THRDMAIN       RESTORE  ADDRESS OF CALLER'S WORKAREA
*
         MVC   0(saver_len,R13),REGSAVE  restore caller's regs
*
         L     R14,savgrs14        RESTORE REGISTER  R14
         LM    R0,R12,savgrs0      RESTORE REGISTERS R0 - R12
         LMH   R14,R12,SAVE_GRANDE RESTORE THE HIGH  HALVES
         BSM   0,R14              RETURN

***********************************************************************
*  STANDARD ERROR DESCRIPTION FORMATTING (LOGIC TABLE RELATED ERRORS) *
***********************************************************************
         USING LOGICTBL,R7
*
STDERROR lay   r15,MDLERRTX
         MVC   ERRDATA,0(r15)     COPY  MODEL INDICATIVE DATA TEMPLATE
*
         MVC   ERRDATA+MDLERRF#-MDLERRTX(8),SPACES
*
         CLC   LTMAJFUN,RE_NX
         JNE   STDERR10
*
         MVC   ERRDATA+MDLERRF#-MDLERRTX(8),LTFILEDD  ASSUME V3 DDNAME
         CLC   LTFILEDD+4(4),HEXFF                    V4 DDNAME ???
         JNE   STDERR10                               NO  -  BRANCH
*
         L     R0,LTFILEID        EVENT FILE  ID
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   ERRDATA+MDLERRF#-MDLERRTX(L'MDLERRF#),NUMMASK
         ED    ERRDATA+MDLERRF#-MDLERRTX(L'MDLERRF#),DBLWORK+4
         MVI   ERRDATA+MDLERRF#-MDLERRTX,C' '
*
STDERR10 L     R0,LTROWNO         ROW   NUMBER  WITHIN   LOGIC   TABLE
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   ERRDATA+MDLERRR#-MDLERRTX(L'MDLERRR#),NUMMASK
         ED    ERRDATA+MDLERRR#-MDLERRTX(L'MDLERRR#),DBLWORK+4
         MVI   ERRDATA+MDLERRR#-MDLERRTX,C'='
*
         L     R0,LTVIEW#         VIEW  NUMBER
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   ERRDATA+MDLERRV#-MDLERRTX(L'MDLERRV#),NUMMASK
         ED    ERRDATA+MDLERRV#-MDLERRTX(L'MDLERRV#),DBLWORK+4
         MVI   ERRDATA+MDLERRV#-MDLERRTX,C'='
*
         MVC   ERRDATA+MDLERRFC-MDLERRTX(L'MDLERRFC),LTFUNC
*
RTNERROR DC    0H
*
* R14 Contains the error message ID
*
         C     R14,abend_msg
         JNE   RTNERR05
*
         DC    XL4'FFFFFFFF'
*
RTNERR05 DS    0H
*
         jas   R9,MSGLOG          write message to the log
*
         if (cghi,R14,eq,EMPTY_REF_LOGIC_TBL)             Logic Table
           lghi R15,1                                     Empty ?????
         else
           if (cgij,R14,eq,EMPTY_LOGIC_TBL)               Logic Table
             lghi R15,1                                   Empty ?????
           else
             lghi r15,8
           endif
         endif
*
         J     RETURNE
*
         DROP  R7
         DROP  r13

***********************************************************************
*  Format ERROR MESSAGE                                               *
***********************************************************************
*        Dsect to map the calling structure for GVBUTMSG
MSGLIST  GVBMSG DSECT,PREFIX=MSG
*
*Parmlist dsect ,
*msgno#   ds    f                  number of message to be formatted
*buffer_a ds    a                  buffer for output
*buffer_l ds    f                  length of buffer
*no_subs  ds    f                  number of substitutions that follow
*subs_start ds  0f
*subs_a   ds    a                  address of substitution text
*subs_l   ds    f                  length of text
*              (these last two fields may repeat no_subs times)
*        Set up parameter list as above and call GVBUTMSG and
*        formatted message is returned in buffer_a area
*
gvbmr96  csect
code     loctr
         push using
         using thrdarea,r13
         using savf4sa,saveziip   use this save area - as it is free
         using genenv,env_area
*RRformat ds   0h
MSGLOG    ds   0h
*
* Write error message to the log
*
         stmg  r14,r12,SAVF4SAG64RS14 save caller's registers
         LAY   R1,MSG_AREA        Address parm list area
         Using MSGLIST,r1
         ST    R14,MSGNUM         Save message number in parms
         MVI   MSGTYPE,c'L'       Type parameter
         LA    R0,0               No prefix override
         ST    R0,MSGPFX          STORE INTO PARAM. LIST
         LA    R0,GENENV          Address of GENENV
         ST    R0,MSGGENV         STORE INTO PARAM. LIST
         LA    r2,prntbuff        find the buffer address
         ST    R2,MSGBUFFA        STORE INTO PARAM. LIST
         MVC   MSGBUFFL,=A(l'prntbuff)  and the length
*
         xc    MSG#SUB,MSG#SUB    clear out number of subs
         mvi   MSG#SUB+3,1          and put a one there
         LA    R2,modname         First parm is source name
         LA    R3,8
*
         la    r4,MSGS1PTR          set pointer
subs     using MSGS1PTR,r4
         stm   r2,r3,subs.MSGS1PTR  save the adlen pair
         aghi  r4,l'MSGS1PTR+l'MSGS1LEN move pointer
* check if any parms already set up - up to 3
         do    from=(r15,3)         room for 3 more max before errdata
           llgt  r2,subs.MSGS1PTR   get next parm address if any
           doexit ltgr,r2,r2,z       and exit if zero
           l     r14,MSG#SUB        increment no of parms
           la    r14,1(,r14)
           st    r14,MSG#SUB
           aghi  r4,l'MSGS1PTR+l'MSGS1LEN move pointer
         enddo ,
* Add last parm from ERRDATA
         if    CLC,ERRDATA,ne,SPACES  ANY INDICATIVE DATA ???
           LA  R2,errdata           point at the error data
           la  r3,l'errdata-1       and get the length
           la  r14,0(r3,r2)         point at last char
           la  r3,1(,r3)            and make this actual length
           do  while=(cli,0(r14),eq,c' ') scan until non blank at end
             bctgr r14,0             Move pointer back by one
             bctgr r3,0              reduce field length by one
           enddo
           stm r2,r3,subs.MSGS1PTR  save the adlen pair
           aghi r4,l'MSGS1PTR+l'MSGS1LEN move pointer
           drop subs
           l     r14,MSG#SUB        increment no of parms
           la    r14,1(,r14)
           st    r14,MSG#SUB
         endif
*
         L     15,=V(GVBUTMSG)
         BASSM 14,15
*
         lay   r1,MSGS2PTR          point at field
         xc    0(4,r1),0(r1)        make this zero for the next time
*
         lmg   r14,r12,SAVF4SAG64RS14 restore caller's registers
         br    r9                 return
         pop   using
                        EJECT

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
*        Print MR95RPT "Report Section"                               *
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
         USING THRDAREA,R13
*
Print_Rptsect ds 0h
         sysstate amode64=NO
         phead hd=cont           Print heading for this section
         rptit msg=rptcont_hd1   column headings
         rptit msg=rptcont_hd2   column  underlining
         xc   prntrdwl,prntrdwl
         mvc  prntline(4),spaces
         if clc,namepgm,eq,=cl8'GVBMR95R'    reference phase
           lay r2,rhparms+24
           mvc 0(4,r2),=cl4'REFR'
           lay r2,rhtprms+24
           mvc 0(4,r2),=cl4'REFR'
           lay r2,rhENVVs+24
           mvc 0(4,r2),=cl4'REFR'
         endif
         pcont cont=ENVV
         pcont cont=parm
         pcont cont=opts
         pcont cont=tprm
         pcont cont=topt
         pcont cont=runv
         pcont cont=view

         if clc,namepgm,eq,=cl8'GVBMR95E'    extract phase
           pcont cont=litp
         endif

         pcont cont=irun
         if clc,namepgm,eq,=cl8'GVBMR95E'    extract phase
           pcont cont=irwf
         else
           pcont cont=iref
         endif

         if clc,namepgm,eq,=cl8'GVBMR95E'    extract phase
           pcont cont=isrc
         endif

         pcont cont=oext

         if clc,namepgm,eq,=cl8'GVBMR95E'    extract phase
           pcont cont=owrt
         endif

         if clc,namepgm,eq,=cl8'GVBMR95E'    extract phase
           pcont cont=lkup
           pcont cont=mems
         endif

         pcont cont=zcpu
         pcont cont=exec

         rptit msg=vb_blankl
         sysstate amode64=YES
         bsm   0,r9
         DROP  R13
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        A L L O C A T E   E X T R A C T   F I L E   A R E A          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
ALLOEXTF ds    0h
         stmg  r2,r4,dblwork2     save in dblwork2, dblwork3, dblwork4
         LH    R0,MAXEXTF#        LOAD NUMBER  OF EXTRACT FILES
         LTR   R0,R0              ANY  FILES   ???
         JNP   ALLOEXIT           BRANCH   IF  NO EXTRACT FILES
*
         LHI   R1,EXTFILEL        LOAD LENGTH  OF EACH  CONTROL ELEMENT
         MR    R0,R0              COMPUTE SIZE OF ALL   CONTROL AREAS
         sty   r1,filecnt_real    Use this as it is temp
         LR    R2,R1              SAVE LENGTH
         LA    R0,8(,R1)          ADD  EYEBALL LENGTH
         GETMAIN RU,LV=(0),LOC=(ANY)   OBTAIN  MEMORY
*
         MVC   0(l'extfeyeb,R1),EXTFEYEB   COPY EYEBALL
         ahi   R1,l'extfeyeb      SKIP OVER    EYEBALL
         st    R1,EXTFILEA        SAVE AREA    ADDRESS
*
         lgr   R14,R1             ZERO AREA
         lr    r15,r2
         xgr   R0,R0
         XR    R1,R1
         MVCL  R14,R0
*
         LH    R0,MAXSTDF#
         LTR   R0,R0
         JNP   ALLOEXIT
*
         STH   R0,FILECNT
*
         llgt  R1,EXTFILEA
         USING EXTFILE,R1
*
         llgt  r3,vdp0801a       Get the 801 extract file record
         using vdp0801b_extract_files_record,r3
         la    r4,vdp0801b_extrfile_ddlist get to where dds start
         lh    r0,vdp0801b_extrfile_count  get number of extract files

         if ltr,r0,r0,p           make sure r0 positive
           do from=(r0)
             xc  extcnt,extcnt
             xc  extbytec,extbytec
*
             ST R1,EXTPRINT       INITIALIZE  PRINT   SEQUENCE
*
      mvc extddnam(l'vdp0801b_extrfile_prefix),vdp0801b_extrfile_prefix
             drop r3
             using vdp0801b_EXTRFILE_DDLIST,r4
             lh r15,vdp0801b_extrfile_ddentry
             cvd r15,dblwork
             oi dblwork+l'dblwork-1,x'0f'
             if chi,r15,le,999             three or four digit ???
               unpk extddnam+l'vdp0801b_extrfile_prefix(3),dblwork
               mvi extddnam+l'vdp0801b_extrfile_prefix+3,c' '
             else
               unpk extddnam+l'dumy(4),dblwork
             endif

             aghi r4,l'vdp0801b_extrfile_ddentry next entry
             aghi R1,EXTFILEL
           enddo
         endif
         drop  r4
*
ALLOEXIT ds    0h
         lmg   r2,r4,dblwork2 restore from dblwork2, dblwork3, dblwork4
         BR    R9             RETURN
*
         DROP  R1
         DROP  R13

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "VNAMSRCH"  -  GLOBAL VARIABLE NAME LIST SEARCH                     *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - MATCHING LIST ENTRY ADDRESS (NOT FOUND = 0)            *
*        R14 - PREVIOUS LIST ENTRY ADDRESS (OUTPUT)                   *
*        R9  - RETURN   ADDRESS                                       *
*        R1  - VARIABLE NAME ADDRESS       (INPUT)                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         USING LOGICTBL,R15
*
VNAMSRCH xgr   R14,R14            INITIALIZE  PREVIOUS LIST ENTRY
         J     VNAM0020           BRANCH AND  CHECK    FOR  NULL  LIST
*
VNAM0010 CLC   LTVNNAME,0(R1)     MATCHING    VARIABLE NAME ???
         JE    VNAMEXIT           YES - FOUND
*
         lgr   R14,R15            ADVANCE TO NEXT LIST ENTRY
         llgt  R15,LTVNNEXT
*
VNAM0020 ltgr  R15,R15            END-OF-LIST ???
         JP    VNAM0010           NO  - CONTINUE   SCANNING LIST
*
VNAMEXIT BR    R9                 RETURN
*
         DROP  R13
         DROP  R15

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCEXIT"  -  LOCATE EXIT PROGRAM RECORD (210) IN VDP               *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - "VDP" EXIT PROGRAM RECORD ADDRESS (NOT FOUND = 0)      *
*        R14 - WORK  REGISTER                                         *
*        R9  - RETURN ADDRESS            (INPUT)                      *
*        R1  - EXIT   PROGRAM  ID        (INPUT)                      *
*        R0  - WORK  REGISTER                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
LOCEXIT  ltg   R15,first_210      SEARCH  "VDP"
         jnp   Locexit_e
         USING vdp0210b_EXIT_PGM_RECORD,R15
*
outerexit do ,                         R15 loaded with start pointer
            do until=(ltg,r15,vdp0210b_next,z) loop thru all exit recs
              doexit (c,r1,eq,vdp0210b_record_id),do=outerexit
*              get out if it is a type 210 record with a matching
*              record Id - exit from both loops
            enddo ,
          enddo ,
Locexit_e ds 0h
          br   r9
*
         DROP  R13
         DROP  R15

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCVIEW"  -  LOCATE VIEW  DEFN  RECORD (1000) IN VDP               *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - "VDP" VIEW  COLUMN RECORD ADDRESS (NOT FOUND = 0)      *
*        R14 - WORK  REGISTER                                         *
*        R9  - RETURN ADDRESS            (INPUT)                      *
*        R1  - EXIT   PROGRAM  ID        (INPUT)                      *
*        R0  - WORK  REGISTER                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
LOCVIEW  ltg   R15,first_1000     SEARCH  "VDP"
         jnp   Locview_e
         USING vdp1000b_VIEW_RECORD,R15
*
outerview do ,
            do until=(ltg,r15,vdp1000b_next,z) loop thru all exit recs
*             if record type is okay and matching view #, then exit
*             as we have the record we need
              doexit (c,r1,eq,vdp1000b_viewid),do=outerview
            enddo ,
          enddo ,
Locview_e ds 0h
          br   r9
*
         DROP  R13
         DROP  R15

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "Loccid"   -  Locate Control Record (50) in VDP                     *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        r15 - "VDP" Control record address (Not Found = 0)           *
*        r14 - Work  Register                                         *
*        r9  - Return address            (INPUT)                      *
*        r1  - Control record id         (INPUT)                      *
*        r0  - Work Register                                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
Loccid   ltg   R15,first_50       SEARCH  "VDP"
         jnp   Loccid_e
         USING vdp0050b_Control_Record,R15
*
outercid  do ,
            do until=(ltg,r15,vdp0050b_next,z) loop thru all exit recs
*             if record type is okay and matching control id, then exit
*             as we have the record we need
              doexit (c,r1,eq,vdp0050b_RECORD_ID),do=outercid
            enddo ,
            xgr r15,r15        If inner loop exits/ends, then not found
          enddo ,
Loccid_e ds 0h
          br   r9
*
         DROP  R13
         DROP  R15

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "Process_cfcc" - The  CFCC verb is a special case, previously       *
*                  this processing was done in GVBMR91 but is now     *
*                  moved to here along with the "Cookies" processing. *
*                  With a CFCC verb we can resolve the compare        *
*                  now and either eliminate the code completely (make *
*                  it do NOOP verb processing) or just have a branch  *
*                  statement which is the default for this verb.      *
*                  The branch will be a branch FALSE                  *
*                  NOTE:                                              *
*                  This will not change the verb entry in the logic   *
*                  table, but rather the code segment will point to   *
*                  NOOP or CFCC branch code instead.                  *
*                                                                     *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*     Input:                                                          *
*        r3  - logic record from DD MR95LTBL                          *
*        r5  - Function table entry for CFCC                          *
*        r7  - Where the next in memory logic table entry will go     *
*        r9  - Return address                                         *
*     Output                                                          *
*        r5  - points to the CFCC or NOOP function table entry        *
*                                                                     *
*     All other register used will be work registers                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using ltcc_rec,r3
         using functbl,r5
*
Process_cfcc ds 0h
*
s1       using savf4sa,saveziip
         stmg  r14,r9,s1.SAVF4SAG64RS14

         using ltcc_rec,r3
         if tm,ltcc_value1_length,x'80',o
           la r1,ltcc_value1_area   This is our parm to Cookie subrtn
           jas r9,Cookie       Call Cookie subroutine
           sty   r0,cfcc_content
           sty   r1,cfcc_length
         else
           ly   r1,ltcc_value1_length
           sty  r1,cfcc_length
         endif
         if tm,ltcc_value2_length,x'80',o
           la r1,ltcc_value2_area   This is our parm to Cookie subrtn
           jas r9,Cookie       Call Cookie subroutine
         else
          ly  r1,ltcc_value2_length
         endif
         if cy,r1,gt,cfcc_length       use the longer length
           sty   r1,cfcc_length
         endif
*
         ly    r15,cfcc_length         Get length to compare
         bctr  r15,r0                  Decrement for "EX"
         lgf   r14,ltcc_compare_type   Get the numeric compare type
         bctr  r14,r0                  Decrement to get correct
         mghi  r14,proc_cfcc_comp_len    branch offset
         lay   r14,proc_cfcc_eq(r14)
         br    r14

proc_cfcc_eq ds 0h
         ex    r15,cfcccomp
         je    proc_cfcc_neednoop  EQ so true, no code needed
         j     Proc_cfcc_exit      NE so std false branch
proc_cfcc_comp_len equ *-proc_cfcc_eq

proc_cfcc_ne ds 0h
         ex    r15,cfcccomp
         jne   proc_cfcc_neednoop  NE so true, no code needed
         j     Proc_cfcc_exit      EQ so std false branch

proc_cfcc_gt ds 0h
         ex    r15,cfcccomp
         jh    proc_cfcc_neednoop  GT so true, no code needed
         j     Proc_cfcc_exit      LE so std false branch

proc_cfcc_ge ds 0h
         ex    r15,cfcccomp
         jnl   proc_cfcc_neednoop  GE so true, no code needed
         j     Proc_cfcc_exit      LT so std false branch

proc_cfcc_lt ds 0h
         ex    r15,cfcccomp
         jl    proc_cfcc_neednoop  LT so true, no code needed
         j     Proc_cfcc_exit      GE so std false branch

proc_cfcc_le ds 0h
         ex    r15,cfcccomp
         jnh   proc_cfcc_neednoop  LE so true, no code needed
         j     Proc_cfcc_exit      GT so std false branch

Proc_cfcc_neednoop ds 0h

*        Need to find the NOOP function code, this needs to be after
*        the CFCC function for this to work correctly. Have put a
*        comment to this effect in GVBMR95

         do while=(clc,fcfunc,ne,NOOP)
           aghi R5,FCENTLEN ADVANCE TO NEXT ENTRY
         enddo


Proc_cfcc_exit ds 0h
         lmg   r14,r4,s1.SAVF4SAG64RS14
         lmg   r6,r9,s1.SAVF4SAG64RS6
         br    r9
*
static   loctr ,
cfcccomp clc   ltcc_value1(0),ltcc_value2   * *  E X E C U T E D  * *

code     loctr
*
         DROP  R13,r3,r5

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "Cookie"   -  Replaces any cookies found in LT values for           *
*               record types F1, F1, V1, V2 with the appropiate       *
*               date value depending on the cookie type.              *
*               If the value length is negative then this will        *
*               signify it is a cookie                                *
*               The length and value fields will be update with       *
*               the correct date and length values.                   *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        r15 - "VDP" Control record address (Not Found = 0)           *
*        r14 - Work  Register                                         *
*        r9  - Return address                (INPUT)                  *
*        r1  - points to a LT??_VALUE? field (INPUT)                  *
*        r1  - contains content code         (OUTPUT)                 *
*        r0  - Work Register                                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
timepart equ 8                    length of time part of cookie
Cookie   ds   0h
*
s1       using savf4sa,Cookie_save
         stmg  r14,r9,s1.SAVF4SAG64RS14
         lgr   r7,r1              Save input values
         lgf   r15,0(,r7)         Get the cookie type
         lpgr  r15,r15            Make it positive
         bctgr r15,r0
         sllg  r15,r15,2          set up to branch to correct routine
         larl  r0,cookietb
         agr   r15,r0
         br    r15
cookietb j     RunDay                   .x'ffffffff'
         j     RunMonth                 .x'fffffffe'
         j     RunYear                  .x'fffffffd'
         j     FirstOfQuarter           .x'fffffffc'
         j     LastOfQuarter            .x'fffffffb'
         j     FirstOfQ1                .x'fffffffa'
         j     FirstOfQ2                .x'fffffff9'
         j     FirstOfQ3                .x'fffffff8'
         j     FirstOfQ4                .x'fffffff7'
         j     LastOfQ1                 .x'fffffff6'
         j     LastOfQ2                 .x'fffffff5'
         j     LastOfQ3                 .x'fffffff4'
         j     LastOfQ4                 .x'fffffff3'
         j     RunPeriod                .x'fffffff2'
         j     BatchDate                .x'fffffff1'
         j     FiscalDay                .x'fffffff0'
         j     FiscalMonth              .x'ffffffef'
         j     FiscalYear               .x'ffffffee'
         j     FiscalPeriod             .x'ffffffed'
         j     FiscalFirstOfQuarter     .x'ffffffec'
         j     FiscalLastOfQuarter      .x'ffffffeb'
RunDay ds 0h
         ly   r1,cur_rdaynum        .Get the RUNDAY day number
         a    r1,4(,r7)             .Add relative days to day number
         jas  r9,conv_daynum_date   .Convert new day number to a date
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cymd       Set content
         lhi  r1,cymd_l     Set content length
         j    Cookie_exit
RunMonth ds 0h
         lay  r1,cur_brdate         .using run date as base
         jas  r9,Relmth
         lay  r15,cur_rdate
         mvc  10(l'cur_rday,r7),6(r15)       .move in day
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cym         Set content
         lhi  r1,cym_l       Set content length
         j    Cookie_exit
RunYear ds 0h
         ly   r1,cur_bryear         .Get the binary rundate year
         a    r1,4(,r7)             .Update the year
         lay  r15,cur_rdate
         mvc  4(l'cur_rdate,r7),0(r15)       .move in default
         cvd  r1,dblwork            .Update the year and make
         unpk 4(4,r7),dblwork+5(3)  .   year
         oi   4+3(r7),c'0'          .   char
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,ccyy           Set content
         lhi  r1,ccyy_l         Set content length
         j    Cookie_exit
FirstOfQuarter ds 0h
         lay  r15,cur_rdate
         mvc  4(l'cur_rdate,r7),0(r15)
         ly   r15,cur_brmth    .get binary month
         sll  r15,2            .get word offset
         ly   r15,First_mth_q_tab(r15) .Get 1st month/day of qtr
         st   r15,8(,r7)       .save it after length and year
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cymd       Set content
         lhi  r1,cymd_l     Set content length
         j    Cookie_exit
LastOfQuarter ds 0h
         lay  r15,cur_rdate
         mvc  4(l'cur_rdate,r7),0(r15)
         ly   r15,cur_brmth    .get binary month
         sll  r15,2            .get word offset
         ly   r15,Last_mth_q_tab(r15) .Get last month/day of qtr
         st   r15,8(,r7)       .save it after length and year
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cymd       Set content
         lhi  r1,cymd_l     Set content length
         j    Cookie_exit
FirstOfQ1 ds 0h
         lay  r15,cur_rdate
         mvc  4(l'cur_ryear,r7),0(r15)       .move in default
         mvc  4+l'cur_ryear(l'cur_rmth+l'cur_rday,r7),=c'0101'
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cymd       Set content
         lhi  r1,cymd_l     Set content length
         j    Cookie_exit
FirstOfQ2 ds 0h
         lay  r15,cur_rdate
         mvc  4(l'cur_ryear,r7),0(r15)       .move in default
         mvc  4+l'cur_ryear(l'cur_rmth+l'cur_rday,r7),=c'0401'
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cymd       Set content
         lhi  r1,cymd_l     Set content length
         j    Cookie_exit
FirstOfQ3 ds 0h
         lay  r15,cur_rdate
         mvc  4(l'cur_ryear,r7),0(r15)       .move in default
         mvc  4+l'cur_ryear(l'cur_rmth+l'cur_rday,r7),=c'0701'
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cymd       Set content
         lhi  r1,cymd_l     Set content length
         j    Cookie_exit
FirstOfQ4 ds 0h
         lay  r15,cur_rdate
         mvc  4(l'cur_ryear,r7),0(r15)       .move in default
         mvc  4+l'cur_ryear(l'cur_rmth+l'cur_rday,r7),=c'1001'
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cymd       Set content
         lhi  r1,cymd_l     Set content length
         j    Cookie_exit
LastOfQ1 ds 0h
         lay  r15,cur_rdate
         mvc  4(l'cur_ryear,r7),0(r15)       .move in default
         mvc  4+l'cur_ryear(l'cur_rmth+l'cur_rday,r7),=c'0331'
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cymd       Set content
         lhi  r1,cymd_l     Set content length
         j    Cookie_exit
LastOfQ2 ds 0h
         lay  r15,cur_rdate
         mvc  4(l'cur_ryear,r7),0(r15)       .move in default
         mvc  4+l'cur_ryear(l'cur_rmth+l'cur_rday,r7),=c'0630'
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cymd           Set content
         lhi  r1,cymd_l         Set content length
         j    Cookie_exit
LastOfQ3 ds 0h
         lay  r15,cur_rdate
         mvc  4(l'cur_ryear,r7),0(r15)       .move in default
         mvc  4+l'cur_ryear(l'cur_rmth+l'cur_rday,r7),=c'0930'
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cymd           Set content
         lhi  r1,cymd_l         Set content length
         j    Cookie_exit
LastOfQ4 ds 0h
         lay  r15,cur_rdate
         mvc  4(l'cur_ryear,r7),0(r15)       .move in default
         mvc  4+l'cur_ryear(l'cur_rmth+l'cur_rday,r7),=c'1231'
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cymd           Set content
         lhi  r1,cymd_l         Set content length
         j    Cookie_exit
RunPeriod ds 0h
         lay  r15,cur_rdate
         mvc  4(l'cur_rdate,r7),0(r15)       .move in default
         mvc  0(4,r7),=al4(l'cur_rdate+timepart)
         xc   4+l'cur_rdate(timepart,r7),4+l'cur_rdate(r7)
         lhi  r0,cymd           Set content
         lhi  r1,cymd_l         Set content length
         j    Cookie_exit
BatchDate ds 0h
         ly   r1,cur_bdaynum        .Get the RUNDAY day number
         a    r1,4(,r7)             .Add relative days to day number
         jas  r9,conv_daynum_date   .Convert new day number to a date
         mvc  0(4,r7),=al4(l'cur_bdate+timepart)
         xc   4+l'cur_bdate(timepart,r7),4+l'cur_bdate(r7)
         lhi  r0,cymd           Set content
         lhi  r1,cymd_l         Set content length
         j    Cookie_exit
FiscalDay ds 0h
         ly   r1,cur_fdaynum        .Get the RUNDAY day number
         a    r1,4(,r7)             .Add relative days to day number
         jas  r9,conv_daynum_date   .Convert new day number to a date
         mvc  0(4,r7),=al4(l'cur_fdate+timepart)
         xc   4+l'cur_fdate(timepart,r7),4+l'cur_fdate(r7)
         lhi  r0,cymd           Set content
         lhi  r1,cymd_l         Set content length
         j    Cookie_exit
FiscalMonth ds 0h
         lay  r1,cur_bfdate         .using run date as base
         jas  r9,Relmth
         lay  r15,cur_fdate
         mvc  10(l'cur_fday,r7),6(r15)       .move in day
         mvc  0(4,r7),=al4(l'cur_fdate+timepart)
         xc   4+l'cur_fdate(timepart,r7),4+l'cur_fdate(r7)
         lhi  r0,cym       Set content
         lhi  r1,cym_l     Set content length
         j    Cookie_exit
FiscalYear ds 0h
         ly   r1,cur_bfyear         .Get the binary fiscal year
         a    r1,4(,r7)             .Update the year
         lay  r15,cur_fdate
         mvc  4(l'cur_fdate,r7),0(r15) .Move in current run date
         cvd  r1,dblwork            .Update the year and make
         unpk 4(4,r7),dblwork+5(3)  .   year
         oi   4+3(r7),c'0'          .   char
         mvc  0(4,r7),=al4(l'cur_fdate+timepart)
         xc   4+l'cur_fdate(timepart,r7),4+l'cur_fdate(r7)
         lhi  r0,ccyy       Set content
         lhi  r1,ccyy_l     Set content length
         j    Cookie_exit
FiscalPeriod ds 0h
         lay  r15,cur_fdate
         mvc  4(l'cur_fdate,r7),0(r15) .Move in current run date
         mvc  0(4,r7),=al4(l'cur_fdate+timepart)
         xc   4+l'cur_fdate(timepart,r7),4+l'cur_fdate(r7)
         lhi  r0,cymd           Set content
         lhi  r1,cymd_l         Set content length
         j    Cookie_exit
FiscalFirstOfQuarter ds 0h
         lay  r15,cur_fdate
         mvc  4(l'cur_fdate,r7),0(r15)
         ly   r15,cur_bfmth    .get binary month
         sll  r15,2            .get word offset
         ly   r15,First_mth_q_tab(r15) .Get 1st month/day of qtr
         st   r15,8(,r7)       .save it after length and year
         mvc  0(4,r7),=al4(l'cur_fdate+timepart)
         xc   4+l'cur_fdate(timepart,r7),4+l'cur_fdate(r7)
         lhi  r0,cymd           Set content
         lhi  r1,cymd_l         Set content length
         j    Cookie_exit
FiscalLastOfQuarter ds 0h
         lay  r15,cur_fdate
         mvc  4(l'cur_fdate,r7),0(r15)
         ly   r15,cur_bfmth    .get binary month
         sll  r15,2            .get word offset
         ly   r15,Last_mth_q_tab(r15) .Get last month/day of qtr
         st   r15,8(,r7)       .save it after length and year
         mvc  0(4,r7),=al4(l'cur_fdate+timepart)
         xc   4+l'cur_fdate(timepart,r7),4+l'cur_fdate(r7)
         lhi  r0,cymd           Set content
         lhi  r1,cymd_l         Set content length
         j    Cookie_exit
Cookie_exit ds 0h
         lmg   r14,r15,s1.SAVF4SAG64RS14
         lmg   r2,r9,s1.SAVF4SAG64RS2
         br    r9
*
         DROP  R13

*----------------------------------------------------------------------
*
*        This routine converts a date to a day number using the
*        following formula
*
*          m = (m +9) mod 12
*          y = y - m / 10
*          return value = 365*y + y/4 - y/100 + y/400 + (m*306 +5)/10+
*                         (d - 1)
*
*        All division is integer division.
*
*        Input Fields: r1: Points to 3 fullwords containing binary
*                            Year
*                            Month
*                            Day
*
*        Output Fields: r1: The day number
*
*----------------------------------------------------------------------
         USING THRDAREA,R13
Conv_date_daynum ds 0h
         stmg r2,r5,cdd_save
         l    r4,0(,r1)            .Get binary year
         l    r5,8(,r1)            .Get binary day
         l    r1,4(,r1)            .Get binary month
         ahi  r1,9                 . m + 9
         sr   r0,r0
         lhi  r15,12
         dr   r0,r15               . (m + 9) mod 12
         lr   r3,r0                .save m value
         srdl r0,32
         lhi  r15,10
         dr   r0,r15               . m/10
         sr   r4,r1                . y = y - m/10
         sr   r0,r0
         lr   r1,r4
         mhi  r4,365               . 365 * y
         srl  r1,2                 .     y / 4
         ar   r4,r1                . + y / 4
         lhi  r14,25
         dr   r0,r14               .     y / 100 = (y / 4) / 25
         sr   r4,r1                . - y / 100
         srl  r1,2                 .     y / 400 = ((y / 4) / 25) / 4
         ar   r4,r1                . + y / 400
         mhi  r3,306               . m * 306
         ahi  r3,5                 . m * 306 + 5
         sr   r2,r2
         dr   r2,r15               . (m * 306 + 5) / 10
         ar   r4,r3
         ar   r4,r5                . + d
         ahi  r4,-1                . + (d - 1)
         lr   r1,r4

         lmg  r2,r5,cdd_save
         br   r9
         drop r13

*----------------------------------------------------------------------
*
*        This routine converts a day number into a date using the
*        following formula
*
*          y = (10000*g +14780) / 3652425
*          ddd = g - (365*y + y/4 - y/100  y/400)
*          if (ddd , 0) then
*            y = y - 1
*            ddd = g - (365*y + y/4 - y/100  y/400)
*          endif
*          mi = (100*ddd + 52) / 3060
*          mm = (mi + 2) mod 12 +1
*          y = y + (mi + 2) / 12
*          dd = ddd - (mi * 305 + 5) / 10 + 1
*
*        All division is integer division.
*
*        Input r1: Contains the day number
*              r7: Points to area to put the date (char)
*
*        Output Fields: Year
*                       Month
*                       Day
*
*----------------------------------------------------------------------
         USING THRDAREA,R13
Conv_daynum_date ds 0h
         lr   r4,r1            .Save day number
         lr   r14,r1           .Save day number
         srdl r4,32
         mghi r5,10000         .      daynum * 10000
         aghi r5,14780         .        + 14780
         lgr  r4,r5            .Set up r4 and r5
         srlg r4,r4,32         .  for divide
         llilf r15,3652425
         dr   r4,r15           .        / 3652425
         lr   r1,r5
         lr   r4,r5
         mhi  r4,365           . 365 * y
         srl  r1,2             .     y / 4
         ar   r4,r1            . + y / 4
         lhi  r15,25
         sr   r0,r0
         dr   r0,r15           .     y / 100 = (y / 4) / 25
         sr   r4,r1            . - y / 100
         srl  r1,2             .     y / 400 = ((y / 4) / 25) / 4
         ar   r4,r1            . + y / 400
         lr   r0,r14
         sr   r0,r4            . daynum - (365*y +y/4 - y/100 + y/400)
         jnm  dayok
         lay  r5,-1(,r5)       . y = y - 1
         lr   r1,r5
         lr   r4,r5
         mhi  r4,365           . 365 * y
         srl  r1,2             .     y / 4
         ar   r4,r1            . + y / 4
         sr   r0,r0
         dr   r0,r15           .     y / 100 = (y / 4) / 25
         sr   r4,r1            . - y / 100
         srl  r1,2             .     y / 400 = ((y / 4) / 25) / 4
         ar   r4,r1            . + y / 400
         lr   r0,r14
         sr   r0,r4            . daynum - (365*y +y/4 - y/100 + y/400)
dayok    ds   0h
         lr   r2,r0
         srdl r2,32
         mhi  r3,100           .  100 * ddd
         ahi  r3,52                 +52
         lhi  r15,3060
         dr   r2,r15           .  mi = (100 * ddd + 52) / 3060
         sr   r2,r2
         lr   r4,r3            .save mi
         ahi  r3,2             . mi + 2
         lhi  r15,12
         dr   r2,r15           . (mi + 2) mod 12
         ar   r5,r3            . y = y +(mi + 2) / 12
         ahi  r2,1             . mm = (mi + 2) mod 12 + 1
         lr   r1,r2            .save mm
         sr   r2,r2
         lr   r3,r4            .get mi back
         mhi  r3,306           . mi * 306
         ahi  r3,5             . mi * 306 + 5
         lhi  r15,10
         dr   r2,r15           . (mi * 306 + 5) / 10
         sr   r0,r3            . ddd - ((mi * 306 + 5) / 10)
         ahi  r0,1             . dd = ddd - ((mi * 306 + 5) / 10) + 1
         cvd  r5,dblwork       .Convert year
         unpk 4(4,r7),dblwork+5(3) .to
         oi   4+3(r7),c'0'     .    character
         cvd  r1,dblwork       .Convert month
         unpk 8(2,r7),dblwork+6(2) .to
         oi   8+1(r7),c'0'     .    character
         cvd  r0,dblwork       .Convert day
         unpk 10(2,r7),dblwork+6(2) .to
         oi   10+1(r7),c'0'    .     character

         br   r9               .return

         drop r13
*----------------------------------------------------------------------
*
*        This routine will change the date +/- x number of months
*        Will also check for cases where start date is july 31
*        changing to June that only has 30 days.
*
*        Input Fields: r1: Points to 3 fullwords containing binary
*                            Year
*                            Month
*                            Day
*
*                      r7:   ds fl04        Area that contains the
*                                           cookie type
*                                           (not used in this routine)
*                            ds cl256       1st 4 bytes contain the
*                                           relative month in binary
*
*        Output Fields: r7:  ds fl04        Will contain the length of
*                                           the newly created date
*                                           (date always 8 bytes long)
*                            ds cl256       The new date
*
*        Work registers     r0
*                           r2
*                           r3
*                           r14
*                           r15
*                           r2
*----------------------------------------------------------------------
         USING THRDAREA,R13

Relmth   ds   0h
         l    r3,4(,r1)        .Get binary month
         lhi  r15,12           .for divide later
         xr   r2,r2
         lt   r4,4(,r7)        .Get relative month value
         jnm  Pos_rmth
Neg_rmth ds   0h
         ar   r3,r4            .Current month + relative month
         ahi  r3,-12           .Subtract 12 mths to get correct years
         ahi  r2,-1            .Make all dividend negative
         dr   r2,r15           .get number of years
         ahi  r2,12            .New month
         l    r4,0(,r1)        .Get year in binary
         ar   r4,r3            .This is the new year
         j    Relmth_exit00
Pos_rmth ds   0h
         ar   r3,r4            .Current month + relative month
         ahi  r3,-1            .Subtract 1 mths to get correct years
         dr   r2,r15           .Get number of years
         ahi  r2,1             .Remainder+1 is months
         l    r4,0(,r1)        .Get the year in binary
         ar   r4,r3            .This is the new year
         lr   r15,r4           .Save year

Relmth_exit00 ds 0h
         cvd  r4,dblwork       .Make year
         unpk 4(4,r7),dblwork+5(3) .   a
         oi   4+3(r7),c'0'     .      char value
         cvd  r2,dblwork             .Make month
         unpk 4+4(2,r7),dblwork+6(2) .   a
         oi   4+4+1(r7),c'0'         .   char value
         sr   r15,r15          .Set RC=0
Relmth_exit   ds 0h
         br   r9               .Return

         drop r13
static   loctr          set up static loctr first
days_in_mth dc  0f
            dc  f'00'       dummy
            dc  f'31'       Jan 31 days
            dc  f'28'       Feb 28 days
            dc  f'31'       Mar 31 days
            dc  f'30'       Apr 30 days
            dc  f'31'       may 31 days
            dc  f'30'       Jun 30 days
            dc  f'31'       Jul 31 days
            dc  f'31'       Aug 31 days
            dc  f'30'       Sep 30 days
            dc  f'31'       Oct 31 days
            dc  f'30'       Nov 30 days
            dc  f'31'       Dec 31 days
*----------------------------------------------------------------------
*
*        First month of a quarter table is a series of 4 bytes entries
*        Each entry contains the first month of that quarter as well
*        as the first day of that month. For the 1st 3 months the 1st
*        month of the quarter is Jan, the 2nd 3 months it is Apr, the
*        3rd is Jul and the 4th is Oct. Of course the 1st day of each
*        of those 4 months is alway 01
*        It is setup like this to make thinks really quick to work out
*
*----------------------------------------------------------------------
*
First_mth_q_tab dc 0f
            dc  cl4'0000'   dummy
            dc  cl4'0101'   Jan 1st month of quarter is Jan
            dc  cl4'0101'   Feb 1st month of quarter is Jan
            dc  cl4'0101'   Mar 1st month of quarter is Jan
            dc  cl4'0401'   Apr 1st month of quarter is Apr
            dc  cl4'0401'   may 1st month of quarter is Apr
            dc  cl4'0401'   Jun 1st month of quarter is Apr
            dc  cl4'0701'   Jul 1st month of quarter is Jul
            dc  cl4'0701'   Aug 1st month of quarter is Jul
            dc  cl4'0701'   Sep 1st month of quarter is Jul
            dc  cl4'1001'   Oct 1st month of quarter is Oct
            dc  cl4'1001'   Nov 1st month of quarter is Oct
            dc  cl4'1001'   Dec 1st month of quarter is Oct
*----------------------------------------------------------------------
*
*        Last month of a quarter table is a series of 4 bytes entries
*        Each entry contains the last month of that quarter as well
*        as the last day of that month. For the 1st 3 months the last
*        month of the quarter is Mar, the 2nd 3 months it is Jun, the
*        3rd is Sep and the 4th is Dec. Of course the last day of each
*        of those 4 months is alway 31, 30, 30 and 31
*        It is setup like this to make thinks really quick to work out
*
*----------------------------------------------------------------------
Last_mth_q_tab dc 0h
            dc  cl4'0000'   dummy
            dc  cl4'0331'   Jan last month of quarter is Mar
            dc  cl4'0331'   Feb last month of quarter is Mar
            dc  cl4'0331'   Mar last month of quarter is Mar
            dc  cl4'0630'   Apr last month of quarter is Jun
            dc  cl4'0630'   may last month of quarter is Jun
            dc  cl4'0630'   Jun last month of quarter is Jun
            dc  cl4'0930'   Jul last month of quarter is Sep
            dc  cl4'0930'   Aug last month of quarter is Sep
            dc  cl4'0930'   Sep last month of quarter is Sep
            dc  cl4'1231'   Oct last month of quarter is Dec
            dc  cl4'1231'   Nov last month of quarter is Dec
            dc  cl4'1231'   Dec last month of quarter is Dec


code     loctr          followed by the code Loctr
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCCOLM"  -  LOCATE VIEW COLUMN RECORD (2000) IN VDP               *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - "VDP" VIEW  COLUMN RECORD ADDRESS (NOT FOUND = 0)      *
*        R14 - WORK  REGISTER                                         *
*        R9  - RETURN ADDRESS            (INPUT)                      *
*        R1  - EXIT   PROGRAM  ID        (INPUT)                      *
*        R0  - WORK  REGISTER                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
LOCCOLM  ds 0h
*
       using logictbl,r7
*      r14 loaded with NV pointer prior to call
nvlogic using logictbl,r14          and address via label
       if (ltgf,r15,nvlogic.lt1000a,nz) Load and test vdp 1000 ptr
         USING vdp2000b_COLUMN_RECORD,R15
         drop nvlogic                 drop r14 as a base
outercol do , R15 addresses first VDP record with view #
           do while=(c,r0,eq,vdp2000b_view_id), view # matches record  +
               until=(ltg,r15,vdp2000b_next_ptr,z) loop thru all elem
             doexit (clc,vdp2000b_record_type,eq,vdpcolmc),and,        +
               (c,r1,eq,vdp2000b_record_id),do=outercol
*              exit with r15 set if record type and Id match
           enddo
           xgr r15,r15                         set not found
         enddo
       endif
       br  r9
*
       drop r7
         DROP  R13
         DROP  R15

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCSORT"  -  LOCATE COLUMN SORT RECORD (2300) IN VDP               *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - "VDP" COLUMN  SORT RECORD ADDRESS (NOT FOUND = 0)      *
*        R14 - WORK  REGISTER                                         *
*        R9  - RETURN ADDRESS            (INPUT)                      *
*        R1  - EXIT   PROGRAM  ID        (INPUT)                      *
*        R0  - WORK  REGISTER                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
locsort  dc 0h
       using logictbl,r7
       llgt r14,ltlognv        load the 'NV' pointer
nvlogic using logictbl,r14          and address via label
       if (ltgf,r15,nvlogic.lt1000a,nz) Load and test vdp 1000 ptr
         USING vdp2300b_SORT_KEY_ATTR_RECORD,R15
         drop nvlogic                 drop r14 as a base
outer    do , R15 addresses first VDP record with view #
           do while=(c,r0,eq,vdp2300b_viewid), view # matches record   +
               until=(ltg,r15,vdp2300b_next_ptr,z) loop thru all elem
             doexit (clc,vdp2300b_record_type,eq,vdpsortc),and,        +
               (c,r1,eq,vdp2300b_column_id),do=outer
*              exit with r15 set if column Id matches
           enddo
           xgr r15,r15                      set not found
         enddo
       endif
       br  r9
*
       drop  r7
         DROP  R13
         DROP  R15

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCJOIN"  -  LOCATE PREVIOUS JOIN/LKLR IN LOGIC TABLE              *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - "VDP" VIEW  COLUMN RECORD ADDRESS (NOT FOUND = 0)      *
*        R14 - WORK  REGISTER                                         *
*        R9  - RETURN ADDRESS            (INPUT)                      *
*        R1  - EXIT   PROGRAM  ID        (INPUT)                      *
*        R0  - WORK  REGISTER                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
***********************************************************************
*  LOAD LOGIC TABLE ROW ADDRESS FROM ROW ADDRESS TABLE                *
***********************************************************************
LOCJOIN  lgf   R14,LTCOUNT        CONVERT ROW NUMBER/COUNT (-1) TO ADDR
         sllg  r14,r14,2
         agf   R14,LTROWADR
*
         L     R0,LTCOUNT
*
LOCJOIN4 aghi  R14,-4             BACKUP  ONE ROW
         llgt  R15,0(,R14)
*
         CLC   LTFUNC-LOGICTBL(L'LTFUNC,R15),JOIN
         BRNE  LOCJOIN6
*
         CLC   LTLUFILE-LOGICTBL(L'LTLUFILE+L'LTLULRID,R15),0(R1)
         BRNE  LOCJOIN8
*
         lgf   R0,LTLUPATH-LOGICTBL(,R15)
         BR    R9
*
LOCJOIN6 CLC   LTFUNC-LOGICTBL(L'LTFUNC,R15),LK_LR
         BRNE  LOCJOIN8
*
         CLC   LTFLDDDN-LOGICTBL(L'LTFLDDDN+L'LTFLDLR,R15),0(R1)
         BRNE  LOCJOIN8
*
         lgf   R0,LTFLDPTH-LOGICTBL(,R15)
         BR    R9
*
LOCJOIN8 BRCT  R0,LOCJOIN4
*
         xgr   R15,R15
         BR    R9
*
         DROP  R13

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "ALLOCATE" - ALLOCATES MEMORY DYNAMICALLY FOR NEW LOGICAL RECORD    *
*              LOOK-UP BUFFERS                                        *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R9  - RETURN   ADDRESS                                       *
*        R5  - NEW      LOGICAL RECORD BUFFER ADDRESS  *** RETURNED ***
*        R4  - PREVIOUS LOGICAL RECORD BUFFER ADDRESS  *** INPUT    ***
*        R3  - NEW      LOGICAL RECORD BUFFER LENGTH   *** INPUT    ***
*        R1  - FILE ID & RECORD ID     ADDRESS         *** INPUT    ***
*            - AREA ADDRESS (GETMAIN)                                 *
*        R0  - AREA LENGTH  (GETMAIN)                                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
ALLOLKUP lgr   R5,R1              SAVE FILE & RECORD ID ADDRESS
*
         LA    R0,l'lkupeyeb(,R3) LOAD AREA LENGTH + EYEBALL
         GETMAIN RU,LV=(0),LOC=(ANY)                                   +
                        (note getmain documented to return 64 bit addr
*
         lgr   R0,R5              SAVE FILE & RECORD ID ADDRESS
*
         MVC   0(l'lkupeyeb,R1),LKUPEYEB   COPY EYEBALL
         aghi  R1,l'lkupeyeb
         lgr   R5,R1              INITIALIZE CURRENT BUFFER ADDRESS
         USING LKUPBUFR,R5
prev     using lkupbufr,r4
*
         ST    R5,prev.LBNEXT     ADD TO BUFFER  CHAIN
         STH   R3,LBLEN
*
         XC    LBNEXT,LBNEXT      INITIALIZE FORWARD CHAIN  POINTER
*
         lgr   R1,R0              LOAD FILE & RECORD ID ADDRESS
*
         MVC   LBDDNAME,0(R1)     INITIALIZE FILE ID/DDNAME
         MVC   LBLRID,L'LBDDNAME(R1)         LOGICAL RECORD ID
         MVC   LBPATHID,L'LBDDNAME+L'LBLRID(R1)      PATH   ID
         MVC   LBPARENT,SVLUJOIN  INITIALIZE PARENT  JOIN   ADDRESS
         XC    LBKEYOFF,LBKEYOFF  INITIALIZE KEY     OFFSET
         XC    LBKEYLEN,LBKEYLEN  INITIALIZE KEY     LENGTH
         XC    LBRECLEN,LBRECLEN  INITIALIZE RECORD   LENGTH
         XC    LBRECCNT,LBRECCNT  INITIALIZE RECORD   COUNT
         XC    LBTBLBEG,LBTBLBEG  INITIALIZE RECORD   TABLE BEGIN
         XC    LBTBLEND,LBTBLEND  INITIALIZE RECORD   TABLE END
         XC    LBMIDDLE,LBMIDDLE  INITIALIZE MIDDLE   ENTRY ADDRESS
         MVC   LBLSTRC,HEXFF      INITIALIZE LAST     CALL  RETURN CODE
         MVC   LBLSTFND,HEXFF     INITIALIZE LAST     ENTRY FOUND
         xc    lblstcnt,lblstcnt  INITIALIZE LAST     EVENT RECORD  NO.
         XC    LBEFFOFF,LBEFFOFF  INITIALIZE EFFECTIVE DATE OFFSET
         XC    LBLKSTK#,LBLKSTK#  INITIALIZE LOOK-UP  STACK COUNT
         XC    LBFLAGS,LBFLAGS    INITIALIZE PROCESSING FLAGS
*
         MVC   LBSTRTUP,SPACES    STARTUP    PARAMETERS
*
***********************************************************************
*  ZERO EXIT PARAMETER LIST (POINTERS)                                *
***********************************************************************
         XC    LBENVA,LBENVA      ENVIRONMENT DATA   ADDRESS
         XC    LBSTARTA,LBSTARTA  START-UP    DATA   ADDRESS
         XC    LBRECA,LBRECA      Address of Event record address
         XC    LBEVENTA,LBEVENTA  EVENT       RECORD ADDRESS  - CURRENT
         XC    LBEXTRA,LBEXTRA    EXTRACT     RECORD ADDRESS  - CURRENT
         XC    LBKEYA,LBKEYA      LOOK-UP     KEY    ADDRESS
         XC    LBANCHA,LBANCHA    WORKAREA    ANCHOR POINTER    ADDRESS
         XC    LBRTNCA,LBRTNCA    RETURN      CODE   ADDRESS
         XC    LBRPTRA,LBRPTRA    RETURN      RECORD POINTER    ADDRESS
         XC    LBFNDCA,LBFNDCA    LOOK-UP     FOUND  COUNTER    ADDRESS
         XC    LBNOTCA,LBNOTCA    LOOK-UP NOT FOUND  COUNTER    ADDRESS
         xc    lbfndcnt,lbfndcnt  zero found counter
         xc    lbnotcnt,lbnotcnt  zero not found counter
*
         BR    R9                 RETURN  TO CALLER
*
         DROP  R5,prev
         DROP  R13

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LOCLB" - SEARCHES THE LOOK-UP BUFFER CHAIN FOR AN ENTRY WHICH      *
*           MATCHES  THE "FILE ID/REC ID"  IN THE LOGIC TABLE ENTRY.  *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R9  - RETURN   ADDRESS                                       *
*        R5  - CURRENT  LOOK-UP RECORD BUFFER ADDRESS (RETURNED)      *
*        R4  - PREVIOUS LOOK-UP RECORD BUFFER ADDRESS (RETURNED)      *
*        R3  - CURRENT  LOOK-UP RECORD BUFFER  LENGTH (RETURNED)      *
*        R1  - FILE ID + RECORD ID + PATH ID  (SEARCH  ARGUMENT)      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
LOCLB    xgr   R3,R3              INITIALIZE CURRENT  BUFFER LENGTH
prev     using lkupbufr,r4
         llgt  R5,prev.LBNEXT     LOAD CURRENT  BUFFER ADDRESS
         USING LKUPBUFR,R5
*
         J     LOCEND             CHECK  FOR END-OF-CHAIN

LOCLOOP  LGR   R4,R5              ADVANCE TO NEXT ENTRY
         llgt  R5,LBNEXT
LOCEND   LTGR  R5,R5              END-OF-CHAIN ???
         BZR   R9                 YES - EXIT SUBROUTINE
*
         CLC   LBDDNAME,0(R1)     MATCHING   ENTRY  ???
         JNE   LOCLOOP            NO  - ADVANCE TO NEXT ENTRY ON CHAIN
         CLC   LBLRID,L'LBDDNAME(R1)
         JNE   LOCLOOP
         CLC   LBPATHID,L'LBDDNAME+L'LBLRID(R1)
         JNE   LOCLOOP

         LGH   R3,LBLEN           LOAD  CURRENT BUFFER  LENGTH
*
         BR    R9                 EXIT  SUBROUTINE
*
         DROP  R5,prev
         DROP  R13

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "Chkcons"  - Checks if a symbol/constant has been found previously  *
*              within this RE/ES or RETK/ET set.                      *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R9  - RETURN   ADDRESS                                       *
*        R7  - CURRENT  LOGIC   TABLE  ROW    ADDRESS                 *
*        cons_fnd flag set when a symbol/constant is found in the     *
*                 symbol table                                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING LOGICTBL,R7
         USING THRDAREA,R13
*
Chkcons  stmg  R14,R1,WORKAREA    uses workarea and workar2
         llgt  R14,SYMTBLB        START SEARCH AT BEG OF SYMBOL TABLE
         USING SYMTABLE,R14
*
         LH    R0,LTV1LEN         LOAD  VALUE LENGTH (-1)
         LR    R15,R0
         BCTR  R15,0
*
Chkcsyml cgf   R14,SYMTBLC        END OF CURRENT ENTRIES ???
         JNL   Chkcsymn           YES -  ATTEMPT TO  ADD NEW   SYMBOL
*
         CH    R0,SYMVALEN
         jne   Chkcsyma
         llgt  R1,SYMVALAD
         EX    R15,COMPLOWV
         JE    Chkcsymd
*
Chkcsyma aghi  R14,SYMTBLEN
         J     Chkcsyml
*
Chkcsymd ds    0h
         lmg   R14,R1,WORKAREA    uses workarea and workar2
         oi    initflag,cons_fnd
         BR    R9
*
Chkcsymn LA    R1,SYMTBLEN(,R14)  WILL ANOTHER ENTRY FIT ???
         cgf   R1,SYMTBLM
         JH    Chkcsymx           NO  - CONTINUE WITHOUT ADDING
*
         ST    R1,SYMTBLC
         la    r1,ltvalues
         ST    R1,SYMVALAD        ADD NEW SYMBOL ADDRESS TO TABLE
         STH   R0,SYMVALEN
*
Chkcsymx lmg   R14,R1,WORKAREA    uses workarea and workar2
         DROP  R14
*
         BR    R9                 RETURN
*
         DROP  R7
         DROP  R13

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "COPYVAL1" - COPIES THE LOW VALUE FROM THE LOGIC TABLE TO THE       *
*              LITERAL POOL.                                          *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R15 - LITERAL  LENGTH (-1)                    * * RETURNED * *
*        R9  - RETURN   ADDRESS                                       *
*        R7  - CURRENT  LOGIC   TABLE  ROW    ADDRESS                 *
*        R6  - CURRENT  LITERAL POOL   ADDRESS                        *
*        R5  - LITERAL  POOL BEGINNING ADDRESS                        *
*        R0  - LITERAL  OFFSET                         * * RETURNED * *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING LOGICTBL,R7
         USING THRDAREA,R13
*
COPYVAL1 stmg  R14,R1,WORKAREA    uses workarea and workar2
         llgt  R14,SYMTBLB        START SEARCH AT BEG OF SYMBOL TABLE
         USING SYMTABLE,R14
*
         LH    R0,LTV1LEN         LOAD  VALUE LENGTH (-1)
         LR    R15,R0
         BCTR  R15,0
*
COPYSYML cgf   R14,SYMTBLC        END OF CURRENT ENTRIES ???
         JNL   COPYSYMN           YES -  ATTEMPT TO  ADD NEW   SYMBOL
*
         CH    R0,SYMVALEN
         jne   COPYSYMA
         llgt  R1,SYMVALAD
         EX    R15,COMPLOWV
         JE    COPYSYMD
*
COPYSYMA aghi  R14,SYMTBLEN
         J     COPYSYML
*
COPYSYMD lgr   R0,R1              COMPUTE OFFSET
         sgr   R0,R5
         stg   R0,WORKAREA+8+8    UPDATE  R0
         lmg   R14,R1,WORKAREA    uses workarea and workar
         BR    R9
*
COPYSYMN LA    R1,SYMTBLEN(,R14)  WILL ANOTHER ENTRY FIT ???
         cgf   R1,SYMTBLM
         JH    COPYSYMX           NO  - CONTINUE WITHOUT ADDING
*
         ST    R1,SYMTBLC
         ST    R6,SYMVALAD        ADD NEW SYMBOL ADDRESS TO TABLE
         STH   R0,SYMVALEN
*
COPYSYMX lmg   R14,R1,WORKAREA    uses workarea and workar2
         DROP  R14
*
         lgh   R15,LTV1LEN        LOAD  VALUE LENGTH  (-1)
         lr    r0,r15
         a     r0,piesave
         st    r0,piesave
         lr    r0,r15             load  value length  (-1)
         clc   ltfunc,cf_ec
         jne   copyv_lks
         a     r0,piesave+4
         st    r0,piesave+4
         j     copyv_last
copyv_lks ds   0h
         clc   ltfunc,lk_s
         jne   copyv_lkc
         a     r0,piesave+8
         st    r0,piesave+8
         j     copyv_last
copyv_lkc ds   0h
         clc   ltfunc,dt_c
         jne   copyv_last
         a     r0,piesave+12
         st    r0,piesave+12
copyv_last ds  0h
         BCTR  R15,0
         LA    R0,1(R6,R15)       COMPUTE NEW LITERAL POOL ENDING
         cgf   R0,LITPOOLM        CHECK FOR LITERAL POOL OVERFLOW ???
         JNH   COPYLO1            BRANCH IF OKAY
*
         lghi  R14,LITP_OVERFLOW  INDICATE OVERFLOW
         llgt  R13,THRDMAIN       USE MAIN DRIVER'S THREAD
         J     STDERROR
*
COPYLO1  lgr   R0,R6              COMPUTE LITERAL OFFSET
         sgr   R0,R5
         EX    R15,COPYLOWV       COPY LOW  VALUE
         LA    R6,1(R6,R15)       ADVANCE CURRENT LITERAL POOL POS
         BR    R9                 RETURN
*
         DROP  R7
         DROP  R13

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *@05I
*                                                                  @05I
* "COPYFLD'  - Copies a fields contents from a logic table entry   @05I
*              to the Literal Pool.                                @05I
*                                                                  @05I
* REGISTER USAGE:                                                  @05I
*                                                                  @05I
*        R14 - Field address                                       @05I
*        R15 - Field length                                        @05I
*        R9  - Return address                                      @05I
*        R7  - Current logic table row address                     @05I
*        R6  - Current literal pool address                        @05I
*        R5  - Literal pool beginning address                      @05I
*        R0  - Literal offset                      * * RETURNED * *@05I
*                                                                  @05I
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *@05I
         USING LOGICTBL,R7                                         @05I
         USING THRDAREA,R13                                        @05I
*                                                                  @05I
COPYFLD  BCTR  R15,0                                               @05I
         LA    R0,1(R6,R15)       COMPUTE NEW LITERAL POOL ENDING  @05I
         cgf   R0,LITPOOLM        CHK FOR LITERAL POOL OVERFLOW ???@05I
         JNH   COPYFLD1           BRANCH IF OKAY                   @05I
*                                                                  @05I
         lghi  R14,LITP_OVERFLOW  INDICATE OVERFLOW                @05I
         llgt  R13,THRDMAIN       USE MAIN DRIVER'S THREAD         @05I
         J     STDERROR                                            @05I
*                                                                  @05I
COPYFLD1 lgr   R0,R6              COMPUTE  LITERAL  OFFSET         @05I
         sgr   R0,R5                                               @05I
         EXrl  R15,COPYFLDV       COPY ACCUMULATOR  VALUE          @05I
         LA    R6,1(R6,R15)       ADVANCE CURRENT LITERAL POOL POS @05I
         BR    R9                 RETURN                           @05I
*                                                                  @05I
COPYFLDV MVC   0(0,R6),0(R14)               * * * E X E C U T E D * * *
         DROP  R7                                                  @05I
         DROP  R13
*                                                                  @05I
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "COPYACC'  - COPIES THE ACCUMULATOR VALUE FROM THE LOGIC TABLE      *
*              TO THE LITERAL POOL.                                   *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R15 - LITERAL  LENGTH (-1)                    * * RETURNED * *
*        R9  - RETURN   ADDRESS                                       *
*        R7  - CURRENT  LOGIC   TABLE  ROW    ADDRESS                 *
*        R6  - CURRENT  LITERAL POOL   ADDRESS                        *
*        R5  - LITERAL  POOL BEGINNING ADDRESS                        *
*        R0  - LITERAL  OFFSET                         * * RETURNED * *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING LOGICTBL,R7
         USING THRDAREA,R13
*
COPYACC  lgh   R15,LTVVLEN        LOAD  VALUE LENGTH
         BCTR  R15,0
         LA    R0,1(R6,R15)       COMPUTE NEW LITERAL POOL ENDING
         cgf   R0,LITPOOLM        CHECK FOR LITERAL POOL OVERFLOW ???
         JNH   COPYACC1           BRANCH IF OKAY
*
         lghi  R14,LITP_OVERFLOW  INDICATE OVERFLOW
         llgt  R13,THRDMAIN       USE MAIN DRIVER'S THREAD
         J     STDERROR
*
COPYACC1 lgr   R0,R6              COMPUTE  LITERAL  OFFSET
         sgr   R0,R5
         EXrl  R15,COPYACCV       COPY ACCUMULATOR  VALUE
         LA    R6,1(R6,R15)       ADVANCE CURRENT LITERAL POOL POS
         BR    R9                 RETURN
*
COPYACCV MVC   0(0,R6),LTVVVAL              * * * E X E C U T E D * * *
         DROP  R7
         DROP  R13

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "SAVECOL#" - SAVES THE CURRENT COLUMN NUMBER IN THE LITERAL POOL    *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R9  - RETURN   ADDRESS                                       *
*        R7  - CURRENT  LOGIC   TABLE  ROW    ADDRESS                 *
*        R6  - CURRENT  LITERAL POOL   POSITION                       *
*        R5  - LITERAL  POOL BEGINNING ADDRESS                        *
*        R0  - LITERAL  OFFSET                         * * RETURNED * *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING LOGICTBL,R7
*
SAVECOL# LH    R0,LTCOLNO         SAVE COLUMN  NUMBER IN LITERAL POOL
*
SAVEHALF LA    R6,1(,R6)          ALIGN   ON HALFWORD BOUNDARY
         nill  R6,x'fffe'
*
         STH   R0,0(,R6)          STORE   HALFWORD
*
         lgr   R0,R6              COMPUTE OFFSET FROM CODE BASE
         sgr   R0,R5
         aghi  R6,2               ADVANCE LITERAL POOL POSITION
*
         BR    R9                 RETURN
*
         DROP  R7

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "SAVEADDR" - SAVES THE ADDRESS IN  R0      IN THE LITERAL POOL      *
*                                                                     *
* "SAVEROWA" - SAVES THE CURRENT LT ROW ADDR IN THE LITERAL POOL      *
*                                                                     *
* "SAVELBA"  - SAVES THE CURRENT LOOKUP BUFFER ADDRESS IN THE         *
*              LITERAL POOL                                           *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R9  - RETURN   ADDRESS                                       *
*        R7  - CURRENT  LOGIC   TABLE  ROW    ADDRESS                 *
*        R6  - CURRENT  LITERAL POOL   POSITION                       *
*        R5  - LITERAL  POOL BEGINNING ADDRESS                        *
*        R0  - LITERAL  OFFSET                         * * RETURNED * *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         USING LOGICTBL,R7
*
SAVEROWA lgr   R0,R7              SAVE LOGIC TABLE ROW ADDRESS
         J     SAVEADDR
*
SAVELBA  llgt  R0,LTLBADDR        SAVE LOOK-UP  BUFFER ADDR IN LIT POOL
*
SAVEADDR LA    R6,3(,R6)          ALIGN  ON   FULLWORD BOUNDARY
         nill  R6,x'fffc'
*
         stmg  R14,R15,workarea
         llgt  R14,SYMTBLB        START SEARCH AT BEG OF SYMBOL TABLE
         USING SYMTABLE,R14
*
SAVESYML cgf   R14,SYMTBLC        END OF CURRENT ENTRIES ???
         JNL   SAVESYMN           YES -  ATTEMPT TO  ADD NEW   SYMBOL
*
         LHI   R15,4
         CH    R15,SYMVALEN
         JNE   SAVESYMA
*
         llgt  R15,SYMVALAD
         cgf   R0,0(,R15)
         JE    SAVESYMD
*
SAVESYMA aghi  R14,SYMTBLEN
         J     SAVESYML
*
SAVESYMD lgr   R0,R15             COMPUTE OFFSET
         sgr   R0,R5
         lmg   R14,R15,workarea
         BR    R9
*
SAVESYMN LA    R15,SYMTBLEN(,R14) WILL ANOTHER ENTRY FIT ???
         cgf   R15,SYMTBLM
         JH    SAVESYMX           NO  - CONTINUE WITHOUT ADDING
*
         ST    R15,SYMTBLC
         ST    R6,SYMVALAD        ADD NEW SYMBOL ADDRESS TO TABLE
         LHI   R15,4
         STH   R15,SYMVALEN
*
SAVESYMX lmg   R14,R15,workarea   drop into the SAVEFULL code below
*                                 .. to add entry
         DROP  R14
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "SAVEFULL" - SAVES A FULLWORD IN THE LITERAL POOL                   *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
SAVEFULL LA    R6,3(,R6)          ALIGN   ON FULLWORD BOUNDARY
         nill  R6,x'fffc'
*
         ST    R0,0(,R6)          STORE   VALUE IN  LITERAL POOL
*
         lgr   R0,R6              COMPUTE OFFSET  FROM POOL BASE
         sgr   R0,R5
         aghi  R6,4               ADVANCE LITERAL POOL POSITION
*
         BR    R9                 RETURN
*
         DROP  R7

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "SAVELVL#" - SAVES THE CURRENT LEVEL NUMBER ADDRESS IN THE          *
*              LITERAL POOL                                           *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R9  - RETURN   ADDRESS                                       *
*        R7  - CURRENT  LOGIC   TABLE  ROW    ADDRESS                 *
*        R6  - CURRENT  LITERAL POOL   POSITION                       *
*        R5  - LITERAL  POOL BEGINNING ADDRESS                        *
*        R1  - LITERAL  OFFSET                         * * RETURNED * *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING LOGICTBL,R7
*
SAVELVL# LA    R1,LTSTRLVL        LOAD LEVEL NUMBER ADDRESS
         ST    R1,0(,R6)          SAVE LEVEL NUMBER ADDR IN LIT POOL
*
         lgr   R1,R6              COMPUTE OFFSET FROM CODE BASE
         sgr   R1,R5
         aghi  R6,04              ADVANCE LITERAL POOL POSITION
*
         BR    R9                 RETURN
*
         DROP  R7

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "LITPSIZE" - UPDATES THE CURRENT LITERAL POOL LENGTH IN THE         *
*              PROLOGUE FOR THE CURRENT VIEW BEFORE INITIATING        *
*              THE NEXT VIEW'S LITERAL POOL.                          *
* "LITPINIT" - PROVIDES INITIAL VALUES FOR "LITP_HDR"                 *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R6  - CURRENT  LITERAL  POOL       ADDRESS                   *
*        R5  - LITERAL  POOL     STARTING   ADDRESS                   *
*        R2  - CURRENT  PROLOGUE ADDRESS                              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING NVPROLOG,R2
         USING LITP_HDR,R5
*
LITPSIZE LA    R6,7(,R6)          COMPUTE LENGTH OF CURRENT  LIT POOL
         nill  R6,x'fff8'         ALIGN   ON DOUBLEWORD BOUNDARY
*
         lgr   R0,R6
         sgr   R0,R5
         ST    R0,NVLITPSZ        SAVE   LIT POOL LENGTH IN PROLOGUE
*
         agf   R0,ESPOOLSZ        UPDATE LIT POOL SIZE  FOR THIS "ES"
         ST    R0,ESPOOLSZ
*
         lgr   R5,R6              ADVANCE TO NEXT LITERAL POOL HEADER
*
litpinit XC    LPVADDR,LPVADDR    clear fields
         xc    lplurtnc,lplurtnc
         ZAP   LPEXTCNT,P000      VIEW EXTRACT  RECORDS   WRITTEN COUNT
         ZAP   LPLKPFND,P000      VIEW LOOKUPS  FOUND     COUNT
         ZAP   LPLKPNOT,P000      VIEW LOOKUPS  NOT FOUND COUNT
*
         LA    R6,LITPHDRL(,R5)
*                                                                 pgc99
         lgf   r15,tokncnt        Get number of tokens            pgc99
         aghi  r15,1              Allow for count field           pgc99
         mghi  r15,8               time 8 (2 words for each      pgc101
*                                  first word is used for the    pgc101
*                                  offset in the thread lit pool pgc101
*                                  of each tokens lit pool. The  pgc101
*                                  2nd word is used to hold the  pgc101
*                                  look up bufer address that    pgc101
*                                  will contain the record to be pgc101
*                                  processed by the token        pgc101
         agr   r6,r15             point oast area in lit pool     pgc99
*
         BR    R9                 RETURN
*
         DROP  R2
         DROP  R5

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "INCKYPOS" - SAVES THE POSITION FOR THIS LOOK-UP KEY FIELD AND      *
*              THEN ADVANCES THE CURRENT POSITION FOR THE NEXT FIELD  *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R9  - RETURN   ADDRESS                                       *
*        R7  - CURRENT  LOGIC   TABLE  ROW    ADDRESS                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING LOGICTBL,R7
*
INCKYPOS LH    R15,KEYPOSN        LOAD CURRENT  LOOK-UP  KEY POSITION
         STH   R15,LTCOLPOS       SAVE POSITION FOR THIS FIELD
         AH    R15,LTCOLLEN
         STH   R15,KEYPOSN        SAVE NEXT     KEY POSITION
*
         BR    R14                RETURN
*
         DROP  R7
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "JUSTIFY" - CALCULATES THE OFFSET OF THIS COLUMN WITHIN THE DATA    *
*             AREA AS IF LEFT JUSTIFIED AND THEN MODIFIES THE OFFSET  *
*             IF THE DATA IS CENTERED OR RIGHT JUSTIFIED.             *
*                                                                     *
*             IF THE COLUMN HAS MULTIPLE OCCURRENCES THIS LOGIC TABLE *
*             ROW IS ADDED TO THE MULTIPLE OCCURRENCE CHAIN.          *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R7  - CURRENT  LOGIC   TABLE  ROW    ADDRESS                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,savesubr
         USING LOGICTBL,R7
*
JUSTIFY  stmg  R14,R12,SAVF4SAG64RS14    SAVE PASS1  REGISTERS
*
         LH    R0,LTCOLPOS        LOAD COLUMN DATA   OFFSET
         STH   R0,LTJUSOFF        ASSUME LEFT JUSTIFICATION
*
         if CLI,LTCOLJUS,NE,C'L'  Left JUSTIFICATION ???

           lh  r1,ltcollen        Use whole column unless mask present
           AHI R1,1               ADD     ONE  (TRUE LENGTH)
*
           lh  r0,ltcolfmt        pick up column format
           if  chi,r0,eq,fc_alnum alpha numeric data?
             LH R1,LTFLDLEN       LOAD    FIELD LENGTH  (-1)
             AHI R1,1             ADD     ONE  (TRUE LENGTH)
           endif
*
           if OC,LTMSKLEN,LTMSKLEN,nz   EDIT MASK PRESENT  ???
*
             LA R14,LTCOLMSK           POINT TO LAST BYTE IN MASK
             lgh r0,LTMSKLEN
             agr r14,r0
             BCTR R14,0
             do from=(r14),while=(cli,0(r14),eq,c' ')
             enddo
             LA R0,LTCOLMSK            LOAD MASK BEGINNING ADDRESS
             sgr R14,R0                 COMPUTE MASK LENGTH (-1)
             LA R1,1(,R14)             LOAD ACTUAL MASK LENGTH
           endif

           LH  R0,LTCOLLEN        USE WHOLE COLUMN UNLESS MASK PRESENT
           AHI R0,1
           if (SR,R0,R1,p)        COMPUTE EXCESS  COLUMN WIDTH
*
             if CLI,LTCOLJUS,ne,C'R'   Not RIGHT JUSTIFICATION ???
               SRL R0,1           DIVIDE BY TWO (CENTER DATA)
             endif
*
             AH R0,LTJUSOFF       ADD  STARTING OFFSET BEFORE ADJUSTMNT
             STH R0,LTJUSOFF      UPDATE JUSTIFIED OFFSET
           endif
*
         endif
         lmg   R14,R12,SAVF4SAG64RS14    LOAD PASS1  REGISTERS
         BR    R9                 RETURN
*
         DROP  R7
         DROP  R13

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "PIPELIST" - BUILDS A LINKED LIST OF "PIPES" WRITTEN TO             *
*              WITHIN AN "ES" SET                                     *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R13 - THREAD WORK    AREA     ADDRESS                        *
*        R9  - RETURN ADDRESS                                         *
*        R7  - LOGIC  TABLE   "WR" ROW ADDRESS                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,thrdarea
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
         USING LOGICTBL,R7
*
PIPELIST stmg  R14,R3,SAVF4SAG64RS14  Save registers
*
         llgt  R3,LTWREXTA        LOAD  EXTRACT ELEMENT ADDR  FOR "WR"
         USING EXTFILE,R3
*
         LA    R15,SAVEPIPE
*
PIPELOOP lgr   R14,R15            SAVE PREVIOUS LIST ELEMENT ADDRESS
         ltgf  R15,0(,R15)        LOAD NEXT     LIST ELEMENT ADDRESS
         JNP   PIPENEW            END-OF-LIST - ADD  NEW  ELEMENT
*
         cgf   R3,4(,R15)         PIPE ALREADY  ON   LIST ??
         JE    PIPEEXIT           YES - EXIT
*
         J     PIPELOOP           SCAN  LIST
*
PIPENEW  LHI   R0,8+8             GET   NEW     LIST ELEMENT
         GETMAIN RU,LV=(0),LOC=(ANY)
*
         MVC   0(8,R1),PIPEEYEB   INITIALIZE EYEBALL
         LH    R0,gpthrdno
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  5(3,R1),DBLWORK
*
         aghi  R1,8
         ST    R1,0(,R14)         CHAIN TO      LAST ELEMENT
         XC    0(4,R1),0(R1)
         ST    R3,4(,R1)
*
pipeadd  ds    0h
         LH    R15,EXTPIPEP       INCREMENT   PARENT THREAD  COUNT
         AHI   R15,1
         STH   R15,EXTPIPEP
*
PIPEEXIT lmg   R14,R3,SAVF4SAG64RS14  Restore registers
         BR    R9                 RETURN TO POINT OF CALL
*
         DROP  R3
         DROP  R7
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        B U I L D   E V E N T   T H R E A D   W O R K   A R E A S    *
*                                                                     *
*                                                                     *
*    1   ALLOCATE THREAD  WORK  AREAS  FOR   EACH  EVENT FILE SET     *
*    2.  ALLOCATE EVENT   FILE  DCB                                   *
*    3.  ALLOCATE EXTRACT FILE  RECORD BUFFER                         *
*    4.  ALLOCATE THREAD  COMPLETION  "ECB"  LIST                     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
THRDBLD  llgt  R13,THRDMAIN       LOAD  MAIN  PROGRAM'S WORK  AREA ADDR
*
         llgt  R7,EXECDADR        LOAD  PARAMETER  AREA ADDRESS
         USING EXECDATA,R7
         xr    r14,r14
*
         PACK  DBLWORK,EXECDISK   CONVERT MAX DISK THREADS TO BINARY
         CVB   R0,DBLWORK
         L     R6,DISKCNT         MAXIMUM EXCEEDED ???
         ar    r14,r6
         if CR,R6,gt,R0
           LR  R6,R0              YES - LIMIT THREADS
         endif
         ST    R6,DISKCNT         SAVE  FINAL DISK COUNT
*
         PACK  DBLWORK,EXECTAPE   CONVERT MAX TAPE THREADS TO BINARY
         CVB   R0,DBLWORK
         L     R15,TAPECNT        MAXIMUM EXCEEDED ???
         ar    r14,r15
         if CR,R15,gt,R0
           LR  R15,R0             YES - LIMIT  THREADS
         endif
         ST    R15,TAPECNT        SAVE  FINAL  TAPE     COUNT
*
         AR    R6,R15             ADD   TAPE   THREAD   COUNT
         A     R6,OTHRCNT         ADD   OTHER  THREAD   COUNT
         a     r14,othrcnt
         st    r14,litpcnt        this is how many literal pools needed
*
         STH   R6,THRDCNT         SAVE  TOTAL  NO.  OF  THREADS
*                                                                pgc300
         if lt,r5,globvnam,p                                     pgc300
           jas   r9,THRDVBLD        Create and init thread vars. pgc300
*                                 area for each thread           pgc300
         endif                                                   pgc300
                        SPACE 3
         if LT,R5,DISKCNT,P       BUILD NUMBER OF DISK  THREADS
*
           do from=(r5)
             LHI R4,DISKDEV
             BRAS R9,THRDCOPY
             BCTR R6,0
           enddo
         endif
                        SPACE 3
         if LT,R5,tapeCNT,P       BUILD NUMBER OF tape  THREADS
*
           do from=(r5)
             LHI R4,tapeDEV
             BRAS R9,THRDCOPY
             BCTR R6,0
           enddo
         endif
                        SPACE 3
         if LT,R5,othrCNT,P       BUILD NUMBER OF tape  THREADS
*
           do from=(r6)
             LHI R4,pipeDEV
             BRAS R9,THRDCOPY
           enddo
         endif
*
         llgt  R13,THRDMAIN
*
         BR    R10                RETURN
*
         DROP  R7
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        Thread Variable Area build                                   *
*          For each thread we build an area for all the thread level  *
*          variables used by the logic table. The variables are also  *
*          initialized depending on their type.                       *
*                                                                     *
*        R6:  Thread count                                            *
*        R9:  Return address                                          *
*       R14:  Points to global variable list                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
THRDVBLD ds    0h
*
*        Get total length of thread level variables
*
         using logictbl,r14
         xgr   r1,r1              Zero variable length counter
         llgt  r14,globvnam       Get start of thread variables
         do until=(lt,r14,ltvnnext,np) loop until end of variables
           st  r1,ltvnlitp        This is the offset in area for var.
           ah  r1,ltvnlen         add the variable length to total
         enddo
         sty   r1,thread_vars_sz  Save size of thread variables
*
*        Get area size needed for all threads
*
         lr    r0,r1
         storage OBTAIN,LENGTH=(0),CHECKZERO=YES
         sty   r1,thread_vars     Save area address
*
*        Now need to initialize Global variables that are in
*        in Packed format
*
         llgt  r14,globvnam       Get start of thread variables
         lhi   r0,FC_PACK         We want this type of field
         llgt  r1,thread_vars     get start of gloabl variables
         do until=(lt,r14,ltvnnext,np) loop until end of variables
           lh r15,ltvnlen         Get length of variable
           bctr r15,0
           if CH,r0,eq,ltvnfmt
             sll r15,4            want it in high nibble
             exrl r15,initgpk        initialize variable
           else
             exrl r15,initglb       initialize variable
           endif
           ah  r1,ltvnlen         add the variable length to total
         enddo
         br    r9                 Return to point of call
initgpk      zap 0(0,r1),p000       Init to packed zero
initglb      xc  0(0,r1),0(r1)      Init to zero
*
         drop  r14
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        T H R E A D   W O R K A R E A   C O P Y                      *
*                                                                     *
*                                                                     *
*        R4:  THREAD TYPE                                             *
*        R6:  THREAD COPY LOOP COUNTER                                *
*        R9:  RETURN ADDRESS                                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
*
THRDCOPY LHI   R0,THRDLEN+l'thrdeyeb    LOAD LENGTH OF THREAD WORKAREA
         ay    r0,thread_vars_sz  Allow for thread level vars    pgc300
         GETMAIN R,LV=(0)         OBTAIN WORKAREA
*
         MVC   0(l'thrdeyeb,R1),THRDEYEB   INITIALIZE  EYEBALL
         LH    R0,THRDCNT         SAVE THE  THREAD NUMBER IN EYEBALL
         SR    R0,R6
         AHI   R0,1
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  5(3,R1),DBLWORK
*
         LA    R2,l'thrdeyeb(,R1) SAVE   POINTER TO NEW WORKAREA
*
         if cgf,R13,eq,THRDMAIN     FIRST  SUBTASK THREAD ???
           st  R2,THRDFRST        SAVE   POINTER TO FIRST  THREAD AREA
         endif
         lgr   R0,R2              LOAD  "TO"   ADDRESS
         LHI   R1,THRDLEN         LOAD  "TO"   LENGTH
         LR    R15,R1             LOAD  "FROM" LENGTH
         lgr   R14,R13            LOAD  "FROM" ADDRESS
         MVCL  R0,R14             COPY   THE   THREAD    WORKAREA
*
         ST    R2,THRDNEXT        SAVE   POINTER TO NEXT THREAD   AREA
*
         if cgf,R13,ne,THRDMAIN     Not the FIRST  SUBTASK THREAD ???
           st  R13,THRDPREV-THRDAREA(,R2) POINT  TO PREV THREAD   AREA
         endif
                        SPACE 3
         lgr   R13,R2             SWITCH  TO NEW WORK    AREA
*
         llgt  r14,thread_vars    Get prev thread variable area  pgc300
         ly    r15,thread_vars_sz Get its size                   pgc300
         lgr   r0,r13             T variable area is at the end  pgc300
         ahi   r0,thrdlen          of the thread work area       pgc300
         lr    r1,r15             length                         pgc300
         mvcl  r0,r14             copy init values               pgc300
*
         XC    THRDNEXT,THRDNEXT  INITIALIZE FORWARD   POINTER
         XC    LBCHAIN,LBCHAIN    INITIALIZE LOOK-UP   BUFFER POINTER
*
         LA    R14,TP90AREA       INITIALIZE "GVBTP90" PARAMETER LIST
         ST    R14,TP90PA
         LA    R14,LKUPKEY
         ST    R14,TP90KEYA
*
         LA    R14,DL96AREA       INITIALIZE "GVBDL96" PARAMETER LIST
         ST    R14,DL96PA
         LA    R14,DL96LEN
         ST    R14,DL96LENA
         LA    R14,DL96RTNC
         ST    R14,DL96RTNA
*
         LA    R14,env_area       ENVIRONMENT DATA   ADDRESS
         sty   R14,GPENVA
         lay   R14,error_buffer   error buffer address for exit wtos
         ST    R14,gp_error_buffer_ptr
         LA    R14,GPDDNAME       FILE INFORMATION   ADDRESS
         sty   R14,GPFILEA
         LA    R14,LKUPKEY        LOOK-UP     KEY    ADDRESS
         sty   R14,GPKEYA
         LA    R14,RETNCODE       RETURN      CODE   ADDRESS
         sty   R14,GPRTNCA
         LA    R14,RETNPTR        RETURN      RECORD POINTER   ADDRESS
         sty   R14,GPBLOCKA
         oiy   GPBLOCKA,X'80'     MARK END-OF-PARAMETER-LIST
*
         LH    R0,THRDCNT         SAVE THE  THREAD NUMBER IN WORK AREA
         SR    R0,R6
         AHI   R0,1
         STH   R0,gpthrdno
*
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
*
         ST    R13,GP_THRD_WA     Save thread WA address
*
         STH   R4,THRDTYP         INIT THREAD TYPE (DISK,TAPE,OTHR)
*
***********************************************************************
*  ALLOCATE EVENT FILE DCB                                            *
***********************************************************************
THRDDCB  ds    0h
         LA   R0,targetbs_l+8    LOAD area length - dcb, etc
         GETMAIN R,LV=(0),LOC=BELOW GET MEMORY below the line
         MVC   0(8,R1),EDCBEYEB   COPY EYEBALL
         UNPK  5(3,R1),DBLWORK
         La   R2,8(,r1)
         using targetbs,r2
target   USING IHADCB,dcbbs

         larl r15,evntfile
source         USING IHADCB,r15
         MVC  target.ihadcb(evntdcbl),source.ihadcb   copy dcb/dcbe

         LA   R0,dcbbs+evntdcbe-evntfile
         ST   R0,target.DCBDCBE
*
         la   r0,exlbs          get address of target exl
         mvi  exlbs,exllaste+exldcbex    set flag
         stcm r0,b'0111',target.dcbexlsa and update the dcblist ptr
         drop target,source

         la   r0,openbs         get address of code area
         mvc  openbs,modopen24  copy in model code
         stcm r0,b'0111',exlbsopen   and set address in list

         larl r0,event_open_exit real exit address
         oilh r0,x'8000'        turn on amode 31
         st   r0,exitbs31        and save

         la   r2,dcbbs          point r2 at dcb
         ST   r2,EVNTDCBA        SAVE DCB   ADDRESS
         drop r2

*
***********************************************************************
*  ALLOCATE EXTRACT RECORD WORK AREA                                  *
***********************************************************************
         LHI   R0,EXTRECL+l'extreyeb  LOAD MAXIMUM EXTRACT RECORD LEN
         GETMAIN RU,LV=(0),LOC=(ANY),BNDRY=PAGE OBTAIN MEMORY
*
         MVC   0(l'extreyeb,R1),EXTREYEB   COPY EYEBALL
         UNPK  5(3,R1),DBLWORK
         AHI   R1,l'extreyeb
         sty   R1,GPEXTRA         SAVE EXTRACT  RECORD ADDRESS
*
***********************************************************************
*  ALLOCATE LOOK-UP STACK                                             *
***********************************************************************
         LHI   R0,STACKSIZ        LOAD NO. OF STACK ENTRIES
         mhi   R0,12              TIMES 12 (= LRID + 8 byte address)
         aghi  R0,l'stakeyeb      + EYEBALL
         GETMAIN RU,LV=(0),LOC=(ANY)   OBTAIN MEMORY
*
         MVC   0(l'stakeyeb,R1),STAKEYEB   COPY EYEBALL
         UNPK  5(3,R1),DBLWORK
         aghi  R1,l'stakeyeb
         ST    R1,gpjstka         SAVE STACK ADDRESS
                        SPACE 3
         BR    R9                 RETURN  TO POINT OF  CALL
*
         DROP  R13
                        EJECT

         using saver,r13
***********************************************************************
*        "G E N I N F O"  -  GENEVA INFORMATION                       *
***********************************************************************
GENINFO  bsm   0,r14              Just return
                        SPACE 3
***********************************************************************
*        "G E N W I N F O"  -  WRITE EXTRACT RECORD INFORMATION       *
***********************************************************************
         push  using
GENWINFO STM   R14,R12,savgrs14   SAVE REGISTERS
*
         L     R15,0(,R1)         LOAD WORK AREA POINTER ADDRESS
         LA    R15,0(,R15)        clean the address
         LHI   R0,LTWRWORK-LTWRAREA
         SR    R15,R0             COMPUTE  "LTWRAREA"    ADDRESS
         using ltwrarea,r15  and map it
         L     R15,LTWRROWA       LOAD "LTWR"  ROW    ADDRESS
         using logictbl,r15        and map the logic table
         L     R15,LTWREXTA       LOAD "EXTFILE"  ADDR
         using extfile,r15        and map the extfile area
         L     R1,4(,R1)
         MVC   0(l'extddnam,R1),EXTDDNAM   DDNAME
*
         LM    R14,R12,savgrs14   RESTORE REGISTERS
         bsm   0,r14              RETURN
         pop   using
                        SPACE 3
***********************************************************************
*        "G E N E N V V"  -  RETURN ENVIRONMENT VARIABLE VALUE        *
***********************************************************************
*
GENENVV  STM   R14,R12,savgrs14   SAVE REGISTERS
*
         larl  r12,GVBMR96        set up the base register again
         L     R3,0(,R1)          LOAD ENVIRONMENT VARIABLE LIST ADDR
         L     R3,0(,R3)
         USING ENVVTBL,R3
*
         L     R4,4(,R1)          LOAD VARIABLE NAME   ADDR
*
GENENVVL LTR   R3,R3              END-OF-LIST   ???
         JNP   GENENVVN           YES -  NOT    FOUND
*
         LH    R5,ENVVNLEN        NAME   LENGTH (-1)
         EXrl  R5,GENENVVC
         JE    GENENVVF
*
         L     R3,ENVVNEXT        LOOP   THROUGH LIST
         J     GENENVVL
*
GENENVVN LHI   R0,4
         J     GENENVVX
*
GENENVVF LH    R0,ENVVVLEN        LOAD VARIABLE VALUE  LEN  (-1)
         AHI   R0,1
         L     R14,8(,R1)         LOAD VARIABLE LEN    ADDR
         ST    R0,0(,R14)
*
         LA    R0,ENVVVALU        LOAD VARIABLE VALUE  ADDR - SOURCE
         L     R14,12(,R1)        LOAD VARIABLE VALUE  POINTER  ADDR
         ST    R0,0(,R14)
*
         XR    R0,R0
*
GENENVVX L     R14,16(,R1)        RETURN CODE ADDRESS
         ST    R0,0(,R14)
*
         LM    R14,R12,savgrs14   RESTORE REGISTERS
         bsm   0,r14              RETURN
*
GENENVVC CLC   0(0,R4),ENVVNAME   * * * * E X E C U T E D * * * *
*
         DROP  R3,r13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        "DUMMY" DSNHLI ENTRY POINT FOR "EXEC SQL" GENERATED CODE     *
*                                                                     *
*        code assumes that R13 is pointing at the thread workarea     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING THRDAREA,R13
*
dsnhli   llgf  R15,DSNHLI2     GEt the saved entry point from workarea
         BR    R15                Off we go
*
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
*
static   loctr ,
         DS   0D
MEGROUND DC    FD'1048575'
invalid_offset ds 0f
F1MEG    DC    F'1048576'
MBminus1 DC    F'1048575'
f512k    DC    F'524288'            512 k
MAXLIMIT DC    F'99999999'
*
MODE31   DS   0XL4
OPENPARM DC    XL8'8000000000000000'
*
ALLFUNC  DC    XL4'FFF0FFF0'      "ALL" FUNCTION
*
equals   dc    cl80'===================================================x
               ============================='
*
VDPCTLID DC    AL2(50)
VDPVIEWC DC    AL2(VDPVIEW)
VDPCOLMC DC    AL2(VDPCOLM)
VDPSORTC DC    AL2(VDPSORT)
*
P000     DC    P'0'
P001     DC    P'1'
P003     DC    P'3'
P008     DC    P'8'
h0       dc    h'0'
h1       dc    h'1'
h10      dc    h'10'
h50      dc    h'50'
*
VDP      dc    cl3'VDP'
vwofmt_ilr  dc    cl20'Input Logical Record'
vwofmt_flf  dc    cl20'Fixed-Length Fields'
vwofmt_df   dc    cl20'Delimited Fields'
vwofmt_hr   dc    cl20'Hardcopy Report'
vwofmt_na   dc    cl20'<Not available>'
*
open_errid ds  c
ZEROES   DC    CL8'00000000'
ZEROES13 DC    CL13'0000000000000'
NINES    DC    PL6'99999999999'
bnines   DC    fd'9999999999999'
bzero    DC    fd'0'
ONE      DC    cl3'001'
C254     DC    cl3'254'
*
TOKNLVL2 DC    A(2)               NAME/TOKEN AVAILABILITY LEVEL (STEP)
TOKNPERS DC    F'0'               TOKEN  PERSISTENCE
GENEVA   DC    CL8'GENEVA'        TOKEN  NAME - MAJOR
GLOBAL   DC    CL8'GLOBAL'        TOKEN  NAME - MINOR
*
GVBDL96A DC    V(GVBDL96)         FIELD   VALUE  FORMATTER
GVBTP90A DC    V(GVBTP90)         VSAM    I/O    SUBROUTINE
GVBUR20A DC    V(GVBUR20)         SEQ     I/O    SUBROUTINE
*
         WXTRN GVBMRDI
MRDIADDR DC    V(GVBMRDI)
*
majfCNTA DC    V(GVBmaj_c)        ADDRESS OF major func table count
majfaDDR DC    V(GVBmaj_t)        ADDRESS OF major function table
FTBLCNTA DC    V(GVBFUNCT)        ADDRESS OF FUNCTION  TABLE    COUNT
FTBLADDR DC    V(GVBFUNTB)        ADDRESS OF FUNCTION  TABLE
TRACADDR DC    V(MR95TRAC)        ADDRESS OF BRANCH    TO TRACE ROUTINE
tracbas  bas   r10,*              dummy instruction
WRTXADDR DC    V(WRTXLEIN)        WRITE   EXIT COMMON  LANG   INTERFACE
LKPXADDR DC    V(LKPXLEIN)        LOOK-UP EXIT COMMON  LANG   INTERFACE
MR95NVA  DC    V(MR95NV)          "NV"    MODEL  CODE  TEMPLATE
IEAN4CR  DC    V(IEAN4CR)         64 bit token create routine
SRCHADDV DC    V(SRCHADDT)        SEARCH ROUTINE ADDRESS TABLE
HASHADDV DC    V(AddHASHV)        Address of HASH Vcon in MR95
*
***********************************************************************
*   PARAMETERS FOR CALLING GVBUTHDR                                   *
***********************************************************************
TYPEPGM  DC    C'Base Product'
TYPEPGMl DC    al2(typepgml-typepgm)
titpgm   dc    c'View Extract Process              '
titpgml  dc    al2(titpgml-titpgm)
rpt_95ctrl  dc c'GVBMR95E Control Report'
rpt_95ctrll dc al2(rpt_95ctrll-rpt_95ctrl)
rpt_95log   dc c'GVBMR95E Log Report'
rpt_95logl  dc al2(rpt_95logl-rpt_95log)
VERSBLD  DC    CL8'&SYSPARM'
ADDBUFF  DC    A(BUFFER)
BUFLGTH  DC    A(L'BUFFER)
HDRLIST  DS    XL(HEADERPR_L)
*****
At_end   loctr  ,                 position buffer at end of module
BUFFER   DS    CL1200             MESSAGE BUILD AREA
static   loctr  ,
*
GVBHDRA  DC    V(GVBUTHDR)
IEA4PSE  DC    V(IEA4PSE)         PAUSE ELEMENT - PAUSE
IEA4RLS  DC    V(IEA4RLS)         PAUSE ELEMENT - RELEASE
*
*
EVNT     DC    CL4'EVNT'          EVENT    FILE  DDNAME  PREFIX
EXTR     DC    CL4'EXTR'          EXTRACT  FILE  DDNAME  PREFIX
DUMY     DC    CL4'DUMY'          "DUMMY"  FILE  DDNAME  PREFIX
TOKN     DC    CL4'TOKN'          LOOK-UP  FILE  DDNAME  PREFIX
NULL     DC    CL8'NULLFILE'      "DD DUMMY"
MR95RED  DC    CL8'MR95RED '      REFERENCE DATA DDNAME (VERSION 3)
GREFXXX  DC    CL8'REFRXXX '      REFERENCE DATA DDNAME (VERSION 4)
*
STGEX00  DC    CL8'STGEX00'       WRITE   DATA
STGEX01  DC    CL8'STGEX01'       WRITE   INPUT
STGEX10  DC    CL8'STGEX10'       SUMMARIZE EXTRACT
STGEX1K  DC    CL8'STGEX1K'       SUMMARIZE EXTRACT
STGEX4K  DC    CL8'STGEX4K'       SUMMARIZE EXTRACT
STGEX8K  DC    CL8'STGEX8K'       SUMMARIZE EXTRACT
STGEX1M  DC    CL8'STGEX1M'       SUMMARIZE EXTRACT
*
GVBXRCK  DC    CL8'GVBXRCK'       COMMON  KEY  READ EXIT
*
EXECEYEB DC    CL8'EXECDATA'      "EXEC"  STATEMENT    DATA EYEBALL
PARMEYEB DC    CL8'PARMTABL'      "MR95"  PARAMETER   TABLE EYEBALL
ENVVEYEB DC    CL8'ENVVTABL'      "MR95"  ENVIRON VAR TABLE EYEBALL
CTRLEYEB DC    CL8'CTRLRDCB'      CONTROL REPORT        DCB EYEBALL
HASHEYEB DC    CL8'HASHRDCB'      HASH table stats report DCB EYEBALL
INITEYEB DC    CL8'INITAREA'      INITIALIZATION  VARIABLES EYEBALL
DSPEYEB  DC    CL8'DSPTABLE'      DATA    SPACE       TABLE EYEBALL
VDPEYEB  DC    CL8'VDPTABLE'      VIEW    DEFN   PARAMETERS EYEBALL
LTBLEYEB DC    CL8'LOGICTBL'      LOGIC   TABLE             EYEBALL
ROWTEYEB DC    CL8'ROWTABLE'      ROW     ADDRESS     TABLE EYEBALL
CODEEYEB DC    CL8'MACHCODE'      MACHINE CODE              EYEBALL
POOLEYEB DC    CL8'MR95POOL'      LITERAL POOL              EYEBALL
LITSTATEYEB DC CL8'LITSTATP'      LITERAL POOL STATS        EYEBALL
EXTFEYEB DC    CL8'EXTFILEA'      EXTRACT FILE AREA         EYEBALL
LKUPEYEB DC    CL8'LKUPBUFR'      LOOK-UP BUFFER       AREA EYEBALL
THRDEYEB DC    CL8'THRDN000'      THREAD  WORK AREA         EYEBALL
EDCBEYEB DC    CL8'EDCBN000'      THREAD  EVENT   FILE  DCB EYEBALL
EXTREYEB DC    CL8'EXTRN000'      THREAD  EXTRACT    RECORD EYEBALL
STAKEYEB DC    CL8'STACK000'      THREAD  LOOK-UP    STACK  EYEBALL
PIPEEYEB DC    CL8'PIPEL000'      THREAD  PIPE WRITTEN LIST EYEBALL
FISCEYEB DC    CL8'FISCAL00'      Fiscal entry EYEBALL
vtblEYEB DC    CL8'Viewtbl '      View table  EYEBALL
reheyeb  DC    CL8'REHTABL '      "REH" table  EYEBALL
reheyem  DC    CL8'REHTABLM'      Mirror "REH" table  EYEBALL
*
LOG_STANDARD DC  CL8'STANDARD'    log message level
LOG_DEBUG    DC  CL8'DEBUG   '    log message level
*
Br11     llgt  r11,nvnxview-nvconst(,r11) Get address of the start of
         br    r11                the code for the next view
branch_inst_len equ *-br11
         ASSERT branch_inst_len,EQ,8
BRANCH   equ   br11,branch_inst_len  used to have to 4 bytes long now
*                                 trying 6 bytes
*
LAY      LAY   R14,0(,R2)
*
*
         using litp_hdr+524288,r2  lit pool base is +512k
nvdisabs larl  R15,*              get address of CODE (filled in l8r)
         using nvprolog,r15
         OI    nvnop+1,X'F0'      DISABLE CURRENTLY EXECUTING VIEW
         drop  r15
         llgt  r11,nvnxview-nvconst(,r11) Get address of the start of
         br    r11
NVDISAB  equ   *-nvdisabs
*
nop4     jnop  *
nop6     jlnop *
CALLVIEW DS    0h                 CALL VIEW   LOGIC
         LARL  R15,CALLRET        Get return address in R15
         llgt  r1,ltwr200a-logictbl(,r5) get RETK logic pointer
         llgt  R1,vdp0200b_file_reader-vdp0200b_file_record(,r1)
         llgt  r14,ltwrre-logictbl(,r5)   Get WRTK's RE           pgc99
         lt    r0,ltreindx-logictbl(,r14) Get its index           pgc99
         jz    callview_3                 Here if RE not RETK/RETXpgc99
         lgr   r14,r2                     Save this WRTK lit pool pgc99
         sgf   r14,lp_base_litp               work out offset    pgc101
         llgt  r2,lp_base_litp               get thread litp     pgc101
         lgr   r0,r14                     Put in right register   pgc99
callview_3 ds  0h                                                 pgc99
         lgf   r14,ltreindx-logictbl(,r1) Get index of next RETK  pgc99
         mghi  r14,8                      its a word             pgc101
         aghi  r14,litphdrl               allow for lit pool hdr  pgc99
         lgf   r14,0-524288(r2,r14)       get its offset          pgc99
         sty   r0,lp_prev_litpo-litp_hdr-524288(r2,r14) Save prev litpl
         sty   r2,lp_base_litp-litp_hdr-524288(r2,r14) Save base litpl
         la    r2,0(r2,r14)               New lit pool            pgc99
         llgt  R1,LTREES-LOGICTBL(,R1)   get ES logic pointer
         lgf   r14,ltesretn-logictbl(,r1)    Get litpool offset    pgc1
         st    r15,0(r14,r2)                 Save return in litp   pgc1
         lgf   r14,ltespr11-logictbl(,r1)   Get litpool offset     pgc1
         st    r11,0(r14,r2)                r11 for when we return pgc1
         llgt  R15,LTviewnv-logictbl(,R1) get code addr from LT
         aghi  r15,-rdtokenl            point at RDTOKEN code
         br    r15
callview_length equ *-callview
CALLRET  DS   0H                  CALLED  VIEW RETURN LOGIC
         ASSERT callview_length,eq,callview_dsect_l
         drop  r2
*
INITPMV  MVC   prntrdw(0),0(r14)         * * * * E X E C U T E D * * *
INITPVN  MVC   prntline(0),ENVVNAME-ENVVTBL(R3) * E X E C U T E D * * *
INITPVV  MVC   1(0,R1),ENVVVALU-ENVVTBL(R3)   * * E X E C U T E D * * *
MASKBLNK CLC  vdp2000b_REPORT_MASK-vdp2000b_COLUMN_RECORD(0,R15),SPACES
CSVMSKMV MVC   LTCOLMSK-LOGICTBL(0,R7),0(R1)  * * E X E C U T E D * * *
MVCR5R1  MVC   0(0,R5),0(R1)                  * * E X E C U T E D * * *
MVCR1R14 MVC   0(0,R1),0(R14)                 * * E X E C U T E D * * *
MVCR14R1 MVC   0(0,R14),0(R1)                 * * E X E C U T E D * * *
BLNKOUT  MVC   0(0,R14),SPACES                * * E X E C U T E D * * *
*
LTF1PROP MVC   0(0,R15),0(R14)              * * * E X E C U T E D * * *
*
         USING LOGICTBL,R7
*
COPYLOWV MVC   0(0,R6),LTVALUES             * * * E X E C U T E D * * *
COMPLOWV CLC   0(0,R1),LTVALUES             * * * E X E C U T E D * * *
*
*
         USING LTF1_REC,R3
LTF1PACK PACK  DBLWORK,LTF1_VALUE(0)        * * * E X E C U T E D * * *
         DROP  R3
*
         USING LTVV_REC,R3
VARVALMV MVC   LTVVVAL(0),LTVV_VALUE-LTVV_REC(R3) * * E X E C U T E D *
         DROP  R3
*
         DROP  R7
*
         USING TBLHEADR,R6
FILLMAT  CLC   TBFILEID(0),4+1(R14)       MATCHING  FILE ID & RECORD ID
         DROP  R6
*
         USING LKUPBUFR,R5
FILLKYCK CLC   0(0,R1),LBKEY                * * * E X E C U T E D * * *
FILLKYSV MVC   LBKEY(0),0(R1)               * * * E X E C U T E D * * *
         DROP  R5
*
FILLKYMV MVC   ERRDATA(0),0(R1)             * * * E X E C U T E D * * *
*
MVCMODEL MVC   0(0,R4),0(R14)               * * * E X E C U T E D * * *
*
MVCSUBS  EQU   *
COPYHIV  MVC   0(0,R6),0(R9)      * * * *   E X E C U T E D   * * * *
*         drop  rp2
plnr15sp mvc   0(0,r15),spaces
*
***********************************************************************
*  "READ" TOKEN GENERATED CODE (EVENT FILE IS TOKEN)                  *
***********************************************************************
         using litp_hdr+524288,r2  lit pool base is +512k
*rdtoken  llgt r15,nvlogtbl-nvconst(,r11) get logic tab ent for NV pgc3
rdtoken  llgt  r15,rdtokenl+(nvlogtbl-nvnop)(,r15) get logic tab NV ent
         llgt  r14,ltviewre-logictbl(,r15) get the RETK          pgc101
         lgf   r14,ltreindx-logictbl(,r14) get the RETK index    pgc101
         mghi  r14,8                       make it the right dw  pgc101
         aghi  r14,litphdrl                allow for litp header pgc101
         agf   r14,lp_base_litp               plus the base litp pgc101
         llgt  r5,4-524288(,r14)           get the lkup buffer   pgc101
         stg   r6,lp_r6_save              save old event rec addr
         lg    r6,lblstfnd-lkupbufr(,r5)  load data   address    pgc101
*                                                                pgc101
         llgt  r14,ltviewre-logictbl(,r15) Get the RE              @05I
         lgf   r14,ltfilcnt-logictbl(,r14) Get ltfilcnt lit addr   @05I
         agr   r14,r2                      point to ltfilcnt       @05I
         CLC   LBLSTCNT-LKUPBUFR(L'LBLSTCNT,R5),0(r14)             @05I
         je    RDTOKEN_A                                           pgc3
         llgt  r14,ltviewes-logictbl(,r15) Get the ET              pgc3
         llgt  r14,ltcodseg-logictbl(,r14) Get the code for ET     pgc3
         br    r14                         got execute it          pgc3
rdtoken_a ds   0h
RDTOKENL EQU   *-RDTOKEN
         drop  r2
*
TOKNUPD  LA    R7,LBDATA-LKUPBUFR(,R5)
         LA    R8,EXSRTKEY-EXTREC(,R7)
         lgh   r0,EXSORTLN-EXTREC(,R7)
         ah    R0,EXTITLLN-EXTREC(,R7)
         ah    r0,EXDATALN-EXTREC(,R7)
         agr   r8,r0
TOKNUPDL EQU   *-TOKNUPD
*
***********************************************************************
*  GENERATED CODE PREFIX IF FIELD IS FROM LOOK-UP                     *
***********************************************************************
LKUPPREF DS   0XL(lkuppref_l)
* use lkupcode macro to create the code
lkuppref_s ds  0h
         lkupcode real
lkuppref_l equ *-lkuppref_s

lkup_number2  ds 0XL(lkuppref_l)
* use lkupcode macro to create the code
         lkupcode 2ND
*
loadprev ltg   r3,prevreca
*
COUNTMSK DC    X'402020206B2020206B202120'
Mem_mask DC    X'4020206b2020206B2020206B202120'
FULLMASK DC    XL10'40202020202020202120'
FULLMASK8  DC   XL8'4020202020202120'
FULLMASK12 DC  XL12'402020202020202020202120'
FULLMASK15 DC  XL15'402020202020202020202020202120'
FULLMASK16 DC  XL16'40202020202020202020202020202120'
CTMASK   DC    C'-999999999999999.99999999'
NUMMASK  DC    XL08'4020202020202120'
NUMMASK2 DC    XL03'402120'
NUMMASK3 DC    XL04'40202020'
NUMMASK4 DC    XL10'40206B2020206B202120'
POSMASK  DC    XL06'402020202120'
*
CSVMASK  DC    CL48'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.99999999'
*
         ORG   *-240
HEXCONV  EQU   *
         ORG   *+240
         DC    CL16'0123456789ABCDEF'
*
*                 A B C D E F
HEXVAL   DC    X'0A0B0C0D0E0F'
                        EJECT
I        DC    CL2'I '            INPUT  MODE
IN       DC    CL2'IN'            VSAM   INFORMATION
OP       DC    CL2'OP'            VSAM   OPEN
SB       DC    CL2'SB'            VSAM   START   BROWSE
BR       DC    CL2'BR'            VSAM   BROWSE
*
NO       DC    CL1'N'
*
TOKEN    DC    CL5'TOKEN'         WRITE   TOKEN  DDNAME  PREFIX
LNNAN    DC    CL5'LNNAN'         NO MASK CODE
LECOBOL  DC    CL3'CEE'           COBOL  VERSION IDENTIFIER
COBOLII  DC    CL3'C2 '           VS   COBOL  II
                        EJECT
*
*
CTRLFILA DC    A(CTRLFILE)        ADDRESS OF   CONTROL RPT  DCB
*
*
STDPARMS DC    FL4'004'         LOGIC TABLE VERSION NUMBER  EXECVERS
         DC    CL3'004'         io_buffer_level      (multsdn)
         DC    xl1'00'
         DC    CL1'N'           SINGLE THREAD MODE          EXECSNGL
         DC    CL1'N'           TRACE LOGIC TABLE EXECUTION EXECTRAC
         DC    CL1'N'           SNAP LOGIC TBL & GEN CODE   EXECSNAP
*
         DC    CL6'001000'      VDP TABLE SIZE (K)          EXECVSIZ
*
         DC    CL4'9999'        DISK THREADS                EXECDISK
         DC    CL4'9999'        TAPE THREADS                EXECTAPE
         DC    CL13'0000000000000' READ EVENT FILE LIMIT    EXECRLIM
         DC    CL1'N'          DUMMY EXTRACT FILES NOT IN JCL EXECDUMY
         DC    CL8'GVBMRSQ'     DB2 PLAN NAME OVERRIDE(MRSQ) EXECSPLN
         DC    CL8'GVBMRDV'     DB2 PLAN NAME OVERRIDE(MRDV) EXECVPLN
         dc    cl1'N'           zIIP to be used              EXECZIIP
         dc    c'9999'          Max num of SRBs to be active EXEC_SRBLI
         dc    cl1'Y'           Overflow mask on by default  execovfl_o
         dc    cl1'N'           Pagefix                      execpagf
         ds    cl8'        '    BATCH_DATE                   exec_bdate
         ds    cl8'        '    RUN_DATE                     exec_rdate
         ds    cl8'        '    FISCAL_DATE                  exec_fdate
         DC    CL8'GVBMRCT'     DB2 PLAN NAME OVERRIDE(MRCT) EXECTPLN
         DC    cL5'00000'       MSGABEND parameter           EXECmsgab
         DC    cL8'00000000'    LTABEND parameter            EXECltab
         DC    cL3'ISO'         db2 date format
         DC    C'Y'             Optimize_packed_output       exec_optpo
         DC    C'N'             ABEND_on_error_condition   exec_uabend
         DC    C'Y'             ESTAE on by default        exec_estae
         DC    C'Y'             Include RefPools in Dump  Exec_Dump_Ref
         DC    C'Y'             Verify VDP/XLT tm  EXEC_check_timestamp
         DC    C'x'             Spare
         DC    CL8'STANDARD'    LOG message level
         DC    C'N'             reference hash table opt  EXEC_HASHPACK
         DC    C'N'             display hash table stats  EXEC_DISPHASH
         DC    CL2'3'           HASH table multiplier     EXEC_HASHMULT
         DC    XL4'3'          HASH table multiplier bin EXEC_HASHMULTB
*
STDPARML EQU   *-STDPARMS
         ASSERT stdparml,eq,execdlen     these two MUST match
*
***********************************************************************
*  RELATIONAL OPERATOR COMPARE CONVERSION TABLE                       *
***********************************************************************
COMPTBL  DC    CL2'  '            '00' - N/A
EQ       DC    CL2'EQ'            '01' -        EQUAL
NE       DC    CL2'NE'            '02' - NOT    EQUAL
GT       DC    CL2'GT'            '03' - GREATER THAN
GE       DC    CL2'GE'            '04' - GREATER THAN OR EQUAL
LT       DC    CL2'LT'            '05' - LESS    THAN
LE       DC    CL2'LE'            '06' - LESS    THAN OR EQUAL
         DC    CL2'  '                 '07' - Like removed
DB       DC    CL2'DB'            '08' - DAYS BETWEEN
DN       DC    CL2'DN'            '09' - DAYS BETWEEN NEGATED
DM       DC    CL2'DM'            '10' - DAYS IN MONTH
CN       DC    CL2'CN'            '11' - "CONTAINS"
BW       DC    CL2'BW'            '12' - "BEGINS WITH"
EW       DC    CL2'EW'            '13' - "ENDS   WITH"
MB       DC    CL2'MB'            '14' - MONTHS BETWEEN
MN       DC    CL2'MN'            '15' - MONTHS BETWEEN NEGATED
YB       DC    CL2'YB'            '16' - YEARS  BETWEEN
YN       DC    CL2'YN'            '17' - YEARS  BETWEEN NEGATED

*        equates added corresponding to the above character settings
*        use for testing LTF1_COMPARE_TYPE

EQbin    equ   01,1                    -        EQUAL
NEbin    equ   02,1                    - NOT    EQUAL
GTbin    equ   03,1                    - GREATER THAN
GEbin    equ   04,1                    - GREATER THAN OR EQUAL
LTbin    equ   05,1                    - LESS    THAN
LEbin    equ   06,1                    - LESS    THAN OR EQUAL
*LKbin   equ   07,1                      not used as like removed
DBbin    equ   08,1                    - DAYS BETWEEN
DNbin    equ   09,1                    - DAYS BETWEEN NEGATED
DMbin    equ   10,1                    - DAYS IN MONTH
CNbin    equ   11,1                    - "CONTAINS"
BWbin    equ   12,1                    - "BEGINS WITH"
EWbin    equ   13,1                    - "ENDS   WITH"
MBbin    equ   14,1                    - MONTHS BETWEEN
MNbin    equ   15,1                    - MONTHS BETWEEN NEGATED
YBbin    equ   16,1                    - YEARS  BETWEEN
YNbin    equ   17,1                    - YEARS  BETWEEN NEGATED
*
***********************************************************************
*  JUSTIFICATION CONVERSION TABLE                                     *
***********************************************************************
JUSTTBL  DC    C'C'               '00' - N/A
         DC    C'L'               '01' - LEFT
         DC    C'C'               '02' - CENTER
         DC    C'R'               '03' - RIGHT
*
***********************************************************************
*  VIEW TYPE CONVERSION TABLE                                         *
***********************************************************************
VIEWTBL  DC    C' '               '00' - N/A
         DC    C'D'               '01' - DETAIL
         DC    C'S'               '02' - SUMMARY
         DC    C'M'               '03' - SUMMARY MERGE (DT AREA)
         DC    C'C'               '04' - COPY
*
***********************************************************************
*  BINARY LENGTH TO BIT MASK CONVERSION TABLE                         *
***********************************************************************
BYTEMASK DC    B'00000001'        ONE    BYTE  "ICM/STCM" BYTE MASK
         DC    B'00000011'        TWO    BYTES
         DC    B'00000111'        THREE  BYTES
         DC    B'00001111'        FOUR   BYTES
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        L O G I C   T A B L E   F U N C T I O N   C O D E S          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
HD       DC    C'HD'                           HEADER
*
RE_NX    DC    CL4'RENX'                           READ     - NO EXIT
RE_EX    DC    CL4'REEX'                           READ     -    EXIT
RE_TK    DC    CL4'RETK'                           READ     - TOKEN
RE_TX    DC    CL4'RETX'                    READ     - TOKn exit  pgc99
*
NV       DC    CL4'NV  '                           NEW VIEW -
*
LU_SM    DC    CL4'LUSM'                           LOOKUP   - SRCH MEM
LU_EX    DC    CL4'LUEX'                                    - EXIT
*
CF_CC    DC    CL4'CFCC'                           COMPARE  - EVENT/RNG
CF_EC    DC    CL4'CFEC'                                    - EVENT/CON
*
CF_LC    DC    CL4'CFLC'                                    - LOOKUP/CN
CF_LA    DC    CL4'CFLA'                                    -
*
CF_CA    DC    CL4'CFCA'                                    -
*
FN_CC    DC    CL4'FNCC'                                    -
*
LK_C     DC    CL4'LKC '                           LKUP KEY - CONSTANT
LK_E     DC    CL4'LKE '                                    - EVENT
LK_L     DC    CL4'LKL '                                    - LOOKUP
LK_P     DC    CL4'LKP '                                    - PREVIOUS
LK_S     DC    CL4'LKS '                                    - SYMBOL
LK_LR    DC    CL4'LKLR'                                    - LR   ID
JOIN     DC    CL4'JOIN'                                    - LR ID OP
LK_DC    DC    CL4'LKDC'                                    - DATE CON
LK_DE    DC    CL4'LKDE'                                    - DATE EVN
*
KS_LK    DC    CL4'KSLK'                           KEY SAVE - LOOK-UP
*
SK_C     DC    CL4'SKC '                           SORT KEY - CONSTANT
SK_E     DC    CL4'SKE '                                    - EVENT
SK_L     DC    CL4'SKL '                                    - LOOKUP
SK_P     DC    CL4'SKP '                                    - PREVIOUS
SK_A     DC    CL4'SKA '                                    - ACCUM
*
SET_A    DC    CL4'SETA'                           SET ACUM - ACCUM
SET_C    DC    CL4'SETC'                           SET ACUM - CONSTANT
SET_E    DC    CL4'SETE'                                    - EVENT
SET_L    DC    CL4'SETL'                                    - LOOKUP
SET_P    DC    CL4'SETP'                                    - PREVIOUS
*
MUL_L    DC    CL4'MULL'                                    - LOOKUP
*
DT_C     DC    CL4'DTC '                           DATA COL - CONSTANT
*
CT_A     DC    CL4'CTA '                                    - ACCUM
*
NOOP     DC    CL4'NOOP'                           NOOP
GOTO     DC    CL4'GOTO'                           GOTO
*
WR_XT    DC    CL4'WRXT'                           WRITE    - EXTRACT
WR_DT    DC    CL4'WRDT'                                    - "DT" AREA
WR_IN    DC    CL4'WRIN'                                    - EVENT
WR_SU    DC    CL4'WRSU'                                    - SUMMARIZE
WR_EX    DC    CL4'WREX'                                    - EXIT
WR_TK    DC    CL4'WRTK'                                    - TOKEN/DT
WR_TX    DC    CL4'WRTX'                                    - TOKEN/EX
*
ET       DC    CL4'ET  '                           END-OF-TOKEN-SET
ES       DC    CL4'ES  '                           END-OF-SET
EN       DC    CL4'EN  '                           END-OF-TABLE
*
*
                        EJECT
.skip_funcodes anop ,
*temppgc PRINT NOGEN
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D A T A   C O N T R O L   B L O C K S                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
code     loctr  ,       these are read only so can go in the code loctr
TRACfile DCB   DSORG=PS,DDNAME=EXTRTRAC,MACRF=(PM),DCBE=TRACDCBE,      X
               RECFM=FBA,LRECL=161
TRACDCBE DCBE  RMODE31=BUFF
TRACDCBL EQU   *-TRACFILE
*
CTRLFILE DCB   DSORG=PS,DDNAME=EXTRRPT,MACRF=(PM),DCBE=CTRLDCBE,       X
               RECFM=VB,LRECL=164
CTRLDCBE DCBE  RMODE31=BUFF
CTRLDCBL EQU   *-CTRLFILE
*
HASHFILE DCB   DSORG=PS,DDNAME=Hnnnnnnn,MACRF=(PM),DCBE=HASHDCBE,      X
               RECFM=VB,LRECL=164
HASHDCBE DCBE  RMODE31=BUFF
HASHDCBL EQU   *-HASHFILE
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   D R I V E R   T H R E A D   I N I T I A L I Z A T I O N           *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,savesubr
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
         using msglist,msg_area
*
INITTHRD stmg  R14,R12,SAVF4SAG64RS14 SAVE   REGISTERS
*
*
***********************************************************************
*  INITIALIZE DRIVER PROGRAM THREAD WORK AREA CONSTANTS               *
***********************************************************************
         MVI   SPACES+0,C' '      INITIALIZE  CONSTANT
         MVC   SPACES+1(L'SPACES-1),SPACES+0
*
         MVI   HEXFF+0,X'FF'      INITIALIZE  CONSTANT
         MVC   HEXFF+1(L'HEXFF-1),HEXFF+0
*
***********************************************************************
*  INITIALIZE DRIVER PROGRAM THREAD WORK AREA VARIABLES               *
***********************************************************************
         MVC   DRIVDDN,SPACES     INITIALIZE  DRIVER     FILE   DDNAME
*
         xc    GPRECCNT,GPRECCNT  Zero event records read count
         MVC   ERRDATA,SPACES     BLANK   OUT INDICATIVE ERROR    DATA
         MVI   SELSWT,C' '        INIT COLUMN SELECTION  SWITCH
*
         MVHHI SAVALDEC,0         INITIALIZE  "GVBDL96"  FIELDS
         MVHHI SAVALRND,0
         MVHHI SAOUTDEC,0
         MVHHI SAOUTRND,0
         MVHHI SAMSKDIG,0
         MVHHI SAMSKDEC,0
         MVI   SALEAD0,C'0'
         MVC   SALEAD0+1(L'SALEAD0-1),SALEAD0
         MVI   SATRAIL0,C'0'
         MVC   SATRAIL0+1(L'SATRAIL0-1),SATRAIL0
*
         LA    R14,DL96AREA       INITIALIZE "GVBDL96" PARAMETER LIST
         ST    R14,DL96PA
         LA    R14,DL96LEN
         ST    R14,DL96LENA
         LA    R14,DL96RTNC
         ST    R14,DL96RTNA
*
         LA    R14,TP90AREA       INITIALIZE "GVBTP90" PARAMETER LIST
         ST    R14,TP90PA
         LA    R14,LKUPKEY
         ST    R14,TP90KEYA
*
         Larl  R14,MDLWTO         INITIALIZE  WTO PARAMETER AREA
         MVC   WTOPARM(WTOPARML),0(R14)

         lhi   r0,20000                                          pgc400
         mhi   r0,6                                              pgc400
         getmain R,LV=(0)                                        pgc400
         st    r1,symtblb                                        pgc400
         st    r1,symtblc                                        pgc400
         ar    r1,r0                                             pgc400
         st    r1,symtblm                                        pgc400
***********************************************************************
*  INITIALIZE EXEC PARM AREA AND OVERLAY WITH OVERRIDES FROM "EXEC"   *
***********************************************************************
INIT_100 llgt  R2,EXECDADR        SAVE  PARAMETER  LENGTH       ADDRESS
*
         LHI   R0,STDPARML+8      LOAD  STANDARD   PARAMETERS   LENGTH
         GETMAIN RU,LV=(0),LOC=(ANY)
         MVC   0(l'execeyeb,R1),EXECEYEB INITIALIZE EYEBALL
*
         LA    R3,l'execeyeb(,R1)
         ST    R3,EXECDADR        SAVE  PARAMETER  DATA  AREA   ADDRESS
         USING EXECDATA,R3
         MVC   0(STDPARML,R3),STDPARMS  INITIALIZE PARAMETER    AREA
*
         xc    workarea,workarea
         lay   r4,workarea
         lay   r5,timelist
         TIME  BIN,(4),LINKAGE=SYSTEM,DATETYPE=YYYYMMDD,MF=(E,(5))
         llgt  r15,workarea+8     get the date
         sllg  r15,r15,4          Make it into
         stg   r15,workarea        a
         oi    workarea+7,x'0F'    packed number
         unpk  exec_bdate,workarea+3(5) Current date is the default
         mvc   exec_rdate,exec_bdate    Current date is the default
         mvc   exec_fdate,exec_bdate    Current date is the default
         drop  r3

         xc    srb_active,srb_active    make sure active is zero
         mvc   srb_limit,=f'9999'       and set no limit
                        SPACE 3
***********************************************************************
*  SAVE MR95 TRACE DCB ADDRESS                                        *
***********************************************************************
         LA    R0,tracDCBL        LOAD DCB  LENGTH
         GETMAIN R,LV=(0),LOC=BELOW        GET  MEMORY  FOR DCB
         STy   R1,TRACDCBA        SAVE DCB ADDRESS IN  CONTROL ELEMENT
         LR    R2,R1
         USING IHADCB,R2
*
         lay   r15,tracfile
         MVC   ihadcb(tracDCBL),0(r15)   copy dcb
*
         LA    R1,tracDCBE-tracFILE(,R2)
         ST    R1,DCBDCBE
*
                        SPACE 3
***********************************************************************
*  LOCATE "TIOT"                                                      *
***********************************************************************
         LA    R14,TIOTADDR       GET  "TIOT" ADDRESS
         sysstate amode64=NO
         sam31
         EXTRACT (R14),'S',FIELDS=TIOT
         sam64
         sysstate amode64=YES
                        SPACE 3
***********************************************************************
*  CREATE GENEVA GLOBAL TOKEN                                         *
***********************************************************************
         MVC   WORKAREA+0(8),GENEVA
         MVC   WORKAREA+8(8),GLOBAL
         LA    R0,gpthrdno        ENVIRONMENT    AREA
         ST    R0,WORKAR2+00
         LA    R0,LBCHAIN         LOOK-UP BUFFER CHAIN
         ST    R0,WORKAR2+04
         LA    R0,execdadr        will need parameters later
         ST    R0,WORKAR2+08
         lr    r0,r13             save address of main thread
         ST    R0,WORKAR2+12
         llgt  r15,iean4cr
*
         CALL  (r15),(TOKNLVL2,WORKAREA,WORKAR2,TOKNPERS,RETNCODE),    X
               MF=(E,WKREENT)
INIT_150 L     R15,RETNCODE       SUCCESSFUL ???
         CHI   R15,8              TOKEN  ALREADY CREATED ???
         jl    INIT_200
*
         lghi  R14,GLOBAL_TOKEN_FAIL
         BRU   RTNERROR
                        SPACE 3
***********************************************************************
*  ALLOCATE LOOK-UP STACK                                             *
***********************************************************************
INIT_200 LHI   R0,STACKSIZ        LOAD NO. OF STACK ENTRIES
         mhi   R0,12              TIMES 8
         ahi   R0,l'stakeyeb      + EYEBALL
         GETMAIN RU,LV=(0),LOC=(ANY)   OBTAIN MEMORY
         mvc   0(l'stakeyeb,r1),stakeyeb   copy eyeball
         ahi   R1,l'stakeyeb
         ST    R1,gpjstka         SAVE STACK ADDRESS
                        SPACE 3
***********************************************************************
*  DYNAMICALLY ESTABLISH GENEVA SUBROUTINE ENTRY POINTS               *
***********************************************************************
         larl  r1,geninfo
         IDENTIFY EP=GENINFO,ENTRY=(1)
         larl  r1,genwinfo
         IDENTIFY EP=GENWINFO,ENTRY=(1)
         larl  r1,genenvv
         IDENTIFY EP=GENENVV,ENTRY=(1)
                        SPACE 3
*
* Initialise parameter list for calling exit INIT function
*
         LA    R14,env_area       ENVIRONMENT DATA   ADDRESS
         STY   R14,GPENVA
*
*        XC    gpthrdno,gpthrdno  set thread number to 0
*
         LAY   R14,error_buffer   error buffer address for exit wtos
         STY   R14,gp_error_buffer_ptr
*
         LA    R14,RETNCODE       RETURN      CODE   ADDRESS
         STY   R14,GPRTNCA
         LA    R14,RETNPTR        RETURN      RECORD POINTER   ADDRESS
         STY   R14,GPBLOCKA
         OIY   GPBLOCKA,X'80'     MARK END-OF-PARAMETER-LIST
*
INITEXIT lmg   R14,R12,SAVF4SAG64RS14 RESTORE REGISTERS
         BR    R14                    RETURN
*
static   loctr ,
cardmove mvc   prntline(0),0(r1)    ** Executed **
errdmove mvc   errdata(0),0(r4)     ** Executed **

code     loctr
         push  using
         USING THRDAREA,R13
         using savf4sa,savesubr
         using msglist,msg_area
*
static   loctr
runviews_a  dc v(runviews)
code     loctr
         entry runviews
runviews amode 31
Runviews stmg  R14,R12,SAVF4SAG64RS14    SAVE   REGISTERS

*** NOTE *** do NOT use R8 in this routine - it is addressing a
*            control block we need
*   entered in amode 31

 sysstate amode64=NO
 rptit msg=vb_blankl
 phead hd=runv

 xgr r10,r10                        create a zero in both halves
 sty r10,runview_ptr                pointer zero in storage

 llgt  r11,TIOTADDR                      LOAD TIOT  ADDRESS
 using tiot1,r11                         map via r11
 LA    r11,tioelngh                      Get address of first entry
 using tioelngh,r11                      and map that
 do until=(lt,r0,tioelngh,z)

   if CLC,tioeddnm,eq,=c'RUNVIEWS'       Found ?

     bas r10,open_runviews
     leave ,

   endif

   llc   R15,tioelngh                    LOAD  ENTRY LENGTH
   agr   r11,R15                         ADVANCE  TO NEXT    DDNAME
 enddo

 llhfr r10,r10                       get the record counter
 if ltr,r10,r10,z                    and test for zero

   rptit msg=nonemsg                 no data so tell them
   rptit msg=vb_blankl

 endif

 drop  r11
 lmg   r14,r12,SAVF4SAG64RS14            restore registers
 bsm   0,r14

open_runviews ds 0h
***********************************************************************
* Open runviews file, and process it, saving the data                 *
***********************************************************************
  LA          R0,runvDCBL        LOAD DCB  LENGTH
  GETMAIN R,LV=(0),LOC=BELOW              GET  MEMORY  FOR DCB
  LR          R2,R1
  USING IHADCB,R2

  larl        r15,runvdcb
  MVC         ihadcb(runvDCBL),0(r15)   copy dcb

  LA          R1,runvDCBE-runvdcb(,R2)
  ST          R1,DCBDCBE

  MVC   WKREENT(8),OPENPARM            OPEN runviews LIST
  OPEN  ((R2),(INPUT)),MODE=31,MF=(E,WKREENT) runviews FILE
  if    TM,dcboflgs,dcbofopn,o         SUCCESSFUL ???

    ly r9,view_cnt                pick up max no of views
    msfi r9,runvent_l             multiply by length of each entry
    ahi  r9,runv_header_l         add on header length

    storage OBTAIN,length=(r9)    get list area

    lgr  r3,r1                    load the base register
    using runview_list,r3            and map it
    sty  r1,runview_ptr           update pointer in workarea
    st   r9,runv_length           save the length of the area
    xc   runv_first,runv_first    clear the first pointer
    xc   runv_count,runv_count    and the counter
    la   r4,runv_entries          get address of first entry
    st   r4,runv_next             and save back into the header

***********************************************************************
*   Read each record and add any valid data into a list               *
***********************************************************************
    Do  INF,label=read_runv

      get   (R2)

      lhlr  r4,r1                 save r1 in upper r4
      lh    r4,dcblrecl           get the record length
      if chi,r4,gt,l'prntline     if too long
        lhi r4,l'prntline         get max
      endif
      bctr  r4,0                  less 1 for execute
      exrl  r4,copy_runview       copy record
      ahi   r4,l'prntrdw+1        get the right length
      sth   r4,prntrdwh           update rdw
      rptit ,
      aih   r10,1                 increment counter in high r10
      llhfr r1,r4                 restore r1

      if  (CLI,0(R1),ne,C'*')     If not a comment then process
        LR  R4,R1                 LOAD RECORD ENDING   ADDRESS
        AH  R4,DCBLRECL

        do while=(cli,0(r1),eq,c' ') scan for non blank
          ahi r1,1
          if cr,r1,gt,r4          ran off the end ?
            iterate read_runv
          endif
        enddo

*       r1 now points at the first non blank on the record
        xr r5,r5                  clear r5

        do while=(cli,0(r1),ne,c' ') scan for blank
          llc r6,0(,r1)           get the character
          if chi,r6,ge,c'0',and,chi,r6,le,c'9' numeric?
            nill r6,x'000f'       remove the zone
            msfi r5,10            multiply previous by 10
            if ar,r5,r6,o         and add into accumulator
              lghi r14,RUNVIEW_OUT_OF_RANGE overflow - out of range
              j   rtnerror        Signal error
            endif
          else
            lghi r14,RUNVIEW_NOT_NUM Not numeric message
            j     rtnerror        Signal error
          endif
          ahi r1,1
          if cr,r1,gt,r4          ran off the end?
            iterate read_runv
          endif
        enddo

*       now r5 has the view number

        if ltr,r5,r5,p,and,cfi,r5,le,2147483647

          l  r6,runv_next         get new entry address
new       using runvent,r6        map the new entry
          alsi runv_next,runvent_l and move to next one
          alsi runv_count,1         add 1 to counter
          st r5,new.runvent_view     and save the view number

*         new entry is pointed to by r6 (named using new)
*
*         now insert new entry into list in ascending order

          la  r7,runv_first       load address of first entry
old       using runvent,r7        and map it

          l   r4,runv_count       get counter

          do from=(r4)

            if lt,r9,old.runvent_next,z  next entry if any

*             here if next is zero (end of list) so just add
*             new entry following the old pointer

              st r6,old.runvent_next  save new address in pointer
              xc new.runvent,new.runvent   clear new entry pointer

            else

*             list has some elements already so compare view number
*             to keep list in ascending view order

next          using runvent,r9

              clc next.runvent_view,new.runvent_view

              if (h)   next is high so new must go before it

                st r6,old.runvent_next store new ptr into old
                st r9,new.runvent_next store next into new
                leave ,

              elseif (l) next is low, so move along to next element

                lr r7,r9          next becomes old

              elseif (e) Equal - so quietly ignore

                alsi runv_next,-runvent_l reset the header
                alsi runv_count,-1
                leave ,                     and split

              endif

            endif

          enddo
          drop new,old,next,r3

        else
          lghi r14,RUNVIEW_OUT_OF_RANGE overflow - out of range
          j       rtnerror        Signal error
        endif
*
      endif ,                     comment test
    enddo ,                       infinite loop (PARMLOOP)
*
***********************************************************************
* END-OF-FILE FOR RUNVIEWS TABLE                                      *
***********************************************************************
RUNVEOF CLOSE ((R2)),MODE=31,MF=(E,WKREENT)

  LA          R0,runvDCBL        LOAD DCB  LENGTH
  FreeMAIN R,LV=(0),a=(2)        release the area

*   Runview file processing completed now
*   first check if runview list has any useful data and if not
*   free the storage and reset the runview_ptr

    using (runview_list,runv_length+l'runv_length),r1  map the header
    if lt,r1,runview_ptr,nz,and,     runview area present              +
               lt,r0,runv_count,z    but zero active records

      sty r0,runview_ptr             make sure pointer is zero

      l r0,runv_length               get the length
      storage RELEASE,addr=(1),length=(0) and return it

    else

*     runview list has active records - so now check runview list
*     against viewtbl list - issue error if runview entry non existent

      la r3,runv_first

      do from=(r2,runv_count)        loop thru the runview list
        using runvent,r3             Map the entry and
        l r3,runvent_next            get next entry
        l r4,runvent_view            view number in r4

        ly r5,viewtbl_b              Get start of View table
        using view_table,r5

        ly r6,view_cnt        Get number of views

runv_lp do ,
          do from=(r6)
            doexit (c,r4,eq,view_nbr),do=runv_lp
            ahi r5,view_table_ent_len
          enddo

*         only get here if the runview number is not in view table
*         so issue error and exit
*         R4 has the invalid view number
          lay   r14,workarea2
          using workarea2,r14
          cvd   r4,workarea
          mvc   workarea2(l'nummask),nummask
          ed    workarea2(l'nummask),workarea+4
          drop  r14
          LA    r1,l'nummask            set length
          ST    R1,MSGS2LEN
          st    r14,MSGS2PTR            save the address of the data
          XC    MSGS3PTR,MSGS3PTR
          lghi  r14,RUNVIEW_NOT_EXIST
          j     rtnerror

        enddo
      enddo
    endif

  endif
***********************************************************************
* RETURN                                                              *
***********************************************************************
  BR    R10                        RETURN
copy_runview mvc  prntline(0),0(r1)  Executed copy of record
  pop   using
 sysstate amode64=YES
*
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   L O A D   P A R A M E T E R   T A B L E   (L I S T)               *
*                                                                     *
*        R1  -  CURRENT SCAN POSITION (ADDRESS)                       *
*        R2  -  DCB ADDRESS                                           *
*        R3  -  PARAMETER TABLE ENTRY ADDRESS                         *
*        R4  -  CURRENT KEYWORD/VALUE ADDRESS                         *
*        R5  -  END-OF-RECORD   ADDRESS                               *
*        R9  -  PREVIOUS  PARAMETER   TABLE   ENTRY ADDRESS           *
*        R15 -  CURRENT KEYWORD/VALUE LENGTH                          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
code     loctr
         USING THRDAREA,R13
         using savf4sa,savesubr
         using msglist,msg_area
*
PARMLOAD stmg  R14,R12,SAVF4SAG64RS14  SAVE   REGISTERS
*
***********************************************************************
* OPEN PARAMETER FILE                                                 *
***********************************************************************
  LA          R0,parmDCBL        LOAD DCB  LENGTH
  GETMAIN R,LV=(0),LOC=BELOW              GET  MEMORY  FOR DCB
  LR          R2,R1
  USING IHADCB,R2

  larl        r15,parmdcb
  MVC         ihadcb(parmDCBL),0(r15)   copy dcb

  LA          R1,parmDCBE-parmdcb(,R2)
  ST          R1,DCBDCBE
*
  if clc,namepgm,eq,=cl8'GVBMR95R'    R for reference
     mvc  dcbddnam,=cl8'REFRPARM'    default is EXTRPARM
  endif
*
  mvc fiscal_default_cnt,=f'-1'   default fiscal date count
  xc    fisdates,fisdates         set start of Fiscal dates to zero
  xc    fisdatee,fisdatee         set end of Fiscal dates to zero

  phead hd=parm

  MVC   WKREENT(8),OPENPARM            OPEN PARAMETER LIST
  sysstate amode64=NO
  sam31
  OPEN  ((R2),(INPUT)),MODE=31,MF=(E,WKREENT) PARAMETER FILE
  sam64
  sysstate amode64=YES
  if    TM,dcboflgs,dcbofopn,o         SUCCESSFUL ???
*
*
***********************************************************************
* READ "MR95" PARAMETERS UNTIL END-OF-FILE                            *
***********************************************************************
    Do  INF,LABEL=PARMLOOP
*   This is an infinite loop which exits via the EOF exit
*   to PARMEOF (label immediately following the enddo)
      sam31
      sysstate amode64=NO
      get   (R2)
      sysstate amode64=YES
      sam64
*
      llgtr r1,r1
      lgr   R4,R1                 SAVE RECORD ADDRESS
      lgr   R5,R1                 LOAD RECORD ENDING   ADDRESS
      lgh   r15,dcblrecl          get lrecl of input
      agr   R5,r15
*
      bctr  r15,r0                set up for "EX"
      ex    r15,cardmove
      ahi   r15,5                 reset r15 to lrecl + RDW
      sth   r15,prntrdwh          save it
      rptit ,                     print parm card
*
      if  (CLI,0(R4),ne,C'*')     If not a comment then process
*
        jas r14,parse_keyword      See if there is a keyword
* R4 -> start of keyword R1 -> End of keyword
        if  (ltgr,r15,r15,nz)      Nothing found (blank line?)
*
          MVC  ERRDATA,SPACES     CLEAR OLD  VALUE
          bctgr R15,0             DECREMENT LENGTH FOR "EX"
          EX   R15,PARMSAVE       SAVE KEYWORD IN ERROR TEXT (IF ERR)
*
* Locate keyword in table
*
          LArl R6,PARMKWRD_table
          using parmkwrd,r6
*
          do inf
            if (CLI,parmkwrd,eq,x'ff')
              BRE  PARMERR
            endif
            EX R15,PARMCLC
            doexit (e)
            aghi R6,parmkwrd_l
          enddo
*  R1-> start of string, R5->End of string
          jas r14,parse_parm     get the parameter value
*  R4-> start of value, R1->End of value, R15=length value      *
*         st  r4,Parm_start
*         st  r1,Parm_end
*         st  r15,Parm_len
*
          if  (ltgr,r15,r15,nz)     Value found ?
*
***********************************************************************
*   BRANCH TO KEYWORD SPECIFIC LOGIC                                  *
***********************************************************************
            bctr r15,0           Reduce this now
            LH R14,parmnum
            drop r6
            casentry r14
*
              case 03
***********************************************************************
*         EXECUTE 1ST THREAD ONLY                                     *
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC EXECSNGL,0(R4)
*
                CLI EXECSNGL,C'1' YES ?
                BRE EXEC_THD_OK
                CLI EXECSNGL,C'N' NO ?
                BRE EXEC_THD_OK
                CLI EXECSNGL,C'A' ALL ?
                BRE EXEC_THD_OK
                lghi R14,PARM_ERR
                BRU PARMERR
                DROP R14
EXEC_THD_OK     EQU  *
*
              case 04
***********************************************************************
*         TRACE LOGIC TABLE EXECUTION                                 *
***********************************************************************
                xgr  r3,r3
                st  r1,Parm_end
*
                jas  r14,GetParmEnt   create a parm entry
                lgr  r3,r1            address parm entry
*
                llgt r1,Parm_end
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC EXECTRAC,0(R4)
*
                CLI EXECTRAC,C'Y' YES ?
                BRE TRAC_PARM_OK
                CLI EXECTRAC,C'N' NO ?
                BRE TRAC_PARM_OK
                lghi R14,PARM_ERR
                BRU  PARMERR
                DROP R14
TRAC_PARM_Ok    equ  *
*
              case 05
***********************************************************************
*         SNAP LOGIC TABLE AND MACHINE CODE                           *
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC EXECSNAP,0(R4)
*
                CLI EXECSNAP,C'Y' YES ???
                BRE SNAP_PARM_Ok
                CLI EXECSNAP,C'N' NO  ???
                BRE SNAP_PARM_Ok
                DROP R14
                lghi R14,PARM_ERR
                BRU PARMERR
SNAP_PARM_Ok    equ  *
*
              case 06
***********************************************************************
*         CONCURRENT DISK THREADS                                     *
***********************************************************************
                lghi  R14,PARM_ERR   set msg no in case of error
                LA    R0,L'EXECDISK-1
                sgr   R0,R15         COMPUTE RIGHT JUSTIFY LENGTH
                BRM   PARMERR        ERROR IF TOO LONG
                llgt  R14,EXECDADR
                USING EXECDATA,R14
                MVC   EXECDISK,ZEROES
                LA    R14,EXECDISK
                agr   R14,R0
                EX    R15,PARMMVC
*
                llgt  R14,EXECDADR
                larl  R15,TRTTBLU     LOAD NUMERIC CLASS TEST TBL ADDR
                TRT   EXECDISK,0(R15) NUMERIC ???
                jnz   disk_parm_err   No - error
*
                clc   execdisk,zeroes must have at least one
                JNE   DISK_PARM_OK
                DROP  R14
DISK_PARM_err   EQU   *
                lghi  R14,PARM_ERR    set msg no in case of error
                J     PARMERR
DISK_PARM_OK    EQU   *
*
              case 07
***********************************************************************
*         CONCURRENT TAPE THREADS                                     *
***********************************************************************
                lghi  R14,PARM_ERR   set msg no in case of error
                LA    R0,L'EXECTAPE-1
                sgr   R0,R15         COMPUTE RIGHT JUSTIFY LENGTH
                BRM   PARMERR        ERROR IF  TOO LONG
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC   EXECTAPE,ZEROES
                LA  R14,EXECTAPE
                agr R14,R0
                EX  R15,PARMMVC
*
                llgt  R14,EXECDADR
                larl R15,TRTTBLU    LOAD NUMERIC CLASS TEST TBL ADDR
                TRT  EXECTAPE,0(R15) NUMERIC ???
                BRZ  TAPE_PARM_OK   Yes - ok
                DROP R14
                lghi R14,PARM_ERR   ERROR MSG
                J    PARMERR
TAPE_PARM_OK    EQU   *
*
              case 08
***********************************************************************
*         READ EVENT RECORD LIMIT                                     *
***********************************************************************
                lghi  R14,PARM_ERR   set msg no in case of error
                LA    R0,L'EXECRLIM-1
                sgr   R0,R15         COMPUTE RIGHT JUSTIFY LENGTH
                BRM   PARMERR        ERROR IF  TOO LONG
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC EXECRLIM,ZEROES13
                LA R14,EXECRLIM
                agr R14,R0
                EX R15,PARMMVC
*
                llgt  R14,EXECDADR
                larl R15,TRTTBLU     LOAD NUMERIC CLASS TEST TBL ADDR
                TRT  EXECRLIM,0(R15) NUMERIC ???
                jz   readlim_ok      YES - CONTINUE
                DROP R14
                lghi R14,PARM_ERR    NO  - INDICATE  ERROR
                BRU  PARMERR
*
readlim_ok ds  0h

              case 09
***********************************************************************
*         "DUMMY" EXTRACT FILES NOT EXPLICITLY IN JCL                 *
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC EXECDUMY,0(R4)
*
                CLI EXECDUMY,C'Y' YES ?
                BRE DUMMY_PARM_OK
                CLI EXECDUMY,C'N' NO  ?
                BRE DUMMY_PARM_OK
                lghi R14,PARM_ERR
                BRU  PARMERR
DUMMY_PARM_OK   EQU  *
                DROP R14
*
              case 10
***********************************************************************
*         DB2 PLAN NAME OVERRIDE - SQL ("MRSQ")                       *
***********************************************************************
                if   (cghi,R15,gt,L'EXECSPLN-1)
                  lghi R15,L'EXECSPLN-1
                endif
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC EXECSPLN,SPACES
                LA R14,EXECSPLN
                DROP R14
                EX R15,PARMMVC
*
              case 11
***********************************************************************
*         DB2 PLAN NAME OVERRIDE - VSAM ("MRDV")                      *
***********************************************************************
                if   (cghi,R15,gt,L'EXECVPLN-1)
                  lghi R15,L'EXECVPLN-1
                endif
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC EXECVPLN,SPACES
                LA R14,EXECVPLN
                DROP R14
                EX R15,PARMMVC
*
              case 12
***********************************************************************
*         "ZIIP"                                                      *
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC EXECZIIP,0(R4)
                DROP R14
*
              case 13
***********************************************************************
*         SRB LIMIT                                                   *
***********************************************************************
                lghi R14,PARM_ERR     INDICATE  ERROR
                LA R0,L'EXEC_srblimit-1
                sgr R0,R15         COMPUTE RIGHT JUSTIFY LENGTH
                BRM PARMERR       ERROR IF  TOO LONG
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC EXEC_srblimit,ZEROES
                LA R14,EXEC_srblimit
                agr R14,R0
                EX R15,PARMMVC
*
                llgt R14,EXECDADR
                larl R15,TRTTBLU  LOAD NUMERIC CLASS TEST TBL ADDR
                if TRT,EXEC_srblimit,0(R15),z NUMERIC ???
                  pack sadblwrk,exec_srblimit Pack the number
                  cvb  r0,sadblwrk      convert to Binary
                  st   r0,srb_limit     and save in thrd area
                else
                  lghi R14,PARM_ERR     INDICATE  ERROR
                  BRU  PARMERR
                endif
                DROP R14
*
              case 14
***********************************************************************
*         ABEND_ON_CALCULATION_OVERFLOW                               *
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC execovfl_on,0(R4)
                DROP R14
*
              case 15
***********************************************************************
*         ALLOW_PAGING_IO_BUFFERS=Y/N (previously PAGEFIX=n/y)        *
*                                                                     *
*          We will flip the Y/N from 'allow_paging_of_io_buffers'     *
*          so the code for PAGEFIX=N/Y does not have to change        *
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC   execpagf,0(R4)
                oi    execpagf,c' '      make it upper case
                CLI   execpagf,C'Y'
                je    pagf_done
                CLI   execpagf,C'N'
                je    pagf_done
                lghi R14,PARM_ERR       INDICATE  ERROR
                BRU    PARMERR
*               lghi  R14,PAGE_FIX_INVALID
*               BRU   RTNERROR
pagf_done ds  0h
                DROP R14
*
              case 16
***********************************************************************
*         RUN_DATE                                                    *
***********************************************************************
                lghi r14,RUNDATE_LEN_ERR
                LA R0,L'exec_rdate-1
                sgr R0,R15         See if it is too long
                jz  rundate_00    ERROR IF  TOO LONG
                cghi r15,2         Is it a possible "VDP" parm?
                jne rtnerror      N: then must be an error
                lghi r14,RUNDATE_INVALID
                clc 0(3,r4),=cl3'VDP'  is it the "VDP" parameter?
                je  rundate_05         Y:
                j   rtnerror           N:
rundate_00  ds  0h
                lay r14,trttblu   Load numeric test table address
                exrl r15,recidtrt check date is numeric
                jz  rundate_05    Y:
                lghi r14,RUNDATE_NOT_NUM N: get error message
                j   parmerr       Signal error
rundate_05  ds  0h
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC exec_rdate,spaces
                LA R14,exec_rdate
                EX R15,PARMMVC
* RTC21113 set default fiscal date  = run date
                llgt R14,EXECDADR
                USING EXECDATA,R14
                if  (clc,exec_fdate,eq,exec_bdate)
                  mvc  exec_fdate,exec_rdate
                endif
                DROP R14
*
              case 17
***********************************************************************
*         FISCAL_DATE_DEFAULT                                         *
***********************************************************************
*
                lhi  r14,FISCALDATE_DEFAULT_DUP Mult defaults error
                asi  fiscal_default_cnt,1 if count is more than
                jh   rtnerror             zero then we have multiple
                lghi r14,FISCALDATE_LEN_ERR
                cghi r15,2                Is it a possible =VDP (3-1)
                je   srstnfnd_15          Y:
                cghi r15,7                Is it a possible date (8-1)
                jne  rtnerror
                larl R14,trttblu          Load numeric test table addr
                trt  0(8,r4),0(r14)       Numeric?
                jz   srstnfnd_13          Y: then continue
                lghi r14,FISCALDATE_DEFAULT_NOT_NUM N:indicate error
                j    rtnerror
srstnfnd_13 ds 0h
                llgt r14,execdadr
                using execdata,r14
                mvc exec_fdate,0(r4) Save fiscal date in table
                drop r14
                j   fiscexitx
srstnfnd_15 ds 0h
                lghi r14,FISCALDATE_VALUE_ERR
                clc 0(3,r4),=cl3'VDP'  is it the "VDP" parameter?
                jne rtnerror
                llgt r14,execdadr
                using execdata,r14
                mvc exec_fdate(14),0(r4)
                drop r14
*               drop curf
fiscexitx   ds 0h
*
              case 18
***********************************************************************
* DB2 PLAN NAME OVERRIDE - SQL ("MRCT")                               *
***********************************************************************
                if (cghi,R15,gt,L'EXECtPLN-1)
                lghi R15,L'EXECtPLN-1
                endif
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC EXECtPLN,SPACES
                LA R14,EXECtPLN
                DROP R14
                EX R15,PARMMVC
*
              case 19
***********************************************************************
*         ABEND MESSAGE NUMBER                                        *
***********************************************************************
                lghi R14,PARM_ERR NO - INDICATE  ERROR
                LA   R0,L'EXECmsgab-1
                sgr  R0,R15         COMPUTE RIGHT JUSTIFY LENGTH
                BRM  PARMERR       ERROR IF  TOO LONG
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC  EXECmsgab,ZEROES
                LA   R14,EXECmsgab
                agr  R14,R0
                EX   R15,PARMMVC
*
                larl R15,TRTTBLU    LOAD NUMERIC CLASS TEST TBL ADDR
                llgt R14,EXECDADR
                if TRT,execmsgab,0(R15),z NUMERIC ???
                  pack dblwork,execmsgab Pack the number
                  cvb  r0,dblwork   convert to Binary
                  st   r0,abend_msg and save in thrd area
                else
                  lghi R14,PARM_ERR NO - INDICATE  ERROR
                  BRU  parmerr
                endif
                DROP R14
*
              case 20
***********************************************************************
*         ABEND LOGIC TABLE ROW NUMBER LTABEND                       *
***********************************************************************
                lghi R14,PARM_ERR NO  - INDICATE  ERROR
                LA   R0,L'EXECltab-1
                sgr  R0,R15         COMPUTE RIGHT JUSTIFY LENGTH
                BRM  PARMERR       ERROR IF  TOO LONG
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC  EXECltab,ZEROES
                LA   R14,EXECltab
                agr  R14,R0
                EX   R15,PARMMVC
*
                larl R15,TRTTBLU    LOAD NUMERIC CLASS TEST TBL ADDR
                llgt R14,EXECDADR
                if TRT,execltab,0(R15),z NUMERIC ???
                  pack dblwork,execltab Pack the number
                  cvb r0,dblwork    convert to Binary
                  st r0,abend_lt    and save in thrd area
                else
                  lghi R14,PARM_ERR NO  - INDICATE  ERROR
                  j PARMERR
                endif
                DROP R14
*
              case 21
***********************************************************************
*         IO_buffer_level      (multsdn)                              *
***********************************************************************
                LA  R0,L'EXECmsdn-1
                sgr R0,R15         COMPUTE RIGHT JUSTIFY LENGTH
                BRM PARMERR        ERROR IF  TOO LONG
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC  EXECmsdn,ZEROES
                LA   R14,EXECmsdn
                agr  R14,R0
                DROP R14
                EX   R15,PARMMVC
*
              case 22
***********************************************************************
*         DB2_VSAM_DATE_format                                        *
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC   EXEC_db2_df,0(r4)
                OC    EXEC_db2_df,SPACES Upper case
*
                do
                select clc,exec_db2_df,eq
                  when (=C'ISO',=c'JIS',=C'DB2',=C'USA',=C'EUR')
                   doexit
                  othrwise ,
                    lghi R14,PARM_ERR  INDICATE  ERROR
                    BRU PARMERR
                endsel
                enddo
                DROP R14
*
              case 23
*
static   loctr
          ds  0d
         using fiscal_date_entry,r3
recidtrt trt  0(0,r4),0(r14)   uses TRTTBLU
recidpk  pack  dblwork,0(0,r4)  * * * * E X E C U T E D * * * *
         drop r3
code     loctr
*
*         Search for character ":" in string
                la r0,c':'         Search for this
                la r6,1(r15,r4)    Point to after the string
                lgr r14,r6          Save end of string address
srstloop        srst r6,r4         See string contains ":"
                jc 1,srstloop      CPU interrupt
                jc 2,srstnfnd      ":" not found
*
*         Here means ":" found and r6 points to it

*
*         Check length of control record id
                lghi r14,FISCALDATE_CTRL_REC_ERR
                sgr r6,r4           Get the length
                jnp rtnerror       length is 0 so must be an error
                lghi r7,10          Get max recid length
                sgr  r7,r6           Is it too long?
                jm rtnerror         Y:
                bctgr r6,r0         decrement length for "EX"
                larl  R14,trttblu   Load numeric test table address
                ex r6,recidtrt       Numeric?
                jz    convert_recid     Y: then continue
                lghi  r14,FISCALDATE_NOT_NUM  N: then indicate error
                j     rtnerror
*
srstnfnd        lghi  r14,FISCALDATE_VALUE_ERR fiscal_date_override
                j     rtnerror                   invalid
*
*         Convert char control record id to binary
convert_recid ds 0h
                ex  r6,recidpk    pack the value int dblwork
                cvb r7,dblwork     and make it binary

curf    using fiscal_date_entry,r3
prvf    using fiscal_date_entry,r1
                stg r1,dblwork2
                ltgf r1,fisdates          .1st one in MR95 parms?
                jnp alloc_fiscal_tabent .Y: then just allocate it

*
*        Check to see if this control record id is already in list
                lghi  r14,FISCALDATE_CTRL_REC_DUP
recid_chklp ds  0h
                c r7,prvf.fiscal_recid  Same as previous one
                je rtnerror             Y: then parameter error
                ltgf r1,prvf.fiscal_next  Get the next entry
                jp recid_chklp          yes there is one

                lghi r0,fiscal_date_entry_len  get table entry size
                aghi r0,8                add  eyeball   len
                stg r15,dblwork    Save r15 contents
                getmain RU,lv=(0),loc=(ANY) get fiscal table entry
                lg  r15,dblwork    Save r15 contents
                mvc 0(8,r1),fisceyeb    copy eyeball
                la r3,8(,r1)            load base  register
                llgt r1,fisdatee          get last fiscal date
                sty r3,prvf.fiscal_next  save new on in prev last
                sty r3,fisdatee          make new the last on
                sty r7,curf.fiscal_recid Save the new record id
                xc  curf.fiscal_next,curf.fiscal_next sero next
                j srst_upd_lens

*
*         Allocate a control record fiscal date table entry
alloc_fiscal_tabent ds 0h
                lghi r0,fiscal_date_entry_len  get table entry size
                aghi r0,8                add  eyeball   len
                stg r15,dblwork    Save r15 contents
                getmain RU,lv=(0),loc=(ANY) get fiscal table entry
                lg  r15,dblwork    Restore r15 contents
                mvc 0(8,r1),fisceyeb    copy eyeball
                la r3,8(,r1)            load base  register
                sty r3,fisdates         save start of fisc dates
                sty r3,fisdatee         save end of fisc dates
                xc  curf.fiscal_next,curf.fiscal_next zero next
                st r7,curf.fiscal_recid  Save the new record id

                drop prvf
*
*         Reset length in r15 and start of parameter in r4
srst_upd_lens ds 0h
                lg r1,dblwork2
                lghi r14,FISCALDATE_LEN_ERR
                sgr r15,r6          make the length
                aghi r15,-2            left correct
                jnp rtnerror       if not +ve then error in parameter
                agr r4,r6           make r4 point
                aghi r4,2              past the ';'
*
*         We are here if the user has specified
*            FISCAL_DATE=recid:VDP or FISCAL_DATE=recid:ccyymmdd
*         This specifying a value for a specific control record
*
                cghi r15,2           Is it a possible =VDP (3-1)
                je srstnfnd_05       Y:
                cghi r15,7           Is it a possible date (8-1)
                jne rtnerror
                larl  R14,trttblu    Load numeric test table address
                trt 0(8,r4),0(r14)   Numeric?
                jz    srstnfnd_03    Y: then continue
                lghi  r14,FISCALDATE_NOT_NUM N: indicate error
                j     rtnerror
srstnfnd_03 ds 0h
                mvc curf.fiscal_date,0(r4) Save fiscal date in table
                j   fiscexit
srstnfnd_05 ds 0h
                lghi r14,FISCALDATE_VALUE_ERR
                clc 0(3,r4),=cl3'VDP'  is it the "VDP" parameter?
                jne rtnerror
                mvc curf.fiscal_date(3),0(r4)
*               j   fiscexit
fiscexit        ds  0H
                drop curf
*
              case 24
***********************************************************************
*               Optimize_packed_output                                *
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC  EXEC_optpo,0(R4)
                OI   EXEC_optpo,X'40' Upper case
*
                if CLI,exec_optpo,ne,c'Y',and, not yes?? and           +
               CLI,exec_optpo,ne,c'N' not no?
                  lghi R14,PARM_ERR
                  BRU PARMERR
                endif
                DROP R14
*
              case 25
***********************************************************************
*               ABEND_ON_ERROR_CONDITION                              *
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC  EXEC_UABEND,0(R4)
                OI   EXEC_UABEND,X'40'  Upper case
*
                if CLI,EXEC_UABEND,ne,c'Y',and, not yes?? and          +
               CLI,EXEC_UABEND,ne,c'N' not no?
                  lghi R14,PARM_ERR
                  BRU PARMERR
*                 lghi R14,ABEND_USER_OPTION_ERR
*                 BRU RTNERROR
                endif
*
              case 26
***********************************************************************
*               ESTAE - RECOVER_FROM_ABEND                            *
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC  EXEC_ESTAE,0(R4)
                OI   EXEC_ESTAE,X'40'  Upper case
*
                if CLI,EXEC_ESTAE,ne,c'Y',and, not yes?? and           +
               CLI,EXEC_ESTAE,ne,c'N' not no?
*                 lghi R14,RECOVER_ABEND_OPT_ERR
*                 BRU RTNERROR
                endif
                DROP R14
*
              Case 27
***********************************************************************
*               Include RefPools in dump Y/N - Default Y              *
***********************************************************************
                LLGT R14,EXECDADR
                Using EXECDATA,R14
                MVC  EXEC_Dump_Ref,0(R4)
                OI   EXEC_Dump_Ref,X'40'  Upper case
*
                If CLI,EXEC_Dump_Ref,NE,C'Y',And, Not Yes and          +
               CLI,EXEC_Dump_Ref,NE,C'N'          Not No
                  lghi R14,PARM_ERR
                  J   PARMERR
                EndIf
                Drop R14
*
              case 28
***********************************************************************
*               EXTRLOG/REFRLOG message level                         *
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC  EXEC_LOGLVL,0(R4)
                OC   EXEC_LOGLVL,SPACES Upper case
                if CLC,EXEC_LOGLVL,ne,LOG_STANDARD,and,                +
               CLC,EXEC_LOGLVL,ne,LOG_DEBUG
                  lghi R14,PARM_ERR
                  BRU PARMERR
                endif

                IF CLC,EXEC_LOGLVL,eq,LOG_DEBUG
                  OI  WORKFLAG1,MSGLVL_DEBUG
                endif
*
                DROP R14
*
              case 29
***********************************************************************
*               VERIFY_CREATION_TIMESTAMP (VDP and JLT/XLT timestamps)*
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC  EXEC_check_timestamp,0(R4)
                OC   EXEC_check_timestamp,SPACES Upper case
                if CLI,EXEC_check_timestamp,ne,C'Y',and,               +
               CLI,EXEC_check_timestamp,ne,c'N'
                  lghi R14,PARM_ERR
                  BRU PARMERR
                endif
*
                DROP R14
*
              case 30
***********************************************************************
*               HASH_PACK PACK the reference key before CKSM          *
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC  EXEC_HASHPACK,0(R4)
                OC   EXEC_HASHPACK,SPACES Upper case
                if CLI,EXEC_HASHPACK,ne,C'Y',and,                      +
               CLI,EXEC_HASHPACK,ne,C'N'
                  lghi R14,PARM_ERR
                  BRU PARMERR
                endif
*
                DROP R14
*
              case 31
***********************************************************************
*               HASH_MULT mult factor for hash table 1-10             *
***********************************************************************
                lghi R14,PARM_ERR
                LA   R0,L'EXEC_HASHMULT-1
                sgr  R0,R15         COMPUTE RIGHT JUSTIFY LENGTH
                BRM  PARMERR        ERROR IF  TOO LONG

                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC  EXEC_HASHMULT,ZEROES
                LA   R14,EXEC_HASHMULT
                agr  R14,R0
                EX   R15,PARMMVC
*
                larl R15,TRTTBLU    LOAD NUMERIC CLASS TEST TBL ADDR
                llgt R14,EXECDADR
                if TRT,exec_HASHMULT,0(R15),z NUMERIC ???
                  pack dblwork,exec_HASHMULT Pack the number
                  cvb r0,dblwork    convert to Binary
                  st  r0,EXEC_HASHMULTB  and save
                  If CHI,R0,GT,10,or, limited to 1-10                  +
               CHI,R0,lt,1
                    lghi R14,PARM_ERR NO - INDICATE ERROR
                    j PARMERR
                  endif
                else
                  lghi R14,PARM_ERR NO  - INDICATE  ERROR
                  j PARMERR
                endif
                DROP R14
*
              case 32
***********************************************************************
*               DISPLAY_HASH display details for ref table hash values*
***********************************************************************
                llgt R14,EXECDADR
                USING EXECDATA,R14
                MVC  EXEC_DISPHASH,0(R4)
                OC   EXEC_DISPHASH,SPACES Upper case
                if CLI,EXEC_DISPHASH,ne,C'Y',and,                      +
               CLI,EXEC_DISPHASH,ne,C'N'
                  lghi R14,PARM_ERR
                  BRU PARMERR
                endif
*
                DROP R14
*
            endcase
          else ,
            b  parmerr           r15 is 0 or negative - split
          endif ,
***********************************************************************
*   ADVANCE TO NEXT PARAMETER KEYWORD                                 *
***********************************************************************
        endif ,                     blank line
      endif ,                      comment test
    enddo ,                       infinite loop (PARMLOOP)
*
***********************************************************************
* END-OF-FILE FOR PARAMETER TABLE                                     *
***********************************************************************
PARMEOF ds    0h
    sysstate amode64=NO
    sam31
    CLOSE ((R2)),MODE=31,MF=(E,WKREENT)
    sam64
    sysstate amode64=YES
*
    MVC        ERRDATA,SPACES
*
*
  else

    rptit msg=nonemsg                 no data so tell them

  endif
***********************************************************************
* RETURN                                                              *
***********************************************************************
  lmg   R14,R12,SAVF4SAG64RS14     RESTORE REGISTERS
  BR    R14                        RETURN
*
parmerr ds     0h
        LHI    R14,PARM_ERR
        BRU    RTNERROR
*
static  loctr ,
          using parmkwrd,r6
PARMCLC CLC    0(0,R4),parmkwrd   * * * * E X E C U T E D * * * *
            DROP R6
PARMMVC MVC    0(0,R14),0(R4)     * * * * E X E C U T E D * * * *
PARMSAVE MVC   ERRDATA(0),0(R4)   * * * * E X E C U T E D * * * *
*
*
PARMPACK PACK  DBLWORK,0(0,R4)  * * * * E X E C U T E D * * * *
PARMPACK_d PACK  DBLWORK(16),0(0,R4)  * * * * E X E C U T E D * * * *
*
parmkwrd_map   dsect
parmkwrd ds    cl35
parmnum  ds    h
parmkwrd_l equ *-parmkwrd
gvbmr96  csect
static   loctr ,
code     loctr ,
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   Read the Trace parameters                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Tracload ds    0h
         stmg  R14,R12,SAVF4SAG64RS14     save REGISTERS
*
* OPEN Trace Parameter file                                           *
*
  LA          R0,tprmDCBL        LOAD DCB  LENGTH
  GETMAIN R,LV=(0),LOC=BELOW              GET  MEMORY  FOR DCB
  LR          R2,R1
  USING IHADCB,R2

  larl r15,tprmdcb
  MVC ihadcb(tprmDCBL),0(r15)           copy dcb

  LA          R1,tprmDCBE-tprmdcb(,R2)
  ST          R1,DCBDCBE
*
  if clc,namepgm,eq,=cl8'GVBMR95R'           R for reference
    mvc         dcbddnam,=cl8'REFRTPRM'    default is EXTRTPRM
  endif
*
  rptit msg=vb_blankl
  phead hd=tprm
*
  MVC          WKREENT(8),OPENPARM            OPEN TRACE PRM LIST
  sysstate amode64=NO
  sam31
  OPEN         ((R2),(INPUT)),MODE=31,MF=(E,WKREENT) TRACE PRM FILE
  sam64
  sysstate amode64=YES
  if    TM,dcboflgs,dcbofopn,o         SUCCESSFUL ???
*
    xgr   r3,r3              Indicate start of parms
*
* Read the Trace parameter file
*
    Do  INF,LABEL=tprmLOOP
*   This is an infinite loop which exits via the EOF exit
*   to tprmEOF (label immediately following the enddo)
      sam31
      sysstate amode64=NO
      get   (R2)
      sysstate amode64=YES
      sam64
*
      llgtr r1,r1
      lgr   R4,R1                 SAVE RECORD ADDRESS
      lgr   R5,R1                 LOAD RECORD ENDING   ADDRESS
      lgh   r15,dcblrecl          get lrecl of input
      agr   R5,r15
*
* write to the control report
*
      bctr  r15,r0                set up for "EX"
      ex    r15,cardmove
      ahi   r15,5                 reset r15 to lrecl + RDW
      sth   r15,prntrdwh          save it
      rptit ,                     print trace parm card
*
* R4 -> start of record, R5 -> End of record
*
      if  (CLI,0(R4),ne,C'*')     If not a comment then process
*
        oi initflag,trace_1st     This is the first on the line
        do inf
          jas r14,parse_keyword      See if there is a keyword
* R4 -> start of keyword R1 -> End of keyword
          if  (ltgr,r15,r15,nz)      Nothing found (blank line?)
*
            MVC  ERRDATA,SPACES     CLEAR OLD  VALUE
            BCTR R15,0              DECREMENT LENGTH FOR "EX"
            EX   R15,PARMSAVE       SAVE KEYWORD IN ERROR TEXT (IF ERR)
*
* Locate keyword in table
*
            LArl R6,TraceParmtable
            using parmkwrd,r6
*
            do inf
              if (CLI,parmkwrd,eq,x'ff')
                je  tprmerr
              endif
              EX R15,TPRMCLC       CLC 0(0,R4),parmkwrd
              doexit (e)
              aghi R6,parmkwrd_l
            enddo
*
*  R1-> start of string, R5->End of string
            jas r14,parse_parm     get the parameter value
*  R4-> start of value, R1->End of value, R15=length value      *
            st  r4,Parm_start
            st  r1,Parm_end
            st  r15,Parm_len
*
            if (ltgr,r15,r15,nz)    if there is a value ...
*
              ltgr r3,r3           Is this the very first record/parm?
              bnz  tprm003         No
              ltgf R3,PARMTBLA     Do we have an entry already
              jz   tprm005         no, get an entry
              j    tprm010         yes, we can use that
*
tprm003       equ *
              tm   initflag,trace_1st first on the line?
              jz   tprm010            No, part current trace set
*
              LH   R14,parmnum
              ch   r14,=H'1'      is this 'view' keyword?
              jne  tprm010        no, must be part of current trace set
*
tprm005       equ *
              jas r14,GetParmEnt  Add new entry to parm chain
              lgr r3,r1           address entry
tprm010       equ *
*
***********************************************************************
*   BRANCH TO KEYWORD SPECIFIC LOGIC                                  *
***********************************************************************
              lgf  r15,Parm_len
              bctr r15,0           Reduce this now
              lgh  R14,parmnum
              drop r6
              using PARMTBL,r3
              casentry r14
*
              case 1
***********************************************************************
*               View ID                                               *
***********************************************************************
* View ID marks the start of a 'trace set'
*
                tm initflag,trace_1st first on the line?
                bz tprmerr            no, parm error
*
*               jas r14,GetParmEnt  Add new entry to parm chain
*               lr  r3,r1
*
                EX R15,PARMPACK
                CVB R0,DBLWORK
                ST R0,PARMVIEW    SAVE VIEW ID
                MVI VIEWTRAC,C'Y' VIEW SPECIFIC TRACE PARAMETERS
*
              case 2
***********************************************************************
*               EVENT RECORD COUNT - FROM VALUE                       *
***********************************************************************
                EX R15,PARMPACK_d
                cvbg r0,dblwork
                stg r0,parmfrom
*
              case 3
***********************************************************************
*               EVENT RECORD COUNT - THRU VALUE
***********************************************************************
                EX R15,PARMPACK_d
                cvbg r0,dblwork
                stg r0,parmthru
*
              case 4
***********************************************************************
*               LOGIC TABLE ROW NUMBER - FROM VALUE
***********************************************************************
                EX R15,PARMPACK
                CVB R0,DBLWORK
                ST R0,PARMROWF    SAVE  LOGIC TABLE FROM  ROW
*
              case 5
***********************************************************************
*               LOGIC TABLE ROW NUMBER - THRU VALUE
***********************************************************************
                EX R15,PARMPACK
                CVB R0,DBLWORK
                ST R0,PARMROWT    SAVE  LOGIC TABLE THRU  ROW
*
              case 6
***********************************************************************
*               LOGIC TABLE FUNCTION CODE
***********************************************************************
                if CHI,R15,gt,3
                  LHI R15,3
                endif
                MVI PARMFUNC+0,C'*'
                MVC PARMFUNC+1(L'PARMFUNC-1),PARMFUNC+0
                LA R14,PARMFUNC
                EX R15,PARMMVC
                ahi r15,1
                ST R15,PARMFLEN   SAVE  FUNCTION CODE LENGTH
*
              case 7
***********************************************************************
*         EVENT FILE DDNAME
***********************************************************************
                if  (CHI,R15,gt,L'PARMDDN-1)
                  LHI R15,L'PARMDDN-1
                endif
                LA R14,PARMDDN
                EX R15,PARMMVC
*
              case 8
***********************************************************************
*         EVENT RECORD VALUE POSITION - VPOS                          *
***********************************************************************
                EX R15,PARMPACK
                CVB R0,DBLWORK
                BCTR R0,0         CONVERT  TO   OFFSET
                STH R0,PARMVOFF   SAVE  VALUE   OFFSET
*
              case 9
***********************************************************************
*         EVENT RECORD VALUE LENGTH - VLEN                            *
***********************************************************************
                EX R15,PARMPACK
                CVB R0,DBLWORK
                STH R0,PARMVLEN   SAVE  VALUE   LENGTH
*
              case 10
***********************************************************************
*         EVENT RECORD FIELD VALUE                                    *
***********************************************************************
                MVC  PARMVALD,SPACES  Init the displayable value
*
                if clc,0(2,r4),eq,=xl2'E77D',or,                       +
               clc,0(2,r4),eq,=xl2'A77D'
* Value X'hhhhhhhh' or x'hhhhhhhh'
                  if (CHI,R15,GT,L'PARMVALU*2+2)
                    LHI R15,L'PARMVALU*2+2
                  endif
*                                   check for end quote
                  l  r1,parm_end      point to end of parm
                  ahi r1,-1           back up one
                  if cli,0(r1),ne,x'7D'
                    b tprmerr         no end quote - error
                  endif
*                 Save displayable value
                  LA R14,PARMVALD
                  EX R15,PARMMVC
*
                  la r4,2(,r4)        point past x'
                  ahi r15,-2          length of hex chars
*                 check for even number of digits
                  if tml,r15,x'0001',o
                    b tprmerr         odd number of digits
                  endif
*
                  lr  r14,r4
                  lr  r1,r15          length
                  la  r15,parmvalu    Address of required result
*
                  jas r9,convert_hex
*
                else
* Value is character
                  if (CHI,R15,GT,L'PARMVALU-1)
                    LHI R15,L'PARMVALU-1
                  endif
                  LA R14,PARMVALU   Save working value
                  EX R15,PARMMVC
                  LA R14,PARMVALD   Save displayable value
                  EX R15,PARMMVC
                endif
*
              case 11
***********************************************************************
*         Displaysource                                              *
***********************************************************************
                if  (CHI,R15,gt,L'PARMDUMP-1)
                  LHI R15,L'PARMDUMP-1
                endif
                LA R14,PARMDUMP
                EX R15,PARMMVC
*
              case 12
***********************************************************************
*         LTROW - trace this row number                               *
***********************************************************************
                EX R15,PARMPACK
                CVB R0,DBLWORK
                ST R0,PARMROWF    SAVE  LOGIC TABLE FROM  ROW
                ST R0,PARMROWT    SAVE  LOGIC TABLE THRU  ROW
*
              case 13
***********************************************************************
*         REC   - trace this record                                   *
***********************************************************************
                EX R15,PARMPACK_d
                cvbg r0,dblwork
                stg r0,parmfrom
                stg r0,parmthru
*
              case 14
***********************************************************************
*         COL   - trace this column                                   *
***********************************************************************
                EX R15,PARMPACK
                cvb r0,dblwork
                st  r0,parmfcol
                st  r0,parmtcol
*
              case 15
***********************************************************************
*         FROMCOL   - trace from this column                          *
***********************************************************************
                EX R15,PARMPACK
                cvb r0,dblwork
                st  r0,parmfcol
*
              case 16
***********************************************************************
*         THRUCOL   - trace to this column                            *
***********************************************************************
                EX R15,PARMPACK
                cvb r0,dblwork
                st  r0,parmtcol
*
              endcase
            else ,
              b  tprmerr           r15 is 0 or negative
            endif ,

* If there is a comma there could be another parameter
* parm_end --> end of the parm value

            llgt r4,parm_end
*
            DO while=(cgr,r4,lt,r5)
              doexit (cli,0(r4),ne,c' ')  advance past spaces
              aghi R4,1               move along one character
            ENDDO
*           cr   r4,r5               all blanks?
*           jnl  doexit              yes, go read next record
            doexit (cgr,r4,ge,r5)     yes, go read next record
            cli  0(r4),c','          Should be a comma
            bne  tprmerr             If not, error
*
* R4->start of string,  R5->End of string
* Loop around and loop for next parm
*
            aghi r4,1                 skip the comma
            ni initflag,x'ff'-trace_1st turn first flag off

          else
            doexit (z)
          endif ,                 End blank line
        enddo ,                   End loop along line
      endif  ,                    End not a comment
    enddo ,                       infinite loop (tprmLOOP)
*
***********************************************************************
* END-OF-FILE FOR Trace parameters                                    *
***********************************************************************
tprmEOF  ds   0h
         sam31
         sysstate amode64=NO
         CLOSE ((R2)),MODE=31,MF=(E,WKREENT)
         sysstate amode64=YES
         sam64
         MVC        ERRDATA,SPACES
*
* Parameters specifying LT rows, and column numbers are mutually
*  exclusive - check all trace parm blocks
*
         ltgf  R3,PARMTBLA        first trace parm block
         do while=(ltr,r3,r3,p)

             if oc,PARMROWs,PARMROWs,nz
               if oc,PARMcols,PARMcols,nz
                 lghi R14,LTROW_COL_MUT_EX
                 j RTNERROR
               endif
             endif
             lgf r3,PARMNEXT
         enddo
*
  else
         rptit msg=nonemsg        no data so tell them
  endif                           End - successful open
  drop   r3
***********************************************************************
* RETURN                                                              *
***********************************************************************

         lmg   R14,R12,SAVF4SAG64RS14     RESTORE REGISTERS
         BR    R14                        RETURN
*
tprmerr  ds    0h
         lghi  R14,TRACE_PARM_ERR
         j     RTNERROR
*
static   loctr ,
         using parmkwrd,r6
tprmCLC  CLC   0(0,R4),parmkwrd   * * * * E X E C U T E D * * * *
         DROP  r6
code     loctr ,
***********************************************************************
* Convert Alphanumeric string to hex string                           *
*                                                                     *
* Input  R14->start of string, R1=length of string                    *
*        R15->address where we want the result to go                  *
*
* R9 - return address
***********************************************************************
convert_hex  ds 0h
          stmg r6,r7,cdd_save    save regs
          xgr R6,R6              ZERO WORK   REGISTER
          ex  r1,string_upper
static    loctr ,
string_upper OC 0(1,R14),SPACES    Upper case
code      loctr ,
*
         do from=(r1)
           IC   R6,0(,R14)        LOAD  1ST HEX   DIGIT
           if cghi,R6,ge,X'F0'    digit 0-9
             SLL R6,28
             SRL R6,24
           else
             aghi R6,-X'C1'       CONVERT TO TABLE BASE
             BRM ch_err
             cghi R6,5
             JH  ch_err
             IC  R6,HEXVAL(R6)
             SLL  R6,4
           endif
*
           xgr R7,R7              ZERO  WORK REGISTER
           IC  R7,1(,R14)         LOAD  2ND  HEX  DIGIT
           if cghi,R7,ge,X'F0'    digit 0-9
             nilf r7,x'0000000f'
           else
*
             aghi R7,-X'C1'       CONVERT TO TABLE BASE
             BRM ch_err
             cghi R7,5
             JH  ch_err
             IC  R7,HEXVAL(R7)
           endif
*
           ogr R6,R7              put nibbles together
*
           STC R6,0(,R15)         save byte in result field
           LA  R14,2(,R14)        point to next two chars
           LA  R15,1(,R15)        point to next byte in char field
           aghi R1,-1
         enddo
*
          lmg  r6,r7,cdd_save    restore regs
          br   r9
*
ch_err    lghi R14,HEX_STRING_ERR
          J   STDERROR
*
***********************************************************************
* Parse the parameter file - return keyword                           *
* String is format 'keyword  =  value' (any number of spaces between) *
*                                                                     *
* Input  R4->start of string,  R5->End of string                      *
* Output R4->start of keyword, R1->End of keyword, R15=length keyword
*                                               R15=0, no keyword found
*
* R14 - return address
***********************************************************************
Parse_keyword ds 0h
*
          xgr R15,R15              zero length means nothing found
*
* Skip leading spaces
*
          DO while=(cgr,r4,lt,r5)
            doexit (cli,0(r4),ne,c' ')
            aghi R4,1               move along one character
          ENDDO
          cgr  r4,r5               all blanks?
          jnl  Parse_k_ret         yes, quit
*
* Locate end of keyword
*
          lgr      R1,R4
          do   while=(cgr,r1,lt,r5)
            doexit (cli,0(r1),eq,c'='),or,(cli,0(r1),eq,c' ')
            aghi R1,1
          enddo
*
          lgr R15,R1
          sgr R15,R4               Calc length of keyword
*
* Limit length to keyword maximum
*
          if   cghi,r15,gt,l'parmkwrd
            Lghi R15,l'parmkwrd
          endif
*
Parse_k_ret ds 0h
*
*         r15 = length/0 , r4 -> start , r1 -> end
*
          br   r14

***********************************************************************
* Parse the parameter file - look for parameter value                 *
*
* Input  R1-> start of string, R5->End of string                      *
* Output R4-> start of value, R1->End of value, R15=length value      *
*
* R14 - return address
***********************************************************************
Parse_parm ds 0h
*
           xgr r15,r15           zero length (no parm found)
*
*   Check for any blanks before the "="                               *
*
          DO while=(cgr,r1,lt,r5)
            doexit (cli,0(r1),ne,c' ')
            aghi R1,1               move along one character
          ENDDO
          cgr   r1,r5               all blanks?
          jnl  Parse_p_ret         yes, quit
*
          cli  0(r1),c'='         Must have an '='
          jne  Parse_p_ret          or error
          aghi  r1,1               skip '='
*
*   Locate start of parameter value
*
          DO while=(cgr,r1,lt,r5)   skip blanks
            doexit (cli,0(r1),ne,c' ')
            aghi R1,1               move along one character
          ENDDO
*
*   Is it enclosed in quotes?
*
          if cli,0(r1),eq,c''''
            aghi  r1,1               skip quote
            lgr  r4,r1              Save start of parameter value
*
*         Locate end quote
*
            do   while=(cgr,r1,lt,r5) have we hit the end yet?
              doexit cli,0(r1),eq,c''''
              aghi R1,1
            enddo
*
            lgr  R15,R1
            sgr  R15,R4              Calculate the length
*
            aghi  r1,1               skip end quote
*
          else
            lgr  r4,r1              Save start of parameter value
*
*   Locate end of parm value
*
            do   while=(cgr,r1,lt,r5) have we hit the end yet?
              doexit (cli,0(r1),eq,c','),or,(cli,0(r1),eq,c' ')
              aghi R1,1
            enddo
*
            lgr  R15,R1
            sgr  R15,R4              Calculate the length
          endif
*
Parse_p_ret ds 0h
*
*         r15 = length/0 , r4 -> start , r1 -> end
*
          br   r14

***********************************************************************
* Get an entry for the trace parameter table/chain                    *
*    and initialize it
* Input  R3 -> Current Entry, or 0
* Output R1 -> New Entry
* R14 - return address
***********************************************************************
GetParmEnt ds 0h
*
          lghi R0,PARMTLEN         LOAD PARAMETER TABLE ENTRY SIZE
          aghi R0,8                ADD  EYEBALL   LEN
          GETMAIN RU,LV=(0),LOC=(ANY) GET PARAMETER TABLE ENTRY
          MVC 0(8,R1),PARMEYEB    COPY EYEBALL
          LA  R1,8(,R1)           Address past Eyecatcher
np        USING PARMTBL,R1        new in chain
*
          XC  np.PARMTBL(PARMTLEN),np.PARMTBL   ZERO AREA
          mvc np.parmfrom,bzero
          mvc np.parmthru,bnines
          MVC np.PARMDDN,SPACES
          MVC np.PARMFUNC,SPACES
          MVC np.PARMDUMP,SPACES
*
cp        USING PARMTBL,R3        current in chain
          if (ltgr,r3,r3,z)        first entry?
            LA  R3,PARMTBLA       INITIALIZE PREVIOUS ENTRY ADDRESS
          endif

          ST R1,cp.PARMNEXT       CHAIN TO old entry
*
          br   r14
          drop np
          drop cp

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   E D I T   P A R A M E T E R S                                     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
code     loctr ,
         USING THRDAREA,R13
         using savf4sa,savesubr
         using genenv,env_area
*
EDITPARM stmg  R14,R12,SAVF4SAG64RS14  SAVE   REGISTERS
*
         llgt  R2,EXECDADR        LOAD "EXEC" STATEMENT DATA ADDRESS
         USING EXECDATA,R2
***********************************************************************
*  EDIT EVENT FILE READ LIMIT                                         *
***********************************************************************
         PACK  dblwork(16),EXECRLIM   CONVERT READ LIMIT TO "PACKED"
         cvbg  r0,dblwork
         stg   r0,readlim
***********************************************************************
*  EDIT IO buffer level      (MULTSDN)                                *
***********************************************************************
         larl  R14,TRTTBLU         LOAD  NUMERIC CLASS TEST TBL ADDR
         if TRT,execmsdn,0(R14),nz NUMERIC  ???
           lghi R14,IO_BUFFER_LEVEL_ERR  INDICATE  ERROR
           j   RTNERROR
         endif

         if clc,execmsdn,eq,zeroes,or,                                 +
               clc,execmsdn,gt,c254
            lghi R14,IO_BUFFER_LEVEL_ERR  must be 1 < multsdn < 254
            j   RTNERROR
         endif

***********************************************************************
*  EDIT "USEZIIP"                                                     *
***********************************************************************
         CLI   EXECZiip,C'Y'      YES ???
         JE    USEZIIPX
         CLI   EXECziip,C'N'      NO  ???
         JE    USEZIIPX
**
**  Set to 'A' in MR95 if it was 'N' and we jUST CPU stats running APF
**
         lghi  R14,ziip_parm_invalid
         BRU   RTNERROR
USEZIIPX EQU   *
*
***********************************************************************
*  EDIT "OVERFLOWON"                                                  *
***********************************************************************
         CLI   execovfl_on,C'Y'      YES ???
         je    editovfl_y
         CLI   execovfl_on,C'y'      YES ???
         je    editovfl_y
         CLI   execovfl_on,C'N'      NO  ???
         je    editovfl_n
         CLI   execovfl_on,C'n'      NO  ???
         je    editovfl_n
         lghi  R14,CALC_OVERFLOW_PARM_ERR
         BRU   RTNERROR
editovfl_y ds 0h
         lgfi  r15,x'0C000000'     set bits 36 and 37 for overflow mask
         st    r15,ovflmask        and save for later
         j     editovfl_end
editovfl_n ds 0h
         xr    r15,r15             set bits 36 and 37 for overflow off
         st    r15,ovflmask        and save for later
editovfl_end ds 0h
*
***********************************************************************
*  EDIT "PAGE_FIX_IO_BUFFERS"                                         *
***********************************************************************
         sysstate amode64=NO
         sam31
         testauth fctn=1      set r15 - not zero means not auth
         if (ltr,r15,r15,nz),and,   nz => not auth                     +
               (cli,execpagf,eq,C'Y')     and we want pagefixing
*          not authourized so set "allow paging" to Y
           mvi execpagf,c'N'       and set this to no

           logit msg=vb_blankl
*          la     r14,nopagefix     get the message number
*          jas r9,errformat         message to the report
           GVBMSG LOG,MSGNO=NO_PAGE_FIX,SUBNO=1,GENENV=GENENV,         +
               SUB1=(modname,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
*
*          mvi prntline,c' '        blank 1st char  (used to be CC)
*          lgf   r15,msg_bufl       get length of msg
*          la    r15,l'prntrdw(,r15) allow for RDW length
*          sth r15,prntrdwh         save length in RDW
*          logit ,
         endif
         sam64
         sysstate amode64=YES
*
***********************************************************************
*  Generate the day number for the Batchdate and create binary        *
*  formats for BATCHDATE, RUNDATE, and FICALDATE year/month/day       *
***********************************************************************
edit_bdate ds 0h
           mvc  cur_bdate,exec_bdate
           pack dblwork,cur_byear
           cvb  r0,dblwork
           st   r0,cur_bbyear
           pack dblwork,cur_bmth
           cvb  r0,dblwork
           st   r0,cur_bbmth
           pack dblwork,cur_bday
           cvb  r0,dblwork
           st   r0,cur_bbday
           la  r1,cur_bbdate         .Point at date to convert
           jas r9,conv_date_daynum      .Convert it
           st  r1,cur_bdaynum        .save day number
*
***********************************************************************
*  RETURN                                                             *
***********************************************************************
EDITEXIT lmg   R14,R12,SAVF4SAG64RS14 RESTORE REGISTERS
         BR    R14                    RETURN
*
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   L O A D   E N V I R O N M E N T   V A R I A B L E   T A B L E     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,savesubr
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
*
ENVVLOAD stmg  R14,R12,SAVF4SAG64RS14  SAVE   REGISTERS
*
***********************************************************************
*  OPEN ENVIRONMENT VARIABLE FILE                                     *
***********************************************************************
         LA   R0,targetqs_l      LOAD area length - dcb, etc
         GETMAIN R,LV=(0),LOC=BELOW GET MEMORY below the line
         LR   R2,R1
         using targetqs,r2
target   USING IHADCB,dcbqs

         larl r1,envvdcb
source         USING IHADCB,r1
         MVC  target.ihadcb(envvdcbl),source.ihadcb   copy dcb/dcbe

         LA   R0,dcbqs+envvDCBE-envvdcb
         ST   R0,target.DCBDCBE
*
         la   r0,exlqs          get address of target exl
         mvi  exlqs,exllaste+exldcbex    set flag
         stcm r0,b'0111',target.dcbexlsa and update the dcblist ptr
         drop target,source

         la   r0,openqs         get address of code area
         mvc  openqs,modopen24  copy in model code
         stcm r0,b'0111',exlqsopen   and set address in list

         larl r0,envv_dcb_open_exit real exit address
         oilh r0,x'8000'        turn on amode 31
         st   r0,exitqs31       and save

         la   r2,dcbqs          point r2 at dcb
         drop r2
         using ihadcb,r2

         if clc,namepgm,eq,=cl8'GVBMR95R'    R for reference
           mvc  dcbddnam,=cl8'REFRENVV'    default is EXTRRPT
         endif

         rptit msg=vb_blankl
         phead hd=envv            write to report file
*
         zap   workarea,P000     20813 set flag to test empty file
*
         MVC   WKREENT(8),OPENPARM      OPEN ENV VARIABLE FILE
         sysstate amode64=NO
         sam31
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,WKREENT)    ENV VAR FILE
         sam64
         sysstate amode64=YES
         TM    48(R2),X'10'       SUCCESSFUL ???
         BRNO  ENVVNONE           NO - NOTHING TO LOAD
*
         mvi   dblwork,0          set first byte of dblwork to zero    +
                                  this will be the reread flag
         LA    R9,gpenvva         INITIALIZE PREVIOUS ENTRY ADDRESS
*
***********************************************************************
*  READ "MR95" ENVIRONMENT VARIABLES UNTIL END-OF-FILE                *
***********************************************************************
ENVVLOOP DO    INF                LOAD VIEW   DEFN  INPUT FILE DCB ADDR
           do ,
             sam31
             sysstate amode64=NO
             GET (R2)             read the next record
             sysstate amode64=YES
             sam64
             if cli,dblwork,ne,0  Has reread been set?
               mvi   dblwork,0      Reset it
               iterate ,            and loop to reissue the get
             endif
             llgtr r1,r1
           enddo
*
           oi  dcboflgs,dcbofppc  set unlike attribute flag

           if  tm,dcbrecfm,dcbrecf,o   Fixed format?
             lgr R4,R1             SAVE RECORD ADDRESS
             lgr R5,R4             LOAD RECORD ENDING   ADDRESS
             lgh R15,DCBLRECL
             agr R5,r15
*     Mirror the input environment variables to report file
             bctr  r15,r0                set up for "EX"
             ex    r15,cardmove
             ahi   r15,5                 reset r15 to lrecl + RDW
             sth   r15,prntrdwh          save it
             rptit ,                     print parm card
           else ,                 Variable format
             lgh r15,0(,r1)        Get length of record
             la r4,4(,r1)         skip rdw as we set r4 to start
             lgr r5,r1             and set r5 to
             agr r5,r15             the end address (actually end+1)
*     Mirror the input environment variables to report file
             sth r15,prntrdwh     save length in our record
             ahi r15,-5           subtract RDW len + 1 for the "EX"
             ex    r15,cardmove
             rptit ,                     print parm card
           endif
*
           if CLI,0(R4),eq,C'*'   COMMENT ???
             iterate ,            YES -  BYPASS  RECORD
           endif
*
           LHI R0,ENVVTLEN        LOAD PARAMETER TABLE ENTRY SIZE
           AHI R0,8               ADD  EYEBALL   LEN
           GETMAIN RU,LV=(0),LOC=(ANY) GET   ENVIRON   VAR  TABLE ENTRY
           MVC 0(8,R1),ENVVEYEB   COPY EYEBALL
           LA  R3,l'envveyeb(,R1)          LOAD BASE  REGISTER
           USING ENVVTBL,R3
*
           XC  ENVVNEXT,ENVVNEXT  ZERO NEXT  POINTER
           XC  ENVVNLEN,ENVVNLEN  ZERO NAME  LENGTH
           MVC ENVVNAME,SPACES
           XC  ENVVVLEN,ENVVVLEN  ZERO VALUE LENGTH
           MVC ENVVVALU,SPACES
*
***********************************************************************
*    COPY ENVIRONMENT VARIABLE NAME AND VALUE INTO TABLE              *
***********************************************************************
           lgr R1,R4              LOAD STARTING SCAN  ADDRESS
*
           do  inf
             doexit cgr,R1,ge,R5, END-OF-RECORD ? - get out if too far +
               do=envvloop
*
             IF CLI,0(R1),Eq,C' ' CHECK TO SEE IF THERE ARE BLANKS
               LA R4,1(,R4)       BEFORE THE VALUE, IF YES INCREMENT
             else
              doexit CLI,0(R1),eq,c'=' VALUE DELIMITER ?? exit loop now
             ENDIF
*
             LA R1,1(,R1)         ADVANCE TO NEXT BYTE
           enddo ,                CONTINUE SEARCH FOR DELIMITER
*
           lgr R15,R1             COMPUTE  NAME LENGTH
           doexit sgr,R15,R4,NP    get out if null length
           if chi,r15,lt,54
             ex r15,errdmove      move in as error data
           endif

           if CHI,R15,gt,L'ENVVNAME  TRUNCATE  IF  TOO LONG
             lghi R15,L'ENVVNAME
           endif

           BCTR R15,0             DECREMENT LENGTH FOR "EX"
           STH R15,ENVVNLEN       SAVE NAME LENGTH (-1)
           EX  R15,ENVVMVNM       COPY NAME
           EX  R15,ENVVUCNM       UPPERCASE NAME
*
           LA  R4,1(,R1)          ADVANCE   BEYOND "="
           lgr R1,R5              LOAD END-OF-VALUE ADDR
*
           do  until=(cli,0(r1),ne,c' ')
             BCTR R1,0            BACKUP   ONE  BYTE
             doexit cgr,R1,lt,R4, BEGINNING OF  NAME   ???             +
               do=ENVVloop         YES -  VALUE  IS MISSING
           enddo                  CONTINUE BACKSCAN
*
           LA  R1,1(,R1)          INCLUDE  LAST BYTE
*
           lgr R15,R1             COMPUTE VALUE LENGTH
           doexit (sgr,R15,R4,NP),or,   too short or                   +
               (chi,r15,gt,l'envvvalu), too long?                      +
               do=ENVVloop         YES -  VALUE  IS MISSING
           BCTR R15,0             DECREMENT LENGTH FOR "EX"
           STH R15,ENVVVLEN       SAVE VALU LENGTH (-1)
           EX  R15,ENVVMVVU       COPY VALUE
           EX  R15,ENVVUCVU       UPPERCASE VALUE
*
           ap  workarea,P001   20813 indicator to test empty file
*
           ST  R3,ENVVNEXT-ENVVTBL(,R9)    CHAIN TO PREVIOUS ENTRY
           lgr R9,R3              ADVANCE  PREVIOUS ENTRY
*
         enddo ,                  envvloop, go and  read next record
*
***********************************************************************
*  ERROR PARSING ENVIRONMENT VARIABLE STRING                          *
***********************************************************************
ENVVERR  lghi  R14,ENV_VAR_ERR
*        MVC   ERRDATA,0(R4)
         BRU   RTNERROR
ENVVnone ds    0h
         rptit msg=nonemsg                 no data so tell them
         b     ENVVEXIT
*
**********************************************************************
*                                                                    *
*        Initialise any I/O modules                                  *
*        -  Load DB2 modules                                         *
*                                                                    *
**********************************************************************
INIT_IO  DS    0h
*
         IF TM,WORKFLAG1,DB2_USED,o
***********************************************************************
*    LOAD DB2 INTERFACE PROGRAMS                                      *
***********************************************************************
           LOAD EP=DSNALI         Load DB2 ASSEMBLER INTERFACE
           ST  R0,DSNALI
*
           LOAD EP=DSNHLI2        LOAD DB2 HIGHER LEVEL LANG ADDRESS
           ST  R0,DSNHLI2
         endif
*
         br    R14
**********************************************************************
*                                                                    *
*        This is the DCB OPEN exit - given control during OPEN       *
*        processing                                                  *
*        See DFSMS Using Data Sets - Chapter 31 for details          *
*                                                                    *
*        Registers at entry                                          *
*           0                                                        *
*           1      - low order 3 bytes --> DCB                       *
*           2-13   - Contents unchanged from execution of OPEN       *
*          14      - Return address                                  *
*          15      - Exit entry address                              *
*                                                                    *
*        Notes - No need to preserve 2-13                            *
*              - Must NOT use the savearea addressed by R13          *
*              - always entered in 24 bit amode                      *
*        No return codes required                                    *
*                                                                    *
**********************************************************************
envv_dcb_open_exit ds 0h
         push  using
         drop  r2
         la    r1,0(,r1)          clean r1 address
         using ihadcb,r1            and address the DCB
*
         mvi   dblwork,x'ff'      set dblwork non-zero - this is our   +
                                  reread switch
         oi    dcboflgs,dcbofppc  set unlike attribute flag

         bsm   0,r14
         pop   using
*
static   loctr
ENVVMVNM MVC   ENVVNAME(0),0(R4)  * * * * E X E C U T E D * * * *
ENVVUCNM OC    ENVVNAME(0),SPACES * * * * E X E C U T E D * * * *
ENVVMVVU MVC   ENVVVALU(0),0(R4)  * * * * E X E C U T E D * * * *
ENVVUCVU OC    ENVVVALU(0),SPACES * * * * E X E C U T E D * * * *
code     loctr
*
***********************************************************************
*  END-OF-FILE FOR ENVIRONMENT VARIABLE TABLE                         *
***********************************************************************
ENVVEOF  ds    0h
         sysstate amode64=NO
         sam31
*
         if (cp,workarea,lt,P001)  no records in ENVV file
           j envvnone              RTC20813
         endif
*
         CLOSE ((R2)),MODE=31,MF=(E,WKREENT)
         sam64
         sysstate amode64=YES
*
***********************************************************************
*  RETURN                                                             *
***********************************************************************
ENVVEXIT lmg   R14,R12,SAVF4SAG64RS14 RESTORE REGISTERS
         BR    R14                    RETURN
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        L O A D   T H E   V I E W   D E F N   P A R A M E T E R S    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,savesubr
         using genenv,env_area
         using msglist,msg_area
*
* This macro updates a pointer to the next VDP record of a similar
* type, and sets the address of the first record of a particular type,
* if it has not been set.
*
* There is also a _NEXT_PTR field in the in-memory VDP elements, that
* chains all of the VDP elements in the table. This is not set by this
* macro. This pointer is required if we want to traverse all the VDP
* elements, as they can be located in non contiguous segments.
*
         macro
         ptrupd &rg1=?,&rg2=?,&lb1=?,&rc1=?,&rc2=?,&rn=?
&lb1     using &rc1._&rc2._record,&rg1
         if ltg,&rg1,curr_&rn,nz
           stg &rg2,&lb1..&rc1._next
           stg &rg2,curr_&rn
         else
           stg &rg2,first_&rn
           stg &rg2,curr_&rn
         endif
         drop  &lb1
         mend

vtype_summary equ 1        These types mean a view will
vtype_detail  equ 2         have a format phase
VDPLOAD  stmg  R14,R12,SAVF4SAG64RS14  SAVE  REGISTERS
*
***********************************************************************
*  OPEN LOGIC TABLE FILE  (BEFORE LOADING VDP)                        *
***********************************************************************
         LA   R0,ltblDCBL        LOAD DCB  LENGTH
         GETMAIN R,LV=(0),LOC=BELOW       GET  MEMORY  FOR DCB
         LR   R2,R1
         USING IHADCB,R2

         larl r15,ltbldcb
         MVC  ihadcb(ltblDCBL),0(r15)   copy dcb

         LA   R1,ltblDCBE-ltbldcb(,R2)
         ST   R1,DCBDCBE

         sty   r2,ltbldcba       save address

         if clc,namepgm,eq,=cl8'GVBMR95R'    R for reference
           mvc  dcbddnam,=cl8'REFRLTBL'    default is EXTRLTBL
         endif
*
         MVC   WKREENT(8),OPENPARM      OPEN LOGIC TABLE FILE
         sysstate amode64=NO
         sam31
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,WKREENT) LOGIC TABLE FILE
         if TM,dcboflgs,dcbofopn,no     UnSUCCESSFUL ???
           sam64
           lghi R14,OPEN_LTBL_FAIL LOAD ERROR MESSAGE NUMBER
           MVC  ERRDATA(8),DCBDDNAM
           J   RTNERROR           PRINT ERROR  MESSAGE - STOP
         endif
         mviy  format_phase,c'N'  no format phase by default
*
***********************************************************************
*  OPEN VIEW DEFINITION PARAMETER FILE (VDP)                          *
***********************************************************************
         LA   R0,vdpDCBL         LOAD DCB  LENGTH
         GETMAIN R,LV=(0),LOC=BELOW       GET  MEMORY  FOR DCB
         LR   R2,R1
         USING IHADCB,R2

         larl r15,vdpdcb
         MVC  ihadcb(vdpDCBL),0(r15)    copy dcb

         LA   R1,vdpDCBE-vdpdcb(,R2)
         ST   R1,DCBDCBE

         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,WKREENT) VIEW DEFN FILE
         sam64
         sysstate amode64=YES
         if TM,dcboflgs,dcbofopn,no     UnSUCCESSFUL ???
           lghi R14,OPEN_VDP_FAIL LOAD ERROR MESSAGE NUMBER
           MVC  ERRDATA(8),DCBDDNAM
           J   RTNERROR           PRINT ERROR  MESSAGE - STOP
         endif
*
         XC    PREVVDPA,PREVVDPA  ZERO PREVIOUS "VDP" RECORD ADDRESS
*        GVBMRDI uses WKREENT so it cannot be used for anything
*        else between the 1st and last call to GVBMRDI.
         xc    WKREENT,WKREENT    Clean this area as it is required
*                                  to be clean for use by GVBMRDI
         xc    maxextf#,maxextf#  set this counter to zero
*
         sysstate amode64=NO      Read ahead first VDP record
         sam31
         get (r2)
         sam64
         sysstate amode64=YES
         asi vdpcnt_real,1        records in VDP dsn
         llgtr R3,R1              LOAD RECORD ADDRESS (INCL RDW)
*
***********************************************************************
*  READ VIEW DEFN PARAMETERS UNTIL END-OF-FILE                        *
***********************************************************************
Vdploop  do    inf
           USING VDP0200_FILE_RECORD,R3
                        EJECT
***********************************************************************
*  SELECT SPECIFIC RECORD TYPES                                       *
***********************************************************************
           LH  R0,VDP0200_RECORD_TYPE
*
           select chi,r0,eq
             when vdpheadr        VDP  GENERATION HEADER RECORD ???
               push  using
               USING VDP0001_GENERATION_RECORD,R3
*
               lgh   R0,VDP0001_VERSION_INFO
               if cghi,R0,ne,VDPVERS      Expected version?
                 CVD  R0,DBLWORK
                 OI   DBLWORK+L'DBLWORK-1,X'0F'
                 UNPK ERRDATA(8),DBLWORK
                 lghi R14,VDP_VER_ERR     LOAD ERROR MESSAGE NUMBER
                 J RTNERROR
               endif
*
               lgf   r4,vdp0001_vdp_byte_count view defn buffer size
               AGHI  R4,2*VDPCUSHN        add some margin and ensure
               ST    R4,VDPSIZE           total vdpsize > 8K
               STY   R4,VDP_seg_len       save size of this segment
*
               LA    R0,l'vdpeyeb(R4)     add eyeball length
               STORAGE OBTAIN,LENGTH=(0),LOC=(ANY),BNDRY=PAGE
*
               MVC   0(l'vdpeyeb,R1),VDPEYEB    COPY EYEBALL
               LA    R7,l'vdpeyeb(,R1)
               ST    R7,VDPBEGIN        Save table address
               STY   R7,vdp_addr_curr_seg Current segment address
               AR    R4,R7              End of table address
*
               llc R15,VDP0001_MAX_DECIMAL_PLACES
               if chi,r15,eq,3     large number mode ?  (20,3)
                 MVI LRGNUM,C'Y'   YES - SET LARGE NUMBER INDICATOR
                 la r14,=ld'1.000' address the 3 DP quantum
               else
                 MVI LRGNUM,C'N'   ASSUME         (15,8)  MODE
                 la r14,=ld'1.00000000' address the 8 DP quantum
               endif
               st r14,dfp_quantum_ptr save the dfp_qanturm pointer
*              MR95 will copy the quantum into Fp9/fp11

               llgt R14,EXECDADR
               USING EXECDATA,R14
               if clc,exec_rdate(3),eq,VDP
                 mvc exec_rdate,vdp0001_run_date .use correct run date
               else
                 mvc vdp0001_run_date,exec_rdate .use correct run date
               endif
               mvc  cur_rdate,exec_rdate
               pack dblwork,cur_ryear    .Convert character year to
               cvb  r14,dblwork          .  numeric
               st   r14,cur_bryear       .Save in binary form
               pack dblwork,cur_rmth     .Convert character month to
               cvb  r14,dblwork          .  numeric
               st   r14,cur_brmth        .Save in binary form
               pack dblwork,cur_rday     .Convert character day to
               cvb  r14,dblwork          .  numeric
               st   r14,cur_brday        .Save in binary form
*
*          Convert date to a day number
               la  r1,cur_bryear         .Point at date to convert
               jas r9,conv_date_daynum      .Convert it
               st  r1,cur_rdaynum        .save day number
*
*          IRUN report fields
               lay r1,vdp_date
t1             using vdp_date,r1
               mvc t1.vdp_date,vdp0001_date
               mvc t1.vdp_time,vdp0001_time
               mvc t1.vdp_desc,vdp0001_description
               drop t1

               pop using

             when (vdpexpgm,vdplr,vdpcolm,vdpsort,vdpjrfil,vdpxtrct,   +
               vdpcntrl)    records of interest

             when vdpview
view           using vdp1000_view_record,r3
               l  r14,view.vdp1000_view_type   .Get view type
               if chi,r14,eq,vtype_summary,or,chi,r14,eq,vtype_detail
                 mviy format_phase,c'Y'   then signal format phase
               endif
               drop view
               LH R14,VIEWCNT     INCREMENT  VIEW COUNT
               AHI R14,1
               STH R14,VIEWCNT
             when (vdpfilei,vdpfilep,vdpfiled,vdpfileo)
               USING VDP0200B_FILE_RECORD,R7
               xc  VDP0200B_ERROR,VDP0200B_ERROR clear error indicator
               if clc,vdp0200_alloc_file_type,eq,=a(pipedev) pipe
                 llgt  R15,EXECDADR
                 USING EXECDATA,R15
                 if cli,EXECSNGL,eq,C'1',or,cli,EXECSNGL,eq,C'A'
                   if TM,WORKFLAG1,MSG811DONE,Z
                     logit msg=vb_blankl
                     GVBMSG LOG,MSGNO=PIPE_WARN,SUBNO=1,GENENV=GENENV, +
               SUB1=(modname,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)

*                    la     r14,msg#811       get the message number
*                    jas r9,errformat         message to the report
*                    mvi prntline,c' '        blank 1st char
*                    lgf   r15,msg_bufl       get length of msg
*                    la    r15,l'prntrdw(,r15) allow for RDW length
*                    sth r15,prntrdwh         save length in RDW
*                    logit ,
                     oi  WORKFLAG1,MSG811DONE
                   endif
                 endif
                 drop r15 execdata
               endif
*
               xc VDP0200_server_id,vdp0200_server_id clear server_id  +
                                   for everyone
               if clc,vdp0200_alloc_file_type,eq,=a(tokendev) token
                 asi tokncnt,1     increment token count
                 mvc vdp0200_server_id,tokncnt
               endif
               LH R15,MAXEXTF#     INCREMENT NO. OF EXTRACT FILES
               AHI R15,1
               STH R15,MAXEXTF#
               if chi,r0,eq,vdpfilei Input file
                 if cli,vdp0200_access_method_id+3,eq,DB2SQL
                     OI  WORKFLAG1,DB2_USED  take note for later
                 endif

                 do label=bracket
                   if cli,vdp0200_access_method_id+3,eq,DB2VSAM
                     OI  WORKFLAG1,DB2_USED  take note for later
*
*   Check access method DB2 via VSAM is available, and initialise
*
                     if (ltgf,r15,MRDIADDR,nz)
                       la     r0,1            function = section 1
                       bassm  r14,r15
*
                       if (ltr,r15,r15,nz)
*                        Error, but just remember it to check later
*                        unless it's serious (12)
                         ST   R0,VDP0200B_ERROR Save msg/reason code
                         MVC  VDP0200B_ERRDATA,ERRDATA save err info
                         lghi r14,DB2_ERR
                         CHI  R15,12          VDP error
                         Jnl  RTNERROR        return
                       endif
                     else
*                      DB2 via VSAM not available
*                      Just remember for later
                       lghi R14,DB2_VSAM_UNAVAILABLE ERROR message
                       ST   R14,VDP0200B_ERROR Save error num
                     endif
*
                   endif
                 enddo
*
               else
               endif
             othrwise ,
               sysstate amode64=NO    Just move onto next VDP record
               sam31
               get (r2)
               sam64
               sysstate amode64=YES
               asi vdpcnt_real,1      records in VDP dsn
               llgtr R3,R1            LOAD RECORD ADDRESS (INCL RDW)
*
               iterate ,                    everything else ignore
           endsel
*
*              now copy the input record (r3 base) to the
*              VDP storage area (r7 base)

           lh r0,vdp0200_record_type
           If chi,r0,eq,vdpfilei,or,                                   +
               chi,r0,eq,vdpfilep,or,                                  +
               chi,r0,eq,vdpfiled,or,                                  +
               chi,r0,eq,vdpfileo
             mvc vdp0200b_view_id(vdp0200_file-vdp0200_view_id),vdp0200+
               _view_id
             mvc vdp0200b_ddname_output,vdp0200_ddname_output
             mvc vdp0200b_server_id,vdp0200_server_id
             mvc vdp0200b_alloc_file_type,vdp0200_alloc_file_type
             mvc vdp0200b_ddname_input,vdp0200_ddname_input
             mvc vdp0200b_alloc_vol_ser,vdp0200_alloc_vol_ser
             mvc vdp0200b_alloc_blksize,vdp0200_alloc_blksize
             mvc vdp0200b_access_method_id,vdp0200_access_method_id
             mvc vdp0200b_minimum_length,vdp0200_minimum_length
             mvc vdp0200b_maximum_length,vdp0200_maximum_length
             mvc vdp0200b_alloc_recfm,vdp0200_alloc_recfm
             mvc vdp0200b_alloc_lrecl,vdp0200_alloc_lrecl
             xc      vdp0200b_mult_part_chain,vdp0200b_mult_part_chain
             xc      vdp0200b_file_reader,vdp0200b_file_reader
             xc      vdp0200b_extfile_addr,vdp0200b_extfile_addr
             mvc vdp0200b_input_exit_pgm_id,vdp0200_input_exit_pgm_id
             mvc vdp0200b_input_exit_startup_parm,vdp0200_input_exit_st+
               artup_parm
             mvc vdp0200b_file,vdp0200_file
             mvc vdp0200b_dsn,vdp0200_dsn
             mvc vdp0200b_dbms_subsys,vdp0200_dbms_subsys
   mvc vdp0200b_dbms_row_fmt_opt_id,vdp0200_dbms_row_fmt_opt_id
   mvc vdp0200b_dbms_include_null_ind,vdp0200_dbms_include_null_ind
             lghi r1,l'vdp0200_dbms_sql
             lgr  r15,r1
             la   r0,vdp0200b_dbms_sql
             la   r14,vdp0200_dbms_sql
             MVCL R0,r14
             la   r1,vdp0200b_end-vdp0200b_rec_len
             sth  r1,vdp0200b_rec_len
*
*            if DB2 via VSAM copy the first DSname prev retrieved
             if cli,vdp0200b_access_method_id+3,eq,DB2VSAM
               if (ltgf,r15,MRDIADDR,nz)
                 la       r0,2            function = section 2
                 bassm    r14,r15
               else
*                DB2 via VSAM not available - remember for later
                 lghi R14,DB2_VSAM_UNAVAILABLE ERROR message
                 ST   R14,VDP0200B_ERROR Save error num
               endif
             endif
           else
             la      R0,vdp0200_View_id   copy record from view id
             LH R1,VDP0200_REC_LEN
             slgfi r1,(l'VDP0200B_REC_LEN+l'VDP0200B_RDW_FLAGS)
             la      R14,vdp0200b_view_id
             LR R15,R1
             MVCL R14,R0
             lgh r15,VDP0200_REC_LEN          Get the record length
             aghi r15,l'VDP0200B_NEXT         Allow for extra field
             aghi r15,l'VDP0200B_NEXT_PTR Allow for extra field
             sth      r15,VDP0200b_REC_LEN     and save it
           endif
           asi VDPCOUNT,1            INCREMENT VDP RECORD COUNT
           drop r3
*          switch to using the instorage copy - so updates
*          are only done to our copy
*
           LH R0,vdp0200b_RECORD_TYPE
*
           select chi,r0,eq

             when vdpcntrl            Control record (50)
               push      using
               USING vdp0050b_CONTROL_RECORD,r7
               using Fiscal_date_table,r14
               llgt r15,execdadr
               using execdata,r15
end_50         do ,
                 if ltgf,r14,fisdates,nz any fiscal dates
                   do until=(ltgf,r14,fiscal_next,np) lp until end
                     if clc,fiscal_recid,eq,vdp0050b_record_id
                       if clc,exec_fdate(3),ne,VDP
                         mvc vdp0050b_fiscal_date,fiscal_date
                       endif
                       leave end_50 found what we wanted so get out
                     endif
                   enddo
                   if clc,exec_fdate(3),ne,VDP
                     mvc vdp0050b_fiscal_date,exec_fdate parm rdate
                   endif
                 else
                   if clc,exec_fdate(3),ne,VDP
                     mvc vdp0050b_fiscal_date,exec_fdate parm rdate
                   endif
                 endif
               enddo

               drop r14
               drop r15
               pop using

           ptrupd rg1=r15,rg2=r7,lb1=vl,rc1=vdp0050b,rc2=control,rn=50

             when vdpexpgm            210 Exit program record
               ptrupd rg1=r15,rg2=r7,lb1=vl,rc1=vdp0210b,rc2=exit_pgm,r+
               n=210

             when vdplr               300 Logical record record
               ptrupd rg1=r15,rg2=r7,lb1=vl,rc1=vdp0300b,rc2=lr,rn=300 +

             when vdpjrfil            650 join refernce file details
               sty r7,vdp0650a        Store record addr in thread main

             when vdpxtrct            801 Extract Files record
               push      using
               USING vdp0801b_extract_files_record,r7
               sty r7,vdp0801a        Store record addr in thread main
               lh r0,vdp0801b_extrfile_count
               sth r0,maxstdf#
               ah      r0,maxextf#
               sth r0,maxextf#
               pop using
*

             when vdpview             VIEW RECORD (1000)
               push      using
               USING vdp1000b_view_RECORD,r7
              if lt,r1,vdp1000b_control_id,nz Get control record id
                bras r9,Loccid                Go find it
                st r15,vdp1000b_control_id Replace with rec address
              endif
               xc vdp1000b_use_count,vdp1000b_use_count
               pop using
               ptrupd rg1=r15,rg2=r7,lb1=vl,rc1=vdp1000b,rc2=view,rn=10+
               00
*
             when (vdpfilei,vdpfilep,vdpfileo)
               if ltgf,R14,PREVVDPA,P WAS PREVIOUS RECORD valid
prev             USING vdp0200b_FILE_RECORD,R14
*
                 LH R0,prev.vdp0200b_RECORD_TYPE
                 if CHI,R0,eq,vdPFILEI          LOGICAL FILE ???
***********************************************************************
*  CHAIN MULTIPLE PARTITIONS OF THE SAME EVENT FILE TOGETHER          *
***********************************************************************
                   L R15,vdp0200b_RECORD_ID
                   if C,R15,eq,prev.vdp0200b_RECORD_ID
                     st R7,prev.vdp0200b_MULT_PART_CHAIN
                   endif
                 endif
                 drop prev
               endif
*              chain next VDP0200
               ptrupd rg1=r15,rg2=r7,lb1=vl,rc1=vdp0200b,rc2=file,rn=20+
               0
*
* Complete VDP0200 processing for DB2 via VSAM
*
               if cli,vdp0200b_access_method_id+3,eq,DB2VSAM
*                DB2 via VSAM process multiple DSNs
                 if (ltgf,r15,MRDIADDR,nz)
                   la r0,3            function = section 3
                   bassm r14,r15
*                  returns current in storage VDP pointer in R7 - it
*                  might have changed
                   stg r7,curr_200
*                  DB2 via VSAM clean up storage
                   lgf   r15,MRDIADDR
                   la    r0,4         function = section 4
                   bassm r14,r15
                 else
*                  DB2 via VSAM not available
                   lghi R14,DB2_VSAM_UNAVAILABLE ERROR message
*                  J RTNERROR           return
                 endif
               endif
*
           endsel
*
           st R7,PREVVDPA      SAVE PREVIOUS VDP ADDR HERE, in case EOF

           sysstate amode64=NO    Read next VDP record incase EOF
           sam31
           get (r2)
           sam64
           sysstate amode64=YES
           asi vdpcnt_real,1      records in VDP dsn
           llgtr R3,R1            LOAD RECORD ADDRESS (INCL RDW)

           lgr r1,r7           keep current vdp address
           lgh R0,vdp0200b_REC_LEN this record length
           agr R7,r0               advance to next row

           llgt R0,vdp_addr_curr_seg    The
           agf R0,VDP_seg_len            space
           sgr R0,R7                      remaining
           if CGHI,R0,lt,VDPCUSHN       Are we likely to run out?
             LGHI  R4,4*VDPCUSHN        another 32K
             STY   R4,VDP_seg_len       keep total
             Agf   R4,VDPSIZE           add existing amount
             ST    R4,VDPSIZE           keep running total

*
             STORAGE OBTAIN,LENGTH=(4),LOC=(ANY),BNDRY=PAGE
*
             LGR   R7,R1                Next VDP slot
             STY   R7,vdp_addr_curr_seg Current segment address
             asi   vdp_seg_cnt,1        Segment count
           endif

*          do chaining using common start of element

           push  using
           USING vdp0050b_CONTROL_RECORD,r1
           llgt  R1,PREVVDPA      PREVIOUS VDP ADDR
           Stg   R7,vdp0050b_NEXT_PTR
           pop   using
*

         enddo ,   vdploop end
         lghi  R14,DB2_ERR        LOAD  ERROR MESSAGE NUMBER
         J     RTNERROR           PRINT ERROR MESSAGE - STOP

                        SPACE 3
***********************************************************************
*  CLOSE VIEW DEFN PARAMETER FILE (VDP)                               *
***********************************************************************
VDPEOF   llgt  R7,PREVVDPA        LOAD    PREVIOUS  "VDP" RECORD  ADDR

         if    ltgr,r7,r7,np       not positive so
           llgt R7,vdp_addr_curr_seg get the beginning of seg addr
         else
           XC   vdp0200b_NEXT_PTR,vdp0200b_NEXT_PTR LAST ELEMENT !!
           lgh  R0,vdp0200b_REC_LEN ADVANCE TO END OF "VDP"
           agr  R7,r0               ADVANCE TO END OF "VDP"
           aghi R7,7                ROUND   TO NEAREST DOUBLEWORD
           nill r7,x'fff8'
         endif

*        free what's unused

         llgt  R1,vdp_addr_curr_seg COMPUTE REMAINING TABLE SPACE
         agf   R1,VDP_seg_len
         lgr   R0,R1
         sgr   R0,R7              This amount is unused
         sgr   R1,R0              First byte of unused space
*
         Llgf  R7,VDPSIZE         Adjust  ACTUAL    SIZE
         Sgr   R7,R0              with the amount we're going to free
         ST    R7,VDPSIZE
         ltgr  R0,R0              Do we have an exact fit ?
         jnp   VDPEOF02             don't release
*                                 but what if we find this negative ?
*
         STORAGE RELEASE,ADDR=(1),LENGTH=(0) release unused space
VDPEOF02 DS    0H
*
         MVC   WKREENT(8),OPENPARM      OPEN VIEW DEFN LIST
         sysstate amode64=NO
         sam31
         CLOSE ((R2)),MODE=31,MF=(E,WKREENT)  CLOSE  VIEW DEFN FILE
         sam64
         sysstate amode64=YES

         if lt,r9,vdp0801a,z      do we have a valid 801 record?
           lghi  r14,MISSING_VDP0801 no - issue message and exit
           j     rtnerror
         endif

         L     R0,VDPCOUNT        MINIMUM   NO.  OF  ROWS
         CHI   R0,1
         JNL   VDPEXIT            YES - CONTINUE
*
***********************************************************************
*  CHECK VERSION OF LOGIC TABLE IF VDP EMPTY                          *
***********************************************************************
         ly    r2,ltbldcba        LOAD  LOGIC TABLE DCB   ADDRESS
         USING IHADCB,R2
*
         lghi  R14,VDP_EMPTY      ASSUME  VDP EMPTY ERROR
         TM    DCBRECFM,X'80'     FIXED/UNDEFINED  FORMAT LOGIC TBL ???
         JNO   RTNERROR           NO  - EMPTY VDP ONLY OK IF VERS3
*
VDPEXIT  lmg   R14,R12,SAVF4SAG64RS14   RESTORE REGISTERS
         BR    R14                      RETURN
*
*
         DROP  R2
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        L O A D   T H E   L O G I C   T A B L E                      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,savesubr
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
         using msglist,msg_area
*
LTBLLOAD stmg  R14,R12,SAVF4SAG64RS14  SAVE   REGISTERS
*
         ly    r2,ltbldcba        LOAD  LOGIC TABLE DCB   ADDRESS
         USING IHADCB,R2
*
         USING LOGICTBL,R7
*
         XC    LTCOUNT,LTCOUNT    RESET  ROW  COUNT (ACTUAL)
         XC    VIEWCNT,VIEWCNT    RESET  VIEW COUNT
                        SPACE 3
***********************************************************************
*  READ LOGIC TABLE UNTIL END-OF-FILE                                 *
***********************************************************************
LTBLLOOP do    inf
           sysstate amode64=NO
           sam31
           get (r2)               READ NEXT   ROW
           sam64
           sysstate amode64=YES
*
           if TM,DCBRECFM,X'80',O FIXED/UNDEFINED  FORMAT RECORDS  ???
             MVC  ERRDATA(8),DCBDDNAM
             LHI R14,LTBL_RECFM_BAD
             J   rtnERROR
           else
*
ltblv4       do ,                 bracketing do
               LA R3,4(,R1)       LOAD RECORD ADDRESS (SKIP RDW)
               USING LTHD_REC,R3
               if clc,=c'GEN ',eq,lthd_function_code  GEN record?
ltgen            using ltgn_rec,r3
*
                 llgt R14,EXECDADR
                 USING EXECDATA,R14
                 if CLI,EXEC_check_timestamp,ne,C'N'
                 drop r14
*                Check the timestamp matches the VDP timestamp
*                They are in different formats so need to copy VDP
*                timestamp to workarea in matching format
*
                 LAY  R1,WORKAREA
                 USING LTGN_DATE,R1
                 LAY  R15,VDP_DATE    VDP date/time saved in thrdarea
                 USING VDP_DATE,R15
                 MVC  ltgn_date_cc,VDP_DATE_CC
                 MVC  ltgn_date_yy,VDP_DATE_YY
                 MVC  ltgn_date_mm,VDP_DATE_MM
                 MVC  ltgn_date_dd,VDP_DATE_DD
                 MVC  ltgn_time_hh,VDP_TIME_HH
                 MVC  ltgn_time_mm,VDP_TIME_MM
                 MVC  ltgn_time_ss,VDP_TIME_SS
                 DROP R1,r15
*
                 if CLC,WORKAREA(14),ne,ltgen.LTGN_DATE
                   lghi R14,VDP_XLT_TIMESTAMP_ERR
                   LA   R1,14
                   ST   R1,MSGS2LEN      set length
                   LAY  R1,WORKAREA
                   ST   r1,MSGS2PTR      save the address of the data
                   LA   R1,14
                   ST   R1,MSGS3LEN      set length
                   LA   R1,ltgen.LTGN_DATE
                   ST   r1,MSGS3PTR      save the address of the data
                   XC   MSGS4PTR,MSGS4PTR
                   J    RTNERROR
                 endif
                 endif
                 if cli,ltgen.ltgn_extract,eq,x'01'
                   mviy extract_phase,c'Y'
                 else
                   mviy extract_phase,c'N'
                   lh r1,maxstdf#         Get number of std extract
                   lh r15,maxextf#        Get max extract files
                   sr r15,r1              ref phase so no std extract
                   xc maxstdf#,maxstdf#   zero it then
                   sth r15,maxextf#       save changed max
                 endif

                 BRAS  R9,ALLOEXTF        ALLOCATE  EXTRACT  FILE AREA

                 lgf   r1,ltgen.LTGN_reccnt get total num of recs
                 mhi   r1,LTRE_LEN         Work out size needed
*                  Allow for this many RExx and ES clones
                 afi   r1,maxclons*(LTRE_LEN+LTES_LEN)

                 st    r1,ltsize
                 la    r0,l'ltbleyeb(,r1)  Add eyeball length
                 GETMAIN RU,LV=(0),LOC=(ANY),BNDRY=PAGE get memory
                 MVC   0(l'ltbleyeb,R1),ltbleyeb Copy eyeball
                 la    r1,l'ltbleyeb(,r1) skip over eyeball
                 st    r1,ltbegin         save table  address
                 lgr   r7,r1              set logic table base
*
                 lgf   r1,ltgen.LTGN_reccnt Get max log table row cnt
                 ahi   r1,maxclons*2     Allow for clone REx and ES
                 sll   r1,2               multiply by four
                 la    r0,l'rowteyeb(,r1) add eyeball length
                 GETMAIN RU,LV=(0),LOC=(ANY) allocate row address tbl
                 mvc   0(l'rowteyeb,r1),rowteyeb copy eyeball
                 la    r1,l'rowteyeb(,r1) skip over eyeball
                 st    r1,ltrowadr        save row addr table address
                 mvc ltfunc,lthd_function_code function code
*
*      get area for our "NV" number and name list
*
                 lgf   r0,ltgen.LTGN_NV_CNT Get number of "NV"s
*      Do not save count here as some NVs could be dupliate views with
*      different file ids. Count later as we add to the view table.
                 xc    view_cnt,view_cnt  clear
                 mhi   r0,view_table_ent_len
                 ahi   r0,l'vtbleyeb      add eyeball length
                 GETMAIN RU,LV=(0),LOC=(ANY) allocate row address tbl
                 mvc   0(l'vtbleyeb,r1),vtbleyeb copy eyeball
                 la    r1,l'vtbleyeb(,r1) skip over eyeball
                 sty   r1,viewtbl_b       save view table address
                 sty   r1,viewtbl_c       save next free entry in table

                 lay   r1,logictbl_date
l1               using logictbl_date,r1
                 mvc   l1.logictbl_date,ltgen.ltgn_date save date
                 mvc   l1.logictbl_time,ltgen.ltgn_time save time
                 mvc   l1.logictbl_desc,ltgen.ltgn_desc save desc
                 drop  ltgen,l1

               else

                 mvc ltfunc,lthd_function_code function code

***********************************************************************
*    SAVE LOGIC TABLE ROW ADDRESS IN ROW ADDRESS TABLE                *
***********************************************************************
                 l  R14,LTCOUNT CONVERT ROW NUMBER/COUNT (-1) TO ADDR
                 SLL R14,2
                 A R14,LTROWADR
                 ST R7,0(,R14)
*
***********************************************************************
*      CHECK FOR CONSECUTIVE LOGIC TABLE ROW NUMBERS                  *
***********************************************************************
                 L R0,LTCOUNT
                 AHI R0,1
                 if C,r0,ne,LTHD_ROW_NBR
                   lghi R14,LTBL_OUT_OF_ORDER
                   CVD R0,DBLWORK
                   OI DBLWORK+L'DBLWORK-1,X'0F'
                   UNPK ERRDATA(8),DBLWORK
                   J RTNERROR
                 endif
*
***********************************************************************
*    SAVE CURRENT DRIVER FILE ID                                      *
***********************************************************************
                 MVC DRIVDDN,HEXFF UPDATE CURRENT DRIVER FILE
                 MVC DRIVFILE,LTHD_FILE_ID DDNAME/ID
*
***********************************************************************
*      INITIALIZE COMMON LOGIC TABLE FIELDS                           *
***********************************************************************
                 XC LTFLAGS,LTFLAGS ZERO PROCESSING   FLAGS
                 XC LTCODSEG,LTCODSEG ZERO GENERATED CODE SEGMENT ADDR
                 XC LTFUNTBL,LTFUNTBL ZERO MODEL CODE TABLE     ENTRY
                 XC LTLBADDR,LTLBADDR ZERO LOOK-UP RECORD BUFFER ADDR
                 XC LTGENLEN,LTGENLEN ZERO GENERATED  LENGTH
*
                 MVC LTROWNO,LTHD_ROW_NBR   ROW       NUMBER
                 MVC LTVIEW#,LTHD_VIEW_ID   VIEW      NUMBER
                 MVC LTseqno,LTHD_Suffix_SEQ_NBR sequence/column num
*
*     the next part fixes up a record type for LKE and LKL,
*     checking for type 4 and making it type 5
*
                 if CLC,LTFUNC,eq,LK_E,or, is it "LKE"?                +
               CLC,LTFUNC,eq,LK_L,andif,       or "LKL"                +
               CLI,LTHD_RECORD_TYPE+3,eq,fc_rtyp04 and type 4?
                   MVI LTHD_RECORD_TYPE+3,fc_rtyp05 make it type 5
                 endif
*
                 MVC ltlognv,CURRNV COPY CURRENT VIEW BEGINNING ROWa
*
***********************************************************************
*     MAKE "GO TO" ROW NUMBERS RELATIVE TO BEG OF TABLE NOT BEG OF FILE
***********************************************************************
                 if LT,R0,LTHD_GOTO_ROW1,np Test the true row value ?
                   L R0,LTROWNO   Not available - ASSUME   NEXT ROW
                   AHI R0,1
                 endif
*
                 ST R0,LTTRUE
*
                 if LT,R0,LTHD_GOTO_ROW2,np Test the false row value ?
                   L R0,LTROWNO   Not available - ASSUME   NEXT ROW
                   AHI R0,1
                 endif
*
                 ST R0,LTFALSE

               endif
***********************************************************************
*    Find matching major function table entry                         *
***********************************************************************
               llgt R5,majfcnta   LOAD ADDR OF major functions count
               LH R0,0(,R5)        and get the count into r0
               llgt R6,majfADDR   LOAD ADDR OF major function table
               USING major_func_table,r6
*
ltouter        do ,
                 do from=(r0)

                   ic r14,major_len
                   doexit ex,r14,majorfunc_clc,eq,do=ltouter           +
                                  exit if found and leave both loops
static             loctr ,
majorfunc_clc      clc major_func(0),ltfunc
code               loctr ,
                   aghi R6,major_func_entl       ADVANCE TO NEXT ENTRY
                 enddo
*
*                here if no entry in major function table
                 lghi R14,BAD_FUNCTION_CODE LOAD ERROR MESSAGE NUMBER
                 J STDERROR       PRINT ERROR MESSAGE - STOP
               enddo
                          EJECT
***********************************************************************
*    The major function table has
*        o  a pointer to one of an array, a vector or an entry
*        o  the operand locations (op1, op2)
*        o  an index value
***********************************************************************
               llgt R5,major_func_ptr get the pointer from entry
               USING FUNCTBL,R5
*
               mvc lt_opt1,major_op1 this is the target type
               mvc lt_opt2,major_op2             source
               xgr r9,r9
               ic  r9,major_index
               Casentry r9

                 case 1,5,7,26,27,47,48,53,59                          +
                      ADDA,CFAC,CFCA,diva,divc,mula,mulc,seta,SUBA
                   bras r9,LTBLVV     for xxxc/xxxa
                   lgr  r15,r5         load r15 with the default
                 case 2,4,54,60         ADDC, CFAA, SETC, SUBC
                   bras r9,LTBLVV     for xxxc/xxxa
                   lghi r0,-1                set R0 -ve for vector code
                   bras r9,vector_select
                 case 3,6,28,49,55,61                                  +
                      ADDx, CFAx, DIVx, MULx, SETx, SUBx
                   bras r9,LTBLV1     for xxxe/l/p
                   xgr r0,r0                set R0 0 for vector code
                   bras r9,vector_select
                   select cli,ltsubfun+1,eq
                     when c'E'
                       mvi lt_opt2,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       mvi lt_opt2,fc_LKUP setup for later (p1basesr)
                     when c'P'
                       mvi lt_opt2,fc_PREV setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt2,fc_PRIOR setup for later (p1basesr)
                   endsel
                 case 10,12,14,16                                      +
                       CFEA, CFLA, CFPA, CFXA
                   bras r9,LTBLV1     for xxxe/l/p
                   xgr r0,r0                set R0 0 for vector code
                   bras r9,vector_select
                 case 18,31,45,58                                      +
                      CF,DTx, LKx, SKx
                   bras r9,LTBLF2
                   bras r9,array_select
                   select cli,ltsubfun,eq
                     when c'E'
                       mvi lt_opt1,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       mvi lt_opt1,fc_LKUP
                     when c'P'
                       mvi lt_opt1,fc_PREV setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt1,fc_PRIOR setup for later (p1basesr)
                   endsel
                   select cli,ltsubfun+1,eq
                     when c'E'
                       mvi lt_opt2,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       if cli,ltsubfun,eq,c'L'
                         mvi lt_opt2,fc_LKUP2
                       else
                         mvi lt_opt2,fc_LKUP
                       endif
                     when c'P'
                       mvi lt_opt2,fc_PREV setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt2,fc_PRIOR setup for later (p1basesr)
                   endsel
                 case 20,24             CS, CX
                   bras r9,LTBLF1
                   select cli,ltsubfun,eq
                     when c'E'
                       mvi lt_opt1,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       mvi lt_opt1,fc_LKUP setup for later (p1basesr)
                     when c'P'
                       mvi lt_opt1,fc_PREV setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt1,fc_PRIOR setup for later (p1basesr)
                   endsel
                   lgr  r15,r5         load r15 with the default
                 case 21                CTA
                   bras r9,LTBLV1       for xxa
                   lgr  r15,r5         load r15 with the default
                 case 22,30,39,40,41,43,44,57                          +
                     CTC,dtc,kslk,LKC,LKDC,LKLR,LKS,SKC
                   bras r9,LTBLF1       for xxc
                   lgr  r15,r5         load r15 with the default
                 case 38                Join
                   bras r9,LTBLJOIN
                   lgr  r15,r5         load r15 with the default
                 case 19,23             CN, CTx
                   bras r9,LTBLF1
                   lghi  r0,1               set R0 1 for vector code
                   bras r9,vector_select
                   select cli,ltsubfun,eq
                     when c'E'
                       mvi lt_opt1,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       mvi lt_opt1,fc_LKUP setup for later (p1basesr)
                     when c'P'
                       mvi lt_opt1,fc_PREV setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt1,fc_PRIOR setup for later (p1basesr)
                   endsel
                 case 9                 CFC
                   bras r9,LTBLFC
                   lghi  r0,1               set R0 1 for vector code
                   bras r9,vector_select
                   bras r9,date_norm_cfcx_select
                   select cli,ltsubfun+1,eq
                     when c'E'
                       mvi lt_opt2,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       mvi lt_opt2,fc_LKUP setup for later (p1basesr)
                     when c'P'
                       mvi lt_opt2,fc_PREV setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt2,fc_PRIOR setup for later (p1basesr)
                   endsel
                 case 11,13,15,17           CFEC, CFLC, CFPC, CFXC
                   bras r9,LTBLFC
                   lghi  r0,1               set R0 1 for vector code
                   bras r9,vector_select
                   bras r9,date_norm_cfxc_select
                 case 25                DIM function
                   bras r9,LTBLVN
                   lgr  r15,r5         load r15 with the default
                 case 29,56             DTA SKA
                   bras r9,LTBLV1       for xxa
                   bras r9,array_select
                 case 32                END-OF-LOGIC-TBL
                   ST R7,LTENROWA SAVE LAST ROW ("EN" ROW)  ADDRESS
                   LHI R0,LTEN_LEN
                   STH R0,LTROWLEN
                   lgr  r15,r5         load r15 with the default
                 case 33                END-OF-SET
                   bras r9,ltbles
                   if ltg,r0,READLIM,nz read LIMIT set ?
                     Aghi R5,FCENTLEN ADVANCE TO NEXT ENTRY
                   endif
                   lgr  r15,r5         load r15 with the default
                 case 34                END-OF-token
                   bras r9,ltbles
                   lgr  r15,r5         load r15 with the default
                 case 35                GEN
                 case 8,36,50           CFCC, GO, no
                   LHI R0,LTF0_LEN
                   STH R0,LTROWLEN
                   do while=(clc,ltfunc,ne,fcfunc)
                     aghi R5,FCENTLEN ADVANCE TO NEXT ENTRY
                   enddo
                   if clc,ltfunc,eq,cf_cc
                     jas r9,process_cfcc
                   endif
                   lgr  r15,r5         load r15 with the default
                 case 37                HEADER             RECORD
                   ST R7,LTHDROWA
                   LHI R0,LTHD_LEN
                   STH R0,LTROWLEN
                   MVC gp_process_date,LTHD_DATESTAMP SAVE CURRENT DATE
                   MVC LTPROCDT,LTHD_DATESTAMP
                   MVC gp_process_time,LTHD_TIMESTAMP SAVE CURRENT TIME
                   MVC LTPROCTM,LTHD_TIMESTAMP
                   MVC LTFINPDT,SPACES
                   LH R0,MAXSTDF#
                   STH R0,LTMAXFIL SAVE MAXIMUM FILE NUMBER
                   lgr  r15,r5         load r15 with the default
                 case 42                LKD

                   bras r9,LTBLF2
                   lghi  r0,1               set R0 1 for vector code
                   bras r9,vector_select

*           Note that this following select tests subfun+1 but sets
*           LT_opt1 (the opposite to all the others )

                   select cli,ltsubfun+1,eq
                     when c'E'
                       mvi lt_opt1,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       mvi lt_opt1,fc_LKUP setup for later (p1basesr)
                     when c'P'
                       mvi lt_opt1,fc_PREV setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt1,fc_PRIOR setup for later (p1basesr)
                   endsel
                 case 46                LOOKUP EVENT ???
                   bras r9,LTBLLU
                   USING FUNCTBL,R5
                   do while=(clc,ltfunc,ne,fcfunc)
                     aghi R5,FCENTLEN ADVANCE TO NEXT ENTRY
                   enddo
                   lgr  r15,r5         load r15 with the default
                 case 51                NEW                VIEW
                   bras r9,LTBLNV
                   lgr  r15,r5         load r15 with the default
                 case 52                READ EVENT ???
                   bras r9,LTBLRE
                   lgr  r15,r5         load r15 with the default
                 case 62                WRITE EXTRACT ???
                   bras r9,LTBLWR
                   do while=(clc,ltfunc,ne,fcfunc)
                     aghi R5,FCENTLEN ADVANCE TO NEXT ENTRY
                   enddo
                   lgr  r15,r5         load r15 with the default
                 case 63,64,65,66       SFEC,SFLC,SFPC,SFXC
                   bras r9,LTBLFC
                   lgr  r15,r5         load r15 with the default
                 case 67                SFC
                   bras r9,LTBLFC
                   select cli,ltsubfun+1,eq
                     when c'E'
                       mvi lt_opt2,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       mvi lt_opt2,fc_LKUP setup for later (p1basesr)
                     when c'P'
                       mvi lt_opt2,fc_PREV setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt2,fc_PRIOR setup for later (p1basesr)
                   endsel
                   lgr  r15,r5         load r15 with the default
                 case 68               SFxx
                   bras r9,LTBLF2
                   select cli,ltsubfun,eq
                     when c'E'
                       mvi lt_opt1,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       mvi lt_opt1,fc_LKUP
                     when c'P'
                       mvi lt_opt1,fc_PREV setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt1,fc_PRIOR setup for later (p1basesr)
                   endsel
                   select cli,ltsubfun+1,eq
                     when c'E'
                       mvi lt_opt2,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       if cli,ltsubfun,eq,c'L'
                         mvi lt_opt2,fc_LKUP2
                       else
                         mvi lt_opt2,fc_LKUP
                       endif
                     when c'P'
                       mvi lt_opt2,fc_PREV setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt2,fc_PRIOR setup for later (p1basesr)
                   endsel
                   lgr  r15,r5         load r15 with the default
                 case 69,70,71,72,73    FNEC,FNLC,FNPC,FNXC,FNCC
                   bras r9,LTBLV2
                   lgr  r15,r5         load r15 with the default
                 case 74                FNC
                   bras r9,LTBLv2
                   select cli,ltsubfun+1,eq
                     when c'E'
                       mvi lt_opt2,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       mvi lt_opt2,fc_LKUP
                     when c'C'
                       mvi lt_opt2,fc_con  setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt2,fc_PRIOR setup for later (p1basesr)
                   endsel
                   lgr  r15,r5         load r15 with the default
                 case 75                FN
                   bras r9,LTBLv2
                   select cli,ltsubfun,eq
                     when c'E'
                       mvi lt_opt1,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       mvi lt_opt1,fc_LKUP setup for later (p1basesr)
                     when c'C'
                       mvi lt_opt1,fc_con  setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt1,fc_PRIOR setup for later (p1basesr)
                   endsel
                   select cli,ltsubfun+1,eq
                     when c'E'
                       mvi lt_opt2,fc_EVNT setup for later (p1basesr)
                     when c'L'
                       if cli,ltsubfun,eq,c'L'
                         mvi lt_opt2,fc_LKUP2
                       else
                         mvi lt_opt2,fc_LKUP
                       endif
                     when c'C'
                       mvi lt_opt2,fc_con  setup for later (p1basesr)
                     when c'X'
                       mvi lt_opt2,fc_PRIOR setup for later (p1basesr)
                   endsel
                   lgr  r15,r5         load r15 with the default
               endcase
             enddo
           endif
***********************************************************************
*    continue scanning function table for "better" match              *
***********************************************************************
           st  r5,ltfuntbl        load addr of function table entry
           st  r15,default_func   and save the default
*
*          lr  r15,r7
*          ah  r15,0(,r7)
*          bctr r15,0
*          ly  r2,SNAPDCBA
*          snap dcb=(r2),id=110,pdata=(regs),storage=((r7),(r15))
*
*          la  r15,fcentlen-1(,r6)
*          ly  r2,SNAPDCBA
*          snap dcb=(r2),id=111,storage=((r6),(r15))
*
           using lten_rec,r3
*
***********************************************************************
*    format 2 (two sets of field attributes)                          *
***********************************************************************
           if cli,fc_rtyp,eq,fc_rtyp05,or,  format 5 record type ???   +
               cli,fc_rtyp,eq,fc_rtyp02,or, format 2 record type ???   +
               cli,fc_rtyp,eq,fc_rtyp12,or, format 12 record type ???  +
               cli,fc_rtyp,eq,fc_rtyp13     format 13 record type ???
*
*            lhi r15,fcentlen-1
*            ar r15,r5
*          ly  r2,SNAPDCBA
*            snap dcb=(r2),id=114,pdata=(regs),storage=((r5),(r15))
*
             lgh r15,ltmsklen      mask  specified  ???
             if ltr,r15,r15,p
               la r0,ltcolmsk     mask  blank      ???
               lgr r1,r15
               la r14,spaces
               if clcl,r0,eq,r14
                 mvc ltfuntbl,default_func reset ltfuntbl to default
               endif
             endif

             if clc,ltmajfun,eq,cf_ec   compare   function ???
*
               if clc,ltfldcon,ne,ltcolcon different content code ???
*
               if oc,ltfldcon,ltfldcon,z,or, zero content code ???     +
               oc,ltcolcon,ltcolcon,z zero content    code ???
                   lghi r14,CF_DTF_INCOMPATIBLE message number
                   j stderror     print error message - stop
                 endif
               endif
               if oc,ltfldcon,ltfldcon,nz non-zero content code ?      +
                    which implies a date
                 if cli,ltsubfun+1,eq,c'A' is it cfxa?
                   llgt r0,cfxa_date_model get address of cfxa model
                 else
                   llgt r0,cfee_date_model get address of cfee model
                 endif
                 st r0,ltfuntbl
static           loctr
cfee_date_model     dc v(cfeedate)
cfxa_date_model     dc v(cfxadate)
code             loctr
               endif
             else
*
*
*            these tests here are to select the default model should
*            various conditions be met
*
*            this if is handling the selection signalled by fcsuble
*            (these entries have been moved to follow the normal ones
*             and fcsuble has been set in the first one as a safety)
*
               lh r0,ltfldlen       same   field  lengths  ???
               if ch,r0,ge,ltcollen,and,cli,fcltsub+1,eq,fcsuble
                 llgt r0,ltfuntbl load current function entry
                 aghi r0,fcentlen move it along to next entry
                 st r0,ltfuntbl and save it back
               endif
*
*            this if is handling the selection signalled by fcsubdec
*            (these entries have been moved to follow the normal ones)
*             and fcsubdec has been set in the first one as a safety)
*
               if cli,fcltsub+1,eq,fcsubdec,and, is subdec set?        +
               clc,ltfldcon,eq,ltcolcon,and, same content      ???     +
               clc,ltndec,eq,ltcoldec,and, same    decimals    ???     +
               clc,ltrndfac,eq,ltcolrnd,and, same rounding     ???     +
               clc,ltsign,eq,ltcolsgn        same  sign        ???
                 llgt r0,ltfuntbl load current function entry
                 aghi r0,fcentlen move it along to next entry
                 st r0,ltfuntbl and save it back
               endif
*
*            this if is handling the selection signalled by fcsubcom
*            (these entries have been moved to follow the normal ones)
*             and fcsubcom has been set in the first one as a safety)
*
               lh r0,ltfldlen
               if cli,fcltsub+1,eq,fcsubcom,and, is subcom set?        +
               ch,r0,eq,ltcollen,and,        same  lengths?            +
               clc,ltfldfmt,eq,ltcolfmt,and, same data    type ???     +
               clc,ltfldcon,eq,ltcolcon,and, same content      ???     +
               clc,ltndec,eq,ltcoldec,and, same    decimals    ???     +
               clc,ltrndfac,eq,ltcolrnd,and, same rounding     ???     +
               clc,ltsign,eq,ltcolsgn        same  sign        ???
                 llgt R14,EXECDADR
                 USING EXECDATA,R14
                 if cli,exec_optpo,eq,c'Y'   optimize on       ???
                   llgt r0,ltfuntbl load current function entry
                   aghi r0,fcentlen move it along to next entry
                   st r0,ltfuntbl and save it back
                 endif
                 drop r14
               endif
*
*            this if is handling differing content codes
*
             if (oc,ltcolcon,ltcolcon,nz),and, field content specified?+
               (clc,ltfldcon,ne,ltcolcon)     and different contents
               mvc ltfuntbl,default_func reset ltfuntbl to the default
             endif
*
*            this if is handling any non standard justification
*            (used to be signalled by fcsubjus)
*
               lh r0,ltcolfmt
               if chi,r0,eq,fc_alnum      alphanumeric data ?
                 if cli,ltcoljus,ne,c'L' and not left justified
                   mvc ltfuntbl,default_func reset ltfuntbl to default
                 endif
*      else --remove because numeric type should ignore justification
*               if chi,r0,eq,fc_num,and, numeric?                     +
*              cli,ltcoljus,ne,c'R'     not right justified
*                  mvc ltfuntbl,default_func reset ltfuntbl to default
*                endif ---
               else
*                numeric type output must be len 16 or less for
*                generated models, otherwise GVBDL96 called by the
*                default function, must handle it
                 lh r0,ltcollen
                 if chi,r0,gt,16
                   mvc ltfuntbl,default_func reset ltfuntbl to default
                 endif
               endif
*
             endif
           endif
           if  oc,ltfuntbl,ltfuntbl,z  is the function code zero ?
***********************************************************************
*            no model code definition                                 *
***********************************************************************
             lghi r14,NO_MACH_MODEL     message number
             j stderror print error message - stop
           endif
                          eject
***********************************************************************
*    save maximum column no. for view                                 *
***********************************************************************
           clc ltmajfun,ct_a      column calculation function ???
           jne ltblgenl           no  -  bypass "highwater" column no.
*
           lh  r0,ltcolno         load   column number
           cli fc_rtyp-functbl(r6),fc_rtyp09  format 9 record type ???
           if (e)
             lh r0,ltvncol#       load   column number
           endif
*
           llgt r1,ltlognv
ltnv       using logictbl,r1
           if  (clc,ltnv.ltmincol,eq,h0),or,       updated this yet?   +
               (ch,r0,lt,ltnv.ltmincol) new value is smaller?
             sth r0,ltnv.ltmincol        no - so save this value now
           endif
*
           if  (ch,r0,gt,ltnv.ltmaxcol)  larger than previous max  col
             sth r0,ltnv.ltmaxcol          yes -  save largest  value
           endif
           drop ltnv
*
***********************************************************************
*    estimate size of generated code                                  *
***********************************************************************
ltblgenl   lh  r0,fccodeln        load  length of skeleton  code
           ltr r0,r0              any   code   ???
           jnp ltblnext           no  - bypass buffer estimation
*
ltbltrac   llgt r14,execdadr
           using execdata,r14
           cli exectrac,c'Y'      trace option  specified ???
           drop r14
           jne ltbltokn
*
           clc ltmajfun,nv
           je  ltbltokn
           clc ltmajfun,re_nx
           je  ltbltokn
           clc ltmajfun,es
           je  ltbltokn
           clc ltmajfun,et
           je  ltbltokn
           clc ltmajfun,hd
           je  ltbltokn
           clc ltmajfun,=c'DI'    Is DI(mx) ?
           je  ltbltokn
*
           cli viewtrac,c'Y'      view  specific trace   option  ???
           jne ltblbrch           no  - trace all views
*
           ltgf r14,ltlognv        load  "nv"  row  addr
           jnp ltbltokn                       no  - bypass
           lt  r14,ltparmtb-logictbl(,r14)    trace this view  ???
           jnp ltbltokn                       no  - bypass
*
ltblbrch   ahi r0,l'branch        add  length of branch instruction
*
ltbltokn   sth r0,ltgenlen        save generated code size
*
***********************************************************************
*    estimate size of literal pool requirements                       *
***********************************************************************
           lgh r0,fclitpln        load literal  pool requirements
*
           tm  ltflag2,ltrtoken   event data    is token  ???
           jno ltblprfx           no  - bypass  length adjustment
           aghi r0,4               add   literal pool   impact
*
           lgh  r14,ltgenlen
           aghi r14,rdtokenl       add  length  of add'l  code
           sth r14,ltgenlen
*
***********************************************************************
*   set flag if function requires a "look-up buffer" prefix (lb addr) *
***********************************************************************
ltblprfx   clc ltfunc,lk_lr       copy look-up lr_id ???
           je  ltblnext
           clc ltfunc,join        join ???
           je  ltblnext
           clc ltfunc,ks_lk       save look-up key   ???
           je  ltblnext
           clc ltfunc,lu_sm       look-up   memory    ???
           je  ltblnext
           clc ltfunc,wr_su       write summarized     ???
           je  ltblnext
*
           clc ltmajfun,mul_l     multiply accumulator ???
           je  ltblmull           yes -  not a look-up
*
           cli ltsubfun+0,c'L'    look-up  argument    ???
           je  ltbllkup
ltblmull   cli ltsubfun+1,c'L'
           jne ltblcons
*
ltbllkup   llgt r14,fcreloca      load  relocation   table address
           cli 0(r14),csltroff    first code = logic table row offset ?
           je  ltblcons
*
*          the reason why we test for this is that the logic table
*          address is usually loaded into r5, so it would be interfer
*          with this prefix stuff
*
           oi  ltflags,ltlkupre   indicate look-up  prefix needed
           aghi r0,l'lkuppref      add literal pool  impact
*
           lgh  r14,ltgenlen
           aghi r14,l'lkuppref     update generated  code   size
           if clc,=c'LL',eq,ltsubfun,and,       is this xxLL?          +
               clc,ltmajfun,ne,mul_l       but NOT MULL
             aghi r14,l'lkuppref   update generated  code   size
           endif
           sth r14,ltgenlen
*
***********************************************************************
*    if function uses a constant add constant len to gen code length  *
***********************************************************************
ltblcons   cli ltsubfun+1,c'R'    low  value   copied  to    pool ???
           je  ltblcon1           yes- include length  in    total
           cli ltsubfun+0,c'S'    symbolic con copied  to    pool ???
           je  ltblcon1           yes- include length  in    total
           clc ltfunc,cf_cc       is it a CFCC?
           je  ltblnext           Y: then ignore it
           cli ltsubfun+0,c'C'    low  value   copied  to    pool ???
           jne ltblcon2           no - exclude length  from  total
ltblcon1   ds  0h
           ni  initflag,x'ff'-cons_fnd
           stg r0,dblwork2
           bras r9,chkcons        see if this constant is repeated
           lg  r0,dblwork2
           tm  initflag,cons_fnd  was it found?
           jo  ltblcon2           y
           lgh r14,ltfldlen       add  length  of low  value to total
           agr r0,r14             add  length  of low  value to total
           aghi r0,1
*
ltblcon2   cli fc_rtyp,fc_rtyp10  variable value  ???
           jne ltblcon3
           lgh  r14,ltvvlen        add  length  of low  value to total
           agr  r0,r14             add  length  of low  value to total
           j   ltblnext
*
ltblcon3   cli ltsubfun+1,c'R'    high value   copied  to    pool ???
           je  ltblcon4           yes- include length  in    total
           cli ltsubfun+1,c'S'    substring    copied  to    pool ???
           je  ltblcon4           yes- include length  in    total
           cli ltsubfun+1,c'C'    low  value   copied  to    pool ???
           jne ltblnext           no - exclude length  from  total
*
ltblcon4   ds  0h
           ni  initflag,x'ff'-cons_fnd
           stg r0,dblwork2
           bras r9,chkcons        see if this constant is repeated
           lg  r0,dblwork2
           tm  initflag,cons_fnd  was it found?
           jo  ltblnext           y
           lgh r14,ltfldlen       add  length  of low  value to total
           agr r0,r14             add  length  of low  value to total
           aghi r0,1
*
                          eject
ltblnext   ds  0h
           using lthd_rec,r3
           if clc,=c'GEN ',ne,lthd_function_code    not a gen record?
             if clc,ltfunc,eq,es,or,clc,ltfunc,eq,et
               mvc symtblc,symtblb  reset current symbol table  beg
             endif
             agf r0,espoolsz      increment estimated "es" pool size
             st r0,espoolsz
*
             lgh r0,ltgenlen       update cumulative  code  size
             agf r0,codesize
             st r0,codesize
*
             l r0,ltcount         increment  record count
             ahi r0,1
             st r0,ltcount
*
             lgh r15,ltrowlen      round length up to fullword
             la r15,3(,r15)
             nill r15,x'fffc'
             sth r15,ltrowlen
*
             agr r7,r15            advance  to next   row    (if any)
             llgt r0,ltbegin        compute remaining table space
             agf r0,ltsize
             sgr r0,r7
             doexit chi,r0,lt,ltcush get out if cushion exhausted
           endif
         enddo
*
ltblover lghi  r14,LTBL_OVERFLOW  load  error message number
         j     stderror           print error message - stop
*
                        eject
***********************************************************************
*  close logic table file                                             *
***********************************************************************
ltbleof  ds    0h
         sysstate amode64=NO
         sam31
         close ((r2)),mode=31,mf=(E,wkreent)    close logic table file
         sam64
         sysstate amode64=YES
*
***********************************************************************
*  allocate extract file area if not pre-allocated for version 4      *
***********************************************************************
         if ltgf,r0,extfilea,z      extract file area not there
           bras r9,alloextf       allocate extract file area
         endif
         lgh   r1,filecnt
         lgf   r0,filecnt_real    get original size of file area
         sty   r1,filecnt_real    Save for reports
         aghi  r1,1               Allow for 1 extra
         mghi  r1,extfilel        get length we need
         aghi  r1,7+l'extfeyeb    Round to a
         srlg  r1,r1,3             double word and
         sllg  r1,r1,3             allow for eyecatcher
         sgr   r0,r1              Length we dontneed
         jnp   ltbl_byp
         agf   r1,extfilea        address to free
         FREEMAIN RU,LV=(0),A=(1) release unused file space
ltbl_byp ds    0h
*
         drop  r2
*
***********************************************************************
*  test for minimum logic table requirements                          *
***********************************************************************
         lgf   r0,ltcount         minimum no.    of rows  ???
         if clc,namepgm,eq,=cl8'GVBMR95E'    extract phase
           lghi  r14,EMPTY_LOGIC_TBL assume bad logic  table
           cghi  r0,minltbl
           brl   rtnerror            no  -   error
*
         else
           lghi  r14,EMPTY_REF_LOGIC_TBL get the message number
           cghi  r0,minltbl
           jl    rtnerror            no  -  information message
         endif
*
         lghi  r14,LTBL_NO_WORK_UNITS assume no threads/work units
         lt    r0,essetcnt        "es"    row(s) present  ???
         jnp   rtnerror           no  -   error
*
***********************************************************************
*  return                                                             *
***********************************************************************
         lmg   r14,r12,SAVF4SAG64RS14    restore   registers
         br    r14                       return
*
***********************************************************************
*    load 'nv' new view record                                        *
***********************************************************************
           using ltnv_rec,r3
*
ltblnv     lhi r0,ltnv_len
           sth r0,ltrowlen
*
           lh  r14,viewcnt        increment  view count
           ahi r14,1
           sth r14,viewcnt
*
           st  r7,ltlognv         initialize view beginning  row   addr
*
           l   r15,ltnv_view_type
           stc r15,ltviewtp
*
           xc  svcolmn#,svcolmn#  zero maximum column  number
*
           mvi ltstatus,c' '
           mvc ltuserid,spaces
           mvc ltlrid,ltnv_source_lr_id
           mvc ltsortln,ltnv_sort_key_len
           xc  lttitlln,lttitlln  do not use the nv value
           mvc ltdataln,ltnv_dt_area_len
           xc  ltmincol,ltmincol
           xc  ltmaxcol,ltmaxcol
           xc  ltmaxocc,ltmaxocc
           xc  ltsumcnt,ltsumcnt
           xc  lt1000a,lt1000a
           xc  ltnvvnam,ltnvvnam
           mvc ltviewre,firstre
           xc  ltviewes,ltviewes
           xc  ltnextnv,ltnextnv
           xc  ltnvtokn,ltnvtokn
           xc  ltfrstlu,ltfrstlu
           xc  ltfrstwr,ltfrstwr
           xc  ltparent,ltparent
           xc  ltparmtb,ltparmtb
           xc  ltsubopt,ltsubopt
           xc  ltnvrelo,ltnvrelo
*
***********************************************************************
*    set little endian indicator                                      *
***********************************************************************
           xr  r0,r0
           ic  r0,ltnv_little_endian
           mvi ltendian,c'N'
           chi r0,lendian
           jne *+8
           mvi ltendian,c'Y'
*
***********************************************************************
*    initialize current "driver lr id"                                *
***********************************************************************
           mvc drivlrid,ltlrid    save  "driver" record  id
*
***********************************************************************
*    set flag indicating view event record is a "token"               *
***********************************************************************
           ltgf r14,ltviewre       load  "re" logic table row address
           jnp ltblnvch
*
ltretk     using logictbl,r14     map the retk data
           lh  r0,ltretk.ltfiltyp load file   type
           chi r0,tokendev        event  data is   token   ???
           jne ltblnvch           no  -  bypass    setting ind
*
           oi  ltretk.ltflag2,ltrtoken yes -  set  indicator in the re
           oi  ltflag2,ltrtoken        and the current lt
*
           mvc ltretk.lttoknlr,ltlrid    token "lrid"
           drop ltretk
*
***********************************************************************
*    chain "nv" rows within "es" set                                  *
***********************************************************************
ltblnvch   llgt r14,currnv        save   previous "nv"   row address
           st  r7,currnv          update current  "nv"   row address
           if ltgr,r14,r14,p       previous  "nv"  row    exists  ???
             st  r7,ltnextnv-logictbl(,r14)  yes - chain "nv"   rows
           endif
*
           if oc,firstnv,firstnv,z    first "nv" row ???
             st  r7,firstnv       yes -  save    row     address
           endif                  no  -  bypass  setting
*
***********************************************************************
*    initialize "lu" and "wr" list pointers for this "nv" to null     *
***********************************************************************
           xc  prevlu,prevlu      reset  previous  "lu"  row
           xc  prevwr,prevwr      reset  previous  "wr"  row
*
***********************************************************************
*    check for view specific trace options                            *
***********************************************************************
           bras r14,tracparm
*
***********************************************************************
*    locate matching vdp view record ("1000")                         *
***********************************************************************
           lgf r1,ltview#
           stg r9,dblwork      save linkage address
           bras r9,locview
           lg  r9,dblwork         restore linkage address
           st  r15,lt1000a
           using vdp1000b_view_record,r15
           stg r15,dblwork2
*     Save the view number and name in our table for
*     printing out later
*
           if lt,r1,vdp1000b_use_count,z
             llgt r14,viewtbl_c        current free entry in our table
             using view_table,r14
             mvc   view_nbr,vdp1000b_viewid   save view number
             mvc   view_name,vdp1000b_view_name save view name
             mvc   view_type,vdp1000b_view_type
             mvc   view_output_media,vdp1000b_output_media
             mvc   view_epa_summ_recs,vdp1000b_EXTRACT_SUMM_RECS
*
*          does view have any summary columns ?
*
             if cli,LTVIEWTP,eq,SUMVIEW
               mviy view_format_summary,c'Y'  signal format summary
             else
               mviy View_format_summary,c'N'
             endif
*
             alsi  view_cnt,1
             aghi  r14,view_table_ent_len     next free entry
             sty   r14,viewtbl_c              save it
             drop  r14

             if ltgf,r15,vdp1000b_control_id,nz is there a control rec?
               using vdp0050b_control_record,r15
               mvc  cur_fdate,vdp0050b_fiscal_date fiscal date for view
               pack dblwork,cur_fyear    .convert character year to
               cvb  r14,dblwork          .  numeric
               st   r14,cur_bfyear       .save in binary form
               pack dblwork,cur_fmth     .convert character month to
               cvb  r14,dblwork          .  numeric
               st   r14,cur_bfmth        .save in binary form
               pack dblwork,cur_fday     .convert character day to
               cvb  r14,dblwork          .  numeric
               st   r14,cur_bfday        .save in binary form
*
*          convert date to a day number
               la  r1,cur_bfyear         .point at date to convert
               sty r9,dblwork
               jas r9,conv_date_daynum      .convert it
               ly  r9,dblwork
               st  r1,cur_fdaynum        .save day number

             endif
           endif
           lg  r15,dblwork2
           using vdp1000b_view_record,r15
           asi vdp1000b_use_count,1      .increment use count
           drop r15
*
           br  r9                 return
                          eject
***********************************************************************
*    load 'f1' f1 format record (1 set of field attributes)           *
***********************************************************************
           using ltf1_rec,r3
*
ltbljoin   stg r9,lt_saver9
*
***********************************************************************
*  "join" - build "ltlu..." logic table row and reset look-up key pos *
*     save file/logical record/path id's for later look-up ("lu..")   *
***********************************************************************
           lhi r0,ltlu_len
           sth r0,ltrowlen
*
           mvc ltlufile,hexff
           mvc ltlufid,ltf1_field_file_id
           mvc ltlulrid,ltf1_lr_id
*
           l   r15,ltf1_value_length
           bctr r15,0
           ex  r15,ltf1pack
           oi  dblwork+l'dblwork-1,x'0f'
           cvb r0,dblwork
           st  r0,ltlupath
*
           mvc ltluwpath,ltf1_column_id
*
           xc  ltlu_next_exit,ltlu_next_exit next data area ptr to null
           xc  ltluexit,ltluexit
           mvc ltluname,spaces
           xc  ltluopt,ltluopt
           xc  ltluptyp,ltluptyp
           xc  ltluaddr,ltluaddr
           xc  ltluentp,ltluentp
           xc  ltluwork,ltluwork
           mvc ltluparm,spaces
           la  r0,ltlu_next_exit     address this area
           st  r0,exit_next          save in previous
           lgr r11,r0                and move pointer to this area
*
           mvc svlufile,ltlufile
           mvc svlulrid,ltlulrid
           mvc svlupath,ltlupath
           mvc svluwpath,ltluwpath
           mvc svlusubr,spaces
           mvc svluparm,spaces
           xc  svluopt,svluopt
*
           lhi r0,5               init  current look-up  key pos
           sth r0,keyposn
*
           xc  lkupstko,lkupstko  reset current stack offset
           xc  lkupstk#,lkupstk#  reset current stack count
*
***********************************************************************
*    locate vdp "lr" record to see if look-up exit specified          *
***********************************************************************
           llgt r14,vdpbegin      search  "vdp"
           using vdp0300b_lr_record,r14
*
           lgf r15,vdpcount
*
           do ,label=outer_join
             do from=(r15)
               lh r0,vdp0300b_record_type
               doexit do=outer_join,(chi,r0,eq,vdplr),and,             +
               (clc,ltlulrid,eq,vdp0300b_record_id)
*
               ltg r14,vdp0300b_next_ptr
               bnpr r9
             enddo
             br r9
           enddo
*
***********************************************************************
*    locate vdp exit program record if "lr" indicates exit program    *
***********************************************************************
           if ltgf,r1,vdp0300b_exit_pgm_id,p   actual address?
*
             mvc ltluparm,vdp0300b_exit_startup
             mvc svluparm,ltluparm
*
             drop r14
*
             bras r9,locexit      locate vdp exit pgm record (210)
             lg r9,lt_saver9         restore linkage
             using vdp0210b_exit_pgm_record,r15
             if ltgr,r15,r15,p       success?
*
               mvc ltluname,vdp0210b_module_name
               mvc ltluptyp,vdp0210b_program_type
               mvc ltluopt,vdp0210b_optimizable
               mvc svlusubr,ltluname
               mvc svluopt,ltluopt
*
             endif
*
           endif
           br  r9
*
           DROP R15
*
***********************************************************************
*    BUILD FORMAT 1 LOGIC TABLE ROW                                   *
***********************************************************************
LTBLF1     stg r9,lt_saver9
           LHI R0,LTF1_LEN
           STH R0,LTROWLEN
*
           MVC LTCOLNO,LTF1_SUFFIX_SEQ_NBR
           MVC LTCOLID,LTF1_COLUMN_ID
*
           MVC LTFLDDDN,HEXFF
           MVC LTFLDFIL,LTF1_FIELD_FILE_ID
           MVC LTFLDLR,LTF1_LR_ID
           MVC LTFLDPTH,SVLUPATH
           MVC LTFLDID,LTF1_FIELD_ID
           MVC LTFLDPOS,LTF1_START_POSITION
           MVC LTFLDSEQ,LTF1_ORDINAL_POSITION
           MVC LTFLDLEN,LTF1_FIELD_LENGTH
           MVC LTFLDFMT,LTF1_FIELD_FORMAT+2
           MVC LTFLDCON,LTF1_CONTENT_ID+2
*
           XC  LTV1LEN,LTV1LEN
           XC  LTV2LEN,LTV2LEN
*
***********************************************************************
*    START OF BUILD LOOK-UP KEY SEQUENCE ("LK_LR")                    *
***********************************************************************
  if CLC,LTFUNC,eq,LK_LR             FIRST KEY  FIELD ???
*
    LHI R0,1
    STH R0,LTFLDPOS
    STH R0,LTFLDFMT
    LHI R0,4
    STH R0,LTFLDLEN
*
    LHI R0,5                      INITIALIZE CURRENT LOOK-UP KEY POSN
    STH R0,KEYPOSN
*
    MVC LTFLDFIL,LTF1_FIELD_FILE_ID
    MVC LTFLDLR,LTF1_LR_ID
*
    l   r15,ltf1_value_length
    bctr r15,0
    ex  r15,ltf1pack
    oi  dblwork+l'dblwork-1,x'0f'
    cvb r0,dblwork
    st  r0,ltfldpth
*
    mvc svluwpath,ltf1_column_id
*
    MVC SVLUFILE,LTFLDDDN
    MVC SVLULRID,LTFLDLR
    mvc svlupath,ltfldpth
*
    MVC SVLUSUBR,SPACES
    MVC SVLUPARM,SPACES
    XC  SVLUOPT,SVLUOPT
*
* If this LKLR is the top of the path then we need to reset the
* LU stack counter. Note: for non-optimisable paths the JOIN is
* replaced by a LKLR since 4.18
*
    mvc LTLUSTEP,LTF1_ORDINAL_POSITION
    if CLC,LTF1_ORDINAL_POSITION,eq,H1  First step in path?
      xc  lkupstko,lkupstko  reset current stack offset
      xc  lkupstk#,lkupstk#  reset current stack count
    endif
    br         r9
*
  endif
***********************************************************************
* UPDATE POSITION OF NEXT LOOK-UP KEY FIELD                           *
***********************************************************************
           CLC LTMAJFUN,LK_LR     LOOK-UP  KEY  FUNCTION     ???
           JNE LTBLF1F            NO  - BYPASS  DETERMINING  POS
*
           LH  R15,KEYPOSN        LOAD CURRENT  LOOK-UP  KEY POSITION
           STH R15,LTFLDPOS       SAVE POSITION FOR THIS FIELD
           AH  R15,LTFLDLEN
           STH R15,KEYPOSN        SAVE NEXT     KEY POSITION
*
           J   LTBLF1G
*
***********************************************************************
*    LOOK-UP KEY SAVE FUNCTION                                        *
***********************************************************************
LTBLF1F    CLC LTFUNC,KS_LK       KEY  SAVE     ???
           JNE LTBLF1G            NO - BYPASS   KEY RESET
*
           LHI R0,1
           STH R0,LTFLDPOS
*
           LH  R0,KEYPOSN
           BCTR R0,0
           STH R0,LTFLDLEN
*
           XC  KEYPOSN,KEYPOSN    RESET   KEY   POSITION
*
           llgt R1,ltlognv                LOAD  CURRENT "NV" ROW ADDR
           LH  R0,LTTITLLN-LOGICTBL(,R1)  CURRENT TITLE KEY  POSITION
           AH  R0,LTFLDLEN                ADVANCE TITLE KEY  POSITION
           AHI R0,4                       EXTEND  AREA  BY   FILEID LEN
           STH R0,LTTITLLN-LOGICTBL(,R1)
*
           J   LTBLF1Z
*
***********************************************************************
*  COPY DECIMALS, ROUNDING, SIGNED, SORT SEQ, RELATIONAL COMPARE OPER *
***********************************************************************
LTBLF1G    LH  R0,LTF1_NO_OF_DECIMALS
           STC R0,LTNDEC
*
           LH  R0,LTF1_ROUNDING
           STC R0,LTRNDFAC
*
           XR  R0,R0
           IC  R0,LTF1_SIGN_IND
           MVI LTSIGN,C'N'
           if CHI,R0,eq,SIGNED
             MVI LTSIGN,C'Y'
           endif
*
           MVI LTSRTSEQ,C'A'
           lgf R0,LTVIEW#
           if  (Lt,R1,LTCOLID,P)    only call locsort if +ve colid
             BRAS R9,LOCSORT
             lg r9,lt_saver9         restore linkage
             if  (LTgR,R15,R15,P)    found?
               USING vdp2300b_SORT_KEY_ATTR_RECORD,R15
               if  (CLI,vdp2300b_SORT_SEQ_ID+3,EQ,DESCEND) and this set
                 MVI LTSRTSEQ,C'D'
               endif
               DROP R15
             endif
           endif
*
           lgf R15,LTF1_COMPARE_TYPE
           SLLg R15,r15,1
           LA  R14,COMPTBL(R15)
           MVC LTRELOPR,0(R14)
*
           XC  LTSTRNO,LTSTRNO
           XC  LTSTRLVL,LTSTRLVL
           XC  LTJUSOFF,LTJUSOFF
*
***********************************************************************
*    CHECK FOR PRESENCE OF CONSTANT                                   *
***********************************************************************
           CLC LTF1_VALUE_LENGTH,ALLFUNC
           JNE LTBLF1H0
*
           lghi R1,L'LTF1_VALUE              LOAD LOOP  COUNTER
           LA  R15,LTF1_VALUE+L'LTF1_VALUE  LOAD ADDR  OF LAST BYTE +1
*
           do ,label=test_blank
             do from=(r1,,r14)
               BCTgR R15,0
               doexit CLI,0(R15),ne,C' ',do=test_blank xit if not blank
             enddo
             lghi R1,1             ASSUME  1 CHARACTER
           enddo
*
           J   LTBLF1H1
*
LTBLF1H0   ds  0h
           if tm,ltf1_value_length,x'80',o
             la r1,ltf1_value_area   This is our parm to Cookie subrtn
             jas r9,Cookie       Call Cookie subroutine
             sty r1,LTF1_VALUE_LENGTH save the resolved length
           endif
           ltgf R1,LTF1_VALUE_LENGTH
           JP  LTBLF1H1
           clc ltfunc,lk_s        Is it build key with symbol
           jne ltblf1h0_a         N:
           mvc ltv1len,ltfldlen   Make sure length ids non-z
           j   ltblf1z            Get out of here
ltblf1h0_a ds  0h
*
***********************************************************************
*    PROVIDE DEFAULT CONSTANT WHEN NONE PROVIDED                      *
***********************************************************************
           CLI LTSUBFUN+0,C'C'    CONSTANT SHOULD BE PRESENT ???
           JNE LTBLF1Z            NO  - EXIT
*
           lghi R15,1              LENGTH OF DEFAULT CONSTANT
           LA  R0,SPACES
           CLI LTFLDFMT+1,FC_ALNUM     ALPHANUMERIC ???
           JE  LTBLF1I
*
           LA  R0,ZEROES          CHANGE DEFAULT TO ZERO
           J   LTBLF1I
*
***********************************************************************
*    SCAN VALUE FOR ESCAPE SEQUENCE ("\")                             *
***********************************************************************
LTBLF1H1   LA  R14,LTF1_VALUE     LOAD SOURCE ADDRESS
           LA  R15,LTF1_VALUE     LOAD TARGET ADDRESS
*
           if CLI,0(R14),eq,X'E0' BACKSLASH ("\") ???
*
             if CLI,1(R14),eq,X'A7' LOWERCASE  "X"  ???
*
               la r14,2(,r14)     skip \x
               ahi r1,-2          adjust length
*
*              check for even number of digits
               if tml,r1,x'0001',o
                 lghi R14,HEX_STRING_ERR
                 J STDERROR
               endif
               lgr r0,r1          remember length
               jas r9,convert_hex
*           calc length of target field
               srlg r15,r0,1
             else
*
               lghi R14,HEX_STRING_ERR
               J STDERROR
             endif
           else
*
LTBLF1H7     MVC 0(1,R15),0(R14)  COPY  1 BYTE (MAY BE SAME BYTE)
             LA R15,1(,R15)
             LA R14,1(,R14)
             BRCTg R1,LTBLF1H7     LOOP  THROUGH WHOLE  STRING
*
             LA R1,LTF1_VALUE     COMPUTE FINAL LENGTH
             sgr R15,R1
           endif
*
           lg r9,lt_saver9           restore linkage
*
           CLC LTF1_VALUE_LENGTH,ALLFUNC  "ALL" FUNCTION  ???
           JNE LTBLF1H8                   NO  - BYPASS PROPAGATION
*
           lgh R1,LTFLDLEN        COMPUTE PROPAGATION  LENGTH (-1)
           sgr R1,R15
           aghi R1,-1
           BRM LTBLF1H8
           LA  R14,LTF1_VALUE
           LA  R15,LTF1_VALUE(R15)
           EX  R1,LTF1PROP
           LH  R15,LTFLDLEN
*
LTBLF1H8   ST  R15,LTF1_VALUE_LENGTH
***********************************************************************
*    ADJUST FIELD POSITION IF "ENDS WITH"                             *
***********************************************************************
           if CLC,LTRELOPR,eq,EW
*
             lgh R0,LTFLDLEN       LOAD     FIELD    LENGTH
             sgr R0,R15            SUBTRACT STRING   LENGTH
             lgh R14,LTFLDPOS      COMPUTE  POSITION FOR   "ENDS WITH"
             agr R0,r14            COMPUTE  POSITION FOR   "ENDS WITH"
             STH R0,LTFLDPOS
           endif
*
***********************************************************************
*    ADJUST FIELD LENGTH IF "BEGINS/ENDS" WITH                        *
***********************************************************************
           if CLC,LTRELOPR,eq,BW,or,                                   +
               CLC,LTRELOPR,eq,EW
             STH R15,LTFLDLEN
           endif
*
***********************************************************************
*    INITIALIZE SOURCE VALUE LENGTH AND ADDRESS                       *
***********************************************************************
           LA  R0,LTF1_VALUE
*
LTBLF1I    stg R0,SAVALADR
           STH R15,SAVALLEN
*
***********************************************************************
*    INITIALIZE TARGET ATTRIBUTES                                     *
***********************************************************************
           MVC SAOUTFMT,LTFLDFMT       RESULT ATTRIBUTES
           MVC SAOUTCON,LTFLDCON
           MVC SAOUTDEC+1(1),LTNDEC
           MVC SAOUTRND+1(1),LTRNDFAC
           MVC SAOUTSGN,LTSIGN
           MVI SAOUTJUS,C'L'
*
           XC  SAMSKLEN,SAMSKLEN
           XC  SAMSKADR,SAMSKADR
*
           LA  R14,LTVALUES            RESULT ADDRESS
           ST  R14,DL96TGTA
*
           LH  R0,LTFLDLEN             RESULT LENGTH
           STH R0,LTV1LEN
           STH R0,DL96LEN
*
***********************************************************************
*    CONVERT DATE CONSTANTS TO SPECIFIED FORMAT (CONTENT CODE)        *
***********************************************************************
LTBLF1J    LH  R1,LTFLDCON        DATE  CONTENT CODE SPECIFIED ???
           LTR R1,R1
           JNP LTBLF1K            NO  - BYPASS  DATE SUBSTRING
*
           MVI SAVALFMT+1,FC_NUM
*          MVI SAVALCON+1,NORMDATE
           MVI SAVALCON+1,cymd
           lhi r1,8
           sth r1,savallen
           MVHHI SAVALDEC,0
           MVHHI SAVALRND,0
           MVI SAVALSGN,C'N'
*
           J   LTBLF1W
*
***********************************************************************
*    COMPLETE SOURCE VALUE ATTRIBUTES                                 *
***********************************************************************
LTBLF1K    MVC SAVALFMT,LTFLDFMT
           MVC SAVALCON,LTFLDCON
           MVHHI SAVALDEC,0
           MVHHI SAVALRND,0
           MVI SAVALSGN,C'Y'
*
***********************************************************************
*    COMPARE EDITED/MASKED NUMERIC FIELD TO CONSTANT                  *
***********************************************************************
LTBLF1L    CLC LTMAJFUN,CF_EC     COMPARE FIELD ???
           JNE LTBLF1M            NO - BYPASS   KEY RESET
           CLI LTSUBFUN+1,C'C'    CONSTANT      ???
           JNE LTBLF1M            NO - BYPASS   KEY RESET
*
           MVI SAOUTFMT+1,FC_float     RESULT ATTRIBUTES
           XC  SAOUTCON,SAOUTCON
*
*   This code MUST match the similar code in GVBMR95 just after
*   label CALL96CE - the CFEC model code expects two numbers
*   with the same layout
*
*          ZAP SAOUTDEC,P008
*          ZAP SAOUTRND,P000
*          MVI SAOUTSGN,C'Y'
*          MVI SAOUTJUS,C'L'
*
           LHI R15,L'fc_float          RESULT LENGTH
           STH R15,LTV1LEN
           STH R15,DL96LEN
*
           J   LTBLF1W                 NO - BYPASS   KEY RESET
*
***********************************************************************
*    SET DATA TYPE FOR SOURCE CONSTANT                                *
***********************************************************************
LTBLF1M    MVI SAVALFMT+1,FC_EDIT      ASSUME NUMERIC SOURCE
*
           CLI LTFLDFMT+1,FC_ALNUM     ALPHANUMERIC   ???
           Jne LTBLF1p
*
LTBLF1N    CH  R15,LTFLDLEN            SAME   LENGTH  ???
           JE  LTBLF1X                 YES -  NO  CONVERSION
*
           MVI SAVALFMT+1,FC_ALNUM     ALPHANUMERIC   SOURCE
           J   LTBLF1U
*
***********************************************************************
*    USE "CT" COLUMN ATTRIBUTES IF APPROPRIATE                        *
***********************************************************************
LTBLF1P    CLC LTMAJFUN,CT_a           "CT"   COLUMN
           JNE LTBLF1Q
*
           LHI R0,L'COLDATA            RESULT LENGTH
           STH R0,LTFLDLEN
           STH R0,LTV1LEN
           STH R0,DL96LEN
*
           MVI SAOUTFMT+1,FC_PACK      RESULT ATTRIBUTES
           XC  SAOUTCON,SAOUTCON
*
           MVHHI SAOUTDEC,8
           CLI LRGNUM,C'Y'
           JNE *+10
           MVHHI SAOUTDEC,3
*
           MVHHI SAOUTRND,0
           MVI SAOUTSGN,C'Y'
           MVI SAOUTJUS,C'L'
*
           J   LTBLF1U
*
***********************************************************************
*    USE MASK IF APPROPRIATE FOR FORMATTING CONSTANT                  *
***********************************************************************
LTBLF1Q    CLC LTMAJFUN,DT_C      "DT" COLUMN ???
           JNE LTBLF1U            NO  - MASKS DON'T  APPLY
*
           lgf R0,LTVIEW#         LOCATE VIEW COLUMN RECORD (2000)
           LTgf R1,LTCOLID         load column and set condition code
           BRNP LTBLF1U           and exit if not positive
           lgf R14,ltlognv        get the nv pointer for the subr
           BRAS R9,LOCCOLM
           lg r9,lt_saver9           restore linkage
           USING vdp2000b_COLUMN_RECORD,R15
           LTgR R15,R15
           JNP LTBLF1U
*
           LA  R14,L'MASKCODE-1
           EX  R14,MASKBLNK
           JE  LTBLF1U
*
***********************************************************************
*    BUILD MASK FROM MASK CODE                                        *
***********************************************************************
           lgh R0,LTFLDLEN             LOAD COLUMN WIDTH
           LA  R14,LTVALUES            BUILD  MASK AFTER   VALUE
           agr R14,R0
           ST  R14,SAMSKADR
           STH R0,SAMSKLEN
           STH R0,LTV2LEN
*
           LA  R1,vdp2000b_REPORT_MASK USE  ORIGINAL MASK  CODE
           MVC WORKAREA(L'MASKCODE),0(R1)   SAVE     CODE
           BRAS R9,MASKCONV
           lg r9,lt_saver9           restore linkage
*
           DROP R15
*
           LTgR R15,R15             SUCCESSFUL ???
           JP  LTBLF1R
*
           STH R15,SAMSKLEN        NO  - ZERO MASK LENGTH
           J   LTBLF1U
*
LTBLF1R    equ  *
*
*  The mask code is in WORKAREA. It is a 5 char code :-
*
* Char 1: U/L/P/R = unsigned/left sign/parenthesis/right sign
* Char 2: N/Y     = No commas/Yes commas (thousand separators)
* Char 3: N/1->8  = Number of digits to display before the point
* Char 4: 0->8    = Number of decimal places to display
* Char 5: Y/N     = decimal point present
*
*  This info could be passed to GVBDL96 at some point...
*
*
           llgt R14,SAMSKADR       RESTORE MASK ADDRESS
*
           CLI WORKAREA,C'L'       SIGN ON LEFT ???
           JNE *+8
           MVI 0(R14),C'-'
*
           CLI WORKAREA,C'P'       SIGN ON LEFT ???
           JNE *+8
           MVI 0(R14),C'('
*
***********************************************************************
*    COMMA SEPARATED VARIABLE ("CSV") FILE                            *
***********************************************************************
           llgt R14,ltlognv
           ltgf R14,LT1000A-LOGICTBL(,R14)
           JNP LTBLF1T
*
           CLI vdp1000b_OUTPUT_MEDIA-vdp1000b_VIEW_RECORD+3(R14),CSV
           JE  LTBLF1S
*
***********************************************************************
*    FILE FORMAT OUTPUT FILE                                          *
***********************************************************************
          CLI vdp1000b_OUTPUT_MEDIA-vdp1000b_VIEW_RECORD+3(R14),FILEFMT
           JNE LTBLF1T
*
           CLI LTFLDFMT+1,FC_EDIT  EDITED NUMERIC ???
           JE  LTBLF1S             YES -  CHECK COMPATIBILITY
*
           CLI LTFLDFMT+1,FC_MASK  MASKED NUMERIC ???
           JE  LTBLF1T             YES -  ACCEPT MASK
           XC  SAMSKLEN,SAMSKLEN   IGNORE MASK UNLESS "MASKED NUMERIC"
           J   LTBLF1U
*
***********************************************************************
*    USE PROVIDED MASK IF IT'S "EDITED NUMERIC COMPATIBLE"            *
***********************************************************************
LTBLF1S    lgr R15,R0              "EDITED NUMERIC COMPATIBILITY" FLAG
           CLI 0(R15),C'Y'
           JE  LTBLF1T
*
           MVI SAOUTFMT+1,FC_EDIT  FORCE NUM FIELD TO "EDITED NUMERIC"
           XC  SAMSKLEN,SAMSKLEN   ZERO  MASK  LEN
           J   LTBLF1U
*
LTBLF1T    MVI LTFLDFMT+1,FC_ALNUM CHANGE TO ALPHANUMERIC  OUTPUT
           MVI SAOUTFMT+1,FC_ALNUM
*
***********************************************************************
*    BLANK OUT TARGET AREA                                            *
***********************************************************************
LTBLF1U    llgt R14,DL96TGTA       RESULT   ADDRESS
           LH  R15,DL96LEN
           BCTR R15,0              BLANK OUT TARGET  AREA
           EX  R15,BLNKOUT
*
***********************************************************************
*    CALL "GVBDL96" TO FORMAT CONSTANT                                *
***********************************************************************
LTBLF1W    LA  R1,DL96LIST
           llgf R15,GVBDL96A
           BASsm R14,R15
*
           LTR R15,R15
           JZ  LTBLF1Z
*
           CHI R15,2              TRUNCATION ???
           JE  LTBLF1Z
*
           lghi R14,DECIMAL_PLACES_GT_FIELD ERROR MESSAGE NUMBER
           cghi R15,13
           JE  STDERROR
           lghi R14,GENEVA_NO_DIG_EXCEEDED ERROR MESSAGE NUMBER
           J   STDERROR           PRINT ERROR MESSAGE - STOP
*
***********************************************************************
*    COPY CONSTANT "AS IS"                                            *
***********************************************************************
LTBLF1X    lg   R14,SAVALADR
           LH  R15,LTV1LEN
           LA  R0,LTVALUES
           LR  R1,R15
           MVCL R0,R14
*
LTBLF1Z    LH  R0,LTROWLEN
           AH  R0,LTV1LEN
           AH  R0,LTV2LEN
           STH R0,LTROWLEN
*
           J   LTBLV2S
                          EJECT
*
***********************************************************************
*    BUILD FORMAT 1 LOGIC TABLE ROW with a constant CFxC codes        *
***********************************************************************
LTBLFC     stg r9,lt_saver9
 LHI R0,LTF1_LEN
 STH R0,LTROWLEN
*
 MVC LTCOLNO,LTF1_SUFFIX_SEQ_NBR
 MVC LTCOLID,LTF1_COLUMN_ID
*
 MVC LTFLDDDN,HEXFF
 MVC LTFLDFIL,LTF1_FIELD_FILE_ID
 MVC LTFLDLR,LTF1_LR_ID
 MVC LTFLDPTH,SVLUPATH
 MVC LTFLDID,LTF1_FIELD_ID
 MVC LTFLDPOS,LTF1_START_POSITION
 MVC LTFLDSEQ,LTF1_ORDINAL_POSITION
 MVC LTFLDLEN,LTF1_FIELD_LENGTH
 MVC LTFLDFMT,LTF1_FIELD_FORMAT+2
 MVC LTFLDCON,LTF1_CONTENT_ID+2
*
 XC  LTV1LEN,LTV1LEN
 XC  LTV2LEN,LTV2LEN
***********************************************************************
*  COPY DECIMALS, ROUNDING, SIGNED, SORT SEQ, RELATIONAL COMPARE OPER *
***********************************************************************
 LH  R0,LTF1_NO_OF_DECIMALS
 STC R0,LTNDEC
*
 LH  R0,LTF1_ROUNDING
 STC R0,LTRNDFAC
*
 XR  R0,R0
 IC  R0,LTF1_SIGN_IND
 MVI LTSIGN,C'N'
 if CHI,R0,eq,SIGNED
   MVI LTSIGN,C'Y'
 endif
*
 MVI LTSRTSEQ,C'A'
 lgf R0,LTVIEW#
 if  (Ltgf,R1,LTCOLID,P)            only call locsort if +ve colid
   BRAS R9,LOCSORT
   lg r9,lt_saver9                   restore linkage
   if            (LTgR,R15,R15,P)    found?
     USING vdp2300b_SORT_KEY_ATTR_RECORD,R15
     if            (CLI,vdp2300b_SORT_SEQ_ID+3,EQ,DESCEND) and this set
       MVI LTSRTSEQ,C'D'
     endif
     DROP R15
   endif
 endif
*
 lgf R15,LTF1_COMPARE_TYPE
 SLLg R15,r15,1
 LA  R14,COMPTBL(R15)
 MVC LTRELOPR,0(R14)
*
 XC  LTSTRNO,LTSTRNO
 XC  LTSTRLVL,LTSTRLVL
 XC  LTJUSOFF,LTJUSOFF
*
***********************************************************************
*    CHECK FOR PRESENCE OF CONSTANT                                   *
***********************************************************************
 do ,
   if CLC,LTF1_VALUE_LENGTH,eq,ALLFUNC
*
     lghi R1,L'LTF1_VALUE              LOAD LOOP  COUNTER
     LA  R15,LTF1_VALUE+L'LTF1_VALUE  LOAD ADDR  OF LAST BYTE +1
*
     do ,label=test_blank2
       do from=(r1,,r14)
         BCTgR R15,0
         doexit CLI,0(R15),ne,C' ',do=test_blank2 xit if not blank
       enddo
       lghi R1,1                   ASSUME  1 CHARACTER
     enddo
***********************************************************************
*      SCAN VALUE FOR ESCAPE SEQUENCE ("\")                           *
***********************************************************************
       LA  R14,LTF1_VALUE     LOAD SOURCE ADDRESS
       LA  R15,LTF1_VALUE     LOAD TARGET ADDRESS
*
       if CLI,0(R14),eq,x'E0'         BACKSLASH ("\") ???
*
         if CLI,1(R14),eq,X'A7'       Not a LOWERCASE  "X"  ???
*
           la r14,2(,r14)       skip \x
           ahi r1,-2            adjust length
*
*          check for even number of digits
           if tml,r1,x'0001',o
             lghi R14,HEX_STRING_ERR
             J STDERROR
           endif
           lgr r0,r1              remember length
           jas r9,convert_hex
*       calc length of target field
           srlg r15,r0,1
         else
*
           lghi R14,HEX_STRING_ERR
           J STDERROR
         endif
       else
*
         do  from=(r1)
*
           MVC 0(1,R15),0(R14)      COPY  1 BYTE (MAY BE SAME BYTE)
           LA    R15,1(,R15)
           LA    R14,1(,R14)
         enddo
         LA      R1,LTF1_VALUE      COMPUTE FINAL LENGTH
         SgR     R15,R1
       endif
*
       lg r9,lt_saver9               restore linkage
*
       if CLC,LTF1_VALUE_LENGTH,eq,ALLFUNC  "ALL" FUNCTION  ???
*
         LgH    R1,LTFLDLEN        COMPUTE PROPAGATION  LENGTH (-1)
         SgR    R1,R15
         if AgHI,R1,-1,nm
           LA  R14,LTF1_VALUE
           LA  R15,LTF1_VALUE(R15)
           EX  R1,LTF1PROP
           LgH  R15,LTFLDLEN
         endif
*
       endif
       ST R15,LTF1_VALUE_LENGTH
*
***********************************************************************
*    ADJUST FIELD POSITION IF "ENDS WITH"                             *
***********************************************************************
       if CLC,LTRELOPR,eq,EW
*
         LgH R0,LTFLDLEN           LOAD     FIELD    LENGTH
         SgR R0,R15                SUBTRACT STRING   LENGTH
         lgh R14,LTFLDPOS          COMPUTE  POSITION FOR   "ENDS WITH"
         Agr R0,r14                COMPUTE  POSITION FOR   "ENDS WITH"
         STH R0,LTFLDPOS
       endif
*
***********************************************************************
* ADJUST FIELD LENGTH IF "BEGINS/ENDS" WITH                           *
***********************************************************************
       if CLC,LTRELOPR,eq,BW,or,                                       +
               CLC,LTRELOPR,eq,EW
         STH R15,LTFLDLEN
       endif
*
***********************************************************************
* INITIALIZE SOURCE VALUE LENGTH AND ADDRESS                          *
***********************************************************************
       LA      R0,LTF1_VALUE
   else
*
     if tm,ltf1_value_length,x'80',o
       la r1,ltf1_value_area   This is our parm to Cookie subrtn
       jas r9,Cookie       Call Cookie subroutine
       sty r1,LTF1_VALUE_LENGTH save the resolved length
       sth r0,cookie_content Save content type for DL96 call
       oi initflag,cookie_call  Signal cookie processing
     endif
     Lgf   R15,LTF1_VALUE_LENGTH
     if ltgr,r1,r15,p          COPY LEN TO  R1 and test
***********************************************************************
*      SCAN VALUE FOR ESCAPE SEQUENCE ("\")                           *
***********************************************************************
       LA  R14,LTF1_VALUE     LOAD SOURCE ADDRESS
       LA  R15,LTF1_VALUE     LOAD TARGET ADDRESS
*
       if CLI,0(R14),eq,x'E0'         BACKSLASH ("\") ???
*
         if CLI,1(R14),eq,X'A7'       Not a LOWERCASE  "X"  ???
           la r14,2(,r14)       skip \x
           ahi r1,-2            adjust length
*
*          check for even number of digits
           if tml,r1,x'0001',o
             lghi R14,HEX_STRING_ERR
             J STDERROR
           endif
           lgr r0,r1            remember length
           jas r9,convert_hex
*         calc length of target field
           srlg r15,r0,1
         else
           lghi R14,HEX_STRING_ERR
           J STDERROR
         endif
       else
*      not hex, just copy bytes
         do from=(r1)
           MVC 0(1,R15),0(R14)    COPY  1 BYTE (MAY BE SAME BYTE)
           LA  R15,1(,R15)
           LA  R14,1(,R14)
         enddo
         LA    R1,LTF1_VALUE      COMPUTE FINAL LENGTH
         SgR    R15,R1
*
       endif
       lg r9,lt_saver9               restore linkage
*
       if CLC,LTF1_VALUE_LENGTH,eq,ALLFUNC  "ALL" FUNCTION  ???
*
         LgH    R1,LTFLDLEN        COMPUTE PROPAGATION  LENGTH (-1)
         SgR    R1,R15
         if AgHI,R1,-1,nm
           LA  R14,LTF1_VALUE
           LA  R15,LTF1_VALUE(R15)
           EX  R1,LTF1PROP
           LgH  R15,LTFLDLEN
         endif
*
       endif
       ST R15,LTF1_VALUE_LENGTH
*
***********************************************************************
*    ADJUST FIELD POSITION IF "ENDS WITH"                             *
***********************************************************************
       if CLC,LTRELOPR,eq,EW
*
         LgH R0,LTFLDLEN           LOAD     FIELD    LENGTH
         SgR R0,R15                SUBTRACT STRING   LENGTH
         lgh R14,LTFLDPOS          COMPUTE  POSITION FOR   "ENDS WITH"
         Agr R0,r14                COMPUTE  POSITION FOR   "ENDS WITH"
         STH R0,LTFLDPOS
       endif
*
***********************************************************************
* ADJUST FIELD LENGTH IF "BEGINS/ENDS" WITH                           *
***********************************************************************
       if CLC,LTRELOPR,eq,BW,or,                                       +
               CLC,LTRELOPR,eq,EW
         STH R15,LTFLDLEN
       endif
*
***********************************************************************
* INITIALIZE SOURCE VALUE LENGTH AND ADDRESS                          *
***********************************************************************
       LA      R0,LTF1_VALUE
     else
       if clc,ltfunc,eq,lk_s      Is it build key with symbol
         mvc ltv1len,ltfldlen     Make sure length ids non-z
         j     ltblf1z            Get out of here
       endif
*
***********************************************************************
*      PROVIDE DEFAULT CONSTANT WHEN NONE PROVIDED                    *
***********************************************************************
       if CLI,LTSUBFUN+0,ne,C'C'        CONSTANT SHOULD BE PRESENT ???
         J LTBLF1Z                NO  - EXIT
       else
*
         LgHI R15,1                LENGTH OF DEFAULT CONSTANT
         if CLI,LTFLDFMT+1,eq,FC_ALNUM     ALPHANUMERIC ?
           LA  R0,SPACES
         else
           LA  R0,ZEROES          CHANGE DEFAULT TO ZERO
         endif
       endif
     endif
   endif
 enddo

 stg R0,SAVALADR
 STH R15,SAVALLEN
*
***********************************************************************
*    INITIALIZE TARGET ATTRIBUTES                                     *
***********************************************************************
 MVC SAOUTCON,LTFLDCON
 MVC SAOUTDEC+1(1),LTNDEC
 MVC SAOUTRND+1(1),LTRNDFAC
 MVC SAOUTSGN,LTSIGN
 MVI SAOUTJUS,C'L'

 if clc,=c'SF',eq,ltfunc       Is this SF? (should get SFCx and SFxC)
   mvc SAOUTFMT,=y(fc_alpha)     then all constants are alpha data
   L   r0,LTF1_VALUE_LENGTH
*
 else
   if cli,ltfldfmt+1,le,fc_alpha
     MVC SAOUTFMT,LTFLDFMT       RESULT ATTRIBUTES
     LgH R0,LTFLDLEN             RESULT LENGTH
   else
     MVC SAOUTFMT,=y(fc_float)   RESULT ATTRIBUTES
     Lghi R0,l'fc_float          RESULT LENGTH
     mvi  saoutsgn,c'Y'          Retain any sign
   endif
 endif
*
 XC  SAMSKLEN,SAMSKLEN
 XC  SAMSKADR,SAMSKADR
*
 LA  R14,LTVALUES            RESULT ADDRESS
 ST  R14,DL96TGTA
*
***********************************************************************
*    CONVERT DATE CONSTANTS TO SPECIFIED FORMAT (CONTENT CODE)        *
***********************************************************************
conv_const do ,                               convert constant

   ni initflag2,x'ff'-date_conv
   lgh  R1,LTFLDCON                 DATE  CONTENT CODE SPECIFIED ???
   if LTgR,R1,R1,p
*
*  If there is a content code, then the constant will be a date
*  in normalised form (unless it's a cookie ... ?)
*
*  Generally we format the date constant to match the date format of
*  the field.
*
*  If the field format is non-sortable format (e.g. DDMMYY) and we are
*  comparing using operators gt,lt,ge,le, we leave the date constant
*  in normalised format.
*
*  Also if the field is alpha, the date format contains slashes
*  or dashes, and we are comparing using operators gt,lt,ge,le
*  we leave the date constant in normalised format.
*
*  If it is either of the last to cases and we have a cookie, then we
*  convert the constant to normalised date format.
*
     lgh r1,LTRELOPR                   Get the operator
     if CH,r1,ne,=c'EQ',and,                                           +
               ch,r1,ne,=c'NE'
       lgh  R1,LTFLDCON                Get Date code again
*      Check for all the sortable date codes with no slashes or dashes
       if (cfi,R1,ne,YMD),and,         Sortable date format?           +
               (cfi,R1,ne,CYMD),and,                                   +
               (cfi,R1,ne,YYDDD),and,                                  +
               (cfi,R1,ne,CYDDD),and,                                  +
               (cfi,R1,ne,MMDD),and,                                   +
               (cfi,R1,ne,MM),and,                                     +
               (cfi,R1,ne,DD),and,                                     +
               (cfi,R1,ne,DDDD),and,                                   +
               (cfi,R1,ne,HMST),and,                                   +
               (cfi,R1,ne,HMS),and,                                    +
               (cfi,R1,ne,HM),and,                                     +
               (cfi,R1,ne,CYMDT),and,                                  +
               (cfi,R1,ne,SSSS),and,                                   +
               (cfi,R1,ne,CYM),and,                                    +
               (cfi,R1,ne,CCYY),and,                                   +
               (cfi,R1,ne,YY),and,                                     +
               (cfi,R1,ne,NORMDATE)
         oi initflag2,date_conv        Indiate must use date conv model

         if tm,initflag,cookie_call,o  Was there a Cookie?
*          convert to normalised date format
           MVC SAOUTCON,NORMDATE
           MVI SAOUTFMT+1,FC_NUM
           mvi saoutsgn,c'N'
           LHI R0,NORMDATE_l
         else
           lg   r1,savaladr
           mvc  ltvalues(NORMDATE_l),0(r1) Copy normalised date
           lhi  r0,NORMDATE_l
           STH  R0,LTV1LEN                 Save length of date
           leave conv_const            skip formating the constant
         endif
       endif
     endif

     MVI SAVALFMT+1,FC_NUM
     if tm,initflag,cookie_call,o   Was there a Cookie?
        mvc savalcon,cookie_content
        ni initflag,x'FF'-cookie_call   turn off flag
     else
       MVI SAVALCON+1,NORMDATE
     endif
     MVHHI SAVALDEC,0
     MVHHI SAVALRND,0
     MVI SAVALSGN,C'N'
*
   else
***********************************************************************
*      COMPLETE SOURCE VALUE ATTRIBUTES                               *
***********************************************************************
     if cli,ltfldfmt+1,le,fc_alpha
       MVC SAvalFMT,LTFLDFMT
     else
       if clc,=c'SF',eq,ltfunc   Is this SF? (should get SFCx and SFxC)
         mvc SAVALFMT,=y(fc_alpha)   then all constants are alpha data
       else
         MVC SAVALFMT,=y(fc_edit)
       endif
     endif
     xc SAVALCON,savalCON  (we know LTFLCON is zero here)
     MVHHI SAVALDEC,0
     MVHHI SAVALRND,0
     MVI SAVALSGN,C'Y'
   endif
*
   STH R0,LTV1LEN
   STH R0,DL96LEN
*
***********************************************************************
*      CALL "GVBDL96" TO FORMAT CONSTANT                              *
***********************************************************************
   LA          R1,DL96LIST
   llgf        R15,GVBDL96A
   BASsm R14,R15
*
   if LTR,R15,R15,P,and,chi,r15,ne,2
*
     if chi,r15,eq,13
       LgHI R14,DECIMAL_PLACES_GT_FIELD LOAD  ERROR MESSAGE  NUMBER
     else
       LgHI R14,GENEVA_NO_DIG_EXCEEDED LOAD  ERROR MESSAGE  NUMBER
     endif
     J        STDERROR           PRINT ERROR MESSAGE - STOP
   endif
*
 enddo ,                         End of convert constant

 LH  R0,LTROWLEN
 AH  R0,LTV1LEN
 AH  R0,LTV2LEN
 STH R0,LTROWLEN
*
 J   LTBLV2S
                          EJECT
***********************************************************************
*    LOAD 'F2' FORMAT RECORD (2 SETS OF FIELD ATTRIBUTES)             *
***********************************************************************
           USING LTF2_REC,R3
*
LTBLf2     stg r9,lt_saver9
           LHI R0,LTF2_LEN
           STH R0,LTROWLEN
*
           XC  LTAC2ADR,LTAC2ADR
*
***********************************************************************
*    MULTIPLE OCCURRENCE GROUP/FIELD VARIABLES                        *
***********************************************************************
           XC  LTCOLOCC,LTCOLOCC
           XC  LTOCCPOS,LTOCCPOS
           XC  LTOCCLEN,LTOCCLEN
           XC  LTOCCFMT,LTOCCFMT
           XC  LTSEGLEN,LTSEGLEN
*
           J   LTBLV2F2
                          EJECT
***********************************************************************
*    LOAD 'RE' READ FORMAT RECORD                                     *
***********************************************************************
           USING LTRE_REC,R3
*
ltblre   do ,
           stg r9,lt_saver9
           LHI R0,LTRE_LEN
           STH R0,LTROWLEN
*
           XC    ltlognv,ltlognv    ZERO   VIEW  ROW ADDR
           xc    ltreindx,ltreindx  Set to zero by default        pgc99
*
***********************************************************************
*    SET DEFAULTS FOR "FILE TYPE"/"ACCESS METHOD"/"HEADER OPTION"     *
***********************************************************************
           LHI R0,DISKDEV
           STH R0,LTFILTYP        EVENT  FILE   TYPE
           LHI R0,SEQFILE
           STH R0,LTACCMTH        ACCESS METHOD
           LHI R0,NOHDR
           STH R0,LTHDROPT        HEADER RECORD OPTION
*
***********************************************************************
*    INITIALIZE EVENT FILE ID AND READ EXIT FIELDS                    *
***********************************************************************
           MVC LTFILEDD,HEXFF
           MVC LTFILEID,LTRE_FILE_ID
           mvc LTTRACNT,bzero
*
           xc  ltre_next_exit,ltre_next_exit Next data area ptr to null
           XC  LTREEXIT,LTREEXIT
           MVC LTRENAME,SPACES
           XC  LTREENTP,LTREENTP
           XC  LTREWORK,LTREWORK
           MVC LTREPARM,SPACES
           la  r0,ltre_next_exit     address this area
           st  r0,exit_next          save in previous
           lgr r11,r0                and move pointer to this area
*
           XC  LTVDP200,LTVDP200
*
***********************************************************************
*    CHAIN "RE'S" WITHIN THIS "ES" SET                                *
***********************************************************************
           XC  LTNEXTRE,LTNEXTRE  ZERO   NEXT     "RE"   ROW ADDRESS
*
           llgt R14,CURRRE         SAVE   PREVIOUS "RE"   ROW ADDRESS
           ST  R7,CURRRE          UPDATE CURRENT  "RE"   ROW ADDRESS
           if ltgr,r14,r14,p       PREVIOUS  "RE"  ROW    EXISTS  ???
             ST R7,LTNEXTRE-LOGICTBL(,R14)   YES - CHAIN "RE"   ROWS
           endif
*
           if OC,FIRSTRE,FIRSTRE,z SAVE FIRST "RE" ROW WITHIN "ES" SET
             ST R7,FIRSTRE
           endif
*
***********************************************************************
*    CHAIN "RE_TK'S"                                                  *
***********************************************************************
           if CLC,LTFUNC,eq,re_TK READ   TOKEN  FUNCTION ???
*
             llgt R14,PREVRETK    SAVE   PREVIOUS "RETK" ROW ADDRESS
             ST R7,PREVRETK       UPDATE CURRENT  "RETK" ROW ADDRESS
             if ltgr,r14,r14,p     PREVIOUS  "RE"  ROW    EXISTS  ???
               ST R7,LTNEXTRE-LOGICTBL(,R14) YES - CHAIN "RE"   ROWS
             endif
*
             if OC,FRSTREtk,FRSTREtk,z SAVE FIRST "RE" ROW WITHIN "ES"
               ST R7,FRSTRETK
             endif
           endif
*
***********************************************************************
*    DB2 VARIABLES                                                    *
***********************************************************************
LTBLRE04   MVC LTEDITPR,SPACES
           XC  LTOBJID,LTOBJID
*
***********************************************************************
*    HEADER RECORD VARIABLES                                          *
***********************************************************************
           ZAP LTVERNO,P000       LOAD FROM VDP
           OI  LTVERNO+L'LTVERNO-1,X'0F'
           xc  LTCTLCNT,ltctlcnt  LOAD FROM VDP
*
***********************************************************************
*    EVENT FILE VARIABLES                                             *
***********************************************************************
           MVC LTVOLSER,SPACES    LOAD FROM VDP
           MVC LTDDNAME,SPACES
           MVC LTDSNAME,SPACES    LOAD FROM VDP
*
***********************************************************************
*    FIND CORRESPONDING FILE RECORD IN VDP                            *
***********************************************************************
           llgt R14,VDPBEGIN       SEARCH  "VDP"
           USING vdp0200b_FILE_RECORD,R14,R15
           lgf R9,VDPCOUNT
*
vdp_rdloop do ,
             do from=(r9)
               LH R0,vdp0200b_RECORD_TYPE
               if CHI,R0,eq,VDPFILEI
*
                 doexit CLC,LTFILEID,eq,vdp0200b_RECORD_ID,            +
               do=vdp_rdloop
*
               endif
               ltg R14,vdp0200b_NEXT_PTR
               jnp vdp_rdloop_err
             enddo
*
vdp_rdloop_err equ *
             L R0,LTFILEID
             CVD R0,DBLWORK
             OI DBLWORK+L'DBLWORK-1,X'0F'
             MVI LTDDNAME+0,C'I'
             UNPK LTDDNAME+1(7),DBLWORK
*
             leave ltblre
           enddo
*
***********************************************************************
*    MATCHING FILE ID FOUND IN "VDP"                                  *
***********************************************************************
           lgr R15,R14            SET-UP SECOND BASE REGISTER FOR VDP
           aghi R15,4096
*
           ST  R14,LTVDP200       SAVE   VDP  "200"  RECORD   ADDRESS
*
           ST  R7,vdp0200b_FILE_READER
           mvc   ltreindx,vdp0200b_server_id
*
           L   R0,vdp0200b_ALLOC_FILE_TYPE
           STH R0,LTFILTYP        COPY FILE   TYPE   FROM "VDP"
*
           L   R0,vdp0200b_ACCESS_METHOD_ID
           STH R0,LTACCMTH        COPY ACCESS METHOD FROM "VDP"
*
           MVC LTDDNAME,vdp0200b_DDNAME_INPUT
*
           MVC LTDSNAME,vdp0200b_DSN
           MVC LTVOLSER,vdp0200b_ALLOC_VOL_SER
*
*          CHECK IF THERE WAS AN ERROR AT VDP LOAD TIME
*
           IF (OC,VDP0200B_ERROR,VDP0200B_ERROR,NZ)
             MVC  ERRDATA(32),VDP0200B_ERRDATA
             LGF  R14,VDP0200B_ERROR
             B RTNERROR
           ENDIF
*
*
*          LR  R15,R7
*          AH  R15,0(,R7)
*          BCTR R15,0
*          ly  r2,SNAPDCBA
*          SNAP DCB=(r2),ID=102,PDATA=(REGS),STORAGE=((R7),(R15))
*
***********************************************************************
*    LOCATE VDP EXIT PROGRAM RECORD IF FILE INDICATES EXIT PROGRAM    *
***********************************************************************
           doexit Ltgf,R1,LTRE_READ_EXIT_ID,np,do=ltblre
*
           ST  R1,LTREEXIT
           MVC LTREPARM,vdp0200b_INPUT_EXIT_STARTUP_PARM
*
           DROP R14,R15
*
           BRAS R9,LOCEXIT        LOCATE VDP EXIT PGM RECORD (210)
           USING vdp0210b_EXIT_PGM_RECORD,R15
           if  LTgR,R15,R15,P      found?
*
             MVC LTRENAME,vdp0210b_MODULE_NAME
             if CLC,LTRENAME,eq,gvbXRCK
               OI LTFLAG2,LTNOMODE INDICATE NO SRB/TCB MODE SWITCH
             endif
*
           endif
           DROP R15
         enddo
*
* Count the number of physical files (PF) in this logical file (LF).
*  The VDP 200 records represent a PF, and the VDP0200 records with
*   matching LF ids are chained together via vdp0200b_MULT_PART_CHAIN
*
         XC  LTREPFcnt,LTREPFcnt
         xr  r1,r1
         using vdp0200b_FILE_RECORD,R14
         if ltgf,r14,ltvdp200,nz     get first PF VDP record
           do until=(ltgf,r14,VDP0200b_mult_part_chain,np) Loop thro PF
             AHI r1,1
           enddo
         endif
         st  r1,ltrePFcnt
*
         lg r9,lt_saver9
         br    r9                 ADVANCE TO NEXT ROW
         eject ,
*
*     this routine will index into the array pointed to by r5
*     the array is effectively 4 arrays:
*                source unsigned       source signed
*       target   1                     2
*     unsigned
*
*       target   3                     4
*       signed
*
*     so test the target sign and if Y then add 2*array_size
*        test the souce sign and if Y then add array_size
*     this will position R5 at one of the points 1  -  4 above
*     then the
*     process is get source format,
*                reduce by one
*                multiply by 4
*                same process for target format
*                then multiply the target index by 0.5 * array row len
*                add source index
*                load the pointer from the array
*     there is an extra step for binary formats - the pointer in the
*     array will be to the bin124/bin124 entry and in the function
*     table this will be followed by the bin124/bin8, bin8/bin124
*     and bin8/bin8 entries (that is source/target) if both binary
*     If only one is binary then the bin8 entry will follow
*
*     process at the end is
*                check both source and target formats for binary
*                or sortable binary.
*                if true then check source length for
*                  greater than 7 (machine lengths here).
*                  if true, then skip two entries
*                  if the target length is greater than 7
*                     skip one more entry
*                else (not both bin|sortb)
*                  check the target fmt for binary and the length for
*                  greater than 7 (machine lengths here).
*                  if true, skip one entry
*
*     The default function entry is returned in r15, for later
*     processing if required (used if there are content codes in the
*     LT entry - signalling date processing)
*
array_select   ds 0h
               llgt r15,0(,r5)           load r15 with the default
               if cli,ltcolsgn,eq,C'Y' is the target signed?
                 aghi r5,array_size*2 move to the signed target arrays
               endif
               if cli,ltsign,eq,C'Y' is the source signed?
                 aghi r5,array_size     move to the signed source array
               endif
               xgr r0,r0
               ic r0,ltfldfmt+1      get the source format
               sllg r0,r0,2              and multiply by 4 (ent size)
*              source index value in r0
               xgr r6,r6
               ic r6,ltcolfmt+1      get the target format
*              Now multiply by row length
               ms      r6,=a(row_length)
               agr     r6,r0
               if ltgf,r5,0(r6,r5),z   and select the entry, setting cc
                 lgr r5,r15              zero selected so use default
               endif
               if (cli,ltfldfmt+1,eq,fc_bin),and,     binary source?   +
               (cli,ltcolfmt+1,eq,fc_bin),orif,       and binary target?
               (cli,ltfldfmt+1,eq,fc_sortb),and,      bsort source?    +
               (cli,ltcolfmt+1,eq,fc_sortb),orif,    and bsort target? +
               (cli,ltfldfmt+1,eq,fc_bin),and,        binary source?   +
               (cli,ltcolfmt+1,eq,fc_sortb),orif,     and bsort target?+
               (cli,ltfldfmt+1,eq,fc_sortb),and,      bsort source?    +
               (cli,ltcolfmt+1,eq,fc_bin)            and binary target?
                 if cli,ltfldlen+1,ge,7  bin 8 source?
                   aghi r5,fcentlen*2    then skip two entry
                 endif

                 if cli,ltcollen+1,ge,7  bin 8 target ?
                   aghi r5,fcentlen      then skip one entries
                 endif
               elseif  cli,ltcolfmt+1,eq,fc_bin,and, bin target&b8     +
               cli,ltcollen+1,ge,7,orif,            or if              +
               cli,ltfldfmt+1,eq,fc_bin,and,  binary source            +
               cli,ltfldlen+1,ge,7,orif,  and bin 8                    +
               cli,ltcolfmt+1,eq,fc_sortb,and, bsort target & b8       +
               cli,ltcollen+1,ge,7,orif,            or if              +
               cli,ltfldfmt+1,eq,fc_sortb,and, bsort                   +
               cli,ltfldlen+1,ge,7  and bin 8
                   aghi r5,fcentlen      then skip one entry
               endif
               br r9
         eject ,
*
*     this routine will index into the vector pointed to by r5
*     using the source format.
*     if signed then move to second half of vector
*     process is get source format,
*                and multiply by 4
*                load the pointer from the vector using index value
*     there is an extra step for binary formats - the pointer in the
*     array will be to the bin124 entry and in the function
*     table this will be followed by the bin8 entry
*     process at the end is
*                check the source fmt for binary and the length for
*                greater than 7 (machine lengths here).
*                if true, skip one function table entry
*
*     The default function entry is returned in r15, for later
*     processing if required (used if there are content codes in the
*     LT entry - signalling date processing)
*
vector_select  ds 0h
               llgt r15,0(,r5)         load r15 with the default
               xgr r6,r6

               if ltgr,r0,r0,m        is r0 negative?                  +
                                     which means we are using vvfmt
                 ic r6,ltvvfmt+1    get the source format
               else ,
                 if (z)
                   ic r6,ltcolfmt+1  get the source format
                   if cli,ltcolsgn,eq,C'Y' is the source signed?
                     aghi r5,row_length     move to right hand half
                   endif
                 else ,              must be +ve
                   ic r6,ltfldfmt+1 get the source format
                   if cli,ltsign,eq,C'Y' is the source signed?
                     aghi r5,row_length     move to right hand half
                   endif
                 endif
               endif

               sllg r6,r6,2              multiply by ent size
*              source index value in r6
               if ltgf,r5,0(r6,r5),z   and select the entry, setting cc
                 lgr r5,r15              zero selected so use default
               endif

               if ltgr,r0,r0,m       is r0 negative?                   +
                                     which means we are using vvfmt
                 if   (cli,ltvvfmt+1,eq,fc_bin),or,   binary source?   +
               (cli,ltvvfmt+1,eq,fc_sortb),andif,     binary sortable  +
               cli,ltvvlen+1,ge,7  and bin 8
                   aghi r5,fcentlen      then skip one entry
                 endif
               else ,
                 if (z)
                   if  (cli,ltcolfmt+1,eq,fc_bin),or, binary source?   +
               (cli,ltcolfmt+1,eq,fc_sortb),andif,    binary sortable  +
               cli,ltcollen+1,ge,7 and bin 8
                     aghi r5,fcentlen    then skip one entry
                   endif
                 else ,              must be +ve
                   if  (cli,ltfldfmt+1,eq,fc_bin),or, binary source?   +
               (cli,ltfldfmt+1,eq,fc_sortb),andif,    binary sortable  +
               cli,ltfldlen+1,ge,7 and bin 8
                     aghi r5,fcentlen    then skip one entry
                   endif
                 endif
               endif
               br r9

*
* check for non-sortable date fields with operators that are not
* 'eq' or 'ne' - have to use normalised dates
*
date_norm_cfxc_select ds 0h
               if tm,initflag2,date_conv,o  Must use date model?
                  llgt r5,cfxc_date_model   addr cfxc date model
               endif
               br r9

date_norm_cfcx_select ds 0h
               if tm,initflag2,date_conv,o  Must use date model?
                  llgt r5,cfcx_date_model   addr cfcx date model
               endif
               br r9

static           loctr
cfxc_date_model  dc v(cfxcdate)
cfcx_date_model  dc v(cfcxdate)
code             loctr
                          EJECT
***********************************************************************
*    LOAD 'LU' - LOOK-UP LOGICAL RECORD FORMAT                        *
***********************************************************************
           USING LTRE_REC,R3
*
LTBLlu     stg r9,lt_saver9
           lghi R14,UNEXPECTED_FUNCTION_CODE
           CLC LTMAJFUN,LU_SM     LOOK-UP FUNCTION  ???
           JNE STDERROR
*
           LHI R0,LTLU_LEN
           STH R0,LTROWLEN
*
           MVC LTLUFILE,SVLUFILE
           MVC LTLULRID,SVLULRID
           MVC LTLUPATH,SVLUPATH
           mvc LTLUWPATH,svluwpath
           xc  ltlu_next_exit,ltlu_next_exit Next data area ptr to null
           XC  LTLUEXIT,LTLUEXIT
           MVC LTLUNAME,SVLUSUBR
           MVC LTLUOPT,SVLUOPT
           XC  LTLUADDR,LTLUADDR
           XC  LTLUENTP,LTLUENTP
           XC  LTLUWORK,LTLUWORK
           MVC LTLUPARM,SVLUPARM
           la  r0,ltlu_next_exit     address this area
           st  r0,exit_next          save in previous
           lgr r11,r0                and move pointer to this area
*
           CLC LTFUNC,LU_SM       SEARCH MEMORY LOOK-UP FUNCTION  ???
           JE  LTBLLU50           YES -  NO  EXIT  USED
*
***********************************************************************
*    LOCATE VDP "LR" RECORD TO SEE IF LOOK-UP EXIT SPECIFIED          *
***********************************************************************
           CLC LTLUNAME,SPACES    LOOK-UP EXIT ALREADY LOCATED ???
           JNE LTBLLU50           YES -  BYPASS SEARCH
*
           llgt R14,VDPBEGIN       SEARCH  "VDP"
           USING vdp0300b_LR_RECORD,R14
*
           lgf R15,VDPCOUNT
*
LTBLLU05   LH  R0,vdp0300b_RECORD_TYPE
           CHI R0,VDPLR
           JNE LTBLLU10
*
           CLC LTLULRID,vdp0300b_RECORD_ID
           JE  LTBLLU20
*
LTBLLU10   DS  0H
           ltg R14,vdp0300b_NEXT_PTR
           Jnp LTBLLU50
           BRCT R15,LTBLLU05
*
           J   LTBLLU50
*
***********************************************************************
*    LOCATE VDP EXIT PROGRAM RECORD IF "LR" INDICATES EXIT PROGRAM    *
***********************************************************************
LTBLLU20   ltgf R1,vdp0300b_EXIT_PGM_ID
           JNP LTBLLU50
*
           MVC LTLUPARM,vdp0300b_EXIT_STARTUP
           MVC SVLUPARM,LTLUPARM
*
           DROP R14
*
           BRAS R9,LOCEXIT        LOCATE VDP EXIT PGM RECORD (210)
           USING vdp0210b_EXIT_PGM_RECORD,R15
           LTgR R15,R15
           JNP LTBLLU50
*
           MVC LTLUNAME,vdp0210b_MODULE_NAME
           MVC LTLUptyp,vdp0210b_program_type
           mvc ltluopt,vdp0210b_optimizable
           MVC SVLUSUBR,LTLUNAME
           MVC SVLUOPT,LTLUOPT
*
           DROP R15
*
***********************************************************************
*    CHAIN "LU" TO PREVIOUS "LU" ROWS WITHIN CURRENT VIEW             *
***********************************************************************
LTBLLU50   XC  LTLUNEXT,LTLUNEXT  ZERO   NEXT     "LU" ROW ADDRESS
           llgt R14,PREVLU         SAVE   PREVIOUS "LU" ROW ADDRESS
           st  R7,PREVLU          UPDATE PREVIOUS "LU" ROW WITH CURRENT
           if ltgr,R14,R14,P       DOES   PREVIOUS  EXIST   ???
             st  R7,LTLUNEXT-LOGICTBL(,R14) YES -  CHAIN   TO  PREVIOUS
           endif
*
***********************************************************************
*  UPDATE FIRST "LU" ROW POINTER WITHIN VIEW ("NV") IF NOT ALREADY SET*
***********************************************************************
           llgt R14,ltlognv
           if    OC,LTFRSTLU-LOGICTBL(L'LTFRSTLU,R14),LTFRSTLU-LOGICTBL+
               (R14),z
             st  R7,LTFRSTLU-LOGICTBL(,R14)
           endif
           lg r9,lt_saver9
           br  r9                 ADVANCE  TO NEXT     ROW
                          EJECT
***********************************************************************
*    LOAD 'WR' - WRITE EXTRACT RECORD FORMAT                          *
***********************************************************************
           USING LTWR_REC,R3
*
LTBLWR     stg r9,lt_saver9
           LHI R0,LTWR_LEN
           STH R0,LTROWLEN
*
           XC  LTWREXT#,LTWREXT#
*
           MVC LTWRFILE,HEXFF
           MVC LTWRFID,LTWR_OUTPUT_FILE_ID
*
           XC  LTWR200A,LTWR200A
           XC  LTWREXTA,LTWREXTA
*
           MVC LTWREXIT,LTWR_WRITE_EXIT_ID
           MVC LTWRNAME,SPACES
           xc  ltwr_next_exit,ltwr_next_exit Next data area ptr to null
           XC  LTWRADDR,LTWRADDR
           XC  LTWRENTP,LTWRENTP
           MVC LTWRPARM,LTWR_WRITE_EXIT_PARMS
           la  r0,ltwr_next_exit     address this area
           st  r0,exit_next          save in previous
           lgr r11,r0                and move pointer to this area
*
           MVC LTWRDEST,LTWR_DEST_TYPE
*
***********************************************************************
*    CHECK IF EXTRACT TIME SUMMARIZATION SPECIFIED                    *
***********************************************************************
           L   R0,LTWR_EXTR_SUM_REC_CNT
           ST  R0,LTWRSUMC
*
           if LTR,R0,R0,p
             llgt R1,ltlognv
           endif
           OI  LTFLAG2-LOGICTBL(R1),LTEXTSUM
*
***********************************************************************
*    UPDATE EXTRACT LIMIT IF OVERRIDE SPECIFIED                       *
***********************************************************************
           mvc LTWRLMT,bnines
*
           llgt R1,ltlognv           LOCATE VDP VIEW ("1000") RECORD
           ltgf R15,LT1000A-LOGICTBL(,R1)
           JNP ltblwr01             NO  -  BYPASS OVERRIDE
         LT R0,vdp1000b_EXTRACT_MAX_REC_COUNT-vdp1000b_VIEW_RECORD(R15)
           JNP ltblwr01
           stg r0,LTWRLMT
*
***********************************************************************
*    SAVE "FLAGGED" BASE VIEW ID FOR EXTRACT WRITES                   *
***********************************************************************
ltblwr01   ds  0h
           L   R0,LTVIEW#
           SLL R0,1
           AHI R0,1
           ST  R0,LTWRVIEW
*
***********************************************************************
*    CHAIN "WR" TO PREVIOUS "WR" ROWS WITHIN CURRENT VIEW             *
***********************************************************************
           XC  LTWRNEXT,LTWRNEXT  ZERO   NEXT     "WR" ROW ADDRESS
           llgt R14,PREVWR         SAVE   PREVIOUS "WR" ROW ADDRESS
           ST  R7,PREVWR          UPDATE PREVIOUS "WR" ROW WITH CURRENT
           LTgR R14,R14            DOES   PREVIOUS  EXIST   ???
           JNP ltblwr02                     NO  -  BYPASS  CHAINING
           ST  R7,LTWRNEXT-LOGICTBL(,R14)   YES -  CHAIN   TO  PREVIOUS
*
***********************************************************************
*    UPDATE FIRST "WR" ROW POINTER WITHIN "NV" ROW IF NOT ALREADY SET *
***********************************************************************
ltblwr02   ds  0h
           llgt R14,ltlognv
           OC  LTFRSTWR-LOGICTBL(L'LTFRSTWR,R14),LTFRSTWR-LOGICTBL(R14)
           JNZ ltblwr03
           ST  R7,LTFRSTWR-LOGICTBL(,R14)
*
***********************************************************************
*    FIND CORRESPONDING OUTPUT FILE RECORD IN VDP (IF EXPECTED)       *
***********************************************************************
ltblwr03   ds  0h
           LT  R0,LTWR_DEST_TYPE
           JNP LTBLWR12
*
           llgt R14,VDPBEGIN       SEARCH  "VDP"
           USING VDP0200b_FILE_RECORD,R14,R15
           lgf R9,VDPCOUNT
*
           lgf R1,LTWRFID         LOAD PARTITION ID (IF ANY)
*
LTBLWR05   lgh R0,VDP0200b_RECORD_TYPE
*
           LTR R1,R1              PARTITION PRESENT ???
           JP  LTBLWR07
*
***********************************************************************
*    WRITING TO EXTRACT FILE (VDP 1600 RECORD)                        *
***********************************************************************
           CgHI R0,VDPFILEO
           JNE LTBLWR10
*
*          USING VDP1600_SUM_OUT_FILE_RECORD,R14,R15
*     1600 record is the same format as the 200b record
           USING VDP0200B_FILE_RECORD,r14
           CLC LTVIEW#,VDP0200b_VIEW_id
           JNE LTBLWR10
*
     MVC LTWRFILE,VDP0200b_DDNAME_OUTPUT
     st r14,ltwr200a
*
*          USING VDP0200b_FILE_RECORD,R14,R15
*
           J   LTBLWR30
*
***********************************************************************
*    WRITING TO PHYSICAL PARTITION (VDP 200 RECORD)                   *
***********************************************************************
LTBLWR07   CHI R0,VDPFILEP
           JNE LTBLWR10
*
           C   R1,VDP0200b_INPUT_FILE_ID
           JE  LTBLWR20
*
LTBLWR10   DS  0H
           ltg R14,VDP0200b_NEXT_PTR
           jnp LTBLWR12
           BRCTg R9,LTBLWR05
*
***********************************************************************
*    VDP RECORD NOT FOUND (BUILD DEFAULT DDNAME)                      *
***********************************************************************
LTBLWR12   MVC LTWRFILE,SPACES
           MVC LTWRFILE(L'EXTR),EXTR

           LH  R0,LTWR_SUFFIX_SEQ_NBR
           STH R0,LTWREXT#
           CVD R0,DBLWORK
           OI  DBLWORK+L'DBLWORK-1,X'0F'
           CHI R0,999                      THREE OR FOUR DIGIT ???
           JH  *+14
           UNPK LTWRFILE+L'EXTR(3),DBLWORK BUILD DDNAME SUFFIX (3)
           J   LTBLWR30
*
           UNPK LTWRFILE+L'EXTR(4),DBLWORK BUILD DDNAME SUFFIX (4)
           J   LTBLWR30
*
LTBLWR15   LA  R14,WRITE_EXIT_LOAD_ERR  Load err MESSAGE NUMBER
           llgt R1,THRDMAIN
           MVC ERRDATA-THRDAREA(8,R1),LTWRNAME  INDICATE PROGRAM NAME
           J   RTNERROR           PRINT ERROR MESSAGE - STOP
*
***********************************************************************
*    MATCHING FILE ID FOUND IN "VDP"                                  *
***********************************************************************
LTBLWR20   lgr R15,R14            SET-UP SECOND BASE REGISTER FOR VDP
           aghi R15,4096
*
           ST  R14,LTWR200A       SAVE   OUTPUT FILE ATTRIBUTES   PTR
           MVC LTWRFILE,VDP0200b_DDNAME_OUTPUT
*
           DROP R14,R15
*
***********************************************************************
*    ASSIGN EXTRACT FILE ENTRY IF NEW OUTPUT FILE DDNAME              *
***********************************************************************
LTBLWR30   llgt R1,EXTFILEA        LOAD FIRST EXTRACT FILE ENTRY ADDR
           USING EXTFILE,R1
*
           LgHI R15,1              LOAD CURRENT  SUBSCRIPT VALUE
*
           LH  R0,FILECNT         LOAD LOOP     COUNTER
           LTR R0,R0              ANY  ENTRIES  ASSIGNED  ???
           JNP LTBLWR33           NO  - ASSIGN  FIRST
*
LTBLWR32   ds  0h
           if  CLC,LTWRFILE,eq,extddnam sAME FILE ID/DDNAME ???
             ST R1,LTWREXTA       SAVE   EXTRACT FILE ENTRY ADDRESS
             STH R15,LTWREXT#     SAVE   SUBSCRIPT
             j   ltblwr35
           endif
*
           LA  R1,EXTFILEL(,R1)   ADVANCE TO NEXT FILE ENTRY
           AgHI R15,1
           BRCT R0,LTBLWR32       CONTINUE SEARCH
*
LTBLWR33   CH  R15,MAXEXTF#       TABLE  OVERFLOW  ???
           JNH LTBLWR34           NO  -  CONTINUE
*
           LgHI R14,EXTFILE_TABLE_OVERFLOW msg number
           J   RTNERROR
*
LTBLWR34   STH R15,FILECNT        UPDATE CURRENT ENTRY COUNT
*
           MVC EXTDDNAM,LTWRFILE  SAVE   DDNAME
           MVC EXTVDPA,LTWR200A
*
           ST  R1,EXTPRINT        INITIALIZE  PRINT    SEQUENCE
*
           xc  extcnt,extcnt
           xc  extbytec,extbytec
*
           ST  R1,LTWREXTA        SAVE   EXTRACT FILE ENTRY ADDRESS
           OC  LTWREXT#,LTWREXT#
           JNZ *+8
           STH R15,LTWREXT#       SAVE   SUBSCRIPT
*
ltblwr35 ds    0h                                                pgc110
           llgt R14,ltlognv           LOCATE VDP VIEW ("1000") RECORD
wrnv       using logictbl,r14
           lb r14,wrnv.ltviewtp            .Get view type
           if chi,r14,eq,vtype_summary,or,chi,r14,eq,vtype_detail
             oi extflag,extfmtph  Y:signal this DD going format phase
           endif
           drop wrnv
ltblwr36 ds    0h                                                pgc110
           llgt R14,LTWR200A       VERSION  4 VDP RECORD ADDRESS
           LTgR R15,R14
           JNP LTBLWR39
           AgHI R15,4096
           USING VDP0200b_FILE_RECORD,R14,R15
*
           ST  R1,VDP0200b_EXTFILE_ADDR
*
***********************************************************************
*    SET PIPE ATTRIBUTES IF A PIPE                                    *
***********************************************************************
           L   R0,VDP0200b_ALLOC_FILE_TYPE  LOAD EXTRACT FILE TYPE
           if CHI,R0,eq,PIPEDEV
*
             if oc,ltwrexit,ltwrexit,z,and, No write exit and          +
               CLC,LTFUNC,eq,wr_su,and,    WR_SU and                   +
               lt,r14,ltwrsumc,np          ltwrsumc is not positive
               lhi r14,1        make sure that ltwrsumc
               st r14,ltwrsumc is at least one
             endif
*
             llgt R14,LTWR200A          RESTORE VDP RECORD ADDRESS
             LgR R15,R14
             AgHI R15,4096
             USING VDP0200b_FILE_RECORD,R14,R15
*
             MVI EXTRECFM,dcbrecv+dcbrecbr ASSUME  RECFM = VB
             if CLI,VDP0200b_ALLOC_RECFM+3,eq,RECFMFB,or,              +
               CLI,VDP0200b_ALLOC_RECFM+3,eq,RECFMFBA
               MVI EXTRECFM,dcbrecf+dcbrecbr CHANGE RECFM = FB
             endif
*
             LH R0,VDP0200b_MINIMUM_LENGTH  RECORD  LENGTH - MIN
             if LTR,R0,R0,np
               LH R0,VDP0200b_ALLOC_LRECL   LOGICAL RECORD LENGTH
               if LTR,R0,R0,np
*
                 LHI R0,EXTRECL           DEFAULT  VB LRECL
                 if TM,EXTRECFM,dcbrecf,o   FIXED BLOCK ???
*
                   llgt R9,ltlognv
                   LH R0,LTDATALN-LOGICTBL(,R9)
*
                 endif
               endif
             endif
             STH R0,EXTLRECL
*
             LH R15,VDP0200b_MAXIMUM_LENGTH RECORD LENGTH - MAX
             if LTR,R15,R15,p,and,        VALUE   PRESENT  ???         +
               TM,EXTRECFM,dcbrecf,o      FIXED   BLOCK ???
               STH R15,EXTLRECL           CHANGE  LRECL
             endif
*
             if LT,R14,VDP0200b_ALLOC_BLKSIZE,np DESIRED BLOCK SIZE
               LHI R14,maxblksi           DEFAULT BLOCK SIZE
             endif
*
             DROP R14,R15
             if CHI,R14,gt,32767
               LHI R14,32767
             endif
*
             STH R14,EXTBLKSI             ASSUME DEFAULT VB BLKSIZE
*
             if TM,EXTRECFM,dcbrecf,o     FIXED BLOCK  ???
               SRDL R14,32            COMPUTE MAXIMUM RECORDS PER BLK
               LH R0,EXTLRECL
               DR R14,R0
               MR R14,R0
               STH R15,EXTBLKSI           ASSUME DEFAULT VB BLKSIZE
             endif
             BRAS  R9,PIPELIST        BUILD LIST OF PIPES FOR  "ES" SET
           endif
           DROP R1
*
***********************************************************************
*    LOCATE VDP EXIT PROGRAM RECORD IF VIEW INDICATES WRITE EXIT      *
***********************************************************************
LTBLWR39   ltgf R1,LTWREXIT
           JNP LTBLWR99
*
           BRAS R9,LOCEXIT        LOCATE VDP EXIT PGM RECORD (210)
           USING vdp0210b_EXIT_PGM_RECORD,R15
           LTgR R15,R15
           JNP LTBLWR99
*
           MVC LTWRNAME,vdp0210b_MODULE_NAME
           MVC ltwrpgm_type,VDP0210b_PROGRAM_TYPE
*
           DROP R15
*
           LA  R9,LTWRNAME        POINT TO SUBROUTINE NAME
           LOAD EPLOC=(R9),ERRET=LTBLWR15
           ST  R0,LTWRADDR        SAVE  SUBROUTINE ADDRESS (31-BIT)
           ST  R0,LTWRENTP        SAVE  TRUE ENTRY POINT
*
           llgtr R1,R0              CEE   SUBROUTINE   ???
           CLC 05(3,R1),LECOBOL
           JNE ltblwr99           NO  - BYPASS  LE   INITIALIZATION
           L   R0,WRTXADDR        SUBSTITUTE COMMON  LANG INTERFACE
           O   R0,MODE31
           ST  R0,LTWRADDR
*
*
LTBLWR99   EQU *
           lg r9,lt_saver9
           br  r9                 ADVANCE  TO NEXT     ROW
                          EJECT
***********************************************************************
*    LOAD VARIABLE NAME RECORD                                        *
***********************************************************************
           USING LTVN_REC,R3
*
LTBLVN     stg r9,lt_saver9
           LHI R0,LTVN_LEN
           STH R0,LTROWLEN
*
           XC  LTVNLEN,LTVNLEN    ZERO VALUE     LEN
           XC  LTVNFMT,LTVNFMT    ZERO FORMAT    CODE
           XC  LTVNCON,LTVNCON    ZERO CONTENT   CODE
           MVI LTVNNDEC,0         ZERO NO. OF    DECIMAL    PLACES
           MVI LTVNSIGN,C'Y'      SET  SIGNED    INDICATOR
*
           MVC LTVNNAME,LTVN_TABLE_NAME
           MVC LTVNCOL#,LTVN_SUFFIX_SEQ_NBR
*
           XC  LTVNNEXT,LTVNNEXT  ZERO NEXT NAME POINTER
           mvc ltvnlitp,invalid_offset set offset to invalid value
*
           llgt R15,GLOBVNAM       LOAD GLOBAL VARIABLE NAME LIST ADDR
           if  ltgf,R14,CURRNV,P
currnv_lt_ptr using logictbl,r14
             llgt R15,currnv_lt_ptr.LTNVVNAM
             OI LTFLAG2,LTLOCAL   INDICATE LOCAL VARIABLE  (VIEW)
           endif
*
           LA  R1,LTVN_TABLE_NAME
           BRAS R9,VNAMSRCH
           lg r9,lt_saver9
           if LTgR,R15,R15,p
*
             llgt R0,LTACADDR-LOGICTBL(,R15) LOAD ACCUMULATOR  ADDRESS
             ST R0,LTACADDR
           else
*
             if CLC,LTFUNC,eq,CT_A
*
               LgHI R14,ACCUM_NOT_FOUND
               J STDERROR
*
             else
               if LTgR,R14,R14,p          ANY  NAMES  ON LIST ???
                ST R7,currnv_lt_ptr.LTVNNEXT YES - ADD NEW NAME TO LIST
*
               else
                 LA R15,GLOBVNAM  LOAD GLOBAL VARIABLE NAME LIST ADDR
                 if LTgf,R14,CURRNV,p
                   LA R15,currnv_lt_ptr.LTNVVNAM
                 endif
                 ST R7,0(,R15)
               endif
*
               ST R7,LTACADDR     INITIALIZE ACCUMULATOR ROW ADDRESS
*
               XgR R0,R0
*
               if clc,=c'DIM',eq,ltfunc
                 select cli,ltsubfun+1,eq Look at the last character
                   WHEN c'1'      VARIABLE - BOOLEAN
                     LHI R0,FC_BIN
                     STH R0,LTVNFMT
                     LHI R0,1
                     STH R0,LTVNLEN
*
                   when c'2'      VARIABLE - BINARY HALFWORD
                     LHI R0,FC_BIN
                     STH R0,LTVNFMT
                     LHI R0,2
                     STH R0,LTVNLEN
*
                   when c'4'      VARIABLE - BINARY FULLWORD
                     LHI R0,FC_BIN
                     STH R0,LTVNFMT
                     LHI R0,4
                     STH R0,LTVNLEN
*
                   when c'8'      VARIABLE - BINARY doubleword
                     LHI R0,FC_BIN
                     STH R0,LTVNFMT
                     LHI R0,8
                     STH R0,LTVNLEN
*
                   when c'N'      VARIABLE - Long DFP
                     LHI R0,FC_FLOAT         set as float type
                     STH R0,LTVNFMT
                     LHI R0,l'fc_float       set length
                     STH R0,LTVNLEN
*
                     MVI LTVNNDEC,8
*
                   when c'D'      VARIABLE - DATE
                     LHI R0,FC_ALNUM
                     STH R0,LTVNFMT
                     LHI R0,54
                     STH R0,LTVNCON
                     LHI R0,20
                     STH R0,LTVNLEN
*
                   when c'S'     VARIABLE - STRING
                     LHI R0,FC_ALNUM
                     STH R0,LTVNFMT
                     LHI R0,256
                     STH R0,LTVNLEN
                 endsel
               endif
*
             endif
           endif
           br  r9                           ADVANCE  TO NEXT   ROW
           drop currnv_lt_ptr
                          EJECT
***********************************************************************
*    LOAD VARIABLE FUNCTION - CONSTANT VALUE                          *
***********************************************************************
           USING LTVV_REC,R3
*
LTBLVV     stg r9,lt_saver9
           LA  R1,LTVV_TABLE_NAME POINT  TO  SEARCH ARGUMENT (ACCUM NM)
*
           ltgf R14,CURRNV         SEARCH VIEW SPECIFIC CHAIN
           JNP LTBLVVTH           NO  -  MUST BE GLOBAL
*
           llgt R15,LTNVVNAM-LOGICTBL(,R14)
           BRAS R9,VNAMSRCH
           LTgR R15,R15
           JNP LTBLVVTH
*
           OI  LTFLAG2,LTLOCAL    INDICATE LOCAL VARIABLE  (VIEW)
           J   LTBLVVFD
*
LTBLVVTH   llgt R15,GLOBVNAM       LOAD THREAD VARIABLE NAME LIST ADDR
           BRAS R9,VNAMSRCH
           LTgR R15,R15
           JP  LTBLVVFD
*
           LgHI R14,ACCUM_NOT_FOUND
           J   STDERROR
*
LTBLVVFD   llgt R0,LTACADDR-LOGICTBL(,R15)   LOAD ACCUMULATOR ROW ADDR
           ST  R0,LTACADDR
*
           LH  R14,LTVNFMT-LOGICTBL(,R15)   COPY ACCUMULATOR TYPE
           STH R14,LTVVFMT
           if LTR,R14,R14,np      value not positive?
             LgHI R14,ACCUM_NO_FORMAT
             J STDERROR
           endif
*
           CLI LTFUNC+3,C'A'      2ND ARGUMENT IS ALSO ACCUMULATOR ???
           JNE *+14               NO  - MUST BE A CONSTANT
           CLC LTFUNC,CF_CA       COMPARE USING A CONSTANT ???
           JNE LTBLVVA2           NO  - PROBABLY  "CFAA"
*
***********************************************************************
*  SCAN VALUE FOR ALPHABETIC CHARACTERS (STRING "ACCUMULATOR" - DIMS) *
***********************************************************************
           LA  R1,LTVV_VALUE
*
           LT  R14,LTVV_VALUE_LENGTH
           JNP LTBLVVCN
*
LTBLVVLP   CLI 0(R1),C'+'
           JE  LTBLVVNX
           CLI 0(R1),C'-'
           JE  LTBLVVNX
           CLI 0(R1),C'.'
           JE  LTBLVVNX
           CLI 0(R1),C'0'
           BRL LTBLVVST
           CLI 0(R1),C'9'
           JH  LTBLVVA2
LTBLVVNX   LA  R1,1(,R1)
           BRCT R14,LTBLVVLP
*
           J   LTBLVVCN
*
LTBLVVA2   MVI LTFUNC+3,C'A'    CHANGE FUNCTION IF NOT ALREADY "SETA"
*
           LA  R1,LTVV_VALUE      POINT  TO 2ND ACCUMULATOR NAME
           ltgf R14,CURRNV         SEARCH VIEW SPECIFIC CHAIN
           JNP LTBLVVAT           NO  -  MUST BE GLOBAL
*
           llgt R15,LTNVVNAM-LOGICTBL(,R14)
           BRAS R9,VNAMSRCH
           LTgR R15,R15
           JP  LTBLVVAF
*
LTBLVVAT   llgt R15,GLOBVNAM       LOAD THREAD VARIABLE NAME LIST ADDR
           BRAS R9,VNAMSRCH
           LTgR R15,R15
           JP  LTBLVVAF
*
           LgHI R14,ACCUM_NOT_FOUND
           J   STDERROR
*
LTBLVVAF   llgt R0,LTACADDR-LOGICTBL(,R15) LOAD 2ND ACCUM ROW ADDR
           ST  R0,LTVVVAL
*
           LHI R0,4
           STH R0,LTVVLEN
           AHI R0,LTVV_LEN
           STH R0,LTROWLEN
*
           lgf R1,LTVV_COMPARE_TYPE
           SLLg R1,r1,1
           LA  R14,COMPTBL(R1)
           MVC LTVVROPR,0(R14)
*
           J   LTBLVVEX
*
***********************************************************************
*    CONSTANT CONTAINS NON-NUMERIC CHARACTERS                         *
***********************************************************************
LTBLVVST   LH  R14,LTVVFMT
           CHI R14,FC_ALNUM       ALPHANUMERIC  ACCUMULATOR ???
           JE  LTBLVVCN           YES - COULD BE DATE OR STRING
*
           LgHI R14,CONST_NON_NUM  LOAD  ERROR MESSAGE  NUMBER
           J   STDERROR           PRINT ERROR MESSAGE - STOP
*
LTBLVVCN   LH  R0,LTVNLEN-LOGICTBL(,R15)
           STH R0,LTVVLEN
*
           AHI R0,LTVV_LEN
           STH R0,LTROWLEN
*
           lgf R1,LTVV_COMPARE_TYPE
           SLLg R1,r1,1
           LA  R14,COMPTBL(R1)
           MVC LTVVROPR,0(R14)
*
***********************************************************************
*    BRANCH TO APPROPRIATE CONVERSION LOGIC                           *
***********************************************************************
           LH  R14,LTVVFMT
           if CHI,R14,ne,FC_ALNUM    Not ALPHANUMERIC  VALUE ???
*
             if LTgf,R1,LTVV_VALUE_LENGTH,np,or,                       +
               CHI,R1,gt,MAXPACK*2-1
               LgHI R14,CONST_LEN_ERR
               J STDERROR
             endif
*
             STH R1,SAVALLEN
*
             select chi,r14,eq       r14 holds the format type
               when FC_BIN           BINARY ACCUMULATOR  VALUE ???
*
                 bctr r1,0
                 ex r1,varvalpk
static           loctr
VARVALPK         PACK  DBLWORK,LTVV_VALUE-LTVV_REC(0,R3)
code             loctr
                 LH R14,LTVVLEN
                 select CHI,r14,eq
                   when 1
                     CVB R0,DBLWORK
                     STC R0,LTVVVAL
                   when 2
                     CVB R0,DBLWORK
                     STH R0,LTVVVAL
                   when 4
                     CVB R0,DBLWORK
                     ST R0,LTVVVAL
                   when 8
                     CVBG R0,DBLWORK
                     STG R0,LTVVVAL
                   othrwise ,
                    LgHI R14,DIM3_UNSUPPORTED INDICATE UNSUPPORTED DIM3
                     J STDERROR
                 endsel
*
               when fc_pack             packed accumulator value??
                 LHI R0,L'COLDATA       RESULT LENGTH
                 STH R0,DL96LEN
                 STH R0,LTVVLEN
*
                 LA R0,LTVVVAL          RESULT ADDRESS
                 ST R0,DL96TGTA
*
                 MVI SAOUTFMT+1,FC_PACK RESULT ATTRIBUTES
                 XC SAOUTCON,SAOUTCON
*
                 MVHHI SAOUTDEC,8
                 if CLI,LRGNUM,eq,C'Y'
                   MVHHI SAOUTDEC,3
                 endif
*
                 MVHHI SAOUTRND,0
                 MVI SAOUTSGN,C'Y'
                 MVI SAOUTJUS,C'L'
*
                 MVI SAVALFMT+1,FC_EDIT SOURCE ATTRIBUTES
                 XC SAVALCON,SAVALCON
                 MVHHI SAVALDEC,0
                 MVHHI SAVALRND,0
                 MVI SAVALSGN,C'Y'
*
                 LA R0,LTVV_VALUE
                 stg R0,SAVALADR
*
                 XC SAMSKLEN,SAMSKLEN   NO MASKING
                 XC SAMSKADR,SAMSKADR
*
                 LA R1,DL96LIST
                 llgf R15,GVBDL96A
                 BASsm R14,R15
*
                 if LTR,R15,R15,nz,and,chi,r15,ne,2 (2 is truncation)
*
                   LgHI R14,DECIMAL_PLACES_GT_FIELD Err msg Num
                   if CHI,R15,ne,13
                     LgHI R14,GENEVA_NO_DIG_EXCEEDED Err Msg Num
                   endif
                   J STDERROR     PRINT ERROR MESSAGE - STOP
                 endif
               when fc_float            DFP accumulator value??
                 LH R0,LTVVLEN
                 STH R0,DL96LEN
*
                 LA R0,LTVVVAL          RESULT ADDRESS
                 ST R0,DL96TGTA
*
                 MVI SAOUTFMT+1,FC_FLOAT RESULT ATTRIBUTES
                 XC SAOUTCON,SAOUTCON
*
                 MVHHI SAOUTDEC,8
*
                 MVHHI SAOUTRND,0
                 MVI SAOUTSGN,C'Y'
                 MVI SAOUTJUS,C'L'
*
                 MVI SAVALFMT+1,FC_EDIT SOURCE ATTRIBUTES
                 XC SAVALCON,SAVALCON
                 MVHHI SAVALDEC,0
                 MVHHI SAVALRND,0
                 MVI SAVALSGN,C'Y'
*
                 LA R0,LTVV_VALUE
                 stg R0,SAVALADR
*
                 XC SAMSKLEN,SAMSKLEN   NO MASKING
                 XC SAMSKADR,SAMSKADR
*
                 LA R1,DL96LIST
                 llgf R15,GVBDL96A
                 BASsm R14,R15
*
                 if LTR,R15,R15,nz,and,chi,r15,ne,2 (2 is truncation)
*
                   LgHI R14,DECIMAL_PLACES_GT_FIELD Err msg Num
                   if CHI,R15,ne,13
                     LgHI R14,GENEVA_NO_DIG_EXCEEDED Err Msg Num
                   endif
                   J STDERROR     PRINT ERROR MESSAGE - STOP
                 endif
               othrwise ,
                 LgHI R14,ACCUM_FORMAT_UNSUPPORTED
                 J STDERROR
               endsel
*
           else ,                    YES - COULD BE DATE OR STRING
            if XC,LTVNCON-LOGICTBL(L'LTVNCON,R15),LTVNCON-LOGICTBL(R15)+
               ,NZ                   check for non-zero
*
               MVI LTVVVAL+0,C'0'
               MVC LTVVVAL+1(20-1),LTVVVAL
*
             else ,
*
               MVI LTVVVAL+0,C' '
               MVC LTVVVAL+1(256-1),LTVVVAL
*
             endif
             if LT,R1,LTVV_VALUE_LENGTH,p
*
               BCTR R1,0
               EX R1,VARVALMV
             endif
           endif
*
ltblvvex   lg r9,lt_saver9
           br  r9
                          EJECT
***********************************************************************
*    LOAD VARIABLE FUNCTION - ONE FIELD DESCRIPTION                   *
*          (USE FORMAT 2 LAYOUT TO INCLUDE ACCUMULATOR ATTRIBUTES)    *
***********************************************************************
           USING LTV1_REC,R3
*
LTBLV1     stg r9,lt_saver9
           LHI R0,LTV2_LEN        USE TWO ATTRIBUTE LOGIC TABLE FORMAT
           STH R0,LTROWLEN
*
           LA  R1,LTV1_ACCUM_NAME POINT  TO  SEARCH ARGUMENT
*
           ltgf R14,CURRNV         SEARCH VIEW SPECIFIC CHAIN
           JNP LTBLV1TH
*
           llgt R15,LTNVVNAM-LOGICTBL(,R14)
           BRAS R9,VNAMSRCH
           lg r9,lt_saver9
           LTgR R15,R15
           JNP LTBLV1TH
           OI  LTFLAG2,LTLOCAL    INDICATE LOCAL VARIABLE  (VIEW)
           J   LTBLV1FD
*
LTBLV1TH   llgt R15,GLOBVNAM       LOAD THREAD VARIABLE NAME LIST ADDR
           BRAS R9,VNAMSRCH
           lg r9,lt_saver9
           LTgR R15,R15
           JP  LTBLV1FD
*
           LgHI R14,ACCUM_NOT_FOUND
           J   STDERROR
*
LTBLV1FD   llgt R0,LTACADDR-LOGICTBL(,R15)   LOAD ACCUMULATOR ROW ADDR
           ST  R0,LTAC2ADR
*
***********************************************************************
*    COPY ATTRIBUTES FROM ACCUMULATOR DEFINITION - SOURCE             *
***********************************************************************
           MVC LTFLDDDN,HEXFF                    ACCUM FILE DDNAME
           XC  LTFLDLR,LTFLDLR                   ACCUM LR ID
           XC  LTFLDPTH,LTFLDPTH                 ACCUM PATH ID
           XC  LTFLDID,LTFLDID                   ACCUM ID
           LHI R0,1
           STH R0,LTFLDPOS                       ACCUM POSITION
           STH R0,LTFLDSEQ                       ACCUM SEQ  NUMBER
           MVC LTFLDLEN,LTVNLEN-LOGICTBL(R15)    ACCUM LENGTH
           MVC LTFLDFMT,LTVNFMT-LOGICTBL(R15)    ACCUM FORMAT
           MVC LTFLDCON,LTVNCON-LOGICTBL(R15)    ACCUM CONTENT
           MVC LTNDEC,LTVNNDEC-LOGICTBL(R15)     ACCUM NO.  DEC PLACES
           MVI LTRNDFAC,0                        ACCUM ROUNDING FACTOR
           MVC LTSIGN,LTVNSIGN-LOGICTBL(R15)     ACCUM SIGNED   IND
*
           MVI LTSRTSEQ,C'A'
*
           MVC LTRELOPR,SPACES
           XC  LTSTRNO,LTSTRNO
           XC  LTSTRLVL,LTSTRLVL
           XC  LTJUSOFF,LTJUSOFF
*
***********************************************************************
*    COPY ATTRIBUTES FROM LOGIC TABLE - TARGET                        *
***********************************************************************
           MVC LTCOLNO,LTV1_SUFFIX_SEQ_NBR
           MVC LTCOLID,LTV1_COLUMN_ID
*
           MVC LTCOLDDN,HEXFF
           MVC LTCOLFIL,LTV1_FIELD_FILE_ID
           MVC LTCOLLR,LTV1_LR_ID
           MVC LTCOLPTH,SVLUPATH
           MVC LTCOLFLD,LTV1_FIELD_ID
           MVC LTCOLPOS,LTV1_START_POSITION
           MVC LTCOLSEQ,LTV1_ORDINAL_POSITION
           MVC LTCOLLEN,LTV1_FIELD_LENGTH
           MVC LTCOLFMT,LTV1_FIELD_FORMAT+2
           MVC LTCOLCON,LTV1_CONTENT_ID+2
*
           LH  R0,LTV1_NO_OF_DECIMALS
           STC R0,LTCOLDEC
*
           LH  R0,LTV1_ROUNDING
           STC R0,LTCOLRND
*
           XR  R0,R0
           IC  R0,LTV1_SIGN_IND
           MVI LTCOLSGN,C'N'
           CHI R0,SIGNED
           JNE *+8
           MVI LTCOLSGN,C'Y'
*
           ltgf R15,LTV1_JUSTIFY_ID
           JP  LTBLV1J
*
           LgHI R15,1              ASSUME LEFT
           CLI LTCOLFMT+1,FC_ALNUM
           JE  LTBLV1J
           LgHI R15,3              ASSUME RIGHT
*
LTBLV1J    CHI R15,3
           JNH *+8
           LgHI R15,3
           LA  R14,JUSTTBL(R15)
           MVC LTCOLJUS,0(R14)
*
           lgf R15,LTV1_COMPARE_TYPE
           SLLg R15,r15,1
           LA  R14,COMPTBL(R15)
           MVC LTRELOPR,0(R14)
*
           XC  LTMSKLEN,LTMSKLEN
           XC  LTV1LEN,LTV1LEN
           XC  LTV2LEN,LTV2LEN
*
           J   LTBLV2MS           BRANCH AND CONVERT MASK (IF ANY)
                          EJECT
***********************************************************************
*    LOAD VARIABLE FUNCTION - FORMAT 2                                *
***********************************************************************
           USING LTV2_REC,R3
*
LTBLV2     stg  r9,lt_saver9
           LHI R0,LTV2_LEN
           STH R0,LTROWLEN
*
           LA  R1,LTV2_ACCUM_NAME POINT  TO  SEARCH ARGUMENT
*
           ltgf R14,CURRNV         SEARCH VIEW SPECIFIC CHAIN
           JNP LTBLV2TH
*
           llgt R15,LTNVVNAM-LOGICTBL(,R14)
           BRAS R9,VNAMSRCH
           lg  r9,lt_saver9
           if LTgR,R15,R15,p
             OI LTFLAG2,LTLOCAL   INDICATE LOCAL VARIABLE  (VIEW)
           else

ltblv2th     llgt R15,GLOBVNAM     LOAD THREAD VARIABLE NAME LIST ADDR
             BRAS R9,VNAMSRCH
             lg r9,lt_saver9
             if LTgR,R15,R15,np

               LgHI R14,ACCUM_NOT_FOUND
               J STDERROR
             endif
           endif

           llgt R0,LTACADDR-LOGICTBL(,R15)   LOAD ACCUMULATOR ROW ADDR
           ST  R0,LTAC2ADR
*
***********************************************************************
*    COMMON LOGIC FOR "LTBLF2/LTBLV2" - FORMAT CODES "05" AND "13"
***********************************************************************
*
LTBLV2F2   MVC LTCOLNO,LTV2_SUFFIX_SEQ_NBR
           MVC LTCOLID,LTV2_COLUMN_ID
*
           MVC LTFLDDDN,HEXFF
           MVC LTFLDFIL,LTV2_FILE_ID1
           MVC LTFLDLR,LTV2_LR_ID1
           MVC LTFLDPTH,SVLUPATH
           MVC LTFLDID,LTV2_FIELD_ID1
           MVC LTFLDPOS,LTV2_START_POSITION1
           MVC LTFLDSEQ,LTV2_ORDINAL_POSITION1
           MVC LTFLDLEN,LTV2_FIELD_LENGTH1
           MVC LTFLDFMT,LTV2_FIELD_FORMAT1+2
           MVC LTFLDCON,LTV2_CONTENT_ID1+2
*
           LH  R0,LTV2_NO_OF_DECIMALS1
           STC R0,LTNDEC
*
           LH  R0,LTV2_ROUNDING1
           STC R0,LTRNDFAC
*
           llc R0,LTV2_SIGN_IND1
           if chi,r0,eq,SIGNED
             MVI LTSIGN,C'Y'
           else
             MVI LTSIGN,C'N'
           endif
*
           MVI LTSRTSEQ,C'A'
           lgf R0,LTVIEW#
           if  (Ltgf,R1,LTCOLID,P)    only call locsort if +ve colid
             BRAS R9,LOCSORT
             lg r9,lt_saver9
             if  (LTgR,R15,R15,P)    found?
               USING vdp2300b_SORT_KEY_ATTR_RECORD,R15
               if  (CLI,vdp2300b_SORT_SEQ_ID+3,EQ,DESCEND) and this set
                 MVI LTSRTSEQ,C'D'
               endif
               DROP R15
             endif
           endif
*
           lgf R15,LTV2_COMPARE_TYPE
           SLLg R15,r15,1
           LA  R14,COMPTBL(R15)
           MVC LTRELOPR,0(R14)
*
           XC  LTSTRNO,LTSTRNO
           XC  LTSTRLVL,LTSTRLVL
           XC  LTJUSOFF,LTJUSOFF
           if tm,ltv2_value_length1,x'80',o
             la r1,ltv2_value_area1   This is our parm to Cookie subrtn
             jas r9,Cookie       Call Cookie subroutine
             sty r1,LTV2_VALUE_LENGTH1 save the resolved length
           endif
           MVC LTV1LEN,LTV2_VALUE_LENGTH1+2
           if tm,ltv2_value_length2,x'80',o
             la r1,ltv2_value_area2   This is our parm to Cookie subrtn
             jas r9,Cookie       Call Cookie subroutine
             sty r1,LTV2_VALUE_LENGTH2 save the resolved length
           endif
           MVC LTV2LEN,LTV2_VALUE_LENGTH2+2
*
           MVC LTCOLDDN,HEXFF
           MVC LTCOLFIL,LTV2_FILE_ID2
           MVC LTCOLLR,LTV2_LR_ID2
           MVC LTCOLPTH,SVLUPATH
           MVC LTCOLFLD,LTV2_FIELD_ID2
           MVC LTCOLPOS,LTV2_START_POSITION2
           MVC LTCOLSEQ,LTV2_ORDINAL_POSITION2
           MVC LTCOLLEN,LTV2_FIELD_LENGTH2
           MVC LTCOLFMT,LTV2_FIELD_FORMAT2+2
           MVC LTCOLCON,LTV2_CONTENT_ID2+2
*
           if CLC,LTMAJFUN,eq,LK_E
             BRAS R14,INCKYPOS
           endif
*
           if clc,ltfunc,eq,=c'FNLL',or, FNLL?                         +
               clc,ltfunc,eq,=C'CFLL'    CFLL?

             LA R1,LTFLDDDN
             BRAS R9,LOCJOIN
             lg r9,lt_saver9
             ST R0,LTFLDPTH

           else

             if CLI,FC_RTYP,eq,fc_RTYP12,or,                           +
               CLI,LTSUBFUN+1,eq,C'L'

               LA R1,LTCOLDDN
               BRAS R9,LOCJOIN
               lg r9,lt_saver9
               ST R0,LTCOLPTH

             endif
           endif
*
           LH  R0,LTV2_NO_OF_DECIMALS2
           STC R0,LTCOLDEC
*
           LH  R0,LTV2_ROUNDING2
           STC R0,LTCOLRND
*
           llgc R0,LTV2_SIGN_IND2
           if CHI,R0,eq,SIGNED
             MVI LTCOLSGN,C'Y'
           else
             MVI LTCOLSGN,C'N'
           endif
*
           if ltgf,R15,LTV2_JUSTIFY_ID2,np
             if  CLI,LTCOLFMT+1,eq,FC_ALNUM,or,                        +
               CLI,LTCOLFMT+1,eq,FC_ALPHA
               LgHI R15,1          ASSUME LEFT
             else
               LgHI R15,3          ASSUME RIGHT
             endif
           endif
*
           if CHI,R15,gt,3
             LgHI R15,3
           endif
           LA  R14,JUSTTBL(R15)
           MVC LTCOLJUS,0(R14)
*
LTBLV2MS   XC  LTMSKLEN,LTMSKLEN
*
           CLC LTMAJFUN,SK_E      SORT   KEY     ???
           JE  LTBLV2R            YES -  IGNORE  MASKS
*
           OC  LTCOLCON,LTCOLCON  OUTPUT CONTENT CODE SPECIFIED ???
           JZ  LTBLV2VC           NO  -  CONTINUE
*
           J   LTBLV2R            KEEP  ORIGINAL  FORMAT
*
***********************************************************************
*    BUILD MASK FROM MASK CODE                                        *
***********************************************************************
LTBLV2VC   lgf R0,LTVIEW#         LOCATE VIEW  COLUMN RECORD (2000)
           ltgf R1,LTCOLID         load column id and set cc
           BRNP LTBLV2R            exit if not positive
           llgt R14,ltlognv        get the nv pointer here
           BRAS R9,LOCCOLM
           lg r9,lt_saver9
           USING vdp2000b_COLUMN_RECORD,R15
           LTgR R15,R15
           JNP LTBLV2R
*
           LA  R14,L'MASKCODE-1   EXPLICIT MASK PRESENT ???
           EX  R14,MASKBLNK
           JE  LTBLV2R            NO  - BYPASS MASK EDITING
*
           LH  R0,LTCOLLEN        LOAD COLUMN  WIDTH
           STH R0,LTMSKLEN
           LA  R1,vdp2000b_REPORT_MASK USE ORIGINAL MASK CODE
           MVC WORKAREA(L'MASKCODE),0(R1)  SAVE     CODE
           LA  R14,LTCOLMSK
           BRAS R9,MASKCONV
           lg r9,lt_saver9
*
           DROP R15
*
           if LTgR,R15,R15,np     Not SUCCESSFUL ???
             STH R15,LTMSKLEN     ZERO MASK LENGTH
             J LTBLV2R
           endif
*
           if CLI,WORKAREA,eq,C'L' SIGN ON LEFT ???
             MVI LTCOLMSK,C'-'
           endif
*
           if CLI,WORKAREA,eq,C'P' SIGN ON LEFT ???
             MVI LTCOLMSK,C'('
           endif
*
***********************************************************************
*    COMMA SEPARATED VARIABLE ("CSV") FILE                            *
***********************************************************************
           llgt R14,ltlognv
           LTgf R14,LT1000A-LOGICTBL(,R14)
           JNP LTBLV2R
*
LTBLV2CS   CLI vdp1000b_OUTPUT_MEDIA-vdp1000b_VIEW_RECORD+3(R14),CSV
           JNE LTBLV2FF
*
           CLI LTCOLFMT+1,FC_ALNUM  ALPHANUMERIC FIELD ???
           JNE LTBLV2EN
*
           XC  LTMSKLEN,LTMSKLEN    IGNORE MASKS ON "CSV" ALPHA FIELDS
           J   LTBLV2R
*
***********************************************************************
*    FILE FORMAT OUTPUT FILE                                          *
***********************************************************************
LTBLV2FF  CLI vdp1000b_OUTPUT_MEDIA-vdp1000b_VIEW_RECORD+3(R14),FILEFMT
           JNE LTBLV2R             NO  -  ACCEPT MASK
*
           CLI LTCOLFMT+1,FC_EDIT  EDITED NUMERIC ???
           JE  LTBLV2EN            YES -  CHECK COMPATIBILITY
*
           CLI LTCOLFMT+1,FC_MASK  MASKED NUMERIC ???
           JE  LTBLV2R             YES -  ACCEPT MASK
*
           XC  LTMSKLEN,LTMSKLEN   IGNORE MASK UNLESS "MASKED NUMERIC"
           J   LTBLV2R
*
LTBLV2EN   LgR  R15,R0              "EDITED NUMERIC COMPATIBILITY" FLAG
           CLI 0(R15),C'Y'
           JE  LTBLV2R
*
***********************************************************************
*    BUILD EDITED NUMERIC MASK                                        *
***********************************************************************
           LLC R15,LTCOLDEC                    OUTPUT   NO  DECIMALS
           if CHI,R15,gt,MAXDEC
             LHI R15,MAXDEC
           endif
*
           LHI R0,MAXDEC                       COMPUTE RIGHT TRUNCATION
           SR  R0,R15                          EXCESS  MASK  DECIMALS
           if CHI,R0,eq,MAXDEC                 ANY DECIMALS?
             AHI R0,1                          allow for DP
           endif
*
           LA  R1,CSVMASK+L'CSVMASK            LOAD END OF MASK ADDR
           SR  R1,R0                           ADJUST  FOR NO DECIMALS
*
           LH  R15,LTCOLLEN                    LOAD COLUMN WIDTH (-1)
           SR  R1,R15                          BACKUP  TO  BEGINNING
*
           BCTR R15,0                          DECREMENT FOR EX
           EX  R15,CSVMSKMV                    REPLACE MASK
           AHI R15,1                           ADD BACK AFTER EX
           STH R15,LTMSKLEN
*
           if CLI,LTCOLSGN,eq,C'Y'
             MVI LTCOLMSK,C'-'
           endif
*
LTBLV2R    LH  R0,LTROWLEN
           AH  R0,LTV1LEN
           AH  R0,LTV2LEN
           AH  R0,LTMSKLEN
           ahi r0,3
           nill r0,X'FFFC'
           STH R0,LTROWLEN
*
           lh  r0,ltv2len           get second length
           if cij,r0,gt,0           greater than 0?

             LH R15,LTV1LEN         Get length of first value
             if cij,r15,gt,0        above zero length?

               LA R14,LTV2_VALUE1
               LA R0,LTCOLMSK
               lgh R1,LTMSKLEN
               Agr R0,r1
               LR R1,R15
               MVCL R0,R14

             endif

LTBLV2V      LH R15,LTV2LEN
             if cij,r15,gt,0        above zero length?

               LA R14,LTV2_VALUE2
               LA R0,LTCOLMSK
               lgh R1,LTMSKLEN
               ah R1,LTV1LEN
               Agr R0,r1
               LR R1,R15
               MVCL R0,R14

             endif

             if CLC,LTFUNC,eq,FN_CC
               br r9
             endif

           else ,

***********************************************************************
*  CONVERT TARGET POSITION TO MACHINE INSTRUCTION OFFSET(POSITION - 1)*
***********************************************************************
             LH R15,LTCOLPOS      LOAD FIELD POSITION
             if cij,r15,le,0      Value missing ???
               LgHI R14,TARGET_FIELD_POS_MISSING
               J STDERROR
             endif

             BCTR R15,0           DECREMENT POSITION (CONVERT TO OFF)
             STH R15,LTCOLPOS
*
***********************************************************************
*    CONVERT TARGET LENGTH TO MACHINE INSTRUCTION LENGTH (LENGTH - 1) *
***********************************************************************
             LH R15,LTCOLLEN      LOAD  FIELD  LENGTH
             if cij,r15,le,0      Value missing ???
               LgHI R14,TARGET_FIELD_LEN_MISSING
               J STDERROR
             endif

             BCTR R15,0           SUBTRACT ONE FOR "EX" INSTRUCTIONS
             STH R15,LTCOLLEN     ADJUST FIELD LENGTH   (-1)

             LH R0,LTCOLFMT       PACKED DECIMAL  FIELD ???
             if cij,r0,lt,0
               LgHI R14,TARGET_FIELD_FMT_MISSING   ERROR
               J STDERROR
             endif

             if cij,R0,eq,FC_PACK,and,     PACKED DECIMAL FIELD and    +
               cij,r15,gt,maxpack-1 LENGTH LIMIT    EXCEEDED   ???
               LgHI R14,TARGET_FIELD_LEN_ERR       ERROR
               J STDERROR
             endif
           endif

***********************************************************************
*  CONVERT SOURCE POSITION TO MACHINE INSTRUCTION OFFSET(POSITION - 1)*
***********************************************************************
LTBLV2S    LH  R15,LTFLDPOS       LOAD FIELD POSITION
           if CL,R15,ne,HEXFF     POSITION not high values ???
*
             if LTR,R15,R15,NP    POSITION absent ??
               LgHI R14,SOURCE_FIELD_POS_MISSING
               J STDERROR
             endif
             BCTR R15,0           DECREMENT POSITION (CONVERT TO OFF)
             STH R15,LTFLDPOS
           endif
*
***********************************************************************
*    CONVERT SOURCE LENGTH TO MACHINE INSTRUCTION LENGTH (LENGTH - 1) *
***********************************************************************
           LH  R15,LTFLDLEN       LOAD  FIELD  LENGTH
           if LTR,R15,R15,NP      value absent ??
             LgHI R14,SOURCE_FIELD_LEN_MISSING
             J STDERROR
           endif
*
           BCTR R15,0             SUBTRACT ONE FOR "EX" INSTRUCTIONS
           STH R15,LTFLDLEN       ADJUST FIELD LENGTH   (-1)
*
           LH  R0,LTFLDFMT        PACKED DECIMAL  FIELD ???
           if LTR,R0,R0,NP
             LgHI R14,SOURCE_FIELD_FMT_MISSING
             J STDERROR
           endif
*
           CHI R0,FC_PACK
           bner r9                NO  -  LENGTH   EDIT  UNNECESSARY
*
           CHI R15,MAXPACK-1      LENGTH LIMIT    EXCEEDED   ???
           bNHr r9                NO  -  ADVANCE  TO  NEXT   ROW
*
           LgHI R14,SOURCE_FIELD_LEN_ERR
           J   STDERROR
*
                          EJECT
***********************************************************************
*          E N D O F   S E T  -  "ES"                                 *
***********************************************************************
           USING LTES_REC,R3
*
LTBLES     LHI R0,LTES_LEN
           STH R0,LTROWLEN
*
           MVC FILEBEG,LTCOUNT    SAVE  FILE BEG ROW# FOR NEXT "ES" SET
*
           XC  ltlognv,ltlognv    ZERO  CURRENT  VIEW ROW ADDR
*
*          MVC LTESSET#,XXXXXXX   PRE-ASSIGNED   SET      NO.
           XC  LTESSET#,LTESSET#  ZERO  ASSIGNED THREAD   NO.
           XC  LTTHRDWK,LTTHRDWK  ZERO  THREAD   WORK     AREA  ADDRESS
*
           MVC   LTPIPELS,SAVEPIPE  SAVE  PIPE LIST ADDRESS
           XC    SAVEPIPE,SAVEPIPE  RESET PIPE ADDRESS
*
           XC  LTNEXTES,LTNEXTES  ZERO  NEXT     "ES"  POINTER
           MVC LTESVNAM,FIRSTVN   SET   FIRST    VARIABLE NAME "ES" SET
           MVC LTFRSTRE,FIRSTRE   SET   FIRST    "RE"  WITHIN  "ES" SET
           MVC LTFRSTNV,FIRSTNV   SET   FIRST    "NV"  WITHIN  "ES" SET
           MVC LTTOKNNV,FIRSTKNV  SET   FIRST  TOKEN READER IN "ES" SET
           XC  LTSAMES#,LTSAMES#  ZERO  SAME   THREAD  SIBLING "ES" SET
           XC  LTLBANCH,LTLBANCH  ZERO  LOOK-UP  BUFR  CHAIN    ANCHOR
*
           MVC LTESLPSZ,ESPOOLSZ  SAVE  LITERAL  POOL  SIZE IN "ES" SET
           l     r14,espoolsz       get current size
           a     r14,litpools       add in total size so far
           st    r14,litpools       and save
           XC  ESPOOLSZ,ESPOOLSZ
*
***********************************************************************
*    INSERT "ES" ROW ADDRESS INTO ALL "NV" ROWS IN THIS SET           *
***********************************************************************
           llgt R14,LTFRSTNV
LTBLES10   LTgR R14,R14            END-OF-CHAIN  ???
           JNP LTBLES11
           ST  R7,LTVIEWES-LOGICTBL(,R14) SAVE "ES"   ROW   ADDRESS
           llgt R14,LTNEXTNV-LOGICTBL(,R14)
           J   LTBLES10
*
***********************************************************************
*    INSERT "ES" ROW ADDRESS INTO ALL "RE" ROWS IN THIS SET           *
***********************************************************************
LTBLES11   llgt R14,LTFRSTRE
LTBLES12   LTgR R14,R14            END-OF-CHAIN  ???
           JNP LTBLES15
           ST  R7,LTREES-LOGICTBL(,R14)  SAVE  "ES"   ROW   ADDRESS
           llgt R14,LTNEXTRE-LOGICTBL(,R14)
           J   LTBLES12
*
***********************************************************************
*    CHECK FOR CONCATENATED EVENT FILES                               *
***********************************************************************
LTBLES15   LH  R0,LTESSET#        THREAD SPECIFIED    ???
           LTR R0,R0
           JNP LTBLES30           NO  -  EACH  "ES"   MARKS NEW THREAD
*
           CH  R0,SVTHRDID        SAME THREAD   AS    PREVIOUS  ???
           JE  LTBLES40           YES - CHAIN  "ES"   SETS
           J   LTBLES50           NO  - NEW    THREAD
*
LTBLES30   CLC DRIVDDN,PREVDDN    SAME DRIVER  FILE   ???
           JE  LTBLES40           YES - ASSUME CONCATENATED "ES" SET
           MVC PREVDDN,DRIVDDN    NO  - NEW    THREAD
           J   LTBLES50
*
LTBLES40   llgt R14,PREVES       YES - CHAIN  CONCATENATED  EVENT FILE
           L   R1,LTSAMES#-LOGICTBL(,R14)
           ST  R7,LTSAMES#-LOGICTBL(,R14)
           ST  R1,LTSAMES#-LOGICTBL(,R7)
*
           J   LTBLES98           EXIT
*
***********************************************************************
*    APPEND "ES" ROW TO APPLICABLE "ES" CHAIN BASED ON THREAD TYPE    *
***********************************************************************
LTBLES50   STH R0,SVTHRDID        SET  PREVIOUS  THREAD ID = CURRENT ID
*
           L   R14,ESSETCNT       INCREMENT "ES" SET COUNT
           AHI R14,1
           ST  R14,ESSETCNT
*
           LTgf R14,FIRSTRE        "RE" PRESENT IN THIS "ES" SET ???
           JP  LTBLES54           YES - DETERMINE FILE TYPE
*
***********************************************************************
*    DEFAULT THREAD TYPE TO "PIPE" IF NO "RE" PRESENT                 *
*    *****NEEDS CODE TO DETECT JLT THREAD FROM PIPES*****             *
***********************************************************************
           LgHI R0,DISKDEV         LOAD THREAD TYPE
           LA  R1,PREVDISK        APPEND  TO "OTHER" CHAIN
*
           J   LTBLES60
*
LTBLES54   LH  R0,LTFILTYP-LOGICTBL(,R14) LOAD EVENT FILE TYPE
*
           CHI R0,TOKENDEV        TOKEN THREAD ???
           JE  LTBLES98           YES-  BRANCH (NOT A THREAD)
*
LTBLES55   CHI R0,PIPEDEV         PIPE THREAD ???
           JNE LTBLES56           NO -  BRANCH
           LA  R1,PREVOTHR        APPEND  TO "OTHR"  CHAIN
*
           L   R15,OTHRCNT
           AHI R15,1
           ST  R15,OTHRCNT
           J   LTBLES60
*
LTBLES56   CHI R0,DISKDEV         DISK  THREAD ???
           JNE LTBLES57           NO -  BRANCH
           LA  R1,PREVDISK        APPEND  TO "DISK"  CHAIN
*
           L   R15,DISKCNT
           AHI R15,1
           ST  R15,DISKCNT
           J   LTBLES60
*
LTBLES57   CHI R0,TAPEDEV         TAPE  THREAD ???
           JNE LTBLES58           NO  - ERROR
           LA  R1,PREVTAPE        APPEND  TO "TAPE"  CHAIN
*
           L   R15,TAPECNT
           AHI R15,1
           ST  R15,TAPECNT
           J   LTBLES60
*
LTBLES58   lay r14,MDLERRTX
           MVC ERRDATA,0(r14)     COPY  MODEL INDICATIVE DATA TEMPLATE
*
           L   R0,LTFILEID-LOGICTBL(,R14)     EVENT FILE ID
           CVD R0,DBLWORK
           OI  DBLWORK+L'DBLWORK-1,X'0F'
           MVC ERRDATA+MDLERRF#-MDLERRTX(L'MDLERRF#),NUMMASK
           ED  ERRDATA+MDLERRF#-MDLERRTX(L'MDLERRF#),DBLWORK+4
           MVI ERRDATA+MDLERRF#-MDLERRTX,C' '
*
           LgHI R14,SOURCE_FILE_TYPE_ERR  UNDEFINED EVENT FILE TYPE
           J   STDERR10
*
***********************************************************************
*    ADD "ES" TO APPLICABLE CHAIN                                     *
***********************************************************************
LTBLES60   llgt R14,0(,R1)         LOAD   PREVIOUS "ES"  ROW  ON CHAIN
           ST  R7,0(,R1)          UPDATE PREVIOUS "ES"  ROW ON CHAIN
*
           LTgR R14,R14            PREVIOUS  "ES" EXISTS ???
           JP  LTBLES80           YES - LINK TO  PREVIOUS
*
           LA  R1,LTNXOTHR        ASSUME PIPE/TOKEN
*
           CHI R0,DISKDEV         DISK DEVICE ???
           JNE *+8
           LA  R1,LTNXDISK
*
           CHI R0,TAPEDEV         TAPE DEVICE ???
           JNE *+8
           LA  R1,LTNXTAPE
*
LTBLES70   ST  R7,0(,R1)          SET  FIRST  AVAILABLE THREAD
           J   LTBLES98
*
LTBLES80   ST  R7,LTNEXTES-LOGICTBL(,R14)
*
***********************************************************************
*    RESET LOGIC TABLE ROW ADDRESSES FOR THIS "ES" SET                *
***********************************************************************
LTBLES98   ST  R7,PREVES          UPDATE  PREVIOUS "ES" ROW ADDR
*
           XC  FIRSTVN,FIRSTVN    RESET   FIRST    VARIABLE NAME ADDR
           XC  FIRSTRE,FIRSTRE    RESET   FIRST    "RE" ROW ADDR
           XC  FIRSTNV,FIRSTNV    RESET   FIRST    "NV" ROW ADDR
           XC  FIRSTKNV,FIRSTKNV  RESET   FIRST TOKEN READR ADDR
           XC  PREVTKNV,PREVTKNV  RESET   FIRST TOKEN READR ADDR
           XC  CURRRE,CURRRE      RESET   CURRENT  "RE" ROW ADDR
           XC  CURRNV,CURRNV      RESET   CURRENT  "NV" ROW ADDR
*
           br r9                  return
         DROP  R3
         DROP  R5
         DROP  R7
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C L O N E   L O G I C   T A B L E   "E S"   S E T S          *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R7  - CURRENT TARGET LOGIC TABLE ROW ADDRESS                 *
*        R6  - CURRENT "ES"   ROW    ADDR IN  "ES"    DISPATCH QUEUE  *
*        R5  - CURRENT "ES"   ROW    ADDR IN  "ES"    PEER     CHAIN  *
*        R4  - CURRENT "ES"   DISPATCH  QUEUE ANCHOR  ADDRESS         *
*        R3  - NEXT    VDP200 RECORD ADDR (FILE PARTITION)            *
*        R2  - FIRST   "RE"   ROW    ADDR FOR "ES"    SET             *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,savesubr
*
CLONLTBL stmg  R14,R12,SAVF4SAG64RS14  SAVE   REGISTERS
*
         lgf   R14,LTCOUNT        LOAD  TARGET ROW# FOR CLONING
         ST    R14,FILEBEG        INITIALIZE BEGINNING  ROW#
*
         BCTR  R14,0              CONVERT SUBSCRIPT FOR PREV "LT" ROW
         SLLg  R14,r14,2
         agf   R14,LTROWADR
         llgt  R7,0(,R14)         LOAD  PREV  "LT"  ROW ADDRESS ("EN")
         USING LOGICTBL,R7
*
***********************************************************************
*  LOOP THROUGH EACH "ES" DISPATCH CHAIN (DISK, TAPE, OTHER)          *
***********************************************************************
         LA    R4,LTNXDISK        LOAD FIRST DISPATCH QUEUE ANCHOR ADDR
*
         llgt  R6,0(,R4)          LOAD FIRST DISPATCH QUEUE "ES"   ROW
*
CLONQUE  ltgr  R6,R6              QUEUE  EMPTY ???
         JNP   CLONNXTQ           YES -  TRY  NEXT    QUEUE
*
***********************************************************************
*  If this is a cloned ES then need to increment the PIPELIST         *
*  EXTPIPEP counter                                                   *
***********************************************************************
cles     using logictbl,r6
         tm    cles.ltflag1,LTESCLON is this a cloned "ES"
         jno   clon_byppl         .N: then bypass this
         la    r15,cles.ltpipels   .any pipes?
*
clon_pipelp ds 0h                 SAVE PREVIOUS LIST ELEMENT ADDRESS
         llgt  R15,0(,R15)        LOAD NEXT     LIST ELEMENT ADDRESS
         ltgr  R15,R15
         JNP   clon_byppl         END-OF-LIST - ADD  NEW  ELEMENT
*
         llgt  r14,4(,r15)        Get the EXTFILE address
clext    using extfile,r14
         lh    r2,clext.extpipep Increment   parent thread  count
         ahi   r2,1
         sth   r2,clext.extpipep
*
         J     clon_pipelp        next entry
*
clon_byppl ds  0h
         drop  cles,clext

*
***********************************************************************
*  GET "RE" ROW ADDRESS  (BEGINNING OF COPIED LOGIC TABLE ROWS)       *
***********************************************************************
         ltgf  R2,LTFRSTRE-LOGICTBL(,R6)  LOAD FIRST "RE" ROW ADDR
         JNP   CLONNXTE
*
***********************************************************************
*  CHECK IF THERE ARE ADDITIONAL PARTITIONS FOR THIS EVENT FILE       *
***********************************************************************
         ltgf  R3,LTVDP200-LOGICTBL(,R2)  LOAD VDP 200 RECORD ADDR
         JNP   CLONNXTE
*
         ltgf  R3,vdp0200b_MULT_PART_CHAIN-vdp0200b_FILE_RECORD(,R3)
         JNP   CLONNXTE
         USING vdp0200b_FILE_RECORD,R3
*
***********************************************************************
*  COMPUTE VIEW RELOCATION FACTOR                                     *
***********************************************************************
         L     R0,LTCOUNT              LOAD CURRENT LOGIC TABLE ROW NO.
         S     R0,LTROWNO-LOGICTBL(,R2)    SUBTRACT "RE"  ROW   NO.
         ST    R0,ROWRELO              SAVE RELOCATION    VALUE
*
***********************************************************************
*  INITIALIZE TEMPORARY LOGIC TABLE LOAD VARIABLES                    *
***********************************************************************
         XC    CLONFRRE,CLONFRRE
         XC    CLONCRRE,CLONCRRE
         XC    CLONFRTK,CLONFRTK
         XC    CLONPVTK,CLONPVTK
         XC    CLONPVES,CLONPVES       ZERO PREVIOUS PEER "ES"
         MVC   PREVDDN,SPACES
         XC    SVTHRDID,SVTHRDID
*
***********************************************************************
*  CLONE DISPATCH QUEUE "ES" SET                                      *
***********************************************************************
         lgr   R5,R6
         BRAS  R10,CLONLOOP
*
***********************************************************************
*  LOOP THROUGH PEER "ES" CHAIN                                       *
***********************************************************************
         llgt  R5,LTSAMES#-LOGICTBL(,R6) LOAD PEER  "ES" CHAIN PTR
*
CLONPEER ltgr  R5,R5                     ANY  PEER  "ES" ???
         JNP   CLONNXTE
*
***********************************************************************
*  GET "RE" ROW ADDRESS  (BEGINNING OF COPIED LOGIC TABLE ROWS)       *
***********************************************************************
         llgt  R2,LTFRSTRE-LOGICTBL(,R5) LOAD FIRST "RE" ROW  ADDR
         ltgr  r2,r2
         JNP   CLONNXTP
*
***********************************************************************
*  CHECK IF THERE ARE ADDITIONAL PARTITIONS FOR THIS EVENT FILE       *
***********************************************************************
         llgt  R3,LTVDP200-LOGICTBL(,R2) LOAD VDP 200 RECORD ADDR
         ltgr  r3,r3
         JNP   CLONNXTP
*
         LTgf  R3,vdp0200b_MULT_PART_CHAIN-vdp0200b_FILE_RECORD(,R3)
         JNP   CLONNXTP
         USING vdp0200b_FILE_RECORD,R3
*
***********************************************************************
*  COMPUTE VIEW RELOCATION FACTOR                                     *
***********************************************************************
         lgf   R0,LTCOUNT
         sgf   R0,LTROWNO-LOGICTBL(,R2)
         ST    R0,ROWRELO
*
***********************************************************************
*  INITIALIZE TEMPORARY LOGIC TABLE LOAD VARIABLES                    *
***********************************************************************
         XC    CLONFRRE,CLONFRRE
         XC    CLONCRRE,CLONCRRE
         XC    CLONFRTK,CLONFRTK
         XC    CLONPVTK,CLONPVTK
         MVC   PREVDDN,SPACES
*
         LH    R0,LTESSET#-LOGICTBL(,R5) INITIALIZE SPECIFIC THREAD ID
         STH   R0,SVTHRDID
*
         BRAS  R10,CLONLOOP
*
***********************************************************************
*  ADVANCE TO NEXT PEER "ES"  (SAME "ES" SET NUMBER - SINGLE THREAD)  *
***********************************************************************
CLONNXTP llgt  R5,LTSAMES#-LOGICTBL(,R5)
         J     CLONPEER
*
***********************************************************************
*  ADVANCE TO NEXT "ES" ON DISPATCH QUEUE                             *
***********************************************************************
CLONNXTE llgt  R6,LTNEXTES-LOGICTBL(,R6)
         J     CLONQUE
*
***********************************************************************
*  ADVANCE TO NEXT DISPATCH QUEUE                                     *
***********************************************************************
CLONNXTQ LA    R4,4(,R4)        ADVANCE TO NEXT "ES" DISPATCH QUEUE
         llgt  R6,0(,R4)        LOAD FIRST DISPATCH QUEUE "ES"  ROW
         LA    R0,LTNXOTHR      LAST QUEUE ???
         cgr   R4,R0
         JNH   CLONQUE          NO  - LOOP THROUGH   ALL QUEUES
*
***********************************************************************
*  INSERT NEW "EN" ROW AT END OF CLONED VIEWS                         *
***********************************************************************
         ST    R7,LTENROWA        SAVE LAST ROW ("EN" ROW)  ADDRESS
*
         LHI   R0,LTEN_LEN
         STH   R0,LTROWLEN
*
         llgt  r14,=v(mr95en)     Point at EN function entry
         using functbl,r14
         XC    LTFLAGS,LTFLAGS    ZERO PROCESSING     FLAGS
         MVC   LTROWNO,LTCOUNT    ROW  NUMBER
         XC    LTVIEW#,LTVIEW#    ZERO VIEW NUMBER
         MVC   LTFUNC,EN          FUNCTION  CODE
         XC    LTCODSEG,LTCODSEG  ZERO GENERATED CODE SEGMENT   ADDRESS
         st    r14,LTFUNTBL       save EN function table address
         XC    LTLBADDR,LTLBADDR  ZERO LOOK-UP RECORD BUFFER    ADDRESS
         mvc   LTGENLEN,fccodeln  ZERO GENERATED      LENGTH
         drop  r14
*
         lgf   R14,LTCOUNT        CONVERT ROW NUMBER/COUNT TO ADDR
         bctgr R14,0
         sllg  R14,r14,2
         A     R14,LTROWADR
         ST    R7,0(,R14)
*
***********************************************************************
*  RETURN                                                             *
***********************************************************************
CLONEXIT lmg   R14,R12,SAVF4SAG64RS14 RESTORE REGISTERS
         BR    R14
*
         DROP  R7
                        EJECT
***********************************************************************
*  SAVE CLONED LOGIC TABLE ROW ADDRESS IN ROW ADDRESS TABLE           *
***********************************************************************
*                                                                     *
*        R2  - SOURCE LT ROW ADDRESS                                  *
*        R3  - VDP   200 RECORD ADDRESS                               *
*        R5  - ENDING ES ROW ADDRESS                                  *
*        R7  - TARGET LT ROW ADDRESS                                  *
*                                                                     *
***********************************************************************
         USING LOGICTBL,R2
*
CLONLOOP do inf                   Loop down the lt forever
*
           select clc,ltfunc,eq
*
           when   (re_nx,re_ex)         READ ??  (NOT RE_TK)
***********************************************************************
*            PROCESS "RE" FUNCTION                                    *
***********************************************************************
*
             BRAS R9,CLONCOPY     COPY   "RE"  ROW
*
clonelt      USING LOGICTBL,R7
*
             OI clonelt.LTFLAG1,LTESCLON  MARK   "RE"  AS   CLONE
             ST R2,clonelt.LTCLONRE       SAVE CLONED "RE"  ROW
*
             XC clonelt.ltlognv,clonelt.ltlognv ZERO VIEW ROW ADDR
*
***********************************************************************
*      CHAIN "RE'S" WITHIN THIS "ES" SET                              *
***********************************************************************
             XC LTNEXTRE,LTNEXTRE ZERO   NEXT     "RE"   ROW ADDRESS
             llgt R14,CLONCRRE    SAVE   PREVIOUS "RE"   ROW ADDRESS
             ST R7,CLONCRRE       UPDATE CURRENT  "RE"   ROW ADDRESS
             if ltgr,R14,R14,P    PREVIOUS  "RE"  ROW    EXISTS  ???
               ST R7,LTNEXTRE-LOGICTBL(,R14) YES - CHAIN "RE"   ROWS
             endif
*
             if OC,CLONFRRE,CLONFRRE,z                                 +
                                  SAVE   FIRST "RE" ROW WITHIN "ES" SET
               ST R7,CLONFRRE
             endif
*
***********************************************************************
*      COPY VDP 0200 FILE RECORD ATTRIBUTES                           *
***********************************************************************
             lgr R15,R3            SET-UP SECOND BASE REGISTER FOR VDP
             AgHI R15,4096
             USING VDP0200b_FILE_RECORD,R3,R15
*
             ST R3,clonelt.ltVDP200 CHANGE VDP "200" RECORD   ADDRESS
             OC    VDP0200b_FILE_READER,VDP0200b_FILE_READER
             JNZ   CLONNSAV
             ST R7,VDP0200b_FILE_READER
*
CLONNSAV     DS   0H
             L R0,VDP0200b_ALLOC_FILE_TYPE
             STH R0,clonelt.ltFILTYP COPY FILE TYPE  FROM "VDP"
*
             L R0,VDP0200b_ACCESS_METHOD_ID
             STH R0,clonelt.ltACCMTH COPY ACCESS METHOD FROM "VDP"
*
             MVC clonelt.ltDDNAME,VDP0200b_DDNAME_INPUT
*
             MVC clonelt.ltDSNAME,VDP0200b_DSN
             MVC clonelt.ltVOLSER,VDP0200b_ALLOC_VOL_SER
*
***********************************************************************
*      CHAIN "TOKEN READER" VIEWS WITHIN "ES" SET                     *
***********************************************************************
             if  TM,clonelt.ltFLAG2,LTRTOKEN,o TOKEN READING VIEW ???
*
               llgt r14,CLONPVTK    SAVE   PREVIOUS "RETK" ROW ADDRESS
               ST R7,CLONPVTK     UPDATE CURRENT  "RETK" ROW ADDRESS
               if ltgr,R14,R14,p  PREVIOUS  "RETK" ROW   EXISTS  ???
                 ST R7,LTNVTOKN-LOGICTBL(,R14) YES - CHAIN "RETK" ROWS
               endif
*
               if OC,CLONFRTK,CLONFRTK,z FIRST "TOKEN   READER" ???
                 ST R7,CLONFRTK   YES -  SAVE    ROW     ADDRESS
               endif
*
*
             else
***********************************************************************
*  MAKE LOGIC TABLE FUNCTION CODE MATCH VDP200 REC ("RENX" VS "REEX") *
***********************************************************************
               XC clonelt.ltREEXIT,clonelt.ltREEXIT
               XC clonelt.ltre_next_exit,clonelt.ltre_next_exit
               MVC clonelt.ltRENAME,SPACES
               XC clonelt.ltREENTP,clonelt.ltREENTP
               XC clonelt.ltREWORK,clonelt.ltREWORK
               MVC clonelt.ltREPARM,SPACES
               la  r0,clonelt.ltre_next_exit
               st  r0,exit_next
               lgr r11,r0
*
               if CLC,clonelt.ltFUNC,ne,RE_TK
*
                 MVC clonelt.ltFUNC,RE_NX
*
                 if  ltgf,R1,vdp0200b_INPUT_EXIT_PGM_ID,p
*
                   ST R1,clonelt.ltREEXIT
                  MVC clonelt.ltREPARM,vdp0200b_INPUT_EXIT_STARTUP_PARM
*
                   DROP R3,R15
*
                   lgr R3,R9
                   BRAS R9,LOCEXIT LOCATE VDP EXIT PGM RECORD (210)
                   LgR R9,R3
                   USING vdp0210b_EXIT_PGM_RECORD,R15
                   if  LTgR,R15,R15,p
*
                     MVC clonelt.ltRENAME,vdp0210b_MODULE_NAME
                     MVC clonelt.ltFUNC,RE_EX
*
                     DROP R15
                   endif
                 endif
               endif
*
*              LR R15,R7
*              AH R15,0(,R7)
*              BCTR R15,0
*          ly  r2,SNAPDCBA
*             SNAP DCB=(r2),ID=119,PDATA=(REGS),STORAGE=((R7),(R15))
*
             endif
             lgh R0,clonelt.ltROWLEN ADVANCE TO NEXT TARGET ROW
             Agr R7,r0               ADVANCE TO NEXT TARGET ROW
*
             L R0,LTCOUNT         INCREMENT  LOGIC  TABLE RECORD COUNT
             AHI R0,1
             ST R0,LTCOUNT

                            EJECT
***********************************************************************
*            E N D O F S E T  -  "ES"                                 *
***********************************************************************
           when   es                    end of set
             BRAS R9,CLONCOPY     COPY  "ES"  ROW
*
             OI clonelt.ltFLAG1,LTESCLON MARK "ES" AS CLONE
             ST R2,clonelt.ltCLONES SAVE CLONED  "ES"  ROW
*
             XC clonelt.ltNEXTES,clonelt.ltNEXTES ZERO NEXT "ES" Ptr
             XC clonelt.ltESVNAM,clonelt.ltESVNAM ZERO 1st VAR NAME
*
             llgt R14,CLONFRRE    SET   FIRST    "RE"  WITHIN  "ES" SET
             ST R14,clonelt.ltFRSTRE
             ST R7,LTREES-LOGICTBL(,R14)
*
***********************************************************************
*      CHECK FOR CONCATENATED EVENT FILES                             *
***********************************************************************
             LH R0,clonelt.ltESSET#
             do ,
               if LTR,R0,R0,p,                                         +
               andif,ch,r0,eq,svthrdid,                                +
               andif,lt,r14,clonpves,p
*
                 llgt R1,LTSAMES#-LOGICTBL(,R14)
                 ST R7,LTSAMES#-LOGICTBL(,R14)
                 ST R1,clonelt.ltSAMES#
*

               else
*
***********************************************************************
*       APPEND "ES" ROW TO APPLICABLE "ES" CHAIN BASED ON THREAD TYPE *
***********************************************************************
                 L R14,ESSETCNT   INCREMENT DISPATCH "ES" SET COUNT
                 AHI R14,1
                 ST R14,ESSETCNT
*
                 if ltgf,R14,clonelt.ltFRSTRE,np Any "RE"s IN "ES" ?
*
***********************************************************************
*          DEFAULT THREAD TYPE TO "DISK" IF NO "RE" PRESENT           *
***********************************************************************
                   LHI R0,DISKDEV LOAD THREAD TYPE
                   LA R1,PREVDISK APPEND  TO "OTHER" CHAIN
*
                 else
                   LH R0,LTFILTYP-LOGICTBL(,R14) LOAD EVENT FILE TYPE
*
                   doexit CHI,R0,eq,TOKENDEV TOKEN SET   ???
*
                   select chi,r0,eq
                     when pipedev pipe thread ???
*
                       LA R1,PREVOTHR APPEND TO "OTHR" CHAIN
*
                       L R15,OTHRCNT
                       AHI R15,1
                       ST R15,OTHRCNT
*
                     when DISKDEV DISK  THREAD ???
*
                       LA R1,PREVDISK APPEND TO "DISK" CHAIN
*
                       L R15,DISKCNT
                       AHI R15,1
                       ST R15,DISKCNT
*
                     when TAPEDEV TAPE  THREAD ???
                       LA R1,PREVTAPE APPEND TO "TAPE" CHAIN
*
                       L R15,TAPECNT
                       AHI R15,1
                       ST R15,TAPECNT
*
                     othrwise
                       Llgt R7,clonelt.ltFRSTRE POINT TO "RE" ROW
*
                     lghi R14,SOURCE_FILE_TYPE_ERR UNDEF  FILE TYPE
                       J STDERROR
                   endsel
*
                 endif
***********************************************************************
*          ADD "ES" TO APPLICABLE CHAIN                               *
***********************************************************************
                 llgt R14,0(,R1)  SAVE  PREVIOUS "ES"   ROW  ON CHAIN
                 ST R7,0(,R1)     ADD   NEW      "ES"   ROW  TO CHAIN
*
                 if LTgR,R14,R14,np No PREVIOUS "ES"  ???
*
                   LA R1,LTNXOTHR ASSUME PIPE/TOKEN
*
                   if CHI,R0,eq,DISKDEV DISK DEVICE ???
                     LA R1,LTNXDISK
                   endif
*
                   if CHI,R0,eq,TAPEDEV TAPE DEVICE ???
                     LA R1,LTNXTAPE
                   endif
*
                   ST R7,0(,R1)   SET  FIRST  AVAILABLE THREAD
                 else
*
                   ST R7,LTNEXTES-LOGICTBL(,R14)
                 endif
*
               endif
             enddo
***********************************************************************
*      RESET LOGIC TABLE ROW ADDRESSES FOR THIS "ES" SET              *
***********************************************************************
             ST R7,CLONPVES       UPDATE  PREVIOUS "ES"   ROW ADDR
*
             XC CLONFRRE,CLONFRRE
             XC CLONCRRE,CLONCRRE
             XC CLONFRTK,CLONFRTK
             XC CLONPVTK,CLONPVTK
*
             lgh R0,clonelt.ltROWLEN ADVANCE TO NEXT TARGET ROW
             Agr R7,r0               ADVANCE TO NEXT TARGET ROW
*
             L R0,LTCOUNT         INCREMENT  LOGIC  TABLE RECORD COUNT
             AHI R0,1
             ST R0,LTCOUNT
*            LH R14,VIEWCNT       INCREMENT  VIEW COUNT
*            AHI R14,1
*            STH R14,VIEWCNT
           when   EN                    END-OF-LOGIC-TBL  ???
             leave ,              YES -  DONE
           othrwise
*            skipping an lt entry - so adjust rowrelo to match
             l   r0,rowrelo
             bctr r0,0
             st  r0,rowrelo
           endsel
           DROP clonelt
                          EJECT
***********************************************************************
*    UPDATE SIZE OF GENERATED CODE                                    *
***********************************************************************
*
           lgh R14,0(,R2)          ADVANCE  TO NEXT  SOURCE ROW
           agr R2,r14             ADVANCE  TO NEXT  SOURCE ROW
*
***********************************************************************
*    CHECK IF BEYOND SOURCE "ES" ROW                                  *
***********************************************************************
           doexit CgR,R2,gt,r5     BEYOND  "ES"  ROW ???
*
           llgt R0,LTBEGIN        COMPUTE REMAINING LOGIC TABLE SPACE
           agf  R0,LTSIZE
           sgr  R0,R7
           if  CHI,R0,LT,LTCUSH   CUSHION exhauseted ???
*
             lghi R14,LTBL_OVERFLOW LOAD  ERROR MESSAGE NUMBER
             J STDERROR           PRINT ERROR MESSAGE - STOP
           endif
*
         enddo
***********************************************************************
*  RETURN                                                             *
***********************************************************************
CLONRETN BR    R10                RETURN
*
         DROP  R2
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O P Y   C L O N E D   L O G I C   T A B L E   R O W        *
*                                                                     *
*        R2  - SOURCE LT ROW ADDRESS                                  *
*        R7  - TARGET LT ROW ADDRESS                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         USING LOGICTBL,R7
source   USING LOGICTBL,R2
*
CLONCOPY lgf   R14,LTCOUNT        CONVERT ROW NUMBER/COUNT TO  ADDR
         bctgr R14,0
         sllg  R14,r14,2
         agf   R14,LTROWADR
         ST    R7,0(,R14)         FILL-IN NEXT LOGIC TABLE ROW ADDR
*
***********************************************************************
*  COPY LOGIC TABLE ROW                                               *
***********************************************************************
         lgr   R0,R7                LOAD TARGET LOGIC TABLE ROW ADDR
         LH    R1,source.LTROWLEN SOURCE LOGIC TABLE ROW LEN
         LR    R15,R1
         lgr   R14,R2               LOAD SOURCE LOGIC TABLE ROW ADDR
         MVCL  R0,R14
*
***********************************************************************
*  RELOCATE "GO TO" ROW NUMBERS                                       *
***********************************************************************
         lgf   R0,LTTRUE            RELOCATE ORIGINAL TRUE  ROW
         agf   R0,ROWRELO
         ST    R0,LTTRUE
*
         lgf   R0,LTFALSE           RELOCATE ORIGINAL FALSE ROW
         agf   R0,ROWRELO
         ST    R0,LTFALSE
*
***********************************************************************
*  UPDATE COMMON FIELDS                                               *
***********************************************************************
         MVC   LTROWNO,LTCOUNT    CHANGE  ROW NUMBER
*
         BR    R9
*
         DROP  R7,source          LOGIC    TABLE
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C H E C K   F O R   V I E W   S P E C I F I C   T R A C E    *
*                                                                     *
*        (VIEWS WHICH ARE EXPLICITLY IDENTIFIED OR READ AN            *
*         EXPLICITLY SPECIFIED EVENT DDNAME ARE LINKED TO             *
*         THEIR RELATED TRACE PARAMETER RECORD)                       *
*                                                                     *
*        (EVENT FILES WHOSE RECORDS ARE INCLUDED AS A HEX DUMP        *
*         IN THE TRACE ARE ALSO FLAGGED)                              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         USING LOGICTBL,R7
*
TRACPARM llgt  R15,PARMTBLA       LOAD  PARAMETER TABLE(CHAIN)  ADDRESS
         USING PARMTBL,R15
*
TRAC_10  ltgr  R15,R15            END-OF-CHAIN    ???
         JNP   TRAC_90            YES -  EXIT
*
         CLI   VIEWTRAC,C'Y'      SPECIFIC  VIEWS SPECIFIED ???
         JNE   TRAC_50
*
         ltgf  R0,PARMVIEW        SPECIFIC   VIEW ???
         JNP   TRAC_50            NO  - CHECK FOR SPECIFIC DDNAME
*
         cgf   R0,LTVIEW#         MATCHING   VIEW NUMBER   ???
         JNE   TRAC_50            NO  - ADVANCE
*
         ST    R15,LTPARMTB       SAVE  PARAMETER TABLE  ENTRY  ADDRESS
*
TRAC_50  llgt  R15,PARMNEXT
         J     TRAC_10            LOOP  THROUGH   CHAIN
*
TRAC_90  BR    R14                RETURN
*
         DROP  R7
         DROP  R13
         DROP  R15
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*     C O N V E R T   L I K E   E X P R E S S I O N                   *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         USING LOGICTBL,R7
         USING LTF1_REC,R3
*
*
         DROP  R3
         DROP  R7
         DROP  R13
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*     C O N V E R T   M A S K   C O D E                               *
*                                                                     *
*                                                                     *
*   REGISTER USAGE                                                    *
*                                                                     *
*        R0  - COLUMN    WIDTH                                        *
*        R1  - MASK CODE ADDR                                         *
*        R14 - CONVERTED MASK ADDRESS                                 *
*        R15 - RETURN    CODE             (0 = NOT FOUND)             *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         USING LOGICTBL,R7
maskcode_map   dsect
maskid   ds    cl5
maskflag ds    c
mask     ds    cl32
maskcode_map_l equ *-maskid
gvbmr96  csect
code     loctr
*
MASKCONV larl  R15,MASKCODE
*
         CLC   0(L'LNNAN,R1),LNNAN     NO MASK  CODE ???
         JE    MASKNOTF                YES - RETURN
*
         using maskcode_map,r15        use mapping
MASKLOOP CLC   maskid,HEXFF            END-OF-TABLE  ???
         JE    MASKNOTF                YES -  RETURN
*
         CLC   maskid,0(R1)    MATCHING CODE  ???
         JE    MASKFND
*
         aghi  R15,maskcode_map_l  INCREMENT TABLE ENTRY ADDRESS
         J     MASKLOOP
*
MASKFND  lgr   R1,R0               SAVE    COLUMN  WIDTH
         LA    R0,maskflag         "EDITED NUMERIC COMPATIBLE" FLAG
*
         aghi  R15,maskcode_map_l  ADVANCE TO  END OF MASK
         sgr   R15,R1
*
         BCTR  R1,0
         EX    R1,MASKCOPY
static   loctr
MASKCOPY MVC   0(0,R14),maskid                * * E X E C U T E D * * *
code     loctr
*
         J     MASKEXIT
*
MASKNOTF xgr   R15,R15
*
MASKEXIT BR    R9                  RETURN
*
         drop  r15
         DROP  R7
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        P R I N T   C O N T R O L   R E P O R T   H E A D I N G S    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
static   loctr
         USING THRDAREA,R13
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
         ds    0d
mvcprnt  mvc   prntline(0),2(r3)
code     loctr
*
PRNTHDR  ds    0h
*
***********************************************************************
* CONTROL REPORT HEADER - CALL GVBUTHDR                               *
***********************************************************************
         LA    R1,HDRLIST
         USING HEADERPR,R1      LOAD PARMS TO DSECT TO CALL
         la    r5,typepgm       gvbuthdr
         st    r5,pgmtype
         la    r5,typepgml      gvbuthdr
         st    r5,pgmtypel
         la    r5,namepgm
         st    r5,pgmname
         la    r5,namepgml
         st    r5,pgmnamel
* RTC20813
         if clc,namepgm,eq,=cl8'GVBMR95R'    R for reference
           mvc titpgm,=cl34'Reference Data Preparation Process'
         endif
         la    r5,titpgm
         st    r5,pgm_title
         la    r5,titpgml
         st    r5,pgm_title_ln
         la    r5,rpt_95ctrl
         st    r5,rpt_title
         la    r5,rpt_95ctrll
         st    r5,rpt_title_ln
         la    r5,rpt_dd95c
         st    r5,rptddn
         la    r5,dblwork
         st    r5,rpt_reccnt
         L     R3,ADDBUFF
         ST    R3,BUFFADD
         LA    R5,BUFLGTH
         ST    R5,BUFFLGTH
         DROP R1
*
         llgf  R15,GVBHDRA      Call GVBUTHDR
         bassm R14,R15
*
***********************************************************************
*  Print Control Report heading lines                                 *
***********************************************************************
*
         lgh   r5,dblwork       Get number of lines in heading
*                                 includes blank lines
prntc_loop ds 0h
         lgh   r6,0(r3)         get the length of the line
         la    r15,4(,r6)        allow for rdw
         sth   r15,prntrdwh      save RDW
         xc    prntrdwl,prntrdwl zero 2nd half word of RDW
         bctr  r6,r0            decrement for "EX"
         ex    r6,mvcprnt       move in header line
         rptit ,                print it
         la    r3,2+1(r6,r3)    next header line
         jct   r5,prntc_loop    Go print next line
*
***********************************************************************
* Log REPORT HEADER - CALL GVBUTHDR                                   *
***********************************************************************
         LA    R1,HDRLIST
         USING HEADERPR,R1      LOAD PARMS TO DSECT TO CALL
         la    r5,typepgm       gvbuthdr
         st    r5,pgmtype
         la    r5,typepgml      gvbuthdr
         st    r5,pgmtypel
         la    r5,namepgm
         st    r5,pgmname
         la    r5,namepgml
         st    r5,pgmnamel
         la    r5,titpgm
         st    r5,pgm_title
         la    r5,titpgml
         st    r5,pgm_title_ln
         la    r5,rpt_95log
         st    r5,rpt_title
         la    r5,rpt_95logl
         st    r5,rpt_title_ln
         la    r5,rpt_dd95l
         st    r5,rptddn
         la    r5,dblwork
         st    r5,rpt_reccnt
         L     R3,ADDBUFF
         ST    R3,BUFFADD
         LA    R5,BUFLGTH
         ST    R5,BUFFLGTH
         DROP R1
*
         llgf  R15,GVBHDRA      Call GVBUTHDR
         bassm R14,R15
*
***********************************************************************
*  Print Log Report heading lines                                     *
***********************************************************************
*
         lgh   r5,dblwork       Get number of lines in heading
*                                 includes blank lines
prntl_loop ds 0h
         lgh   r6,0(r3)         get the length of the line
         la    r15,4(,r6)        allow for rdw
         sth   r15,prntrdwh     save RDW
         xc    prntrdwl,prntrdwl zero 2nd half word of RDW
         bctr  r6,r0            decrement for "EX"
         ex    r6,mvcprnt       move in header line
         logit ,                print it
         la    r3,2+1(r6,r3)    next header line
         jct   r5,prntl_loop    Go print next line


         br    r9               return
*
         macro
         prtparm &msg=?,&p1=?,&p2=?,&p1l=0,&p2l=0,&po1=0,&po2=0
         aif  ('&msg' eq '?').ppmexit1
         lay   r14,&msg
         la    r15,l'&msg-1
         ex    r15,initpmv
         aif  ('&p1' eq '?').ppmp2
         mvc   prntline+&po1.(&p1l),&p1  Move in parm
.ppmp2    anop
         aif  ('&p2' eq '?').ppmp3
         mvc   prntline+&po2.(&p2l),&p2  Move in parm
.ppmp3    anop
         rptit ,
.ppmexit anop
         mend
*
* Macro: Skip leading Zeros
*
*  R1 will point to start of number, or 0 if all zeros
*  R2 = length of number -1
*
         macro
         skipzero &parm=?
         aif  ('&parm' eq '?').szmexit
         la    r1,&parm
         la    r2,&parm+l'&parm-1
         DO while=(cr,r1,lt,r2)
            doexit (cli,0(r1),ne,c'0')
            AHI R1,1               move along one character
         ENDDO
         sr  r2,r1  get length
.szmexit anop
         mend
*
***********************************************************************
*  PRINT PARAMETER RECAP SECTION - LINE 1                             *
***********************************************************************
*
PRNTRPT  DS    0H
         sysstate amode64=NO
         sam31
*
         rptit msg=vb_blankl
         phead hd=opts            Print the OPTS header
*
         llgt  R3,EXECDADR        SAVE  PARAMETER  LENGTH       ADDRESS
         USING EXECDATA,R3
*
         la    R4,PARMKWRD_table  Address the parm keyword table
         USING parmkwrd_map,R4
*
* Loop through Parmkwrd_table
*
         do while=(CLI,parmkwrd,ne,x'ff')
*
           mvc   prntline(l'parmkwrd),parmkwrd
           mvi   prntline+l'parmkwrd,c' '
           mvi   prntline+l'parmkwrd+1,c'='
           mvi   prntline+l'parmkwrd+2,c' '
*
           lgh R14,parmnum
           casentry r14
*
            case 3   EXECUTE_IN_SINGLE_THREAD_MODE
*
             mvc   prntline+l'parmkwrd+3(l'EXECSNGL),EXECSNGL  value
             la    r15,l'parmkwrd+3+l'EXECSNGL+l'prntrdw len for RDW
             sth   r15,prntrdwh
             rptit ,                       write to report
*
            case 4   TRACE
*
             mvc   prntline+l'parmkwrd+3(l'EXECTRAC),EXECTRAC  value
             la    r15,l'parmkwrd+3+l'EXECTRAC+l'prntrdw len for RDW
             sth   r15,prntrdwh
             rptit ,                       write to report

            case 5   DUMP_LT_AND_GENERATED_CODE
*
             mvc   prntline+l'parmkwrd+3(l'EXECSNAP),EXECSNAP  value
             la    r15,l'parmkwrd+3+l'EXECSNAP+l'prntrdw len for RDW
             sth   r15,prntrdwh
             rptit ,                       write to report
*
            case 6   MAX_NBR_OF_PARALLEL_DISK_THREADS
*
             skipzero parm=execdisk
*            r1 -> number, r2=length
             ex    r2,mvc_opt
             la    r15,l'parmkwrd+4+l'prntrdw(r2) len for   RDW
*            mvc   prntline+l'parmkwrd+3(l'EXECDISK),EXECDISK  value
*            la    r15,l'parmkwrd+3+l'EXECDISK+l'prntrdw len for RDW
             sth   r15,prntrdwh
             rptit ,                       write to report

            case 7   MAX_NBR_OF_PARALLEL_TAPE_THREADS
*
             skipzero parm=exectape
*            r1 -> number, r2=length
             ex    r2,mvc_opt
             la    r15,l'parmkwrd+4+l'prntrdw(r2) len for   RDW
*            mvc   prntline+l'parmkwrd+3(l'EXECTAPE),EXECTAPE  value
*            la    r15,l'parmkwrd+3+l'EXECTAPE+l'prntrdw len for RDW
             sth   r15,prntrdwh
             rptit ,                       write to report

            case 8   STOP_AFTER_SOURCE_RECORD_NBR
*
*            Skip leading zeros
             skipzero parm=execrlim
*            r1 -> number, r2=length
             ex   r2,mvc_opt
             la   r15,l'parmkwrd+4+l'prntrdw(r2) len for   RDW
             sth   r15,prntrdwh
             rptit ,                       write to report

            case 9   TREAT_MISSING_VIEW_OUTPUTS_AS_DUMMY
*
             mvc   prntline+l'parmkwrd+3(l'EXECDUMY),EXECDUMY  value
             la    r15,l'parmkwrd+3+l'EXECDUMY+l'prntrdw len for RDW
             sth   r15,prntrdwh
             rptit ,                       write to report

            case 10  PLAN_NAME_FOR_DB2_SQL
*
             mvc   prntline+l'parmkwrd+3(l'EXECSPLN),EXECSPLN  value
             la    r15,l'parmkwrd+3+l'EXECSPLN+l'prntrdw len for RDW
             sth   r15,prntrdwh
             rptit ,                       write to report

**          don't show this
**          case 11  PLAN_NAME_FOR_DB2_VSAM
*
**           mvc   prntline+l'parmkwrd+3(l'EXECVPLN),EXECVPLN  value
**           la    r15,l'parmkwrd+3+l'EXECVPLN+l'prntrdw len for RDW
**           sth   r15,prntrdwh
**           rptit ,                       write to report

            case 12  USE_ZIIP
*
             mvc   prntline+l'parmkwrd+3(l'EXECZIIP),EXECZIIP  value
             la    r15,l'parmkwrd+3+l'EXECZIIP+l'prntrdw len for RDW
             sth   r15,prntrdwh
             rptit ,                       write to report

            case 13  MAX_NBR_OF_ZIIP_THREADS
*
*            Skip leading zeros
             skipzero parm=exec_srblimit
*            r1 -> number, r2=length
             ex   r2,mvc_opt
             la   r15,l'parmkwrd+4+l'prntrdw(r2) len for   RDW
*            mvc prntline+l'parmkwrd+3(l'EXEC_SRBLIMIT),EXEC_SRBLIMIT
*            la  r15,l'parmkwrd+3+l'EXEC_SRBLIMIT+l'prntrdw len for RDW
             sth   r15,prntrdwh
             rptit ,                       write to report

            case 14  ABEND_ON_CALCULATION_OVERFLOW
*
             mvc prntline+l'parmkwrd+3(l'execovfl_on),execovfl_on
             la  r15,l'parmkwrd+3+l'execovfl_on+l'prntrdw len for   RDW
             sth   r15,prntrdwh
             rptit ,                       write to report

            case 15  ALLOW_PAGING_OF_IO_BUFFERS
*
             mvc prntline+l'parmkwrd+3(l'execpagf),execpagf
             la  r15,l'parmkwrd+3+l'execpagf+l'prntrdw len for   RDW
             sth   r15,prntrdwh
             rptit ,                       write to report

            case 16  RUN_DATE
*
             mvc prntline+l'parmkwrd+3(l'exec_rdate),exec_rdate
             la  r15,l'parmkwrd+3+l'exec_rdate+l'prntrdw len for   RDW
             sth   r15,prntrdwh
             rptit ,                       write to report

            case 17  Fiscal_date_default
*
             mvc prntline+l'parmkwrd+3(l'exec_fdate),exec_fdate
             la  r15,l'parmkwrd+3+l'exec_fdate+l'prntrdw len for RDW
             sth   r15,prntrdwh
             rptit ,                       write to report
*

**          don't show this
**          case 18  PLAN_NAME_FOR_DB2_CATALOG
*
**           mvc prntline+l'parmkwrd+3(l'EXECTPLN),EXECTPLN
**           la  r15,l'parmkwrd+3+l'EXECTPLN+l'prntrdw len for   RDW
**           sth   r15,prntrdwh
**           rptit ,                       write to report

            case 19  ABEND_ON_MESSAGE_NBR
*
*            Skip leading zeros
             skipzero parm=execmsgab
*            r1 -> number, r2=length

             ex   r2,mvc_opt
             la   r15,l'parmkwrd+4+l'prntrdw(r2) len for   RDW
             sth  r15,prntrdwh
             rptit ,                       write to report

            case 20  ABEND_ON_LOGIC_TABLE_ROW_NBR
*
*            Skip leading zeros
             skipzero parm=execltab
*            r1 -> number, r2=length

             ex   r2,mvc_opt
             la  r15,l'parmkwrd+4+l'prntrdw(r2) len for   RDW
             sth   r15,prntrdwh
             rptit ,                       write to report
*
            case 21  IO_BUFFER_LEVEL
*
*            Skip leading zeros
             skipzero parm=execmsdn
*            r1 -> number, r2=length

             ex   r2,mvc_opt
             la  r15,l'parmkwrd+4+l'prntrdw(r2) len for   RDW
             sth   r15,prntrdwh
             rptit ,                       write to report
*
**          don't show this
**          case 22  DB2_VSAM_DATE_FORMAT
*
**           mvc prntline+l'parmkwrd+3(l'EXEC_db2_df),EXEC_db2_df
**           la  r15,l'parmkwrd+3+l'EXEC_db2_df+l'prntrdw len for RDW
**           sth   r15,prntrdwh
**           rptit ,                       write to report
*
            case 23  FISCAL_DATE_OVERRIDE
*
* Loop through the fiscal_date_table records to get fiscal_dates
         ltgf  r5,fisdates
         jnp fis_byp
         USING Fiscal_date_entry,R5
*
Fis_loop ds    0h
         xgr   r0,r0
         ly    R0,fiscal_recid         Get the record id
         cvd   r0,dblwork
         mvc   workarea(12),recidmsk
         edmk  workarea(12),dblwork+2
*
         la    r15,workarea+11         point to end of record id
         sr    r15,r1                  length of number -1
*
         la    r2,prntline+3+l'parmkwrd
         ex    r15,mvc_recid           move fiscal_recid
         la    r2,1(r15,r2)            point after recid
         mvi   0(r2),c':'
         mvc   1(8,r2),fiscal_date     get the fiscal_date
         la    r15,l'parmkwrd+13+l'prntrdw(r15)  set length for RDW
         sth   r15,prntrdwh            set rdw
         rptit ,
*
         ltgf  r5,fiscal_next
         jp    Fis_loop
         j     fis_bypx
*
fis_byp  ds    0h
         mvi   prntline+l'parmkwrd+3,c'N'   no value specified
         la    r15,l'parmkwrd+3+1+l'prntrdw len for RDW
         sth   r15,prntrdwh
         rptit ,                       write to report

fis_bypx ds    0h
*
static   loctr ,
mvc_recid mvc  0(0,r2),0(r1)                 move record id
recidmsk DC    X'402020202020202020202021'   Record id mask
code     loctr ,
*
            case 24  Optimize_packed_output
*
             mvc   prntline+l'parmkwrd+3(l'exec_optpo),exec_optpo value
             la    r15,l'parmkwrd+3+l'exec_optpo+l'prntrdw len for RDW
             sth   r15,prntrdwh
             rptit ,                       write to report
*
            case 25  ABEND_ON_ERROR_CONDITION
*
             mvc prntline+l'parmkwrd+3(l'exec_UABEND),exec_UABEND value
             la   r15,l'parmkwrd+3+l'exec_UABEND+l'prntrdw len for RDW
             sth  r15,prntrdwh
             rptit ,                       write to report
*
            case 26  ESTAE Y/N Recover_from_abend
*
             mvc prntline+l'parmkwrd+3(l'exec_ESTAE),exec_ESTAE value
             la   r15,l'parmkwrd+3+l'exec_ESTAE+l'prntrdw len for RDW
             sth  r15,prntrdwh
             rptit ,                       write to report
*
            Case 27  INCLUDE_REF_TABLES_IN_SYSTEM_DUMP Y/N
*
             MVC PRNTLINE+L'PARMKWRD+3(L'EXEC_Dump_Ref),EXEC_Dump_Ref
             LA   R15,L'PARMKWRD+3+L'EXEC_Dump_Ref+L'PRNTRDW        RDW
             STH  R15,PRNTRDWH
             RPTIT ,                       Write to report
*
            case 28  Log_message_level
             mvc prntline+l'parmkwrd+3(l'exec_loglvl),exec_loglvl value
             la   r15,l'parmkwrd+3+l'exec_loglvl+l'prntrdw len for RDW
             sth  r15,prntrdwh
             rptit ,                       write to report
*
            case 29  VERIFY_CREATION_TIMESTAMP
             mvc prntline+l'parmkwrd+3(1),EXEC_check_timestamp    value
             la   r15,l'parmkwrd+3+1+l'prntrdw              len for RDW
             sth  r15,prntrdwh
             rptit ,                       write to report
*
            case 30  HASH_PACK  - hidden parm (display if debug on)

             if CLC,EXEC_LOGLVL,EQ,LOG_DEBUG DEBUG level logging?
               mvc prntline+l'parmkwrd+3(l'exec_hashpack),exec_hashpack
               la r15,l'parmkwrd+3+l'exec_hashpack+l'prntrdw
               sth r15,prntrdwh
               rptit ,                     write to report
             endif
*
            case 31  HASH_MULT  - hidden parm (display if debug on)
             if CLC,EXEC_LOGLVL,EQ,LOG_DEBUG DEBUG level logging?
               mvc prntline+l'parmkwrd+3(l'EXEC_HASHMULT),EXEC_HASHMULT
               la r15,l'parmkwrd+3+l'EXEC_HASHMULT+l'prntrdw
               sth r15,prntrdwh
               rptit ,                     write to report
             endif
*
            case 32  DISPLAY_HASH - hidden parm (display if debug on)
             if CLC,EXEC_LOGLVL,EQ,LOG_DEBUG DEBUG level logging?
               mvc prntline+l'parmkwrd+3(l'exec_disphash),exec_disphash
               la r15,l'parmkwrd+3+l'exec_disphash+l'prntrdw
               sth r15,prntrdwh
               rptit ,                     write to report
             endif
*
            endcase
            AHI R4,parmkwrd_l
         enddo                   End of looping through Parmkwrd_table
         BR  r9                  return
*
static    loctr ,
mvc_opt     mvc prntline+l'parmkwrd+3(0),0(r1)
code      loctr ,
*
         DROP  R3,r4
         sysstate amode64=YES
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        A L L O C A T E   M E M O R Y   W O R K   A R E A S          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,savesubr
*
MEMALLOC stmg  R14,R12,SAVF4SAG64RS14   SAVE REGISTERS
*
***********************************************************************
*  ALLOCATE MACHINE CODE BUFFER                                       *
***********************************************************************
         lgf   R0,CODESIZE        LOAD  ESTIMATED CODE BUFFER SIZE
         aghi  R0,CODECUSH        ADD   CUSHION
         ST    R0,CODESIZE        SAVE  BUFFER    SIZE
*
         aghi  R0,8               ADD   EYEBALL LENGTH
         GETMAIN RU,LV=(0),LOC=(ANY),BNDRY=PAGE OBTAIN MEMORY
*
         MVC   0(8,R1),CODEEYEB   COPY  EYEBALL
         aghi  R1,8               SKIP  OVER   EYEBALL
         ST    R1,CODEBEG         SAVE  CODE BEGINNING ADDRESS
*
         agf   R1,CODESIZE        SAVE  END-OF-BUFFER  ADDRESS
         ST    R1,CODEEND
*
*
***********************************************************************
*  RETURN                                                             *
***********************************************************************
         lmg   R14,R12,SAVF4SAG64RS14    RESTORE   REGISTERS
         BR    R14                       RETURN
*
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        A L L O C A T E   L I T P O O L   W O R K   A R E A S        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,savesubr
*
ALLOCLIT stmg  R14,R12,SAVF4SAG64RS14   SAVE REGISTERS
*
***********************************************************************
*  ALLOCATE LITERAL POOL                                              *
***********************************************************************
*
*     old calculation
*
*        lgh   R0,VIEWCNT         Number of views
*        sllg  r0,r0,12
*        mhy   r0,=h'6144'        Multiple by 6 k pages, not 4.
*        lgf   R1,litpcnt         TIMES NO.  ES sets
*        lgf   R1,litpcnt         TIMES NO.  WORK UNITS
*        aghi  r1,1               just in cae it is zero           pgc3
*        lgf   r15,tokncnt        get number of tokens            pgc99
*        aghi  r15,1              allow for token count           pgc99
*        sllg  r15,r15,2           times by 4 (1 word for each)   pgc99
*        agr   r0,r15             Allow for the token offset areaspgc99
*
*     new calculation -- one meg per work unit
*
         lgf   R1,litpcnt         NO.  WORK UNITS
         sll   r1,20              * 1 meg
         mvc   savebase,litpools
*        agf   R0,LITPOOLS        ADD   ESTIMATED LITERAL POOL SIZE
*        MR    R0,R0
*
         ST    R1,LITPOOLS        SAVE  POOL SIZE
*
         LA    R0,l'pooleyeb(,R1)          ADD   EYEBALL LENGTH
         GETMAIN RU,LV=(0),LOC=(ANY),BNDRY=PAGE
*
         MVC   0(l'pooleyeb,R1),POOLEYEB   COPY  EYEBALL
         aghi  R1,l'pooleyeb      SKIP  OVER   EYEBALL
         ST    R1,LITPOOLB        SAVE  POOL BEGINNING ADDRESS
         ST    R1,LITPOOLC        SET  CURRENT ADDRESS
*
         agf   R1,LITPOOLS        SAVE END-OF-POOL ADDRESS
         ST    R1,LITPOOLM
*
***********************************************************************
*  ALLOCATE LITERAL POOL stats summary area                           *
*   The reason for this is because the literal pools are assembled    *
*   dynamically and reshuffled during phase-1, for tokens and while   *
*   cloning. This small structure is added so we can report at one    *
*   time during the processing.                                       *
***********************************************************************
         lgh   R0,VIEWCNT         Number of views
         lgf   r15,tokncnt        Plus number of tokens
         aghi  r15,1              + 1
         agr   r0,r15
         lgf   R1,litpcnt         TIMES NO.  ES sets
         aghi  r1,1               + 1 in cae it is zero
         MR    R0,R0
         mghi  r1,lp_stat_len     times bytes for each entry
         ST    R1,LITSTATS        SAVE  POOL SIZE
*
         LA    R0,l'litstateyeb(,R1)   ADD   EYEBALL LENGTH
         GETMAIN RU,LV=(0),LOC=(ANY),BNDRY=PAGE
*
         MVC   0(l'litstateyeb,R1),LITSTATEYEB COPY  EYEBALL
         aghi  R1,l'litstateyeb   SKIP  OVER   EYEBALL
         ST    R1,LITSTATA        SAVE  POOL BEGINNING ADDRESS
         ST    R1,LITSTATC        SAVE  POOL CURRENT   ADDRESS
         A     R1,LITSTATS        add usable size and back up to
         AGHI  R1,-lp_stat_len    the last entry
         ST    R1,LITSTATE        last  entry

         LLGT  R0,LITSTATA        ZERO AREA
         LLGF  R1,LITSTATS
         XGR   R14,R14
         XGR   R15,R15
         MVCL  R0,R14
***********************************************************************
*  RETURN                                                             *
***********************************************************************
         lmg   R14,R12,SAVF4SAG64RS14    RESTORE   REGISTERS
         BR    R14                       RETURN
*
*
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        O P E N   T H E   E X T R A C T   F I L E S                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,savesubr
*
OPENEXTF stmg  R14,R12,SAVF4SAG64RS14  SAVE   REGISTERS
*
   llgt        R7,LTBEGIN         LOAD LOGIC TABLE ADDRESS
   USING LOGICTBL,R7
*
   lgf         R6,LTCOUNT         LOAD LOGIC TABLE  LOOP COUNTER

   do   from=(r6)
     do ,
       if CLC,LTMAJFUN,eq,WR_XT  WRITE EXTRACT FUNCTION CODE ???
*
         if ltgf,R14,LTWR200A,p  VERSION  4 VDP RECORD AVAILABLE  ???
           USING VDP0200b_FILE_RECORD,R14
*
           L   R0,VDP0200b_ALLOC_FILE_TYPE
           doexit (chi,R0,eq,TOKENDEV)  Tokens exit now
         endif
*
         llgt r3,ltwrexta
         USING EXTFILE,R3
*
***********************************************************************
*  GET EXTRACT FILE ATTRIBUTES FROM JCL IF DDNAME PRESENT             *
***********************************************************************
         BRAS  R9,EXTFJCL         SCAN JCL("TIOT")  FOR DDNAME
*
         Llgt  R2,EXTDCBA         LOAD DCB ADDRESS (IF  ANY)
         USING IHADCB,R2
*
         ltgf  R14,LTWR200A       200 record available?
         brnp  opendecb           no, must be an EXTR
         USING VDP0200b_FILE_RECORD,R14
*
         L     R0,VDP0200b_ALLOC_FILE_TYPE
         if (CHI,R0,eq,PIPEDEV)
*          set up IO for PIPES
           if ltgf,R0,VDP0200b_FILE_READER,np
*           set up for null pipe
            push  using
             llgt  R1,EXECDADR    LOAD PARAMETER AREA  ADDRESS
             USING EXECDATA,R1
             CLI   EXECDUMY,C'Y'
             pop   using
             BRE   OPENNULL
*
           MVC ERRDATA(L'VDP0200b_DDNAME_OUTPUT),vdp0200b_DDNAME_OUTPUT
             lghi  R14,NO_READER_PIPE
             BRU   RTNERROR
*
***********************************************************************
*  CHANGE I/O SUBROUTINE ADDRESSES FOR NULL PIPES                     *
***********************************************************************
OPENNULL     LARL  R0,PIPEPUT
             ST    R0,EXTPUTA
             OC    EXTPUTA,MODE31
             larl r15,put6431
             oilh r15,x'8000'
             st r15,extput_6431
             LARL  R0,PIPENULL
             ST    R0,EXTCHKA
             OC    EXTCHKA,MODE31
             larl r15,chk6431
             oilh r15,x'8000'
             st r15,extchk_6431
*
             DROP  R14
           else
***********************************************************************
*  CHANGE I/O SUBROUTINE ADDRESSES FOR WRITING TO PIPES               *
***********************************************************************
OPENPIPE     LARL  R0,PIPEPUT
             ST    R0,EXTPUTA
             OC    EXTPUTA,MODE31
             larl r15,put6431
             oilh r15,x'8000'
             st r15,extput_6431
             LARL  R0,PIPECHK
             ST    R0,EXTCHKA
             OC    EXTCHKA,MODE31
             larl r15,chk6431
             oilh r15,x'8000'
             st r15,extchk_6431
           endif
*
         else
***********************************************************************
*  BYPASS "DECB" INITIALIZATION IF ALREADY DONE                       *
***********************************************************************
OPENDECB   ds    0h
           doexit oc,extdecbf,extdecbf,nz,or,   DECB's already done?   +
               ltgr,r2,r2,np
*
           CLC   DCBDDNAM(L'DUMY),DUMY   DUMMY  FILE ???
           BRE   OPENBUFR
*
           llgt r14,execdadr
           using execdata,r14
           if CLI,execpagf,eq,c'Y'
             la r15,extrdcbe-extrfile(,r2)  point to DCB's DCBE
             using DCBE,r15
             oi dcbeflg3,dcbebfxu      signal we are fixing buffers
             drop r15
           endif
           drop r14
           mvi   open_errid,c' '       Clear openerror indicator
           MVC   WKREENT(8),OPENPARM
           sam31
           sysstate amode64=NO
           OPEN  ((R2),(OUTPUT)),MODE=31,MF=(E,WKREENT)  EXTRACT FILE
           sysstate amode64=YES
           sam64
           TM    48(R2),X'10'       TEST FOR  SUCCESSFUL OPEN ???
           BRO   OPENOKAY           WRITE SORT CONTROL RECORD IF OK
*
           lghi  R14,OPEN_EXTR_FAIL INDICATE OPEN FAILED OTHERWISE
           MVC   ERRDATA(L'LTFUNC),LTFUNC
           MVI   ERRDATA+L'LTFUNC,C':'
           MVC   ERRDATA+L'LTFUNC+1(8),EXTDDNAM
           BRU   RTNERROR
*
openokay   ds    0h
           if    cli,open_errid,ne,c' ' Did open exit find a problem?
             xgr   r0,r0
             ic    r0,open_errid       Load the error indicator to r0
             j     openerr             and split
           endif
* Save this until version 5: see RTC23117
*  If the extract file if going to a format phase job then
*  it must be RECFM=VB
*          IF    TM,EXTFLAG,extfmtph,o Is this going to a format phase?
*            IF TM,dcbRECFM,dcbrecv+dcbrecbr,no rec format must be VB
*              LGHI  R14,EXTFILE_RECFM_ERR
*              MVC   ERRDATA(8),EXTDDNAM
*              J  RTNERROR
*            endif
*          endif
*
           MVC   EXTRECFM,DCBRECFM
           MVC   EXTLRECL,DCBLRECL
           llgc  R15,dcbncp         Get number of buffer set by OPEN
           sth   r15,EXTBUFNO
*
           MVC   EXTBLKSI,DCBBLKSI
*
           MVC   EXTPUTA+1(3),DCBPUTA
           OC    EXTPUTA,MODE31
           larl r15,put6431
           oilh r15,x'8000'
           st  r15,extput_6431
           MVC   EXTCHKA+1(3),DCBCHCKA
           OC    EXTCHKA,MODE31
           larl r15,chk6431
           oilh r15,x'8000'
           st  r15,extchk_6431

         endif
*
OPENBUFR ds    0h
         doexit oc,extdecbf,extdecbf,nz       DECB's already done?
*
         lgh   R15,EXTBUFNO       LOAD  NO. OF BUFFERS
         if    ltr,r15,r15,z      If exit did not set NCP (DUMMY?)
           lhi   r15,2              then set default of 2
           sth   r15,extbufno
         endif
*
         LA    R14,MDLWRTL+4      GET  DECB POOL
         MR    R14,R14            COMPUTE DECB    POOL SIZE
         ly    r0,write_buffer_tot
         ar    r0,r15
         sty   r0,write_buffer_tot add in DECB pool size
         lgr   R0,R15
         GETMAIN R,LV=(0)         OBTAIN  DECB'S
*        r1 returned with 64 bit pointer
         ST    R1,EXTDECBF        POINT   TO FIRST DECB
         ST    R1,EXTDECBC
         lgr   R4,R1              SAVE    DECB  ADDRESS
*
         LH    R14,EXTBLKSI       LOAD PHYSICAL BLOCKSIZE
         AHI   R14,7              ROUND TO NEXT DOUBLEWORD
         SRL   R14,3
         SLL   R14,3
         STH   R14,SAVEBUFL
*
         lgh   R15,EXTBUFNO       COMPUTE  BUFFER POOL SIZE
         MR    R14,R14
         ly    r0,write_buffer_tot
         ar    r0,r15
         sty   r0,write_buffer_tot add in buffer pool size
         lgr   R0,R15
*
         llgt  R14,EXECDADR       LOAD PARAMETER AREA  ADDRESS
         USING EXECDATA,R14
*        if  CLI,EXECTRAC,EQ,C'Y'      TRACE OPTION SPECIFIED ??
         if  CLC,EXEC_LOGLVL,EQ,LOG_DEBUG DEBUG level logging?
           drop r14
trace_sa   using savf4sa,savesrb
           stmg R14,R12,trace_sa.SAVF4SAG64RS14
           mvhhi prntrdwh,l'tracext
           Larl R1,TRACEXT
           MVC PRNTLINE+00(L'TRACEXT),0(R1)
           MVC PRNTLINE+13(8),EXTDDNAM
           CVD R0,DBLWORK
           OI DBLWORK+L'DBLWORK-1,X'0F'
           UNPK PRNTLINE+27(8),DBLWORK
           LH R0,EXTBUFNO
           CVD R0,DBLWORK
           OI DBLWORK+L'DBLWORK-1,X'0F'
           UNPK PRNTLINE+44(4),DBLWORK
           logit
           lmg R14,R12,trace_sa.SAVF4SAG64RS14
           drop trace_sa
         endif
*
         GETMAIN RU,LV=(0),LOC=(ANY),BNDRY=PAGE GET  BUFFER POOL
         lgr R5,R1              LOAD  BUFFER ADDRESS
         llgt r14,execdadr
         using execdata,r14
         if CLI,execpagf,eq,c'Y'
           testauth ,
           if ltr,r15,r15,z  If r15 is zero then authorized
             modeset mode=SUP
             lgr  r1,r5           Get address of area
             stg  r2,dblwork2     save contents (DCB)
             lgh  r2,EXTBUFNO     get number of buffers
             mh   r2,savebufl     work out total length
             aghi r2,-1           so we will be at the last byte
             agr  r2,r1           end byte
             PGSER R,FIX,A=(1),EA=(2),ECB=0
             modeset mode=PROB
*            WTO 'Page fix extract file'
             lg   r2,dblwork2      restore contents
           endif
         endif
         drop r14
         llgt  r2,extdcba           point to DCB again
*
         LA    R0,4(,R5)          SAVE FIRST RECORD ADDRESS (SKIP BDW)
         TM    EXTRECFM,X'40'     VARIABLE/UNDEFINED    ???
         BRO   *+8                YES - LEAVE  ROOM FOR BDW
         lgr   R0,R5              NO  - DATA IN 1ST BYTE (FIXED)
         ST    R0,EXTRECAD
         lgr   R14,R5
         lgh   R1,EXTBLKSI
         agr   R14,r1
         ST    R14,EXTEOBAD
*
         LH    R0,EXTBUFNO        INITIALIZE LOOP COUNTER
OPENRING LA    R14,MDLWRTL+4(,R4) LOAD ADDRESS OF NEXT DECB
         CHI   R0,1               LAST  DECB   ???
         BRP   *+10               NO  - USE IT (BYPASS NEXT INSTR)
         llgt  R14,EXTDECBF       YES - POINT  TO FIRST
*
         ST    R14,0(,R4)         CHAIN DECB'S TOGETHER
         lay   R14,MDLWRT         COPY  MODEL  DECB
         MVC   4(MDLWRTL,R4),0(R14)
         MVC   4+6(2,R4),EXTBLKSI SAVE  BLOCK  SIZE    IN DECB
         ST    R2,4+8(,R4)        SAVE  DCB    ADDRESS IN DECB
         ST    R5,4+12(,R4)       SAVE  BUFFER ADDRESS IN DECB
*
         MVI   4(R4),X'7F'        INDICATE I/O COMPLETE (BUFFER FREE)
*
         llgt  R4,0(,R4)          ADVANCE  TO NEXT DECB
         lgh   R1,SAVEBUFL        ADVANCE  TO NEXT BUFFER
         agr   R5,r1              ADVANCE  TO NEXT BUFFER
         BRCT  R0,OPENRING        LOOP THROUGH ALL DECB'S
*
*        LA    R9,EXTFILEL(,R3)
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=(REGS),ID=120,STORAGE=((R3),(R9))
*        L     R9,EXTDECBF
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=(REGS),ID=121,STORAGE=((R9),(R5))
*
*
       endif ,               not a write
     enddo ,
     lgh  R1,ltrowlen        ADVANCE TO NEXT LOGIC TABLE  ROW
     agr  R7,r1              ADVANCE TO NEXT LOGIC TABLE  ROW
   enddo
   lmg  R14,R12,SAVF4SAG64RS14  RESTORE  REGISTERS
   BR   R14                     RETURN
                        SPACE 3
OPENERR1 lghi  R0,C'1'
         BRU   OPENERR
OPENERR2 lghi  R0,C'2'
         BRU   OPENERR
OPENERR3 lghi  R0,C'3'
         BRU   OPENERR
OPENERR4 lghi  R0,C'4'
         BRU   OPENERR
OPENERR5 lghi  R0,C'5'
         BRU   OPENERR
OPENERR6 lghi  R0,C'6'
         BRU   OPENERR
OPENERR7 lghi  R0,C'7'
         BRU   OPENERR
OPENERR8 lghi  R0,C'8'
*
OPENERR  lghi  R14,EXTR_DCB_ERR   INDICATE MISSING DCB INFORMATION
         MVC   ERRDATA(L'EXTDDNAM),EXTDDNAM
         MVI   ERRDATA+L'EXTDDNAM+0,C'-'
         STC   R0,ERRDATA+L'EXTDDNAM+1
         BRU   RTNERROR
*
         DROP  R2
         DROP  R3
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "EXTFJCL" - OBTAINS MEMORY FOR "DCB" AND SETS FILE ATTRIBUTES       *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R9  - RETURN   ADDRESS                                       *
*        R7  - "WR"     ROW    (OPTIONAL)                             *
*        R3  - "EXTFILE"   AREA ADDRESS                               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING EXTFILE,R3
*
EXTFJCL  llgt  R14,TIOTADDR       LOAD TIOT  ADDRESS
         LA    R14,24(,R14)       LOAD ADDRESS OF FIRST DDNAME
         sgr   R15,R15            CLEAR LENGTH REGISTER
*
EXTFTIOT CLC   4(L'EXTDDNAM,R14),EXTDDNAM  MATCHING   DDNAME ???
         BRE   EXTFDCB                     YES - BRANCH
*
EXTFNEXT IC    R15,0(R14)         LOAD  ENTRY LENGTH
         agr   R14,R15            ADVANCE  TO NEXT    DDNAME
         ltgf  R1,0(R14)          IS  LENGTH  OF NEXT ENTRY ZERO ???
         BRNZ  EXTFTIOT           NO  - LOOP  THROUGH TIOT
*
***********************************************************************
*  DDNAME NOT IN JCL                                                  *
***********************************************************************
         ltgf  R14,LTWR200A       VERSION  4 VDP RECORD AVAILABLE  ???
         BRNP  EXTFSTD            NO  - CHECK IF STANDARD EXTRACT
*
         L     R0,VDP0200b_ALLOC_FILE_TYPE-vdp0200b_FILE_RECORD(,R14)
         CHI   R0,PIPEDEV         PIPE  ???
         BRNE  EXTFSUBR           NO  - CHECK FOR WRITE EXIT
*
EXTFPIPE LHI   R15,PIPEBUFR       YES -  ONLY ALLOCATE  3
         STH   R15,EXTBUFNO       SAVE NO. OF BUFFERS
*
         BR    r9
*
EXTFSTD  LH    R0,LTWREXT#        STANDARD EXTRACT FILE NUMBER ???
         CH    R0,MAXSTDF#
         BRNH  OPENERR2           YES - DDNAME  MUST BE IN JCL
*
EXTFSUBR CLC   LTWRNAME,SPACES    EXIT  SPECIFIED  ???
         Bner  r9                 YES - DDNAME NOT REQUIRED IN JCL
*
EXTFDUMY llgt  R14,EXECDADR
         USING EXECDATA,R14
         CLI   EXECDUMY,C'Y'      DUMMY FILES MISSING FROM JCL ???
         BRNE  OPENERR3           NO  - ERROR
         DROP  R14
*
         OC    EXTDCBA,EXTDCBA    DCB  ALREADY  ALLOCATED ???
         Bnzr  r9                 YES -   SKIP  REDUNDANT ALLOCATION
*
         lghi  R0,targetbs_l+8    LOAD area length - dcb, etc
         GETMAIN R,LV=(0),LOC=BELOW GET MEMORY below the line
         lgr   R2,R1
         using targetbs,r2
target   USING IHADCB,dcbbs

         larl r14,extrfile
source         USING IHADCB,r14
         MVC  target.ihadcb(evntdcbl),source.ihadcb   copy dcb/dcbe

         LA   R0,dcbbs+evntdcbe-evntfile
         ST   R0,target.DCBDCBE
*
         la   r0,exlbs          get address of target exl
         mvi  exlbs,exllaste+exldcbex    set flag
         stcm r0,b'0111',target.dcbexlsa and update the dcblist ptr
         drop target,source

         la   r0,openbs         get address of code area
         mvc  openbs,modopen24  copy in model code
         stcm r0,b'0111',exlbsopen   and set address in list

         larl r0,extr_dcb_open_exit  real exit address
         oilh r0,x'8000'        turn on amode 31
         st   r0,exitbs31        and save

         la   r2,dcbbs          point r2 at dcb
         ST   R2,EXTDCBA         SAVE DCB ADDRESS IN  CONTROL ELEMENT
         drop r2
         using ihadcb,r2

         MVC   DCBDDNAM(4),DUMY   MARK   FILE NUMBER AS "DUMMY"
         LH    R0,LTWREXT#
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         CHI   R0,999                      THREE OR FOUR DIGIT ???
         BRH   *+14
         UNPK  DCBDDNAM+L'DUMY(3),DBLWORK  BUILD DDNAME SUFFIX (3)
         BRU   EXTFATTR
         UNPK  DCBDDNAM+L'DUMY(4),DBLWORK  BUILD DDNAME SUFFIX (4)
*
EXTFATTR MVI   EXTRECFM,X'41'     SET VB FORMAT
         MVI   DCBRECFM,X'41'
*
         LHI   R0,EXTRECL         SET DEFAULT RECORD LENGTH
         STH   R0,EXTLRECL
         STH   R0,DCBLRECL
*
         LHI   R0,maxblksi        SET DEFAULT BLOCK SIZE
         STH   R0,EXTBLKSI
         STH   R0,DCBBLKSI
*
         LHI   R0,3
         STH   R0,EXTBUFNO
*
         larl  r0,pipeput
         stcm  r0,b'0111',DCBputa
         larl  r0,pipenull
         stcm  r0,b'0111',DCBCHCKA
         MVC     EXTPUTA+1(3),DCBPUTA
         OC      EXTPUTA,MODE31
         larl r15,put6431
         oilh r15,x'8000'
         st    r15,extput_6431
         MVC     EXTCHKA+1(3),DCBCHCKA
         OC      EXTCHKA,MODE31
         larl r15,chk6431
         oilh r15,x'8000'
         st    r15,extchk_6431
*
         Br    r9                 return to caller
*
EXTFDCB  OC    EXTDCBA,EXTDCBA    DCB  ALREADY  ALLOCATED ???
         Bnzr  r9                 YES -   SKIP  REDUNDANT ALLOCATION
*
         lghi  R0,targetbs_l+8    LOAD area length - dcb, etc
         GETMAIN R,LV=(0),LOC=BELOW GET MEMORY below the line
         lgr   R2,R1
         using targetbs,r2
target   USING IHADCB,dcbbs

         larl r14,extrfile
source         USING IHADCB,r14
         MVC  target.ihadcb(evntdcbl),source.ihadcb   copy dcb/dcbe

         LA   R0,dcbbs+evntdcbe-evntfile
         ST   R0,target.DCBDCBE
*
         la   r0,exlbs          get address of target exl
         mvi  exlbs,exllaste+exldcbex    set flag
         stcm r0,b'0111',target.dcbexlsa and update the dcblist ptr
         drop target,source

         la   r0,openbs         get address of code area
         mvc  openbs,modopen24  copy in model code
         stcm r0,b'0111',exlbsopen   and set address in list

         larl r0,extr_dcb_open_exit  real exit address
         oilh r0,x'8000'        turn on amode 31
         st   r0,exitbs31        and save

         la   r2,dcbbs          point r2 at dcb
         ST   R2,EXTDCBA         SAVE DCB ADDRESS IN  CONTROL ELEMENT
         drop r2
         using ihadcb,r2

         MVC   DCBDDNAM,EXTDDNAM
         lhi   r15,pipebufr       get default number of buffers
         sth   r15,extbufno       and save in extract file are
*
         Br    r9                 return to caller

         push  using
         drop  ,
         using extfile,r9

put6431  lhlr  r14,r14            save low r14 in high r14
         l     r15,extputa        get the address
         basr  r14,r15            and go there
         srlg  r14,r14,32         restore low r14, clearing high part
         bsm   0,r14              return to caller in correct mode

chk6431  lhlr  r14,r14            save low r14 in high r14
         l     r15,extchka        get the address
         basr  r14,r15            and go there
         srlg  r14,r14,32         restore low r14, clearing high part
         bsm   0,r14              return to caller in correct mode

         pop   using

**********************************************************************
*                                                                    *
*        This is the DCB OPEN exit - given control during OPEN       *
*        processing                                                  *
*        See DFSMS Using Data Sets - Chapter 31 for details          *
*                                                                    *
*        Registers at entry                                          *
*           0                                                        *
*           1      - low order 3 bytes --> DCB                       *
*           2-13   - Contents unchanged from execution of OPEN       *
*          14      - Return address                                  *
*          15      - Exit entry address                              *
*                                                                    *
*        Notes - No need to preserve 2-13                            *
*              - Must NOT use the savearea addressed by R13          *
*              - always entered in 24 bit amode                      *
*        No return codes required                                    *
*                                                                    *
**********************************************************************
*
* In this exit DCB and DCBE values are checked and set
*
extr_dcb_open_exit ds 0h
         push  using
         drop  r2
         la    r1,0(,r1)          clean r1 address
         using ihadcb,r1            and address the DCB

* now entered in amode 31 so no need for samxx instructions

*
* check number of write buffers and set MULTACC if appropriate
*
         do ,
* If BUFNO not set in the JCL use the MULTSDN option to calculate
* the DCBNCP value.
* If BUFNO is set in the JCL, use this value in DCBNCP and set DCBBUFNO
* to 0.
* The buffers will be allocated later according to the value in
* DCBNCP, and because we allocate them later and not at OPEN time
* the buffers can be allocated above the line.
*
           llc R15,DCBBUFNO       load bufno from DCB (in JCL)
           if ltr,R15,R15,np      "BUFNO" not in JCL ???
             L     R10,EXECDADR
             USING EXECDATA,R10
             PACK  DBLWORK,EXECmsdn get the value for MULTSDN
             CVB   R9,DBLWORK
             DROP  R10
* Set MULTACC to half MULTSDN (use half of the buffers in each CP)
             L R2,DCBDCBE     LOAD  DCBE   ADDRESS
             USING DCBE,R2
             AHI R9,1             ROUND MULTSDN to MULTIPLE OF 2
             SRL R9,1
             STC R9,DCBEMACC      MULTACC =  1/2 multsdn
             SLL R9,1
             stc r9,dcbemsdn       set the sdn value
             DROP R2
             mvi   dcbncp,x'00'   set NCP to zero and on return
*                                 from the OPEN this will be set
           else
             if CHI,r15,lt,2      check for minimum ???
               LHI R15,2          NO  - USE    MINIMUM
             else
               if  CHI,R15,gt,5   CHECK THRESHOLD  FOR "MULTACC"
                 L R2,DCBDCBE     LOAD  DCBE   ADDRESS
                 USING DCBE,R2
                 AHI R15,1        ROUND TO MULTIPLE OF  2
                 SRL R15,1
                 STC R15,DCBEMACC MULTACC  =  1/2 BUFNO
                 SLL R15,1
                 if CHI,r15,gt,255      NCP can not be greater than 255
                   lhi r15,254
                   mvi dcbemacc,x'7F'
                 endif
                 DROP R2
               endif
             endif
*
             MVI DCBbufno,x'00'     let us allocate buffers later
             STC R15,DCBNCP         SET  "DCBNCP" OPTION
           endif
*
           if TM,dcbrECFM,dcbrecl,z   "RECFM" not specified
             OI DCBRECFM,dcbrecv+dcbrecbr   then set VB
           endif
           MVC EXTRECFM,dcbRECFM

           lh  r5,dcblrecl            get LOGICAL  RECORD LENGTH

           lh  r8,dcbblksi            get blocksize

           l    r9,ltviewnv           get address of prolog code
           using nvprolog,r9           and map
           l    r9,nvlogtbl             and get pointer to the NV
           drop r9
nvlogic    using logictbl,r9          R9 should point at the last NV
           if ltr,r9,r9,p             safety check
             lh r10,nvlogic.ltdataln add the lengths
             ah r10,nvlogic.ltsortln
             ah r10,nvlogic.lttitlln

*            keep minimum in r10 for later test
           else
             lhi r10,extrecl          Make r10 the max if r9 is zero
           endif

           if ltr,r9,r9,p             safety check
             if cli,nvlogic.ltviewtp,eq,copyview,or,   copy view       +
               cli,nvlogic.ltviewtp,eq,extronly        extract only
               if chi,r5,le,1
                 lr r5,r10
               endif
             else                    not one of the special types

               if cr,r5,lt,r10       is jcl lrecl too short
                 lr r5,r10
               endif
             endif
           endif
           drop nvlogic

           if ltgfr,R9,R8,np,or,chi,r8,le,1  LTGFR and
             LgHI R9,maxblksi        LGHI used as DSGFR used later
           endif
*
           if CR,R9,lt,R5            blksize set is less than lrecl
             mvi open_errid,c'8'      set bad return
             leave ,
           endif

           if TM,dcbRECFM,dcbrecf,o   Fixed LENGTH RECORDS ???

             Dsgfr R8,R5             COMPUTE MAXIMUM RECORDS
             MR R8,R5                per block

           endif

           sth r5,dcblrecl            get LOGICAL  RECORD LENGTH
           STH R5,EXTLRECL             save it
           sth r9,dcbblksi            get blocksize
           STH R9,EXTBLKSI save it
*
         enddo
         bsm   0,r14
         pop   using
*
         DROP  R2
         DROP  R3
         DROP  R7
**********************************************************************
*                                                                    *
*        This is the DCB OPEN exit - given control during OPEN       *
*        processing                                                  *
*        See DFSMS Using Data Sets - Chapter 31 for details          *
*                                                                    *
*        Registers at entry                                          *
*           0                                                        *
*           1      - low order 3 bytes --> DCB                       *
*           2-13   - Contents unchanged from execution of OPEN       *
*          14      - Return address                                  *
*          15      - Exit entry address                              *
*                                                                    *
*        Notes - No need to preserve 2-13                            *
*              - Must NOT use the savearea addressed by R13          *
*              - always entered in 24 bit amode                      *
*        No return codes required                                    *
*                                                                    *
**********************************************************************
*
* In this exit DCB and DCBE values are checked and set
*
event_open_exit ds 0h
         push  using
*        drop  r2
         la    r1,0(,r1)          clean r1 address
         using ihadcb,r1            and address the DCB

* now entered in amode 31 so no need for samxx instructions

*
* check number of read buffers and set MULTACC if appropriate
*
         do ,
* If BUFNO not set in the JCL use the MULTSDN option to calculate
* the DCBNCP value.
* If BUFNO is set in the JCL, use this value in DCBNCP and set DCBBUFNO
* to 0.
* The buffers will be allocated later according to the value in
* DCBNCP, and because we allocate them later and not at OPEN time
* the buffers can be allocated above the line.
*
           llc R15,DCBBUFNO       load bufno from DCB (in JCL)
           if ltr,R15,R15,np      "BUFNO" not in JCL ???
             L     R10,EXECDADR
             USING EXECDATA,R10
             PACK  DBLWORK,EXECmsdn get the value for MULTSDN
             CVB   R9,DBLWORK
             DROP  R10
* Set MULTACC to half MULTSDN (use half of the buffers in each CP)
             L R2,DCBDCBE     LOAD  DCBE   ADDRESS
             USING DCBE,R2
             AHI R9,1             ROUND MULTSDN to MULTIPLE OF 2
             SRL R9,1
             STC R9,DCBEMACC      MULTACC =  1/2 multsdn
             SLL R9,1
             stc r9,dcbemsdn       set the sdn value
             DROP R2
             mvi   dcbncp,x'00'   set NCP to zero and on return
*                                 from the OPEN this will be set
           else
             if CHI,r15,lt,2      check for minimum ???
               LHI R15,2          NO  - USE    MINIMUM
             else
               if  CHI,R15,gt,5   CHECK THRESHOLD  FOR "MULTACC"
                 L R2,DCBDCBE     LOAD  DCBE   ADDRESS
                 USING DCBE,R2
                 AHI R15,1        ROUND TO MULTIPLE OF  2
                 SRL R15,1
                 STC R15,DCBEMACC MULTACC  =  1/2 BUFNO
                 SLL R15,1
                 if CHI,r15,gt,255      NCP can not be greater than 255
                   lhi r15,254
                   mvi dcbemacc,x'7F'
                 endif
                 DROP R2
               endif
             endif
             MVI DCBbufno,x'00'     let us allocate buffers later
             STC R15,DCBNCP         SET  "DCBNCP" OPTION
           endif
*
         enddo
         drop  r1
*
         bsm   0,r14
         pop   using
         DROP  R13
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        F I L L   M E M O R Y   R E S I D E N T   T A B L E S        *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R7:   REFERENCE DATA RECORD COUNT                            *
*        R6:   REFERENCE DATA HEADER RECORD ADDRESS ("REH")           *
*        R5:   LOOK-UP   BUFFER AREA DSECT  BASE                      *
*        R4:   DATASPACE ALLOCATION  PARAMETER AREA ADDRESS           *
*        R3:   LOOK-UP   KEY  LENGTH                                  *
*              LOOK-UP   DATA LENGTH                                  *
*        R2:   LOOK-UP   DATA TARGET ROW    ADDRESS                   *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using genparm,parm_area
         using savf4sa,savesubr
         using MSGLIST,MSG_AREA
*
LOADLKUP stmg  R14,R12,SAVF4SAG64RS14  SAVE   REGISTERS
*
***********************************************************************
*  OPEN REFERENCE DATA HEADER FILE ("MR95REH")                        *
***********************************************************************
         LA   R0,hdrDCBL         LOAD DCB  LENGTH
         GETMAIN R,LV=(0),LOC=BELOW       GET  MEMORY  FOR DCB
         LR   R2,R1
         sty  r1,hdrdcba         save the dcb address
         USING IHADCB,R2

         larl r15,hdrdcb
         MVC  ihadcb(hdrDCBL),0(r15)    copy dcb

         LA   R1,hdrDCBE-hdrdcb(,R2)
         ST   R1,DCBDCBE

         if clc,namepgm,eq,=cl8'GVBMR95R'    R for reference
           mvc  dcbddnam,=cl8'REFRREH'     default is EXTRREH
         endif

         MVC   WKREENT(8),OPENPARM      OPEN  PARAMETER LIST
         sam31
         sysstate amode64=NO
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,WKREENT) TABLE HEADER FILE
         sysstate amode64=YES
         sam64
         lghi  R14,OPEN_REH_FAIL  ASSUME OPEN FAILED
         TM    48(R2),X'10'       SUCCESSFUL  OPEN   ???
         JNO   RTNERROR           NO - INDICATE ERROR
         sgr   r0,r0
         lgr   r7,r0              Zero header record counter
         sty   r0,rehcnt          zero header file record count
         sty   r0,grefcnt         Initialise to zero

         ltgf  r15,vdp0650a       Get VDP reference files rec address
         jnp   filldone           If none then here
         using vdp0650b_join_record,r15
         lgf   r1,vdp0650b_grefcnt Get number of GREFxxx files
         la    r15,vdp0650b_gref_entry
         using vdp0650b_gref_entry,r15
         do from=(r1)
           if tm,vdp0650b_gref_flag,vdp0650b_gref_ephase,o
             aghi r0,1             extract phase entry so add to count
           endif
           l   r14,vdp0650b_gref_entlen Get length of PF name
           la  r15,vdp0650b_gref_pf(r14) get address of next
         enddo
         drop  r15
         sty   r0,grefcnt         Save for later
         aghi  r0,1               Allow an extra 1
         mghi  r0,tbhdrlen        Times llength of each entry
         aghi  r0,l'reheyeb       Plus the eyecatcher
         GETMAIN RU,LV=(0),LOC=(ANY)   OBTAIN  MEMORY
         mvc   0(l'reheyeb,r1),reheyeb  Move in eyecatcher
         la    r6,l'reheyeb(,r1)          Start of table
         sty   r6,rehtbla         Save it
         Using Tblheadr,r6
*
***********************************************************************
*  Read all the reference data header file records ("MR95REH") into
*  our internal table before processing the reference files
***********************************************************************
FILLHDR  ds     0h
         sam31
         sysstate amode64=NO
         get   (2)
         sysstate amode64=YES
         sam64
         mvc   tblheadr(TBHRECL),0(r1) Save record in table
         xc    TBREFMEM,TBREFMEM       clear other TBLheadr work fields
         xc    TBHPRIME,TBHPRIME
*
         aghi  r7,1               increment count
         aghi  r6,tbhdrlen        next free entry in hdr rec table
         cgf   r7,grefcnt         More records than expected
         jnh   Fillhdr            N:
*
Fillhdr_err ds 0h                 Y:
* ERROR reading the reference header files - count error
* rtc21222 R7 has the number of GREF files
*          GREFCNT has the count of GREF files in REH
         lay   r14,workarea1
         using workarea1,r14
         cvd   r7,workarea
         mvc   workarea1(l'nummask3),nummask3
         ed    workarea1(l'nummask3),workarea+6
         drop  r14
         LA    R1,l'nummask3
         ST    R1,MSGS2LEN             set length
         St    R14,MSGS2PTR            save the address of the data
*
         lay r14,workarea2
         using workarea2,r14
         lgf   r7,grefcnt
         cvd   r7,workarea
         mvc   workarea2(l'nummask3),nummask3
         ed    workarea2(l'nummask3),workarea+6
         drop r14
         LA    R1,l'nummask3
         ST    R1,MSGS3LEN             set length
         ST    r14,MSGS3PTR            save the address of the data
         XC    MSGS4PTR,MSGS4PTR
*
         lghi  r14,REH_COUNT_ERR  no  - indicate  error
         j     rtnerror
*
* Finished reading the reference header files into memory
*
Filleof  ds    0h
         sam64
         cgf   r7,grefcnt         More records than expected
         jne   Fillhdr_err        N:
         ltgr  r7,r7              any reference files
         jz    filldone           no
*
***********************************************************************
*  Have now read all the header records so we can work out how much
*  memory we will need to contain all the records from all the
*  reference files.
***********************************************************************
         sgr   r4,r4              zero total memory counter
         llgt  r6,rehtbla         Get table of "REH" records
* loop through all the ref header records adding up memory needed.
*
* NOTE:
* If we are going to use a HASH table we will need memory for the
* hash table index too.
* For now we will assume we are using a hash table unless effective
* dates are used.
*
         do from=(r7)

           if ltgf,r1,tbreccnt,p  load record count, test for +ve
             lgh  r3,tbreclen     load record length
             aghi r3,lkprefln     add  prefix length
             mlgr r2,r1           compute  table size
             stg  r3,tbrefmem     save here for later
             agr r4,r3            add to total

             if cli,TBEFFDAT,eq,x'00' Effective dates used?
*
* Calculate the number of hash table slots that will be used
*  (Calculate the nearest prime greater than number of records)
*
*              Pass R1 with reference record count to FindPrime
               JAS R9,FINDPRIME
               stg R1,TBHPRIME    PRIME for hash table returned
*
               sllg R1,R1,3       space for 1 doubleword  per rec
               agr  r4,r1         add to total
             endif

           endif

           aghi r6,tbhdrlen       point at next entry
         enddo

*        If we need any memory we will allocate all of it now

         ltgr   r4,r4             any memory needed?
         jnp   Fillreh_pre

*20254   stg   r4,Refpools        Save size we need
         lgr   R15,R4
         AG    R15,MEGROUND
         SRLG  R15,R15,20
*
         STG   R15,DBLWORK
         sty   r15,Refpools_real  Save size we really got (MB boundary)
*
         if TM,WORKFLAG1,MSGLVL_DEBUG,o debug messages
           mvhhi prntrdwh,l'trac64b
           larl  R1,TRAC64B
           MVC   PRNTLINE+00(L'TRAC64B),0(R1)
*
           CVD   R15,DBLWORK2
           OI    DBLWORK2+L'DBLWORK2-1,X'0F'
           UNPK  PRNTLINE+39(9),DBLWORK2
*
           logit
         endif
*
         IARV64 REQUEST=GETSTOR,SEGMENTS=DBLWORK,ORIGIN=dblwork2,      X
               FPROT=NO,SVCDUMPRGN=YES,USERTKN=NO_USERTKN,             X
               MF=(E,GET64MEM,COMPLETE)
*
         lg    r1,dblwork2        Get beginning address
         stg   r1,Refpoolb        Save it  as beginning address
         stg   r1,Refpoolc        Save it  as current as well
*                                 available to use to the beginning
Fillreh_pre  ds 0h
         lgf   r10,grefcnt        Get number of REH/GREFxxx recs/files
         llgt  r6,rehtbla         Get start of REH records
Fillreh_loop ds 0h

         XC    WORKAREA,WORKAREA  CLEAR WORK  AREA (INCL PATH ID)
*
         CLI   TBFILEID+0,C'$'    V3 CUSTOM REH ???
         JNE   FILLHDR4
         CLI   TBFILEID+1,C'$'
         JNE   FILLHDR4
*
***********************************************************************
*  OPEN REFERENCE DATA FILE ("MR95RED") USING "GVBUR20" (IF VERSION 3)*
***********************************************************************
FILLRED  DS    0H
*
* Looks like V3 lookup data. Return Error message
*
         LA    R14,REH_FILE_DATA_ERR
         J     RTNERROR
*
***********************************************************************
*  VERSION 4.0 HEADER FILE                                            *
***********************************************************************
FILLHDR4 MVI   TBREDVER,C'4'      INDICATE   "V4" RED
*
         if TM,WORKFLAG1,MSGLVL_DEBUG,o debug messages
*          C'Reference file xxxxxxxx LR xxxxxxxx Records xxxxxxxxx
           LAY R1,tracref
           MVC prntrdw(l'tracref),0(r1)
           L   R0,TBFILEID
           CVD R0,DBLWORK
           OI  DBLWORK+L'DBLWORK-1,X'0F'
           UNPK prntline+15(8),DBLWORK
*
           L   R0,TBRECID
           CVD R0,DBLWORK
           OI  DBLWORK+L'DBLWORK-1,X'0F'
           UNPK PRNTLINE+27(8),DBLWORK
           L   R0,TBRECCNT
           CVD R0,DBLWORK
           OI  DBLWORK+L'DBLWORK-1,X'0F'
           UNPK PRNTLINE+44(9),DBLWORK
           logit
         endif
*
         MVC   WORKAREA(L'LBDDNAME),HEXFF    BUILD LOOK-UP BUFFER ID
         MVC   WORKAREA(L'LBFILEID),TBFILEID
         MVC   WORKAREA+L'LBDDNAME(L'LBLRID),TBRECID
*
         ltgf  R7,TBRECCNT        LOAD  RECORD COUNT
         JNP   FILLOCLB           DATA RECORDS - ALLOCATE PLACEHOLDER
*
***********************************************************************
*  OPEN REFERENCE DATA FILE ("GREF---") USING "GVBUR20" (IF VERSION 4)*
***********************************************************************
         XR    R0,R0              INITIALIZE "GVBUR20" PARAMETERS
         STH   R0,UR20FC          OPEN
*
         MVC   UR20DDN,GREFXXX
         LH    R0,TBXFILE#                 CONVERT FILE NO. TO DEC
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'   FORCE A VALID  ZONE
         CHI   R0,999                      THREE OR FOUR DIGIT ???
         JH    *+14
         UNPK  UR20DDN+4(3),DBLWORK        BUILD DDNAME SUFFIX (3)
         J     *+10
         UNPK  UR20DDN+4(4),DBLWORK        BUILD DDNAME SUFFIX (4)
*
         MVI   UR20OPT1,C'I'
         MVI   UR20OPT2,C' '
         LHI   R0,26              Set buffers to even number
         STH   R0,UR20NBUF
*
         LA    R0,UR20PARM        CALL "GVBUR20"
         sty   R0,GENPARM1
         lay   R1,GENPARM1
         llgf  R15,GVBUR20A
         BASsm R14,R15
*
         LHI   R0,8               CHANGE FUNCTION CODE TO "READ SEQ"
         STH   R0,UR20FC
         CHI   R15,4              OPEN SUCCESSFUL ???
         JNH   FILLOCLB           YES - CONTINUE
*
         CHI   R15,8              EMPTY RED FILE  ???
         JE    FILLEPTE           YES - INDICATE ERROR, REH CNT WRONG
*
         lghi  R14,OPEN_REF_FAIL  NO  - INDICATE  ERROR
         MVC   ERRDATA(8),UR20DDN
         J     RTNERROR
                        EJECT
***********************************************************************
*  ALLOCATE LOOK-UP BUFFER CHAIN ELEMENT FOR THIS "LR ID"             *
***********************************************************************
FILLOCLB ds    0h
         LA    R1,WORKAREA        POINT  TO  FILE  ID + RECORD ID
         LA    R4,LBCHAIN         POINT  TO CHAIN  ANCHOR
         BRAS  R9,LOCLB           LOCATE MATCHING  LOOKUP BUFFER
         USING LKUPBUFR,R5
*
         ltgr  R5,R5              BUFFER FOUND (ALREADY ALLOCATED) ???
         JNP   FILLALLO           NO  -  ALLOCATE  BUFFER
*
         lghi  R14,REH_FILE_DUPLICATE  INDICATE  ERROR (DUPLICATE)
         MVC   ERRDATA(12),WORKAREA SAVE INDICATIVE DATA
         J     RTNERROR
*
FILLALLO lghi  R3,LBPREFLN+LKPREFLN+L'LBLRID  PREFIX+POINTERS+KEY LRID
         lgh   R1,TBKEYLEN        ADD  KEY    LENGTH
         ah    R1,TBRECLEN        ADD  KEY    LENGTH
         agr   R3,r1              ADD  RECORD LENGTH
*
         LA    R1,WORKAREA        POINT TO  FILE & RECORD   ID'S
         BRAS  R9,ALLOLKUP        ALLOCATE  LOOKUP BUFFER
*
         LH    R4,TBRECLEN        SAVE RECORD LENGTH
         STH   R4,LBRECLEN
*
         LH    R15,TBKEYLEN       LOAD  KEY LENGTH
         BCTR  R15,0              DECREMENT FOR "EX" INSTRUCTION
         STH   R15,LBKEYLEN
*
         MVC   LBHPRIME,TBHPRIME  Copy in case hash table used
***********************************************************************
*  ADJUST LOOK-UP KEY OFFSET DEPENDING ON "RED" VERSION               *
***********************************************************************
         LH    R15,TBKEYOFF       LOAD  KEY OFFSET
         STH   R15,LBKEYOFF
*
         CLI   TBADJPOS,C'N'      ADJUST STARTING POSITIONS   ???
         JE    Filladj1           NO  - BYPASS SETTING ADJUST FLAG
         OI    LBFLAGS+1,LBADJPOS SET ADJUST POSITIONS IND (DEFAULT)
*
Filladj1 ds    0h
         CLI   TBEFFDAT,C'Y'      VERSION  3 VALUE ???
         JNE   Filladj2
         MVI   TBEFFDAT,X'02'     YES - EQUATE  TO EFF + END DATES
*
Filladj2 ds    0h
         XR    R15,R15            EFFECTIVE DATES    PRESENT ???
         IC    R15,TBEFFDAT
         LTR   R15,R15
         JNP   FILLCNT            NO  - BYPASS  COMPUTING OFFSET
*
         LH    R0,TBKEYLEN        LOAD  KEY  LENGTH (EXCL DATE)
         STH   R0,LBEFFOFF
         AHI   R0,4-1             ADD    EFFECTIVE   DATE LENGTH
         STH   R0,LBKEYLEN        UPDATE KEY LENGTH (INCL DATE)  (-1)
*
         CHI   R15,ENDRANGE       END   DATES   PRESENT   ???
         JNE   FILLENDO
         OI    LBFLAGS,LBEFFEND
         J     FILLEFFD           YES - COMPUTE EFFECTIVE DATE OFFSET
*
FILLENDO CHI   R15,ENDONLY        END   DATES   PRESENT   ???
         JNE   FILLEFFD
         OI    LBFLAGS,LBEFFEND
         J     FILLCNT            YES - COMPUTE EFFECTIVE DATE OFFSET
*
FILLEFFD OI    LBFLAGS,LBEFFDAT   SET   EFFECTIVE DATE INDICATOR
***********************************************************************
*  BUILD PLACEHOLDER LOOK-UP RECORD IF THERE ARE NO DATA RECORDS      *
***********************************************************************
FILLCNT  DS    0h
         OI    LBFLAGS,LBMEMRES   Turn on mem resident flag
*
         ltgf  R7,TBRECCNT        LOAD  RECORD COUNT
         jp    FILLDATA           YES - ALLOCATE REFERENCE DATA AREA
*
         XC    LBDATA+0(LKPREFLN),LBDATA
         LA    R0,LBDATA+LKPREFLN
         LH    R1,TBKEYLEN
         AH    R1,TBRECLEN
         xgr   R14,R14
         xgr   R15,R15
         ICM   R15,B'1000',HEXFF
         MVCL  R0,R14
*
         SGR   R0,R0
         LA    R0,LBDATA          POINT TABLE DATA TO PLACEHOLDER REC
         STG   R0,LBTBLBEG
         STG   R0,LBMIDDLE
         STG   R0,LBTBLEND
*
         aghi  r6,tbhdrlen        Next entry in REH table
         jct   r10,Fillreh_loop   ADVANCE  TO NEXT HEADER
         j     filldone
                        EJECT
***********************************************************************
*  Allocate primary address space reference data area from our pool   *
***********************************************************************
FILLDATA DS    0H
         lg    r2,refpoolc      Get start of current free area in pool
         stg   R2,LBTBLBEG      Save it
         lgr   R3,R2            Get the end of area needed
         ag    r3,tbrefmem      from previously stored amount
         stg   R3,LBTBLEND
         stg   R3,refpoolc      Next free area
*
         USING LKUPTBL,R2
                        EJECT
FILLSCAN DS    0H
***********************************************************************
*  READ NEXT REFERENCE DATA RECORD ("MR95RED")                        *
***********************************************************************
FILLLOOP LA    R0,UR20PARM        CALL "GVBUR20"
         sty   R0,GENPARM1
         lay   R1,GENPARM1
         llgf  R15,GVBUR20A
         bassm R14,R15            GET  THE  NEXT  RECORD
*
         LTR   R15,R15            SUCCESSFUL ???
         JZ    FILL_ASC           YES - CONTINUE
*
         CHI   R15,8              END OF RED DATA ???
         JE    FILLEPTE           YES -  INDICATE ERROR, REH CNT WRONG
*
         lghi  R14,REF_READ_ERR   NO  -  INDICATE ERROR
         MVC   ERRDATA+00(8),UR20DDN
         J     RTNERROR
*
*        TBRECCNT has the REH value
*        LBRECCNT has the actual count
*
FILLEPTE DS    0H 
*
         MVC   ERRDATA+00(8),UR20DDN  Ref file DDN in error
         lgf   r7,tbreccnt
         cvd   r7,workarea
         mvc   dblwork2(l'nummask),nummask
         ed    dblwork2(l'nummask),workarea+4
         LA    R1,l'nummask
         la    r14,dblwork2
*         
* Skip leading spaces
*
         DO    from=(r1)  
           doexit (cli,0(r14),ne,c' ')
           aghi R14,1                  move along one character
         ENDDO         
*
         ST    R1,MSGS2LEN             set length
         ST    r14,MSGS2PTR            save the address of the data
*
* if rec count 
         lgf   r7,lbreccnt             counted records
*
         if (chi,r7,eq,-1)             if indicated more than in REH
           XC    MSGS3PTR,MSGS3PTR
           lghi  R14,REF_REC_NUM_ERR2 Indicate RED too big 
         else 
           cvd   r7,workarea
           mvc   dblwork(l'nummask),nummask
           ed    dblwork(l'nummask),workarea+4
           LA    R1,l'nummask
           LA    r14,dblwork
*         
* Skip leading spaces
*
           DO    from=(r1)  
             doexit (cli,0(r14),ne,c' ')
             aghi R14,1                  move along one character
           ENDDO 
           ST    R1,MSGS3LEN             set length
           ST    r14,MSGS3PTR            save the address of the data
           XC    MSGS4PTR,MSGS4PTR
           lghi  R14,REF_REC_NUM_ERR     INDICATE PREMATURE END OF RED
         endif
*
         J     RTNERROR
*
***********************************************************************
*  MAKE SURE REFERENCE DATA KEYS ARE IN ASCENDING SEQUENCE            *
*  Note:this is not required for Hash table searches                  *
***********************************************************************
FILL_ASC ds    0h
         llgt  R14,UR20RECA       LOAD  RECORD  ADDRESS
* Is a binary tree to be used ?
* Yes - keys must be in ascending sequence
* no  - skip to FILLCOPY
* Must use binary tree if effective dates are bing used...
         MVI   LBINDEX,LBHASH     Assume we are going to use HASH table
*
         TM    LBFLAGS,LBEFFDAT+LBEFFEND Effective dates used?
         BZ    FILLCOPY                  No, then can use HASH table
*
         MVI   LBINDEX,LBBINARY
*
FILLSKHL ds    0h
         LGH   R15,LBKEYLEN
*
FILLHILO LGH   R1,LBKEYOFF        COMPUTE KEY ADDRESS
         agr   R1,R14
*
         CG    R2,LBTBLBEG        FIRST   ROW ???
         JE    FILLSAVE           YES -  SKIP SEQUENCE CHECK
         EX    R15,FILLKYCK       NO  - CHECK FOR  ASCENDING SEQUENCE
         JNH   FILLSERR
*
FILLSAVE EX    R15,FILLKYSV       SAVE  CURRENT LOOK-UP  KEY
*
***********************************************************************
*  COPY REFERENCE DATA RECORD INTO MEMORY                             *
***********************************************************************
FILLCOPY DS    0H
         XC    LKLOWENT,LKLOWENT  ZERO BINARY SEARCH PATHS
         XC    LKHIENT,LKHIENT
*
         LGR   R3,R4              LOAD RECORD LENGTH
         LGR   R15,R4
         LA    R2,LKUPDATA
         MVCL  R2,R14             NOTE: R2 IS ADVANCED BY "MVCL"
*
         LGF   R15,LBRECCNT       INCREMENT   RECORD   COUNT
         LA    R15,1(,R15)
         ST    R15,LBRECCNT
*
         BRCTG R7,FILLLOOP        LOAD  ALL REFERENCE RECORDS
*
* Test for end of file 
*
*         LA    R0,UR20PARM        CALL "GVBUR20"
*         sty   R0,GENPARM1
         lay   R1,GENPARM1
         llgf  R15,GVBUR20A
         bassm R14,R15            GET  THE  NEXT  RECORD
*
         CHI   R15,8              END OF RED DATA ?
         JE    FILLALL            YES, all good to carry on
*                 and BUILD BINARY SEARCH PATHS if required
*
*        set count to high to indicate greater than REH value
         LHi   R14,-1
         ST    R14,LBRECCNT
*
         LTR   R15,R15            Read an extra record?
         JZ    FILLEPTE           Yes, INDICATE ERROR, REH CNT WRONG
*
         lghi  R14,REF_READ_ERR   Some other read error
         MVC   ERRDATA+00(8),UR20DDN
         J     RTNERROR
*
FILLALL  DS    0H 
         CLI   LBINDEX,LBBINARY   Is a binary tree to be used?
         JE    BLDPATH            BUILD BINARY SEARCH PATHS
         CLI   LBINDEX,LBHASH     Is a HASH table to be used?
         JE    BLDHASH            BUILD HASH table index
*
Bldexit_v4 ds  0h
         XC    REFADDR,REFADDR    RESET REFERENCE REC  ADDRESS
*
         LGHI  R0,4               CLOSE THE TABLE DATA FILE
         STH   R0,UR20FC
         LAY   R0,UR20PARM        CALL "GVBUR20"
         STY   R0,GENPARM1
         LAY   R1,GENPARM1
         llgf  R15,GVBUR20A
         bassm R14,R15            ISSUE THE CLOSE
*
         aghi  r6,tbhdrlen        Next entry in REH table
         jct   r10,Fillreh_loop   Advance to the next header entry
*        j     filldone
*
***********************************************************************
*  CLOSE REFERENCE DATA HEADER AND DATA FILES                         *
***********************************************************************
Filldone ds    0h
         Ly    R2,HDRDCBa
         sam31
         sysstate amode64=NO
         CLOSE ((R2)),MODE=31,MF=(E,WKREENT)
         sam64
         sysstate amode64=YES
*
         XC    REFADDR,REFADDR    RESET REFERENCE REC  ADDRESS
*
         lhi   R0,4               CLOSE "MR95RED" IF   OPEN
         STH   R0,UR20FC
         MVC   UR20DDN,MR95RED
*
         LA    R0,UR20PARM        CALL "GVBUR20"
         sty   R0,GENPARM1
*
         lay   R1,GENPARM1
         llgf  R15,GVBUR20A
         bassm R14,R15            ISSUE THE CLOSE

FILLEXIT lmg   R14,R12,SAVF4SAG64RS14  RESTORE REGISTERS
         BR    R14
*
***********************************************************************
*  LOOK-UP DATA KEY SEQUENCE ERROR                                    *
***********************************************************************
FILLSERR DS    0H
* Set up fields for the error message
* Key Value
         lghi  r0,l'ERRDATA
         cgr   R15,R0             KEY  LENGTH  EXCEEDS MAXIMUM ???
         JNH   FILLSER6           NO - SHOW    COMPLETE KEY
         lgr   R15,R0             YES - USE    MAXIMUM  LENGTH ALLOWED
FILLSER6 EX    R15,FILLKYMV       MOVE  KEY TO INDICATIVE DATA AREA
* DDname
         CLC   LBDDNAME+4(4),HEXFF         V4 DDNAME ???
         JE    FILLSER2                    YES -  BRANCH
         MVC   DBLWORK,LBDDNAME
         J     FILLSER4
FILLSER2 DS    0H
         MVC   DBLWORK,UR20DDN
         LA    r1,l'UR20DDN       set length of DDName
         ST    R1,MSGS2LEN        Save in msg list
         LA    r1,DBLWORK         Address of DDName
         st    r1,MSGS2PTR        save in msg list
*
FILLSER4 DS    0H
* LRID
         L     R0,LBLRID          LOGICAL RECORD ID
         CVD   R0,DBLWORK2
         OI    DBLWORK2+L'DBLWORK2-1,X'0F'
         UNPK  DBLWORK3,DBLWORK2
*
         LA    R1,l'DBLWORK3      set length of LR ID number
         ST    R1,MSGS3LEN        Save in msg list
         LA    r1,DBLWORK3        Address of LR ID
         st    r1,MSGS3PTR        save in msg list
*
         XC    MSGS4PTR,MSGS4PTR
*
         lghi  R14,REF_SEQ_ERR    KEYS  ARE OUT-OF-SEQUENCE
         J     RTNERROR           RETURN ERROR MESSAGE
*
         DROP  R2
         DROP  R6
                        EJECT
***********************************************************************
*           B U I L D   B I N A R Y   S E A R C H   P A T H S         *
*                                                                     *
*        SEARCH PATHS ARE BUILT BY TRAVERSING THE TREE                *
*        TOP TO BOTTOM, LEFT TO RIGHT                                 *
*                                                                     *
*        R7  - LOOK-UP TABLE  ENTRY    LENGTH                         *
*        R6  - CURRENT STACK  ELEMENT  ADDRESS                        *
*        R5  - LOOK-UP BUFFER ADDRESS                                 *
*        R4  - CURRENT TOP    SUBSCRIPT                               *
*        R3  - CURRENT BOT    SUBSCRIPT                               *
*        R2  - CURRENT NODE   ADDRESS                                 *
*        R1  - CURRENT NODE   SUBSCRIPT                               *
*                                                                     *
***********************************************************************
BLDPATH  ds    0h
*
         stmg  r6,r7,dblwork2
         LGHI  R3,1               INITIALIZE  BOTTOM SUBSCRIPT
         LGF   R4,LBRECCNT        INITIALIZE     TOP SUBSCRIPT
         LTGR  R4,R4              ANY   RECORDS   IN TABLE ???
         JNP   BLDEXIT            NO  - EXIT
*
         LAY   R6,BINSTACK        INITIALIZE CURRENT STACK POINTER
*
         LGH   R7,LBRECLEN        LOAD TABLE ENTRY  LENGTH
         AGHI  R7,LKPREFLN
*
         LA    R14,0(R3,R4)       COMPUTE MIDDLE NODE SUBSCRIPT
         SRLG  R14,R14,1
         LGR   R1,R14             CURRENT SUBSCRIPT = MIDDLE SUBSCRIPT
*
         BCTGR R14,0              COMPUTE MIDDLE NODE ADDRESS
         LGR   R15,R7
         MLGR  R14,R14
         AG    R15,LBTBLBEG
         LGR   R2,R15
         STG   R2,LBMIDDLE        SAVE    MIDDLE NODE ADDRESS
*
         XGR   R0,R0              R0 = 0
                        SPACE 3
BLDLOOP  LAY   R15,BINSTACK       STACK EMPTY ???
         CGR   R6,R15
         BRL   BLDEXIT            YES - DONE
*
***********************************************************************
*  FOLLOW LEFT BRANCHES "PUSHING" INTERMEDIATE NODES UNTIL LEAF NODE  *
***********************************************************************
         CGR   R1,R3              CURRENT NODE SUBS = BOTTOM NODE SUBS?
         JNE   BLD030             NO  - FOLLOW LEFT   TREE BRANCH
*
         STG   R0,LKLOWENT-LKUPTBL(,R2)   ZERO LOW POINTER/NO LEFT NODE
*
         CGR   R1,R4              CURRENT NODE SUBS = TOP    NODE SUBS?
         JNE   BLD010             NO  - FOLLOW RIGHT  TREE BRANCH
*
         STG   R0,LKHIENT-LKUPTBL(,R2)  ZERO HIGH POINTER/NO RIGHT NODE
*
***********************************************************************
*  "POP" STACK WHEN LEAF NODE REACHED (NO LEFT/RIGHT BRANCHES)        *
***********************************************************************
         LGHI  R15,L'BINSTACK     "POP" STACK
         SGR   R6,R15
         LAY   R15,BINSTACK       STACK EMPTY ???
         CGR   R6,R15
         BRL   BLDEXIT            YES - DONE
*
         LG    R2,00(,R6)
         LG    R3,08(,R6)
         LG    R4,16(,R6)
         J     BLD020
*
BLD010   LA    R3,1(,R1)          BOTTOM SUBS = CURRENT SUBS + 1
*
***********************************************************************
*  COMPUTE NEXT "RIGHT BRANCH" MIDDLE NODE SUBSCRIPT                  *
***********************************************************************
BLD020   LA    R14,0(R3,R4)       COMPUTE NEW MIDDLE (RIGHT BRANCH)
         SRLG  R14,R14,1
         LGR   R1,R14
*
         BCTGR R14,0
         LGR   R15,R7
         MLGR  R14,R14
         AG    R15,LBTBLBEG
         STG   R15,LKHIENT-LKUPTBL(,R2)
         LGR   R2,R15
         J     BLDLOOP
*
***********************************************************************
*  IF CURRENT NODE IS NOT LEFTMOST NODE IS IT THE RIGHTMOST NODE      *
***********************************************************************
BLD030   CGR   R1,R4              CURRENT NODE SUBS = TOP NODE SUBS?
         JNE   BLD040             NO  - FOLLOW LEFT  TREE BRANCH
*
         STG   R0,LKHIENT-LKUPTBL(,R2)   ZERO HIGH POINTER
         J     BLD050
*
***********************************************************************
*  "PUSH" INTERMEDIATE NODE WITH LEFT/RIGHT BRANCHES ONTO STACK       *
***********************************************************************
BLD040   LGR   R14,R3             SAVE CURRENT BOTTOM SUBSCRIPT
         LA    R3,1(,R1)          PUSH CURRENT + 1
         STG   R2,00(,R6)
         STG   R3,08(,R6)
         STG   R4,16(,R6)
         AGHI  R6,L'BINSTACK
         LGR   R3,R14
*
***********************************************************************
*  COMPUTE NEXT "LEFT BRANCH" MIDDLE NODE SUBSCRIPT                   *
***********************************************************************
BLD050   LGR   R4,R1              TOP SUBSCRIPT = CURR SUBSCRIPT - 1
         BCTGR R4,0
*
         LA    R14,0(R3,R4)       COMPUTE NEW MIDDLE  (LEFT BRANCH)
         SRLG  R14,R14,1
         LGR   R1,R14
*
         BCTGR R14,0
         LGR   R15,R7
         MLGR  R14,R14
         AG    R15,LBTBLBEG
         STG   R15,LKLOWENT-LKUPTBL(,R2)
         LGR   R2,R15
         J     BLDLOOP
*
BLDEXIT  ds    0h
*
         lmg   r6,r7,dblwork2
         J     Bldexit_V4
         drop  r13
*
***********************************************************************
*           BUILD HASH table index                                    *
*                                                                     *
*  On entry:                                                          *
*                                                                     *
*        R5  - LOOK-UP BUFFER ADDRESS                                 *
*        R13 - Thread work area                                       *
*                                                                     *
*  Register usage:                                                    *
*                                                                     *
*        R2  - Hash slot calculation                                  *
*        R3  - Hash slot calculation                                  *
*        R4  - Collision count HWM                                    *
*        R5  - LOOK-UP BUFFER ADDRESS                                 *
*        R6  - Pointer to current reference record in ref table       *
*        R7  -                                                        *
*        R8  - EXECDATA (EXTRPARM values)                             *
*        R9  - Record count for this reference table                  *
*        R10 - Subroutine return address                              *
*        R11 -                                                        *
*        R12 - static base                                            *
*        R14 - Address of key                                         *
*        R15 - Length of key                                          *
*                                                                     *
***********************************************************************
BLDHASH  ds    0h
         USING THRDAREA,R13
         using savf4sa,savesub3
* save registers in bottom level save area as it calls no other subrout
         stmg  R14,R12,SAVF4SAG64RS14     SAVE REGISTERS
*
* report on hash values?
*
         llgt  R8,EXECDADR
         USING EXECDATA,R8
         if cli,exec_disphash,eq,c'Y'
*          open special report file
           bas r10,Open_hash_report
         endif
*
         lg    r2,refpoolc      Get start of current free area in pool
         STG   r2,LBHASHBEG
         XC    LBCOLLISION,LBCOLLISION
*
         LG    R1,LBHPRIME        Get size of hash table
         SLLG  R1,R1,3            MULTIPLY BY 8  (DOUBLEWORDS)
*
         agr   r1,r2              calculate end of index
         STG   r1,LBHASHEND
         STG   r1,refpoolc        next available address in pool
*
* loop through the reference data computing slot addresses
*
         SR    R4,R4              COLLISION HWM
         LG    R6,LBTBLBEG        Start of data table for this REFR
         USING LKUPTBL,R6         Entry header mapping
         LGF   R9,LBRECCNT        Number of ref records for this file
         do from=(r9)
*
           lgh   r15,LBKEYLEN     key length
           la  r14,LKUPDATA       load reference record address
** key always at start of data ???
*          lgh r0,LBKEYOFF        ...add key offset
*          agr r14,R0             ...address key
*
           if cli,EXEC_HASHPACK,eq,c'Y' Packing before checksum?
*  if numeric, then pack - to see if we get less collisions
*
             sgr r1,r1             zero work registers
             lay r3,pgmwork        address work area to hold packed key
             la r15,16(,r15)
             srlg r2,r15,4
             j  testpack1
testpack0    pack 0(9,R3),0(16,R14) pack first 16 bytes
             LA R14,16(,R14)      ADVANCE source
             LA R3,9(,R3)         ADVANCE target
             LA r1,9(,r1)         keep track of packed key length
testpack1    BRCT R2,testpack0
             nill r15,x'ff0f'
             exrl r15,testpack
             LA r1,9(,r1)         keep track of packed key length

             MVI LBHASHT,C'P'     Save HASH type include PACK

             lay r14,pgmwork      address of packed data
             lgr r15,r1           length of packed data
           else
             MVI LBHASHT,C' '     clear HASH type field
             AHI R15,1            Add 1 to length (length is -1 for EX)
           endif
*
           sgr r3,r3              zero work registers
           sgr r2,r2
*
hashloop   cksm r3,r14            Compute Checksum
           jnz hashloop
*
           lgr r1,r3              remember hash result for report
           d   r2,LBHPRIME+4      remainder in R2 is the hash value
*
           if cli,exec_disphash,eq,c'Y' logging full report?
*            log hash statistics R1=HASH result,R2=Mod result (slot no)
             bas r10,write_hash_report
           endif
*
           sllg R2,r2,3           CONVERT SUBSCRIPT  TO SLOT ADDRESS
           AG  R2,LBHASHBEG
           LGR R3,R2
*
           if ltg,R2,0(,R3),z     Is this slot empty?
             STG R6,0(,R3)        Save addr of ref record in hash table
             XC LKSNEXT,LKSNEXT   NO SYNONYMS (NEXT)
             XC LKSCOUNT,LKSCOUNT NO SYNONYMS (COUNTER)
           else   ,               Slot not empty - collision - chain
*            count total collisions
             ASI LBCOLLISION,1
             LGR R7,R2            save pointer to first entry
syncount     USING LKUPTBL,R7
syn          USING LKUPTBL,R1
             do until=(ltg,r2,syn.LKSNEXT,z) loop to end of syn chain
               LGR r1,r2
             enddo
             STG r6,syn.LKSNEXT
             AGSI syncount.LKSCOUNT,1
             IF CG,R4,LT,syncount.LKSCOUNT
              AGFI R4,1
             ENDIF
           endif
           drop syn,syncount

*          point to next REF rec
           LGH  R7,LBRECLEN        LOAD TABLE ENTRY  LENGTH
           AGHI R7,LKPREFLN
           AGR  r6,r7
*
         enddo
*
         if cli,exec_disphash,eq,c'Y'  logging full report?
*          close special report file
           bas r10,close_hash_report
         endif
*
* LOG the number of collisions
         LAY   R1,traccol
         MVC   prntrdw(l'traccol),0(r1)
         L     R0,LBFILEID
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  prntrdw+4+15(8),DBLWORK
*
         L     R0,LBCOLLISION
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   WORKAREA(12),FULLMASK12
         ED    WORKAREA(12),DBLWORK+2
         MVC   prntrdw+4+51(12),WORKAREA
*
         L     R0,LBHPRIME+4
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   WORKAREA(12),FULLMASK12
         ED    WORKAREA(12),DBLWORK+2
         MVC   prntrdw+4+70(12),WORKAREA
*
         CVD   R4,DBLWORK      collision HWM of longest chain
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   WORKAREA(12),FULLMASK12
         ED    WORKAREA(12),DBLWORK+2
         MVC   prntrdw+4+89(12),WORKAREA
*
         logit debug=Y
*
* restore registers
         lmg  R14,R0,SAVF4SAG64RS14
         lmg  R2,R12,SAVF4SAG64RS2
         J     Bldexit_v4
         ds   0d
testpack pack 0(9,r3),0(0,r14)
*
         DROP  R8   EXECPARMS
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* Open Report file for reference table Hash statistics                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Open_hash_report ds  0h

         STORAGE OBTAIN,LENGTH=HASHDCBL+l'hasheyeb,  Get memory for DCB+
               LOC=BELOW                 make sure it is belowfor DCB
*        Storage returns a valid 64-bit address
         lgr   r2,r1                     copy for later
         ahi   R2,l'hasheyeb
         MVC   0(l'hasheyeb,R1),HASHEYEB COPY   EYEBALL
         ST    R2,HASHDCBA        SAVE   DCB  ADDRESS
*
         larl  R14,hashfile              COPY MODEL   DCB
         MVC   0(HASHDCBL,R2),0(R14)
         using ihadcb,r2
         LA    R0,HASHDCBE-HASHFILE(,R2) SET  DCBE    ADDRESS IN  DCB
         ST    R0,DCBDCBE
*
         L     R0,LBFILEID
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         unpk  dcbddnam+1(7),dblwork     build ddname from LRID
*
         MVC   WKREENT(8),OPENPARM       OPEN  PARAMETER LIST
         sysstate amode64=NO
         sam31
         OPEN  ((R2),(OUTPUT)),MODE=31,MF=(E,WKREENT) CONTROL RPT FILE
         sam64
         sysstate amode64=YES
         if tm,dcboflgs,dcbofopn,z    SUCCESSFULLY OPENED  ??
          WTO   'GVBMR95 - UNABLE TO OPEN HASH STATISTICS FILE'
         endif
*
         BR    R10
*
         drop  r2

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* Write Report file for reference table Hash statistics               *
*                                                                     *
*  On entry:                                                          *
*                                                                     *
*        R1  - HASH result                                            *
*        R2  - Mod result (slot no)                                   *
*        R5  - LOOK-UP BUFFER ADDRESS                                 *
*        R6  - Pointer to current reference record in ref table       *
*        R13 - Thread work area                                       *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
write_hash_report ds 0h
*
         stmg  r1,r2,dblwork2
*
         lay   r3,prntrdw
         xc    prntrdwl,prntrdwl
         lhi   r15,hashrecl+4
         sth   r15,prntrdwh
         using hashREC,prntline
*
         L     R0,LBFILEID
         CVD   R0,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  hashLRID,DBLWORK
*
         lgh   R15,LBKEYLEN       key length
         la    R14,LKUPDATA       load reference record address
         MVC   hashKEY,spaces
         exrl  r15,mvckey
*
         ST    R1,hashHASH
         ST    R2,hashSLOT
*
* check the stats file is open - otherwise just skip
         llgt  R2,HASHDCBA
         using ihadcb,r2
         if      (cij,r2,ne,0),and,        not zero?                   +
               (tm,dcboflgs,dcbofopn,o)    and dcb is open
*
           sam31
           sysstate amode64=NO
           put (r2),(r3)
           sam64
           sysstate amode64=YES
*
         endif
*
         lmg   r1,r2,dblwork2
         BR    R10
*
MVCKEY   MVC   hashKEY(0),0(r14)
         DROP  R5   lkupbufr
*
static   loctr
HASHREC  DSECT ,
hashLRID ds cl8
hashKEY  ds cl100 (100 for now could be longer)
hashHASH ds cl4  (after pack(if doing it) and checksum)
hashSLOT ds cl4  (Mod of the hash)
hashRECL equ *-HASHREC
code     loctr
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* Close Report file for reference table Hash statistics               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Close_hash_report ds  0h
         lay   R2,HASHDCBA
         using ihadcb,r2
         if      (cij,r2,ne,0),and,        not zero?                   +
               (tm,dcboflgs,dcbofopn,o)    and dcb is open
           MVC WKREENT(8),OPENPARM
           sam31
           sysstate amode64=NO
           CLOSE ((R2)),MODE=31,MF=(E,WKREENT)
           sysstate amode64=YES
           sam64
         endif

         BR    R10

         DROP  r2,R13

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* FINDPRIME                                                           *
*                                                                     *
* Find the nearest prime number greater than number of elements       *
* R1 Contains number of elements                                      *
*    Returns the nearest prime in R1                                  *
*                                                                     *
* Enhancement - check the prime is "not too close to a power of 2"    *
*               (see Programming notes for CHECKSUM in the POP).      *
*             - Set a minimum size hash table                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,savesub3
FindPrime DS  0H
* save registers in bottom level save area as it calls no other routine
         stmg  R14,R12,SAVF4SAG64RS14     SAVE REGISTERS
         SGR   R0,R0
         llgt  R15,EXECDADR
         USING EXECDATA,R15
         M     R0,EXEC_HASHMULTB
         drop  r15
*        mghi  r1,3      try * 3
*        sllg  r1,r1,2   number * 4
*        SGR   R0,R0
*        LHI   R4,3      divided by 3
*        DR    R0,R4
*
         if CHI,R1,lt,1024
           LHI   r1,1024 Make hash table min of 1024 entries
         endif
         oill  r1,1      Make the number odd
*
         SGR  R0,R0
         LGR  R2,R0
         LGR  R3,R1      Save the number
* R2 and R3 contain number of elements (+1 perhaps)
*
*   THIS LOOPS THROUGH INCREASING ODD NUMBERS TESTING IF IT'S PRIME
*
         do inf
* calulate the largest factor to test - square root of number
           LGR  R5,R3
           cdgr FP1,r5
           SQDR FP2,FP1
           cgdr R5,0,FP2    R5 contains largest factor to test
* IsAPrime DS  0H
           LHI R4,3         Start testing for factors from 3
*
*          THIS LOOPS,TESTING POTENTIAL FACTORS
*
           do inf
             LGR r0,R2
             LGR R1,R3
             DR  R0,R4
             if ltr,r0,r0,z Any remainder?
               doexit (z)   no - not a prime
             endif
             CGR R4,R5      test all up to square root?
             JH  primefound yes - prime found
             ahi r4,2       no  - try next factor
           enddo
*
           AGFI R3,2        Try next odd number
         enddo
*
primefound ds 0h
         lgr r1,r3
*
* restore registers
         lmg  R14,R0,SAVF4SAG64RS14
         lmg  R2,R12,SAVF4SAG64RS2
         BR   R9

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* P A S S   O N E   T H R O U G H   T H E   L O G I C   T A B L E     *
*                                                                     *
*    1.  CONVERT  "TRUE/FALSE" ROW NUMBERS  TO ROW ADDRESSES          *
*    2.  LOOK-UP   AND SAVE RECORD BUFFER   ADDRESSES                 *
*    3.  COPY PREVIOUS  TEMPORARY LITERAL   POOL  INTO CODE  BUFFER   *
*    4.  COPY  MODEL CODE SEGMENT SKELETONS INTO  CODE BUFFER         *
*    5.  COPY  LITERALS/CONSTANTS TO TEMPORARY LITERAL POOL           *
*    6.  INSERT CONSTANT  OFFSETS  AND LENGTHS INTO  SKELETON         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,savesubr
         using genenv,env_area
         USING INITVAR,R8
*
pass1    stg   R14,PASS1SAV       SAVE RETURN ADDRESS
*
         llgt  R13,THRDMAIN       LOAD MAIN  DRIVER'S THREAD ADDR
*
         llgt  R7,LTBEGIN         LOAD LOGIC TABLE ADDRESS
         USING LOGICTBL,R7
         lgr   r15,r7             save it                         pgc2
         lgh   r0,0(r7)           get next entry                  pgc2
         agr   r15,r0             get next entry                  pgc2
         clc   re_tk,ltfunc-logictbl(R15) is it a RETK?           pgc2
         jne   pass1_d            N:                              pgc2
         mvc   errdata(8),ltddname-logictbl(r15) keep dd in case error
         llgt  r7,ltnxdisk        Y:Get the first ES from here    pgc2
         ltgf  r15,ltnxothr       also get first ES from here   pgc102
         jz    pass1_c                                          pgc102
         cgr   r7,r15             see if "disk" before "othr"   pgc102
         jl    pass1_c            Y then use "disk"             pgc102
         lgr   r7,r15             N then use "othr"             pgc102
pass1_c  ds    0h                                               pgc102
         llgt  r7,ltfrstre        Get the 1st RE (where we start) pgc2
         if (ltgr,r7,r7,z) then
           lghi  R14,NO_TOKEN_WRITER no token writer present
           J     RTNERROR
         endif
pass1_d  ds    0h                                               pgc102
*
         llgt  R2,CODEBEG         INITIALIZE  CODE BASE  REGISTER
         USING NVPROLOG,R2
         lgr   R3,R2              INITIALIZE    CODE SEGMENT   ADDRESS
         lgr   R4,R3              SAVE  CURRENT CODE POSITION
*
         llgt  R5,LITPOOLB        LOAD  LITERAL POOL BEGINNING ADDRESS
         USING LITP_HDR,R5
         BRAS  R9,LITPINIT
         st    r5,litp_base       Litpool start for event set     pgc99
         agf   r5,f512k           make base in middle of litpool
*
         xc    frstrtkn,frstrtkn  init RETK/RETX processing vars pgc1
         xc    currrtkn,currrtkn      "                          pgc1
         xc    firstnv,firstnv    1st NV of set?                   pgc6
         XC    CURRNV,CURRNV      ZERO  PREVIOUS "NV" PROLOGUE    ADDR
         XC    INITNV,INITNV      ZERO  "ES"  INITIALIZATION VIEW "NV"
         XC    ESPOOLSZ,ESPOOLSZ  RESET ESTIMATED LIT POOL   SIZE
                        EJECT
P1LOOP   CLC   LTMAJFUN,RE_NX     READ  EVENT   ???
         JE    P1FUNRE            YES  - LINK   PIPE (IF ANY)
*
         CLC   LTFUNC,ET          END-OF-TOKEN  ???
         JE    P1FUNET            YES  - COPY   MODEL CODE
         CLC   LTFUNC,ES          END-OF-SET    ???
         JE    P1FUNES            YES  - COPY   MODEL CODE
*
         CLC   LTFUNC,EN          END-OF-TABLE  ???
         JE    P1FUNEN            YES  - COPY   FINAL MARKER
*
         CLC   LTFUNC,NV          NEW   VIEW  ???
         JE    P1FUNNV            YES - CHECK IF PLACEHOLDER "NV" REQD
*
         J     P1INSERT           CHECK FOR IMPLICIT "NV" INSERTION
                        EJECT
***********************************************************************
*  PROCESS CLONED "RE/ES" SETS                                        *
***********************************************************************
p1funre  ds    0h                                                 pgc99
         st    r7,currre                                          pgc99
         xc    parent_token,parent_token    Zero by default       pgc99
         TM    LTFLAG1,LTESCLON   CLONED  "RE" ROW ???
         JNO   P1FUNRE0           NO  - BYPASS CLONING OF LITERAL POOL
*
         llgt  R9,LTCLONRE        LOAD  CLONED "RE"   ROW ADDRESS
         MVC   LTCODSEG,LTCODSEG-LOGICTBL(R9)
         mvc   ltnxrtkn,ltnxrtkn-logictbl(r9)                    pgc110
*
         llgt  R9,LTREES-LOGICTBL(,R9)  CLONED "ES"   ROW ADDRESS
         llgt  R14,LTESLPAD-LOGICTBL(,R9) ORIG LITERAL POOL ADDR
*
         sgf   r5,f512k
         lgr   R0,R5                      COPY LITERAL POOL
         llgt  R1,LTESLPSZ-LOGICTBL(,R9)  LOAD LITERAL POOL SIZE
         agf   R1,LITPOOLC                Add the current position
         cgf   R1,LITPOOLM                Past the end ?
         jnh   P1FUNREQ
           lghi  R14,LITP_OVERFLOW        Indicate overflow
           J     STDERROR
P1FUNREQ EQU   *
         llgt  R1,LTESLPSZ-LOGICTBL(,R9)  LOAD LITERAL POOL SIZE AGAIN
         lgr   R15,R1
         MVCL  R0,R14
*
         llgt  R14,LTREES                 LOAD   "ES"  ROW ADDRESS
         ST    R5,LTESLPAD-LOGICTBL(,R14) UPDATE  LIT  POOL   ADDR
         st    r7,lp_RE_addr-litp_hdr(,r5) save RE addr in lit pool
*
         lgr   R6,R0              UPDATE  END-OF-POOL  ADDRESS
         sgr   R0,R5

         ST    R0,LTESLPSZ-LOGICTBL(,R14) UPDATE  LIT  POOL   SIZE
         agf   r5,f512k
*
         MVC   LTCODSEG-LOGICTBL(,R14),LTCODSEG-LOGICTBL(R9)
         llgt  R1,LTESCODE-LOGICTBL(,R9)  COPY  MACH CODE ADDRESS
         ST    R1,LTESCODE-LOGICTBL(,R14)
*
         BRAS  R10,P1CLONLB       CLONE  LOOK-UP  BUFFER CHAIN
*
         llgt  r15,ltclonre       Is there a clone of this         @05I
         lgf   r14,ltfilcnt-logictbl(,r15) Get lit pool offs       @05I
         st    r14,ltfilcnt       Save in this clone               @05I
         agr   r14,r5             offset in literal pool           @05I
         xc    0(l'ltfilcnt,r14),0(r14) set to zero                @05I
         J     p1funre5                                            @05C
*
*******************************************************************@05I
*  Make the LTFILCNT field of RExx entry reside in literal pool    @05I
*******************************************************************@05I
P1FUNRE0 ds    0h                                                  @05I
         if clc,ltfunc,eq,re_tk,or,clc,ltfunc,eq,re_tx            pgc99
           st  r7,parent_token    This is the parent token for    pgc99
*                                   any WRTK's from this set      pgc99
           llgt r1,litp_base      Get start of base litpool       pgc99
           aghi r1,litphdrl       Get to start of token offsets   pgc99
           lgf r15,ltreindx       Get this RETK/RETX index        pgc99
           mghi r15,8             Make a word offset             pgc101
           agr r1,r15             Get correct offset              pgc99
           lgr r14,r6             Get current addres in lit pool  pgc99
           sgf r14,litp_base      Get offset                      pgc99
           st  r14,0(,r1)         save in token offset table      pgc99
           aghi r6,8+litphdrl     Allow for litp hdr and savearea pgc99
         endif                                                    pgc99
         la    r14,ltfilcnt       Get field address                @05I
         lghi  r15,l'ltfilcnt     Get field length                 @05I
         jas   r9,COPYFLD         Save LTFILCNT in literal pool    @05I
         st    r0,ltfilcnt        Save literal pool offset         @05I

P1FUNRE5 ds    0h                                                  @05I
***********************************************************************
*  LOGIC FOR "RE" (CONNECT PIPE READERS TO PIPE WRITERS)              *
***********************************************************************
         ltgf  R1,LTVDP200        LOAD VDP EVENT FILE REC ADDRESS  @05C
         JNP   P1NEXT             NO - BYPASS SEARCH
*
         ST    R3,LTCODSEG        SAVE  CODE  ADDRESS FOR THIS  LT ROW
*
         ltgf  R15,vdp0200b_EXTFILE_ADDR-vdp0200b_FILE_RECORD(,R1)
         JNP   P1NEXT             EVENT FILE HAS NO ASSOCIATED "WR"
*
P1PIPECK LH    R0,LTFILTYP        FILE MUST BE PIPE/TOKEN TO RD/WRT
         CHI   R0,PIPEDEV
         JE    P1PIPEOK
         CHI   R0,TOKENDEV
         JE    P1PIPEOK
*
         lghi  R14,SOURCE_PIPE_ERR LOAD "MUST BE PIPE/TOKEN" ERROR MSG
         MVC   ERRDATA(8),LTFILEDD DISPLAY WHICH FILE
         J     RTNERROR
*
p1pipeok ds    0h                                                  @03C
         st    r15,lt_pipe_wrexta SAVE EXTRACT FILE ADDR IN "RE"   @03I
*
         LH    R0,EXTPIPEC-EXTFILE(,R15)
         AHI   R0,1
         STH   R0,EXTPIPEC-EXTFILE(,R15)
*
         J     P1NEXT
                        EJECT
***********************************************************************
*  LOGIC FOR "ET" CALL RETURN                                         *
***********************************************************************
P1FUNET  ds    0h                                                 pgc1
         mvc   ltviewnv,firstnv                                    pgc6
         sgf   r5,f512k
         st    r5,lteslpad        save lit pool add in ET as well  pgc5
         l     r9,currre
         st    r9,lp_RE_addr-litp_hdr(,r5) Save RE addr in lit pool
         agf   r5,f512k
*
         la    r0,ltesretn        Set up return address in litpool pgc1
         jas   r9,saveaddr        Save in litpool                  pgc1
         st    r0,ltesretn        Save offset in ET                pgc1
         la    r0,ltespr11        Set up prev r11 addr  in litpool pgc1
         jas   r9,saveaddr        Save in litpool                  pgc1
         st    r0,ltespr11        Save offset in ET                pgc1
         llgt  R9,LTFUNTBL        LOAD ADDRESS  OF  FUNCTION TBL ENTRY
         USING FUNCTBL,R9
*
         llgt  R14,FCMODELA       LOAD ADDRESS OF MODEL CODE
         LH    R15,FCCODELN       LOAD LENGTH  OF MODEL CODE
         BCTR  R15,0              DECREMENT LENGTH  FOR "EX" INSTR
         EX    R15,MVCMODEL       COPY MODEL  CODE
         DROP  R9
*
         ST    R4,LTCODSEG        SAVE CODE ADDRESS FOR  ROW
         LA    R4,1(R4,R15)       SAVE END-OF-CODE  POSITION
*
*        Set up disable view code for last view in RETK/ET set
*
         llgt  R1,CURRNV          LOAD PREVIOUS "NV" CODE  SEGMENT ADDR
         llgt  R1,NVLOGTBL-NVPROLOG(,R1)   LOAD "NV" LOGIC TBL ROW ADDR
         llgt  R14,LTCODSEG-LOGICTBL(,R1)  LOAD 1ST  "NV" CODE SEG ADDR
         push  using
         drop  r2
         using nvprolog,r14
         ST    R3,NVNXVIEW                 SET  NEXT VIEW CODE SEG ADDR
         lgr   r9,r3
         sgr   r9,r14
         srlg  r9,r9,1
         st    r9,nvnop+2
         pop   using
*
         llgt  r1,ltcodseg        start of ET code                 pgc1
         aghi  r1,2               where we want to correct reloc   pgc1
         bras  r9,saverowa        save logic tbl row  address      pgc1
         sth   r0,0(,r1)          store  low    order 3 nibbles    pgc1
         ni    0(r1),x'0F'                                         pgc1
         oi    0(r1),x'20'                                         pgc1
         srl   r0,12                                               pgc1
         stc   r0,2(,r1)          store  high   order 2 nibbles    pgc1
*
*        st    r7,dblwork         save for later                 pgc500
*        l     r7,ltfrstre        Get the first RE                 pgc1
*                                                                  pgc1
         lgr   r14,r6             Get current end of literal pool pgc99
         sgr   r14,r5              and work out its length        pgc99
         agf   r14,f512k          get correct len as base reg is
*                                  in the middle of the literal
*                                  pool
         st    r14,lteslpsz       save length in "ET"

* *********************************************************************
*    this next section is added to gather literal pool statistcs as
*    the literal pools are assigned so the ~LITP report can be printed
*    after phase-1, adding tokens and cloning sets
* *********************************************************************
litpre   using logictbl,r15
         llgt  r15,ltfrstre
         llgf  r0,litpre.ltfileid          Get file id for this "RE"
         drop  litpre
litpstat using lpstatlitp,r15
         Llgt  R15,LITSTATC
         c     r15,LITSTATE        if we're we past the last entry
         jnh   litp2_00            then over-write last entry continue
         tm    initflag,litpstatovf  not again ?
         jo    litp2_A0
         st    r14,dblwork3
         st    r0,dblwork3+4
         logit msg=vb_blankl
*        la    r14,msg#806         get the message number
*        jas   r9,errformat        message to the report
             GVBMSG LOG,MSGNO=LITP_STATS_OVERFLOW,SUBNO=1,             +
               GENENV=GENENV,                                          +
               SUB1=(modname,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)

*        mvi   prntline,c' '       blank 1st char  (used to be CC)
*        lgf   r15,msg_bufl        get length of msg
*        la    r15,l'prntrdw(,r15) allow for RDW length
*        sth   r15,prntrdwh        save length in RDW
*        logit ,
         oi    initflag,litpstatovf
         llgf  r14,dblwork3
         llgf  r0,dblwork3+4
litp2_A0 ds    0h
         llgt  r15,LITSTATE        just continue w/out having all stats
litp2_00 ds    0h
         mvc   0(lp_stat_leng,R15),piesave    stats for this pool
         st    r0,litpstat.lp_lfid_litp
         st    r14,litpstat.lp_leng_litp
         c     r14,f1meg           is literal too large?
         jl    litp2_03
         oi    initflag,litp2lrg
litp2_03 ds    0h
         drop  litpstat
         xc    piesave(16),piesave                             reset
         ASI   LITSTATC,lp_stat_len
         ASI   LITPCNTA,1         Increment actual literal pools
* *********************************************************************

         llgt  r7,ltfrstre        Get the first RE                 pgc1
         st    r14,ltreltpl          Save length in "RE"          pgc99
         st    r14,ltreltpl_cum      Save length in "RE" cum      pgc99
*                                                                 pgc99
*       Now add this literal pool length to cumulative length     pgc99
*       in parent token(s)                                        pgc99
         lgr   r15,r7                                             pgc99
p1funet1_lp ds 0h                                                 pgc99
parent_re using logictbl,r15                                      pgc99
         ltgf  r15,parent_re.ltrertkn_parent Parent token if one  pgc99
         jz    p1funet1_end                                       pgc99
         lgr   r1,r14             Save value in r14               pgc99
         agf   r1,parent_re.ltreltpl_cum Increment cumulative     pgc99
         st    r1,parent_re.ltreltpl_cum and save it              pgc99
         j     p1funet1_lp                                        pgc99
p1funet1_end ds 0h                                                pgc99
*                                                                  pgc1
         drop  parent_re
         xc    firstnv,firstnv    1st NV of set?                   pgc6
         ltgf  r7,ltnxrtkn        Another RETK/RETX on queue?      pgc1
         jz    p1funet3           N: then go back to ES            pgc1
*                                                                 pgc99

*                                                                  pgc1
         st    r7,currrtkn        Make this the current RETK/RETX pgc99
         st    r7,frstrtkn        Make it the 1st as well         pgc99
*                                                                  pgc1
         lgr   r2,r4              reset base/first "nv" seg addr   pgc1
         lgr   r3,r4              reset base/first code seg addr   pgc1
*                                                                  pgc1
         mvc   ltescode,escodbeg  save  current code beginning     pgc1
         xc    escodbeg,escodbeg  reset machine code beginning     pgc1
*                                                                  pgc1
         mvc   ltesinit,initnv    save  initialtn code begin       pgc1
         xc    initnv,initnv      reset initialtn "nv" code addr   pgc1
*                                                                  pgc1
         xc    prevnv,prevnv      zero previous "nv" code seg addr pgc1
         xc    currnv,currnv      zero current  "nv" code seg addr pgc1
*                                                                  pgc1
         mvc   symtblc,symtblb    reset current symbol table  beg
         lgr   r5,r6              Set litpool start to this       pgc99
         st    r5,litp_base_tok   save start of tok lit pool section
         agf   r5,f512k
         j     p1loop                                              pgc1
p1funet3 ds    0h                                                  pgc1
         lg    r15,p1_return      N: finished queue so return to ESpgc1
         br    r15                                                 pgc1
*
***********************************************************************
*  LOGIC FOR "ES"                                                     *
***********************************************************************
P1FUNES  TM    LTFLAG1,LTESCLON   CLONED "ES" SET ???
         JNO   p1funes_a          NO  -  PERFORM  "ES" LOGIC
         lgr   R5,R6              UPDATE NEXT LIT POOL ADDRESS
         ST    R5,LITPOOLC
         agf   r5,f512k
*                                                                pgc500
         ltgf  r15,ltesrtkq       Are there tokens               pgc500
         jnp   p1next             N:                             pgc500
tokn_re  using logictbl,r15                                      pgc500
p1funes_tloop ds 0h                                              pgc500
         llgt  r1,tokn_re.ltrees  Get "ET" for this "RETK"       pgc500
tokn_es  using logictbl,r1                                       pgc500
         la    r1,tokn_es.ltpipels  Is there a pipe list?        pgc500
         drop  tokn_es                                           pgc500
p1funes_pipelp ds 0h                                             pgc500
         ltgf  r1,0(,r1)          load next list element address pgc500
         jnp   p1funes_tnext      N:                             pgc500
*                                                                pgc500
         llgt  r14,4(,r1)         Get the EXTFILE address        pgc500
clext2   using extfile,r14                                       pgc500
         lh    r2,clext2.extpipep Increment parent thread count  pgc500
         ahi   r2,1                                              pgc500
         sth   r2,clext2.extpipep                                pgc500
         drop  clext2                                            pgc500
*                                                                pgc500
         j     p1funes_pipelp     next entry                     pgc500
p1funes_tnext ds 0h                                              pgc500
         ltgf  r15,tokn_re.ltnxrtkn .Is there another "RETK"?    pgc500
         jp    p1funes_tloop      Y:                             pgc500
         drop  tokn_re                                           pgc500
*                                                                pgc500
         J     P1NEXT             BYPASS "ES"  CODE INSERTION
*
p1funes_a ds   0h                                                pgc500
         mvc   ltviewnv,firstnv                                    pgc6
         llgt  r1,LITPOOLC        LITERAL POOL current ADDR
         st    r1,LTESLPAD        SAVE  LITERAL POOL  ADDR
         l     r9,currre
         st    r9,lp_RE_addr-litp_hdr(,r1) Save RE addr in lit pool
*
         llgt  R9,LTFUNTBL        LOAD ADDRESS  OF  FUNCTION TBL ENTRY
         USING FUNCTBL,R9
*
         lgf   R14,FCMODELA       LOAD ADDRESS OF MODEL CODE
         LH    R15,FCCODELN       LOAD LENGTH  OF MODEL CODE
         BCTR  R15,0              DECREMENT LENGTH  FOR "EX" INSTR
         EX    R15,MVCMODEL       COPY MODEL  CODE
         DROP  R9
*
         ST    R4,LTCODSEG        SAVE CODE ADDRESS FOR  ROW
         LA    R4,1(R4,R15)       SAVE END-OF-CODE  POSITION
*
P1ESRLIM ltg   r14,READLIM        EVENT READ LIMIT SPECIFIED ???
         jz    P1FUNES2           NO  - BYPASS
*
         la    r14,readlim        Get field address                @05I
         lghi  r15,l'readlim      Get field length                 @05I
         jas   r9,COPYFLD         Save LTFILCNT in literal pool    @05I
         STH   R0,4+2(,R3)        savefull returns the offset in r0
         NI    4+2(R3),X'0F'      remove any extra bits
         OI    4+2(R3),X'20'      and set r2 in
         SRL   R0,12
         STC   R0,4+2+2(,R3)      save high order byte
*
P1FUNES2 llgt  R1,CURRNV          LOAD PREVIOUS "NV" CODE  SEGMENT ADDR
         llgt  R1,NVLOGTBL-NVPROLOG(,R1)   LOAD "NV" LOGIC TBL ROW ADDR
         llgt  R14,LTCODSEG-LOGICTBL(,R1)  LOAD 1ST  "NV" CODE SEG ADDR
         push  using
         drop  r2
         using nvprolog,r14
         ST    R3,NVNXVIEW                 SET  NEXT VIEW CODE SEG ADDR
         lgr   r9,r3
         sgr   r9,r14
         srlg  r9,r9,1
         st    r9,nvnop+2
         pop   using
         xc    firstnv,firstnv    1st NV of set?                   pgc6
*                                                                  pgc1
*        Check if there were any WRTK/WRTX's in this ES            pgc1
*        We suspend the processing the last part of this ES until  pgc1
*        all the queue has been processed. At the end of the queue pgc1
*        processing we return here and pick up where we left off.  pgc1
*                                                                  pgc1
         ltgf  r15,frstrtkn       Is there a matching RETK queue?  pgc1
         jnp   p1funes2_b         N:                               pgc1
         st    r15,ltesrtkq       and save for future use         pgc99
         stg   r1,p1_r1save       Save r1 contents                 pgc1
         stg   r7,p1_r7save       Save current logic table row     pgc1
         st    r7,curres          Save current ES
         lay   r1,p1funes2_a      Get return address               pgc1
         stg   r1,p1_return       Save it for later                pgc1
*                                                                  pgc1
         lgr   r0,r6              calc current size..
         sgr   r0,r5              .. of literal pool
         agf   r0,f512k            (r5 points to top + 512k)
         st    r0,lteslpsz

* *********************************************************************
*    this next section is added to gather literal pool statistcs as
*    the literal pools are assigned so the ~LITP report can be printed
*    after phase-1, adding tokens and cloning sets
* *********************************************************************
         stg   r15,dblwork2
litpre   using logictbl,r15
         llgt  r15,ltfrstre
         llgf  r2,litpre.ltfileid          Get file id for this "RE"
         drop  litpre
litpstat using lpstatlitp,r15
         Llgt  R15,LITSTATC
         c     r15,LITSTATE        if we're we past the last entry
         jnh   litp2_02            then over-write last entry continue
         tm    initflag,litpstatovf  not again ?
         jo    litp2_A2
         st    r0,dblwork3
         logit msg=vb_blankl
*        la    r14,msg#806         get the message number
*        jas   r9,errformat        message to the report
             GVBMSG LOG,MSGNO=LITP_STATS_OVERFLOW,SUBNO=1,             +
               GENENV=GENENV,                                          +
               SUB1=(modname,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
*
*        mvi   prntline,c' '       blank 1st char  (used to be CC)
*        lgf   r15,msg_bufl        get length of msg
*        la    r15,l'prntrdw(,r15) allow for RDW length
*        sth   r15,prntrdwh        save length in RDW
*        logit ,
         oi    initflag,litpstatovf
         llgf  r0,dblwork3
litp2_A2 ds    0h
         llgt  r15,LITSTATE        just continue w/out having all stats
litp2_02 ds    0h
         MVC   0(lp_stat_leng,R15),piesave     stats for this pool
         ST    R2,litpstat.lp_lfid_litp
         st    r0,litpstat.lp_leng_litp
         c     r0,f1meg           is literal too large?
         jl    litp2_04
         oi    initflag,litp2lrg
litp2_04 ds    0h
         drop  litpstat
         xc    piesave(16),piesave                             reset
         ASI   LITSTATC,lp_stat_len
         ASI   LITPCNTA,1         Increment actual literal pools
         lg    r15,dblwork2
* *********************************************************************

         xc    lteslpsz,lteslpsz
*                                                                  pgc1
         lgr   r2,r4              reset base/first "nv" seg addr   pgc1
         lgr   r3,r4              reset base/first code seg addr   pgc1
*                                                                  pgc1
         mvc   ltescode,escodbeg  save  current code beginning     pgc1
         xc    escodbeg,escodbeg  reset machine code beginning     pgc1
*                                                                  pgc1
         mvc   ltesinit,initnv    save  initialtn code begin       pgc1
         xc    initnv,initnv      reset initialtn "nv" code addr   pgc1
*                                                                  pgc1
         xc    prevnv,prevnv      zero previous "nv" code seg addr pgc1
         xc    currnv,currnv      zero current  "nv" code seg addr pgc1
         lgr   r7,r15             New logic table entry            pgc1
         mvc   currrtkn,frstrtkn  make current the 1st one        pgc99
         lgr   r5,r6              Set lit pool start to here      pgc99
         st    r5,litp_base_tok   save start of tok lit pool section
         agf   r5,f512k
         mvc   symtblc,symtblb    reset current symbol table  beg
         j     p1loop             go process the RETK/RETX queue   pgc1
p1funes2_a ds  0h                                                  pgc1
         lg    r1,p1_r1save       restore NV                       pgc1
         lg    r7,p1_r7save       restore this logic table entry   pgc1
         xc    frstrtkn,frstrtkn  reset for next set of WRTK/WRTXs pgc1
         xc    currrtkn,currrtkn  reset for next set of WRTK/WRTXs
         llgt  r5,litp_base       restore litpool start           pgc99
         j     p1funes2_c         N:                               pgc1
p1funes2_b ds  0h                                                  pgc1
         mvc   ltescode,escodbeg  save  current code beginning     pgc1
         mvc   ltesinit,initnv    save  initialization code begin  pgc1
         sgf   r5,f512k           Base in middile of litpool
p1funes2_c ds  0h                                                  pgc1
*
         ST    R5,PREVLITP        SAVE   CURRENT  LITERAL POOL   HEADER
*
         BRAS  R9,LITPSIZE        INITIALIZE NEXT LITERAL POOL   HEADER
         st    r5,litp_base       Litpool start for event set     pgc99
*
P1FUNES4 lgr   R2,R4              RESET BASE/FIRST  "NV"  SEGMENT  ADDR
         lgr   R3,R4              RESET BASE/FIRST  CODE  SEGMENT  ADDR
*
         ST    R5,LITPOOLC        RESET CURRENT LITERAL POOL BEG   ADDR
         MVC   LTESLPSZ,ESPOOLSZ  SAVE  ACTUAL  LITERAL POOL SIZE
         ltgf  r15,ltesrtkq       Is there a matching RETK queue?  pgc1
         jp    p1funes2_e         Y:                               pgc1
*
* *********************************************************************
*    this next section is added to gather literal pool statistcs as
*    the literal pools are assigned so the ~LITP report can be printed
*    after phase-1, adding tokens and cloning sets
* *********************************************************************
litpre   using logictbl,r15
         llgt  r15,ltfrstre
         llgf  r2,litpre.ltfileid          Get file id for this "RE"
         drop  litpre
litpstat using lpstatlitp,r15
         Llgt  R15,LITSTATC
         c     r15,LITSTATE        if we're we past the last entry
         jnh   litp2_01            then over-write last entry continue
         tm    initflag,litpstatovf  not again ?
         jo    litp2_A1
         logit msg=vb_blankl
*        la    r14,msg#806         get the message number
*        jas   r9,errformat        message to the report
             GVBMSG LOG,MSGNO=LITP_STATS_OVERFLOW,SUBNO=1,             +
               GENENV=GENENV,                                          +
               SUB1=(modname,8),                                       +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
*
*        mvi   prntline,c' '       blank 1st char  (used to be CC)
*        lgf   r15,msg_bufl        get length of msg
*        la    r15,l'prntrdw(,r15) allow for RDW length
*        sth   r15,prntrdwh        save length in RDW
*        logit ,
         oi    initflag,litpstatovf
litp2_A1 ds    0h
         llgt  r15,LITSTATE        just continue w/out having all stats
litp2_01 ds    0h
         MVC   0(lp_stat_leng,R15),piesave      stats for this pool
         ST    R2,litpstat.lp_lfid_litp
         mvc   litpstat.lp_leng_litp,ESPOOLSZ
         clc   litpstat.lp_leng_litp,f1meg is literal too large?
         jl    litp2_05
         oi    initflag,litp2lrg
litp2_05 ds    0h
         drop  litpstat
         xc    piesave(16),piesave                                reset
         ASI   LITSTATC,lp_stat_len
         ASI   LITPCNTA,1         Increment actual literal pools
* *********************************************************************

p1funes2_e ds  0h                                                  pgc1
         agf   r5,f512k           make base in middile of litpool
*
         lgr   R2,R4              RESET BASE/FIRST  "NV"  SEGMENT  ADDR
         lgr   R3,R4              RESET BASE/FIRST  CODE  SEGMENT  ADDR
*
         XC    ESPOOLSZ,ESPOOLSZ
*
         XC    ESCODBEG,ESCODBEG  RESET MACHINE CODE BEGINNING
*
         XC    INITNV,INITNV      RESET INITIALIZATION "NV" CODE ADDR
*
         MVC   SYMTBLC,SYMTBLB    RESET CURRENT SYMBOL TABLE  BEGINNING
*
         XC    PREVNV,PREVNV      ZERO PREVIOUS "NV" CODE  SEGMENT ADDR
         XC    CURRNV,CURRNV      ZERO CURRENT  "NV" CODE  SEGMENT ADDR
*
         J     P1NEXT
         DROP  R7 LOGICTBL

*
         USING LOGICTBL,R7
*
***********************************************************************
*  LOGIC FOR "EN"                                                     *
***********************************************************************
P1FUNEN  llgt  R9,LTFUNTBL        LOAD ADDRESS  OF  FUNCTION TBL ENTRY
         USING FUNCTBL,R9
*
         lgh   R15,FCCODELN       LOAD LENGTH  OF MODEL CODE
         llgt  R14,FCMODELA       LOAD ADDRESS OF MODEL CODE
         BCTgr R15,0              DECREMENT LENGTH  FOR "EX" INSTR
         EX    R15,MVCMODEL       COPY MODEL  CODE
*
         ST    R4,LTCODSEG        SAVE CODE ADDRESS FOR ROW
         LA    R4,1(R4,R15)       SAVE END-OF-CODE  POSITION
*
         DROP  R9
*
         sgf   r5,f512k
         ST    R5,LITPOOLC        SAVE END OF LITERAL POOL
         agf   r5,f512k
*
         J     P1NEXT             DONE WITH   PASS1 ("EN")
*
***********************************************************************
*  SUFFIX ON INITIALIZATION VIEW NEEDED?? ("NV" PRECEDING FIRST "RE") *
***********************************************************************
p1funnv  ds    0h
r14prolog using nvprolog,r14
         if    (ltgf,r14,initnv,p),and, "ES" INITIALIZATION VIEW ???   +
               (OC,r14prolog.NVNXVIEW,r14prolog.nvnxview,z) and not    +
                                       updated yet
*
*
         MVC   0(NVDISAB,R3),NVDISABs  DISABLE AFTER 1ST EXECUTION
         sgr   r14,r3             calculate difference back to start
         srag  r14,r14,1          divide by 2 - preserving sign
         st    r14,2(,r3)         and update larl offset
         aghi  R3,NVDISAB
         sgr   r3,r2              calculate difference in bytes
         srag  r3,r3,1            divide by 2 - halfwords count
         st    r3,2(,r2)          and update the JLNOP offset
         aghi  R4,NVDISAB
         lgr   r3,r4
         st    r3,nvnxview
         ST    R2,PREVNV          SAVE PREVIOUS "NV"     ADDRESS
*
         lgr   R2,R3              RESET  BASE/FIRST  "NV" SEGMENT ADDR
*
   elseif (lt,r14,currnv,p)    ,   Load the previous nv code seg addr
*
         MVC   0(L'BRANCH,R3),BRANCH       INSERT BRANCH TO NEXT  VIEW
         MVC   L'BRANCH(L'BRANCH,R3),BRANCH
         aghi  R3,2*L'BRANCH+1    +3 to align on next halfword bndry
*
         nill  R3,x'fffe'         throw away lowest bit
         lgr   r4,r3
*
       endif
*
***********************************************************************
*  UPDATE MINIMUM SORT KEY LENGTH FOR EACH EXTRACT FILE USED IN VIEW  *
***********************************************************************
r14logic using logictbl,r14
r9logic  using logictbl,r9
*
         lgr   r14,r7                                    pgc400
         lgr   r9,r14                                    pgc400
P1SKMINL CLC   WR_xt(2),r14logic.LTMAJFUN
         JNE   P1SKMINN
         CLC   wr_tk,r14logic.LTFUNC
         JE    P1SKMINN
         CLC   wr_tx,r14logic.LTFUNC
         JE    P1SKMINN
*
         ltgf  R1,r14logic.LTWREXTA LOAD EXTRACT FILE ADDRESS
         JNP   P1SKMINN           VALID ADDRESS - NO BYPASS MIN CHECK
         USING EXTFILE,R1
*
         LH    R15,r9logic.LTSORTLN       LOAD SORT KEY LENGTH
         AHI   R15,EXSRTKEY-EXTREC        ADD  PREFIX   LENGTH
         CH    R15,EXTMINLN       LARGER THAN  PREVIOUS LENGTH ???
         JNH   *+8                NO  -  SKIP  UPDATE
         STH   R15,EXTMINLN
         DROP  R1
*
P1SKMINN AH    R14,r14logic.ltrowlen
         CLC   nv,r14logic.LTFUNC
         JE    P1SKPFIL
         CLC   r14logic.LTMAJFUN,ES
         JE    P1SKPFIL
         CLC   r14logic.LTMAJFUN,ET
         JE    P1SKPFIL
         CLC   r14logic.LTFUNC,EN
         JNE   P1SKMINL
*
P1SKPFIL EQU   *
*        MVC   LTMAXOCC,MAXOCCUR  SAVE MAXIMUM  OCCURRENCE COUNT VALUE
*
         CLC   LTFUNC,NV          NEW  VIEW ???
         JNE   P1NEXT             NO - DONE WITH THIS LOGIC TBL ROW
*
***********************************************************************
*  UPDATE PREVIOUS "NV" CODE PROLOG AND START A NEW "NV" CODE SEGMENT *
***********************************************************************
P1FUNNV2 ltgf  R14,CURRNV         LOAD PREVIOUS "NV"  CODE  SEG ADDRESS
*        JNP   P1FUNNV6           NO  - BYPASS  SETTING FORWARD ADDRESS
         JNP   P1nvtokn           NO  - BYPASS  SETTING FORWARD ADDRESS
*
         ltgf  R1,r14prolog.NVLOGTBL  PLACEHOLDER "NV" CODE NEEDED
         JNP   P1FUNNV4                    YES - USE EXISTING CODE ADDR
         llgt  R14,LTCODSEG-LOGICTBL(,R1)  LOAD 1ST  "NV" CODE SEG ADDR
P1FUNNV4 ST    R3,r14prolog.NVNXVIEW  SET  NEXT VIEW CODE SEG ADDR
         lgr   r2,r3              Get current address
         sgr   r2,r14             subtract the base - offset
         srlg  r2,r2,1            divide by 2 - halfword offset
         st    r2,r14prolog.nvnop+2   set offset in jlnop so that
*              this view can be disabled
         J     P1FUNNV6           do the new view logic
*
***********************************************************************
*  APPEND SUFFIX TO MACHINE CODE IF EVENT RECORD IS TOKEN             *
***********************************************************************
P1NVTOKN TM    LTFLAG2,LTRTOKEN   EVENT  DATA IS TOKEN ???
*        JNO   P1NEXT             NO  -  BYPASS  ADD'L CODE
         JNO   p1funnv6          NO  -  BYPASS  ADD'L CODE
*
         llgt  R15,LTVIEWRE       LOAD  "RE"  ROW   ADDRESS
         CLC   LTFUNC-LOGICTBL(L'LTFUNC,R15),RE_TK
         JNE   P1NEXT
*
         ltgf  R0,LTLBADDR-LOGICTBL(,R15)  IS THIS "RETK" SET CALLED ??
         JZ    P1NEXT                      NO  - BYPASS SUFFIX
*
P1NVTKN4 MVC   0(RDTOKENL,R4),RDTOKEN    COPY  READ TOKEN CODE
*
         aghi  R4,RDTOKENL
*
         lgr   r3,r4
*
         TM    LTFLAG2,LTTKNUPD   TOKEN UPDATE VIEW   ???
*        BRNO  P1NEXT
         BRNO  p1funnv6
*
         MVC   0(TOKNUPDL,R4),TOKNUPD
         aghi  R4,TOKNUPDL
         lgr   r3,r4
***********************************************************************
*  LOGIC FOR NEW "NV"                                                 *
***********************************************************************
P1FUNNV6 lgr   R2,R3              ADVANCE CODE SEGMENT BASE   POINTER
*
         llgt  R9,LTFUNTBL        LOAD ADDRESS  OF  FUNCTION TBL ENTRY
         USING FUNCTBL,R9
*
         lgh   R15,FCCODELN       LOAD LENGTH  OF MODEL CODE
         llgt  R14,FCMODELA       LOAD ADDRESS OF MODEL CODE
         BCTR  R15,0              DECREMENT LENGTH  FOR "EX" INSTR
         EX    R15,MVCMODEL       COPY MODEL  CODE
*
         ST    R4,LTCODSEG        SAVE CODE ADDRESS FOR ROW
         LA    R4,1(R4,R15)       SAVE END-OF-CODE  POSITION
*
         DROP  R9
*
         ST    R7,NVLOGTBL        SAVE LOGIC TABLE ROW ADDRESS
*
         oc    firstnv,firstnv    1st NV of set?                   pgc6
         jnz   p1funnv7           N:                               pgc6
         st    r2,firstnv         Y:                               pgc6
p1funnv7 ds    0h                                                  pgc6
         ST    R2,CURRNV          UPDATE PREVIOUS "NV" CODE SEG  ADDR
         ST    R2,LTVIEWNV        SAVE    ACTUAL  "NV" CODE SEG  ADDR
*
         OC    ESCODBEG,ESCODBEG  MACHINE CODE BEGINNING ALREADY SET ??
         JNZ   *+8
         ST    R2,ESCODBEG        NO  -   THIS "NV"  BEGINS MACH CODE
*
         L     R0,LTVIEW#
         ST    R0,NVVIEWID
*
         MVC   DRIVLRID,LTLRID
*
         ST    R7,LASTMULT        INITIALIZE   PREV CHAIN  PTR
         XC    MAXOCCUR,MAXOCCUR  ZERO MAXIMUM OCCURRENCE  COUNT VALUE
         XC    SVCOLMN#,SVCOLMN#  ZERO MAXIMUM COLUMN  NUMBER
         XC    SVTITLE#,SVTITLE#  ZERO MAXIMUM TITLE   KEY NUMBER
         XC    KEYPOSN,KEYPOSN    ZERO CURRENT LOOK-UP KEY POSITION
*
P1FUNNV8 llgt  R1,EXECDADR
         USING EXECDATA,R1
         CLI   EXECTRAC,C'Y'      TRACE OPTION SPECIFIED ??
         JNE   P1NVNOP
         DROP  R1
*
         CLI   VIEWTRAC,C'Y'      VIEW  SPECIFIC TRACE   OPTION  ???
*        JNE   P1NVTOKN           NO  - TRACE ALL VIEWS
         jne   p1toktst           NO  - TRACE ALL VIEWS
*
         llgt  R14,LTVIEWNV       LOAD  "NV"  CODE ADDR
         llgt  R14,NVLOGTBL-NVPROLOG(,R14)    LOAD  "NV" ROW  ADDR
         ltgf  R14,LTPARMTB-LOGICTBL(,R14)    TRACE THIS VIEW ???
*        JP    P1NVTOKN                       YES - BYPASS  NOP
         jp    p1toktst                       YES - BYPASS  NOP
*
         ASSERT l'NVTRACE,eq,l'nop4           make sure that the nop   +
                          matches the length of the instruction
P1NVNOP  MVC   NVTRACE,NOP4       NOP  BRANCH INSTRUCTION
p1toktst TM    LTFLAG2,LTRTOKEN   EVENT  DATA IS TOKEN ???
         JNO   P1NEXT             NO  -  BYPASS  ADD'L CODE
*
         ASSERT l'NVlevnt,eq,l'nop6           make sure that the nop   +
                          matches the length of the instruction
         mvc   nvlevnt,nop6
         j     p1next
                        EJECT
***********************************************************************
*  INSERT "IMPLICIT NV" FOR "ES" SET LEVEL FUNCTIONS (BEFORE "RE")    *
***********************************************************************
P1INSERT CLC   LTMAJFUN,HD        HEADER ???
         JE    P1TRUE             YES -  BYPASS "NV" INSERT
         CLC   LTMAJFUN,RE_NX     READ   EVENT  ???
         JE    P1TRUE             YES -  BYPASS "NV" INSERT
*
         ltgf  R0,CURRNV          ARE FUNCTIONS WITHIN AN "NV" ???
         JP    P1TRUE             YES -  BYPASS IMPLICIT "NV" INSERT
*
         llgt  R14,MR95NVA        COPY "NVPROLOG" (HEADER)
         MVC   0(nvcodeln,R3),0(R14)
*
         lgr   R2,R3              ADVANCE   BASE REGISTER
         aghi  R3,nvcodeln        ADVANCE CURRENT SEGMENT ADDRESS
         lgr   R4,R3              ADVANCE CURRENT END-OF-CODE
*
         ST    R2,CURRNV          SET  PLACEHOLDER  "NV"  CODE  POINTER
         ST    R2,INITNV          INDICATE  "ES"  INITIALIZATION "NV"
         ST    R2,LTVIEWNV        SAVE "NV" CODE  SEGMENT ADDR
*
         ST    R2,LTCODSEG        SAVE "NV" MACHINE  CODE ADDR  (TEMP)
         ST    R2,ESCODBEG        SAVE "ES" MACHINE  CODE BEGIN ADDRESS
*
*        MVC   SYMTBLC,SYMTBLB    RESET CURRENT SYMBOL TABLE  BEGINNING
                        EJECT
***********************************************************************
*  CONVERT ROW NUMBERS TO ACTUAL ADDRESSES OF LOGIC TABLE ROWS        *
***********************************************************************
P1TRUE   EQU   *
         lgf   R15,LTTRUE
         bctgr R15,0              SUBTRACT ONE
         sllg  R15,r15,2          LOAD  LENGTH   OF EACH ENTRY
         agf   R15,LTROWADR       ADD  BASE ADDRESS TO   OFFSET
         llgt  R15,0(,R15)
         ST    R15,LTTRUE         CONVERT SUBSCRIPT TO   ADDRESS
                        SPACE 3
P1FALSE  lgf   R15,LTFALSE
         bctgr R15,0              SUBTRACT ONE
         sllg  R15,r15,2          LOAD  LENGTH   OF EACH ENTRY
         agf   R15,LTROWADR       ADD  BASE ADDRESS TO   OFFSET
         llgt  R15,0(,R15)
         ST    R15,LTFALSE        CONVERT SUBSCRIPT TO   ADDRESS
*
*        LR    R15,R7
*        AH    R15,0(,R7)
*        BCTR  R15,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=(REGS),ID=130,STORAGE=((R7),(R15))
                        SPACE 3
***********************************************************************
*  FOR FUNCTIONS REQUIRING A LOOK-UP BUFFER                           *
*      CHECK THE CURRENT THREAD'S LOOK-UP BUFFER  CHAIN               *
*      IF FOUND, SAVE THE LOOK-UP BUFFER  ADDRESS                     *
*      IF NOT,   ALLOCATE LOOK-UP BUFFER  AND COPY FROM MAIN THREAD   *
***********************************************************************
         CLC   LTFUNC,LK_DC       BUILD EFFECTIVE  DATE ???
         JE    P1FUNCCD           YES - LOOK-UP  BUFFER NOT REQUIRED
         CLC   LTFUNC,LK_DE       BUILD EFFECTIVE  DATE ???
         JE    P1FUNCCD           YES - LOOK-UP  BUFFER NOT REQUIRED
         CLC   LTFUNC,KS_LK       KEY   SAVE   FUNCTION ???
         JE    P1FUNCCD           YES - LOOK-UP  BUFFER NOT REQUIRED
         CLC   LTMAJFUN,MUL_l     MULTIPLY     FUNCTION ???
         JNE   *+14               NO  - BYPASS SPECIFIC TEST
         CLC   LTFUNC,MUL_L       MULTIPLY   BY LOOK-UP ???
         JNE   P1FUNCCD           NO  - LOOK-UP  BUFFER NOT REQUIRED
*
         TM    LTFLAGS,LTLKUPRE   LOOK-UP PREFIX CODE REQUIRED ???
         BRO   P1LKUPBF           YES - GET LOOK-UP BUFFER ADDRESS
*
         CLC   LTMAJFUN,LU_SM     LOOK-UP  FUNCTION ???
         JE    P1LKUPBF           YES - GET LOOK-UP BUFFER ADDRESS
*
         CLC   LTFUNC,JOIN        LOOK-UP   KEY  LR ???
         JE    P1LKUPBF           YES - GET LOOK-UP BUFFER ADDRESS
*
         CLC   LTFUNC,LK_LR       LOOK-UP   KEY  LR ???
         JE    P1FUNLK            YES - LOOK-UP  BUFFER NOT NEEDED
*
         CLI   LTSUBFUN+0,C'L'    LOOK-UP  ARGUMENT ???
         JE    P1LKUPBF
         CLI   LTSUBFUN+1,C'L'
         JNE   P1FUNWR            NO  - CHECK  FOR  "WR" OPTIONS
*
P1LKUPBF bras  R14,p1look_up_alloc
         J     P1FUNCCD
p1look_up_alloc stmg  R14,R12,SAVF4SAG64RS14  SAVE   PASS1 REGISTERS
*
*
         XC    WORKAREA,WORKAREA  CLEAR WORK  AREA  (PATH)
*
         LA    R1,LTLUFILE        POINT TO FILE ID + RECORD ID
         CLC   LTFUNC,JOIN        JOIN  ???
         JE    P1LBLOC
         CLC   LTMAJFUN,LU_SM     LOOK-UP  ???
         JE    P1LBLOC
*
         LA    R1,LTFLDDDN        POINT TO FILE ID + RECORD ID
         CLC   LTFUNC,MUL_L       MUL   ???
         JE    *+12
         CLI   LTSUBFUN+0,C'L'    FIRST ARGUMENT REQUIRES LOOK-UP ???
         JNE   P1LB2NDA           NO  - ASSUME 2ND ATTRIBUTE  SET
*
         llgt  R9,LTFUNTBL        LOAD ADDR OF FUNCTION TABLE ENTRY
         CLI   FC_RTYP-FUNCTBL(R9),FC_RTYP12   NAMED "F1" ???
         JNE   P1LBLOC
*
P1LB2NDA ds    0h
         if    cli,ltsubfun+0,ne,c'C'   not constant as 1st operand?
           LA    R1,LTCOLDDN        NO  - MUST  BE SECOND  ARGUMENT
         endif
*
P1LBLOC  MVC   WORKAREA(L'LTLUFILE+L'LTLULRID),0(R1) EXCLUDE PATH
         LA    R1,WORKAREA
*
         LA    R4,LBCHAIN         POINT  TO  CHAIN ANCHOR
         BRAS  R9,LOCLB           LOCATE BUFFER IN MAIN DRIVER  AREA
*
         ltgr  R5,R5              BUFFER FOUND ???
         JP    P1LBADR0           YES -  COPY BUFFER TO CURRENT "ES"
*
         CLC   LTFUNC,JOIN        JOIN  ???
         JE    P1LBALLO           YES - BUILD PLACEHOLDER LOOK-UP BUFR
*
         CLC   LTFUNC,LU_EX       CALL  LOOK-UP  EXIT
         JE    P1LBALLO           YES - BUILD PLACEHOLDER LOOK-UP BUFR
*
         lghi  R14,LKUPBUFR_NOT_FOUND NO - INDICATE  ERROR
         J     STDERROR
*
         USING LKUPBUFR,R5
P1LBERR2 lghi  R14,LKUP_EXIT_NOT_FOUND No - ERROR MESSAGE NUMBER
         MVC   ERRDATA(8),LBSUBNAM   INDICATE PROGRAM NAME
         J     RTNERROR           PRINT ERROR MESSAGE - STOP
*
***********************************************************************
*  ALLOCATE TEMPLATE LOOK-UP BUFFER IN MAIN DRIVER FOR LOOK-UP EXITS  *
***********************************************************************
P1LBALLO lghi  R3,LBPREFLN        LOAD  PREFIX LENGTH
         aghi  R3,MAXKEYLN        ADD   LOOK-UP   KEY  WORK AREA LENGTH
*
         LA    R1,WORKAREA        POINT TO FILE/RECORD ID'S (EXCL PATH)
*
         BRAS  R9,ALLOLKUP        ALLOCATE   BUFFER
         USING LKUPBUFR,R5
*
         CLC   LTLUNAME,SPACES    EXIT SPECIFIED ???
         JE    P1LBADR0           NO  - BYPASS   LOAD
*
         OI    LBFLAGS,LBSUBPGM
         MVC   LBSUBNAM,LTLUNAME
         MVC   LBSTRTUP,LTLUPARM
*
         if    cli,ltluopt,eq,ltluopty
           oi  lbflags+1,lbexitopt
         endif
*
         l     r15,ltluptyp
*
         LA    R9,LBSUBNAM        POINT TO SUBROUTINE NAME
         LOAD  EPLOC=(R9),ERRET=P1LBERR2
         ST    R0,LBSUBADR        SAVE  SUBROUTINE ADDRESS (31-BIT)
         ST    R0,LBSUBENT        SAVE  TRUE ENTRY POINT
*
         llgtr R1,R0              CEE   SUBROUTINE   ???
         CLC   05(3,R1),LECOBOL
         JNE   P1LBADR0           NO  - BYPASS  LE   INITIALIZATION
         L     R0,LKPXADDR        SUBSTITUTE COMMON  LANG INTERFACE
         O     R0,MODE31
         ST    R0,LBSUBADR
*
***********************************************************************
*  SAVE TEMPLATE ADDRESS FROM MAIN THREAD LOOK-UP CHAIN               *
*  LOAD EXIT  IF NOT  ALREADY LOADED                                  *
***********************************************************************
P1LBADR0 ST    R5,LTLBADDR        SAVE    BUFFER ADDRESS (MAIN THREAD)
         ST    R3,DBLWORK         SAVE    BUFFER LENGTH
*
         CLC   LTFUNC,LU_EX       LOOK-UP EXIT   FUNCTION  ???
         JNE   P1LBADR1           NO - GET LOOK-UP BUFFER ADDR
*
         CLC   LTLUNAME,SPACES    EXIT SPECIFIED ???
         JE    P1LBADR1           NO  - BYPASS   LOAD
*
         TM    LBFLAGS,LBSUBPGM   EXIT ALREADY LOADED ???
         BRO   P1LBADR1           YES - BYPASS REDUNDANT  LOAD
*
         OI    LBFLAGS,LBSUBPGM
         MVC   LBSUBNAM,LTLUNAME
*
         if    cli,ltluopt,eq,ltluopty
           oi  lbflags+1,lbexitopt
         endif
*
         MVC   LBSTRTUP,LTLUPARM
         l     r15,ltluptyp
*
         LA    R9,LBSUBNAM        POINT TO SUBROUTINE NAME
         LOAD  EPLOC=(R9),ERRET=P1LBERR2
         ST    R0,LBSUBADR        SAVE  SUBROUTINE ADDRESS (31-BIT)
         ST    R0,LBSUBENT        SAVE  TRUE ENTRY POINT
*
         lgr   R1,R0              CEE   SUBROUTINE   ???
         CLC   05(3,R1),LECOBOL
         JNE   P1LBADR1           NO  - BYPASS  LE   INITIALIZATION
         L     R0,LKPXADDR        SUBSTITUTE COMMON  LANG INTERFACE
         O     R0,MODE31
         ST    R0,LBSUBADR
*
***********************************************************************
*  COPY TEMPLATE FROM MAIN THREAD TO "ES" CHAIN                       *
***********************************************************************
P1LBADR1 LA    R1,LTLUFILE        POINT TO FILE/RECORD/PATH ID'S
         CLC   LTFUNC,JOIN        JOIN  ???
         JE    P1LBADR2
         CLC   LTMAJFUN,LU_SM     LOOK-UP  ???
         JE    P1LBADR2
*
         LA    R1,LTFLDDDN        POINT TO FILE ID + RECORD ID
         CLC   LTFUNC,MUL_L       MUL   ???
         JE    *+12
         CLI   LTSUBFUN+0,C'L'    FIRST ARGUMENT REQUIRES LOOK-UP ???
         JNE   P1LB2NDB           NO  - ASSUME 2ND ATTRIBUTE  SET
*
         llgt  R9,LTFUNTBL        LOAD ADDR OF FUNCTION TABLE ENTRY
         CLI   FC_RTYP-FUNCTBL(R9),FC_RTYP12   NAMED "F1" ???
         JNE   P1LBADR2
*
P1LB2NDB ds    0h
         if    cli,ltsubfun+0,ne,c'C'   not constant as 1st operand?
           LA    R1,LTCOLDDN        NO  - MUST  BE SECOND  ARGUMENT
         endif
*
P1LBADR2 ds    0h
*
*      point to LB chain anchor in 'ES' (could be 'ET' if token view)
*
*      If this is an 'ET' (token source) then the LB chain must be
*      anchored from it's literal pool using an offset
*
         llgt  r4,LTlognv           point to NV
nv_curr  using logictbl,r4
         if TM,nv_curr.ltflag2,LTRTOKEN,z source a token?
           llgt R4,nv_curr.LTVIEWES      no, then address ES
           aghi R4,LTLBANCH-LOGICTBL     address LB chain anchor
         else
           llgt  r5,litp_base_tok    addr start of tok lit pool section
           agf   r5,f512k
           llgt R4,nv_curr.LTVIEWES  then address ET
           drop nv_curr
et_curr    using logictbl,r4
           if tm,et_curr.ltesflg1,ltlbanco,z have offset to LB chain?
             la   r0,0                Save zero address first time
             jas  r9,SAVEFULL         Save in litpool
             st   r0,et_curr.LTLBANCH Save offset in ET
             oi   et_curr.ltesflg1,ltlbanco Have now set offset
           endif
           lgf  r4,et_curr.LTLBANCH    yes, get offset in litp
           agr  r4,r5           point to LB anchor chain
           drop et_curr
         endif
         BRAS  R9,LOCLB           LOCATE BUFFER IN CURRENT "ES"
         ltgr  R5,R5              BUFFER FOUND ???
         JP    P1LBADR4           YES -  USE ALREADY ALLOCATED BUFR
*
         lgf   R3,DBLWORK         LOAD BUFFER LENGTH
*
         LA    R1,LTLUFILE        POINT TO FILE/RECORD/PATH ID'S
         CLC   LTFUNC,JOIN        JOIN  ???
         JE    P1LBADR3
         CLC   LTMAJFUN,LU_SM     LOOK-UP  ???
         JE    P1LBADR3
*
         lghi  R14,LKUPBUFR_LF_NOT_FOUND LU buffer for LF not found
         J     STDERROR
*
P1LBADR3 BRAS  R9,ALLOLKUP        ALLOCATE BUFFER FOR CURRENT "ES"
*
         LHI   R0,LBKEYOFF-LKUPBUFR  LOAD LENGTH OF PRESERVED DATA
         sgr   R3,R0              EXCLUDE PRESERVED DATA FROM COPY
*
         LA    R0,LBKEYOFF-LKUPBUFR(,R5)  LOAD "TO"   ADDRESS
         lgr   R1,R3                      LOAD "TO"   LENGTH
         lgr   R15,R3                     LOAD "FROM" LENGTH
         llgt  R14,LTLBADDR               LOAD "FROM" ADDRESS
         aghi  R14,LBKEYOFF-LKUPBUFR      SKIP  PRESERVED  DATA
         MVCL  R0,R14             COPY LOOK-UP BUFFER REMAINDER
*
         LH    R0,LKUPSTK#        COPY CURRENT STACK  ENTRY    COUNT
         AHI   R0,1
         STH   R0,LBLKSTK#
*
         MVC   LBWEPATH,ltluwpath copy workbench path id
*
         LA    R0,env_area        ENVIRONMENT DATA   ADDRESS
         ST    R0,LBENVA
         LA    R0,LBSTRTUP        START-UP    DATA   ADDRESS
         ST    R0,LBSTARTA
         LA    R0,LBEVENTA        EVENT RECORD ADDRESS - CURRENT
         ST    R0,LBRECA
         LHI   R0,0               EXTRACT     RECORD ADDRESS - CURRENT
         ST    R0,LBEXTRA
         LA    R0,LKUPKEY         LOOK-UP     KEY    ADDRESS
         ST    R0,LBKEYA
         LA    R0,LBSUBWRK        WORKAREA    ANCHOR POINTER   ADDRESS
         ST    R0,LBANCHA
         LA    R0,LBLSTRC         RETURN      CODE   ADDRESS
         ST    R0,LBRTNCA
         LA    R0,LBLSTFND        RETURN      RECORD POINTER   ADDRESS
         ST    R0,LBRPTRA
         L     R14,LTlognv
         xr    R0,R0                  FOUND COUNTER  ADDRESS
         ST    R0,LBFNDCA
         ST    R0,LBNOTCA         NOT FOUND COUNTER  ADDRESS
         OI    LBNOTCA,X'80'      MARK END-OF-PARAMETER-LIST
*
P1LBADR4 ST    R5,LTLBADDR        SAVE BUFFER ADDRESS (CURR THREAD)
         oi    ltflag1,ltlkup_alloc    and mark the lt entry
         if CLC,LTFUNC,eq,JOIN,or, JOIN                                +
               OC,SVLUJOIN,SVLUJOIN,z Parent addr 0 (prev LKLR step 1)
           ST    R5,SVLUJOIN       SAVE PARENT JOIN/path ADDRESS
           ST    R5,LBPARENT       SAVE PARENT JOIN ADDRESS (THIS path)
         endif
*
         TM    LBFLAGS,LBSUBPGM   LOOKUP EXIT  ???
         BRO   P1LBEXIT           DON'T UPDATE FLD POSITION
***********************************************************************
*  ADJUST LOOK-UP FIELD OFFSETS FOR VERSION 4.0                       *
***********************************************************************
         if CLC,LTFUNC,eq,SET_L,or,       SET   ???                    +
               CLC,LTFUNC,eq,=c'ADDL',or, ADD   ???                    +
               CLC,LTFUNC,eq,=c'SUBL',or, SUB   ???                    +
               CLC,LTFUNC,eq,MUL_L,or,    MUL   ???                    +
               CLC,LTFUNC,eq,=c'DIVL',or, DIV   ???                    +
               CLC,LTFUNC,eq,CF_LA,orif,  CF_LA ???                    +
               CLC,=C'FC',ne,LTFUNC+1,and, not xFC (SFCL or CFCL)      +
               clI,LTSUBFUN+0,ne,C'L',and,                             +
               clI,LTSUBFUN+1,eq,C'L'
           if TM,LBFLAGS+1,LBADJPOS,o UPDATE  SOURCE FIELD OFFSET ???

             LH R0,LTCOLPOS       UPDATE  SOURCE FIELD OFFSET(INCL KEY)
             AH R0,LBKEYLEN       KEY LENGTH (-1)
             AHI R0,1

             if TM,LBFLAGS,LBEFFEND+lbeffdat,o DATES PRESENT ???
               AHI R0,4
             endif

             STH R0,LTCOLPOS
           endif
         else
           if TM,LBFLAGS+1,LBADJPOS,o UPDATE SOURCE FIELD OFFSET ???
*
             LH R0,LTFLDPOS       UPDATE  SOURCE FIELD OFFSET(INCL KEY)
             AH R0,LBKEYLEN       KEY LENGTH (-1)
             AHI R0,1
*
             if TM,LBFLAGS,LBEFFEND+lbeffdat,o DATES PRESENT ???
               AHI   R0,4
             endif
             STH   R0,LTFLDPOS
           endif
         endif
*
         DROP  R5
*
P1LBEXIT lmg   R14,R5,SAVF4SAG64RS14   RESTORE PASS1 REGISTERS
         lmg   R7,R12,SAVF4SAG64RS7    except r6 (current lip pool)
         br    r14
                        EJECT
P1FUNLK  CLC   LTLUSTEP,H1        Is this top of LU path?
         BNE   P1FUNCCD           NO  - BYPASS RESET
*
         XC    SVLUJOIN,SVLUJOIN  YES - ZERO   PARENT JOIN  ADDRESS
         XC    LKUPSTKO,LKUPSTKO  RESET LOOK-UP STACK OFFSET
         XC    LKUPSTK#,LKUPSTK#  RESET LOOK-UP STACK COUNT
*
         J     P1FUNCCD
*
P1FUNWR  CLC   LTMAJFUN,WR_EX     WRITE ???
         JNE   P1FUNCCD           NO  - GENERATE CODE FOR  FUNCTION
*
         mvc   ltwrre,currre                                      pgc99

* saverowa returns r0 = offset, r5 = lit pool start address
         BRAS  R9,SAVEROWA
         ST    R0,LTWREXTO        SAVE  OFFSET
*
         aghi  R6,LTWRARLN-4      ADVANCE LITERAL POOL END
*
         lgr   R1,R0
         agr   R1,R5
         USING LTWRAREA,R1
*
         XC    LTWRWORK,LTWRWORK  WRITE EXIT PGM WORK  AREA ANCHOR
         XC    LTWRSUMA,LTWRSUMA  SUMMARY VIEW   WORK  AREA ADDRESS
         xc    LTWRCNTI,ltwrcnti  EXTRACT RECORD COUNT - INPUT
         xc    LTWRCNTO,ltwrcnto  EXTRACT RECORD COUNT - OUTPUT
*
* save offset to CT columns in logictbl for WR
*
         LHI   R0,EXSRTKEY-EXTREC COMPUTE OFFSET TO FIRST SUB-TOTAL
         llgt  R15,LTLOGNV        LOAD "NV" "LT" ROW    ADDR
         ah    R0,LTSORTLN-LOGICTBL(,R15)   LOAD SORT   KEY    LENGTH
         ah    R0,LTTITLLN-LOGICTBL(,R15)   ADD  TITLE  AREA   LENGTH
         ah    R0,LTDATALN-LOGICTBL(,R15)   ADD  DATA   AREA   LENGTH
         ST    R0,ltwr_offset_ct
*
* CALL EXIT INIT function for ASM exits only
*
         LT    R15,LTWRADDR       USER EXIT SPECIFIED ?
         JZ    P1wrnoex           no, go
         cli   LTWRPGM_TYPE+3,LTWRPGM_TYPE_ASM Assembler?
         jne   P1wrnoex
*
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
*
         MVC   gpphase,IN        INDICATE "init"  PHASE
         MVC   ERRDATA(8),LTWRNAME SAVE EXIT NAME
*
         LA    R0,LTWRPARM       POINT TO WRITE PARAMETERS
         STY   R0,GPSTARTA
         LA    R0,LTWRWORK       LOAD EXIT WRITE ANCHOR
         STY   R0,GPWORKA
*
         mvc   gp_error_buffer_len,=a(l'error_buffer) set max len
         xc    gp_error_reason,gp_error_reason     clear reason
*
         LAy   1,PARM_AREA
         BASSM R14,R15          CALL EXIT WITH INITIALIZE OPTION
*
         if (oc,gp_error_reason,gp_error_reason,nz) is the       +
*                                          code non zero ==> text
          l r15,gp_error_buffer_len get length of text
          sthy r15,error_bufl        and save in prefix
          lay r9,error_bufl
          WTO TEXT=(9),MF=(E,WTOPARM)
          l r15,gp_error_buffer_len get length of text
          ahi  r15,4
          sthy r15,error_bufl
          logit msg=error_bufl
         endif
*
         L     R15,RETNCODE
         chi   r15,8              higher than 8 is bad - can't carry on
         bnh   P1wrnoex
*
         lghi  R14,SEVERE_EXIT_ERR  Load err MESSAGE NUMBER
         J     RTNERROR             PRINT ERROR MESSAGE - STOP
*
P1wrnoex ds    0h
*
         la    r0,ltwrtkrc        Put this in
         jas   r9,saveaddr          the
         st    r0,ltwrtkrc          literal pool
*
         DROP  R1
*
***********************************************************************
*  CHECK IF OUTPUT DESTINATION IS A TOKEN (LOCATE LOOK-UP BUFFER)     *
***********************************************************************
         CLC   LTFUNC,WR_TK       WRITE TOKEN  (DT AREA) ???
         JE    *+14               YES - LOCATE CORRESPONDING "LKUPBUFR"
         CLC   LTFUNC,WR_TX       WRITE TOKEN  (EXIT)    ???
         JNE   P1FUNCCD           NO  - BYPASS SPECIAL   CALL LOGIC
*
         XC    WORKAREA,WORKAREA  CLEAR LOOK-UP BUFFER   KEY
*
         ltgf  R14,LTWR200A       VDP200 RECORD AVAILABLE(V4) ???
         JP    P1FUNWR1
*
         L     R0,LTWRFILE+L'TOKN
         J     P1FUNWR2
*
P1FUNWR1 ds    0h
        mvc errdata(8),VDP0200B_DDNAME_OUTPUT-vdp0200b_FILE_RECORD(R14)
         llgt  R14,vdp0200b_FILE_READER-vdp0200b_FILE_RECORD(,R14)
         if (ltgr,r14,r14,z) then
           lghi  R14,NO_TOKEN_READER no token reader resent
           J     RTNERROR
         endif
*
         stg   r14,dblwork2        Save address of RETK for this WRTK
next_re  using logictbl,r14                                       pgc99
         ltgf  r1,currrtkn         Get last RETK/RETX              pgc1
         jz    p1funwr1_a                                          pgc1
curr_re  using logictbl,r1                                         pgc1

         cgr   r1,r14                same RETK/RETX?              pgc99
         je    p1funwr1_b            Y: then don't add to queue   pgc99
         oc    next_re.ltreltpl,next_re.ltreltpl Done elsewhere   pgc99
         jnz   p1funwr1_spec         Y: then special              pgc99
         llgt  r15,curr_re.ltnxrtkn  Get possible next RETK/RETX  pgc99
         st    r15,next_re.ltnxrtkn  Store it in new chain entry  pgc99
         st    r14,curr_re.ltnxrtkn  Chain new one in              pgc1
         st    r14,currrtkn        Save as current RETK/RETX in ES pgc1
         mvc   next_re.ltrertkn_parent,parent_token               pgc99
         j     p1funwr1_b                                          pgc1
p1funwr1_a ds  0h                                                  pgc1
         oc    next_re.ltreltpl,next_re.ltreltpl Done elsewhere   pgc99
         jnz   p1funwr1_spec         Y: then special              pgc99
         llgt  r15,ltwrre          Get the "RE" for this "WR"    pgc102
wrre     using logictbl,r15                                      pgc102
         clc   wrre.ltfunc,re_tk   was it a "RETK"               pgc102
         je    p1funwr1_aa         y: then not interested        pgc102
         clc   wrre.ltfunc,re_tx   was it a "RETX"               pgc102
         je    p1funwr1_aa         y: then not interested        pgc102
         st    r14,wrre.ltnxrtkn   then this is the first RETK   pgc102
*                                  for this RE - ES set          pgc102
         drop  wrre                                              pgc102
p1funwr1_aa ds 0h                                                pgc102
         st    r14,frstrtkn        Save as first RETK/RETX in ES   pgc1
         st    r14,currrtkn        Save as current RETK/RETX in ES pgc1
         j     p1funwr1_b                                         pgc99
p1funwr1_spec ds 0h                                               pgc99
         stg   r7,dblwork         Save it temporarily            pgc100
p1funwr1_speclp ds 0h                                            pgc100
         llgt  r1,litp_base       Get base lit pool               pgc99
         aghi  r1,litphdrl         get start of token offsets     pgc99
         lgf   r15,next_re.ltreindx Get this RETK/RETX index      pgc99
         mhi   r15,8              Make a word offset             pgc101
         agr   r1,r15             Get the right entry             pgc99
         ltgf  r15,0(,r1)         Get this litpool offset         pgc99
         jnz   p1funwr1_spec_end  already done                    pgc99
         lgr   r15,r6             Get current address in lit pool pgc99
         sgf   r15,litp_base      Make it an offset               pgc99
         st    r15,0(,R1)         save in RETK/RETX offset table  pgc99
         llgt  r15,next_re.ltrees Get the ET for this RETK       pgc100
         llgt  r0,lteslpad-logictbl(,r15) Get litpool for ET     pgc100
         lgf   r1,next_re.ltreltpl Get the lit pool length       pgc100
         lgr   r7,r1                                             pgc100
         mvcl  r6,r0              copy literal pool              pgc100
         ltgf  r14,next_re.ltnxrtkn Get possible next RETK/RETX   pgc99
         jnz   p1funwr1_speclp    and go process this one         pgc99
p1funwr1_spec_end ds 0h                                          pgc100
         lg    r7,dblwork         Save it temporarily            pgc100
p1funwr1_b ds  0h                                                  pgc1
         drop  next_re,curr_re                                    pgc99
         L     R0,LTTOKNLR-LOGICTBL(,R14)
*
P1FUNWR2 MVC   WORKAREA(L'TOKN),TOKN     BUILD LOOK-UP BUFFER KEY
         ST    R0,WORKAREA+L'TOKN
         ST    R0,WORKAREA+L'LTFILEID
*
         stmg  R3,R5,SAVF4SAG64RS3
*
         LA    R1,WORKAREA
         llgt  R4,LTlognv         POINT  TO  CHAIN ANCHOR
         llgt  R4,LTVIEWES-LOGICTBL(,R4)
         aghi  R4,LTLBANCH-LOGICTBL
         BRAS  R9,LOCLB           LOCATE BUFFER IN CURRENT "ES"
         ltgr  R5,R5              BUFFER FOUND ???
         JP    P1FUNWR4           YES -  USE ALREADY ALLOCATED BUFR
*
         lghi  R3,LBPREFLN+8192   ASSUME "WRTX" (TOKEN  WRITER EXIT)
         CLC   LTFUNC,WR_TX
         JE    P1FUNWR3
         CLC   LTFUNC,WR_IN       "COPY INPUT"   ???
         BRE   P1FUNWR3           YES - USE SAME DEFAULT LEN
*
         llgt  R15,LTlognv        LOAD "NV" "LT" ROW    ADDR
         lgh   R1,LTSORTLN-LOGICTBL(,R15)   LOAD SORT   KEY    LENGTH
         ah    R0,LTTITLLN-LOGICTBL(,R15)   ADD  TITLE  AREA   LENGTH
         ah    R1,LTDATALN-LOGICTBL(,R15)   ADD  DATA   AREA   LENGTH
         BRNP  P1FUNWR3
*
         LA    R3,LBPREFLN(,R1)             ADD  BUFFER PREFIX LENGTH
*
P1FUNWR3 LA    R1,WORKAREA        POINT TO FILE/RECORD/PATH    ID'S
         BRAS  R9,ALLOLKUP        ALLOCATE BUFFER FOR CURRENT "ES"
         USING LKUPBUFR,R5
*
         llgt  R15,LTlognv        LOAD "NV" "LT" ROW    ADDR
         LH    R0,LTSORTLN-LOGICTBL(,R15)   LOAD SORT   KEY    LENGTH
         BCTR  R0,0
         STH   R0,LBKEYLEN
         AHI   R0,1
         AH    R0,LTTITLLN-LOGICTBL(,R15)   LOAD TITLE  AREA   LENGTH
         AH    R0,LTDATALN-LOGICTBL(,R15)   LOAD DATA   AREA   LENGTH
         STH   R0,LBRECLEN
         lg    r15,dblwork2       Get the RETK for this WRTK
lretk    using logictbl,r15
         st    r0,lretk.ltrerecl
         drop  lretk
*
         LA    R0,LBDATA
*??WRTX  CLC   LTFUNC,WR_TX       TOKEN WRITE EXIT ???
*??WRTX  JNE   *+12
*??WRTX  OI    LBFLAGS+1,LBWRTX
*??WRTX  aghi  R0,4               SKIP "RDW"
*
         stg   R0,LBLSTFND
*
         LA    R0,env_area       ENVIRONMENT DATA   ADDRESS
         ST    R0,LBENVA
         LA    R0,LBSTRTUP        START-UP    DATA   ADDRESS
         ST    R0,LBSTARTA
         LA    R0,LBEVENTA        EVENT       RECORD ADDRESS - CURRENT
         ST    R0,LBRECA
         LHI   R0,0               EXTRACT     RECORD ADDRESS - CURRENT
         ST    R0,LBEXTRA
         LA    R0,LKUPKEY         LOOK-UP     KEY    ADDRESS
         ST    R0,LBKEYA
         LA    R0,LBSUBWRK        WORKAREA    ANCHOR POINTER   ADDRESS
         ST    R0,LBANCHA
         LA    R0,LBLSTRC         RETURN      CODE   ADDRESS
         ST    R0,LBRTNCA
         LA    R0,LBLSTFND        RETURN      RECORD POINTER   ADDRESS
         ST    R0,LBRPTRA
         L     R14,LTlognv
         xgr   R0,R0                   FOUND COUNTER ADDRESS
         ST    R0,LBFNDCA
         ST    R0,LBNOTCA         NOT  FOUND COUNTER ADDRESS
         OI    LBNOTCA,X'80'      MARK END-OF-PARAMETER-LIST
*
P1FUNWR4 lgr   R0,R5              SAVE LOOK-UP BUFFER   ADDR
*
         lmg   R3,R5,SAVF4SAG64RS3
*
         BRAS  R9,SAVEADDR
         ST    R0,LTWRLUBO        SAVE OFFSET
*
         DROP  R5
***********************************************************************
*  CONNECT LOGIC TABLE ROWS TO "NV" CODE SEGMENT                      *
***********************************************************************
P1FUNCCD ds    0h
*
         ST    R3,LTCODSEG        SAVE  CODE  ADDRESS FOR THIS  LT ROW
*
***********************************************************************
*  RESET CURRENT LOOK-UP STACK OFFSET IF IT'S A "JOIN" FUNCTION       *
***********************************************************************
         CLC   LTFUNC,JOIN        JOIN  ???
         JNE   *+16
         XC    LKUPSTKO,LKUPSTKO  RESET LOOK-UP STACK OFFSET
         XC    LKUPSTK#,LKUPSTK#  RESET LOOK-UP STACK COUNT
*
***********************************************************************
*  "PROMOTE" GOTO ROW NUMBERS IF TARGET IS GOTO                       *
***********************************************************************
         llgt  R14,LTTRUE         BRANCH TO "GOTO" ???
         CLC   LTFUNC-LOGICTBL(L'LTFUNC,R14),GOTO
         JNE   P1PROMFB
*
         lgf   R14,LTTRUE-LOGICTBL(,R14) "GOTO" TARGET ROW NUMBER
         bctgr R14,0              SUBTRACT ONE
         sllg  R14,r14,2          TIMES LENGTH OF EACH ENTRY
         agf   R14,LTROWADR       ADD   BASE ADDRESS  TO  OFFSET
         llgt  R14,0(,R14)        LOAD "GOTO" TARGET ROW  ADDRESS
         ST    R14,LTTRUE         TRANSFER "GOTO" ROW TO EARLIER FUNCT
*
P1PROMFB ltgf  R14,LTFALSE        BRANCH TO "GOTO" ???
         JNP   P1TRACE
         CLC   LTFUNC-LOGICTBL(L'LTFUNC,R14),GOTO
         JNE   P1TRACE
*
         lgf   R14,LTTRUE-LOGICTBL(,R14)  "GOTO" TARGET ROW NUMBER
         bctgr R14,0              SUBTRACT ONE
         sllg  R14,r14,2          TIMES LENGTH OF EACH  ENTRY
         agf   R14,LTROWADR       ADD   BASE ADDRESS  TO  OFFSET
         llgt  R14,0(,R14)        LOAD "GOTO" TARGET ROW  ADDRESS
         ST    R14,LTFALSE        TRANSFER "GOTO" ROW TO EARLIER FUNCT
*
                        EJECT
***********************************************************************
*  INSERT BRANCH TO TRACE FUNCTION IF TRACE OPTION ENABLED            *
***********************************************************************
P1TRACE  llgt  R1,EXECDADR
         USING EXECDATA,R1
         do ,
           if CLI,EXECTRAC,eq,C'Y'    TRACE OPTION SPECIFIED ??
             DROP  R1
*
             LH    R0,LTGENLEN                ANY GENERATED CODE ???
             if LTR,R0,R0,P,and,  any generated code?                  +
               clc,ltmajfun,ne,es,and,   and not ES                    +
               clc,ltmajfun,ne,et        and not ET

               if CLI,VIEWTRAC,eq,C'Y'   SPECIFIC TRACE   OPTION  ???

                doexit (ltgf,R14,NVLOGTBL-NVPROLOG(,R2),np) get NV adr
                doexit (ltgf,R14,LTPARMTB-LOGICTBL(,R14),np) TRACE?
               endif

               if TM,LTFLAGS,LTOMITGO,NO  OMITTED GOTO CODE not set ???
*
                 llgt R1,TRACADDR LOAD TRACE  CODE  ADDRESS
                 MVC 0(4,R3),0(R1) COPY BRANCH INSTRUCTION
                 aghi R3,4        ADVANCE     STARTING CODE POSITION
                 aghi R4,4        ADVANCE     ENDING   CODE POSITION
*
                 mvc NVTRACE-NVPROLOG(,R2),0(r1) TURN ON 'NV' TRACE
               endif
             endif
           endif
         enddo
                        SPACE 3
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        R9:  FUNCTBL BASE REGISTER                                   *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
P1LKUPRE llgt  R9,LTFUNTBL        LOAD ADDRESS  OF  FUNCTION TBL ENTRY
         USING FUNCTBL,R9
*
         LH    R15,FCCODELN       LOAD LENGTH OF   MODEL CODE
         LTR   R15,R15            NULL LENGTH ???
         JNP   P1NEXT             YES -  SKIP CODE GENERATION
         DROP  R9
*
         if TM,LTFLAGS,LTLKUPRE,o LOOK-UP PREFIX REQUIRED ???
*
           MVC 0(L'LKUPPREF,R3),LKUPPREF INSERT  LOOK-UP PREFIX
*
           BRAS R9,SAVELBA        SAVE   LOOK-UP BUFFER ADDRESS
           ST  R0,LTLUBOFF        OVERLAY  ADDR  WITH OFFSET
           oi  ltflag1,ltlkup_offset  remember offset set now
           STH R0,2(,R3)          STORE   LOW   ORDER 3 NIBBLES
           NI  2(R3),X'0F'
           OI  2(R3),X'20'
           SRL R0,12
           STC R0,4(,R3)          STORE   HIGH  ORDER 2 NIBBLES
*
           aghi R3,L'LKUPPREF     ADVANCE CODE BASE  ADDRESS
           aghi R4,L'LKUPPREF     ADVANCE CODE END   ADDRESS

           if clc,=c'LL',eq,ltsubfun,and,  do we need a second lookup? +
               clc,ltmajfun,ne,mul_l       but NOT MULL
*            need a second lookup buffer here so temporarily remove
*            the first L to fool the allocation routine to use
*            ltcolddn
             mvi ltsubfun,c' '        blank out first L
             mvc dblwork4(l'ltlbaddr),ltlbaddr save this buffer
*            now call the allocation routine again
             bras r14,p1look_up_alloc get the buffer
*            restore the missing L
             mvi ltsubfun,c'L'
*            now save the address in the literal pool and
*            insert the code to get the second buffer
             bras r9,savelba
             mvc ltlbaddr,dblwork4 restore this buffer
             mvc 0(l'lkup_number2,r3),lkup_number2
             sth r0,2(,r3)        store   low   order 3 nibbles
             ni 2(r3),x'0f'
             oi 2(r3),x'20'
             srl r0,12
             stc r0,4(,r3)        store   high  order 2 nibbles
             aghi R3,L'LKUP_number2 ADVANCE CODE BASE  ADDRESS
             aghi R4,L'LKUP_number2 ADVANCE CODE END   ADDRESS
           endif
         endif

         if cli,ltsubfun,eq,c'P',or,  Do we need R3 loaded with the    +
               cli,ltsubfun+1,eq,c'P'    previous record address ?
*
           MVC 0(L'LOADPREV,R3),loadprev

           aghi R3,L'loadprev     ADVANCE CODE BASE  ADDRESS
           aghi R4,L'loadprev     ADVANCE CODE END   ADDRESS
         endif
         ST    R2,LTVIEWNV        REPLACE LT ROW ADDR WITH NV CODE ADDR
*
***********************************************************************
*  COPY TEMPLATE MACHINE CODE FROM "MR95" INTO CODE SEGMENT           *
***********************************************************************
         TM    LTFLAGS,LTOMITGO   OMITTED GOTO CODE
         BRO   P1NEXT
*
         llgt  R9,LTFUNTBL        LOAD ADDRESS  OF  FUNCTION TBL ENTRY
         USING FUNCTBL,R9
*
         llgt  R14,FCMODELA       LOAD ADDRESS OF MODEL CODE
         BCTR  R15,0              DECREMENT LENGTH  FOR "EX" INSTR
         EX    R15,MVCMODEL       COPY MODEL  CODE
*
         llgt  R14,FCRELOCA       LOAD RELOCATION   TABLE  ADDRESS
         DROP  R9
*
         LA    R4,1(R4,R15)       SAVE END-OF-CODE  POSITION
*
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=(REGS),ID=135
*
***********************************************************************
*  PROCESS RELOCATABLE SECTIONS OF MACHINE CODE (INCL SUBSTITUTIONS)  *
***********************************************************************
P1RELOLP CLI   0(R14),X'FF'       END-OF-TABLE  ???
         JE    P1NEXT             YES -  ADVANCE TO NEXT LOGIC ROW
*
         xgr   R1,R1              LOAD RELOCATION VALUE OFFSET
         IC    R1,1(,R14)
         agr   R1,R3              ADD  CODE  BASE TO    OFFSET
*
         xgr   R0,R0              LOAD RELOCATION VALUE TYPE CODE
         IC    R0,0(,R14)
         sllg  R0,r0,2            MULTIPLY  BY  4 (BRANCH TABLE INDEX)
         LARL  R15,P1RELOTB
         agr   R15,R0
         BR    R15
P1RELOTB DC    A(0)
         J     P1SRCLN       01 - SOURCE  FIELD  LENGTH
         J     P1SRCLNL      02 - SOURCE  FIELD  LENGTH  (LEFT  1/2)
         J     P1SRCLNR      03 - SOURCE  FIELD  LENGTH  (RIGHT 1/2)
         J     p1relonx      04 - SOURCE  FIELD  OFFSET
*                               (no longer used so deleted)
         J     P1SRCLOF      05 - LONG    SOURCE FIELD   OFFSET
         J     P1TGTLN       06 - TARGET  FIELD  LENGTH
         J     P1TGTLNL      07 - TARGET  FIELD  LENGTH  (LEFT  1/2)
         J     P1TGTLNR      08 - TARGET  FIELD  LENGTH  (RIGHT 1/2)
         J     p1V1len       09 - Value   1      length
         J     P1V1OFF       10 - VALUE   1      OFFSET
         J     P1V2OFF       11 - VALUE   2      OFFSET
         J     P1TRUEO       12 - TRUE    BRANCH DISPLACEMENT (OPT )
         J     P1TRUEM       13 - TRUE    BRANCH DISPLACEMENT (MAND)
         J     P1FALSEO      14 - FALSE   BRANCH DISPLACEMENT (OPT )
         J     P1FALSEM      15 - FALSE   BRANCH DISPLACEMENT (MAND)
         J     P1RELOPR      16 - RELATIONAL COMPARISON  CONDITION CODE
         J     P1RELOP2      17 - RELATIONAL COMPARISON  CONDITION CODE
         J     P1LTROFF      18 - LOGIC  TBL ROW ADDRESS OFFSET
         J     P1LBAOFF      19 - LOOK-UP BUFFER ADDRESS OFFSET
         J     P1LBAOF2      20 - LOOK-UP BUFFER TWO     OFFSET
         J     P1COLNO       21 - "CT"    COLUMN NUMBER  OFFSET
         J     p1relonx      22 - SHIFT && ROUND DECIMAL PLACES
*                               (no longer used so deleted)
         J     P1SRPCT       23 - SHIFT && ROUND "CT" COL DEC PLACES
         J     P1LSTBYT      24 - LAST/RIGHTMOST TARGET  BYTE    OFFSET
         J     P1BYTMSS      25 - "ICM"    BYTES MOVED   MASK
         J     P1BYTMSK      26 - "STCM"   BYTES MOVED   MASK
         J     P1LRID        27 - LOGICAL RECORD ID
         J     P1KEYLEN      28 - LOOK-UP        KEY     LENGTH
         J     P1TTLOFF      29 - TITLE  KEY OFFSET
         J     P1LVLOFF      30 - HIERARCHICAL   LOOK-UP LEVEL
         J     P1ACCOFF      31 - ACCUMULATOR    ADDRESS OFFSET
         J     P1ACCOF2      32 - ACCUMULATOR  2 ADDRESS OFFSET
         J     P1ACCVAL      33 - ACCUMULATOR    VALUE   OFFSET
         J     P1ACCLEN      34 - ACCUMULATOR    LENGTH
         J     P1TRTBL       35 - NUMERIC TRANSLATE TEST TABLE
         J     P1SUBCTR      36 - SUBSTRING LOOP  COUNTER
         J     P1SUBLEN      37 - SUBSTRING LENGTH
         J     P1SUBOFF      38 - SUBSTRING STARTING OFFSET
         J     P1SUBVAL      39 - SUBSTRING VALUE    OFFSET
         J     P1SDNLN       40 - SORT   DESCENDING  LENGTH (TARGET)
         J     P1subctrcx    41 - substr counter for SFCx
         J     P1TKNLEN      42 - SORT   KEY LENGTH (TOKEN   WRITER)
         J     P1subctrxc    43 - substr counter for SFxC
         J     P1CALLVW      44 - CALL  VIEW MACHINE CODE
         J     P1relonx      45 - JUSTIFIED  LENGTH
*                               (no longer used so deleted)
         J     P1JUSOFF      46 - JUSTIFIED  OFFSET
         J     P1CTACUM      47 - "CT" COLUMN NUM FOR ACCUMULATOR VALUE
         J     P1relonx      48 - LAST/RIGHTMOST  SOURCE  BYTE   OFFSET
*                               (no longer used so deleted)
         J     P1SRPSRC      49 - skip conversion code if no shifting
         J     P1SDNSLN      50 - SORT DESCENDING LENGTH (SOURCE)
         J     P1relonx      51 - SORT DESCENDING OFFSET (SOURCE)
*                               (no longer used so logically deleted)
         J     P1TGTLOF      52 - LONG TARGET     FIELD   OFFSET
         J     P1LKPSTK      53 - LOOK-UP   STACK OFFSET
         J     P1SFTDIG      54 - SET "SHIFTDIG" FOR LRGNUM ARITH
         J     P1CTACUM_12   55 - "CT" COLUMN NUM FOR ACCUM VALUE  @02I
         J     P1calldl96    56 - set correct BAS to select source
         J     P1calldl96r   57 - set correct BAS (reversed fields)
         J     P1calldl96n   58 - set correct BAS normal
         J     P1relonx      59 - Align 2 pack numbers via dec pt.
*                               (no longer used so deleted)
         j     p1dfpexp      60 - Create DFP biased exponent
         j     p1dfpopt      61 - optimise DFP loads
         j     p1dfpexp_s    62 - Create DFP biased exp source
         J     P1V1OFFr      63 - VALUE   1      OFFSET
         J     P1V2OFFr      64 - VALUE   2      OFFSET
         J     P1TGTLOFr     65 - LONG TARGET     FIELD   OFFSET
         J     P1SRCLOFr     66 - LONG    SOURCE FIELD   OFFSET
         J     P1calldl96n_r 67 - set correct BAS normal
         J     p1SRCHRT      68 - Lookup search routine address
         J     p1cmpdt1      69 - Adjust compare for CFxC with date
         J     p1cmpdt2      70 - Adjust compare for CFCx with date
         ASSERT (csmaxval*4),EQ,(p1srcln-(p1relotb+l'p1relotb))
**** DO NOT move the p1SRCLN label - ASSERT above is reliant onit
P1SRCLN  LH    R15,LTFLDLEN       LOAD   SOURCE FIELD LENGTH (-1)
         STC   R15,0(,R1)
         J     P1RELONX
*
P1SRCLNL LH    R15,LTFLDLEN       LOAD   SOURCE FIELD LENGTH (-1)
         SLL   R15,28
         SRL   R15,24
         STC   R15,DBLWORK
         OC    0(1,R1),DBLWORK
         J     P1RELONX
*
P1SRCLNR LH    R15,LTFLDLEN       LOAD   SOURCE FIELD LENGTH (-1)
         SLL   R15,28
         SRL   R15,28
         STC   R15,DBLWORK
         OC    0(1,R1),DBLWORK
         J     P1RELONX
*
P1SRCLOF LH    R15,LTFLDPOS       LOAD   SOURCE FIELD POSITION
         STH   R15,0(,R1)         STORE  LOW    ORDER 3 NIBBLES
         l     r10,ltfldfil        get type in case it is an X opnd
         NI    0(R1),X'0F'        make sure register part is 0
         SRL   R15,12
         STC   R15,2(,R1)         STORE  HIGH   ORDER 2 NIBBLES
         j     p1basesr
*
P1SRCLOFr LH   R15,LTFLDPOS       LOAD   SOURCE FIELD POSITION
         STH   R15,0(,R1)         STORE  LOW    ORDER 3 NIBBLES
         l     r10,ltfldfil        get type in case it is an X opnd
         NI    0(R1),X'0F'        make sure register part is 0
         SRL   R15,12
         STC   R15,2(,R1)         STORE  HIGH   ORDER 2 NIBBLES
         j     p1basetg
*
P1TGTLN  LH    R15,LTCOLLEN       LOAD   TARGET FIELD LENGTH (-1)
         STC   R15,0(,R1)
         J     P1RELONX
*
P1TGTLNL LH    R15,LTCOLLEN       LOAD   TARGET FIELD LENGTH (-1)
         SLL   R15,28
         SRL   R15,24
         STC   R15,DBLWORK
         OC    0(1,R1),DBLWORK
         J     P1RELONX
*
P1TGTLNR LH    R15,LTCOLLEN       LOAD   TARGET FIELD LENGTH (-1)
         SLL   R15,28
         SRL   R15,28
         STC   R15,DBLWORK
         OC    0(1,R1),DBLWORK
         J     P1RELONX
*
P1V1LEN  lgh   r0,ltv1len         get the length of needle
         bctr  R0,0               machine length
         STC   R0,0(,R1)          STORE into compare
         J     P1RELONX
*
P1V1OFF  BRAS  R9,COPYVAL1        COPY  LOW  VALUE TO LITERAL POOL
         STH   R0,0(,R1)          STORE LOW   ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         SRL   R0,12
         STC   R0,2(,R1)          STORE HIGH  ORDER 2 NIBBLES
         J     P1BASETG
*
P1V1OFFr BRAS  R9,COPYVAL1        COPY  LOW  VALUE TO LITERAL POOL
         STH   R0,0(,R1)          STORE LOW   ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         SRL   R0,12
         STC   R0,2(,R1)          STORE HIGH  ORDER 2 NIBBLES
         J     P1BASEsr
*
*  COPY HIGH VALUE TO LITERAL POOL
P1V2OFF  lgh   R15,LTV2LEN        LOAD  VALUE LENGTH  (-1)
         bctgr R15,0
         LA    R0,1(R6,R15)       COMPUTE NEW LITERAL POOL LENGTH
         c     R0,LITPOOLM        CHECK FOR LITERAL POOL OVERFLOW ???
         JNH   COPYHI1            BRANCH IF OKAY
*
         lghi  R14,LITP_OVERFLOW  INDICATE OVERFLOW
         llgt  R13,THRDMAIN       USE MAIN DRIVER'S THREAD
         J     STDERROR
*
COPYHI1  lgr   R0,R6              COMPUTE LITERAL OFFSET
         sgr   R0,R5
         lgh   R9,LTV1LEN
         LA    R9,LTVALUES(r9)
         EX    R15,COPYHIV        COPY HIGH VALUE
         LA    R6,1(R6,R15)       ADVANCE CURRENT LITERAL POOL POS
*
         STH   R0,0(,R1)          STORE  LOW    ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         SRL   R0,12
         STC   R0,2(,R1)          STORE  HIGH   ORDER 2 NIBBLES
         J     P1BASETG
*
*  COPY HIGH VALUE TO LITERAL POOL
P1V2OFFr lgh   R15,LTV2LEN        LOAD  VALUE LENGTH  (-1)
         bctgr R15,0
         LA    R0,1(R6,R15)       COMPUTE NEW LITERAL POOL LENGTH
         if c,R0,gt,litpoolm     CHECK FOR LITERAL POOL OVERFLOW ???
           lghi R14,LITP_OVERFLOW INDICATE OVERFLOW
           llgt R13,THRDMAIN      USE MAIN DRIVER'S THREAD
           J   STDERROR
         endif
         lgr   R0,R6              COMPUTE LITERAL OFFSET
         sgr   R0,R5
         lgh   R9,LTV1LEN
         LA    R9,LTVALUES(r9)
         EX    R15,COPYHIV        COPY HIGH VALUE
         LA    R6,1(R6,R15)       ADVANCE CURRENT LITERAL POOL POS
*
         STH   R0,0(,R1)          STORE  LOW    ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         SRL   R0,12
         STC   R0,2(,R1)          STORE  HIGH   ORDER 2 NIBBLES
         J     P1BASEsr
*
P1TRUEO  ds    0h                 TRUE   BRANCH ALREADY   OMITTED ???
         TM    LTFLAGS,LTOMITTB   TRUE   BRANCH ALREADY   OMITTED ???
         BRO   P1RELONX           YES -  DON'T  REPROCESS
*
P1TRUEM  LA    R0,mc_disp_l(,r1)  TRUE   BRANCH INSTR  AT END-OF-CODE
         cgr   R0,R4
         JNE   P1RELONX           NO  -  DON'T  OMIT TRUE BRANCH INSTR
*
         lgr   R0,R7              LOAD   NEXT   ROW  ADDR
         lgh   R15,0(,R7)
         agr   R0,r15
         C     R0,LTTRUE          BRANCH TO NXT ROW  ???
         JNE   P1RELONX           NO  -  CANNOT REMOVE
*
         aghi  R4,-l'mc_jump      REMOVE BRANCH (OVERWRITE IT)
         OI    LTFLAGS,LTOMITTB   INDICATE TRUE BRANCH OMITTED
         J     P1RELONX
*
P1FALSEO ds    0h
         LA    R0,mc_disp_l(,r1)  FALSE  BRANCH INSTR  AT  END-OF-CODE
         cgr   R0,R4
         JE    P1FALSE5           YES -  MAY BE POSSIBLE   TO OMIT
*
         CLI   2(R14),CSTRUEO     IS NEXT RELOCATION A TRUE BRANCH ???
         JNE   P1RELONX           NO  -  THEN CANNOT OVERWRITE BOTH
*
         LA    R0,mc_disp_l+l'mc_jump(,r1)                             +
                                  TRUE   BRANCH INSTR  AT  END-OF-CODE
         cgr   R0,R4
         JNE   P1RELONX           NO  -  THEN CANNOT OVERWRITE BOTH
*
         lgr   R15,R7             LOAD   NEXT   ROW    ADDR
         lgh   R0,0(,R7)
         agr   R15,r0
         TM    LTFLAGS-LOGICTBL(R15),LTOMITGO   NEXT   ROW   OMITTED ?
         JNO   P1false3                         NO  -  USE   IT
         lgh   r0,0(,R15)                      YES -  CHECK FOLLOWING
         agr   R15,r0                           YES -  CHECK FOLLOWING
P1false3 C     R15,LTTRUE         TRUE   BRANCH TO NXT ROW   ???
         JNE   P1RELONX           NO  -  CANNOT REMOVE
*
         aghi  R4,-l'mc_jump      REMOVE   TRUE BRANCH (OVERWRITE IT)
         OI    LTFLAGS,LTOMITTB   INDICATE TRUE BRANCH OMITTED
*
P1FALSE5 lgh   R1,0(,r7)          LOAD   NEXT   ROW ADDR
         la    R0,0(r1,R7)
         C     R0,LTFALSE         BRANCH TO NXT ROW ???
         JNE   P1RELONX           NO  -  CANNOT BE  REMOVED
*
         aghi  R4,-l'mc_jump      REMOVE BRANCH (OVERWRITE IT)
         OI    LTFLAGS,LTOMITFB   INDICATE FLSE BRANCH OMITTED
         J     P1RELONX
*
P1FALSEM J     P1RELONX
                        EJECT
P1RELOP2 LA    R9,LTVVROPR
         J     *+8
P1RELOPR LA    R9,LTRELOPR
*
         NI    0(R1),X'0F'        CLEAR  CONDITION  CODE
*
P1ROP_EQ CLC   0(L'EQ,R9),EQ      EQUAL  ???
         JNE   P1ROP_NE           NO  -  CHECK NEXT RELATIONAL OPER
         OI    0(R1),X'70'        FALSE  CODE
         J     P1RELONX
*
P1ROP_NE CLC   0(L'EQ,R9),NE      NOT    EQUAL ???
         JNE   P1ROP_LT           NO  -  CHECK NEXT RELATIONAL OPER
         OI    0(R1),X'80'        FALSE  CODE
         J     P1RELONX
*
P1ROP_LT CLC   0(L'EQ,R9),LT      LESS   THAN  ???
         JNE   P1ROP_LE           NO  -  CHECK NEXT RELATIONAL OPER
         OI    0(R1),X'B0'        FALSE  CODE (BNL)
         J     P1RELONX
*
P1ROP_LE CLC   0(L'EQ,R9),LE      LESS   THAN  OR   EQUAL
         JNE   P1ROP_GT           NO  -  CHECK NEXT RELATIONAL OPER
         OI    0(R1),X'20'        FALSE  CODE (BH)
         J     P1RELONX
*
P1ROP_GT CLC   0(L'EQ,R9),GT      GREATER THAN ???
         JNE   P1ROP_GE           NO  -  CHECK NEXT RELATIONAL OPER
         OI    0(R1),X'D0'        FALSE  CODE (BNH)
         J     P1RELONX
*
P1ROP_GE CLC   0(L'EQ,R9),GE      GREATER THAN OR   EQUAL  ???
         JNE   P1ROP_BW           NO  -  CHECK NEXT REALTIONAL OPER
         OI    0(R1),X'40'        FALSE  CODE (BL)
         J     P1RELONX
*
P1ROP_BW CLC   0(L'EQ,R9),BW      BEGINS WITH  ???
         JNE   P1ROP_EW           NO  -  CHECK NEXT REALTIONAL OPER
         OI    0(R1),X'70'        FALSE  CODE (NE)
         J     P1RELONX
*
P1ROP_EW CLC   0(L'EQ,R9),EW      ENDS   WITH  ???
         JNE   P1RELONX           NO  -  LEAVE AS   "NOP"
         OI    0(R1),X'70'        FALSE  CODE (NE)
         J     P1RELONX
*
P1LTROFF BRAS  R9,SAVEROWA        SAVE LOGIC TBL ROW  ADDRESS
         STH   R0,0(,R1)          STORE  LOW    ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         OI    0(R1),X'20'
         SRL   R0,12
         STC   R0,2(,R1)          STORE  HIGH   ORDER 2 NIBBLES
         J     P1RELONX
*
P1LBAOFF BRAS  R9,SAVELBA         SAVE LOOK-UP BUFFER ADDRESS
         ST    R0,LTLUBOFF        OVERLAY  ADDR  WITH OFFSET
         oi    ltflag1,ltlkup_offset  remember offset set now
         STH   R0,0(,R1)          STORE  LOW    ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         OI    0(R1),X'20'
         SRL   R0,12
         STC   R0,2(,R1)          STORE  HIGH   ORDER 2 NIBBLES
         J     P1RELONX
*
P1LBAOF2 stmg  R14,R12,SAVF4SAG64RS14    SAVE PASS1  REGISTERS
* would this ever be called for a function in a token?
         LA    R1,LTCOLDDN        LOAD   SEARCH      ARGUMENT
         llgt  R4,LTVIEWNV        POINT  TO   CHAIN  ANCHOR   IN "ES"
         llgt  R4,NVLOGTBL-NVPROLOG(,R4)
         llgt  R4,LTVIEWES-LOGICTBL(,R4)
         aghi  R4,LTLBANCH-LOGICTBL
         BRAS  R9,LOCLB           LOCATE BUFFER
         stg   R5,SAVF4SAG64RS0
*
         lmg   R14,R12,SAVF4SAG64RS14    LOAD PASS1  REGISTERS
*
         ltgr  R15,R0             BUFFER FOUND ???
         JP    P1LBA_05           YES -  COPY BUFFER TO CURRENT "ES"
*
         DC    H'0'
*
P1LBA_05 TM    LBFLAGS-LKUPBUFR(R15),LBSUBPGM   LOOKUP EXIT  ???
         BRO   P1LBA_06           DON'T UPDATE FLD POSITION
*
         LH    R9,LTCOLPOS        UPDATE LOOK-UP COLUMN POSITION
         AH    R9,LBKEYLEN-LKUPBUFR(,R15)
         AHI   R9,1
         STH   R9,LTCOLPOS
*
P1LBA_06 BRAS  R9,SAVEADDR        SAVE LOOK-UP BUFFER ADDRESS TWO
         STH   R0,0(,R1)          STORE  LOW    ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         OI    0(R1),X'20'
         SRL   R0,12
         STC   R0,2(,R1)          STORE  HIGH   ORDER 2 NIBBLES
         J     P1RELONX
*
P1COLNO  BRAS  R9,SAVECOL#        SAVE  "CT"   COLUMN NUMBER
p1accum  STH   R0,0(,R1)          STORE  LOW    ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         OI    0(R1),X'20'
         SRL   R0,12
         STC   R0,2(,R1)          STORE  HIGH   ORDER 2 NIBBLES
*
         llgt  R1,LTVIEWNV        LOAD  "NV"  CODE  SEGMENT ADDRESS
         llgt  R1,NVLOGTBL-NVPROLOG(,R1) LOAD "NV"  ROW     ADDRESS
         TM    LTFLAG2-LOGICTBL(R1),LTEXTSUM  EXTRACT  TIME SUMMARY ???
         JNO   P1RELONX
*
         stmg  R14,R12,SAVF4SAG64RS14    SAVE PASS1 REGISTERS
*
         lgr   R4,R1
         ltgf  R1,LTSUBOPT-LOGICTBL(,R4) LOAD   SUBTOTAL OPTIONS  ADDR
         JP    P1COL04                   ALLOCATED - USE EXISTING AREA
*
         LH    R0,LTMAXCOL-LOGICTBL(,R4)
         GETMAIN RU,LV=(0),LOC=(ANY)
*
         ST    R1,LTSUBOPT-LOGICTBL(,R4)
*
         lgr   R0,R1
         LH    R1,LTMAXCOL-LOGICTBL(,R4)
         xgr   R14,R14
         LHI   R15,SUBTOT
         SLL   R15,24
         MVCL  R0,R14
*
P1COL04  lgf   R0,LTVIEW#         LOCATE VIEW COLUMN RECORD (2000)
         ltgf  R1,LTCOLID         load column and set cc
         BRNP  P1COL08            and exit now if not +ve
         llgt  R14,LTVIEWNV       load nv pointer - two step load
         llgt  R14,NVLOGTBL-nvprolog(,R14) here in pass1
         BRAS  R9,LOCCOLM
         USING vdp2000b_COLUMN_RECORD,R15
         ltgr  R15,R15
         JNP   P1COL08
*
         lgh   R1,LTCOLNO
         bctgr R1,0
         agf   R1,LTSUBOPT-LOGICTBL(,R4)
         MVC   0(1,R1),vdp2000b_SUBTOTAL_TYPE_ID+3
         DROP  R15
*
P1COL08  lmg   R14,R12,SAVF4SAG64RS14    LOAD PASS1  REGISTERS
*
         J     P1RELONX
*
P1SRPSRC LLC   R15,LTNDEC        COMPUTE SHIFT AMT TO ALIGN DEC POINT
         LLC   R0,LTCOLDEC
         SR    R15,R0
         LLC   R0,LTCOLRND
         SR    R15,R0
         LLC   R0,LTRNDFAC
         SR    R15,R0                                              @04C
*
         if (LTR,R15,R15,z)       zero - means that we do not need to  +
                                  quantise the data so force a branch
           OI    0(R1),X'F0'        make the branch unconditional
         endif
         J     P1RELONX
*
P1DFPEXP LLC   R15,LTCOLDEC       COMPUTE DFP Biased exponent
         LLC   R0,LTCOLRND
         SR    R15,R0
         lcr   r15,r15            invert so it can be used as a power
*
         AHI   R15,6176           Add DFPBias
         sth   r15,0(0,r1)        and update the instruction
*
         J     P1RELONX
*
P1DFPEXP_S LLC   R15,LTndec       COMPUTE DFP Biased exp for source
         LLC   R0,LTRNDFAC                                         @04C
         SR    R15,R0             this will have the no of dec places
         lcr   r15,r15            invert so it can be used as a power
*
         AHI   R15,6176           Add DFPBias
         sth   r15,0(0,r1)        and update the instruction
*
         J     P1RELONX
*
P1DFPopt ds    0h
*        this code is used to allow the loads of the FPR to be removed
*        if the load of R14 is also being removed.
*        the model MUST use R14 to address the accumulator which is
*        then loaded into FP0/2.
*        And if the load of R14 is removed (see the code that sets
*        LTOMITLD) then this code will insert a branch to skip 8 bytes
*        (2 LD instructions)
*
         if TM,LTFLAG2,LTOMITLD,o The load of r14 has been optimised?
           mvc 0(l'skip8,r1),skip8    move in a branch to skip 8 bytes
         endif
         j     p1relonx
static   loctr
skip8    j     *+8
code     loctr
P1SRPCT  LA    R15,3              ASSUME  LARGE NUMBERS WITH 3 DECIMALS
         CLI   LRGNUM,C'Y'
         JE    *+8
         LA    R15,8              USE  STANDARD NUMBERS WITH 8 DECIMALS
*
*        ZAP   DBLWORK,0(L'P008,R15) COMP SHIFT AMT TO ALIGN DEC POINT
         LLC   R0,LTNDEC
         SR    R15,R0
         LLC   R0,LTRNDFAC
         SR    R15,R0
*
         LTR   R15,R15            ANY  SHIFTING REQUIRED ???
         JZ    P1SRPCT5           NO  -  OMIT OR REPLACE
*
         stc   r15,1(,r1)         Save shift amount
         ni    1(r1),x'3F'        Only have last 6 bits
         J     P1RELONX
*
P1SRPCT5 LA    R0,2(,R1)          "SRP"  INSTR  AT   END-OF-CODE  ???
         cgr   R0,R4
         JNE   P1SRPCT8           NO  -  REPLACE    WITH   "NOP"
*
         aghi  R4,-6              REMOVE BRANCH (OVERWRITE IT)
         OI    LTFLAGS,LTOMITSR   INDICATE TRUE BRANCH OMITTED
         J     P1RELONX
*
P1SRPCT8 lgr   R15,R1             BACKUP TO BEGINNING OF "SRP"
         aghi  R15,-4
         MVC   0(6,R15),NOP6      REPLACE  "SRP" WITH "NOP"
         J     P1RELONX
*
P1LSTBYT lH    R15,LTCOLLEN       load   TARGET FIELD LENGTH (-1)
         icm   r0,b'0010',0(r1)   get the data there (x'....x...')
         nill  r0,x'f000'         remove any junk    (x'....x000')
         or    r15,r0             OR register into r15
         STH   R15,0(,R1)
         J     P1relonx           (do not set base any more)
*
P1BYTMSS LH    R15,LTFLDLEN       LOAD   SOURCE FIELD LENGTH (-1)
         CHI   R15,4-1            TRUNCATE   IF TOO   LONG
         JNH   *+8
         LHI   R15,4-1
         IC    R15,BYTEMASK(R15)  LOAD  CORRESPONDING "ICM"  MASK
         STC   R15,DBLWORK
         OC    0(1,R1),DBLWORK
         J     P1RELONX
*
P1BYTMSK LH    R15,LTCOLLEN       LOAD   TARGET FIELD LENGTH (-1)
         CHI   R15,4-1            TRUNCATE   IF TOO   LONG
         JNH   *+8
         LHI   R15,4-1
         IC    R15,BYTEMASK(R15)  LOAD  CORRESPONDING "STCM" MASK
         STC   R15,DBLWORK
         OC    0(1,R1),DBLWORK
         J     P1RELONX
*
P1LRID   L     R0,LTFLDLR         LOAD   LOGICAL RECORD ID
         BRAS  R9,SAVEADDR
         STH   R0,0(,R1)          STORE  LOW    ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         SRL   R0,12
         STC   R0,2(,R1)          STORE  HIGH   ORDER 2 NIBBLES
         J     P1BASESR
*
P1SRCHRT EQU   *
* Get the address of the LKUPBUFR
         if TM,ltflag1,ltlkup_offset,nz  Is this an offset or address?
          LGF  R9,LTLBADDR        offset, get 32 bits
          LGR  r15,r5             get litpool 64 bit address <-r5
          AGFR r15,r9             add offset -> A(LKUPBUFR)
          LLGT r9,0(0,r15)        get 31 bit address of LKUPBUFR
         else
          LLGT R9,LTLBADDR        31 bit address of LKUPBUFR
         endif
* Are we using HASH table ?
         using LKUPBUFR,R9
         CLI   LBINDEX,LBHASH     Is a HASH table to be used?
         JE    P1HASH
* Get the search key length
         LH    R0,LBKEYLEN        this is key len -1
         sll   r0,2               make into offset in addr table
         LLGT  r9,SrchAddV        Address of search routine addr tab
         AGR   r0,r9              Address of reqd search routine
         LLGT  r15,=v(gvbmr95)    address of MR95
         SGR   r0,r15             make it an offset in MR95
         STH   R0,0(,R1)          STORE  LOW    ORDER 3 NIBBLES
         NI    0(R1),X'0F'        clear left nibble
         OI    0(R1),X'C0'        set register
         SRL   R0,12
         STC   R0,2(,R1)          STORE  HIGH   ORDER 2 NIBBLES
         J     P1RELONX
P1HASH   ds    0h
* Address hash table search routine
         LLGT  r0,HASHADDV        address of LU hash table VCON
         LLGT  r15,=v(gvbmr95)    address of MR95
         SGR   r0,r15             make it an offset in MR95
         STH   R0,0(,R1)          STORE  LOW    ORDER 3 NIBBLES
         NI    0(R1),X'0F'        clear left nibble
         OI    0(R1),X'C0'        set register
         SRL   R0,12
         STC   R0,2(,R1)          STORE  HIGH   ORDER 2 NIBBLES
         J     P1RELONX
         drop  r9
*
P1KEYLEN llgt  R9,LTLBADDR        LOAD   VALUE LENGTH
         LH    R15,LBKEYLEN-LKUPBUFR(,R9)
         STC   R15,0(,R1)
         J     P1RELONX
*
P1TTLOFF LH    R0,KEYPOSN         CURRENT TITLE KEY  OFFSET
         STH   R0,LTFLDPOS        SAVE POSITION FOR  THIS KEY
         AH    R0,LTFLDLEN        ADVANCE TITLE KEY  OFFSET
         AHI   R0,1+4
         STH   R0,KEYPOSN
*
         J     P1RELONX
*
P1LVLOFF BRAS  R9,COPYVAL1        HIERARCHICAL LOOK-UP LEVEL
         STH   R0,0(,R1)          STORE LOW  ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         OI    0(R1),X'20'
         SRL   R0,12
         STC   R0,2(,R1)          STORE HIGH ORDER 2 NIBBLES
         J     P1RELONX
*
P1ACCOFF ds    0h
         llgt  r9,ltacaddr        Load accum LT row address
         if TM,LTFLAG2,LTLOCAL,z  not LOCAL VARIABLE ???
*          This is the processing for a thread level DIM* variable
           lgf r0,ltvnlitp-logictbl(,r9) Get offset in thread varpgc300
           aghi r0,thrdlen        thread vars at end of wk area  pgc300
           sth r0,0(,r1)          store  low    order 3 nibbles  pgc300
           ni  0(r1),x'0F'                                       pgc300
           oi  0(r1),x'D0'        make it based on r13           pgc300
           srl r0,12                                             pgc300
           stc r0,2(,r1)          store  high   order 2 nibbles  pgc300
         else

           lgf R0,LTVNLITP-LOGICTBL(,R9) load offset value
           if  c,r0,eq,invalid_offset    Offset not set yet?
*
             lgh R0,LTVNLEN-LOGICTBL(,R9) LOAD ACCUMULATOR   LEN
*
             LA R6,3(,R6)         ALIGN  ON   FULLWORD BOUNDARY
             nill R6,x'fffc'
*
             lgr R15,R6            SAVE    ACCUMULATOR  ADDRESS
             agr R6,R0             ADVANCE LITERAL POOL ADDR
             sgr R15,R5            COMPUTE ACCUMULATOR  OFFSET
             ST R15,LTVNLITP-LOGICTBL(,R9) SAVE ACCUMULATOR OFFSET
*
             lgr R0,R15
           endif
*
           ni ltflag2,x'ff'-ltomitld turn off this flag now
           if c,r0,eq,lastacca    SAME   ACCUM  ADDR/OFF AS  LAST ???
*                             yes - so optimize
             L R9,LTROWNO         LAST   ACCUM ADDR FROM PREVIOUS ROW
             BCTR R9,0
             if C,R9,eq,LASTACCR
*
               lgr R9,R1
               aghi R9,-2
               if (clc,0(2,r9),eq,lay),and, compare first two bytes and+
               (clc,l'lay-1(1,r9),eq,lay+l'lay-1) and last byte of LAY
*
                 OI LTFLAG2,LTOMITLD
               endif
             endif
*
           endif
***********************************************************************
*    SAVE ROW# AND ACCUMULATOR ADDR IF THIS IS A "LAY R14,0(,R2)"     *
***********************************************************************
           MVC LASTACCA,invalid_offset make value an impossible offset
*
           lgr R9,R1
           aghi R9,-2
           if (clc,0(2,r9),eq,lay),and,     compare first two bytes and+
               (clc,l'lay-1(1,r9),eq,lay+l'lay-1) and last byte of LAY
*
*
             ST R0,LASTACCA       UPDATE LAST  ACCUM  ADDRESS
             MVC LASTACCR,LTROWNO
           endif
*
           if TM,LTFLAG2,LTOMITLD,o
             MVC 0(L'LAY,R9),NOP6
           else
             STH R0,0(,R1)        STORE  LOW    ORDER 3 NIBBLES
             NI 0(R1),X'0F'
             OI 0(R1),X'20'
             SRL R0,12
             STC R0,2(,R1)        STORE  HIGH   ORDER 2 NIBBLES
           endif
         endif
         J     P1RELONX
*
P1ACCOF2 llgt  R15,LTFUNTBL       LOAD FUNCTION TABLE ADDRESS
         if CLI,FC_RTYP-FUNCTBL(R15),eq,FC_RTYP10 VARIABLE VALUE???
           lgf   R9,LTVVVAL
         else
*
           if TM,LTFLAG2,LTLOCAL,z  LOCAL VARIABLE ???
*
*          This is the processing for a thread level DIM* variable
             llgt r9,ltac2adr       Load accum LT row address    pgc300
             lgf r0,ltvnlitp-logictbl(,r9) Get offset            pgc300
             aghi r0,thrdlen      thread vars at end of wk area  pgc300
             sth r0,0(,r1)        store  low    order 3 nibbles  pgc300
             ni  0(r1),x'0F'                                     pgc300
             oi  0(r1),x'D0'      make it based on r13           pgc300
             srl r0,12                                           pgc300
             stc r0,2(,r1)        store  high   order 2 nibbles  pgc300
             j   p1relonx                                        pgc300
*
           endif
           llgt  R9,LTAC2ADR              LOAD  ACCUM  LT  ROW ADDRESS
         endif
*
         lgf   R0,LTVNLITP-LOGICTBL(,R9) load offset value
         if    c,r0,eq,invalid_offset    Offset not set yet?
*
           lgh R0,LTVNLEN-LOGICTBL(,R9)  LOAD ACCUMULATOR   LEN
*
           LA  R6,3(,R6)          ALIGN  ON   FULLWORD BOUNDARY
           nill R6,x'fffc'
*
           lgr R15,R6             SAVE    ACCUMULATOR  ADDRESS
           agr R6,R0              ADVANCE LITERAL POOL ADDR
           sgr R15,R5             COMPUTE ACCUMULATOR  OFFSET
           ST  R15,LTVNLITP-LOGICTBL(,R9) SAVE ACCUMULATOR OFFSET
*
           lgr R0,R15
*
         endif
         ni ltflag2,x'ff'-ltomitld turn off this flag now
         if c,r0,eq,lastacca      SAME   ACCUM  ADDR/OFF AS  LAST ???
*                           yes - so optimize
           L   R9,LTROWNO         LAST   ACCUM ADDR FROM PREVIOUS ROW
           BCTR R9,0
           if C,R9,eq,LASTACCR
             lgr R9,R1
             aghi R9,-2
             if (clc,0(2,r9),eq,lay),and,   compare first two bytes and+
               (clc,l'lay-1(1,r9),eq,lay+l'lay-1)  and last byte of LAY
*
               OI LTFLAG2,LTOMITLD
             endif
           endif
         endif
***********************************************************************
*  SAVE ROW# AND ACCUMULATOR ADDR IF THIS IS A "LAY  R14,0(,R2)"      *
***********************************************************************
         MVC   LASTACCA,invalid_offset  make value an impossible offset
*
         lgr   R9,R1
         aghi  R9,-2
         if (clc,0(2,r9),eq,lay),and,       compare first two bytes and+
               (clc,l'lay-1(1,r9),eq,lay+l'lay-1)  and last byte of LAY
*
           ST  R0,LASTACCA        UPDATE LAST  ACCUM  ADDRESS
           MVC LASTACCR,LTROWNO
         endif
*
         if TM,LTFLAG2,LTOMITLD,o
           MVC 0(L'LAY,R9),NOP6
         else
           STH R0,0(,R1)          STORE  LOW    ORDER 3 NIBBLES
           NI  0(R1),X'0F'
           OI  0(R1),X'20'
           SRL R0,12
           STC R0,2(,R1)          STORE  HIGH   ORDER 2 NIBBLES
         endif
         J     P1RELONX
*
*this next row is copying a value from the LT to the pool
P1ACCVAL BRAS  R9,COPYACC         COPY   ACCUMULATOR   CONSTANT
         STH   R0,0(,R1)          STORE  LOW   ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         OI    0(R1),X'20'
         SRL   R0,12
         STC   R0,2(,R1)          STORE  HIGH  ORDER 2 NIBBLES
         J     P1RELONX
*
P1ACCLEN llgt  R15,LTACADDR       LOAD   VALUE LENGTH
         LH    R15,LTVNLEN-LOGICTBL(,R15)
         BCTR  R15,0
         STC   R15,0(,R1)
         J     P1RELONX
*
P1TRTBL  llgt  R9,LTFUNTBL        LOAD ADDRESS  OF  FUNCTION TBL ENTRY
         USING FUNCTBL,R9
*
         CLI   LTFLDFMT+1,FC_PACK PACKED SOURCE ???
         JE    P1TRTBL4
*
         larl  R0,TRTTBLU         ASSUME UNSIGNED NUMERIC
*
         CLI   LTFLDFMT+1,FC_ALNUM   ALPHANUMERIC ???
         JE    P1TRTBL8
*
         CLI   LTSIGN,C'Y'
         JNE   P1TRTBL8
         larl  R0,TRTTBLN         SIGNED  NUMERIC
         J     P1TRTBL8
*
P1TRTBL4 larl  R0,TRTTBLP         ASSUME  SIGNED  PACKED
         CLI   LTSIGN,C'Y'
         JE    P1TRTBL8
         larl  R0,TRTTBLF         UNSIGNED PACKED
*
P1TRTBL8 BRAS  R9,SAVEADDR
         STH   R0,0(,R1)          STORE  LOW    ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         OI    0(R1),X'20'
         SRL   R0,12
         STC   R0,2(,R1)          STORE  HIGH   ORDER 2 NIBBLES
         J     P1RELONX
         DROP  R9
*
*
p1subctrcx LH  R0,LTV1LEN         length of haystack
         LH    R15,LTFLDLEN       LOAD     FIELD  LENGTH (-1)
         SR    R0,R15             COMPUTE  LOOP   COUNTER
*
         BRAS  R9,SAVEFULL
         STH   R0,0(,R1)          STORE LOW  ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         OI    0(R1),X'20'
         SRL   R0,12
         STC   R0,2(,R1)          STORE HIGH ORDER 2 NIBBLES
         J     P1RELONX
*
p1subctrxc LH  R15,LTV1LEN        length of SUB-STRING
         LH    R0,LTFLDLEN        LOAD     FIELD  LENGTH (-1)
         SR    R0,R15             COMPUTE  LOOP   COUNTER
         if AHI,R0,1+1,np         add 1 for the machine length and     +
                                  1 for the calculation - check for +ve
           lhi   r0,1               if 0 or -ve, make it 1
         endif
*
         BRAS  R9,SAVEFULL
         STH   R0,0(,R1)          STORE LOW  ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         OI    0(R1),X'20'
         SRL   R0,12
         STC   R0,2(,R1)          STORE HIGH ORDER 2 NIBBLES
         J     P1RELONX
*
p1subctr LH    R15,LTcollen       length of SUB-STRING
         LH    R0,LTFLDLEN        LOAD     FIELD  LENGTH (-1)
         SR    R0,R15             COMPUTE  LOOP   COUNTER
         if AHI,R0,1,np           + 1 for the calculation - check
           lhi   r0,1               if 0 or -ve, make it 1
         endif
*
         BRAS  R9,SAVEFULL
         STH   R0,0(,R1)          STORE LOW  ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         OI    0(R1),X'20'
         SRL   R0,12
         STC   R0,2(,R1)          STORE HIGH ORDER 2 NIBBLES
         J     P1RELONX
*
p1sublen LH    R15,LTV1LEN        COMPUTE  END OF SUB-STRING
         BCTR  R15,0              LENGTH -1
         STC   R15,0(,R1)
         J     P1RELONX
*
P1SUBoff lH    R0,LTFLDPOS
         STH   R0,0(,R1)
         J     P1BASESR
*
P1SUBVAL LA    R9,LTVALUES        get addr of start
         lgh   R15,LTV1LEN        get length
*
         bctgr R15,0              LENGTH  -1
*
         LA    R0,1(R6,R15)       COMPUTE NEW LITERAL POOL ENDING
         if C,R0,gt,LITPOOLM      CHECK FOR LITERAL POOL OVERFLOW ???
           LgHI R14,LITP_OVERFLOW  INDICATE OVERFLOW
           llgt R13,THRDMAIN       USE MAIN DRIVER'S THREAD
           J   STDERROR
         endif
         lgr   R0,R6              COMPUTE SUBSTRING OFFSET
         sgr   R0,R5
         EX    R15,MVCSUBS        COPY    SUBSTRING
         LA    R6,1(R6,R15)       ADVANCE CURRENT   LITERAL  POOL POS
*
         STH   R0,0(,R1)          STORE  LOW  ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         OI    0(R1),X'20'
         SRL   R0,12
         STC   R0,2(,R1)          STORE  HIGH ORDER 2 NIBBLES
         J     P1RELONX
*
P1SDNLN  CLI   LTSRTSEQ,C'D'      SORT   DESCENDING ???
         JE    *+12               YES -  INSERT LEN
         aghi  R4,-6              NO  -  EXCL "XC" FROM CODE(OVERWRITE)
         J     P1RELONX
*
         LH    R15,LTCOLLEN       LOAD   TARGET FIELD LENGTH (-1)
         STC   R15,0(,R1)
         J     P1RELONX
*
P1TKNLEN llgt  R15,LTVIEWNV       LOAD "NV" CODE SEGMENT ADDRESS
         llgt  R15,NVLOGTBL-NVPROLOG(,R15)
         LH    R15,LTSORTLN-LOGICTBL(,R15) LOAD SORT KEY LENGTH
*
         STH   R15,0(,R1)
         J     P1RELONX
*
***********************************************************************
*  GENERATE A "VIEW CALL" TO TOKEN READING "RETK/ET" SET OF VIEWS     *
***********************************************************************
P1CALLVW ltgf  R9,LTWR200A        VDP200 OUTPUT PARTITION AVAILABLE ???
         JNP   P1RELONX           NO  - MUST BE V3  LOGIC TABLE
*
***********************************************************************
*  BUILD VIEW CALL MACHINE CODE (SUBSEQUENT "WRTK" IN "ES" SET)       *
***********************************************************************
         MVC   0(CALLVIEW_length,R1),CALLVIEW COPY TEMPLATE
*
*        ST    R15,L'CALLVIEW-4(,R1)   SAVE   CALLED VIEW "RETK"
* not needed in new technique as callview code gets this itself
*
         aghi  R4,CALLVIEW_length      UPDATE END-OF-CODE ADDR
         llgt  r9,vdp0200b_file_reader-vdp0200b_file_record(,r9)   pgc3
retk     using logictbl,r9                                         pgc3
         mvc   retk.ltlbaddr,ltwrlubo   Save Lookup buffer offset  pgc3
         drop  retk                                                pgc3
*
         J     P1RELONX                LOOP
*
***********************************************************************
*  SEARCH CHAIN FOR MATCHING TOKEN READING "RETK/ET" SET              *
***********************************************************************
P1CALL00 xgr   R0,R0              INITIALIZE LR ID
         ltgf  R9,vdp0200b_FILE_READER-vdp0200b_FILE_RECORD(,R9)
         JNP   *+10
         lgf   R0,LTTOKNLR-LOGICTBL(,R9)   LOAD "LR ID"   FROM "RE"
*
         llgt  R9,FRSTRETK        LOAD 1ST TOKEN READING "NV"
P1CALL05 ltgr  R9,R9
         JNP   P1CALL10
*
         C     R0,LTTOKNLR-LOGICTBL(,R9) MATCHING LOGICAL RECORD ID'S
         JE    P1CALL15           YES -  CLONE    "RETK"  SET
*
         llgt  R9,LTNEXTRE-LOGICTBL(,R9)  LOAD NEXT TOKEN READING SET
         J     P1CALL05
*
P1CALL10 DC    H'0'
*
***********************************************************************
*  CLONE CALLED "RETK" SET (FIRST CALLING "WRTK" IN "ES" SET)         *
***********************************************************************
P1CALL15 DS    0H
*
***********************************************************************
*  BUILD VIEW CALL MACHINE CODE                                       *
***********************************************************************
         MVC   0(CALLVIEW_length,R1),CALLVIEW COPY TEMPLATE
*
         aghi  R4,CALLVIEW_length      UPDATE END-OF-CODE ADDR
*
         J     P1RELONX                LOOP
*
***********************************************************************
*  JUSTIFIED COLUMN DATA OFFSET                                       *
***********************************************************************
P1JUSOFF BRAS  R9,JUSTIFY         GET    TARGET FIELD POSITION
         LH    R15,LTJUSOFF       LOAD   TARGET FIELD POSITION
         STH   R15,0(,R1)
         J     P1BASETG
*
***********************************************************************
*  TARGET "CT" COLUMN NUMBER FOR ACCUMULATOR VALUE                    *
***********************************************************************
P1CTACUM LH    R0,LTVNCOL#        SAVE COLUMN  NUMBER IN LITERAL POOL
         larl  r9,p1accum         set up return address for savehalf
         j     savehalf           this will return on r9
*
*
*******************************************************************@02I
*  TARGET "CT" COLUMN NUMBER FOR ACCUMULATOR VALUE RT=12           @02I
*******************************************************************@02I
P1CTACUM_12 BRU  P1COLNO                                           @03C
*
***********************************************************************
*  Call to DL96 - need to select correct BAS instruction              *
***********************************************************************
P1Calldl96 ds  0h
         select cli,lt_opt2,eq
         when fc_evnt
           llgt  r15,=a(call96at)
         when fc_lkup
           llgt  r15,=a(call96al)
         when fc_prev
           llgt  r15,=a(call96ap)
         when fc_prior
           llgt  r15,=a(call96ax)
         othrwise
         endsel
         mvc   0(l'bas96,r1),0(r15)   move in the correct BAS
         J     P1RELONX
         extrn call96at,call96al,call96ap,call96ax
static   loctr
bas96    bas   r10,*              Just for the length above
code     loctr
***********************************************************************
*  Call to DL96 - source fields reversed                              *
***********************************************************************
P1Calldl96r ds  0h
         select cli,lt_opt2,eq
         when fc_evnt
           llgt  r15,=a(call96tr)
         when fc_lkup
           llgt  r15,=a(call96lr)
         when fc_prev
           llgt  r15,=a(call96pr)
         when fc_prior
           llgt  r15,=a(call96xr)
         othrwise
         endsel
         mvc   0(l'bas96,r1),0(r15)   move in the correct BAS
         J     P1RELONX
         extrn call96tr,call96lr,call96pr,call96xr
*
***********************************************************************
*  Call to DL96 - source fields normal                                *
***********************************************************************
P1Calldl96n ds  0h
         select cli,lt_opt1,eq
         when fc_evnt
           llgt  r15,=a(call96t)
         when fc_lkup
           llgt  r15,=a(call96l)
         when fc_prev
           llgt  r15,=a(call96p)
         when fc_prior
           llgt  r15,=a(call96x)
         othrwise
         endsel
         mvc   0(l'bas96,r1),0(r15)   move in the correct BAS
         J     P1RELONX
***********************************************************************
*  Call to DL96 - source fields normal - use opt2                     *
***********************************************************************
P1Calldl96n_r ds  0h
         select cli,lt_opt2,eq
         when fc_evnt
           llgt  r15,=a(call96t)
         when fc_lkup
           llgt  r15,=a(call96l)
         when fc_prev
           llgt  r15,=a(call96p)
         when fc_prior
           llgt  r15,=a(call96x)
         othrwise
         endsel
         mvc   0(l'bas96,r1),0(r15)   move in the correct BAS
         J     P1RELONX
         extrn call96t,call96l,call96p,call96x
***********************************************************************
P1SDNSLN CLI   LTSRTSEQ,C'D'      SORT   DESCENDING ???
         JE    *+12               YES -  INSERT LEN
         aghi  R4,-6              NO  -  EXCL "XC" FROM CODE(OVERWRITE)
         J     P1RELONX
*
         LH    R15,LTFLDLEN       LOAD   TARGET FIELD LENGTH (-1)
         STC   R15,0(,R1)
         J     P1RELONX
*
P1TGTLOF BRAS  R9,JUSTIFY         GET    TARGET FIELD POSITION
         l     r10,ltcolfil        get type in case it is an X opnd
         LH    R15,LTCOLPOS       LOAD   TARGET FIELD POSITION
         STH   R15,0(,R1)         STORE  LOW    ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         SRL   R15,12
         STC   R15,2(,R1)         STORE  HIGH   ORDER 2 NIBBLES
         J     P1BASETG
*
P1TGTLOFr BRAS  R9,JUSTIFY         GET    TARGET FIELD POSITION
         l     r10,ltcolfil        get type in case it is an X opnd
         LH    R15,LTCOLPOS       LOAD   TARGET FIELD POSITION
         STH   R15,0(,R1)         STORE  LOW    ORDER 3 NIBBLES
         NI    0(R1),X'0F'
         SRL   R15,12
         STC   R15,2(,R1)         STORE  HIGH   ORDER 2 NIBBLES
         J     P1BASEsr
*
P1LKPSTK LH    R0,LKUPSTKO        CURRENT STACK      OFFSET
         STH   R0,0(,R1)          SAVE   OFFSET FOR  THIS LOOK-UP
         AHI   R0,4+8
         STH   R0,LKUPSTKO
*
         LH    R0,LKUPSTK#        CURRENT STACK COUNT
         AHI   R0,1
         STH   R0,LKUPSTK#
*
         if chi,r0,gt,STACKSIZ    Reached LU stack limit?
           LgHI R14,LKUP_STACK_OVERFLOW INDICATE OVERFLOW
           llgt R13,THRDMAIN      USE MAIN DRIVER'S THREAD
           J   STDERROR
         endif
*
         J     P1RELONX
*
P1SFTDIG LLC   R15,LTCOLDEC
*
         STH   R15,0(,R1)         SAVE   SHIFT  AMT
         J     P1RELONX
*
P1cmpdt1 equ   *
         lh    r15,ltfldcon
         if    (chi,r15,eq,DMY),or,(chi,r15,eq,MDY),or,                +
               (chi,r15,eq,YsMsD),or,(chi,r15,eq,DsMsY),or,            +
               (chi,r15,eq,YsDDD),or,(chi,r15,eq,D_M_Y),or,            +
               (chi,r15,eq,Y_DD),or,(chi,r15,eq,Y_M_D)
           Mvc   0(6,r1),P1cmpCLC1   No Century so change the CLC
         else
           if  (chi,r15,eq,MMsDD),or,(chi,r15,eq,DDsMM),or,            +
               (chi,r15,eq,D_M),or,(chi,r15,eq,M_D)
             Mvc 0(6,r1),P1cmpCLC3   No CCYY    so change the CLC
           endif
         endif
         J     P1RELONX
*
P1cmpdt2 equ   *
         lh    r15,ltfldcon
         if    (chi,r15,eq,DMY),or,(chi,r15,eq,MDY),or,                +
               (chi,r15,eq,YsMsD),or,(chi,r15,eq,DsMsY),or,            +
               (chi,r15,eq,YsDDD),or,(chi,r15,eq,D_M_Y),or,            +
               (chi,r15,eq,Y_DD),or,(chi,r15,eq,Y_M_D)
           Mvc   0(6,r1),P1cmpCLC2   No Century so change the CLC
         else
           if  (chi,r15,eq,MMsDD),or,(chi,r15,eq,DDsMM),or,            +
               (chi,r15,eq,D_M),or,(chi,r15,eq,M_D)
             Mvc 0(6,r1),P1cmpCLC4   No CCYY    so change the CLC
           endif
         endif
         J     P1RELONX
*
static   loctr
P1cmpCLC1 CLC  workarea+2(6),2(R14)
P1cmpCLC3 CLC  workarea+4(4),4(R14)
P1cmpCLC2 CLC  2(6,r14),workarea+2
P1cmpCLC4 CLC  4(4,r14),workarea+4
code     loctr
*
         J     P1RELONX
                        EJECT
**********************************************************************
*  INSERT SPECIFIED BASE REGISTER INTO MACHINE INSTRUCTION           *
**********************************************************************
P1BASESR llgt  R9,LTFUNTBL        LOAD ADDRESS  OF  FUNCTION TBL ENTRY
         USING FUNCTBL,R9
*
         xgr   R15,R15
         IC    R15,lt_opt1
         sllg  r15,r15,2
         larl  r0,p1basetb
         agr   R15,r0
         BR    R15
*
P1BASETG llgt  R9,LTFUNTBL        LOAD ADDRESS  OF  FUNCTION TBL ENTRY
         USING FUNCTBL,R9
*
         xgr r15,r15
         IC    R15,lt_opt2
         sllg  r15,r15,2
         larl  r0,p1basetb
         agr   R15,r0
         BR    R15
*
*
P1BASETB J     P1NEXT             00 -  N/A
         J     P1BASECO           01 -  CONSTANT  (LITERAL POOL)
         J     P1BASEEV           02 -  EVENT      RECORD
         J     P1BASEL1           03 -  LOOK-UP    RECORD-1
         J     P1BASEPR           04 -  PREVIOUS   RECORD
         J     P1BASELK           05 -  LOOK-UP    KEY
         J     P1BASESK           06 -  SORT       KEY
         J     P1BASETK           07 -  TITLE      KEY
         J     P1BASEDT           08 -  DATA               AREA ("DT")
         J     P1BASECT           09 -  CALCULATED COLUMN  AREA ("CT")
         J     P1BASEL2           0A -  LOOK-UP    RECORD-2
         J     P1BASEAC           0B -  ACCUMULATOR
         J     P1BASEex           0C -  Prior col (extract)
*
P1BASECO xgr   R15,R15
         lghi  R0,R2              CONSTANT  (LITERAL POOL)
         J     P1BOFF04
*
P1BASEEV xgr   R15,R15
         lghi  R0,R6              EVENT  RECORD
         J     P1BOFF04
*
P1BASEL1 xgr   R15,R15
         lghi  R0,R5              LOOKUP RECORD - 1
         J     P1BOFF04
*
P1BASEL2 xgr   R15,R15
         lghi  R0,R1              LOOKUP RECORD - 2
         J     P1BOFF04
*
P1BASEAC xgr   R15,R15
         lghi  R0,R14             ACCUMULATOR
         J     P1BOFF04
*
P1BASEPR xgr   R15,R15
         lghi  R0,R3              PREVIOUS EVENT RECORD
         J     P1BOFF04
*
P1BASELK lghi  R15,LKUPKEY-THRDAREA
         Lghi  R0,R13             WORK   AREA
         J     P1BOFF04
*
P1BASESK lghi  R15,EXSRTKEY-EXTREC
         lghi  R0,R7              EXTRACT RECORD - SORT  KEY
         J     P1BOFF04
*
P1BASETK llgt  R15,LTVIEWNV
         llgt  R15,NVLOGTBL-NVPROLOG(,R15)
         lgh   R15,LTSORTLN-LOGICTBL(,R15)
         aghi  R15,EXSRTKEY-EXTREC
         lghi  R0,R7              EXTRACT RECORD - TITLE KEY
         J     P1BOFF04
*
P1BASEEX ds    0h                 Prior column in extract record
*        prior column offsets are relative to the start of the area
*        indidcated in the FID which is passed here in R10
         la    r0,r7          Set up the base register value
         do ,
           select chi,r10,eq  test value in r10
             when 1           - sort key offset

               lghi R15,EXSRTKEY-EXTREC

             when 2           - sort key title (ignored)

               xgr r15,r15

             when 3           - DT area
               leave ,        exit the do

             when 4           - CT area
*            calculate the offset to the CT area for this view
               llgt R15,LTLOGNV   LOAD "NV" "LT" ROW ADDR
NVent          using logictbl,r15
               LHI R0,EXSRTKEY-EXTREC Length of extract record header
               ah R0,NVent.LTSORTLN LOAD SORT KEY LENGTH
               ah R0,NVent.LTTITLLN ADD TITLE AREA LENGTH
               ah R0,NVent.LTDATALN ADD DATA AREA LENGTH
               drop NVent
               LR R15,r0    offset to CT area
               aghi r15,1   add one as adjusted for offset
               la   r0,r7     Set up the base register again
           endsel
           j   p1boff04       add_in_offset
         enddo

P1BASEDT ds    0h  R9 has the address of the function table entry
         stg   r8,dblwork          Save r8
         llgt  R8,LTVIEWNV
         llgt  R8,NVLOGTBL-NVPROLOG(,R8)
         lgh   R15,LTSORTLN-LOGICTBL(,R8)
         ah    R15,LTTITLLN-LOGICTBL(,R8)
         aghi  R15,EXSRTKEY-EXTREC
         lg    r8,dblwork          Restpre r8
         la    r0,r7
         j     p1boff04
*
*
P1BASECT lghi  R15,COLDATA-COLEXTR
         LA    R0,R7              CALCULATED COLUMN ("CT") AREA
*
P1BOFF04 ds    0h        ADD FIELD OFFSET TO BASE OFFSET carefully
         DROP R9
         stmg   r8,r9,dblwork        save r8/9
         lb     r8,2(,r1)         get the hh part with sign in r8
         lh     r9,0(,r1)         r9  will be ....RLLL
         sll    r9,20             shift so that r9 is LLL.....
         srda r8,20               shift so that r9 is ...HHLLL with    +
                                correct sign
         ar     r15,r9            so now add to r15
         sth    r15,0(,r1)        update the machine code lll part     +
                                the register is updated below
         sra    r15,12            shift out the LLL part
         stc    r15,2(,r1)        and save the HH part
         lmg    r8,r9,dblwork        restore r8/9
*
         SLL   R0,4               LEFT ADJUST BASE REGISTER IN BYTE
         STC   R0,DBLWORK
         mvz   0(1,R1),DBLWORK
*
P1RELONX aghi  R14,2              ADVANCE TO NEXT RELOCATION TBL ENTRY
         J     P1RELOLP           LOOP
*
P1NEXT   EQU   *
         if    (tm,ltflag1,ltlkup_alloc,o),and,   Lkup buffer allocated+
               (tm,ltflag1,ltlkup_offset,z)     and offset flag off ?
           bras r9,savelba        then save the buffer addr in pool
           st   r0,ltluboff       save the offset
           oi   ltflag1,ltlkup_offset  and set the flag
         endif
         llgt  R14,EXECDADR
         USING EXECDATA,R14
*        if (CLI,EXECTRAC,eq,C'Y') TRACE OPTION  SPECIFIED ???
         if TM,WORKFLAG1,MSGLVL_DEBUG,o debug messages
           BRAS R9,PRNTMAP        PRINT GENERATED  CODE ANALYSIS
         endif
         DROP  R14
*
         lgr   R3,R4              ADVANCE  TO END OF CODE SEGMENT
*
         LA    R15,80(,R3)        ENOUGH ROOM FOR 80 MORE BYTES ???
         C     R15,CODEEND
         JNH   P1ADV
*
         lghi  R14,CODE_OVERFLOW  INDICATE OVERFLOW
         J     STDERROR
*
P1ADV    lgh   R0,LTROWLEN        ADVANCE TO NEXT ROW
         agr   R7,r0              ADVANCE TO NEXT ROW


         llgt  R14,THRDMAIN       CHECK UPDATED END OF TBL  ADDRESS
         C     R7,LTENROWA-THRDAREA(,R14)
         JNH   P1LOOP

         lghi  r0,20000                                          pgc400
         mghi  r0,6                                              pgc400
         llgt  r1,symtblb                                        pgc400
         freemain RU,LV=(0),A=(1) release unused   space         pgc400
                        SPACE 3
**********************************************************************
*  RELEASE UNUSED MACHINE CODE BUFFER                                *
**********************************************************************
         aghi  R3,7               ROUND   TO NEAREST DOUBLEWORD
         srlg  R3,r3,3
         SLLg  R3,r3,3
         llgt  R0,CODEEND-THRDAREA(,R14)  LOAD ALLOCATED ENDING  ADDR
         ST    R3,CODEEND-THRDAREA(,R14)  UPDATE  ENDING ADDRESS
*
         sgr   R0,R3              COMPUTE UNUSED  LENGTH
         lgr   R1,R3              SAVE    UNUSED ADDRESS
*
         sgf   R3,CODEBEG-THRDAREA(,R14)  ACTUAL    SIZE
         ST    R3,CODESIZE-THRDAREA(,R14)
*
         FREEMAIN RU,LV=(0),A=(1) RELEASE UNUSED   SPACE

*******************************************************************pgc2
*  RELEASE UNUSED LITERAL POOL MEMORY                             *pgc2
*******************************************************************pgc2
         llgt  R3,LITPOOLC        LOAD    CURRENT  ENDING ADDRESS  pgc2
         aghi  R3,7               ROUND   TO   DOUBLEWORD          pgc2
         SRLg  R3,r3,3                                             pgc2
         SLLg  R3,r3,3                                             pgc2
         ST    R3,LITPOOLC                                         pgc2
         ST    R3,LITPOOLM                                         pgc2
*                                                                  pgc2
         llgt  R1,LITPOOLB        COMPUTE REMAINING TABLE SPACE    pgc2
         agf   R1,LITPOOLS                                         pgc2
         lgr   R0,R1                                               pgc2
         sgr   R0,R3                                               pgc2
         sgr   R1,R0                                               pgc2
*                                                                  pgc2
         sgf   R3,LITPOOLB        UPDATE  ACTUAL    SIZE           pgc2
         ST    R3,LITPOOLS                                         pgc2
         sty   R3,LITPool_sz      save in thread main area for later
*                                                                  pgc2
         FREEMAIN RU,LV=(0),A=(1) RELEASE UNUSED    SPACE          pgc2
*                                                                  pgc2
*
*
**********************************************************************
*  RELEASE UNUSED LOGIC TABLE SPACE                                  *
**********************************************************************
         aghi  R7,7               ROUND   TO NEAREST DOUBLEWORD
         SRLg  R7,r7,3
         SLLg  R7,r7,3
*
         llgt  R0,LTBEGIN         COMPUTE REMAINING TABLE SPACE
         agf   R0,LTSIZE
         sgr   R0,R7
         lgr   R1,R7
*
         sgf   R7,LTBEGIN         UPDATE  ACTUAL    SIZE
         ST    R7,LTSIZE
*
         FREEMAIN RU,LV=(0),A=(1) RELEASE UNUSED    SPACE
*
         tm    initflag,litp2lrg  was the literal pool too large
         jno   p1exit
         lghi  r14,LITP_EXCEEDED
         j     rtnerror
p1exit   ds    0h
         lg    R14,PASS1SAV       RETURN
         BR    R14
*
*
         DROP  R2
         DROP  R7

toptrept dsect                 Trace options report
toptid   ds    cl7               View id
toptf1   ds    cl2
toptfunc ds    cl4               Function code
toptf2   ds    cl2
toptdd   ds    cl8               source DD
toptf3   ds    cl2
toptfrec ds    cl13              'from' source record number
toptf4   ds    cl2
topttrec ds    cl13              'thru' source record number
toptf5   ds    cl1
toptflt  ds    cl8               'from' LT row
toptf6   ds    cl1
topttlt  ds    cl8               'thru' LT row
toptf7   ds    cl7
toptfcol ds    cl8               'from' column
*optf8   ds    cl1
topttcol ds    cl8               'thru' column
toptf9   ds    cl2
toptsoff ds    cl12              Source field value offset
toptf10  ds    cl2
toptslen ds    cl12              Source field value length
toptf11  ds    cl2
toptsval ds    cl36              Source field value
toptrept_len equ *-toptrept
*
viewrept dsect                 View Report
viewid   ds    cl7               view id
viewf1   ds    cl2
viewname ds    cl48              view name
viewf2   ds    cl2
viewophs ds    cl9               output phase
viewf3   ds    cl12
viewofmt ds    cl13              output format
viewf4   ds    cl2
viewoagr ds    cl1               Extract Aggregation
viewf5   ds    cl4
viewepab ds    cl12              EPA buffer records (summary buf)
viewf6   ds    cl2
viewofgr ds    cl1               Format Aggregation
viewrept_len equ *-viewrept

* ~LITP literal pool report dsect
litprept    dsect
litpfileid  ds   cl7               LF ID
litpf1      ds   cl1               FILLER
litpASIZE   ds   cl10              Actual size
litpf2      ds   cl2               FILLER
litppct1    ds   cl3               Percent
litpf3      ds   cl1               % sym
litpf3a     ds   cl1               filler
litpTOT     ds   cl10              Total
litpf4      ds   cl2               FILLER
litppct2    ds   cl3               Percent
litpf5      ds   cl1               % sym
litpf5a     ds   cl1               filler
litpCFEC    ds   cl10              CFEC
litpf6      ds   cl2               FILLER
litppct3    ds   cl3               Percent
litpf7      ds   cl1               % sym
litpf7a     ds   cl1               filler
litpLKS     ds   cl10              LKS
litpf8      ds   cl2               FILLER
litppct4    ds   cl3               Percent
litpf9      ds   cl5               % symR
litpDTC     ds   cl10              DTC
litpf10     ds   cl2               FILLER
litppct5    ds   cl3               Percent
litpf11     ds   cl1               FILLER
litprept_len equ *-litprept
*
* ~IRUN report dsect
irunrept dsect
irundd   ds    cl8               IRUN DD name
irunf1   ds    cl2
irunrcnt ds    cl12              IRUN Record count
irunf2   ds    cl2
irundate ds    cl10              IRUN Date
irunf3   ds    cl1
iruntime ds    cl8               IRUN Time
irunf4   ds    cl2
iruncrby ds    cl48              IRUN Created by
irunrept_len equ *-irunrept
         ASSERT l'irunrcnt,eq,l'countmsk
*
* ~IREF report dsect
irefrept dsect
irefdd   ds    cl8               IREF DD name
ireff2   ds    cl2
irefrcnt ds    cl12              IREF Record count
ireff3   ds    cl1
irefmemu ds    cl15              IREF Memory usage
ireff4   ds    cl2
irefpfnm ds    cl48              IREF PF name
ireff5   ds    cl2
ireftlr  ds    cl8               target lr id
ireff6   ds    cl1
ireftlf  ds    cl8               target lf id
irefklen ds    cl4               IREF Key length
ireff7   ds    cl2
irefsted ds    cl1               effective date starting
ireff8   ds    cl3
irefened ds    cl1               effective data ending
irefrept_len equ *-irefrept
         ASSERT l'irefrcnt,eq,l'countmsk
         ASSERT l'irefmemu,eq,l'mem_mask
*
gvbmr96  csect
*
***********************************************************************
*  PRINT TRACE PARAMETERS                                             *
***********************************************************************
PRNTTRC DS  0H
       stg   R14,PASS1SAV       SAVE RETURN ADDRESS
*
       rptit msg=vb_blankl
       phead hd=topt           Trace options
*
       llgt  R9,EXECDADR        LOAD  PARAMETER  AREA ADDRESS
       USING EXECDATA,R9
       if cli,EXECTRAC,eq,c'Y'   Is trace on?
       drop r9
*
         rptit msg=rpttopt_hd1   column headings
         rptit msg=rpttopt_hd2   column  underlining
         rptit msg=rpttopt_hd3   column  underlining
*
rp1      using toptrept,prntline
         mvc prntline,spaces
*
* "global trace set" and "view trace sets" are mutually exclusive
* If both exist global trace set will be ignored
*
         cli viewtrac,c'Y'       do we have View trace sets?
         je  PRNTVTRC            yes, then ignore general one
*
* address the global trace set parms (for all views)
*
         L     R3,PARMTBLA       LOAD TRACE PARAMETER TABLE LIST ADDR
         USING PARMTBL,R3
*
         jas   r9,buildtrac      build trace parm output
         rptit ,
*
         j     PRNTPXIT
PRNTVTRC equ   *                 Print View trace sets in use
* Loop through the logic table to see if the
* NVs point to a trace set
         llgt  R7,LTBEGIN         LOAD LOGIC TABLE ADDRESS
         USING LOGICTBL,R7
*
         do until=(CLC,LTFUNC,eq,EN)
*
         If CLC,LTFUNC,eq,NV     New View entry?
          If  lt,r3,LTPARMTB,p   trace parms for this View?
*
           USING PARMTBL,R3
*
*          build trace parm output for the current VIEW
*
           jas  r9,buildtrac
*
           rptit ,
*
          endif
         endif
         lgh   R0,ltrowlen        Next logic table entry
         agr   R7,r0
         enddo  ,                 Loop
*
        else
         rptit msg=nonemsg        no data so tell them
        endif
*
***********************************************************************
*  PRINT BLANK LINE                                                   *
***********************************************************************
PRNTPXIT ds    0h
         rptit msg=vb_blankl
*
         lg   R14,PASS1SAV       SAVE RETURN ADDRESS
         BR   r14
*
***********************************************************************
*  Build the trace parameters report output R3 -> PARMTBL             *
***********************************************************************
BUILDTRAC  DS  0H
           mvc rp1.toptrept(toptrept_len),spaces
*
           LT  R0,PARMVIEW            View number?
           jz  prntprm1               No, then it's for all views
           cvd r0,dblwork
           mvc dblwork2,fullmask
           ed  dblwork2,dblwork+4     Move in the view id
           mvc rp1.toptid,dblwork2+1
           j   prntprm2
prntprm1   equ *
           cli viewtrac,c'Y'          do we have View trace sets?
           ber r9                     yes, then ignore general one
           larl r1,topt_ALL
           mvc rp1.toptid,0(r1)
prntprm2   equ *
*
           MVC rp1.toptfunc,PARMFUNC     Function code
           mvc rp1.toptdd,parmddn        Event file ddname
*
           lg  r0,parmfrom
           cvdg r0,dblwork
           mvc dblwork2(16),Fullmask16
           ed  dblwork2(16),dblwork+8
           mvc rp1.toptfrec,dblwork2+3   From event rec
*
           lg  r0,parmthru
           cvdg r0,dblwork
           mvc dblwork2(16),Fullmask16   thru event rec
           ed  dblwork2(16),dblwork+8
           mvc rp1.topttrec,dblwork2+3
*
           L   R0,PARMROWF
           CVD R0,DBLWORK
           mvc rp1.toptflt,fullmask8
           ed  rp1.toptflt,dblwork+4      from LT row
*
           LT  R0,PARMROWT
           jz  prntprm3
           CVD R0,DBLWORK
           mvc rp1.topttlt,fullmask8
           ed  rp1.topttlt,dblwork+4      thru LT row
           j   prntprm4
prntprm3   equ *
           mvc rp1.topttlt,=cl8' 9999999'
prntprm4   equ *
*
           L   R0,PARMfcol
           CVD R0,DBLWORK
           mvc rp1.toptfcol,fullmask8
           ed  rp1.toptfcol,dblwork+4     from column
*
           if LT,R0,PARMtcol,nz
             CVD R0,DBLWORK
             mvc rp1.topttcol,fullmask8
             ed  rp1.topttcol,dblwork+4   thru column
           else
             mvc rp1.topttcol,=cl8'  999999'
           endif
*
           mvi rp1.toptfcol+1,c'>'
*
           Lh  R0,PARMVLEN        EVENT RECORD  VALUE  SPECIFIED ???
           if Ltr,R0,r0,nz
*
             CVD R0,DBLWORK
             MVC rp1.toptslen,fullmask12
             ED rp1.toptslen,DBLWORK+2
*
             LH R0,PARMVOFF
             AHI R0,1             convert offset to position
             CVD R0,DBLWORK
             MVC rp1.toptsoff,fullmask12
             ED rp1.toptsoff,DBLWORK+2
*
             MVC rp1.toptsval,PARMVALD
           endif
*
           br  r9
         drop  rp1
         DROP  R3
*
***********************************************************************
*  Print list of views and view names                                 *
***********************************************************************
PRNTVIEW ds    0h
         phead hd=view
*
         if clc,namepgm,eq,=cl8'GVBMR95R'    ref phase
           rptit msg=rptvier_hd2   column headings 2
           rptit msg=rptvier_hd3   column headings 3
           rptit msg=rptvier_hd4   column headings 4
         else
           rptit msg=rptview_hd2   column headings 2
           rptit msg=rptview_hd3   column headings 3
           rptit msg=rptview_hd4   column  underlining
         endif
*
         xc    processed_view_cnt,processed_view_cnt
         mvc   prntline,spaces
*
         lgf   r3,view_cnt        Get number of views
         llgt  r5,viewtbl_b       Get start of View table
         using view_table,r5
*
* RTC21006 Bubble Sort the View Table
*
sortvws  llgt  R14,viewtbl_b      load view table first entry address
current  using view_table,r14
         llgt  R15,viewtbl_c      load view table end enrty adress
*
outerlp  do inf
           doexit (cgr,r14,ge,r15)
           lay   R1,current.view_nbr+view_table_ent_len
           doexit (cgr,r1,ge,r15)
next       using view_table,r1
*
innerlp    do inf
*            if (clc,current.view_nbr,gt,next.view_nbr)
             if (clc,current.view_nbr,ge,next.view_nbr)
               xc  next.view_nbr(view_table_ent_len),current.view_nbr
               xc  current.view_nbr(view_table_ent_len),next.view_nbr
               xc  next.view_nbr(view_table_ent_len),current.view_nbr
             endif
*
             aghi  r1,view_table_ent_len
             doexit (cgr,r1,ge,r15)
           enddo
           aghi    r14,view_table_ent_len
         enddo
         drop current
*
         ds  0H
         do from=(r3)
           L   R0,view_nbr
           llgt r14,runview_ptr
           do label=runv_test
             if ltgr,r14,r14,nz      check that we have runview data
               using (runview_list,runv_length),r14 and map the data
               l r15,runv_count     get counter
               llgt r1,runv_first      and get first entry
               using runvent,r1     and map that
               do from=(r15),while=(ltr,r1,r1,nz)
                 if cl,r0,eq,runvent_view
                   leave runv_test  found - so print it
                 elseif (h)
                   llgt r1,runvent_next
                 elseif (l)
                   leave
                 endif
               enddo
               drop r1,r14

*              here if the view no is NOT in runviews
*              so set the view no to zero to prevent printing
               xgr r0,r0
             endif
           enddo
           if ltr,r0,r0,nz
rp1          using viewrept,prntline

             cvd r0,dblwork
             mvc dblwork2,fullmask
             ed  dblwork2,dblwork+4     Move in the view id
             mvc rp1.viewid,dblwork2+1
*
             mvc rp1.viewname,view_name Move in view name
             mvc rp1.viewf3,=cl12'           >'
*
             if clc,namepgm,eq,=cl8'GVBMR95R'    Ref  phase
               mvc rp1.viewophs,=cl09'Reference'
               mvc rp1.viewofmt,=cl13'Fixed-Length'
               mvi rp1.viewoagr,c' '
               mvi rp1.viewofgr,c' '
             else
              if cli,view_type+3,eq,COPYVIEW
               mvc rp1.viewophs,=cl09'Extract'
               mvc rp1.viewofmt,=cl13'Source-Record'
               mvi rp1.viewoagr,c' '
               mvi rp1.viewofgr,c' '
              else
               if cli,view_type+3,eq,EXTRONLY
                mvc rp1.viewophs,=cl09'Extract'
                mvc rp1.viewofmt,=cl13'Fixed-Length'
                mvi rp1.viewoagr,c' '
                mvi rp1.viewofgr,c' '
               else
                mvc rp1.viewophs,=cl13'Format'
                if (ltgf,r0,view_epa_summ_recs,nz)
                 mvi rp1.viewoagr,c'Y'
                else
                 mvi rp1.viewoagr,c'N'
                endif
                if cliy,view_format_summary,eq,c'Y'
                  mvi rp1.viewofgr,c'Y'
                else
                  mvi rp1.viewofgr,c'N'
                endif
                if cli,view_type+3,eq,DETVIEW
                 if cli,view_output_media+3,eq,FILEFMT
                  mvc rp1.viewofmt,=cl13'Fixed-Length'
                 else
                  if cli,view_output_media+3,eq,CSV
                   mvc rp1.viewofmt,=cl13'Delimited'
                  else
                   mvc rp1.viewofmt,=cl13'Report'
                  endif
                 endif
                else
                 if cli,view_output_media+3,eq,FILEFMT
                  mvc rp1.viewofmt,=cl13'Fixed-Length'
                 else
                  if cli,view_output_media+3,eq,CSV
                   mvc rp1.viewofmt,=cl13'Delimited'
                  else
                   mvc rp1.viewofmt,=cl13'Report'
                  endif
                 endif
                endif
               endif
              endif       first if
             endif       end ref phase if
*
             if (clc,namepgm,eq,=cl8'GVBMR95E'),and,(cliy,rp1.viewoagr,x
               ne,c' ')
               l   r0,view_epa_summ_recs
               cvd r0,dblwork
               mvc rp1.viewepab,countmsk
               ed  rp1.viewepab,dblwork+3  Move in EPA buffer rec
             else
               mvc rp1.viewepab,=cl12'            '
             endif ,

             rptit ,
             alsi  processed_view_cnt,1
             drop  rp1
           endif
           la  r5,view_table_ent_len(,r5)  Next view entry
         enddo
         drop  r5
*
         BR    R9
*
* ****************************************************************
*
* Print literal pool statistics
*
* ****************************************************************
litpstat using lpstatlitp,r5
Prlitpst ds    0h
         stmg  R14,R12,SAVF4SAG64RS14  SAVE  REGISTERS
         oc    pieaddr,pieaddr
         jnz   litp2
         rptit msg=vb_blankl
         phead hd=litp
         rptit msg=rptlitp_hd1   print LITP column headings
         rptit msg=rptlitp_hd2
         rptit msg=rptlitp_hd3
         mvc   prntline,spaces
*
litp2    ds    0h
*
rp2      using litprept,prntline
         lhi   r0,litprept_len+4
         sth   r0,prntrdw
*
         llgf  r15,litpstat.lp_lfid_litp file id for this "RE"
*
         cvd   r15,dblwork
         mvc   dblwork2,fullmask
         ed    dblwork2,dblwork+4     Move in the view id
         mvc   rp2.litpfileid,dblwork2+1

         drop  rp2
*
rp1      using litprept,prntline
*
         lhi   r0,litprept_len+4
         sth   r0,prntrdwh
*
         mvc   rp1.litpf1,spaces
         mvc   rp1.litpf2,spaces
         mvc   rp1.litpf3a,spaces
         mvc   rp1.litpf4,spaces
         mvc   rp1.litpf5a,spaces
         mvc   rp1.litpf6,spaces
         mvc   rp1.litpf7a,spaces
         mvc   rp1.litpf8,spaces
         mvc   rp1.litpf10,spaces
         mvi   rp1.litpf3,c'%'
         mvi   rp1.litpf5,c'%'
         mvi   rp1.litpf7,c'%'
         mvi   rp1.litpf9,c'%'
         mvi   rp1.litpf11,c'%'
*
         asi   pieaddr,1
         l     r15,litpstat.lp_leng_litp
         cvd   r15,dblwork
         lay   r14,litptots         set total counter start
         l     r4,0(r14)
         ar    r4,r15
         st    r4,0(r14)
         mvc   rp1.litpasize,nummask4
         ed    rp1.litpasize,dblwork+4
         mhi   r15,100
         xr    r14,r14
         d     r14,MBminus1
         cvd   r15,dblwork
         lay   r14,litptots+4
         lh    r4,0(r14)
         ar    r4,r15
         sth   r4,0(r14)
         mvc   rp1.litppct1,nummask2
         ed    rp1.litppct1,dblwork+6
*
         l     r15,litpstat.lp_sybl_litp
         cvd   r15,dblwork
         lay   r14,litptots+6
         l     r4,0(r14)
         ar    r4,r15
         st    r4,0(r14)
         mvc   rp1.litpTOT,nummask4
         ed    rp1.litpTOT,dblwork+4
         mhi   r15,100
         xr    r14,r14
         d     r14,MBminus1
         cvd   r15,dblwork
         lay   r14,litptots+10
         lh    r4,0(r14)
         ar    r4,r15
         sth   r4,0(r14)
         mvc   rp1.litppct2,nummask2
         ed    rp1.litppct2,dblwork+6
*
         l     r15,litpstat.lp_cfel_litp
         cvd   r15,dblwork
         lay   r14,litptots+12
         l     r4,0(r14)
         ar    r4,r15
         st    r4,0(r14)
         mvc   rp1.litpCFEC,nummask4
         ed    rp1.litpCFEC,dblwork+4
         mhi   r15,100
         xr    r14,r14
         d     r14,MBminus1
         cvd   r15,dblwork
         lay   r14,litptots+16
         lh    r4,0(r14)
         ar    r4,r15
         sth   r4,0(r14)
         mvc   rp1.litppct3,nummask2
         ed    rp1.litppct3,dblwork+6
*
         l     r15,litpstat.lp_totl_litp
         cvd   r15,dblwork
         lay   r14,litptots+18
         l     r4,0(r14)
         ar    r4,r15
         st    r4,0(r14)
         mvc   rp1.litpLKS,nummask4
         ed    rp1.litpLKS,dblwork+4
         mhi   r15,100
         xr    r14,r14
         d     r14,MBminus1
         cvd   r15,dblwork
         lay   r14,litptots+22
         lh    r4,0(r14)
         ar    r4,r15
         sth   r4,0(r14)
         mvc   rp1.litppct4,nummask2
         ed    rp1.litppct4,dblwork+6
*
         l     r15,litpstat.lp_lksl_litp
         cvd   r15,dblwork
         lay   r14,litptots+24
         l     r4,0(r14)
         ar    r4,r15
         st    r4,0(r14)
         mvc   rp1.litpDTC,nummask4
         ed    rp1.litpDTC,dblwork+4
         mvi   rp1.litpDTC,c'>'
         mhi   r15,100
         xr    r14,r14
         d     r14,MBminus1
         cvd   r15,dblwork
         lay   r14,litptots+28
         lh    r4,0(r14)
         ar    r4,r15
         sth   r4,0(r14)
         mvc   rp1.litppct5,nummask2
         ed    rp1.litppct5,dblwork+6
         rptit ,
*
         lmg   R14,R12,SAVF4SAG64RS14  SAVE  REGISTERS
         br    r10
         drop  rp1
         drop  litpstat
*
* ***********************************************************
*
* Print LITP totals for extract phase
*
* ***********************************************************
rp3     using litprept,prntline
litpsummary ds  0h
        stmg  R14,R12,SAVF4SAG64RS14  SAVE  REGISTERS
*
        rptit msg=rptlitp_hd4

        lhi   r0,litprept_len+4
        sth   r0,prntrdwh

        mvc   prntline,spaces    blank out printline
        mvc   rp3.litpfileid,=c'Total     '
        lay   r4,litptots            FILLER
        l     r15,0(r4)
        cvd   r15,dblwork
        mvc   rp3.litpasize,nummask4
        ed    rp3.litpasize,dblwork+4
        lh    r15,4(r4)
        cvd   r15,dblwork
        mvc   rp3.litppct1,nummask2
        ed    rp3.litppct1,dblwork+6
        mvi   rp3.litpf3,c'%'
        l     r15,6(r4)
        cvd   r15,dblwork
        mvc   rp3.litpTOT,nummask4
        ed    rp3.litpTOT,dblwork+4
        lh    r15,10(r4)
        cvd   r15,dblwork
        mvc   rp3.litppct2,nummask2
        ed    rp3.litppct2,dblwork+6
        mvi   rp3.litpf5,c'%'
        l     r15,12(r4)
        cvd   r15,dblwork
        mvc   rp3.litpCFEC,nummask4
        ed    rp3.litpCFEC,dblwork+4
        lh    r15,16(r4)
        cvd   r15,dblwork
        mvc   rp3.litppct3,nummask2
        ed    rp3.litppct3,dblwork+6
        mvi   rp3.litpf7,c'%'
        l     r15,18(r4)
        cvd   r15,dblwork
        mvc   rp3.litpLKS,nummask4
        ed    rp3.litpLKS,dblwork+4
        lh    r15,22(r4)
        cvd   r15,dblwork
        mvc   rp3.litppct4,nummask2
        ed    rp3.litppct4,dblwork+6
        mvi   rp3.litpf9,c'%'
        l     r15,24(r4)
        cvd   r15,dblwork
        mvc   rp3.litpDTC,nummask4
        ed    rp3.litpDTC,dblwork+4
        mvi   rp3.litpDTC,c'>'
        lh    r15,28(r4)
        cvd   r15,dblwork
        mvc   rp3.litppct5,nummask2
        ed    rp3.litppct5,dblwork+6
        mvi   rp3.litpf11,c'%'
        rptit ,

        rptit msg=rptlitp_hd6

        lhi   r0,litprept_len+4
        sth   r0,prntrdwh

        mvc   prntline,spaces    blank out printline
        mvc   rp3.litpAsize,nummask4
        mvc   rp3.litpfileid(8),=cl8'Max Size'
        llgf  r0,MBminus1
        cvd   r0,dblwork
        ed    rp3.litpAsize,dblwork+4
        mvc   rp3.litppct1,=c'100'
        mvc   rp3.litpf3,=c'%'
        mvi   rp3.litpDTC,c'>'
        rptit

        rptit msg=rptlitp_hd6

        lmg   R14,R12,SAVF4SAG64RS14  SAVE  REGISTERS
        br    r10

***********************************************************************
*  Print the ~IRUN report section to MR95E RPT                        *
*****************************************************i*****************
Prirunst ds    0h
         stmg  R14,R12,SAVF4SAG64RS14  SAVE  REGISTERS
         rptit msg=vb_blankl
         phead hd=irun
         rptit msg=rptirun_hd1   column headings
         rptit msg=rptirun_hd2   column  underlining
         lhi   r0,irunrept_len+4
         sth   r0,prntrdwh
rp1      using irunrept,prntline
         mvc   rp1.irundd,=cl8'MR95VDP'
         mvc   rp1.irunf1,spaces
         mvc   rp1.irunf2,spaces
         mvc   rp1.irunf3,spaces
         mvc   rp1.irunf4,spaces
         ly    R0,vdpcnt_real
         cvd   r0,dblwork
         mvc   rp1.irunrcnt,countmsk
         ed    rp1.irunrcnt,dblwork+3
         lay   r15,vdp_date
         mvc   rp1.irundate,0(r15)
         lay   r15,vdp_time
         mvc   rp1.iruntime,0(r15)
         lay   r15,vdp_desc
         mvc   rp1.iruncrby,0(r15)
         rptit ,
         mvc   rp1.irundd,=cl8'EXTRLTBL'
         if clc,namepgm,eq,=cl8'GVBMR95R'    R for reference
           mvc   rp1.irundd,=cl8'REFRLTBL'
         endif
         ly    R0,ltcount
         cvd   r0,dblwork
         mvc   rp1.irunrcnt,countmsk
         ed    rp1.irunrcnt,dblwork+3
         lay   r15,logictbl_date
         mvc   rp1.irundate(4),0(r15)
         mvi   rp1.irundate+4,c'-'
         mvc   rp1.irundate+5(2),4(r15)
         mvi   rp1.irundate+7,c'-'
         mvc   rp1.irundate+8(2),6(r15)
         lay   r15,logictbl_time
         mvc   rp1.iruntime(2),0(r15)
         mvi   rp1.iruntime+2,c':'
         mvc   rp1.iruntime+3(2),2(r15)
         mvi   rp1.iruntime+5,c':'
         mvc   rp1.iruntime+6(2),4(r15)
         lay   r15,logictbl_desc
         mvc   rp1.iruncrby,0(r15)
         rptit ,
         lhi   r0,irunf2-irunrept+4
         sth   r0,prntrdwh
         mvc   rp1.irundd,=cl8'EXTRREH'
         if clc,namepgm,eq,=cl8'GVBMR95R'    R for reference
           mvc   rp1.irundd,=cl8'REFRREH'
         endif
         ly    R0,grefcnt
         cvd   r0,dblwork
         mvc   rp1.irunrcnt,countmsk
         ed    rp1.irunrcnt,dblwork+3
         rptit ,
         lmg   R14,R12,SAVF4SAG64RS14  SAVE  REGISTERS
         br    r10
         drop  rp1
*
***********************************************************************
*  Print the IRUN report section for GVBMR95R                         *
***********************************************************************
Prirunstr ds    0h
         stmg  R14,R12,SAVF4SAG64RS14  SAVE  REGISTERS
         rptit msg=vb_blankl
         phead hd=irun
         rptit msg=rptirun_hd1   column headings
         rptit msg=rptirun_hd2   column  underlining
         lhi   r0,irunrept_len+4
         sth   r0,prntrdwh
rp1      using irunrept,prntline
         mvc   rp1.irundd,=cl8'MR95VDP'
         mvc   rp1.irunf1,spaces
         mvc   rp1.irunf2,spaces
         mvc   rp1.irunf3,spaces
         mvc   rp1.irunf4,spaces
         ly    R0,vdpcnt_real
         cvd   r0,dblwork
         mvc   rp1.irunrcnt,countmsk
         ed    rp1.irunrcnt,dblwork+3
         lay   r15,vdp_date
         mvc   rp1.irundate,0(r15)
         lay   r15,vdp_time
         mvc   rp1.iruntime,0(r15)
         lay   r15,vdp_desc
         mvc   rp1.iruncrby,0(r15)
         rptit ,
         mvc   rp1.irundd,=cl8'EXTRLTBL'
         if clc,namepgm,eq,=cl8'GVBMR95R'    R for reference
           mvc   rp1.irundd,=cl8'REFRLTBL'
         endif
         ly    R0,ltcount
         cvd   r0,dblwork
         mvc   rp1.irunrcnt,countmsk
         ed    rp1.irunrcnt,dblwork+3
         lay   r15,logictbl_date
         mvc   rp1.irundate(4),0(r15)
         mvi   rp1.irundate+4,c'-'
         mvc   rp1.irundate+5(2),4(r15)
         mvi   rp1.irundate+7,c'-'
         mvc   rp1.irundate+8(2),6(r15)
         lay   r15,logictbl_time
         mvc   rp1.iruntime(2),0(r15)
         mvi   rp1.iruntime+2,c':'
         mvc   rp1.iruntime+3(2),2(r15)
         mvi   rp1.iruntime+5,c':'
         mvc   rp1.iruntime+6(2),4(r15)
         lay   r15,logictbl_desc
         mvc   rp1.iruncrby,0(r15)
         rptit ,

         lmg   R14,R12,SAVF4SAG64RS14  SAVE  REGISTERS
         br    r10
         drop  rp1

*
***********************************************************************
*  Print the IRWF report section to MR95E RPT                         *
***********************************************************************
Prirwfst ds    0h
         stmg  R14,R12,SAVF4SAG64RS14  SAVE  REGISTERS

         rptit msg=vb_blankl
         phead hd=irwf
         if (ltgf,r3,vdp0650a,p),and,  Get VDP 650 record if available +        
               (lt,r1,grefcnt,p)       and ref count for extr not 0 

           rptit msg=rptiref_hd1   column headings
           rptit msg=rptiref_hd2   column  underlining
           rptit msg=rptiref_hd3   column  underlining

           lhi  r0,irefrept_len+l'prntrdw
           sth  r0,prntrdwh
*
           lhi  r10,1
*
           using vdp0650b_join_record,r3
           la   r3,vdp0650b_gref_entry
           using vdp0650b_gref_entry,r3
rp2        using irefrept,prntline
           mvc  rp2.irefdd,=cl8'REFR'
           mvc  rp2.ireff2,spaces
           mvc  rp2.ireff3,spaces
           mvc  rp2.ireff4,spaces
           mvc  rp2.ireff5,spaces
           mvc  rp2.ireff6,spaces
           mvc  rp2.ireff7,spaces
           mvc  rp2.ireff8,spaces
           xr   r2,r2             Zero record total
           xgr  r5,r5             Zero memory total

Fillrp2_loop   ds 0h
           using tblheadr,r6
           ly   r1,grefcnt               Number of REH records
           llgt r6,rehtbla               R6 -> REH table
*
*          find correct REH table record by matching on LF_ID
*
Fillrp3_loop   ds 0h
           clc  vdp0650b_gref_joinid,tbfileid
           je   Fillrp4_loop
           ahi  r6,tbhdrlen             Point at next record
           brct r1,Fillrp3_loop
           llgt r6,rehtbla              Not found:R6 -> REH 1st entry
*
Fillrp4_loop   ds 0h
           cvd  r10,dblwork
           unpk rp2.irefdd+4(3),dblwork+6(2)
           oi   rp2.irefdd+6,c'0'
           lhy  R0,tbkeylen
           cvd  r0,dblwork
           mvc  rp2.irefklen,=x'40202120'
           ed   rp2.irefklen,dblwork+6
           ly   R0,tbreccnt
           ar   r2,r0            Add record count to total
           cvd  r0,dblwork
           mvc  rp2.irefrcnt,countmsk
           ed   rp2.irefrcnt,dblwork+3
           lg   R0,tbrefmem
           agr  r5,r0            Add memory usage to total
           cvdg r0,dblwork
           mvc  rp2.irefmemu,mem_mask
           ed   rp2.irefmemu,dblwork+8+2
           mvc  rp2.irefpfnm,spaces
           l    r4,vdp0650b_gref_entlen Get length of PF name
           bctr r4,r0                  Decrement for "EX"
           ex   r4,rp2move
*
           l    r0,TBRECID                LR ID
           cvd  r0,dblwork
           mvc  dblwork2,nummask
           ed   dblwork2,dblwork+4        Move in the view id
           mvc  rp2.ireftlr,dblwork2+1
*
           l    r0,TBFILEID               LF ID
           cvd  r0,dblwork
           mvc  dblwork2,nummask
           ed   dblwork2,dblwork+4        Move in the view id
           mvc  rp2.ireftlf,dblwork2+1
*
           XR    R15,R15            EFFECTIVE DATES    PRESENT ???
           IC    R15,TBEFFDAT
           LTR   R15,R15
           JNP   Fillrp5_LB         NO  - BYPASS  COMPUTING OFFSET
           MVI   rp2.irefsted,C'Y'
           J     Fillrp6_LB
Fillrp5_LB EQU   *
           MVI   rp2.irefsted,C'N'
Fillrp6_LB EQU   *
*
           CHI   R15,ENDRANGE       END   DATES   PRESENT   ???
           JNE   Fillrp7_LB
           MVI   rp2.irefened,C'Y'
           J     Fillrp8_LB
Fillrp7_LB EQU   *
           MVI   rp2.irefened,C'N'
Fillrp8_LB EQU   *
*
           rptit ,
*         Get to next entry in VDP0650 record
           la   r3,vdp0650b_gref_entry_len+1(r4,r3)
           ahi  r10,1                   Increment loop counter
           cy   r10,grefcnt             Processed all records?
           jnh  Fillrp2_loop            N:

           rptit msg=rptiref_hd4 column  underlining
           mvc  rp2.irefdd,=cl8'Total'
           mvc  rp2.ireff2,spaces
           cvd  r2,dblwork
           mvc  rp2.irefrcnt,countmsk
           ed   rp2.irefrcnt,dblwork+3
           cvdg r5,dblwork
           mvc  rp2.irefmemu,mem_mask
           ed   rp2.irefmemu,dblwork+8+2
           rptit ,
           rptit msg=rptiref_hd5   column  underlining

         else ,                  No 650 available
           rptit msg=nonemsg        no trace options tell them
         endif

         lmg  R14,R12,SAVF4SAG64RS14  SAVE  REGISTERS
         br   r10

static   loctr
rp2move  mvc  rp2.irefpfnm(0),vdp0650b_gref_pf
code     loctr

         drop rp2
         drop r6 tblheadr
*
*
        DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C L O N E   L O O K U P   B U F F E R   C H A I N            *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R7  - CURRENT  "RE" ROW ADDR  (CLONED FROM ANOTHER "RE")     *
*        R6  - CURRENT  "LT" ROW ADDR                                 *
*        R5  - ALLOCATED LOOK-UP BUFFER ADDRESS                       *
*        R4  - PREVIOUS  LOOK-UP BUFFER ADDRESS                       *
*        R3  - LOOK-UP   BUFFER  LENGTH                               *
*        R2  - ORIGINAL  LITERAL POOL BASE ADDRESS                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING THRDAREA,R13
         using savf4sa,savesubr
         USING LOGICTBL,R7
*
P1CLONLB stmg  R14,R12,SAVF4SAG64RS14     SAVE REGISTERS
*
         mvi   retkchan,c'N'      reset flag                      pgc11
         llgt  R14,LTCLONRE               LOAD CLONED  "RE" ROW  ADDR
*
*        this loop is checking the LT RE entry to see if is a clone
*        and if so, gets the LTCLONRE pointer until we are back to
*        the non clone entry
*        this is done so that the correct ending ES address is
*        saved in CLONPVES
*
origre   using logictbl,r14
         do    while=(tm,origre.ltflag1,ltesclon,o)
           llgt  r14,origre.ltclonre
         enddo
         llgt  R14,origre.LTREES         LOAD CLONED  "ES" ROW  ADDR
         drop  origre
         llgt  r15,ltrees               Get "ES" for cloned "RE"  pgc11
origes   Using logictbl,r14                                       pgc11
cures    Using logictbl,r15                                       pgc11
         mvc   cures.ltesrtkq,origes.ltesrtkq  Put queue in clone pgc11
         drop  origes                                             pgc11
         ST    R14,CLONPVES               SAVE ENDING  LOOP ADDRESS
*
         llgt  r2,cures.lteslpad      load original lit pool addr pgc11
         agf   r2,f512k
         drop  cures                                              pgc11
         USING LITP_HDR,R2
*
         llgt  R6,LTFRSTNV-LOGICTBL(,R14) LOAD 1ST "NV" ROW IN   ORIG
         ST    R6,CURRNV                  SET  CURRENT "NV"
*
         llgt  R10,LTREES                 LOAD NEW "ES"   ROW    ADDR
         la    r10,ltlbanch-logictbl(,r10) lkup bufr chain addr   pgc11
         xc    0(4,r10),0(r10)    zero lkup chain anchor pgc11
*
P1CLONLP DC    0H
         if (CLC,LTFUNC-LOGICTBL(,r6),eq,LK_LR),and,                   +
               CLC,LTLUSTEP-LOGICTBL(,R6),eq,H1 top of no-opt LU path?
           XC    SVLUJOIN,SVLUJOIN YES - ZERO PARENT JOIN ADDRESS
           J     P1CLONNX
         endif
*
         lgf   R14,LTLUBOFF-LOGICTBL(,R6)      ASSUME   LOOK-UP  FUNC
*
         CLC   LTFUNC-LOGICTBL(,R6),JOIN       JOIN ???
         JE    P1CLONLO
*
         CLC   LTMAJFUN-LOGICTBL(,R6),LU_SM    LOOK-UP  FUNCTION  ???
         JE    P1CLONLO                        NO  - ADVANCE "LT" ROW
*
         CLC   LTFUNC-LOGICTBL(,R6),WR_TK      TOKEN WRITE  FUNC  ???
         JE    *+14                            YES - ADVANCE "LT" ROW
         CLC   LTFUNC-LOGICTBL(,R6),WR_TX      TOKEN WRITE  FUNC  ???
         JNE   P1CLONNX                        NO  - ADVANCE "LT" ROW
*
         lgf   R14,LTWRLUBO-LOGICTBL(,R6) LOAD OFFSET TO LKUPBUFR ADDR
*
P1CLONLO llgt  R15,0(R2,R14)              LOAD ORIG LOOKUP BUFFER ADDR
*
         LA    R1,LBDDNAME-LKUPBUFR(,R15)
         lgr   r4,r10                     lk-up bufr chain addr   pgc11
         BRAS  R9,LOCLB           LOCATE MATCHING  LOOKUP BUFFER
         USING LKUPBUFR,R5
*
         ltgr  R5,R5              BUFFER FOUND (ALREADY ALLOCATED) ???
         JNP   P1CLONAB           NO  -  ALLOCATE  NEW  LKUPBUFR
*
*
         llgt  R9,EXECDADR
         USING EXECDATA,R9
         if (CLI,EXECTRAC,eq,C'Y') TRACE OPTION  SPECIFIED ???
           BRAS R9,PRNTLO         PRINT  LOCATE LOOK-UP BUFFER RESULTS
         endif
         DROP  R9
*
         st    r5,0(r2,r14)       update addr in new literal pool pgc11
*
         CLC   LTFUNC-LOGICTBL(,R6),JOIN   JOIN ???
         JNE   P1CLONNX           NO  - ADVANCE
*
         ST    R5,SVLUJOIN        SAVE  PARENT JOIN ADDRESS
         J     P1CLONNX
*
P1CLONAB lgh   R3,LBLEN-LKUPBUFR(,R15)    LOAD LOOK-UP BUFFER LENGTH
         LA    R1,LBDDNAME-LKUPBUFR(,R15)
         BRAS  R9,ALLOLKUP            ALLOCATE CLONE  LOOK-UP BUFFER
*
         CLC   LTFUNC-LOGICTBL(,R6),JOIN  JOIN ???
         JNE   P1CLONNJ
         ST    R5,SVLUJOIN        YES - UPDATE PARENT LOOK-UP BUFFER
P1CLONNJ EQU   *
         OC    SVLUJOIN,SVLUJOIN  Parent addr zero (prev LKLR, step 1)
         bnz   P1CLONNLK
         ST    r5,SVLUJOIN
P1CLONNLK equ  *
*
         LHI   R15,LBPREFLN-1     LOAD  "TO"   LENGTH (-1)
         llgt  R1,0(R2,R14)       LOAD  ORIG   LOOKUP  BUFFER ADDR
         EX    R15,MVCR5R1        COPY  LOOK-UP BUFFER REMAINDER
*
         XC    LBNEXT,LBNEXT      CLEAR FORWARD POINTER
         MVC   LBPARENT,SVLUJOIN  UPDATE PARENT LOOK-UP BUFFER  ADDRESS
*
         la    r0,lbdata                                         pgc301
*??WRTX  TM    LBFLAGS+1,LBWRTX   Is this for a WRTX?             pgc4
*??WRTX  jno   p1clonab_a         N                               pgc4
*??WRTX  aghi  r0,4                                               pgc4
p1clonab_a ds  0h                                                 pgc4
         stg   r0,lblstfnd                                       pgc301
*
         LA    R0,env_area        ENVIRONMENT  DATA   ADDRESS
         ST    R0,LBENVA
         LA    R0,LBSTRTUP        START-UP     DATA   ADDRESS
         ST    R0,LBSTARTA
         LA    R0,LBEVENTA        EVENT        RECORD ADDRESS - CURRENT
         ST    R0,LBRECA
         LHI   R0,0               EXTRACT      RECORD ADDRESS - CURRENT
         ST    R0,LBEXTRA
         LA    R0,LKUPKEY         LOOK-UP      KEY    ADDRESS
         ST    R0,LBKEYA
         LA    R0,LBSUBWRK        WORKAREA     ANCHOR POINTER   ADDRESS
         ST    R0,LBANCHA
         LA    R0,LBLSTRC         RETURN       CODE   ADDRESS
         ST    R0,LBRTNCA
         LA    R0,LBLSTFND        RETURN       RECORD POINTER   ADDRESS
         ST    R0,LBRPTRA
*
         la    r0,lplkpfnd-litp_hdr(,r2)   found counter address  pgc11
         ST    R0,LBFNDCA
         la    r0,lplkpnot-litp_hdr(,r2)   not found counter addr pgc11
         ST    R0,LBNOTCA
         OI    LBNOTCA,X'80'              MARK END-OF-PARAMETER-LIST
*
         llgt  R9,EXECDADR
         USING EXECDATA,R9
         if (CLI,EXECTRAC,eq,C'Y') TRACE OPTION  SPECIFIED ???
           BRAS R9,PRNTAB         PRINT ALLOCATE LOOK-UP BUFFER RESULTS
         endif
         DROP  R9
*
         st    r5,0(r2,r14)          save cloned lkupbufr addr    pgc11
*
P1CLONNX lgh   R0,0(,R6)          ADVANCE TO NEXT "LT" ROW
         agr   R6,r0              ADVANCE TO NEXT "LT" ROW
         C     R6,CLONPVES        END-OF-SET ???
         jl    p1clonlp
clet     using logictbl,r10       clone ET                        pgc11
cles     using logictbl,r6        cloned ES (original)            pgc11
         cli   retkchan,c'Y'      Are we processing RETK chain?   pgc11
         jne   p1clones           N: then see if there is a chain pgc11
         ltgf  r7,ltnxrtkn        Is there a RETK in queue?       pgc11
         jz    p1clonex_0         N: then get out of here         pgc11
         lg    r2,p1clon_r2save   Restore thread litp addr       pgc100
         lgf   r15,ltreindx       Get this RETK/RETX index       pgc100
         mghi  r15,8              Make a word offset             pgc101
*        agr   r1,r15             Get correct offset             pgc100
         la    r15,litphdrl(r15,r2) Get to start of tokn offsets pgc100
         agf   r2,0(,r15)         Start of litp for this RETK    pgc100
         agf   r2,f512k           lit pool base is in middle
         llgt  r10,ltrees         load RETK "ET" row addr         pgc11
         st    r10,clonpves       save ending  loop address       pgc11
         llgt  r6,clet.ltfrstnv   Load 1st "nv" row               pgc11
         st    r6,currnv          Set  current "nv"               pgc11
*        la    r10,dumy_ltlbanch  use dummy one for RETK/ET       pgc11
*        XC    dumy_ltlbanch,dumy_ltlbanch and zero it            pgc11
         lgf   r10,clet.LTLBANCH  get offset of LB anchor
         agr   r10,r2             point to LB anchor chain
         xc    0(4,r10),0(r10)
         j     p1clonlp           N: then get out of here         pgc11
p1clones ds    0h                                                 pgc11
         ltgf  r7,cles.ltesrtkq   Is there a RETK queue?          pgc11
         jz    p1clonex           N: then get out of here         pgc11
         s     r2,f512k           lit pool base is in middle
         stg   r2,p1clon_r2save   Save thread litp address       pgc100
         lgf   r15,ltreindx       Get this RETK/RETX index       pgc100
         mghi  r15,8              Make a word offset             pgc101
*        agr   r1,r15             Get correct offset             pgc100
         la    r15,litphdrl(r15,r2) Get to start of tokn offsets pgc100
         agf   r2,0(,r15)         Start of litp for this RETK    pgc100
         agf   r2,f512k           lit pool base is in middle
         llgt  r10,ltrees         load RETK "ET" row addr         pgc11
         st    r10,clonpves       save ending  loop address       pgc11
         llgt  r6,clet.ltfrstnv   Load 1st "nv" row               pgc11
         st    r6,currnv          Set  current "nv"               pgc11
*        la    r10,dumy_ltlbanch  use dummy one for RETK/ET       pgc11
*        XC    dumy_ltlbanch,dumy_ltlbanch and zero it            pgc11
         lgf   r10,clet.LTLBANCH  get offset of LB anchor
         agr   r10,r2             point to LB anchor chain
         xc    0(4,r10),0(r10)
*
         mvi   retkchan,c'Y'      Signal processing RETK chain    pgc11
         j     p1clonlp           N: then get out of here         pgc11
         drop  cles                                               pgc11
         drop  clet                                               pgc11
p1clonex_0 ds  0h                                                pgc100
         lg    r2,p1clon_r2save   Restore thread litp addr       pgc100
         agf   r2,f512k           lit pool base is in middle
***********************************************************************
*  RETURN                                                             *
***********************************************************************
P1CLONEX ds    0h                                                 pgc11
         mvi   retkchan,c'N'      reset flag                      pgc11
         lmg   R14,R12,SAVF4SAG64RS14  RESTORE REGISTERS
         BR    R10                     RETURN
*
         DROP  R2                      LITERAL POOL   HEADER
         DROP  R5                      LOOKUP  BUFFER
         DROP  R7                      LOGIC   TABLE
         DROP  R13                     THREAD  AREA
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* "PRNTMAP" - PRINT LOGIC TABLE MAP                                   *
*                                                                     *
* REGISTER USAGE:                                                     *
*                                                                     *
*        R7  - CURRENT  LOGIC   TABLE  ROW    ADDRESS                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING LOGICTBL,R7
         USING THRDAREA,R13
         using savf4sa,savesubr
*
PRNTMAP  stmg  R14,R12,SAVF4SAG64RS14    SAVE PASS1  REGISTERS
*
*
         if    cliy,printmap_header,ne,c'D'  Headings done?
           logit msg=vb_blankl
           mviy   printmap_header,c'D'     Set them as done
           lay    r14,printmap_headings    point at them
           lhy    r15,printmap_headings    Get msg length
           bctr   r15,r0
           ex     r15,initpmv              and copy
           logit
         endif
*
***********************************************************************
*  LOGIC TABLE ROW NUMBER                                             *
***********************************************************************
         L     R0,LTROWNO
         CVD   R0,DBLWORK
         UNPK  prntline+00(6),DBLWORK
         OI    prntline+05,X'F0'
         MVI   prntline+6,C' '
*
***********************************************************************
*  LOGIC TABLE ROW ADDRESS                                            *
***********************************************************************
         st    R7,DBLWORK
         UNPK  prntline+07(9),DBLWORK(5)
         TR    prntline+07(8),HEXCONV
         MVI   prntline+07+8,C' '
*
***********************************************************************
*  LOGIC TABLE FUNCTION CODE                                          *
***********************************************************************
         MVC   prntline+16(4),LTFUNC
         MVI   prntline+20,C' '
*
***********************************************************************
*  FLAGS                                                              *
***********************************************************************
         LH    R1,LTFLAGS
         STH   R1,DBLWORK
         UNPK  prntline+21(5),DBLWORK(3)
         TR    prntline+21(4),HEXCONV
         MVI   prntline+21+4,C' '
*
***********************************************************************
*  CODE SEGMENT ADDRESS                                               *
***********************************************************************
         llgt  R0,LTCODSEG              LOAD  CODE ADDRESS
*
         TM    LTFLAG1,LTESCLON         CLONED "RE/ES" ???
         JNO   PRNTMAP5
*
         CLC   LTMAJFUN,RE_NX           "RE"   ROW ???
         JNE   PRNTMAP5
         llgt  R14,LTREES
         llgt  R0,LTESCODE-LOGICTBL(,R14)
*
PRNTMAP5 ST    R0,DBLWORK
         UNPK  prntline+26(9),DBLWORK(5)
         TR    prntline+26(8),HEXCONV
         MVI   prntline+26+8,C' '
*
***********************************************************************
*  CUMULATIVE CODE SEGMENT LENGTH                                     *
***********************************************************************
         lgr   R0,R4
         sgr   R0,R2
         STH   R0,DBLWORK
         UNPK  prntline+35(5),DBLWORK(3)
         TR    prntline+35(4),HEXCONV
         MVI   prntline+35+4,C' '
*
***********************************************************************
*  FUNCTION TABLE ENTRY ADDRESS                                       *
***********************************************************************
         llgt  R0,LTFUNTBL
         llgf  r9,=v(gvbmr95)     pick up address without the amode bit
         sgr   r0,r9              make it an offset in the trace -     +
                                  easier for debugging
         ST    R0,DBLWORK
         UNPK  prntline+40(9),DBLWORK(5)
         TR    prntline+40(8),HEXCONV
         MVI   prntline+40+8,C' '
*
***********************************************************************
*  PARAMETER ADDRESS/OFFSET                                           *
***********************************************************************
         L     R0,LTLUBOFF
         ST    R0,DBLWORK
         UNPK  prntline+49(9),DBLWORK(5)
         TR    prntline+49(8),HEXCONV
         MVI   prntline+49+8,C' '
*
***********************************************************************
*  LITERAL POOL BEGINNING ADDRESS                                     *
***********************************************************************
         lgr   R0,R5                    LOAD  CODE ADDRESS
*
         lgr   R14,R7                   ASSUME "ES" ROW
         CLC   LTFUNC,ES
         JE    *+28
         CLC   LTFUNC,ET
         JE    *+18
         TM    LTFLAG1,LTESCLON         CLONED "RE" ???
         JNO   PRNTMAP7
         llgt  R14,LTREES
*
         llgt  R0,LTESLPAD-LOGICTBL(,R14)
*
PRNTMAP7 ST    R0,DBLWORK
         UNPK  prntline+60(9),DBLWORK(5)
         TR    prntline+60(8),HEXCONV
         MVI   prntline+60+8,C' '
*
***********************************************************************
*  CURRENT LITERAL POOL SIZE                                          *
***********************************************************************
         lgr   R15,R6
         sgr   R15,R5
*
         lgr   R14,R7                   ASSUME "ES" ROW
         CLC   LTFUNC,ES
         JE    *+28
         CLC   LTFUNC,ET
         JE    *+18
         TM    LTFLAG1,LTESCLON         CLONED "RE" ???
         JNO   PRNTMAP8
         llgt  R14,LTREES
*
         llgt  R15,LTESLPSZ-LOGICTBL(,R14)
*
PRNTMAP8 STH   R15,DBLWORK
         UNPK  prntline+69(5),DBLWORK(3)
         TR    prntline+69(4),HEXCONV
         MVI   prntline+69+4,C' '
*
***********************************************************************
*  ESTIMATED LITERAL POOL DEMAND                                      *
***********************************************************************
         LH    R0,LTGENLEN
         llgt  R1,LTFUNTBL
         LH    R1,FCCODELN-FUNCTBL(,R1)
         SR    R0,R1
         STH   R0,DBLWORK
         UNPK  prntline+74(5),DBLWORK(3)
         TR    prntline+74(4),HEXCONV
         MVI   prntline+74+4,C' '
*
***********************************************************************
*  LITERAL POOL ENDING ADDRESS                                        *
***********************************************************************
         lgr   R0,R6
*
         lgr   R14,R7                   ASSUME "ES" ROW
         CLC   LTFUNC,ES
         JE    PRNTMAP9
         CLC   LTFUNC,ET
         JE    PRNTMAP9
         TM    LTFLAG1,LTESCLON         CLONED "RE" ???
         JNO   PRNTMAPA
         llgt  R14,LTREES
*
PRNTMAP9 llgt  R0,LTESLPAD-LOGICTBL(,R14)
         agf   R0,LTESLPSZ-LOGICTBL(,R14)
*
PRNTMAPA ST    R0,DBLWORK
         UNPK  prntline+79(9),DBLWORK(5)
         TR    prntline+79(8),HEXCONV
         MVI   prntline+79+8,C' '
         lhi   r15,88+l'prntrdw
         sth   r15,prntrdwh
*
         logit
*
***********************************************************************
*  PRINT LOOK-UP BUFFER CHAIN ASSOCIATED WITH "ES"                    *
***********************************************************************
PRNTLBCH ds    0h
         CLC   LTFUNC,ES
         JE    *+14
         CLC   LTFUNC,ET
         JNE   PRNTRTRN
*
         MVC   PRNTLINE,SPACES
*
         LTGF  R9,LTLBANCH        CHAIN ANCHOR IN "ES"
         JNP   PRNTRTRN
*
         LHI   R10,1
*
***********************************************************************
*  SEQUENCE NUMBER                                                    *
***********************************************************************
PRNTCHLP CVD   R10,DBLWORK
         UNPK  prntline+03(3),DBLWORK
         OI    prntline+05,X'F0'
*
***********************************************************************
*  LOOK-UP BUFFER ADDRESS                                             *
***********************************************************************
         ST    R9,DBLWORK
         UNPK  prntline+07(9),DBLWORK(5)
         TR    prntline+07(8),HEXCONV
         MVI   prntline+07+8,C' '
*
***********************************************************************
*  LOOK-UP BUFFER LABEL                                               *
***********************************************************************
         MVI   prntline+16,C'L'
         MVI   prntline+17,C'B'
         MVI   prntline+18,C':'
*
***********************************************************************
*  FLAGS                                                              *
***********************************************************************
         LH    R1,LBFLAGS-LKUPBUFR(,R9)
         STH   R1,DBLWORK
         UNPK  prntline+21(5),DBLWORK(3)
         TR    prntline+21(4),HEXCONV
         MVI   prntline+21+4,C' '
*
***********************************************************************
*  FILE ID                                                            *
***********************************************************************
         L     R0,LBFILEID-LKUPBUFR(,R9)
         ST    R0,DBLWORK
         UNPK  prntline+26(9),DBLWORK(5)
         TR    prntline+26(8),HEXCONV
         MVI   prntline+26+8,C' '
*
***********************************************************************
*  LR ID                                                              *
***********************************************************************
         L     R0,LBLRID-LKUPBUFR(,R9)
         ST    R0,DBLWORK
         UNPK  prntline+35(9),DBLWORK(5)
         TR    prntline+35(8),HEXCONV
         MVI   prntline+35+8,C' '
*
***********************************************************************
*  PATH ID                                                            *
***********************************************************************
         L     R0,LBPATHID-LKUPBUFR(,R9)
         ST    R0,DBLWORK
         UNPK  prntline+44(9),DBLWORK(5)
         TR    prntline+44(8),HEXCONV
         MVI   prntline+44+8,C' '
*
***********************************************************************
*  PARENT JOIN                                                        *
***********************************************************************
         L     R0,LBPARENT-LKUPBUFR(,R9)
         ST    R0,DBLWORK
         UNPK  prntline+53(9),DBLWORK(5)
         TR    prntline+53(8),HEXCONV
         MVI   prntline+53+8,C' '
*
***********************************************************************
*  RECORD LENGTH                                                      *
***********************************************************************
         LH    R1,LBRECLEN-LKUPBUFR(,R9)
         STH   R1,DBLWORK
         UNPK  prntline+62(5),DBLWORK(3)
         TR    prntline+62(4),HEXCONV
         MVI   prntline+62+4,C' '
*
***********************************************************************
*  KEY OFFSET                                                         *
***********************************************************************
         LH    R1,LBKEYOFF-LKUPBUFR(,R9)
         STH   R1,DBLWORK
         UNPK  prntline+67(5),DBLWORK(3)
         TR    prntline+67(4),HEXCONV
         MVI   prntline+67+4,C' '
*
***********************************************************************
*  KEY LEN                                                            *
***********************************************************************
         LH    R1,LBKEYLEN-LKUPBUFR(,R9)
         STH   R1,DBLWORK
         UNPK  prntline+72(5),DBLWORK(3)
         TR    prntline+72(4),HEXCONV
         MVI   prntline+72+4,C' '
*
***********************************************************************
*  EFFECTIVE DATE OFFSET                                              *
***********************************************************************
         LH    R1,LBEFFOFF-LKUPBUFR(,R9)
         STH   R1,DBLWORK
         UNPK  prntline+77(5),DBLWORK(3)
         TR    prntline+77(4),HEXCONV
         MVI   prntline+77+4,C' '
*
         lhi   r1,l'prntrdw+77+4+1
         sth   r1,prntrdwh
         logit ,
*
         AHI   R10,1              INCREMENT SEQUENCE NUMBER
*
         LTGF  R9,LBNEXT-LKUPBUFR(,R9)
         JP    PRNTCHLP
*
PRNTRTRN lmg   R14,R12,SAVF4SAG64RS14    SAVE PASS1  REGISTERS
         BR    R9
*
*
***********************************************************************
*  PRINT LOOK-UP BUFFER FOUND KEY AND RESULT                          *
***********************************************************************
prntlo_sa using savf4sa,saveziip
PRNTLO   stmg  R14,R12,prntlo_sa.SAVF4SAG64RS14
*
         MVC   PRNTLINE,SPACES
*
***********************************************************************
*  LOOK-UP BUFFER ADDRESS                                             *
***********************************************************************
         ST    R15,DBLWORK
         UNPK  prntline+07(9),DBLWORK(5)
         TR    prntline+07(8),HEXCONV
         MVI   prntline+07+8,C' '
*
***********************************************************************
*  LOOK-UP BUFFER LABEL                                               *
***********************************************************************
         MVI   prntline+16,C'K'
         MVI   prntline+17,C'E'
         MVI   prntline+18,C'Y'
*
***********************************************************************
*  FLAGS                                                              *
***********************************************************************
         LH    R0,LBFLAGS-LKUPBUFR(,R15)
         STH   R0,DBLWORK
         UNPK  prntline+21(5),DBLWORK(3)
         TR    prntline+21(4),HEXCONV
         MVI   prntline+21+4,C' '
*
***********************************************************************
*  FILE ID                                                            *
***********************************************************************
         L     R0,LBFILEID-LKUPBUFR(,R15)
         ST    R0,DBLWORK
         UNPK  prntline+26(9),DBLWORK(5)
         TR    prntline+26(8),HEXCONV
         MVI   prntline+26+8,C' '
*
***********************************************************************
*  LR ID                                                              *
***********************************************************************
         L     R0,LBLRID-LKUPBUFR(,R15)
         ST    R0,DBLWORK
         UNPK  prntline+35(9),DBLWORK(5)
         TR    prntline+35(8),HEXCONV
         MVI   prntline+35+8,C' '
*
***********************************************************************
*  PATH ID                                                            *
***********************************************************************
         L     R0,LBPATHID-LKUPBUFR(,R15)
         ST    R0,DBLWORK
         UNPK  prntline+44(9),DBLWORK(5)
         TR    prntline+44(8),HEXCONV
         MVI   prntline+44+8,C' '
*
***********************************************************************
*  PARENT JOIN                                                        *
***********************************************************************
         L     R0,LBPARENT-LKUPBUFR(,R15)
         ST    R0,DBLWORK
         UNPK  prntline+53(9),DBLWORK(5)
         TR    prntline+53(8),HEXCONV
         MVI   prntline+53+8,C' '
*
***********************************************************************
*  RECORD LENGTH                                                      *
***********************************************************************
         LH    R0,LBRECLEN-LKUPBUFR(,R15)
         STH   R0,DBLWORK
         UNPK  prntline+62(5),DBLWORK(3)
         TR    prntline+62(4),HEXCONV
         MVI   prntline+62+4,C' '
*
***********************************************************************
*  KEY OFFSET                                                         *
***********************************************************************
         LH    R0,LBKEYOFF-LKUPBUFR(,R15)
         STH   R0,DBLWORK
         UNPK  prntline+67(5),DBLWORK(3)
         TR    prntline+67(4),HEXCONV
         MVI   prntline+67+4,C' '
*
***********************************************************************
*  KEY LEN                                                            *
***********************************************************************
         LH    R0,LBKEYLEN-LKUPBUFR(,R15)
         STH   R0,DBLWORK
         UNPK  prntline+72(5),DBLWORK(3)
         TR    prntline+72(4),HEXCONV
         MVI   prntline+72+4,C' '
*
***********************************************************************
*  EFFECTIVE DATE OFFSET                                              *
***********************************************************************
         LH    R0,LBEFFOFF-LKUPBUFR(,R15)
         STH   R0,DBLWORK+4
         UNPK  prntline+77(5),DBLWORK(3)
         TR    prntline+77(4),HEXCONV
         MVI   prntline+77+4,C' '
*
***********************************************************************
*  LITERAL POOL ADDRESS                                               *
***********************************************************************
         ST    R2,DBLWORK
         UNPK  prntline+82(9),DBLWORK(5)
         TR    prntline+82(8),HEXCONV
         MVI   prntline+82+8,C' '
*
***********************************************************************
*  LITERAL POOL OFFSET                                                *
***********************************************************************
         ST    R14,DBLWORK
         UNPK  prntline+91(7),DBLWORK+1(4)
         TR    prntline+91(6),HEXCONV
         MVI   prntline+91+6,C' '
*
         lhi   r1,l'prntrdw+91+6+1
         sth   r1,prntrdwh
         logit ,
*
         MVC   PRNTLINE,SPACES
*
***********************************************************************
*  LOOK-UP BUFFER ADDRESS                                             *
***********************************************************************
         ST    R5,DBLWORK
         UNPK  prntline+07(9),DBLWORK(5)
         TR    prntline+07(8),HEXCONV
         MVI   prntline+07+8,C' '
*
***********************************************************************
*  LOOK-UP BUFFER LABEL                                               *
***********************************************************************
         MVI   prntline+16,C'F'
         MVI   prntline+17,C'N'
         MVI   prntline+18,C'D'
*
***********************************************************************
*  FLAGS                                                              *
***********************************************************************
         LH    R0,LBFLAGS-LKUPBUFR(,R5)
         STH   R0,DBLWORK
         UNPK  prntline+21(5),DBLWORK(3)
         TR    prntline+21(4),HEXCONV
         MVI   prntline+21+4,C' '
*
***********************************************************************
*  FILE ID                                                            *
***********************************************************************
         L     R0,LBFILEID-LKUPBUFR(,R5)
         ST    R0,DBLWORK
         UNPK  prntline+26(9),DBLWORK(5)
         TR    prntline+26(8),HEXCONV
         MVI   prntline+26+8,C' '
*
***********************************************************************
*  LR ID                                                              *
***********************************************************************
         L     R0,LBLRID-LKUPBUFR(,R5)
         ST    R0,DBLWORK
         UNPK  prntline+35(9),DBLWORK(5)
         TR    prntline+35(8),HEXCONV
         MVI   prntline+35+8,C' '
*
***********************************************************************
*  PATH ID                                                            *
***********************************************************************
         L     R0,LBPATHID-LKUPBUFR(,R5)
         ST    R0,DBLWORK
         UNPK  prntline+44(9),DBLWORK(5)
         TR    prntline+44(8),HEXCONV
         MVI   prntline+44+8,C' '
*
***********************************************************************
*  PARENT JOIN                                                        *
***********************************************************************
         L     R0,LBPARENT-LKUPBUFR(,R5)
         ST    R0,DBLWORK
         UNPK  prntline+53(9),DBLWORK(5)
         TR    prntline+53(8),HEXCONV
         MVI   prntline+53+8,C' '
*
***********************************************************************
*  RECORD LENGTH                                                      *
***********************************************************************
         LH    R0,LBRECLEN-LKUPBUFR(,R5)
         STH   R0,DBLWORK
         UNPK  prntline+62(5),DBLWORK(3)
         TR    prntline+62(4),HEXCONV
         MVI   prntline+62+4,C' '
*
***********************************************************************
*  KEY OFFSET                                                         *
***********************************************************************
         LH    R0,LBKEYOFF-LKUPBUFR(,R5)
         STH   R0,DBLWORK
         UNPK  prntline+67(5),DBLWORK(3)
         TR    prntline+67(4),HEXCONV
         MVI   prntline+67+4,C' '
*
***********************************************************************
*  KEY LEN                                                            *
***********************************************************************
         LH    R0,LBKEYLEN-LKUPBUFR(,R5)
         STH   R0,DBLWORK
         UNPK  prntline+72(5),DBLWORK(3)
         TR    prntline+72(4),HEXCONV
         MVI   prntline+72+4,C' '
*
***********************************************************************
*  EFFECTIVE DATE OFFSET                                              *
***********************************************************************
         LH    R0,LBEFFOFF-LKUPBUFR(,R5)
         STH   R0,DBLWORK
         UNPK  prntline+77(5),DBLWORK(3)
         TR    prntline+77(4),HEXCONV
         MVI   prntline+77+4,C' '
*
***********************************************************************
*  LITERAL POOL ADDRESS                                               *
***********************************************************************
         L     R1,SVLITPAD                 LOAD NEW LITERAL POOL ADDR
         ST    R1,DBLWORK
         UNPK  prntline+82(9),DBLWORK(5)
         TR    prntline+82(8),HEXCONV
         MVI   prntline+82+8,C' '
*
         lhi   r1,l'prntrdw+82+8+1
         sth   r1,prntrdwh
         logit ,
*
         lmg   R14,R12,prntlo_sa.SAVF4SAG64RS14
         BR    R9
*
*
***********************************************************************
*  PRINT LOOK-UP BUFFER FROM/TO INFO                                  *
***********************************************************************
PRNTAB   stmg  R14,R12,prntlo_sa.SAVF4SAG64RS14
         lg    R3,DBLWORK         SAVE END-OF-LOOP ADDRESS
*
         MVC   PRNTLINE,SPACES
*
***********************************************************************
*  LOOK-UP BUFFER ADDRESS                                             *
***********************************************************************
         L     R15,0(R2,R14)      LOAD  ORIG   LOOKUP  BUFFER ADDR
         ST    R15,DBLWORK
         UNPK  prntline+07(9),DBLWORK(5)
         TR    prntline+07(8),HEXCONV
         MVI   prntline+07+8,C' '
*
***********************************************************************
*  LOOK-UP BUFFER LABEL                                               *
***********************************************************************
         MVI   prntline+16,C'F'
         MVI   prntline+17,C'R'
         MVI   prntline+18,C'M'
*
***********************************************************************
*  FLAGS                                                              *
***********************************************************************
         LH    R0,LBFLAGS-LKUPBUFR(,R15)
         STH   R0,DBLWORK
         UNPK  prntline+21(5),DBLWORK(3)
         TR    prntline+21(4),HEXCONV
         MVI   prntline+21+4,C' '
*
***********************************************************************
*  FILE ID                                                            *
***********************************************************************
         L     R0,LBFILEID-LKUPBUFR(,R15)
         ST    R0,DBLWORK
         UNPK  prntline+26(9),DBLWORK(5)
         TR    prntline+26(8),HEXCONV
         MVI   prntline+26+8,C' '
*
***********************************************************************
*  LR ID                                                              *
***********************************************************************
         L     R0,LBLRID-LKUPBUFR(,R15)
         ST    R0,DBLWORK
         UNPK  prntline+35(9),DBLWORK(5)
         TR    prntline+35(8),HEXCONV
         MVI   prntline+35+8,C' '
*
***********************************************************************
*  PATH ID                                                            *
***********************************************************************
         L     R0,LBPATHID-LKUPBUFR(,R15)
         ST    R0,DBLWORK
         UNPK  prntline+44(9),DBLWORK(5)
         TR    prntline+44(8),HEXCONV
         MVI   prntline+44+8,C' '
*
***********************************************************************
*  PARENT JOIN                                                        *
***********************************************************************
         L     R0,LBPARENT-LKUPBUFR(,R15)
         ST    R0,DBLWORK
         UNPK  prntline+53(9),DBLWORK(5)
         TR    prntline+53(8),HEXCONV
         MVI   prntline+53+8,C' '
*
***********************************************************************
*  RECORD LENGTH                                                      *
***********************************************************************
         LH    R0,LBRECLEN-LKUPBUFR(,R15)
         STH   R0,DBLWORK
         UNPK  prntline+62(5),DBLWORK(3)
         TR    prntline+62(4),HEXCONV
         MVI   prntline+62+4,C' '
*
***********************************************************************
*  KEY OFFSET                                                         *
***********************************************************************
         LH    R0,LBKEYOFF-LKUPBUFR(,R15)
         STH   R0,DBLWORK
         UNPK  prntline+67(5),DBLWORK(3)
         TR    prntline+67(4),HEXCONV
         MVI   prntline+67+4,C' '
*
***********************************************************************
*  KEY LEN                                                            *
***********************************************************************
         LH    R0,LBKEYLEN-LKUPBUFR(,R15)
         STH   R0,DBLWORK
         UNPK  prntline+72(5),DBLWORK(3)
         TR    prntline+72(4),HEXCONV
         MVI   prntline+72+4,C' '
*
***********************************************************************
*  EFFECTIVE DATE OFFSET                                              *
***********************************************************************
         LH    R0,LBEFFOFF-LKUPBUFR(,R15)
         STH   R0,DBLWORK
         UNPK  prntline+77(5),DBLWORK(3)
         TR    prntline+77(4),HEXCONV
         MVI   prntline+77+4,C' '
*
***********************************************************************
*  LITERAL POOL ADDRESS                                               *
***********************************************************************
         ST    R2,DBLWORK
         UNPK  prntline+82(9),DBLWORK(5)
         TR    prntline+82(8),HEXCONV
         MVI   prntline+82+8,C' '
*
***********************************************************************
*  LITERAL POOL OFFSET                                                *
***********************************************************************
         ST    R14,DBLWORK
         UNPK  prntline+91(7),DBLWORK+1(4)
         TR    prntline+91(6),HEXCONV
         MVI   prntline+91+6,C' '
*
         lhi   r1,l'prntrdw+91+6+1
         sth   r1,prntrdwh
         logit ,
*
         MVC   PRNTLINE,SPACES
*
***********************************************************************
*  LOOK-UP BUFFER ADDRESS                                             *
***********************************************************************
         ST    R5,DBLWORK
         UNPK  prntline+07(9),DBLWORK(5)
         TR    prntline+07(8),HEXCONV
         MVI   prntline+07+8,C' '
*
***********************************************************************
*  LOOK-UP BUFFER LABEL                                               *
***********************************************************************
         MVI   prntline+16,C'N'
         MVI   prntline+17,C'E'
         MVI   prntline+18,C'W'
*
***********************************************************************
*  FLAGS                                                              *
***********************************************************************
         LH    R0,LBFLAGS-LKUPBUFR(,R5)
         STH   R0,DBLWORK
         UNPK  prntline+21(5),DBLWORK(3)
         TR    prntline+21(4),HEXCONV
         MVI   prntline+21+4,C' '
*
***********************************************************************
*  FILE ID                                                            *
***********************************************************************
         L     R0,LBFILEID-LKUPBUFR(,R5)
         ST    R0,DBLWORK
         UNPK  prntline+26(9),DBLWORK(5)
         TR    prntline+26(8),HEXCONV
         MVI   prntline+26+8,C' '
*
***********************************************************************
*  LR ID                                                              *
***********************************************************************
         L     R0,LBLRID-LKUPBUFR(,R5)
         ST    R0,DBLWORK
         UNPK  prntline+35(9),DBLWORK(5)
         TR    prntline+35(8),HEXCONV
         MVI   prntline+35+8,C' '
*
***********************************************************************
*  PATH ID                                                            *
***********************************************************************
         L     R0,LBPATHID-LKUPBUFR(,R5)
         ST    R0,DBLWORK
         UNPK  prntline+44(9),DBLWORK(5)
         TR    prntline+44(8),HEXCONV
         MVI   prntline+44+8,C' '
*
***********************************************************************
*  PARENT JOIN                                                        *
***********************************************************************
         L     R0,LBPARENT-LKUPBUFR(,R5)
         ST    R0,DBLWORK
         UNPK  prntline+53(9),DBLWORK(5)
         TR    prntline+53(8),HEXCONV
         MVI   prntline+53+8,C' '
*
***********************************************************************
*  RECORD LENGTH                                                      *
***********************************************************************
         LH    R0,LBRECLEN-LKUPBUFR(,R5)
         STH   R0,DBLWORK
         UNPK  prntline+62(5),DBLWORK(3)
         TR    prntline+62(4),HEXCONV
         MVI   prntline+62+4,C' '
*
***********************************************************************
*  KEY OFFSET                                                         *
***********************************************************************
         LH    R0,LBKEYOFF-LKUPBUFR(,R5)
         STH   R0,DBLWORK
         UNPK  prntline+67(5),DBLWORK(3)
         TR    prntline+67(4),HEXCONV
         MVI   prntline+67+4,C' '
*
***********************************************************************
*  KEY LEN                                                            *
***********************************************************************
         LH    R0,LBKEYLEN-LKUPBUFR(,R5)
         STH   R0,DBLWORK
         UNPK  prntline+72(5),DBLWORK(3)
         TR    prntline+72(4),HEXCONV
         MVI   prntline+72+4,C' '
*
***********************************************************************
*  EFFECTIVE DATE OFFSET                                              *
***********************************************************************
         LH    R0,LBEFFOFF-LKUPBUFR(,R5)
         STH   R0,DBLWORK
         UNPK  prntline+77(5),DBLWORK(3)
         TR    prntline+77(4),HEXCONV
         MVI   prntline+77+4,C' '
*
***********************************************************************
*  LITERAL POOL ADDRESS                                               *
***********************************************************************
         L     R1,SVLITPAD                 LOAD NEW LITERAL POOL ADDR
         ST    R1,DBLWORK
         UNPK  prntline+82(9),DBLWORK(5)
         TR    prntline+82(8),HEXCONV
         MVI   prntline+82+8,C' '
*
         lhi   r1,l'prntrdw+82+8+1
         sth   r1,prntrdwh
         logit ,
*
         stg   R3,DBLWORK         RESTORE END-OF-LOOP ADDRESS
         lmg   R14,R12,prntlo_sa.SAVF4SAG64RS14
*
         BR    R9
*
         DROP  R7
         DROP  R13
*
***********************************************************************
*  "PIPE PUT" EXIT CODE FOR PIPING DATA FROM ONE THREAD TO ANOTHER    *
***********************************************************************
         USING THRDAREA,R13
         using saver,thrdarea
*
PIPEPUT  STM   R14,R12,savgrs14       Save REGISTERS
*
         LR    R2,R1
*
***********************************************************************
*  SET PIPE "READ BUFFER" STATUS TO AVAILABLE (WAKE UP READ ROUTINE)  *
***********************************************************************
         LT    R1,MDLWRTL-4(,R2)  LOAD READ EXIT DECB ADDRESS
         JNP   PUTEXIT            READ EXIT PRESENT - NO BYPASS POST
*
         iilf  R0,x'7F000000'     LOAD  COMPLETION  CODE
         XR    R15,R15            ZERO  REGISTER
         CS    R15,R0,0(R1)       THREAD WAITING ???
         JE    PUTEXIT            NO  - MARK  BUFFER AVAILABLE
         POST  (1),(0)
*
***********************************************************************
*  "PIPE PUT" - RETURN                                                *
***********************************************************************
PUTEXIT  LM    R14,R12,savgrs14   RESTORE RETURN REGISTER
         Bsm   0,r14              RETURN
*
                        EJECT
***********************************************************************
*  "PIPE CHECK" - IS BUFFER EMPTY (AVAILABLE TO FILL)                 *
***********************************************************************
         USING THRDAREA,R13
         using saver,saveziip
PIPECHK  STM   R14,R12,savgrs14   SAVE REGISTERS
*
         LR    R2,R1
*
***********************************************************************
*  BYPASS WAIT IF ECB ALREADY POSTED COMPLETE                         *
***********************************************************************
         TM    0(R2),X'40'
         BRO   PIPECHKR
*
         WAIT  ECB=(2)            WAIT FOR BUFFER TO BE EMPTIED
*
***********************************************************************
*  "PIPE CHECK" - RETURN                                              *
***********************************************************************
PIPECHKR LM    R14,R12,savgrs14   SAVE REGISTERS
         bsm   0,r14              RETURN
*
PIPENULL MVI   0(R1),X'7F'        MARK     I/O    COMPLETE
         bsm   0,r14              RETURN
*
static   loctr
         using targetqs,r15
modopen24 l    r15,exitqs31    entry point of DCB exit
         bsm   r14,r15         save amode, switch and branch
lenopen24 equ  *-modopen24
         drop  r15
*
         LTORG
*
* Parameter keyword tables
*
* In alphabetical order for report
*
PARMKWRD_table dc 0h
*        Keep in alpha order for the log output
         DC    CL35'ABEND_ON_CALCULATION_OVERFLOW      ',H'14'
         DC    CL35'ABEND_ON_ERROR_CONDITION           ',H'25'
         DC    CL35'ABEND_ON_LOGIC_TABLE_ROW_NBR       ',H'20'
         DC    CL35'ABEND_ON_MESSAGE_NBR               ',H'19'
         DC    CL35'DB2_CATALOG_PLAN_NAME              ',H'18' (MRCT)
         DC    CL35'DB2_SQL_PLAN_NAME                  ',H'10' (MRSQ)
         DC    CL35'DB2_VSAM_PLAN_NAME                 ',H'11' (MRDV)
         DC    CL35'DB2_VSAM_DATE_FORMAT               ',H'22'
         DC    CL35'DISK_THREAD_LIMIT                  ',H'06'
         DC    CL35'DUMP_LT_AND_GENERATED_CODE         ',H'05'
         DC    CL35'EXECUTE_IN_PARENT_THREAD           ',H'03'
         DC    CL35'FISCAL_DATE_DEFAULT                ',H'17'
         DC    CL35'FISCAL_DATE_OVERRIDE               ',H'23'
         DC    CL35'INCLUDE_REF_TABLES_IN_SYSTEM_DUMP  ',H'27'
         DC    CL35'IO_BUFFER_LEVEL                    ',H'21'
         DC    CL35'LOG_MESSAGE_LEVEL                  ',H'28'
         DC    CL35'OPTIMIZE_PACKED_OUTPUT             ',H'24'
         DC    CL35'PAGE_FIX_IO_BUFFERS                ',H'15'
         DC    CL35'RECOVER_FROM_ABEND                 ',H'26'
         DC    CL35'RUN_DATE                           ',H'16'
         DC    CL35'SOURCE_RECORD_LIMIT                ',H'08'
         DC    CL35'TAPE_THREAD_LIMIT                  ',H'07'
         DC    CL35'TRACE                              ',H'04'
         DC    CL35'TREAT_MISSING_VIEW_OUTPUTS_AS_DUMMY',H'09'
         DC    CL35'USE_ZIIP                           ',H'12'
         DC    CL35'VERIFY_CREATION_TIMESTAMP          ',H'29'
         DC    CL35'ZIIP_THREAD_LIMIT                  ',H'13'
*
         DC    CL35'HASH_PACK                          ',H'30'
         DC    CL35'HASH_MULT                          ',H'31'
         DC    CL35'DISPLAY_HASH                       ',H'32'
         DC    XL4'FFFFFFFF'
*
* Trace parameter keywords
*
TraceParmtable dc 0H
         DC    CL35'VIEW        ',H'01'
         DC    CL35'FROMREC     ',H'02'
         DC    CL35'THRUREC     ',H'03'
         DC    CL35'FROMLTROW   ',H'04'
         DC    CL35'THRULTROW   ',H'05'
         DC    CL35'LTFUNC      ',H'06'
         DC    CL35'DDNAME      ',H'07'
         DC    CL35'VPOS        ',H'08'
         DC    CL35'VLEN        ',H'09'
         DC    CL35'VALUE       ',H'10'
         DC    CL35'DISPLAYSOURCE',H'11'   INCL EVENT RECS IN TRACE
         DC    CL35'LTROW        ',H'12'
         DC    CL35'REC          ',H'13'
         DC    CL35'COL          ',H'14'
         DC    CL35'FROMCOL      ',H'15'
         DC    CL35'THRUCOL      ',H'16'
         DC    XL4'FFFFFFFF'
*
         ds    0d
***********************************************************************
*  TRANSLATE AND TEST TABLE FOR "UNSIGNED NUMERIC" CLASS TEST         *
***********************************************************************
*                   0 1 2 3 4 5 6 7 8 9 A B C D E F
*
TRTTBLU  DC    XL16'08080808080808080808080808080808'  00-0F
         DC    XL16'08080808080808080808080808080808'  10-1F
         DC    XL16'08080808080808080808080808080808'  20-2F
         DC    XL16'08080808080808080808080808080808'  30-3F
         DC    XL16'08080808080808080808080808080808'  40-4F
         DC    XL16'08080808080808080808080808080808'  50-5F
         DC    XL16'08080808080808080808080808080808'  60-6F
         DC    XL16'08080808080808080808080808080808'  70-7F
         DC    XL16'08080808080808080808080808080808'  80-8F
         DC    XL16'08080808080808080808080808080808'  90-9F
         DC    XL16'08080808080808080808080808080808'  A0-AF
         DC    XL16'08080808080808080808080808080808'  B0-BF
         DC    XL16'08080808080808080808080808080808'  C0-CF
         DC    XL16'08080808080808080808080808080808'  D0-DF
         DC    XL16'08080808080808080808080808080808'  E0-EF
         DC    XL16'00000000000000000000080808080808'  F0-FF
                        EJECT
***********************************************************************
*  TRANSLATE AND TEST TABLE FOR "SIGNED NUMERIC" CLASS TEST           *
***********************************************************************
*                   0 1 2 3 4 5 6 7 8 9 A B C D E F
*
TRTTBLN  DC    XL16'08080808080808080808080808080808'  00-0F
         DC    XL16'08080808080808080808080808080808'  10-1F
         DC    XL16'08080808080808080808080808080808'  20-2F
         DC    XL16'08080808080808080808080808080808'  30-3F
         DC    XL16'08080808080808080808080808080808'  40-4F
         DC    XL16'08080808080808080808080808080808'  50-5F
         DC    XL16'08080808080808080808080808080808'  60-6F
         DC    XL16'08080808080808080808080808080808'  70-7F
         DC    XL16'08080808080808080808080808080808'  80-8F
         DC    XL16'08080808080808080808080808080808'  90-9F
         DC    XL16'08080808080808080808080808080808'  A0-AF
         DC    XL16'08080808080808080808080808080808'  B0-BF
         DC    XL16'04040404040404040404080808080808'  C0-CF
         DC    XL16'04040404040404040404080808080808'  D0-DF
         DC    XL16'08080808080808080808080808080808'  E0-EF
         DC    XL16'00000000000000000000080808080808'  F0-FF
                        SPACE 3
***********************************************************************
*  TRANSLATE AND TEST TABLE FOR "SIGNED PACKED" CLASS TEST            *
***********************************************************************
*                    0 1 2 3 4 5 6 7 8 9 A B C D E F
*
TRTTBLP  DC    XL16'00000000000000000000080804040804'  00-0F
         DC    XL16'00000000000000000000080804040804'  10-1F
         DC    XL16'00000000000000000000080804040804'  20-2F
         DC    XL16'00000000000000000000080804040804'  30-3F
         DC    XL16'00000000000000000000080804040804'  40-4F
         DC    XL16'00000000000000000000080804040804'  50-5F
         DC    XL16'00000000000000000000080804040804'  60-6F
         DC    XL16'00000000000000000000080804040804'  70-7F
         DC    XL16'00000000000000000000080804040804'  80-8F
         DC    XL16'00000000000000000000080804040804'  90-9F
         DC    XL16'08080808080808080808080808080808'  A0-AF
         DC    XL16'08080808080808080808080808080808'  B0-BF
         DC    XL16'08080808080808080808080808080808'  C0-CF
         DC    XL16'08080808080808080808080808080808'  D0-DF
         DC    XL16'08080808080808080808080808080808'  E0-EF
         DC    XL16'08080808080808080808080808080808'  F0-FF
                        SPACE 3
***********************************************************************
*  TRANSLATE AND TEST TABLE FOR "UNSIGNED PACKED" CLASS TEST          *
***********************************************************************
*                    0 1 2 3 4 5 6 7 8 9 A B C D E F
*
TRTTBLF  DC    XL16'00000000000000000000080808080804'  00-0F
         DC    XL16'00000000000000000000080808080804'  10-1F
         DC    XL16'00000000000000000000080808080804'  20-2F
         DC    XL16'00000000000000000000080808080804'  30-3F
         DC    XL16'00000000000000000000080808080804'  40-4F
         DC    XL16'00000000000000000000080808080804'  50-5F
         DC    XL16'00000000000000000000080808080804'  60-6F
         DC    XL16'00000000000000000000080808080804'  70-7F
         DC    XL16'00000000000000000000080808080804'  80-8F
         DC    XL16'00000000000000000000080808080804'  90-9F
         DC    XL16'08080808080808080808080808080808'  A0-AF
         DC    XL16'08080808080808080808080808080808'  B0-BF
         DC    XL16'08080808080808080808080808080808'  C0-CF
         DC    XL16'08080808080808080808080808080808'  D0-DF
         DC    XL16'08080808080808080808080808080808'  E0-EF
         DC    XL16'08080808080808080808080808080808'  F0-FF
                        EJECT
MASKCODE DC    CL5'LNN0N',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ'
         DC    CL5'LN10N',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9'
         DC    CL5'LN20N',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ99'
         DC    CL5'LN30N',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZ999'
         DC    CL5'LN40N',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZ9999'
         DC    CL5'LN50N',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZ99999'
         DC    CL5'LN60N',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZ999999'
         DC    CL5'LN70N',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZ9999999'
         DC    CL5'LN80N',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZ99999999'
         DC    CL5'LN11Y',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.9'
         DC    CL5'LN12Y',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.99'
         DC    CL5'LN13Y',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZZ9.999'
         DC    CL5'LN14Y',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZZ9.9999'
         DC    CL5'LN15Y',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZZ9.99999'
         DC    CL5'LN16Y',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZZ9.999999'
         DC    CL5'LN17Y',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZZ9.9999999'
         DC    CL5'LN18Y',C'Y',CL32'-ZZZZZZZZZZZZZZZZZZZZZ9.99999999'
         DC    CL5'LYN0N',C'N',CL32'-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ'
         DC    CL5'LY10N',C'N',CL32'-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9'
         DC    CL5'LY11Y',C'N',CL32'-Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9'
         DC    CL5'LY12Y',C'N',CL32'-ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99'
         DC    CL5'LY13Y',C'N',CL32'-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999'
         DC    CL5'LY14Y',C'N',CL32'-ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999'
         DC    CL5'LY15Y',C'N',CL32'-Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99999'
         DC    CL5'LY16Y',C'N',CL32'-,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999999'
         DC    CL5'LY17Y',C'N',CL32'-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999999'
         DC    CL5'LY18Y',C'N',CL32'-ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99999999'
         DC    CL5'PYN0N',C'N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ)'
         DC    CL5'PYN1Y',C'N',CL32'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.9)'
         DC    CL5'PYN2Y',C'N',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.99)'
         DC    CL5'PY10N',C'N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9)'
         DC    CL5'PY11Y',C'N',CL32'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9)'
         DC    CL5'PY12Y',C'N',CL32',ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99)'
         DC    CL5'PY20N',C'N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,Z99)'
         DC    CL5'PY21Y',C'N',CL32'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,Z99.9)'
         DC    CL5'PY22Y',C'N',CL32',ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,Z99.99)'
         DC    CL5'RNN0N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ-'
         DC    CL5'RN10N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9-'
         DC    CL5'RN20N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ99-'
         DC    CL5'RN30N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZ999-'
         DC    CL5'RN40N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZ9999-'
         DC    CL5'RN50N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZ99999-'
         DC    CL5'RN60N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZ999999-'
         DC    CL5'RN70N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZ9999999-'
         DC    CL5'RN80N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZ99999999-'
         DC    CL5'RN11Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.9-'
         DC    CL5'RN12Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.99-'
         DC    CL5'RN13Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZ9.999-'
         DC    CL5'RN14Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZ9.9999-'
         DC    CL5'RN15Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZ9.99999-'
         DC    CL5'RN16Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZ9.999999-'
         DC    CL5'RN17Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZ9.9999999-'
         DC    CL5'RN18Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZ9.99999999-'
         DC    CL5'RYN0N',C'N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ-'
         DC    CL5'RY10N',C'N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9-'
         DC    CL5'RY11Y',C'N',CL32'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9-'
         DC    CL5'RY12Y',C'N',CL32',ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-'
         DC    CL5'RY13Y',C'N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999-'
         DC    CL5'RY14Y',C'N',CL32'ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999-'
         DC    CL5'RY15Y',C'N',CL32'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99999-'
         DC    CL5'RY16Y',C'N',CL32',ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999999-'
         DC    CL5'RY17Y',C'N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999999-'
         DC    CL5'RY18Y',C'N',CL32'ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99999999-'
         DC    CL5'UNN0N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ'
         DC    CL5'UN10N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9'
         DC    CL5'UN20N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ99'
         DC    CL5'UN30N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ999'
         DC    CL5'UN40N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9999'
         DC    CL5'UN50N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZ99999'
         DC    CL5'UN60N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZ999999'
         DC    CL5'UN70N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZ9999999'
         DC    CL5'UN80N',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZ99999999'
         DC    CL5'UN11Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.9'
         DC    CL5'UN12Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.99'
         DC    CL5'UN13Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZZ9.999'
         DC    CL5'UN14Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZZ9.9999'
         DC    CL5'UN15Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZZ9.99999'
         DC    CL5'UN16Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZZ9.999999'
         DC    CL5'UN17Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZZ9.9999999'
         DC    CL5'UN18Y',C'Y',CL32'ZZZZZZZZZZZZZZZZZZZZZZ9.99999999'
         DC    CL5'UYN0N',C'N',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ'
         DC    CL5'UY10N',C'N',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9'
         DC    CL5'UY11Y',C'N',CL32'ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9'
         DC    CL5'UY12Y',C'N',CL32'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99'
         DC    CL5'UY13Y',C'N',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999'
         DC    CL5'UY14Y',C'N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999'
         DC    CL5'UY15Y',C'N',CL32'ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99999'
         DC    CL5'UY16Y',C'N',CL32'Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999999'
         DC    CL5'UY17Y',C'N',CL32'ZZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999999'
         DC    CL5'UY18Y',C'N',CL32'ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99999999'
         DC    XL5'FFFFFFFFFF'
*
                        EJECT
*
* the following are written to the EXTRLOG
*
*
         ds    0f
tracprd  dc    0cl(tracprde-tracprds)
tracprds dc    al2(tracprde-tracprds),al2(0)
         DC    C'Parameter validation complete'
tracprde equ   *
tracvdb  dc    0cl(tracvdbe-tracvdbs)
tracvdbs dc    al2(tracvdbe-tracvdbs),al2(0)
         DC    C'VDP  Load beginning'
tracvdbe equ   *
tracvdd  dc    0cl(tracvdde-tracvdds)
tracvdds dc    al2(tracvdde-tracvdds),al2(0)
         DC    C'VDP  Load finished'
tracvdde equ   *
tracltb  dc    0cl(tracltbe-tracltbs)
tracltbs dc    al2(tracltbe-tracltbs),al2(0)
         DC    C'Logic Table   load beginning'
tracltbe equ   *
tracltd  dc    0cl(tracltde-tracltds)
tracltds dc    al2(tracltde-tracltds),al2(0)
         DC    C'Logic Table   Load finished'
tracltde equ   *
traclob  dc    0cl(traclobe-traclobs)
traclobs dc    al2(traclobe-traclobs),al2(0)
         DC    C'"ES"  set  cloning started'
traclobe equ   *
traclod  dc    0cl(traclode-traclods)
traclods dc    al2(traclode-traclods),al2(0)
         DC    C'"ES"  set  cloning finished'
traclode equ   *
tracopb  dc    0cl(tracopbe-tracopbs)
tracopbs dc    al2(tracopbe-tracopbs),al2(0)
         DC    C'Extract  file open started'
tracopbe equ   *
tracopd  dc    0cl(tracopde-tracopds)
tracopds dc    al2(tracopde-tracopds),al2(0)
         DC    C'Extract  file open finished'
tracopde equ   *
tracrfb  dc    0cl(tracrfbe-tracrfbs)
tracrfbs dc    al2(tracrfbe-tracrfbs),al2(0)
         DC    C'Reference table load beginning'
tracrfbe equ   *
tracrfd  dc    0cl(tracrfde-tracrfds)
tracrfds dc    al2(tracrfde-tracrfds),al2(0)
         DC    C'Reference table load finished'
tracrfde equ   *
tracp1b  dc    0cl(tracp1be-tracp1bs)
tracp1bs dc    al2(tracp1be-tracp1bs),al2(0)
         DC    C'Machine  code generation Pass-1 started'
tracp1be equ   *
tracp1d  dc    0cl(tracp1de-tracp1ds)
tracp1ds dc    al2(tracp1de-tracp1ds),al2(0)
         DC    C'Machine  code generation Pass-1 finished'
tracp1de equ   *
*
traccol  dc    0cl(traccole-traccols)
traccols dc    al2(traccole-traccols),al2(0)
         DC    C'Reference file xxxxxxxx Hash table collision count nnn+
               nnnnnNNNN Prime=xxxxxxxxxxxx chain=xxxxxxxxxxxx'
traccole equ   *
*
tracref  dc    0cl(tracrefe-tracrefs)
tracrefs dc    al2(tracrefe-tracrefs),al2(0)
         DC    C'Reference file xxxxxxxx LR xxxxxxxx Records xxxxxxxxx'
tracrefe equ   *
         ds    0h
*RACREF  DC    CL60'        FILE:NNNNNNNN LR:         RECS=NNNNNNNNN'
TRAC64B  DC    CL60'        64 BIT GETMAIN MEGABYTES:       NNNNNNNN'
TRACRE   DC    CL60'        GENERATING CODE FOR: XXXX NNNNNNNNN'
TRACEXT  DC    CL60'        DDN: XXXXXXXX BUFR NNNNNNNN  BUFNO: NNNN'

topt_ALL       dc cl7'ALL    '
*
         copy  gvbrpthd
         PRINT NOGEN
         DCBD  DSORG=PS
         IHADCBE
         IHAexlst
         IEFZB4D0
         IEFZB4D2
         ihasaver
tiot dsect
     ieftiot1
GVBMR96  CSECT
*
static   loctr
         LTORG
printmap_headings dc 0cl(pmhe-pmhs)
pmhs     dc    al2(pmhe-pmhs),al2(0)
         dc    c'LT row  LT addr FUNC Flag Codeaddr  Len  Functbl  Parm+
               off   Lit_strt size dmnd  Lit_end'
pmhe     equ   *
code     loctr
runvdcb  DCB   DSORG=PS,DDNAME=RUNVIEWS,MACRF=(GL),DCBE=RUNVDCBE
runvdcbe DCBE  RMODE31=BUFF,EODAD=runveof
runvDCBL EQU   *-runvdcb
*
PARMDCB  DCB   DSORG=PS,DDNAME=EXTRPARM,MACRF=(GL),DCBE=PARMDCBE
PARMDCBE DCBE  RMODE31=BUFF,EODAD=PARMEOF
parmDCBL EQU   *-parmdcb
*
TPRMDCB  DCB   DSORG=PS,DDNAME=EXTRTPRM,MACRF=(GL),DCBE=TPRMDCBE
TPRMDCBE DCBE  RMODE31=BUFF,EODAD=TPRMEOF
tprmDCBL EQU   *-tprmdcb
*
VDPDCB   DCB   DSORG=PS,DDNAME=MR95VDP,MACRF=(GL),DCBE=VDPDCBE
VDPDCBE  DCBE  RMODE31=BUFF,EODAD=VDPEOF
vdpDCBL  EQU   *-vdpdcb
*
LTBLdcb  DCB   DSORG=PS,DDNAME=EXTRLTBL,MACRF=(GL),DCBE=LTBLDCBE
LTBLDCBE DCBE  RMODE31=BUFF,EODAD=LTBLEOF
ltblDCBL EQU   *-ltbldcb
*
HDRDCB   DCB   DSORG=PS,DDNAME=EXTRREH,MACRF=(GL),DCBE=HDRDCBE
HDRDCBE  DCBE  RMODE31=BUFF,EODAD=FILLEOF
hdrDCBL  EQU   *-hdrdcb
                        SPACE 3
ENVVDCB  DCB   DSORG=PS,DDNAME=EXTRENVV,MACRF=(GL),DCBE=ENVVDCBE
ENVVDCBE DCBE  RMODE31=BUFF,EODAD=ENVVEOF
envvDCBL EQU   *-envvdcb
*
EVNTFILE DCB   DSORG=PS,DDNAME=EVNTFILE,MACRF=(R),DCBE=EVNTDCBE,       +
               EODAD=0
EVNTDCBE DCBE  RMODE31=BUFF,BLOCKTOKENSIZE=LARGE
EVNTDCBL EQU   *-EVNTFILE
                        SPACE 3
EXTRFILE DCB   DSORG=PS,DDNAME=EXTR,MACRF=(W),DCBE=EXTRDCBE
EXTRDCBE DCBE  RMODE31=BUFF,BLOCKTOKENSIZE=LARGE
EXTRDCBL EQU   *-EXTRFILE

targetqs dsect
*        mapping dsect for dcb with exit
*        first part is some model code to allow entry from
*        rmode 24 and branch up to the exit in rmode 31
* *note*  openqs and exitqs31 must be not be moved as we use
*         same code for both targetqs (qsam) and targetbs
*
openqs   ds    xl(lenopen24)
exitqs31 ds    a

dcbqs    ds    xl(envvdcbl)    Area for DCB/DCBE
exlqs    ds    x
exlqsopen ds   al3
         ds   0d
targetqs_l equ *-targetqs

targetbs dsect
*        same as targetqs except that this is a bsam dcb
openbs   ds    xl(lenopen24)
exitbs31 ds    a

dcbbs    ds    xl(extrdcbl)    Area for DCB/DCBE
exlbs    ds    x
exlbsopen ds   al3
         ds   0f
         ds   0d
targetbs_l equ *-targetbs

gvbmr96  csect
         sysstate amode64=NO
MDLWRT   WRITE WRDECB,SF,EXTRFILE,0,0,MF=L
         DC    A(0)               PIPE READ DECB ADDRESS
MDLWRTL  EQU   *-MDLWRT
         sysstate amode64=YES
                        SPACE 3
MDLWTO   WTO   TEXT=(R2),MF=L     MODEL WTO TO DISPLAY CONSOLE MESSAGES
*
***********************************************************************
*  STANDARD ERROR MESSAGE TEMPLATE                                    *
***********************************************************************
MDLERRTX DS   0CL50
         DC    CL05'FILE='
MDLERRF# DC    CL08' NNNNNNN'
         DC    CL05', ROW'
MDLERRR# DC    CL08'=NNNNNNN'
         DC    CL06', VIEW'
MDLERRV# DC    CL08'=NNNNNNN'
         DC    CL05', FC='
MDLERRFC DC    CL04'FFFF'
         DC    CL01' '
         END
