         TITLE 'GVBMRBS - BSAM INITIALIZATION FOR "GVBMR95"'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2000, 2016.
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
*  GVBMRBS - PERFORMS INITIALIZATION FOR READING SEQUENTIAL FILES     *
*                                                                     *
*  NOTE:     GVBMRBS RUNS IN 31-BIT ADDRESSING MODE.                  *
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
*        R11 - PROGRAM   BASE REGISTER                                *
*                                                                     *
*        R10 - INTERNAL  SUBROUTINE  RETURN ADDRESS (1ST LEVEL)       *
*        R9  - INTERNAL  SUBROUTINE  RETURN ADDRESS (2ND LEVEL)       *
*                                                                     *
*        R8  -                                                        *
*        R7  - BUFFER    COUNT                                        *
*        R6  - RECORD    ADDRESS                                      *
*                                                                     *
*        R5  - BUFFER    LENGTH        (ONE BLOCK)                    *
*        R4  - BUFFER    ADDRESS                                      *
*            - PARAMETER LIST ADDRESS  (GENPARM,GENPIPE,GENWRITE)     *
*            - "ECB"     CONTENTS      (LINKPIPE)                     *
*                                                                     *
*        R3  - DECB      ADDRESS                                      *
*            - EXTRACT   FILE CONTROL  AREA   ADDRESS ("PIPING")      *
*                                                                     *
*        R2  - DCB       ADDRESS                                      *
*                                                                     *
*        R1  - TEMPORARY WORK REGISTER                                *
*        R0  - TEMPORARY WORK REGISTER                                *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         Copy  GVBASSRT
         Copy  GVBMR95C
         Copy  EXECDATA
         Copy  GVBMR95W
         Copy  GVBMR95L
         Copy  GVB0200B
         Copy  GVBX95PA
         Copy  GVBUTEQU
*
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
         SPACE 2
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        REGISTER SAVE AREA OFFSETS:                                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         SPACE 2
IOBLEN   EQU   40                 LENGTH OF DYNAMICALLY ALLOCATED IOB
*
         EJECT
         PRINT NOGEN
*
GVBMRBS  RMODE 24
GVBMRBS  AMODE 31
GVBMRBS  CSECT
         j     code
         SysState ARCHLVL=3
         Print OFF,NOGEN
         Copy  ASMMSP
LEAVE    OpSyn ASM_LEAVE
         ASMMREL ON
         IEABRCX DEFINE
         Print ON
MRBSEYE  GVBEYE GVBMRBS
static   loctr
code     loctr
         USING THRDAREA,R13       REUSE/RETAIN   MAIN THREAD SAVE  AREA
savesub  using savf4sa,savesubr
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
*
         stmg  R14,R12,savesub.SAVF4SAG64RS14 save registers
*
         LR    R12,R15            set up static area pointer
         USING (GVBMRBS,code),R12
*
         if CLI,IDENTIND,ne,c'Y'  IDENTIFY NOT ISSUED ???

           ENQ (GENEVA,PGMNAME,E,,STEP),RNL=NO
           if LTR,R15,R15,nz
             GVBMSG LOG,MSGNO=INIT_ENQ_FAIL,SUBNO=1,GENENV=GENENV,     +
               SUB1=(PGMNAME,L'PGMNAME),                               +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
*
             LA  R15,12
             B   RTNERROR
*
           endif
           MVI IDENTIND,C'Y'
           larl r1,genparm_routine
           IDENTIFY EP=GENPARM,ENTRY=(1)
           larl r1,genpipe
           IDENTIFY EP=GENPIPE,ENTRY=(1)
           larl r1,genwrite
           IDENTIFY EP=GENWRITE,ENTRY=(1)
*
           DEQ (GENEVA,PGMNAME,,STEP),RNL=NO
         endif
                        SPACE 3
IDENTOK  L     R2,EVNTDCBA        LOAD  DCB  ADDRESS
         USING IHADCB,R2
*
*21062   NI    DCBRECFM,X'3F'     RESET RECFM IF REUSING THREAD
         NI    DCBRECFM,X'00'     RESET RECFM IF REUSING THREAD
         xc    dcblrecl,dcblrecl  RESET RECFM IF REUSING THREAD
         xc    dcbblksi,dcbblksi  RESET RECFM IF REUSING THREAD
*
*
         if CLC,EVNTSUBR,ne,SPACES READ EXIT PROGRAM NAME PROVIDED ??

           LGFI R0,MAXBLKSI-4     READ  EXIT   DEFAULT "LRECL"
           STH R0,DCBLRECL
           ST  R0,GPRECLEN
           ST  R0,GPRECMAX
           LGFI R0,MAXBLKSI       READ  EXIT   DEFAULT "BLKSIZE"
           STH R0,DCBBLKSI
           ST  R0,GPBLKMAX
           MVI DCBRECFM,X'50'     READ  EXIT   DEFAULT "RECFM=VB"
           MVI GPRECFMT,C'V'
*
           if lt,R14,THRDRE,p     LOAD  "RE" LOGIC TABLE ROW     ADDR
             using logictbl,r14
             if lt,R14,LTVDP200,p
               using vdp0200b_file_record,r14
               if lt,R0,vdp0200b_ALLOC_BLKSIZE,p
                 STH R0,DCBBLKSI
                 ST R0,GPBLKMAX
               endif
               llh     R0,VDP0200b_ALLOC_LRECL
               if ltr,r0,r0,p
                 STH R0,DCBLRECL
                 ST R0,GPRECLEN
                 ST R0,GPRECMAX
               endif
               if lt,r15,vdp0200b_ALLOC_RECFM,p
                 LA R14,RECFMTBL(R15)
*
                 MVC DCBRECFM(L'RECFMTBL),00(R14) RECORD FORMAT
                 if TM,DCBRECFM,X'80',o
                   MVI GPRECFMT,C'F'
                 else
                     MVI GPRECFMT,C'V'
                 endif
               endif
             endif
             drop r14
           endif
           LA  R9,EVNTSUBR        LOAD  EXIT   PROGRAM
           LOAD EPLOC=(R9),ERRET=P1SUBERR
           ST  R0,EVNTCHKA
*
           LArl R14,P1BR14        NOP   READ ROUTINE ADDRESS (BR 14)
           Oilh R14,x'8000'       set amode 31 bit on
           ST  R14,EVNTGETA

           LR  R1,R0
           if CLC,lecobol,eq,05(R1)   COBOL SUBROUTINE   ???
             LArl R14,LEREADX     OVERRIDE BSAM READ ROUTINE ADDRESS
           else
             LArl R14,P1CALLRX    OVERRIDE BSAM READ ROUTINE ADDRESS
           endif
         else
           LArl R14,NEWBUFFR      INITIALIZE   READ ROUTINE ADDRESS
         endif
         Oilh R14,x'8000'         set amode 31 bit on
         ST    R14,EVNTREAD       MAKE  LANG INTERFACE  MAIN ENTRY
                        EJECT
***********************************************************************
*  IF "PIPED INPUT" MAKE EVENT DCB ATTRIBUTES CONSISTENT  WITH  PIPE  *
***********************************************************************
         L     R14,THRDRE         LOAD  "RE" LOGIC TABLE ROW     ADDR
*        note this is the READ logic table entry
readlt   using logictbl,r14
         if    clc,readlt.ltfiltyp,eq,=y(pipedev)   is this a pipe?
           lt r3,readlt.lt_pipe_wrexta        pipe extract filea   @01I
           drop    readlt
           jnp  EVNTOPEN          NO  - BYPASS   COPYING ATTRIBUTES
           USING EXTFILE,R3
*
           MVC DCBLRECL,EXTLRECL  COPY "PIPE" ATTRIBUTES
           MVC DCBRECFM,EXTRECFM
           MVC DCBGETA,PIPEGETA+1
           MVC DCBCHCKA,PIPECHKA+1
           llh R0,EXTBUFNO
           STC R0,DCBBUFNO
           llh R0,EXTBLKSI
           STH R0,DCBBLKSI
           ST  R0,GPBLKMAX
*
           L   R5,EXTDECBF        LOAD  OUTPUT DECB  ADDR (PIPE)
           L   R14,CEEWADDR       SAVE  FIRST  DECB  ADDR
           ST  R5,LEWDECBA-LEINTER(,R14)
           MVC LERECFM-LEINTER(1,R14),EXTRECFM
           llh R0,EXTLRECL        READ  EXIT  RETURNS
           ST  R0,LELRECL-LEINTER(,R14)
           ST  R0,GPRECMAX
*
           CLC EVNTSUBR,SPACES    PIPE  READ  EXIT PGM NAME PROVIDED ??
           Jne EVNTBUFR           YES - DON'T OVERWRITE    ADDRESSES
*
           MVC EVNTGETA+1(3),DCBGETA
           OC  EVNTGETA,MODE31
           MVC EVNTCHKA+1(3),DCBCHCKA
           OC  EVNTCHKA,MODE31
*
           LA  R6,EVNTDECB        LOAD  DECB  ANCHOR/POINTER ADDRESS
           L   R15,LINKPIPA       CALL  "LINK PIPES" SUBROUTINE
           BASR R14,R15
*
           B   DECBFRST           WAIT  FOR   FIRST  BLOCK
*
           DROP R3
         endif
         EJECT
***********************************************************************
*  OPEN SEQUENTIAL EVENT FILE IF NO READ EXIT SPECIFIED               *
***********************************************************************
EVNTOPEN DS    0H
         if CLC,EVNTSUBR,eq,SPACES READ EXIT SPECIFIED  ???
*                                  No, issue open
           l r14,execdadr
           using execdata,r14
           if CLI,execpagf,eq,c'Y'   Check for page fixing
             testauth ,
             if ltr,r15,r15,z        If r15 is zero the authorized
               l  r15,DCBDCBE        point to DCB's DCBE
               using DCBE,r15
               oi dcbeflg3,dcbebfxu  Signl we fix the buffers
               drop r15
             endif
           endif
           drop r14
*
           MVC   LKUPKEY(8),OPENPARM
           OPEN  ((R2),INPUT),MODE=31,MF=(E,LKUPKEY) OPEN EVENT FILE
           MVC   LKUPKEY,SPACES
           if TM,48(R2),X'10',no   OPEN  SUCCESSFUL ???
             GVBMSG LOG,MSGNO=OPEN_SOURCE_FAIL,SUBNO=2,GENENV=GENENV,  +
               SUB1=(PGMNAME,L'PGMNAME),                               +
               SUB2=(GPDDNAME,L'GPDDNAME),                             +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
             LA   R15,8
             B    RTNERROR
           endif
         endif
*
***********************************************************************
*  SYNCHRONIZE EVENT FILE ATTRIBUTES                                  *
***********************************************************************
SEQSUBR  L     R14,CEEWADDR       SET DEFAULT READ EXIT RETURNS ATTRIB
         MVC   LERECFM-LEINTER(1,R14),DCBRECFM
*
         llh   R0,DCBLRECL
         ST    R0,LELRECL-LEINTER(,R14)
         ST    R0,GPRECMAX
         llh   R0,DCBBLKSI
         ST    R0,GPBLKMAX
*
         if CLC,EVNTSUBR,eq,SPACES  Read exit? (don't overwrite addr)
           MVC   EVNTGETA+1(3),DCBGETA
           OC    EVNTGETA,MODE31
           MVC   EVNTCHKA+1(3),DCBCHCKA
           OC    EVNTCHKA,MODE31
         endif
***********************************************************************
*  OBTAIN A SET OF "DECB'S AND BUFFERS" FOR EVENT FILE                *
***********************************************************************
EVNTBUFR EQU   *
*        LA    R9,4095(,R13)
*        SNAP  DCB=SNAPDCB,ID=100,STORAGE=((R13),(R9))
*
         if CLC,EVNTSUBR,ne,SPACES READ EXIT  SUBROUTINE SPECIFIED ???
           LA  R7,2                YES, ALLOCATE TWO BUFFERS (OVERRIDE)
         else
           llc   r7,dcbncp
           if    ltr,r7,r7,z      If exit did not set NCP (DUMMY?)
             lhi r7,2             then set default of 2
           endif
         endif
         sth   r7,EVNTbufno       save number of buffers in the pool
*
*        The following code must be serialised to keep acurate
*        statistics of the MRBS i/o buffer storage use, as
*        multiple threads/TCBs could be operating.
*        The current total and high water mark are kept in the
*        THRDMAIN area.
*        The size of the getmains are accumulated in DBLWORK
*        and used.
*
         ENQ (GENEVA,ENQSTAT,E,,STEP),RNL=NO
         if LTR,R15,R15,nz
*          Issue a warning message
           GVBMSG LOG,MSGNO=STATS_ENQ_FAIL,SUBNO=1,GENENV=GENENV,      +
               SUB1=(PGMNAME,L'PGMNAME),                               +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         endif
*
         LA    R14,DECBLEN        GET   DECB   POOL
         LR    R15,R7
         MR    R14,R14
*
         LR    R0,R15
         sty   r0,dblwork         save DECB size for later
         GETMAIN R,LV=(0)
         ST    R1,EVNTdecbp       SAVE DECB    POOL  ADDRESS
         ST    R1,EVNTDECB        SAVE current DECB  ADDRESS
         LR    R3,R1              LOAD CURRENT DECB  ADDRESS
         USING DECB,R3
*        SNAP  DCB=SNAPDCB,PDATA=REGS,ID=002
*
         llh   R5,DCBBLKSI        LOAD PHYSICAL BLOCKSIZE
         if LTR,R5,R5,nz          Is the blocksize 0? (DUMMY?)
           LA  R5,7(,R5)          ROUND TO NEXT DOUBLEWORD
           SRL R5,3
           SLL R5,3
*
           LR  R0,R5              COMPUTE  BUFFER POOL SIZE
           LR  R1,R7
           MR  R0,R0
*
           LR  R0,R1              GET   BUFFERS
           st  r1,EVNTbufps       save size of  buffer pool
           ay  r1,dblwork         add size we got to any previous
           sty r1,dblwork         and save it to use in statistics
*                                   printing in MR95
           GETMAIN RU,LV=(0),LOC=(ANY),BNDRY=PAGE
           LR  R4,R1              LOAD CURRENT BUFFER  ADDRESS
           st  r4,EVNTbufp        save address of buffer pool
         else
           xr  r4,r4
           st  r4,EVNTbufps       Reset buffer pool size
           st  r4,EVNTbufp        Reset address of buffer pool
         endif
*
d1       using thrdarea,r14
         ly    r14,thrdmain          Get the initial thread area
         ly    r1,d1.read_buffer_tot Get current total of buffers
         ay    r1,dblwork            Add size of buffers from above
         sty   r1,d1.read_buffer_tot
*
* Increase buffer high water mark stats
*
         if cy,r1,gt,d1.read_buffer_hwm If current total > HWM
           sty   r1,d1.read_buffer_hwm  save new HWM
         endif
         drop  d1
*
         DEQ (GENEVA,ENQSTAT,,STEP),RNL=NO

decbpagf ds    0h
         l     r15,dcbdcbe      get buffer no. set in open exit
         using dcbe,r15
         if  tm,dcbeflg3,dcbebfxu,o   we fix the buffers
           if LTR,r4,r4,nz
             modeset mode=SUP
             lr r1,r4            address of start of area
             lr r3,r4            save address of area
             a r3,EVNTbufps
             ahi r3,-1           point to last byte
             st r1,evntpgfs      save page fix start address
             st r3,evntpgfe      save page fix end   address
             PGSER R,FIX,A=(1),EA=(3),ECB=0
             modeset mode=PROB
           endif
         endif
         drop r15
         l     r3,EVNTdecbp       R3 -> start of DECBs
         using DECB,r3
*
DECBLOOP LA    R14,DECBLEN(,R3)   LOAD ADDRESS OF NEXT DECB
         if cij,r7,le,1           LAST  DECB   ??
           L   R14,EVNTDECB       YES - POINT  TO FIRST
         endif
*
         XC    0(DECBLEN,R3),0(R3) ZERO DECB   AREA
         ST    R14,DECBNEXT       CHAIN DECB   PREFIXES TOGETHER
*
         MVC   DECBBSAM(MDLREADL),MDLREAD      COPY  MODEL DECB
         MVC   DECBSIZE,DCBBLKSI  SAVE  BLOCK  SIZE     IN DECB
         ST    R2,DECBDCB         SAVE  DCB    ADDRESS  IN DECB
         ST    R4,DECBBUFR        SET   BUFFER ADDRESS  IN DECB
         MVC   DECBDDN,GPDDNAME   SET   DDNAME
*
         LA    R1,DECBECB         POINT TO  DECB (FOLLOWING PREFIX)
         Llgf  R15,EVNTGETA       LOAD  READ     SUBROUTINE ADDRESS
         BASSM R14,R15            ISSUE READ
*
         L     R3,DECBNEXT        LOAD  NEXT DECB  ADDRESS
         AR    R4,R5              LOAD  NEXT BLOCK ADDRESS
         BCT   R7,DECBLOOP        NO  - LOOP THROUGH ALL DECB'S
                        SPACE 3
DECBFRST L     R3,EVNTDECB        POINT TO FIRST READ
                        SPACE 3
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=150
*
         if clc,EVNTSUBR,ne,SPACES    READ  EXIT NAME PROVIDED ??
           Llgf  R15,EVNTREAD
           bassm r14,r15
         else
           LArl R15,NEWEVNT       ASSUME SEQ  FILE
           Oilh r15,x'8000'       make it 31-BIT
           Bassm r9,r15           WAIT  FOR COMPLETION OF FIRST READ
         endif

RECFRST  stg   R6,savesub.SAVF4SAG64RS6    Pass rec addr. back
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=159
                        SPACE 5
RETURN   sgr   R15,R15            SET  RETURN CODE  TO ZERO
*
RTNERROR DS    0H
         lg    R14,savesub.SAVF4SAG64RS14    restore r14
         lmg   R0,R12,savesub.SAVF4SAG64RS0  restore r0 - r12
         BSM   0,R14                         RETURN
*
         DROP  R3
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        "R E A D   E X I T   I N T E R F A C E S"                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
***********************************************************************
* "ERROR" - UNABLE TO LOAD READ EXIT SUBROUTINE                       *
***********************************************************************
P1SUBERR DS    0H                   ERROR WITH LOAD OF READ EXIT
         GVBMSG LOG,MSGNO=LOAD_READ_EXIT_FAIL,SUBNO=2,GENENV=GENENV,   +
               SUB1=(PGMNAME,L'PGMNAME),                               +
               SUB2=(EVNTSUBR,L'EVNTSUBR),                             +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         LA    R15,8
         B     RTNERROR
                        SPACE 3
***********************************************************************
*                                                                     *
*  SUBROUTINE FOR LINKING PIPE READ/WRITE "DECB" LISTS                *
*                                                                     *
*        R2  ==> EVENT    DCB                                         *
*        R5  ==> OUTPUT   PIPE DECB LIST   ADDR                       *
*        R6  ==> READ     PIPE DECB LIST   ANCHOR ADDR                *
*        R13 ==> THREAD   WORK AREA ADDR                              *
*            ==> REGISTER SAVE AREA ADDR                              *
*                                                                     *
***********************************************************************
         USING THRDAREA,R13
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
         using saver,thrdarea
         USING IHADCB,R2
*
code     loctr
LINKPIPE STM   R14,R12,savgrs14   SAVE CALLER'S REGISTERS
*
***********************************************************************
* OBTAIN A SET OF DECB'S FOR "PIPE READ" AND LINK THEM TO "PIPE WRITE"*
***********************************************************************
*
*        The following code updates the read i/o buffer use total and
*        hwm in the THRDMAIN area. We use ENQSTAT to serialise the
*        update.
*
         ENQ (GENEVA,ENQSTAT,E,,STEP),RNL=NO
         if LTR,R15,R15,nz
*          Issue a warning message
           GVBMSG LOG,MSGNO=STATS_ENQ_FAIL,SUBNO=1,GENENV=GENENV,      +
               SUB1=(PGMNAME,L'PGMNAME),                               +
               MSGBUFFER=(PRNTBUFF,L'PRNTBUFF),                        +
               MF=(E,MSG_AREA)
         endif
*
         SR    R7,R7              LOAD "BUFNO" FROM   DCB
         IC    R7,DCBBUFNO
*
         LA    R14,DECBLEN+IOBLEN GET   DECB   POOL  (INCL "IOB")
         LR    R15,R7
         MR    R14,R14
*
         st    r15,EVNTbufps      save size of  buffer pool
         LR    R0,R15
         GETMAIN R,LV=(0)
         ST    R1,0(,R6)          SAVE DECB    POOL   ADDRESS
         LR    R3,R1              LOAD CURRENT DECB   ADDRESS
         st    r1,EVNTbufp        save address of buffer pool
*
l1       using thrdarea,r14
         ly    r14,thrdmain          Get the initial thread area
         ly    r1,l1.read_buffer_tot Get current size of buffers
         ar    r15,r1                add this buffer size
         sty   r1,l1.read_buffer_tot
*
*        check high water mark stats
*
         if cy,r1,gt,l1.read_buffer_hwm If current total > HWM
           sty   r1,l1.read_buffer_hwm  save new HWM
         endif
         drop  l1
*
         DEQ (GENEVA,ENQSTAT,,STEP),RNL=NO
*
         USING DECB,R3
*
LINKDECB LA    R14,DECBLEN+IOBLEN(,R3)  LOAD ADDRESS  OF NEXT DECB
         if cij,r7,le,1           LAST  DECB   ??
           L   R14,0(,R6)         YES - POINT   TO 1ST DECB
         endif
*
         XC    0(DECBLEN+IOBLEN,R3),0(R3) ZERO DECB     AREA
         ST    R14,DECBNEXT       CHAIN   DECB PREFIXES TOGETHER
*
         Larl  R14,MDLREAD                     COPY  MODEL DECB
         MVC   DECBBSAM(MDLREADL),0(R14)
         MVC   DECBSIZE,DCBBLKSI  SAVE  BLOCK  SIZE     IN DECB
         ST    R2,DECBDCB         SAVE  DCB    ADDRESS  IN DECB
         LA    R0,DECB+DECBLEN    SET   IOB    ADDRESS  IN DECB
         ST    R0,DECBIOB
         L     R0,DECBBUFR-DECB(,R5)    BUFFER ADDRESS  IN DECB
         ST    R0,DECBBUFR
         MVC   DECBDDN,GPDDNAME   SET   DDNAME
*
         L     R4,4(,R5)                LOAD   WRITE ECB    CONTENTS
*
         LA    R14,DECB+4+MDLREADL-4    LINK   PIPE DECB'S (WRT/READ)
         LA    R0,4(,R5)
         ST    R0,0(,R14)
         LA    R14,4+MAPWRTL-4(,R5)
         LA    R0,4(,R3)
         ST    R0,0(,R14)
*
LINKRTRY ds    0h
         select CLI,4(R5),EQ
         when (x'00',x'80')       BLOCK FILLED ??
           MVI 4(R3),X'7F'        YES - READY  TO  READ
*
         when X'70'               LAST  BLOCK  ??
           MVI 4(R3),X'70'        YES - COPY   EOF
         endsel
*
         C     R4,4(,R5)          DID   WRITE ECB  CHANGE ???
         BRE   LINKNEXT           NO  - CONTINUE
         L     R4,4(,R5)          YES - RECHECK
         B     LINKRTRY
*
LINKNEXT L     R3,DECBNEXT        LOAD  NEXT DECB  ADDRESS
         L     R5,DECBNEXT-DECB(,R5)    LOAD NEXT  OUTPUT DECB  ADDR
         BCT   R7,LINKDECB        NO  - LOOP THROUGH  ALL DECB'S
                        SPACE 3
         LM    R14,R12,savgrs14   RESTORE CALLER'S REGISTERS (EXIT)
         bsm   0,r14              RETURN
                        SPACE 3
***********************************************************************
* "NOP" - SUBSTITUTE CODE FOR "BSAM READ" ROUTINE                     *
***********************************************************************
P1BR14   Bsm   0,r14                NOP  (RETURN IMMEDIATELY)
                        SPACE 3
***********************************************************************
*                                                                     *
* "READ EXIT" CALL - SUBSTITUTE CODE FOR "BSAM CHECK" ROUTINE         *
*                                                                     *
*    1. BUILD PARAMETER  LIST                                         *
*    2. ALTERNATE BETWEEN TWO BUFFER AREAS                            *
*    3. INITIALIZE CURRENT BUFFER POSITION (RECEND - USED BY GENWRITE)*
*                                                                     *
***********************************************************************
*  CALL AN "LE/COBOL" READ EXIT VIA COMMON LANGUAGE INTERFACE         *
********************************************************************#VB
         DROP  R12
LEREADX  L     R3,EVNTDECB          LOAD  DECB  PREFIX    ADDRESS
         USING DECB,R3
*
         L     R1,THRDRE            LOAD  "RE"  LOGIC TBL ROW ADDR
         LA    R0,LTREPARM-LOGICTBL(,R1)
         sty   R0,GPSTARTA
         LA    R0,LTREWORK-LOGICTBL(,R1)
         sty   R0,GPWORKA
*
         sty   R14,GENPARM2         SAVE  REAL  RETURN    ADDRESS
         sty   R13,GENPARM3         HIDE  WORK  AREA ADDR BEHIND  LIST
         oiy   GENPARM3,X'80'       MARK  END   OF PARAMETER LIST
*
         L     R14,DECBNEXT       SWITCH BUFRS (PREV REC  STILL AVAIL)
         lgf   R1,DECBBUFR-DECB(,R14)
         lgf   R0,DECBBUFR
         ST    R1,DECBBUFR
         ST    R0,DECBBUFR-DECB(,R14)
*
         LA    R0,RETNBSIZ
         sty   R0,GPBLKSIZ
         L     R0,GPBLKMAX
         ST    R0,RETNBSIZ
*
         stg   R1,RETNPTR           INITIALIZE       BUFFER ADDRESS
         STG   R1,RECADDR           Save Buffer address
         LAY   R0,RECADDR           Load address of that location
         STY   R0,GPEVENTA          Save pointer in parameter list
         if TM,DCBRECFM,X'80',no    FIXED/UNDEFINED  LENGTH RECORDS ???
           aghi R1,4
         endif
         stg   R1,RECEND            INITIALIZE NEXT  RECORD ADDRESS
*
         L     R1,CEEWADDR          LOAD  PARAMETER  LIST   ADDRESS
         USING LEINTER,R1
         LAY   R0,PARM_AREA         Address GENPARM
         ST    R0,LEPARMP
         MVC   LESUBADR,EVNTCHKA
*
         L     R15,LECEEADR         CALL  LANGUAGE  INTERFACE  PROGRAM
         Bassm r14,R15
         j     rxreturn
*
         DROP  R1
         DROP  R3
         EJECT
***********************************************************************
*  CALL A "NON LE/COBOL" READ EXIT                                    *
***********************************************************************
P1CALLRX L     R3,EVNTDECB          LOAD  DECB  PREFIX    ADDRESS
         USING DECB,R3
*
         L     R1,THRDRE            LOAD  "RE"  LOGIC TBL ROW ADDR
         LA    R0,LTREPARM-LOGICTBL(,R1)
         sty   R0,GPSTARTA
         LA    R0,LTREWORK-LOGICTBL(,R1)
         sty   R0,GPWORKA
*
         sty   R14,GENPARM2         SAVE  REAL  RETURN    ADDRESS
         sty   R13,GENPARM3         HIDE  WORK  AREA ADDR BEHIND  LIST
         oiy   GENPARM3,X'80'       MARK  END   OF PARAMETER LIST
*
         L     R14,DECBNEXT       SWITCH BUFRS (PREV REC  STILL AVAIL)
         llgt  R1,DECBBUFR-DECB(,R14)
         L     R0,DECBBUFR
         ST    R1,DECBBUFR
         ST    R0,DECBBUFR-DECB(,R14)
*
         LA    R0,RETNBSIZ
         sty   R0,GPBLKSIZ
         L     R0,GPBLKMAX
         ST    R0,RETNBSIZ
*
         stg   R1,RETNPTR           INITIALIZE       BUFFER ADDRESS
         STG   R1,RECADDR           Save Buffer address
         LAY   R0,RECADDR           Load address of that location
         STY   R0,GPEVENTA          Save pointer in parameter list
         if TM,DCBRECFM,X'80',no    FIXED/UNDEFINED  LENGTH RECORDS ???
           aghi R1,4
         endif
         stg   R1,RECEND            INITIALIZE  NEXT RECORD ADDRESS
*
         LAY   R1,PARM_AREA         Address GENPARM
         Llgf  R15,EVNTCHKA
         BASSM R14,R15              CALL   READ EXIT
                        SPACE 3
***********************************************************************
* RETURN FROM READ EXIT                                               *
***********************************************************************
         DROP  R2
RXRETURN L     R11,EVNTDCBA         LOAD  DCB    ADDRESS
         using ihadcb,r11
         L     R3,EVNTDECB          LOAD  DECB   PREFIX    ADDRESS
         L     R4,CEEWADDR          LOAD  EXIT   WORKAREA  ADDRESS
         USING LEINTER,R4
*
         lg    R14,RETNPTR          UPDATE RESULT POINTER
         STG   R14,RECADDR          Save Buffer address
*
         L     R15,RETNCODE         LOAD   RETURN CODE
         LTR   R15,R15              NORMAL COMPLETION ???
         BRC   8,P1CNORML           YES -  CONTINUE               (BZ)
         CHI   R15,8                END-OF-FILE   ???
         BRC   7,P1CERROR           NO  -  BYPASS RETURN          (BNE)
*
         lg    R14,RECEND           EMPTY  BUFFER("GENWRITE" USED) ???
         sgf   R14,DECBBUFR
         cghi  R14,4
         BRC   13,P1CERROR          YES - BRANCH                  (BNH)
*
RXLOCATN larl  r0,p1callrx          get address
         Oilh  R0,x'8000'           SAVE  EXIT PROGRAM ADDRESS (31-BIT)
         ST    R0,EVNTREAD
*
         larl  R0,GENWEOF           NO  - PASS    PARTIAL    BUFFER
         Oilh  R0,x'8000'
         ST    R0,EVNTCHKA
*
         TM    LERECFM,X'80'        FIXED/UNDEFINED  RECORDS ???
         BRC   1,P1CNORML           YES - BYPASS BDW BUILD        (BO)
         L     R1,DECBBUFR          COMPUTE DATA LENGTH
         STH   R14,0(,R1)           BUILD   BDW
         XC    2(2,R1),2(R1)
*
P1CNORML MVI   DECBECB,X'7F'
*
         if CLI,GPRECFMT,eq,C'F'    ADJUST DCB IF EXIT RETURNS RECFM=FB
           NI  DCBRECFM,X'2F'
           OI  DCBRECFM,X'90'
           MVC LERECFM,DCBRECFM
         endif
*
         LG    R6,RECADDR           Load Buffer address from DECB
         TM    LERECFM,X'80'        FIXED/UNDEFINED  LENGTH   ???
         BRC   14,P1CALVAR          NO  - BRANCH  TO VARIABLE LEN (BNO)
*
         lgf   R15,RETNBSIZ         LOAD  BLOCK   LENGTH (RETURNED)
         agr   R15,r6               ADD   BLOCK   LENGTH TO BASE
         stg   R15,EODADDR
         L     R15,GPRECMAX         LOAD  RECORD  LENGTH (FIXED)
         ST    R15,GPRECLEN         RESET CURRENT RECORD LENGTH
*
         llgt  R14,GENPARM2         LOAD  RETURN  ADDRESS
         BSM   0,R14                RETURN
*
P1CALVAR ds    0h
         sam64
         lgh   R15,0(,R6)           LOAD   BLOCK  LENGTH FROM "BDW"
         agr   R15,r6               COMPUTE END-OF-BLOCK ADDRESS
         stg   R15,EODADDR
         llh   R15,4(,R6)           LOAD   RECORD LENGTH FROM "RDW"
         sam31
         ahi   R15,-4               EXCLUDE "RDW"
         ST    R15,GPRECLEN         SAVE  CURRENT RECORD LENGTH
         aghi  R6,8                 SKIP   "BDW" + "RDW"
         STG   R6,RECADDR           Save First Record address
*
         llgt  R14,GENPARM2         LOAD  RETURN  ADDRESS
         BSM   0,R14                RETURN
*
P1CERROR l     r11,dcbdcbe          --> DCBE
         using dcbe,r11
         l     R14,DCBeEODA
         CHI   R15,8                END-OF-FILE   CODE ???
         BER   R14                  BRANCH TO  END-OF-FILE  ADDRESS
*
         l     R14,DCBeSYNA         LOAD  ABNORMAL END ADDR
         BR    R14                  BRANCH TO ABNORMAL-END  ADDRESS
*
         DROP  R11
         DROP  R3
         DROP  R4
         DROP  R13
         EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*     S E T U P   A D D R E S S E S   F O R   N E W   B U F F E R     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING IHADCB,R2
         USING THRDAREA,R13
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
*
NEWBUFFR LR    R9,R14             SAVE RETURN  ADDRESS
*********************************************************************
*        this is branched to from MR95                              *
*        so be careful
*********************************************************************
*
         L     R3,EVNTDECB        LOAD CURRENT DECB   PREFIX ADDRESS
         USING DECB,R3
         L     R1,DECBPREV        PRECEDING   BUFFER  AVAILABLE  ???
         LTR   R1,R1
         BNP   EVNTNXTB           NO  - DON'T START   ANOTHER I/O
*
         XC    DECBPREV,DECBPREV  CLEAR AVAILABLE  INDICATION
         MVC   10(2,R1),DCBBLKSI  RESET BLOCK SIZE IN DECB
         LA    R1,4(,R1)          LOAD  DECB  ADDRESS
         XC    0(4,R1),0(R1)      CLEAR THE   ECB
         llgf  R15,EVNTGETA       CALL  "GET" ROUTINE
         BASSM R14,R15
*
EVNTNXTB LR    R14,R3             SAVE     CURRENT DECB  PREFIX ADDR
         L     R3,0(,R3)          ADVANCE  TO NEXT DECB  PREFIX
         ST    R14,DECBPREV       INDICATE PRECEDING  AVAILABLE
         ST    R3,EVNTDECB
                        SPACE 3
NEWEVNT  EQU   *
*
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=151
*
         LA    R1,4(,R3)          POINT  TO   DECB
         Llgf  R15,EVNTCHKA       I/O    COMPLETED  SUCCESSFULLY ???
         BASSM R14,R15            CALL CHECK ROUTINE (31-BIT)
*
         llgt  R6,16(,R3)         LOAD   BUFFER  ADDRESS  FROM   DECB
         TM    DCBRECFM,X'80'     FIXED/UNDEFINED  LENGTH  ???
         BNO   NEWVAR             NO  - BRANCH TO VARIABLE LENGTH LOGIC
*
         STG   R6,RECADDR         Save First Record address
         llh   R0,DCBLRECL        SAVE   RECORD  LENGTH  (ASSUME FIXED)
         ST    R0,GPRECLEN
         lgh   R15,DCBBLKSI       COMPUTE BLOCK  LENGTH
         L     R14,20(,R3)        LOAD IOB ADDRESS
         lgh   R0,14(,R14)        get RESIDUAL COUNT (CSW)
         sgr   R15,r0             SUBTRACT RESIDUAL COUNT (CSW)
         agr   R15,r6             ADD     BLOCK  LENGTH TO BASE
         stg   R15,EODADDR
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=152
         BSM   0,R9               RETURN
*
NEWVAR   ds    0h
         sam64
         lgh   R15,0(,R6)         LOAD   BLOCK  LENGTH FROM "BDW"
         agr   R15,r6             COMPUTE END-OF-BLOCK ADDRESS
         stg   R15,EODADDR
         llh   R0,4(,R6)          LOAD   RECORD LENGTH FROM "RDW"
         sam31
         ahi   R0,-4              EXCLUDE "BDW" + "RDW"
         ST    R0,GPRECLEN        SAVE  CURRENT RECORD LENGTH
         aghi  R6,8               SKIP    "BDW" + "RDW"
         STG   R6,RECADDR         Save First Record address
*
         BSM   0,R9               RETURN
*
         DROP  R2
         DROP  R3
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
static   loctr
         DS   0D
MODE31   DS   0XL4
OPENPARM DC    XL8'8000000000000000'
P000     DC    PL4'0'
*
GENEVA   DC    CL8'GENEVA  '      MAJOR  ENQ NODE
PGMNAME  DC    CL8'GVBMRBS '      MINOR  ENQ NODE
ENQSTAT  DC    CL8'BUFSTATS'      MINOR  ENQ  NODE
*
LINKPIPA DC    A(LINKPIPE)        LINK PIPES   SUBROUTINE
PIPEGETA DC    A(PIPEGET)         PIPE READ    EXIT
PIPECHKA DC    A(PIPECHK)         PIPE CHECK   EXIT
PIPE     DC    CL4'PIPE'          PIPE DDNAME  PREFIX
*
LECOBOL  DC    CL3'CEE'           COBOL  VERSION  IDENTIFIER
*
RECFMTBL DS   0XL1                RECORD FORMAT
         DC    XL1'00'            0 - UNDEFINED
         DC    XL1'90'            1 - FB
         DC    XL1'50'            2 - VB
         DC    XL1'94'            3 - FBA
         DC    XL1'54'            4 - VBA
RECFMCNT EQU   ((*-RECFMTBL)/(L'RECFMTBL))
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V A R I A B L E S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
IDENTIND DC    C'N'               IDENTIFY ISSUED INDICATOR
                        SPACE 3
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D A T A   C O N T R O L   B L O C K S                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
* GVBDECB maps the DECBs with extra data fields, as used by
*  GVBMRBS and GVBMR95
*
MDLREAD  GVBDECB PRE=MDL,DSECT=N,TYPE=READ,DECB=RDECB
MDLWRT   GVBDECB PRE=MDL,DSECT=N,TYPE=WRITE,DECB=WRDECB

         EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   "G E N W R I T E"  -  F I L L   "S T G M R 9 5"   B U F F E R     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
code     loctr
         USING THRDAREA,R13       REUSE/RETAIN   MAIN THREAD SAVE  AREA
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
         using saver,thrdarea
GENWRITE stm   R14,R12,SAVgrs14
*
         LArl  R12,gvbmrbs        set up static area pointer
         USING (GVBMRBS,code),R12
*
         LR    R4,R1              SAVE PARAMETER LIST     ADDRESS
*
         L     R10,0(,R4)         LOAD "GENPARM" ADDRESS (1ST PARM)
         LHI   R0,parm_area-THRDAREA
         SR    R10,R0
*
         drop  r13
         USING THRDAREA,R10
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
         using saver,thrdarea
*
         L     R2,EVNTDCBA        LOAD "PSEUDO-FILE" DCB  ADDRESS
         USING IHADCB,R2
*
         L     R3,EVNTDECB        LOAD "PSEUDO-FILE" DECB ADDRESS
         USING DECB,R3
*
         lg    R14,RECEND         LOAD  TARGET ADDRESS WITHIN BUFFER
*
         lgh   R1,DCBLRECL       LOAD  RECORD LENGTH (ASSUME FIXED)
         agr   R1,r14             COMPUTE TRIAL  NEXT  RECORD ADDRESS
*
         if TM,DCBRECFM,X'80',z   FIXED/UNDEFINED LENGTH  ???
           L   R15,4(,R4)         LOAD  RECORD LENGTH
           lgh R1,0(,R15)
           agr R1,R14
           aghi R1,4             ADD +4 FOR  RDW
         endif
*
         L     R0,DECBBUFR        LOAD  END-OF-BLOCK ADDRESS
         AH    R0,DCBBLKSI
*
         CR    R1,R0              BUFFER FULL ???
         BH    GENWFULL           YES - PASS  BUFFER   TO  "GVBMR95"
         stg   R1,RECEND          NO  - ADVANCE NEXT RECORD ADDRESS
*
         SR    R0,R1              COMPUTE RESIDUAL VALUE  (UNUSED BUFR)
         STH   R0,WORKAREA+14     SAVE  VALUE   IN PSEUDO  IOB
         LA    R0,WORKAREA
         ST    R0,DECBIOB
*
         TM    DCBRECFM,X'80'     FIXED/UNDEFINED LENGTH ???
         BO    GENWMOVE           YES - BYPASS  BUILDING RDW
         LA    R1,4(,R15)
         STH   R1,0(,R14)
         XC    2(2,R14),2(R14)
         LA    R14,4(,R14)
*
GENWMOVE L     R0,8(,R4)          LOAD SOURCE RECORD ADDRESS
         LR    R1,R15
         MVCL  R14,R0
*
*        llh   R15,DCBLRECL       LOAD SOURCE RECORD LENGTH
*        if  TM,DCBRECFM,X'80',Z  FIXED/UNDEFINED LENGTH ???
*          L   R15,4(,R4)         LOAD SOURCE RECORD LENGTH
*          llh R15,0(,R15)
*        endif
*        L     R9,RECEND          LOAD NEXT   RECORD ADDRESS
*        SR    R9,R15             BACKUP TO   BEGINNING   OF  THIS REC
*        BCTR  R15,0
*        LA    R15,0(R9,R15)
*        SNAP  DCB=SNAPDCB,PDATA=(REGS),ID=200,STORAGE=((R9),(R15))
*
         lm    R14,R12,SAVgrs14
         Bsm   0,r14
         EJECT
***********************************************************************
* PASS FULL BUFFER TO "GVBMR95"                                       *
***********************************************************************
GENWFULL MVI   DECBECB,X'7F'      INDICATE  NORMAL COMPLETION
*
         ST    R13,DECBR13S       SAVE READ EXIT'S SAVE AREA ADDRESS
*
***********************************************************************
*   "FIRST BUFFER" LOGIC  (CALLED FROM "GVBMRBS" FIRST RECORD LOGIC)  *
***********************************************************************
GENWFRST OC    GPRECCNT,GPRECCNT  FIRST BUFFER  ???
         BNZ   GENWNEXT           NO  - TREAT   AS SUBSEQUENT
*
         LArl  R0,GENWRETN        CHANGE CALLED PROGRAM ADDRESS
         Oilh  R0,x'8000'
         ST    R0,EVNTCHKA
*
         LArl  R0,P1CALLRX        OVERRIDE BSAM READ ROUTINE ADDRESS
         Oilh  R0,x'8000'         SAVE  EXIT PROGRAM ADDRESS (31-BIT)
         ST    R0,EVNTREAD
*
***********************************************************************
*   "SUBSEQUENT BUFFER" LOGIC                                         *
***********************************************************************
GENWNEXT LR    R13,R10
*
         lm    R14,R12,SAVgrs14
         BSM   0,R14
*
         DROP  R2
         DROP  R3
         DROP  R10
         DROP  R12
                        SPACE 3
***********************************************************************
*   RETURN TO READ EXIT AFTER CALL TO "GENWRITE"                      *
***********************************************************************
         USING THRDAREA,R13
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
         using saver,thrdarea
GENWRETN stm   R14,R12,SAVgrs14
*
         L     R3,EVNTDECB        LOAD "PSEUDO-FILE" DECB ADDRESS
         USING DECB,R3
*
         L     R13,DECBR13S       RESTORE CALLER'S RSA ADDRESS
         lm    R14,R12,SAVgrs14
         BR    R15                RETRY   MOVE
*
         DROP  R3
         DROP  R13
                        SPACE 3
         USING THRDAREA,R13
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
GENWEOF  L     R1,EVNTDECB        LOAD "PSEUDO-FILE" DECB ADDRESS
         USING DECB,R1
*
         MVI   DECBECB,X'70'      INDICATE END-OF-FILE
*
         Bsm   0,r14              RETURN
*
         DROP  R1
         DROP  R13
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   "G E N P A R M"  -  S E T   E X I T   P A R A M E T E R S         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING THRDAREA,R13       REUSE/RETAIN   MAIN THREAD SAVE  AREA
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
         using saver,thrdarea
GENPARM_routine  stm   R14,R12,savgrs14
*
         LArl  R12,gvbmrbs        set up static area pointer
         USING (GVBMRBS,code),R12
*
         L     R10,0(,R1)         LOAD "ENV INFO" LIST  ADDRESS
         LHI   R0,env_area-THRDAREA
         SR    R10,R0
         drop  r13
         USING THRDAREA,R10
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
         using saver,thrdarea
*
         L     R4,CEEWADDR        LOAD EXIT WORK  AREA  ADDRESS
         USING LEINTER,R4
*
PARMCHKF L     R14,4(,R1)         LOAD RETURNED "RECFM" ADDRESS
         CLI   0(R14),C'F'
         BNE   PARMCHKV
         MVI   LERECFM,X'90'
         MVI   GPRECFMT,C'F'
         B     PARMRECL
*
PARMCHKV CLI   0(R14),C'V'
         BNE   PARMRECL
         MVI   LERECFM,X'50'
         MVI   GPRECFMT,C'V'
*
PARMRECL L     R14,8(,R1)         LOAD RETURNED "LRECL" ADDRESS
         llh   R15,0(,R14)
         LTR   R15,R15
         BNP   PARMEXIT
         ST    R15,LELRECL
         ST    R15,GPRECMAX
*
PARMEXIT ds    0h
         drop  r10
         USING THRDAREA,R13       REUSE/RETAIN   MAIN THREAD SAVE  AREA
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
         using saver,thrdarea
         lm    R14,R12,SAVgrs14   restore registers
         SR    R15,R15
         BSM   0,R14              RETURN
*
         drop  r12,r4
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   "G E N P I P E"  -  R E A D   E V E N T   P I P E                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
GENPIPE  stm   R14,R12,SAVgrs14
*
         LArl  R12,GVBMRBS        SET  PGM BASE  REGISTER  TO BEGINNING
         USING (GVBMRBS,code),R12
*
*
         LR    R4,R1              SAVE PARAMETER LIST      ADDRESS
         LR    R10,R13            SAVE REGISTER  SAVE AREA ADDRESS
*
         L     R13,0(,R4)         LOAD "ENV INFO" ADDRESS (1ST PARM)
         LHI   R0,env_area-THRDAREA
         SR    R13,R0
*
         L     R2,EVNTDCBA        LOAD PIPE DCB  ADDRESS
         USING IHADCB,R2
*
         L     R5,CEEWADDR        LOAD READ EXIT WORKAREA  ADDRESS
         USING LEINTER,R5
*
         MVC   LEREGSAV(72),THRDAREA    TRANSFER SAVE AREA CONTENTS
*
         ST    R10,savprev CHAIN REGISTER SAVE AREAS
*
***********************************************************************
*  SWAP "EODAD" IN DCB TO CATCH END-OF-FILE                           *
***********************************************************************
         l     r3,dcbdcbe         get dcbe address
         using dcbe,r3
         L     R7,DCBEeoda        SAVE ORIGINAL  END-OF-FILE ADDR
         LArl  R14,GENPEOF
         OILH  R14,X'8000'
         ST    R14,DCBEEODA
*
         L     R3,LERDECBA        LOAD PIPE DECB ADDRESS
         USING DECB,R3
*
         LTR   R3,R3              PIPE READ DECB'S ALLOCATED   ???
         BP    GENPREAD           YES - GET NEXT   BLOCK
*
         LA    R6,LERDECBA        BUILD AND LINK   READ/WRITE  DECB'S
         L     R5,LEWDECBA        LOAD  1ST OUTPUT PIPE DECB   ADDR
         L     R15,LINKPIPA
         BASR  R14,R15
*
         L     R5,CEEWADDR        LOAD READ EXIT WORKAREA ADDRESS
         L     R3,LERDECBA        LOAD PIPE DECB ADDRESS
         B     GENPWAIT           WAIT  FOR FIRST  PIPE   BLOCK
                        SPACE 3
GENPREAD MVC   DECBSIZE,DCBBLKSI  RESET BLOCK SIZE IN     DECB
         LA    R1,4(,R3)          LOAD  DECB  ADDRESS
         XC    0(4,R1),0(R1)      CLEAR THE   ECB
         SR    R15,R15
         ICM   R15,B'0111',DCBGETA
         BASR  R14,R15
*
         L     R3,0(,R3)          ADVANCE  TO NEXT DECB   PREFIX
         ST    R3,LERDECBA
*
         CLI   4(R3),X'7F'        I/O ALREADY SUCCESSFULLY COMPLETED ??
         BRE   GENPBLK
*
GENPWAIT LA    R1,DECBECB         WAIT FOR PIPE BLOCK
         SR    R15,R15
         ICM   R15,B'0111',DCBCHCKA
         BASR  R14,R15
*
GENPBLK  L     R6,DECBBUFR        LOAD  BUFFER    ADDRESS  FROM    DECB
*
         L     R14,16(,R4)        PASS  RECORD    LENGTH   TO    CALLER
         llh   R15,DCBLRECL
         STH   R15,0(,R14)
*
         TM    DCBRECFM,X'80'     FIXED/UNDEFINED LENGTH   ???
         BNO   GENPVAR            NO  - BRANCH TO VARIABLE LENGTH LOGIC
*
         L     R14,8(,R4)         PASS  BLOCK  ADDRESS  TO CALLER
         ST    R6,0(,R14)
         llh   R15,DCBBLKSI       COMPUTE BLOCK LENGTH
         L     R14,DECBIOB        LOAD IOB ADDRESS
         SH    R15,14(,R14)       SUBTRACT RESIDUAL COUNT (CSW)
         L     R14,4(,R4)         PASS   BLOCK LENGTH   TO CALLER
         STH   R15,0(,R14)
*
         L     R14,12(,R4)        PASS  RECORD FORMAT   TO CALLER
         MVI   0(R14),C'F'
         B     GENPRC0            RETURN - NORMAL COMPLETION (X'7F')
*
GENPVAR  llh   R15,0(,R6)         LOAD   BLOCK LENGTH   FROM "BDW"
         L     R14,4(,R4)         PASS   BLOCK LENGTH   TO CALLER
         STH   R15,0(,R14)
         L     R14,8(,R4)         PASS   BLOCK ADDRESS  TO CALLER
         ST    R6,0(,R14)
*
         L     R14,12(,R4)        PASS   RECORD FORMAT  TO CALLER
         MVI   0(R14),C'V'
*
GENPRC0  L     R14,20(,R4)        PASS   RETURN CODE TO CALLER (EOF)
         SR    R0,R0
         STH   R0,0(,R14)
*
GENPEXIT DS    0H                 RESTORE  ORIGINAL  END-OF-FILE ADDR
         l     r14,dcbdcbe
         using dcbe,r14
         ST    R7,DCBEeoda        RESTORE  ORIGINAL  END-OF-FILE ADDR
         drop  r14
         MVC   0(72,R13),LEREGSAV RESTORE  ORIGINAL  CONTENTS
         Lr    R13,R10            RESTORE  R13
         lm    R14,R12,SAVgrs14
         bsm   0,r14              RETURN
                        SPACE 3
GENPEOF  L     R14,20(,R4)        PASS   RETURN CODE TO CALLER (EOF)
         LA    R0,8
         STH   R0,0(,R14)
*
         B     GENPEXIT
*
         DROP  R2
         DROP  R3
         DROP  R5
         DROP  R13
         DROP  R12
         EJECT
***********************************************************************
*                                                                     *
*  EXIT CODE FOR PIPING DATA FROM ONE THREAD TO ANOTHER               *
*                                                                     *
*        R1  ==> DECB                                                 *
*        R13 ==> THRDAREA                                             *
*                                                                     *
***********************************************************************
         USING THRDAREA,R13
         using genenv,env_area
         using genparm,parm_area
         using genfile,file_area
         using savf4sa,thrdarea
*
PIPEGET  stmg  R14,R12,SAVF4SAG64RS14
*
         LR    R2,R1
*
***********************************************************************
*  CHECK FOR "END-OF-FILE" BEFORE PROCEEDING                          *
***********************************************************************
         CLI   EOFEVNT,C'Y'       LAST BLOCK  PROCESSED  ???
         BNE   PIPEPOST
*
         using decbecb,r2         map the DECB
         L     R14,DECBDCB get the DCB ptr
         using ihadcb,r14
         l     r14,dcbdcbe        get the DCBE pointer
         using dcbe,r14
         l     R15,DCBeEODA       get EOD pointer
         drop  r14
         stg   R15,SAVF4SAG64RS14
         B     PUTEXIT
*
***********************************************************************
*  SET "WRITE BUFFER" STATUS TO AVAILABLE (WAKE UP WRITE ROUTINE)     *
***********************************************************************
PIPEPOST L     R1,MDLREADL-4(,R2) LOAD  WRITE EXIT   DECB ADDRESS
         Llilf R0,X'7F000000'     LOAD  COMPLETION   CODE
         SR    R15,R15            ZERO  REGISTER
         CS    R15,R0,0(R1)       THREAD WAITING ???
         BRE   PUTEXIT            NO  - MARK  BUFFER AVAILABLE
         POST  (1),(0)
*
***********************************************************************
*  RETURN                                                             *
***********************************************************************
PUTEXIT  lmg   R14,R12,SAVF4SAG64RS14
         bsm   0,r14              RETURN
                        EJECT
PIPECHK  stmg  R14,R12,SAVF4SAG64RS14
*
         LR    R2,R1
*
***********************************************************************
*  CHECK FOR "END-OF-FILE" BEFORE PROCEEDING                          *
***********************************************************************
         CLI   EOFEVNT,C'Y'       LAST BLOCK  PROCESSED  ???
         BRE   PIPEEOF
*
PIPEWAIT TM    0(R2),X'40'
         BO    CHKRETRN
*
         WAIT  ECB=(2)            WAIT FOR  DATA BLOCK
*
***********************************************************************
*  RETURN                                                             *
***********************************************************************
CHKRETRN L     R14,DECBDCB                COMPUTE RESIDUAL LENGTH
         using ihadcb,r14
         llh   R0,DCBBLKSI
         L     R15,MDLREADL-4(,R2)
         SH    R0,DECBSIZE-DECBECB(,R15)
         L     R15,DECBIOB
         STH   R0,14(,R15)
*
         CH    R0,DCBBLKSI                EMPTY   BLOCK    ???
         BL    CHKEXIT                    NO  -   PROCESS
*
PIPEEOF  L     R14,DECBDCB
         l     r14,dcbdcbe
         using dcbe,r14
         l     R15,DCBeEODA
         drop  r14
         stg   R15,SAVF4SAG64RS14
*
CHKEXIT  lmg   R14,R12,SAVF4SAG64RS14
*
         if CLI,0(R1),eq,x'70'        LAST    BLOCK   ???
           MVI EOFEVNT,C'Y'       INDICATE  EOF
         endif
         bsm   0,R14              RETURN
*
         DROP  R13
         EJECT

         DCBD  DSORG=PS
         IHADCBE
         ihasaver
*
GVBMRBS  CSECT
static   loctr
*
         LTORG
*
         END