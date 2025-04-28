**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2009, 2022.
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
*  GVBUTHDR - Header format function program for assembler and cobol  *
*             SAFR modules                                            *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*           0 - Successful                                            *
*           8 - Buffer overflow                                       *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - Return code                                            *
*                                                                     *
*        R14 - Return address                                         *
*                                                                     *
*        R13 - Register save area address (Work area base register)   *
*                                                                     *
*        R12 - Program base register                                  *
*                                                                     *
*        R11 - temporary work register                                *
*                                                                     *
*        R10 - Not used                                               *
*                                                                     *
*        R9  - temporary work register                                *
*                                                                     *
*        R8  - temporary work register                                *
*                                                                     *
*        R7  - temporary work register                                *
*                                                                     *
*        R6  - Parameter list HEADERPR                                *
*                                                                     *
*        R5  - Record count
*                                                                     *
*        R4  - Buffer storage location                                *
*                                                                     *
*        R3  - temporary work register                                *
*                                                                     *
*        R2  - temporary work register                                *
*                                                                     *
*        R1  - temporary work register                                *
*                                                                     *
***********************************************************************
                        EJECT
GVBUTHDR TITLE 'GenevaERS - header function'

         COPY GVBHDR
         print off,nogen,noprint
         COPY ASMMSP
LEAVE    OPSYN ASM_LEAVE
         asmmrel on
         sysstate archlvl=2
         PRINT on,GEN,noprint
GVBUTHDR CSECT
GVBUTHDR AMODE 31                 Set addressing mode to 31
GVBUTHDR RMODE ANY                and residency to any
         USING GVBUTHDR,R12
         BRU   SAVEREG
UTHDREYE GVBEYE GVBUTHDR
*
         DS   0H
SAVEREG  STM   R14,R12,12(R13)    Save Caller's register in caller
*                                 Provided R13 Save area
         LR    R12,R15            Set up program base register
*
         LR    R6,R1
*
         STORAGE OBTAIN,          get some storage for workarea        +
               LENGTH=worklen,                                         +
               COND=NO,           Unconditional                        +
               CHECKZERO=YES      ask system to tell us if it's cleared
         if (chi,r15,ne,x'14')    X'14' signals the storage is clean
           LR R2,R1               ZERO WORK  AREA
           LHI R3,worklen
           XR R14,R14
           XR R15,R15
           MVCL R2,R14
         endif
         ST    R13,4(,R1)         Save caller's save area in my
*                                 save area (backward chain)
         ST    R1,8(,R13)         Save my save area address in caller's
*                                 save area (forward chain)
         LR    R13,R1             Point to our save area
         USING WORKAREA,R13
         USING HEADERPR,R6
         L     R3,BUFFADD         LOAD BUFFER ADDRESS TO ADDRESS
         ST    R3,BUFFER          LOCATION BUFFER
*
***********************************************************************
*  WRITE BUFFER LENGTH CHECK.                                         *
***********************************************************************
*
         L     R3,BUFFLGTH        LOAD BUFFER LENGTH TO REGISTER 3
         MVC   bufferln,0(R3)     STORE BUFFER LENGTH TO 'BUFFERLN'
         if Clc,bufferln,ge,=a(minbuff)  CHECK FOR SUFFICIENT LENGTH
*
***********************************************************************
*    Initialise buffer to blanks                                      *
***********************************************************************
*
           xr   r2,r2             set source address to zero
           iilf 3,x'40000000'     make the pad X'40' and zero length
           L   R8,BUFFER
           L   R9,bufferln
           MVCL R8,R2             Move blanks to buffer area

           L   R4,BUFFER           LOAD  OUTPUT  ADDRESS TO R4
           XR  R5,R5              Init record count
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 1                                  *
***********************************************************************
*
           lhi r9,l'hdr01+8        Total length of output line data
           MVC 2(L'HDR01,R4),HDR01    COPY  TEXT
           l   r3,Rptddn           Point to length of pgm name
           mvc 2+l'hdr01(8,r4),0(r3) Move in report DDname
           sth r9,0(r4)            Save the length of output data
           la  r4,2(r9,r4)         Where to put next output data
           AHI R5,1                Add to record count
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 2  Blank line                      *
***********************************************************************
*
           lhi r9,l'hdr02          Blank line is 1 blank
           MVC 2(L'HDR02,R4),HDR02 Copy text
           sth r9,0(r4)            Save its length (1)
           la  r4,2(r9,r4)         Where to put next output data
           AHI R5,1                Add to record count
*
***********************************************************************
*    CONTROL REPORT HEADING - loop through title lines                *
***********************************************************************
*
           LA  R2,HDR_TITLE        Start of title lines
           LA  R3,HDR_TITLE_LEN(,R2) End of title lines

           do until=(CR,r2,ge,r3)
*          loop until end of title lines
             lh   r9,0(r2)         length of output line
             ahi  r9,1             plus 2 for len field minus 1 for EX
             exrl r9,MVCTITLE      copy title line to buffer
             la r4,1(r9,r4)        Where to put next output data
             la r2,1(r9,r2)        Next title line
             AHI R5,1              Add to record count
           enddo
*
* MVCTITLE   MVC  0(0,R4),0(R2)    Copy title line incl. 2 byte length
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 5                                  *
***********************************************************************
*
           MVC 2(L'HDR05,R4),HDR05    COPY  TEXT
*
           l   r3,Pgmtypel        Point to program type length
           lh  r9,0(,r3)          Get the program type length
           l   r3,Pgmtype         Point to pgm type
           bctr r9,r0             Decrement for "EX"
           ex  r9,Mvetype         "Base Product" or other type
           ahi r9,l'hdr05+1       Get total length of output line
           sth r9,0(r4)
           la  r4,2(r9,r4)        Where to put next output data

Mvetype    mvc l'hdr05+2(0,r4),0(r3) Move for report type

           AHI R5,1                Add to record count
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 6                                  *
***********************************************************************
*
           lhi r9,l'hdr06
           MVC 2(L'HDR06,R4),HDR06 Copy text
           sth r9,0(r4)            Save its length
           la  r4,2(r9,r4)         Where to put next output data
           AHI R5,1                Add to record count
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 7  Blank line                      *
***********************************************************************
*
           lhi r9,l'hdr07          Blank line is 1 blank
           MVC 2(L'HDR07,R4),HDR07 Copy text
           sth r9,0(r4)            Save its length (1)
           la  r4,2(r9,r4)         Where to put next output data
           AHI R5,1                Add to record count
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 8                                  *
***********************************************************************
*
           MVC 2(L'HDR08,R4),HDR08    COPY  TEXT
*
           l   r3,Pgmnamel        Point to program type length
           lh  r9,0(,r3)          Get the program type length
           l   r3,Pgmname         Point to pgm type
           bctr r9,r0             Decrement for "EX"
           ex  r9,Mvestd          "Base Product" or other type
           ahi r9,l'hdr08+1       Get total length of output line
           sth r9,0(r4)
           la  r4,2(r9,r4)        Where to put next output data
           AHI R5,1                Add to record count

Mvestd     mvc l'hdr08+2(0,r4),0(r3) Move for report typre

*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 9                                  *
***********************************************************************
*
           MVC 2(L'HDR09,R4),HDR09    COPY  TEXT
*
           l   r3,Pgm_title_ln    Point to program type length
           lh  r9,0(,r3)          Get the program type length
           l   r3,Pgm_title       Point to pgm type
           bctr r9,r0             Decrement for "EX"
           ex  r9,Mvestd          Move in pgm name
           ahi r9,l'hdr09+1       Get total length of output line
           sth r9,0(r4)
           la  r4,2(r9,r4)        Where to put next output data
           AHI R5,1                Add to record count
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 10                                 *
***********************************************************************
*
*          try to obtain the date and time that the module was built
*          from the binder
           mvc 2(l'hdr10,r4),hdr10    Copy text
           bras r10,binddate
           if (ltr,r15,r15,nz)    then binder call failed  -  so use
             la    r8,executed      time of assembly
             USING BLDTIME,R8
*
             MVC  l'hdr10+2+0(4,R4),bldYEAR  format the date/time into
             mvi  l'hdr10+2+4(r4),c'-'  this format: 2008-01-01 00:00
             MVC  l'hdr10+2+5(2,R4),bldMNTH
             mvi  l'hdr10+2+7(r4),c'-'
             MVC  l'hdr10+2+8(2,R4),bldDAY
             MVC  l'hdr10+2+11(2,R4),bldHR
             mvi  l'hdr10+2+13(r4),c':'
             MVC  l'hdr10+2+14(2,R4),bldMIN
             lhi  r9,l'hdr10+16   length of variable data
             sth r9,0(r4)         Save its length
             la   r4,2(r9,r4)     Where to put next output data
             AHI R5,1             Add to record count
             DROP R8
           else
             LA R11,ZONEDDTE      LOAD ADDRESS OF ZONED DATE TO R11
             using exedate,r11
             UNPK ZONEDDTE,0(16,R8) Unpack the execute dateZONE DECIMAL
             MVC  l'hdr10+2+0(4,r4),exeyear+1  Move to right offset
             mvi  l'hdr10+2+4(r4),c'-'         2008-01-01 00:00
             MVC  l'hdr10+2+5(2,r4),exemnth+1
             mvi  l'hdr10+2+7(r4),c'-'         2008-01-01 00:00
             MVC  l'hdr10+2+8(2,r4),exeday+1
             drop r11
*
             LA R11,ZONEDTME      Get address of zoned time
             using exetime,r11
             UNPK ZONEDTME,0(4,R8) unpack time
             MVC  l'hdr10+2+11(2,r4),exehr+1  move time to right offset
             mvi  l'hdr10+2+13(r4),c':'
             MVC  l'hdr10+2+14(2,r4),exemin+1
             mvi  l'hdr10+2+16(r4),c':'
             MVC  l'hdr10+2+17(2,r4),exesec+1
             lhi  r9,L'HDR10+19   length of variable data
             sth r9,0(r4)         Save its length
             la   r4,2(r9,r4)     Where to put next output data
             AHI R5,1                Add to record count
             drop r11
           endif
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 11 Blank line                      *
***********************************************************************
*
           lhi r9,l'hdr11          Blank line is 1 blank
           MVC 2(L'HDR11,R4),HDR11 Copy text
           sth r9,0(r4)            Save its length (1)
           la  r4,2(r9,r4)         Where to put next output data
           AHI R5,1                Add to record count
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 12                                 *
***********************************************************************
*
*
*
           MVC 2(L'HDR12,R4),HDR12   COPY LINE 9 TEMPLATE TO BUFFER
*
           LA R3,TMEDTE           INITIALIZE SYSTEM EXE TIME
           TIME DEC,(3),DATETYPE=YYYYMMDD,                             +
               LINKAGE=SYSTEM,MF=(E,TIMELIST)
*
           LA  R11,ZONEDDTE       LOAD ADDRESS OF ZONED DATE TO R11
           using exedate,r11
           UNPK ZONEDDTE,0(16,R3) UNPACK THE EXE DATE TO ZONE DECIMAL
           MVC  l'hdr12+2+0(4,R4),exeyear+1 Move date to right offset
           mvi  l'hdr12+2+4(r4),c'-'  this format: 2008-01-01 00:00
           MVC  l'hdr12+2+5(2,R4),exemnth+1 in line 12
           mvi  l'hdr10+2+7(r4),c'-'
           MVC  l'hdr12+2+8(2,R4),exeday+1
           drop r11
*
           LA  R11,ZONEDTME       LOAD ADDRESS OF ZONED TIME TO R11
           using exetime,r11
           UNPK ZONEDTME,0(4,R3)  UNPACK THE EXE TIME TO ZONE DECIMAL
           MVC  l'hdr12+2+11(2,R4),exehr+1   Move time to right offset
           mvi  l'hdr10+2+13(r4),c':'
           MVC  l'hdr12+2+14(2,R4),exemin+1  in line 12
           mvi  l'hdr10+2+16(r4),c':'
           MVC  l'hdr12+2+17(2,R4),exesec+1
           lhi  r9,L'HDR12+19   length of variable data
           sth r9,0(r4)         Save its length
           la   r4,2(r9,r4)     Where to put next output data
           AHI R5,1                Add to record count
           drop r11
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 13 Blank line                      *
***********************************************************************
*
           lhi r9,l'hdr13          Blank line is 1 blank
           MVC 2(L'HDR13,R4),HDR13 Copy text
           sth r9,0(r4)            Save its length (1)
           la  r4,2(r9,r4)         Where to put next output data
           AHI R5,1                Add to record count
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 14                                 *
***********************************************************************
*
*
           lhi r9,l'hdr14+8        Total length of line
           mvc 2(l'hdr14,R4),hdr14 COPY LINE 14 TEMPLATE TO BUFFER
           l   r3,Rptddn           Point to length of pgm name
           mvc l'hdr14+2(8,r4),0(r3) Move in report DDname
           sth r9,0(r4)            Save its length (1)
           la  r4,2(r9,r4)         Where to put next output data
           AHI R5,1                Add to record count
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 15                                 *
***********************************************************************
*
*
           MVC 2(L'HDR15,R4),HDR15   COPY LINE 9 TEMPLATE TO BUFFER
           l   r3,Rpt_title_ln    Point to program type length
           lh  r9,0(,r3)          Get the program type length
           l   r3,Rpt_title       Point to pgm type
           bctr r9,r0             Decrement for "EX"
           ex  r9,Mvestd          Move in pgm name
           ahi r9,l'hdr15+1       Get total length of output line
           sth r9,0(r4)
           l   r3,Pgmnamel        Point to program type length
           lh  r15,0(,r3)         Get the program type length
           l   r3,Pgmname         Point to pgm type
           bctr r15,r0            Decrement for "EX"
           ex  r15,Mvestd         "Base Product" or other type
           la  r4,2(r9,r4)        Where to put next output data
           AHI R5,1                Add to record count
*
***********************************************************************
*    CONTROL REPORT HEADING - LINE 16 Blank line                      *
***********************************************************************
*
           lhi r9,l'hdr16          Blank line is 1 blank
           MVC 2(L'HDR16,R4),HDR16 Copy text
           sth r9,0(r4)            Save its length (1)
           la  r4,2(r9,r4)         Where to put next output data
           AHI R5,1                Add to record count
*
           l   r3,Rpt_reccnt      Get where to pt record count
*                                 Number of records in heading
           sth r5,0(r3)           give it back to user
         else
*
           LA  R15,BUFFOVR
         endif
*
         LR    R1,R13
         L     R13,4(,R13)        Restore caller's save area address
         STORAGE RELEASE,LENGTH=WORKLEN,ADDR=(1) Our savearea
*
         L     R14,12(,R13)       Restore caller's register
         LM    R0,R12,20(R13)     Restore caller's register
         Bsm   0,r14              Return

binddate ds    0h
***********************************************************************
*                                                                     *
*        Routine to use the fast data interface to the binder         *
*        to extract the bind date and time of the module              *
*                                                                     *
***********************************************************************

 do ,                              bracketing do
   load EP=IEWBFDAT                locate the binder interface
   st r0,BINDFDA                   and save that entry point

   csvquery INADDR==a(*),          use any address in the module       +
               OUTEPTKN=EPTOKEN,   ask for the eptoken                 +
               plistver=2,         version 2 added outeptkn            +
               MF=(E,CSVQUERY)
   doexit (ltr,r15,r15,nz)

*  SQ      -  start query with binder using query token
*  Mtoken  -  returned token from binder for this query
*  eptoken -  module token from csvquery above

   l  r15,bindfda
   CALL (15),(SQ,MTOKEN,EPTOKEN),VL,                                   +
               MF=(E,PARMLIST)               Call fast data.
   doexit (ltr,r15,r15,nz)

   IEWBUFF FUNC=GETBUF,TYPE=IDRB   Get an IDRB buffer
   IEWBUFF FUNC=INITBUF,TYPE=IDRB    and intialise it

*  GD      -  Get data
*  Mtoken  -  token from binder for this query (from SQ call)
*  B_IDRB_Vstring -  h'6',C'B_IDRB'      data class requested
*  Iewbidb_base   -  base register for buffer
*  Cursor  -  cursor in items (not used in this case)
*  Count   -  count of entries returned (should be 1 in this case)

   l  r15,bindfda
   CALL (15),(GD,MTOKEN,B_IDRB_VSTRING,0,(IEWBidb_BASE),CURSOR,        +
               COUNT,0),VL,MF=(E,PARMLIST)
   doexit (chi,r15,gt,4)           return code 0 or 4 are ok

*  here we have a buffer with the IDR data in it
   xc   tmedte,tmedte                 clear it all out
   pack indate(5),idb_date_bound(8)   remove the zones as we get
   mvi  indate+l'indate,x'00'          clean the junk out
   pack intime(4),idb_time_bound(7)    the data out of the buffer
   mvi  intime+3,x'00'                 clean the junk out

   l  r15,bindfda
   CALL (15),(EN,MTOKEN),VL,       end the binder dialog               +
               MF=(E,PARMLIST)             Call fast data.
*  and return the buffer now

   IEWBUFF FUNC=FREEBUF,type=IDRB

   convtod convval=tmedte,         input is date time from binder      +
               todval=output_tod,  output is 4 byte TOD clock          +
               mf=(E,convtod)

   stckconv stckval=output_tod,    TOD clock value from above          +
               convval=tmedte,     area to format the output           +
               datetype=YYYYMMDD,  same format as &sysdatc             +
               mf=(E,stckconv)

   la    r8,tmedte
   xr    r15,r15

 enddo

 br r10
*
         TITLE 'Data Areas, Constants, and Tables'
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
SQ       DC C'SQ',X'0001'
GD       DC C'GD',X'0001'
EN       DC C'EN',X'0001'
***********************************************************************
* B_IDRB class name represented as vstring                            *
***********************************************************************
B_IDRB_VSTRING        DC H'6',C'B_IDRB'
MVCTITLE MVC  0(0,R4),0(R2)   Copy title line incl. 2 byte length
MVEPGM   MVC 1(0,R4),0(R3)
BUFFOVR  EQU   8              BUFFER OVERFLOW ERROR
MINBUFF  equ   1200           15 lines in header message
LNLENGTH equ   81             Character length per line is 81
BLNKOFF  equ   81             Use to offset counter to next line
*
DCHAR    equ   C'D'         Insert character D to build info
MCHAR    equ   C'M'         Insert character M to build info
OCHAR    equ   C'O'         Insert character O to build info
PM4TYPE  DC    C'Base Product'
PMHTYPE  DC    C'H Extension'
PMSTYPE  DC    C'S Extension'
*
EXECUTED DC    CL8'&SYSDATC'      Date
         DC    CL8'&SYSTIME'      Time
*
         COPY HDRINFO
bldTIME  DSECT
bldyear  ds    CL4
bldmnth  ds    CL2
bldday   ds    CL2
bldhr    ds    Cl2
dash     ds    CL1
bldmin   ds    CL2
*
EXEdate  DSECT
exeyear  ds    CL4
exemnth  ds    CL2
exeday   ds    cL2
*
EXETIME  DSECT
exehr    ds    cl2
exemin   ds    cl2
exesec   ds    cl2
*
GVBUTHDR CSECT
         IEWBUFF TYPE=IDRB,               IDRB buffer                  +
               FUNC=MAPBUF,               create mappings              +
               size=1,                    one record                   +
               headreg=2,                 header register 2            +
               entryreg=11                entry register 11
*
WORKAREA DSECT
*
SAVEAREA DS    18A                18 fullword save area
*
BUFFER   DS    A        Store buffer address
BUFFERLN DS    F        Store buffer length
*
TMEDTE   DS   0CL16     TIME AND DATE RETURNED
intime   ds    cl8      time
indate   ds    cl4      date
         ds    cl4      * must be zero *
ZONEDTME DS    CL8      Zoned decimal for executed time
ZONEDDTE DS    CL16     Zoned deciaml for executed date
*
TIMELIST TIME LINKAGE=SYSTEM,MF=L
*
bindfda  ds    a        entry point for Binder Fast Data access
         CSVQUERY plistver=2,MF=(L,CSVQUERY)
convtod  ds   0x
         convtod  mf=L
stckconv ds   0x
         stckconv mf=L
output_tod ds  fd
PARMLIST DS 32F     Common area for passing parameters.
MTOKEN   DS F       Current session identifier.
CURSOR   DS F       Fast data cursor position.
COUNT    DS F       Number of entries obtained.
EPTOKEN  DS CL8     CSVQUERY token.
RETCODE  DS F       Return code.
RSNCODE  DS F       Reason code.
WORKLEN  EQU   *-WORKAREA
         END
