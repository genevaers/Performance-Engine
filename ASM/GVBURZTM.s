GVBURZTM TITLE 'CPU time formatter'
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
****START OF SPECIFICATIONS *******************************************
*                                                                     *
* MODULE NAME: GVBURZTM                                               *
*                                                                     *
* DESCRIPTIVE NAME: CPU time reporter                                 *
*                                                                     *
* FUNCTION: Formats and prints the CPU times for enclaves             *
*                                                                     *
* NOTES:                                                              *
*                                                                     *
*   DEPENDENCIES: NONE                                                *
*                                                                     *
*   RESTRICTIONS: NONE                                                *
*                                                                     *
*   REGISTER CONVENTIONS: STANDARD                                    *
*                                                                     *
* MODULE TYPE: CSECT                                                  *
*                                                                     *
*   PROCESSOR: ASSEMBLER                                              *
*                                                                     *
*   MODULE SIZE: between 1K and 2K bytes                              *
*                                                                     *
*   ATTRIBUTES: REENTRANT, REUSABLE                                   *
*                                                                     *
* ENTRY POINT: GVBURZTM                                               *
*                                                                     *
*   PURPOSE: SEE FUNCTION                                             *
*                                                                     *
*   LINKAGE: BALR R14,R15                                             *
*                                                                     *
*   INPUT:                                                            *
*                                                                     *
*     REGISTERS: Standard z/OS call structure                         *
*                R1 -> parmlist                                       *
*                                                                     *
*        Parmlist is a list of four pointers                          *
*                                                                     *
*        Pointer1 --> +ve OPEN DCB - RECFM FA LRECL 133               *
*                     zero ---> use WTOs to output information        *
*                                                                     *
*        Pointer2 --> Ziiptime (8 byte TOD value)                     *
*                     Total Ziip eligible time                        *
*                                                                     *
*        Pointer3 --> Ziiponcptime (8 byte TOD value)                 *
*                     Total Ziip eligible time processed on a CP      *
*                                                                     *
*        Pointer4 --> Enclave total cpu time (8 byte TOD value)       *
*                     Grand total enclave cpu time (CP + ZIIP)        *
*                                                                     *
*   Should be called via the following z/OS CALL macro                *
*                                                                     *
*        Call  gvburztm,(<dcba>,<ziiptimea>,<ziiponcptime>,<enctime>) *
*                                                                     *
*   OUTPUT:                                                           *
*                                                                     *
*     REGISTERS:                                                      *
*                                                                     *
*       UNCHANGED REGISTERS: R0 - R14                                 *
*                                                                     *
*       OUTPUT REGISTERS: r15                                         *
*                                                                     *
*       UNPREDICTABLE REGISTERS: NONE                                 *
*                                                                     *
*     OTHER: Report printed                                           *
*                                                                     *
*   EXIT-NORMAL =                                                     *
*      RETURN CODE (R15) =                                            *
*         0 Normal completion                                         *
*                                                                     *
*   EXIT-ERROR = N/A                                                  *
*                                                                     *
*   ABEND CODES = NONE                                                *
*                                                                     *
*   MESSAGES ISSUED: NONE                                             *
*                                                                     *
* EXTERNAL REFERENCES:                                                *
*                                                                     *
*   PROCEDURES INVOKED: NONE                                          *
*                                                                     *
*   CONTROL BLOCKS:                                                   *
*                                                                     *
*     SYSTEM CONTROL BLOCKS: NONE                                     *
*                                                                     *
*     PRODUCT CONTROL BLOCKS:                                         *
*                                                                     *
* INTERNAL TABLES:                                                    *
* MACROS ISSUED:                                                      *
*                                                                     *
*   SYSTEM MACROS: Storage                                            *
*                                                                     *
*   PRODUCT MACROS:                                                   *
*                                                                     *
****END OF SPECIFICATIONS *********************************************
GVBURZTM CSECT
GVBURZTM AMODE 31                 Set addressing mode to 31
GVBURZTM RMODE ANY                and residency to any
         BRU   SAVEREG
URZTMEYE GVBEYE GVBURZTM
*
         print off,noprint
         SYSSTATE ARCHLVL=2
         COPY  ASMMSP
LEAVE    OPSYN ASM_LEAVE
         asmmrel on
         asmdreg
.*  Redefine LTORG so that it uses a special LOCTR
LTORG_   OPSYN LTORG
*
         MACRO
&NAME    LTORG ,
         LCLC  &CSECT
&CSECT   SETC  '&SYSECT'
&CSECT._static LOCTR
&NAME    LTORG_ ,
&csect.  loctr
         Mend
         print on,noprint
         DS   0H
SAVEREG  STM   R14,R12,12(R13)    Save Caller's register in caller
*                                 Provided R13 Save area
         cnop  0,4
         bras  r12,past_adcon
         dc    a(gvburztm_static_start)
past_adcon l   r12,0(,r12)        Get the static area address
         USING gvburztm_static_start,r12
gvburtzm_static loctr
gvburztm_static_start ds 0d
gvburztm loctr
*
         lm    r7,r10,0(r1)       Load up the passed parameters
*        so after here
*        r7 is the address of our open DCB and r8, 9 and 10 are
*        pointers to ziiptime, ziiponcptime and enc_cptime respectively
*
         STORAGE OBTAIN,LENGTH=WORKLEN,COND=NO  Our savearea
*
         ST    R13,4(,R1)         Save caller's save area in my
*                                 save area (backward chain)
         ST    R1,8(,R13)         Save my save area address in caller's
*                                 save area (forward chain)
         LR    R13,R1             Point to our save area
         using workarea,r13
         xc    dblwork,dblwork    clear this out now
*
*        format and print results here
*
*        Copy templates to working storage

         mvc  line1_rdw(line1txt_len),line1txt_rdw
         mvc  line2_rdw(line2txt_len),line2txt_rdw
         mvc  line3_rdw(line3txt_len),line3txt_rdw
         mvc  line4_rdw(line4txt_len),line4txt_rdw
         mvc  line5_rdw(line5txt_len),line5txt_rdw

*        convert the values from TOD clock form to binary

         la   r1,stckconveform       address the execute form
         STCKCONV STCKVAL=(8),       the ToD clock input value         +
               CONVVAL=bin_line1,    unsigned binary FW output value   +
               TIMETYPE=BIN,         Binary time                       +
               DATETYPE=YYYYDDD,     Julian date                       +
               MF=(E,(1))             and execute form
onziip_time using tod_dsect,bin_line1
         l     r3,onziip_time.date   0YYYYDDD  packed with no sign
         nilf  r3,x'00000FFF'        00000DDD
         sll   r3,4                  0000ddd0
         oilf  r3,x'0000000f'        0000dddf
         st    r3,dblwork+4          save it
         cvb   r3,dblwork            get the value in binary
         if    ahi,r3,-1,p           do we have more than 1 day?
dayof001_s equ  60*60*24*100     create number of hundreths in a day   +
                ss mm hh .01
           m     r2,=a(dayof001_s)       create number of days in units+
                                         of 0.01 seconds
           al    r3,onziip_time.seconds add on part of day
           st    r3,onziip_time.seconds    and store new total
         endif

         la   r1,stckconveform       address the execute form
         STCKCONV STCKVAL=(9),                                         +
               CONVVAL=bin_line2,                                      +
               TIMETYPE=BIN,                                           +
               DATETYPE=YYYYDDD,     Julian date                       +
               MF=(E,(1))
cpziip_time using tod_dsect,bin_line2
         l     r3,cpziip_time.date   0YYYYDDD  packed with no sign
         nilf  r3,x'00000FFF'        00000DDD
         sll   r3,4                  0000ddd0
         oilf  r3,x'0000000f'        0000dddf
         st    r3,dblwork+4          save it
         cvb   r3,dblwork            get the value in binary
         if    ahi,r3,-1,p           do we have more than 1 day?
           m     r2,=a(dayof001_s)       create number of days in units+
                                         of 0.01 seconds
           al    r3,cpziip_time.seconds add on part of day
           st    r3,cpziip_time.seconds    and store new total
         endif

         la   r1,stckconveform       address the execute form
         STCKCONV STCKVAL=(10),                                        +
               CONVVAL=bin_line5,                                      +
               TIMETYPE=BIN,                                           +
               DATETYPE=YYYYDDD,     Julian date                       +
               MF=(E,(1))
enclave_time using tod_dsect,bin_line5
         l     r3,enclave_time.date  0YYYYDDD  packed with no sign
         nilf  r3,x'00000FFF'        00000DDD
         sll   r3,4                  0000ddd0
         oilf  r3,x'0000000f'        0000dddf
         st    r3,dblwork+4          save it
         cvb   r3,dblwork            get the value in binary
         if    ahi,r3,-1,p           do we have more than 1 day?
           m     r2,=a(dayof001_s)       create number of days in units+
                                         of 0.01 seconds
           al    r3,enclave_time.seconds add on part of day
           st    r3,enclave_time.seconds   and store new total
         endif

         l     r3,onziip_time.seconds
         al    r3,cpziip_time.seconds  line3 = line1 + line2

         l     r4,enclave_time.seconds
         slr   r4,r3                 line4 = line5 - line3

         st    r3,bin_line3
         st    r4,bin_line4

         la    r2,bin_line1          point to first input value
         la    r3,value_line1          and first output value

         la    r5,convtod_input      point to the parmlists
         la    r6,convtod_output

         do    from=(r4,5)           repeat 5 times
           mvc   convtod_input,convtod_input_data  set up the parmlist
timein     using timebin,convtod_input    and map it
           mvc   timein.timebin,0(r2)  save input time in convtod parm
           drop  timein
           la    r1,convtodeform       address the parm

           convtod CONVVAL=(5),        input area                      +
               TODVAL=(6),             output area                     +
               TIMETYPE=BIN,           time value is BIN (0.01 s)      +
               DATETYPE=YYYYMMDD,        (date type - not used)        +
               MF=(E,(1))

*          time is now in TOD clock form in area addressed by r6

           la   r1,stckconveform       address the execute form
           STCKCONV STCKVAL=(6),       Input TOD value                 +
               CONVVAL=(3),            output area                     +
               MF=(E,(1))

*          output from stckconv is   HHMMSSthmiju0000
*          make the values valid packed numbers
           using (stckconvtime,stckconvtime+l'value_line1),r3          +
                  map the area carefully to avoid misuse of r3
           oi  stckconvtime+l'stckconvtime-1,X'0f'

*              shift and round -     shift right by 7 places, truncate
*                Truncating to avoid any possible propogation as the
*                input field is      HHMMSSthmiju000F
           srp stckconvtime,64-7,0
*                output is           0000000HHMMSSthF
           l   r1,stckconvdate       0YYYYDDD  packed with no sign
           nilf r1,x'00000FFF'       00000DDD
           sll r1,4                  0000ddd0
           oilf r1,x'0000000f'       0000dddf
           st  r1,dblwork+4          save it
           cvb r1,dblwork            get the value in binary
           if  ahi,r1,-1,p           check number of days > 1
             do from=(r1)
               ap stckconvtime,=p'24000000' add 24 hours for each day
             enddo
           endif

           ahi r2,l'bin_line1        move to next input
           ahi r3,l'value_line1        and output
         enddo
         drop r3

*        Now format these times into the output text
line1    using output_line,line1_out    map the output lines
line2    using output_line,line2_out
line3    using output_line,line3_out
line4    using output_line,line4_out
line5    using output_line,line5_out

*        move the pattern into the output lines
         mvc   line1.time,timepatt
         mvc   line2.time,timepatt
         mvc   line3.time,timepatt
         mvc   line4.time,timepatt
         mvc   line5.time,timepatt
*        and now edit in the time - missing the first 3 bytes/6digits
*                                   as they are all zero
         ed    line1.time,value_line1+3
         ed    line2.time,value_line2+3
         ed    line3.time,value_line3+3
         ed    line4.time,value_line4+3
         ed    line5.time,value_line5+3

*        set up values to calculate the percentage with two decimal
*        points - hence the multiply by 10000

         la    r1,bin_line1               point at the first value
         l     r5,bin_line5                 and set up divisor
         la    r2,line1_percent               and point to output

         do    from=(r3,5)                repeat for 5 values
           l     r9,0(,r1)                get the value
           m     r8,=f'10000'             fix r8 and multiply by 10000 +
                                          so that we get percentages
           if    ltr,r5,r5,nz             check for zero
             dr    r8,r5                    if ok, do the divide
           endif

           cvd   r9,0(,r2)                convert and send out

           ahi   r1,l'bin_line1           move r1
           ahi   r2,l'line1_percent        and r2

         enddo


*        and format these percentages into the output lines

         mvc   line1.percent,pcntpatt
         mvc   line2.percent,pcntpatt
         mvc   line3.percent,pcntpatt
         mvc   line4.percent,pcntpatt
         mvc   line5.percent,pcntpatt
         ed    line1.percent,line1_percent+5  data is at the right
         ed    line2.percent,line2_percent+5
         ed    line3.percent,line3_percent+5
         ed    line4.percent,line4_percent+5
         ed    line5.percent,line5_percent+5

         drop  line1,line2,line3,line4,line5

*        and write it all out

         if    ltr,r7,r7,nz       then we have a DCB so use PUTs

           put   (7),hdr1txt_rdw

           put   (7),hdr2txt_rdw

           put   (7),line1_rdw

           put   (7),line2_rdw

           put   (7),dashtxt_rdw

           put   (7),line3_rdw

           put   (7),line4_rdw

           put   (7),dashtxt_rdw

           put   (7),line5_rdw

           PUT   (7),hdr2txt_rdw
         else ,
*          here when r7 is zero - so use WTOs
*          set up the execute form first
           mvc   wtoeform(wtolform_l),wtolform

           lhi   r1,l'hdr1txt      Setup
           sth   r1,hdr1txt_wto      all
           sth   r1,hdr2txt_wto      the
           sth   r1,dashtxt_wto      WTO
           sth   r1,line1_rdw        lengths
           sth   r1,line2_rdw
           sth   r1,line3_rdw
           sth   r1,line4_rdw
           sth   r1,line5_rdw

           la    r1,wtoeform      and address it
           xr    r0,r0             clear r0
           la    r7,hdr1txt_wto
           wto   TEXT=(7),MF=(E,(1))   and away we go

           la    r1,wtoeform      and address it
           xr    r0,r0             clear r0
           la    r7,hdr2txt_wto
           wto   TEXT=(7),MF=(E,(1))   and away we go

           la    r1,wtoeform      and address it
           xr    r0,r0             clear r0
           mvc   line1_wto,line1txt_wto
           la    r7,line1_wto
           wto   TEXT=(7),MF=(E,(1))   and away we go

           la    r1,wtoeform      and address it
           xr    r0,r0             clear r0
           mvc   line2_wto,line2txt_wto
           la    r7,line2_wto
           wto   TEXT=(7),MF=(E,(1))   and away we go

           la    r1,wtoeform      and address it
           xr    r0,r0             clear r0
           la    r7,dashtxt_wto
           wto   TEXT=(7),MF=(E,(1))   and away we go

           la    r1,wtoeform      and address it
           xr    r0,r0             clear r0
           mvc   line3_wto,line3txt_wto
           la    r7,line3_wto
           wto   TEXT=(7),MF=(E,(1))   and away we go

           la    r1,wtoeform      and address it
           xr    r0,r0             clear r0
           mvc   line4_wto,line4txt_wto
           la    r7,line4_wto
           wto   TEXT=(7),MF=(E,(1))   and away we go

           la    r1,wtoeform      and address it
           xr    r0,r0             clear r0
           la    r7,dashtxt_wto
           wto   TEXT=(7),MF=(E,(1))   and away we go

           la    r1,wtoeform      and address it
           xr    r0,r0             clear r0
           mvc   line5_wto,line5txt_wto
           la    r7,line5_wto
           wto   TEXT=(7),MF=(E,(1))   and away we go

           la    r1,wtoeform      and address it
           xr    r0,r0             clear r0
           la    r7,hdr2txt_wto
           wto   TEXT=(7),MF=(E,(1))   and away we go

         endif
*
         lr    r1,r13             get the storage address into r1
         l     r13,4(,r13)        get the previous savearea
         STORAGE RELEASE,ADDR=(1),LENGTH=WORKLEN
         lm    r14,r12,12(r13)    restore the registers
         xr    r15,r15            clear the return code
         BSM   0,R14              RETURN (SWITCH TO 31-BIT MODE)
         eject ,
* this is the static area, addressable by r12.
gvburztm_static Loctr
timepatt dc    x'402020207A20207A21204B2020'
*                  H H H : M M : S S . t h
pcntpatt dc    x'402021204B2020'
*                  9 9 9 . 9 9

hdr1txt_rdw dc y(hdr1txt_len)
hdr1txt_wto dc y(0)
*19720
hdr1txt  dc c'Description                  HHHH:MM:SS.hh  Percent'
hdr1txt_len equ *-hdr1txt_rdw

hdr2txt_rdw dc y(hdr2txt_len)
hdr2txt_wto dc y(0)
hdr2txt  dc c'===========================  -------------  -------'
hdr2txt_len equ *-hdr2txt_rdw

dashtxt_rdw dc y(dashtxt_len)
dashtxt_wto dc y(0)
dashtxt  dc c'                             -------------  -------'
dashtxt_len equ *-dashtxt_rdw

line1txt_rdw dc y(line1txt_len)
line1txt_wto dc y(0)
line1txt dc c'zIIP-eligible time on zIIP    hhh:mm:ss.th   999.99'
*                       1         2         3         4         5
*             012345678901234567890123456789012345678901234567890
line1txt_len equ *-line1txt_rdw

line2txt_rdw dc y(line2txt_len)
line2txt_wto dc y(0)
line2txt dc c'zIIP-eligible time on CP      hhh:mm:ss.th   999.99'
line2txt_len equ *-line2txt_rdw

line3txt_rdw dc y(line3txt_len)
line3txt_wto dc y(0)
line3txt dc c'zIIP-eligible time            hhh:mm:ss.th   999.99'
line3txt_len equ *-line3txt_rdw

line4txt_rdw dc y(line4txt_len)
line4txt_wto dc y(0)
line4txt dc c'Other time                    hhh:mm:ss.th   999.99'
line4txt_len equ *-line4txt_rdw

line5txt_rdw dc y(line5txt_len)
line5txt_wto dc y(0)
line5txt dc c'Total enclave CPU time        hhh:mm:ss.th   999.99'
*                       1         2         3         4         5
*             012345678901234567890123456789012345678901234567890
line5txt_len equ *-line5txt_rdw

wtolform wto TEXT=,MF=L
wtolform_l equ *-wtolform
stckconv_lform stckconv MF=L
stckconv_lform_l  equ *-stckconv_lform
         ds 0d
convtod_input_data ds 0xl(convtod_input_length)
timebin  dc 2f'0'     1 8-byte area with 0
         dc x'19000101'   Input date in YYYYMMDD form
         dc f'0'
convtod_input_length equ *-timebin
gvburztm Loctr
output_line dsect
banner   ds cl29
time     ds cl(l'timepatt)
         ds cl2
percent  ds cl(l'pcntpatt)

tod_dsect dsect
seconds   ds f        converted time of day in 0.01 units
          ds f        zero for TIME=BIN
date      ds f        0YYYYDDD unsigned packed decimal
          ds f        not used
workarea dsect
savearea ds   18f
bin_line1   ds    xl16
bin_line2   ds    xl16
bin_line3   ds    xl16
bin_line4   ds    xl16
bin_line5   ds    xl16
value_line1 DS   0CL16
stckconvtime ds   CL8
Stckconvdate ds   cl8
value_line2 DS    CL16
value_line3 DS    CL16
value_line4 DS    CL16
value_line5 DS    CL16
            ds    0d             fix alignment of next field
line1_percent DS    pl8
line2_percent DS    pl8
line3_percent DS    pl8
line4_percent DS    pl8
line5_percent DS    pl8
line1_rdw     ds   h
line1_wto     ds   h
line1_out     ds   cl133
line2_rdw     ds   h
line2_wto     ds   h
line2_out     ds   cl133
line3_rdw     ds   h
line3_wto     ds   h
line3_out     ds   cl133
line4_rdw     ds   h
line4_wto     ds   h
line4_out     ds   cl133
line5_rdw     ds   h
line5_wto     ds   h
line5_out     ds   cl133
         ds   0d
wtoeform ds   xl(wtolform_l)
         ds   0d
stckconveform ds   xl(stckconv_lform_l)
         ds   0d
convtodeform  convtod MF=L
         ds   0d
convtod_input ds   xl(convtod_input_length)
convtod_output ds  2f
dblwork  ds   d
         ds   0d
worklen  equ  *-workarea
gvburztm Loctr
         ltorg
         END
