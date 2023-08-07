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
***********************************************************************
*                                                                     *
* SAFR Message Build Services                                         *
*                                                                     *
* This program will build a SAFR Message, substituting optional text  *
* strings, in a user-supplied buffer.                                 *
*                                                                     *
*   INPUT?                                                            *
*      PARAMETER RECEIVED = MSGNUM - message number                   *
*      HOW PASSED: Calling program                                    *
*                                                                     *
*      PARAMETER RECEIVED = BUFFADDR - buffer address                 *
*      HOW PASSED: Calling program                                    *
*                                                                     *
*      PARAMETER RECEIVED = BUFFLEN - buffer length                   *
*      HOW PASSED: Calling program                                    *
*                                                                     *
*      PARAMETER RECEIVED = MSG#SUB - number of substitution strings  *
*      HOW PASSED: Calling program                                    *
*                                                                     *
*      PARAMETER RECEIVED = MSGS1PTR - address of string 1            *
*      HOW PASSED: Calling program                                    *
*                                                                     *
*      PARAMETER RECEIVED = MSGS1LEN - length of string 1             *
*      HOW PASSED: Calling programN                                   *
*                                                                     *
*      PARAMETER RECEIVED = MSGS2PTR - address of string 2            *
*      HOW PASSED: Calling program                                    *
*                                                                     *
*      PARAMETER RECEIVED = MSGS2LEN - length of string 2             *
*      HOW PASSED: Calling programN                                   *
*                                                                     *
*      PARAMETER RECEIVED = MSGS3PTR - address of string 3            *
*      HOW PASSED: Calling program                                    *
*                                                                     *
*      PARAMETER RECEIVED = MSGS3LEN - length of string 3             *
*      HOW PASSED: Calling programN                                   *
*                                                                     *
*   EXIT-NORMAL =                                                     *
*      RETURN CODE (R15) =                                            *
*         0 Normal completion - Message in buffer                     *
*                                                                     *
*      SEVERITY CODE of Message Just Built (R0) =                     *
*         0 - I(nformation)                                           *
*         4 - W(arning)                                               *
*         8 - E(rror)                                                 *
*        12 - S(evere)                                                *
*        16 - C(ritical)                                              *
*        20 - U(nrecoverable)                                         *
*                                                                     *
*   EXIT-ERROR =                                                      *
*      RETURN CODE (R15) =                                            *
*         4 - No text for Substitution Variable - Message in buffer   *
*         8 - Buffer Overflow - Truncated Message in Buffer           *
*        12 - Message Number NOT Found - Message 000s in Buffer       *
*        16 - Message Number NOT Found -                              *
*                Truncated Message 000s in Buffer                     *
*        20 - Buffer NOT Specified                                    *
*                                                                     *
***********************************************************************
*
         Copy  GVBASSRT
         Copy  GVBMR95W
         Copy  GVBMR95C
         Copy  GVBX95PA
         Copy  GVBMR95L
         Copy  GVB0200B
         Copy  GVBMRZPE        ZIIP function equates
         Copy  GVBLOGIT
*
GVBUTMSG TITLE 'SAFR - message services'
GVBUTMSG CSECT
GVBUTMSG AMODE 31                 Set addressing mode to 31
GVBUTMSG RMODE ANY                and residency to any
         BRU   SAVEREG
UTMSGEYE GVBEYE GVBUTMSG
*
         DS   0H
         print off
         sysstate ARCHLVL=2
         COPY ASMMSP
LEAVE    OPSYN ASM_LEAVE
         asmmrel on
         IEABRCX DEFINE
         print on
static   loctr
code     loctr
SAVEREG  STM   R14,R12,12(R13)    Save Caller's register in caller
*                                 Provided R13 Save area
         LR    R12,R15            Set up program base register
         USING (GVBUTMSG,code),R12
*
         LR    R8,R1
*        USING PARMLIST,R8
         USING MBSLIST,R8
*
         STORAGE OBTAIN,LENGTH=MSGWLEN,COND=NO  Our savearea
         XC    0(MSGWLEN,R1),0(R1)              clear
*
         ST    R13,4(,R1)         Save caller's save area in my
*                                 save area (backward chain)
         ST    R1,8(,R13)         Save my save area address in caller's
*                                 save area (forward chain)
         LR    R13,R1             Point to our save area
         using MSGWORK,r13
*
         LA    R0,SEVEREC         Default REASON CODE -severe (12)
*                                 Passing parm list from caller
*        L     R3,MSGNUM          Load msg # into R3
*        ST    R3,MBSMSG#         Store msg number to MBSMSG#
*        L     R3,MSGPFX          Load prefix address
*        ST    R3,MBSPFX          Store msg prefix address
*        L     R3,BUFFADDR        Load buffer address into R3
*        ST    R3,MBSBADDR        Store buffer address to MBSBADDR
*        L     R3,BUFFLEN         Load buffer length into R3
*        ST    R3,MBSBLEN         Store buffer lenght to MBSBLEN
*        L     R3,MSG#SUB         Load msg sub into R3
*        ST    R3,MBS#SUB         Store msg sub to MBSBADDR
*
*        L     R3,MSGS1PTR        Load address of msg string 1 into R3
*        ST    R3,MBSS1PTR        Store msg string 1 to MBSS1PTR
*        L     R3,MSGS1LEN        Load length of msg string 1 into R3
*        ST    R3,MBSS1LEN        Store msg length 1 to MBSS1PTR
*        L     R3,MSGS2PTR        Load address of msg string 2 into R3
*        ST    R3,MBSS2PTR        Store msg string 2 to MBSS1PTR
*        L     R3,MSGS2LEN        Load length of msg string 2 into R3
*        ST    R3,MBSS2LEN        Store msg length 2 to MBSS1PTR
*        L     R3,MSGS3PTR        Load address of msg string 3 into R3
*        ST    R3,MBSS3PTR        Store msg string 3 to MBSS1PTR
*        L     R3,MSGS3LEN        Load length of msg string 3 into R3
*        ST    R3,MBSS3LEN        Store msg length 3 to MBSS1PTR
*        L     R3,MSGS4PTR        Load address of msg string 4 into R3
*        ST    R3,MBSS4PTR        Store msg string 4 to MBSS1PTR
*        L     R3,MSGS4LEN        Load length of msg string 4 into R3
*        ST    R3,MBSS4LEN        Store msg length 4 to MBSS1PTR
*
CHKBUFAD LA    R15,MBSNOBUF       Error Code for NO BUFFER SPECIFIED
         L     R10,MBSBUFFA       Buffer address
         LTR   R10,R10            Check its specified
         BRZ   GETBUFF            No buffer supplied, get one.
*
         LT    R11,MBSBUFFL       Insert buffer length to R11
         JNZ   GOTBUFF
*
GETBUFF  DS    0H
         STORAGE OBTAIN,LENGTH=MSGMAX,COND=NO  Our savearea
         LR    R10,R1
         LHI   R11,MSGMAX
         ST    R10,MBSBUFFA
         ST    R11,MBSBUFFL
*
GOTBUFF  DS    0H
         XR    R14,R14            Zero source address
         LA    R15,C' '           One  blank
         SLA   R15,24             Shift left to pad character
         MVCL  R10,R14            Set R10 field to blank
         L     R10,MBSBUFFA       Refresh buffer address
*
         lt    r11,mbspfx         Is there a message prefix?
         JZ    PFXDFLT            No, the default is GVB
*
         CLC   0(3,R11),=CL3'GVB'
         JE    PFXDFLT
         MVC   MBSmsg_tab_name(3),0(R11) Build message table name
         MVC   MBSmsg_tab_name+3(5),=cl5'UTMUE' Build msg table name
         LOAD  EPLOC=MBSmsg_tab_name
         ST    R0,MBSmsg_tab_addr
         LR    R11,R0
         J     DIRSTART
*
PFXDFLT  DS    0H
         USING MDSHEAD,R11        Map Message Table address
         L     R11,=V(GVBUTMUE)   Load message table address to R11
DIRSTART DS    0H
         L     R3,MBSNUM          Load message number to R3
         L     R4,MDSDIRA         Start of directory
         L     R5,MDSDIRND        End of directory
         LA    R6,MSGDIRL         Load length of directory to R6
*
         LA    R15,MBSNOFND       Error code for message # not found
         USING MSGDIR,R5          Map message directory entry
DIRLOOP  SR    R5,R6              Subtract, back to start of entry
         CR    R5,R4              Compare, before the start of Table?
         BRL   BADMSG#            Yes, invalid mesage number
         C     R3,MSGDIR#         Compare R3 with message number
         BRL   DIRLOOP            Low - try previous
         L     R6,MDSSTRT          Load address of first message
         A     R6,MSGDIRO         Offset to this meesage group - add
         DROP  R5
*
         USING MSGFORM,R6         Map Message Directory entry
GRPLOOP  CLC   MSG#,FFFF          Is it end of table?
         BRE   BADMSG#            Yes, inclaid message number
         C     R3,MSG#            Finding msg# in the table
         BRL   BADMSG#            No, invalid message number
         BRE   FOUND              Yea :) go process it
         XR    R5,R5              Clear work register
         IC    R5,MSGEL           Insert length of message element
         AR    R6,R5              Bump to next message
         BRU   GRPLOOP            Go check next message
BADMSG#  L     R6,MDS000          Issue message 000
         L     R4,MBSBUFFL        Buffer length
*        MVI   0(R10),X'00'       Internal print control
         MVI   0(R10),c' '        Internal print control: blank
         LA    R10,1(,R10)        Space over it
         BCTR  R4,0               Decrement space remaining
         XR    R5,R5              Clear work register
         IC    R5,MDSPRFLN        Length of prefix
         CR    R4,R5              Check length
         BRL   FINEXIT            buffer length less than prefix
         BCTR  R5,0               make zero origin
         EX    R5,MOVEPRE         move in prefix
         LA    R5,1(,R5)          Original length
         SR    R4,R5              Length of buffer remaining
         AR    R10,R5             Pointer to next byte
         IC    R5,MSGML           Length of message
         LA    R3,MSGTEXT         Start of message
         LA    R2,Mdsdigits+1+1   Length of msg# + code + blank
         CR    R2,R4              Check length
         BRH   OFLOZERO           No good, BUFFER OVERFLOW
         MVC 0(Mdsdigits+1+1,R10),0(R3) Move msg# + code + blank
         SR    R4,R2              Length of buffer remaining
         AR    R10,R2             Pointer to next byte
         SR    R5,R2              Length of message remaining
         AR    R3,R2              Pointer to next byte
         BRU   OKTOMOVE           OK, go move it
OFLOZERO BCTR  R4,0               Make zero origin
         EX    R4,MOVETXT         Move in message text
         LA    R15,MBSNOFOF       Message 000, and OVERFLOW!
         BRU   FINEXIT            Go exit
OKTOMOVE BCTR  R5,0               Make zero origin
         EX    R5,MOVETXT         Move in message text
         LA    R5,1(,R5)          Reset to one origin
         SR    R4,R5              Length of buffer remaining
         AR    R10,R5             Pointer to the next byte
         LA    R2,Mdsdigits       Number of digits in message
         CR    R4,R2              Will it fit?
         BRL   FINEXIT            No, go EXIT
         L     R3,MBSNUM          Pickup message number
         CVD   R3,DTEMP           Convert to packed
         UNPK  DTEMP((mdsdigits*2)-1),DTEMP+L'DTEMP-mdsdigits+1(mdsdigi+
               ts)
         MVC   0(mdsdigits,R10),DTEMP Move in message number
         AR    R10,R2             Pointer to next byte
         BRU   FINEXIT            Go EXIT
static     loctr
MOVEPRE  MVC   0(*-*,R10),MDSPRTXT Move prefix to buffer
MOVETXT  MVC   0(*-*,R10),0(R3)   Move message text to buffer
code       loctr
*
FOUND    LA    R15,MBSNOBUF       Load error code for no buffer
         L     R4,MBSBUFFL        Buffer length
         XC    0(4,R10),0(R10)    Clear RDW area
         MVI   4(R10),c' '        Internal print control: blank
         LA    R10,5(,R10)        Point past RDW and CC
         AHI   R4,-5              Decrement space remaining
         XR    R5,R5              Clear work register
         IC    R5,MDSPRFLN        Length of prefix
         CR    R4,R5              Compare buffer and length of prefix
         BRL   FINEXIT            less than prefix
         BCTR  R5,0               Make zero origin
         EX    R5,MOVEPRE         Move in prefix
         LA    R5,1(,R5)          Original length
         SR    R4,R5              Buffer length remaining
         AR    R10,R5             Pointer to the next byte
         IC    R5,MSGML           Length of msg
         LA    R3,MSGTEXT         Start of message
         DROP  R6                 No longer need message information
         XR    R2,R2              Clear work register
         TRT mdsdigits(1,R3),SEVTAB Translate to severtiy code
         LR    R0,R2              Save it for return
         LA    R2,Mdsdigits+1+1   Length of msg# + code + blank
         CR    R2,R4              Check length with buffer
         BRH   OFLOMOVE           Bahh, buffer overflow
         MVC 0(Mdsdigits+1+1,R10),0(R3) Move msg# + code + blank
         SR    R4,R2              Length of buffer remaining
         AR    R10,R2             Pointer to next byte
         SR    R5,R2              Length of message remaining
         AR    R3,R2              Pointer to next byte
         BCTR  R5,0               Make message length zero origin
         XR    R15,R15            Clear return code
*
LOOPAMP  EX    R5,FINDAMP         Go search for ampersnad
static   loctr
FINDAMP  TRT   0(*-*,R3),AMPTAB   Search for ampersand
code     loctr
         BRZ   CHECKLEN           Not found, go move message
         LR    R6,R1              Address of ampersand
         SR    R6,R3              Length of field to move
         BRZ   SUBPARM            Zero, go substitute string
         CR    R4,R6              Room left in output buffer?
         BRL   OFLOMOVE           No, move as much as possible!
         BCTR  R6,0               Make zero origin
         EX    R6,MOVETXT         Move in message text
         LA    R6,1(,R6)          Original length
         SR    R4,R6              Length of buffer remaining
         SR    R5,R6              Length of message remaining
         AR    R10,R6             Pointer to next byte in buffer
         LR    R3,R1              Address of ampersand
*
SUBPARM  XR    R2,R2              Clear work register
         TRT   1(1,R3),NUMTAB     Character after ampersand
         BRZ   NOSUB              Not valid numberic 1-9
         L     R1,MBS#SUB         Number of symbols to substitute
         CR    R1,R2              Number of sub fields < # in msg txt
         BRL   NOSUBMSG           Yes, go set flag and continue
         BCTR  R2,0               Make substitute parm. no. 0 origin
         SLL   R2,3               Mutliply by 8 to get pointer
         L     R7,MBSS1PTR(R2)    Pointer to substitute field
         LTR   R7,R7              Zero?
         BRZ   NOSUBMSG           Yes, go set flag and continue
         L     R2,MBSS1LEN(R2)    Length of substitute field
         LTR   R2,R2              Zero?
         BRZ   NOSUBMSG           Yes, go set flag and continue
         CR    R2,R4              Will it fit in buffer?
         BRH   OFLOSUBS           No, move as much as possible
         BCTR  R2,0               Make subtitute ext length 0 origin
         EX    R2,MOVESUB         Move substitute text
static   loctr
MOVESUB  MVC   0(*-*,R10),0(R7)   Move substitute text to buffer
code     loctr
         LA    R2,1(,R2)          Original length
         SR    R4,R2              Length of buffer remaining
         AR    R10,R2             Pointer to next byte in buffer
         LA    R2,2               Ampersand + number
*
NEXTPARM SR    R5,R2              End of message text
         BRM   FINEXIT            Yes, EXIT
         AR    R3,R2              Step past ampersand + number
         BRU   LOOPAMP            Go look for next ampersand
NOSUBMSG LA    R15,MBSNOSUB       Error code for no substitute parm.
NOSUB    LA    R2,1               Length of ampersand
         CR    R2,R4              Will it fit in buffer?
         BRH   OFLOEXIT           No, set R15 and EXIT
         MVI   0(R10),C' '        Move in blanks
         AR    R10,R2             Pointer to next byte in buffer
         MVI   0(R10),C' '        Move in blanks
         AR    R10,R2             Pointer to next byte in buffer
         MVI   0(R10),C' '        Move in blanks
         AR    R10,R2             Pointer to next byte in buffer
         LA    R2,3               Length of ampersand
         BRU   NEXTPARM           Step past ampersand and continue
*
OFLOSUBS LR    R3,R7              Point to substitute text
OFLOMOVE LTR   R4,R4              Zero length
         BRZ   OFLOEXIT           Yes, buffer overflow
         BCTR  R4,0               Make zero origin
         BRU   OFLOMOV2           Go move as much as possible
*
CHECKLEN LTR   R4,R4              Zero length?
         BRZ   OFLOEXIT           Yes, buffer overflow
         BCTR  R4,0               Make buffer length zero orgin
         BRU   OKTOMOV2           Ok, go move it
OFLOMOV2 EX    R4,MOVETXT         Move in message text
         LA    R10,1(R4,R10)      Point to next byte in buffer
OFLOEXIT LA    R15,MBSBUFOF       Error Code for buffer overflow
         BRU   FINEXIT            Go exit
OKTOMOV2 EX    R5,MOVETXT         Move in message text
         LA    R10,1(R5,R10)      Point to next byte in buffer
*
FINEXIT  DS    0h
         L     R9,MBSBUFFA        Address start of buffer again
         SR    R10,R9             Subtract start address
         STH   R10,0(,R9)         Save message length in RDW
         AHI   R10,-4
         ST    R10,MBSBUFFL       Save true message length
*
* Message formatting complete here.
*
         If CLI,MBSTYPE,eq,C'L'   Write to LOG request?
*   check we can write to the log. If not switch to WTO
           USING GENENV,R2
           USING THRDAREA,R3
           IF (LT,R2,MBSGENV,Z),or,                                    +
               LT,R3,GP_THRD_WA,Z,or,                                  +
               LT,R1,LOGFDCBA,z

             if (LT,R1,MBSDCBA,Z) Log DCB address supplied (GVBMR88)?
               MVI MBSTYPE,C'W'   no - WTO
             endif

           endif
         endif
*
         If CLI,MBSTYPE,eq,C'L'   Write to LOG request?
* If this is called from GVBMR95...
*   address the thread work area and check we are in TCB mode
           IF (LT,R2,MBSGENV,NZ)
             USING GENENV,R2
             L  R3,GP_THRD_WA     Address thread workarea
             DROP R2
             USING THRDAREA,R3
             if CLI,THREAD_MODE,EQ,C'S' Are we in SRB MODE?
               if ltgf,r15,workazip,nz
                 lgr r4,r13           save address of MSG WA
                 lgr r13,r3           use thread work area
                 la r1,TCB_switch     Switch to TCB mode
                 bassm r14,r15    Call zIIP module
                 lgr r13,r4       restore MSG WA
                 OI MSGFLAG1,MSGSWTCH Remember that we switched
               else
                 B LOGEXIT        Can't write to log (something wrong)
               endif
             endif
             LY R1,LOGFDCBA       Address the DCB
             ST R1,MBSDCBA        Save in parm list
           endif

* Write to the log
           ENQ (GENEVA,LOGNAME,E,,STEP),RNL=NO

           Lgf R1,MBSDCBA         Address the DCB
           L    R9,MBSBUFFA       Address the message
           PUT (1),(9)

           DEQ (GENEVA,LOGNAME,,STEP),RNL=NO
*
           If TM,MSGFLAG1,MSGSWTCH,nz,and, Did we switch to TCB mode?  +
               CLI,THREAD_MODE,EQ,C'P' and we in TCB MODE?
             ltgf r15,workazip
             lgr r4,r13                save address of MSG WA
             lgr r13,r3                use thread work area
             la    r1,SRB_switch       Switch back to SRB mode
             bassm r14,r15             Call zIIP module
             lgr r13,r4                restore MSG WA
           endif
           DROP R3
         else
* WTO
           If CLI,MBSTYPE,eq,C'W'   Write to operator?
             MVC MBSWTO(MBSWTOL),mdlwto
             L   R9,MBSBUFFA        Address message buffer
             ahi r9,2               WTO requires 2 byte length prefix
             ahi r9,4               truncate first 4 bytes " ** "
             ahi r10,-4             reduce length
             sth r10,0(r9)          save message length
             WTO TEXT=(R9),MF=(E,MBSWTO)
           endif
         endif
LOGEXIT  DS    0h

*
* return code based on severity type of message
*
         LR    R1,R13
         L     R13,4(,R13)        Restore caller's save area address
         STORAGE RELEASE,LENGTH=MSGWLEN,ADDR=(1) Our savearea
*
         L     R14,12(,R13)       Restore caller's register
         LM    R0,R12,20(R13)     Restore caller's register
         bsm   0,R14              Return
*
         TITLE 'Data Areas, Constants, and Tables'
static   loctr
MSGMAX   EQU   164                MAXIMUM message length
SEVEREC  EQU   12                 Severe error code
MBSOK    EQU   0                  OK - Message in buffer
MBSNOSUB EQU   4                  No text for substitution variable
*                                 - message in buffer (+ &VAR)
MBSBUFOF EQU   8                  Buffer overflow
*                                 - message in buffer truncated
MBSNOFND EQU   12                 Message number not found in table
*                                 - message 000 in buffer
MBSNOFOF EQU   16                 Message number not found in table
*                                 - no space for message 000 in buffer
MBSNOBUF EQU   20                 Buffer not specified
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
GENEVA   DC    CL8'GENEVA  '          MAJOR  ENQ  NODE
LOGNAME  DC    CL8'MR95LOG '
*
FFFF     DC    X'FFFFFFFF'        Severe error code
AMPTAB   DC    80X'00',X'FF',175X'00' Search for ampersand
NUMTAB   DC    241X'00',X'010203040506070809',6X'00' Check for 1 to 9
SEVTAB   DC    195X'FF'           Severity code table
         DC    X'10'              C - critical
         DC    X'FF'              Invalid
         DC    X'80'              E-error
         DC    3X'FF'             Invalid
         DC    X'00'              I-information
         DC    11X'FF'            Invalid
         DC    X'02'              N-notice
         DC    12X'FF'            Invalid
         DC    X'0C'              S-severe
         DC    X'FF'              Invalid
         DC    X'14'              U-unrecoverable
         DC    X'FF'              Invalid
         DC    X'04'              W-warning
         DC    25X'FF'            Invalid
*
MDLWTO   WTO   TEXT=(R2),MF=L     MODEL WTO
         ltorg
code     loctr
MBSLIST  GVBMSG DSECT,PREFIX=MBS
*
* Message work area
*
MSGWORK  DSECT
         DS    18A                18 fullword save area
*
DTEMP    DS    D                  Doubleword temp work area
*
MSGFLAG1 DS    X
MSGSWTCH EQU   X'80'              We switched from SRB to TCB
*
MSGFLAG2 DS    X
*
MSGFLAG3 DS    X
*
MSGFLAG4 DS    X
*
*MBSMSG#  DS    F                  Message number
*MBSPFX   DS    F                  Message prefix pointer
*MBSBADDR DS    A                  Address of buffer to input message
*MBSBLEN  DS    F                  Length of buffer to input message
*MBS#SUB  DS    F                  Number of symbols to substitute
*MBSS1PTR DS    A                  Pointer to substiution text 1
*MBSS1LEN DS    F                  Substitution text length 1
*MBSS2PTR DS    A                  Pointer to substiution text 2
*MBSS2LEN DS    F                  Substitution text length 2
*MBSS3PTR DS    A                  Pointer to substiution text 3
*MBSS3LEN DS    F                  Substitution text length 3
*MBSS4PTR DS    A                  Pointer to substiution text 4
*MBSS4LEN DS    F                  Substitution text length 4
*MBSS5PTR DS    A                  Pointer to substiution text 5
*MBSS5LEN DS    F                  Substitution text length 5
*MBSS6PTR DS    A                  Pointer to substiution text 6
*MBSS6LEN DS    F                  Substitution text length 6
*MBSS7PTR DS    A                  Pointer to substiution text 7
*MBSS7LEN DS    F                  Substitution text length 7
*MBSS8PTR DS    A                  Pointer to substiution text 8
*MBSS8LEN DS    F                  Substitution text length 8
MBSmsg_tab_name ds cl8
MBSmsg_tab_addr ds A
*
MBSWTO   WTO TEXT=(R9),MF=L
MBSWTOL  EQU   *-MBSWTO
*
MSGWLEN  EQU   *-MSGWORK
*        COPY GVBPARM
MDSHEAD  GVBMSGDF TYPE=DSECT
MSGDIR   DSECT
MSGDIR#  DS    A                  First message number
MSGDIRO  DS    A                  Message group offset
MSGDIRL  EQU   *-MSGDIR#          Message directory entry length
MSGFORM  DSECT
MSG#     DS    A                  Message number
MSGEL    DS    AL1                Message element length
MSGML    DS    AL1                Message length
MSGTEXT  DS    C                  Variable length message
         END
