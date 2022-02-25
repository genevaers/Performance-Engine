# Visit https://genevaers.org for the full documentation, examples and guides.

GenevaERS’s Performance Engine, which resolves scores of queries or processes in a single pass through a database, today is executed via standard JCL
This repo contains parts written in HLASM Source used create a number of executable modules to execute on z/OS

# pe  
To build  
:gear:  

## Hardware and software requirements on z/OS systems  

- [Any supported release of z/OS)

- [High Level Assembler Toolkit as the build of this product does require its structured macros] 

- [Git product installed in USS)

## Installation, configuration and Build on z/OS

Instructions for installation and configuration of software and building of GenevaERS Perfromance Engine.

Clone this pe repo into a USS environment 

- Allocate the COPYASM and ASM PDS datasets
<pre> 
    e.g. tsocmd "ALLOC DSN('High_Level_Qualifier.ASM') NEW CATALOG UNIT(SYSDA) SPACE(100,100) DIR(10) TRACKS LRECL(80) RECFM(F) DSORG(PO) BLKSIZE(32760) "
</pre>
- Move the ASM suffixed parts into a z/OS PDS from USS
<pre> 
    e.g. cp gvbmr95.ASM "//'High_Level_Qualifier.ASM(GVBMR95W)'"   
</pre>
- Move the COPYASM suffixed parts into a z/OS PDS
<pre>
    e.g. cp gvbhdr.MAC "//'High_Level_Qualifier.MAC(DL96AREA)'" 
</pre>
- Allocate the PDS dataset to hold object data
<pre> 
    e.g. tsocmd "ALLOC DSN('High_Level_Qualifier.OBJECT') NEW CATALOG UNIT(SYSDA) SPACE(100,100) DIR(10) TRACKS LRECL(80) RECFM(F) DSORG(PO) BLKSIZE(32760) "
</pre>
- Allocate the PDS dataset to hold the modules
<pre>
    e.g. tsocmd "ALLOC DSN('High_Level_Qualifier.GVBLOAD') NEW CATALOG UNIT(SYSDA) SPACE(2000,400) DIR(10) TRACKS LRECL(0) RECFM(U) DSORG(PO) BLKSIZE(32760) "
</pre>
- Assemble each ASM part 
  - use these options 
<pre> 
    //MEMBER  EXEC PGM=ASMA90,
    // PARM=\(NODECK,OBJECT,ADATA,'SYSPARM\(RELEASE)','OPTABLE\(ZS7)',
    // 'PC\(GEN),FLAG\(NOALIGN),SECTALGN\(256),GOFF,LIST\(133)'\) 
</pre>    
  - use this SYSLIB concaenation
<pre>   
    //SYSLIB   DD DISP=SHR,DSN=&HLQ..MAC "      <-- the MAC library from the clone
    //         DD DISP=SHR,DSN=ASM.SASMMAC2     <-- HLASM Toolkit
    //         DD DISP=SHR,DSN=SYS1.MACLIB      <-- z/OS system macros 
    //         DD DISP=SHR,DSN=SYS1.MODGEN      <-- z/OS system macros
    //         DD DISP=SHR,DSN=CEE.SCEEMAC      <-- z/OS LE macros
</pre>    
  - SYSLIN will be
<pre>  
    //SYSLIB   DD DISP=SHR,DSN=&HLQ..OBJECT      <-- the OBJECT previously allocated
</pre>
- Bind the following modules
  -   list of modules to bind
<pre>                                                      
    GVBMR95
    GVBMR93
    GVBMR88
</pre>  
  - use these options 
<pre> 
    //MEMBER EXEC PGM=IEWL,                                   
    // PARM=(XREF,LET,LIST,MAP,AMODE(31),RMODE(ANY),REUS(RENT)) 
</pre>   
  - use this SYSLIB concaenation
<pre> 
    //SYSLIB   DD DISP=SHR,DSN=&HLQ..OBJECT     <-- the OBJECT library previously created
    //         DD DISP=SHR,DSN=CEE.SCEERUN     
    //         DD DISP=SHR,DSN=CEE.SCEELKED     
    //         DD DISP=SHR,DSN=CEE.SCEELIB    
    //         DD DISP=SHR,DSN=SYS1.CSSLIB
    //         DD DISP=SHR,DSN=SYS1.LINKLIB    
    //         DD DISP=SHR,DSN=DSN.V12R1M0.SDSNLOAD   DB2 optional
</pre>                                               
  - SYSLIN will be
 <pre>
     //SYSLIB   DD DISP=SHR,DSN=&HLQ..OBJECT      <-- the OBJECT previously allocated
 </pre>
 - SYSLMOD will be
<pre>
    //SYSLIB   DD DISP=SHR,DSN=&HLQ..GVBLOAD      <-- the GVBLOAD previously allocated
</pre>
