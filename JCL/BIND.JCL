//DB2BIND  JOB (),'Bind',CLASS=A,MSGCLASS=X,REGION=0M,              
//             NOTIFY=&SYSUID                                             
//********************************************************************    
//*                                                                       
//* (C) COPYRIGHT IBM CORPORATION 2021.                                   
//*    Copyright Contributors to the GenevaERS Project.                   
//*SPDX-License-Identifier: Apache-2.0                                    
//*                                                                       
//********************************************************************    
//*                                                                       
//*  Licensed under the Apache License, Version 2.0 (the "License");      
//*  you may not use this file except in compliance with the License.     
//*  You may obtain a copy of the License at                              
//*                                                                       
//*     http://www.apache.org/licenses/LICENSE-2.0                        
//*                                                                       
//*  Unless required by applicable law or agreed to in writing, software  
//*  distributed under the License is distributed on an "AS IS" BASIS,    
//*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express         
//*  or implied.                                                          
//*  See the License for the specific language governing permissions      
//*  and limitations under the License.                                   
//*                                                                       
//******************************************************************      
//*         BIND GenevaERS DB2 PACKAGES & PLANS                           
//******************************************************************      
//*                                                                       
//*   These instructions can be used for initial installation or          
//*   a rebind after service.                                             
//*                                                                       
//*   The following variables will need to be set to match the DB2        
//*   subsystem the packages are to reside.                               
//*                                                                       
//*   .   ensure variables are exportable                                 
//*                                                                       
//         EXPORT SYMLIST=*                                               
//*                                                                       
//*                                                                       
//********************************************************************    
//*   Setup JCL variables                                                 
//*                                                                       
//*   - Determine DB2 LIBRARIES High Level Qualifier                      
//*                                                                       
//* Some sites typically have a HLA value of 'DSN' for Db2 libraries      
//* Some sites use a mid level qualifier showing version e.g. 'V13R1M0'   
//*                                                                       
//*                                                                       
//*                                                                       
//         SET HLA=DSN                                                    
//         SET HLB=V13R1M0                                                
//         SET HL1=&HLA..&HLB.                                            
//*                                                                       
//********************************************************************    
//*                                                                      
//********************************************************************  
//*   Setup BIND variables                                              
//*                                                                     
//*   SUBSID  - the Db2 subsystem ID value, we are using 'DM13' here    
//*                                                                     
//         SET SUBSID=DM13                                              
//*                                                                     
//*   DSNAME  - the DBRM fully qualified dataset name                   
//*                                                                     
//         SET DSNAME='GEBT.LATEST.GVBDBRM'                             
//*                                                                     
//*   LOCNAME - a location of the DBMS where the package is to be       
//*             freed. The location name must be defined in the         
//*             SYSIBM.LOCATIONS table  e.g. DM13                       
//*                                                                     
//         SET LOCNAME=DM13                                             
//*                                                                     
//*   COLLID - Identifies the collection of the package                 
//*            There is no default.                                     
//*                                                                     
//         SET COLLID=ERS01                                             
//*                                                                     
//*   PLANSFX - if you have appended a suffix to the supplied names     
//*             then this suffix can be used.                           
//*                                                                     
//         SET SPLANSFX=X                                               
//*                                                                     
//*   PLAN   - Identifies the application plan name                     
//*            There is no default.                                     
//*                                                                     
//         SET PLAN=GVBMRSQ&SPLANSFX                                    
//*                                                                     
//*   MEMBER - Identifies the member name of the database request       
//*            module (DBRM) to include in the package.                 
//*            The DBRM member name becomes the package name.           
//*            There is no default.                                     
//*                                                                     
//         SET MEMBER=GVBMRSQ                                           
//*                                                                     
//*   SCHEMA  - The qualifier name                                      
//*             The default is the authorization ID of the package      
//*             owner or the plan owner                                 
//*             The example here is using a scama name of SAFRWBGD      
//*                                                                     
//         SET SCHEMA=SAFRWBGD                                          
//*                                                                     
//*   OWNER   - The OWNER option determines the authorization ID of     
//*             the owner of a plan or package.                         
//*                                                                     
//         SET OWNER=USERID1                                            
//*                                                                     
//*   SYSADM to the SYSADM-authorized userid for this subsystem         
//*                                                                     
//         SET SYSADM=USERID2                                           
//*                                                                     
//*   DB2 Runlib dataset name                                           
//*                                                                    
//* - the value used here is a suggestion, consult your DBA        
//*   for the proper name                                          
//*                                                                
//         SET RUNLIB='DSN131.RUNLIB.LOAD'                         
//*                                                                
//*   PLAN name used by Dynamic SQL Update programe                
//*                                                                
//* - the value used here is a suggestion, consult your DBA        
//*   for the proper name                                          
//*                                                                
//         SET TIAD=DSNTIA13                                       
//*                                                                
//PACKAGE EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(4,LT)                
//STEPLIB DD  DISP=SHR,DSN=&HL1..SDSNEXIT                          
//        DD  DISP=SHR,DSN=&HL1..SDSNLOAD                          
//DBRMLIB DD  DISP=SHR,DSN=&DSNAME.                                
//SYSTSPRT DD  SYSOUT=*                                            
//SYSPRINT DD  SYSOUT=*                                            
//SYSUDUMP DD  SYSOUT=*                                            
//SYSTSIN DD *,SYMBOLS=EXECSYS                                     
  DSN SYSTEM(&SUBSID.)                                             
  BIND PACKAGE(&COLLID.) -                                         
       OWNER(&SYSADM.)   -                                         
       MEMBER(&MEMBER.)  -                                         
       CURRENTDATA(YES)  -                                         
       ENCODING(EBCDIC)  -                                         
       ACTION(REPLACE)   -                                         
       RELEASE(COMMIT)   -                                         
       QUALIFIER(&SCHEMA) -                                        
       VALIDATE(BIND)    -                                         
       EXPLAIN(YES)      -                                         
       ISOLATION(CS)     -                                         
       LIB('&DSNAME.')   -                                         
       FLAG(I)                                                     
  BIND                    -                                        
       PLAN(&PLAN.)       -                                        
       PKLIST(&COLLID..*) -                                        
       ACTION(REPLACE)    -                                        
       ISO(CS)            -                                        
       CURRENTDATA(YES)   -                                        
       QUALIFIER(&SCHEMA) -                                        
       OWNER(&SYSADM.)    -                                        
       ENCODING(EBCDIC)                                            
  RUN PROGRAM(DSNTIAD) PLAN(&TIAD.) -                              
  LIB('&RUNLIB.')                                                  
//SYSIN   DD *,SYMBOLS=EXECSYS                                     
SET CURRENT SQLID='&SYSADM.';                                      
GRANT EXECUTE ON PLAN &PLAN. TO PUBLIC;                            
//*