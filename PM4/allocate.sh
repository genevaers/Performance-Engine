#!/bin/bash                                                                                                                                      
#                                                                                                                                                
# (c) Copyright IBM Corporation 2020.                                                                                                            
#     Copyright Contributors to the GenevaERS Project.                                                                                           
# SPDX-License-Identifier: Apache-2.0                                                                                                            
#                                                                                                                                                
# ***************************************************************************                                                                    
#                                                                                                                                                
#   Licensed under the Apache License, Version 2.0 (the "License");                                                                              
#   you may not use this file except in compliance with the License.                                                                             
#   You may obtain a copy of the License at                                                                                                      
#                                                                                                                                                
#     http://www.apache.org/licenses/LICENSE-2.0                                                                                                 
#                                                                                                                                                
#   Unless required by applicable law or agreed to in writing, software                                                                          
#   distributed under the License is distributed on an "AS IS" BASIS,                                                                            
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.                                                                     
#   See the License for the specific language governing permissions and                                                                          
#   limitations under the License.                                                                                                               
# ****************************************************************************                                                                   
#                                                                                                                                                
#   script to allocate z/OS Datasets and load ASM and MAC members                                                                                
#                                                                                                                                                
echo " Allocation of Datasets "                                                                                                                  
echo " High Level Qualifier is " $1                                                                                                              
echo " DB2 Flag setting is     " $2                                                                                                              
echo " LIST Flag setting is    " $3                                                                                                              
#                                                                                                                                                
if (( $? )); then                                                                                                                                
 echo "Usage : allocate.sh <High Level Qualifier> \n e.g. GEBT"                                                                                  
 echo "        A high level qualifier was not supplied "                                                                                         
 set -e                                                                                                                                          
 exit 1                                                                                                                                          
fi                                                                                                                                               
#                                                                                                                                                
#   .   alloc ADATA                                                                                                                              
#                                                                                                                                                
                                                                                                                                                 
#   .   remove any prior instance                                                                                                                
tsocmd "ALLOC DSN('$1.ADATA') OLD DELETE"                                                                                                        
if (( $? )); then                                                                                                                                
            echo "---------------------"                                                                                                         
            echo "file has been not  deleted"                                                                                                    
            echo "---------------------"                                                                                                         
                                                                                                                                                 
else                                                                                                                                             
            echo "expected error with delete of $1.ADATA"                                                                                        
fi                                                                                                                                               
                                                                                                                                                 
#   .   alloc new file                                                                                                                           
tso -t "ALLOC DSN('$1.ADATA') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(3000,3000) LRECL(8188) DSORG(PO) RECFM(V,B) BLKSIZE(32760) DSNTYPE(LIBRARY)"  
if (( $? )); then                                                                                                                                
            echo "---------------------"                                                                                                         
            echo "file has not been  created"                                                                                                    
             echo "---------------------"                                                                                                      
            exit 1                                                                                                                            
else                                                                                                                                          
            echo "File has been created $1.ADATA"                                                                                             
fi                                                                                                                                            
                                                                                                                             
#                                                                                                                                             
# --------------------------------------------------------------------------                                                                  
#                                                                                                                                             
#   .   alloc DB2 files if flag is set                                                                                                        
#                                                                                                                                             
# --------------------------------------------------------------------------                                                                  
#                                                                                                                                             
if [ $2 = 'DB2' ]; then                                                                                                                       
#                                                                                                                                             
#   .   alloc ASMDB2                                                                                                                          
#                                                                                                                                             
                                                                                                                                              
#   .   remove any prior instance                                                                                                             
tsocmd "ALLOC DSN('$1.ASMDB2') OLD DELETE"                                                                                                    
if (( $? )); then                                                                                                                             
            echo "---------------------"                                                                                                      
            echo "file has been not  deleted"                                                                                                 
            echo "---------------------"                                                                                                      
                                                                                                                                              
else                                                                                                                                          
            echo "expected error with delete of $1.ASMDB2"                                                                                    
fi                                                                                                                                            
                                                                                                                                              
#   .   alloc new file                                                                                                                        
tsocmd "ALLOC DSN('$1.ASMDB2') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(30,30) LRECL(80) RECFM(F,B) DSORG(PO) BLKSIZE(32720) DSNTYPE(LIBRARY)"    
if (( $? )); then                                                                                                                             
            echo "---------------------"                                                                                                      
            echo "file has not been  created"                                                                                                
            echo "---------------------"                                                                                                     
            exit 1                                                                                                                           
else                                                                                                                                         
            echo "File has been created $1.ASMDB2"                                                                                           
fi                                                                                                                                           
#                                                                                                                                            
#   .   alloc GVBDBRM                                                                                                                        
#                                                                                                                                            
                                                                                                                                             
#   .   remove any prior instance                                                                                                            
tsocmd "ALLOC DSN('$1.GVBDBRM') OLD DELETE"                                                                                                  
if (( $? )); then                                                                                                                            
            echo "---------------------"                                                                                                     
            echo "file has been not  deleted"                                                                                                
            echo "---------------------"                                                                                                     
                                                                                                                                             
else                                                                                                                                         
            echo "expected error with delete of $1.GVBDBRM"                                                                                  
fi                                                                                                                                           
                                                                                                                                             
#   .   alloc new file                                                                                                                       
tsocmd "ALLOC DSN('$1.GVBDBRM') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(30,30) LRECL(80) RECFM(F,B) DSORG(PO) BLKSIZE(32720) DSNTYPE(LIBRARY)"  
if (( $? )); then                                                                                                                            
            echo "---------------------"                                                                                                     
            echo "file has not been  created"                                                                                                
            echo "---------------------"                                                                                                     
            exit 1                                                                                                                           
else                                                                                                                                         
            echo "File has been created $1.GVBDBRM"                                                                                          
fi                                                                                                                                           
#                                                                                                                                            
#   .   alloc BTCHDB2                                                                                                                        
#                                                                                                                                            
                                                                                                                                             
#   .   remove any prior instance                                                                                                            
tsocmd "ALLOC DSN('$1.BTCHDB2') OLD DELETE"                                                                                                  
if (( $? )); then                                                                                                                            
            echo "---------------------"                                                                                                     
            echo "file has been not  deleted"                                                                                                
            echo "---------------------"                                                                                                     
                                                                                                                                             
else                                                                                                                                         
            echo "expected error with delete of $1.BTCHDB2"                                                                                  
fi                                                                                                                                           
                                                                                                                                             
#   .   alloc new file                                                                                                                       
tsocmd "ALLOC DSN('$1.BTCHDB2') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(10,10) LRECL(80) RECFM(F,B) DSORG(PO) BLKSIZE(32720) DSNTYPE(LIBRARY)"  
if (( $? )); then                                                                                                                            
            echo "---------------------"                                                                                                     
            echo "file has not been  created"                                                                                                
            echo "---------------------"                                                                                                     
            exit 1                                                                                                                           
else                                                                                                                                         
            echo "File has been created $1.BTCHDB2"                                                                                          
fi                                                                                                                                           
else                                                                                                                                         
            echo "DB2 has not been set and DB2 files not allocated"                                                                              
fi                                                                                                                                               
#                                                                                                                                                
#   .   alloc ASMLANGX                                                                                                                           
#                                                                                                                                                
                                                                                                                                                 
#   .   remove any prior instance                                                                                                                
tsocmd "ALLOC DSN('$1.ASMLANGX') OLD DELETE"                                                                                                     
if (( $? )); then                                                                                                                                
            echo "---------------------"                                                                                                         
            echo "file has been not  deleted"                                                                                                    
            echo "---------------------"                                                                                                         
                                                                                                                                                 
else                                                                                                                                             
            echo "expected error with delete of $1.ASMLANGX"                                                                                     
fi                                                                                                                                               
                                                                                                                                                 
#   .   alloc new file                                                                                                                           
tsocmd "ALLOC DSN('$1.ASMLANGX') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(600,600) LRECL(1562) RECFM(V,B) DSORG(PO) BLKSIZE(32760) DSNTYPE(LIBRARY)" 
if (( $? )); then                                                                                                                                
            echo "---------------------"                                                                                                         
            echo "file has not been  created"                                                                                                    
            echo "---------------------"                                                                                                         
            exit 1                                                                                                                               
else                                                                                                                                             
            echo "File has been created $1.ASMLANGX"                                                                                             
fi                                                                                                                                               
#                                                                                                                                                
#   .   alloc GVBSAMP                                                                                                                            
#                                                                                                                                                
                                                                                                                                                 
#   .   remove any prior instance                                                                                                                
tsocmd "ALLOC DSN('$1.GVBSAMP') OLD DELETE"                                                                                                      
if (( $? )); then                                                                                                                                
            echo "---------------------"                                                                                                         
            echo "file has been not  deleted"                                                                                                    
            echo "---------------------"                                                                                                         
                                                                                                                                                 
else                                                                                                                                             
            echo "expected error with delete of $1.GVBSAMP"                                                                                      
fi                                                                                                                                               
                                                                                                                                                 
#   .   alloc new file                                                                                                                           
tsocmd "ALLOC DSN('$1.GVBSAMP') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(10,10) LRECL(80) RECFM(F,B) DSORG(PO) BLKSIZE(32720) DSNTYPE(LIBRARY)"      
if (( $? )); then                                                                                                                                
            echo "---------------------"                                                                                                         
            echo "file has not been  created"                                                                                                    
            echo "---------------------"                                                                                                         
            exit 1                                                                                                                               
else                                                                                                                                             
            echo "File has been created $1.GVBSAMP"                                                                                              
fi                                                                                                                                               
#                                                                                                                                                
#   .   alloc GVBLOAD                                                                                                                            
#                                                                                                                                                
                                                                                                                                                 
#   .   remove any prior instance                                                                                                                
tsocmd "ALLOC DSN('$1.GVBLOAD') OLD DELETE"                                                                                                    
if (( $? )); then                                                                                                                              
            echo "---------------------"                                                                                                       
            echo "file has been not  deleted"                                                                                                  
            echo "---------------------"                                                                                                       
                                                                                                                                               
else                                                                                                                                           
            echo "expected error with delete of $1.GVBLOAD"                                                                                    
fi                                                                                                                                             
                                                                                                                                               
#   .   alloc new file                                                                                                                         
tsocmd "ALLOC DSN('$1.GVBLOAD') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(2800,1200) LRECL(0) RECFM(U) DSORG(PO) BLKSIZE(32760) DSNTYPE(LIBRARY)"   
if (( $? )); then                                                                                                                              
            echo "---------------------"                                                                                                       
            echo "file has not been  created"                                                                                                  
            echo "---------------------"                                                                                                       
            exit 1                                                                                                                             
else                                                                                                                                           
            echo "File has been created $1.GVBLOAD"                                                                                            
fi                                                                                                                                             
#                                                                                                                                              
#   .   alloc GVBOBJ                                                                                                                           
#                                                                                                                                              
                                                                                                                                               
#   .   remove any prior instance                                                                                                              
tsocmd "ALLOC DSN('$1.GVBOBJ') OLD DELETE"                                                                                                     
if (( $? )); then                                                                                                                              
            echo "---------------------"                                                                                                       
            echo "file has been not  deleted"                                                                                                  
            echo "---------------------"                                                                                                       
                                                                                                                                               
else                                                                                                                                           
            echo "expected error with delete of $1.GVBOBJ"                                                                                     
fi                                                                                                                                             
                                                                                                                                               
#   .   alloc new file                                                                                                                         
tsocmd "ALLOC DSN('$1.GVBOBJ') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(100,100) LRECL(80) RECFM(F,B) DSORG(PO) BLKSIZE(32720) DSNTYPE(LIBRARY)"   
if (( $? )); then                                                                                                                              
            echo "---------------------"                                                                                                       
            echo "file has not been  created"                                                                                                  
            echo "---------------------"                                                                                                       
            exit 1                                                                                                                             
else                                                                                                                                           
            echo "File has been created $1.GVBOBJ"                                                                                             
fi                                                                                                                                             
                                                                                                                                               
#   .   alloc GVBCOPY                                                                                                                          
#                                                                                                                                              
                                                                                                                                               
#   .   remove any prior instance                                                                                                              
tsocmd "ALLOC DSN('$1.GVBCOPY') OLD DELETE"                                                                                                    
if (( $? )); then                                                                                                                              
            echo "---------------------"                                                                                                       
            echo "file has been not  deleted"                                                                                                  
            echo "---------------------"                                                                                                       
                                                                                                                                               
else                                                                                                                                           
            echo "expected error with delete of $1.GVBCOPY"                                                                                 
fi                                                                                                                                          
                                                                                                                                            
#   .   alloc new file                                                                                                                      
tsocmd "ALLOC DSN('$1.GVBCOPY') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(100,100) LRECL(80) RECFM(F,B) DSORG(PO) BLKSIZE(32720) DSNTYPE(LIBRARY)"
if (( $? )); then                                                                                                                           
            echo "---------------------"                                                                                                    
            echo "file has not been  created"                                                                                               
            echo "---------------------"                                                                                                    
            exit 1                                                                                                                          
else                                                                                                                                        
            echo "File has been created $1.GVBCOPY"                                                                                         
fi                                                                                                                                          
#   .   alloc GVBASM                                                                                                                        
#                                                                                                                                           
                                                                                                                                            
#   .   remove any prior instance                                                                                                           
tsocmd "ALLOC DSN('$1.GVBASM') OLD DELETE"                                                                                                  
if (( $? )); then                                                                                                                           
            echo "---------------------"                                                                                                    
            echo "file has been not  deleted"                                                                                               
            echo "---------------------"                                                                                                    
                                                                                                                                            
else                                                                                                                                        
            echo "expected error with delete of $1.GVBASM"                                                                                  
fi                                                                                                                                          
                                                                                                                                            
#   .   alloc new file                                                                                                                      
tsocmd "ALLOC DSN('$1.GVBASM') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(50,50) LRECL(80) RECFM(F,B) DSORG(PO) BLKSIZE(32720) DSNTYPE(LIBRARY)"  
if (( $? )); then                                                                                                                           
            echo "---------------------"                                                                                                    
            echo "file has not been  created"                                                                                               
            echo "---------------------"                                                                                                    
            exit 1                                                                                                                          
else                                                                                                                                        
            echo "File has been created $1.GVBASM"                                                                                          
fi                                                                                                                                          
#   .   alloc GVBMAC                                                                                                                        
#                                                                                                                                           
                                                                                                                                            
#   .   remove any prior instance                                                                                                           
tsocmd "ALLOC DSN('$1.GVBMAC') OLD DELETE"                                                                                                  
if (( $? )); then                                                                                                                           
            echo "---------------------"                                                                                                    
            echo "file has been not  deleted"                                                                                               
            echo "---------------------"                                                                                                    
                                                                                                                                            
else                                                                                                                                        
            echo "expected error with delete of $1.GVBMAC"                                                                                  
fi                                                                                                                                          
                                                                                                                                            
#   .   alloc new file                                                                                                                      
tsocmd "ALLOC DSN('$1.GVBMAC') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(10,10) LRECL(80) RECFM(F,B) DSORG(PO) BLKSIZE(32720) DSNTYPE(LIBRARY)"  
if (( $? )); then                                                                                                                           
            echo "---------------------"                                                                                                    
            echo "file has not been  created"                                                                                               
            echo "---------------------"                                                                                                    
              exit 1                                                                                                                                  
else                                                                                                                                                
            echo "File has been created $1.GVBMAC"                                                                                                  
fi                                                                                                                                                  
#                                                                                                                                                   
#   .   alloc JOB                                                                                                                                   
#                                                                                                                                                   
                                                                                                                                                    
#   .   remove any prior instance                                                                                                                   
tsocmd "ALLOC DSN('$1.JOB') OLD DELETE"                                                                                                             
if (( $? )); then                                                                                                                                   
            echo "---------------------"                                                                                                            
            echo "file has been not  deleted"                                                                                                       
            echo "---------------------"                                                                                                            
                                                                                                                                                    
else                                                                                                                                                
            echo "expected error with delete of $1.JOB"                                                                                             
fi                                                                                                                                                  
                                                                                                                                                    
#   .   alloc new file                                                                                                                              
tsocmd "ALLOC DSN('$1.JOB') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(10,10) LRECL(80) RECFM(F,B) DSORG(PO) BLKSIZE(32720) DSNTYPE(LIBRARY)"             
if (( $? )); then                                                                                                                                   
            echo "---------------------"                                                                                                            
            echo "file has not been  created"                                                                                                       
            echo "---------------------"                                                                                                            
            exit 1                                                                                                                                  
else                                                                                                                                                
            echo "File has been created $1.JOB"                                                                                                     
fi                                                                                                                                                  
# --------------------------------------------------------------------------                                                                        
#                                                                                                                                                   
#   .   alloc LIST files if flag is set                                                                                                             
#                                                                                                                                                   
# --------------------------------------------------------------------------                                                                        
#                                                                                                                                                   
if [ $3 = 'LIST' ]; then                                                                                                                            
#                                                                                                                                                   
#   .   alloc LISTASM                                                                                                                               
#                                                                                                                                                   
                                                                                                                                                    
#   .   remove any prior instance                                                                                                                   
tsocmd "ALLOC DSN('$1.LISTASM') OLD DELETE"                                                                                                         
if (( $? )); then                                                                                                                                   
            echo "---------------------"                                                                                                            
            echo "file has been not  deleted"                                                                                                       
            echo "---------------------"                                                                                                            
                                                                                                                                                    
else                                                                                                                                                
            echo "expected error with delete of $1.LISTASM"                                                                                         
fi                                                                                                                                                  
                                                                                                                                                    
#   .   alloc new file                                                                                                                              
tsocmd "ALLOC DSN('$1.LISTASM') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(2000,1000) LRECL(133) RECFM(F,B,M) DSORG(PO) BLKSIZE(32718) DSNTYPE(LIBRARY)"  
if (( $? )); then                                                                                                                                   
            echo "---------------------"                                                                                                            
            echo "file has not been  created"                                                                                                       
            echo "---------------------"                                                                                                            
            exit 1                                                                                                                                 
else                                                                                                                                               
            echo "File has been created $1.LISTASM"                                                                                                
fi                                                                                                                                                 
#                                                                                                                                                  
#   .   alloc LISTLINK                                                                                                                             
#                                                                                                                                                  
                                                                                                                                                   
#   .   remove any prior instance                                                                                                                  
tsocmd "ALLOC DSN('$1.LISTLINK') OLD DELETE"                                                                                                       
if (( $? )); then                                                                                                                                  
            echo "---------------------"                                                                                                           
            echo "file has been not  deleted"                                                                                                      
            echo "---------------------"                                                                                                           
                                                                                                                                                   
else                                                                                                                                               
            echo "expected error with delete of $1.LISTLINK"                                                                                       
fi                                                                                                                                                 
                                                                                                                                                   
#   .   alloc new file                                                                                                                             
tsocmd "ALLOC DSN('$1.LISTLINK') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(100,100) LRECL(121) RECFM(F,B,A) DSORG(PO) BLKSIZE(32670) DSNTYPE(LIBRARY)"  
if (( $? )); then                                                                                                                                  
            echo "---------------------"                                                                                                           
            echo "file has not been  created"                                                                                                      
            echo "---------------------"                                                                                                           
            exit 1                                                                                                                                 
else                                                                                                                                               
            echo "File has been created $1.LISTLINK"                                                                                               
fi                                                                                                                                                 
else                                                                                                                                               
            echo "LIST files has not been created"                                                                                                 
fi                                                                                                                                                 
#                                                                                                                                                  
#   .   alloc GVBXSD                                                                                                                               
#                                                                                                                                                  
                                                                                                                                                   
#   .   remove any prior instance                                                                                                                  
tsocmd "ALLOC DSN('$1.GVBXSD') OLD DELETE"                                                                                                         
if (( $? )); then                                                                                                                                  
            echo "---------------------"                                                                                                           
            echo "file has been not  deleted"                                                                                                      
            echo "---------------------"                                                                                                           
                                                                                                                                                   
else                                                                                                                                               
            echo "expected error with delete of $1.GVBXSD"                                                                                         
fi                                                                                                                                                 
                                                                                                                                                   
#   .   alloc new file                                                                                                                             
tsocmd "ALLOC DSN('$1.GVBXSD') NEW CATALOG TRACKS UNIT(SYSDA) SPACE(10,10) LRECL(259) RECFM(V,B) DSORG(PO) BLKSIZE(32760) DSNTYPE(LIBRARY)"        
if (( $? )); then                                                                                                                                  
            echo "---------------------"                                                                                                           
            echo "file has not been  created"                                                                                                      
            echo "---------------------"                                                                                                           
            exit 1                                                                                                                                 
else                                                                                                                                               
            echo "File has been created $1.GVBXSD"                                                                                                 
fi                                                                                                                                                 
#       
exit 0  
