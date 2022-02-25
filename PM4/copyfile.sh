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
#   script to copy files to z/OS                                                          
#                                                                                         
echo " Copy unix files to z/OS PDS"                                                       
echo " High Level Qualifier is " $1                                                       
echo " USS Directory is is " $2                                                           
#                                                                                         
if (( $? )); then                                                                         
 echo "Usage : copyfile.sh <High Level Qualifier> \n e.g. GEBT"                           
 echo "        A high level qualifier was not supplied "                                  
 set -e                                                                                   
 exit 1                                                                                   
fi                                                                                        
#                                                                                         
#   .   copyfiles into ASM                                                             
#                                                                                         
for entry in `ls $2/ASM`; do                                                              
    fullf=$entry                                                                          
    fname=${fullf%%.*}                                                                    
    echo $fname                                                                           
    cp -F nl $2/ASM/$entry "//'$1.GVBASM($fname)'"                                        
    if (( $? )); then                                                                     
                echo "-----------------------------------"                                
                echo "$fname has not been moved to GVBASM"                                
                echo "-----------------------------------"                                
                exit 1                                                                    
    fi                                                                                    
done     
#                                                                                         
#   .   copyfiles into GVBMAC                                                             
#  
for entry in `ls $2/MAC`; do                                                              
    fullf=$entry                                                                          
    fname=${fullf%%.*}                                                                    
    echo $fname                                                
    cp -F nl $2/MAC/$entry "//'$1.GVBMAC($fname)'"             
    if (( $? )); then                                          
                echo "-----------------------------------"     
                echo "$fname has not been moved to GVBMAC"     
                echo "-----------------------------------"     
                exit 1                                         
    fi                                                         
done      
for entry in `ls $2/CPY`; do                                                              
    fullf=$entry                                                                          
    fname=${fullf%%.*}                                                                    
    echo $fname                                                
    cp -F nl $2/MAC/$entry "//'$1.GVBMAC($fname)'"             
    if (( $? )); then                                          
                echo "-----------------------------------"     
                echo "$fname has not been moved to GVBMAC"     
                echo "-----------------------------------"     
                exit 1                                         
    fi                                                         
done                                                           
exit  
