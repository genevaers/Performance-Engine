# Performance-Engine               

## Pre-Requisite Products for Build

- Git
- High Level Assembler Toolkit

## Setting up Git on USS

You need to install a Git client on the z/OS system you will use to Build the Performance Engine.
Rocket Software has provided the port for Git to be used on z/OS
In their port they added code page conversion capability to support EBCDIC files. 
The .gitattributes file is used to specify the desired code pages.

For the latest information, please review the README provided with Git from Rocket Software.

You will need at least 550 MB of free space for Rocket® Git and associated tools.

## Installing Git on USS:

Note: Rocket Git is also available as an SMP/E installable package as part of IBM Z Open Development or IBM Dependency Based Build stand-alone from Passport Advantage. You can install Rocket Git by using SMP/E and skip step 1 to step 5.

    1.  Download Rocket Git from the Rocket z/OS tools page. You need to download the following tools.
        Git
        Gzip
        Bash
        Perl

    2.  Create a directory (for example, /var/rocket/) for the tools.
        Create the directory in a file system with at least 550 MB of free space or
        Create a new file system of at least 550 MB and mount as /var/rocket

    3.  Untar Gzip to /var/rocket and follow directions in /var/rocket/share/doc/gzip/README.ZOS.

     tar -C /var/rocket -xovf gzip-1.6-edc_<build>.tar

    4.  Untar Bash to /var/rocket and follow directions in /var/rocket/share/doc/bash/README.ZOS.

     gzip -d bash-4.3_<build>.tar.gz
     tar -C /var/rocket -xovf bash-4.3_<build>.tar

    5.  Follow similar directions for both Git and Perl.

    6.  Add environment variables documented in the README.ZOS files to your USS environment

Since the Git environment variables may need to be set from both a login session and
a sample shell script gitenv.sh is listed here.

Sample gitenv.sh

#!/bin/sh
##############################################################################################
##
##  Sample shell script used to set Rocket's Git environment variables
##
##  The Rocket Git documentation located in /<installDir>/share/doc/git/<version>/README.ZOS
##  instructs users to create and update new and existing and environment variables for
##  the Git client to run.  Normally this would be achieved by adding them to the user's
##  .profile login script.  However the Jenkins agent runs in a non-login shell so the .profile
##  script is not executed automatically.  This shell script can be executed by both the users
##  .profile script and the Jenkins agent at startup
##
##  Setup Instructions:
##  1) Replace all references to "rsusr" with the appropriate install directory name
##  2) In the Jenkins Agent definition configuration add the path to this file in the
##     Prefix Start Agent Command field
##
###############################################################################################
# git the environment variables
export GIT_SHELL=/var/rocket/bin/bash
export GIT_EXEC_PATH=/var/rocket/libexec/git-core
export GIT_TEMPLATE_DIR=/var/rocket/share/git-core/templates

#common the environment variables
export PATH=$PATH:/var/rocket/bin
export MANPATH=$MANPATH:/var/rocket/man
export PERL5LIB=$PERL5LIB:/var/rocket/lib/perl5
export LIBPATH=$LIBPATH:/rsusr/rocket/lib/perl5/<5.24.0 | 5.24.4>/os390/CORE
** use 5.24.0 if Git V110 is installed and 5.24.4 if Git V111 is installed

#ASCII support the environment variables
export _BPXK_AUTOCVT=ON
export _CEE_RUNOPTS="FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"
export _TAG_REDIR_ERR=txt
export _TAG_REDIR_IN=txt
export _TAG_REDIR_OUT=txt

#set git editor to create comments on encdoing ISO8859-1
#git config --global core.editor "/bin/vi -W filecodeset=ISO8859-1"


  