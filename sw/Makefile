#++
# Makefile - Makefile for the Spare Time Gizmos SBC1802 EPROM
#
# Copyright (C) 2024 by Spare Time Gizmos.  All rights reserved.
#
# This firmware is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 59 Temple
# Place, Suite 330, Boston, MA  02111-1307  USA
#
#DESCRIPTION:
#   This Makefile will build the EPROM image for the Spare Time Gizmos SBC1802.
# The result is a file called eprom.hex, which is ready to burn into a 27C256.
#
#TARGETS:
#  make all	- rebuild eprom.hex
#  make clean	- clean up all generated files except sbc1802.hex
#  make distro	- make the sbc1802.zip source file distribution
#
# REVISION HISTORY:
# dd-mmm-yy	who     description
# 17-Jan-24	RLA	New file.
# 17-May-24	RLA	Add BASIC3.
#--

# Tool paths and options - you can change these as necessary...
TASM=D:/bob/bin/tasm/tasm
export TASMTABS=D:/bob/bin/tasm/
TASMOPTS=-t1802 -lat -g0 -i -p44 -DTASM -DSBC1802
A18=d:/bob/bin/a18
ROMMERGE=D:/bob/bin/rommerge.exe
ROMCKSUM=D:/bob/bin/romcksum.exe
ROMTEXT=D:/bob/bin/romtext.exe
ZIP=C:/Program Files/7-Zip/7z.exe
RM=C:/cygwin64/bin/rm.exe
ECHO=C:/cygwin64/bin/echo.exe
DATE=C:/cygwin64/bin/date.exe

# All components of the EPROM image ...
HEXFILES = sbc1802.hex bios1802.hex ut1802.hex basic3.hex help.hex
LISTFILES = $(HEXFILES:.hex=.lst)

# The default target builds everything...
all:	eprom.hex

sbc1802.hex:	sbc1802.asm bios.inc ut71.inc sysdat.asm
	@$(ECHO) -e "\n*** BUILDING SBC1802 MONITOR ***"
	@$(RM) -f sysdat.asm
	@$(DATE) +"SYSDAT:	.TEXT	\"%d-%#b-%Y %H:%M\\000\"" >sysdat.asm
	@$(DATE) +"BUILDY:	.BYTE	%y" >>sysdat.asm
	@$(TASM) $(TASMOPTS) $< $@

bios1802.hex:	bios1802.asm bios.inc rtl1802.asm 
	@$(ECHO) -e "\n*** BUILDING SBC1802 BIOS ***"
	@$(TASM) $(TASMOPTS) $< $@

ut1802.hex:	ut1802.asm sbc1802.inc bios.inc ut71.inc
	@$(ECHO) -e "\n*** BUILDING SBC1802 UT71 ***"
	@$(TASM) $(TASMOPTS) $< $@

basic3.hex:	basic3.asm
	@$(ECHO) -e "\n*** BUILDING BASIC3 ***"
	$(A18) $< -l $(<:.asm=.lst) -o $@

help.hex:	sbc1802.hlp
	@$(ECHO) -e "\n*** COMPILING HELP TEXT ***"
	@$(ROMTEXT) -a0xE000 sbc1802.hlp help.hex

merged.hex:	$(HEXFILES)
	@$(ECHO) -e "\n*** MERGING FILES INTO EPROM IMAGE ***"
	@$(ROMMERGE) -s32k -o32768 merged.hex $(HEXFILES)

eprom.hex:	merged.hex
	@$(ECHO) -e "\n*** CALCULATING EPROM CHECKSUM ***"
	@$(ROMCKSUM) merged.hex -s32K -o32768 -c32764 eprom.hex

# The "clean" target does what you'd expect...
clean:
	$(RM) -f $(HEXFILES)
	$(RM) -f $(LISTFILES)
	$(RM) -f merged.hex
	$(RM) -f *.*\~ \#*.*\#

#   The "distro" target builds a sbc1802.zip file which contains all the tools,
# source files, readme files, license files, etc that are usually included in
# the source distribution.  (A binary distribution, of course, only contains
# the SBC1802.hex file!)
distro:
	@$(ECHO) Building source distribution ...
	$(RM) -f sbc1802.zip
	$(ZIP) a sbc1802.zip \
	  sbc1802.asm rtl1802.asm ut1802.asm sysdat.asm \
	  sbc1802.inc bios.inc ut71.inc \
	  Makefile. readme.txt license.txt eprom.hex

