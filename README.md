# SBC1802 COSMAC CDP1802 Microsystem
![COSMAC Microsystem Chassis](images/SBC1802_Chassis.jpg)

After building the Elf 2000 almost 20 years ago, I decided it was time for another 1802 based project.  The primary goal of the Elf2K was to be as much like the original Elf as possible, however this time around the goals were entirely different.

  My plan was to design a machine that

  1) could run both ElfOS and RCA MicroDOS
  2) used as many of the LSI RCA CDP18xx family chips as possible
  3) supported multiple peripherals and mass storage devices

I ended up splitting the design into two boards, a "base" board and an expansion board.  Originally I planned to fit it all onto a single board but that ended up being too large and two boards, which can be stacked in the vertical dimension, are much more practical.  The expannsion board is optional and the base board is functional without it.  Also, the expansion bus is available for additional future, well, expansion.

## BASE BOARD

The base board contains -

  * CDP1805 CPU (an 1802 will work too)
  * 32K EPROM and 64K battery backed up SRAM
  * CDP1877 priority interrupt controller
  * CDP1854 UART with CTS/RTS support and programmable baud rate
  * CDP1879 real time clock with battery backup
  * IDE/ATA interface
  * POST display
  * buffered expansion bus for the expansion card(s)
  * basic front panel interface

  The memory mapping can be programmed at runtime to support either the ElfOS or MicroDOS memory maps; 60K of RAM is available in ElfOS mode and 58K in MicroDOS mode.  In addition, all 32K of the EPROM can be used by way of a simple bank switching scheme.  The CDP1879 RTC, the CDP1877 PIC, and the memory control register (which controls the memory map/bank switching) are memory mapped and occupy 32 bytes of address space.  Another 224 bytes of RAM are set aside for use by the EPROM firmware as scratchpad storage.

  The base board also contains logic to drive a basic, "turn key" front panel.  Indicators for POWER, RUN, DISK, and Q are included, along with switches/buttons for RESET and ATTENTION.  The latter causes a user defined interrupt to occur.  By using ribbon cables with a DIP header on one end it would be easy enough to co-opt the the POST display and DIP switches into front panel LED and switch registers.  However, note that there is no support for LOAD mode, nor the memory protect function, used in the original Elf.

  The base board can be used and is functional without the CDP1877 interrupt controller; after all, neither ElfOS nor MicroDOS actually use interrupts of any kind.  Likewise the base board is functional without the CDP1879 real time clock too, however you'll lose the ability to have the OS time set automatically.


## EXPANSION BOARD

When installed on the base board, the expansion board adds -

  * RCA style two level I/O
  * Another CDP1854 serial port
  * CDP1851 programmable I/O interface
  * two CDP1855 multiply/divide units
  * CDP1878 dual counter/timer
  * AY-3-8912 programmable sound (aka music) generator

  The base board alone has no I/O group logic, however when the expansion board is added it implements the standard RCA type two level I/O.  With the expansion board present all base board ports are now mapped into group 0 or 1.  I/O port #1 is implemented to write or read the I/O group select register.

  The second serial interface is essentially identical to the primary SLU on the base board.  This one also implements CTS/RTS handshaking and a programmable baud rate and character format.

  The CDP1851 is a 24 bit programmable parallel I/O chip with all the I/O pins brought out to a header and are cleverly arranged so that it should be possible to implement a Centronics style parallel port.  Alternatively they can be used as general purpose parallel I/Os as well.  Likewise all the inputs and outputs for the CDP1878 dual timer/counter are brought out to a header, and/or the timers can also be used to generate CPU interrupts at programmed intervals.

  The two CDP1855 MDUs are able to compute a 16x16 multiply or 32/16 divide operation in 16 clock cycles, which about as long as it takes the 1802/5 to execute a single instruction.  You just write the desired values to the X annd Y registers, do one NOP, and then read the result from the Z register.

  The 8912 is a three voice programmable sound chip, including an ASDR envelope generator.  This particular chip is the same one used in the Elf2K music card to play play simple MIDI tunes, and was super popular in 1980s vintage arcade games and 8 bit microcomputers.  Google "chiptunes" for examples of the kinds of sounds it can produce.  The three sound voices are mixed to produce left, center and right channels and sent to a stereo headphone jack.

  Lastly, the expansion card is functional without one or even all of the specialty chips installed.  The CDP1855s, second CDP1854, CDP1878, CDP1851 and the AY3-8912 may all be omitted if you're unable to source them.
