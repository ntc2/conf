#!/bin/bash

# Based on http://ubuntuforums.org/showpost.php?p=4455945&postcount=6
# Recommends www.lesswatts.org

echo 5 > /proc/sys/vm/laptop_mode  #start laptop mode (this might be redundant)
echo 0 > /proc/sys/kernel/nmi_watchdog #stop the watchdog 

# I don't have anything like this.
#echo Y > /sys/module/snd_ac97_codec/parameters/powe2.6.24.22.6.24.2r_save #set power save on AC97 (this might be redundant)

# This is already set for me.
#echo ondemand > /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor # set cpu scaling gov (this is probably redundant)

echo 1500 > /proc/sys/vm/dirty_writeback_centisecs 

# Mine were set to "2" before this.  Powertop had a different
# recommendation that I don't remember ...
for i in /sys/bus/usb/devices/*/power/autosuspend; do echo 1 > $i; done  #tell usb devices to autosuspend

# Commented out ones I don't have.
#/etc/init.d/preload stop  # i think thsi was creating alot of wakeups, so i made it stop
/etc/init.d/sysklogd stop  #this was writing to the hard drive every so often
if [[ -x /etc/init.d/ssh ]]; then
    /etc/init.d/ssh stop #I generally don't plan to ssh into my computer when its running battery
fi
#/etc/init.d/apache stop #I don't even know why I have apache...
/etc/init.d/bluetooth stop #i do use bluetooth, but not very often.

# -B1 means most aggressive powersave mode possible and -S12 means
# -spin down HD after 12*5 = 60 seconds of inactivity.
hdparm -B1 -S12 /dev/sda  #spindown timeout for hard drive

echo crt_disable > /proc/acpi/ibm/video #shut off VGA output
echo dvi_disable > /proc/acpi/ibm/video #shut off another output

# My own addition.  Makes the bluetooth light turn off.
echo disable > /proc/acpi/ibm/bluetooth

ifconfig eth0 down #shutdown wired etherent (I can't remember the last time I used it)

modprobe -r pcmcia # unload pcmcia modules, responsible for wakeups
modprobe -r yenta_socket #ditto
modprobe -r uhci_hcd #usb 1.1 drivers, responsible for many many wakeups
#modprobe -r thinkpad_acpi #i usually unload this manually, causes alot of wakeups.  Also screws with suspending if i have it unloaded
modprobe -r e1000e #driver for wired ethernet, almost never need it

# Bluetooth
modprobe -r rfcomm #part of driver for bluetooth
modprobe -r btusb
modprobe -r l2cap
modprobe -r bluetooth #bluetooth driver

# Infrared?
modprobe -r nsc_ircc
modprobe -r irda # i dont use irda

# Firewire
modprobe -r sbp2
modprobe -r ohci1394
modprobe -r ieee1394

amixer set Line mute nocap  #mute line -in, if its not muted AC97 won't powersave
amixer set Mic mute nocap #mute mic, ditto

#iwpriv wlan0 set_power 5 #set powersave mode on wireless.  If I'm not using wireless, I unload the ipw2200 module entirely
# Above doesn't work, but this from powertop might:
echo 5 > /sys/bus/pci/drivers/iwl3945/0000:03:00.0/power_level

# I don't have this, since I have no multibase on battery probably.
#killall hald-addon-storage #stops polling of the cdrom, among other
#things
# I don't have conky.
#killall conky  #conky tends to make alot of wakeups as well.
