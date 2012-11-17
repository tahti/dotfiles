#!/bin/bash
# Quick a dirty script to make a list of internal ips on a LAN
# Questions, Comments or Death Threats can be sent to crackers@question-defense.com
# This is made for Backtrack 4 so every one else is on their own
 
#set some variables
subnet=$(echo $2 | cut -f 1 -d .)
outfile=$subnet"_ip.list"
temp=ip.tmp
 
#Check for the proper arguments
if [ -z "$1" ]; then
    echo usage: $0 interface subnet
    echo "example: arp-scan eth0 192.168.1.0/24"
    exit
fi
 
if [ -z "$2" ]; then
    echo usage: $0 interface subnet
    echo "example: arp-scan eth0 192.168.1.0/24"
    echo "example: arp-scan eth0 192.168.1.0:255.255.255.0"
    exit
fi
 
#check for arp-scan
echo "Checking for arp-scan"
dpkg --status arp-scan | grep -q not-installed
 
if [ $? -eq 0 ]; then
    echo "Downloading arp-scan...."
    sudo apt-get install arp-scan -y
    else
    echo "arp-scan found!"
    fi
 
#running the scan
/usr/bin/arp-scan $1 $2 > $temp 2>/dev/null
cat ip.tmp | grep $subnet  | awk {'print $1'} > $outfile
rm -rf $temp
 
count=$(wc -l $outfile | awk {'print $1'})
echo $count "active ip's found"
dir=$(pwd)
echo "Your file is named" $outfile "and is located in the" $dir
