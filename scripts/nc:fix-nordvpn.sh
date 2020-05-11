#!/usr/bin/env zsh

# When I suspend or hibernate my laptop with NordVPN connected,
# networking is often totally hosed when the laptop resumes. Turns out
# the problem is due to NordVPN leaving bad 'iptables' rules. This
# solution is based on https://unix.stackexchange.com/a/568906/19827.

sudo pkill -9 nordvpn
sudo iptables --policy INPUT ACCEPT
sudo iptables --policy FORWARD ACCEPT
sudo iptables --policy OUTPUT ACCEPT
sudo iptables -t nat --flush
sudo iptables -t mangle --flush
sudo iptables --flush
sudo iptables -X
