#!/bin/sh
# Run on machine startup as root.

cjail --init
iptables -t nat -I PREROUTING -p tcp --dport 80 -j REDIRECT --to-ports 3222

