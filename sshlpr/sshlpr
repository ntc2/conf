#!/bin/sh

# SSH-LPR Backend

# The purpose of the back end is to send print jobs to a remote system
# through ssh and lpr on the remote side. It requires that the user who
# wishes to print have a key-based authentication set up for the server in
# question. The printer URI is sshlpr://[[luser:]ruser@]server/queue where
# ruser is the name of remote user if it is different from the local user
# name, luser is the local user account with the SSH key, if different
# from the user who actually printed, server is the host and queue is the
# print queue name as it would get passed to lpr -P queue.

# With no parameters, we need to tell CUPS what we are.
if [ $# -le 0 ]
then
	echo "network sshlpr \"Unknown\" \"LPR thorugh SSH\""
	exit 0
fi

# If we get the correct number of arguments, as per:
# 	$1=job-id $2=user $3=title $4=copies $5=options $6=[file]
[ $# -lt 5 -o $# -gt 6 ] && exit 1

# Parse URL
user=$2
localuser=$2

[ -z "$DEVICE_URI" ] && exit 1

server="`echo $DEVICE_URI | cut -f 3 -d /`"
printer="`echo $DEVICE_URI | cut -f 4 -d /`"
if echo $server | grep '@' > /dev/null 2> /dev/null
then
	user="`echo $server | cut -f 1 -d '@' `"
	server="`echo $server | cut -f 2 -d '@' `"
fi

if echo $user | grep ':' > /dev/null 2> /dev/null
then
	localuser="`echo $user | cut -f 1 -d ':' `"
	user="`echo $user | cut -f 2 -d ':' `"
fi

echo Doing su $localuser -c "ssh -q ${user}@$server lpr -P $printer -#$4" >&2

# Has CUPS given us a file in $6?
cat $6 | su $localuser -c "ssh -q ${user}@$server lpr -P $printer -#$4" || exit 1
exit 0
