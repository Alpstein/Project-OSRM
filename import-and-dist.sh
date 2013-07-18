#!/bin/bash -xe
# host running wrapper
WRAPPERHOST="127.0.0.1"
source /etc/osrmwrapper

test -n "$1"
test -n "$2"
script -c '/usr/bin/time -v bash -c "{ ./import.sh '"$1"' && ./dist-data.sh '"$2"' && ssh '"$WRAPPERHOST"' sudo /usr/sbin/service apache2 restart ; }"' /tmp/import.log
