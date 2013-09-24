#!/bin/bash -xe
# host running wrapper
WRAPPERHOST="127.0.0.1"
# map profiles to hosts
PROFILES="bicycle:127.0.0.1"

source /etc/osrmwrapper

INSTANCE="$1"
test -n "$INSTANCE"
# INSTANCE="5000"

DATADIR=$PWD/build/data
RSYNC="rsync -avzW"
if [ "$WRAPPERHOST" = "127.0.0.1" ]; then
    WRAPPERHOST=""
else
    WRAPPERHOST="$WRAPPERHOST:"
fi
$RSYNC $DATADIR/test.ways.dbm ${WRAPPERHOST}/usr/local/lib/osrm-data/wrapper/test.$INSTANCE.ways.dbm
for PH in $PROFILES; do
    P="$(echo $PH|cut -f 1 -d :)"
    H="$(echo $PH|cut -f 2 -d :)"
    ssh $H sudo /usr/sbin/service osrm$INSTANCE stop
    $RSYNC $DATADIR/$P/ $H:/usr/local/lib/osrm-data/$INSTANCE/
    ssh $H sudo /usr/sbin/service osrm$INSTANCE start
    RETRY="600"
    while ! wget -S -O - http://$H:$INSTANCE/hello && [ $RETRY -gt 0 ]; do
	sleep 1
	RETRY=$[RETRY-1]
    done
    [ $RETRY -gt 0 ]
done
