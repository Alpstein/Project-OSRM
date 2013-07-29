#!/bin/bash
rm -v /tmp/import.log
for i in $(seq 10); do
    script -a -c 'bash -c "./import.sh scale'$i'"' /tmp/import.log
done
