#!/usr/bin/env bash

MAX_TRIES=5
COUNT=0

while [  $COUNT -lt $MAX_TRIES ]; do

	bash -c "$*"

	if [ $? -eq 0 ]; then
		exit 0
	fi

	let COUNT=COUNT+1
	
	echo "Failed attempt $COUNT"

	sleep 5

done

echo "Too many failed attempts"

exit 1

