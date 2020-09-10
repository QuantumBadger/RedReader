#!/usr/bin/env bash

set -x

EMUPLAT="$1"

echo "Downloading emulator image for $EMUPLAT"

set -e

sdkmanager --update

sdkmanager "$EMUPLAT"

yes | sdkmanager --licenses

echo no | avdmanager create avd --force -n test -k "$EMUPLAT" --device "Nexus 4"

if [ -f "/home/user/.android/avd/test.avd/config.ini" ]; then
	echo "vm.heapSize=64" >> /home/user/.android/avd/test.avd/config.ini
else 
    echo "Cannot find emulator config file"
	exit 1
fi


set +e

echo "Starting emulator..."

$ANDROID_HOME/emulator/emulator \
		-avd test \
		-memory 3072 \
        -no-audio \
        -gpu guest \
        -no-snapshot \
        -no-window \
        -no-boot-anim &

EMULATOR_PID=$!

echo "Emulator process ID: $EMULATOR_PID"

trap "kill $EMULATOR_PID" EXIT

while [ "`adb shell getprop sys.boot_completed | tr -d '\r' `" != "1" ] ; do
	echo "Waiting for emulator..."
	sleep 10;
done

set -e

sleep 10

echo "Disabling transitions"

adb shell settings put global window_animation_scale 0.0
adb shell settings put global transition_animation_scale 0.0
adb shell settings put global animator_duration_scale 0.0

echo "Running tests..."

./config/scripts/retry.sh ./gradlew check connectedCheck

echo "Completed successfully"


