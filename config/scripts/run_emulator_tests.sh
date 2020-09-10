#!/usr/bin/env bash

set -x

EMUPLAT="$1"

echo "Downloading emulator image for $EMUPLAT"

set -e

sdkmanager --update

sdkmanager "$EMUPLAT"

yes | sdkmanager --licenses

echo no | avdmanager create avd --force -n test -k "$EMUPLAT" --device "Nexus 4"

AVD_CONFIG_PATH=~/.android/avd/test.avd/config.ini

if [ -f $AVD_CONFIG_PATH ]; then
	echo "vm.heapSize=64" >> $AVD_CONFIG_PATH
	echo "hw.keyboard=yes" >> $AVD_CONFIG_PATH
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
        -no-window \
        -no-snapshot \
        -no-boot-anim &

EMULATOR_PID=$!

echo "Emulator process ID: $EMULATOR_PID"

trap "kill $EMULATOR_PID" EXIT

while [ "`adb shell getprop sys.boot_completed | tr -d '\r' `" != "1" ] ; do
	echo "Waiting for emulator..."
	sleep 10;
done


sleep 10

echo "Disabling transitions"

adb shell settings put global window_animation_scale 0.0
adb shell settings put global transition_animation_scale 0.0
adb shell settings put global animator_duration_scale 0.0
adb shell settings put system screen_off_timeout 1800000

adb shell "echo \"update system set value=0.0 where name='window_animation_scale';\" | sqlite3 /data/data/com.android.providers.settings/databases/settings.db"
adb shell "echo \"update system set value=0.0 where name='transition_animation_scale';\" | sqlite3 /data/data/com.android.providers.settings/databases/settings.db"

set -e

adb reboot

sleep 10

while [ "`adb shell getprop sys.boot_completed | tr -d '\r' `" != "1" ] ; do
	echo "Waiting for emulator..."
	sleep 10;
done

sleep 10

adb shell input keyevent 82

echo "Running tests..."

./config/scripts/retry.sh ./gradlew check connectedCheck

echo "Completed successfully"


