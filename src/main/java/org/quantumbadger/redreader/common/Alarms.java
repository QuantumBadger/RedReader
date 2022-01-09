/*******************************************************************************
 * This file is part of RedReader.
 *
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.common;

import android.annotation.SuppressLint;
import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import org.quantumbadger.redreader.receivers.NewMessageChecker;
import org.quantumbadger.redreader.receivers.RegularCachePruner;

import java.util.HashMap;
import java.util.Map;

public class Alarms {
	private static final Map<Alarm, AlarmManager> alarmMap = new HashMap<>();
	private static final Map<Alarm, PendingIntent> intentMap = new HashMap<>();

	/*
		An enum to represent an alarm that may be created.
		If you wish to add an alarm, just add it at the top of the enum with the 3 arguments,
		and then call startAlarm() on it.
	 */

	public enum Alarm {
		MESSAGE_CHECKER(AlarmManager.INTERVAL_HALF_HOUR, NewMessageChecker.class, true),
		CACHE_PRUNER(AlarmManager.INTERVAL_HOUR, RegularCachePruner.class, true);

		private final long interval;
		private final Class<? extends BroadcastReceiver> alarmClass;
		private final boolean startOnBoot;

		Alarm(
				final long interval,
				final Class<? extends BroadcastReceiver> alarmClass,
				final boolean startOnBoot) {
			this.interval = interval;
			this.alarmClass = alarmClass;
			this.startOnBoot = startOnBoot;
		}

		private long interval() {
			return interval;
		}

		private Class<? extends BroadcastReceiver> alarmClass() {
			return alarmClass;
		}

		private boolean startOnBoot() {
			return startOnBoot;
		}
	}

	/**
	 * Starts the specified alarm
	 */

	public static void startAlarm(final Alarm alarm, final Context context) {
		if(!alarmMap.containsKey(alarm)) {
			final Intent alarmIntent = new Intent(context, alarm.alarmClass());

			int flags = 0;

			if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
				flags |= PendingIntent.FLAG_IMMUTABLE;
			}

			@SuppressLint("UnspecifiedImmutableFlag")
			final PendingIntent pendingIntent = PendingIntent.getBroadcast(
					context,
					0,
					alarmIntent,
					flags);

			final AlarmManager alarmManager
					= (AlarmManager)(context.getSystemService(Context.ALARM_SERVICE));
			alarmManager.setInexactRepeating(
					AlarmManager.RTC,
					System.currentTimeMillis(),
					alarm.interval(),
					pendingIntent);

			alarmMap.put(alarm, alarmManager);
			intentMap.put(alarm, pendingIntent);
		}
	}

	/**
	 * Stops the specified alarm
	 *
	 * @param alarm alarm to stop
	 */

	public static void stopAlarm(final Alarm alarm) {
		if(alarmMap.containsKey(alarm)) {
			alarmMap.get(alarm).cancel(intentMap.get(alarm));
			alarmMap.remove(alarm);
			intentMap.remove(alarm);
		}
	}

	/**
	 * Starts all alarms that are supposed to start at device boot
	 *
	 * @param context
	 */

	public static void onBoot(final Context context) {
		for(final Alarm alarm : Alarm.values()) {
			if(alarm.startOnBoot()) {
				startAlarm(alarm, context);
			}
		}
	}
}
