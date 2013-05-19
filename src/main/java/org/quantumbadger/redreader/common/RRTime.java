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

import android.content.Context;
import android.text.format.DateFormat;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.quantumbadger.redreader.R;

public class RRTime {

	private static final DateTimeFormatter
			dtFormatter12hr = DateTimeFormat.forPattern("yyyy-MM-dd h:mm a"),
			dtFormatter24hr = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm");


	public static long utcCurrentTimeMillis() {
		return DateTime.now(DateTimeZone.UTC).getMillis();
	}

	public static String formatDateTime(final long utc_ms, final Context context) {

		final DateTime dateTime = new DateTime(utc_ms);
		final DateTime localDateTime  = dateTime.withZone(DateTimeZone.getDefault());

		if(DateFormat.is24HourFormat(context)) {
			return dtFormatter24hr.print(localDateTime);
		} else {
			return dtFormatter12hr.print(localDateTime);
		}
	}

	public static String formatDurationMsAgo(final Context context, final long totalMs) {
		return String.format(context.getString(R.string.time_ago), formatDurationMs(totalMs));
	}

	// TODO externalise strings
	// TODO tidy this up
	public static String formatDurationMs(final long totalMs) {

		long ms = totalMs;

		final long years = ms / (365L * 24L * 60L * 60L * 1000L);
		ms %= (365L * 24L * 60L * 60L * 1000L);

		final long months = ms / (30L * 24L * 60L * 60L * 1000L);
		ms %= (30L * 24L * 60L * 60L * 1000L);

		if(years > 0) {
			if(months > 0) {
				return String.format("%d %s, %d %s", years, s("year", years), months, s("month", months));
			} else {
				return String.format("%d %s", years, s("year", years));
			}
		}

		final long days = ms / (24L * 60L * 60L * 1000L);
		ms %= (24L * 60L * 60L * 1000L);

		if(months > 0) {
			if(days > 0) {
				return String.format("%d %s, %d %s", months, s("month", months), days, s("day", days));
			} else {
				return String.format("%d %s", months, s("month", months));
			}
		}

		final long hours = ms / (60L * 60L * 1000L);
		ms %= (60L * 60L * 1000L);

		if(days > 0) {
			if(hours > 0) {
				return String.format("%d %s, %d %s", days, s("day", days), hours, s("hour", hours));
			} else {
				return String.format("%d %s", days, s("day", days));
			}
		}

		final long mins = ms / (60L * 1000L);
		ms %= (60L * 1000L);

		if(hours > 0) {
			if(mins > 0) {
				return String.format("%d %s, %d %s", hours, s("hour", hours), mins, s("min", mins));
			} else {
				return String.format("%d %s", hours, s("hour", hours));
			}
		}

		final long secs = ms / 1000;
		ms %= 1000;

		if(mins > 0) {
			if(secs > 0) {
				return String.format("%d %s, %d %s", mins, s("min", mins), secs, s("sec", secs));
			} else {
				return String.format("%d %s", mins, s("min", mins));
			}
		}

		if(secs > 0) {
			if(ms > 0) {
				return String.format("%d %s, %d %s", secs, s("sec", secs), ms, "ms");
			} else {
				return String.format("%d %s", secs, s("sec", secs));
			}
		}

		return ms + " ms";
	}

	// TODO use the Android string stuff
	private static String s(final String str, final long n) {
		if(n == 1) return str;
		else return str + "s";
	}

	public static long since(long timestamp) {
		return utcCurrentTimeMillis() - timestamp;
	}
}
