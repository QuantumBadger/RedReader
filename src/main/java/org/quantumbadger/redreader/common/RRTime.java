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

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import java.util.HashSet;
import java.util.Locale;

public class RRTime {

	private static final DateTimeFormatter dtFormatter;
	private static final HashSet<String> twelveHourCountries;

	static {
		twelveHourCountries = new HashSet<String>();

		twelveHourCountries.add("AUS");
		twelveHourCountries.add("GBR");
		twelveHourCountries.add("BGD");
		twelveHourCountries.add("CAN");
		twelveHourCountries.add("COL");
		twelveHourCountries.add("CRI");
		twelveHourCountries.add("EGY");
		twelveHourCountries.add("SLV");
		twelveHourCountries.add("GHA");
		twelveHourCountries.add("HND");
		twelveHourCountries.add("IRN");
		twelveHourCountries.add("JOR");
		twelveHourCountries.add("KOR");
		twelveHourCountries.add("MYS");
		twelveHourCountries.add("MEX");
		twelveHourCountries.add("NZL");
		twelveHourCountries.add("NPL");
		twelveHourCountries.add("NIC");
		twelveHourCountries.add("NGA");
		twelveHourCountries.add("PHL");
		twelveHourCountries.add("SAU");
		twelveHourCountries.add("SGP");
		twelveHourCountries.add("TWN");
		twelveHourCountries.add("VEN");
		twelveHourCountries.add("USA");

		if(twelveHourCountries.contains(Locale.getDefault().getISO3Country())) {
			dtFormatter = DateTimeFormat.forPattern("yyyy-MM-dd h:mm a"); // 12 hour

		} else {
			dtFormatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm"); // 24 hour
		}
	}

	public static long utcCurrentTimeMillis() {
		return DateTime.now(DateTimeZone.UTC).getMillis();
	}

	public static String formatDateTime(final long utc_ms) {

		final DateTime dateTime = new DateTime(utc_ms);
		final DateTime localDateTime  = dateTime.withZone(DateTimeZone.getDefault());

		return dtFormatter.print(localDateTime);
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
