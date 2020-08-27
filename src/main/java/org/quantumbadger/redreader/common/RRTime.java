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
import org.joda.time.Duration;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.PeriodFormatter;
import org.joda.time.format.PeriodFormatterBuilder;
import org.quantumbadger.redreader.R;

import java.util.Arrays;
import java.util.List;

public class RRTime {

	private static final DateTimeFormatter
			dtFormatter12hr = DateTimeFormat.forPattern("yyyy-MM-dd h:mm a"),
			dtFormatter24hr = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm");


	public static long utcCurrentTimeMillis() {
		return DateTime.now(DateTimeZone.UTC).getMillis();
	}

	public static String formatDateTime(final long utc_ms, final Context context) {

		final DateTime dateTime = new DateTime(utc_ms);
		final DateTime localDateTime = dateTime.withZone(DateTimeZone.getDefault());

		if(DateFormat.is24HourFormat(context)) {
			return dtFormatter24hr.print(localDateTime);
		} else {
			return dtFormatter12hr.print(localDateTime);
		}
	}

	public static String formatDurationFrom(final Context context, final long startTime) {
		final String space = " ";
		final String comma = ",";
		final String separator = comma + space;

		final long endTime = utcCurrentTimeMillis();
		final DateTime dateTime = new DateTime(endTime);
		final DateTime localDateTime = dateTime.withZone(DateTimeZone.getDefault());
		final Period period = new Duration(startTime, endTime).toPeriodTo(localDateTime);

		final PeriodFormatter periodFormatter = new PeriodFormatterBuilder()
				.appendYears()
				.appendSuffix(space)
				.appendSuffix(
						context.getString(R.string.time_year),
						context.getString(R.string.time_years))
				.appendSeparator(separator)
				.appendMonths()
				.appendSuffix(space)
				.appendSuffix(
						context.getString(R.string.time_month),
						context.getString(R.string.time_months))
				.appendSeparator(separator)
				.appendDays()
				.appendSuffix(space)
				.appendSuffix(
						context.getString(R.string.time_day),
						context.getString(R.string.time_days))
				.appendSeparator(separator)
				.appendHours()
				.appendSuffix(space)
				.appendSuffix(
						context.getString(R.string.time_hour),
						context.getString(R.string.time_hours))
				.appendSeparator(separator)
				.appendMinutes()
				.appendSuffix(space)
				.appendSuffix(
						context.getString(R.string.time_min),
						context.getString(R.string.time_mins))
				.appendSeparator(separator)
				.appendSeconds()
				.appendSuffix(space)
				.appendSuffix(
						context.getString(R.string.time_sec),
						context.getString(R.string.time_secs))
				.appendSeparator(separator)
				.appendMillis()
				.appendSuffix(space)
				.appendSuffix(context.getString(R.string.time_ms))
				.toFormatter();

		String duration
				= periodFormatter.print(period.normalizedStandard(PeriodType.yearMonthDayTime()));

		final List<String> parts = Arrays.asList(duration.split(comma));
		if(parts.size() >= 2) {
			duration = parts.get(0) + comma + parts.get(1);
		}

		return String.format(context.getString(R.string.time_ago), duration);
	}

	public static long since(final long timestamp) {
		return utcCurrentTimeMillis() - timestamp;
	}

	public static long secsToMs(final long secs) {
		return secs * 1000;
	}

	public static long minsToMs(final long mins) {
		return secsToMs(mins * 60);
	}

	public static long hoursToMs(final long hours) {
		return minsToMs(hours * 60);
	}
}
