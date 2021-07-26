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
import androidx.annotation.NonNull;
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
import java.util.Locale;

public class RRTime {

	private static final DateTimeFormatter dtFormatter12hr
			= DateTimeFormat.forPattern("yyyy-MM-dd h:mm a");

	private static final DateTimeFormatter dtFormatter24hr
			= DateTimeFormat.forPattern("yyyy-MM-dd HH:mm");

	private static final DateTimeFormatter dtFormatterFilename
			= DateTimeFormat.forPattern("yyyy_MM_dd__HH_mm_ss");


	public static long utcCurrentTimeMillis() {
		return DateTime.now(DateTimeZone.UTC).getMillis();
	}

	public static String formatDateTime(final long utcMs, final Context context) {

		final DateTime dateTime = new DateTime(utcMs);
		final DateTime localDateTime = dateTime.withZone(DateTimeZone.getDefault());

		if(DateFormat.is24HourFormat(context)) {
			return dtFormatter24hr.print(localDateTime);
		} else {
			return dtFormatter12hr.print(localDateTime);
		}
	}

	public static String formatDateTimeFilenameSafe(final long utcMs) {

		final DateTime dateTime = new DateTime(utcMs);
		final DateTime localDateTime = dateTime.withZone(DateTimeZone.getDefault());

		return dtFormatterFilename.print(localDateTime);
	}

	public static String formatDurationFrom(
			final Context context,
			final long startTime,
			final int timeStringRes,
			final int unitsToDisplay) {
		return formatDurationFrom(
				context,
				startTime,
				utcCurrentTimeMillis(),
				timeStringRes,
				unitsToDisplay);
	}

	public static String formatDurationFrom(
			final Context context,
			final long startTime,
			final long endTime,
			final int timeStringRes,
			final int unitsToDisplay) {

		final String space = " ";
		final String comma = ",";
		final String separator = comma + space;

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

		StringBuilder duration
				= new StringBuilder(periodFormatter.print(
						period.normalizedStandard(PeriodType.yearMonthDayTime())));

		final List<String> parts = Arrays.asList(duration.toString().split(comma));
		if(parts.size() >= unitsToDisplay) {
			duration = new StringBuilder(parts.get(0));

			for(int i = 1; i < unitsToDisplay; i ++) {
				duration.append(comma).append(parts.get(i));
			}
		}

		return String.format(context.getString(timeStringRes), duration.toString());
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

	@NonNull
	public static String msToMinutesAndSecondsString(final long ms) {

		if(ms < 0) {
			return "<negative time>";
		}

		final int secondsTotal = (int)(ms / 1000);

		final int mins = secondsTotal / 60;
		final int secs = secondsTotal % 60;

		return String.format(Locale.US, "%d:%02d", mins, secs);
	}
}
