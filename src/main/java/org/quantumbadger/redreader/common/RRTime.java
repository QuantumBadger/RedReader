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
import android.support.annotation.StringRes;
import android.text.format.DateFormat;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.Duration;
import org.joda.time.Period;
import org.joda.time.PeriodType;
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
		final DateTime localDateTime = dateTime.withZone(DateTimeZone.getDefault());

		if (DateFormat.is24HourFormat(context)) {
			return dtFormatter24hr.print(localDateTime);
		} else {
			return dtFormatter12hr.print(localDateTime);
		}
	}

	public static String formatDurationFrom(final Context context, final long startTime) {

		final long endTime = utcCurrentTimeMillis();
		final DateTime dateTime = new DateTime(endTime);
		final DateTime localDateTime = dateTime.withZone(DateTimeZone.getDefault());
		Period period = new Duration(startTime, endTime).toPeriodTo(localDateTime).normalizedStandard(PeriodType.yearMonthDayTime());

		String formattedDuration = formatDuration(context, period.getYears(), R.string.time_year, R.string.time_years);
		if (formattedDuration == null) {
			formattedDuration = formatDuration(context, period.getMonths(), R.string.time_month, R.string.time_months);
		}
		if (formattedDuration == null) {
			formattedDuration = formatDuration(context, period.getDays(), R.string.time_day, R.string.time_days);
		}
		if (formattedDuration == null) {
			formattedDuration = formatDuration(context, period.getHours(), R.string.time_hour, R.string.time_hours);
		}
		if (formattedDuration == null) {
			formattedDuration = formatDuration(context, period.getMinutes(), R.string.time_min, R.string.time_mins);
		}
		if (formattedDuration == null) {
			// No need to being detailed as RedReader doesn't update the displayed timestamps
			// making any displayed detailed duration out of date almost immediately.
			formattedDuration = formatDuration(context, 1, R.string.time_min, R.string.time_mins);
		}

		return String.format(context.getString(R.string.time_ago), formattedDuration);
	}

	private static String formatDuration(Context context, int value, @StringRes int singleId, @StringRes int pluralId) {
		switch (value) {
			case 0:
				return null;
			case 1:
				return "1 " + context.getString(singleId);
			default:
				return String.valueOf(value) + " " + context.getString(pluralId);
		}
	}

	public static long since(long timestamp) {
		return utcCurrentTimeMillis() - timestamp;
	}
}
