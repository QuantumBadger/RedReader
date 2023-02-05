package org.quantumbadger.redreader.common.time

import android.content.Context
import org.joda.time.DateTimeZone
import org.joda.time.Period
import org.joda.time.PeriodType
import org.joda.time.format.PeriodFormatterBuilder
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.BetterSSB
import java.util.*

data class TimePeriod(
	val start: TimestampUTC,
	val end: TimestampUTC
) {
	fun format(
		context: Context,
		timeStringRes: Int,
		unitsToDisplay: Int
	): String {
		val space = " "
		val nbsp = BetterSSB.NBSP.toString()
		val comma = ","
		val separator = comma + space

		val period = Period(
			start.value.withZone(DateTimeZone.getDefault()),
			end.value.withZone(DateTimeZone.getDefault())
		)

		val periodFormatter = PeriodFormatterBuilder()
			.appendYears()
			.appendSuffix(nbsp)
			.appendSuffix(
				context.getString(R.string.time_year),
				context.getString(R.string.time_years)
			)
			.appendSeparator(separator)
			.appendMonths()
			.appendSuffix(nbsp)
			.appendSuffix(
				context.getString(R.string.time_month),
				context.getString(R.string.time_months)
			)
			.appendSeparator(separator)
			.appendDays()
			.appendSuffix(nbsp)
			.appendSuffix(
				context.getString(R.string.time_day),
				context.getString(R.string.time_days)
			)
			.appendSeparator(separator)
			.appendHours()
			.appendSuffix(nbsp)
			.appendSuffix(
				context.getString(R.string.time_hour),
				context.getString(R.string.time_hours)
			)
			.appendSeparator(separator)
			.appendMinutes()
			.appendSuffix(nbsp)
			.appendSuffix(
				context.getString(R.string.time_min),
				context.getString(R.string.time_mins)
			)
			.appendSeparator(separator)
			.appendSeconds()
			.appendSuffix(nbsp)
			.appendSuffix(
				context.getString(R.string.time_sec),
				context.getString(R.string.time_secs)
			)
			.appendSeparator(separator)
			.appendMillis()
			.appendSuffix(nbsp)
			.appendSuffix(context.getString(R.string.time_ms))
			.toFormatter()

		var duration = StringBuilder(
			periodFormatter.print(
				period.normalizedStandard(PeriodType.yearMonthDayTime())
			)
		)

		val parts =
			Arrays.asList(
				*duration.toString().split(comma.toRegex()).dropLastWhile { it.isEmpty() }
					.toTypedArray())

		if (parts.size >= unitsToDisplay) {
			duration = StringBuilder(parts[0])
			for (i in 1 until unitsToDisplay) {
				duration.append(comma).append(parts[i])
			}
		}

		return String.format(context.getString(timeStringRes), duration.toString())
	}
}
