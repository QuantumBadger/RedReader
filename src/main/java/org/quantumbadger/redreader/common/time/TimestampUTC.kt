package org.quantumbadger.redreader.common.time

import android.content.Context
import android.text.format.DateFormat
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.format.DateTimeFormat

data class TimestampUTC(
	val value: DateTime
) {
	companion object {

		private val dtFormatter12hr = DateTimeFormat.forPattern("yyyy-MM-dd h:mm a")
		private val dtFormatter24hr = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
		private val dtFormatterFilename = DateTimeFormat.forPattern("yyyy_MM_dd__HH_mm_ss")

		fun now() = TimestampUTC(DateTime.now(DateTimeZone.UTC))

		fun fromUtcMs(value: Long) = TimestampUTC(DateTime(value, DateTimeZone.UTC))
	}

	fun toUtcMs() = value.millis

	fun elapsedPeriod() = TimePeriod(this, now())

	fun elapsedPeriodSince(start: TimestampUTC) = TimePeriod(start, this)

	fun format(context: Context) : String {
		val localDateTime = value.withZone(DateTimeZone.getDefault())

		return if (DateFormat.is24HourFormat(context)) {
			dtFormatter24hr.print(localDateTime)
		} else {
			dtFormatter12hr.print(localDateTime)
		}
	}

	fun formatFilenameSafe() : String {
		val localDateTime = value.withZone(DateTimeZone.getDefault())
		return dtFormatterFilename.print(localDateTime)
	}
}
