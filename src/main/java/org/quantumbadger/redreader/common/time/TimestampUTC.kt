package org.quantumbadger.redreader.common.time

import android.content.Context
import android.text.format.DateFormat
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.format.DateTimeFormat

data class TimestampUTC(
	val utcMs: Long
) {
	companion object {

		private val dtFormatter12hr = DateTimeFormat.forPattern("yyyy-MM-dd h:mm a")
		private val dtFormatter24hr = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
		private val dtFormatterFilename = DateTimeFormat.forPattern("yyyy_MM_dd__HH_mm_ss")

		fun now() = TimestampUTC(DateTime.now(DateTimeZone.UTC).millis)
	}

	fun elapsed() = DurationMs(now().utcMs - utcMs)

	fun elapsedSince(start: TimestampUTC) = DurationMs(utcMs - start.utcMs)

	fun format(context: Context) : String {
		val dateTime = DateTime(utcMs)
		val localDateTime = dateTime.withZone(DateTimeZone.getDefault())

		return if (DateFormat.is24HourFormat(context)) {
			dtFormatter24hr.print(localDateTime)
		} else {
			dtFormatter12hr.print(localDateTime)
		}
	}

	fun formatFilenameSafe() : String {
		val localDateTime = DateTime(utcMs).withZone(DateTimeZone.getDefault())
		return dtFormatterFilename.print(localDateTime)
	}
}
