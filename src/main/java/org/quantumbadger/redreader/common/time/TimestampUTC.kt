package org.quantumbadger.redreader.common.time

import android.content.Context
import android.os.Parcelable
import android.text.format.DateFormat
import kotlinx.parcelize.Parcelize
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.format.DateTimeFormat

@Parcelize
data class TimestampUTC(
	val value: DateTime
) : Parcelable {
	companion object {

		private val dtFormatter12hr = DateTimeFormat.forPattern("yyyy-MM-dd h:mm a")
		private val dtFormatter24hr = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
		private val dtFormatterFilename = DateTimeFormat.forPattern("yyyy_MM_dd__HH_mm_ss")

		@JvmStatic
		fun now() = TimestampUTC(DateTime.now(DateTimeZone.UTC))

		@JvmStatic
		// TODO incorrectly using this for comments
		fun fromUtcMs(value: Long) = TimestampUTC(DateTime(value, DateTimeZone.UTC))

		@JvmStatic
		fun fromUtcSecs(value: Long) = fromUtcMs(value * 1000)
	}

	fun toUtcMs() = value.millis

	fun toUtcSecs() = toUtcMs() / 1000

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
