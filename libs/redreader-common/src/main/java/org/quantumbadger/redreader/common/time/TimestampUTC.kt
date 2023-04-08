package org.quantumbadger.redreader.common.time

import kotlinx.datetime.Clock
import kotlinx.datetime.Instant
import kotlinx.datetime.TimeZone
import kotlinx.datetime.toLocalDateTime
import kotlinx.serialization.Serializable
import java.util.*

@Serializable
data class TimestampUTC(
	val value: Instant
) : Comparable<TimestampUTC> {
	companion object {

		@JvmStatic
		fun now() = TimestampUTC(Clock.System.now())

		@JvmStatic
		fun fromUtcMs(value: Long) = TimestampUTC(Instant.fromEpochMilliseconds(value))

		@JvmStatic
		fun fromUtcSecs(value: Long) = TimestampUTC(Instant.fromEpochSeconds(value, 0))

		@JvmField
		val ZERO = TimestampUTC(Instant.fromEpochMilliseconds(0))

		@JvmStatic
		fun oldest(a: TimestampUTC, b: TimestampUTC) = if (a.value < b.value) a else b
	}

	fun toUtcMs() = value.toEpochMilliseconds()

	fun toUtcSecs() = value.epochSeconds

	fun elapsed() = TimePeriod(this, now()).asDuration()

	fun elapsedPeriod() = TimePeriod(this, now())

	fun elapsedPeriodSince(start: TimestampUTC) = TimePeriod(start, this)

	fun format() = localDateTime().run {
		String.format(
				Locale.US,
				"%d-%02d-%02d %02d:%02d",
				year,
				monthNumber,
				dayOfMonth,
				hour,
				minute
		)
	}

	fun formatFilenameSafe() = localDateTime().run {
		String.format(
				Locale.US,
				"%d_%02d_%02d__%02d_%02d_%02d",
				year,
				monthNumber,
				dayOfMonth,
				hour,
				minute,
				second
		)
	}

	private fun localDateTime() = value.toLocalDateTime(TimeZone.currentSystemDefault())

	fun isLessThan(other: TimestampUTC) = value < other.value
	fun isGreaterThan(other: TimestampUTC) = value > other.value

	fun add(duration: TimeDuration) = TimestampUTC(value + duration.value)
	fun subtract(duration: TimeDuration) = TimestampUTC(value - duration.value)

	override fun compareTo(other: TimestampUTC) = value.compareTo(other.value)

	fun hasPassed() = now().isGreaterThan(this)
}
