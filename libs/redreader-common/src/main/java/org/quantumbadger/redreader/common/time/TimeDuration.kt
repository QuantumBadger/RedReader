package org.quantumbadger.redreader.common.time

import kotlin.time.Duration
import kotlin.time.Duration.Companion.days
import kotlin.time.Duration.Companion.hours
import kotlin.time.Duration.Companion.milliseconds
import kotlin.time.Duration.Companion.minutes
import kotlin.time.Duration.Companion.seconds

data class TimeDuration(
	val value: Duration
) {
	fun toMs() = value.inWholeMilliseconds

	fun isLessThan(other: TimeDuration) = value < other.value
	fun isGreaterThan(other: TimeDuration) = value > other.value

	companion object {
		@JvmStatic
		fun ms(value: Long) = TimeDuration(value.milliseconds)

		@JvmStatic
		fun secs(value: Long) = TimeDuration(value.seconds)

		@JvmStatic
		fun minutes(value: Long) = TimeDuration(value.minutes)

		@JvmStatic
		fun hours(value: Long) = TimeDuration(value.hours)

		@JvmStatic
		fun days(value: Long) = TimeDuration(value.days)
	}
}
