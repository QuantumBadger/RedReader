package org.quantumbadger.redreader.common.time

import org.joda.time.Duration

data class TimeDuration(
	val value: Duration
) {
	fun toMs() = value.millis
}
