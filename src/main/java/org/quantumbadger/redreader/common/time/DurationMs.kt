package org.quantumbadger.redreader.common.time

data class DurationMs(
	val durationMs: Long
) : Comparable<DurationMs> {

	override fun compareTo(other: DurationMs) = durationMs.compareTo(other.durationMs)
}
