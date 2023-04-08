package org.quantumbadger.redreader.common.time

import android.content.res.Resources
import org.quantumbadger.redreader.R

class TimeStringsImpl constructor(
		private val resources: Resources
) : TimeStrings {
	override val year: String
		get() = resources.getString(R.string.time_year)

	override val years: String
		get() = resources.getString(R.string.time_years)

	override val month: String
		get() = resources.getString(R.string.time_month)

	override val months: String
		get() = resources.getString(R.string.time_months)

	override val day: String
		get() = resources.getString(R.string.time_day)

	override val days: String
		get() = resources.getString(R.string.time_days)

	override val hour: String
		get() = resources.getString(R.string.time_hour)

	override val hours: String
		get() = resources.getString(R.string.time_hours)

	override val min: String
		get() = resources.getString(R.string.time_min)

	override val mins: String
		get() = resources.getString(R.string.time_mins)

	override val sec: String
		get() = resources.getString(R.string.time_sec)

	override val secs: String
		get() = resources.getString(R.string.time_secs)

	override val ms: String
		get() = resources.getString(R.string.time_ms)
}
