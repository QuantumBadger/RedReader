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
