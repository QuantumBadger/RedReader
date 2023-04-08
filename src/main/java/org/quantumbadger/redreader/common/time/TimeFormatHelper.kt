package org.quantumbadger.redreader.common.time

import android.content.Context
import androidx.annotation.StringRes
import java.util.*

object TimeFormatHelper {

	@JvmStatic
	fun format(
			period: TimePeriod,
			context: Context,
			@StringRes stringRes: Int,
			unitsToDisplay: Int
	) : String {

		val res = context.resources

		return String.format(Locale.US, res.getString(stringRes), period.format(
				TimeStringsImpl(res),
				unitsToDisplay
		))
	}

}
