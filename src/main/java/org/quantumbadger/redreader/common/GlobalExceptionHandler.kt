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
package org.quantumbadger.redreader.common

import android.content.Context
import android.util.Log
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.activities.BaseActivity
import org.quantumbadger.redreader.activities.BugReportActivity
import org.quantumbadger.redreader.common.AndroidCommon.runOnUiThread
import org.quantumbadger.redreader.common.General.checkThisIsUIThread
import org.quantumbadger.redreader.common.General.readWholeStreamAsUTF8
import java.io.*

class GlobalExceptionHandler private constructor(context: Context) :
	Thread.UncaughtExceptionHandler {
	private val mNextHandler = Thread.getDefaultUncaughtExceptionHandler()
	private val mFile: File

	init {
		mFile = getFileLocation(context)
	}

	override fun uncaughtException(t: Thread, e: Throwable) {
		Log.e(TAG, "Got unhandled exception", e)
		try {
			PrintWriter(FileWriter(mFile, true)).use { writer ->
				writer.println("--- Begin stack trace ---")
				e.printStackTrace(writer)
				writer.println("--- End stack trace ---")
				writer.flush()
			}
		} catch (e2: Exception) {
			Log.e(TAG, "Failed writing exception to disk", e2)
		}
		if (mNextHandler != null) {
			Log.e(TAG, "Invoking next handler")
			mNextHandler.uncaughtException(t, e)
		}
	}

	companion object {
		private const val TAG = "GlobalExceptionHandler"
		private var sCheckedForLastCrash = false
		private fun getFileLocation(context: Context): File {
			return File(context.filesDir, "unhandled_exception.txt")
		}

		fun init(context: Context) {
			Thread.setDefaultUncaughtExceptionHandler(GlobalExceptionHandler(context))
		}

		@JvmStatic
		fun handleLastCrash(activity: BaseActivity) {
			checkThisIsUIThread()
			if (sCheckedForLastCrash) {
				return
			}
			sCheckedForLastCrash = true
			val file = getFileLocation(activity)
			val context = activity.applicationContext
			Thread(Runnable {
				if (!file.exists()) {
					return@Runnable
				}
				val fileText: String
				try {
					FileInputStream(file).use { inStr -> fileText = readWholeStreamAsUTF8(inStr) }
				} catch (e: IOException) {
					Log.e(TAG, "Got exception when reading file", e)
					return@Runnable
				}
				if (!file.delete()) {
					Log.e(TAG, "Unable to delete file")
				}
				runOnUiThread {
					DialogUtils.showDialogPositiveNegative(
						activity,
						context.getString(R.string.error_title_report_previous_crash),
						context.getString(R.string.error_message_report_previous_crash),
						R.string.dialog_yes,
						R.string.dialog_no,
						{
							BugReportActivity.sendBugReport(
								activity, RRError(
									"Previous crash",
									null,
									true,
									null,
									null,
									null,
									fileText
								)
							)
						}
					) {}
				}
			}).start()
		}
	}
}
