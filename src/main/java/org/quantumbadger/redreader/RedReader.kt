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

package org.quantumbadger.redreader

import android.content.Context
import android.os.Process
import android.util.Log
import androidx.multidex.MultiDexApplication
import org.quantumbadger.redreader.cache.CacheManager
import org.quantumbadger.redreader.common.*
import org.quantumbadger.redreader.io.RedditChangeDataIO
import org.quantumbadger.redreader.receivers.NewMessageChecker
import org.quantumbadger.redreader.receivers.announcements.AnnouncementDownloader
import org.quantumbadger.redreader.reddit.api.RedditOAuth
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager

class RedReader : MultiDexApplication() {

	companion object {

		const val TAG = "RedReader"

		@JvmStatic
		fun getInstance(context: Context) = context.applicationContext as RedReader
	}

	lateinit var packageInfo: AndroidCommon.PackageInfo

    override fun onCreate() {

		super.onCreate()

		Log.i(TAG, "Application created.")

		packageInfo = AndroidCommon.getPackageInfo(this)

		GlobalExceptionHandler.init(this)
		PrefsUtility.init(this)
		Fonts.onAppCreate(assets)

		RedditOAuth.init(this)

		Log.i(TAG, "Config: " + GlobalConfig.appName + " (" + GlobalConfig.appBuildType + ")")

		val cm = CacheManager.getInstance(this)

		object : Thread() {
            override fun run() {
                Process.setThreadPriority(Process.THREAD_PRIORITY_BACKGROUND)
                cm.pruneTemp()
                cm.pruneCache()
            }
        }.start()

		object : Thread() {
            override fun run() {
                RedditChangeDataIO.getInstance(this@RedReader)
                    .runInitialReadInThisThread()
                RedditChangeDataManager.pruneAllUsersDefaultMaxAge()
            }
        }.start()

		Alarms.onBoot(this)
        AnnouncementDownloader.performDownload(this)
        NewMessageChecker.checkForNewMessages(this)
    }
}
