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

package org.stffnn.reddreader

import android.app.Application
import android.content.Context
import android.os.Process
import android.util.Log
import org.stffnn.reddreader.cache.CacheManager
import org.stffnn.reddreader.common.Alarms
import org.stffnn.reddreader.common.AndroidCommon
import org.stffnn.reddreader.common.Fonts
import org.stffnn.reddreader.common.GlobalConfig
import org.stffnn.reddreader.common.GlobalExceptionHandler
import org.stffnn.reddreader.common.PrefsUtility
import org.stffnn.reddreader.compose.prefs.ComposePrefsSingleton
import org.stffnn.reddreader.io.RedditChangeDataIO
import org.stffnn.reddreader.receivers.NewMessageChecker
import org.stffnn.reddreader.receivers.announcements.AnnouncementDownloader
import org.stffnn.reddreader.reddit.api.RedditOAuth
import org.stffnn.reddreader.reddit.prepared.RedditChangeDataManager

class RedReader : Application() {

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
		PrefsUtility.applyLanguageSetting()
		ComposePrefsSingleton.init(this)
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
