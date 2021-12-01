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

package org.quantumbadger.redreader;

import android.app.Application;
import android.util.Log;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.Alarms;
import org.quantumbadger.redreader.common.Fonts;
import org.quantumbadger.redreader.common.GlobalExceptionHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.io.RedditChangeDataIO;
import org.quantumbadger.redreader.receivers.NewMessageChecker;
import org.quantumbadger.redreader.receivers.announcements.AnnouncementDownloader;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;

public class RedReader extends Application {

	@Override
	public void onCreate() {

		super.onCreate();

		Log.i("RedReader", "Application created.");

		GlobalExceptionHandler.init(this);

		PrefsUtility.init(this);

		Fonts.onAppCreate(getAssets());

		final CacheManager cm = CacheManager.getInstance(this);

		new Thread() {
			@Override
			public void run() {

				android.os.Process.setThreadPriority(android.os.Process.THREAD_PRIORITY_BACKGROUND);

				cm.pruneTemp();
				cm.pruneCache();
			}
		}.start();

		new Thread() {
			@Override
			public void run() {
				RedditChangeDataIO.getInstance(RedReader.this)
						.runInitialReadInThisThread();
				RedditChangeDataManager.pruneAllUsersDefaultMaxAge();
			}
		}.start();

		Alarms.onBoot(this);

		AnnouncementDownloader.performDownload(this);
		NewMessageChecker.checkForNewMessages(this);
	}
}
