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

package org.saiditnet.redreader;

import android.app.Application;
import android.os.Environment;
import android.util.Log;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.common.Alarms;
import org.saiditnet.redreader.io.RedditChangeDataIO;
import org.saiditnet.redreader.receivers.NewMessageChecker;
import org.saiditnet.redreader.reddit.prepared.RedditChangeDataManager;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.util.UUID;

public class RedReader extends Application {

	@Override
	public void onCreate() {

		super.onCreate();

		Log.i("RedReader", "Application created.");

		final Thread.UncaughtExceptionHandler androidHandler = Thread.getDefaultUncaughtExceptionHandler();

		Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
			public void uncaughtException(Thread thread, Throwable t) {

				try {
					t.printStackTrace();

					File dir = Environment.getExternalStorageDirectory();

					if(dir == null) {
						dir = Environment.getDataDirectory();
					}

					final FileOutputStream fos = new FileOutputStream(new File(dir, "redreader_crash_log_" + UUID.randomUUID().toString() + ".txt"));
					final PrintWriter pw = new PrintWriter(fos);
					t.printStackTrace(pw);
					pw.flush();
					pw.close();

				} catch(Throwable t1) {}

				androidHandler.uncaughtException(thread, t);
			}
		});

		final CacheManager cm = CacheManager.getInstance(this);

		new Thread() {
			@Override
			public void run() {

				android.os.Process.setThreadPriority(android.os.Process.THREAD_PRIORITY_BACKGROUND);

				cm.pruneTemp();
				cm.pruneCache(); // Hope for the best :)
			}
		}.start();

		new Thread() {
			@Override
			public void run() {
				RedditChangeDataIO.getInstance(RedReader.this).runInitialReadInThisThread();
				RedditChangeDataManager.pruneAllUsers(RedReader.this);
			}
		}.start();

		Alarms.onBoot(this);

		NewMessageChecker.checkForNewMessages(this);
	}
}
