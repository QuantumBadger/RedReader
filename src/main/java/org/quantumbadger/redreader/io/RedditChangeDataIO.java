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

package org.quantumbadger.redreader.io;

import android.content.Context;
import androidx.annotation.NonNull;
import android.util.Log;
import org.quantumbadger.redreader.common.TriggerableThread;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicBoolean;

public class RedditChangeDataIO {

	private static final String TAG = "RedditChangeDataIO";

	private static final int DB_VERSION = 1;
	private static final String DB_FILENAME = "rr_change_data.dat";
	private static final String DB_WRITETMP_FILENAME = "rr_change_data_tmp.dat";

	private static RedditChangeDataIO INSTANCE;
	private static boolean STATIC_UPDATE_PENDING = false;

	@NonNull
	public static synchronized RedditChangeDataIO getInstance(final Context context) {

		if(INSTANCE == null) {
			INSTANCE = new RedditChangeDataIO(context);

			if(STATIC_UPDATE_PENDING) {
				INSTANCE.notifyUpdate();
			}
		}

		return INSTANCE;
	}

	public static synchronized void notifyUpdateStatic() {

		if(INSTANCE != null) {
			INSTANCE.notifyUpdate();
		} else {
			STATIC_UPDATE_PENDING = true;
		}
	}

	private final Context mContext;
	private final Object mLock = new Object();

	private final AtomicBoolean mIsInitialReadStarted = new AtomicBoolean(false);
	private boolean mIsInitialReadComplete = false;
	private boolean mUpdatePending = false;

	private final class WriteRunnable implements Runnable {
		@Override
		public void run() {

			final long startTime = System.currentTimeMillis();

			try {

				final File dataFileTmpLocation = getDataFileWriteTmpLocation();

				Log.i(TAG, String.format(Locale.US, "Writing tmp data file at '%s'", dataFileTmpLocation.getAbsolutePath()));

				final ExtendedDataOutputStream dos
						= new ExtendedDataOutputStream(
								new BufferedOutputStream(
										new FileOutputStream(dataFileTmpLocation),
										64 * 1024));

				dos.writeInt(DB_VERSION);

				RedditChangeDataManager.writeAllUsers(dos);

				dos.flush();
				dos.close();

				Log.i(TAG, "Write successful. Atomically replacing data file...");

				final File dataFileLocation = getDataFileLocation();

				if(!dataFileTmpLocation.renameTo(dataFileLocation)) {
					Log.e(TAG, "Atomic replace failed!");
					return;
				}

				Log.i(TAG, "Write complete.");

				final long bytes = dataFileLocation.length();
				final long duration = System.currentTimeMillis() - startTime;

				Log.i(TAG, String.format(Locale.US, "%d bytes written in %d ms", bytes, duration));

			} catch(final IOException e) {
				Log.e(TAG, "Write failed!", e);
			}
		}
	}

	private final TriggerableThread mWriteThread = new TriggerableThread(new WriteRunnable(), 5000);

	private RedditChangeDataIO(final Context context) {
		mContext = context;
	}

	private void notifyUpdate() {

		synchronized(mLock) {

			if(mIsInitialReadComplete) {
				triggerUpdate();
			} else {
				mUpdatePending = true;
			}
		}
	}

	private File getDataFileLocation() {
		return new File(mContext.getFilesDir(), DB_FILENAME);
	}

	private File getDataFileWriteTmpLocation() {
		return new File(mContext.getFilesDir(), DB_WRITETMP_FILENAME);
	}

	public void runInitialReadInThisThread() {

		if(mIsInitialReadStarted.getAndSet(true)) {
			throw new RuntimeException("Attempted to run initial read twice!");
		}

		Log.i(TAG, "Running initial read...");

		try {
			final File dataFileLocation = getDataFileLocation();

			Log.i(TAG, String.format(Locale.US, "Data file at '%s'", dataFileLocation.getAbsolutePath()));

			if(!dataFileLocation.exists()) {
				Log.i(TAG, "Data file does not exist. Aborting read.");
				return;
			}

			final ExtendedDataInputStream dis
					= new ExtendedDataInputStream(
							new BufferedInputStream(
									new FileInputStream(dataFileLocation),
									64 * 1024));

			try {

				final int version = dis.readInt();

				if(DB_VERSION != version) {
					Log.i(TAG, String.format(Locale.US, "Wanted version %d, got %d. Aborting read.", DB_VERSION, version));
					return;
				}

				RedditChangeDataManager.readAllUsers(dis, mContext);

				Log.i(TAG, "Initial read successful.");

			} finally {
				try {
					dis.close();
				} catch(final IOException e) {
					Log.e(TAG, "IO error while trying to close input file", e);
				}
			}

		} catch(final Exception e) {
			Log.e(TAG, "Initial read failed", e);

		} finally {
			notifyInitialReadComplete();
		}
	}

	private void notifyInitialReadComplete() {

		synchronized(mLock) {
			mIsInitialReadComplete = true;

			if(mUpdatePending) {
				triggerUpdate();
				mUpdatePending = false;
			}
		}
	}

	private void triggerUpdate() {
		mWriteThread.trigger();
	}
}
