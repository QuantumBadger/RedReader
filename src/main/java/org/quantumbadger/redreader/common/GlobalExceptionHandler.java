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

package org.quantumbadger.redreader.common;

import android.content.Context;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.BugReportActivity;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;

public class GlobalExceptionHandler implements Thread.UncaughtExceptionHandler {

	private static final String TAG = "GlobalExceptionHandler";

	private static boolean sCheckedForLastCrash = false;

	@Nullable private final Thread.UncaughtExceptionHandler mNextHandler;
	@NonNull private final File mFile;

	private static File getFileLocation(@NonNull final Context context) {
		return new File(context.getFilesDir(), "unhandled_exception.txt");
	}

	public static void init(@NonNull final Context context) {
		Thread.setDefaultUncaughtExceptionHandler(new GlobalExceptionHandler(context));
	}

	private GlobalExceptionHandler(@NonNull final Context context) {
		mNextHandler = Thread.getDefaultUncaughtExceptionHandler();
		mFile = getFileLocation(context);
	}

	@Override
	public void uncaughtException(@NonNull final Thread t, @NonNull final Throwable e) {

		Log.e(TAG, "Got unhandled exception", e);

		try(PrintWriter writer = new PrintWriter(new FileWriter(mFile, true))) {
			writer.println("--- Begin stack trace ---");
			e.printStackTrace(writer);
			writer.println("--- End stack trace ---");
			writer.flush();

		} catch (final Exception e2) {
			Log.e(TAG, "Failed writing exception to disk", e2);
		}

		if(mNextHandler != null) {
			Log.e(TAG, "Invoking next handler");
			mNextHandler.uncaughtException(t, e);
		}
	}

	public static void handleLastCrash(@NonNull final BaseActivity activity) {

		General.checkThisIsUIThread();

		if(sCheckedForLastCrash) {
			return;
		}

		sCheckedForLastCrash = true;

		final File file = getFileLocation(activity);
		final Context context = activity.getApplicationContext();

		new Thread(() -> {

			if(!file.exists()) {
				return;
			}

			final String fileText;

			try(InputStream is = new FileInputStream(file)) {
				fileText = General.readWholeStreamAsUTF8(is);

			} catch(final IOException e) {
				Log.e(TAG, "Got exception when reading file", e);
				return;
			}

			if(!file.delete()) {
				Log.e(TAG, "Unable to delete file");
			}

			AndroidCommon.runOnUiThread(() -> {

				DialogUtils.showDialogPositiveNegative(
						activity,
						context.getString(R.string.error_title_report_previous_crash),
						context.getString(R.string.error_message_report_previous_crash),
						R.string.dialog_yes,
						R.string.dialog_no,
						() -> {
							BugReportActivity.sendBugReport(activity, new RRError(
									"Previous crash",
									null,
									true,
									null,
									null,
									null,
									fileText));
						},
						() -> {
							// Do nothing
						}
				);

			});

		}).start();

	}
}
