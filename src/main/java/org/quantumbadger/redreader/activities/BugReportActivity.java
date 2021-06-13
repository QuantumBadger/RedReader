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

package org.quantumbadger.redreader.activities;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.RRError;

import java.util.ArrayList;
import java.util.LinkedList;

public class BugReportActivity extends BaseActivity {

	private static final ArrayList<RRError> errors = new ArrayList<>();

	public static synchronized void addGlobalError(final RRError error) {
		errors.add(error);
	}

	public static synchronized void handleGlobalError(final Context context, final String text) {
		handleGlobalError(context, new RRError(text, null, true, new RuntimeException()));
	}

	public static synchronized void handleGlobalError(final Context context, final Throwable t) {

		if(t != null) {
			Log.e("BugReportActivity", "Handling exception", t);
		}

		handleGlobalError(context, new RRError(null, null, true, t));
	}

	public static synchronized void handleGlobalError(
			final Context context,
			final RRError error) {

		addGlobalError(error);

		AndroidCommon.UI_THREAD_HANDLER.post(() -> {
			final Intent intent = new Intent(context, BugReportActivity.class);
			intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
			context.startActivity(intent);
		});

	}

	private static synchronized LinkedList<RRError> getErrors() {
		final LinkedList<RRError> result = new LinkedList<>(errors);
		errors.clear();
		return result;
	}

	public static void sendBugReport(
			@NonNull final Context context,
			@NonNull final RRError error) {

		sendBugReport(context, General.listOfOne(error));
	}

	public static void sendBugReport(
			@NonNull final Context context,
			@NonNull final Iterable<RRError> errors) {

		final StringBuilder sb = new StringBuilder(1024);

		sb.append("Error report -- RedReader v")
				.append(Constants.version(context))
				.append("\r\n\r\n");

		sb.append("Manufacturer: ").append(Build.MANUFACTURER).append("\r\n");
		sb.append("Model: ").append(Build.MODEL).append("\r\n");
		sb.append("Product: ").append(Build.PRODUCT).append("\r\n");
		sb.append("Android release: ").append(Build.VERSION.RELEASE).append("\r\n");
		sb.append("Android SDK: ").append(Build.VERSION.SDK_INT).append("\r\n");

		for(final RRError error : errors) {
			sb.append("\r\n-------------------------------\r\n");
			if(error.title != null) {
				sb.append("Title: ").append(error.title).append("\r\n");
			}
			if(error.message != null) {
				sb.append("Message: ").append(error.message).append("\r\n");
			}
			if(error.httpStatus != null) {
				sb.append("HTTP Status: ").append(error.httpStatus).append("\r\n");
			}
			if(error.url != null) {
				sb.append("URL: ").append(error.url).append("\r\n");
			}
			if(error.debuggingContext != null) {
				sb.append("Debugging context: ").append(error.debuggingContext).append("\r\n");
			}
			if(error.response != null) {
				sb.append("Response: ").append(error.response.toString()).append("\r\n");
			}
			appendException(sb, error.t, 25);
		}

		final Intent intent = new Intent(Intent.ACTION_SENDTO);
		intent.putExtra(
				Intent.EXTRA_EMAIL,
				new String[] {
						"bug"
								+ "reports"
								+ (char)64
								+ "redreader"
								+ '.'
								+ "org"}); // no spam, thanks
		intent.putExtra(Intent.EXTRA_SUBJECT, "Bug Report");
		intent.putExtra(Intent.EXTRA_TEXT, sb.toString());

		if(Build.VERSION.SDK_INT >= 15) {
			final Intent emailSelectorIntent = new Intent(Intent.ACTION_SENDTO);
			emailSelectorIntent.setData(Uri.parse("mailto:"));
			intent.setSelector(emailSelectorIntent);
		}

		try {
			context.startActivity(Intent.createChooser(
					intent,
					context.getApplicationContext()
							.getString(R.string.bug_chooser_title)));

		} catch(final android.content.ActivityNotFoundException ex) {
			General.quickToast(context, R.string.error_toast_no_email_apps);
		}
	}

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);

		final LinearLayout layout = new LinearLayout(this);
		layout.setOrientation(LinearLayout.VERTICAL);

		final TextView title = new TextView(this);
		title.setText(R.string.bug_title);
		layout.addView(title);
		title.setTextSize(20.0f);

		final TextView text = new TextView(this);
		text.setText(R.string.bug_message);

		layout.addView(text);
		text.setTextSize(15.0f);

		final int paddingPx = General.dpToPixels(this, 20);
		title.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);
		text.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

		final Button send = new Button(this);
		send.setText(R.string.bug_button_send);

		send.setOnClickListener(v -> {
			sendBugReport(this, getErrors());
			finish();
		});

		final Button ignore = new Button(this);
		ignore.setText(R.string.bug_button_ignore);

		ignore.setOnClickListener(v -> finish());

		layout.addView(send);
		layout.addView(ignore);

		final ScrollView sv = new ScrollView(this);
		sv.addView(layout);

		setBaseActivityListing(sv);
	}

	public static void appendException(
			final StringBuilder sb,
			final Throwable t,
			final int recurseLimit) {

		if(t != null) {

			sb.append("Exception: ");
			sb.append(t.getClass().getCanonicalName()).append("\r\n");
			sb.append(t.getMessage()).append("\r\n");

			for(final StackTraceElement elem : t.getStackTrace()) {
				sb.append("  ").append(elem.toString()).append("\r\n");
			}

			if(recurseLimit > 0 && t.getCause() != null) {
				sb.append("Caused by: ");
				appendException(sb, t.getCause(), recurseLimit - 1);
			}
		}
	}
}
