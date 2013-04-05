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
import android.os.Bundle;
import android.view.View;
import android.widget.ScrollView;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.widget.Button;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.RRError;

import java.util.LinkedList;

public class BugReportActivity extends Activity {

	private static final LinkedList<RRError> errors = new LinkedList<RRError>();

	public static synchronized void addGlobalError(RRError error) {
		errors.add(error);
	}

	public static synchronized void handleGlobalError(Context context, String text) {
		handleGlobalError(context, new RRError(text, null, new RuntimeException()));
	}

	public static synchronized void handleGlobalError(Context context, Throwable t) {
		handleGlobalError(context, new RRError(null, null, t));
	}

	public static synchronized void handleGlobalError(Context context, RRError error) {
		errors.add(error);
		final Intent intent = new Intent(context, BugReportActivity.class);
		intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
		context.startActivity(intent);
	}

	private static synchronized LinkedList<RRError> getErrors() {
		final LinkedList<RRError> result = new LinkedList<RRError>(errors);
		errors.clear();
		return result;
	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {

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

		send.setOnClickListener(new View.OnClickListener() {
			public void onClick(View v) {

				final LinkedList<RRError> errors = BugReportActivity.getErrors();

				StringBuilder sb = new StringBuilder(1024);

				sb.append("Error report -- RedReader v" + Constants.version(BugReportActivity.this) + "\r\n\r\n");

				for(RRError error : errors) {
					sb.append("-------------------------------");
					if(error.title != null) sb.append("Title: " + error.title + "\r\n");
					if(error.message != null) sb.append("Message: " + error.message + "\r\n");
					if(error.httpStatus != null) sb.append("HTTP Status: " + error.httpStatus + "\r\n");
					appendException(sb, error.t, 25);
				}

				final Intent intent = new Intent(Intent.ACTION_SEND);
				intent.setType("message/rfc822");
				intent.putExtra(Intent.EXTRA_EMAIL, new String[] {"bug" + "reports" + '@' + "redreader" + '.' + "org"}); // no spam, thanks
				intent.putExtra(Intent.EXTRA_SUBJECT, "Bug Report");
				intent.putExtra(Intent.EXTRA_TEXT, sb.toString());

				try {
					startActivity(Intent.createChooser(intent, "Email bug report"));
				} catch (android.content.ActivityNotFoundException ex) {
					General.quickToast(BugReportActivity.this, "No email apps installed!");
				}

				finish();
			}
		});

		final Button ignore = new Button(this);
		ignore.setText(R.string.bug_button_ignore);

		ignore.setOnClickListener(new View.OnClickListener() {
			public void onClick(View v) {
				finish();
			}
		});

		layout.addView(send);
		layout.addView(ignore);

		final ScrollView sv = new ScrollView(this);
		sv.addView(layout);

		setContentView(sv);
	}

	public static void appendException(StringBuilder sb, Throwable t, int recurseLimit) {
		if(t != null) {
			sb.append("Exception: " + t.getMessage() + "\r\n");
			for(StackTraceElement elem : t.getStackTrace()) {
				sb.append("  " + elem.toString() + "\r\n");
			}

			if(recurseLimit > 0 && t.getCause() != null) {
				sb.append("Caused by: ");
				appendException(sb, t.getCause(), recurseLimit - 1);
			}
		}
	}
}
