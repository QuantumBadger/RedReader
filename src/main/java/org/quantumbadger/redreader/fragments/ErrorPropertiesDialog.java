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

package org.quantumbadger.redreader.fragments;

import android.content.Context;
import android.os.Bundle;
import android.widget.LinearLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.common.UriString;

import java.net.SocketTimeoutException;
import java.net.UnknownHostException;

public final class ErrorPropertiesDialog extends PropertiesDialog {

	private static final String ARG_TITLE = "title";
	private static final String ARG_MESSAGE = "message";
	private static final String ARG_HTTP_STATUS = "httpStatus";
	private static final String ARG_URL = "url";
	private static final String ARG_EXCEPTION = "t";
	private static final String ARG_RESPONSE = "response";
	private static final String ARG_DEBUGGING_CONTEXT = "debuggingContext";
	private static final String ARG_REPORTABLE = "reportable";

	// Everything this dialog needs is stored in its arguments Bundle so that it
	// survives the activity being recreated (rotation, process death, etc). The
	// Bundle is serialized along with the rest of the activity state, and the whole
	// lot has to fit within the Binder transaction limit (~1MB, shared with everything
	// else), so large fields such as HTTP response bodies are truncated.
	private static final int MAX_SHORT_FIELD_LENGTH = 2_000;
	private static final int MAX_LONG_FIELD_LENGTH = 16_000;

	// Note: this class must have a public no-argument constructor (the implicit
	// default). The FragmentManager re-instantiates this dialog via reflection when
	// the activity is recreated while the dialog is showing.

	public static ErrorPropertiesDialog newInstance(@NonNull final RRError error) {

		final ErrorPropertiesDialog dialog = new ErrorPropertiesDialog();

		final Bundle args = new Bundle();

		args.putString(
				ARG_TITLE,
				StringUtils.truncateNullable(error.title, MAX_SHORT_FIELD_LENGTH));
		args.putString(
				ARG_MESSAGE,
				StringUtils.truncateNullable(error.message, MAX_SHORT_FIELD_LENGTH));

		if(error.t != null) {
			final StringBuilder sb = new StringBuilder(1024);
			BugReportActivity.appendException(sb, error.t, 25);
			args.putString(
					ARG_EXCEPTION,
					StringUtils.truncate(sb.toString(), MAX_LONG_FIELD_LENGTH));
		}

		if(error.httpStatus != null) {
			args.putInt(ARG_HTTP_STATUS, error.httpStatus);
		}

		if(error.url != null) {
			args.putString(
					ARG_URL,
					StringUtils.truncate(error.url.value, MAX_SHORT_FIELD_LENGTH));
		}

		if(error.responseString != null) {
			args.putString(
					ARG_RESPONSE,
					StringUtils.truncate(error.responseString, MAX_LONG_FIELD_LENGTH));
		}

		if(error.debuggingContext != null) {
			args.putString(
					ARG_DEBUGGING_CONTEXT,
					StringUtils.truncate(error.debuggingContext, MAX_LONG_FIELD_LENGTH));
		}

		args.putBoolean(
				ARG_REPORTABLE,
				error.reportable
						&& !(error.t instanceof UnknownHostException)
						&& !(error.t instanceof SocketTimeoutException));

		dialog.setArguments(args);

		return dialog;
	}

	@Override
	protected void interceptBuilder(@NonNull final MaterialAlertDialogBuilder builder) {

		if(requireArguments().getBoolean(ARG_REPORTABLE, false)) {

			builder.setPositiveButton(
					R.string.button_error_send_report,
					(dialog, which) -> BugReportActivity.sendBugReport(
							requireContext(),
							errorFromArguments()));
		}
	}

	/**
	 * Reconstructs an RRError from the (possibly truncated) values stored in the
	 * arguments Bundle, for use in the bug report. The original Throwable cannot be
	 * restored, so its pre-rendered stack trace is included as debugging context.
	 */
	@NonNull
	private RRError errorFromArguments() {

		final Bundle args = requireArguments();

		final String debuggingContext = args.getString(ARG_DEBUGGING_CONTEXT);
		final String exception = args.getString(ARG_EXCEPTION);

		final StringBuilder context = new StringBuilder();

		if(debuggingContext != null) {
			context.append(debuggingContext);
		}

		if(exception != null) {
			if(context.length() > 0) {
				context.append("\r\n");
			}
			context.append(exception);
		}

		return new RRError(
				args.getString(ARG_TITLE),
				args.getString(ARG_MESSAGE),
				true,
				null,
				args.containsKey(ARG_HTTP_STATUS) ? args.getInt(ARG_HTTP_STATUS) : null,
				UriString.fromNullable(args.getString(ARG_URL)),
				context.length() > 0 ? context.toString() : null,
				args.getString(ARG_RESPONSE),
				null);
	}

	@Override
	protected String getTitle(final Context context) {
		return context.getString(R.string.props_error_title);
	}

	@Override
	protected void prepare(
			@NonNull final BaseActivity context,
			@NonNull final LinearLayout items) {

		final Bundle args = requireArguments();

		items.addView(propView(
				context,
				R.string.props_title,
				args.getString(ARG_TITLE),
				true));
		items.addView(propView(
				context,
				"Message",
				args.getString(ARG_MESSAGE),
				false));

		if(args.containsKey(ARG_HTTP_STATUS)) {
			items.addView(propView(
					context,
					"HTTP status",
					String.valueOf(args.getInt(ARG_HTTP_STATUS)),
					false));
		}

		addIfPresent(context, items, "URL", args.getString(ARG_URL));
		addIfPresent(context, items, "Exception", args.getString(ARG_EXCEPTION));
		addIfPresent(context, items, "Response", args.getString(ARG_RESPONSE));
	}

	private void addIfPresent(
			@NonNull final BaseActivity context,
			@NonNull final LinearLayout items,
			@NonNull final String title,
			@Nullable final String value) {

		if(value != null) {
			items.addView(propView(context, title, value, false));
		}
	}
}
