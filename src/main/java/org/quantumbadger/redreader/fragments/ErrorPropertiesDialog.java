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
import androidx.appcompat.app.AppCompatActivity;
import android.widget.LinearLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.common.RRError;

public final class ErrorPropertiesDialog extends PropertiesDialog {

	public static ErrorPropertiesDialog newInstance(final RRError error) {

		final ErrorPropertiesDialog dialog = new ErrorPropertiesDialog();

		final Bundle args = new Bundle();

		args.putString("title", error.title);
		args.putString("message", error.message);

		if(error.t != null) {
			final StringBuilder sb = new StringBuilder(1024);
			BugReportActivity.appendException(sb, error.t, 10);
			args.putString("t", sb.toString());
		}

		if(error.httpStatus != null) {
			args.putString("httpStatus", error.httpStatus.toString());
		}

		if(error.url != null) {
			args.putString("url", error.url);
		}

		dialog.setArguments(args);

		return dialog;
	}

	@Override
	protected String getTitle(Context context) {
		return context.getString(R.string.props_error_title);
	}

	@Override
	protected void prepare(AppCompatActivity context, LinearLayout items) {

		items.addView(propView(context, R.string.props_title, getArguments().getString("title"), true));
		items.addView(propView(context, "Message", getArguments().getString("message"), false));

		if(getArguments().containsKey("httpStatus")) {
			items.addView(propView(context, "HTTP status", getArguments().getString("httpStatus"), false));
		}

		if(getArguments().containsKey("url")) {
			items.addView(propView(context, "URL", getArguments().getString("url"), false));
		}

		if(getArguments().containsKey("t")) {
			items.addView(propView(context, "Exception", getArguments().getString("t"), false));
		}
	}
}
