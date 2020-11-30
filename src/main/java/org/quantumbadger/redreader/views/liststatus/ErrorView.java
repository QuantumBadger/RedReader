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

package org.quantumbadger.redreader.views.liststatus;

import android.graphics.Color;
import android.view.Gravity;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.fragments.ErrorPropertiesDialog;

import java.util.Locale;

public final class ErrorView extends StatusListItemView {

	public ErrorView(final AppCompatActivity activity, final RRError error) {

		super(activity);

		final TextView textView = new TextView(activity);
		textView.setText(error.title);
		textView.setTextColor(Color.WHITE);
		textView.setTextSize(18.0f);
		textView.setPadding(0, 0, 0, (int)(4 * dpScale));

		final TextView messageView = new TextView(activity);
		messageView.setText(error.message);

		messageView.setTextColor(Color.WHITE);
		messageView.setTextSize(14.0f);
		messageView.setPadding(0, 0, 0, (int)(15 * dpScale));

		final Button detailsButton = new Button(activity);
		detailsButton.setTextColor(Color.WHITE);
		detailsButton.setText(activity.getApplicationContext()
				.getString(R.string.button_error_details)
				.toUpperCase(Locale.getDefault()));
		detailsButton.setBackgroundColor(Color.rgb(0xF8, 0x17, 0x17));

		detailsButton.setOnClickListener(v -> ErrorPropertiesDialog.newInstance(error)
				.show(
						activity.getSupportFragmentManager(),
						null));

		final LinearLayout layout = new LinearLayout(activity);
		layout.setOrientation(LinearLayout.VERTICAL);
		layout.addView(textView);
		layout.addView(messageView);
		layout.addView(detailsButton);

		layout.setPadding(
				(int)(15 * dpScale),
				(int)(15 * dpScale),
				(int)(15 * dpScale),
				(int)(15 * dpScale));

		final LinearLayout.LayoutParams layoutParams
				= (LinearLayout.LayoutParams)detailsButton.getLayoutParams();

		layoutParams.width = ViewGroup.LayoutParams.WRAP_CONTENT;

		//noinspection RtlHardcoded
		layoutParams.gravity = Gravity.RIGHT;

		detailsButton.setLayoutParams(layoutParams);

		setContents(layout);

		setBackgroundColor(Color.rgb(0xCC, 0x00, 0x00));
	}
}
