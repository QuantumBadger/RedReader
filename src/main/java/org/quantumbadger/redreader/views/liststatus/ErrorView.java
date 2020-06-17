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
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.fragments.ErrorPropertiesDialog;

public final class ErrorView extends StatusListItemView {

	public ErrorView(final AppCompatActivity activity, final RRError error) {

		super(activity);

		final TextView textView = new TextView(activity);
		textView.setText(error.title);
		textView.setTextColor(Color.WHITE);
		textView.setTextSize(15.0f);
		textView.setPadding((int) (15 * dpScale), (int) (10 * dpScale), (int) (10 * dpScale), (int) (4 * dpScale));

		final TextView messageView = new TextView(activity);
		messageView.setText(error.message);

		messageView.setTextColor(Color.WHITE);
		messageView.setTextSize(12.0f);
		messageView.setPadding((int) (15 * dpScale), 0, (int) (10 * dpScale), (int) (10 * dpScale));

		final LinearLayout layout = new LinearLayout(activity);
		layout.setOrientation(LinearLayout.VERTICAL);
		layout.addView(textView);
		layout.addView(messageView);

		setContents(layout);

		setBackgroundColor(Color.rgb(0xCC, 0x00, 0x00));

		setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				ErrorPropertiesDialog.newInstance(error).show(activity.getSupportFragmentManager(), null);
			}
		});
	}
}
